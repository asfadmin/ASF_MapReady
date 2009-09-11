// Implementation of interface described in table_file.h.

// Note that since we actually fork processes to handle thread locking
// and unlocking, most of the thread related field lock and offset
// type caching actually languishes unused.  It is tested and working
// pretty well though.  The code as it would be in a world where fcntl
// worked as we would like is in table_file_if_fcntl_worked_right.c

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "table_file.h"
#include "utilities.h"

#ifdef G_LOG_DOMAIN
#  undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "TableFile"

// Actual size of one entry, inclue field key and field value.
#define TABLE_FILE_ENTRY_SIZE (TABLE_FILE_MAX_KEY_LENGTH + 1 \
                               + TABLE_FILE_MAX_VALUE_LENGTH + 1)

// Global hash of table path name GString instances to the instances
// of this class attached to them.  Used to implement the
// only-one-instance-per-file-per-process rule, and to allow GMutex
// reinitialization on fork.  FIIXME: this is actually slightly broken
// in that it prevents the forked processes from opening new instances
// associated with files open in their parents, which so far as I know
// causes no trouble (in fact we have to do this to implement the lock
// slave process workaround, at the moment we just explicitly clear
// this table in the slave process).
static GHashTable *tables;

// We help to make sure threaded programmers don't screw up and create
// multiple instances pointing to the same file concurrently.
static GStaticMutex tables_hash_mutex = G_STATIC_MUTEX_INIT;

// At fork time we technically have to reinitialize mutexes.
static void
post_fork_child (void)
{
  g_static_mutex_init (&tables_hash_mutex);

  GPtrArray *values = my_g_hash_table_values (tables);
  guint ii;
  for ( ii = 0 ; ii < values->len ; ii++ ) {
    TableFile *ct = g_ptr_array_index (values, ii);
    ec_lock_free_regardless (ct->table_lock);
    ct->table_lock = ec_lock_new ();
  }
}

// We are depending on the fact that no locks are held at fork time.
// This function just runs assertions to that effect and is redundant
// with the unused flag that is carried around in the structure.  But
// hey, it doesn't hurt to be careful.
static void
pre_fork_parent (void)
{
  gboolean lock_result = g_static_mutex_trylock (&tables_hash_mutex);
  g_assert (lock_result);
  g_static_mutex_unlock (&tables_hash_mutex);

  GPtrArray *values = my_g_hash_table_values (tables);
  guint ii;
  for ( ii = 0 ; ii < values->len ; ii++ ) {
    TableFile *ct = g_ptr_array_index (values, ii);
    g_assert ((! ec_lock_have_reader_lock (ct->table_lock))
	      && (! ec_lock_have_writer_lock (ct->table_lock)));
  }  
}

// Flag true iff we have already called pthread_atfork().
static gboolean pthread_atfork_done = FALSE;

// The table file has a title line which we lock when we want to get a
// table lock.  This string gets set to the text of that title line.
static GString *title_line = NULL;

// Entry size in bytes, including key and value strings and
// terminating NUL bytes of both.
static const size_t entry_size = (TABLE_FILE_MAX_KEY_LENGTH + 1 
				  + TABLE_FILE_MAX_VALUE_LENGTH + 1);

// The value of the symbol we use to mark an empty table slot.
static gchar *free_symbol = NULL;

// The length in bytes of the free symbol;
static size_t free_symbol_length;

// We also have to be sure that when the user sets a field to the
// empty string, the field doesn't accidently end up looking like a
// field that has been marked free.  So we have this special nonfree
// symbol as well.  Its probably only really important that it is
// different than the free symbol.
static gchar *nonfree_symbol = NULL;

static size_t nonfree_symbol_length;

// Get information about one of the locks on fd, which must have an
// l_whence value of SEEK_SET.  The passed in value of *l_type is
// ignored and F_WRLCK is passed to fcntl (since only a writer lock
// will conflict with reader locks and therefore return accurate
// information about at least one existing reader lock).  The passed
// in values of *lstart, and *l_len are used as input and output to
// the fcntl system call.  A value of zero for *l_len means "remainder
// of file" as for fcntl.  On exit from this routine, *l_type is the
// type of lock held on the queried region, or F_UNLCK if no lock is
// held.  Note that there may be other locks on which no information
// is returned.  The l_start and l_len results are with respect to the
// start of the file.  The l_pid output is the PID of the process
// holding the lock, if any.  The *l_start, *l_len, and *l_pid
// arguments are unchenged if *l_type is F_UNLCK, as for fcntl.
/* 
 * Sadly, this code doesn't seem to work, probably we can't query for
 * locks we hold ourselves.
static void
check_file_lock (int fd, short *l_type, off_t *l_start, off_t *l_len,
		 pid_t *l_pid)
{
  static struct flock lock_spec;
  lock_spec.l_type = F_WRLCK;
  lock_spec.l_whence = SEEK_SET;
  lock_spec.l_start = *l_start;
  lock_spec.l_len = *l_len; // 0 means "remainder of file" in this case.
  int return_code = fcntl (fd, F_GETLK, &lock_spec);
  g_assert (return_code != -1);
  g_assert (lock_spec.l_type != F_UNLCK || lock_spec.l_whence == SEEK_SET);
  *l_type = lock_spec.l_type;
  *l_start = lock_spec.l_start;
  *l_len = lock_spec.l_len;
  *l_pid = lock_spec.l_pid;
}
*/

// Convenience interface to fcntl.  The l_whence field of the flock
// struct is automatically set to SEEK_SET, and the l_type field must
// be either F_RDLCK of F_WRLCK.  Note that passing zero for l_len
// means "remainder of file", as in fcntl.  This function blocks until
// the requested lock is obtained.
static void
obtain_file_lock (int fd, short l_type, off_t l_start, off_t l_len)
{
  g_assert (l_type == F_RDLCK || l_type == F_WRLCK);

  static struct flock lock_spec;
  lock_spec.l_type = l_type;
  lock_spec.l_whence = SEEK_SET;
  lock_spec.l_start = l_start;
  lock_spec.l_len = l_len;	// 0 means "remainder of file" in this case.
  int return_code = fcntl (fd, F_SETLKW, &lock_spec);
  if ( return_code != 0 ) {
    GString *pts = pid_thread_string ();
    GString *lock_type = g_string_new ("");
    if ( l_type == F_RDLCK ) {
      g_string_append (lock_type, "F_RDLCK");
    }
    else if ( l_type == F_WRLCK ) {
      g_string_append (lock_type, "F_WRLCK");
    }
    GString *lock_range = g_string_new ("");
    g_string_append_printf (lock_range, "%lld - %lld", (long long int) l_start,
			    (long long int) l_start 
			    + (long long int) l_len - 1);
    g_error ("in %s: fcntl %s of range %s failed: %s\n", pts->str,
	     lock_type->str, lock_range->str, strerror (errno));
    my_g_string_free (pts);
  }
  g_assert (return_code == 0);  
}

// Analagous to obtain_file_lock(), but doesn't block: if the lock is
// not immediately obtainable, we give up and return FALSE (otherwise
// we obtain the lock and return TRUE).
static gboolean
try_file_lock (int fd, short l_type, off_t l_start, off_t l_len)
{
  g_assert (l_type == F_RDLCK || l_type == F_WRLCK);

  static struct flock lock_spec;
  lock_spec.l_type = l_type;
  lock_spec.l_whence = SEEK_SET;
  lock_spec.l_start = l_start;
  lock_spec.l_len = l_len;	// 0 means "remainder of file" in this case.
  int return_code = fcntl (fd, F_SETLK, &lock_spec);
  if ( return_code != 0 ) {
    if ( return_code == -1 && (errno == EACCES || errno == EAGAIN) ) {
      return FALSE;
    }
    GString *pts = pid_thread_string ();
    GString *lock_type = g_string_new ("");
    if ( l_type == F_RDLCK ) {
      g_string_append (lock_type, "F_RDLCK");
    }
    else if ( l_type == F_WRLCK ) {
      g_string_append (lock_type, "F_WRLCK");
    }
    GString *lock_range = g_string_new ("");
    g_string_append_printf (lock_range, "%lld - %lld", (long long int) l_start,
			    (long long int) l_start 
			    + (long long int) l_len - 1);
    g_error ("%s: fcntl %s of range %s failed: %s\n", pts->str, lock_type->str,
	     lock_range->str, strerror (errno));
    my_g_string_free (pts);
  }
  g_assert (return_code == 0);  

  return TRUE;
}


// Release the lock of type l_type held on fd region defined by
// l_start and l_len with respect to the start of the file.
static void
release_file_lock (int fd, short l_type, off_t l_start, off_t l_len)
{
  // We had better hold at least the lock we are about to release.

  /* Sadly, I don't think the lock queries work when we are the ones
   * holding the lock.
   *
  short l_type_copy = l_type;
  off_t l_start_copy = l_start, l_len_copy = l_len;
  pid_t l_pid;
  check_file_lock (fd, &l_type_copy, &l_start_copy, &l_len_copy, &l_pid);
  g_assert (l_type_copy == l_type);
  g_assert (l_start_copy <= l_start);
  g_assert (l_len_copy == 0 || l_len_copy >= l_len + (l_start - l_start_copy));
  // If its a writer lock, then only one process can be holding it,
  // and it better be us.
  if ( l_type == F_WRLCK ) {
    g_assert (l_pid == getpid ());
  }
  */

  // Release the lock.
  static struct flock lock_spec;
  lock_spec.l_type = F_UNLCK;
  lock_spec.l_whence = SEEK_SET;
  lock_spec.l_start = l_start;
  lock_spec.l_len = l_len;
  int return_code = fcntl (fd, F_SETLK, &lock_spec);
  g_assert (return_code != -1);  
}

// For each thread, we keep a flag indicating whether it has a slave
// process to do its bidding, and possibly the pipes used to
// communicate with that slave.
typedef struct {
  gboolean has_slave;
  int p2c_pipe[2], c2p_pipe[2];
} thread_table_entry_type;

// Return true iff an entry for the calling thread exists in table.
static gboolean
thread_table_entry_exists (GHashTable *table)
{
  GString *pts = pid_thread_string ();
  gboolean result = my_g_hash_table_entry_exists (table, pts);
  g_string_free (pts, TRUE);

  return result;
}

// Lookup the current thread in thread_table, dieing if it isn't found.
static thread_table_entry_type *
thread_table_entry_lookup (GHashTable *thread_table)
{
  GString *pts = pid_thread_string ();
  thread_table_entry_type *result
    = g_hash_table_lookup (thread_table, pts);
  g_assert (result != NULL);
  g_string_free (pts, TRUE);

  return result;
}

// Add entry keyed by current thread to table.  It is an error if an
// entry with key thread is already present in table.
static void
thread_table_add_entry (GHashTable *table, thread_table_entry_type *entry)
{
  g_assert (! thread_table_entry_exists (table));

  GString *pts = pid_thread_string ();
  // Here the table takes ownership of pts.
  g_hash_table_insert (table, pts, entry);
}

// Return true iff the current thread has a proxy process to which it
// is supposed to forward all its lock requests and queries.
static gboolean
have_slave (TableFile *self) 
{
  gboolean result = FALSE;
  
  gboolean already_have_ds_lock = ec_lock_have_writer_lock (self->ds_lock);
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_lock (self->ds_lock);
  }
  {
    if ( thread_table_entry_exists (self->thread_table) ) {
      thread_table_entry_type *table_entry
	= thread_table_entry_lookup (self->thread_table);
      if ( table_entry->has_slave ) {
	result = TRUE;
      }
    }
  }
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_unlock (self->ds_lock);
  }

  return result;
}

// A message capable of expressing all the possible return values from
// the methods we need our slave process to execute.
typedef struct {
  gboolean return_value;
  gboolean field_exists;  // Use by trylock only as of this writing.
  guint entry_count;  // A different name for the field_count.
  guint field_count;  // Number of fields result from a catalog method.
  // The fields names that result from a catalog method call.
  gchar field_value[TABLE_FILE_MAX_VALUE_LENGTH + 1];
  gchar fields[TABLE_FILE_MAX_KEY_LENGTH + 1][MAX_FIELD_COUNT];
} method_results_type;

// Operation that our slave process needs to perform if we are a
// spawned thread.  This list should include all the methods that do
// file locking, since POSIX fcntl-base file locking gets confused
// when used from multiple threads in the same process.
typedef enum {
  TABLE_OPERATION_TABLE_READER_LOCK,
  TABLE_OPERATION_TABLE_READER_UNLOCK,
  TABLE_OPERATION_TABLE_WRITER_LOCK,
  TABLE_OPERATION_ADD_FIELD,
  TABLE_OPERATION_REMOVE_FIELD,
  TABLE_OPERATION_FIELD_READER_LOCK,
  TABLE_OPERATION_FIELD_READER_UNLOCK,
  TABLE_OPERATION_FIELD_WRITER_LOCK,
  TABLE_OPERATION_FIELD_WRITER_TRYLOCK,
  TABLE_OPERATION_FIELD_WRITER_UNLOCK,
  TABLE_OPERATION_TABLE_WRITER_UNLOCK,
  TABLE_OPERATION_HAVE_TABLE_READER_LOCK,
  TABLE_OPERATION_HAVE_TABLE_WRITER_LOCK,
  TABLE_OPERATION_HAVE_FIELD_READER_LOCK,
  TABLE_OPERATION_HAVE_FIELD_WRITER_LOCK,
  TABLE_OPERATION_CATALOG,
  TABLE_OPERATION_ENTRY_COUNT,
  TABLE_OPERATION_GET_FIELD_VALUE,
  TABLE_OPERATION_SET_FIELD_VALUE
} table_operation_type;

// A message capable of expressing all the possible arguments to the
// methods we need our slave process to execute.
typedef struct {
  table_operation_type table_operation;
  gchar field_key[TABLE_FILE_MAX_KEY_LENGTH + 1];
  gchar field_value[TABLE_FILE_MAX_VALUE_LENGTH + 1];
} method_call_type;

// Forward a method to a slave process for execution, marshalling
// results into the return structure.
static method_results_type
command_slave (TableFile *self, table_operation_type op, const gchar *key,
	       const gchar *value)
{
  method_results_type method_results;

  gboolean already_have_ds_lock = ec_lock_have_writer_lock (self->ds_lock);
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_lock (self->ds_lock);
  }
  {
    thread_table_entry_type *te
      = thread_table_entry_lookup (self->thread_table);
    g_assert (te->has_slave);
				
    int *p2c = te->p2c_pipe, *c2p = te->c2p_pipe;
  
    method_call_type method_call;
    method_call.table_operation = op;
    if ( key != NULL ) {
      strcpy (method_call.field_key, key);
    }
    if ( value != NULL ) {
      strcpy (method_call.field_value, value);
    }

    ssize_t bytes_written = write (p2c[1], &method_call,
				   sizeof (method_call_type));
    g_assert (bytes_written == sizeof (method_call_type));
  
    // I don't think its possible for the table entry to get
    // relocated, but we have to release ds_lock to aboid blocking
    // here and you can't be too careful, so we make a copy of the
    // descriptor.
    int c2p_read_end = c2p[0];

    ec_lock_writer_unlock (self->ds_lock);
    {
      ssize_t bytes_read = read (c2p_read_end, &method_results,
				 sizeof (method_results_type));
      g_assert (bytes_read == sizeof (method_results_type));
    }
    ec_lock_writer_lock (self->ds_lock);
  }
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_unlock (self->ds_lock);
  }

  return method_results;
}

// Return true iff we have a table write lock on the table.
static gboolean
have_table_writer_lock (TableFile *self)
{
  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_HAVE_TABLE_WRITER_LOCK, NULL,
		       NULL);
    return results.return_value;
  }


  gboolean result = ec_lock_have_writer_lock (self->table_lock);

  // Consistency check: if we hold the intrapocess writer lock, we
  // should also hold the interprocess file writer lock.
  /* Sadly doesn't work: can't query for locks we hold ourselves. 
   *
  if ( result ) {
    short l_type;			// Not an input to check_file_lock.
    off_t l_start = 0, l_len = title_line->len + 1;
    pid_t l_pid;                  // For return of pid holding lock.
    check_file_lock (self->table_fd, &l_type, &l_start, &l_len, &l_pid);
    
    g_assert (l_type == F_WRLCK && l_start == 0 
	      && l_len == title_line->len + 1 && l_pid == getpid ());
  }
  */

  return result;
}

// Return true iff we have a table read lock on the table.  Note that
// this method will return false if we have a writer lock on the
// table.
static gboolean
have_table_reader_lock (TableFile *self)
{
  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_HAVE_TABLE_READER_LOCK, NULL,
		       NULL);
    return results.return_value;
  }

  gboolean result = ec_lock_have_reader_lock (self->table_lock);

  // Consistency check: if we hold the intraprocess reader lock, then
  // at least one PID had better hold a file reader lock.
  /* Sadly doesn't work: can't query for locks we hold ourselves.
  if ( result ) {
    short l_type;			// Not an input to check_file_lock.
    off_t l_start = 0, l_len = title_line->len + 1;
    pid_t l_pid;                  // For return of pid holding lock.
    check_file_lock (self->table_fd, &l_type, &l_start, &l_len, &l_pid);

    g_assert (l_type == F_RDLCK && l_start == 0 
	      && l_len == title_line->len + 1);
  }
  */

  return result;
}

// We will install a more useful error handler for this class.
static gboolean error_message_handler_set = FALSE;

static void
table_file_error_logger (const char *log_domain, GLogLevelFlags log_level,
			 const gchar *message, gpointer user_data)
{
  if ( log_level & G_LOG_LEVEL_ERROR ) {
    trmsg ("%s-ERROR **: %s", log_domain, message);
  }
  else {
    trmsg ("%s-WHAT_THE_HECK_EVER **: %s", log_domain, message);
  }
}

TableFile *
table_file_new (const char *file)
{
  // Make sure we have the GThread system up and running.
  if ( !g_thread_supported () ) {
    g_thread_init (NULL);
  }

  // Install an error handler that prints process and thread
  // information.
  if ( ! error_message_handler_set ) {
    g_log_set_handler (G_LOG_DOMAIN,
		       G_LOG_LEVEL_ERROR | G_LOG_FLAG_FATAL
		       | G_LOG_FLAG_RECURSION,
		       table_file_error_logger, NULL);
    error_message_handler_set = TRUE;
  }

  // Make sure the table_title global is set.
  if ( title_line == NULL ) {
    title_line = g_string_new ("TABLE FILE\n");
  }

  // Make sure the free_symbol and free_symbol_length globals are set.
  if ( free_symbol == NULL ) {
    // For our free symbol we will use a NUL byte, followed by a magic
    // string, followed by another NUL byte.
    free_symbol_length = 1 + strlen ("EMPTY SLOT") + 1;
    free_symbol = g_new (gchar, free_symbol_length);
    free_symbol[0] = '\0';
    int sprintf_result = sprintf (free_symbol + 1, "EMPTY SLOT");
    g_assert (sprintf_result == strlen ("EMPTY SLOT"));
  }

  // Make sure the nonfree_symbol and nonfree_symbol length globals are set.
  if  ( nonfree_symbol == NULL ) {
    // For our nonfree symbol we will use a NUL byte, followed by a
    // magic string, followed by another NUL byte.
    nonfree_symbol_length = 1 + strlen ("NONEMPTY SLOT") + 1;
    nonfree_symbol = g_new (gchar, nonfree_symbol_length);
    nonfree_symbol[0] = '\0';
    int sprintf_result = sprintf (nonfree_symbol + 1, "NONEMPTY SLOT");
    g_assert (sprintf_result == strlen ("NONEMPTY SLOT"));    
  }

  TableFile *self;

  g_static_mutex_lock (&tables_hash_mutex);
  {
    // File name as GString.
    GString *fags = g_string_new (file);
    
    // Create the tables existence hash, if it doesn't already exist.
    if ( tables == NULL ) {
      tables = g_hash_table_new_full ((GHashFunc) g_string_hash,
				      (GEqualFunc) g_string_equal,
				      (GDestroyNotify) my_g_string_free,
				      NULL);
    }

    g_assert (g_hash_table_lookup (tables, fags) == NULL);

    self = g_new (TableFile, 1);

    g_hash_table_insert (tables, fags, self);
  }
  g_static_mutex_unlock (&tables_hash_mutex);

  if ( ! pthread_atfork_done ) {
#ifndef G_THREADS_IMPL_POSIX
#  error "This code probably only works if POSIX threads are in use."
#endif
    int return_code = pthread_atfork (pre_fork_parent, NULL, post_fork_child);
    g_assert (return_code == 0);
    pthread_atfork_done = TRUE;
  }

  self->path = g_string_new (file);
    
  self->ds_lock = ec_lock_new ();

  self->unused = TRUE;

  self->table_fd = open (self->path->str, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  g_assert (self->table_fd != -1);

  self->pid = getpid ();

  // FIIXME: this is a buggy approach.  Not only does the table grow
  // without bound, but if a thread address ever got recycled we might
  // get confused and mistake an old entry for a new one.  But it will
  // hopefully work well enough for our purposes for the moment.
  self->thread_table
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     g_free);

  thread_table_entry_type *new_entry = g_new (thread_table_entry_type, 1);
  new_entry->has_slave = FALSE;
  // See FIIXME comment above other thread_table_add_entry call.
  thread_table_add_entry (self->thread_table, new_entry);
  
  // If the file is new, it won't have a title line yet, in which case
  // we add one.  If it has any contents, the first part better be the
  // title line.
  static struct flock lock_spec;
  lock_spec.l_type = F_WRLCK;
  lock_spec.l_whence = SEEK_SET;
  lock_spec.l_start = 0;
  lock_spec.l_len = title_line->len + 1;
  int return_code = fcntl (self->table_fd, F_SETLKW, &lock_spec);
  g_assert (return_code == 0);
  gchar *buffer = g_new (gchar, title_line->len + 1);
  size_t bytes_to_read = title_line->len + 1;
  ssize_t bytes_read = read (self->table_fd, buffer, bytes_to_read);
  g_assert (bytes_read != -1);
  if ( bytes_read == 0 ) {
    size_t bytes_to_write = title_line->len + 1;
    ssize_t bytes_written = write (self->table_fd, title_line->str,
				   title_line->len + 1);
    g_assert (bytes_written != -1);
    g_assert (bytes_to_write < SSIZE_MAX);
    g_assert (bytes_written == bytes_to_write);
  }
  else {
    g_assert (strncmp (title_line->str, buffer, bytes_to_read) == 0);
  }
  lock_spec.l_type = F_UNLCK;
  return_code = fcntl (self->table_fd, F_SETLK, &lock_spec);
  g_assert (return_code == 0);

  self->table_lock = ec_lock_new ();

  self->field_locks 
    = g_hash_table_new_full ((GHashFunc) g_string_hash, 
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     (GDestroyNotify) ec_lock_unref);

  self->field_reader_lock_counts
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     (GDestroyNotify) NULL);
  
  self->field_values
    = g_hash_table_new_full ((GHashFunc) g_string_hash, 
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     (GDestroyNotify) my_g_string_free);

  self->field_offsets
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     (GDestroyNotify) NULL);

  self->reference_count = 1;

  return self;
}

static gboolean
have_field_reader_lock (TableFile *self, const char *key);

static gboolean
have_field_writer_lock (TableFile *self, const char *key);

// Store the fields list that results from a catalog method call in
// the results structure we use for passing information through the
// pipe between a thread and its lock slave processes.
static void
store_catalog_in_results_structure (GPtrArray *fields,
				    method_results_type *results)
{
  results->field_count = fields->len;

  guint ii;
  for ( ii = 0 ; ii < fields->len ; ii++ ) {
    GString *cf = g_ptr_array_index (fields, ii);
    strcpy ((results->fields)[ii], cf->str);
  }
}

// See definition for details.
static off_t
table_entry_count (TableFile *self);

// Execute the method represented by method_call, and return the
// results in method_results.z
static void
run_method (TableFile *self, method_call_type *method_call,
	    method_results_type *method_results)
{
  gchar *key = method_call->field_key;
  gchar *value = method_call->field_value;

  gboolean *return_value = &(method_results->return_value);
  gboolean *field_exists = &(method_results->field_exists);
  guint *entry_count = &(method_results->entry_count);

  switch ( method_call->table_operation ) {
  case TABLE_OPERATION_TABLE_READER_LOCK:
    table_file_table_reader_lock (self);
    break;
  case TABLE_OPERATION_TABLE_READER_UNLOCK:
    table_file_table_reader_unlock (self);
    break;
  case TABLE_OPERATION_TABLE_WRITER_LOCK:
    table_file_table_writer_lock (self);
    break;
  case TABLE_OPERATION_ADD_FIELD:
    *return_value = table_file_add_field (self, key, value);
    break;
  case TABLE_OPERATION_REMOVE_FIELD:
    table_file_remove_field (self, key);
    break;
  case TABLE_OPERATION_TABLE_WRITER_UNLOCK:
    table_file_table_writer_unlock (self);
    break;
  case TABLE_OPERATION_FIELD_READER_LOCK:
    *return_value = table_file_field_reader_lock (self, key);
    break;
  case TABLE_OPERATION_FIELD_READER_UNLOCK:
    table_file_field_reader_unlock (self, key);
    break;
  case TABLE_OPERATION_FIELD_WRITER_LOCK:
    *return_value = table_file_field_writer_lock (self, key);
    break;
  case TABLE_OPERATION_FIELD_WRITER_TRYLOCK:
    *return_value
      = table_file_field_writer_trylock (self, key, field_exists);
    break;
  case TABLE_OPERATION_FIELD_WRITER_UNLOCK:
    table_file_field_writer_unlock (self, key);
    break;
  case TABLE_OPERATION_HAVE_TABLE_READER_LOCK:
    *return_value = have_table_reader_lock (self);
    break;
  case TABLE_OPERATION_HAVE_TABLE_WRITER_LOCK:
    *return_value = have_table_writer_lock (self);
    break;
  case TABLE_OPERATION_HAVE_FIELD_READER_LOCK:
    *return_value = have_field_reader_lock (self, key);
    break;
  case TABLE_OPERATION_HAVE_FIELD_WRITER_LOCK:
    *return_value = have_field_writer_lock (self, key);
    break;
  case TABLE_OPERATION_CATALOG:
    {
      GPtrArray *fields = table_file_catalog (self);
      store_catalog_in_results_structure (fields, method_results);
      my_g_ptr_array_really_free (fields, (FreeFunc) my_g_string_free);
      break;
    }
  case TABLE_OPERATION_ENTRY_COUNT:
    *entry_count = table_entry_count (self);
    break;
  case TABLE_OPERATION_GET_FIELD_VALUE:
    {
      GString *field_value = table_file_get_field_value (self, key);
      strcpy (method_results->field_value, field_value->str);
      g_string_free (field_value, TRUE);
      break;
    }
  case TABLE_OPERATION_SET_FIELD_VALUE:
    table_file_set_field_value (self, key, value);
    break;
  default:
    g_assert_not_reached ();
    break;
  }
}

static GString *
op_code_to_string (method_call_type *method_call)
{
  switch ( method_call->table_operation ) {
  case TABLE_OPERATION_TABLE_READER_LOCK:
    return g_string_new ("TABLE_OPERATION_TABLE_READER_LOCK");
    break;
  case TABLE_OPERATION_TABLE_READER_UNLOCK:
    return g_string_new ("TABLE_OPERATION_TABLE_READER_UNLOCK");
    break;
  case TABLE_OPERATION_TABLE_WRITER_LOCK:
    return g_string_new ("TABLE_OPERATION_TABLE_WRITER_LOCK");
    break;
  case TABLE_OPERATION_ADD_FIELD:
    return g_string_new ("TABLE_OPERATION_ADD_FIELD");
    break;
  case TABLE_OPERATION_REMOVE_FIELD:
    return g_string_new ("TABLE_OPERATION_REMOVE_FIELD");
    break;
  case TABLE_OPERATION_FIELD_READER_LOCK:
    return g_string_new ("TABLE_OPERATION_FIELD_READER_LOCK");
    break;
  case TABLE_OPERATION_FIELD_READER_UNLOCK:
    return g_string_new ("TABLE_OPERATION_FIELD_READER_UNLOCK");
    break;
  case TABLE_OPERATION_FIELD_WRITER_LOCK:
    return g_string_new ("TABLE_OPERATION_FIELD_WRITER_LOCK");
    break;
  case TABLE_OPERATION_FIELD_WRITER_TRYLOCK:
    return g_string_new ("TABLE_OPERATION_FIELD_WRITER_TRYLOCK");
    break;
  case TABLE_OPERATION_FIELD_WRITER_UNLOCK:
    return g_string_new ("TABLE_OPERATION_FIELD_WRITER_UNLOCK");
    break;
  case TABLE_OPERATION_TABLE_WRITER_UNLOCK:
    return g_string_new ("TABLE_OPERATION_TABLE_WRITER_UNLOCK");
    break;
  case TABLE_OPERATION_HAVE_TABLE_READER_LOCK:
    return g_string_new ("TABLE_OPERATION_HAVE_TABLE_READER_LOCK");
    break;
  case TABLE_OPERATION_HAVE_TABLE_WRITER_LOCK:
    return g_string_new ("TABLE_OPERATION_HAVE_TABLE_WRITER_LOCK");
    break;
  case TABLE_OPERATION_HAVE_FIELD_READER_LOCK:
    return g_string_new ("TABLE_OPERATION_HAVE_FIELD_READER_LOCK");
    break;
  case TABLE_OPERATION_HAVE_FIELD_WRITER_LOCK:
    return g_string_new ("TABLE_OPERATION_HAVE_FIELD_WRITER_LOCK");
    break;
  case TABLE_OPERATION_CATALOG:
    return g_string_new ("TABLE_OPERATION_CATALOG");
    break;
  case TABLE_OPERATION_ENTRY_COUNT:
    return g_string_new ("TABLE_OPERATION_ENTRY_COUNT");
    break;
  case TABLE_OPERATION_GET_FIELD_VALUE:
    return g_string_new ("TABLE_OPERATION_GET_FIELD_VALUE");
    break;
  case TABLE_OPERATION_SET_FIELD_VALUE:
    return g_string_new ("TABLE_OPERATION_SET_FIELD_VALUE");
    break;
  default: 
    g_assert_not_reached ();
    break;
  }
}

// Fork a child that can do file locking for a thread (since POSIX
// fcntl gets confused when threads try to do this for themselves).
// This function is designed st it is safe for two threads to race
// into it.
static void
fork_locker (const gchar *path, int *p2c_pipe, int *c2p_pipe)
{
  int return_code = pipe (p2c_pipe);
  g_assert (return_code == 0);
  return_code = pipe (c2p_pipe);
  g_assert (return_code == 0);

  pid_t pid = fork ();
  g_assert (pid != -1);

  if ( pid == 0 ) { // Child.

    // Work around the bug that causes us not to be able to create
    // independent instances in children by clearing the table.
    g_hash_table_destroy (tables);
    tables = g_hash_table_new_full ((GHashFunc) g_string_hash,
				    (GEqualFunc) g_string_equal,
				    (GDestroyNotify) my_g_string_free,
				    NULL);

    TableFile *proxy_instance = table_file_new (path);

    return_code = close (p2c_pipe[1]);
    g_assert (return_code != -1);

    return_code = close (c2p_pipe[0]);
    g_assert (return_code != -1);

    method_call_type method_call;
    method_results_type method_results;

    ssize_t bytes_read;
    while ( (bytes_read = read (p2c_pipe[0], &method_call,
				sizeof (method_call_type)))
	    == sizeof (method_call_type) ) {
      GString *operation_string = op_code_to_string (&method_call);
      run_method (proxy_instance, &method_call, &method_results);
      g_string_free (operation_string, TRUE);
      ssize_t bytes_written = write (c2p_pipe[1], &method_results,
				     sizeof (method_results));
      if ( bytes_written != sizeof (method_results) ) {
	break;
      }
    }

    // We leek the proxy_instance, but it doesn't matter.

    // When a pipe read or write fails, we assume it is because the
    // parent was done.  IMPROVEME: it might be possible to do some
    // error detection of some sort here, without spewing spurious
    // warnings when this process exits merebly because the thread it
    // is slaved to did so.
    exit (0);
  }

  else { // Parent.

    return_code = close (p2c_pipe[0]);
    g_assert (return_code != -1);
    return_code = close (c2p_pipe[1]);
    g_assert (return_code != -1);

    // Now the methods that need slave process services can write to
    // the pipe.
  }
}

// First, if a process or thread change has occurred, insist that self
// hasn't yet been used, as described in the interface.  Next open a
// new descriptor for the current process, or spawn a locking slave
// process for the current thread and set a flag indicating that a
// lock slave exists.  Finally, whether or not a process or thread change
// has occurred, mark self as used.
static void
do_piles_of_crazy_stuff (TableFile *self)
{
  gboolean already_have_ds_lock = ec_lock_have_writer_lock (self->ds_lock);
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_lock (self->ds_lock);
  }
  {
    pid_t current_pid = getpid ();
    if ( self->pid != current_pid ) {
      g_assert (self->unused);
      int return_code = close (self->table_fd);
      g_assert (return_code == 0);
      self->table_fd
	= open (self->path->str, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
      g_assert (self->table_fd != -1);      
      self->pid = current_pid;
      thread_table_entry_type *new_entry = g_new (thread_table_entry_type, 1);
      new_entry->has_slave = FALSE;
      // See FIIXME comment above other thread_table_add_entry call.
      thread_table_add_entry (self->thread_table, new_entry);
    }
    else {
      if ( ! thread_table_entry_exists (self->thread_table) ) {
	thread_table_entry_type *new_entry
	  = g_new (thread_table_entry_type, 1);
	new_entry->has_slave = TRUE;
	ec_lock_writer_unlock (self->ds_lock);
	fork_locker (self->path->str, new_entry->p2c_pipe,
		     new_entry->c2p_pipe);
	ec_lock_writer_lock (self->ds_lock);
	// FIIXME: unfortunately the thread table grows without bounds
	// as new threads are encountered.  This shouldn't be much of
	// an issue since new threads can't be formed once an instance
	// has been used, and sensible programmers use thread pools
	// rather than continually spawning new threads.  I can't
	// think of a way to fix the problem though.  You need
	// something like atexit for threads, which so far as I can
	// tell doesn't exist.
	thread_table_add_entry (self->thread_table, new_entry);
      }
    }
    self->unused = FALSE;
  }
  if ( ! already_have_ds_lock ) {
    ec_lock_writer_unlock (self->ds_lock);
  }
}

void
table_file_table_reader_lock (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_TABLE_READER_LOCK, NULL, NULL);
    return;
  }

  ec_lock_reader_lock (self->table_lock);

  // Lock title line.
  obtain_file_lock (self->table_fd, F_RDLCK, 0, title_line->len + 1);
}

// Return the number of entries in the table, INCLUDING ENTRIES WHICH
// HAVE BEEN MAKED FREE WITH OUR SPECIAL FREE SYMBOL.  Requires a
// (writer) lock on self->ds_lock to be held.
static off_t
table_entry_count (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_ENTRY_COUNT, NULL, NULL);

    return results.entry_count;
  }

  g_assert (ec_lock_have_writer_lock (self->ds_lock));

  int fd = self->table_fd;

  struct stat stat_buf;
  int return_code = fstat (fd, &stat_buf);
  g_assert (return_code != -1);
  
  size_t non_header_size = stat_buf.st_size - (title_line->len + 1);

  if ( non_header_size % (TABLE_FILE_MAX_KEY_LENGTH + 1
			  + TABLE_FILE_MAX_VALUE_LENGTH + 1) != 0 ) {
    g_error ("invalid table file size.  You almost certainly want to delete "
	     "the apparently corrupted table file '%s'\n", self->path->str);
  }

  g_assert (non_header_size % (TABLE_FILE_MAX_KEY_LENGTH + 1
			       + TABLE_FILE_MAX_VALUE_LENGTH + 1) == 0);

  return non_header_size / (TABLE_FILE_MAX_KEY_LENGTH + 1
			    + TABLE_FILE_MAX_VALUE_LENGTH + 1);
}

// Return the offset of key in table, or (off_t) -1 is the key isn't
// found in the table.  Requires a (writer) lock on ds_lock to be
// held.
static off_t
table_field_offset (TableFile *self, const char *key)
{
  g_assert (ec_lock_have_writer_lock (self->ds_lock));

  // Require that key argument be short enough.
  g_assert (strnlen (key, TABLE_FILE_MAX_KEY_LENGTH + 1)
	    <= TABLE_FILE_MAX_KEY_LENGTH);

  int fd = self->table_fd;

  off_t entry_count = table_entry_count (self);

  off_t result = -1;		// Result to be returned.

  // Size of key field in file.  The passed in key string is NUL
  // terminated and is shorter by at least one.
  size_t field_key_size = TABLE_FILE_MAX_KEY_LENGTH + 1;

  // Buffer for keys to be read from the file.
  gchar *buffer = g_new (gchar, field_key_size);

  off_t ii;
  for ( ii = 0 ; ii < entry_count ; ii++ ) {
    off_t desired_offset = title_line->len + 1 + ii * TABLE_FILE_ENTRY_SIZE;
    off_t lseek_result = lseek (fd, desired_offset, SEEK_SET);
    g_assert (lseek_result != (off_t) -1);
    g_assert (lseek_result == desired_offset);
    ssize_t bytes_read = read (fd, buffer, field_key_size);
    g_assert (bytes_read != -1);
    g_assert (bytes_read == field_key_size);  

    // Ensure that the key we have read is NUL terminated.
    g_assert (strnlen (buffer, TABLE_FILE_MAX_KEY_LENGTH + 1)
	      <= TABLE_FILE_MAX_KEY_LENGTH);

    if ( strncmp (buffer, key, TABLE_FILE_MAX_KEY_LENGTH + 1) == 0 ) {
      result = desired_offset;
      break;
    }
  }

  g_free (buffer);

  g_assert (result == -1 || result >= title_line->len + 1);

  return result;
}

// Return true iff an entry having key key appears in table file.
// Requires a (writer) lock on ds_lock to be held.
static gboolean
table_has_entry (TableFile *self, const char *key)
{
  g_assert (ec_lock_have_writer_lock (self->ds_lock));

  return (table_field_offset (self, key) != (off_t) -1);
}

gboolean
table_file_field_exists (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  g_assert (have_table_reader_lock (self) || have_table_writer_lock (self));

  gboolean result;		// To be returned.

  ec_lock_writer_lock (self->ds_lock);
  {
    result = table_has_entry (self, key);
  } 
  ec_lock_writer_unlock (self->ds_lock);

  return result;
}

// Return true iff the beginning free_symbol_length bytes of buffer
// exactly match those of free_symbol (which isn't just a string).
static gboolean
is_free_symbol (gchar *buffer)
{
  size_t ii;
  for ( ii = 0 ; ii < free_symbol_length ; ii++ ) {
    if ( buffer[ii] != free_symbol[ii] ) {
      return FALSE;
    }
  }

  return TRUE;
}

// Unpack the list of fields in results into a new GPtrArray.
static GPtrArray *
unpack_catalog_from_results (method_results_type *results)
{
  GPtrArray *result = g_ptr_array_new ();

  guint ii;
  for ( ii = 0 ; ii < results->field_count ; ii++ ) {
    GString *new_field = g_string_new ((results->fields)[ii]);
    g_ptr_array_add (result, new_field);
  }

  return result;
}

GPtrArray *
table_file_catalog (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_CATALOG, NULL, NULL);

    return unpack_catalog_from_results (&results);
  }

  g_assert (have_table_reader_lock (self) || have_table_writer_lock (self));

  GPtrArray *result = g_ptr_array_new ();

  // Size of key field in file.  The passed in key string is NUL
  // terminated and is shorter by at least one.
  size_t field_key_size = TABLE_FILE_MAX_KEY_LENGTH + 1;

  // Buffer for keys to be read from the file.
  gchar *buffer = g_new (gchar, field_key_size);

  ec_lock_writer_lock (self->ds_lock);
  {
    off_t entry_count = table_entry_count (self);

    // Size of key field in file.  The passed in key string is NUL
    // terminated and is shorter by at least one.
    size_t field_key_size = TABLE_FILE_MAX_KEY_LENGTH + 1;

    off_t ii;
    for ( ii = 0 ; ii < entry_count ; ii++ ) {
      off_t desired_offset = title_line->len + 1 + ii * TABLE_FILE_ENTRY_SIZE;
      off_t lseek_result = lseek (self->table_fd, desired_offset, SEEK_SET);
      g_assert (lseek_result == desired_offset);
      ssize_t bytes_read = read (self->table_fd, buffer, field_key_size);
      g_assert (bytes_read != -1);
      g_assert (bytes_read == field_key_size);  

      g_assert (strnlen (buffer, TABLE_FILE_MAX_KEY_LENGTH + 1)
		<= TABLE_FILE_MAX_KEY_LENGTH);

      if ( !is_free_symbol (buffer) ) {
	g_ptr_array_add (result, g_string_new (buffer));
      }
    }
  }
  ec_lock_writer_unlock (self->ds_lock);

  g_free (buffer);

  return result;
}

void
table_file_table_reader_unlock (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_TABLE_READER_UNLOCK, NULL, NULL);
    return;
  }
  
  // We had better already hold a reader lock.
  g_assert (ec_lock_have_reader_lock (self->table_lock));

  release_file_lock (self->table_fd, F_RDLCK, 0, title_line->len + 1);
  
  ec_lock_reader_unlock (self->table_lock);
}

void
table_file_table_writer_lock (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_TABLE_WRITER_LOCK, NULL, NULL);
    return;
  }

  ec_lock_writer_lock (self->table_lock);

  obtain_file_lock (self->table_fd, F_WRLCK, 0, title_line->len + 1);
} 

// Returns the offset in table of the first free slot available (which
// may be the end of the file).  Requires a writer lock on ds_lock to
// be held while this function runs.
static off_t
first_free_slot_offset (TableFile *self)
{
  g_assert (ec_lock_have_writer_lock (self->ds_lock));

  int fd = self->table_fd;

  off_t entry_count = table_entry_count (self);

  off_t result = -1;		// Result to be returned.

  // Buffer for keys to be read from the file.
  gchar *buffer = g_new (gchar, TABLE_FILE_MAX_KEY_LENGTH + 1);

  off_t ii;
  for ( ii = 0 ; ii < entry_count ; ii++ ) {
    off_t desired_offset = title_line->len + 1 + ii * TABLE_FILE_ENTRY_SIZE;
    off_t lseek_result = lseek (fd, desired_offset, SEEK_SET);
    g_assert (lseek_result == desired_offset);
    size_t bytes_to_read = TABLE_FILE_MAX_KEY_LENGTH + 1;
    ssize_t bytes_read = read (fd, buffer, bytes_to_read);
    g_assert (bytes_read != -1);
    g_assert (bytes_read == bytes_to_read);
    if ( is_free_symbol (buffer) ) {
      result = desired_offset;
      break;
    }
  }
  
  g_free (buffer);

  if ( result == -1 ) {
    result = title_line->len + 1 + entry_count * TABLE_FILE_ENTRY_SIZE;
  }

  g_assert (result >= title_line->len + 1);

  return result;
}

gboolean
table_file_add_field (TableFile *self, const char *key, const char *value)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_ADD_FIELD, key, value);
    return results.return_value;
  }

  g_assert (have_table_writer_lock (self));
  g_assert (strnlen (key, TABLE_FILE_MAX_KEY_LENGTH + 1)
	    <= TABLE_FILE_MAX_KEY_LENGTH);
  g_assert (strnlen (key, TABLE_FILE_MAX_VALUE_LENGTH + 1)
	    <= TABLE_FILE_MAX_VALUE_LENGTH);

  ec_lock_writer_lock (self->ds_lock);

  // IMPROVEME: its inefficient to parse the table here to determine
  // if we already have the entry, then again just below to find the
  // first free slot.
  if ( table_has_entry (self, key) ) {
    ec_lock_writer_unlock (self->ds_lock);
    return FALSE;
  }
  
  off_t slot_offset = first_free_slot_offset (self);
  
  // Key and value as GString instances, for convenience.
  GString *kags = g_string_new (key), *vags = g_string_new (value);

  // Offset of value portion of entry.
  off_t value_offset = slot_offset + TABLE_FILE_MAX_KEY_LENGTH + 1;

  // As promised in the interface, the new field is born with a write
  // lock already granted to the creator.
  ECLock *new_lock = ec_lock_new ();
  ec_lock_writer_lock (new_lock);
  obtain_file_lock (self->table_fd, F_WRLCK, value_offset,
		    TABLE_FILE_MAX_VALUE_LENGTH + 1);

  // Seek to position of entry.
  off_t lseek_result = lseek (self->table_fd, slot_offset, SEEK_SET);
  g_assert (lseek_result != (off_t) -1);
  g_assert (lseek_result == slot_offset);
			    
  // Store the key.
  size_t bytes_to_write = kags->len + 1;
  ssize_t write_count = write (self->table_fd, kags->str, bytes_to_write);
  g_assert (write_count != -1);
  g_assert (write_count == bytes_to_write);

  // Seek to the position to be used for the value.
  lseek_result = lseek (self->table_fd, value_offset, SEEK_SET);
  g_assert (lseek_result != (off_t) -1);
  g_assert (lseek_result == value_offset);

  // Store the value.
  bytes_to_write = vags->len + 1;
  write_count = write (self->table_fd, vags->str, bytes_to_write);
  g_assert (write_count != -1);
  g_assert (write_count == bytes_to_write);

  // To make sure the file is the correct size in the case where we
  // are adding an entry to the end of the file, we write a byte at
  // the end of the space devoted to this value.
  off_t desired_offset = value_offset + TABLE_FILE_MAX_VALUE_LENGTH;
  lseek_result = lseek (self->table_fd, desired_offset, SEEK_SET);
  g_assert (lseek_result != (off_t) -1);
  g_assert (lseek_result == desired_offset);
  char nul_byte = '\0';
  write_count = write (self->table_fd, &nul_byte, 1);
  g_assert (write_count != -1);
  g_assert (write_count == 1);

  my_g_string_free (vags);
  my_g_string_free (kags);

  // Add entry to the table of field locks.
  g_hash_table_insert (self->field_locks, g_string_new (key), new_lock);

  // Create the field reader lock count entry.
  g_hash_table_insert (self->field_reader_lock_counts, g_string_new (key),
		       GUINT_TO_POINTER (0));

  // Create the field value cache entry.
  g_hash_table_insert (self->field_values, g_string_new (key),
		       g_string_new (value));

  // Create the field offset cache entry.
  g_hash_table_insert (self->field_offsets, g_string_new (key),
		       GUINT_TO_POINTER (((guint) slot_offset)));

  ec_lock_writer_unlock (self->ds_lock);

  return TRUE;
}

// Return true iff the calling thread holds a write lock on field with
// key key.  This routine acquires a writer lock on self->ds_lock if
// the caller doesn't already hold one, so it won't necessarily ever
// return if there is a deadlock condition going on somewhere else.
static gboolean
have_field_writer_lock (TableFile *self, const char *key)
{
  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_HAVE_FIELD_WRITER_LOCK, key,
		       NULL);
    return results.return_value;
  }

  // This flag gets set TRUE iff we don't already hold a writer lock
  // on ds_lock and have to acquire one.
  gboolean acquired_writer_lock = FALSE;

  // Acquire a data structure lock, if necessary.
  if ( !ec_lock_have_writer_lock (self->ds_lock) ) {
    ec_lock_writer_lock (self->ds_lock);
    acquired_writer_lock = TRUE;
  }

  GString *kags = g_string_new (key); // Key as GString.

  gboolean result;

  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  if ( field_lock == NULL ) {
    result = FALSE;
  }
  else {
    result = ec_lock_have_writer_lock (field_lock);
  }

  // Consistency check: if we hold a lock, we have better also hold
  // the correct file lock.
  /* Sadly doesn't work: can't query for locks we hold ourselves.
  if ( result ) {
    short l_type;
    gpointer dummy, value_as_gpointer;
    gboolean lookup_result
      = g_hash_table_lookup_extended (self->field_offsets, kags, &dummy,
				      &value_as_gpointer);
    g_assert (lookup_result);
    off_t field_value_offset = ((off_t) (GPOINTER_TO_UINT (value_as_gpointer))
				+ TABLE_FILE_MAX_KEY_LENGTH + 1);
    off_t field_value_size = TABLE_FILE_MAX_VALUE_LENGTH + 1;
    off_t l_start = field_value_offset;
    off_t l_len = field_value_size;
    pid_t l_pid;
    check_file_lock (self->table_fd, &l_type, &l_start, &l_len, &l_pid);
    g_assert (l_type == F_WRLCK);
    g_assert (l_start == field_value_offset && l_len == field_value_size);
    g_assert (l_pid == getpid ());
  }
  */

  my_g_string_free (kags);

  // Release the data structure lock, if we had to acquire it.
  if ( acquired_writer_lock ) {
    ec_lock_writer_unlock (self->ds_lock);
  }

  return result;
}

void
table_file_remove_field (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_REMOVE_FIELD, key, NULL);
    return;
  }

  g_assert (have_table_writer_lock (self));
  g_assert (strnlen (key, TABLE_FILE_MAX_KEY_LENGTH + 1)
	    <= TABLE_FILE_MAX_KEY_LENGTH);
  g_assert (have_field_writer_lock (self, key));

  GString *kags = g_string_new (key); // Key as GString instance.

  ec_lock_writer_lock (self->ds_lock);

  // Consistency check: must not have any reader locks counted.
  g_assert (GPOINTER_TO_UINT (g_hash_table_lookup 
			        (self->field_reader_lock_counts, kags))
	    == 0);

  // Lookup field offset (first we get "Field Offset As GPointer").
  gpointer foagp = g_hash_table_lookup (self->field_offsets, kags);
  off_t field_offset = (off_t) GPOINTER_TO_UINT (foagp);

  // We remove the entry by putting our special free symbol in the
  // space where the key normally goes.
  off_t lseek_result = lseek (self->table_fd, field_offset, SEEK_SET);
  g_assert (lseek_result != (off_t) -1);
  g_assert (lseek_result == field_offset);
  size_t bytes_to_write = free_symbol_length;
  ssize_t bytes_written = write (self->table_fd, free_symbol, bytes_to_write);
  g_assert (bytes_written != -1);
  g_assert (bytes_written == bytes_to_write);

  // Release the file field lock.
  release_file_lock (self->table_fd, F_WRLCK,
		     field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
		     TABLE_FILE_MAX_VALUE_LENGTH + 1);

  // Now we need to remove the intraprocess field lock.  It should be
  // safe to unlock then remove the field without an additional
  // atomicity lock, since we are currently inside a table write lock,
  // and locking an individual field for reading or writing requires a
  // table reader lock to be held.
  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  g_assert (field_lock != NULL);
  ec_lock_writer_unlock (field_lock);
  gboolean return_code = g_hash_table_remove (self->field_locks, kags);
  g_assert (return_code == TRUE);

  // Remove field reader lock count, field value, and field offset
  // cache entries.
  g_hash_table_remove (self->field_reader_lock_counts, kags);
  g_hash_table_remove (self->field_values, kags);
  g_hash_table_remove (self->field_offsets, kags);

  ec_lock_writer_unlock (self->ds_lock);

  my_g_string_free (kags);
}

void
table_file_table_writer_unlock (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_TABLE_WRITER_UNLOCK, NULL, NULL);
    return;
  }

  // We had better already hold a writer lock.
  g_assert (have_table_writer_lock (self));

  release_file_lock (self->table_fd, F_WRLCK, 0, title_line->len + 1);

  ec_lock_writer_unlock (self->table_lock);
}

// Return as a new GString instance the value of the field the key to
// which is at offset field_offset in the file referred to by fd.
// This routine requires that the ds_lock protecting fd be held.
static GString *
lookup_field_value (TableFile *self, off_t field_offset)
{
  g_assert (ec_lock_have_writer_lock (self->ds_lock));

  // Seek the the start of the value part of the field.
  off_t desired_offset = field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1;
  off_t lseek_result = lseek (self->table_fd, desired_offset, SEEK_SET);
  g_assert (lseek_result == desired_offset);
  
  // Read the field value.
  ssize_t bytes_to_read = TABLE_FILE_MAX_VALUE_LENGTH + 1;
  GString *result = g_string_sized_new (bytes_to_read);
  ssize_t bytes_read = read (self->table_fd, result->str, bytes_to_read);
  g_assert (bytes_read != -1);
  g_assert (bytes_read == bytes_to_read);

  return result;
}

gboolean
table_file_field_reader_lock (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_FIELD_READER_LOCK, key, NULL);
    return results.return_value;
  }

  g_assert (have_table_reader_lock (self) || have_table_writer_lock (self));

  // IMPROVEME: might be nice to confirm that the current thread
  // doesn't already hold a reader lock on the field (rather than
  // letting the ECLock catch the problem).

  GString *kags = g_string_new (key); // Key as GString instance.

  ec_lock_writer_lock (self->ds_lock);

  // The entry may be present in our hash of locks, but some other
  // process may have removed the actual entry.  This possibility is
  // covered below.
  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  
  // We also might not have an entry in our hash of locks for this
  // process, but some other process might have added the field.
  off_t field_offset = table_field_offset (self, key);
  
  // If the field doesn't exist...
  if ( field_offset == (off_t) -1 ) {
    // Do some incremental lock table maintenance.  If we have a
    // field_lock for the enntry but we didn't find the entry in the
    // file, some other process must have removed it, so we remove the
    // entries from our field lock table, field reader lock count
    // table, and field value cache table. 
    if ( field_lock != NULL ) {
      gboolean remove_result = g_hash_table_remove (self->field_locks, kags);
      g_assert (remove_result);
      gpointer dummy;		// Dummy that we don't really use.
      gpointer value;	        // Pointer to hash table value.
      gboolean lock_count_entry_found
	= g_hash_table_lookup_extended (self->field_reader_lock_counts,
					kags, &dummy, &value);
      g_assert (lock_count_entry_found);
      // Consistency check (better not have any reader locks counted here).
      g_assert (GPOINTER_TO_UINT (value) == 0);
      remove_result 
	= g_hash_table_remove (self->field_reader_lock_counts, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_values, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_offsets, kags);
      g_assert (remove_result == TRUE);
    }
  
    ec_lock_writer_unlock (self->ds_lock);
  
    my_g_string_free (kags);

    return FALSE;
  }

  // If we didn't have a field lock in our table of locks for this
  // process...
  if ( field_lock == NULL ) {
    field_lock = ec_lock_new ();
    ec_lock_reader_lock (field_lock);
    // Note that here this hash table takes over ownership of kags
    // (and field_lock).
    g_hash_table_insert (self->field_locks, kags, field_lock);
    ec_lock_writer_unlock (self->ds_lock);
    {
      obtain_file_lock (self->table_fd, F_RDLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
    }
    ec_lock_writer_lock (self->ds_lock);
    // Its possible that another thread got a reader lock since we
    // checked, so we use a read-write cycle here.
    gpointer lock_count_as_gpointer 
      = g_hash_table_lookup (self->field_reader_lock_counts, kags);
    guint lock_count = GPOINTER_TO_UINT (lock_count_as_gpointer);
    lock_count++;
    g_hash_table_insert (self->field_reader_lock_counts,
			 g_string_new (kags->str),
			 GUINT_TO_POINTER (lock_count));
    // Cache the field value and offset.
    GString *field_value = lookup_field_value (self, field_offset);
    g_hash_table_insert (self->field_values, g_string_new (kags->str),
			 field_value);
    g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			 GUINT_TO_POINTER (((guint) field_offset)));
  }

  else {
    // Depending on having ds_lock (see comment above)...
    guint lock_count 
      = GPOINTER_TO_UINT (g_hash_table_lookup (self->field_reader_lock_counts,
					       kags));
    if ( lock_count > 0 ) {
      // Note that here we don't need to worry about locks going away,
      // since we hold ds_lock (and we don't need to release it since
      // this operation shouldn't block).
      ec_lock_reader_lock (field_lock);
      obtain_file_lock (self->table_fd, F_RDLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
      lock_count++;
      // Note that here this hash table takes ownership of kags.
      g_hash_table_replace (self->field_reader_lock_counts, kags,
			   GUINT_TO_POINTER (lock_count));
    }
    else {
      ec_lock_writer_unlock (self->ds_lock);
      {
	ec_lock_reader_lock (field_lock);
	// Note that here we are (necessarily) outside ds_lock and so
	// are trusting ultimately in fcntl not to screw things up
	// when one thread uses table_fd to lock a region and others
	// potentially use it to read or write or lock other regions.
	// The fcntl system call is supposed to be thread safe, and
	// the documentation doesn't say anything about advisory file
	// locking either depending on file descriptor position or
	// actually repositioning anything, but this is still pretty
	// scary.
	obtain_file_lock (self->table_fd, F_RDLCK,
			  field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			  TABLE_FILE_MAX_VALUE_LENGTH + 1);
      }
      ec_lock_writer_lock (self->ds_lock);
      // Its possible that another thread got a reader lock since we
      // checked, so we use a read-write cycle here.
      gpointer lock_count_as_gpointer 
	= g_hash_table_lookup (self->field_reader_lock_counts, kags);
      lock_count = GPOINTER_TO_UINT (lock_count_as_gpointer);
      lock_count++;
      g_hash_table_insert (self->field_reader_lock_counts,
			   g_string_new (kags->str),
			   GUINT_TO_POINTER (lock_count));
      // The field value may have changed, so we read it again.
      GString *field_value = lookup_field_value (self, field_offset);
      // Note that here this hash table takes ownership of kags and
      // field_value.
      g_hash_table_replace (self->field_values, kags, field_value);
      // Our cached notion of the field offset might have changed.
      g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			   GUINT_TO_POINTER (((guint) field_offset)));

    }
  }

  ec_lock_writer_unlock (self->ds_lock);

  return TRUE;
}

// Return true iff the calling thread holds a reader lock on field
// with key key.  This routine acquires a writer lock on self->ds_lock
// if the caller doesn't already hold one, so it won't necessarily
// ever return if there is a deadlock condition going on somewhere
// else.
static gboolean
have_field_reader_lock (TableFile *self, const char *key)
{
  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_HAVE_FIELD_READER_LOCK, key,
		       NULL);
    return results.return_value;
  }

  // This flag gets set TRUE iff we don't already hold a writer lock
  // on ds_lock and have to acquire one.
  gboolean acquired_writer_lock = FALSE;
  
  // Acquire a data structure lock, if necessary.
  if ( !ec_lock_have_writer_lock (self->ds_lock) ) {
    ec_lock_writer_lock (self->ds_lock);
    acquired_writer_lock = TRUE;
  }
  
  GString *kags = g_string_new (key); // Key as GString.

  gboolean result;
  
  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  if ( field_lock == NULL ) {
    result = FALSE;
  }
  else {
    result = ec_lock_have_reader_lock (field_lock);
  }
  
  // Consistency check: if we hold a lock, we have better also have at
  // least one reader lock counted, and at least one process somewhere
  // had better hold the correct file lock.
  /* Sadly doesn't work: can't query for locks we hold ourselves.
  if ( result ) {
    gpointer dummy, value_as_gpointer;
    gboolean lookup_result
      = g_hash_table_lookup_extended (self->field_reader_lock_counts, kags,
				      &dummy, &value_as_gpointer);
    g_assert (lookup_result);
    g_assert (GPOINTER_TO_UINT (value_as_gpointer) > 0);
    
    short l_type;
    lookup_result
      = g_hash_table_lookup_extended (self->field_offsets, kags, &dummy,
				      &value_as_gpointer);
    g_assert (lookup_result);
    off_t field_value_offset = ((off_t) (GPOINTER_TO_UINT (value_as_gpointer))
				+ TABLE_FILE_MAX_KEY_LENGTH + 1);
    off_t field_value_size = TABLE_FILE_MAX_VALUE_LENGTH + 1;
    off_t l_start = field_value_offset;
    off_t l_len = field_value_size;
    pid_t l_pid;
    check_file_lock (self->table_fd, &l_type, &l_start, &l_len, &l_pid);
    g_assert (l_type == F_RDLCK);
    g_assert (l_start == field_value_offset && l_len == field_value_size);
    // Note that we can't use getpid on the returned l_pid, since our
    // query might have returned information on a reader lock held by
    // some other process.
  }
  */  

  my_g_string_free (kags);
  
  // Release the data structure lock, if we had to acquire it.
  if ( acquired_writer_lock ) {
    ec_lock_writer_unlock (self->ds_lock);
  }
  
  return result;
}

GString *
table_file_get_field_value (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_GET_FIELD_VALUE, key, NULL);
    return g_string_new (results.field_value);
  }

  g_assert (have_field_reader_lock (self, key)
	    || have_field_writer_lock (self, key));

  GString *kags = g_string_new (key); // Key as GString instance.

  GString *result;		// Result to be returned.

  ec_lock_writer_lock (self->ds_lock);
  {
    GString *field_value = g_hash_table_lookup (self->field_values, kags);
    g_assert (field_value != NULL);
    
    result = g_string_new (field_value->str);
  }
  ec_lock_writer_unlock (self->ds_lock);

  my_g_string_free (kags);

  return result;
}

void
table_file_field_reader_unlock (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_FIELD_READER_UNLOCK, key, NULL);
    return;
  }

  g_assert (have_field_reader_lock (self, key));

  // Key as GString.
  GString *kags = g_string_new (key);

  ec_lock_writer_lock (self->ds_lock);
  {
    // Look up file offset to unlock.
    gpointer dummy, field_offset_as_gpointer;
    gboolean lookup_result
      = g_hash_table_lookup_extended (self->field_offsets, kags, &dummy,
				      &field_offset_as_gpointer);
    g_assert (lookup_result);
    guint field_offset = GPOINTER_TO_UINT (field_offset_as_gpointer);
    
    // Look up intraprocess lock.
    ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
    g_assert (field_lock != NULL);

    // Release file lock.
    release_file_lock (self->table_fd, F_RDLCK,
		       field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
		       TABLE_FILE_MAX_VALUE_LENGTH + 1);

    // Unlock intraprocess lock.
    ec_lock_reader_unlock (field_lock);

    // Lookup field reader count.
    gpointer field_reader_lock_count_as_gpointer;
    lookup_result
      = g_hash_table_lookup_extended (self->field_reader_lock_counts,
				      kags, &dummy,
				      &field_reader_lock_count_as_gpointer);
    g_assert (lookup_result);
    guint field_reader_lock_count
      = GPOINTER_TO_UINT (field_reader_lock_count_as_gpointer);
    g_assert (field_reader_lock_count > 0);

    // Decrement field reader count.
    field_reader_lock_count--;

    // Store new field reader count.  This table now takes ownership
    // of kags.
    g_hash_table_insert (self->field_reader_lock_counts, kags,
			 GUINT_TO_POINTER (field_reader_lock_count));
  }
  ec_lock_writer_unlock (self->ds_lock);
}

gboolean
table_file_field_writer_lock (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_FIELD_WRITER_LOCK, key, NULL);
    return results.return_value;
  }

  g_assert (have_table_reader_lock (self) || have_table_writer_lock (self));

  GString *kags = g_string_new (key); // Key as GString instance.

  ec_lock_writer_lock (self->ds_lock);

  // The entry may be present in our hash of locks, but some other
  // process may have removed the actual entry.  This possibility is
  // covered below.
  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  
  // We also might not have an entry in our hash of locks for this
  // process, but some other process might have added the field.
  off_t field_offset = table_field_offset (self, key);
  
  // If the field doesn't exist...
  if ( field_offset == (off_t) -1 ) {
    // Do some incremental lock table maintenance.  If we have a
    // field_lock for the entry but we didn't find the entry in the
    // file, some other process must have removed it, so we remove the
    // entries from our field lock table, field reader lock count
    // table, and field value cache table.  IMPROVEME: this is
    // probably cut and paste from the field_reader_lock method.
    if ( field_lock != NULL ) {
      gboolean remove_result = g_hash_table_remove (self->field_locks, kags);
      g_assert (remove_result);
      gpointer dummy;		// Dummy value we don't really use.
      gpointer value;		// Pointer to hash table value.
      gboolean lock_count_entry_found
	= g_hash_table_lookup_extended (self->field_reader_lock_counts,
					kags, &dummy, &value);
      g_assert (lock_count_entry_found);
      // Consistency check (better not have any reader locks counted here).
      g_assert (GPOINTER_TO_UINT (value) == 0);
      remove_result
	= g_hash_table_remove (self->field_reader_lock_counts, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_values, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_offsets, kags);
      g_assert (remove_result == TRUE);
    }
  
    ec_lock_writer_unlock (self->ds_lock);
  
    my_g_string_free (kags);

    return FALSE;
  }

  // If we didn't have a field lock in our table of locks for this
  // process...
  if ( field_lock == NULL ) {
    field_lock = ec_lock_new ();
    ec_lock_writer_lock (field_lock);
    // Note that here the hash table takes over ownership of kags (and
    // field_lock).
    g_hash_table_insert (self->field_locks, kags, field_lock);
    ec_lock_writer_unlock (self->ds_lock);
    {
      obtain_file_lock (self->table_fd, F_WRLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
    }
    ec_lock_writer_lock (self->ds_lock);
    g_hash_table_insert (self->field_reader_lock_counts,
			 g_string_new (kags->str), GUINT_TO_POINTER (0));
    GString *field_value = lookup_field_value (self, field_offset);
    g_hash_table_insert (self->field_values, g_string_new (kags->str),
			 field_value);
    g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			 GUINT_TO_POINTER (((guint) field_offset)));
  }
  
  else {
    // Consistency check.  We better not already have the lock.
    g_assert (!have_field_writer_lock (self, key));
    
    ec_lock_writer_unlock (self->ds_lock);
    {
      ec_lock_writer_lock (field_lock);
      
      obtain_file_lock (self->table_fd, F_WRLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
    }
    ec_lock_writer_lock (self->ds_lock);
    
    // Consistency check.  Our field reader lock count better be zero.
    guint field_reader_lock_count
      = GPOINTER_TO_UINT (g_hash_table_lookup (self->field_reader_lock_counts,
					       kags));
    g_assert (field_reader_lock_count == 0);

    // The field value may have changed, so we read it again.
    GString *field_value = lookup_field_value (self, field_offset);
    
    // Note that here this hash table takes ownership of kags and
    // field_value.
    g_hash_table_replace (self->field_values, kags, field_value);

    // Update our cached notion of the field offset.
    g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			 GUINT_TO_POINTER (((guint) field_offset)));
  }

  ec_lock_writer_unlock (self->ds_lock);

  return TRUE;
}

gboolean
table_file_field_writer_trylock (TableFile *self, const char *key,
				 gboolean *field_exists)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    method_results_type results
      = command_slave (self, TABLE_OPERATION_FIELD_WRITER_TRYLOCK, key, NULL);
    if ( field_exists != NULL ) {
      *field_exists = results.field_exists;
    }
    return results.return_value;
  }

  g_assert (have_table_reader_lock (self) || have_table_writer_lock (self));

  GString *kags = g_string_new (key); // Key as GString instance.

  // We can lock ds_lock, since it isn't ever held when we are doing
  // anything that might actually block (i.e. it isn't held when we
  // are trying to obtain any table or field locks).
  ec_lock_writer_lock (self->ds_lock);
  
  // The entry may be present in our hash of locks, but some other
  // process may have removed the actual entry.  This possibility is
  // covered below.
  ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
  
  // We also might not have an entry in our hash of locks for this
  // process, but some other process might have added the field.
  off_t field_offset = table_field_offset (self, key);
  
  // If the field doesn't exist...
  if ( field_offset == (off_t) -1 ) {
    // Do some incremental lock table maintenance.  If we have a
    // field_lock for the enntry but we didn't find the entry in the
    // file, some other process must have removed it, so we remove the
    // entries from our field lock table, field reader lock count
    // table, and field value cache table.  IMPROVEME: this is cut and
    // paste from the field_writer_lock method at least.
    if ( field_lock != NULL ) {
      gboolean remove_result = g_hash_table_remove (self->field_locks, kags);
      g_assert (remove_result);
      gpointer dummy;		// Dummy value we don't really use.
      gpointer value;		// Pointer to hash table value.
      gboolean lock_count_entry_found
	= g_hash_table_lookup_extended (self->field_reader_lock_counts,
					kags, &dummy, &value);
      g_assert (lock_count_entry_found);
      // Consistency check (better not have any reader locks counted here).
      g_assert (GPOINTER_TO_UINT (value) == 0);
      remove_result
	= g_hash_table_remove (self->field_reader_lock_counts, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_values, kags);
      g_assert (remove_result == TRUE);
      remove_result = g_hash_table_remove (self->field_offsets, kags);
      g_assert (remove_result == TRUE);
    }
  
    my_g_string_free (kags);

    if ( field_exists != NULL ) {
      *field_exists = FALSE;
    }
    ec_lock_writer_unlock (self->ds_lock);
    return FALSE;
  }

  if ( field_exists != NULL ) {
    *field_exists = TRUE;
  }

  // If we didn't have a field lock in our table of locks for this
  // process...
  if ( field_lock == NULL ) {
    field_lock = ec_lock_new ();
    gboolean try_lock_result = ec_lock_writer_trylock (field_lock);
    g_assert (try_lock_result);   // Field just created: better be lockable.
    gboolean file_lock_result
      = try_file_lock (self->table_fd, F_WRLCK,
		       field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
		       TABLE_FILE_MAX_VALUE_LENGTH + 1);
    if ( file_lock_result ) {
      // Note that here the hash table takes over ownership of kags
      // (and field_lock).
      g_hash_table_insert (self->field_locks, kags, field_lock);
      g_hash_table_insert (self->field_reader_lock_counts,
			   g_string_new (kags->str), GUINT_TO_POINTER (0));
      GString *field_value = lookup_field_value (self, field_offset);
      g_hash_table_insert (self->field_values, g_string_new (kags->str),
			   field_value);
      g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			   GUINT_TO_POINTER (((guint) field_offset)));
      ec_lock_writer_unlock (self->ds_lock);
      return TRUE;
    }
    else {
      // Since we didn't get the file lock, we don't want to maintain
      // the thread lock either.
      ec_lock_writer_unlock (field_lock);
      // In theory it should be possible to save our ECLock for this
      // field, since we know the field exists.  But I think its safer
      // just to pretend nothing has happened if we don't get the
      // lock, thus avoiding introducing a possibly new combination of
      // cached state and lock values.
      ec_lock_unref (field_lock);
      ec_lock_writer_unlock (self->ds_lock);
      return FALSE;
    }
  }
  
  else {
    // Consistency check.  We better not already have the lock.
    g_assert (!have_field_writer_lock (self, key));
    
    gboolean lock_result = ec_lock_writer_trylock (field_lock);

    if ( lock_result ) {
      gboolean file_lock_result
	= try_file_lock (self->table_fd, F_WRLCK,
			 field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			 TABLE_FILE_MAX_VALUE_LENGTH + 1);
      if ( file_lock_result ) {
	// Consistency check.  Our field reader lock count better be
	// zero.
	guint field_reader_lock_count
	  = GPOINTER_TO_UINT (g_hash_table_lookup
			      (self->field_reader_lock_counts,
			       kags));
	g_assert (field_reader_lock_count == 0);

	// Update our cached notion of the field offset.
	g_hash_table_insert (self->field_offsets, g_string_new (kags->str),
			     GUINT_TO_POINTER (((guint) field_offset)));

	// The field value may have changed, so we read it again.
	GString *field_value = lookup_field_value (self, field_offset);
	
	// Note that here this hash table takes ownership of kags and
	// field_value.
	g_hash_table_replace (self->field_values, kags, field_value);

	ec_lock_writer_unlock (self->ds_lock);
	return TRUE;
      }
      else {
	ec_lock_writer_unlock (field_lock);
	ec_lock_writer_unlock (self->ds_lock);
	return FALSE;
      }
    }
    else {
      ec_lock_writer_unlock (self->ds_lock);
      return FALSE;
    }
  }
}

void
table_file_set_field_value (TableFile *self, const char *key,
			    const char *value)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_SET_FIELD_VALUE, key, value);
    return;
  }

  if ( !have_field_writer_lock (self, key) ) {
    GString *pts = pid_thread_string ();
    g_error ("don't have_field_writer_lock() of %s in %s\n", key, pts->str);
    my_g_string_free (pts);
  }
  g_assert (have_field_writer_lock (self, key));

  GString *kags = g_string_new (key); // Key as GString instance.

  ec_lock_writer_lock (self->ds_lock);
  {
    // Get field offset in actual table file.
    gpointer field_offset_as_gpointer
      = g_hash_table_lookup (self->field_offsets, kags);
    off_t field_offset = (off_t) (GPOINTER_TO_UINT (field_offset_as_gpointer));

    // Write field in actual table.
    off_t desired_offset = field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1;
    off_t lseek_result = lseek (self->table_fd, desired_offset, SEEK_SET);
    g_assert (lseek_result != (off_t) -1);
    g_assert (lseek_result == desired_offset);
    // If the user has set the field value to the empty string, we
    // have to be sure to write our nonfree_symbol to ensure that the
    // field doesn't get mistaken for one marked unused.
    if ( value[0] == '\0' ) {
      ssize_t bytes_written
	= write (self->table_fd, nonfree_symbol, nonfree_symbol_length);
      g_assert (bytes_written == nonfree_symbol_length);
    }
    else {
      size_t value_string_length
	= strnlen (value, TABLE_FILE_MAX_VALUE_LENGTH + 1);
      g_assert (value_string_length <= TABLE_FILE_MAX_VALUE_LENGTH);
      size_t bytes_to_write = value_string_length + 1;
      ssize_t bytes_written = write (self->table_fd, value, bytes_to_write);
      g_assert (bytes_written == bytes_to_write);
    }

    g_hash_table_insert (self->field_values, kags, g_string_new (value));
  }
  ec_lock_writer_unlock (self->ds_lock);
}

void
table_file_field_writer_unlock (TableFile *self, const char *key)
{
  do_piles_of_crazy_stuff (self);

  if ( have_slave (self) ) {
    command_slave (self, TABLE_OPERATION_FIELD_WRITER_UNLOCK, key, NULL);
    return;
  }

  g_assert (have_field_writer_lock (self, key));

  GString *kags = g_string_new (key);  // Key as GString.

  ec_lock_writer_lock (self->ds_lock);
  {
    // Consistency check: field reader lock count should be zero.
    gpointer dummy, field_reader_lock_count_as_gpointer;
    gboolean lookup_result
      = g_hash_table_lookup_extended (self->field_reader_lock_counts,
				      kags, &dummy,
				      &field_reader_lock_count_as_gpointer);
    g_assert (lookup_result);
    guint field_reader_lock_count
      = GPOINTER_TO_UINT (field_reader_lock_count_as_gpointer);
    g_assert (field_reader_lock_count == 0);

    // Look up file offset to unlock.
    gpointer field_offset_as_gpointer;
    lookup_result
      = g_hash_table_lookup_extended (self->field_offsets, kags,
				      &dummy, &field_offset_as_gpointer);
    g_assert (lookup_result);
    guint field_offset = GPOINTER_TO_UINT (field_offset_as_gpointer);
    
    // Look up intraprocess lock.
    ECLock *field_lock = g_hash_table_lookup (self->field_locks, kags);
    g_assert (field_lock != NULL);

    // Release file lock.
    release_file_lock (self->table_fd, F_WRLCK,
		       field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
		       TABLE_FILE_MAX_VALUE_LENGTH + 1);

    // Unlock intraprocess lock.
    ec_lock_writer_unlock (field_lock);
  }
  ec_lock_writer_unlock (self->ds_lock);

  my_g_string_free (kags);
}

void
table_file_dump_locks (TableFile *self)
{
  do_piles_of_crazy_stuff (self);

  // Needs conditional forwarding to lock slave.
  g_assert_not_reached ();

  ec_lock_writer_lock (self->ds_lock);
  {
    g_message ("Table locks held:\n");
    if ( have_table_reader_lock (self) ) {
      g_message ("Table reader\n");
    }
    if ( have_table_writer_lock (self) ) {
      g_message ("Table writer\n");
    }
    GPtrArray *keys = my_g_hash_table_keys (self->field_locks);
    GPtrArray *values = my_g_hash_table_values (self->field_locks);
    g_assert (keys->len == values->len);
    guint ii;
    for ( ii = 0 ; ii < keys->len ; ii++ ) {
      GString *cln = g_ptr_array_index (keys, ii); // Current lock name.
      ECLock *cl = g_ptr_array_index (values, ii); // Current lock.
      if ( ec_lock_have_reader_lock (cl) ) {
	g_message ("%s: reader\n", cln->str);
      }
      if ( ec_lock_have_writer_lock (cl) ) {
	g_message ("%s: writer\n", cln->str);
      }
    }
    my_g_ptr_array_really_free (values, NULL);
    my_g_ptr_array_really_free (keys, NULL);
  }
  ec_lock_writer_unlock (self->ds_lock);
}

TableFile *
table_file_ref (TableFile *self)
{
  self->reference_count++;
  
  return self;
}

void
table_file_unref (TableFile *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    g_hash_table_destroy (self->thread_table);
    g_hash_table_destroy (self->field_offsets);
    g_hash_table_destroy (self->field_values);
    g_hash_table_destroy (self->field_reader_lock_counts);
    g_hash_table_destroy (self->field_locks);
    ec_lock_unref (self->table_lock);
    int return_code = close (self->table_fd);
    g_assert (return_code == 0);
    ec_lock_unref (self->ds_lock);
    g_string_free (self->path, TRUE);
    g_free (self);
  }
}
