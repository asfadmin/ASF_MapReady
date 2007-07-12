// Implementation of interface described in table_file.h.
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
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
// only-one-instance-per-file rule, and to allow GMutex
// reinitialization on fork.  FIXME: this is actually slightly broken
// in that it prevents the forked processes from opening new instances
// associated with files open in their parents, which so far as I know
// causes no trouble.
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
  // FIXME: this is a test fiddle
  lock_spec.l_pid = -1;
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

// FIXME: review all functions one more time for ds->lock safety, in
// definintion and use.

// Return true iff we have a table write lock on the table.
static gboolean
have_table_writer_lock (TableFile *self)
{
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

  // FIXME: not needed?
  //  self->thread_id = g_thread_self ();
  
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

// First, if a process change has occurred, insist that self hasn't
// yet been used, as described in the interface.  Next open a new
// descriptor for the current process.  Finally, whether or not a
// process change has occurred, mark self as used.
static void
maybe_reopen_table_and_ensure_and_definately_unset_unused (TableFile *self)
{
  ec_lock_writer_lock (self->ds_lock);
  {
    pid_t current_pid = getpid ();
    // FIXME: not needed?
    //    GThread *current_thread = g_thread_self ();
    if ( self->pid != current_pid ) {
      g_assert (self->unused);
      int return_code = close (self->table_fd);
      g_assert (return_code == 0);
      self->table_fd
	= open (self->path->str, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
      g_assert (self->table_fd != -1);      
      self->pid = current_pid;
      //      self->thread_id = current_thread;
    }
    // FIXME: at the moment, the multithread stuff doesn't work yet.
    //    else {
    //      if ( self->thread_id != current_thread ) { 
    //	g_assert (self->unused);
    //	self->thread_id = current_thread;
    //      }
    //  }
    self->unused = FALSE;
  }
  ec_lock_writer_unlock (self->ds_lock);
}

void
table_file_table_reader_lock (TableFile *self)
{
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  ec_lock_reader_lock (self->table_lock);

  // Lock title line.
  obtain_file_lock (self->table_fd, F_RDLCK, 0, title_line->len + 1);
}

// Returne the number of entries in the table, INCLUDING ENTRIES WHICH
// HAVE BEEN MAKED FREE WITH OUR SPECIAL FREE SYMBOL.  Requires a
// (writer) lock on self->ds_lock to be held.
static off_t
table_entry_count (TableFile *self)
{
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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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

GPtrArray *
table_file_catalog (TableFile *self)
{
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  // We had better already hold a reader lock.
  g_assert (ec_lock_have_reader_lock (self->table_lock));
  
  release_file_lock (self->table_fd, F_RDLCK, 0, title_line->len + 1);
  
  ec_lock_reader_unlock (self->table_lock);
}

void
table_file_table_writer_lock (TableFile *self)
{
  trmsg ("Doing table_writer_lock");
  //  trmsg ("table_writer_lock check 1");

  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  //  trmsg ("table_writer_lock check 2");

  ec_lock_writer_lock (self->table_lock);

  //  trmsg ("table_writer_lock check 3");

  //  trmsg ("pre obtain_file_lock in %s", __func__);
  obtain_file_lock (self->table_fd, F_WRLCK, 0, title_line->len + 1);
  //  trmsg ("post obtain_file_lock in %s", __func__);

  //  trmsg ("table_writer_lock check 4");
  trmsg ("Finished table_writer_lock");
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
  //  trmsg ("add_field check 1");

  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  g_assert (have_table_writer_lock (self));
  g_assert (strnlen (key, TABLE_FILE_MAX_KEY_LENGTH + 1)
	    <= TABLE_FILE_MAX_KEY_LENGTH);
  g_assert (strnlen (key, TABLE_FILE_MAX_VALUE_LENGTH + 1)
	    <= TABLE_FILE_MAX_VALUE_LENGTH);

  ec_lock_writer_lock (self->ds_lock);

  //  trmsg ("add_field check 2");

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

  //  trmsg ("add_field check 3");

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

  //  trmsg ("add_field check 4");

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

  //  trmsg ("add_field check 5");

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

  //  trmsg ("add_field check 6");

  return TRUE;
}

// Return true iff the calling thread holds a write lock on field with
// key key.  This routine acquires a writer lock on self->ds_lock if
// the caller doesn't already hold one, so it won't necessarily ever
// return if there is a deadlock condition going on somewhere else.
static gboolean
have_field_writer_lock (TableFile *self, const char *key)
{
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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  // We had better already hold a writer lock.
  g_assert (have_table_writer_lock (self));

  release_file_lock (self->table_fd, F_WRLCK, 0, title_line->len + 1);

  ec_lock_writer_unlock (self->table_lock);

  trmsg ("Finished table_writer_unlock");
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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  if ( !have_field_reader_lock (self, key) ) {
    GString *pts = pid_thread_string ();
    g_error ("don't have_field_reader_lock() of %s in %s\n", key, pts->str);
    my_g_string_free (pts);
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
  trmsg ("Doing field_writer_lock of %s", key);

  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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

    trmsg ("About to return FALSE from %s", __func__);
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
      //      trmsg ("pre obtain_file_lock");
      obtain_file_lock (self->table_fd, F_WRLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
      //      trmsg ("post obtain_file_lock");
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
      //      trmsg ("pre obtain_file_lock 2: offset: %lld",
      //	     (long long int) field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1);
      
      obtain_file_lock (self->table_fd, F_WRLCK,
			field_offset + TABLE_FILE_MAX_KEY_LENGTH + 1,
			TABLE_FILE_MAX_VALUE_LENGTH + 1);
      //      trmsg ("post obtain_file_lock 2");
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

  trmsg ("Finished field_writer_lock of %s", key);

  return TRUE;
}

gboolean
table_file_field_writer_trylock (TableFile *self, const char *key,
				 gboolean *field_exists)
{
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

  if ( !have_field_writer_lock (self, key) ) {
    GString *pts = pid_thread_string ();
    g_error ("don't have_field_writer_lock() of %s in %s\n", key, pts->str);
    my_g_string_free (pts);
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

  trmsg ("Finished field_writer_unlock of %s", key);
}

void
table_file_dump_locks (TableFile *self)
{
  maybe_reopen_table_and_ensure_and_definately_unset_unused (self);

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
