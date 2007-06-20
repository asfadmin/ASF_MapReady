// This class is really a simple sort of interthread/interprocces
// synchronization and communication mechanism.
//
// Actually, at the moment its a ghastly kludge to contend with the
// fact that POSIX fcntl based file locking doesn't appear to work as
// one could wish when threads are involved.  Linux at least thinks it
// detects deadlock in fcntl when two threads in a single process are
// involved in a blocking situation with another process which would
// only be a block cycle if those threads are viewed as a single graph
// node.  To work around this issue and still support the interface
// described below we have to do some strange things.  The code as we
// would like it to be is in files
// table_file_if_fcntl_worked_righ.[hc].
//
// Specifically, it is an interface to a file consisting of key-value
// pairs, with full support for explicit interthread and interprocess
// read/write lock synchronization.  The locking is explicit so that
// clients may use the locks to synchronize other operations besides
// those on the table itself (perhaps with the table as a
// representation of the state of those other operations).
//
// It is possible to create multiple instances refering to the same
// file in different, possibly unrelated processes, and they will
// correctly communicate and synchronize with each other using POSIX
// advisory file locking.  It is NOT possible to create multiple
// instances referring to the same file in different threads in the
// same process (but see below).
//
// A given instance will also work correctly in processes created with
// fork, or new threads spawned with g_thread_new or the like.
// However, any locks maintained by the instance will be mangled in
// the forked child or spawned thread.  In other words, besides the
// arrangement described in the previous paragraph, the supported
// paradigm for use of this class in a multithread or multiprocess
// program is:
// 
//   1. Create instance with table_file_new method.
//   2. Fork processes or spawn threads.
//   3. Call other methods on the created instance.
//
// Only one instance may exist in a given process for a given path.
// It is an error to try to create an additional instance for a given
// path.  Note that due to operating system cleverness (or lack
// thereof, depending on point of view) it may be possible to defeat
// this class' trapping of this error condition with extra slashes or
// the like.  So don't do that.
//
// It should also be noted that opening and closing the file an
// instance of this class is created with via any interface other than
// the one defined by this class will break that instance (because all
// POSIX advisory locks are dropped whenever any descriptor is
// closed).
//
// Finally, since fcntl file locking currently gets confused when
// multiple threads in the same process try to lock files, the truth
// is that threads actually create slave process to do their locking
// for them.  Probably all this means for correct clients is that they
// might get a SIGCHLD they weren't expecting when threads exit.  By
// default this signal is ignored.  For clients that get themselves in
// deadlock situations, its possible that these spawned processes
// might persist in a deadlocked state and cause confusing problems on
// subsequent runs even if the original erroneas parent process is
// killed.
//
// The following fundamental operations are supported:
//
//   1. Determine if a specific field exists.
//   2. Get catalog of all field keys.
//   3. Add field.
//   4. Remove field.
//   5. Reset field value.
//   6. Get field value.
//
// This class supports reader/writer locks both for the entire table
// (hereafter referred to as the table level lock) and for individual
// fields.  Each fundamental operation requires that certain locks be
// held by the process attempting it:
//
//   Operation                   Required Locks
//   ------------------------------------------------
//   Determine field existence   table reader (or table writer)
//   Get catalog of fields       table reader (or table writer)               
//   Add field                   table writer     
//   Remove field                table writer and field writer
//   Reset field value           field writer 
//   Get field value             field reader (or field writer)
//
// Obtaining a reader or writer lock for an individual field requires
// that at least a table reader lock be held while the field lock is
// obtained, but the table lock can safely be released once this field
// lock is obtained.  A table writer lock will also suffice, of
// course.  In other words, the individual field lock methods:
//
//   1. table_file_field_reader_lock
//   2. table_file_field_writer_lock
//   3. table_file_field_writer_trylock
// 
// all require the caller to hold a table reader or writer lock
// obtained with either the table_file_table_reader_lock method or the
// table_file_table_writer_lock method, respectively.
//
// No table level lock needs to be held to release a field level lock.
//
// Finally, it is important to understand that when a new field is
// added, it comes into existence with a field writer lock granted to
// the thread that created it, and when a field is removed, the field
// lock the removing thread is required to hold is automaticly
// released.  A thread may therefore safely create a field, release
// the table level lock required to create that field, do other work,
// and then modify the value of the field, all without fear of
// interference by other threads.
//
// Thus, from a given thread's perspective, the sequence of calls for
// adding, setting, reading, and finally removing a field with key k
// from table t in a way that monopolizes locks only minimally might
// look like this:
//
//   table_writer_lock (t);
//   add_field (t, k, "value_text");
//   table_writer_unlock (t);
//   (Do other initialization work...)
//   set_field_value (t, k, "new_value_text");
//   field_writer_unlock (t, k);
//   (Time passes, probably other use the field value...)
//   table_reader_lock (t);
//   field_reader_lock (t, k);
//   table_reader_unlock (t);
//   GString *fetched_value = get_field_value (t, k);
//   field_reader_unlock (t, k);
//   (Other stuff gets done using the new field value...)
//   table_writer_lock (t);
//   field_writer_lock (t, k);
//   remove_field (t, k);
//   table_writer_unlock (t);
//
// Where the method class name prefix has been omitted for clarity.
//
// This class is not particularly efficient.  It would be horrendous
// for a multiprocess database backend or the like.  The point is to
// have a simple, correct interface to a catalog shared between
// multiple processes and/or threads, with explicit lock operations on
// the file fields in case clients want to use the file locks to
// synchronize relatively expensive operations other than the actual
// field reads and writes themselves.

#ifndef TABLE_FILE_H
#define TABLE_FILE_H

#include <glib.h>

#include "ec_lock.h"

// The path data member is read-only public, the other members are
// private.
typedef struct {
  GString *path;		// Path of table file.
  ECLock *ds_lock;		// Lock for this data structure itself.
  gboolean unused;		// True iff self hasn't been locked.
  // File descriptor of open table file.  Each process using a given
  // table file maintains its own open file descriptor of that file:
  // all the methods detect a change in PID and open a new descriptor
  // if necessary.
  int table_fd;		
  // The pid in which the table_fd data member originated.  If this is
  // not equal to getpid (), the methods open a new descriptor.
  pid_t pid;
  // Hash of known (pid, thread) tupple GString instances (as returned
  // by pid_thread_string() to structures detailing how those threads
  // perform lock operations (directly or through a slave).
  GHashTable *thread_table;
  ECLock *table_lock;           // Intraprocess lock for entire table.
  // Hash of GString key representations to individual ECLock
  // instances (for intraprocess locking).
  GHashTable *field_locks;
  // Hash of GString key representations to guint reader lock counts.
  GHashTable *field_reader_lock_counts;
  // Hash of GString key representations to field value GStrings,
  // which are correct and useful only when multiple readers lock a
  // field at the same time.
  GHashTable *field_values;
  // Hash of field offset guint values, which are guaranteed to stay
  // correct only while somebody continually holds at least a reader
  // lock on the field.  Note that we can only easily be sure of
  // continuous locking between the time we (we the inhabitants of a
  // single thread) acquire and release a field reader or writer lock.
  GHashTable *field_offsets;
  int reference_count;
} TableFile;

// Maximum key and value string lengths, not including trailing NUL bytes.
#define TABLE_FILE_MAX_KEY_LENGTH 2000
#define TABLE_FILE_MAX_VALUE_LENGTH 2000

// We have a smallish maximum number of fields only because the IPC
// that forwards requiest to lock slaves is lazy code that does
// everything in one write.  IMPROVEME: this could be fixed by making
// the protocol that threads use to communicate with their proxy
// slaves slightly more sophisticated.
#define MAX_FIELD_COUNT 200

// Create a new table instance.  The file argument is the path name of
// a table file which already exists or is to be created.  It is an
// error if an instance already exists for file in the current
// process.
TableFile *
table_file_new (const char *file);

// Obtain a reader lock on the table contents.  Holding this lock
// ensure that individual fields will not be added or removed by other
// threads or processes, but doesn't guarantee that individual field
// contents will not be changed (for that a field reader lock must be
// obtained).
void
table_file_table_reader_lock (TableFile *self);

// Return TRUE iff field with key key exists, or false otherwise.
// Requires a table reader or table writer lock.
gboolean
table_file_field_exists (TableFile *self, const char *key);

// Return a new GPtrArray of new GString instances set to the current
// key values of the table.  Requires a table reader or table writer
// lock.
GPtrArray *
table_file_catalog (TableFile *self);

// Release a table reader lock.
void
table_file_table_reader_unlock (TableFile *self);

// Obtain a writer lock on the table contents.  Holding this lock
// allows fields to be added.  If the current thread also holds a
// writer lock on an individual field, that field can be removed.
void
table_file_table_writer_lock (TableFile *self);

// Add a new field.  Requires a table writer lock to be held.  Returns
// true if entry addition succeeded, or false if addition failed
// because the entry already existed.
gboolean
table_file_add_field (TableFile *self, const char *key, const char *value);

// Remove a field.  Requires both a table writer lock and a field writer
// lock for the field to be removed.
void
table_file_remove_field (TableFile *self, const char *key);

// Release a table writer lock.
void
table_file_table_writer_unlock (TableFile *self);

// Obtain a reader lock on the value associated with key.  Requires a
// table reader or writer lock to be held.  Returns true after
// blocking until a lock is obtained, or false if the key of interest
// didn't exist.
gboolean
table_file_field_reader_lock (TableFile *self, const char *key);

// Return a new GString instance set to the value associated with key.
// A reader lock must be held on the field before this method is
// invoked.  The result is returned in the value argument.
GString *
table_file_get_field_value (TableFile *self, const char *key);

// Release a field reader lock.
void
table_file_field_reader_unlock (TableFile *self, const char *key);

// Obtain a writer lock on the value associated with key.  Requires a
// table reader or writer lock to be held.  Returns true after
// blocking until a lock is obtained, or false if the key of interest
// didn't exist.
gboolean
table_file_field_writer_lock (TableFile *self, const char *key);

// Attempt to obtain a writer lock on the value associated with key.
// Requires a table reader or writer lock to be held.  Returns true if
// a lock is immediately obtained, or false if the lock attempt failed
// because someone else holds the lock or because the field doesn't
// exist.  If the field_exists argumentis non-NULL, the value it
// points to is set to TRUE if the field at least exists, or FALSE
// otherwise.  Note that attempting to lock a lock the caller itself
// holds is still an error.
gboolean
table_file_field_writer_trylock (TableFile *self, const char *key,
				 gboolean *field_exists);

// Set value of field corresponding to key.  A writer lock must be
// held on the field before this method is invoked.
void
table_file_set_field_value (TableFile *self, const char *key,
			    const char *value);

// Release a field writer lock.
void
table_file_field_writer_unlock (TableFile *self, const char *key);

// Dump a snapshot of the table and field locks currently held by the
// caller using g_message on the TableFile domain.  Note that no
// information can be given on fields for which we don't hold locks,
// since we can't even be sure they will exist continually during this
// routine.
void
table_file_dump_locks (TableFile *self);

// Increment reference count of self, returning self for convenience.
TableFile *
table_file_ref (TableFile *self);

// Decrement reference count of self, freeing self if reference count
// falls to zero.  It is an error to cause an instance to be freed if
// any locks of any sort are still held on it by any processes or
// threads.  The logic behind this is that we don't want to do any
// automatic cleanup, and we don't want to leave dangling locks.
// Since all file and thread locks will be reclaimed anyway when a
// process exits, it may sometimes be most expeditious not to free an
// instance of this class.
void
table_file_unref (TableFile *self);

#endif // TABLE_FILE_H
