// A error checking reader/writer lock.  It dies with a useful message
// on obvious errors, and tracing can be enable to help debug more
// subtle problems.  A bit slower of course, but it knows if you've
// been bad or good (so be good for goodness sake).

#ifndef EC_LOCK_H
#define EC_LOCK_H

#include <glib.h>

// Helper type representing current state of the lock.
typedef enum {
  EC_LOCK_STATE_UNLOCKED,	// No threads hold any locks.
  EC_LOCK_STATE_READER_LOCKED,  // One or more threads hold reader locks.
  EC_LOCK_STATE_WRITER_LOCKED   // One thread holds a writer lock.
} ec_lock_state_t;

// Data members are all private.  Use the interface methods.
typedef struct {
  GMutex *state_mutex;		// Lock for state condition.
  GCond *state_cv;		// State variable for condition signaling.
  ec_lock_state_t state;        // Reader locked, writer locked, or unlocked.
  GStaticRWLock *lock;		// Redundant lock (defensive programming).
  GStaticMutex *trace_lock;	// Lock for trace status of self.
  GHashTable *readers;		// Hash keyed by reader threads.
  GThread *writer;		// Thread holding writer lock, or NULL.
  gboolean trace_on;		// True iff general tracing is enabled.
  GHashTable *traced_threads;	// Hash keyed by specific threads to trace.
  GHashTable *thread_names;	// GString names this instance has for threads.
  GMutex *ref_count_mutex;	// Mutex for reference counting.
  gint reference_count;	        // Reference count for the instance.
  GStaticRWLock *free_lock;	// Used to make sure destruction is atomic.
  gboolean destroyed;		// Flag true iff atomically destroyed.
} ECLock;

// Create a new unlocked instance with tracing off.
ECLock *
ec_lock_new (void);

void
ec_lock_reader_lock (ECLock *self);

void
ec_lock_reader_unlock (ECLock *self);

void
ec_lock_writer_lock (ECLock *self);

// Non-blocking: immediately returns true if a writer lock is
// obtained, or FALSE otherwise.  Note that calling writer_trylock is
// still forbidden when the caller itself holds the lock.
gboolean
ec_lock_writer_trylock (ECLock *self);

void
ec_lock_writer_unlock (ECLock *self);

// Return true iff caller holds reader rights on self.
gboolean
ec_lock_have_reader_lock (ECLock *self);

// Return true iff caller holds writer rights on self.
gboolean
ec_lock_have_writer_lock (ECLock *self);

// Assign a name by which the current thread is known to self.  If
// provided, this name will be used in trace messages in addition to
// the harder-to-read thread object address.  For performance reasons,
// a maximum of EC_LOCK_MAX_THREAD_NAMES may be used at one time.  Use
// the dename_thread method to remove the name of the thread that is
// about to go away if you create lots of threads.  If threads are not
// denamed before they finish, there is a possibility that a confusing
// address to name mapping might persist in the internal hash table of
// the instance and cause confusion.
void
ec_lock_name_thread (ECLock *self, const char *name);

#define EC_LOCK_MAX_THREAD_NAMES 1000

// Remove the current thread from the list of names maintained by instance.
void
ec_lock_dename_thread (ECLock *self);

// Enable general tracing of all lock activity to G_LOG_DOMAIN
// "ECLock" with G_LOG_LEVEL_DEBUG.
void
ec_lock_enable_tracing (ECLock *self);

// Disable general tracing.
void
ec_lock_disable_tracing (ECLock *self);

// Add current thread to a list of threads for which activity is to be
// traced to G_LOG_DOMAIN "ECLock" with G_LOG_LEVEL_DEBUG.  Any calls
// into self that can be proven to begin after this method returns
// will be fully traced.
void
ec_lock_add_to_trace_list (ECLock *self);

// Remove current thread from trace list (see add_to_trace_list method).
void
ec_lock_remove_from_trace_list (ECLock *self);

// Increment reference count of self.  Returns self as a convenience.
ECLock *
ec_lock_ref (ECLock *self);

// Decrement reference count of self, freeing self before returning if
// reference count falls to zero. It is an error to free a lock locked
// by and readers or a writer, or in use by any other method of the
// instance.
void
ec_lock_unref (ECLock *self);

// Free self, regardless of reference count.  It is an error to free a
// lock locked by and readers or a writer, or in use by any other
// method of the instance.
void
ec_lock_free (ECLock *self);

// Free self, regardless of reference count or lock status.  This
// method is probably only useful in functions registered with
// pthread_atfork or the like, where we want to destroy and
// reinitialize a mutex that is no longer valid (because mutexes
// aren't gauranteed to survive fork() calls).
void
ec_lock_free_regardless (ECLock *self);

#endif // EC_LOCK_H
