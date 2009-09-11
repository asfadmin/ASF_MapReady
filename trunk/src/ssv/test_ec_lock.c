// Test code to exercise the ec_lock class.  The exercise isn't
// particularly complete but it should help :)

#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>

#include "ec_lock.h"
#include "utilities.h"

///////////////////////////////////////////////////////////////////////////////
//
// Thread Routines
//
// To test a lock, at some point we have to do some concurrent stuff!
//
///////////////////////////////////////////////////////////////////////////////

// Threads return pointers, so here we have some actual values for the
// return pointers to point to.
static const guint thread_success = 0, thread_failure = 1;

// This integer simulates the corruptible data that the lock we are
// testing is protecting.  The data is in an uncorrupt
// (nontransitionary) state when this value is zero, otherwise the
// data is in a transitionary state which must not exist outside an
// atomic transaction.
static int corruptible_data = 0;

// A writer operation which must be performed atomically (i.e. a
// working writer lock must be held while this operation is
// performed).
static void
do_writer_test_atom (gdouble max_writer_atom_time)
{
  g_assert (corruptible_data == 0);
  corruptible_data = 1;
  my_sleep (my_random (max_writer_atom_time));
  corruptible_data = 0;
  g_assert (corruptible_data == 0);
}

// A reader operation which must be performed atomically (i.e. a
// working reader lock mustbe held while htis operation is performed).
static void
do_reader_test_atom (gdouble max_reader_atom_time)
{
  g_assert (corruptible_data == 0);
  my_sleep (my_random (max_reader_atom_time));
  g_assert (corruptible_data == 0);
}

// Approximate length of time threads should run.  After this amount
// of time has passed, no new operations will be begun (it may take
// longer than this for all pending operations to complete).
#define THREAD_TEST_RUN_TIME 600.0

// Thread functions themselves pretty much just do random locking and
// corresponding need-to-be-atomic operations for a short amount of
// time.
static gpointer
test_thread_func (gpointer data)
{
  ECLock *lock = (ECLock *) data;

  GTimer *timer = g_timer_new ();

  // This fraction of the time when we ask for a lock we will ask for
  // a writer lock, the rest of the time we'll ask for a reader lock.
  const gdouble writer_lock_fraction = 0.2;

  // This fraction of the time when we ask for a writer lock we will
  // do so with the writer_trylock() method, the rest of the time we
  // will use the writer_lock() method.
  const gdouble writer_trylock_fraction = 0.7;

  // The maximum times that simulated atomic read or write operations
  // (which is what the lock is supposed to protect) can take to
  // complete.  The minimum is zero.
  const gdouble max_reader_atom_time = 0.03;
  const gdouble max_writer_atom_time = 0.09;

  // The maximum time that a thread will be idle between attempted
  // lock or unlock operations..
  const gdouble max_idle_time = 0.1;

  // The lock stat as the current thread knows it (truth may be
  // different).
  ec_lock_state_t lock_state = EC_LOCK_STATE_UNLOCKED;

  gulong junk;			// For useless parameter of g_timer_elapsed().

  while (    g_timer_elapsed (timer, &junk) < THREAD_TEST_RUN_TIME 
	  || lock_state != EC_LOCK_STATE_UNLOCKED ) {
    switch ( lock_state ) {
    case EC_LOCK_STATE_UNLOCKED:
      if ( random_fraction () < writer_lock_fraction ) {
	if ( random_fraction () < writer_trylock_fraction ) {
	  gboolean lock_result = ec_lock_writer_trylock (lock);
	  if ( lock_result ) {
	    lock_state = EC_LOCK_STATE_WRITER_LOCKED;
	  }
	}
	else {
	  ec_lock_writer_lock (lock);
	  lock_state = EC_LOCK_STATE_WRITER_LOCKED;
	  do_writer_test_atom (max_writer_atom_time);
	}
      }
      else {
	ec_lock_reader_lock (lock);
	lock_state = EC_LOCK_STATE_READER_LOCKED;
	do_reader_test_atom (max_reader_atom_time);
      }
      break;
    case EC_LOCK_STATE_READER_LOCKED:
      ec_lock_reader_unlock (lock);
      lock_state = EC_LOCK_STATE_UNLOCKED;
      break;
    case EC_LOCK_STATE_WRITER_LOCKED:
      ec_lock_writer_unlock (lock);
      lock_state = EC_LOCK_STATE_UNLOCKED;
      break;
    default:
      g_assert_not_reached ();
      break;
    }
  }

  my_sleep (my_random (max_idle_time));

  g_timer_destroy (timer);

  return (int *) &thread_success;
}

///////////////////////////////////////////////////////////////////////////////

int
main (void)
{
  ECLock *lock = ec_lock_new ();

  g_print ("Testing general tracing and locking functionality...\n");
  ec_lock_enable_tracing (lock);
  {
    ec_lock_reader_lock (lock);
    g_assert (ec_lock_have_reader_lock (lock));
    ec_lock_reader_unlock (lock);

    ec_lock_writer_lock (lock);
    ec_lock_have_writer_lock (lock);
    ec_lock_writer_unlock (lock);

  }
  ec_lock_disable_tracing (lock);

  g_print ("Testing disabling of tracing...\n");
  g_print ("Expect nothing after the colon except a period:");
  ec_lock_reader_lock (lock);
  ec_lock_reader_unlock (lock);
  ec_lock_writer_lock (lock);
  ec_lock_writer_unlock (lock);
  g_print (".\n");

  g_print ("Testing tracing of an individual thread via the trace list..\n");
  ec_lock_add_to_trace_list (lock);
  {
    ec_lock_reader_lock (lock);
    ec_lock_reader_unlock (lock);

    ec_lock_writer_lock (lock);
    ec_lock_writer_unlock (lock);    

    ec_lock_writer_trylock (lock);
    ec_lock_writer_unlock (lock);

    // Note that we can't test writer_trylock calls on a locked lock
    // here, since this call is still not allowed when we are the ones
    // that hold the lock.
  }
  ec_lock_remove_from_trace_list (lock);

  g_print ("Testing thread naming functionality...\n");
  ec_lock_add_to_trace_list (lock);
  {
    ec_lock_name_thread (lock, "foo");

    ec_lock_reader_lock (lock);
    ec_lock_reader_unlock (lock);
    ec_lock_writer_lock (lock);
    ec_lock_writer_unlock (lock);        
    ec_lock_writer_trylock (lock);
    ec_lock_writer_unlock (lock);

    ec_lock_dename_thread (lock);

    g_print ("Thread is now denamed, the next two lock/unlock cycles \n"
	     "should only show thread object addresses...\n");
    ec_lock_reader_lock (lock);
    ec_lock_reader_unlock (lock);
    ec_lock_writer_lock (lock);
    ec_lock_writer_unlock (lock);            
    ec_lock_writer_trylock (lock);
    ec_lock_writer_unlock (lock);
  }
  ec_lock_remove_from_trace_list (lock);

  g_print ("Testing disabling of tracing of an individual thread...\n");
  g_print ("Expect nothing after the colon except a period:");
  ec_lock_reader_lock (lock);
  ec_lock_reader_unlock (lock);
  ec_lock_writer_lock (lock);
  ec_lock_writer_unlock (lock);
  ec_lock_writer_trylock (lock);
  ec_lock_writer_unlock (lock);
  g_print (".\n");

  g_print ("\n");

  g_print ("Performing expected failure testing...\n");

  g_print ("\n");

  g_print ("Testing double read lock (failure expected)...\n");
  pid_t pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    // Child.
    ec_lock_reader_lock (lock);
    ec_lock_reader_lock (lock);
  }
  int child_status;
  pid_t waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");

  g_print ("Testing double write lock (failure expected)...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_writer_lock (lock);
    ec_lock_writer_lock (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");

  g_print ("Testing double write lock with writer_trylock (failure "
	   "expected)...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_writer_lock (lock);
    ec_lock_writer_trylock (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");
  

  g_print ("Testing reader unlock of unlocked lock (failure expected)...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_reader_unlock (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");

  g_print ("Testing writer unlock of unlocked lock (failure expected)...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_writer_unlock (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");

  g_print ("Testing free of reader locked lock...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_reader_lock (lock);
    ec_lock_free (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n"); 

  g_print ("Testing free of writer locked lock...\n");
  pid = fork ();
  g_assert (pid != -1);
  if ( pid == 0 ) {
    ec_lock_writer_lock (lock);
    ec_lock_free (lock);
  }
  waitpid_result = waitpid (pid, &child_status, 0);
  g_assert (waitpid_result == pid);
  g_assert (WIFSIGNALED (child_status) && WTERMSIG (child_status) == SIGABRT);
  g_print ("\n");
  g_print ("Ok, got expected failure.\n");
  g_print ("\n");
  
  // Now we launch a few threads that take random locks and perform
  // correspondign need-to-be-atomic operations for random amounts of
  // time for a few seconds, in order to exercise the internal
  // consistency checks in the class, watch for non-atomic operations,
  // and trigger any lockups that might be floating around.
  g_print ("Testing actual operation of lock with multiple threads.\n");
  g_print ("Test is currently set to run for %lf seconds...\n",
	   THREAD_TEST_RUN_TIME);
  const guint test_thread_count = 5;
  GPtrArray *test_threads = g_ptr_array_new ();
  guint ii;
  for ( ii = 0 ; ii < test_thread_count ; ii++ ) {
    GThread *tmp = g_thread_create (test_thread_func, lock, TRUE, NULL);
    g_assert (tmp != NULL);
    g_ptr_array_add (test_threads, tmp);
  }
  for ( ii = 0 ; ii < test_thread_count ; ii++ ) {
    gpointer return_pointer 
      = g_thread_join (g_ptr_array_index (test_threads, ii));
    g_assert (*((int *) return_pointer) == thread_success);
  }

  g_print ("Freeing ECLock instance...\n");
  ec_lock_free (lock);

  g_print ("Basic testing of ECLock class complete.\n");

  exit (EXIT_SUCCESS);
}
