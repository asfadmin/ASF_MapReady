// Implementation of the interface described in ec_lock.h.

#include "ec_lock.h"
#include "utilities.h"

#ifdef G_LOG_DOMAIN
#  undef G_LOG_DOMAIN
#  define G_LOG_DOMAIN "ECLock"
#endif

ECLock *
ec_lock_new (void)
{
  // Make sure we have the GThread system up and running.
  if ( !g_thread_supported () ) {
    g_thread_init (NULL);
  }

  ECLock *self = g_new (ECLock, 1);

  self->state_mutex = g_mutex_new ();
  self->state_cv = g_cond_new ();
  self->state = EC_LOCK_STATE_UNLOCKED;

  self->lock = g_new (GStaticRWLock, 1);
  g_static_rw_lock_init (self->lock);

  self->trace_lock = g_new (GStaticMutex, 1);
  g_static_mutex_init (self->trace_lock);
  self->readers = g_hash_table_new (g_direct_hash, g_direct_equal);
  self->writer = NULL;
  self->trace_on = FALSE;
  self->traced_threads = g_hash_table_new (g_direct_hash, g_direct_equal);
  self->thread_names 
    = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, 
			     (GDestroyNotify) my_g_string_free);

  self->ref_count_mutex = g_mutex_new ();
  self->reference_count = 1;
  self->free_lock = g_new (GStaticRWLock, 1);
  g_static_rw_lock_init (self->free_lock);
  self->destroyed = FALSE;

  return self;
}

// If tracing is enabled globally or for the current thread, print a
// message describing a transaction.  The message string consists of a
// sring identifying the current thread as well as possible (using the
// name if available in the thread_names hash, and the thread object
// address in any case), followed by a space, followed by the
// transaction message, followed by a newline.
static void
maybe_trace (ECLock *self, const char *transaction)
{
  g_static_mutex_lock (self->trace_lock);
  {
    if ( self->trace_on 
	 || my_g_hash_table_entry_exists (self->traced_threads,
					  g_thread_self ()) ) {
      GString *message = g_string_new ("Thread ");
      GString *thread_name = g_hash_table_lookup (self->thread_names,
						  g_thread_self ());
      if ( thread_name != NULL ) {
	g_string_append_printf (message, "named '%s' ", thread_name->str);
      }
      g_string_append_printf (message, "at location %p ", g_thread_self ());
      g_string_append_printf (message, "%s\n", transaction);
      
      g_debug (message->str);

      my_g_string_free (message);
    }
  }
  g_static_mutex_unlock (self->trace_lock);
}  

// This is a good working example of how to do a variadic macro, which
// is one way to call another variadic macro (there are probably other
// ways).  Turns out MAYBE_TRACE didn't need to be variadic in this
// case so at the moment this isn't used.
/*
#define MAYBE_TRACE(INSTANCE, FORMAT, ...) \
  do { \
    g_static_mutex_lock (INSTANCE->trace_lock); \
    { \
      if ( INSTANCE->trace_on ) { \
        g_debug (FORMAT, __VA_ARGS__); \
      } \
      else if ( my_g_hash_table_entry_exists (INSTANCE->traced_threads, \
                                              g_thread_self ()) ) { \
        g_debug (FORMAT, __VA_ARGS__); \
      } \
    } \
    g_static_mutex_unlock (INSTANCE->trace_lock); \
  } \
  while ( 0 )
*/

// This is the core functionality of the ec_lock_have_reader_lock
// method, but it doesn't hold any protective locks on the self
// itself, so is only suitable for use inside method implementations
// that hold this lock anyway.  In fact, it is the only such routine
// that is safe to use in these situations, since the locks involved
// are not recursive.  So if an assertion is tripped which involves
// this routine, it implies that the interface method
// ec_lock_have_reader_lock would have returned the wrong result if
// made immediately before the call was made (i.e. an invalid method
// call with respect to reader lock status has been made).
//
// Note also that calling this routine when only a reader lock on
// ds_lock is held assumes that multiple readers can concurrently
// interrogate a GHashTable -- something which I'm not sure is
// gauranteed by glib (but it seems to work).
static gboolean
have_reader_lock_unprotected (ECLock *self)
{
  return my_g_hash_table_entry_exists (self->readers, g_thread_self ());
}

void
ec_lock_reader_lock (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_assert (!have_reader_lock_unprotected (self));

    g_mutex_lock (self->state_mutex);
    while ( self->state == EC_LOCK_STATE_WRITER_LOCKED ) {
      g_cond_wait (self->state_cv, self->state_mutex);
    }
    {    
      // To make sure we haven't screwed up this class, we operate an
      // actual reader/writer lock.  It had better work...
      gboolean lock_result = g_static_rw_lock_reader_trylock (self->lock);
      g_assert (lock_result == TRUE);

      g_assert (self->writer == NULL);
      
      g_hash_table_insert (self->readers, g_thread_self (), NULL);

      maybe_trace (self, "obtained reader lock");

      self->state = EC_LOCK_STATE_READER_LOCKED;

      g_cond_broadcast (self->state_cv);
    }
    g_mutex_unlock (self->state_mutex);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_reader_unlock (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_assert (have_reader_lock_unprotected (self));

    g_mutex_lock (self->state_mutex);
    {    
      // To make sure we haven't screwed up this class, we operate an
      // actual reader/writer lock.
      g_static_rw_lock_reader_unlock (self->lock);

      g_hash_table_remove (self->readers, g_thread_self ());

      maybe_trace (self, "released reader lock");

      if ( g_hash_table_size (self->readers) == 0 ) {
	self->state = EC_LOCK_STATE_UNLOCKED;
	g_cond_broadcast (self->state_cv);
      }
    }
    g_mutex_unlock (self->state_mutex);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

// See the comment associated with the have_reader_lock_unprotected
// routine.
static gboolean
have_writer_lock_unprotected (ECLock *self)
{
  return (self->writer == g_thread_self ());
}

void
ec_lock_writer_lock (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    if ( have_writer_lock_unprotected (self) ) {
      GString *pts = pid_thread_string ();
      g_error ("%s: erroneous attempt to writer lock a lock already held",
	       pts->str);
    }
    g_assert (!have_writer_lock_unprotected (self));
    
    g_mutex_lock (self->state_mutex);
    while ( self->state != EC_LOCK_STATE_UNLOCKED ) {
      g_cond_wait (self->state_cv, self->state_mutex);
    }
    {    
      // To make sure we haven't screwed up this class, we operate an
      // actual reader/writer lock.  It had better work...
      gboolean lock_result = g_static_rw_lock_writer_trylock (self->lock);
      g_assert (lock_result == TRUE);

      // Also, the table of readers had better be empty.
      g_assert (g_hash_table_size (self->readers) == 0);
      // And there better not be any other writer either.
      g_assert (self->writer == NULL);

      self->writer = g_thread_self ();

      maybe_trace (self, "obtained writer lock");

      self->state = EC_LOCK_STATE_WRITER_LOCKED;

      // No need to broadcast the new state here; other threads are
      // only interested when a writer lock is released!
    }
    g_mutex_unlock (self->state_mutex);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

gboolean
ec_lock_writer_trylock (ECLock *self)
{
  gboolean result;

  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_assert (!have_writer_lock_unprotected (self));

    g_mutex_lock (self->state_mutex);
    if ( self->state != EC_LOCK_STATE_UNLOCKED ) {
      // Consistency check: we better not be able to successfully
      // trylock the independent lock either.
      gboolean lock_result = g_static_rw_lock_writer_trylock (self->lock);
      g_assert (lock_result == FALSE);

      maybe_trace (self,
		   "failed to obtain writer lock in writer_trylock method");

      result = FALSE;
    }
    else {
      // To make sure we haven't screwed up this class, we operate an
      // actual reader/writer lock.  It had better work...
      gboolean lock_result = g_static_rw_lock_writer_trylock (self->lock);
      g_assert (lock_result == TRUE);

      // Also, the table of readers had better be empty.
      g_assert (g_hash_table_size (self->readers) == 0);
      // And there better not be any other writer either.
      g_assert (self->writer == NULL);
      
      self->writer = g_thread_self ();
      
      maybe_trace (self, "obtained writer lock in writer_trylock method");
      
      self->state = EC_LOCK_STATE_WRITER_LOCKED;
      
      // No need to broadcast the new state here; other threads are
      // only interested when a writer lock is released!
 
      result = TRUE;
    }
    g_mutex_unlock (self->state_mutex);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);

  return result;
}

void
ec_lock_writer_unlock (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_assert (have_writer_lock_unprotected (self));
    
    g_mutex_lock (self->state_mutex);
    {    
      // To help make sure we haven't screwed up this class, we
      // operate an actual reader/writer lock.
      g_static_rw_lock_writer_unlock (self->lock);

      // There obviously shouldn't be any readers at this point.
      g_assert (g_hash_table_size (self->readers) == 0);

      self->writer = NULL;

      maybe_trace (self, "released writer lock");

      self->state = EC_LOCK_STATE_UNLOCKED;
      g_cond_broadcast (self->state_cv);
    }
    g_mutex_unlock (self->state_mutex);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

gboolean
ec_lock_have_reader_lock (ECLock *self)
{
  gboolean result;

  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    result = have_reader_lock_unprotected (self);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);

  return result;
}

gboolean
ec_lock_have_writer_lock (ECLock *self)
{
  gboolean result;

  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    result = have_writer_lock_unprotected (self);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);

  return result;
}

void
ec_lock_name_thread (ECLock *self, const char *name)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_static_mutex_lock (self->trace_lock);
    {
      g_assert (g_hash_table_size (self->thread_names)
		< EC_LOCK_MAX_THREAD_NAMES);
      g_hash_table_insert (self->thread_names, g_thread_self (),
			   g_string_new (name));
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_dename_thread (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_static_mutex_lock (self->trace_lock);
    {
      g_hash_table_remove (self->thread_names, g_thread_self ());
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_enable_tracing (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_static_mutex_lock (self->trace_lock);
    {
      self->trace_on = TRUE;
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_disable_tracing (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_static_mutex_lock (self->trace_lock);
    {
      self->trace_on = FALSE;
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_add_to_trace_list (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);

    g_static_mutex_lock (self->trace_lock);
    {
      g_hash_table_insert (self->traced_threads, g_thread_self (), NULL);
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

void
ec_lock_remove_from_trace_list (ECLock *self)
{
  g_static_rw_lock_reader_lock (self->free_lock);
  {
    g_assert (!self->destroyed);
    
    g_static_mutex_lock (self->trace_lock);
    {
      g_hash_table_remove (self->traced_threads, g_thread_self ());
    }
    g_static_mutex_unlock (self->trace_lock);
  }
  g_static_rw_lock_reader_unlock (self->free_lock);
}

ECLock *
ec_lock_ref (ECLock *self)
{
  g_mutex_lock (self->ref_count_mutex);
  {
    g_assert (self->reference_count != 0);
    self->reference_count++;
  }
  g_mutex_unlock (self->ref_count_mutex);

  return self;
}

void
ec_lock_unref (ECLock *self)
{
  g_mutex_lock (self->ref_count_mutex);
  {
    self->reference_count--;
    g_assert (self->reference_count >= 0);
  }
  g_mutex_unlock (self->ref_count_mutex);

  // Note that if ec_lock_unref is racing an ec_lock_ref that might
  // save an instance, this is a bug which we don't help with (I
  // haven't even thought about whether there might be a way).
  if ( self->reference_count == 0 ) {
    ec_lock_free (self);
  }
}

void
ec_lock_free (ECLock *self)
{
  // We are now going to destroy this instance, so nobody else better
  // be using it.
  gboolean lock_status = g_static_rw_lock_writer_trylock (self->free_lock);
  // As we said in the interface, its an error to free an instance in
  // use by any other method of this class (waiting in lock, being
  // queried for status, whatever -- all those uses are deemed naughty
  // race conditions).
  g_assert (lock_status == TRUE);
  {
    self->destroyed = TRUE;
  }
  g_static_rw_lock_writer_unlock (self->free_lock);

  g_mutex_lock (self->state_mutex);
  {
    g_assert (self->state == EC_LOCK_STATE_UNLOCKED);
    g_assert (g_hash_table_size (self->readers) == 0);
    g_assert (self->writer == NULL);
  }
  g_mutex_unlock (self->state_mutex);

  g_mutex_free (self->state_mutex);

  g_cond_free (self->state_cv);

  g_static_rw_lock_free (self->lock);
  g_free (self->lock);

  g_static_mutex_free (self->trace_lock);
  g_free (self->trace_lock);

  g_hash_table_destroy (self->readers);

  g_hash_table_destroy (self->traced_threads);

  g_hash_table_destroy (self->thread_names);

  g_static_rw_lock_free (self->free_lock);
  // FIIXME: I'm not certain now where I got the idea that we need
  // g_free as well as g_static_rw_lock_free, though I have a hazy
  // idea I may have looked at the source once upon a time to
  // determine this.
  g_free (self->free_lock);

  g_free (self);
}

void
ec_lock_free_regardless (ECLock *self)
{
  g_mutex_free (self->state_mutex);

  g_cond_free (self->state_cv);

  g_static_rw_lock_free (self->lock);
  g_free (self->lock);

  g_static_mutex_free (self->trace_lock);
  g_free (self->trace_lock);

  g_hash_table_destroy (self->readers);

  g_hash_table_destroy (self->traced_threads);

  g_hash_table_destroy (self->thread_names);

  g_static_rw_lock_free (self->free_lock);
  // FIIXME: I'm not certain now where I got the idea that we need
  // g_free as well as g_static_rw_lock_free, though I have a hazy
  // idea I may have looked at the source once upon a time to
  // determine this.
  g_free (self->free_lock);

  g_free (self);
}
