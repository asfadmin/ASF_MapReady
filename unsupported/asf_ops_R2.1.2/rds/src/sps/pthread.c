/*=============================================================================
 |  @(#)pthread.c	2.10 96/10/31 11:25:22
 |
 |  POSIX Thread Emulation Package for Silicon Graphics.
 |  Copyright (C) Jet Propulsion Laboratory
 |  Alaska SAR Facility (ASF) Project.
 *============================================================================*/

#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timers.h>
#include <ulocks.h>
#include <signal.h>
#include <errno.h>
#include "pthread_wrapper.h"

#ifdef PTHREAD_DEBUG
#include <syslog.h>
#endif
static char sccsid_pthread_c[] = "@(#)pthread.c	2.10 96/10/31 11:25:22";

/*  Flag to tell whether we (this library) have initialized yet or not  */

usptr_t *pthread_arena = NULL;
static   sigset_t kill_signal;

int pthread_detach( pthread_t *thread )
{
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_init - initialize POSIX thread emulation
 |
 |  SYPNOSIS
 |	void *pthread_init(void);
 |
 |  DESCRIPTION
 |	This function initializes the POSIX thread emulation library.
 |
 *---------------------------------------------------------------------------*/
static
void *pthread_init(void)
{
    char  arena_name[64];
    pid_t pid;

    sprintf (arena_name, "/tmp/pthread_arena.%d", pid = getpid());
    if ((pthread_arena = usinit(arena_name)) == NULL) {
	perror("pthread_init");
	exit(-1);
    }
    unlink(arena_name);

    sigemptyset(&kill_signal);
    sigaddset(&kill_signal, SIGHUP);

    return ((void *) pthread_arena);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      getusPtrHandle - return a handle to the emulated pthread arena.
 |
 |  SYPNOSIS
 |	usptr_t *getusPtrHandle();
 |
 |  DESCRIPTION
 |	This function returns a handle to the emulated pthread arena..
 |
 *---------------------------------------------------------------------------*/
/* cws supplied */

usptr_t *getusPtrHandle()
{
    if (pthread_arena == NULL && pthread_init() == NULL)
	return(NULL);

  return(pthread_arena);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_exit - terminates the calling thread.
 |
 |  SYPNOSIS
 |	void pthread_exit(pthread_addr_t status);
 |
 |  DESCRIPTION
 |	This routine terminates the calling thread and makes a status value 
 |	available to any thread that calls pthread_join and specifies the 
 |	terminating thread.
 |
 |	An implicit call to pthread_exit is issued when a thread returns from
 |	the start routine that was used to create it. The function's return 
 |	value serves as the thread's exit status. The process exits when the 
 |	last running thread calls pthread_exit.
 |
 *---------------------------------------------------------------------------*/
void pthread_exit(pthread_addr_t status)
{
    exit((int) status);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_create - creates a thread object and thread.
 |
 |  SYPNOSIS
 |	int pthread_create (pthread_t *thread, 
 |			    pthread_attr_t attr ,
 |			    pthread_startroutine start_routine ,
 |			    pthread_addr_t arg);
 |  DESCRIPTION
 |  This routine creates a thread object and a thread. A thread is a single,
 |  sequential flow of control within a program. It is the active execution of
 |  a designated routine, including any nested routine invocations.  A thread
 |  object defines and controls the executing thread.
 |
 |  Calling this routine sets into motion the following actions:
 |
 |  +  An internal thread object is created to describe the thread.
 |
 |  +  The associated executable thread is created with attributes specified
 |     by the attr parameter (or with default attributes if
 |
 |     PTHREAD_ATTR_DEFAULT is specified).
 |
 |  +  The thread parameter receives the new thread.
 |
 |  +  The start_routine function is called when this routine successfully
 |     completes.
 |
 |  The thread is created in the ready state and therefore might immediately
 |  begin executing the function specified by the start_routine parameter.
 |  The newly created thread will begin running before pthread_create completes
 |  if the new thread follows the SCHED_RR or SCHED_FIFO scheduling policy or 
 |  has a priority higher than the creating thread, or both.  Otherwise, the 
 |  new thread begins running at its turn, which with sufficient processors 
 |  might also be before pthread_create returns.
 |
 |  The start_routine is passed a copy of the arg parameter. The value of the
 |  arg parameter is unspecified.
 |
 |  The thread object exists until the pthread_detach routine is called or the
 |  thread terminates, whichever occurs last.
 |
 |  The synchronization between the caller of pthread_create and the newly
 |  created thread is through the use of the pthread_join routine (or any other
 |  mutexes or condition variables they agree to use).
 |
 |  A thread terminates when one of the following events occurs:
 |
 |  +  The thread returns from its start routine.
 |
 |  +  The thread exits (within a routine) as the result of calling the
 |     pthread_exit routine.
 |
 |  +  The thread is canceled.
 |
 |  The following actions are performed when a thread terminates:
 |
 |  +  If the thread terminates by returning from its start routine or cal-
 |     ling pthread_exit, the return value is copied into the thread object.
 |     If the start routine returns normally and the start routine is a pro-
 |     cedure that does not return a value, then the result obtained by
 |     pthread_join is unpredictable. If the thread has been canceled, a
 |     return value of -1 is copied into the thread object. The return value
 |     can be retrieved by other threads by calling the pthread_join routine.
 |
 |  +  Each per-thread context destructor is removed from the list of des-
 |     tructors for this thread and then is called. This step destroys all
 |     the per-thread context associated with the current thread.
 |
 |  +  Each cleanup handler that has been declared by pthread_cleanup_push
 |     and not yet removed by pthread_cleanup_pop is called. The most
 |     recently pushed handler is called first.
 |
 |  +  A flag is set in the thread object indicating that the thread has ter-
 |     minated. This flag must be set in order for callers of pthread_join to
 |     return from the call.
 |
 |  +  A broadcast is made so that all threads currently waiting in a call to
 |     pthread_join can return from the call.
 |
 |  +  The thread object is marked to indicate that it is no longer needed by
 |     the thread itself. A check is made to determine if the thread object
 |     is no longer needed by other threads, that is, if pthread_detach has
 |     been called. If that routine has been called, then the thread object
 |     is deallocated.
 |
 |  If an error condition occurs, no thread is created, the contents of thread
 |  are undefined, and this routine returns -1 and sets errno to the
 |  corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_create (pthread_t *thread, const pthread_attr_t attr,
		    pthread_startroutine_t start_routine, pthread_addr_t arg)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_create\n", getpid());
#endif
    if (pthread_arena == NULL && pthread_init() == NULL)
	return -1;

    return ((*thread = sproc((void(*)(void*))start_routine, PR_SALL, arg)) == -1
	? -1 : 0);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_join - cause the calling thread to wait for the termination 
 |		       of a specified thread
 |  SYPNOSIS
 |	int pthread_join (pthread_t thread, pthread_addr_t *status);
 |
 |  DESCRIPTION
 |	This routine causes the calling thread to wait for the termination of
 |	a specified thread. A call to this routine returns after the specified
 |	thread has terminated.
 |
 |	Any number of threads can call this routine. All threads are awakened
 |	when the specified thread terminates.
 |
 |	If the current thread calls this routine, a deadlock results.
 |
 |	The results of this routine are unpredictable if the value for thread
 |	refers to a thread object that no longer exists.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_join (pthread_t thread, pthread_addr_t *status)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_join\n", getpid());
#endif
    return (waitpid((pid_t)thread, (int *)status, 0) == -1 ? -1 : 0);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_yield - notifies the scheduler that the current thread is 
 |			willing to release its processor to other threads
 |			of the same or higher priority
 |  SYPNOSIS
 |	void pthread_yield (void);
 |
 |  DESCRIPTION
 |	This service notifies the scheduler that the current thread is willing
 |	to release its processor to other threads of the same priority. 
 |	(A thread releases its processor to a thread of a higher priority 
 |	without calling this service.)
 |
 |	If the current thread's scheduling policy (as specified in a call to
 |	pthread_attr_setsched or pthread_setscheduler) is SCHED_FIFO, SCHED_RR,
 |	or SCHED_OTHER, this service yields the processor to other threads of
 |	the same priority. If no threads of the same priority are ready to 
 |	execute, the thread continues.
 |
 |	This service allows knowledge of the details of an application to be 
 |	used to increase fairness. It increases fairness of access to the 
 |	processor by removing the current thread from the processor. It also 
 |	increases fairness of access to shared resources by removing the 
 |	current thread from the pro- cessor as soon as it is finished with the 
 |	resource.
 |
 |	Call this service when a thread is executing code that denies access to
 |	other threads on a uniprocessor if the scheduling policy is SCHED_FIFO.
 |
 |	Use pthread_yield carefully because misuse causes unnecessary context
 |	switching, which increases overhead without increasing fairness. For 
 |	example, it is counter-productive for a thread to yield while it has 
 |	a needed resource locked.
 |
 *---------------------------------------------------------------------------*/
void pthread_yield (void)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_yield\n", getpid());
#endif
    if (pthread_arena == NULL && pthread_init() == NULL)
	return;

    sginap(0);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_self - obtains the identifier of the current thread.
 |
 |  SYPNOSIS
 |	pthread_t pthread_self(void);
 |
 |  DESCRIPTION
 |	This routine allows a thread to obtain its own identifier.  Use this 
 |	identifier in calls to pthread_setprio and pthread_setscheduler.
 |
 |	This value becomes meaningless when the thread object is deleted - 
 |	that is, when the thread has terminated its execution and 
 |	pthread_detach has been called.
 |
 |	The function returns the identifier of the calling thread to pthread_t.
 |
 *---------------------------------------------------------------------------*/
pthread_t pthread_self (void)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_self\n", getpid());
#endif
    if (pthread_arena == NULL && pthread_init() == NULL)
	return -1;

    return ((pthread_t) getpid());
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_mutex_init - creates a mutex.
 |
 |  SYPNOSIS
 |	int pthread_mutex_init (pthread_mutex_t *mutex, 
 |				pthread_mutexattr_t attr)
 |  DESCRIPTION
 |	This routine creates a mutex and initializes it to the unlocked state.
 |
 |	The created mutex is not automatically deallocated because it is 
 |	considered shared among multiple threads if the thread that called 
 |	this routine terminates.
 |
 |	If an error condition occurs, this routine returns -1, the mutex is 
 |	not initialized, and the contents of mutex are undefined. This routine
 |	sets errno to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_mutex_init (pthread_mutex_t *mutex, pthread_mutexattr_t attr)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_mutex_init\n", getpid());
#endif

    if (pthread_arena == NULL && pthread_init() == NULL)
	return -1;

    /*  switch to usnewpollsema if it turns out you need to support
     *  pthread_mutex_trylock.
     */
    return (*mutex = usnewsema(pthread_arena, 1)) ? 0 : -1;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_mutex_destroy - deletes a mutex.
 |
 |  SYPNOSIS
 |	int pthread_mutex_destroy (pthread_mutex_t *mutex)
 |
 |  DESCRIPTION
 |	This routine deletes a mutex and should be called when a mutex object 
 |	will no longer be referenced. The effect of calling this routine is to 
 |	reclaim storage for the mutex object.
 |
 |	It is illegal to delete a mutex that has a current owner (in other 
 |	words, is locked).
 |
 |	The results of this routine are unpredictable if the mutex object 
 |	specified in the mutex parameter does not currently exist.
 |
 |	If an error condition occurs, this routine returns 1 and sets errno to 
 |	the corresponding error value. 
 |
 *---------------------------------------------------------------------------*/
int pthread_mutex_destroy (pthread_mutex_t *mutex)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_mutex_destroy\n", getpid());
#endif
    usfreesema(*mutex, pthread_arena);
    return 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_mutex_lock - Locks an unlocked mutex. If the mutex is locked,
 |			     this routine causes the thread to wait for the 
 |			     mutex to become available.
 |  SYPNOSIS
 |	int pthread_mutex_lock (pthread_mutex_t *mutex)
 |
 |  DESCRIPTION
 |	This routine locks a mutex. If the mutex is locked when a thread calls
 |	this routine, the thread waits for the mutex to become available.
 |
 |	The thread that has locked a mutex becomes its current owner and 
 |	remains the owner until the same thread has unlocked it. This routine 
 |	returns with the mutex in the locked state and with the current thread
 |	as the mutex's current owner.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno 
 |	to the corresponding error value. 
 |
 *---------------------------------------------------------------------------*/
int pthread_mutex_lock (pthread_mutex_t *mutex)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_mutex_lock\n", getpid());
#endif
    return (uspsema(*mutex) == -1 ? -1 : 0);	
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_mutex_lock - unlocks a mutex.
 |
 |  SYPNOSIS
 |	int pthread_mutex_unlock (pthread_mutex_t *mutex)
 |
 |  DESCRIPTION
 |	This routine unlocks a mutex. If no threads are waiting for the mutex,
 |	the mutex unlocks with no current owner. If one or more threads are 
 |	waiting to lock the specified mutex, this routine causes one thread to
 |	return from its call to pthread_mutex_lock.
 |
 |	The results of calling this routine are unpredictable if the mutex 
 |	specified in mutex is unlocked. The results of calling this routine 
 |	are also unpredictable if the mutex specified in mutex is currently 
 |	owned by a thread other than the calling thread.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno 
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_mutex_unlock (pthread_mutex_t *mutex)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_mutex_unlock\n", getpid());
#endif
    return (usvsema(*mutex) == -1 ? -1 : 0);	
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cond_init - creates a condition variable.
 |
 |  SYPNOSIS
 |	int pthread_cond_init (pthread_cond_t *cond, pthread_condattr_t attr);
 |
 |  DESCRIPTION
 |	This routine creates and initializes a condition variable. A condition
 |	variable is a synchronization object used in conjunction with a mutex.
 |	A mutex controls access to shared data; a condition variable allows 
 |	threads to wait for that data to enter a defined state. The state is 
 |	defined by a Boolean expression called a predicate.
 |
 |	A condition variable is signaled or broadcast to indicate that a 
 |	predicate might have become true. The broadcast operation indicates 
 |	that all waiting threads should resume and reevaluate the predicate. 
 |	The signal operation is used when any one waiting thread can continue.
 |
 |	If a thread that holds a mutex determines that the shared data is not
 |	in the correct state for it to proceed (the associated predicate is 
 |	not true), it waits on a condition variable associated with the 
 |	desired state. Waiting on the condition variable automatically releases
 |	the mutex so that other threads can modify or examine the shared data.
 |	When a thread modifies the state of the shared data so that a predicate
 |	might be true, it signals or broadcasts on the appropriate condition 
 |	variable so that threads waiting for that predicate can continue.
 |
 |	It is important that all threads waiting on a particular condition 
 |	variable at any time hold the same mutex. If they do not, the behavior
 |	of the wait operation is unpredictable (an implementation can use the
 |	mutex to control internal access to the condition variable object). 
 |	However, it is legal for a client to store condition variables and 
 |	mutexes and later reuse them in different combinations. The client must
 |	ensure that no threads use the condition variable with the old mutex.
 |	At any time, an arbitrary number of condition variables can be 
 |	associated with a single mutex, each representing a different predicate
 |	of the shared data protected by that mutex.
 |
 |	Condition variables are not owned by a particular thread. Any 
 |	associated storage is not automatically deallocated when the creating 
 |	thread terminates.
 |
 |	If an error condition occurs, this routine returns -1, the condition 
 |	variable is not initialized, and the contents of cond are undefined. 
 |	This routine sets errno to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cond_init (pthread_cond_t *cond, pthread_condattr_t attr)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_cond_init\n", getpid());
#endif
    if (pthread_arena == NULL && pthread_init() == NULL)
	return -1;

    return ((*cond = usnewsema(pthread_arena, 0)) ? 0 : -1);
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cond_destroy - deletes a condition variable.
 |
 |  SYPNOSIS
 |	int pthread_cond_destroy (pthread_cond_t *cond);
 |
 |  DESCRIPTION
 |	This routine deletes a condition variable. Call this routine when a 
 |	condition variable will no longer be referenced. The effect of calling
 |	this routine is to give permission to reclaim storage for the condition
 |	variable.
 |
 | 	The results of this routine are unpredictable if the condition variable
 |	specified in cond does not exist.
 |
 |	The results of this routine are also unpredictable if there are threads
 |	waiting for the specified condition variable to be signaled or broadcast
 |	when it is deleted.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno 
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cond_destroy (pthread_cond_t *cond)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_cond_destroy\n", getpid());
#endif
    usfreesema(*cond, pthread_arena);
    return 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cond_wait - causes a thread to wait for a condition variable 
 |			    to be signaled or broadcast.
 |  SYPNOSIS
 |	int pthread_cond_wait (pthread_cond_t *cond, pthread_mutex_t *mutex);
 |
 |  DESCRIPTION
 |	This routine causes a thread to wait for a condition variable to be
 |	signaled or broadcast. Each condition corresponds to one or more 
 |	predicates based on shared data. The calling thread waits for the data
 |	to reach a particular state (for the predicate to become true).
 |
 |	Call this routine after you have locked the mutex specified in mutex.
 |	The results of this routine are unpredictable if this routine is called
 |	without first locking the mutex.
 |
 |	This routine atomically releases the mutex and causes the calling 
 |	thread to wait on the condition. If the wait is satisfied as a result
 |	of some thread calling pthread_cond_signal or pthread_cond_broadcast,
 |	the mutex is reacquired and the routine returns.
 |
 |	A thread that changes the state of storage protected by the mutex in
 |	such a way that a predicate associated with a condition variable might
 |	now be true must call either pthread_cond_signal or 
 |	pthread_cond_broadcast for that condition variable. If neither call is
 |	made, any thread waiting on the condition variable continues to wait.
 |
 |	This routine might (with low probability) return when the condition
 |	variable has not been signaled or broadcast. When a spurious wakeup
 |	occurs, the mutex is reacquired before the routine returns.
 |	(To handle this type of situation, enclose this routine in a loop that
 |	checks the predicate.)
 |
 |	If an error condition occurs, this routine returns -1 and errno is set
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cond_wait (pthread_cond_t *cond, pthread_mutex_t *mutex)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_cond_wait\n", getpid());
#endif
    return (usvsema(*mutex) == -1 ||
	    uspsema(*cond)  == -1 ||
	    uspsema(*mutex) == -1)
	? -1 : 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cond_signal - Wakes one thread that is waiting on a condition 
 |			      variable
 |  SYPNOSIS
 |	int pthread_cond_signal (pthread_cond_t *cond);
 |
 |  DESCRIPTION
 |	This routine wakes one thread waiting on a condition variable. Calling
 |	this routine implies that data guarded by the associated mutex has
 |	changed so that it might be possible for a single waiting thread to
 |	proceed. Call this routine when any thread waiting on the specified
 |	condition variable might find its predicate true, but only one thread
 |	should proceed.
 |
 |	Call this routine when the associated mutex is either locked or
 |	unlocked.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cond_signal (pthread_cond_t *cond)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_cond_signal\n", getpid());
#endif
    return (ustestsema(*cond) < 0 && usvsema(*cond) == -1) ? -1 : 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cond_broadcast - Wakes all threads that are waiting on a 
 |				 condition variable.
 |  SYPNOSIS
 |	int pthread_cond_broadcast (pthread_cond_t *cond);
 |
 |  DESCRIPTION
 |	This routine wakes all threads waiting on a condition variable. 
 |	Calling this routine implies that data guarded by the associated mutex
 |	has changed so that it might be possible for one or more waiting
 |	threads to proceed. If any waiting thread might be able to proceed,
 |	call pthread_cond_signal.
 |
 |	Call this routine when the associated mutex is either locked or
 |	unlocked.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cond_broadcast (pthread_cond_t *cond)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_cond_broadcast\n", getpid());
#endif
    while (ustestsema(*cond) < 0)
	if (usvsema(*cond) == -1)
	    return -1;
    return 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_attr_create - creates a thread attributes object that is used
 |			      to specify the attributes of threads when they
 |			      are created.
 |  SYPNOSIS
 |	int pthread_attr_create (pthread_attr_t *attr);
 |
 |  DESCRIPTION
 |	This routine creates a thread attributes object that is used to 
 |	specify the attributes of threads when they are created. The attributes
 |	object created by this routine is only used in calls to pthread_create.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno 
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_attr_create (pthread_attr_t *attr)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_attr_create\n", getpid());
#endif
    if (pthread_arena == NULL && pthread_init() == NULL)
	return -1;

    memset(attr, 0, sizeof(pthread_attr_t));
    return 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_attr_delete - deletes a thread attributes object.
 |
 |  SYPNOSIS
 |	int pthread_attr_delete (pthread_attr_t *attr);
 |
 |  DESCRIPTION
 |	This routine deletes a thread attributes object.  This routine gives
 |	permission to reclaim storage for the thread attributes object.
 |	Threads that were created using this thread attributes object are not
 |	affected by the deletion of the thread attributes object.
 |
 |	The results of calling this routine are unpredictable if the value
 |	specified by the attr parameter refers to a thread attributes object
 |	that does not exist.
 |
 |	If an error condition occurs, this routine returns 1 and sets errno to
 |	the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_attr_delete (pthread_attr_t *attr)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_attr_delete\n", getpid());
#endif
    return 0;
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_attr_setstacksize - changes the stacksize attribute of thread
 |				    creation.
 |  SYPNOSIS
 |	int pthread_attr_setstacksize (pthread_attr_t *attr, long stacksize)
 |
 |  DESCRIPTION
 |	This routine sets the minimum size (in bytes) of the stack needed for
 |	a thread created using the attributes object specified by the attr 
 |	parameter.
 |
 |	A thread's stack is fixed at the time of thread creation. Only the 
 |	main or initial thread can dynamically extend its stack.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_attr_setstacksize (pthread_attr_t *attr, long stacksize)
{
#ifdef PTHREAD_DEBUG
    printfLLog (LOG_DEBUG, "%d: pthread_attr_setstacksize\n", getpid());
#endif
    return 0;    /* always succeeds because ignored :-)  */
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_cancel - Allows a thread to request that it or another thread
 |			 terminate execution.
 |  SYPNOSIS
 |	int pthread_cancel (pthread_t thread);
 |
 |  DESCRIPTION
 |	This routine sends a cancel to the specified thread. A cancel is a 
 |	mechanism by which a calling thread informs either itself or the called
 |	thread to terminate as quickly as possible. Issuing a cancel does not 
 |	guarantee that the canceled thread receives or handles the cancel.
 |	The canceled thread can delay processing the cancel after receiving it.
 |	For instance, if a cancel arrives during an important operation, the 
 |	canceled thread can continue if what it is doing cannot be interrupted
 |	at the point where the cancel is requested.
 |
 |	Because of communication delays, the calling thread can only rely on
 |	the fact that a cancel eventually becomes pending in the designated
 |	thread (provided that the thread does not terminate beforehand).
 |	Furthermore, the calling thread has no guarantee that a pending cancel
 |	is be delivered because delivery is controlled by the designated thread.
 |
 |	Termination processing when a cancel is delivered to a thread is
 |	similar to pthread_exit. Outstanding cleanup routines are executed in
 |	the context of the target thread, and a status of -1 is made available
 |	to any threads joining with the target thread.
 |
 |	The results of this routine are unpredictable if the value specified in
 |	thread refers to a thread that does not currently exist.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_cancel (pthread_t thread)
{
/*
 |  union sigval value = {0};
 |  return sigqueue((pid_t)thread, SIGHUP, value);
 */
    return kill((pid_t)thread, SIGHUP);
}

/*----------------------------------------------------------------------------
 |  NAME
 |	pthread_setcancel - enables or disables the current thread's general 
 |			    cancelability.
 |  SYPNOSIS
 |	int pthread_setcancel (int state);
 |
 |	State of general cancelability set for the calling thread. On return,
 |	receives the prior state of general cancelability. Valid values are as
 |	follows:
 |
 |	CANCEL_ON
 |		General cancelability is enabled.
 |
 |	CANCEL_OFF
 |		General cancelability is disabled.
 |
 |  DESCRIPTION
 |	This routine enables or disables the current thread's general 
 |	cancelability and returns the previous cancelability state to the 
 |	state parameter.
 |
 |	When general cancelability is set to CANCEL_OFF, a cancel cannot be
 |	delivered to the thread, even if a cancelable routine is called or
 |	asynchronous cancelability is enabled.
 |
 |	When a thread is created, the default general cancelability state is
 |	CANCEL_ON.
 |
 |	Possible Dangers of Disabling Cancelability
 |
 |	The most important use of cancels is to ensure that indefinite wait
 |	operations are terminated. For example, a thread waiting on some
 |	network connection, which may take days to respond (or may never
 |	respond), is normally made cancelable.
 |
 |	However, when cancelability is disabled, no routine is cancelable.
 |	Waits must be completed normally before a cancel can be delivered. As
 |	a result, the program stops working and the user is unable to cancel
 |	the operation.
 |
 |	When disabling cancelability, be sure that no long waits can occur or
 |	that it is necessary for other reasons to defer cancels around that
 |	particular region of code.
 |
 |	If an error condition occurs, this routine returns -1 and sets errno
 |	to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_setcancel (int state)
{
    return (state == CANCEL_ON  
		? sigprocmask(SIG_UNBLOCK, &kill_signal, NULL) :
    	   (state == CANCEL_OFF 
		? sigprocmask(SIG_BLOCK, &kill_signal, NULL) 
		: -1));
}
