/*-----------------------------------------------------------------------------*
 | NAME
 |	pthread_cond_timedwait - Causes a thread to wait for a condition 
 |	variable to be  signaled or broadcast for a specified period of time.
 |
 | SYNOPSIS
 |	 #include <pthread.h>
 |
 |  	 int pthread_cond_timedwait(
 |	     pthread_cond_t *cond ,
 |	     pthread_mutex_t *mutex ,
 |	     struct timespec *abstime );
 |
 | PARAMETERS
 |	cond 	
 |	     Condition variable waited on.
 |	mutex
 |	     Mutex associated with the condition variable specified in cond.
 |	abstime
 |           Absolute time at which the wait expires, if the condition has not
 |           been signaled or broadcast. 
 |
 | DESCRIPTION
 |	This routine causes a thread to wait until one of the following occurs:
 |	+  The specified condition variable is signaled or broadcast.
 |	+  The current system clock time is greater than or equal to the time
 |	   specified by the abstime parameter.
 |
 |	This routine is identical to pthread_cond_wait except that this routine
 |	can return before a condition variable is signaled or broadcast - 
 |	specifically, when a specified time expires.
 |
 |	If the current time equals or exceeds the expiration time, this routine
 |	returns immediately, without causing the current thread to wait. 
 |	Otherwise, waiting on the condition variable can become a nonblocking
 |	loop.
 |
 |	Call this routine after you lock the mutex specified in mutex.  
 |	The results of this routine are unpredictable if this routine is 
 |	called without first locking the mutex.
 |
 | RETURN VALUES
 |	If an error condition occurs, this routine returns -1 and errno is set
 |	to the corresponding error value. Possible return values are as follows:
 |
 |      Return	Error       	Description
 |
 |        0			Successful completion.
 |       -1	[EINVAL] 	The value specified by cond, mutex, or
 |				abstime is invalid.
 |
 |				Different mutexes are supplied for
 |				concurrent pthread_cond_timedwait
 |				operations or pthread_cond_wait opera-
 |				tions on the same condition variable.
 |       -1	[EAGAIN]	The time specified by abstime expired.
 |       -1	[EDEADLK]	A deadlock condition is detected.
 |
 *----------------------------------------------------------------------------*/
#include <errno.h>
#include <sys/time.h>
#include <sys/timers.h>
#include <signal.h>
#include "pthread_wrapper.h"

static char sccsid_pthread_ext_c[] = "@(#)pthread_ext.c	1.8 97/08/02 08:55:03";

typedef struct {
    pthread_cond_t*	cond;
    pthread_mutex_t*	mutex;
    struct timespec*	timeout;
    int			expired;

} pthread_timer_t;

/*----------------*
 *  Timer Thread
 *----------------*/
static
void timer_main(pthread_timer_t *timer)
{
    struct timespec timeout;
    long tv_usec = timer->timeout->tv_nsec / 1000;
#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    gettimeofday((struct timeval*) &timeout, 0);

    if (timeout.tv_sec  < timer->timeout->tv_sec ||
       (timeout.tv_sec == timer->timeout->tv_sec && timeout.tv_nsec < tv_usec)){

	if (timeout.tv_nsec < tv_usec) {
	    timeout.tv_nsec = tv_usec - timeout.tv_nsec;
	    timeout.tv_sec  = timer->timeout->tv_sec - timeout.tv_sec;
	}
	else {
	    timeout.tv_nsec = 1000000 - (timeout.tv_nsec - tv_usec);
	    timeout.tv_sec  = (timer->timeout->tv_sec - timeout.tv_sec) - 1;
	}
	select(0,0,0,0, (struct timeval*) &timeout);
    }
    timer->expired = 1;
    pthread_cond_signal(timer->cond);
}

/*--------------------------*
 *  pthread_cond_timedwait
 *--------------------------*/
int pthread_cond_timedwait( pthread_cond_t *cond, pthread_mutex_t *mutex,
			    struct timespec *abstime)
{
    pthread_addr_t  status;
    pthread_t timer_thread;
    pthread_timer_t timer;
    register int wait_error, wait_errno;

    timer.cond = cond;
    timer.mutex = mutex;
    timer.timeout = abstime;
    timer.expired = 0;

    if (pthread_create( &timer_thread, pthread_attr_default,
	   (pthread_startroutine_t) timer_main,
	   (pthread_addr_t) &timer)
	== -1)
	return -1;

    wait_error = pthread_cond_wait(cond, mutex);
    wait_errno = errno;

    pthread_cancel(timer_thread);
    pthread_join(timer_thread, &status);

    return (wait_error == 0 && timer.expired)
	 ? (errno = EAGAIN, -1) : (errno = wait_errno, wait_error);
}

/*----------------------------------------------------------------------------
 |  NAME
 |      pthread_kill - Allows a thread to request that it or another thread
 |                       terminate execution.
 |  SYPNOSIS
 |      int pthread_kill (pthread_t thread);
 |
 |  DESCRIPTION
 |      This routine sends a kill to the specified thread. A kill is a
 |      mechanism by which a calling thread informs either itself or the called
 |      thread to terminate as quickly as possible. Issuing a kill does not
 |      guarantee that the canceled thread receives or handles the kill.
 |      The killed thread can delay processing the kill after receiving it.
 |      For instance, if a kill arrives during an important operation, the
 |      killed thread can continue if what it is doing cannot be interrupted
 |      at the point where the kill is requested.
 |
 |      Because of communication delays, the calling thread can only rely on
 |      the fact that a kill eventually becomes pending in the designated
 |      thread (provided that the thread does not terminate beforehand).
 |      Furthermore, the calling thread has no guarantee that a pending kill
 |      is be delivered because delivery is controlled by the designated thread. 
 |
 |      Termination processing when a kill is delivered to a thread is
 |      similar to pthread_exit. Outstanding cleanup routines are executed in
 |      the context of the target thread, and a status of -1 is made available
 |      to any threads joining with the target thread.
 |
 |      The results of this routine are unpredictable if the value specified in
 |      thread refers to a thread that does not currently exist.
 |
 |      If an error condition occurs, this routine returns -1 and sets errno
 |      to the corresponding error value.
 |
 *---------------------------------------------------------------------------*/
int pthread_kill (pthread_t thread)
{
/*
 |  union sigval value = {0};
 |  return sigqueue((pid_t)thread, SIGKILL, value);
 */
    return kill((pid_t)thread, SIGKILL);
}

