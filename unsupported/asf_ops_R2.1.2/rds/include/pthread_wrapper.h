/*-----------------------------------------------------------------------------
 *  @(#)pthread_wrapper.h	2.8 96/10/30 09:22:46
 *
 *  POSIX Thread Emulation Package for Silicon Graphics:  Include file.
 *  Copyright (C) Jet Propulsion Laboratory
 *  Alaska SAR Facility (ASF) Project.
 *
 *----------------------------------------------------------------------------*/

#ifndef _ASF_PTHREAD_H_
#define _ASF_PTHREAD_H_

static char sccsid_pthread_wrapper_h[] =
	"@(#)pthread_wrapper.h	2.8 96/10/30 09:22:46";

/* fake pthread by jtg */
/* Redesigned and re-written by Tuan N. Truong */

#ifndef	__sgi
/*#include "/usr/include/pthread.h"*/
#include <pthread.h>

#else
#include <sys/types.h>
#include <sys/prctl.h>
#include <ulocks.h>

#define CANCEL_ON	1
#define CANCEL_OFF	0

typedef int  pthread_t;
typedef int  pthread_addr_t;
typedef int  pthread_attr_t;
typedef int  pthread_mutexattr_t;
typedef int  pthread_condattr_t;
typedef void (*pthread_startroutine_t)(void *);

typedef usema_t* pthread_mutex_t;
typedef usema_t* pthread_cond_t;

#define pthread_attr_default 0
#define pthread_condattr_default 0
#define pthread_mutexattr_default 0

int  pthread_attr_create(pthread_attr_t *attr);
int  pthread_attr_delete(pthread_attr_t *attr);
int  pthread_attr_setstacksize(pthread_attr_t *attr, long stacksize);

int  pthread_create(pthread_t *thread, const pthread_attr_t attr,
		    pthread_startroutine_t start_routine, pthread_addr_t arg);

int  pthread_join(pthread_t thread, pthread_addr_t *status);
int  pthread_cancel(pthread_t thread);
int  pthread_setcancel(int state);
void pthread_yield(void);
void pthread_exit(pthread_addr_t status);

pthread_t pthread_self(void);

int  pthread_mutex_init(pthread_mutex_t *mutex, pthread_mutexattr_t attr);
int  pthread_mutex_destroy(pthread_mutex_t *mutex);
int  pthread_mutex_lock(pthread_mutex_t *mutex);
int  pthread_mutex_unlock(pthread_mutex_t *mutex);

int  pthread_cond_init(pthread_cond_t *cond, pthread_condattr_t attr);
int  pthread_cond_destroy(pthread_cond_t *cond);
int  pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex);
int  pthread_cond_signal(pthread_cond_t *cond);
int  pthread_cond_broadcast(pthread_cond_t *cond);

#define pthread_equal(thread1, thread2)	(thread1 == thread2)

#endif	/*! NOPTHREAD */

#endif	/*!_ASF_PTHREAD_H_ */
