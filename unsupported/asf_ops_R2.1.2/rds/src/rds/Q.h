/*=============================================================================
 |  @(#)Q.h	1.5 98/02/10 10:43:39
 |
 |  Circular Queue Object.
 |  Copyright(C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_Q_H_
#define _RDS_Q_H_

#include <sys/types.h>
#include "pthread_wrapper.h"

static const char sccsid_Q_h[] =
	"@(#)Q.h	1.5 98/02/10 10:43:39";

typedef struct Q_t {			/* Circular Queue Object */
    int		quit; 			/* True to bail out in emergency */
    char*	name;

    pthread_mutex_t	Mutex;		/* Mutex for Q synchronization */
    pthread_cond_t	NotEmptyCond;	/* Signaled when queue is not empty */
    pthread_cond_t	NotFullCond;	/* Signaled when queue is not full */

    void**	base;			/* Array of Q element pointers */
    size_t	size;			/* Number of queue elements */
    off_t	head;			/* (head+1) % size => First in queue */
    off_t	tail;			/* tail => Last inserted */
    int		(*cmp)(void*, void*);	/* For priority queues */
    int		(*rem)(struct Q_t*, void* elm, void* userData);
					/* Return 0 => do not remove item */
    void*	(*insert)(struct Q_t*, void* elm, void* userData);
    void*	insertData;		/* Callback user data */

    void*	(*remove)(struct Q_t*, void* elm, void* userData);
    void*	removeData;		/* Callback user data */

} Q_t;

extern	void*	Q_Remove(Q_t* que, void* elm);
extern	void*	Q_Insert(Q_t* que, void* elm);
extern	void*	Q_Peek(Q_t* que, void* elm);
extern	void*	Q_Tail(Q_t* que, void* elm);
extern	void	Q_Lock(Q_t* que);
extern	void	Q_Unlock(Q_t* que);
extern	void	Q_Reset(Q_t* que);
extern	void	Q_Shutdown(Q_t* que);
extern	void	Q_Destroy(Q_t* que);
extern	Q_t*	Q_Init(Q_t* que, void* base, size_t size, int (*cmp)(),
		       void* (*insert)(), void* insertData,
		       void* (*remove)(), void* removeData,
		       int (*removeTest)(), char* name);
#endif /*!_RDS_Q_H_ */
