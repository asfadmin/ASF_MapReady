/*=============================================================================
 |  @(#)Status.h	1.6 98/02/10 10:36:19
 |
 |  Circular Queue Object.
 |  Copyright(C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_STATUS_H_
#define _RDS_STATUS_H_

#include <sys/types.h>
#include "pthread_wrapper.h"

static const char sccsid_Status_h[] =
	"@(#)Status.h	1.6 98/02/10 10:36:19";

#define STATUS_MSGLEN	1023		/* Longest message size */

typedef struct {			/* Status Message */
    pthread_mutex_t	Mutex;		/* For thread synchronization */
    int		err;			/* A copy of errno, whenever possible */
    int		len;			/* Actual space remained in msg_buf */
    char	msg[STATUS_MSGLEN+1];
} Status_t;

extern	int		Status_Log(Status_t*, const char* format, ...);
extern	Status_t*	Status_Init(Status_t* obj);
extern	void		Status_Reset(Status_t*);
extern	void		Status_Destroy(Status_t*);
extern	const char* 	Status_String(const Status_t*);
extern	const int*	Status_Errno(const Status_t* obj);

#endif /*!_RDS_STATUS_H_ */
