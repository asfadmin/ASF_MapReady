/*============================================================================
 |  @(#)Status.c	1.5 98/02/10 10:36:10
 |
 |  Status Message Object.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *===========================================================================*/
#ifdef	NOSTDARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include "Status.h"

static const char sccsid_Status_c[] =
        "@(#)Status.c	1.5 98/02/10 10:36:10";

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
#ifdef  NOSTDARG
int Status_Log(va_alist)
va_dcl
#else

int Status_Log(Status_t* obj, const char *format, ...)
#endif
{
    int n;
    va_list ap;
    va_start(ap, format);

    pthread_mutex_lock(&obj->Mutex);

    n = vsprintf(obj->msg, format, ap);
    if (n > obj->len) {
	obj->len = 0;
	pthread_mutex_unlock(&obj->Mutex);
	return -1;
    }
    obj->len -= n;
    memmove(obj->msg + obj->len, obj->msg, n);
    pthread_mutex_unlock(&obj->Mutex);
    return (obj->err = -1);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
Status_t* Status_Init(Status_t* obj)
{
    static char init_error[] = "can't setup error reporting\n";
    obj->len = STATUS_MSGLEN;
    obj->msg[0] = obj->msg[STATUS_MSGLEN] = 0;

    if (pthread_mutex_init(&obj->Mutex, pthread_mutexattr_default) == -1) {
	obj->len -= sizeof(init_error)-1;
	memcpy(obj->msg + obj->len, init_error, sizeof(init_error)-1);
	obj->err = -1;
	return NULL;
    }
    obj->err = 0;
    return obj;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
void Status_Reset(Status_t* obj)
{
    obj->err = 0;
    obj->len = STATUS_MSGLEN;
    obj->msg[0] = obj->msg[STATUS_MSGLEN] = 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
void Status_Destroy(Status_t* obj)
{
    pthread_mutex_destroy(&obj->Mutex);
    obj->err = 0;
    obj->len = STATUS_MSGLEN;
    obj->msg[0] = obj->msg[STATUS_MSGLEN] = 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
const char* Status_String(const Status_t* obj)
{
    return (obj->msg + obj->len);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
const int* Status_Errno(const Status_t* obj)
{
    return (&obj->err);
}
