/*=============================================================================
 |  @(#)Relay.h	1.2 96/02/23 19:19:44
 |
 |  Message Relay Object.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_RELAY_H_
#define	_RELAY_H_

#ifdef  NOSTDARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include "Msg.h"


#ifdef	NOSTDARG
void*   Relay();
#else
void*   Relay(Msg_t, void* obj, va_list);
#endif


#ifdef  NOSTDARG
#define	va_init(_ap,_msg,_obj)		\
    Msg_t   _msg;			\
    va_list _ap;			\
    va_start(_ap);			\
    _msg = va_arg(_ap, Msg_t);		\
    if (_msg !=_Relay)			\
	*(void**)&_obj = va_arg(_ap, void*); \
    else {				\
        va_list new = _ap;		\
        _msg = va_arg(new, Msg_t);	\
	*(void**)&_obj = va_arg(new, void*); \
        _ap  = va_arg(new, va_list);	\
    }
#else
#define	va_init(_ap,_msg,_obj)		\
    va_list _ap;			\
    va_start(_ap, _msg);		\
    if (_msg !=_Relay)			\
	*(void**)&_obj = va_arg(_ap, void*); \
    else {				\
        va_list new = _ap;		\
        _msg = va_arg(new, Msg_t);	\
	*(void**)&_obj = va_arg(new, void*); \
        _ap  = va_arg(new, va_list);	\
    }
#endif

#endif	/*!_RELAY_H_ */
