/*=============================================================================
 |  @(#)Relay.c	1.2 96/02/23 19:20:55
 |
 |  Message Relay Object.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Relay.h"
#include "Obj.h"



#ifdef  _NO_PROTO
void* Relay(msg, obj, ap)
Msg_t msg;
void* obj;
va_list ap;
#else

void* Relay(Msg_t msg, void* obj, va_list ap)
#endif
{
    return obj ? (*Type(obj))(_Relay, msg, obj, ap) : NULL;
}

