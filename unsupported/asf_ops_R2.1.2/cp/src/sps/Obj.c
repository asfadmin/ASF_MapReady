/*=============================================================================
 |  @(#)Obj.c	1.2 96/02/23 19:09:33
 |
 |  Basic Object Class
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Obj.h"
#include "Relay.h"

static  char sccsid_Obj_c[] =
	"@(#)Obj.c	1.2 96/02/23 19:09:33";


#ifdef	NOSTDARG
Obj_t* Obj (va_alist)
va_dcl
#else

Obj_t* Obj (Msg_t msg, ...)
#endif
{
    Obj_t* obj;
    va_init(ap, msg, obj);
    if (msg != _Init || !obj) return NULL;
    obj->send = va_arg(ap, Method_t);
    return obj;
}

#ifdef  _NO_PROTO
Method_t Type(obj)
void* obj;
#else

Method_t Type(void* obj)
#endif
{
    return obj ? ((Obj_t*) obj)->send : NULL;
}
