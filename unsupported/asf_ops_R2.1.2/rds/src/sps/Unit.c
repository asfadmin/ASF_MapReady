/*=============================================================================
 |  @(#)Unit.c	1.2 96/02/23 18:42:26
 |
 |  Unit Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Unit.h"
#include "Relay.h"

static  char sccsid_Unit_c[] =
        "@(#)Unit.c	1.2 96/02/23 18:42:26";

#ifdef	NOSTDARG
Unit_t* Unit(va_alist)
va_dcl
#else

Unit_t* Unit(Msg_t msg, ...)
#endif
{
    String_t*	image;
    Unit_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Unit_t*) Obj(_Init, obj, Unit);

    case _Image:
	image = va_arg(ap, String_t*);
        return (Unit_t*) String(_Cat, image, "<", Val(obj), ">", 0);

    default:
        return (Unit_t*) String(_Relay, msg, obj, ap);
    }
}
