/*=============================================================================
 |  @(#)Object.c	1.3 96/02/27 09:43:42
 |
 |  OBJECT Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/

#include "Object.h"
#include "Relay.h"

static  char sccsid_Object_c[] =
        "@(#)Object.c	1.3 96/02/27 09:43:42";

#ifdef	NOSTDARG
Object_t* Object (va_alist)
va_dcl
#else

Object_t* Object (Msg_t msg, ...)
#endif
{
    char	blank[64];
    size_t	indent;
    String_t*	image;
    Obj_t*	value;
    Object_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
#ifdef	DEBUG
printf(" [Object] ");
#endif
	return (Object_t*) Obj(_Init, obj, Object);

    case _Find:
	return (Object_t*) Relay(_Find, Value(obj), ap);

    case _Image:
        image  = va_arg(ap, String_t*);
	if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (! String(_Cat, image, blank, "OBJECT = ", Name(obj), "\n", 0) ||
	    ! String(_Cat, Image(Value(obj), image, indent+2),
		     blank, "END_OBJECT = ", Name(obj), "\n", 0))
            return NULL;
        return (Object_t*) image;

    default:
	return (Object_t*) Label(_Relay, msg, obj, ap);
    }
}
