/*=============================================================================
 |  @(#)Group.c	1.2 96/02/23 17:54:43
 |
 |  GROUP Object Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Group.h"
#include "Relay.h"

static  char sccsid_Group_c[] =
        "@(#)Group.c	1.2 96/02/23 17:54:43";

#ifdef  NOSTDARG
Group_t* Group (va_alist)
va_dcl
#else

Group_t* Group (Msg_t msg, ...)
#endif
{
    size_t	indent;
    char	blank[64];
    String_t*	image;
    Obj_t*	value;
    Group_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
#ifdef  DEBUG
printf(" [Group] ");
#endif
	return (Group_t*) Obj(_Init, obj, Group);

    case _Find:
	return Relay(_Find, Value(obj), ap) ? obj : NULL;

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (! String(_Cat, image, blank, "GROUP = ", Name(obj), "\n", 0) ||
	    ! String(_Cat, Image(Value(obj), image, indent+2),
		           blank, "END_GROUP = ", Name(obj), "\n", 0))
            return NULL;
	return (Group_t*) image;

    default:
        return (Group_t*) Label(_Relay, msg, obj, ap);
    }
}
