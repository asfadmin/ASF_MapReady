/*=============================================================================
 |  @(#)Range.c	1.2 96/02/23 18:22:42
 |
 |  Range Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Range.h"
#include "String.h"
#include "Relay.h"

static  char sccsid_Range_c[] =
        "@(#)Range.c	1.2 96/02/23 18:22:42";

#ifdef	NOSTDARG
Range_t* Range (va_alist)
va_dcl
#else

Range_t* Range (Msg_t msg, ...)
#endif
{
    size_t	indent;
    String_t*	image;
    char	blank[32];
    Obj_t**	range;
    Range_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Range_t*) Obj(_Init, obj, Range);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

	range = (Obj_t**) Val(obj);
        if (! String(_Cat, image, blank, NULL)  ||
	    ! Image(range[0], image, 0) ||
            ! String(_Cat, image, " .. ", NULL) ||
	    ! Image(range[1], image, 0))
	    return NULL;
	return (Range_t*) image;

    default:
        return (Range_t*) Array(_Relay, msg, obj, ap);
    }
}
