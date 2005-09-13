/*=============================================================================
 |  @(#)Set.c	1.3 96/02/23 19:03:43
 |
 |  SET Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Obj.h"
#include "odl_String.h"
#include "Set.h"
#include "Relay.h"


#ifdef	NOSTDARG
Set_t* Set (va_alist)
va_dcl
#else

Set_t* Set (Msg_t msg, ...)
#endif
{
    int		i;
    size_t	indent;
    String_t*	image;
    Obj_t**	elem;
    char	blank[32];
    Set_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Set_t*) Obj(_Init, obj, Set);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (! String(_Cat, image, blank, "{", NULL))
	    return NULL;
	i = 0;
	elem = (Obj_t**) Val(obj);
	while (elem[i] != NULL)
	     if (! Image(elem[i], image, 0) ||
		 ! String(_Cat, image, !elem[++i] ? "}" : (i%5?", ":",\n"), 0))
		return NULL;
	return (Set_t*) image;

    default:
        return (Set_t*) Array(_Relay, msg, obj, ap);
    }
}
