/*=============================================================================
 |  @(#)Seq2.c	1.2 96/02/23 18:28:43
 |
 |  2-Dimensional Sequnence Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Seq2.h"
#include "odl_String.h"
#include "Relay.h"


#ifdef	NOSTDARG
Seq2_t* Seq2 (va_alist)
va_dcl
#else

Seq2_t* Seq2 (Msg_t msg, ...)
#endif
{
    int		i;
    size_t	indent;
    String_t*	image;
    char	blank[32];
    Obj_t**	elem;
    Seq2_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Seq2_t*) Obj(_Init, obj, Seq2);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (! String(_Cat, image, blank, "(", NULL))
	    return NULL;
	i = 0;
	elem = (Obj_t**) Val(obj);
	while (elem[i] != NULL)
	     if (! Image(elem[i], image, 0) ||
		 ! String(_Cat, image, !elem[++i] ? ")" : (i%2?" ":"\n"), 0))
		return NULL;
	return (Seq2_t*) image;

    default:
        return (Seq2_t*) Array(_Relay, msg, obj, ap);
    }
}
