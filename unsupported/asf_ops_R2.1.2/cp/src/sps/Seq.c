/*=============================================================================
 |  @(#)Seq.c	1.2 96/02/23 18:25:53
 |
 |  Sequnence (1-Dimensional Array) Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Seq.h"
#include "String.h"
#include "Relay.h"

static  char sccsid_Seq_c[] =
        "@(#)Seq.c	1.2 96/02/23 18:25:53";

#ifdef	NOSTDARG
Seq_t* Seq (va_alist)
va_dcl
#else

Seq_t* Seq (Msg_t msg, ...)
#endif
{
    int		i;
    size_t	indent;
    String_t*	image;
    Obj_t**	elem;
    char	blank[32];
    Seq_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Seq_t*) Obj(_Init, obj, Seq);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (! String(_Cat, image, blank, "(", 0))
	    return NULL;
	i = 0;
	elem = (Obj_t**) Val(obj);
	while (elem[i] != NULL)
	     if (! Image(elem[i], image, 0) ||
		 ! String(_Cat, image, !elem[++i] ? ")" : (i%5?", ":",\n"), 0))
		return NULL;
	return (Seq_t*) image;

    default:
        return (Seq_t*) Array(_Relay, msg, obj, ap);
    }
}
