/*=============================================================================
 |  @(#)Ident.c	1.2 96/02/23 17:58:08
 |
 |  Identifier Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Ident.h"
#include "Relay.h"

static  char sccsid_Ident_c[] =
        "@(#)Ident.c	1.2 96/02/23 17:58:08";

#ifdef	NOSTDARG
Ident_t* Ident (va_alist)
va_dcl
#else

Ident_t* Ident (Msg_t msg, ...)
#endif
{
    char	blank[64];
    String_t*	image;
    size_t	indent;
    Ident_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
#ifdef	DEBUG
printf(" [ID] ");
#endif
	return (Ident_t*) Obj(_Init, obj, Ident);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))

        if (indent > 0) memset(blank, ' ', indent);
	blank[indent] = 0;

        return (Ident_t*) String(_Cat, image, blank, Val(obj), 0);

    default:
        return (Ident_t*) String(_Relay, msg, obj, ap);
    }
}
