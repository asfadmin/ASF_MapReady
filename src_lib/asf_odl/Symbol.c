/*=============================================================================
 |  @(#)Symbol.c	1.2 96/02/23 18:36:56
 |
 |  Symbol Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Symbol.h"
#include "Relay.h"


#ifdef	NOSTDARG
Symbol_t* Symbol(va_alist)
va_dcl
#else

Symbol_t* Symbol(Msg_t msg, ...)
#endif
{
    char	blank[64];
    String_t*	image;
    size_t	indent;
    Symbol_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	return (Symbol_t*) Obj(_Init, obj, Symbol);

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
	    return NULL;

        if (indent > 0) memset(blank, ' ', indent);
	blank[indent] = 0;

        return (Symbol_t*) String(_Cat, image, blank, "'", Val(obj), "'", 0);

    default:
        return (Symbol_t*) String(_Relay, msg, obj, ap);
    }
}
