/*=============================================================================
 |  @(#)Double.c	1.3 96/10/29 15:32:37
 |
 |  Double Precision Number Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "asf.h"
#include "Double.h"
#include "Relay.h"


#ifdef  NOSTDARG
Double_t* Double(va_alist)
va_dcl
#else

Double_t* Double(Msg_t msg, ...)
#endif
{
    char	blank[64], buf[64];
    String_t*	image;
    size_t	indent;
    Double_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	Obj(_Init, obj, Double);
        obj->vlast = -1;
        obj->value = 0.0;
	obj->unit = 0;
        return String(_Relay, _Init, &obj->image, ap) ? obj : NULL;

    case _Destroy:
	Destroy(&obj->image);
	if (obj->unit) {
	    Destroy(obj->unit);
	    free(obj->unit);
	    obj->unit = 0;
	}
	return NULL;

    case _SetVal:
        obj->value = va_arg(ap, double);
        obj->vlast = 1;
        return obj;

    case _Val:
	if (obj->vlast < 0) {
	    if (sscanf(Val(&obj->image), "%lf", &obj->value) != 1)
		return NULL;
	    obj->vlast = 0;
	}
	return (Double_t*) &obj->value;

    case _Unit_of:
        return Val(obj->unit);

    case _SetUnit:
        return (obj->unit = va_arg(ap, Unit_t*), obj);

    case _Image:
        image = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

        if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (obj->vlast > 0) {
            sprintf(buf, "%.7e", obj->value); /* 7 digits for e-format */
	    if (! SetVal(&obj->image, buf))
                return NULL;
	    obj->vlast = 0;
        }
        return (Double_t*) String(_Cat, image, blank, Val(&obj->image), 0);

    default:
	return NULL;
    }
}
