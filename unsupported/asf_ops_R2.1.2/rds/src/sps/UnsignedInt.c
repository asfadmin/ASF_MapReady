/*=============================================================================
 |  @(#)UnsignedInt.c	1.2 96/02/23 18:45:41
 |
 |  Unsigned Integer Number Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "UnsignedInt.h"
#include "Relay.h"

static  char sccsid_UnsignedInt_c[] =
        "@(#)UnsignedInt.c	1.2 96/02/23 18:45:41";


#ifdef	NOSTDARG
UnsignedInt_t* UnsignedInt (va_alist)
va_dcl
#else

UnsignedInt_t* UnsignedInt (Msg_t msg, ...)
#endif
{
    char	blank[64], buf[16];
    String_t*	image;
    size_t	indent;
    UnsignedInt_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	Obj(_Init, obj, UnsignedInt);
	obj->vlast = -1;
	obj->value = 0;
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
	obj->value = va_arg(ap, unsigned int);
	obj->vlast = 1;
	return obj;

    case _Val:
	if (obj->vlast < 0) {
	    if (sscanf(Val(&obj->image), "%u", &obj->value) != 1)
		return NULL;
	    obj->vlast = 0;
	}
	return (UnsignedInt_t*) &obj->value;

    case _Unit_of:
	return (UnsignedInt_t*) Val(obj->unit);
	
    case _SetUnit:
	return (obj->unit = va_arg(ap, Unit_t*), obj);
	
    case _Image:
	image = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

	if (indent > 0) memset(blank, ' ', indent);
	blank[indent] = 0;

	if (obj->vlast > 0) {
	    sprintf(buf, "%u", obj->value);
	    if (! SetVal(&obj->image, buf)) return NULL;
	    obj->vlast = 0;
	}
	return (UnsignedInt_t*) String(_Cat, image, blank, Val(&obj->image), 0);

    default:
      return NULL;
    }
}
