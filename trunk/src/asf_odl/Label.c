/*=============================================================================
 |  @(#)Label.c	1.2 96/02/23 18:04:10
 |
 |  Label Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Label.h"
#include "Relay.h"

#ifdef	NOSTDARG
Label_t* Label (va_alist)
va_dcl
#else

Label_t* Label (Msg_t msg, ...)
#endif
{
    char	blank[64];
    String_t*	image;
    size_t	indent;
    Label_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
#ifdef	DEBUG
printf(" [Label] ");
#endif
	Obj(_Init, obj, Label);
	obj->name  = va_arg(ap, String_t*);
	obj->value = va_arg(ap, Obj_t*);
        return (Label_t*) obj;

    case _Destroy:
	if (obj->value) {
	    Destroy(obj->value);
	    free(obj->value);
	    obj->value = 0;
	}
	if (obj->name) {
	    Destroy(obj->name);
	    free(obj->name);
	    obj->name = 0;
	}
	return NULL;

    case _Val:
	return (Label_t*) Val(obj->value);

    case _Name:
	return (Label_t*) Val(obj->name);

    case _Len:
	return (Label_t*) Len(obj->name);

    case _Value:
	return (Label_t*) obj->value;

    case _SetVal:
	return Relay(_SetVal, obj->value, ap) ? obj : NULL;

    case _SetName:
	return Relay(_SetVal, obj->name, ap) ? obj : NULL;

    case _SetValue:
	return (obj->value = va_arg(ap, Obj_t*), obj);

    case _Image:
	image  = va_arg(ap, String_t*);
	if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
	    return NULL;

	if (indent > 0) memset(blank, ' ', indent);
	blank[indent] = 0;

        if (! String(_Cat, image, blank, Val(obj->name), " = ", 0) ||
            ! String(_Cat, Image(obj->value, image, 0),  "\n", 0))
	    return NULL;

	return (Label_t*) image;

    default:
	return NULL;
    }
}
