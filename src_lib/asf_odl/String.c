/*=============================================================================
 |  @(#)String.c	1.2 96/02/23 18:33:38
 |
 |  String Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "odl_String.h"
#include "Relay.h"


#ifdef	NOSTDARG
String_t* String(va_alist)
va_dcl
#else

String_t* String(Msg_t msg, ...)
#endif
{
    char	blank[64], *str;
    size_t	indent, len;
    String_t*	obj, *image;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	Obj(_Init, obj, String);
	str = va_arg(ap, char*);
	obj->size = va_arg(ap, size_t);
	if (! (obj->ptr = (char*) malloc(obj->size+1))) {
	    obj->size = obj->used = 0;
#ifdef	DEBUG
printf(" [String Out of Memory] ");
#endif
	    return NULL;
	}
	memcpy(obj->ptr, str, obj->used = obj->size);
	obj->ptr[obj->size] = 0;
#ifdef	DEBUG
printf("[String %s] ", obj->ptr);
#endif
	return (String_t*) obj;

    case _Destroy:
	if (obj->ptr) {
	    free(obj->ptr);
	    obj->ptr  = 0;
	    obj->size = obj->used = 0;
	}
	return NULL;

    case _Val:
	return  (String_t*) obj->ptr;

    case _SetVal:
	if (! (str = va_arg(ap, char*)))
	    return NULL;

	len = strlen(str);
	if (len > obj->size) {
	    obj->ptr = (char*) realloc(obj->ptr, len+1);
	    if (! obj->ptr ) {
		obj->size = obj->used = 0;
		return NULL;
	    }
	    obj->size = len;
	}
	memcpy(obj->ptr, str, len+1);
	obj->used = len;
	return obj;

    case _Len:
	return (String_t*) obj->used;

    case _Cat:
	while ( (str = va_arg(ap, char*)) ) {
	    len = strlen(str);

	    if (obj->used + len > obj->size) {
		obj->size = obj->used + len;

		if (! (obj->ptr = (char*) realloc(obj->ptr, obj->size+1)))
		    return (String_t*) (obj->size = obj->used = 0);
	    }
	    memcpy((char*) obj->ptr + obj->used, str, len+1);
	    obj->used += len;
	}
	return obj;

    case _Image:
	image  = va_arg(ap, String_t*);
	if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
	    return NULL;

	if (indent > 0) memset(blank, ' ', indent);
	blank[indent] = 0;

        return String(_Cat, image, blank, "\"", obj->ptr, "\"", 0);

    default:
	return NULL;
    }
}
