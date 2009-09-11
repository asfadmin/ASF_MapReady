/*=============================================================================
 |  @(#)Array.c	1.3 96/02/23 17:46:43
 |
 |  Array Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/

#include "Array.h"
#include "Label.h"
#include "Relay.h"


#ifdef	NOSTDARG
Array_t* Array (va_alist)
va_dcl
#else

Array_t* Array (Msg_t msg, ...)
#endif
{
    char        blank[64];
    size_t      indent;
    String_t*   image;
    unsigned int		i;
    char	*whoName;
    size_t	whoSize, size;
    Obj_t*	new_elem;
    Array_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
	Obj(_Init, obj, Array);
	obj->emax = va_arg(ap, size_t);
	obj->incr = obj->emax ? obj->emax : 1;
	obj->nelm = 0;
#ifdef	DEBUG
printf(" [Array %x(%d)] ", obj, obj->emax);
#endif
	obj->elem = (Obj_t**) MALLOC((obj->emax + 1) * sizeof(Obj_t*));
	return obj->elem ? (obj->elem[0]=0, obj) : (obj->emax=0, (Array_t*)0);

    case _Destroy:
	for (i = 0; i < obj->nelm; ++i)
	     if (obj->elem[i]) {
		Destroy(obj->elem[i]);
		free(obj->elem[i]);
		obj->elem[i] = 0;
	     }
	if (obj->elem) {
	    free(obj->elem);
	    obj->elem = 0;
	    obj->nelm = obj->emax = 0;
	    obj->incr = 1;
	}
	return NULL;

    case _Val:
	return (Array_t*) obj->elem;

    case _Len:
	return (Array_t*) obj->nelm;

    case _Value:
	return (Array_t*) (obj->nelm ? Value(obj->elem[0]) : 0);

    case _Name:
	return (Array_t*) (obj->nelm ? Name(obj->elem[0]) : 0);

    case _Elem:
	new_elem = va_arg(ap, Obj_t*);
#ifdef	DEBUG
printf(" [%x + %x] ", obj, new_elem);
#endif
	if (obj->nelm == obj->emax) {
	    obj->emax += obj->incr;
	    obj->elem = (Obj_t**) 
		realloc(obj->elem, (obj->emax+1)*sizeof(Obj_t*));
	    if (obj->elem == NULL) {
		obj->nelm = obj->emax = 0;
		return NULL;
	    }
	}
	obj->elem[obj->nelm++] = new_elem;
	obj->elem[obj->nelm] = NULL;
	return obj;

    case _Find:
	whoName = va_arg(ap, char*);
	whoSize = va_arg(ap, size_t);
#ifdef	DEBUG
printf("Find: %s, %d\n", whoName, whoSize);
#endif
	for (size = 0; size < whoSize && whoName[size] != '.'; ++size);
	if (size == 0 || obj->elem == NULL)
	    return NULL;
	
	for (i = 0; obj->elem[i] != NULL; ++i) {
#ifdef	DEBUG
printf("Subfind: %s, %d\n", Name(obj->elem[i]), Len(obj->elem[i]));
#endif
	    if (size == Len(obj->elem[i]) &&
		! strncasecmp(whoName, Name(obj->elem[i]), size))
		return (whoSize > size)
			? Find(obj->elem[i], whoName+size+1, whoSize-size-1)
			: (String_t*) obj->elem[i];
	}
#ifdef	DEBUG
printf("Find:  Not found\n");
#endif
	return NULL;

    case _Image:
        image  = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

        if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        for (i = 0; obj->elem[i] != NULL; ++i)
	    if (! Image(obj->elem[i], image, indent))
                return NULL;
        return (Array_t*) image;

    default:
	return NULL;
    }
}
