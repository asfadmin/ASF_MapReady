/*=============================================================================
 |  @(#)Msg.c	1.3 96/02/23 19:18:59
 |
 |  Inter-Object Messaging Functions.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "Msg.h"
#include "Obj.h"

static  char sccsid_Msg_c[] =
	"@(#)Msg.c	1.3 96/02/23 19:18:59";

#ifdef	_NO_PROTO
void  Destroy(obj)
void* obj;
#else

void  Destroy(void* obj)
#endif
{
    if (obj) (void) (*Type(obj))(_Destroy, obj);
}

#ifdef	_NO_PROTO
void* Val(obj)
void* obj;
#else

void* Val(void* obj)
#endif
{
    return obj ? (*Type(obj))(_Val, obj) : NULL;
}

#ifdef	_NO_PROTO
char* Name(obj)
void* obj;
#else

char* Name(void* obj)
#endif
{
    return obj ? (char*) (*Type(obj))(_Name, obj) : NULL;
}

#ifdef  _NO_PROTO
char* Image(obj, string_obj, indent)
void* obj;
void* string_obj;
size_t indent;
#else

char* Image(void* obj, void* string_obj, size_t indent)
#endif
{
    return obj ? (char*) (*Type(obj))(_Image, obj, string_obj, indent) : NULL;
}

#ifdef	_NO_PROTO
size_t Len(obj)
void* obj;
#else

size_t Len(void* obj)
#endif
{
    return obj ? (size_t) (*Type(obj))(_Len, obj) : 0;
}

#ifdef	_NO_PROTO
char* Unit_of(obj)
void* obj;
#else

char* Unit_of(void* obj)
#endif
{
    return obj ? (char*) (*Type(obj))(_Unit_of, obj) : NULL;
}

#ifdef	_NO_PROTO
void* Value(obj)
void* obj;
#else

void* Value(void* obj)
#endif
{
    return obj ? (*Type(obj))(_Value, obj) : NULL;
}

#ifdef  _NO_PROTO
void* Elem(array, elem)
void* array;
void* elem;
#else

void* Elem(void* array, void* elem)
#endif
{
    return array ? (*Type(array))(_Elem, array, elem) : NULL;
}

#ifdef  _NO_PROTO
void* Find(obj, whoName, whoNameLen)
void* obj;
char* whoName;
size_t whoNameLen;
#else

void* Find(void* obj, char* whoName, size_t whoNameLen)
#endif
{
    return obj ? (*Type(obj))(_Find, obj, whoName, whoNameLen) : NULL;
}

#ifdef  _NO_PROTO
void* Lookup(obj, whoName)
void* obj;
char* whoName;
#else

void* Lookup(void* obj, char* whoName)
#endif
{
    return (!obj ? NULL
		 : (!whoName ? obj : (*Type(obj))(_Find, obj,
						  whoName, strlen(whoName))));
}

#ifdef  _NO_PROTO
void* SetUnit(obj, unit_obj)
void* obj;
void* unit_obj;
#else

void* SetUnit(void* obj, void* unit_obj)
#endif
{
    return obj ? (*Type(obj))(_SetUnit, obj, unit_obj) : NULL;
}

#ifdef  NOSTDARG
void* SetVal(va_alist)
va_dcl
#else

void* SetVal(void* obj, ...)
#endif
{
    va_list ap;
#ifdef	NOSTDARG
    void*  obj;
    va_start(ap);
    obj = va_arg(ap, void*);
#else
    va_start(ap, obj);
#endif

    return obj ? (*Type(obj))(_Relay, _SetVal, obj, ap) : NULL;
}
