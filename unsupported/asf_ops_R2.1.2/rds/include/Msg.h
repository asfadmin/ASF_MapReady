/*=============================================================================
 |  @(#)Msg.h	1.3 96/02/23 19:10:41
 |
 |  Inter-Object Messages.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_MSG_H_
#define	_MSG_H_

#include <string.h>
#ifdef  NOSTDARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif

static	char sccsid_Msg_h[] =
	"@(#)Msg.h	1.3 96/02/23 19:10:41";

/*------------*
 |  Messages
 *------------*/
typedef enum {
    _Relay,
    _Init,
    _Destroy,
    _Val,
    _SetVal,
    _Name,
    _SetName,
    _Value,
    _SetValue,
    _Unit_of,
    _SetUnit,
    _Image,
    _Len,
    _Cat,
    _Find,
    _Elem

} Msg_t;

#ifdef	DEBUG
static	char* msgText[] = {
    "_Relay",
    "_Init",
    "_Destroy",
    "_Val",
    "_SetVal",
    "_Name",
    "_SetName",
    "_Value",
    "_SetValue",
    "_Unit_of",
    "_SetUnit",
    "_Image",
    "_Len",
    "_Cat",
    "_Find",
    "_Elem"
};
#endif

#ifdef	_NO_PROTO

void	Destroy();
void*	Val();
char*	Name();
char*	Image();
size_t	Len();
char*	Unit_of();
void*	Value();
void*	Elem();
void*	Find();
void*	Lookup();
void*	SetUnit();

#else	/*!_NO_PROTO */

void	Destroy(void* obj);
void*	Val(void* obj);
char*	Name(void* obj);
char*	Image(void* obj, void* string_obj, size_t indent);
size_t	Len(void* obj);
char*	Unit_of(void* obj);
void*	Value(void* obj);
void*	Elem(void* array, void* new_elem);
void*	Find(void* obj, char* whoName, size_t whoNameLen);
void*	Lookup(void* obj, char* whoName);
void*	SetUnit(void* obj, void* unit_obj);

#endif	/*!_NO_PROTO */


#ifdef	NOSTDARG
void*	SetVal();
#else
void*	SetVal(void* obj, ...);
#endif

#endif	/*!_MSG_H_ */
