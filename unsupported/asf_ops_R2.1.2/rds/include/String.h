/*=============================================================================
 |  @(#)String.h	1.3 96/02/23 19:02:29
 |
 |  String Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_STRING_H_
#define	_ODL_STRING_H_

static	char sccsid_String_h[] =
	"@(#)String.h	1.3 96/02/23 19:02:29";

#include <string.h>
#include <malloc.h>
#include "Obj.h"
#include "Msg.h"

typedef struct {
    Obj_t	hdr;
    size_t      used;
    size_t      size;
    char*       ptr;

} String_t;

#ifdef	NOSTDARG
extern	String_t*		String();
#else
extern	String_t*		String(Msg_t, ...);
#endif

#ifndef NewString
#define NewString(_s,_n)	String(_Init, malloc(sizeof(String_t)),_s,_n)
#endif

#endif	/*!_ODL_STRING_H_ */
