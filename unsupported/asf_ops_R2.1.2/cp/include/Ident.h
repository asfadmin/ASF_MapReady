/*============================================================================*
 |  @(#)Ident.h	1.2 96/02/23 17:59:37
 |
 |  Identifier Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_IDENT_H_
#define	_ODL_IDENT_H_

static	char sccsid_Ident_h[] =
	"@(#)Ident.h	1.2 96/02/23 17:59:37";

#include "Msg.h"
#include "String.h"

typedef	String_t		Ident_t;

#ifdef	NOSTDARG
extern	Ident_t*		Ident();
#else
extern	Ident_t*		Ident(Msg_t, ...);
#endif

#ifndef	NewIdent
#define	NewIdent(_s,_n)		Ident(_Init, NewString(_s,_n))
#endif

#endif	/*!_ODL_IDENT_H_ */
