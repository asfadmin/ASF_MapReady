/*=============================================================================
 |  @(#)Symbol.h	1.2 96/02/23 18:37:24
 |
 |  Symbol Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_SYMBOL_H_
#define	_ODL_SYMBOL_H_


#include "Msg.h"
#include "Obj.h"
#include "odl_String.h"

typedef String_t		Symbol_t;

#ifdef	NOSTDARG
extern	Symbol_t*		Symbol();
#else
extern	Symbol_t*		Symbol(Msg_t, ...);
#endif

#ifndef NewSymbol
#define NewSymbol(_s,_n)	Symbol(_Init, NewString(_s,_n))
#endif

#endif	/*!_ODL_SYMBOL_H_ */
