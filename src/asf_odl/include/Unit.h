/*=============================================================================
 |  @(#)Unit.h	1.2 96/02/23 18:43:02
 |
 |  Unit Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_UNIT_H_
#define	_ODL_UNIT_H_


#include "Obj.h"
#include "Msg.h"
#include "odl_String.h"

typedef String_t	Unit_t;

#ifdef	NOSTDARG
extern	Unit_t*		Unit();
#else
extern	Unit_t*		Unit(Msg_t, ...);
#endif

#ifndef NewUnit
#define NewUnit(_s,_n)	Unit(_Init, NewString(_s,_n))
#endif

#endif	/*!_ODL_UNIT_H_ */
