/*=============================================================================
 |  @(#)Set.h	1.3 96/02/23 19:02:49
 |
 |  SET Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_SET_H_
#define	_ODL_SET_H_


#include "Msg.h"
#include "Array.h"

typedef	Array_t		Set_t;

#ifdef	NOSTDARG
extern	Set_t*		Set();
#else
extern	Set_t*		Set(Msg_t, ...);
#endif

#define NewSet(_array)	Set(_Init, _array)

#endif	/*!_ODL_SET_H_ */
