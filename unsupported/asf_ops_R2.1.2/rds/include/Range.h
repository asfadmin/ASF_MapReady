/*=============================================================================
 |  @(#)Range.h	1.2 96/02/23 18:23:47
 |
 |  Range Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_RANGE_H
#define	_ODL_RANGE_H

static	char sccsid_Range_h[] =
	"@(#)Range.h	1.2 96/02/23 18:23:47";

#include "Msg.h"
#include "Array.h"

typedef	Array_t			Range_t;

#ifdef	NOSTDARG
extern	Range_t*		Range();
#else
extern	Range_t*		Range(Msg_t, ...);
#endif

#define NewRange(_array)        Range(_Init, _array)

#endif	/*!_ODL_RANGE_H */
