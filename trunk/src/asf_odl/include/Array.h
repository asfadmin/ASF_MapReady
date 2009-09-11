/*=============================================================================
 |  @(#)Array.h	1.2 96/02/23 17:48:35
 |
 |  Array Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_ARRAY_H_
#define	_ODL_ARRAY_H_

#include "asf.h"

#include "Obj.h"
#include "Msg.h"

typedef struct {
    Obj_t	hdr;
    size_t	incr;
    size_t	nelm;
    size_t	emax;
    Obj_t**	elem;

} Array_t;

#ifdef	NOSTDARG
extern	Array_t*	Array();
#else
extern	Array_t*	Array(Msg_t, ...);
#endif

#ifndef	NewArray
#define	NewArray(_n)	Array(_Init, MALLOC(sizeof(Array_t)),_n)
#endif

#endif	/*!_ODL_ARRAY_H_ */
