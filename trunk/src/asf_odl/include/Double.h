/*=============================================================================
 |  @(#)Double.h	1.2 96/02/23 17:52:40
 |
 |  Double Precision Number Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_DOUBLE_H_
#define	_ODL_DOUBLE_H_


#include "asf.h"
#include "Obj.h"
#include "Msg.h"
#include "odl_String.h"
#include "Unit.h"

typedef struct {
    Obj_t	hdr;
    int		vlast;	/* last altered: 1=value, 0=synchronized, -1=image */
    double	value;
    Unit_t*	unit;
    String_t	image;

} Double_t;

#ifdef	NOSTDARG
extern	Double_t*	Double();
#else
extern	Double_t*	Double(Msg_t, ...);
#endif

#ifndef	NewDouble
#define	NewDouble(_s,_n) Double(_Init, MALLOC(sizeof(Double_t)),_s,_n)
#endif

#endif	/*!_ODL_DOUBLE_H_ */
