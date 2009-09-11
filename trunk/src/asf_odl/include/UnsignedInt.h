/*=============================================================================
 |  @(#)UnsignedInt.h	1.2 96/02/23 18:46:21
 |
 |  Unsigned Integer Number Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_UNSIGNEDINT_H_
#define	_ODL_UNSIGNEDINT_H_

#include <malloc.h>
#include "Obj.h"
#include "Msg.h"
#include "odl_String.h"
#include "Unit.h"

typedef struct {
    Obj_t	hdr;
    String_t	image;
    Unit_t*	unit;
    int		vlast;	/* last altered: 1=value, 0=synchronized, -1=image */
    int		value;

} UnsignedInt_t;

#ifdef	NOSTDARG
extern	UnsignedInt_t*		UnsignedInt();
#else
extern	UnsignedInt_t*		UnsignedInt(Msg_t, ...);
#endif

#ifndef	NewUnsignedInt
#define	NewUnsignedInt(_s,_n)	UnsignedInt(_Init, malloc(sizeof(UnsignedInt_t)),_s,_n)
#endif

#endif	/*!_ODL_UNSIGNEDINT_H_ */
