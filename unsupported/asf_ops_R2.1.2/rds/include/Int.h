/*=============================================================================
 |  @(#)Int.h	1.2 96/02/23 18:02:32
 |
 |  Integer Number Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_INT_H_
#define	_ODL_INT_H_

static	char sccsid_Int_h[] =
	"@(#)Int.h	1.2 96/02/23 18:02:32";

#include <malloc.h>
#include "Obj.h"
#include "Msg.h"
#include "String.h"
#include "Unit.h"

typedef struct {
    Obj_t	hdr;
    String_t	image;
    Unit_t*	unit;
    int		vlast;	/* last altered: 1=value, 0=synchronized, -1=image */
    int		value;

} Int_t;

#ifdef	NOSTDARG
extern	Int_t*		Int();
#else
extern	Int_t*		Int(Msg_t, ...);
#endif

#ifndef	NewInt
#define	NewInt(_s,_n)	Int(_Init, malloc(sizeof(Int_t)),_s,_n)
#endif

#endif	/*!_ODL_INT_H_ */
