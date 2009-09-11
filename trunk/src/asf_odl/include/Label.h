/*============================================================================*
 |  @(#)Label.h	1.2 96/02/23 18:05:16
 |
 |  Label Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_LABEL_H_
#define	_ODL_LABEL_H_


#include "Obj.h"
#include "Msg.h"
#include "Ident.h"

typedef struct {
    Obj_t	hdr;
    Ident_t*	name;
    Obj_t*	value;

} Label_t;

#ifdef	NOSTDARG
extern	Label_t*	Label();
#else
extern	Label_t*	Label(Msg_t, ...);
#endif

#ifndef NewLabel
#define NewLabel(_n,_v)	Label(_Init, MALLOC(sizeof(Label_t)),_n,_v)
#endif

#endif	/*!_ODL_LABEL_H_ */
