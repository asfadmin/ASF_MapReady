/*============================================================================*
 |  @(#)Object.h	1.2 96/02/23 18:14:27
 |
 |  OBJECT Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_OBJECT_H_
#define	_ODL_OBJECT_H_


#include "Msg.h"
#include "Label.h"

typedef Label_t			Object_t;

#ifdef	NOSTDARG
extern	Object_t*		Object();
#else
extern	Object_t*		Object(Msg_t, ...);
#endif

#ifndef NewObject
#define NewObject(_n,_v)	Object(_Init, NewLabel(_n,_v))
#endif

#endif	/*!_ODL_OBJECT_H_ */
