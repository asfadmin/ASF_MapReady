/*=============================================================================
 |  @(#)Group.h	1.2 96/02/23 17:55:52
 |
 |  GROUP Object Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_GROUP_H_
#define	_ODL_GROUP_H_


#include "Msg.h"
#include "Label.h"

typedef	Label_t			Group_t;

#ifdef	NOSTDARG
extern	Group_t*		Group();
#else
extern	Group_t*		Group(Msg_t, ...);
#endif

#ifndef	NewGroup
#define	NewGroup(_n,_v)		Group(_Init, NewLabel(_n,_v))
#endif

#endif	/*!_ODL_GROUP_H_ */
