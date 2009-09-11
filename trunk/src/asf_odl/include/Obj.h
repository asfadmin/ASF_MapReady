/*=============================================================================
 |  @(#)Obj.h	1.2 96/02/23 19:10:04
 |
 |  Basic Object Class
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_OBJ_H_
#define	_OBJ_H_

#include "Msg.h"


#ifdef	NOSTDARG
typedef void*	(*Method_t)();
#else
typedef void*	(*Method_t)(Msg_t, ...);
#endif

typedef struct {
    Method_t	send;

} Obj_t;

#ifdef  NOSTDARG
extern  Obj_t*		Obj();
#else
extern  Obj_t*		Obj(Msg_t, ...);
#endif

#ifdef  _NO_PROTO
extern  Method_t	Type();
#else
extern  Method_t	Type(void* obj);
#endif

#ifndef	NewObj
#define	NewObj(_obj,_method)	Obj(_Init,_obj,_method)
#endif

#ifndef NULL
#define	NULL		0
#endif

#endif	/*!_OBJ_H_ */
