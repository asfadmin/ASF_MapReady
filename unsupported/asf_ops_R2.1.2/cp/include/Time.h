/*=============================================================================
 |  @(#)Time.h	1.2 96/02/23 18:40:02
 |
 |  Time Object Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_TIME_H_
#define	_ODL_TIME_H_

static	char sccsid_Time_h[] =
	"@(#)Time.h	1.2 96/02/23 18:40:02";

#include <time.h>
#include <malloc.h>
#include "Obj.h"
#include "Msg.h"
#include "String.h"
#include "Unit.h"

#ifndef	_ODL_R1A_H_
/*----------------------------*
 |  Augmented 'tm' structure
 *----------------------------*/
typedef struct {
    struct tm   tm;                     /* Standard 'tm' structure */
    long        tv_usec;                /* and microseconds */

} GMT_t;
#endif

typedef struct {
    Obj_t	hdr;
    String_t	image;
    Unit_t*	unit;
    int		vlast;	/* last altered: 1=value, 0=synchronized, -1=image */
    GMT_t	value;

} Time_t;

#ifdef	NOSTDARG
extern	Time_t*		Time();
#else
extern	Time_t*		Time(Msg_t, ...);
#endif

#ifndef NewTime
#define NewTime(_s,_n)	Time(_Init, malloc(sizeof(Time_t)),_s,_n)
#endif

#endif	/*!_ODL_TIME_H_ */
