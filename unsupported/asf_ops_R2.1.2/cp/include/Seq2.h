/*=============================================================================
 |  @(#)Seq2.h	1.3 96/02/23 19:03:11
 |
 |  2-Dimensional Sequnence Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_SEQ2_H_
#define	_ODL_SEQ2_H_

static	char sccsid_Seq2_h[] =
	"@(#)Seq2.h	1.3 96/02/23 19:03:11";

#include "Msg.h"
#include "Array.h"

typedef	Array_t		Seq2_t;

#ifdef	NOSTDARG
extern	Seq2_t*		Seq2();
#else
extern	Seq2_t*		Seq2(Msg_t, ...);
#endif

#define NewSeq2(_array)	Seq2(_Init, _array)

#endif	/*!_ODL_SEQ2_H_ */
