/*=============================================================================
 |  @(#)Seq.h	1.2 96/02/23 18:26:24
 |
 |  Sequnence (1-Dimensional Array) Class Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_SEQ_H_
#define	_ODL_SEQ_H_

static	char sccsid_Seq_h[] =
	"@(#)Seq.h	1.2 96/02/23 18:26:24";

#include "Msg.h"
#include "Array.h"

typedef	Array_t		Seq_t;

#ifdef	NOSTDARG
extern	Seq_t*		Seq();
#else
extern	Seq_t*		Seq(Msg_t, ...);
#endif

#define NewSeq(_array)  Seq(_Init, _array)

#endif	/*!_ODL_SEQ_H_ */
