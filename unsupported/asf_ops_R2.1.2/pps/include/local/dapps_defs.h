/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	dapps_defs.h
Description:	This file contains definitions which in all likelihood will be
		needed throughout the DAPPS.
Notes:		

RCS Info:
 $Source$
 $Revision$
 $Date$
 $Author$
==============================================================================*/

#ifndef	_DAPPSDEFS_
#define	_DAPPSDEFS_

#pragma ident "@(#)dapps_defs.h	1.1  11/21/96"

#include <values.h>   /* for MAXINT, etc...     */
#include <macros.h>   /* for min(), max(), etc...   */


#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef OK
#define OK 0
#endif

#ifndef ERROR
#define ERROR -1
#endif

#ifndef NULL
#define NULL	(void *) 0
#endif

#endif /*  _DAPPSDEFS_   */
