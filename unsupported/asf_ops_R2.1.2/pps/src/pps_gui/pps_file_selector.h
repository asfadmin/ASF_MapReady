/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
       pps_file_selector.h
       This header file is included by pps_file_selector.c

*******************************************************************************/

#pragma ident	"@(#)pps_file_selector.h	1.1  11/21/96"

#ifndef	_PPS_FILE_SELECTOR_INCLUDED
#define	_PPS_FILE_SELECTOR_INCLUDED

#include <stdio.h>
#include "UxLib.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

extern swidget	pps_file_selector;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_file_selector( swidget _UxUxParent );

#endif	/* _PPS_FILE_SELECTOR_INCLUDED */
