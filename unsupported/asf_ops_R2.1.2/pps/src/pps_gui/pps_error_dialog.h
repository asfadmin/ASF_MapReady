/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
       pps_error_dialog.h
       This header file is included by pps_error_dialog.c

*******************************************************************************/

#ifndef	_PPS_ERROR_DIALOG_INCLUDED
#define	_PPS_ERROR_DIALOG_INCLUDED

#pragma ident	"@(#)pps_error_dialog.h	1.1  11/21/96"

#include <stdio.h>
#include "UxLib.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	swidget	UxUxParent;
} _UxCpps_error_dialog;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCpps_error_dialog    *UxPps_error_dialogContext;
#define UxParent                UxPps_error_dialogContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	pps_error_dialog;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_error_dialog( swidget _UxUxParent );

#endif	/* _PPS_ERROR_DIALOG_INCLUDED */
