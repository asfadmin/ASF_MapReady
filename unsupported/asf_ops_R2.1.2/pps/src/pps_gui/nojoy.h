/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
       nojoy.h
       This header file is included by nojoy.c

*******************************************************************************/

#ifndef	_NOJOY_INCLUDED
#define	_NOJOY_INCLUDED

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
} _UxCnojoy;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCnojoy               *UxNojoyContext;
#define UxParent                UxNojoyContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	nojoy;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_nojoy( swidget _UxUxParent );

#endif	/* _NOJOY_INCLUDED */
