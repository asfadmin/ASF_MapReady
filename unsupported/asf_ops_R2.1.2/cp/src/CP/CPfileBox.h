
/*******************************************************************************
       CPfileBox.h
       (Generated from interface file CPfileBox.i)
       This header file is included by CPfileBox.c

*******************************************************************************/

#ifndef	_CPFILEBOX_INCLUDED
#define	_CPFILEBOX_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

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
} _UxCCPfileBox;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPfileBox           *UxCPfileBoxContext;
#define UxParent                UxCPfileBoxContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPfileBox;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPfileBox();

#endif	/* _CPFILEBOX_INCLUDED */
