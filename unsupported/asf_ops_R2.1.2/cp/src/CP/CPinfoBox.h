
/*******************************************************************************
       CPinfoBox.h
       (Generated from interface file CPinfoBox.i)
       This header file is included by CPinfoBox.c

*******************************************************************************/

#ifndef	_CPINFOBOX_INCLUDED
#define	_CPINFOBOX_INCLUDED

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
} _UxCCPinfoBox;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPinfoBox           *UxCPinfoBoxContext;
#define UxParent                UxCPinfoBoxContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPinfoBox;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPinfoBox();

#endif	/* _CPINFOBOX_INCLUDED */
