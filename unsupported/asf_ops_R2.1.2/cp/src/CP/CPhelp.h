
/*******************************************************************************
       CPhelp.h
       (Generated from interface file CPhelp.i)
       This header file is included by CPhelp.c

*******************************************************************************/

#ifndef	_CPHELP_INCLUDED
#define	_CPHELP_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
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
	Widget	UxmainWindow2;
	Widget	Uxmenu1;
	Widget	Uxmenu1_p1;
	Widget	Uxmenu1_p1_b1;
	Widget	Uxmenu1_top_b1;
	Widget	Uxform1;
	Widget	UxscrolledWindow1;
	swidget	UxUxParent;
} _UxCCPhelp;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPhelp              *UxCPhelpContext;
#define mainWindow2             UxCPhelpContext->UxmainWindow2
#define menu1                   UxCPhelpContext->Uxmenu1
#define menu1_p1                UxCPhelpContext->Uxmenu1_p1
#define menu1_p1_b1             UxCPhelpContext->Uxmenu1_p1_b1
#define menu1_top_b1            UxCPhelpContext->Uxmenu1_top_b1
#define form1                   UxCPhelpContext->Uxform1
#define scrolledWindow1         UxCPhelpContext->UxscrolledWindow1
#define UxParent                UxCPhelpContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPhelp;
extern Widget	CPhelpText;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPhelp();

#endif	/* _CPHELP_INCLUDED */
