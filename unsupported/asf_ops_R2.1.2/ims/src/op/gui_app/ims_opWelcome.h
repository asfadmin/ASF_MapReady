
/*******************************************************************************
       ims_opWelcome.h
       This header file is included by ims_opWelcome.c

*******************************************************************************/

#ifndef	_IMS_OPWELCOME_INCLUDED
#define	_IMS_OPWELCOME_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
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
	Widget	Uxwelcome;
	Widget	UxwelcomeTitleLB1;
	Widget	UxwelcomeTitleLB2;
	Widget	UxwelcomeTitleLB3;
	Widget	Uxalaska1LBL;
	Widget	UxphotoPB;
	Widget	UxaccountPB;
	Widget	UxorderPB;
	Widget	UxreportsPB;
	Widget	UxexitPB;
	Widget	Uxalaska2LBL;
	swidget	UxUxParent;
} _UxCwelcome;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCwelcome             *UxWelcomeContext;
#define welcome                 UxWelcomeContext->Uxwelcome
#define welcomeTitleLB1         UxWelcomeContext->UxwelcomeTitleLB1
#define welcomeTitleLB2         UxWelcomeContext->UxwelcomeTitleLB2
#define welcomeTitleLB3         UxWelcomeContext->UxwelcomeTitleLB3
#define alaska1LBL              UxWelcomeContext->Uxalaska1LBL
#define photoPB                 UxWelcomeContext->UxphotoPB
#define accountPB               UxWelcomeContext->UxaccountPB
#define orderPB                 UxWelcomeContext->UxorderPB
#define reportsPB               UxWelcomeContext->UxreportsPB
#define exitPB                  UxWelcomeContext->UxexitPB
#define alaska2LBL              UxWelcomeContext->Uxalaska2LBL
#define UxParent                UxWelcomeContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	dl2dtkPB;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_welcome( swidget _UxUxParent );

#endif	/* _IMS_OPWELCOME_INCLUDED */
