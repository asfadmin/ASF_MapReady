
/*******************************************************************************
       vc_muinterval.h
       This header file is included by vc_muinterval.c

*******************************************************************************/

#ifndef	_VC_MUINTERVAL_INCLUDED
#define	_VC_MUINTERVAL_INCLUDED

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
	Widget	UxMUInterval_dialogShell;
	Widget	UxMU_Interval_rc;
	Widget	Uxlabel42;
	Widget	Uxlabel62;
	swidget	UxUxParent;
} _UxCMUInterval_dialogShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCMUInterval_dialogShell *UxMUInterval_dialogShellContext;
#define MUInterval_dialogShell  UxMUInterval_dialogShellContext->UxMUInterval_dialogShell
#define MU_Interval_rc          UxMUInterval_dialogShellContext->UxMU_Interval_rc
#define label42                 UxMUInterval_dialogShellContext->Uxlabel42
#define label62                 UxMUInterval_dialogShellContext->Uxlabel62
#define UxParent                UxMUInterval_dialogShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	MUIntervalDialog;
extern Widget	Interval_tf;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_MUInterval_dialogShell( swidget _UxUxParent );

#endif	/* _VC_MUINTERVAL_INCLUDED */
