
/*******************************************************************************
       vc_phaseselect.h
       This header file is included by vc_phaseselect.c

*******************************************************************************/

#ifndef	_VC_PHASESELECT_INCLUDED
#define	_VC_PHASESELECT_INCLUDED

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
	Widget	UxAPSPhaseSelection;
	Widget	Uxlabel72;
	Widget	UxscrolledWindowList9;
	Widget	UxscrolledList_REQQ;
	Widget	UxpushButton_QUIT1;
	swidget	UxUxParent;
} _UxCAPSPhaseSelection;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSPhaseSelection   *UxAPSPhaseSelectionContext;
#define APSPhaseSelection       UxAPSPhaseSelectionContext->UxAPSPhaseSelection
#define label72                 UxAPSPhaseSelectionContext->Uxlabel72
#define scrolledWindowList9     UxAPSPhaseSelectionContext->UxscrolledWindowList9
#define scrolledList_REQQ       UxAPSPhaseSelectionContext->UxscrolledList_REQQ
#define pushButton_QUIT1        UxAPSPhaseSelectionContext->UxpushButton_QUIT1
#define UxParent                UxAPSPhaseSelectionContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSPhaseSelection( swidget _UxUxParent );

#endif	/* _VC_PHASESELECT_INCLUDED */
