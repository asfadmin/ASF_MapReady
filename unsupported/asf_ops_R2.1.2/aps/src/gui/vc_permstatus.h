
/*******************************************************************************
       vc_permstatus.h
       This header file is included by vc_permstatus.c

*******************************************************************************/

#ifndef	_VC_PERMSTATUS_INCLUDED
#define	_VC_PERMSTATUS_INCLUDED

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
	Widget	UxMU_perm_scrolledWindow;
	Widget	UxMU_permStatusRefresh_pb;
	Widget	UxMU_Quit_pb;
	Widget	Uxlabel74;
	Widget	Uxlabel73;
	swidget	UxUxParent;
} _UxCMUPermissionStatus;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCMUPermissionStatus  *UxMUPermissionStatusContext;
#define MU_perm_scrolledWindow  UxMUPermissionStatusContext->UxMU_perm_scrolledWindow
#define MU_permStatusRefresh_pb UxMUPermissionStatusContext->UxMU_permStatusRefresh_pb
#define MU_Quit_pb              UxMUPermissionStatusContext->UxMU_Quit_pb
#define label74                 UxMUPermissionStatusContext->Uxlabel74
#define label73                 UxMUPermissionStatusContext->Uxlabel73
#define UxParent                UxMUPermissionStatusContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	MUPermissionStatus;
extern Widget	AutoPoll_tb;
extern Widget	MU_perm_scrolledList;
extern Widget	PollTime_lbl;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_MUPermissionStatus( swidget _UxUxParent );

#endif	/* _VC_PERMSTATUS_INCLUDED */
