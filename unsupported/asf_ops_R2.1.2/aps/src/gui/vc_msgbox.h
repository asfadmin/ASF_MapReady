
/*******************************************************************************
       vc_msgbox.h
       This header file is included by vc_msgbox.c

*******************************************************************************/

#ifndef	_VC_MSGBOX_INCLUDED
#define	_VC_MSGBOX_INCLUDED

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
	Widget	UxAPSMsgBoxDlg;
	Widget	UxscrolledWindowText_msgBox;
	Widget	UxscrolledText_msgBox;
	swidget	UxUxParent;
} _UxCAPSMsgBoxDlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSMsgBoxDlg        *UxAPSMsgBoxDlgContext;
#define APSMsgBoxDlg            UxAPSMsgBoxDlgContext->UxAPSMsgBoxDlg
#define scrolledWindowText_msgBox UxAPSMsgBoxDlgContext->UxscrolledWindowText_msgBox
#define scrolledText_msgBox     UxAPSMsgBoxDlgContext->UxscrolledText_msgBox
#define UxParent                UxAPSMsgBoxDlgContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSMsgBoxDlg( swidget _UxUxParent );

#endif	/* _VC_MSGBOX_INCLUDED */
