
/*******************************************************************************
       reqHelpDialog.h
       (Generated from interface file reqHelpDialog.i)
       This header file is included by reqHelpDialog.c

*******************************************************************************/

#ifndef	_REQHELPDIALOG_INCLUDED
#define	_REQHELPDIALOG_INCLUDED

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
} _UxCreqHelpDialog;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCreqHelpDialog       *UxReqHelpDialogContext;
#define UxParent                UxReqHelpDialogContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	reqHelpDialog;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_reqHelpDialog();

#endif	/* _REQHELPDIALOG_INCLUDED */
