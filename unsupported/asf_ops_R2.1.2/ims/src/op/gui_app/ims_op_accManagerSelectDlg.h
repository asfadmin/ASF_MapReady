
/*******************************************************************************
       ims_op_accManagerSelectDlg.h
       This header file is included by ims_op_accManagerSelectDlg.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCMANAGERSELECTDLG_INCLUDED
#define	_IMS_OP_ACCMANAGERSELECTDLG_INCLUDED

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
	Widget	UxmanagerSelectionDlg;
	swidget	UxUxParent;
} _UxCmanagerSelectionDlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCmanagerSelectionDlg *UxManagerSelectionDlgContext;
#define managerSelectionDlg     UxManagerSelectionDlgContext->UxmanagerSelectionDlg
#define UxParent                UxManagerSelectionDlgContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_managerSelectionDlg( swidget _UxUxParent );

#endif	/* _IMS_OP_ACCMANAGERSELECTDLG_INCLUDED */
