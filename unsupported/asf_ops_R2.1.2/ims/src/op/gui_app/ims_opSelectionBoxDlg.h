
/*******************************************************************************
       ims_opSelectionBoxDlg.h
       This header file is included by ims_opSelectionBoxDlg.c

*******************************************************************************/

#ifndef	_IMS_OPSELECTIONBOXDLG_INCLUDED
#define	_IMS_OPSELECTIONBOXDLG_INCLUDED

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
	Widget	UxselectionBoxDlg;
	Widget	UxcommentFormW;
	Widget	Uxlabel37;
	Widget	Uxselection_commentSW;
	Widget	Uxselection_commentST;
	swidget	UxUxParent;
} _UxCselectionBoxDlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCselectionBoxDlg     *UxSelectionBoxDlgContext;
#define selectionBoxDlg         UxSelectionBoxDlgContext->UxselectionBoxDlg
#define commentFormW            UxSelectionBoxDlgContext->UxcommentFormW
#define label37                 UxSelectionBoxDlgContext->Uxlabel37
#define selection_commentSW     UxSelectionBoxDlgContext->Uxselection_commentSW
#define selection_commentST     UxSelectionBoxDlgContext->Uxselection_commentST
#define UxParent                UxSelectionBoxDlgContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_selectionBoxDlg( swidget _UxUxParent );

#endif	/* _IMS_OPSELECTIONBOXDLG_INCLUDED */
