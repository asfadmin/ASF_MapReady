
/*******************************************************************************
       ims_opCommentDlg.h
       This header file is included by ims_opCommentDlg.c

*******************************************************************************/

#ifndef	_IMS_OPCOMMENTDLG_INCLUDED
#define	_IMS_OPCOMMENTDLG_INCLUDED

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
	Widget	UxcommentDlg;
	Widget	UxcommentUpdatePB;
	Widget	UxcommentCancelPB;
	Widget	UxcommentSW;
	Widget	UxcommentST;
	Widget	UxcommentLB;
	Widget	UxcommentOldSW;
	Widget	UxcommentOldST;
	Widget	UxcommentOldLB;
	Widget	UxcommentNewLB;
	Widget	UxcommentLB1;
	swidget	UxUxParent;
} _UxCcommentDlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCcommentDlg          *UxCommentDlgContext;
#define commentDlg              UxCommentDlgContext->UxcommentDlg
#define commentUpdatePB         UxCommentDlgContext->UxcommentUpdatePB
#define commentCancelPB         UxCommentDlgContext->UxcommentCancelPB
#define commentSW               UxCommentDlgContext->UxcommentSW
#define commentST               UxCommentDlgContext->UxcommentST
#define commentLB               UxCommentDlgContext->UxcommentLB
#define commentOldSW            UxCommentDlgContext->UxcommentOldSW
#define commentOldST            UxCommentDlgContext->UxcommentOldST
#define commentOldLB            UxCommentDlgContext->UxcommentOldLB
#define commentNewLB            UxCommentDlgContext->UxcommentNewLB
#define commentLB1              UxCommentDlgContext->UxcommentLB1
#define UxParent                UxCommentDlgContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_commentDlg( swidget _UxUxParent );

#endif	/* _IMS_OPCOMMENTDLG_INCLUDED */
