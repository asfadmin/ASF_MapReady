static char *sccs = "@(#)ims_opBrowseDlgCb.c	5.1  03/17/96";
/*******************************************************************************

	File:			ims_opBrowseDlgCb.c

	Function:	Callback functions for browse dialog box 

	Author:		Jennifer Ting

	Date:			3/1995

*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <X11/Shell.h>
#include <UxXt.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

#define _IMS_OP_BROWSEDLGCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opBrowseDlg.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: browseDlg_okCb
**
** Description:		Callback function for the Done button in browseDlg,
**								destroys the browse dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void browseDlg_okCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCbrowseDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxBrowseDlgContext;
	UxBrowseDlgContext = UxContext =
			(_UxCbrowseDlg *) UxGetContext( UxWidget );
	{
		XtDestroyWidget(wgt);	
	}
	UxBrowseDlgContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: browseDlg_popupCb
**
** Description:		Callback function to popup the browse dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. text   - text to be displayed in the browse dlg.
**								3. cb 		- not used.
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void browseDlg_popupCb(
	Widget wgt, 
	char *text,
	char *label)
{
	_UxCbrowseDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	Widget browse_dlg_w;
	XmString label_text;


	browse_dlg_w = create_browseDlg(wgt);

	UxSaveCtx = UxBrowseDlgContext;
	UxBrowseDlgContext = UxContext =
		(_UxCbrowseDlg *) UxGetContext(browse_dlg_w);
	{
		label_text = XmStringCreateLocalized (label);
		XtVaSetValues (browseDlgLB, XmNlabelString, label_text, NULL);
		XmTextSetString (browseST, text);

		XtManageChild(browse_dlg_w);
		XmStringFree (label_text);

	}
	UxBrowseDlgContext = UxSaveCtx;

}
	
