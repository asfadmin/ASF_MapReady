static char *sccs = "@(#)ims_op_accAccountUserDeleteDlgCb.c	5.1  03/17/96";
/*******************************************************************************
**
** File:		ims_op_accounts_users_delete_dlgCb.c
**
** Function:		Callback functions for the delete_dlg dialog
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
******************************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/MessageB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>


/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accDeleteDlg.h"
#undef CONTEXT_MACRO_ACCESS

void delete_dlgCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCdelete_dlg      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        SELECTED_BUTTON * select_ptr = *(( SELECTED_BUTTON * *) cd) ;
	UxSaveCtx = UxDelete_dlgContext;
	UxDelete_dlgContext = UxContext =
	  (_UxCdelete_dlg *) UxGetContext( UxWidget );
	{
         
          XmAnyCallbackStruct *cbs =(XmAnyCallbackStruct *)cb ;
          /*printf("at CB\n");*/
         if ( cbs->reason == XmCR_OK )
          *select_ptr = OK ;
         else
          *select_ptr = CANCEL ;
  
         XtRemoveCallback ( wgt, XmNcancelCallback,delete_dlgCb,
                           cd ) ;
         XtRemoveCallback ( wgt, XmNokCallback,delete_dlgCb,
                              cd ) ;
         
         XtUnmanageChild ( wgt ) ; 

	}
	UxDelete_dlgContext = UxSaveCtx;


}

void prepare_message_box (SELECTED_BUTTON ** selection , char * msg,
                           char * the_title ) {

 static int once = 0 ;

 XmString text = XmStringCreateLtoR (msg, XmFONTLIST_DEFAULT_TAG ) ;
 XmString title = XmStringCreateLocalized (the_title);
 
 XtVaSetValues ( UxGetWidget ( delete_dlg ),
	XmNdialogType, XmDIALOG_QUESTION,
	XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
	XmNmessageString,text,
	XmNdialogTitle, title,
	NULL ) ;
 /*if ( once == 0 ) {*/
 XtAddCallback (delete_dlg,XmNcancelCallback,
                (XtCallbackProc) delete_dlgCb,
                selection ) ;
 XtAddCallback (delete_dlg,XmNokCallback,
                (XtCallbackProc) delete_dlgCb,
                selection ) ;
  
 XmStringFree ( text ) ;
 XmStringFree ( title ) ;
 XtManageChild ( UxGetWidget ( delete_dlg ) ) ;
  

}
