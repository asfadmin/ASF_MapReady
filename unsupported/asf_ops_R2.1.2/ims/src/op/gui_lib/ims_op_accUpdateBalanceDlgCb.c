static char *sccs = "@(#)ims_op_accUpdateBalanceDlgCb.c	5.2  07/23/96";
/*******************************************************************************
**
** File:		ims_op_update_balance_dlgCb.c
**
** Function:		Callback functions for the update_balance_dlg screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Revision:  06/12/96   J. Ting   Modified function update_balance_dlg_okCb
**                                 for PR 942.
**
******************************************************************************/#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/SelectioB.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>
#include <ims_acct.h>

extern OP_GLOBAL_DATA glbData ;
extern Widget msg_dlg ;
extern void account_data_createCb ( 
			Widget ,XtPointer,XtPointer ) ;
static int update_balance ( char * , DBREAL ) ;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accUpdateBalanceDlg.h"
#undef CONTEXT_MACRO_ACCESS

/*************************************************************************
** NAME :	update_balance_dlg
**
** DESCRIPTION: Call back for the ok button of update_balance_dlg
**
** PARAMETERS: cd : 1 at creation time. 0 otherwise.
**
*************************************************************************/
void  update_balance_dlg_okCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCupdate_balance_dlg      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    int               UxClientData = (int) cd;
    XmSelectionBoxCallbackStruct *cbs = 
              (XmSelectionBoxCallbackStruct *) cb ;
    XmString string ;
    char * text;
    char * acc_id ;
    char * begin_b ;
    char * current_b ;
    float number, begin, current, hold ;
    char buffer [IMS_COL128_LEN+1] ;
    OP_ACCOUNT *acc_ptr  ;

    UxSaveCtx = UxUpdate_balance_dlgContext;
    UxUpdate_balance_dlgContext = UxContext =
	       (_UxCupdate_balance_dlg *) UxGetContext( UxWidget ) ;
    {
	if ( UxClientData == 1 ) /* create */
	{
	    XtVaSetValues (UxWidget, XmNdefaultButton, (Widget)NULL, NULL ) ;
	    return ;
	}
	else if (  UxClientData == 2 ) /* cancel */
	{
			/*
			** 06/13/96 - search for the toplevel shell, then destroy 
			** the widget.  This is done for PR 942.
			*/
	 	  while(wgt && !XtIsShell(wgt))
      wgt = XtParent(wgt);

	    XtUnmanageChild (wgt) ;
	    XtDestroyWidget (wgt) ;
	    return ;
	}

	/* ok button */
	XmStringGetLtoR ( cbs->value, XmFONTLIST_DEFAULT_TAG, &text) ;
	ims_truncStr(text) ;

	/* validate entry */
	if (!ims_isReal (text))
	{
	    XtFree (text);
	    return;
	}
	number = (float) atof (text ) ;
	XtFree (text);

	if ( number != 0.0 ) 
	{
	    /*
	    ** Widgets account_param, begin_param, current_param , and
	    ** hold_param are passed as parameters when
            ** create_update_balance_dlg is created and are visible now. 
	    */ 
	    acc_id = XmTextGetString (account_param) ;
	    begin_b = XmTextGetString (begin_param) ;
	    current_b = XmTextGetString (current_param) ;

	    ims_truncStr(acc_id) ;
	    ims_truncStr(begin_b) ;
	    ims_truncStr(current_b) ;

	    if ( update_balance  ( acc_id, number) != IMS_OK )
	    {
		XtFree (acc_id);  
		XtFree (begin_b);
		XtFree (current_b);    
                return;
	    }

	    /* 
	    ** Update balance fields since the update was successfull 
	    */
	    begin = (float) atof ( begin_b ) ;
	    begin += number ;
	    current = (float) atof ( current_b ) ;
	    current += number ;
	    sprintf(buffer,"%f",begin) ;
	    XmTextSetString( begin_param,buffer ) ; 
	    sprintf(buffer,"%f",current) ;
	    XmTextSetString( current_param,buffer ) ;  
	    /* sprintf(buffer,"%f",hold) ;
               XmTextSetString( hold_param,buffer ) ;  */

	    XtUnmanageChild (UxWidget ) ;
	    XtDestroyWidget (UxWidget) ;

	    acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr;
	    while ( (acc_ptr != (OP_ACCOUNT *) NULL )  ) 
	    {
                if ( !strcmp(acc_ptr->account_id, acc_id) ) 
		{
		    acc_ptr->begin_balance = begin ;
		    acc_ptr->curr_balance = current ;
		    /* acc_ptr->hold_balance = hold ; */
		    break ;
                }                
                acc_ptr = acc_ptr->next ;
	    }   
	    XtFree (acc_id);  
	    XtFree (begin_b);
	    XtFree (current_b);    
	}
	else if ( number == 0.0 ) 
	{
	    string = XmStringCreateLocalized ("") ;
	    XtVaSetValues (UxWidget, XmNtextString,string,NULL ) ;
	    XmStringFree (string) ;
	}
    }
    UxUpdate_balance_dlgContext = UxSaveCtx;
}

/*************************************************************************
** NAME :	update_balance
**
** DESCRIPTION: Updates the begin and current balances through calls to
**		the database functions.
**
** PARAMETERS:  IN = acc_id:account id. number:amount to be addedd to balances.
**
*************************************************************************/


static int update_balance ( char * acc_id, float number ) 
{
    OP_CAT_STRUCT *catReq;
    IMS_QI_DESC_OBJ *qDesc;
    IMS_MSG_STRUCT *msgDesc;
    IMS_ACCT_TRANS_TYPE type ;


    catReq = &(glbData.accounts_users_data.catReq);
    qDesc = catReq->qDesc;
    msgDesc = catReq->msgDesc;

    if (number < 0)
    {
	number *= -1.0 ;
	type = DEBIT ;
    }
    else
	type = CREDIT ;

    if (ims_acctTran (qDesc, msgDesc, acc_id, -1, number, type) < IMS_OK) 
    {
	msg_dlgCb (msg_dlg, IMS_ERROR, "Balance was not updated");
	return (IMS_ERROR) ; 
    }

    return IMS_OK ;

}








