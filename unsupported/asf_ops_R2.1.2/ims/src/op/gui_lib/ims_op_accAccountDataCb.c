static char *sccs = "@(#)ims_op_accAccountDataCb.c	5.6  09/15/97";
/*******************************************************************************
**
** File:		ims_op_accAccountDataCb.c
**
** Function:		Callback functions for the account_data screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Modified:
**
**   11/08/95   A. Tilden   Corrected file name.
**                          Added column names to insert statement that
**                          creates account and account manager data.
**                          Modified the error message, displayed in case 
**                          of create or update failures, to specify 
**                          the missing fields.
**                          Added quicklook widget and callback.
**
**   06/12/96   J. Ting     Modified function account_data_closeCb for PR 942.
**   07/09/96   J. Ting     Modified function account_data_createCb.
**   09/06/96   J. Ting     Modified function get_fields_data and function
**                          create_account_data_query function for PR 84.
**   09/16/96   J. Ting     Modified set_account_data for PR 43.  Allow the
**                          GDC operator to change the Account type.
**   10/07/96   J. Ting     R2.1 - take out the account manager info.
**   10/14/96   J. Ting     R2.1 - take out the quicklook_p field to 
**                                 match schema version 3.50.
******************************************************************************/
#include <stdio.h>
#include <time.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/ToggleB.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

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
#include <ims_op_accCb.h>

extern void free_account_data ( ) ;
extern int execute_search_account_query ( ) ;
extern void prepare_message_box (SELECTED_BUTTON ** , char *, char *  ) ;
extern Widget update_balance_dlg ;
extern OP_GLOBAL_DATA glbData ;
extern Widget msg_dlg  ;
extern void msg_dlgCb (Widget, int, char * ) ;
extern is_date_valid (char* ) ;


static char * id = (char*) NULL ;
static char * type = (char*) NULL ;
static char * resource = (char*) NULL ;
static char * begin_balance = (char*) NULL ;
static char * current_balance = (char*) NULL ;
static char * on_hold = (char*) NULL ;
static char * creation = (char*) NULL ;
static char * expiration = (char*) NULL ;
static char * rate = (char*) NULL ;
static char * manager = (char*) NULL ;
static char  validation[2] = { 'Y', '\0'} ;
/*********************************************
static char  quicklook[2]  = { 'N', '\0'} ;
*********************************************/
static char * special_proc = (char*) NULL ;
static char * comments = (char*) NULL ;
static char * mgr_user_id = (char*) NULL ;

static void create_account_data_query (ACCOUNT_USER_DATA) ;
static int execute_account_data_query (ACCOUNT_USER_DATA) ;
static int get_fields_data (Widget) ;
static void free_string_data ( ) ;
static int is_time_valid ( char * ) ;

/******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAccountData.h"
#undef CONTEXT_MACRO_ACCESS

/* This callback is used to set the label of the screen and called at   */
/* creation time.                                                       */
void  account_data_labelCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
          /* create is a variable defined with this context */
          /* It is passed as a parameter when this interface is created */
          if ( create == CREATE ){
	   XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
	     "Create Account", 15, NULL ) ;
	   XtVaSetValues ( XtParent ( wgt ), XmNtitle, 
              "Create Account", NULL ) ;
          }
          else if ( create == DATA ) {
	   XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
	     "Account Data", 13, NULL ) ;
	   XtVaSetValues ( XtParent ( wgt ), XmNtitle, 
             "Account Data", NULL ) ;
          }

	  
	}
	UxAccount_dataContext = UxSaveCtx;


}



/*===========================================================================*
** 
** Function Name: account_data_closeCb
**
** Description:		callback function for close button and mwm quit button.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - look for the toplevel shell first, 
**                              use XtDestroyWidget (wgt) instead of
**                              XtDestroyWidget (XtParent(XtParent(wgt)))
**
**==========================================================================*/
void account_data_closeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        static SELECTED_BUTTON selected  ;
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
        /* prepare message box */
       /***
           selected = NOTHING ;
           selected_ptr = &selected ;
           prepare_message_box (&selected_ptr,
            "The account data has been modified.\nDo you really want to close?",
             "Closing Account Data");
           
           while ( selected == NOTHING) {
                         XtAppProcessEvent ( UxAppContext, XtIMAll);
           }
           if ( selected == OK)
            XtDestroyWidget( XtParent ( XtParent ( wgt )  ) );
        ***/	

           free_string_data ( ) ;
			  	 while(wgt && !XtIsShell(wgt))
    		    wgt = XtParent(wgt);

	         XtDestroyWidget( wgt );

	}
	UxAccount_dataContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: account_data_createCb
**
** Description:		callback function for the Create (Update) button
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 10/03/96 - take out all account manager related fields.
**
**==========================================================================*/
void account_data_createCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int               UxClientData = (int) cd;
	XtPointer               UxCallbackArg = cb;
	int status ;
        XmScrollBarCallbackStruct *cbs ;
	char err_msg[IMS_COL128_LEN+1] ;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
         if ( UxClientData ) { /* Widget creation */
            if ( create == CREATE ){ /* create account */
	     XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
	     "Create", 7, NULL ) ;
            }
            else {
              XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
 	     "Update", 7, NULL ) ;
            }
            return ;
         }
         /* Get data */
         if ( get_fields_data ( UxWidget) < IMS_OK )
           return ;
	 /* check fields that can not be NULL */
         if ( !( id && *id ) || !( type && *type ) || 
               !( resource && *resource ) || !( creation && *creation ) ||
               !( rate && *rate ) || 
               !( begin_balance && *begin_balance ) || 
               !( current_balance && *current_balance ) ||
               !( on_hold && *on_hold ) ||
							 !( manager && *manager)) 
	 {
	    err_msg[0] = '\0' ;
	    if ( !( id && *id ) )
	      strcat ( err_msg, "ID, " ) ;
	    if ( !( type && *type ) )
	     strcat ( err_msg, "Type, " ) ;
	    if ( !( resource && *resource ) )
	      strcat ( err_msg, "Resource, " ) ;
	    if ( !( begin_balance && *begin_balance ) )
	      strcat ( err_msg, "Begin_Balance, " ) ;
	    if ( !( current_balance && *current_balance ) )
	      strcat ( err_msg, "Current_Balance, " ) ;
	    if ( !( on_hold && *on_hold ) )
	      strcat ( err_msg, "On_Hold, " ) ;
	    if ( !( creation && *creation ) )
	      strcat ( err_msg, "Creation_Time, " ) ;
	    if ( !( rate && *rate ) )
	      strcat ( err_msg, "Rate, " ) ;
	    if ( !( manager && *manager ) )
	      strcat ( err_msg, "Manager, " ) ;
	    strcat ( err_msg, "cannot be empty." ) ;

	    msg_dlgCb (msg_dlg, IMS_ERROR, err_msg ) ;
	    return ;
          } 
          if ( create == CREATE ){
            create_account_data_query ( CREATE ) ;
            if ( (status = execute_account_data_query (CREATE ) ) < IMS_OK )
              return ;
          }
          else {/* update */
            create_account_data_query ( DATA ) ;
            if ( (status = execute_account_data_query (DATA) ) < IMS_OK )
              return ;
          }
          /*update account list */
          if ( glbData.accounts_users_data.account_data.accounts_cnt > 0 ) {
              free_account_data ( ) ;
              if (  execute_search_account_query ( ) < IMS_OK ) {
               return ;
              }
              cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
              cbs->value = 0 ; 
              accounts_users_accounts_sbCb ((Widget)NULL,NULL,(XtPointer)cbs ) ;
              XtFree ((char *)cbs) ; 
          }
	}
	UxAccount_dataContext = UxSaveCtx;

	(void) account_data_closeCb ( wgt, cd, cb ) ;
}

/* Callback for resources option menu */
void account_data_option_resourcesCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        int UxData = (int) cd ;
        XmString button_label ;
        char *text ;
	Dimension  marginLeft, marginRight;
	Dimension  marginWidth;
	Pixel      selectColor;
	Pixel      background;
	XmString   label;
	Widget		 menu_item;
	Dimension  indicatorSize;
	XmFontList fontList;
	int i ;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
          if ( UxData) { /* creation */
             
            /* Get resources */
            XtVaGetValues ( UxWidget, 
		                 XmNfontList, &fontList,
				 XmNbackground, &background,
				 XmNmarginLeft, &marginLeft,
				 XmNmarginRight, &marginRight,
				 XmNmarginWidth, &marginWidth, NULL ) ;
            XtUnmanageChild (UxWidget ) ;
            for ( i = 0 ; i < glbData.resource_type_count ; i++ ) {
			label = XmStringCreateLocalized  
                (glbData.resource_type[i].item_name);
		menu_item = XtVaCreateManagedWidget 
                  ((char *)label, xmPushButtonWidgetClass, 
                  option_menu_pane_resource,
                  XmNlabelString, label,
		  XmNfontList, fontList,
		  XmNbackground, background,
		  XmNmarginLeft, marginLeft,
		  XmNmarginRight, marginRight,
                  XmNmarginWidth, marginWidth,	
		  NULL);

		  XtAddCallback (menu_item, XmNactivateCallback,
			(XtCallbackProc) account_data_option_resourcesCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxAccount_dataContext );

                XmStringFree (label);
                if ( i == 0 ) {
                  XtVaGetValues ( menu_item, XmNlabelString, &button_label,
                                                                     NULL ) ;
                  if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
                     XmTextSetString (  resourceTF  , text ) ;
                     XtFree ( text ) ;
                  }
                  XmStringFree ( button_label ) ;
                }      
            }
          }/* end creation */
          else {
            XtVaGetValues ( UxWidget, XmNlabelString, &button_label,NULL ) ;
            if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
              XmTextSetString ( resourceTF  , text ) ;
              XtFree ( text ) ;
            }
            XmStringFree ( button_label ) ;
          }      
	}
	UxAccount_dataContext = UxSaveCtx;


}

/* Callback for type option menu */
void account_data_option_typeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        int UxData = (int) cd ;
        XmString button_label ;
        char *text ;
	Dimension  marginLeft, marginRight;
	Dimension  marginWidth;
	Pixel      selectColor;
	Pixel      background;
	XmString   label;
	Widget		 menu_item;
	Dimension  indicatorSize;
	XmFontList fontList;
	int i ;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
          if ( UxData) { /* creation */
             
            /* Get resources */
            XtVaGetValues ( UxWidget, 
		                 XmNfontList, &fontList,
				 XmNbackground, &background,
				 XmNmarginLeft, &marginLeft,
				 XmNmarginRight, &marginRight,
				 XmNmarginWidth, &marginWidth, NULL ) ;
            XtUnmanageChild (UxWidget ) ;
            for ( i = 0 ; i < glbData.account_type_count ; i++ ) {
			label = XmStringCreateLocalized  
                (glbData.account_type[i].item_name);
		menu_item = XtVaCreateManagedWidget 
                  ((char *)label, xmPushButtonWidgetClass, 
                  option_menu_pane_type,
                  XmNlabelString, label,
		  XmNfontList, fontList,
		  XmNbackground, background,
		  XmNmarginLeft, marginLeft,
		  XmNmarginRight, marginRight,
                  XmNmarginWidth, marginWidth,	
		  NULL);

		  XtAddCallback (menu_item, XmNactivateCallback,
			(XtCallbackProc) account_data_option_typeCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxAccount_dataContext );

                XmStringFree (label);
                if ( i == 0 ) {
                  XtVaGetValues ( menu_item, XmNlabelString, &button_label,
                                                                     NULL ) ;
                  if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
                     XmTextSetString (  typeTF  , text ) ;
                     XtFree ( text ) ;
                  }
                  XmStringFree ( button_label ) ;
                }      
            }
          }/* end creation */
          else {
            XtVaGetValues ( UxWidget, XmNlabelString, &button_label,NULL ) ;
            if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
              XmTextSetString ( typeTF  , text ) ;
              XtFree ( text ) ;
            }
            XmStringFree ( button_label ) ;

          }            
	}
	UxAccount_dataContext = UxSaveCtx;


}
/* not used yet. Special proc option menu */
void account_data_option_special_procCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        XmString button_label ;
        char *text ;
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
          /*** XtVaGetValues ( UxWidget, XmNlabelString, &button_label,NULL ) ;
          if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
            XmTextSetString (  ( procTF ) , text ) ;
            XtFree ( text ) ;
          }
          XmStringFree ( button_label ) ;
          ***/
	}
	UxAccount_dataContext = UxSaveCtx;


}


/* Create Callback for the current_balanceTF */
void account_data_current_balanceCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
 
	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int ) cd;
	XtPointer               UxCallbackArg = cb;
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
             if ( UxClientData == 1 )  {
               if ( create == CREATE )
                   XtVaSetValues ( UxWidget, XmNeditable, True, NULL ) ;
               else
                   XtVaSetValues ( UxWidget, XmNeditable, False, NULL ) ;
               
             }
                             
               
	}
	UxAccount_dataContext = UxSaveCtx;


}

void account_data_begin_balanceCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int) cd;
	
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
           if ( UxClientData == 1 )  {
               if ( create == CREATE )
                   XtVaSetValues ( UxWidget, XmNeditable, True, NULL ) ;
               else
                 XtVaSetValues ( UxWidget, XmNeditable, False, NULL ) ;
               
           }
                             
               
	}
	UxAccount_dataContext = UxSaveCtx;


}

void account_data_on_holdCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int) cd;
	
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
             if ( UxClientData == 1 )  {
               if ( create == CREATE )
                   XtVaSetValues ( UxWidget, XmNeditable, True, NULL ) ;
               else
                   XtVaSetValues ( UxWidget, XmNeditable, False, NULL ) ;
               
             }
                             
               
	}
	UxAccount_dataContext = UxSaveCtx;


}


void account_data_account_idCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int) cd;
	
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
             if ( UxClientData == 1 )  {
               if ( create == CREATE )
                   XtVaSetValues ( UxWidget, XmNeditable, True, NULL ) ;
               else
                   XtVaSetValues ( UxWidget, XmNeditable, False, NULL ) ;
               
             }
                             
               
	}
	UxAccount_dataContext = UxSaveCtx;


}


/* Create and activate Callback for the update_balancePB */
void account_data_update_balanceCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccount_data      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    int               UxClientData = (int) cd;
    XmPushButtonCallbackStruct *  UxCallbackArg =
	(XmPushButtonCallbackStruct *) cb;
        
    UxSaveCtx = UxAccount_dataContext;
    UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
    {
	if ( UxClientData == 1 )  
	{
	    if ( create == CREATE )
		XtSetSensitive ( UxWidget, False ) ;
	    else
		XtSetSensitive ( UxWidget, True ) ; 
	}

	if ( UxCallbackArg != NULL ) 
	    if ( UxCallbackArg->reason == XmCR_ACTIVATE ) 
	    {
	       create_update_balance_dlg ( NO_PARENT, idTF,
		     		begin_balanceTF, current_balanceTF, on_holdTF);
	/*
	** 06/13/96 - PR942
	** This is to add the callbacks to the window manager quit
	** button for each screen, this is to correct PR 942
	*/
	addWinMgrCloseCB (update_balance_dlg, update_balance_dlg_okCb, 2);

                XtManageChild (  ( update_balance_dlg ) ) ;
	    }
               
    }
    UxAccount_dataContext = UxSaveCtx;

}



/* Callback for the yesTB and noTB */
void account_data_validationCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int) cd;
	XmToggleButtonCallbackStruct *UxCallbackArg = 
                            ( XmToggleButtonCallbackStruct * ) cb;
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
           if ( UxClientData == 1 ) {
              if ( UxCallbackArg->set )
                 validation[0] = 'Y' ;
              else
	         validation[0] = 'N' ;             
           }
        }
	UxAccount_dataContext = UxSaveCtx;


}


/********************************************************************
** 10/14/96 - quicklook field is taken out for schema 3.50.
** Callback for the yesTB1 and noTB1 
void account_data_quicklookCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                 UxWidget = wgt;
	int                    UxClientData = (int) cd;
	XmToggleButtonCallbackStruct *UxCallbackArg = 
                            ( XmToggleButtonCallbackStruct * ) cb;
        
	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
           if ( UxClientData == 1 ) {
              if ( UxCallbackArg->set )
                 quicklook[0] = 'Y' ;
              else
	         	 quicklook[0] = 'N' ;             
           }
        }
	UxAccount_dataContext = UxSaveCtx;


}
********************************************************************/

/* Not used */
void  account_data_cancelCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
            XtDestroyWidget( XtParent ( XtParent ( wgt )  ) );

	}
	UxAccount_dataContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: get_fields_data
**
** Description:		obtain data from each data field
**
** Revision History: 09/06/96 - Apostrophe is allowed only in comments and
**                              address fields - this is to correct PR 84.
**
**                   10/07/96 - Modified to take out the account manager info.
**                   10/14/96 - Modified to add mgr_user_id.
**
**==========================================================================*/
static int get_fields_data ( 
			Widget wgt)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        char ex_date [12], ex_time[10] ;
        int len ;
	char buffer[20] ;
	XmString string ;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext(UxWidget ) ;
	{

	      free_string_data ( ) ;

	      id = XmTextGetString ( idTF ) ;  
	      ims_truncStr(id) ;
				if (isApostrophe(id))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Account Id!"); 
          return (IMS_ERROR) ;
				}

	      type = XmTextGetString ( typeTF ) ; 
	      ims_truncStr(type) ;
	      resource = XmTextGetString ( resourceTF ) ; 
	      ims_truncStr(resource) ;
	      begin_balance = XmTextGetString ( begin_balanceTF ) ; 
	      ims_truncStr(begin_balance) ;
	      current_balance = XmTextGetString ( current_balanceTF ) ; 
	      ims_truncStr(current_balance) ;
	      on_hold = XmTextGetString ( on_holdTF ) ; 
	      ims_truncStr(on_hold) ;
	      creation = XmTextGetString ( creationTF ) ; 
	      ims_truncStr(creation) ;
	      expiration = XmTextGetString ( expirationTF ) ; 
	      ims_truncStr(expiration) ;

              len = strlen ( expiration ) ;
              if ( len > 10 ) {
                strncpy (ex_date, expiration, 10 ) ;
                ex_date[10] ='\0' ;
              }
              else
                strcpy ( ex_date, expiration ) ;
              if ((is_date_valid (ex_date)) < IMS_OK) {
                /*strcpy (buffer,"0000-00-00 00:00:00" ) ;*/
                XmTextSetString (expirationTF, ""/*0000-00-00 00:00:00"*/);
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Date!"); 
                return (IMS_ERROR) ;
              }
              if ( len > 11 ) {
                strcpy (ex_time, &expiration[11]) ;
	        if ((is_time_valid (ex_time)) < IMS_OK) {
		  msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Time!"); 
                  XmTextSetString (expirationTF, ""/*0000-00-00 00:00:00"*/);
                  return (IMS_ERROR) ;
                }
              }
              
	      rate = XmTextGetString ( rateTF ) ; 
	      ims_truncStr(rate) ;
	      manager = XmTextGetString ( managerTF ) ; 
	      ims_truncStr(manager) ;
	      special_proc = XmTextGetString ( procTF ) ; 
	      ims_truncStr(special_proc) ;
	      comments = XmTextGetString ( commentsST ) ; 
	      ims_truncStr(comments) ;
	      mgr_user_id = XmTextGetString ( managerTF ) ; 
	      ims_truncStr(mgr_user_id) ;

				/*************************************************************
	      first_name = XmTextGetString ( first_nameTF ) ; 
	      ims_truncStr(first_name) ;
				if (isApostrophe(first_name))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid First Name!"); 
          return (IMS_ERROR) ;
				}
	      m_i = XmTextGetString ( m_iTF ) ; 
	      ims_truncStr(m_i) ;
				if (isApostrophe(m_i))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Middle Initial!"); 
          return (IMS_ERROR) ;
				}
	      last_name = XmTextGetString ( last_nameTF ) ; 
	      ims_truncStr(last_name) ;
				if (isApostrophe(last_name))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Last Name!"); 
          return (IMS_ERROR) ;
				}
	      title = XmTextGetString ( titleTF ) ; 
	      ims_truncStr(title) ;
				if (isApostrophe(title))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Title!"); 
          return (IMS_ERROR) ;
				}
	      organization = XmTextGetString ( organizationTF ) ; 
	      ims_truncStr(organization) ;
				if (isApostrophe(organization))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Organization!"); 
          return (IMS_ERROR) ;
				}
	      address = XmTextGetString ( addressTF ) ; 
	      ims_truncStr(address) ;
	      city = XmTextGetString ( cityTF ) ;
	      ims_truncStr(city) ;
				if (isApostrophe(city))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid City!"); 
          return (IMS_ERROR) ;
				}
	      state = XmTextGetString ( stateTF ) ; 
	      ims_truncStr(state) ;
				if (isApostrophe(state))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid State!"); 
          return (IMS_ERROR) ;
				}
	      country = XmTextGetString ( countryTF ) ; 
	      ims_truncStr(country) ;
				if (isApostrophe(country))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Country!"); 
          return (IMS_ERROR) ;
				}
	      zip = XmTextGetString ( zipTF ) ; 
	      ims_truncStr(zip) ;
				if (isApostrophe(zip))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Zip Code!"); 
          return (IMS_ERROR) ;
				}
	      phone = XmTextGetString ( phoneTF ) ; 
	      ims_truncStr(phone) ;
				if (isApostrophe(phone))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Phone Number!"); 
          return (IMS_ERROR) ;
				}
	      fax = XmTextGetString ( faxTF ) ;
	      ims_truncStr(fax) ; 
				if (isApostrophe(fax))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Fax Number!"); 
          return (IMS_ERROR) ;
				}
 	      email = XmTextGetString ( emailTF ) ; 
	      ims_truncStr(email) ;
				if (isApostrophe(email))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Email!"); 
          return (IMS_ERROR) ;
				}
        **************************************************************/

	}
	UxAccount_dataContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: free_string_data
**
** Revision History: 10/07/96 - Modified to take out the account manager info.
**                   10/14/96 - Modified to add mgr_user_id field.
**
**==========================================================================*/
static void free_string_data ( ) {
	
	if ( id !=NULL ) {
	  XtFree( id ) ;
	  id = ( char * ) NULL ;
	}
	if ( type !=NULL ) {
	  XtFree( type ) ;
	  type = ( char * ) NULL ;
	}
	if ( resource !=NULL ) {
	  XtFree( resource ) ;
	  resource = ( char * ) NULL ;
	}
	if ( begin_balance !=NULL ) {
	  XtFree( begin_balance ) ;
	  begin_balance = ( char * ) NULL ;
	}
	if ( current_balance !=NULL ) {
	  XtFree( current_balance ) ;
	  current_balance = ( char * ) NULL ;
	}
	if ( on_hold !=NULL ) {
	  XtFree( on_hold ) ;
	  on_hold = ( char * ) NULL ;
	}
	if ( creation !=NULL ) {
	  XtFree( creation ) ;
	  creation = ( char * ) NULL ;
	}
	if ( expiration !=NULL ) {
	  XtFree( expiration ) ;
	  expiration = ( char * ) NULL ;
	}
	if ( rate !=NULL ) {
	  XtFree( rate ) ;
	  rate = ( char * ) NULL ;
	}
	if ( manager !=NULL ) {
	  XtFree( manager ) ;
	  manager = ( char * ) NULL ;
	}
	if ( special_proc !=NULL ) {
	  XtFree( special_proc ) ;
	  special_proc = ( char * ) NULL ;
	}
	if ( comments !=NULL ) {
	  XtFree( comments ) ;
	  comments = ( char * ) NULL ;
	}
	if ( mgr_user_id !=NULL ) {
	  XtFree( mgr_user_id ) ;
	  mgr_user_id = ( char * ) NULL ;
	}

	/***************************************
	if ( first_name !=NULL ) {
	  XtFree( first_name ) ;
	  first_name = ( char * ) NULL ;
	}
	if ( m_i !=NULL ) {
	  XtFree( m_i ) ;
	  m_i = ( char * ) NULL ;
	}
	if ( last_name !=NULL ) {
	  XtFree( last_name ) ;
	  last_name = ( char * ) NULL ;
	}
	if ( title !=NULL ) {
	  XtFree( title ) ;
	  title = ( char * ) NULL ;
	}
	if ( organization !=NULL ) {
	  XtFree( organization ) ;
	  organization = ( char * ) NULL ;
	}
	if ( address !=NULL ) {
	  XtFree( address ) ;
	  address = ( char * ) NULL ;
	}
	if ( city !=NULL ) {
	  XtFree( city ) ;
	  city = ( char * ) NULL ;
	}
	if ( state !=NULL ) {
	  XtFree( state ) ;
	  state = ( char * ) NULL ;
	}
	if ( country !=NULL ) {
	  XtFree( country ) ;
	  country = ( char * ) NULL ;
	}
	if ( zip !=NULL ) {
	  XtFree( zip ) ;
	  zip = ( char * ) NULL ;
	}
	if ( phone !=NULL ) {
	  XtFree( phone ) ;
	  phone = ( char * ) NULL ;
	}
	if ( fax !=NULL ) {
	  XtFree( fax ) ;
	  fax = ( char * ) NULL ;
	}
	if ( email !=NULL ) {
	  XtFree( email ) ;
	  email = ( char * ) NULL ;
	}
	*********************************/

}


/* Not used but left here if needed */
void account_data_losing_focusCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int widget_num  = (int) cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
       switch ( widget_num ) {
            case 1 :
	      id = XmTextGetString ( idTF ) ;  
              break ;
            case 2 :
	      type = XmTextGetString ( typeTF ) ; 
              break ;
            case 3 :
	      resource = XmTextGetString ( resourceTF ) ; 
              break ;
            case 4 :
	      begin_balance = XmTextGetString ( begin_balanceTF ) ; 
              break ;
            case 5 :
	      current_balance = XmTextGetString ( current_balanceTF ) ; 
              break ;
            case 6 :
	      on_hold = XmTextGetString ( on_holdTF ) ; 
              break ;
            case 7 :
	      creation = XmTextGetString ( creationTF ) ; 
              break ;
            case 8 :
	      expiration = XmTextGetString ( expirationTF ) ; 
              break ;
            case 9 :
	      rate = XmTextGetString ( rateTF ) ; 
              break ;
            case 10 :
	      special_proc = XmTextGetString ( procTF ) ; 
              break ;
            case 11 :
	      comments = XmTextGetString ( commentsST ) ; 
              break ;
/***********************************************************
            case 12 :
	      first_name = XmTextGetString ( first_nameTF ) ; 
              break ;
            case 13 :
	      m_i = XmTextGetString ( m_iTF ) ; 
              break ;
            case 14 :
	      last_name = XmTextGetString ( last_nameTF ) ; 
              break ;
            case 15 :
	      title = XmTextGetString ( titleTF ) ; 
              break ;
            case 16 :
	      organization = XmTextGetString ( organizationTF ) ; 
              break ;
            case 17 :
	      address = XmTextGetString ( typeTF ) ; 
              break ;
            case 18 :
	      city = XmTextGetString ( cityTF ) ;
              break ;
            case 19 :
	      state = XmTextGetString ( stateTF ) ; 
              break ;
            case 20 :
	      country = XmTextGetString ( countryTF ) ; 
              break ;
            case 21 :
	      zip = XmTextGetString ( zipTF ) ; 
              break ;
            case 22 :
	      phone = XmTextGetString ( phoneTF ) ; 
              break ;
            case 23 :
	      fax = XmTextGetString ( faxTF ) ; 
              break ;
            case 24 :
	      email = XmTextGetString ( emailTF ) ; 
              break ;
***************************************************************/
            default :
              break ;
          }

	}
	UxAccount_dataContext = UxSaveCtx;


}

/* Focus callback for creationTF */
void account_data_creation_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        
        char time_str [30] ;
        time_t epoch_time = time ( NULL ) ;        
        struct tm * broken_time  = localtime ( & epoch_time ) ;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
        if ( create == CREATE ) {

          sprintf(time_str, "%4d-%02d-%02d  %02d:%02d:%02d\0",
               broken_time->tm_year + 1900, broken_time->tm_mon + 1,
               broken_time->tm_mday, broken_time->tm_hour,
               broken_time->tm_min, broken_time->tm_sec) ;
          XmTextSetString (  UxWidget , time_str ) ;
        }
          
	}
	UxAccount_dataContext = UxSaveCtx;

}

/* Focus callback for current_balanceTF */
void account_data_current_balance_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccount_data       *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer		        UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
    char                   *begin_bal_str;
    char                   *current_bal_str;

    UxSaveCtx = UxAccount_dataContext;
    UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
    {
        if ( create == CREATE ) 
	{
	    current_bal_str = XmTextGetString (current_balanceTF);
	    ims_truncStr (current_bal_str);
	    if ((current_bal_str) && !(*current_bal_str))
	    {
		begin_bal_str = XmTextGetString (begin_balanceTF);
		ims_truncStr (begin_bal_str);
		if (begin_bal_str)
		{
		    XmTextSetString (  UxWidget , begin_bal_str );
		    XtFree (begin_bal_str);
		}
		XtFree (current_bal_str);
	    }
	}
    }
    UxAccount_dataContext = UxSaveCtx;

}

/* Focus callback for on_holdTF */
void account_data_on_hold_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccount_data       *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
    char                   *on_hold_val;
    char                    on_hold_str[26];

    UxSaveCtx = UxAccount_dataContext;
    UxAccount_dataContext = UxContext =
	     (_UxCaccount_data *) UxGetContext( UxWidget ) ;
    {
        if ( create == CREATE ) 
	{
	    on_hold_val = XmTextGetString (on_holdTF);
	    ims_truncStr (on_hold_val);
	    if ((on_hold_val) && !( *on_hold_val ) )
	    {
		sprintf (on_hold_str, "0.00\0") ;
		XmTextSetString (  UxWidget , on_hold_str ) ;
		XtFree (on_hold_val);
	    }
        }
          
    }
    UxAccount_dataContext = UxSaveCtx;

}


/* Focus callback for rateTF */
void account_data_rate_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccount_data      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
    char                   *rate_val;    
    char                    rate_str[26];

    UxSaveCtx = UxAccount_dataContext;
    UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
    {
        if ( create == CREATE ) 
	{
	    rate_val = XmTextGetString (rateTF);
	    ims_truncStr (rate_val);
	    if ((rate_val) && !( *rate_val ) )
	    {
		sprintf (rate_str, "1.00\0") ;
		XmTextSetString (  UxWidget , rate_str ) ;
		XtFree (rate_val);
	    }
        }
          
    }
    UxAccount_dataContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: set_account_data
**
** Revision History: 10/07/96 - Modified to take out the account manager info.
**                   10/14/96 - Modified to take out the quicklook_p field
**                              and added managerTF field.
**==========================================================================*/
void set_account_data ( Widget wgt ) {

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
  OP_ACCOUNT *acc_ptr  ;
  char buffer[IMS_COL255_LEN+1] ;
  int child_num,i , j ;
  WidgetList widget_list ;
  char * text ;
  XmString button_label ;
  char name[IMS_COL30_LEN+1];

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;

  acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;
  /* find selected account */
  while ( (acc_ptr != (OP_ACCOUNT *) NULL ) &&
                    (acc_ptr->selected == 0) )
    acc_ptr = acc_ptr->next ;
  if ( acc_ptr == (OP_ACCOUNT *) NULL ) {
    msg_dlgCb (msg_dlg, IMS_ERROR, "No Account Data!");
  }
  else {

    /* get button labels from option menu pane type */
    XtVaGetValues ( option_menu_pane_type, XmNnumChildren, &child_num,
                     XmNchildren, &widget_list, NULL ) ;
    name[0] = '\0' ;
    for ( j=0; j< glbData.account_type_count; j++ ) {
        if ( glbData.account_type[j].item_id == acc_ptr->account_type){
             strcpy ( name, glbData.account_type[j].item_name ) ;
             break ;
        }
    }
    if ( name[0] != '\0' ) {
      for ( i=0 ; i < child_num ; i++ ) {
         XtVaGetValues ( widget_list[i], XmNlabelString, &button_label,NULL ) ;
         if ( XmStringGetLtoR ( button_label, XmFONTLIST_DEFAULT_TAG, &text)) {
           if ( !strcmp(name,text ) ) {
             /* set option menu button */
             XtVaSetValues (option_menu_pane_type , XmNmenuHistory,
                                              widget_list[i], NULL ) ;      
             XtFree ( text ) ;
             break ;
           }   
           XtFree ( text ) ;
         }
      }
    }
    XmTextSetString( typeTF,name ) ;

		/*
		** 09/16/96 - Modified for PR 43.  Allow the GDC operator to 
		** update the account type of an account after it is created.
		**
    ** XtSetSensitive ( option_menu_type, False );
		*/

    /* get button labels from option menu pane resource */
    XtVaGetValues ( option_menu_pane_resource, XmNnumChildren, &child_num,
                     XmNchildren, &widget_list, NULL ) ;
    name[0] = '\0' ;
    for ( j=0; j< glbData.resource_type_count; j++ ) {
        if ( glbData.resource_type[j].item_id == acc_ptr->resource_type){
             strcpy ( name, glbData.resource_type[j].item_name ) ;
             break ;
        }
    }
    if ( name[0] != '\0' ) {
      for ( i=0 ; i < child_num ; i++ ) {
         XtVaGetValues ( widget_list[i], XmNlabelString, &button_label,NULL ) ;
         if ( XmStringGetLtoR ( button_label, XmFONTLIST_DEFAULT_TAG, &text)) {
           if ( !strcmp(name,text ) ) {
             /* set option menu button */
             XtVaSetValues (option_menu_pane_resource , XmNmenuHistory,
                                              widget_list[i], NULL ) ;      
             XtFree ( text ) ;
             break ;
           }   
           XtFree ( text ) ;
         }
      }
    }
    XmTextSetString( resourceTF,name ) ;
    XtSetSensitive ( option_menu_resource, False );

    XmTextSetString( idTF,acc_ptr->account_id ) ;
    XmTextSetString( creationTF,acc_ptr->create_time ) ;
    XmTextSetString( expirationTF,acc_ptr->expire_time ) ;
    sprintf(buffer,"%f\0",acc_ptr->rate_multiplier ) ;
    XmTextSetString( rateTF,buffer ) ;  
    sprintf(buffer,"%f\0",acc_ptr->begin_balance ) ;
    XmTextSetString( begin_balanceTF,buffer ) ;
    sprintf(buffer,"%f\0",acc_ptr->curr_balance ) ;
    XmTextSetString( current_balanceTF,buffer ) ;
    sprintf(buffer,"%f\0",acc_ptr->hold_balance ) ;
    XmTextSetString( on_holdTF,buffer ) ;
    if ( acc_ptr->op_validate_p[0] == 'Y' ) 
      XmToggleButtonSetState ( yesTB, True, True ) ;
    else
      XmToggleButtonSetState ( noTB, True, True ) ;
    XmTextSetString( commentsST,acc_ptr->op_comment ) ;      
    XmTextSetString( managerTF,acc_ptr->mgr_user_id ) ;

		/***************************************************
		if ( acc_ptr->quicklook_p[0] == 'Y' )
		  XmToggleButtonSetState ( yesTB1, True, True ) ;
		else
		  XmToggleButtonSetState ( noTB1, True, True ) ;

    XmTextSetString( first_nameTF,acc_ptr->first_name ) ;
    XmTextSetString( m_iTF,acc_ptr->initial_name ) ;
    XmTextSetString( last_nameTF,acc_ptr->last_name ) ;
    XmTextSetString( titleTF,acc_ptr->title ) ;
    XmTextSetString( zipTF,acc_ptr->zipcode ) ;
    XmTextSetString( phoneTF,acc_ptr->phone ) ;
    XmTextSetString( faxTF,acc_ptr->fax ) ;
    XmTextSetString( organizationTF,acc_ptr->organization ) ;
    XmTextSetString( addressTF,acc_ptr->street ) ;
    XmTextSetString( cityTF,acc_ptr->city ) ;
    XmTextSetString( stateTF,acc_ptr->state ) ;
    XmTextSetString( countryTF,acc_ptr->country ) ;
    XmTextSetString( emailTF,acc_ptr->email ) ;
    **************************************************/

  }
	UxAccount_dataContext = UxSaveCtx;
}

/***************************************************************************
**
** Function Name :	create_account_data_query
**
** Description:		Initializes glbData.accounts_users_data
**			.account_data.queryStruct.
** 			sqlBuf with query to update or create an account. It 
**			assumes that at least account_id, account_type,
**			resource, begin balance, current balance, on hold, 
**			creation, rate, first_name, and last name
**			have non NULL values.
**
** Arguments:		save ( CREATE=create, DATA = update ).
**
** Return Value:	none
**
** Revision: 09/06/96 Allows apostrophe for comments and address fields.	
**                    This is to correct PR 84.
**
**           10/07/96 Modified to take out the account manager information.
**           10/14/96 Modified to take out the quicklook_p field.
**
****************************************************************************/

static void create_account_data_query ( ACCOUNT_USER_DATA save ) {
  OP_ACC_USR_DATA *acc_usr_ptr ;
  OP_QUERY_STRUCT *sql ;
  char *sqlPtr ;
  char *acc_mgr_sqlPtr ;
  char  attributes[IMS_COL1024_LEN*5+1] ;
  char *aPtr ;
  int   cnt,temp,i ;
  char  temp_buf [IMS_COL1024_LEN*3 + 1] ;
	char text[IMS_COL512_LEN+1];

	memset (text, 0, sizeof(char));
  acc_usr_ptr = & (glbData.accounts_users_data ) ;
  sql = &(acc_usr_ptr->account_data.queryStruct) ;
  sql->select[0]= sql->from[0]= sql->where[0]= sql->sqlBuf[0] = '\0' ;
  attributes[0] = '\0' ;
  sql->sPtr = sql->select ;
  sql->fPtr = sql->from ;
  sql->wPtr = sql->where ;
  aPtr      = attributes ;
  sqlPtr = sql->sqlBuf ;

  if ( save == CREATE ) {
    strcpy (sql->sPtr,"account ") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
  
	strcpy( aPtr, "( account_id" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( id && *id )
      sprintf( sql->wPtr, "('%s',", id ) ;
    else
     sprintf( sql->wPtr, "('',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy( aPtr, ", account_type" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    temp = 0 ;
    if ( type && *type ) {
      for ( i = 0 ; i < glbData.account_type_count ; i++ )
        if ( !strcmp (glbData.account_type[i].item_name,type) )
          temp = glbData.account_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", create_time" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( creation && *creation )
      sprintf( sql->wPtr, "'%s',", creation ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", expire_time" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( expiration && *expiration )
      sprintf( sql->wPtr, "'%s',", expiration ) ;
    else
     sprintf( sql->wPtr, "NULL,") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", resource_type" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    temp = 0 ;
    if ( resource && *resource ) {
      for ( i = 0 ; i < glbData.resource_type_count ; i++ )
        if ( !strcmp (glbData.resource_type[i].item_name,resource) )
          temp = glbData.resource_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", rate_multiplier" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( rate && *rate ) 
      sprintf( sql->wPtr, "%f,", (DBREAL)atof(rate) ) ;
    else
      sprintf( sql->wPtr, "," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", begin_balance" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( begin_balance && *begin_balance ) 
      sprintf( sql->wPtr, "%f,", (DBREAL)atof(begin_balance) ) ;
    else
      sprintf( sql->wPtr, "," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", curr_balance" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( current_balance && *current_balance ) 
      sprintf( sql->wPtr, "%f,", (DBREAL)atof(current_balance) ) ;
    else
      sprintf( sql->wPtr, "," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", hold_balance" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( on_hold && *on_hold ) 
      sprintf( sql->wPtr, "%f,", (DBREAL)atof(on_hold) ) ;
    else
      sprintf( sql->wPtr, "," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", op_validate_p" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    sprintf( sql->wPtr, "'%s',", validation ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	/***********************************************************
	strcpy( aPtr, ", quicklook_p" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
	sprintf( sql->wPtr, "'%s',", quicklook ) ;
	sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
	***********************************************************/

	strcpy( aPtr, ", account_proc" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( special_proc && *special_proc )
      sprintf( sql->wPtr, "'%s',", special_proc ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", mgr_user_id" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( mgr_user_id && *mgr_user_id )
      sprintf( sql->wPtr, "'%s',", mgr_user_id) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", op_comment" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( comments && *comments )
		{
			(void)ims_formatQuotedString (comments, text);
      sprintf( sql->wPtr, "'%s')", text) ;
		}
    else
     sprintf( sql->wPtr, "'')") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ") \n" ) ;
	aPtr = aPtr + strlen( aPtr ) ;

    sprintf ( sqlPtr, "insert into %s%svalues %s",
                      sql->select, attributes, sql->where ) ;
    /*printf ( "%s\n",sqlPtr ) ;*/

    /* create query for account_mgr table */
/****
    sql = &(acc_usr_ptr->account_data.acc_mgr_queryStruct) ;
    sql->select[0]= sql->from[0]= sql->where[0]='\0' ;
	attributes[0] = '\0' ;
    sql->sPtr = sql->select ;
    sql->fPtr = sql->from ;
    sql->wPtr = sql->where ;
	aPtr      = attributes ;
    acc_mgr_sqlPtr = sql->sqlBuf ;

    
    strcpy (sql->sPtr,"account_mgr ") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
  
	strcpy ( aPtr, "( account_id" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( id && *id )
      sprintf( sql->wPtr, "('%s',", id ) ;
    else
     sprintf( sql->wPtr, "('',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy ( aPtr, ", first_name" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( first_name && *first_name )
      sprintf( sql->wPtr, "'%s',", first_name ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy ( aPtr, ", initial_name" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( m_i && *m_i )
      sprintf( sql->wPtr, "'%s',", m_i ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy ( aPtr, ", last_name" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( last_name && *last_name )
      sprintf( sql->wPtr, "'%s',", last_name ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy ( aPtr, ", title" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( title && *title )
      sprintf( sql->wPtr, "'%s',", title ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy ( aPtr, ", zipcode" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( zip && *zip )
      sprintf( sql->wPtr, "'%s',", zip ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy ( aPtr, ", phone" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( phone && *phone )
      sprintf( sql->wPtr, "'%s',", phone ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy ( aPtr, ", fax" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( fax && *fax )
      sprintf( sql->wPtr, "'%s',", fax ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy ( aPtr, ", organization" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( organization && *organization )
      sprintf( sql->wPtr, "'%s',", organization ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy ( aPtr, ", street" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( address && *address )
		{
			(void) ims_formatQuotedString (address, text);
      sprintf( sql->wPtr, "'%s',", text) ;
		}
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy ( aPtr, ", city" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( city && *city )
      sprintf( sql->wPtr, "'%s',", city ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy ( aPtr, ", state " ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( state && *state )
      sprintf( sql->wPtr, "'%s',", state ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy ( aPtr, ", country" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( country && *country )
      sprintf( sql->wPtr, "'%s',", country ) ;
    else
     sprintf( sql->wPtr, "'',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy ( aPtr, ", email" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    if ( email && *email )
      sprintf( sql->wPtr, "'%s')", email ) ;
    else
     sprintf( sql->wPtr, "'')") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy ( aPtr, ") \n" ) ;
	aPtr = aPtr + strlen ( aPtr ) ;
    
    sprintf ( acc_mgr_sqlPtr, " insert into %s%svalues %s",
                      sql->select, attributes, sql->where ) ;
    *printf ( "%s\n",acc_mgr_sqlPtr ) ;*
****/
   
   }
   else { /* update */
    strcpy (sql->sPtr,"account ") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;

    temp = 0 ;
    if ( type && *type ) {
      for ( i = 0 ; i < glbData.account_type_count ; i++)
        if ( !strcmp (glbData.account_type[i].item_name,type) )
          temp = glbData.account_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "account_type=%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( creation && *creation )
      sprintf( sql->wPtr, "create_time='%s',", creation ) ;
    else
     sprintf( sql->wPtr, "create_time='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( expiration && *expiration )
      sprintf( sql->wPtr, "expire_time='%s',", expiration ) ;
    else
     sprintf( sql->wPtr, "expire_time=NULL,") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    temp = 0 ;
    if ( resource && *resource ) {
      for ( i = 0 ; i < glbData.resource_type_count ; i++)
        if ( !strcmp (glbData.resource_type[i].item_name,resource) )
          temp = glbData.resource_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "resource_type=%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( rate && *rate ) 
      sprintf( sql->wPtr, "rate_multiplier=%f,", (DBREAL)atof(rate) ) ;
    else
      sprintf( sql->wPtr, "rate_multiplier=," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    /***if ( begin_balance && *begin_balance ) 
      sprintf( sql->wPtr, "begin_balance=%f,", (DBREAL)atof(begin_balance) ) ;
    else
      sprintf( sql->wPtr, "begin_balance=," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( current_balance && *current_balance ) 
      sprintf( sql->wPtr, "curr_balance=%f,",(DBREAL)atof(current_balance)) ;
    else
      sprintf( sql->wPtr, "curr_balance=," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( on_hold && *on_hold ) 
      sprintf( sql->wPtr, "hold_balance=%f,", (DBREAL)atof(on_hold) ) ;
    else
      sprintf( sql->wPtr, "hold_balance=," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    ***/

    sprintf( sql->wPtr, "op_validate_p='%s',", validation ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	/***********************************************************
	sprintf( sql->wPtr, "quicklook_p='%s',", quicklook ) ;
	sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
	***********************************************************/

    if ( special_proc && *special_proc )
      sprintf( sql->wPtr, "account_proc='%s',", special_proc ) ;
    else
     sprintf( sql->wPtr, "account_proc='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( mgr_user_id && *mgr_user_id )
      sprintf( sql->wPtr, "mgr_user_id ='%s',", mgr_user_id ) ;
    else
     sprintf( sql->wPtr, "mgr_user_id ='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( comments && *comments )
		{
			(void) ims_formatQuotedString (comments, text);
      sprintf( sql->wPtr, "op_comment='%s'", text) ;
		}
    else
     sprintf( sql->wPtr, "op_comment=''") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf( sql->wPtr," where account_id='%s'", id ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf ( sqlPtr, "update %s set %s",
                      sql->select, sql->where ) ;
    /*printf ( "%s\n",sqlPtr ) ; */

/* create query for account_mgr now */
/****
    sql = &(acc_usr_ptr->account_data.acc_mgr_queryStruct) ;
    sql->select[0]= sql->from[0]= sql->where[0]='\0' ;
    sql->sPtr = sql->select ;
    sql->fPtr = sql->from ;
    sql->wPtr = sql->where ;
    acc_mgr_sqlPtr = sql->sqlBuf ;
    
    strcpy (sql->sPtr,"account_mgr ") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
      
    if ( first_name && *first_name )
      sprintf( sql->wPtr, "first_name='%s',", first_name ) ;
    else
     sprintf( sql->wPtr, "first_name='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
    if ( m_i && *m_i )
      sprintf( sql->wPtr, "initial_name='%s',", m_i ) ;
    else
     sprintf( sql->wPtr, "initial_name='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
    if ( last_name && *last_name )
      sprintf( sql->wPtr, "last_name='%s',", last_name ) ;
    else
     sprintf( sql->wPtr, "last_name='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
    if ( title && *title )
      sprintf( sql->wPtr, "title='%s',", title ) ;
    else
     sprintf( sql->wPtr, "title='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
    if ( zip && *zip )
      sprintf( sql->wPtr, "zipcode='%s',", zip ) ;
    else
     sprintf( sql->wPtr, "zipcode='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
    if ( phone && *phone )
      sprintf( sql->wPtr, "phone='%s',", phone ) ;
    else
     sprintf( sql->wPtr, "phone='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( fax && *fax )
      sprintf( sql->wPtr, "fax='%s',", fax ) ;
    else
     sprintf( sql->wPtr, "fax='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
    if ( organization && *organization )
      sprintf( sql->wPtr, "organization='%s',", organization ) ;
    else
     sprintf( sql->wPtr, "organization='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( address && *address )
		{
			(void) ims_formatQuotedString (address, text);
      sprintf( sql->wPtr, "street='%s',", text) ;
		}
    else
     sprintf( sql->wPtr, "street='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( city && *city )
      sprintf( sql->wPtr, "city='%s',", city ) ;
    else
     sprintf( sql->wPtr, "city='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
    if ( state && *state )
      sprintf( sql->wPtr, "state='%s',", state ) ;
    else
     sprintf( sql->wPtr, "state='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( country && *country )
      sprintf( sql->wPtr, "country='%s',", country ) ;
    else
     sprintf( sql->wPtr, "country='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
    if ( email && *email )
      sprintf( sql->wPtr, "email='%s'", email ) ;
    else
     sprintf( sql->wPtr, "email=''") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf( sql->wPtr," where account_id='%s'", id ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf ( acc_mgr_sqlPtr, " update %s set %s",
                      sql->select, sql->where ) ;
    *printf ( "%s\n",acc_mgr_sqlPtr ) ; *
*/

  }


}/* end of create_account_data_query */


/***************************************************************************
**
** Function Name :	execute_account_data_query
**
** Description:		Executes the query formed by create_query by calling
**			ims_op_accCat
**
** Arguments:		none
**
** Return Value:	none
**
** Revision: 10/07/96 - Modified to take out account manager query Struct
**
****************************************************************************/
static int execute_account_data_query ( ACCOUNT_USER_DATA save ) {
	
  OP_CAT_STRUCT *catReq;
  int status;
  int user_count;
  char Msg[IMS_COL1024_LEN+1];

  catReq = &(glbData.accounts_users_data.catReq);
  catReq->item[1] = ( char *)       
             &(glbData.accounts_users_data.account_data.queryStruct.sqlBuf);

	/*****
  catReq->item[2] = ( char *)       
      &(glbData.accounts_users_data.account_data.acc_mgr_queryStruct.sqlBuf);
	*****/

  if ((status = ims_op_accCat (catReq, OP_ACC_SAVEACCDATA)) < IMS_OK) {   
    /* Display error messages */
    if ( save == CREATE ) 
      sprintf(Msg,
              "Creation of new account failed. Account may already exists"); 
    else
      sprintf(Msg,
              "Updating of account failed.");  
    msg_dlgCb (msg_dlg, IMS_FATAL, Msg);
    return (IMS_FATAL) ; 
			  
  }

  return (IMS_OK) ;

}/* end of execute_query */


static int is_time_valid (
  char *field_value		/* Field Value */
)
{
  char value[IMS_COL15_LEN+1];
  char hours[3], minutes[3], seconds[3];

  strcpy(value, field_value);
  ims_truncStr(value);
  if(!*value)
    return (IMS_OK);    

/* User Profile will do this in '93 */
  
  hours[0] = value[0];
  hours[1] = value[1];
  hours[2] = '\0';
  
  minutes[0] = value[3];
  minutes[1] = value[4];
  minutes[2] = '\0';
  
  seconds[0] = value[6];
  seconds[1] = value[7];
  seconds[2] = '\0';

  if ( ((int)strlen(value) < 8)  ||
     /*!ims_isInteger(hours)     ||*/
     (value[2] != ':')    ||
     /*!ims_isInteger(minutes)   ||*/
     (value[5] != ':')    
     /*!ims_isInteger(seconds)*/)  {
        
    return IMS_ERROR;
  }

  if(atoi(hours)   < 0  ||
     atoi(hours)   > 23 ||
     atoi(minutes) < 0  ||
     atoi(minutes) > 59 ||
     atoi(seconds) < 0  ||
     atoi(seconds) > 59)  {
        
    return IMS_ERROR;
  }

  return IMS_OK;
}


void template21 ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{

	}
	UxAccount_dataContext = UxSaveCtx;


}


void account_data_check_dateCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccount_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccount_dataContext;
	UxAccount_dataContext = UxContext =
			(_UxCaccount_data *) UxGetContext( UxWidget ) ;
	{
		cbs = (XmTextVerifyCallbackStruct *)cb;
		len = XmTextGetLastPosition(wgt);

		if (cbs->reason == XmCR_MOVING_INSERT_CURSOR)
		{
			if (cbs->newInsert != len)
				cbs->doit = False;
			return;
		}

		/* no backspacing or typing in the middle of the string */
		if (cbs->currInsert < len)
		{
			cbs->doit = False;
			return;
		}

		if (cbs->text->length == 0) /* backspace */
		{
			if ((cbs->startPos == 4) || (cbs->startPos == 7) ||
			     (cbs->startPos == 10) |
                             (cbs->startPos == 13) || (cbs->startPos == 16))
				cbs->startPos--;
			return;
		}
		/**if (cbs->text->length >1) don't allow clipboard copies 
		{
			cbs->doit = False;
			return;
		}
  		**/
		/* don't allow non-digits or let the input exceed 19 chars */
		if (!isdigit(c = cbs->text->ptr[0]) || len >= 19)
			cbs->doit = False;
		else if (len == 3 || len == 6)
		{
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c ;
				cbs->text->ptr[1] = '-';
		}
                else if  (len == 9 )
		{
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c ;
				cbs->text->ptr[1] = ' ';
	        }
                else if (len == 12 || len == 15)
	        {
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c ;
				cbs->text->ptr[1] = ':';
		}


	}
	UxAccount_dataContext = UxSaveCtx;

}

