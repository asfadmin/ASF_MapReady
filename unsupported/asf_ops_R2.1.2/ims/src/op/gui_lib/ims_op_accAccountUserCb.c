static char *sccs = "@(#)ims_op_accAccountUserCb.c	5.7  09/15/97";
/*******************************************************************************
**
** File:		ims_op_accAccountUserCb.c
**
** Function:		Callback functions for the accounts_users screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
**                 
** Modified:
**    11-27-95  Alin Tilden  Modified function get_datasets_assigned_to_account
**                           to initialize the op_account_dataset list as well
**                           as the op_assigned_datasets list.
**    12-06-95  Alin Tilden  Modified function accounts_users_init_popCb 
**                           in order to initialize global pointers.
**                           Corrected the maintenance of selected_user, and
**                           selected_account flags in functions:
**                           accounts_users_users_listCb, and
**                           accounts_users_accounts_listCb.
**
** 		04-22-96  J. Ting 		 Modified function get_users_assigned_to_account
**													 to fix problem reports 754 and 755 for R1BPrime.
**
**		04-23-96  J. Ting			 Modified function assign_datasets_updateCb. 
**                           Initialized catReq->item[0], see R1BP problem
**                           reports 754 and 755.
**
**    05-08-96  J. Ting      Modified function delete_selected_user
**                           a user cannot be removed if there is any 
**                           orders associated with it.
**
**    05-08-96  J. Ting      Modified function delete_selected_account
**                           an account cannot be removed if there is any
**                           orders associated with it.
** 
**    06-12-96  J. Ting      Modified the following functions for PR 942:
**                           accounts_users_closeCb,
**                           accounts_users_create_interfacesCb,
**                           accounts_users_init_popCb
**                           accounts_users_create_usersCb,
**                           accounts_users_create_accountCb,
**
**    06-12-96  J. Ting      Modified function accounts_users_delete_userCb
**                           If mwm quit is activated, it caused the program
**                           to core dump.  Therefore, for now, set the
**                           delete response to DO_NOTHING.
**
**    06-12-96  J. Ting      Modified function accounts_users_delete_accountCb
**                           If mwm quit is activated, it caused the program
**                           to core dump.  Therefore, for now, set the
**                           delete response to DO_NOTHING.
**
**    10-08-96  J. Ting      Modified delete_selected_account to comment out
**                           delete_account_manager call.
**    
**	  07-23-97  D. Ting			 PR2625 - delete_selected_account, delete 
**													 account_extension as well
**
**    07-30-97  D. Ting		   PR2441 - added a call to getManager and associated
**													 processing
**
******************************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include <UxXt.h>

#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_cmnQuery.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>
#include "ims_op_accCb.h"


/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/
#define USERS_LIST_LENGTH  25 /* also used for the accounts list length */

   
extern OP_GLOBAL_DATA glbData ;
extern Widget create_user_data(Widget,int);
extern Widget create_account_data(Widget,ACCOUNT_USER_DATA);
extern Widget create_search_users (Widget);
extern Widget create_assign_users(Widget ) ;
extern Widget create_delete_dlg(Widget);
extern Widget create_assign_datasets (Widget) ;
extern Widget create_search_accounts(Widget);
extern Widget create_update_balance_dlg (Widget);
extern void delete_dlgCb ( Widget, XtPointer, XtPointer ) ;
extern Widget update_balance_dlg ;
extern Widget search_users ;
extern Widget user_data ;
extern Widget account_data ;
extern Widget assign_users ;
extern Widget delete_dlg ;
extern Widget assign_datasets ;
extern Widget search_accounts ;
extern Widget user_dataLB ;
extern Widget account_dataLB ;
extern Widget msg_dlg ;

extern void msg_dlgCb( Widget , int , char *) ;
extern void assign_datasets_assign_sbCb ( Widget, XtPointer, XtPointer ) ; 
extern void prepare_message_box (SELECTED_BUTTON ** , char *, char *  ) ;
extern void set_user_data (Widget ) ;
extern void set_account_data (Widget ) ;
extern void assign_users_init_dataCb (Widget,XtPointer,XtPointer);
extern void free_assign_users ( ) ;
extern void free_assigned_users ( ) ;
extern void assign_datasets_init_dataCb (Widget,XtPointer,XtPointer);
extern void free_assign_datasets ( ) ;
extern void free_assigned_datasets ();
extern void free_account_data ();
extern void free_user_data ();

void accounts_users_users_sbCb (Widget , XtPointer, XtPointer) ;
void accounts_users_accounts_sbCb (Widget , XtPointer, XtPointer) ;

static int delete_selected_user ( ) ;   
static int delete_selected_account ( ) ;    
static int get_users_assigned_to_account ( char * ) ;
static int get_datasets_assigned_to_account ( char * ) ;
static int selected_account = 0 ;
static int selected_user = 0 ;
static int user_window_top = 0 ;
static int account_window_top = 0 ;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include <ims_op_accAccountUser.h>
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: accounts_users_create_interfacesCb
**
** Description:		This function creates the accounts and users interfaces
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - add mwm quit button support to correct PR 942.
**
**==========================================================================*/

/* creates the interfaces that are unique */
void	accounts_users_create_interfacesCb( )
{

	/*create_user_data(NO_PARENT) ;*/ /* Done in multiple copies */
	/*create_account_data(NO_PARENT);*/
	/*create_update_balance_dlg(NO_PARENT);*/

        /* initialize connection flag and acc & user widget */
        glbData.accounts_users_data.connect_flag = 0 ;
	glbData.accounts_usersW = create_accounts_users(NO_PARENT);

	/*
	** 06/12/96 - PR942
	** This is to add the callbacks to the window manager quit
	** button for each screen, this is to correct PR 942
	*/
	addWinMgrCloseCB (glbData.accounts_usersW, accounts_users_closeCb, NULL);

	glbData.assign_usersW = create_assign_users(NO_PARENT ) ;
	addWinMgrCloseCB (glbData.assign_usersW, assign_users_closeCb, NULL);

	glbData.assign_datasetsW = create_assign_datasets(NO_PARENT) ;
	addWinMgrCloseCB (glbData.assign_datasetsW, assign_datasets_closeCb, NULL);

	create_delete_dlg (NO_PARENT) ;
        create_msg_dlg (NO_PARENT ) ;


        /* The following two screens are created in accounts_users_init_popCb */
        /* because they need data from the data base for the options menus    */
	/*create_search_accounts(NO_PARENT); */                  
        /*create_search_users(NO_PARENT);   */ 

}
/* Activate callbacks for the Create Account and Account Data buttons. */
/* Passed with different values for the client data. */
void accounts_users_create_accountCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                   UxWidget = wgt;
    int                      UxClient_data = (int)cd;
    XtPointer                UxCallbackArg = cb;

      
    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext (UxWidget);
    {
	if (UxClient_data) 
	{
	    create_account_data ( NO_PARENT, CREATE  );
			/*
			** 06/12/96 - PR942
			** This is to add the callbacks to the window manager quit
			** button for each screen, this is to correct PR 942
			*/
			addWinMgrCloseCB (account_data, account_data_closeCb, NULL);

	    XtPopup ( XtParent (  (account_data)), XtGrabNone ) ;
	}
	else if (selected_account) 
	{
	    create_account_data ( NO_PARENT, DATA  );
			/*
			** 06/12/96 - PR942
			** This is to add the callbacks to the window manager quit
			** button for each screen, this is to correct PR 942
			*/
			addWinMgrCloseCB (account_data, account_data_closeCb, NULL);

	    set_account_data (account_data ) ;
	    XtPopup ( XtParent (  (account_data)), XtGrabNone ) ;
	}
	else /* no account selected */
	    msg_dlgCb ( msg_dlg, 1/*IMS_ERROR*/,
		       "No account has been selected!");
    }
    UxAccounts_usersContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: accounts_users_delete_accountCb
**
** Description:		This function delete the selected account
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - J. Ting   Disabled mwm quit function to 
**                                        prevent program core.
**
**==========================================================================*/
void accounts_users_delete_accountCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

  _UxCaccounts_users        *UxSaveCtx, *UxContext;
  Widget                     UxWidget = wgt;
  XtPointer                  UxClient_data = cd;
  XtPointer                  UxCallbackArg = cb;
  static SELECTED_BUTTON     selected ;
  static SELECTED_BUTTON    *selected_ptr ;
  XmScrollBarCallbackStruct *cbs;

  UxSaveCtx = UxAccounts_usersContext;
  UxAccounts_usersContext = UxContext =
    (_UxCaccounts_users *) UxGetContext( UxWidget );

  {
    if ( !selected_account )
    {                     /* no account selected */
      msg_dlgCb ( msg_dlg, 1/*IMS_ERROR*/,
		 "No account has been selected!");
      return ;
    }
          
    selected = NOTHING ;
    selected_ptr = &selected ;
    create_account_data ( NO_PARENT, DATA );

	  /* 
	  ** 06/12/96 - this is done to prevent program core.
    ** Tell the Window Mgr. that you are going to handle 
    ** the close action yourself by setting XmNdeleteResponse
    ** to XmDO_NOTHING
    */
    XtVaSetValues(XtParent(account_data), 
           XmNdeleteResponse, XmDO_NOTHING,
     	     NULL );

    set_account_data ( account_data ) ;
    XtPopup ( XtParent (  (account_data)), XtGrabNone ) ;
    prepare_message_box (&selected_ptr,
	"Do you really want to delete\n the selected account?",
        "Delete Account");
           
    while ( selected == NOTHING)
    {
      XtAppProcessEvent ( UxAppContext, XtIMAll);
    }
    XtDestroyWidget( XtParent (  ( account_data ) ) ) ;

    /* proceed to delete, or ignore if cancel */
    if ( selected == OK ) 
    {
      if ( delete_selected_account ( ) < IMS_OK )
	return ;

      /* update displayed user list */
      if ( glbData.accounts_users_data.account_data.accounts_cnt > 0 ) 
      {
	free_account_data ( ) ;
	if (  execute_search_account_query ( ) < IMS_OK ) 
	{
	  return ;
	}
	cbs = ( XmScrollBarCallbackStruct * )
	      XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
	cbs->value = 0 ; 
	accounts_users_accounts_sbCb ((Widget)NULL,NULL,(XtPointer)cbs) ;
	XtFree ((char *)cbs); 
             
      }
    }/* end of if ( selected == OK ... */
          
  }
  UxAccounts_usersContext = UxSaveCtx;

}


/* Activate callbacks for the Create User and User Data buttons. */
/* Passed with different values for the client data. */

void accounts_users_create_userCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                   UxWidget = wgt;
    int                      UxClient_data = (int) cd;
    XtPointer                UxCallbackArg = cb;
        
      
    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
    {
	if (UxClient_data) 
	{
	    create_user_data ( NO_PARENT, CREATE  );
			/*
			** 06/12/96 - PR942
			** This is to add the callbacks to the window manager quit
			** button for each screen, this is to correct PR 942
			*/
			addWinMgrCloseCB (user_data, user_data_closeCb, NULL);

	    XtPopup ( XtParent (  (user_data)), XtGrabNone ) ;
	}
	else if (selected_user) 
	{
	    create_user_data ( NO_PARENT, DATA  );
	    set_user_data ( user_data ) ;
			/*
			** 06/12/96 - PR942
			** This is to add the callbacks to the window manager quit
			** button for each screen, this is to correct PR 942
			*/
			addWinMgrCloseCB (user_data, user_data_closeCb, NULL);

	    XtPopup ( XtParent (  (user_data)), XtGrabNone ) ;
	}
	else /* no user selected */
	    msg_dlgCb ( msg_dlg, 1,
		       "No user has been selected!");
     }
    UxAccounts_usersContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: accounts_users_delete_userCb
**
** Description:		This function delete the selected user
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - J. Ting   Disabled mwm quit function to 
**                                        prevent program core.
**
**==========================================================================*/
void accounts_users_delete_userCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClient_data = cd;
	XtPointer               UxCallbackArg = cb;
        static SELECTED_BUTTON selected ;
        static SELECTED_BUTTON * selected_ptr ;
        XmScrollBarCallbackStruct *cbs ;

	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{
           if ( !selected_user ) {
             /* no user selected */
             msg_dlgCb ( msg_dlg, IMS_ERROR,
                                  "No user has been selected!");
             return ;
           }
          
	  selected = NOTHING ;
          selected_ptr = &selected ;
          create_user_data ( NO_PARENT, DATA  );

	 			  /* 
				  ** 06/12/96 - this is done to prevent program core.
          ** Tell the Window Mgr. that you are going to handle 
		      ** the close action yourself by setting XmNdeleteResponse
		      ** to XmDO_NOTHING
		      */
  	      XtVaSetValues(XtParent(user_data), 
		               XmNdeleteResponse, XmDO_NOTHING,
	           	     NULL );

          set_user_data ( user_data ) ;
          XtPopup ( XtParent (  (user_data)), XtGrabNone ) ;
          prepare_message_box (&selected_ptr,
             "Do you really want to delete\n the selected user?",
             "Delete User");
           
          while ( selected == NOTHING){
            XtAppProcessEvent ( UxAppContext, XtIMAll);
          }

       		XtDestroyWidget( XtParent (  ( user_data ) ) ) ;

          /* proceed to delete, or ignore if cancel */
          if ( selected == OK ) {
            if ( delete_selected_user ( ) < IMS_OK )
              return ;
            /* update displayed user list */
            if ( glbData.accounts_users_data.user_data.users_cnt > 0 ) {
              free_user_data ( ) ;
              if (  execute_search_user_query ( ) < IMS_OK ) {
               return ;
              }
              cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
              cbs->value = 0 ; 
              accounts_users_users_sbCb ((Widget)NULL , NULL,(XtPointer)cbs) ;
              XtFree ((char *)cbs) ; 
             
           }
         }/* end of if ( selected == OK ... */

	}
	UxAccounts_usersContext = UxSaveCtx;


}

void accounts_users_search_accountsCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClient_data = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{
           XtPopup (XtParent (( search_accounts) ), XtGrabNone ) ;
           glbData.search_accountsFlag = 1 ; /* screen has been pop up */
 
	}
	UxAccounts_usersContext = UxSaveCtx;

}

void accounts_users_assign_usersCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

  _UxCaccounts_users      *UxSaveCtx, *UxContext;
  Widget                  UxWidget = wgt;
  XtPointer               UxClient_data = cd;
  XtPointer               UxCallbackArg = cb;
  OP_ACCOUNT *acc_ptr  ;

  UxSaveCtx = UxAccounts_usersContext;
  UxAccounts_usersContext = UxContext =
    (_UxCaccounts_users *) UxGetContext( UxWidget );
  {
    if ( !selected_account ) 
    {                         /* no account selected */
      msg_dlgCb ( msg_dlg, 1/*IMS_ERROR*/,
		 "No account has been selected!");
      return ;
    }

    acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;
    /* find selected account */
    while ((acc_ptr != (OP_ACCOUNT *) NULL ) &&
	   (acc_ptr->selected == 0) )
      acc_ptr = acc_ptr->next ;

    if ( acc_ptr == (OP_ACCOUNT *) NULL ) 
    {
      msg_dlgCb (msg_dlg, IMS_ERROR, "No account Data!");
      return  ;
    }

    /* Change cursor to watch cursor */
    timeOutCursors (True);

    /* Get all users and the users assigned to the selectd account. */
    if (get_users_assigned_to_account (acc_ptr->account_id) == IMS_OK) 
    {
      /* Pop up and Initialize data in assign_users. */
      XtPopup ( XtParent (  ( assign_users ) ), XtGrabNone ) ;
      glbData.assign_usersFlag = 1 ; /* screen has been popped up */
      assign_users_init_dataCb (assign_users,(XtPointer)acc_ptr,NULL);   
    }
    /* Change cursor back to normal */
    timeOutCursors (False);
    
  }

  UxAccounts_usersContext = UxSaveCtx;

}



void accounts_users_assign_datasetsCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                   UxWidget = wgt;
    XtPointer                UxClient_data = cd;
    XtPointer                UxCallbackArg = cb;
    OP_ACCOUNT              *acc_ptr  ;

    
    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
    {
	if ( !selected_account ) 
	{
	    /* no account selected */
	    msg_dlgCb ( msg_dlg, 1/*IMS_ERROR*/,
		       "No account has been selected!");
	    return ;
	}

	acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;
	/* find selected account */
	while ( (acc_ptr != (OP_ACCOUNT *) NULL ) &&
	       (acc_ptr->selected == 0) )
	    acc_ptr = acc_ptr->next ;
	if ( acc_ptr == (OP_ACCOUNT *) NULL ) 
	{
            msg_dlgCb (msg_dlg, IMS_ERROR, "No account Data!");
            return;
	}

	/* Change cursor to watch cursor */
	timeOutCursors (True);

	/* Get all datasets assigned to the selectd account */
	if (get_datasets_assigned_to_account (acc_ptr->account_id) == IMS_OK)
	{
	    /* Pop up and Initialize data in assign_users */
	    XtPopup ( XtParent (  ( assign_datasets ) ), XtGrabNone ) ;
	    glbData.assign_datasetsFlag = 1 ; /* screen has been pop up */
	    assign_datasets_init_dataCb (assign_datasets,
					 (XtPointer)acc_ptr,NULL);  
	}

	/* Change cursor back to normal */
	timeOutCursors (False);
 
    }
    UxAccounts_usersContext = UxSaveCtx;
    
}

void accounts_users_accounts_listCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClient_data = cd;
    XmListCallbackStruct   *cbs = (XmListCallbackStruct *) cb;
    OP_ACCOUNT             *acc_ptr, *temp;
    int                     i;

    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
	(_UxCaccounts_users *) UxGetContext( UxWidget );
    {

	acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;
	temp = acc_ptr ;
	while (temp != (OP_ACCOUNT *) NULL ) 
	{
	    temp->selected = 0 ;
	    temp = temp->next ;
	}
	while ( (acc_ptr != (OP_ACCOUNT *) NULL ) &&
	       (acc_ptr->position != account_window_top) )
            acc_ptr = acc_ptr->next ;

	for ( i=0 ; ( acc_ptr != (OP_ACCOUNT *) NULL ) &&
	     ( i < cbs->item_position - 1 ) ; i++ )
            acc_ptr = acc_ptr->next ;
          
	if (XmListPosSelected (wgt, cbs->item_position))
	{
	    acc_ptr->selected = 1;
	    selected_account = 1;
	}
	else
	{
	    acc_ptr->selected = 0;
	    selected_account = 0;
	}
	
    }
    UxAccounts_usersContext = UxSaveCtx;

}

/* single selection callback for the usersSL */
void accounts_users_users_listCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClient_data = cd;
    XmListCallbackStruct   *cbs = (XmListCallbackStruct *) cb;
    OP_USER_PROFILE        *usr_ptr, *temp;
    int                    i;

    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
	(_UxCaccounts_users *) UxGetContext( UxWidget );
    {
	/* Since our list is application defined (virtual) we have  */
	/*to keep track of the selections.                        */

	usr_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr ;
	temp = usr_ptr ;
	while (temp != (OP_USER_PROFILE *) NULL ) 
	{
	    temp->selected = 0 ;
	    temp = temp->next ;
	}
	while ( (usr_ptr != (OP_USER_PROFILE *) NULL ) &&
	       (usr_ptr->position != user_window_top) )
            usr_ptr = usr_ptr->next ;

	for ( i=0 ; ( usr_ptr != (OP_USER_PROFILE *) NULL ) &&
	     ( i < cbs->item_position - 1 ) ; i++ )
            usr_ptr = usr_ptr->next ;
	
	if (XmListPosSelected (wgt, cbs->item_position))
	{
	    usr_ptr->selected = 1;
	    selected_user = 1;
	}
	else
	{
	    usr_ptr->selected = 0;
	    selected_user = 0;
	}
    }
    UxAccounts_usersContext = UxSaveCtx;

}

void accounts_users_search_usersCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClient_data = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{
          
           XtPopup (XtParent (( search_users) ), XtGrabNone ) ;
          glbData.search_usersFlag = 1 ; /* screen has been pop up */

	}
	UxAccounts_usersContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: accounts_users_closeCb
**
** Description:		callback function for close button and mwm quit button.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - modified to use glbData.accounts_usersW
**                   to get the context instead of using UxWidget. This
**                   is done for mwm quit button to work - PR 942.
**
**==========================================================================*/
void accounts_users_closeCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClient_data = cd;
    XtPointer               UxCallbackArg = cb;
    OP_CAT_STRUCT *catReq;
    Widget sb ;
    OP_USER_PROFILE *curr_ptr, *last_ptr;
    OP_ACCOUNT      *currAcct_ptr, *lastAcct_ptr;

    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( glbData.accounts_usersW );
		/*
			(_UxCaccounts_users *) UxGetContext( UxWidget );
		*/

    {
	/*
	 ** Clear data from the users and accounts list
	 */
	if (glbData.accounts_users_data.user_data.users_cnt > 0) 
	{
	    glbData.accounts_users_data.user_data.users_cnt = 0;
	    curr_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr;
	    while (curr_ptr != (OP_USER_PROFILE*) NULL) 
	    {
		last_ptr = curr_ptr->next ;
		free (curr_ptr) ;
		curr_ptr = last_ptr ;
	    }

	    selected_user = 0 ;
            XmListDeleteAllItems ( usersSL  ) ;
            XtVaGetValues ( users_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
            XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue, 0,
                                              XmNsliderSize, 1, NULL ) ;
	}

	if (glbData.accounts_users_data.account_data.accounts_cnt > 0 ) 
	{
	    glbData.accounts_users_data.account_data.accounts_cnt = 0 ;
	    currAcct_ptr = glbData.accounts_users_data.account_data.op_account_ptr;
	    while (currAcct_ptr != (OP_ACCOUNT*) NULL) 
	    {
		lastAcct_ptr = currAcct_ptr->next ;
		free (currAcct_ptr) ;
		currAcct_ptr = lastAcct_ptr ;
	    }

	    selected_account = 0 ;
            XmListDeleteAllItems ( accountsSL  ) ;
            XtVaGetValues ( accounts_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
            XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue, 0,
                                              XmNsliderSize, 1, NULL ) ;
	}

           /* close db connection */
           /***glbData.accounts_users_data.connect_flag = 0 ;
	       catReq = &(glbData.accounts_users_data.catReq);

	       (void) ims_op_accCat (catReq, OP_ACC_CLOSECONNECTION);
           ***/
	   
	XtPopdown ( XtParent (accounts_users) );
	glbData.accounts_usersFlag = 0 ; /* screen has been pop down */
            
    }
    UxAccounts_usersContext = UxSaveCtx;

}

/* Callback for the creation of users_sbSL and the scrollbar of users_sbSW */
void accounts_users_users_sbCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

        Widget sb ;
        int i,j ;
        int selected_users[USERS_LIST_LENGTH] ;
        static cnt=0 ;
        char  buffer[IMS_COL255_LEN+1] ;
	char  temp[IMS_COL30_LEN+1];
        static OP_USER_PROFILE *user_ptr = (OP_USER_PROFILE *)NULL ;
        
        XmString str ;
        XmStringTable datasets_str ;
        XmScrollBarCallbackStruct *cbs =
         (XmScrollBarCallbackStruct * ) cb ;
        int slider ;
        static int view ;
       
        int create = (int) cd ;
                    XmFontList fontlist ;
                    int width ;

/* get the context of scroll bar */
	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
             (_UxCaccounts_users *) UxGetContext(users_sbSW);
		/*(_UxCaccounts_users *) UxGetContext(XtParent(UxWidget));*/
	{
            
        /* set the call backs for the scrollbar at creation time */
        if ( create == 1 ) {
            
            XtVaGetValues ( users_sbSW , 
                    XmNverticalScrollBar, &sb,NULL ) ;
            XtAddCallback ( sb, 
                XmNvalueChangedCallback, accounts_users_users_sbCb, NULL) ;
            XtAddCallback ( sb, 
                XmNincrementCallback, accounts_users_users_sbCb, NULL ) ;
            XtAddCallback ( sb, 
             XmNdecrementCallback, accounts_users_users_sbCb, NULL ) ;
            XtAddCallback ( sb, 
                XmNpageIncrementCallback, accounts_users_users_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                XmNpageDecrementCallback, accounts_users_users_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoTopCallback, accounts_users_users_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoBottomCallback, accounts_users_users_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNdragCallback, accounts_users_users_sbCb,NULL ) ;
            
            return ; 
         }
         if ( wgt == (Widget) NULL )  /* new list */
           selected_user = 0 ;
         user_window_top = cbs->value ;
         user_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr ;
         /* initialize this only when: */
         if ( cbs->value == 0 ) {
           cnt = glbData.accounts_users_data.user_data.users_cnt ;
           if ( cnt > USERS_LIST_LENGTH )
            view = USERS_LIST_LENGTH ;
           else
            view = cnt ;
         }
           
        /*Delete all items from  list */
         XmListDeleteAllItems ( usersSL  ) ;
         /* Update lists */
             
         datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         /* index into users list until index begins at window top */
         while ( (user_ptr != NULL) && (user_ptr->position !=
                                                     user_window_top) ){
           user_ptr = user_ptr->next ;
         }
         
         for (i = 0 ; i < view && user_ptr != (OP_USER_PROFILE *)NULL ; i ++ ){
           temp[0] = '\0' ;
           for ( j = 0 ; j < glbData.user_type_count ; j++ )
             if ( user_ptr->user_type == glbData.user_type[j].item_id )
               strcpy ( temp, glbData.user_type[j].item_name ) ;
           sprintf (buffer,
                    " %-16s%-12s %s %c %c\0",
                    user_ptr->user_id, temp,
                    user_ptr->last_name,
                    user_ptr->first_name[0], user_ptr->initial_name[0] ) ;
           selected_users[i] = 0 ;
           if ( user_ptr->selected ) /* set by users_listCb */
             selected_users[i] = 1 ;
           datasets_str[i] = XmStringCreateLocalized ( buffer ) ;
           user_ptr = user_ptr->next ;

                      }  
         XmListAddItemsUnselected ( usersSL,datasets_str,view,1 ) ;
         
         for ( i = 0 ; i < view ; i++ ) {
           /* select list item previously selected */
           if (selected_users[i])
             XmListSelectPos (usersSL, i+1, False ) ;
           XmStringFree ( datasets_str[i] ) ;
         }
         XmStringFree ( (XmString)datasets_str ) ;

         /* set slider size and list value */
         
         if ( cnt > USERS_LIST_LENGTH )
           slider = USERS_LIST_LENGTH ;
         else 
           slider = cnt ;
         XtVaGetValues ( users_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
         if ( cnt > 0 )
           XtVaSetValues ( sb, XmNmaximum, cnt, XmNvalue,
                    cbs->value, XmNsliderSize, slider, NULL ) ;
         else
           XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue,
                    cbs->value, XmNsliderSize, 1, NULL ) ;

         
	  
       }

   	UxAccounts_usersContext = UxSaveCtx;


}

/* Callback for the creation of accounts_sbSL and the scroll bar of */
/* accounts_sbSW */
void accounts_users_accounts_sbCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

        Widget sb ;
        int i,j, selected_accounts[USERS_LIST_LENGTH] ;
        static int cnt = 0 ;
        char buffer[IMS_COL255_LEN+1] ;
        OP_ACCOUNT *acc_ptr = (OP_ACCOUNT *)NULL ;
	char  temp[IMS_COL30_LEN+1];
	char  temp1[IMS_COL30_LEN+1];
        XmString str ;
        XmStringTable datasets_str ;
        XmScrollBarCallbackStruct *cbs =
         (XmScrollBarCallbackStruct * ) cb ;
        int slider ;
        static int view = 0;
        int create = (int) cd ;


/* get the context of the parent of scroll bar */
	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext(accounts_sbSW);
	{
            
        /* set the call backs for the scrollbar at creation time */
        if ( create == 1 ) {
            
            XtVaGetValues ( accounts_sbSW , 
                    XmNverticalScrollBar, &sb,NULL ) ;
            XtAddCallback ( sb, 
                XmNvalueChangedCallback, accounts_users_accounts_sbCb, NULL) ;
            XtAddCallback ( sb, 
                XmNincrementCallback, accounts_users_accounts_sbCb, NULL ) ;
            XtAddCallback ( sb, 
             XmNdecrementCallback, accounts_users_accounts_sbCb, NULL ) ;
            XtAddCallback ( sb, 
                XmNpageIncrementCallback, accounts_users_accounts_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                XmNpageDecrementCallback, accounts_users_accounts_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoTopCallback, accounts_users_accounts_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNdragCallback, accounts_users_accounts_sbCb,NULL ) ;
            
            return ;
         }
         
         if ( wgt == (Widget) NULL )  /* new list */
           selected_account = 0 ;
         account_window_top = cbs->value ;
         acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;

         /* init this only when: */
         if ( cbs->value == 0 ) {
           cnt = glbData.accounts_users_data.account_data.accounts_cnt ;
           if ( cnt > USERS_LIST_LENGTH )
             view = USERS_LIST_LENGTH ;
           else
             view = cnt ;
         }

         /*Delete all items from  list */
         XmListDeleteAllItems ( accountsSL  ) ;
         /* Update lists */
             
         datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         /* index into accounts list until index begins at window top*/
         while ( (acc_ptr !=NULL) && (acc_ptr->position !=
                                                    account_window_top) ){
           acc_ptr = acc_ptr->next ;
         }
         for ( i = 0 ; i < view && acc_ptr != (OP_ACCOUNT *)NULL ; i ++ ) {
           temp[0] = '\0' ;
           for ( j = 0 ; j < glbData.account_type_count ; j++ )
             if ( acc_ptr->account_type == glbData.account_type[j].item_id )
               strcpy ( temp, glbData.account_type[j].item_name ) ;
           temp1[0] = '\0' ;
           for ( j = 0 ; j < glbData.resource_type_count ; j++ )
             if ( acc_ptr->resource_type == glbData.resource_type[j].item_id )
               strcpy ( temp1, glbData.resource_type[j].item_name ) ;
           sprintf (buffer, " %-16s  %-13s  %s\0",
              acc_ptr->account_id, temp, temp1) ;
           selected_accounts[i] = 0 ;
           if ( acc_ptr->selected ) /* set by accounts_listCb*/
             selected_accounts[i] = 1 ;
           datasets_str[i] = XmStringCreateLocalized ( buffer ) ;
           acc_ptr = acc_ptr->next ;
         }  
         XmListAddItemsUnselected ( accountsSL,datasets_str,view,1 ) ;
         for ( i = 0 ; i < view ; i++ ){
           /* select item previously selected */
           if ( selected_accounts[i] )
             XmListSelectPos ( accountsSL, i+1, False ) ; 
           XmStringFree ( datasets_str[i] ) ;
         }
         XmStringFree ( (XmString) datasets_str ) ;

         /* set slider size and list value */
         
         if ( cnt > USERS_LIST_LENGTH )
           slider = USERS_LIST_LENGTH ;
         else 
           slider = cnt ;
         XtVaGetValues ( accounts_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
         if ( cnt > 0 )
           XtVaSetValues ( sb, XmNmaximum, cnt, XmNvalue,
                    cbs->value, XmNsliderSize, slider, NULL ) ;
         else
           XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue,
                    cbs->value, XmNsliderSize, 1, NULL ) ;
         
	  
       }

   	UxAccounts_usersContext = UxSaveCtx;


}

/*===========================================================================*
** 
** Function Name: accounts_users_create_interfacesCb
**
** Description:		Establish db connection and initialize acc & usr global data 
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - add mwm quit button support to correct PR 942.
**
**==========================================================================*/
void accounts_users_init_popCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClient_data = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_ACC_USR_DATA *acc_usr_data_ptr ;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char Msg[IMS_COL1024_LEN+1];



	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{
        /* check if db connection has been done*/
	if (glbData.accounts_users_data.connect_flag){
		XtPopup(XtParent(glbData.accounts_usersW), XtGrabNone);
                glbData.accounts_usersFlag = 1 ; /* screen has been pop up */
		return;
	}
	else { /* no connection yet, init acc & usr global data */

	  /* Change cursor to watch cursor */
	  timeOutCursors (True);

	  acc_usr_data_ptr = &glbData.accounts_users_data;

	  acc_usr_data_ptr->user_data.users_cnt = 0;
	  acc_usr_data_ptr->user_data.op_user_profile_ptr = 
                                                (OP_USER_PROFILE *)NULL;
	  acc_usr_data_ptr->user_data.assign_users_cnt = 0 ;	
          acc_usr_data_ptr->user_data.op_assign_users_ptr = 
                                                (OP_ASSIGN_USERS *)NULL;
	  acc_usr_data_ptr->user_data.assigned_users_cnt = 0 ;	
          acc_usr_data_ptr->user_data.op_assigned_users_ptr = 
                                                (OP_ASSIGN_USERS *)NULL;
	  acc_usr_data_ptr->user_data.op_account_user_ptr = 
	                                        (OP_ACCOUNT_USER *)NULL;

	  acc_usr_data_ptr->account_data.accounts_cnt = 0 ;
	  acc_usr_data_ptr->account_data.op_account_ptr = (OP_ACCOUNT *)NULL;
	  acc_usr_data_ptr->account_data.assign_datasets_cnt = 0;
	  acc_usr_data_ptr->account_data.op_assign_datasets_ptr =
                                                (OP_ASSIGN_DATASETS *)NULL;
	  acc_usr_data_ptr->account_data.assigned_datasets_cnt = 0;
	  acc_usr_data_ptr->account_data.op_assigned_datasets_ptr =
                                                (OP_ASSIGN_DATASETS *)NULL;
	  acc_usr_data_ptr->account_data.op_account_dataset_ptr =
                                                (OP_ACCOUNT_DATASET *)NULL;

	  catReq = &(acc_usr_data_ptr->catReq);
	  catReq->msgDesc = glbData.msgDesc;
	  catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;

	  userSpec = &(catReq->userSpec);
	  userSpec->dbUserName = glbData.userSpec.userName;
	  userSpec->dbPassword = glbData.userSpec.password;
	  userSpec->server = glbData.userSpec.server;
	  userSpec->dbName = glbData.userSpec.dbName;
 
	  if ((status = ims_op_accCat (catReq,OP_ACC_OPENCONNECTION)) < IMS_OK){

	    /* Change cursor back to normal */
	    timeOutCursors (False);

	    sprintf(Msg, "Database Connection Failed!"); 
	    msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 
	    return;
	  }
          else{
	    acc_usr_data_ptr->connect_flag = 1;
	    if ((status = ims_op_accCat (catReq, OP_ACC_GETCATALOGITEMS))
                                                                 < IMS_OK){
	      /* Change cursor back to normal */
	      timeOutCursors (False);
	      sprintf(Msg, "Couldn't get catalog items!"); 
	      msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 
	      return;
	    }
	    glbData.search_accountsW = create_search_accounts(NO_PARENT);
	    glbData.search_usersW = create_search_users(NO_PARENT);

			/*
			** 06/12/96 - PR942
			** This is to add the callbacks to the window manager quit
			** button for each screen, this is to correct PR 942
			*/
			addWinMgrCloseCB (glbData.search_usersW, search_users_closeCb, NULL);
			addWinMgrCloseCB (glbData.search_accountsW, search_accounts_closeCb, NULL);

	  }
	      /* Change cursor back to normal */
	  timeOutCursors (False);
          XtPopup(XtParent(glbData.accounts_usersW), XtGrabNone);
          glbData.accounts_usersFlag = 1 ; /* screen has been pop up */

	}
 
	}
	UxAccounts_usersContext = UxSaveCtx;


}


/****************************************************************************
**
** FUNCTION : delete_selected_user
**
** DESCRIPTION : Delete a selected user from the Users and Accounts Screen.
**		
** Modified: 05/08/96 - J. Ting  Modified the query to delete a user
**                               only when there is no order associated
**                               with the user_id.
**
****************************************************************************/
static int delete_selected_user ( ) 
{
	
  OP_USER_PROFILE *usr_ptr  ;
  OP_CAT_STRUCT   *catReq;  
  char             buffer[IMS_COL255_LEN*2+1] ;
  int              status;
	int							 count =0;

  status = IMS_OK;
  catReq = &(glbData.accounts_users_data.catReq);  
  usr_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr;

  /* find selected user */
  while ( (usr_ptr != (OP_USER_PROFILE *) NULL ) &&
                    (usr_ptr->selected == 0) )
    usr_ptr = usr_ptr->next ;

  if ( usr_ptr == (OP_USER_PROFILE *) NULL ) 
  {
    msg_dlgCb (msg_dlg, IMS_ERROR, "No User Data!");
    return (IMS_ERROR) ;
  }
  else 
  {
    sprintf (buffer,
	     "delete user_profile "
	     "where (user_id = '%s') "
	     "      and ((select count(*) from order_queue "
	     "            where order_queue.user_id = '%s') "
	     "           <= 0) ",
		/********************************************************************
		** 05-08-96  The following is the original query for this function. 
		**
	  ** "delete user_profile "
	  ** "where (user_id = '%s') "
	  ** "      and ((select count(*) from user_profile up, order_queue oq"
	  ** "            where up.user_id = '%s' "
	  ** "                  and up.user_id = oq.user_id "
	  ** "                  and (oq.status != 4 or oq.status != 12)) "
	  ** "           <= 0) ",
		********************************************************************/
	     usr_ptr->user_id,
	     usr_ptr->user_id);
    catReq->item[1] = (char *) buffer ;

    if ((status = ims_op_accCat (catReq, OP_ACC_DELETEUSER)) < IMS_OK) 
    {
       if (status == IMS_NOROWS)
       {
	  sprintf (buffer,
		   "User %s cannot be deleted.\nUser has orders associated with it.", usr_ptr->user_id);
	  msg_dlgCb (msg_dlg, IMS_ERROR, buffer);
	  return (IMS_ERROR);
      }
      else
      {
	  /* Display error messages */
	  sprintf (buffer, 
		   "Internal Error: deleting user failed.");
	  msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
	  return (IMS_FATAL);
      }
    }
		/* added PR2441*/
    sprintf (buffer,
	     "select account_id from account "
	     "where mgr_user_id = '%s'"
	     ,usr_ptr->user_id);
    catReq->item[1] = (char *) buffer ;
    catReq->item[0] = &count ;

    if ((status = ims_op_accCat (catReq, OP_ACC_GETMANAGER)) < IMS_OK) 
    {
	  /* Display error messages */
	  		sprintf (buffer, 
		   		"Internal Error: get manager failed.");
	  		msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
	  		return (IMS_FATAL);
    }else{
				if ( *(int *)catReq->item[0] != 0 )
				{
	  		  sprintf (buffer,
		   		 "User %s cannot be deleted.\nUser is also an account manager.", usr_ptr->user_id);
	  		  msg_dlgCb (msg_dlg, IMS_ERROR, buffer);
	  		  return (IMS_ERROR);
				}
		}
		/* end of added */

    return (IMS_OK) ;

  }/* end of else */

} /* end of delete_selected_user */


/****************************************************************************
**
** FUNCTION : delete_selected_account
**
** DESCRIPTION : Delete a selected account from the Users and Accounts Screen.
**		
** Modified: 05/08/96 - J. Ting  Modified the query to delete an account
**                               only when there is no order associated
**                               with the account_id.
**
**           10/08/96 - J. Ting  Commented out delete account manager call.
**
****************************************************************************/
static int delete_selected_account ( ) 
{
	
  OP_ACCOUNT    *acc_ptr  ;
  OP_CAT_STRUCT *catReq;  
  char           buffer[IMS_COL255_LEN*2+1] ;
  int            status;

  status = IMS_OK;
  catReq = &(glbData.accounts_users_data.catReq);  
  acc_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;

  /* find selected account */
  while ((acc_ptr != (OP_ACCOUNT *) NULL ) &&
	 (acc_ptr->selected == 0) )
    acc_ptr = acc_ptr->next ;

  if ( acc_ptr == (OP_ACCOUNT *) NULL ) 
  {
    msg_dlgCb (msg_dlg, IMS_ERROR, "No account Data!");
    return (IMS_ERROR) ;
  }
  else 
  {
    sprintf (buffer,
	     "delete account "
	     "where (account_id = '%s')"
	     "      and ((select count (*) from order_queue "
	     "            where order_queue.account_id = '%s')"
	     "           <= 0) ",
		/********************************************************************
		** 05-08-96  The following is the original query for this function. 
		**
	  ** "delete account "
	  ** "where (account_id = '%s')"
	  ** "      and ((select count (*) from account ac, order_queue oq "
	  ** "            where ac.account_id = '%s' "
	  ** "                  and ac.account_id = oq.account_id "
	  ** "                  and (oq.status != 4 or oq.status != 12)) "
	  ** "           <= 0) ",
		********************************************************************/
	     acc_ptr->account_id,
	     acc_ptr->account_id);
    catReq->item[1] = (char *) buffer;

    if ((status = ims_op_accCat (catReq, OP_ACC_DELETEACCOUNT)) < IMS_OK) 
    {
      if (status == IMS_NOROWS)
      {
	 sprintf (buffer, 
		  "Account %s cannot be deleted.\nAccount has orders associated with it.", acc_ptr->account_id);
	 msg_dlgCb (msg_dlg, IMS_ERROR, buffer);
	 return (IMS_ERROR);
      }
      else
      {
	 /* Display error messages */
	 sprintf (buffer, "Internal Error: deleting account failed.");
	 msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
	 return (IMS_FATAL); 
      }
    }

    /* delete account_mgr related to this account */
		/**********************************************************************
    sprintf (buffer,
	     "delete account_mgr where account_id = '%s'",
	     acc_ptr->account_id);
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_DELETEACCOUNTMGR) < IMS_OK) 
    {
      ** Display error messages **
      sprintf(buffer, "Internal Error: deleting account manager failed.");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      return (IMS_FATAL) ; 
    }
	  **********************************************************************/
		/* delete account_user */
    sprintf (buffer,
	     "delete account_user where account_id = '%s'",
	     acc_ptr->account_id);
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_DELETEACCOUNTUSER) < IMS_OK) 
    {
      sprintf(buffer, "Internal Error: deleting account user failed.");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      return (IMS_FATAL) ; 
    }

		/* delete account_dataset */
    sprintf (buffer,
	     "delete account_dataset where account_id = '%s'",
	     acc_ptr->account_id);
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_DELETEACCOUNTDATASET) < IMS_OK) 
    {
      sprintf(buffer, "Internal Error: deleting account dataset failed.");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      return (IMS_FATAL) ; 
    }

    sprintf (buffer,
	     "delete account_extension where account_id = '%s'",
	     acc_ptr->account_id);
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_DELETEACCOUNTEXTENSION) < IMS_OK) 
    {
      sprintf(buffer, "Internal Error: deleting account extension failed.");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      return (IMS_FATAL) ; 
    }

    return (IMS_OK) ;

  }/* end of else */

} /* end of delete_selected_account */

/****************************************************************************
**
**
** FUNCTION : get_users_assigned_to_account
**
** DESCRIPTION : Gets the user_id and name from the users assigned to
**		 the selected account.
**		
** Modified: 04/22/96 - J. Ting Initialize item[0] to int count to 
**                      fix problem reports 754 and 755.
**
****************************************************************************/
static int  get_users_assigned_to_account ( char * acc_id) 
{
	
  OP_ACCOUNT *acc_ptr  ;
  OP_CAT_STRUCT *catReq;  
  char buffer[IMS_COL255_LEN+1] ;
	int count = 0;

  catReq = &(glbData.accounts_users_data.catReq);  
  buffer[0] = '\0' ;

	/*
	** 04-22-96  Assign catReq->item[0] to count
	*/
  catReq->item[0] = (int *) &count;

  /* first get all users */
  strcpy (buffer,
	  "select user_id, first_name, initial_name, last_name "
	  "from user_profile order by user_id") ;
  catReq->item[1] = (char *) buffer ;

  if (ims_op_accCat (catReq, OP_ACC_GET_ALL_USERS) < IMS_OK) 
  {
    /* Display error messages */
    sprintf(buffer, "Internal Error: Couldn't get users.");   
    msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
    return (IMS_FATAL) ; 
  }

  /* get data */
  glbData.accounts_users_data.user_data.assign_users_cnt = 
    *(int *)catReq->item[0];
  glbData.accounts_users_data.user_data.op_assign_users_ptr  = 
    (OP_ASSIGN_USERS *)catReq->item[2];    

  /* get users assigned to the selected account */
  buffer[0] = '\0' ;
  sprintf (buffer, 
	   "select user_id, first_name, initial_name, last_name "
	   "from user_profile "
	   "where user_id in "
	   "      (select user_id "
	   "       from account_user "
	   "       where account_id='%s') "
	   "order by user_id",
	   acc_id) ;
  catReq->item[1] = (char *) buffer;

  if (ims_op_accCat (catReq, OP_ACC_GET_ASSIGN_USERS) < IMS_OK) 
  {
    /* free assign users, assigned_users, and account_user lists */
    free_assign_users ();
    free_assigned_users ();

    /* Display error messages */
    sprintf(buffer,
	    "Internal Error: Couldn't get users assigned to account");   
    msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
    return (IMS_FATAL) ; 
  }

  /* get data */
  glbData.accounts_users_data.user_data.assigned_users_cnt = 
    *(int *)catReq->item[0];
  glbData.accounts_users_data.user_data.op_assigned_users_ptr  = 
    (OP_ASSIGN_USERS *)catReq->item[2]; 
  glbData.accounts_users_data.user_data.op_account_user_ptr =
    (OP_ACCOUNT_USER *)catReq->item[3];

  return (IMS_OK) ;

} /* end of get_users_assigned_to_account */


/****************************************************************************
**
**
** FUNCTION : get_datasets_assigned_to_account
**
** DESCRIPTION : Gets the datasets assigned to the selected account
**               from the dataset_relation and account_dataset tables.
**		
**
** Modified :  04-23-96  J. Ting  Assign catReq->item[0] to int count
**
****************************************************************************/
static int  get_datasets_assigned_to_account ( char * acc_id) 
{
	
  OP_ACCOUNT *acc_ptr  ;
  OP_CAT_STRUCT *catReq;  
  char buffer[IMS_COL255_LEN*2+1] ;
	int count = 0;

  catReq = &(glbData.accounts_users_data.catReq);  
  buffer[0] = '\0' ;

	/*
	** 04-23-96  Assign catReq->item[0] to count
	*/
  catReq->item[0] = (int *) &count;

  /* first get all datasets */
  strcpy (buffer,
	  "select dataset_idx, dataset, sensor, platform "
	  "from dataset_relation "
	  "order by dataset_idx, dataset" ) ;

  catReq->item[1] = (char *) buffer ;

  if (ims_op_accCat (catReq, OP_ACC_GET_ALL_DATASETS) < IMS_OK) 
  {
    /* Display error messages */
    sprintf(buffer,"Internal Error: Couldn't get datasets.");   
    msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
    return (IMS_FATAL) ; 
  }

  /* get data */
  glbData.accounts_users_data.account_data.assign_datasets_cnt = 
    *(int *)catReq->item[0];
  glbData.accounts_users_data.account_data.op_assign_datasets_ptr  = 
    (OP_ASSIGN_DATASETS *)catReq->item[2];    

  /* get datasets assigned to the selected account */
  buffer[0] = '\0' ;
  sprintf (buffer,
	   "select t1.dataset_idx, t1.dataset, t1.sensor,"
	   "       t1.platform, t2.oagdr "
	   "from dataset_relation t1, account_dataset t2 "
	   "where t1.dataset_idx = t2.dataset_idx "
	   "      and t2.account_id = '%s' "
	   "order by t1.dataset_idx, t1.dataset", 
	   acc_id ) ;
  catReq->item[1] = (char *) buffer ;

  if (ims_op_accCat (catReq, OP_ACC_GET_ASSIGN_DATASETS) < IMS_OK) 
  {
    /* free assign datasets, assigned datasets, and account_dataset lists */
    free_assign_datasets ();
    free_assigned_datasets ();

    /* Display error messages */
    sprintf(buffer,
	    "Internal Error: Couldn't get datasets assigned to account.");  
    msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
    return (IMS_FATAL) ; 
  }

  /* get data */
  glbData.accounts_users_data.account_data.assigned_datasets_cnt = 
    *(int *)catReq->item[0];
  glbData.accounts_users_data.account_data.op_assigned_datasets_ptr  = 
    (OP_ASSIGN_DATASETS *)catReq->item[2];
  glbData.accounts_users_data.account_data.op_account_dataset_ptr = 
    (OP_ACCOUNT_DATASET *)catReq->item[3];

  return (IMS_OK) ;

} /* end of get_datasets_assigned_to_account */


void template ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClient_data = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{

	}
	UxAccounts_usersContext = UxSaveCtx;


}

/*===========================================================================*
**
** Function Name: accounts_users_goto_welcomeCb
**
** Description:     Pop up the welcome screen from accounts users screen
**
** Arguments:           1. widget - Widget that is calling this callback
**                              2. cd       - not used
**                              3. cbs      - not used
**
** Return Value:    None
**
** Revision History:
**
**==========================================================================*/
 
void    accounts_users_goto_welcomeCb(
    Widget wgt,
    XtPointer cd,
    XtPointer cb)
{
    _UxCaccounts_users      *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
 
 
    UxSaveCtx = UxAccounts_usersContext;
    UxAccounts_usersContext = UxContext =
            (_UxCaccounts_users *) UxGetContext( glbData.accounts_usersW );
    {
        XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
        glbData.welcomeFlag = 1;
    }
 
    UxAccounts_usersContext = UxSaveCtx;
}

