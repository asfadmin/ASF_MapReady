static char *sccs = "@(#)ims_op_accAssignUserCb.c	5.3  07/23/96";
/*******************************************************************************
**
** File:		ims_op_accAssignUserCb.c
**
** Function:		Callback functions for the assign_users screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Modified:
**    12-06-95  Alin Tilden  Modified functions assign_users_add_usersCb,
**                           assign_users_closeCb, free_assign_users,
**                           assign_users_delete_usersCb,and added function
**                           assign_users_updateCb in order to add an
**                           UPDATE option and to prevent executing all changes
**                           made on the screen to the catalog automatically.
**
**		04-23-96  J. Ting			 Modified function assign_datasets_updateCb. 
**                           Initialized catReq->item[0], see R1BP problem
**                           reports 754 and 755.
**
**    06-12-96  J. Ting      Modified function assign_users_closeCb for
**                           PR 942.
**
******************************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
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

extern void msg_dlgCb( Widget , int , char *) ;
extern void prepare_message_box (SELECTED_BUTTON ** , char *, char *  ) ;
extern OP_GLOBAL_DATA glbData;
extern Widget msg_dlg ;


#define ASSIGN_USERS_LIST_LENGTH 21

static int user_window_top = 0 ;
static int assigned_window_top = 0 ;
static int selected_user = 0 ;
static int selected_assigned = 0 ;
void assign_users_users_sbCb ( Widget , XtPointer , XtPointer ) ;
void assign_users_assigned_sbCb (Widget, XtPointer, XtPointer ) ;
void free_assign_users ( ) ;
void free_assigned_users ( ) ;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAssignUser.h"
#undef CONTEXT_MACRO_ACCESS


void assign_users_users_listCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        int i ;
	XmListCallbackStruct * cbs = (XmListCallbackStruct *) cb;
        OP_ASSIGN_USERS *usr_ptr, *temp ;

	UxSaveCtx = UxAssign_usersContext;
	UxAssign_usersContext = UxContext =
			(_UxCassign_users *) UxGetContext( UxWidget );
	{
          /* Since our list is application defined (virtual) we have  */
          /*to keep track of the selections.                        */
	  selected_user = 1 ;
          usr_ptr = glbData.accounts_users_data.user_data.op_assign_users_ptr ;
          temp = usr_ptr ;

          if (cbs->reason == XmCR_EXTENDED_SELECT ) {  
            if (cbs->selection_type == XmINITIAL) {
/* deselect the all list                                     */
              while (temp != (OP_ASSIGN_USERS *) NULL ) {
                temp->selected = 0 ;
                temp = temp->next ;
              }
              
            }
            /**else if (cbs->selection_type == XmMODIFICATION) {
              
            }
            else 
            **/
          }
/* get to the window top as set by the scroll bar */
          while ( (usr_ptr != (OP_ASSIGN_USERS *) NULL ) &&
                    (usr_ptr->position != user_window_top) )
            usr_ptr = usr_ptr->next ;
/* select the items selected for this part of the list   */
          temp = usr_ptr ;
          while ( (temp != (OP_ASSIGN_USERS *) NULL ) && 
                    (temp->position < 
                      ( user_window_top+ASSIGN_USERS_LIST_LENGTH) ) ) {
                temp->selected = 0 ;
                for ( i = 0 ; i < cbs->selected_item_count ; i++ )
                  if ( temp->position == user_window_top +
                                        cbs->selected_item_positions[i] - 1 ){
                    temp->selected = 1 ;
                    break ;
                  }
                temp = temp->next ;
          }

	}
	UxAssign_usersContext = UxSaveCtx;
}


void assign_users_assigned_listCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        int i ;
	XmListCallbackStruct * cbs = (XmListCallbackStruct *) cb;
        OP_ASSIGN_USERS *usr_ptr, *temp ;

	UxSaveCtx = UxAssign_usersContext;
	UxAssign_usersContext = UxContext =
			(_UxCassign_users *) UxGetContext( UxWidget );
	{
          /* Since our list is application defined (virtual) we have  */
          /*to keep track of the selections.                        */
	  selected_assigned = 1 ;
          usr_ptr =glbData.accounts_users_data.user_data.op_assigned_users_ptr ;
          temp = usr_ptr ;

          if (cbs->reason == XmCR_EXTENDED_SELECT ) {  
            if (cbs->selection_type == XmINITIAL) {
/* deselect the all list                                     */
              while (temp != (OP_ASSIGN_USERS *) NULL ) {
                temp->selected = 0 ;
                temp = temp->next ;
              }
              
            }
            /**else if (cbs->selection_type == XmMODIFICATION) {
              
            }
            else 
            **/
          }
/* get to the window top as set by the scroll bar */
          while ( (usr_ptr != (OP_ASSIGN_USERS *) NULL ) &&
                    (usr_ptr->position != assigned_window_top) )
            usr_ptr = usr_ptr->next ;
/* select the items selected for this part of the list   */
          temp = usr_ptr ;
          while ( (temp != (OP_ASSIGN_USERS *) NULL ) && 
                    (temp->position < 
                      ( assigned_window_top+ASSIGN_USERS_LIST_LENGTH) ) ) {
                temp->selected = 0 ;
                for ( i = 0 ; i < cbs->selected_item_count ; i++ )
                  if ( temp->position == assigned_window_top +
                                        cbs->selected_item_positions[i] - 1 ){
                    temp->selected = 1 ;
                    break ;
                  }
                temp = temp->next ;
          }


	}
	UxAssign_usersContext = UxSaveCtx;
}


void	assign_users_users_sbCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
  _UxCassign_users          *UxSaveCtx, *UxContext;
  Widget                     UxWidget = wgt;
  Widget                     sb ;
  int                        i,j ;
  int                        selected_users[ASSIGN_USERS_LIST_LENGTH] ;
  static                     cnt=0 ;
  char                       buffer[IMS_COL255_LEN+1] ;
  char                       temp[IMS_COL30_LEN+1];
  static OP_ASSIGN_USERS    *user_ptr = (OP_ASSIGN_USERS  *)NULL ;

  XmString                   str ;
  XmStringTable              datasets_str ;
  XmScrollBarCallbackStruct *cbs =
    (XmScrollBarCallbackStruct * ) cb ;
  int                        slider ;
  static int                 view ;
  int                        create = (int) cd ;
  XmFontList                 fontlist ;
  int                        width ;

  /* get the context of the  scroll bar */

  UxSaveCtx = UxAssign_usersContext;
  UxAssign_usersContext = UxContext =
    (_UxCassign_users *) UxGetContext(assign_users);

  {
    /* set the call backs for the scrollbar at creation time */
    if ( create == 1 )
    {
      XtVaGetValues (assign_users_sbSW , 
		     XmNverticalScrollBar, &sb,NULL ) ;
      XtAddCallback (sb, 
		     XmNvalueChangedCallback, assign_users_users_sbCb, NULL) ;
      XtAddCallback (sb, 
		     XmNincrementCallback, assign_users_users_sbCb, NULL ) ;
      XtAddCallback (sb, 
		     XmNdecrementCallback, assign_users_users_sbCb, NULL ) ;
      XtAddCallback (sb, 
		     XmNpageIncrementCallback, assign_users_users_sbCb,NULL ) ;
      XtAddCallback (sb, 
		     XmNpageDecrementCallback, assign_users_users_sbCb,NULL ) ;
      XtAddCallback (sb, 
		     XmNtoTopCallback, assign_users_users_sbCb,NULL ) ;
      XtAddCallback (sb, 
		     XmNtoBottomCallback, assign_users_users_sbCb,NULL ) ;
      XtAddCallback (sb, 
		     XmNdragCallback, assign_users_users_sbCb,NULL ) ;
            
      return ;
      
    }

    if ( wgt == (Widget) NULL )  /* new list */
      selected_user = 0 ;
    user_window_top = cbs->value ;
    user_ptr = glbData.accounts_users_data.user_data.op_assign_users_ptr ;

    /* initialize this only when: */
    if ( cbs->value == 0 ) 
    {
      cnt = glbData.accounts_users_data.user_data.assign_users_cnt ;
      if ( cnt > ASSIGN_USERS_LIST_LENGTH )
	view = ASSIGN_USERS_LIST_LENGTH ;
      else
	view = cnt ;
    }

    /*Delete all items from  list */
    XmListDeleteAllItems ( assign_usersSL  ) ;

    /* Update lists */
    datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;

    /* index into users list until index begins at window top */
    while ((user_ptr != NULL) 
	   && (user_ptr->position != user_window_top) )
    {
      user_ptr = user_ptr->next ;
    }

    for (i = 0 ; i < view && user_ptr != (OP_ASSIGN_USERS *)NULL ; i ++ )
    {
      temp[0] = '\0' ;
      sprintf (buffer,
	       "     %-16s  %s %s %s\0",
	       user_ptr->user_id, 
	       user_ptr->last_name,
	       user_ptr->first_name, user_ptr->initial_name ) ;
      selected_users[i] = 0 ;
      if ( user_ptr->selected ) /* set by users_listCb */
	selected_users[i] = 1 ;
      datasets_str[i] = XmStringCreateLocalized ( buffer ) ;
      user_ptr = user_ptr->next ;
    }  

    /* Add items to list unselected */           
    XmListAddItemsUnselected ( assign_usersSL,datasets_str,view,1 ) ;
    /* Set selection policy to multiple select */
    XtVaSetValues (assign_usersSL, XmNselectionPolicy,XmMULTIPLE_SELECT,
		   NULL );
    for ( i = 0 ; i < view ; i++ ) 
    {
      /* select list item previously selected */
      if (selected_users[i])
	XmListSelectPos (assign_usersSL, i+1, False ) ;
      XmStringFree ( datasets_str[i] ) ;
    }
    XmStringFree ( (XmString)datasets_str ) ;
    /* Change back to extended select */
    XtVaSetValues (assign_usersSL, XmNselectionPolicy,XmEXTENDED_SELECT,
		   NULL );

    /* set slider size and list value */
    if ( cnt > ASSIGN_USERS_LIST_LENGTH )
      slider = ASSIGN_USERS_LIST_LENGTH ;
    else 
      slider = cnt ;

    XtVaGetValues ( assign_users_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
    /* XtVaGetValues ( sb, XmNmaximum, XmNsliderSize, &slider, NULL ) ;*/
    if ( cnt > 0 ) 
    {
      XtVaSetValues (sb, XmNmaximum, cnt, XmNvalue,
		     cbs->value, XmNsliderSize, slider, NULL ) ;

      /*XtVaGetValues ( sb, XmNmaximum, XmNsliderSize, &slider, NULL ) ;*/
      
    }
    else
      XtVaSetValues (sb, XmNmaximum, 1, XmNvalue,
		     cbs->value, XmNsliderSize, 1, NULL ) ;
  }
  UxAssign_usersContext = UxSaveCtx;
}


void	assign_users_assigned_sbCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        Widget sb ;
        int i,j ;
        int selected_users[ASSIGN_USERS_LIST_LENGTH] ;
        static cnt=0 ;
        char  buffer[IMS_COL255_LEN+1] ;
	char  temp[IMS_COL30_LEN+1];
        static OP_ASSIGN_USERS  *user_ptr = (OP_ASSIGN_USERS  *)NULL ;

        XmString str ;
        XmStringTable datasets_str ;
        XmScrollBarCallbackStruct *cbs =
         (XmScrollBarCallbackStruct * ) cb ;
        int slider ;
        static int view ;
        int create = (int) cd ;
                                 
        /* get the context of the  scroll bar */

	UxSaveCtx = UxAssign_usersContext;
	UxAssign_usersContext = UxContext =
			(_UxCassign_users *) UxGetContext(assign_users);
	{
	
        /* set the call backs for the scrollbar at creation time */
        if ( create == 1 ) {
            
            XtVaGetValues ( assigned_users_sbSW , 
                    XmNverticalScrollBar, &sb,NULL ) ;
            XtAddCallback ( sb, 
                XmNvalueChangedCallback, assign_users_assigned_sbCb, NULL) ;
            XtAddCallback ( sb, 
                XmNincrementCallback, assign_users_assigned_sbCb, NULL ) ;
            XtAddCallback ( sb, 
             XmNdecrementCallback, assign_users_assigned_sbCb, NULL ) ;
            XtAddCallback ( sb, 
                XmNpageIncrementCallback, assign_users_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                XmNpageDecrementCallback, assign_users_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoTopCallback, assign_users_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoBottomCallback, assign_users_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNdragCallback, assign_users_assigned_sbCb,NULL ) ;
            
            return ;

         }
         if ( wgt == (Widget) NULL )  /* new list */
           selected_assigned = 0 ;
         assigned_window_top = cbs->value ;
         user_ptr =glbData.accounts_users_data.user_data.op_assigned_users_ptr ;
         /* initialize this only when: */
         if ( cbs->value == 0 ) {
           cnt = glbData.accounts_users_data.user_data.assigned_users_cnt ;
           if ( cnt > ASSIGN_USERS_LIST_LENGTH )
            view = ASSIGN_USERS_LIST_LENGTH ;
           else
            view = cnt ;
         }
         /*Delete all items from  list */
         XmListDeleteAllItems ( assigned_usersSL  ) ;
         /* Update lists */
             
         datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         /* index into users list until index begins at window top */
         while ( (user_ptr != NULL) && (user_ptr->position !=
                                                     assigned_window_top) ){
           user_ptr = user_ptr->next ;
         }
         for (i = 0 ; i < view && user_ptr != (OP_ASSIGN_USERS *)NULL ; i ++ )
	 {
           temp[0] = '\0' ;
	   sprintf (buffer,
		    "     %-16s  %s %s %s\0",
		    user_ptr->user_id, 
		    user_ptr->last_name,
		    user_ptr->first_name, user_ptr->initial_name ) ;
	   selected_users[i] = 0 ;
	   if (user_ptr->selected)   /* set by users_listCb */
	       selected_users[i] = 1 ;
	   datasets_str[i] = XmStringCreateLocalized ( buffer ) ;

           user_ptr = user_ptr->next ;
         }  
/* Add items to list unselected */           
         XmListAddItemsUnselected ( assigned_usersSL,datasets_str,view,1 ) ;
/* Set selection policy to multiple select */
         XtVaSetValues ( assigned_usersSL, XmNselectionPolicy,XmMULTIPLE_SELECT,
                                                                        NULL );
         for ( i = 0 ; i < view ; i++ ) {
           /* select list item previously selected */
           if (selected_users[i])
             XmListSelectPos (assigned_usersSL, i+1, False ) ;
           XmStringFree ( datasets_str[i] ) ;
         }
         XmStringFree ( (XmString)datasets_str ) ;
/* Change back to extended select */
         XtVaSetValues ( assigned_usersSL, XmNselectionPolicy,XmEXTENDED_SELECT,
                                                                        NULL );
         /* set slider size and list value */
         
         if ( cnt > ASSIGN_USERS_LIST_LENGTH )
           slider = ASSIGN_USERS_LIST_LENGTH ;
         else 
           slider = cnt ;
         XtVaGetValues ( assigned_users_sbSW, XmNverticalScrollBar, &sb,NULL ) ;
          /* XtVaGetValues ( sb, XmNmaximum, XmNsliderSize, &slider, NULL ) ;*/
         if ( cnt > 0 ) {
           XtVaSetValues ( sb, XmNmaximum, cnt, XmNvalue,
                    cbs->value, XmNsliderSize, slider, NULL ) ;

            /*XtVaGetValues ( sb, XmNmaximum, XmNsliderSize, &slider, NULL ) ;*/
  
         }
         else
           XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue,
                    cbs->value, XmNsliderSize, 1, NULL ) ;
	}
	UxAssign_usersContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: assign_users_closeCb
**
** Description:		callback function for close button and mwm quit button.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/12/96 - modified to use glbData.assign_usersW
**                   to get the context instead of using UxWidget. This
**                   is done for mwm quit button to work - PR 942.
**
**==========================================================================*/
void	assign_users_closeCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	int i ;

	UxSaveCtx = UxAssign_usersContext;
	UxAssign_usersContext = UxContext =
			(_UxCassign_users *) UxGetContext( glbData.assign_usersW );
	{
	  free_assign_users ( ) ;
	  free_assigned_users ( ) ;
          glbData.assign_usersFlag = 0 ; /* screen has been pop down */
	  XtPopdown (XtParent (glbData.assign_usersW)) ;

	}
	UxAssign_usersContext = UxSaveCtx;
} 

void assign_users_init_dataCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
  _UxCassign_users        *UxSaveCtx, *UxContext;
  Widget                  UxWidget = wgt;
  OP_ACCOUNT *acc_ptr = (OP_ACCOUNT *) cd;
  char buffer[100] ;
  XmScrollBarCallbackStruct *cbs ;
  UxSaveCtx = UxAssign_usersContext;
  UxAssign_usersContext = UxContext =
    (_UxCassign_users *) UxGetContext( UxWidget );
  {

    /* set the text fields with the account data */
    XmTextSetString(account_idTF ,acc_ptr->account_id ) ;
    XmTextSetString(creationTF ,acc_ptr->create_time ) ;
    XmTextSetString(expirationTF ,acc_ptr->expire_time ) ;
    buffer [0] = '\0' ;
    sprintf(buffer, "%f", (float)acc_ptr->curr_balance ) ;
    XmTextSetString(current_balanceTF , buffer ) ;

    /*  set the lists data */
    cbs = ( XmScrollBarCallbackStruct * )
      XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
    cbs->value = 0 ; 
    
    assign_users_users_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
    assign_users_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
    
    XtFree ((char *)cbs) ;
  }

  UxAssign_usersContext = UxSaveCtx;
}

/*===========================================================================*
**
** Function Name:   free_assign_users
**
** Description:     Function for freeing the linked list:
**                  op_assign_users.
**
** Arguments:       None
**
** Return Value:    None
**
** Revision History:
**     12-05-95  Alin Tilden  Modified to free only the op_assign_users list.
**
**==========================================================================*/

void free_assign_users ( ) 
{
  
  OP_ASSIGN_USERS *curr_ptr, *last_ptr ;

  if ( glbData.accounts_users_data.user_data.assign_users_cnt > 0 ) 
  {
    glbData.accounts_users_data.user_data.assign_users_cnt = 0 ;
    curr_ptr = glbData.accounts_users_data.user_data.op_assign_users_ptr ;
    while ( curr_ptr != (OP_ASSIGN_USERS *) NULL ) 
    {
      last_ptr = curr_ptr->next ;
      free (curr_ptr) ;
      curr_ptr = last_ptr ;
    }
    glbData.accounts_users_data.user_data.op_assign_users_ptr = 
      (OP_ASSIGN_USERS *) NULL;
  }

}/* end free_assign_users */



/*===========================================================================*
**
** Function Name:   free_assigned_users
**
** Description:     Function for freeing the linked lists:
**                  op_assigned_users, and op_account_user.
**
** Arguments:       None
**
** Return Value:    None
**
**==========================================================================*/

void free_assigned_users ( ) 
{
  
  OP_ASSIGN_USERS *curr_ptr, *last_ptr;
  OP_ACCOUNT_USER *curr_accusr_ptr, *last_accusr_ptr;

  if ( glbData.accounts_users_data.user_data.assigned_users_cnt > 0 ) 
  {
    glbData.accounts_users_data.user_data.assigned_users_cnt = 0 ;
    curr_ptr = glbData.accounts_users_data.user_data.op_assigned_users_ptr ;
    while ( curr_ptr != (OP_ASSIGN_USERS *) NULL ) 
    {
      last_ptr = curr_ptr->next ;
      free (curr_ptr) ;
      curr_ptr = last_ptr ;
    }
    glbData.accounts_users_data.user_data.op_assigned_users_ptr =
      (OP_ASSIGN_USERS *)NULL;

    curr_accusr_ptr = glbData.accounts_users_data.user_data.op_account_user_ptr;
    while (curr_accusr_ptr != (OP_ACCOUNT_USER *)NULL)
    {
      last_accusr_ptr = curr_accusr_ptr->next;
      free (curr_accusr_ptr);
      curr_accusr_ptr = last_accusr_ptr;
    }
    glbData.accounts_users_data.user_data.op_account_user_ptr =
      (OP_ACCOUNT_USER *)NULL;
  }

}/* end free_assigned_users */


 
/*===========================================================================*
**
** Function Name:   assign_users_add_usersCb
**
** Description:     Callback function for the ASSIGN USERS widget
**
** Arguments:       1. widget - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
**     12-04-95  Alin Tilden  Modified to maintain op_assigned_users list
**                            and op_account_user list internally instead of
**                            writing modifications directly to the catalog.
**
**==========================================================================*/

void	assign_users_add_usersCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCassign_users        *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
    OP_ASSIGN_USERS        *assign_ptr;
    OP_ASSIGN_USERS        *assigned_ptr, *temp, *new_user_ptr;
    OP_ACCOUNT_USER        *acc_user_ptr, *tmp_accusr_ptr, *new_accusr_ptr;
    int cnt;
    int match;
    XmScrollBarCallbackStruct *cbs;

    UxSaveCtx = UxAssign_usersContext;
    UxAssign_usersContext = UxContext =
	(_UxCassign_users *) UxGetContext( UxWidget );
    {
	  
	assign_ptr = 
	    glbData.accounts_users_data.user_data.op_assign_users_ptr;
	assigned_ptr = 
	    glbData.accounts_users_data.user_data.op_assigned_users_ptr;
	acc_user_ptr = 
	    glbData.accounts_users_data.user_data.op_account_user_ptr;

	/* check all users since we have the extended select option */
	cnt = 0;
	while (assign_ptr != (OP_ASSIGN_USERS *) NULL ) 
	{
	    match = 0;
	    assigned_ptr = 
		glbData.accounts_users_data.user_data.op_assigned_users_ptr;

	    if ( assign_ptr->selected ) 
	    {
		/* check that user is not already in the assigned user list */
		cnt++;
		temp = assigned_ptr;
		while ( temp != (OP_ASSIGN_USERS *) NULL ) 
		{
		    if (strcmp (assign_ptr->user_id, temp->user_id) == 0) 
		    {
			match = 1;
			break;
		    }
		    temp = temp->next ;
		}
		/* if a match is found, skip this user */
		if (match == 1)
		{
		    assign_ptr = assign_ptr->next;
		    continue;
		}

		/* add user to assigned users linked list */
		if ((new_user_ptr = 
		     (OP_ASSIGN_USERS *)malloc(sizeof(OP_ASSIGN_USERS)))
		    == (OP_ASSIGN_USERS *)NULL)
		{
		    msg_dlgCb (msg_dlg, IMS_FATAL, "Memory allocation failed.");
		    return;
		}
		else
		{
		    new_user_ptr->prev = (OP_ASSIGN_USERS *)NULL;
		    new_user_ptr->next = (OP_ASSIGN_USERS *)NULL;
		    new_user_ptr->account_user_ptr = (OP_ACCOUNT_USER *)NULL;
		    new_user_ptr->selected = 0;
		    new_user_ptr->position = 0;
		    strcpy (new_user_ptr->user_id, assign_ptr->user_id);
		    strcpy (new_user_ptr->first_name, assign_ptr->first_name);
		    strcpy (new_user_ptr->initial_name, 
			    assign_ptr->initial_name);
		    strcpy (new_user_ptr->last_name, assign_ptr->last_name);
		
		    if (assigned_ptr == (OP_ASSIGN_USERS *)NULL)
		    {
			glbData.accounts_users_data.user_data.
			    op_assigned_users_ptr = new_user_ptr;
		    }
		    else
		    {
			/* 
			 * find the right place in the assigned users list 
			 * to add the new user.
			 */
			while ((assigned_ptr->next != (OP_ASSIGN_USERS *)NULL)
			       &&
			       (strcmp (new_user_ptr->user_id, 
					assigned_ptr->user_id) > 0))
			{
			    assigned_ptr = assigned_ptr->next;
			}
			
			if (strcmp (new_user_ptr->user_id, 
				    assigned_ptr->user_id) < 0)
			{
			    /* insert before */
			    temp = assigned_ptr->prev;
			    assigned_ptr->prev = new_user_ptr;
			    new_user_ptr->prev = temp;
			    new_user_ptr->next = assigned_ptr;

			    if (temp != (OP_ASSIGN_USERS *)NULL)
				temp->next = new_user_ptr;
			    else
				glbData.accounts_users_data.user_data.
				    op_assigned_users_ptr = new_user_ptr;
			}
			else
			{
			    /* insert after */
			    temp = assigned_ptr->next;
			    assigned_ptr->next = new_user_ptr;
			    new_user_ptr->prev = assigned_ptr;
			    new_user_ptr->next = temp;

			    if (temp != (OP_ASSIGN_USERS *)NULL)
				temp->prev = new_user_ptr;
			}
		    } /* end of if (assigned_ptr != NULL) */

		    /* 
		     * if user_id is not already in account_user list, add it.
		     * Note: user_id is unique in the account_user list.
		     */
		    acc_user_ptr =
			glbData.accounts_users_data.user_data.op_account_user_ptr;
		    if (acc_user_ptr != (OP_ACCOUNT_USER *) NULL)
		    {
			while ((acc_user_ptr->next != (OP_ACCOUNT_USER *) NULL)
			       && 
			       (strcmp (acc_user_ptr->user_id, 
					new_user_ptr->user_id) < 0))
			{
			    acc_user_ptr = acc_user_ptr->next;
			}

			if (strcmp (new_user_ptr->user_id, 
				    acc_user_ptr->user_id) == 0)
			{
			    /* 
			     * this is an attempt to add an already existing 
			     * account_user entry, do nothing.
			     */
			    if (acc_user_ptr->account_user_status == DELETED)
			    {
				acc_user_ptr->account_user_status = UNCHANGED;
			    }
			    new_user_ptr->account_user_ptr = acc_user_ptr;
			    assign_ptr = assign_ptr->next ;
			    continue;
			}
		    }

		    /* add a new account_user entry */
		    if ((new_accusr_ptr = 
			 (OP_ACCOUNT_USER *)malloc(sizeof(OP_ACCOUNT_USER)))
			== (OP_ACCOUNT_USER *)NULL)
		    {
			msg_dlgCb (msg_dlg, 
				   IMS_FATAL, 
				   "Memory allocation failed.");
			return;
		    }
		    else
		    {
			new_accusr_ptr->prev = (OP_ACCOUNT_USER *)NULL;
			new_accusr_ptr->next = (OP_ACCOUNT_USER *)NULL;
			strcpy (new_accusr_ptr->user_id, new_user_ptr->user_id);
			new_accusr_ptr->account_user_status = NEW;
			new_user_ptr->account_user_ptr = new_accusr_ptr;
			
			if (acc_user_ptr == (OP_ACCOUNT_USER *) NULL)
			{
			    glbData.accounts_users_data.user_data.
				op_account_user_ptr = new_accusr_ptr;
			}
			else
			{
			    if (strcmp (new_accusr_ptr->user_id, 
					acc_user_ptr->user_id) < 0)
			    {
				/* insert before */
				tmp_accusr_ptr = acc_user_ptr->prev;
				acc_user_ptr->prev = new_accusr_ptr;
				new_accusr_ptr->prev = tmp_accusr_ptr;
				new_accusr_ptr->next = acc_user_ptr;

				if (tmp_accusr_ptr != (OP_ACCOUNT_USER *)NULL)
				    tmp_accusr_ptr->next = new_accusr_ptr;
				else
				    glbData.accounts_users_data.user_data.
					op_account_user_ptr = new_accusr_ptr;
			    }
			    else
			    {
				/* insert after */
				tmp_accusr_ptr = acc_user_ptr->next;
				acc_user_ptr->next = new_accusr_ptr;
				new_accusr_ptr->prev = acc_user_ptr;
				new_accusr_ptr->next = tmp_accusr_ptr;
			    
				if (tmp_accusr_ptr != (OP_ACCOUNT_USER *)NULL)
				    tmp_accusr_ptr->prev = new_accusr_ptr;
			    }
			    
			} /* end of if (acc_user_ptr == NULL) */
		    } /* end of if (malloc (OP_ACCOUNT_USER)) */
		} /* end of if (malloc (OP_ASSIGN_USERS)) */

	    }/* end of if ( assign_ptr->selected ... */               

	    assign_ptr = assign_ptr->next ;

	} /* while (assign_ptr is not NULL) */

	if (cnt <= 0)
	{
	    msg_dlgCb (msg_dlg, IMS_ERROR, 
		       "No user has been selected!");
	    return;
	}
	else
	{
	    /*
	     * update the position and reset the selection status
	     * for each entry in the assigned_users list.
	     */
	    assigned_ptr = 
		glbData.accounts_users_data.user_data.op_assigned_users_ptr;
	    cnt = 0;
	    while (assigned_ptr != (OP_ASSIGN_USERS *)NULL)
	    {
		assigned_ptr->position = cnt;
		cnt++;
		assigned_ptr->selected = 0;
		assigned_ptr = assigned_ptr->next;
	    }
	    glbData.accounts_users_data.user_data.assigned_users_cnt = cnt;
	}

	/* display new assigned_users list */
	cbs = ( XmScrollBarCallbackStruct * )
	    XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
	cbs->value = 0 ; 
	assign_users_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
	
	XtFree ((char *)cbs) ;

    }

    UxAssign_usersContext = UxSaveCtx;
	
} /* end of assign_users_add_usersCb */


/*===========================================================================*
**
** Function Name:   assign_users_delete_usersCb
**
** Description:     Callback function for the DELETE widget
**
** Arguments:       1. widget - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
**     12-04-95  Alin Tilden  Modified to maintain the op_assigned_users list
**                            internally instead of writing the modifications
**                            directly to the catalig.
**
**==========================================================================*/
 
void	assign_users_delete_usersCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCassign_users          *UxSaveCtx, *UxContext;
    Widget                     UxWidget = wgt;
    XtPointer                  UxClientData = cd;
    XtPointer                  UxCallbackArg = cb;
    OP_ASSIGN_USERS           *assigned_ptr, *temp ;
    OP_ACCOUNT_USER           *acc_user_ptr, *tmp_accusr_ptr;
    int                        cnt;
    XmScrollBarCallbackStruct *cbs ;

    UxSaveCtx = UxAssign_usersContext;
    UxAssign_usersContext = UxContext =
	(_UxCassign_users *) UxGetContext( UxWidget );

    {
	assigned_ptr = 
	    glbData.accounts_users_data.user_data.op_assigned_users_ptr;
	acc_user_ptr = 
	    glbData.accounts_users_data.user_data.op_account_user_ptr;
    
	cnt = 0;
	while (assigned_ptr != (OP_ASSIGN_USERS *)NULL)
	{
	    temp = (OP_ASSIGN_USERS *)NULL;

	    if (assigned_ptr->selected)
	    {
		cnt++;

		/*
		 * update the status for this user_id in 
		 * the account_user list
		 */
		acc_user_ptr = assigned_ptr->account_user_ptr;

		switch (acc_user_ptr->account_user_status)
		{
		case UNCHANGED :
		case MODIFIED :
		    acc_user_ptr->account_user_status = DELETED;
		    break;

		case DELETED :
		    break;

		case NEW :
		    /* delete this account_user entry */
		    if (acc_user_ptr->next != (OP_ACCOUNT_USER *)NULL)
		    {
			acc_user_ptr->next->prev = acc_user_ptr->prev;
		    }
		    if (acc_user_ptr->prev != (OP_ACCOUNT_USER *)NULL)
		    {
			acc_user_ptr->prev->next = acc_user_ptr->next;
		    }
		    else
		    {
			glbData.accounts_users_data.user_data.
			    op_account_user_ptr = acc_user_ptr->next;
		    }
		    free (acc_user_ptr);
		    break;
		    
		default:
		    break;
		}

		/* delete this user from the assigned_users list */
		if (assigned_ptr->next != (OP_ASSIGN_USERS *)NULL)
		{
		    assigned_ptr->next->prev = assigned_ptr->prev;
		}
		if (assigned_ptr->prev != (OP_ASSIGN_USERS *)NULL)
		{
		    assigned_ptr->prev->next = assigned_ptr->next;
		}
		else
		{
		    glbData.accounts_users_data.user_data.
			op_assigned_users_ptr = assigned_ptr->next;
		}
		temp = assigned_ptr;

	    } /* end of if (assigned_ptr->selected) */

	    assigned_ptr = assigned_ptr->next;
	    if (temp)
		free (temp);

	} /* end of while (assigned_ptr) */

	if (cnt <= 0)
	{
	    msg_dlgCb (msg_dlg, IMS_ERROR,
		       "No user has been selected!");
	    return ;            
	}
	else
	{
	    /*
	     * update the position and reset the selection status
	     * for each entry in the assigned_users list
	     */
	    assigned_ptr = 
		glbData.accounts_users_data.user_data.op_assigned_users_ptr;
	    cnt = 0;
	    while (assigned_ptr != (OP_ASSIGN_USERS *)NULL)
	    {
		assigned_ptr->position = cnt;
		cnt++;
		assigned_ptr->selected = 0;
		assigned_ptr = assigned_ptr->next;
	    }
	    glbData.accounts_users_data.user_data.assigned_users_cnt = cnt;
	}

	/* display new assigned_users list */
	cbs = ( XmScrollBarCallbackStruct * )
	    XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
	cbs->value = 0 ; 
	assign_users_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
    
	XtFree ((char *)cbs) ;
    }

    UxAssign_usersContext = UxSaveCtx;

} /* end of assign_users_delete_usersCb */


/*===========================================================================*
**
** Function Name:   assign_users_updateCb
**
** Description:     Callback function for the UPDATE widget
**                  This function updates the catalog with the modifications
**                  made on the assign users screen.
**
** Arguments:       1. widget   - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
** 		04/23/96  		J. Ting   Assigned catReq->item[0] to count for catReq
** 													  OP_ACC_GET_ASSIGN_USERS
**
**==========================================================================*/

void	assign_users_updateCb(
				 Widget wgt, 
				 XtPointer cd, 
				 XtPointer cb)
{
  _UxCassign_users          *UxSaveCtx, *UxContext;
  Widget                     UxWidget = wgt;
  XtPointer                  UxClientData = cd;
  XtPointer                  UxCallbackArg = cb;
  OP_ACCOUNT_USER           *acc_user_ptr, *new_accusr_ptr;
  OP_ACCOUNT_USER           *delete_ptr, *delete_start;
  OP_ACCOUNT_USER           *add_ptr, *add_start;
  OP_CAT_STRUCT             *catReq;
  char                      *acc_id ;
  char                       buffer[IMS_COL255_LEN*2+1];
  XmScrollBarCallbackStruct *cbs ;
	int count = 0;

  UxSaveCtx = UxAssign_usersContext;
  UxAssign_usersContext = UxContext =
    (_UxCassign_users *) UxGetContext( UxWidget );
  {
    acc_user_ptr = 
      glbData.accounts_users_data.user_data.op_account_user_ptr;
    delete_ptr = delete_start = (OP_ACCOUNT_USER *)NULL;
    add_ptr = add_start = (OP_ACCOUNT_USER *)NULL;

    while (acc_user_ptr != (OP_ACCOUNT_USER *)NULL)
      {
	switch (acc_user_ptr->account_user_status)
	{
	case UNCHANGED :
	    break;

	case MODIFIED : 
	    /*
	     * This case will not happen since existing
	     * account_user data can not be modified at this time 
	     */
	    break;

	case DELETED :
	  /* add acc_user_ptr to delete list */
	  if ((new_accusr_ptr = 
	       (OP_ACCOUNT_USER *)malloc(sizeof(OP_ACCOUNT_USER)))
	      == (OP_ACCOUNT_USER *)NULL)
	  {
	    msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	    return;
	  }
	  else
	  {
	    new_accusr_ptr->prev = (OP_ACCOUNT_USER *)NULL;
	    new_accusr_ptr->next = (OP_ACCOUNT_USER *)NULL;
	    strcpy (new_accusr_ptr->user_id, acc_user_ptr->user_id);
	    new_accusr_ptr->account_user_status = 
	      acc_user_ptr->account_user_status;
	    
	    if (delete_start == (OP_ACCOUNT_USER *)NULL)
	    {
	      delete_start = new_accusr_ptr;
	      delete_ptr = delete_start;
	    }
	    else
	    {
	      delete_ptr->next = new_accusr_ptr;
	      new_accusr_ptr->prev = delete_ptr;
	      delete_ptr = delete_ptr->next;
	    }
	  }
	  break;

	case NEW :
	  /* add acc_user_ptr to add list */
	  if ((new_accusr_ptr = 
	       (OP_ACCOUNT_USER *)malloc(sizeof(OP_ACCOUNT_USER)))
	      == (OP_ACCOUNT_USER *)NULL)
	  {
	    msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	    return;
	  }
	  else
	  {
	    new_accusr_ptr->prev = (OP_ACCOUNT_USER *)NULL;
	    new_accusr_ptr->next = (OP_ACCOUNT_USER *)NULL;
	    strcpy (new_accusr_ptr->user_id, acc_user_ptr->user_id);
	    new_accusr_ptr->account_user_status = 
	      acc_user_ptr->account_user_status;
	    
	    if (add_start == (OP_ACCOUNT_USER *)NULL)
	    {
	      add_start = new_accusr_ptr;
	      add_ptr = add_start;
	    }
	    else
	    {
	      add_ptr->next = new_accusr_ptr;
	      new_accusr_ptr->prev = add_ptr;
	      add_ptr = add_ptr->next;
	    }
	  }	  
	  break;

	default:
	  break;
	}
	    
	acc_user_ptr = acc_user_ptr->next;
      } /* end of while (acc_user_ptr) */

    /* prepare for database updating */
    acc_id = (char *)XmTextGetString (account_idTF) ;
    ims_truncStr(acc_id) ;

    /* 
     * if any account_user entries have been deleted,
     * execute the deletion
     */
    if (delete_start != (OP_ACCOUNT_USER *)NULL)
    {
      catReq = &(glbData.accounts_users_data.catReq);	 
      catReq->item[1] = (OP_ACCOUNT_USER *)delete_start;
      catReq->item[2] = (char *)acc_id;

      if (ims_op_accCat (catReq, OP_ACC_DELETE_ASSIGN_USERS) < IMS_OK) 
      {
	/* Display error messages */
	msg_dlgCb (msg_dlg, IMS_FATAL,
		   "Couldn't delete user(s) from account.");
	XtFree ( acc_id ) ;
	while (delete_start != (OP_ACCOUNT_USER *)NULL)
	{
	    delete_ptr = delete_start->next;
	    free (delete_start);
	    delete_start = delete_ptr;
	}
	return ; 
      }
    }
    while (delete_start != (OP_ACCOUNT_USER *)NULL)
    {
	delete_ptr = delete_start->next;
	free (delete_start);
	delete_start = delete_ptr;
    }

    /* 
     * if any new account_USER have been added to
     * this account, execute insert
     */
    if (add_start != (OP_ACCOUNT_USER *)NULL)
    {
      catReq = &(glbData.accounts_users_data.catReq);	 
      catReq->item[1] = (OP_ACCOUNT_USER *)add_start;
      catReq->item[2] = (char *)acc_id;

      if (ims_op_accCat (catReq, OP_ACC_ADD_ASSIGN_USERS) < IMS_OK) 
      {
	/* Display error messages */
	msg_dlgCb (msg_dlg, IMS_FATAL,
		   "Couldn't assign user(s) to account.");
	XtFree ( acc_id ) ;
	while (add_start != (OP_ACCOUNT_USER *)NULL)
	{
	    add_ptr = add_start->next;
	    free (add_start);
	    add_start = add_ptr;
	}
	return ; 
      }
    }
    while (add_start != (OP_ACCOUNT_USER *)NULL)
    {
	add_ptr = add_start->next;
	free (add_start);
	add_start = add_ptr;
    }

    /* free assigned_users list */
    free_assigned_users ();

    /* 
     * get the new assigned_users list
     * for this account id, from the catalog.
     */
    catReq = &(glbData.accounts_users_data.catReq);	 
    buffer[0] = '\0';
    sprintf (buffer,
	     "select t1.user_id, t1.first_name, t1.initial_name,"
	     "       t1.last_name "
	     "from user_profile t1, account_user t2 "
	     "where t1.user_id = t2.user_id "
	     "      and t2.account_id = '%s' "
	     "order by t1.user_id", 
	     acc_id );

		/*
		** 04/23/96 - assign catReq->item[0] to count
		*/
    catReq->item[0] = (int *) &count;
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_GET_ASSIGN_USERS) < IMS_OK) 
    {
      /* Display error messages */
      sprintf(buffer,
	      "Couldn't get users assigned to account");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      XtFree ( acc_id ) ;
      return ; 
    }

    /* get data */
    glbData.accounts_users_data.user_data.assigned_users_cnt = 
      *(int *)catReq->item[0];
    glbData.accounts_users_data.user_data.op_assigned_users_ptr = 
      (OP_ASSIGN_USERS *)catReq->item[2]; 
    glbData.accounts_users_data.user_data.op_account_user_ptr = 
      (OP_ACCOUNT_USER *)catReq->item[3];
    cbs = ( XmScrollBarCallbackStruct * )
      XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
    cbs->value = 0 ; 
    assign_users_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;

    XtFree ((char *)cbs) ;
    XtFree ( acc_id ) ;
  }

  UxAssign_usersContext = UxSaveCtx;

  (void) assign_users_closeCb (wgt, cd, cb);

} /* end of assign_users_updateCb */



void	template20 (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAssign_usersContext;
	UxAssign_usersContext = UxContext =
			(_UxCassign_users *) UxGetContext( UxWidget );
	{
	


	}
	UxAssign_usersContext = UxSaveCtx;
}

