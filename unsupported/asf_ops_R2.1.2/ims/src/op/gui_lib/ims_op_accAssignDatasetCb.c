static char *sccs = "@(#)ims_op_accAssignDatasetCb.c	5.3  07/23/96";
/*******************************************************************************
**
** File:		ims_op_accAssignDatasetsCb.c
**
** Function:		Callback functions for the assign_datasets screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Modified:
**    11-27-95  Alin Tilden  Modified functions assign_datasets_add_datasetsCb,
**                           assign_datasets_closeCb, free_assign_datasets,
**                           assign_datasets_delete_datasetsCb,
**                           assign_datasets_permissionsCb, and added function
**                           assign_datasets_updateCb in order to add an
**                           UPDATE option and to prevent executing all changes
**                           made on the screen to the catalog automatically.
**    12-07-95  Alin Tilden  Made modifications to assign_datasets_add_
**                           datasetsCb in order to correct the problem of
**                           loosing the selection.
**
**		04-23-96  J. Ting			 Modified function assign_datasets_updateCb. 
**                           Initialized catReq->item[0], see R1BP problem
**                           reports 754 and 755.
**        
**    06-12-96  J. Ting      Modified function assign_datasets_closeCb for
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

static int dataset_window_top = 0 ;
static int assigned_window_top = 0 ;
static int selected_dataset = 0 ;
static int selected_assigned = 0 ;
static void assign_datasets_permissionsCb(Widget , XtPointer , XtPointer ) ;
void assign_datasets_datasets_sbCb ( Widget , XtPointer , XtPointer ) ;
void assign_datasets_assigned_sbCb (Widget, XtPointer, XtPointer ) ;
void free_assign_datasets ( ) ;
void free_assigned_datasets ( ) ;

#define DATASETS_LIST_LENGTH 20

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAssignDataset.h"
#undef CONTEXT_MACRO_ACCESS 
    
void assign_datasets_datasets_listCb	(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XmListCallbackStruct *cbs = (XmListCallbackStruct *) cb;
        int i ;
        OP_ASSIGN_DATASETS *datasets_ptr, *temp ;

	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( UxWidget );
	{
      /* Since our list is application defined (virtual) we have  */
          /*to keep track of the selections.                        */
	  selected_dataset = 1 ;
          datasets_ptr =
              glbData.accounts_users_data.account_data.op_assign_datasets_ptr ;
          temp = datasets_ptr ;

          if (cbs->reason == XmCR_EXTENDED_SELECT ) {  
            if (cbs->selection_type == XmINITIAL) {
/* deselect the all list                                     */
              while (temp != (OP_ASSIGN_DATASETS *) NULL ) {
                temp->selected = 0 ;
                temp = temp->next ;
              }
              
            }
          }
/* get to the window top as set by the scroll bar */
          while ( (datasets_ptr != (OP_ASSIGN_DATASETS *) NULL ) &&
                    (datasets_ptr->position != dataset_window_top) )
            datasets_ptr = datasets_ptr->next ;
/* select the items selected for this part of the list   */
          temp = datasets_ptr ;
          while ( (temp != (OP_ASSIGN_DATASETS *) NULL ) && 
                    (temp->position < 
                      ( dataset_window_top+DATASETS_LIST_LENGTH) ) ) {
                temp->selected = 0 ;
                for ( i = 0 ; i < cbs->selected_item_count ; i++ )
                  if ( temp->position == dataset_window_top +
                                        cbs->selected_item_positions[i] - 1 ){
                    temp->selected = 1 ;
                    break ;
                  }
                temp = temp->next ;
          }


	}
	UxAssign_datasetsContext = UxSaveCtx;
}

     
void assign_datasets_datasets_sbCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCassign_datasets     *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    Widget sb ;
    int i,j ;
    int selected_datasets[DATASETS_LIST_LENGTH] ;
    static cnt=0 ;
    char  buffer[IMS_COL255_LEN+1] ;
    char  temp[IMS_COL30_LEN+1];
    static OP_ASSIGN_DATASETS  *dataset_ptr = (OP_ASSIGN_DATASETS  *)NULL ;

    XmString str ;
    XmStringTable datasets_str ;
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct * ) cb ;
    int slider ;
    static int view ;

    int create = (int) cd ;

    /* get the context of the parent of scroll bar */
    UxSaveCtx = UxAssign_datasetsContext;
    UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext(assign_datasets );

    {
	if ( create == 1 ) 
	{
            XtVaGetValues ( assign_datasets_sbSW , 
                    XmNverticalScrollBar, &sb,NULL ) ;
            XtAddCallback ( sb, 
                XmNvalueChangedCallback, assign_datasets_datasets_sbCb, NULL) ;
            XtAddCallback ( sb, 
                XmNincrementCallback, assign_datasets_datasets_sbCb, NULL ) ;
            XtAddCallback ( sb, 
             XmNdecrementCallback, assign_datasets_datasets_sbCb, NULL ) ;
            XtAddCallback ( sb, 
                XmNpageIncrementCallback, assign_datasets_datasets_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                XmNpageDecrementCallback, assign_datasets_datasets_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoTopCallback, assign_datasets_datasets_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoBottomCallback, assign_datasets_datasets_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNdragCallback, assign_datasets_datasets_sbCb,NULL ) ;
            
            return ;
         }
         if ( wgt == (Widget) NULL )  /* new list */
           selected_dataset = 0 ;
         dataset_window_top = cbs->value ;
         dataset_ptr =
           glbData.accounts_users_data.account_data.op_assign_datasets_ptr ;
         /* initialize this only when: */
         if ( cbs->value == 0 ) 
	 {
           cnt = glbData.accounts_users_data.account_data.assign_datasets_cnt ;
           if ( cnt > DATASETS_LIST_LENGTH )
            view = DATASETS_LIST_LENGTH ;
           else
            view = cnt ;
         }
         /* Delete all items from  list */
         XmListDeleteAllItems ( assign_datasetsSL  ) ;

	 /* Update lists */
         datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;

         /* index into users list until index begins at window top */
         while ( (dataset_ptr != (OP_ASSIGN_DATASETS *)NULL) 
		 && (dataset_ptr->position != dataset_window_top) )
	 {
           dataset_ptr = dataset_ptr->next ;
         }
         for (i = 0 ; i < view && dataset_ptr 
                        != (OP_ASSIGN_DATASETS *)NULL ; i ++ )
	 {
           temp[0] = '\0' ;
	   ims_truncStr(dataset_ptr->dataset) ;
           ims_truncStr(dataset_ptr->platform) ;
           ims_truncStr(dataset_ptr->sensor) ;
           /*sprintf (buffer,
                  "%-80s, %-30s, %-30s\0",
                      dataset_ptr->dataset, 
                      dataset_ptr->platform,
                      dataset_ptr->sensor) ;*/
           sprintf (buffer,
                  "%s, %s, %s\0",
                      dataset_ptr->dataset, 
                      dataset_ptr->platform,
                      dataset_ptr->sensor) ;
           selected_datasets[i] = 0 ;
           if ( dataset_ptr->selected ) /* set by datasets_listCb */
             selected_datasets[i] = 1 ;
           datasets_str[i] = XmStringCreateLocalized ( buffer ) ;
           dataset_ptr = dataset_ptr->next ;
         }  

	 /* Add items to list unselected */           
         XmListAddItemsUnselected ( assign_datasetsSL,datasets_str,view,1 ) ;

	 /* Set selection policy to multiple select */
         XtVaSetValues ( assign_datasetsSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         for ( i = 0 ; i < view ; i++ ) 
	 {
	     /* select list item previously selected */
	     if (selected_datasets[i])
		 XmListSelectPos (assign_datasetsSL, i+1, False ) ;
	     XmStringFree ( datasets_str[i] ) ;
         }
         XmStringFree ( (XmString)datasets_str ) ;

	 /* Change back to extended select */
         XtVaSetValues ( assign_datasetsSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );

         /* set slider size and list value */
         if ( cnt > DATASETS_LIST_LENGTH )
           slider = DATASETS_LIST_LENGTH ;
         else 
           slider = cnt ;
         XtVaGetValues ( assign_datasets_sbSW, XmNverticalScrollBar,&sb,NULL ) ;
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
        UxAssign_datasetsContext = UxSaveCtx;

}

void assign_datasets_assigned_listCb	(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XmListCallbackStruct *cbs = (XmListCallbackStruct *) cb;
        int i ;
        OP_ASSIGN_DATASETS *datasets_ptr, *temp ;

	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( UxWidget );
	{
      /* Since our list is application defined (virtual) we have  */
          /*to keep track of the selections.                        */
	  selected_assigned = 1 ;
          datasets_ptr =
           glbData.accounts_users_data.account_data.op_assigned_datasets_ptr ;
          temp = datasets_ptr ;

          if (cbs->reason == XmCR_EXTENDED_SELECT ) {  
            if (cbs->selection_type == XmINITIAL) {
/* deselect the all list                          */
              while (temp != (OP_ASSIGN_DATASETS *) NULL ) {
                  temp->selected = 0 ;
                  temp = temp->next ;
              }
              if ( (int)cd != 0 ) {
                assign_datasets_permissionsCb (wgt, cd, cb ) ;  
              }           
            }
          }
/* get to the window top as set by the scroll bar */
          while ( (datasets_ptr != (OP_ASSIGN_DATASETS *) NULL ) &&
                    (datasets_ptr->position != assigned_window_top) )
            datasets_ptr = datasets_ptr->next ;
/* select the items selected for this part of the list   */
          temp = datasets_ptr ;
          while ( (temp != (OP_ASSIGN_DATASETS *) NULL ) && 
                    (temp->position < 
                      ( assigned_window_top+DATASETS_LIST_LENGTH) ) ) {
                temp->selected = 0 ;
                for ( i = 0 ; i < cbs->selected_item_count ; i++ )
                  if ( temp->position == assigned_window_top +
                                        cbs->selected_item_positions[i] - 1 ){
                    temp->selected = 1 ;
                    break ;
                  }

               temp = temp->next ;
          }
/* select the other lists. set first them to multiple_select  */
	 XmListDeselectAllItems (assigned_datasetsSL ) ;
	 XmListDeselectAllItems (orderSL ) ;
	 XmListDeselectAllItems (addSL ) ;
	 XmListDeselectAllItems (getSL ) ;
	 XmListDeselectAllItems (deleteSL ) ;
	 XmListDeselectAllItems (replaceSL ) ;

         XtVaSetValues ( assigned_datasetsSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( orderSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( addSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( getSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( deleteSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( replaceSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );

         for ( i = 0 ; i < cbs->selected_item_count ; i++ ) {
                  XmListSelectPos (assigned_datasetsSL,
                                     cbs->selected_item_positions[i], False ) ;
                  XmListSelectPos ( orderSL, cbs->selected_item_positions[i], 
                                                                      False ) ;
                  XmListSelectPos ( addSL, cbs->selected_item_positions[i], 
                                                                      False ) ;
                  XmListSelectPos ( getSL, cbs->selected_item_positions[i], 
                                                                      False ) ;
                  XmListSelectPos ( deleteSL, cbs->selected_item_positions[i], 
                                                                      False ) ;
                  XmListSelectPos ( replaceSL, cbs->selected_item_positions[i], 
                                                                      False ) ;
          }
/* Change back to extended select */
         XtVaSetValues ( assigned_datasetsSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( orderSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( addSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( getSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( deleteSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( replaceSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );

	}
	UxAssign_datasetsContext = UxSaveCtx;
}


void assign_datasets_assigned_sbCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
        Widget sb ;
        int i,j ;
        int selected_datasets[DATASETS_LIST_LENGTH] ;
        static cnt=0 ;
        char  buffer[IMS_COL255_LEN+1] ;
	char  temp[IMS_COL30_LEN+1];
        static OP_ASSIGN_DATASETS  *dataset_ptr = (OP_ASSIGN_DATASETS  *)NULL ;

        XmString str ;
        XmStringTable datasets_str,order,add,get,delete,replace ;
        XmScrollBarCallbackStruct *cbs =
         (XmScrollBarCallbackStruct * ) cb ;
        int slider ;
        static int view ;


        int create = (int) cd ;

        /* get the context of the parent of scroll bar */
	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( assign_datasets );
	{
         if ( create == 1 ) {
            
            XtVaGetValues ( guide_sbSW , 
                    XmNverticalScrollBar, &sb,NULL ) ;
            XtAddCallback ( sb, 
                XmNvalueChangedCallback, assign_datasets_assigned_sbCb, NULL) ;
            XtAddCallback ( sb, 
                XmNincrementCallback, assign_datasets_assigned_sbCb, NULL ) ;
            XtAddCallback ( sb, 
             XmNdecrementCallback, assign_datasets_assigned_sbCb, NULL ) ;
            XtAddCallback ( sb, 
                XmNpageIncrementCallback, assign_datasets_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                XmNpageDecrementCallback, assign_datasets_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoTopCallback, assign_datasets_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNtoBottomCallback, assign_datasets_assigned_sbCb,NULL ) ;
            XtAddCallback ( sb, 
                   XmNdragCallback, assign_datasets_assigned_sbCb,NULL ) ;
            
            return ;

         }
         if ( wgt == (Widget) NULL )  /* new list */
           selected_assigned = 0 ;
         assigned_window_top = cbs->value ;
         dataset_ptr =
           glbData.accounts_users_data.account_data.op_assigned_datasets_ptr ;
         /* initialize this only when: */
         if ( cbs->value == 0 ) {
           cnt =glbData.accounts_users_data.account_data.assigned_datasets_cnt ;
           if ( cnt > DATASETS_LIST_LENGTH )
            view = DATASETS_LIST_LENGTH ;
           else
            view = cnt ;
         }
         /*Delete all items from  list */
         XmListDeleteAllItems ( assigned_datasetsSL  ) ;
         XmListDeleteAllItems ( orderSL  ) ;
         XmListDeleteAllItems ( addSL  ) ;
         XmListDeleteAllItems ( getSL  ) ;
         XmListDeleteAllItems ( deleteSL  ) ;
         XmListDeleteAllItems ( replaceSL  ) ;
      /* Update lists */
             
         datasets_str = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         order = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         add = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         get = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         delete = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         replace = (XmStringTable)XtMalloc(view*sizeof(XmString*)) ;
         /* index into users list until index begins at window top */
         while ( (dataset_ptr != NULL) && (dataset_ptr->position !=
                                                     assigned_window_top) ){
           dataset_ptr = dataset_ptr->next ;
         }
         for (i = 0 ; i < view && dataset_ptr 
                        != (OP_ASSIGN_DATASETS *)NULL ; i ++ ){
           temp[0] = '\0' ;
           sprintf (buffer,
                  "%s, %s, %s\0",
                      dataset_ptr->dataset, 
                      dataset_ptr->platform,
                      dataset_ptr->sensor) ; 
           selected_datasets[i] = 0 ;
           if ( dataset_ptr->selected ) /* set by datasets_listCb */
             selected_datasets[i] = 1 ;
           datasets_str[i] = XmStringCreateLocalized ( buffer ) ;
	   if ( 16 & dataset_ptr->account_dataset_ptr->permissions )
	     order [i] = XmStringCreateLocalized ( "Y" ) ;
 	   else
	     order [i] = XmStringCreateLocalized ( "N" ) ;
	   if ( 8 & dataset_ptr->account_dataset_ptr->permissions )
	     add [i] = XmStringCreateLocalized ( "Y" ) ;
 	   else
	     add [i] = XmStringCreateLocalized ( "N" ) ;
	   if ( 4 & dataset_ptr->account_dataset_ptr->permissions )
	     get[i] = XmStringCreateLocalized ( "Y" ) ;
 	   else
	     get [i] = XmStringCreateLocalized ( "N" ) ;
	   if ( 2 & dataset_ptr->account_dataset_ptr->permissions )
	     delete [i] = XmStringCreateLocalized ( "Y" ) ;
 	   else
	     delete [i] = XmStringCreateLocalized ( "N" ) ;
	   if ( 1 & dataset_ptr->account_dataset_ptr->permissions )
	     replace [i] = XmStringCreateLocalized ( "Y" ) ;
 	   else
	     replace [i] = XmStringCreateLocalized ( "N" ) ;

           dataset_ptr = dataset_ptr->next ;
         }  
/* Add items to list unselected */           
         XmListAddItemsUnselected ( assigned_datasetsSL,datasets_str,view,1 ) ;
         XmListAddItemsUnselected ( orderSL,order,view,1 ) ;
         XmListAddItemsUnselected ( addSL, add,view,1 ) ;
         XmListAddItemsUnselected ( getSL, get,view,1 ) ;
         XmListAddItemsUnselected ( deleteSL, delete,view,1 ) ;
         XmListAddItemsUnselected ( replaceSL, replace,view,1 ) ;
/* Set selection policy to multiple select */
         XtVaSetValues ( assigned_datasetsSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( orderSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( addSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( getSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( deleteSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         XtVaSetValues ( replaceSL,
             XmNselectionPolicy,XmMULTIPLE_SELECT,NULL );
         for ( i = 0 ; i < view ; i++ ) {
           /* select list item previously selected */
           if (selected_datasets[i]){
             XmListSelectPos (assigned_datasetsSL, i+1, False ) ;
             XmListSelectPos (orderSL, i+1, False ) ;
             XmListSelectPos (addSL, i+1, False ) ;
             XmListSelectPos (getSL, i+1, False ) ;
             XmListSelectPos (deleteSL, i+1, False ) ;
             XmListSelectPos (replaceSL, i+1, False ) ;
           }
           XmStringFree ( datasets_str[i] ) ;
           XmStringFree ( order[i] ) ;
           XmStringFree ( add[i] ) ;
           XmStringFree ( get[i] ) ;
           XmStringFree ( delete[i] ) ;
           XmStringFree ( replace[i] ) ;
         }
         XmStringFree ( (XmString)datasets_str ) ;
	 XmStringFree ( (XmString)order );
	 XmStringFree ( (XmString)add );
	 XmStringFree ( (XmString)get );
	 XmStringFree ( (XmString)delete );
	 XmStringFree ( (XmString)replace );

/* Change back to extended select */
         XtVaSetValues ( assigned_datasetsSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( orderSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( addSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( getSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( deleteSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         XtVaSetValues ( replaceSL,
              XmNselectionPolicy,XmEXTENDED_SELECT,NULL );
         /* set slider size and list value */
         
         if ( cnt > DATASETS_LIST_LENGTH )
           slider = DATASETS_LIST_LENGTH ;
         else 
           slider = cnt ;
         XtVaGetValues(guide_sbSW, XmNverticalScrollBar,&sb,NULL ) ;
          /* XtVaGetValues ( sb, XmNmaximum, XmNsliderSize, &slider, NULL ) ;*/
         if ( cnt > 0 ) {
           XtVaSetValues ( sb, XmNmaximum, cnt, XmNvalue,
                    cbs->value, XmNsliderSize, slider, NULL ) ;
         }
         else
           XtVaSetValues ( sb, XmNmaximum, 1, XmNvalue,
                    cbs->value, XmNsliderSize, 1, NULL ) ;	  
        }
	UxAssign_datasetsContext = UxSaveCtx;

}


/*===========================================================================*
**
** Function Name:   assign_datasets_add_datasetsCb
**
** Description:     Callback function for the ASSIGN widget
**
** Arguments:       1. widget - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
**     11-24-95  Alin Tilden  Modified to maintain op_account_dataset list
**                            in addition to op_assigned_datasets.
**
**==========================================================================*/

void	assign_datasets_add_datasetsCb (
					Widget wgt, 
					XtPointer cd, 
					XtPointer cb)
{
    _UxCassign_datasets     *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    XtPointer               UxClientData = cd;
    XtPointer               UxCallbackArg = cb;
    OP_ASSIGN_DATASETS  *assign_ptr;
    OP_ASSIGN_DATASETS  *assigned_ptr, *temp, *temp1, *new_asd_ptr;
    OP_ACCOUNT_DATASET  *acc_dataset_ptr, *tmp_accdset_ptr, *new_accdset_ptr;
    int cnt;
    int match;
    XmScrollBarCallbackStruct *cbs;

    UxSaveCtx = UxAssign_datasetsContext;
    UxAssign_datasetsContext = UxContext =
		(_UxCassign_datasets *) UxGetContext( UxWidget );
    {
      assign_ptr = 
	glbData.accounts_users_data.account_data.op_assign_datasets_ptr ;
      assigned_ptr = 
        glbData.accounts_users_data.account_data.op_assigned_datasets_ptr ;
      acc_dataset_ptr =
	glbData.accounts_users_data.account_data.op_account_dataset_ptr ;

      /* check all datasets since we have the extended select option */
      cnt = 0 ;                          /* number of selected users */
      while (assign_ptr != (OP_ASSIGN_DATASETS *) NULL ) 
      {
	match = 0;
	assigned_ptr = 
	  glbData.accounts_users_data.account_data.op_assigned_datasets_ptr;

	if ( assign_ptr->selected ) 
	{
	  /* check that dataset is not already in the assigned datasets list */
	  cnt++;
	  temp = assigned_ptr ;
          while (( temp != (OP_ASSIGN_DATASETS *) NULL ))
	  {
	    /* dataset_idx is not unique, compare all fields */
            if (( assign_ptr->dataset_idx == temp->dataset_idx ) 
		&& ( strcmp (assign_ptr->dataset, temp->dataset) == 0 )
		&& ( strcmp (assign_ptr->sensor, temp->sensor) == 0 )
		&& ( strcmp (assign_ptr->platform, temp->platform) == 0 ))
	    {
		match = 1;
		break;
            }
	    temp = temp->next ;
	  }
	  /* if a match is found, skip this dataset */
	  if (match == 1)
	  {
	      assign_ptr = assign_ptr->next ;
	      continue;
	  }
	     
	  /* add dataset to assigned datasets linked list */
	  if ((new_asd_ptr = 
	       (OP_ASSIGN_DATASETS *)malloc(sizeof(OP_ASSIGN_DATASETS))) 
	      == (OP_ASSIGN_DATASETS *)NULL)
	  { 
	      msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	      return;
	  }
	  else
	  {
	      new_asd_ptr->prev = (OP_ASSIGN_DATASETS *)NULL;
	      new_asd_ptr->next = (OP_ASSIGN_DATASETS *)NULL;
	      new_asd_ptr->account_dataset_ptr = (OP_ACCOUNT_DATASET *)NULL;
	      new_asd_ptr->dataset_idx = assign_ptr->dataset_idx;
	      new_asd_ptr->selected = 0;
	      new_asd_ptr->position = 0;
	      strcpy (new_asd_ptr->dataset, assign_ptr->dataset);
	      strcpy (new_asd_ptr->sensor, assign_ptr->sensor);
	      strcpy (new_asd_ptr->platform, assign_ptr->platform);

	      if (assigned_ptr == (OP_ASSIGN_DATASETS *)NULL)
	      {
		  glbData.accounts_users_data.account_data.
		      op_assigned_datasets_ptr = new_asd_ptr;
	      }
	      else
	      {
		  /* find the right place in the list to add the new entry */
		  while ((assigned_ptr->next != (OP_ASSIGN_DATASETS *)NULL)
			 && 
			 (new_asd_ptr->dataset_idx > assigned_ptr->dataset_idx))
		  {
		      assigned_ptr = assigned_ptr->next;
		  }

		  if (new_asd_ptr->dataset_idx < assigned_ptr->dataset_idx)
		  {
		      /* insert before */
		      temp = assigned_ptr->prev;
		      assigned_ptr->prev = new_asd_ptr;
		      new_asd_ptr->prev = temp;
		      new_asd_ptr->next = assigned_ptr;

		      if (temp != (OP_ASSIGN_DATASETS *)NULL)
			  temp->next = new_asd_ptr;
		      else
			  glbData.accounts_users_data.account_data.
			      op_assigned_datasets_ptr = new_asd_ptr;
		  }
		  else
		  {
		      /* insert after */
		      temp = assigned_ptr->next;
		      assigned_ptr->next = new_asd_ptr;
		      new_asd_ptr->prev = assigned_ptr;
		      new_asd_ptr->next = temp;

		      if (temp != (OP_ASSIGN_DATASETS *)NULL)
			  temp->prev = new_asd_ptr;
		  }
	      } /* end of if (assigned_ptr != NULL) */

	      /* 
	       * if dataset is not already in account_dataset list, add it.
	       * Note: dataset_idx is unique in the account_dataset list.
	       */
	      acc_dataset_ptr =
		  glbData.accounts_users_data.account_data.op_account_dataset_ptr;
	      if (acc_dataset_ptr != (OP_ACCOUNT_DATASET *) NULL)
	      {
		  while ((acc_dataset_ptr->next != (OP_ACCOUNT_DATASET *) NULL)
			 && 
			 (acc_dataset_ptr->dataset_idx 
			  < new_asd_ptr->dataset_idx))
		  {
		      acc_dataset_ptr = acc_dataset_ptr->next;
		  }

		  if (new_asd_ptr->dataset_idx == acc_dataset_ptr->dataset_idx)
		  {
		      /* update existing account_dataset entry */
		      if (acc_dataset_ptr->account_dataset_status == DELETED)
		      {
			  acc_dataset_ptr->account_dataset_status = MODIFIED;
			  /* permissions defaults = 20. OAGDR : YNYNN */
			  acc_dataset_ptr->permissions = 20;
		      }
		      acc_dataset_ptr->dataset_count++;
		      new_asd_ptr->account_dataset_ptr = acc_dataset_ptr;
		      assign_ptr = assign_ptr->next ;
		      continue;
		  }
	      }

	      /* add a new account_dataset entry */
	      if ((new_accdset_ptr = 
		   (OP_ACCOUNT_DATASET *)malloc(sizeof(OP_ACCOUNT_DATASET)))
		  == (OP_ACCOUNT_DATASET *)NULL)
	      {
		  msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
		  return;
	      }
	      else
	      {
		  new_accdset_ptr->prev = (OP_ACCOUNT_DATASET *)NULL;
		  new_accdset_ptr->next = (OP_ACCOUNT_DATASET *)NULL;
		  new_accdset_ptr->dataset_idx = new_asd_ptr->dataset_idx;
		  new_accdset_ptr->permissions = 20; /* default: YNYNN */
		  new_accdset_ptr->account_dataset_status = NEW;
		  new_accdset_ptr->dataset_count = 1;
		  new_asd_ptr->account_dataset_ptr = new_accdset_ptr;
		  
		  if (acc_dataset_ptr == (OP_ACCOUNT_DATASET *) NULL)
		  {
		      glbData.accounts_users_data.account_data.
			  op_account_dataset_ptr = new_accdset_ptr;
		  }
		  else
		  {
		      if (new_accdset_ptr->dataset_idx 
			  < acc_dataset_ptr->dataset_idx)
		      {
			  /* insert before */
			  tmp_accdset_ptr = acc_dataset_ptr->prev;
			  acc_dataset_ptr->prev = new_accdset_ptr;
			  new_accdset_ptr->prev = tmp_accdset_ptr;
			  new_accdset_ptr->next = acc_dataset_ptr;
		      
			  if (tmp_accdset_ptr != (OP_ACCOUNT_DATASET *)NULL)
			      tmp_accdset_ptr->next = new_accdset_ptr;
			  else
			      glbData.accounts_users_data.account_data.
				  op_account_dataset_ptr = new_accdset_ptr;
		      }
		      else
		      {
			  /* insert after */
			  tmp_accdset_ptr = acc_dataset_ptr->next;
			  acc_dataset_ptr->next = new_accdset_ptr;
			  new_accdset_ptr->prev = acc_dataset_ptr;
			  new_accdset_ptr->next = tmp_accdset_ptr;
		      
			  if (tmp_accdset_ptr != (OP_ACCOUNT_DATASET *)NULL)
			      tmp_accdset_ptr->prev = new_accdset_ptr;
		      }
		  } /* end of if (acc_dataset_ptr == NULL) */
	      } /* end of if (malloc(OP_ACCOUNT_DATASET)) */  
	    
	  } /* end of if (malloc(OP_ASSIGN_DATASETS)) */
        }/* end of if ( assign_ptr->selected ... */               

	assign_ptr = assign_ptr->next ;

      } /* while (assign_ptr is not NULL) */

      if (cnt <= 0)
      {
	msg_dlgCb ( msg_dlg, IMS_ERROR, 
		   "No dataset has been selected!");
        return ;            
      }
      else
      {
	/*
	 * update the position and reset the selection status
	 * for each entry in the assigned_datasets list
	 */
	assigned_ptr = 
	  glbData.accounts_users_data.account_data.op_assigned_datasets_ptr;
	cnt = 0;
	while (assigned_ptr != (OP_ASSIGN_DATASETS *) NULL )
	{
	  assigned_ptr->position = cnt;
	  cnt++;
	  assigned_ptr->selected = 0;
	  assigned_ptr = assigned_ptr->next;
	}
	glbData.accounts_users_data.account_data.assigned_datasets_cnt = cnt;
      }

      /* display the new assigned_datasets list */
      cbs = ( XmScrollBarCallbackStruct * )
	    XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
      cbs->value = 0 ; 
      assign_datasets_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
      XtFree ((char *)cbs) ;
   }
   UxAssign_datasetsContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name:   assign_datasets_closeCb
**
** Description:     Callback function for the CLOSE widget
**                  This function pops down the screen without
**                  making any changes to the catalog.
**
** Arguments:       1. widget - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
**     11-24-96  Alin Tilden  Modified to maintain op_account_dataset list
**                            in addition to op_assigned_datasets.
**
**     06/12/96  J. Ting      Modified to use glbData.assign_datasetsW to get
**                            the context instead of using UxWidget. This
**                            is done for mwm quit button to work - PR 942.
**
**==========================================================================*/

void	assign_datasets_closeCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( glbData.assign_datasetsW );
	{
	  free_assign_datasets ( ) ;
	  free_assigned_datasets ( ) ;

	  /* Delete all items from  list */
	  XmListDeleteAllItems ( assigned_datasetsSL  ) ;
	  XmListDeleteAllItems ( orderSL  ) ;
	  XmListDeleteAllItems ( addSL  ) ;
	  XmListDeleteAllItems ( getSL  ) ;
	  XmListDeleteAllItems ( deleteSL  ) ;
	  XmListDeleteAllItems ( replaceSL  ) ;

          glbData.assign_datasetsFlag = 0 ; /* screen has been pop down */
	  XtPopdown ( XtParent (glbData.assign_datasetsW) );
          /* clean up */
	}
	UxAssign_datasetsContext = UxSaveCtx;
}


void	assign_datasets_init_dataCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	OP_ACCOUNT *acc_ptr = (OP_ACCOUNT *) cd;
	char buffer[100] ;
        XmScrollBarCallbackStruct *cbs ;

	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( UxWidget );
	{
/* set the text fields with the account data                            */
	  XmTextSetString(account_idTF ,acc_ptr->account_id ) ;
	  XmTextSetString(creationTF ,acc_ptr->create_time ) ;
	  XmTextSetString(expirationTF ,acc_ptr->expire_time ) ;
          buffer [0] = '\0' ;
          sprintf(buffer, "%f", (float)acc_ptr->curr_balance ) ;
	  XmTextSetString(current_balanceTF , buffer ) ;
/*  set the lists data                                                   */
          cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
          cbs->value = 0 ; 

          assign_datasets_datasets_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
          assign_datasets_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;

          XtFree ((char *)cbs) ;
	  
	}
	UxAssign_datasetsContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name:   free_assign_datasets
**
** Description:     Function for freeing the linked list:
**                  op_assign_datasets.
**
** Arguments:       None
**
** Return Value:    None
**
** Revision History:
**     11-27-95  Alin Tilden  Modified to free the new op_assign_datasets list.
**
**==========================================================================*/
 
void free_assign_datasets ( ) {
  
  OP_ASSIGN_DATASETS *curr_ptr, *last_ptr ;

  if ( glbData.accounts_users_data.account_data.assign_datasets_cnt > 0 ) 
  {
    glbData.accounts_users_data.account_data.assign_datasets_cnt = 0 ;
    curr_ptr = glbData.accounts_users_data.account_data.op_assign_datasets_ptr ;
    while ( curr_ptr != (OP_ASSIGN_DATASETS *) NULL ) 
    {
       last_ptr = curr_ptr->next ;
       free (curr_ptr) ;
       curr_ptr = last_ptr ;
    }
    glbData.accounts_users_data.account_data.op_assign_datasets_ptr =
      (OP_ASSIGN_DATASETS *)NULL;
  }/* end if */

}/* end free_assign_datasets */


/*===========================================================================*
**
** Function Name:   free_assigned_datasets
**
** Description:     Function for freeing the linked lists:
**                  op_assigned_datasets, and op_account_dataset.
**
** Arguments:       None
**
** Return Value:    None
**
**==========================================================================*/
 
void free_assigned_datasets ( ) {
  
  OP_ASSIGN_DATASETS *curr_ptr, *last_ptr ;
  OP_ACCOUNT_DATASET *curr_accdset_ptr, *last_accdset_ptr;

  if ( glbData.accounts_users_data.account_data.assigned_datasets_cnt > 0 ) 
  {
    glbData.accounts_users_data.account_data.assigned_datasets_cnt = 0;
    curr_ptr =
      glbData.accounts_users_data.account_data.op_assigned_datasets_ptr;
    while ( curr_ptr != (OP_ASSIGN_DATASETS *)NULL ) 
    {
       last_ptr = curr_ptr->next ;
       free (curr_ptr) ;
       curr_ptr = last_ptr ;
    }
    glbData.accounts_users_data.account_data.op_assigned_datasets_ptr =
      (OP_ASSIGN_DATASETS *)NULL;
  }/* end if */

  curr_accdset_ptr = 
      glbData.accounts_users_data.account_data.op_account_dataset_ptr;
  while (curr_accdset_ptr != (OP_ACCOUNT_DATASET *)NULL)
  {
      last_accdset_ptr = curr_accdset_ptr->next;
      free (curr_accdset_ptr);
      curr_accdset_ptr = last_accdset_ptr;
  }
  glbData.accounts_users_data.account_data.op_account_dataset_ptr =
      (OP_ACCOUNT_DATASET *)NULL;

}/* end free_assigned_datasets */


/*===========================================================================*
**
** Function Name:   assign_datasets_delete_datasetsCb
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
**     11-24-95  Alin Tilden  Modified to maintain op_account_dataset list
**                            in addition to op_assigned_datasets.
**
**==========================================================================*/
 
void assign_datasets_delete_datasetsCb(
				       Widget wgt, 
				       XtPointer cd, 
				       XtPointer cb)
{
  _UxCassign_datasets     *UxSaveCtx, *UxContext;
  Widget                   UxWidget = wgt;
  XtPointer                UxClientData = cd;
  XtPointer                UxCallbackArg = cb;
  OP_ASSIGN_DATASETS      *assigned_ptr, *temp ;
  OP_ACCOUNT_DATASET      *acc_dset_ptr, *tmp_accdset_ptr;
  int cnt ;
  XmScrollBarCallbackStruct *cbs ;

  UxSaveCtx = UxAssign_datasetsContext;
  UxAssign_datasetsContext = UxContext =
    (_UxCassign_datasets *) UxGetContext( UxWidget );
  {
    assigned_ptr =
      glbData.accounts_users_data.account_data.op_assigned_datasets_ptr;
    acc_dset_ptr =
      glbData.accounts_users_data.account_data.op_account_dataset_ptr;

    cnt = 0 ;
    while (assigned_ptr != (OP_ASSIGN_DATASETS *) NULL ) 
    {
      temp = (OP_ASSIGN_DATASETS *) NULL;

      if ( assigned_ptr->selected )
      {
	cnt++;

	/* 
	 * update the status and the count for this dataset_idx
	 * in the account_dataset list 
	 */
	acc_dset_ptr = assigned_ptr->account_dataset_ptr;

	switch ( acc_dset_ptr->account_dataset_status )
	{
	case UNCHANGED : 
	case MODIFIED :
	  acc_dset_ptr->dataset_count--;
	  if (acc_dset_ptr->dataset_count <= 0)
	  {
	    acc_dset_ptr->account_dataset_status = DELETED;
	    acc_dset_ptr->dataset_count = 0;
	  }
	  break;

	case DELETED :
	  break;

	case NEW :
	  acc_dset_ptr->dataset_count--;
	  if (acc_dset_ptr->dataset_count <= 0)
	  {
	    /* delete this account_dataset list entry */
	    if (acc_dset_ptr->next != (OP_ACCOUNT_DATASET *)NULL)
	    {
	      acc_dset_ptr->next->prev = acc_dset_ptr->prev;
	    }
	    if (acc_dset_ptr->prev != (OP_ACCOUNT_DATASET *)NULL)
	    {
	      acc_dset_ptr->prev->next = acc_dset_ptr->next;
	    }
	    else
	    {
	      glbData.accounts_users_data.account_data.
		op_account_dataset_ptr = acc_dset_ptr->next;
	    }
	    free (acc_dset_ptr);
	  }
	  break;
  
	default:
	  break;
	}

	/* delete this dataset from the assigned_datasets list */
	if (assigned_ptr->next != (OP_ASSIGN_DATASETS *)NULL)
	{
	  assigned_ptr->next->prev = assigned_ptr->prev;
	}
	if (assigned_ptr->prev != (OP_ASSIGN_DATASETS *)NULL)
	{
	  assigned_ptr->prev->next = assigned_ptr->next;
	}
	else
	{
	  glbData.accounts_users_data.account_data.
	    op_assigned_datasets_ptr = assigned_ptr->next;
	}
	temp = assigned_ptr;

      } /* end of if (selected) */

      assigned_ptr = assigned_ptr->next;
      if (temp)
	free (temp);

    } /* end of while (assigned_ptr) */

    if (cnt <= 0)
    {
      msg_dlgCb ( msg_dlg, IMS_ERROR, 
		 "No dataset has been selected!");
      return ;            
    }
    else
    {
      /*
       * update the position and reset the selection status
       * for each entry in the assigned_datasets list
       */
      assigned_ptr = 
	glbData.accounts_users_data.account_data.op_assigned_datasets_ptr;
      cnt = 0;
      while (assigned_ptr != (OP_ASSIGN_DATASETS *) NULL )
      {
	assigned_ptr->position = cnt;
	cnt++;
	assigned_ptr->selected = 0;
	assigned_ptr = assigned_ptr->next;
      }
      glbData.accounts_users_data.account_data.assigned_datasets_cnt = cnt;
    }

    /* display new assigned_datasets list */
    cbs = ( XmScrollBarCallbackStruct * )
      XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
    cbs->value = 0 ; 
    assign_datasets_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
    XtFree ((char *)cbs) ;
  }
  UxAssign_datasetsContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name:   assign_datasets_permissionsCb
**
** Description:     Callback function for the PERMISSIONS widgets
**
** Arguments:       1. widget - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - XmListCallbackStruct
**
** Return Value:    None
**
** Revision History:
**     11-24-95  Alin Tilden  Modified to maintain op_account_dataset list
**                            in addition to op_assigned_datasets.
**
**==========================================================================*/

void	assign_datasets_permissionsCb(
				      Widget wgt, 
				      XtPointer cd, 
				      XtPointer cb)
{
  _UxCassign_datasets     *UxSaveCtx, *UxContext;
  Widget                   UxWidget = wgt;
  int                      the_widget = (int) cd;
  XmListCallbackStruct    *cbs = (XmListCallbackStruct *) cb;
  char                    *text ;
  XmString                 str ;
  OP_ASSIGN_DATASETS      *assigned_ptr, *dset_ptr ;
  int                      dset_position;

  UxSaveCtx = UxAssign_datasetsContext;
  UxAssign_datasetsContext = UxContext =
    (_UxCassign_datasets *) UxGetContext( UxWidget );

  {
    assigned_ptr =
      glbData.accounts_users_data.account_data.op_assigned_datasets_ptr ;

    /* get to the window top as set by the scroll bar */
    while ( (assigned_ptr != (OP_ASSIGN_DATASETS *) NULL ) &&
	    (assigned_ptr->position != assigned_window_top) )
    {
      assigned_ptr = assigned_ptr->next ;
    }

    while ( (assigned_ptr != (OP_ASSIGN_DATASETS *) NULL ) && 
	    (assigned_ptr->position < 
	     ( assigned_window_top+DATASETS_LIST_LENGTH) ) ) 
    {
      if (assigned_ptr->position 
	  == assigned_window_top + cbs->item_position - 1 )
      {
	break;
      }
      assigned_ptr = assigned_ptr->next ;
    }

    if ( assigned_ptr == ( OP_ASSIGN_DATASETS * ) NULL )
      return ;

    /* read the modification made to permissions on the display */
    XmStringGetLtoR ( cbs->item,XmFONTLIST_DEFAULT_TAG, &text ) ;
    ims_truncStr(text ) ;

    /* 
     * update permissions kept in account_dataset list
     * Note: in account_dataset list, dataset_idx is unique
     */
    if ( strcmp ( text, "Y" ) ) 
    {
      /* request to change from "N" to "Y" */
      switch ( the_widget ) 
      {
      case 1 : assigned_ptr->account_dataset_ptr->permissions |= 16 ;
	       break ;

      case 2 : assigned_ptr->account_dataset_ptr->permissions |= 8 ;
	       break ;

      case 3 : assigned_ptr->account_dataset_ptr->permissions |= 4 ;
	       break ;

      case 4 : assigned_ptr->account_dataset_ptr->permissions |= 2 ;
	       break ;

      case 5 : assigned_ptr->account_dataset_ptr->permissions |= 1 ;
	       break ;

      default : XtFree (text);
	        return;
      }

      str = XmStringCreateLocalized ("Y");
    }
    else 
    {
      /* request to change from "Y" to "N" */
      switch ( the_widget ) 
      {
      case 1 : assigned_ptr->account_dataset_ptr->permissions &= 15 ;
	       break ;

      case 2 : assigned_ptr->account_dataset_ptr->permissions &= 23 ;
	       break ;

      case 3 : assigned_ptr->account_dataset_ptr->permissions &= 27 ;
	       break ;

      case 4 : assigned_ptr->account_dataset_ptr->permissions &= 29 ;
	       break ;

      case 5 : assigned_ptr->account_dataset_ptr->permissions &= 30 ;
	       break ;

      default : XtFree (text);
                return;
      }

      str = XmStringCreateLocalized ("N");
    }
    
    /* update display for all datasets with the same dataset_idx */
    dset_ptr =
      glbData.accounts_users_data.account_data.op_assigned_datasets_ptr ;

    /* get to the window top as set by the scroll bar */
    while ( (dset_ptr != (OP_ASSIGN_DATASETS *) NULL ) &&
	    (dset_ptr->position != assigned_window_top) )
    {
      dset_ptr = dset_ptr->next ;
    }

    while ( (dset_ptr != (OP_ASSIGN_DATASETS *) NULL ) && 
	    (dset_ptr->position < 
	     ( assigned_window_top+DATASETS_LIST_LENGTH) ) ) 
    {
      if (dset_ptr->dataset_idx == assigned_ptr->dataset_idx)
      {
	dset_position = dset_ptr->position - assigned_window_top + 1;
	XmListReplacePositions (wgt, &dset_position, &str, 1);
      }
      
      dset_ptr = dset_ptr->next;
    }

    /* unselect the selected dataset */
    XmListSelectPos (wgt, cbs->item_position, False) ;

    /* update dataset status in account_dataset list */
    switch (assigned_ptr->account_dataset_ptr->account_dataset_status)
    {
    case UNCHANGED :
      assigned_ptr->account_dataset_ptr->account_dataset_status = MODIFIED;
      break;

    case MODIFIED : 
    case DELETED : 
    case NEW :
      break;

    default :
      break;
    }

    XtFree ( text ) ;
    XmStringFree (str ) ;
  }
  UxAssign_datasetsContext = UxSaveCtx;
}



/*===========================================================================*
**
** Function Name:   assign_datasets_updateCb
**
** Description:     Callback function for the UPDATE widget
**                  This function updates the catalog with the modifications
**                  made on the assign datasets screen.
**
** Arguments:       1. widget   - Widget that is calling this callback
**                  2. cd       - not used
**                  3. cb       - not used
**
** Return Value:    None
**
** Revision History:
** 		04/23/96  		J. Ting   Assigned catReq->item[0] to count for catReq
** 													  OP_ACC_GET_ASSIGN_DATASETS
**
**==========================================================================*/

void	assign_datasets_updateCb(
				 Widget wgt, 
				 XtPointer cd, 
				 XtPointer cb)
{
  _UxCassign_datasets       *UxSaveCtx, *UxContext;
  Widget                     UxWidget = wgt;
  XtPointer                  UxClientData = cd;
  XtPointer                  UxCallbackArg = cb;
  OP_ACCOUNT_DATASET        *acc_dset_ptr, *new_accdset_ptr;
  OP_ACCOUNT_DATASET        *update_ptr, *update_start;
  OP_ACCOUNT_DATASET        *delete_ptr, *delete_start;
  OP_ACCOUNT_DATASET        *add_ptr, *add_start;
  OP_CAT_STRUCT             *catReq;
  char                      *acc_id ;
  char                       buffer[IMS_COL255_LEN*2+1];
  XmScrollBarCallbackStruct *cbs ;
	int count = 0;

  UxSaveCtx = UxAssign_datasetsContext;
  UxAssign_datasetsContext = UxContext =
    (_UxCassign_datasets *) UxGetContext( UxWidget );
  {
    acc_dset_ptr = 
      glbData.accounts_users_data.account_data.op_account_dataset_ptr;
    update_ptr = update_start = (OP_ACCOUNT_DATASET *)NULL;
    delete_ptr = delete_start = (OP_ACCOUNT_DATASET *)NULL;
    add_ptr = add_start = (OP_ACCOUNT_DATASET *)NULL;

    while (acc_dset_ptr != (OP_ACCOUNT_DATASET *)NULL)
      {
	switch (acc_dset_ptr->account_dataset_status)
	{
	case UNCHANGED :
	  break;

	case MODIFIED :
	  /* add acc_dset_ptr to update list */
	  if ((new_accdset_ptr = 
	       (OP_ACCOUNT_DATASET *)malloc(sizeof(OP_ACCOUNT_DATASET)))
	      == (OP_ACCOUNT_DATASET *)NULL)
	  {
	    msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	    return;
	  }
	  else
	  {
	    new_accdset_ptr->prev = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->next = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->dataset_idx = acc_dset_ptr->dataset_idx;
	    new_accdset_ptr->permissions = acc_dset_ptr->permissions;
	    new_accdset_ptr->account_dataset_status = 
	      acc_dset_ptr->account_dataset_status;
	    
	    if (update_start == (OP_ACCOUNT_DATASET *)NULL)
	    {
	      update_start = new_accdset_ptr;
	      update_ptr = update_start;
	    }
	    else
	    {
	      update_ptr->next = new_accdset_ptr;
	      new_accdset_ptr->prev = update_ptr;
	      update_ptr = update_ptr->next;
	    }

	  }
	  break;

	case DELETED :
	  /* add acc_dset_ptr to delete list */
	  if ((new_accdset_ptr = 
	       (OP_ACCOUNT_DATASET *)malloc(sizeof(OP_ACCOUNT_DATASET)))
	      == (OP_ACCOUNT_DATASET *)NULL)
	  {
	    msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	    return;
	  }
	  else
	  {
	    new_accdset_ptr->prev = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->next = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->dataset_idx = acc_dset_ptr->dataset_idx;
	    new_accdset_ptr->permissions = acc_dset_ptr->permissions;
	    new_accdset_ptr->account_dataset_status = 
	      acc_dset_ptr->account_dataset_status;
	    
	    if (delete_start == (OP_ACCOUNT_DATASET *)NULL)
	    {
	      delete_start = new_accdset_ptr;
	      delete_ptr = delete_start;
	    }
	    else
	    {
	      delete_ptr->next = new_accdset_ptr;
	      new_accdset_ptr->prev = delete_ptr;
	      delete_ptr = delete_ptr->next;
	    }
	  }
	  break;

	case NEW :
	  /* add acc_dset_ptr to add list */
	  if ((new_accdset_ptr = 
	       (OP_ACCOUNT_DATASET *)malloc(sizeof(OP_ACCOUNT_DATASET)))
	      == (OP_ACCOUNT_DATASET *)NULL)
	  {
	    msg_dlgCb ( msg_dlg, IMS_FATAL, "Memory allocation failed.");
	    return;
	  }
	  else
	  {
	    new_accdset_ptr->prev = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->next = (OP_ACCOUNT_DATASET *)NULL;
	    new_accdset_ptr->dataset_idx = acc_dset_ptr->dataset_idx;
	    new_accdset_ptr->permissions = acc_dset_ptr->permissions;
	    new_accdset_ptr->account_dataset_status = 
	      acc_dset_ptr->account_dataset_status;
	    
	    if (add_start == (OP_ACCOUNT_DATASET *)NULL)
	    {
	      add_start = new_accdset_ptr;
	      add_ptr = add_start;
	    }
	    else
	    {
	      add_ptr->next = new_accdset_ptr;
	      new_accdset_ptr->prev = add_ptr;
	      add_ptr = add_ptr->next;
	    }
	  }	  
	  break;

	default:
	  break;
	}
	    
	acc_dset_ptr = acc_dset_ptr->next;
      } /* end of while(acc_dset_ptr) */

    /* prepare for database updating */
    acc_id = (char *)XmTextGetString (account_idTF) ;
    ims_truncStr(acc_id) ;

    /* 
     * if any existing account_dataset entries have been modified,
     * update the catalog.
     */
    if (update_start != (OP_ACCOUNT_DATASET *)NULL)
    {
      catReq = &(glbData.accounts_users_data.catReq);
      catReq->item[1] = (OP_ACCOUNT_DATASET *)update_start;
      catReq->item[2] = (char *)acc_id;

      if (ims_op_accCat (catReq, OP_ACC_UPDATE_ASSIGN_DATASETS) < IMS_OK) 
      {
	/* Display error messages */
	msg_dlgCb (msg_dlg, IMS_FATAL,
                   "Internal Error: update_assign_datasets failed.");
	XtFree ( acc_id );
	while (update_start != (OP_ACCOUNT_DATASET *)NULL)
	{
	    update_ptr = update_start->next;
	    free (update_start);
	    update_start = update_ptr;
	}
	return ; 
      }
    }
    while (update_start != (OP_ACCOUNT_DATASET *)NULL)
    {
	update_ptr = update_start->next;
	free (update_start);
	update_start = update_ptr;
    }

    /* 
     * if any account_dataset entries have been deleted,
     * execute the deletion
     */
    if (delete_start != (OP_ACCOUNT_DATASET *)NULL)
    {
      catReq = &(glbData.accounts_users_data.catReq);	 
      catReq->item[1] = (OP_ACCOUNT_DATASET *)delete_start;
      catReq->item[2] = (char *)acc_id;

      if (ims_op_accCat (catReq, OP_ACC_DELETE_ASSIGN_DATASETS) < IMS_OK) 
      {
	/* Display error messages */
	msg_dlgCb (msg_dlg, IMS_FATAL,
		   "Couldn't delete dataset(s) from account.");
	XtFree ( acc_id ) ;
	while (delete_start != (OP_ACCOUNT_DATASET *)NULL)
	{
	    delete_ptr = delete_start->next;
	    free (delete_start);
	    delete_start = delete_ptr;
	}
	return ; 
      }
    }
    while (delete_start != (OP_ACCOUNT_DATASET *)NULL)
    {
	delete_ptr = delete_start->next;
	free (delete_start);
	delete_start = delete_ptr;
    }

    /* 
     * if any new account_dataset have been added to
     * this account, execute insert
     */
    if (add_start != (OP_ACCOUNT_DATASET *)NULL)
    {
      catReq = &(glbData.accounts_users_data.catReq);	 
      catReq->item[1] = (OP_ACCOUNT_DATASET *)add_start;
      catReq->item[2] = (char *)acc_id;

      if (ims_op_accCat (catReq, OP_ACC_ADD_ASSIGN_DATASETS) < IMS_OK) 
      {
	/* Display error messages */
	msg_dlgCb (msg_dlg, IMS_FATAL,
		   "Couldn't assign dataset(s) to account.");
	XtFree ( acc_id ) ;
	while (add_start != (OP_ACCOUNT_DATASET *)NULL)
	{
	    add_ptr = add_start->next;
	    free (add_start);
	    add_start = add_ptr;
	}
	return ; 
      }
      while (add_start != (OP_ACCOUNT_DATASET *)NULL)
      {
	  add_ptr = add_start->next;
	  free (add_start);
	  add_start = add_ptr;
      }
    }

    /* free assigned_datasets, and account_dataset lists */
    free_assigned_datasets ();

    /* 
     * get the new assigned_datasets, and account_dataset lists 
     * for this account id, from the catalog.
     */
    catReq = &(glbData.accounts_users_data.catReq);	 
    buffer[0] = '\0';
    sprintf (buffer,
	     "select t1.dataset_idx, t1.dataset, t1.sensor,"
	     "       t1.platform, t2.oagdr "
	     "from dataset_relation t1, account_dataset t2 "
	     "where t1.dataset_idx = t2.dataset_idx "
	     "      and t2.account_id = '%s' "
	     "order by t1.dataset_idx, t1.dataset", 
	     acc_id );

		/*
		** 04/23/96 - assign catReq->item[0] to count
		*/
    catReq->item[0] = (int *) &count;
    catReq->item[1] = (char *) buffer;

    if (ims_op_accCat (catReq, OP_ACC_GET_ASSIGN_DATASETS) < IMS_OK) 
    {
      /* Display error messages */
      sprintf(buffer,
	      "Couldn't get datasets assigned to account");   
      msg_dlgCb (msg_dlg, IMS_FATAL, buffer);
      XtFree ( acc_id ) ;
      return ; 
    }

    /* get data */
    glbData.accounts_users_data.account_data.assigned_datasets_cnt = 
      *(int *)catReq->item[0];
    glbData.accounts_users_data.account_data.op_assigned_datasets_ptr = 
      (OP_ASSIGN_DATASETS *)catReq->item[2]; 
    glbData.accounts_users_data.account_data.op_account_dataset_ptr = 
      (OP_ACCOUNT_DATASET *)catReq->item[3];
    cbs = ( XmScrollBarCallbackStruct * )
      XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
    cbs->value = 0 ; 
    assign_datasets_assigned_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;

    XtFree ((char *)cbs) ;
    XtFree ( acc_id ) ;
  }

  UxAssign_datasetsContext = UxSaveCtx;

  (void) assign_datasets_closeCb (wgt, cd, cb);
}


void	template10(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCassign_datasets     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAssign_datasetsContext;
	UxAssign_datasetsContext = UxContext =
			(_UxCassign_datasets *) UxGetContext( UxWidget );
	{
	  ;
	}
	UxAssign_datasetsContext = UxSaveCtx;
}
