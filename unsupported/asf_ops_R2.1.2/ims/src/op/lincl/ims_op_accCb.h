/************************************************************************
**
** ims_op_accCb.h
**
*************************************************************************/

#ifndef _IMS_OPACCCB_H
#define _IMS_OPACCCB_H
 
static char *sccsOpAccCb = "@(#)ims_op_accCb.h	5.2  11/01/96";


/*----------------------------------------------------------------------*/
/* callback functions for accounts_users screen                         */
/*----------------------------------------------------------------------*/

void accounts_users_create_interfacesCb (  ) ;
void accounts_users_create_accountCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_delete_accountCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_search_accountsCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_assign_usersCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_assign_datasetsCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_create_userCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_delete_userCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_accounts_listCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_search_usersCb ( Widget, XtPointer, XtPointer ) ;
void accounts_users_users_listCb (Widget, XtPointer, XtPointer ) ;
void accounts_users_closeCb (Widget, XtPointer, XtPointer ) ;
void accounts_users_users_sbCb (Widget, XtPointer, XtPointer ) ;
void accounts_users_accounts_sbCb (Widget, XtPointer, XtPointer ) ;
void accounts_users_init_popCb (Widget, XtPointer, XtPointer ) ;
void accounts_users_goto_welcomeCb (Widget, XtPointer, XtPointer ) ;


/*-----------------------------------------------------------------------*/
/* callback functions for user_data screen                               */
/*-----------------------------------------------------------------------*/

void user_data_labelCb ( Widget, XtPointer, XtPointer ) ;
void user_data_closeCb (  Widget, XtPointer, XtPointer ) ;
void user_data_cancelCb (  Widget, XtPointer, XtPointer ) ;
void user_data_option_priorityCb ( Widget, XtPointer, XtPointer ) ;
void user_data_option_typeCb ( Widget, XtPointer, XtPointer ) ;
void user_data_losing_focusCb (  Widget, XtPointer, XtPointer ) ;
void user_data_createCb (  Widget, XtPointer, XtPointer ) ;

/*------------------------------------------------------------------------*/
/* callback functions for account_data screen                             */
/*------------------------------------------------------------------------*/

void account_data_labelCb ( Widget, XtPointer, XtPointer ) ;
void account_data_option_resourcesCb ( Widget, XtPointer, XtPointer ) ;
void account_data_option_typeCb ( Widget, XtPointer, XtPointer ) ;
void account_data_current_balanceCb (  Widget, XtPointer, XtPointer ) ;
void account_data_begin_balanceCb ( Widget, XtPointer, XtPointer ) ;
void account_data_update_balanceCb ( Widget, XtPointer, XtPointer ) ;
void account_data_validationCb ( Widget, XtPointer, XtPointer ) ;
void account_data_quicklookCb ( Widget, XtPointer, XtPointer ) ;
void account_data_closeCb ( Widget, XtPointer, XtPointer ) ;
void account_data_cancelCb ( Widget, XtPointer, XtPointer ) ;
void account_data_option_special_procCb ( Widget, XtPointer, XtPointer ) ;
void account_data_losing_focusCb ( Widget, XtPointer, XtPointer ) ;
void account_data_creation_focusCb ( Widget, XtPointer, XtPointer ) ;
void account_data_current_balance_focusCb ( Widget, XtPointer, XtPointer ) ;
void account_data_on_hold_focusCb ( Widget, XtPointer, XtPointer ) ;
void account_data_rate_focusCb ( Widget, XtPointer, XtPointer ) ;
void account_data_createCb ( Widget, XtPointer, XtPointer ) ;
void account_data_check_dateCb ( Widget, XtPointer, XtPointer ) ;

/*-------------------------------------------------------------------------*/
/* callback functions for delete_dlg, msgBoxDlg */
/*-------------------------------------------------------------------------*/

void delete_dlgCb ( Widget ,XtPointer,XtPointer ) ;
void msg_dlgCb ( Widget, int, char * ) ;
void msg_dlg_okCb ( Widget, XtPointer, XtPointer ) ;
/*------------------------------------------------------------------------*/
/* callback functions for search_users screen                             */
/*------------------------------------------------------------------------*/

void search_users_option_typeCb ( Widget ,XtPointer,XtPointer ) ;
void search_users_closeCb ( Widget ,XtPointer,XtPointer ) ;
void search_users_start_searchCb ( Widget, XtPointer, XtPointer ) ;
void search_users_losing_focusCb ( Widget, XtPointer, XtPointer ) ;
/*------------------------------------------------------------------------*/
/* callback functions for search_accounts screen                          */
/*------------------------------------------------------------------------*/

void search_accounts_option_resourcesCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_option_typeCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_closeCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_start_searchCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_losing_focusCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_start_searchCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_check_dateCb ( Widget, XtPointer, XtPointer ) ;
void search_accounts_check_balanceCb ( Widget, XtPointer, XtPointer ) ;

/*------------------------------------------------------------------------*/
/* callback functions for assing_datasets screen                          */
/*------------------------------------------------------------------------*/

void assign_datasets_datasets_sbCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_assigned_sbCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_datasets_listCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_assigned_listCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_add_datasetsCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_delete_datasetsCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_closeCb ( Widget, XtPointer, XtPointer ) ;
void assign_datasets_init_dataCb (  Widget, XtPointer, XtPointer ) ;
void assign_datasets_permissionsCb (  Widget, XtPointer, XtPointer ) ;
void assign_datasets_updateCb (  Widget, XtPointer, XtPointer ) ;
void assign_users_updateCb (  Widget, XtPointer, XtPointer ) ;

/*------------------------------------------------------------------------*/
/* callback functions for assing_users screen                          */
/*------------------------------------------------------------------------*/

void assign_users_users_sbCb ( Widget, XtPointer, XtPointer ) ;
void assign_users_assigned_sbCb ( Widget, XtPointer, XtPointer ) ;
void assign_users_assigned_listCb ( Widget, XtPointer, XtPointer ) ;
void assign_users_init_dataCb (  Widget, XtPointer, XtPointer ) ;
void assign_users_closeCb (  Widget, XtPointer, XtPointer ) ;
void assign_users_users_listCb (  Widget, XtPointer, XtPointer ) ;
void assign_users_add_usersCb (  Widget, XtPointer, XtPointer ) ;
void assign_users_delete_usersCb (  Widget, XtPointer, XtPointer ) ;

/*------------------------------------------------------------------------*/
/* callback functions for update_balance_dlg screen                       */
/*------------------------------------------------------------------------*/

void update_balance_dlg_okCb( Widget, XtPointer, XtPointer ) ;

/*------------------------------------------------------------------------*/
/* callback functions for manager select dlg screen                       */
/*------------------------------------------------------------------------*/

void account_manager_select_dlg_okCb ( Widget, XtPointer, XtPointer ) ;
void account_manager_select_dlg_popupCb ( Widget, XtPointer, XtPointer ) ;

#endif  /* !IMS_OPACCCB */
