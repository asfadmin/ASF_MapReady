static char *sccs = "@(#)ims_op_accSearchAccountCb.c	5.7  11/22/96";
/*******************************************************************************
**
** File:		ims_op_accSearchAccountCb.c
**
** Function:		Callback functions for the search_accounts screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Modified:
**
**   11/10/95   A. Tilden   Corrected file name.
**                          Modified search account screen query to query the
**                          account and account_mgr tables separately.
**                          Modified to pop down search screen after
**                          account query is complete.
**
**   04/24/96   J. Ting     Modified function create_search_account_query.
**                          Corrected query construction problem with 
**                          operator "and".
**
**   04/24/96   J. Ting     Modified function search_accounts_start_searchCb.
**													Change timeout cursors back to normal cursors
**                          if the query execution fails.
**
**   04/25/96   J. Ting     Modified function execute_search_account_query
**                          to fix search account query problem, also
**                          added flag account_mgr_search_flag to indicate
**                          whether any account manager search parameter 
**                          is specified in the Search Account screen.
**
**   06/12/96  J. Ting      Modified function search_accounts_closeCb for
**                          PR 942.
**
**   09/06/96  J. Ting      Modified function create_search_account_query
**                          for PR 84.
**
**   10/07/96  J. Ting      R2.1 - took out account manager information.
**
**   10/14/96  J. Ting      R2.1 - took out quicklook_p field.
**
**   11/05/96  J. Ting      R2.1 - changed all local_dir to ftp_dir
******************************************************************************/
#include <stdio.h>
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

#define LOW_YEAR 1990
#define HIGH_YEAR 2050

extern accounts_users_accounts_sbCb ( Widget, XtPointer , XtPointer ) ;
extern void msg_dlgCb( Widget wgt, int status, 	char *msg) ;


extern OP_GLOBAL_DATA glbData;
extern  Widget msg_dlg ;

static char * id = (char*) NULL ;
static char * type = (char*) NULL ;
static char * resource = (char*) NULL ;
static char * start_creation = (char*) NULL ;
static char * end_creation = (char*) NULL ;
static char * start_expiration = (char*) NULL ;
static char * end_expiration = (char*) NULL ;
static char * start_balance = (char*) NULL ;
static char * end_balance = (char*) NULL ;
static char * comments = (char*) NULL ;
static char * mgr_user_id = (char*) NULL ;
/********************************************
static char * first_name = (char*) NULL ;
static char * m_i = (char*) NULL ;
static char * last_name = (char*) NULL ;
static char * organization = (char*) NULL ;
static char * city = (char*) NULL ;
static char * state = (char*) NULL ;
static char * country = (char*) NULL ;
static char * zip = (char*) NULL ;
static char * phone = (char*) NULL ;
static char * fax = (char*) NULL ;
static char * email = (char*) NULL ;
********************************************/

void free_account_data ( ) ;
static void create_search_account_query ( ) ;
int execute_search_account_query ( ) ;
int is_date_valid( char  *) ;
int account_mgr_search_flag;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accSearchAccount.h"
#undef CONTEXT_MACRO_ACCESS


void search_accounts_option_resourcesCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
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


	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
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
			(XtCallbackProc) search_accounts_option_resourcesCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxSearch_accountsContext );

                XmStringFree (label);
                /***if ( i == 0 ) {
                  XtVaGetValues ( menu_item, XmNlabelString, &button_label,
                                                                     NULL ) ;
                  if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
                     XmTextSetString (  resourceTF  , text ) ;
                     XtFree ( text ) ;
                  }
                  XmStringFree ( button_label ) ;
                } ***/     
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
	UxSearch_accountsContext = UxSaveCtx;


}


void search_accounts_option_typeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt ;

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

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
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
			(XtCallbackProc) search_accounts_option_typeCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxSearch_accountsContext );

                XmStringFree (label);
                /***if ( i == 0 ) {
                  XtVaGetValues ( menu_item, XmNlabelString, &button_label,
                                                                     NULL ) ;
                  if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
                     XmTextSetString (  typeTF  , text ) ;
                     XtFree ( text ) ;
                  }
                  XmStringFree ( button_label ) ;
                } ***/     
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
	UxSearch_accountsContext = UxSaveCtx;


}

/*===========================================================================*
**
** Function Name:   search_accounts_closeCb
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
**     06/12/96  J. Ting   Modified to use glbData.search_accountsW to get
**                         the context instead of using UxWidget. This
**                         is done for mwm quit button to work - PR 942.
**
**     10/07/96  J. Ting   Modified to take out account manager information.
**     10/14/96  J. Ting   Modified to add new field mgr_user_id.
**==========================================================================*/
void search_accounts_closeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
       
	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( glbData.search_accountsW ) ;
	{
           /* free strings and clear text fields */
              if ( id != NULL ) {
                XtFree ( id ) ;
	        id = (char*) NULL ;
	        XmTextSetString ( idTF,"" ) ; 
              }
              if ( type != NULL ) {
                XtFree ( type ) ;
                type = (char * ) NULL ;
	        XmTextSetString ( typeTF,"" ) ; 
              }
              if ( resource != NULL ) {
                XtFree ( resource ) ;
                resource = (char * ) NULL ;
                XmTextSetString ( resourceTF,"" ) ; 
              }
              if ( start_creation != NULL ) {
                XtFree ( start_creation ) ;
                start_creation = (char *)NULL ;
                XmTextSetString ( start_creationTF,"" ) ;
              } 
              if ( end_creation != NULL ) {
                XtFree ( end_creation ) ;
                end_creation = (char * )NULL ;
                XmTextSetString ( end_creationTF,"" ) ;
              }
              if ( start_expiration != NULL ) {
                XtFree ( start_expiration ) ;
                start_expiration = (char * )NULL ;
                XmTextSetString ( start_expirationTF,"" ) ; 
              }
              if ( end_expiration != NULL ) {
                XtFree ( end_expiration ) ;
                end_expiration = (char * )NULL ;
                XmTextSetString ( end_expirationTF,"" ) ;
              }
              if ( start_balance != NULL ) {
                XtFree ( start_balance ) ;
                start_balance = (char *)NULL ;
                XmTextSetString ( start_balanceTF,"" ) ; 
              }
              if ( end_balance != NULL ) {
                XtFree ( end_balance ) ;
                end_balance= ( char *)NULL ;
                XmTextSetString ( end_balanceTF,"" ) ; 
              }
              if ( comments != NULL ) {
                XtFree ( comments ) ;
                comments = (char *)NULL ;
                XmTextSetString ( commentsST,"" ) ;
              } 
              if ( mgr_user_id != NULL ) {
                XtFree ( mgr_user_id ) ;
			        mgr_user_id = (char*) NULL ;
	    		    XmTextSetString ( managerTF,"" ) ; 
              }
				/*****************************************************
              if ( first_name != NULL ) {
                XtFree ( first_name ) ;
                first_name = (char *)NULL ;
                XmTextSetString ( first_nameTF,"" ) ;
              }
              if ( m_i != NULL ) {
                XtFree ( m_i ) ;
                m_i = (char *)NULL ;
                XmTextSetString ( m_iTF,"" ) ; 
              }
              if ( last_name != NULL ) {
                XtFree ( last_name ) ;
                last_name = (char *)NULL ;
                XmTextSetString ( last_nameTF,"" ) ;
              }
              if ( organization != NULL ) {
                XtFree ( organization ) ;
                organization = (char *)NULL ;
                XmTextSetString ( organizationTF,"" ) ;
              }
              if ( city != NULL ) {
                XtFree ( city ) ;
                city = (char *)NULL ;
                XmTextSetString ( cityTF,"" ) ;
              }
              if ( state != NULL ) {
                XtFree ( state ) ;
                state = (char *)NULL ;
                XmTextSetString ( stateTF,"" ) ;
              }
              if ( country != NULL ) {
                XtFree ( country ) ;
	        country = (char *)NULL ;
                XmTextSetString ( countryTF,"" ) ;
              }
              if ( zip != NULL ) {
                XtFree ( zip ) ;
                zip = (char *)NULL ;
                XmTextSetString ( zipTF,"" ) ;
              }
              if ( phone != NULL ) {
                XtFree ( phone ) ;
                phone = (char *)NULL ;
                XmTextSetString ( phoneTF,"" ) ;
              }
              if ( fax != NULL ) {
                XtFree ( fax ) ;
                fax = (char *)NULL ;
                XmTextSetString ( faxTF,"" ) ;
              }
              if ( email != NULL ) {
                XtFree ( email ) ;
                email = (char *)NULL ;
                XmTextSetString ( emailTF,"" ) ; 
              }
       *******************************************************/    
              XtPopdown(XtParent(glbData.search_accountsW));
              glbData.search_accountsFlag = 0 ; /* screen has been pop down */	  
	}
	UxSearch_accountsContext = UxSaveCtx;


}



/*===========================================================================*
**
** Function Name:   search_accounts_losing_focusCb
**
** Revision History:
**     10/07/96  J. Ting   Modified to take out account manager information.
**     10/14/96  J. Ting   Added mgr_user_id field.
**
**==========================================================================*/
void search_accounts_losing_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int widget_num = ( int ) cd;
	XtPointer               UxCallbackArg = cb;
        char Msg[IMS_COL1024_LEN+1];

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
	{
       switch ( widget_num ) {
            case 1 :
              if ( id != NULL ) {
                XtFree ( id ) ;
              }
	      id = XmTextGetString ( idTF ) ; 
	      ims_truncStr(id) ;
              break ;
            case 2 :
              if ( type != NULL ) {
                XtFree ( type ) ;
              }
	      type = XmTextGetString ( typeTF ) ; 
	      ims_truncStr(type) ;
              break ;
            case 3 :
              if ( resource != NULL ) {
                XtFree ( resource ) ;
              }
	      resource = XmTextGetString ( resourceTF ) ; 
	      ims_truncStr(resource) ;
              break ;
            case 4 :
              if ( start_creation != NULL ) {
                XtFree ( start_creation ) ;
              }
	      start_creation = XmTextGetString ( start_creationTF ) ; 
	      if ((is_date_valid (start_creation)) < IMS_OK) {
			XmTextSetString (start_creationTF, "");			
			msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Date!"); 
                        return ;
              }

	      ims_truncStr(start_creation) ;
              break ;
            case 5 :
              if ( end_creation != NULL ) {
                XtFree ( end_creation ) ;
              }
	      end_creation = XmTextGetString ( end_creationTF ) ;
	      if ((is_date_valid (end_creation)) < IMS_OK) {
			XmTextSetString (end_creationTF, "");			
			msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Date!"); 
                        return ;
              }
	      ims_truncStr(end_creation) ; 
              break ;
            case 6 :
              if ( start_expiration != NULL ) {
                XtFree ( start_expiration ) ;
              }
	      start_expiration = XmTextGetString ( start_expirationTF ) ; 
	      if ((is_date_valid (start_expiration)) < IMS_OK) {
			XmTextSetString (start_expirationTF, "");
			msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Date!"); 
                        return ;
              }
	      ims_truncStr(start_expiration) ;
              break ;
            case 7 :
              if ( end_expiration != NULL ) {
                XtFree ( end_expiration ) ;
              }
	      end_expiration = XmTextGetString ( end_expirationTF ) ;
	      if ((is_date_valid (end_expiration)) < IMS_OK) {
			XmTextSetString (end_expirationTF, "");
			msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Date!"); 
                        return ;
              }
	      ims_truncStr(end_expiration) ;
              break ;
            case 8 :
              if ( start_balance != NULL ) {
                XtFree ( start_balance ) ;
              }
	      start_balance = XmTextGetString ( start_balanceTF ) ; 
	      ims_truncStr(start_balance) ;
              break ;
            case 9 :
              if ( end_balance != NULL ) {
                XtFree ( end_balance ) ;
              }
	      end_balance= XmTextGetString ( end_balanceTF ) ; 
	      ims_truncStr(end_balance) ;
              break ;
            case 10 :
              if ( comments != NULL ) {
                XtFree ( comments ) ;
              }
	      comments = XmTextGetString ( commentsST ) ; 
	      ims_truncStr(comments) ;
              break ;
            case 11 :
              if ( mgr_user_id != NULL ) {
                XtFree ( mgr_user_id ) ;
              }
	      mgr_user_id = XmTextGetString ( managerTF ) ; 
	      ims_truncStr(mgr_user_id) ;
              break ;
/*******************************************************************
            case 11 :
              if ( first_name != NULL ) {
                XtFree ( first_name ) ;
              }
	      first_name = XmTextGetString ( first_nameTF ) ; 
	      ims_truncStr(first_name) ;
              break ;
            case 12 :
              if ( m_i != NULL ) {
                XtFree ( m_i ) ;
              }
	      m_i = XmTextGetString ( m_iTF ) ; 
	      ims_truncStr(m_i) ;
              break ;
            case 13 :
              if ( last_name != NULL ) {
                XtFree ( last_name ) ;
              }
	      last_name = XmTextGetString ( last_nameTF ) ; 
	      ims_truncStr(last_name) ;
              break ;
            case 14 :
              if ( organization != NULL ) {
                XtFree ( organization ) ;
              }
	      organization = XmTextGetString ( organizationTF ) ;
	      ims_truncStr(organization) ;
              break ; 
            case 15 :
              if ( city != NULL ) {
                XtFree ( city ) ;
              }
	      city = XmTextGetString ( cityTF ) ;
	      ims_truncStr(city) ;
              break ;
            case 16 :
              if ( state != NULL ) {
                XtFree ( state ) ;
              }
	      state = XmTextGetString ( stateTF ) ; 
	      ims_truncStr(state) ;
              break ;
            case 17 :
              if ( country != NULL ) {
                XtFree ( country ) ;
              }
	      country = XmTextGetString ( countryTF ) ; 
	      ims_truncStr(country) ;
              break ;
            case 18 :
              if ( zip != NULL ) {
                XtFree ( zip ) ;
              }
	      zip = XmTextGetString ( zipTF ) ; 
	      ims_truncStr(zip) ;
              break ;
            case 19 :
              if ( phone != NULL ) {
                XtFree ( phone ) ;
              }
	      phone = XmTextGetString ( phoneTF ) ;
	      ims_truncStr(phone) ; 
              break ;
            case 20 :
              if ( fax != NULL ) {
                XtFree ( fax ) ;
              }
	      fax = XmTextGetString ( faxTF ) ;
	      ims_truncStr(fax) ; 
              break ;
            case 21 :
              if ( email != NULL ) {
                XtFree ( email ) ;
              }
	      email = XmTextGetString ( emailTF ) ; 
	      ims_truncStr(email) ;
              break ;
*****************************************************************/

            default :
              break ;
          }

	}
	UxSearch_accountsContext = UxSaveCtx;


}

void search_accounts_start_searchCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
	{
        XmScrollBarCallbackStruct *cbs ;
        Widget sb ;

	  	/* Change cursor to watch cursor */
	  	timeOutCursors (True);

        free_account_data ( ) ;
        create_search_account_query ( ) ;
        if ( execute_search_account_query ( ) < IMS_OK ) {

						/*
						** 4/23/96 - change cursor back to normal if the 
						** query execution fails.
						*/
	  			 	timeOutCursors (False);

            return ;
        }

        /* Update accounts list                                               */
          
        cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
        cbs->value = 0 ; 
        accounts_users_accounts_sbCb ( (Widget)NULL, NULL, (XtPointer)cbs ) ;
        XtFree ((char *)cbs) ;

	  	/* Change cursor back to normal */
	  	timeOutCursors (False);

	}
	UxSearch_accountsContext = UxSaveCtx;

	(void) search_accounts_closeCb (wgt, cd, cb) ;
}


void free_account_data ( ) {
  
  OP_ACCOUNT *curr_ptr, *last_ptr ;

  if ( glbData.accounts_users_data.account_data.accounts_cnt > 0 ) {
    glbData.accounts_users_data.account_data.accounts_cnt = 0 ;
    curr_ptr = glbData.accounts_users_data.account_data.op_account_ptr ;
    while ( curr_ptr != (OP_ACCOUNT*) NULL ) {
       last_ptr = curr_ptr->next ;
       free (curr_ptr) ;
       curr_ptr = last_ptr ;
    }
  }/* end if */
}/* end free_account_data */

/***************************************************************************
**
** Function Name :	create_search_account_query
**
** Description:		Initializes 
**                      glbData.accounts_users_data.account_data.
**                         search_queryStruct.sqlBuf and
**                      glbData.accounts_users_data.account_data.
**                         search_mgr_queryStruct.sqlBuf
**       		with query to be executed.
**
** Arguments:		void
**
** Return Value:	none
**
** Modified:  04/24/96  J. Ting  Added if (cnt) check for using 'and'
**
** Modified:  04/24/96  J. Ting  Added account_mgr_search_flag.
**
** Modified:  09/06/96  J. Ting  Added apostrophe support for comments
**                               as per PR 84.
**
** Modified:  10/07/96  J. Ting  Took out account manager information.
**
** Modified:  10/14/96  J. Ting  Added mgr_user_id for schema 3.50.
**
** Modified:  10/14/96  J. Ting  Took out quicklook_p for schema 3.50.
**
****************************************************************************/
static void create_search_account_query ( ) {
  
  OP_ACC_USR_DATA *acc_usr_ptr ;
  OP_QUERY_STRUCT *sql ;
  OP_QUERY_STRUCT *accmgrsql ;
  char * sqlPtr ;
  char * accmgrsqlPtr ;
  int cnt, temp,i ;
	char text[IMS_COL512_LEN+1];

  acc_usr_ptr = & (glbData.accounts_users_data ) ;
  sql = &(acc_usr_ptr->account_data.search_queryStruct) ;
  sql->select[0]= sql->from[0]= sql->where[0]= sql->sqlBuf[0] = '\0' ;
  sql->sPtr = sql->select ;
  sql->fPtr = sql->from ;
  sql->wPtr = sql->where ;
  sqlPtr = sql->sqlBuf ;

/*********************************************************************
  accmgrsql = &(acc_usr_ptr->account_data.search_mgr_queryStruct) ;
  accmgrsql->select[0]= accmgrsql->from[0]= accmgrsql->where[0]= 
    accmgrsql->sqlBuf[0] = '\0';
  accmgrsql->sPtr = accmgrsql->select ;
  accmgrsql->fPtr = accmgrsql->from ;
  accmgrsql->wPtr = accmgrsql->where ;
  accmgrsqlPtr = accmgrsql->sqlBuf ;
*********************************************************************/

  strcpy (sql->sPtr,"t1.account_id, t1.account_type, t1.resource_type,\n"
      "t1.begin_balance, t1.curr_balance, t1.hold_balance,\n"
      "convert (char(12),t1.create_time,111),\n"
      "convert (char(12),t1.create_time,108),\n"
      "convert (char(12),t1.expire_time,111),\n"
      "convert (char(12),t1.expire_time,108),\n"
      "t1.rate_multiplier, t1.op_validate_p, \n"
      "t1.account_proc, t1.op_comment, t1.mgr_user_id\n" ) ;
  sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
  
  strcpy (sql->fPtr, "account t1 ") ;
  sql->fPtr = sql->fPtr + strlen ( sql->fPtr ) ;

	/*
	** 04/24/96  Added if ( cnt ) check for using 'and' in 
	** the where clause.
	*/
  cnt = 0 ; /* used to count the 'and' in sql */
  if ( id && *id ) {
    sprintf( sql->wPtr, "t1.account_id = '%s'", id ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( id ) ;
    id = (char *) NULL ;*/
    cnt++ ;
  }

  temp = 0 ;
  if ( type && *type ) {
    for ( i = 0 ; i < glbData.account_type_count ; i++ )
      if ( !strcmp (glbData.account_type[i].item_name,type) )
        temp = glbData.account_type[i].item_id ;

    if ( cnt )
    	sprintf( sql->wPtr, " and t1.account_type = %d", temp ) ;
    else
    	sprintf( sql->wPtr, "t1.account_type = %d", temp ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  temp = 0 ;
  if ( resource && *resource ) {
    for ( i = 0 ; i < glbData.resource_type_count ; i++ )
      if ( !strcmp (glbData.resource_type[i].item_name,resource) )
        temp = glbData.resource_type[i].item_id ;

    if ( cnt )
      sprintf( sql->wPtr, " and t1.resource_type = %d", temp ) ;
		else
      sprintf( sql->wPtr, "t1.resource_type = %d", temp ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( start_creation && *start_creation ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.create_time >= '%s'", start_creation ) ;
		else
      sprintf( sql->wPtr, "t1.create_time >= '%s'", start_creation ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( end_creation && *end_creation ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.create_time <= '%s'", end_creation ) ;
		else
      sprintf( sql->wPtr, "t1.create_time <= '%s'", end_creation ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( start_expiration && *start_expiration ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.expire_time >= '%s'",start_expiration) ;
		else
      sprintf( sql->wPtr, "t1.expire_time >= '%s'",start_expiration) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( end_expiration && *end_expiration ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.expire_time <= '%s'", end_expiration ) ;
		else  
      sprintf( sql->wPtr, "t1.expire_time <= '%s'", end_expiration ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( start_balance && *start_balance ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.curr_balance >= %s", start_balance ) ;
		else
      sprintf( sql->wPtr, "t1.curr_balance >= %s", start_balance ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( end_balance && *end_balance ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.curr_balance <= %s", end_balance ) ;
		else
      sprintf( sql->wPtr, "t1.curr_balance <= %s", end_balance ) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }

  if ( comments && *comments ) {
		(void) ims_formatQuotedString (comments, text);
		if ( cnt )
      sprintf( sql->wPtr, " and t1.op_comment = '%s'", text) ;
		else
      sprintf( sql->wPtr, "t1.op_comment = '%s'", text) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( ftp_dir ) ;
    ftp_dir = (char *) NULL ;*/
    cnt++ ;
  }

  if ( mgr_user_id && *mgr_user_id ) {
		if ( cnt )
      sprintf( sql->wPtr, " and t1.mgr_user_id = '%s'", mgr_user_id) ;
		else
      sprintf( sql->wPtr, "t1.mgr_user_id = '%s'", mgr_user_id) ;

    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    cnt++ ;
  }


  if ( cnt )
  {
    sprintf ( sqlPtr, " select %s from %s where %s order by t1.account_id",
	                sql->select, sql->from, sql->where ) ;
  }
  else
  {
    sprintf ( sqlPtr, " select %s from %s order by t1.account_id",
	                sql->select, sql->from ) ;
  }

/*
printf ("%s\n", sqlPtr ) ;
*/


  /*
   * set up query structure for account_mgr query.
   */
/*************************************************************************
  strcpy (accmgrsql->sPtr,"t2.first_name, t2.initial_name,\n"
      "t2.last_name, t2.title, t2.organization, t2.street, t2.city,\n"
      "t2.state, t2.country, t2.zipcode, t2.phone, t2.fax, t2.email\n" ) ;
  accmgrsql->sPtr = accmgrsql->sPtr + strlen(accmgrsql->sPtr) ;

  strcpy (accmgrsql->fPtr, "account_mgr t2 ") ;
  accmgrsql->fPtr = accmgrsql->fPtr + strlen ( accmgrsql->fPtr ) ;

	** 
	** 04/24/96 - Added if (cnt) check for 'and' 
	**
  cnt = 0 ; ** used to count the 'and' in sql **
  if ( first_name && *first_name ) {
      sprintf( accmgrsql->wPtr, "t2.first_name = '%s'", first_name ) ;
    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( first_name ) ;
    first_name = (char *) NULL ;**
    cnt++ ;
  }

  if ( m_i && *m_i ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.initial_name = '%s'", m_i ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.initial_name = '%s'", m_i ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( m_i ) ;
    m_i = (char *) NULL ;**
    cnt++ ;
  }

  if ( last_name && *last_name ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.last_name = '%s'", last_name ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.last_name = '%s'", last_name ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( last_name ) ;
    last_name = (char *) NULL ;**
    cnt++ ;
  }

  if ( zip && *zip ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.zipcode = '%s'", zip ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.zipcode = '%s'", zip ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( zip ) ;
    zip = (char *) NULL ;**
    cnt++ ;
  }

  if ( phone && *phone ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.phone = '%s'", phone ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.phone = '%s'", phone ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( phone ) ;
    phone = (char *) NULL ;**
    cnt++ ;
  }

  if ( fax && *fax ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.fax = '%s'", fax ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.fax = '%s'", fax ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( fax ) ;
    fax = (char *) NULL ;**
    cnt++ ;
  }

  if ( organization && *organization ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.organization = '%s'", organization ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.organization = '%s'", organization ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( organization ) ;
    organization = (char *) NULL ;**
    cnt++ ;
  }

  if ( city && *city ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.city = '%s'", city ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.city = '%s'", city ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( city ) ;
    city = (char *) NULL ;**
    cnt++ ;
  }

  if ( state && *state ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.state = '%s'", state ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.state = '%s'", state ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( state ) ;
    state = (char *) NULL ;**
    cnt++ ;
  }

  if ( country && *country ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.country = '%s'", country ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.country = '%s'", country ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( country ) ;
    country = (char *) NULL ;**
    cnt++ ;
  }

  if ( email && *email ) {
		if ( cnt )
      sprintf( accmgrsql->wPtr, " and t2.email = '%s'", email ) ;
		else
      sprintf( accmgrsql->wPtr, "t2.email = '%s'", email ) ;

    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
    **XtFree ( email ) ;
    email = (char *) NULL ;**
    cnt++ ;
  }

  if ( cnt )
  {
		**
		** 04/24/96 - set account_mgr_search_flag to one to indicate 
		** that account manager search criteria is specified 
		**
		account_mgr_search_flag = 1;

    sprintf( accmgrsql->wPtr, " and t2.account_id =" ) ;
    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
  }
  else
  {
		**
		** 04/24/96 - set account_mgr_search_flag to zero to indicate 
		** that no account manager search criteria is specified 
		**
		account_mgr_search_flag = 0;

    sprintf( accmgrsql->wPtr, "t2.account_id =" ) ;
    accmgrsql->wPtr = accmgrsql->wPtr + strlen( accmgrsql->wPtr ) ;
  }
  sprintf ( accmgrsqlPtr, " select %s from %s where %s",
	                    accmgrsql->select, accmgrsql->from, accmgrsql->where ) ;

**
printf ("%s\n", accmgrsqlPtr ) ;
****************************************************************************/

}/* end of create_query*/


/***************************************************************************
**
** Function Name :	execute_search_account_query
**
** Description:		Executes the query formed by create_query by calling
**			ims_op_accCat
**
** Arguments:		none
**
** Return Value:	none
**
** Modified: 04/25/96  Corrected search account query problem.
** 					 10/07/96  Took out account manager information.
**
****************************************************************************/
int execute_search_account_query ( ) {
	
  OP_CAT_STRUCT *catReq;
  int status;
  int account_count;
  char Msg[IMS_COL1024_LEN+1];
  OP_ACCOUNT *aPtr1, *aPtr2, *aPtr3;
	int aCount;

  catReq = &(glbData.accounts_users_data.catReq);
  catReq->item[0] = ( int * ) &account_count ;
  catReq->item[1] = ( char *)                       
        &(glbData.accounts_users_data.account_data.search_queryStruct.sqlBuf);

/**************************************************************************
  catReq->item[2] = ( char *)                       
        &(glbData.accounts_users_data.account_data.search_mgr_queryStruct.sqlBuf);
**************************************************************************/

  if ((status = ims_op_accCat (catReq, OP_ACC_GETACCOUNTLIST)) < IMS_OK) {   
    /* Display error messages */
    sprintf(Msg, "Couldn't get accounts");   
    msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 
			return (IMS_FATAL);
  }
  else {
   /* assign returned users and deference qDesc->cmd from sqlBuf */
   catReq->qDesc->cmd = NULL ;   
   glbData.accounts_users_data.account_data.accounts_cnt =
                                               *(int *)catReq->item[0];
   glbData.accounts_users_data.account_data.op_account_ptr  = 
                             (OP_ACCOUNT *)catReq->item[3];

	 /*
	 ** 04/25/96 - Corrected Search Account Query problem:
   ** If account manager search parameter was specified in the Search
	 ** Account Screen, remove the account from the account list if the 
	 ** account does not have manager information returned from the query.
	 **
   if (( glbData.accounts_users_data.account_data.accounts_cnt > 0 ) &&
			 ( account_mgr_search_flag ))
	 {
    aPtr1 = glbData.accounts_users_data.account_data.op_account_ptr; 

    while (aPtr1 != (OP_ACCOUNT *) NULL) 
		{
			if ((aPtr1->first_name[0] != '\0') && (aPtr1->last_name[0] != '\0')) 
			{
				aPtr1 = aPtr1->next;
			}
			else
			{
				** delete this entry **
				aPtr2 = aPtr1->prev;
				aPtr3 = aPtr1->next;

				if ((aPtr2 != (OP_ACCOUNT *)NULL) && (aPtr3 != (OP_ACCOUNT *)NULL))
				{
					** item in between two items **
					aPtr2->next = aPtr3;
					aPtr3->prev = aPtr2;
					free (aPtr1);
					aPtr1 = aPtr3;
				}
				else if ((aPtr2 == (OP_ACCOUNT *)NULL) && (aPtr3 == (OP_ACCOUNT *)NULL))
				{
					** the only item in the list ** 
					free (aPtr1);
					aPtr1 = (OP_ACCOUNT *)NULL;
					glbData.accounts_users_data.account_data.op_account_ptr = NULL;
					glbData.accounts_users_data.account_data.accounts_cnt = 0;
				}
				else if ((aPtr2 != (OP_ACCOUNT *)NULL))
				{
					** last item in the list **
					free (aPtr1);
					aPtr1 = (OP_ACCOUNT *)NULL;
					aPtr2->next = (OP_ACCOUNT *)NULL;
				}
				else
				{
					** first item in the list **
					aPtr3->prev = (OP_ACCOUNT *)NULL;
					free (aPtr1);
					aPtr1 = aPtr3;
					glbData.accounts_users_data.account_data.op_account_ptr = aPtr1;
				}
			}
		} ** while **
	
    aPtr1 = glbData.accounts_users_data.account_data.op_account_ptr ;
		aCount = 0;
		while (aPtr1 != (OP_ACCOUNT *)NULL)
		{
			aPtr1->position = aCount++;
			aPtr1 = aPtr1->next;
		}
		glbData.accounts_users_data.account_data.accounts_cnt = aCount;
	
	 } ** if account_mgr_search_flag **

  ************************************************************************/
  }/* end if */
  return (IMS_OK);

}/* end of execute_query */

void search_accounts_check_dateCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
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
			if ((cbs->startPos == 4) || (cbs->startPos == 7))
				cbs->startPos--;
			return;
		}
		if (cbs->text->length >1) /* don't allow clipboard copies */
		{
			cbs->doit = False;
			return;
		}

		/* don't allow non-digits or let the input exceed 10 chars */
		if (!isdigit(c = cbs->text->ptr[0]) || len >= 10)
			cbs->doit = False;
		else
			if (len == 3 || len == 6)
			{
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c ;
				cbs->text->ptr[1] = '-';
			 }


	}
	UxSearch_accountsContext = UxSaveCtx;
}


int is_day_valid(
	int day, 
	int month, 
	int year)
{
	
	static int day_table[2][13] = {
		{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
		{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} };
	int i, leap;

	leap = year%4 == 0 && year%100 != 0 || year%400 == 0;

	if ((day > 0) && (day <= day_table[leap][month]))
		return (IMS_OK);
	else
		return (IMS_ERROR);

}
int is_date_valid(
	char  *fieldValue)
{
	char year[5], month[3], day[3];
	char value[IMS_COL15_LEN+1];
	int  yeari, monthi, dayi;
	int len;

	strcpy (value, fieldValue);
	ims_truncStr (value);
	if (!*value)
		return (IMS_OK);

	month[0] = value[5];
	month[1] = value[6];
	month[2] = '\0';

	day[0] = value[8];
	day[1] = value[9];
	day[2] = '\0';

	year[0] = value[0];
	year[1] = value[1];
	year[2] = value[2];
	year[3] = value[3];
	year[4] = '\0';


	if ((len = strlen(value)) < 10 || !ims_isInteger(year) ||
			!ims_isInteger(month) || !ims_isInteger(day))
	{
		return (IMS_ERROR);
	}

	yeari = atoi(year);
	monthi = atoi(month);
	dayi = atoi(day);

	if (yeari < LOW_YEAR  ||
			yeari > HIGH_YEAR ||
			monthi < 1           ||
			monthi > 12          ||
			dayi < 1             ||
			dayi > 31            ||
			(is_day_valid(dayi, monthi, yeari) < IMS_OK))

	{
		return (IMS_ERROR);
	}

	return (IMS_OK);

}
void search_accounts_check_balanceCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
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
		/*if (cbs->currInsert < len)
		{
			cbs->doit = False;
			return;
		}*/

		if (cbs->text->length == 0)  /*backspace*/ 
		{
					return;
		}
		/*if (cbs->text->length >1) don't allow clipboard copies 
		{
			cbs->doit = False;
			return;
		}
                */
		/* don't allow non-digits, except '.','-' */
                if ( ( cbs->text->ptr[0] != '.' )&&( cbs->text->ptr[0] != '-' ))

		  if (!isdigit(c = cbs->text->ptr[0]) ) 
                    cbs->doit = False;
        }
	UxSearch_accountsContext = UxSaveCtx;
}


void template7( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCsearch_accounts      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_accountsContext;
	UxSearch_accountsContext = UxContext =
			(_UxCsearch_accounts *) UxGetContext( UxWidget ) ;
	{

	}
	UxSearch_accountsContext = UxSaveCtx;


}
