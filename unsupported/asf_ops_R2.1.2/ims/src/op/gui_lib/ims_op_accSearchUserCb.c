static char *sccs = "@(#)ims_op_accSearchUserCb.c	5.4  11/22/96";
/*******************************************************************************
**
** File:		ims_op_accSearchUserCb.c
**
** Function:		Callback functions for the search_users screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
** Modified:
**
**   11/13/95   A. Tilden   Corrected file name.
**                          Modified to pop down search screen after
**                          account query is complete.
**
**   04/24/96   J. Ting     Modified function search_users_start_searchCb.
**													Change timeout cursors back to normal cursors
**                          if the query execution fails.
**
**   06-12-96  J. Ting      Modified function search_users_closeCb for
**                          PR 942.
**
**   11-05-96  J. Ting      R2.1 - modified all local_dir to ftp_dir
**
**   11-11-96  J. Ting      R2.1 - added access_dir.
**
******************************************************************************/#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>
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
extern accounts_users_users_sbCb ( Widget, XtPointer , XtPointer ) ;
extern OP_GLOBAL_DATA glbData;
extern Widget msg_dlg ;

static char * id = (char*) NULL ;
static char * first_name = (char*) NULL ;
static char * m_i = (char*) NULL ;
static char * last_name = (char*) NULL ;
static char * type = (char*) NULL ;
static char * title = (char*) NULL ;
static char * organization = (char*) NULL ;
static char * city = (char*) NULL ;
static char * state = (char*) NULL ;
static char * country = (char*) NULL ;
static char * zip = (char*) NULL ;
static char * phone = (char*) NULL ;
static char * fax = (char*) NULL ;
static char * email = (char*) NULL ;
static char * ftp_dir = (char*) NULL ;
static char * access_dir = (char*) NULL ;

void free_user_data ( ) ;
static void create_search_user_query ( ) ;
int execute_search_user_query ( ) ;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accSearchUser.h"
#undef CONTEXT_MACRO_ACCESS

void search_users_option_typeCb	 (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_users        *UxSaveCtx, *UxContext;
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
     
	UxSaveCtx = UxSearch_usersContext;
        UxSearch_usersContext = UxContext = 
                          (_UxCsearch_users *) UxGetContext( UxWidget );
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
            for ( i = 0 ; i < glbData.user_type_count ; i++ ) {
			label = XmStringCreateLocalized  
                (glbData.user_type[i].item_name);
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
			(XtCallbackProc) search_users_option_typeCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxSearch_usersContext );

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
	UxSearch_usersContext = UxSaveCtx;
}

/*===========================================================================*
**
** Function Name:   search_users_closeCb
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
**     06/12/96  J. Ting  Modified to use glbData.search_usersW to get
**                        the context instead of using UxWidget. This
**                        is done for mwm quit button to work - PR 942.
**
**     11/11/96  J. Ting  R2.1 - Modified local_dir to ftp_dir.
**                        R2.1 - Added access_dir.
**==========================================================================*/
void	search_users_closeCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_usersContext;
	UxSearch_usersContext = UxContext = 
          (_UxCsearch_users *) UxGetContext( glbData.search_usersW );

	{
           /* free strings  and clear text fields */
              if ( id != NULL ) {
                XtFree ( id ) ;
                id = (char *) NULL ;
                XmTextSetString ( idTF, "" ) ;
              }
              if ( first_name != NULL ) {
                XtFree ( first_name ) ;
		first_name = (char *) NULL ;
		XmTextSetString ( first_nameTF, "" ) ;
              }
              if ( m_i != NULL ) {
                XtFree ( m_i ) ;
		m_i = (char *) NULL ;
		XmTextSetString ( m_iTF, "" ) ;
              }	      
              if ( last_name != NULL ) {
                XtFree ( last_name ) ;
		last_name = (char *) NULL ;
		XmTextSetString ( last_nameTF, "" ) ;
              }
              if ( type != NULL ) {
                XtFree ( type ) ;
		type = (char *) NULL ;
		XmTextSetString ( typeTF, "" ) ;
              }
              if ( title != NULL ) {
                XtFree ( title ) ;
		title = (char *) NULL ;
		XmTextSetString ( titleTF, "" ) ;
              }
              if ( organization != NULL ) {
                XtFree ( organization ) ;
		organization = (char *) NULL ;
		XmTextSetString ( organizationTF, "" ) ;
              }
              if ( city != NULL ) {
                XtFree ( city ) ;
		city = (char *) NULL ;
		XmTextSetString ( cityTF, "" ) ;
              }
              if ( state != NULL ) {
                XtFree ( state ) ;
		state = (char *) NULL ;
		XmTextSetString ( stateTF, "" ) ;
              }
              if ( country != NULL ) {
                XtFree ( country ) ;
		country = (char *) NULL ;
		XmTextSetString ( countryTF, "" ) ;
              }
              if ( zip != NULL ) {
                XtFree ( zip ) ;
		zip = (char *) NULL ;
		XmTextSetString ( zipTF, "" ) ;
              }
              if ( phone != NULL ) {
                XtFree ( phone ) ;
		phone = (char *) NULL ;
		XmTextSetString ( phoneTF, "" ) ;
              }
              if ( fax != NULL ) {
                XtFree ( fax ) ;
		fax = (char *) NULL ;
		XmTextSetString ( faxTF, "" ) ;
              }
              if ( email != NULL ) {
                XtFree ( email ) ;
		email = (char *) NULL ;
		XmTextSetString ( emailTF, "" ) ;
              }
              if ( ftp_dir != NULL ) {
                XtFree ( ftp_dir ) ;
		ftp_dir = (char *) NULL ;
		XmTextSetString ( ftp_dirST, "" ) ;
              }
              if ( access_dir != NULL ) {
                XtFree ( access_dir ) ;
		access_dir = (char *) NULL ;
		XmTextSetString ( access_dirST, "" ) ;
              }
	
              XtPopdown(XtParent(glbData.search_usersW));  
              glbData.search_usersFlag = 0 ; /* screen has been pop down */

	}
	UxSearch_usersContext = UxSaveCtx;
}

void	search_users_start_searchCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_usersContext;
	UxSearch_usersContext = UxContext = 
                          (_UxCsearch_users *) UxGetContext( UxWidget );

	{
        XmScrollBarCallbackStruct *cbs ;
        Widget sb ;

	  	/* Change cursor to watch cursor */
	  	timeOutCursors (True);
 
        free_user_data ( ) ;
        create_search_user_query ( ) ;
        if ( execute_search_user_query ( ) < IMS_OK ) {
	  				/* Change cursor to watch cursor */
	  				timeOutCursors (False);
            return ;
        }

		/* Update users list                                               */
        /*XtVaGetValues ( users_sbSW, XmNverticalScrollBar, &sb, NULL ) ;*/
        cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
        cbs->value = 0 ; 
        accounts_users_users_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
        XtFree ((char *)cbs) ;

	  	/* Change cursor back to normal */
	  	timeOutCursors (False);

	}
	UxSearch_usersContext = UxSaveCtx;

	(void) search_users_closeCb (wgt, cd, cb) ;
}

void	search_users_losing_focusCb (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int widget_num = (int) cd ;

	UxSaveCtx = UxSearch_usersContext;
	UxSearch_usersContext = UxContext = 
                          (_UxCsearch_users *) UxGetContext( UxWidget );

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
              if ( first_name != NULL ) {
                XtFree ( first_name ) ;
              }
	      first_name = XmTextGetString ( first_nameTF ) ; 
	      ims_truncStr(first_name) ;
              break ;
            case 3 :
              if ( m_i != NULL ) {
                XtFree ( m_i ) ;

              }	      
	      m_i = XmTextGetString ( m_iTF ) ; 
	      ims_truncStr(m_i) ;
              break ;
            case 4 :
              if ( last_name != NULL ) {
                XtFree ( last_name ) ;
              }
	      last_name = XmTextGetString ( last_nameTF ) ; 
	      ims_truncStr(last_name) ;
              break ;
            case 5 :
              if ( type != NULL ) {
                XtFree ( type ) ;

              }
	      type = XmTextGetString ( typeTF ) ;
	      ims_truncStr(type) ; 
              break ;
            case 6 :
              if ( title != NULL ) {
                XtFree ( title ) ;

              }
	      title = XmTextGetString ( titleTF ) ; 
	      ims_truncStr(title) ;
              break ;
            case 7 :
              if ( organization != NULL ) {
                XtFree ( organization ) ;
              }
	      organization = XmTextGetString ( organizationTF ) ; 
	      ims_truncStr(organization) ;
              break ;
            case 8 :
              if ( city != NULL ) {
                XtFree ( city ) ;
              }
	      city = XmTextGetString ( cityTF ) ;
	      ims_truncStr(city) ;
              break ;
            case 9 :
              if ( state != NULL ) {
                XtFree ( state ) ;
              }
	      state = XmTextGetString ( stateTF ) ; 
	      ims_truncStr(state) ;
              break ;
            case 10 :
              if ( country != NULL ) {
                XtFree ( country ) ;
              }
	      country = XmTextGetString ( countryTF ) ;
	      ims_truncStr(country) ; 
              break ;
            case 11 :
              if ( zip != NULL ) {
                XtFree ( zip ) ;
              }
	      zip = XmTextGetString ( zipTF ) ; 
	      ims_truncStr(zip) ;
              break ;
            case 12 :
              if ( phone != NULL ) {
                XtFree ( phone ) ;
              }
	      phone = XmTextGetString ( phoneTF ) ;
	      ims_truncStr(phone) ; 
              break ;
            case 13 :
              if ( fax != NULL ) {
                XtFree ( fax ) ;
              }
	      fax = XmTextGetString ( faxTF ) ; 
	      ims_truncStr(fax) ;
              break ;
            case 14 :
              if ( email != NULL ) {
                XtFree ( email ) ;
              }
	      email = XmTextGetString ( emailTF ) ; 
	      ims_truncStr(email) ;
              break ;
            case 15 :
              if ( ftp_dir != NULL ) {
                XtFree ( ftp_dir ) ;
              }
	      ftp_dir = XmTextGetString ( ftp_dirST ) ;
	      ims_truncStr(ftp_dir) ;
              break ;
            case 16 :
              if ( access_dir != NULL ) {
                XtFree ( access_dir ) ;
              }
	      access_dir = XmTextGetString ( access_dirST ) ;
	      ims_truncStr(access_dir) ;
              break ;

            default :
              break ;

          }
	}
	UxSearch_usersContext = UxSaveCtx;
}

void	template8 (
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_users        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_usersContext;
	UxSearch_usersContext = UxContext = 
                          (_UxCsearch_users *) UxGetContext( UxWidget );

	{
	}
	UxSearch_usersContext = UxSaveCtx;
}
/***************************************************************************
**
** Function Name :	create_search_user_query
**
** Description:		Initializes glbData.accounts_users_data.queryStruct.
**       			    sqlBuf with query to be executed.
**
** Arguments:		void
**
** Return Value:	none
**
** Revision:      11-11-96  R2.1 - Modified local_dir to ftp_dir
**                          R2.1 - Added access_dir
****************************************************************************/
static void create_search_user_query ( ) {
  
  OP_ACC_USR_DATA *acc_usr_ptr ;
  OP_QUERY_STRUCT *sql ;
  char * sqlPtr ;
  int cnt, temp, i ;

  acc_usr_ptr = & (glbData.accounts_users_data ) ;
  sql = &(acc_usr_ptr->user_data.search_queryStruct) ;
  sql->select[0]= sql->from[0]= sql->where[0]= sql->sqlBuf[0] = '\0' ;
  sql->sPtr = sql->select ;
  sql->fPtr = sql->from ;
  sql->wPtr = sql->where ;
  sqlPtr = sql->sqlBuf ;

  strcpy (sql->sPtr,
	"t1.user_id, t1.user_type, t1.auth_key, t1.priority,\n"
	"t1.first_name, t1.initial_name, t1.last_name, t1.title,\n"
	"t1.zipcode, t1.phone, t1.fax, t1.organization, t1.street,\n"
	"t1.city, t1.state, t1.country, t1.email, t1.ftp_dir, t1.access_dir\n" ) ;
  sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
  
  strcpy (sql->fPtr, "user_profile t1 ") ;
  sql->fPtr = sql->fPtr + strlen ( sql->fPtr ) ;
  cnt = 0 ; /* used to count the 'and' in sql */
  if ( id && *id ) {
    sprintf( sql->wPtr, "t1.user_id = '%s'", id ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( id ) ;
    id = (char *) NULL ;*/
    cnt++ ;
  }

  temp = 0 ;
  if ( type && *type ) {
    for ( i = 0 ; i < glbData.user_type_count ; i++ )
       if ( !strcmp (glbData.user_type[i].item_name,type) )
          temp = glbData.user_type[i].item_id ;
    if ( cnt )
      sprintf( sql->wPtr, " and t1.user_type = %d", temp ) ;
    else
      sprintf( sql->wPtr, "t1.user_type = %d", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( type ) ;
    type = (char *) NULL ;*/
    cnt++ ;
  }
  if ( first_name && *first_name ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.first_name = '%s'", first_name ) ;
    else
      sprintf( sql->wPtr, "t1.first_name = '%s'", first_name ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( first_name ) ;
    first_name = (char *) NULL ;*/
    cnt++ ;
  }
  if ( m_i && *m_i ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.initial_name = '%s'", m_i ) ;
    else
      sprintf( sql->wPtr, "t1.initial_name = '%s'", m_i ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( m_i ) ;
    m_i = (char *) NULL ;*/
    cnt++ ;
  }
  if ( last_name && *last_name ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.last_name = '%s'", last_name ) ;
    else
      sprintf( sql->wPtr, "t1.last_name = '%s'", last_name ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( last_name ) ;
    last_name = (char *) NULL ;*/
    cnt++ ;
  }
  if ( title && *title ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.title = '%s'", title ) ;
    else
      sprintf( sql->wPtr, "t1.title = '%s'", title ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*title = (char *) NULL ;
    XtFree ( title ) ;*/
    cnt++ ;
  }
  if ( zip && *zip ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.zipcode = '%s'", zip ) ;
    else
      sprintf( sql->wPtr, "t1.zipcode = '%s'", zip ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( zip ) ;
    zip = (char *) NULL ;*/
    cnt++ ;
  }
  if ( phone && *phone ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.phone = '%s'", phone ) ;
    else
      sprintf( sql->wPtr, "t1.phone = '%s'", phone ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( phone ) ;
    phone = (char *) NULL ;*/
    cnt++ ;
  }
  if ( fax && *fax ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.fax = '%s'", fax ) ;
    else
      sprintf( sql->wPtr, "t1.fax = '%s'", fax ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( fax ) ;
    fax = (char *) NULL ;*/
    cnt++ ;
  }
  if ( organization && *organization ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.organization = '%s'", organization ) ;
    else
      sprintf( sql->wPtr, "t1.organization = '%s'", organization ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( organization ) ;
    organization = (char *) NULL ;*/
    cnt++ ;
  }
  if ( city && *city ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.city = '%s'", city ) ;
    else
      sprintf( sql->wPtr, "t1.city = '%s'", city ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( city ) ;
    city = (char *) NULL ;*/
    cnt++ ;
  }
  if ( state && *state ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.state = '%s'", state ) ;
    else
      sprintf( sql->wPtr, "t1.state = '%s'", state ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( state ) ;
    state = (char *) NULL ;*/
    cnt++ ;
  }
  if ( country && *country ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.country = '%s'", country ) ;
    else
      sprintf( sql->wPtr, "t1.country = '%s'", country ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( country ) ;
    country = (char *) NULL ;*/
    cnt++ ;
  }
  if ( email && *email ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.email = '%s'", email ) ;
    else
      sprintf( sql->wPtr, "t1.email = '%s'", email ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( email ) ;
    email = (char *) NULL ;*/
    cnt++ ;
  }
  if ( ftp_dir && *ftp_dir ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.ftp_dir = '%s'", ftp_dir ) ;
    else
      sprintf( sql->wPtr, "t1.ftp_dir = '%s'", ftp_dir ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( ftp_dir ) ;
    ftp_dir = (char *) NULL ;*/
    cnt++ ;
  }

  if ( access_dir && *access_dir ) {
    if ( cnt )
      sprintf( sql->wPtr, " and t1.access_dir = '%s'", access_dir ) ;
    else
      sprintf( sql->wPtr, "t1.access_dir = '%s'", access_dir ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    /*XtFree ( access_dir ) ;
    access_dir = (char *) NULL ;*/
    cnt++ ;
  }


  if ( cnt ){
    sprintf ( sqlPtr, " select %s from %s where %s order by t1.user_id",
                      sql->select, sql->from, sql->where ) ;
    /*printf (" select %s from %s where %s order by t1.user_id",
                      sql->select, sql->from, sql->where ) ;*/
  }
  else{
    sprintf ( sqlPtr, " select %s from %s  order by t1.user_id",
                      sql->select, sql->from ) ;

    /*printf ( " select %s from %s  order by t1.user_id",
                      sql->select, sql->from ) ;*/

  }
}/* end of create_query*/

/***************************************************************************
**
** Function Name :	execute_search_user_query
**
** Description:		Executes the query formed by create_query by calling
**			ims_op_accCat
**
** Arguments:		none
**
** Return Value:	none
**
****************************************************************************/
int execute_search_user_query ( ) {
	
  OP_CAT_STRUCT *catReq;
  int status;
  int user_count;
  char Msg[IMS_COL1024_LEN+1];

  catReq = &(glbData.accounts_users_data.catReq);
  catReq->item[0] = ( int * ) &user_count ;
  catReq->item[1] = ( char *)
         &(glbData.accounts_users_data.user_data.search_queryStruct.sqlBuf);
  if ((status = ims_op_accCat (catReq, OP_ACC_GETUSERLIST)) < IMS_OK) {   
    /* Display error messages */
    sprintf(Msg, "Couldn't get users");   
    msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 
			return (IMS_FATAL);
  }
  else {
   /* assign returned users and deference qDesc->cmd from sqlBuf */
   catReq->qDesc->cmd = NULL ;   
   glbData.accounts_users_data.user_data.users_cnt = *(int *)catReq->item[0];
   glbData.accounts_users_data.user_data.op_user_profile_ptr  = 
                             (OP_USER_PROFILE *)catReq->item[2];

  }

  return (IMS_OK);

}/* end of execute_query */

void free_user_data ( ) {
  
  OP_USER_PROFILE *curr_ptr, *last_ptr ;

  if ( glbData.accounts_users_data.user_data.users_cnt > 0 ) {
    glbData.accounts_users_data.user_data.users_cnt = 0 ;
    curr_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr ;
    while ( curr_ptr != (OP_USER_PROFILE*) NULL ) {
       last_ptr = curr_ptr->next ;
       free (curr_ptr) ;
       curr_ptr = last_ptr ;
    }
  }/* end if */
}/* end free_user_data */
