static char *sccs = "@(#)ims_op_accUserDataCb.c	5.4  11/22/96";
/*******************************************************************************
**
** File:		ims_op_accUserDataCb.c
**
** Function:		Callback functions for the user_data screen
**
** Author:		J. Armando Cardona
**
** Date:		May 1995
**
** Modified:
**
**   11/08/95   A. Tilden   Corrected file name.
**                          Removed local IK_MakeAuthenticator function and
**                          modified to use the same function from the public
**                          IK library in order to create the encrypted 
**                          authenticator. 
**			    			Added column names to insert statement that
**                          creates user_profile data.
**                          Modified the error message displayed in case of
**                          create/update failure to specify missing fields.
**
**   06/12/96   J. Ting     Modified user_data_closeCb to correct PR 942.
**
**   07/08/96   J. Ting     Modified user_data_createCb to check for addtional
**                          fields before creating the user.
**
**   07/11/96   J. Ting     Modified update error message due to new primary 
**                          unique keys created on first_name, last_name,
**                          and password.
**
**   09/06/96   J. Ting     Added apostrophe support for address field
**                          as per PR 84.  Modified functions get_fields_data
**                          and create_user_data_query.
**
**   09/16/96   J. Ting     Modified function set_user_data for PR 43.  Allow
**                          the operator to update the user type and priority
**                          after the user is created.
**
**   11/05/96   J. Ting     R2.1 - modified all local_dir to ftp_dir
**
**   11/11/96   J. Ting     R2.1 - added access_dir.
******************************************************************************/
/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE
 
#include <stdio.h>
#include <time.h>
#include <IK_Network.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

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

extern void msg_dlgCb( Widget , int , char *) ;
extern void prepare_message_box (SELECTED_BUTTON ** , char *, char *  ) ;
extern void free_user_data ( ) ;
extern int execute_search_user_query ( ) ;
extern OP_GLOBAL_DATA glbData;
extern Widget msg_dlg ;


static char * id = (char*) NULL ;
static char * first_name = (char*) NULL ;
static char * m_i = (char*) NULL ;
static char * last_name = (char*) NULL ;
static char * type = (char*) NULL ;
static char * priority = (char*) NULL ;
static char * password = (char*) NULL ;
static char * title = (char*) NULL ;
static char * organization = (char*) NULL ;
static char * address = (char*) NULL ;
static char * city = (char*) NULL ;
static char * state = (char*) NULL ;
static char * country = (char*) NULL ;
static char * zip = (char*) NULL ;
static char * phone = (char*) NULL ;
static char * fax = (char*) NULL ;
static char * email = (char*) NULL ;
static char * ftp_dir = (char*) NULL ;
static char * access_dir = (char*) NULL ;

static void create_user_data_query (ACCOUNT_USER_DATA) ;
static int execute_user_data_query (ACCOUNT_USER_DATA) ;
static int get_fields_data (Widget) ;
static void free_string_data ( ) ;
void set_user_data ( Widget wgt ) ;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accUserData.h"
#undef CONTEXT_MACRO_ACCESS

/* Callback to set label of screen at creation time */
void user_data_labelCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
          /* create is a context defined variable and is passed to */
          /* to create_user_data.                                  */
          if ( create == CREATE ){
	   XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
	     "Create User", 12, NULL ) ;
	   XtVaSetValues ( XtParent(XtParent ( wgt )), XmNtitle, 
              "Create User", NULL ) ;
          }
          else {
	   XtVaSetValues ( wgt, XtVaTypedArg, XmNlabelString,XmRString,
	     "User Data", 10, NULL ) ;
	   XtVaSetValues ( XtParent ( wgt ), XmNtitle, 
             "User Data", NULL ) ;
          }


	}
	UxUser_dataContext = UxSaveCtx;


}
/* Create callback for idTF  */ 
void user_data_user_idCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int                UxClientData = (int) cd;
	
        
	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
             if ( UxClientData == 1 )  {
               if ( create == CREATE )
                   XtVaSetValues ( UxWidget, XmNeditable, True, NULL ) ;
               else
                   XtVaSetValues ( UxWidget, XmNeditable, False, NULL ) ;
               
             }
                             
               
	}
	UxUser_dataContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: user_data_closeCb
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
void user_data_closeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        static SELECTED_BUTTON selected  ;
        static SELECTED_BUTTON *selected_ptr  ;

	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
        /* prepare message box */
           /***selected = NOTHING ;
           selected_ptr = &selected ;
           prepare_message_box (&selected_ptr,
             "The user data has been modified.\nDo you really want to close?",
             "Closing User Data");
           
           while ( selected == NOTHING) {
                         XtAppProcessEvent ( UxAppContext, XtIMAll);
           }
           if ( selected == OK) {
            free_string_data ( ) ;
            XtDestroyWidget( XtParent ( XtParent ( wgt )  ) );
	   /XtPopdown(XtParent(XtParent(UxWidget)));/
           }
	   ***/
           free_string_data ( ) ;
					 /*
					 ** 6/96 - find the toplevel shell 
					 */
			  	 while(wgt && !XtIsShell(wgt))
    		    wgt = XtParent(wgt);

           XtDestroyWidget( wgt );
	  
	}
	UxUser_dataContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: user_data_createCb
**
** Description: Activate and create callback for the Create(Update) button.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 07/08/96 - check for first name, last name, type,
**                              priority, password, address, city, 
**                              country, phone, email.
**
**==========================================================================*/
void user_data_createCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int               UxClientData = (int) cd;
	XtPointer               UxCallbackArg = cb;
	int status ;
        XmScrollBarCallbackStruct *cbs ;
	char err_msg[IMS_COL128_LEN+1] ;

	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
/* The variable 'create' is passed as argument to 'create_user_data'.      */
         if ( UxClientData ) { /* Widget creation */
            if ( create == CREATE ){ /* create user */
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

	 /*
	 ** check fields that can not be NULL 
	 ** 07/08/96 - add the following checking: priority, password,
	 ** address, city, country, phone, email
   */
          if ( !( id && *id ) || !( type && *type ) || 
               !( first_name && *first_name ) || 
							 !( last_name && *last_name ) ||  
							 !( priority && *priority ) ||
							 !( password && *password ) ||
							 !( address && *address ) ||
							 !( city && *city ) ||
							 !( country && *country ) ||
							 !( phone && *phone ) ||
							 !( email && *email ))
		  {
			err_msg[0] = '\0';
			if ( !( id && *id ) )
				strcat (err_msg, "ID, " ) ;
			if ( !( type && *type ) )
				strcat (err_msg, "Type, " ) ;
			if ( !( first_name && *first_name ) )
				strcat (err_msg, "First_Name, " ) ;
			if ( !( last_name && *last_name) )
				strcat (err_msg, "Last_Name, " ) ;
			if ( !( priority && *priority) )
				strcat (err_msg, "Priority, " ) ;
			if ( !( password && *password ) )
				strcat (err_msg, "Password, " ) ;
			if ( !( address && *address ) )
				strcat (err_msg, "Address, " ) ;
			if ( !( city && *city ) )
				strcat (err_msg, "City, " ) ;
			if ( !( country && *country ) )
				strcat (err_msg, "Country, " ) ;
			if ( !( phone && *phone ) )
				strcat (err_msg, "Phone, " ) ;
			if ( !( email && *email ) )
				strcat (err_msg, "Email, " ) ;
			strcat ( err_msg, "the field(s) cannot be empty." ) ;

            msg_dlgCb (msg_dlg, IMS_ERROR, err_msg ) ;
            return ;
          } 
          if ( create == CREATE ) {
            create_user_data_query ( CREATE ) ;
            if ( (status = execute_user_data_query ( CREATE ) ) < IMS_OK )
              return ;
          }
          else { /* update */
            create_user_data_query ( DATA ) ;
            if ( (status = execute_user_data_query ( DATA ) ) < IMS_OK )
              return ;
          }
          /* update displayed user list */
          if ( glbData.accounts_users_data.user_data.users_cnt > 0 ) {
              free_user_data ( ) ;
              if (  execute_search_user_query ( ) < IMS_OK ) {
               return ;
              }
              cbs = ( XmScrollBarCallbackStruct * )
                          XtMalloc ( sizeof ( XmScrollBarCallbackStruct ) ) ;
              cbs->value = 0 ; 
              accounts_users_users_sbCb ((Widget)NULL , NULL, (XtPointer)cbs ) ;
              XtFree ((char *)cbs) ; 
          }
	}
	UxUser_dataContext = UxSaveCtx;

	(void) user_data_closeCb (wgt, cd, cb);
}


/* Callback for the priority option menu */
void user_data_option_priorityCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
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


	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
          XtVaGetValues ( UxWidget, XmNlabelString, &button_label,NULL ) ;

          if ( UxData) { /* creation */
            /* Get resources */
            XtVaGetValues ( UxWidget, 
		                 XmNfontList, &fontList,
				 XmNbackground, &background,
				 XmNmarginLeft, &marginLeft,
				 XmNmarginRight, &marginRight,
				 XmNmarginWidth, &marginWidth, NULL ) ;
            XtUnmanageChild (UxWidget ) ;
            for ( i = 0 ; i < glbData.priority_count ; i++ ) {
			label = XmStringCreateLocalized  
                (glbData.priority[i].item_name);
		menu_item = XtVaCreateManagedWidget 
                  ((char *)label, xmPushButtonWidgetClass, 
                  option_menu_pane_priority,
                  XmNlabelString, label,
		  XmNfontList, fontList,
		  XmNbackground, background,
		  XmNmarginLeft, marginLeft,
		  XmNmarginRight, marginRight,
                  XmNmarginWidth, marginWidth,	
		  NULL);

		  XtAddCallback (menu_item, XmNactivateCallback,
			(XtCallbackProc) user_data_option_priorityCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxUser_dataContext );

                XmStringFree (label);
                if ( i == 0 ) {
                  XtVaGetValues ( menu_item, XmNlabelString, &button_label,
                                                                     NULL ) ;
                  if ( XmStringGetLtoR ( button_label, 
                                 XmFONTLIST_DEFAULT_TAG, &text)) {
                     XmTextSetString ( priorityTF  , text ) ;
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
              XmTextSetString ( priorityTF, text ) ;
              XtFree ( text ) ;
            }
            XmStringFree ( button_label ) ;

          }

      
	}
	UxUser_dataContext = UxSaveCtx;


}

/* This function is also called at creation of first button of option menu */
void user_data_option_typeCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
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
     
        
	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
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
			(XtCallbackProc) user_data_option_typeCb, 
			 NULL) ;
	          UxPutContext( menu_item, 
                                      (char *) UxUser_dataContext );

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
	UxUser_dataContext = UxSaveCtx;


}


/*===========================================================================*
** 
** Function Name: get_fields_data
**
** Description:		obtain data from each data field
**
** Revision History: 09/06/96 - Apostrophe is allowed only in comments and
**                              address fields - this is to correct PR 84.
**                   11/11/96 - R2.1 - Added access_dir.
**
**==========================================================================*/
static int get_fields_data ( Widget wgt ) {
	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;


	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
              free_string_data ( ) ;

	      id = XmTextGetString ( idTF ) ;
	      ims_truncStr(id) ;
				if (isApostrophe(id))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid User Id!"); 
          return (IMS_ERROR) ;
				}

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

	      type = XmTextGetString ( typeTF ) ; 
	      ims_truncStr(type) ;
	      priority = XmTextGetString ( priorityTF ) ;
	      ims_truncStr(priority) ; 
	      password = XmTextGetString ( passwordTF ) ;
	      ims_truncStr(password) ; 
				if (isApostrophe(password))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Password!"); 
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

	      ftp_dir = XmTextGetString ( ftp_dirST ) ; 
	      ims_truncStr(ftp_dir) ;
				if (isApostrophe(ftp_dir))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid FTP Dir!"); 
          return (IMS_ERROR) ;
				}

	      access_dir = XmTextGetString ( access_dirST ) ; 
	      ims_truncStr(access_dir) ;
				if (isApostrophe(access_dir))
				{
	        msg_dlgCb (msg_dlg, IMS_ERROR, "Invalid Access Dir!"); 
          return (IMS_ERROR) ;
				}


	}
	UxUser_dataContext = UxSaveCtx;

   
} /* end of get_fields_data */

static void free_string_data ( ) {

              if ( id != NULL ){ 
                XtFree ( id ) ;
                id = (char *) NULL ;
              }
              if ( first_name != NULL ){ 
                XtFree ( first_name ) ;
                first_name = (char *) NULL ;
              }
              if ( m_i != NULL ){ 
                XtFree ( m_i ) ;
                m_i = (char *) NULL ;
              }
              if ( last_name != NULL ){ 
                XtFree ( last_name ) ;
                last_name = (char *) NULL ;
              }
              if ( type != NULL ){ 
                XtFree ( type ) ;
                type = (char *) NULL ;
              }
              if ( priority != NULL ){ 
                XtFree ( priority ) ;
                priority = (char *) NULL ;
              }
              if ( password != NULL ){ 
                XtFree ( password ) ;
                password = (char *) NULL ;
              }
              if ( title != NULL ){ 
                XtFree ( title ) ;
                title = (char *) NULL ;
              }
              if ( organization != NULL ){ 
                XtFree ( organization ) ;
                organization = (char *) NULL ;
              }
              if ( address != NULL ){ 
                XtFree ( address ) ;
                address = (char *) NULL ;
              }
              if ( city != NULL ){ 
                XtFree ( city ) ;
                city = (char *) NULL ;
              }
              if ( state != NULL ) {
                XtFree ( state ) ;
                state = (char *) NULL ;
              }
              if ( country != NULL ) {
                XtFree ( country ) ;
                country = (char *) NULL ;
              }
              if ( zip != NULL ){ 
                XtFree ( zip ) ;
                zip = (char *) NULL ;
              }
              if ( phone != NULL ) {
                XtFree ( phone ) ;
                phone = (char *) NULL ;
              }
              if ( fax != NULL ) {
                XtFree ( fax ) ;
                fax = (char *) NULL ;
              }
              if ( email != NULL ) {
                XtFree ( email ) ;
                email = (char *) NULL ;
              }
              if ( ftp_dir != NULL ){ 
                XtFree ( ftp_dir ) ;
                ftp_dir = (char *) NULL ;
              }
              if ( access_dir != NULL ){ 
                XtFree ( access_dir ) ;
                access_dir = (char *) NULL ;
              }


}
/* Not used */
void user_data_losing_focusCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	int widget_num  = (int) cd;


	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
          switch ( widget_num ) {
            case 1 :
	      id = XmTextGetString ( idTF ) ;
              break ;
            case 2 :
	      first_name = XmTextGetString ( first_nameTF ) ; 
              break ;
            case 3 :
	      m_i = XmTextGetString ( m_iTF ) ; 
              break ;
            case 4 :
	      last_name = XmTextGetString ( last_nameTF ) ; 
              break ;
            case 5 :
	      type = XmTextGetString ( typeTF ) ; 
              break ;
            case 6 :
	      priority = XmTextGetString ( priorityTF ) ; 
              break ;
            case 7 :
	      password = XmTextGetString ( passwordTF ) ; 
              break ;
            case 8 :
	      title = XmTextGetString ( titleTF ) ; 
              break ;
            case 9 :
	      organization = XmTextGetString ( organizationTF ) ; 
              break ;
            case 10 :
	      address = XmTextGetString ( addressTF ) ; 
              break ;
          case 11 :
	      city = XmTextGetString ( cityTF ) ;
              break ;
            case 12 :
	      state = XmTextGetString ( stateTF ) ; 
              break ;
            case 13 :
	      country = XmTextGetString ( countryTF ) ; 
              break ;
            case 14 :
	      zip = XmTextGetString ( zipTF ) ; 
              break ;
            case 15 :
	      phone = XmTextGetString ( phoneTF ) ; 
              break ;
            case 16 :
	      fax = XmTextGetString ( faxTF ) ; 
              break ;
            case 17 :
	      email = XmTextGetString ( emailTF ) ; 
              break ;
            case 18 :
	      ftp_dir = XmTextGetString ( ftp_dirST ) ; 
              break ;
            case 19 :
	      access_dir = XmTextGetString ( access_dirST ) ; 
              break ;

            default :
              break ;
          }

	}
	UxUser_dataContext = UxSaveCtx;


}
/* Used to set the text fields with data */
/* Data is available in the global area and was obtained though the */
/* search users screen.                                             */
void set_user_data ( Widget wgt ) {

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
  OP_USER_PROFILE *usr_ptr  ;
  char buffer[IMS_COL255_LEN+1] ;
  int child_num,i , j ;
  WidgetList widget_list ;
  char * text ;
  XmString button_label ;
  char name[IMS_COL30_LEN+1];

	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;

  usr_ptr = glbData.accounts_users_data.user_data.op_user_profile_ptr ;
  /* find selected user */
  while ( (usr_ptr != (OP_USER_PROFILE *) NULL ) &&
                    (usr_ptr->selected == 0) )
    usr_ptr = usr_ptr->next ;
  if ( usr_ptr == (OP_USER_PROFILE *) NULL ) {
    msg_dlgCb (msg_dlg, IMS_ERROR, "No User Data!");
  }
  else {
    /* get button labels from option menu pane type */
    XtVaGetValues ( option_menu_pane_type, XmNnumChildren, &child_num,
                     XmNchildren, &widget_list, NULL ) ;
    name[0] = '\0' ;
    for ( j=0; j< glbData.user_type_count; j++ ) {
        if ( glbData.user_type[j].item_id == usr_ptr->user_type){
             strcpy ( name, glbData.user_type[j].item_name ) ;
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
		** 09/16/96 - PR 43, allow the GDC operator to modify the 
		** user type after the user is created.
		**
    ** XtSetSensitive ( option_menu_type, False );
		*/

    /* get button labels from option menu pane priority */
    XtVaGetValues ( option_menu_pane_priority, XmNnumChildren, &child_num,
                     XmNchildren, &widget_list, NULL ) ;
    name[0] = '\0' ;
    for ( j=0; j< glbData.priority_count; j++ ) {
      if ( glbData.priority[j].item_id == usr_ptr->priority){
        strcpy ( name, glbData.priority[j].item_name ) ;
        break ;
      }
    }
    if ( name[0] != '\0' ) {
      for ( i=0 ; i < child_num ; i++ ) {
         XtVaGetValues ( widget_list[i], XmNlabelString, &button_label,NULL ) ;
         if ( XmStringGetLtoR ( button_label, XmFONTLIST_DEFAULT_TAG, &text)) {
           if ( !strcmp(name,text ) ) {
             /* set option menu button */
              XtVaSetValues (option_menu_pane_priority , XmNmenuHistory,
                      widget_list[i], NULL ) ;      
             XtFree ( text ) ;
             break ;
           }   
           XtFree ( text ) ;
         }
      }
    }
    XmTextSetString( priorityTF,name ) ;

		/*
		** 09/16/96 - PR 43, allow the GDC operator to modify the 
		** user priority after the user is created.
		**
    ** XtSetSensitive ( option_menu_priority, False );
		*/

    XmTextSetString( idTF,usr_ptr->user_id ) ;
    XmTextSetString( passwordTF,usr_ptr->auth_key ) ; 
    XmTextSetString( first_nameTF,usr_ptr->first_name ) ;
    XmTextSetString( m_iTF,usr_ptr->initial_name ) ;
    XmTextSetString( last_nameTF,usr_ptr->last_name ) ;
    XmTextSetString( titleTF,usr_ptr->title ) ;
    XmTextSetString( zipTF,usr_ptr->zipcode ) ;
    XmTextSetString( phoneTF,usr_ptr->phone ) ;
    XmTextSetString( faxTF,usr_ptr->fax ) ;
    XmTextSetString( organizationTF,usr_ptr->organization ) ;
    XmTextSetString( addressTF,usr_ptr->street ) ;
    XmTextSetString( cityTF,usr_ptr->city ) ;
    XmTextSetString( stateTF,usr_ptr->state ) ;
    XmTextSetString( countryTF,usr_ptr->country ) ;
    XmTextSetString( emailTF,usr_ptr->email ) ;
    XmTextSetString( ftp_dirST,usr_ptr->ftp_dir ) ;
    XmTextSetString( access_dirST,usr_ptr->access_dir ) ;


  }/* end of else */
	UxUser_dataContext = UxSaveCtx;
}

/* not used */
void user_data_cancelCb( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{

	_UxCuser_data      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
        static SELECTED_BUTTON selected  ;
	UxSaveCtx = UxUser_dataContext;
	UxUser_dataContext = UxContext =
			(_UxCuser_data *) UxGetContext( UxWidget ) ;
	{
            free_string_data ( ) ;
            XtDestroyWidget( XtParent ( XtParent ( wgt )  ) );
		  
	}
	UxUser_dataContext = UxSaveCtx;


}


/***************************************************************************
**
** Function Name :	create_user_data_query
**
** Description:		Initializes glbData.accounts_users_data.queryStruct.
** 			sqlBuf with query to update or create a user. It 
**			assumes that at least user_id, user_type, first_name,
**			and last_name have non NULL values.
**					.
**
** Arguments:		save ( CREATE=create, DATA = update ).
**
** Return Value:	none
** Revision: 09/06/96 J. Ting  added apostrophe support for address field
**                             as per PR 84.
**
**           11/11/96 J. Ting  R2.1 - Modified local_dir to ftp_dir.
**                             R2.1 - Added access_dir.
****************************************************************************/
static void create_user_data_query ( ACCOUNT_USER_DATA save ) {
  
  OP_ACC_USR_DATA *acc_usr_ptr ;
  OP_QUERY_STRUCT *sql ;
  char  *sqlPtr ;
  char   attributes[IMS_COL1024_LEN*5+1];
  char  *aPtr;
  int    cnt,temp,i ;
	char text[IMS_COL255_LEN+2];

  acc_usr_ptr = & (glbData.accounts_users_data ) ;
  sql = &(acc_usr_ptr->user_data.queryStruct) ;
  sql->select[0]= sql->from[0]= sql->where[0]= sql->sqlBuf[0] = '\0' ;
  attributes[0] = '\0' ;
  sql->sPtr = sql->select ;
  sql->fPtr = sql->from ;
  sql->wPtr = sql->where ;
  aPtr      = attributes ;
  sqlPtr = sql->sqlBuf ;

  if ( save == CREATE ) {
    strcpy (sql->sPtr,"user_profile \n") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;

	strcpy( aPtr, "( user_id" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( id && *id )
      sprintf( sql->wPtr, "('%s',", id ) ;
    else
      sprintf( sql->wPtr, "('',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    
	strcpy( aPtr, ", user_type" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    temp = 0 ;
    if ( type && *type ) {
      for ( i = 0 ; i < glbData.user_type_count ; i++ )
        if ( !strcmp (glbData.user_type[i].item_name,type) )
          temp = glbData.user_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", auth_key, crypt_key" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( password && *password ) 
      sprintf( sql->wPtr, "'%s','%s',",
               password,
               IK_MakeAuthenticator(first_name ? first_name:" ",
                                    last_name ? last_name:" ",
		 		    password) ) ;
    else
      sprintf( sql->wPtr, "'',''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", priority" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    temp = 0 ;
    if ( priority && *priority ) {
      for ( i = 0 ; i < glbData.priority_count; i++ )
        if ( !strcmp (glbData.priority[i].item_name,priority) )
          temp = glbData.priority[i].item_id ;
    }      
    sprintf( sql->wPtr, "%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", first_name" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( first_name && *first_name ) 
      sprintf( sql->wPtr, "'%s',", first_name ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
  
	strcpy( aPtr, ", initial_name" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( m_i && *m_i )
      sprintf( sql->wPtr, "'%s',", m_i ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", last_name" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( last_name && *last_name ) 
      sprintf( sql->wPtr, "'%s',", last_name ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", title" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( title && *title ) 
      sprintf( sql->wPtr, "'%s',", title ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", zipcode" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( zip && *zip ) 
      sprintf( sql->wPtr, "'%s',", zip ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", phone" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( phone && *phone ) 
      sprintf( sql->wPtr, "'%s',", phone ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", fax" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( fax && *fax ) 
      sprintf( sql->wPtr, "'%s',", fax ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
	strcpy( aPtr, ", organization" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( organization && *organization ) 
      sprintf( sql->wPtr, "'%s',", organization ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", street" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( address && *address ) 
		{
			(void) ims_formatQuotedString (address, text);
      sprintf( sql->wPtr, "'%s',", text) ;
		}
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", city" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( city && *city ) 
      sprintf( sql->wPtr, "'%s',", city ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", state" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( state && *state ) 
      sprintf( sql->wPtr, "'%s',", state ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", country" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( country && *country )
      sprintf( sql->wPtr, "'%s',", country ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
   
	strcpy( aPtr, ", email" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( email && *email )
      sprintf( sql->wPtr, "'%s',", email ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", ftp_dir" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( ftp_dir && *ftp_dir )
      sprintf( sql->wPtr, "'%s',", ftp_dir ) ;
    else
      sprintf( sql->wPtr, "''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, ", access_dir" ) ;
	aPtr = aPtr + strlen( aPtr ) ;
    if ( access_dir && *access_dir )
      sprintf( sql->wPtr, "'%s')", access_dir ) ;
    else
      sprintf( sql->wPtr, "'')" ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

	strcpy( aPtr, " ) \n" ) ;
	aPtr = aPtr + strlen( aPtr ) ;

    sprintf ( sqlPtr, "insert into %s%svalues %s",
                      sql->select, attributes, sql->where ) ;
    /*printf ( "insert into %s%svalues %s",
                      sql->select, attributes, sql->where ) ;*/

  }
  else { /* update */
    strcpy (sql->sPtr,"user_profile \n") ;
    sql->sPtr = sql->sPtr + strlen(sql->sPtr) ;
  
    /*if ( id && *id )
      sprintf( sql->wPtr, "user_id='%s',", id ) ;
    else
     sprintf( sql->wPtr, "user_id='',") ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
    */

    temp = 0 ;
    if ( type && *type ) {
      for ( i = 0 ; i < glbData.user_type_count ; i++)
        if ( !strcmp (glbData.user_type[i].item_name,type) )
          temp = glbData.user_type[i].item_id ;
    }      
    sprintf( sql->wPtr, "user_type=%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( password && *password ) 
      sprintf( sql->wPtr, "auth_key='%s',crypt_key='%s',", 
               password,
               IK_MakeAuthenticator(first_name ? first_name:" ",
                                    last_name ? last_name:" ",
		 		    password)  ) ;
    else
      sprintf( sql->wPtr, "auth_key='',crypt_key=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    temp = 0 ;
    if ( priority && *priority ) {
      for ( i = 0 ; i < glbData.priority_count; i++ )
        if ( !strcmp (glbData.priority[i].item_name,priority) )
          temp = glbData.priority[i].item_id ;
    }      
    sprintf( sql->wPtr, "priority=%d,", temp ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( first_name && *first_name ) 
      sprintf( sql->wPtr, "first_name='%s',", first_name ) ;
    else
      sprintf( sql->wPtr, "first_name=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
  
    if ( m_i && *m_i )
      sprintf( sql->wPtr, "initial_name='%s',", m_i ) ;
    else
      sprintf( sql->wPtr, "initial_name=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( last_name && *last_name ) 
      sprintf( sql->wPtr, "last_name='%s',", last_name ) ;
    else
      sprintf( sql->wPtr, "last_name=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( title && *title ) 
      sprintf( sql->wPtr, "title='%s',", title ) ;
    else
      sprintf( sql->wPtr, "title=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( zip && *zip ) 
      sprintf( sql->wPtr, "zipcode='%s',", zip ) ;
    else
      sprintf( sql->wPtr, "zipcode=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( phone && *phone ) 
      sprintf( sql->wPtr, "phone='%s',", phone ) ;
    else
      sprintf( sql->wPtr, "phone=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( fax && *fax ) 
      sprintf( sql->wPtr, "fax='%s',", fax ) ;
    else
      sprintf( sql->wPtr, "fax=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
 
    if ( organization && *organization ) 
      sprintf( sql->wPtr, "organization='%s',", organization ) ;
    else
      sprintf( sql->wPtr, "organization=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( address && *address ) 
		{
			(void) ims_formatQuotedString (address, text);
      sprintf( sql->wPtr, "street='%s',", text) ;
		}
    else
      sprintf( sql->wPtr, "street=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( city && *city ) 
      sprintf( sql->wPtr, "city='%s',", city ) ;
    else
      sprintf( sql->wPtr, "city=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( state && *state ) 
      sprintf( sql->wPtr, "state='%s',", state ) ;
    else
      sprintf( sql->wPtr, "state=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( country && *country )
      sprintf( sql->wPtr, "country='%s',", country ) ;
    else
      sprintf( sql->wPtr, "country=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;
   
    if ( email && *email )
      sprintf( sql->wPtr, "email='%s',", email ) ;
    else
      sprintf( sql->wPtr, "email=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( ftp_dir && *ftp_dir )
      sprintf( sql->wPtr, "ftp_dir='%s',", ftp_dir ) ;
    else
      sprintf( sql->wPtr, "ftp_dir=''," ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    if ( access_dir && *access_dir )
      sprintf( sql->wPtr, "access_dir='%s' ", access_dir ) ;
    else
      sprintf( sql->wPtr, "access_dir='' " ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf( sql->wPtr,"\n where user_id='%s'", id ) ;
    sql->wPtr = sql->wPtr + strlen( sql->wPtr ) ;

    sprintf ( sqlPtr, "update %s set %s",
                      sql->select, sql->where ) ;
    /*printf ( "update %s set %s",
                      sql->select, sql->where ) ;*/



  }

}/* end of create_user_data_query*/

/***************************************************************************
**
** Function Name :	execute_user_data_query
**
** Description:		Executes the query formed by create_query by calling
**			ims_op_accCat
**
** Arguments:		none
**
** Return Value:	none
**
** Revision History: 07/11/96 - modify update error message due to new
**                              primary unique keys created on first_name,
**                              last_name and password.
**
****************************************************************************/
static int execute_user_data_query ( ACCOUNT_USER_DATA save ) {
	
  OP_CAT_STRUCT *catReq;
  int status;
  int user_count;
  char Msg[IMS_COL1024_LEN+1];

  catReq = &(glbData.accounts_users_data.catReq);
  catReq->item[1] = ( char *)
                   &(glbData.accounts_users_data.user_data.queryStruct.sqlBuf);
  if ((status = ims_op_accCat (catReq, OP_ACC_SAVEUSERDATA)) < IMS_OK) {   
    /* Display error messages */
    if ( save == CREATE )
     sprintf(Msg, "Creation of new user failed. User may already exist.");   
    else
     strcpy(Msg, "Updating of user failed.\n"
									"Possible duplicated entry for First Name, Last Name, and Password.");
    msg_dlgCb (msg_dlg, IMS_FATAL, Msg);
    return (IMS_FATAL) ; 
			  
  }

  return (IMS_OK) ;

}/* end of execute_query */

 
