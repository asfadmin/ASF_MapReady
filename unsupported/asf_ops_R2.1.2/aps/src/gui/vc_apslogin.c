
/*******************************************************************************
	vc_apslogin.c

       Associated Header file: vc_apslogin.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_apslogin.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_apslogin.c"

#include "cb_apslogin.h"
#include "gui_utils.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_apslogin.h"
#undef CONTEXT_MACRO_ACCESS

Widget	APSLoginForm;
Widget	LoginTextField;
Widget	PasswordTextField;
Widget	DatabaseTextField;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSLoginForm()
{
	Widget		_UxParent;


	/* Creation of APSLoginForm */
	_UxParent = XtVaCreatePopupShell( "APSLoginForm_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 0,
			XmNy, 20,
			XmNwidth, 530,
			XmNheight, 362,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSLoginForm",
			XmNiconName, "APSLoginForm",
			NULL );

	APSLoginForm = XtVaCreateManagedWidget( "APSLoginForm",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 530,
			XmNheight, 362,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			NULL );
	XtAddCallback( APSLoginForm, XmNdestroyCallback,
		(XtCallbackProc) cb_exit_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( APSLoginForm, (char *) UxAPSLoginFormContext );
	UxPutClassCode( APSLoginForm, _UxIfClassId );


	/* Creation of APSLoginLabel1 */
	APSLoginLabel1 = XtVaCreateManagedWidget( "APSLoginLabel1",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 181,
			XmNy, 25,
			XmNwidth, 74,
			XmNheight, 50,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-i-normal--34-240-100-100-p-193-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "APS" ),
			NULL );
	UxPutContext( APSLoginLabel1, (char *) UxAPSLoginFormContext );


	/* Creation of APSLoginLabel2 */
	APSLoginLabel2 = XtVaCreateManagedWidget( "APSLoginLabel2",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 265,
			XmNy, 25,
			XmNwidth, 91,
			XmNheight, 50,
			XmNfontList, UxConvertFontList("-b&h-lucidabright-demibold-i-normal--34-240-100-100-p-203-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "login" ),
			NULL );
	UxPutContext( APSLoginLabel2, (char *) UxAPSLoginFormContext );


	/* Creation of LoginLabel */
	LoginLabel = XtVaCreateManagedWidget( "LoginLabel",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 72,
			XmNy, 97,
			XmNwidth, 60,
			XmNheight, 25,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Login:" ),
			NULL );
	UxPutContext( LoginLabel, (char *) UxAPSLoginFormContext );


	/* Creation of PasswordLabel */
	PasswordLabel = XtVaCreateManagedWidget( "PasswordLabel",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 42,
			XmNy, 147,
			XmNwidth, 90,
			XmNheight, 25,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Password:" ),
			NULL );
	UxPutContext( PasswordLabel, (char *) UxAPSLoginFormContext );


	/* Creation of DatabaseLabel */
	DatabaseLabel = XtVaCreateManagedWidget( "DatabaseLabel",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 47,
			XmNy, 197,
			XmNwidth, 85,
			XmNheight, 25,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Database:" ),
			NULL );
	UxPutContext( DatabaseLabel, (char *) UxAPSLoginFormContext );


	/* Creation of LoginTextField */
	LoginTextField = XtVaCreateManagedWidget( "LoginTextField",
			xmTextFieldWidgetClass,
			APSLoginForm,
			XmNwidth, 310,
			XmNx, 132,
			XmNy, 92,
			XmNheight, 35,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1" ),
			NULL );
	XtAddCallback( LoginTextField, XmNactivateCallback,
		(XtCallbackProc) cb_verify_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( LoginTextField, (char *) UxAPSLoginFormContext );

	cb_init_userid( LoginTextField,
			(XtPointer) UxAPSLoginFormContext, (XtPointer) NULL );


	/* Creation of PasswordTextField */
	PasswordTextField = XtVaCreateManagedWidget( "PasswordTextField",
			xmTextFieldWidgetClass,
			APSLoginForm,
			XmNwidth, 310,
			XmNx, 132,
			XmNy, 142,
			XmNheight, 35,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1" ),
			NULL );
	XtAddCallback( PasswordTextField, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_save_password,
		(XtPointer) UxAPSLoginFormContext );
	XtAddCallback( PasswordTextField, XmNactivateCallback,
		(XtCallbackProc) cb_verify_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( PasswordTextField, (char *) UxAPSLoginFormContext );

	cb_init_password( PasswordTextField,
			(XtPointer) UxAPSLoginFormContext, (XtPointer) NULL );


	/* Creation of DatabaseTextField */
	DatabaseTextField = XtVaCreateManagedWidget( "DatabaseTextField",
			xmTextFieldWidgetClass,
			APSLoginForm,
			XmNwidth, 310,
			XmNx, 132,
			XmNy, 192,
			XmNheight, 35,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1" ),
			NULL );
	XtAddCallback( DatabaseTextField, XmNactivateCallback,
		(XtCallbackProc) cb_verify_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( DatabaseTextField, (char *) UxAPSLoginFormContext );

	cb_init_database( DatabaseTextField,
			(XtPointer) UxAPSLoginFormContext, (XtPointer) NULL );


	/* Creation of Ok_PushButton */
	Ok_PushButton = XtVaCreateManagedWidget( "Ok_PushButton",
			xmPushButtonWidgetClass,
			APSLoginForm,
			XmNx, 112,
			XmNy, 253,
			XmNwidth, 65,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNfontList, UxConvertFontList("-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1" ),
			NULL );
	XtAddCallback( Ok_PushButton, XmNactivateCallback,
		(XtCallbackProc) cb_verify_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( Ok_PushButton, (char *) UxAPSLoginFormContext );


	/* Creation of Reset_PushButton */
	Reset_PushButton = XtVaCreateManagedWidget( "Reset_PushButton",
			xmPushButtonWidgetClass,
			APSLoginForm,
			XmNx, 245,
			XmNy, 253,
			XmNwidth, 65,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "RESET" ),
			XmNfontList, UxConvertFontList("-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1" ),
			NULL );
	XtAddCallback( Reset_PushButton, XmNactivateCallback,
		(XtCallbackProc) cb_reset_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( Reset_PushButton, (char *) UxAPSLoginFormContext );


	/* Creation of Exit_PushButton */
	Exit_PushButton = XtVaCreateManagedWidget( "Exit_PushButton",
			xmPushButtonWidgetClass,
			APSLoginForm,
			XmNx, 377,
			XmNy, 253,
			XmNwidth, 65,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "EXIT" ),
			XmNfontList, UxConvertFontList("-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1" ),
			NULL );
	XtAddCallback( Exit_PushButton, XmNactivateCallback,
		(XtCallbackProc) cb_exit_login,
		(XtPointer) UxAPSLoginFormContext );

	UxPutContext( Exit_PushButton, (char *) UxAPSLoginFormContext );


	/* Creation of APSLoginAsterisk */
	APSLoginAsterisk = XtVaCreateManagedWidget( "APSLoginAsterisk",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 255,
			XmNy, 32,
			XmNwidth, 10,
			XmNheight, 15,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-i-normal--20-140-100-100-p-111-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "*" ),
			NULL );
	UxPutContext( APSLoginAsterisk, (char *) UxAPSLoginFormContext );


	/* Creation of labelCopyright_line1 */
	labelCopyright_line1 = XtVaCreateManagedWidget( "labelCopyright_line1",
			xmLabelWidgetClass,
			APSLoginForm,
			XmNx, 56,
			XmNy, 323,
			XmNwidth, 386,
			XmNheight, 28,
			RES_CONVERT( XmNlabelString, "*Copyright (c)1996, California Institute of Technology.\nALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged." ),
			NULL );
	UxPutContext( labelCopyright_line1, (char *) UxAPSLoginFormContext );

	XtVaSetValues(APSLoginForm,
			XmNinitialFocus, PasswordTextField,
			NULL );


	XtAddCallback( APSLoginForm, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSLoginFormContext);


	return ( APSLoginForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSLoginForm( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSLoginForm        *UxContext;
	static int		_Uxinit = 0;

	UxAPSLoginFormContext = UxContext =
		(_UxCAPSLoginForm *) UxNewContext( sizeof(_UxCAPSLoginForm), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSLoginForm();

	XtVaSetValues( gui_GetShellWidget( rtrn ),
		XmNdeleteResponse, XmDESTROY,
		NULL ) ;
	
	cb_map_login();
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

