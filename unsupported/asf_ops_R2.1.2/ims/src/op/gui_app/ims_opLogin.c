
/*******************************************************************************
	ims_opLogin.c

       Associated Header file: ims_opLogin.h
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
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "ims_opCb.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opLogin.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_login()
{
	Widget		_UxParent;


	/* Creation of login */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "login_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 299,
			XmNy, 217,
			XmNwidth, 440,
			XmNheight, 450,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "login",
			NULL );

	}

	login = XtVaCreateManagedWidget( "login",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 440,
			XmNheight, 450,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNnoResize, TRUE,
			NULL );
	UxPutContext( login, (char *) UxLoginContext );
	UxPutClassCode( login, _UxIfClassId );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			login,
			XmNx, 65,
			XmNy, 285,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User  ID :" ),
			NULL );
	UxPutContext( label1, (char *) UxLoginContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			login,
			XmNx, 57,
			XmNy, 337,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Password :" ),
			NULL );
	UxPutContext( label2, (char *) UxLoginContext );


	/* Creation of userIdText */
	userIdText = XtVaCreateManagedWidget( "userIdText",
			xmTextWidgetClass,
			login,
			XmNwidth, 204,
			XmNx, 169,
			XmNy, 277,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( userIdText, XmNactivateCallback,
		(XtCallbackProc) login_moveFocusCb,
		(XtPointer) UxLoginContext );

	UxPutContext( userIdText, (char *) UxLoginContext );


	/* Creation of passwdText */
	passwdText = XtVaCreateManagedWidget( "passwdText",
			xmTextWidgetClass,
			login,
			XmNwidth, 204,
			XmNx, 169,
			XmNy, 325,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( passwdText, XmNmodifyVerifyCallback,
		(XtCallbackProc) login_passwdCb,
		(XtPointer) UxLoginContext );

	UxPutContext( passwdText, (char *) UxLoginContext );


	/* Creation of label3 */
	label3 = XtVaCreateManagedWidget( "label3",
			xmLabelWidgetClass,
			login,
			XmNx, 68,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ASF  IMS/DADS  OPERATIONS" ),
			XmNheight, 36,
			NULL );
	UxPutContext( label3, (char *) UxLoginContext );


	/* Creation of label206 */
	label206 = XtVaCreateManagedWidget( "label206",
			xmLabelWidgetClass,
			login,
			XmNx, 104,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			XmNheight, 36,
			XmNlabelType, XmPIXMAP,
			NULL );
	UxPutContext( label206, (char *) UxLoginContext );


	/* Creation of separator34 */
	separator34 = XtVaCreateManagedWidget( "separator34",
			xmSeparatorWidgetClass,
			login,
			XmNwidth, 440,
			XmNheight, 16,
			XmNx, -1,
			XmNy, 375,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator34, (char *) UxLoginContext );


	/* Creation of okPB */
	okPB = XtVaCreateManagedWidget( "okPB",
			xmPushButtonWidgetClass,
			login,
			XmNx, 35,
			XmNy, 395,
			XmNwidth, 120,
			XmNheight, 37,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNshadowThickness, 4,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			NULL );
	UxPutContext( okPB, (char *) UxLoginContext );


	/* Creation of cancelPB */
	cancelPB = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			login,
			XmNx, 279,
			XmNy, 395,
			XmNwidth, 120,
			XmNheight, 37,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNshadowThickness, 4,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			NULL );
	UxPutContext( cancelPB, (char *) UxLoginContext );


	/* Creation of label150 */
	label150 = XtVaCreateManagedWidget( "label150",
			xmLabelWidgetClass,
			login,
			XmNx, 66,
			XmNy, 229,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Copyright (C) 1996, California Institute of Technology.  U.S. Government\n"
"Sponsorship under NASA Contract NAS7-1260 is acknowledged." ),
			XmNheight, 36,
			NULL );
	UxPutContext( label150, (char *) UxLoginContext );

	XtVaSetValues(label206,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/rsat_small.xpm" ),
			NULL );


	XtAddCallback( login, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxLoginContext);


	return ( login );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_login( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxClogin               *UxContext;
	static int		_Uxinit = 0;

	UxLoginContext = UxContext =
		(_UxClogin *) UxNewContext( sizeof(_UxClogin), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_login();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

