
/*******************************************************************************
	ims_opMsgBoxDlg.c

       Associated Header file: ims_opMsgBoxDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/MessageB.h>

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
#include "ims_opMsgBoxDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_msgBoxDlg()
{
	Widget		_UxParent;


	/* Creation of msgBoxDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "msgBoxDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 400,
			XmNy, 400,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "msgBoxDlg",
			NULL );

	msgBoxDlg = XtVaCreateWidget( "msgBoxDlg",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_MESSAGE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmessageString, "" ),
			XmNtextFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNokLabelString, "OK" ),
			XmNresizePolicy, XmRESIZE_ANY,
			XmNnoResize, FALSE,
			XmNdefaultPosition, TRUE,
			NULL );
	XtAddCallback( msgBoxDlg, XmNokCallback,
		(XtCallbackProc) msgBoxDlg_okCb,
		(XtPointer) UxMsgBoxDlgContext );
	XtAddCallback( msgBoxDlg, XmNcancelCallback,
		(XtCallbackProc) msgBoxDlg_okCb,
		(XtPointer) UxMsgBoxDlgContext );

	UxPutContext( msgBoxDlg, (char *) UxMsgBoxDlgContext );
	UxPutClassCode( msgBoxDlg, _UxIfClassId );


	XtAddCallback( msgBoxDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxMsgBoxDlgContext);


	return ( msgBoxDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_msgBoxDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCmsgBoxDlg           *UxContext;
	static int		_Uxinit = 0;

	UxMsgBoxDlgContext = UxContext =
		(_UxCmsgBoxDlg *) UxNewContext( sizeof(_UxCmsgBoxDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_msgBoxDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

