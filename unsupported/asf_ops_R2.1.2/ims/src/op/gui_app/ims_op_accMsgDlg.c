
/*******************************************************************************
	ims_op_accMsgDlg.c

       Associated Header file: ims_op_accMsgDlg.h
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

#include "ims_op_accCb.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accMsgDlg.h"
#undef CONTEXT_MACRO_ACCESS

Widget	msg_dlg;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_msg_dlg()
{
	Widget		_UxParent;


	/* Creation of msg_dlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "msg_dlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 192,
			XmNy, 120,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "msg_dlg",
			NULL );

	msg_dlg = XtVaCreateWidget( "msg_dlg",
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
			RES_CONVERT( XmNokLabelString, "   OK   " ),
			XmNresizePolicy, XmRESIZE_ANY,
			XmNnoResize, FALSE,
			NULL );
	XtAddCallback( msg_dlg, XmNokCallback,
		(XtCallbackProc) msg_dlg_okCb,
		(XtPointer) UxMsg_dlgContext );

	UxPutContext( msg_dlg, (char *) UxMsg_dlgContext );
	UxPutClassCode( msg_dlg, _UxIfClassId );


	XtAddCallback( msg_dlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxMsg_dlgContext);


	return ( msg_dlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_msg_dlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCmsg_dlg             *UxContext;
	static int		_Uxinit = 0;

	UxMsg_dlgContext = UxContext =
		(_UxCmsg_dlg *) UxNewContext( sizeof(_UxCmsg_dlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_msg_dlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

