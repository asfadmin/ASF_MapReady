
/*******************************************************************************
	vc_msgbox.c

       Associated Header file: vc_msgbox.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/MessageB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
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
#pragma ident	"@(#)vc_msgbox.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_msgbox.c"

#include "gui_utils.h"

extern Widget mainIface ;

void popup_message() ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_msgbox.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
Auxiliary code from the Declarations Editor:
*******************************************************************************/

/*==============================================================================
Function:       popup_message
 
Description:    creates and displays messages in a MessageDialog widget
 
Parameters:     dialog_type	(5/96: Unused) specifies the symbol type
                           	to display in the widget

                dialog_title	title to use for the widget

                msg        	message to be displayed

		grab_kind  	XtPopup grab_kind
 
Returns:
 
Creator:        Unknown
 
Creation Date:
 
Notes:
                5/96:	changed the message area to be a scrolled Text
                     	widget to handle long and mega-line messages.
                5/96:	got rid of using the dialog_type, but kept the
                     	parameter for historical purposes.
==============================================================================*/
void
popup_message(dialog_type, dialog_title, msg, grab_kind)
    int dialog_type ;
    char *dialog_title ;
    char *msg ;
    XtGrabKind grab_kind ;
{
    Widget msg_box ;
    XmString XmString_dialog_title ;
 
    msg_box = (Widget) create_APSMsgBoxDlg(XtParent(mainIface)) ;
    XmString_dialog_title = XmStringCreateSimple(dialog_title) ;

    XtVaSetValues(msg_box,
        XmNdialogType, XmDIALOG_TEMPLATE,
        XmNdialogTitle, XmString_dialog_title,
        NULL) ;

    XtVaSetValues(scrolledText_msgBox,
        XmNvalue, msg,
        NULL) ;

    XmStringFree(XmString_dialog_title) ;
 
    XtManageChild(msg_box) ;
    XtPopup(XtParent(msg_box), grab_kind) ;

    XFlush( XtDisplay( msg_box ) ) ;

    DisplayXCursor(True) ;

    return ;
}


/* ARGSUSED2 */
void
destroy_message_box(widget_UNUSED, client_data, cbs)
    Widget widget_UNUSED ;
    XtPointer client_data, cbs ;
{
	DisplayXCursor(False) ;
    XtDestroyWidget((Widget) client_data) ;
}

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  okCallback_APSMsgBoxDlg(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMsgBoxDlg        *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMsgBoxDlgContext;
	UxAPSMsgBoxDlgContext = UxContext =
			(_UxCAPSMsgBoxDlg *) UxGetContext( UxWidget );
	{
	UxPopdownInterface(UxThisWidget) ;
	XtDestroyWidget(UxWidget) ;
	DisplayXCursor(False) ;
	
	}
	UxAPSMsgBoxDlgContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSMsgBoxDlg()
{
	Widget		_UxParent;


	/* Creation of APSMsgBoxDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "APSMsgBoxDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 25,
			XmNy, 401,
			XmNwidth, 532,
			XmNheight, 287,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSMsgBoxDlg",
			NULL );

	APSMsgBoxDlg = XtVaCreateWidget( "APSMsgBoxDlg",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_MESSAGE,
			XmNunitType, XmPIXELS,
			XmNbuttonFontList, UxConvertFontList("rockwell-bold" ),
			XmNnoResize, FALSE,
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNdefaultPosition, FALSE,
			XmNheight, 287,
			XmNwidth, 532,
			NULL );
	XtAddCallback( APSMsgBoxDlg, XmNokCallback,
		(XtCallbackProc) okCallback_APSMsgBoxDlg,
		(XtPointer) UxAPSMsgBoxDlgContext );

	UxPutContext( APSMsgBoxDlg, (char *) UxAPSMsgBoxDlgContext );
	UxPutClassCode( APSMsgBoxDlg, _UxIfClassId );


	/* Creation of scrolledWindowText_msgBox */
	scrolledWindowText_msgBox = XtVaCreateManagedWidget( "scrolledWindowText_msgBox",
			xmScrolledWindowWidgetClass,
			APSMsgBoxDlg,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 11,
			XmNy, 25,
			NULL );
	UxPutContext( scrolledWindowText_msgBox, (char *) UxAPSMsgBoxDlgContext );


	/* Creation of scrolledText_msgBox */
	scrolledText_msgBox = XtVaCreateManagedWidget( "scrolledText_msgBox",
			xmTextWidgetClass,
			scrolledWindowText_msgBox,
			XmNwidth, 476,
			XmNheight, 168,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			NULL );
	UxPutContext( scrolledText_msgBox, (char *) UxAPSMsgBoxDlgContext );


	XtAddCallback( APSMsgBoxDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSMsgBoxDlgContext);


	return ( APSMsgBoxDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSMsgBoxDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSMsgBoxDlg        *UxContext;
	static int		_Uxinit = 0;

	UxAPSMsgBoxDlgContext = UxContext =
		(_UxCAPSMsgBoxDlg *) UxNewContext( sizeof(_UxCAPSMsgBoxDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSMsgBoxDlg();

	XtUnmanageChild(XmMessageBoxGetChild(rtrn, XmDIALOG_CANCEL_BUTTON)) ;
	XtUnmanageChild(XmMessageBoxGetChild(rtrn, XmDIALOG_HELP_BUTTON)) ;
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

