
/*******************************************************************************
       CPhelp.c
       (Generated from interface file CPhelp.i)
       Associated Header file: CPhelp.h
       Associated Resource file: CPhelp.rf
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <X11/Shell.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPhelp_i[] = "@(#)CPhelp.i	1.3 96/07/18 19:02:01";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPhelp.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPhelp;
Widget	CPhelpText;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_menu1_p1_b1( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPhelp              *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPhelpContext;
	UxCPhelpContext = UxContext =
			(_UxCCPhelp *) UxGetContext( UxWidget );
	UxPopdownInterface(CPhelp);
	UxCPhelpContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPhelp()
{
	Widget		_UxParent;
	Widget		menu1_p1_shell;


	/* Creation of CPhelp */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	CPhelp = XtVaCreatePopupShell( "CPhelp",
			topLevelShellWidgetClass,
			_UxParent,
			XmNx, 470,
			XmNy, 120,
			XmNwidth, 700,
			XmNheight, 510,
			XmNdeleteResponse, XmUNMAP,
			XmNiconName, "SPS Help",
			NULL );
	UxPutContext( CPhelp, (char *) UxCPhelpContext );
	UxPutClassCode( CPhelp, _UxIfClassId );


	/* Creation of mainWindow2 */
	mainWindow2 = XtVaCreateManagedWidget( "mainWindow2",
			xmMainWindowWidgetClass,
			CPhelp,
			XmNunitType, XmPIXELS,
			XmNx, 0,
			XmNy, -30,
			XmNwidth, 10,
			XmNheight, 40,
			NULL );
	UxPutContext( mainWindow2, (char *) UxCPhelpContext );


	/* Creation of menu1 */
	menu1 = XtVaCreateManagedWidget( "menu1",
			xmRowColumnWidgetClass,
			mainWindow2,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( menu1, (char *) UxCPhelpContext );


	/* Creation of menu1_p1 */
	menu1_p1_shell = XtVaCreatePopupShell ("menu1_p1_shell",
			xmMenuShellWidgetClass, menu1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menu1_p1 = XtVaCreateWidget( "menu1_p1",
			xmRowColumnWidgetClass,
			menu1_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menu1_p1, (char *) UxCPhelpContext );


	/* Creation of menu1_p1_b1 */
	menu1_p1_b1 = XtVaCreateManagedWidget( "menu1_p1_b1",
			xmPushButtonGadgetClass,
			menu1_p1,
			RES_CONVERT( XmNlabelString, "Exit" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( menu1_p1_b1, XmNactivateCallback,
		(XtCallbackProc) activateCB_menu1_p1_b1,
		(XtPointer) UxCPhelpContext );

	UxPutContext( menu1_p1_b1, (char *) UxCPhelpContext );


	/* Creation of menu1_top_b1 */
	menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
			xmCascadeButtonWidgetClass,
			menu1,
			RES_CONVERT( XmNlabelString, "File" ),
			XmNsubMenuId, menu1_p1,
			RES_CONVERT( XmNmnemonic, "F" ),
			NULL );
	UxPutContext( menu1_top_b1, (char *) UxCPhelpContext );


	/* Creation of form1 */
	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass,
			mainWindow2,
			XmNhorizontalSpacing, 5,
			XmNverticalSpacing, 5,
			NULL );
	UxPutContext( form1, (char *) UxCPhelpContext );


	/* Creation of scrolledWindow1 */
	scrolledWindow1 = XtVaCreateManagedWidget( "scrolledWindow1",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNheight, 362,
			XmNwidth, 380,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNy, 2,
			XmNtopAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( scrolledWindow1, (char *) UxCPhelpContext );


	/* Creation of CPhelpText */
	CPhelpText = XtVaCreateManagedWidget( "CPhelpText",
			xmTextWidgetClass,
			scrolledWindow1,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNvalue, "This is a sample help message",
			XmNfontList, UxConvertFontList("-misc-fixed-*-*-*-*-14-*" ),
			NULL );
	UxPutContext( CPhelpText, (char *) UxCPhelpContext );


	XtAddCallback( CPhelp, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPhelpContext);

	XmMainWindowSetAreas( mainWindow2, menu1, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, form1 );

	return ( CPhelp );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPhelp( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPhelp              *UxContext;
	static int		_Uxinit = 0;

	UxCPhelpContext = UxContext =
		(_UxCCPhelp *) UxNewContext( sizeof(_UxCCPhelp), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		UxLoadResources( "CPhelp.rf" );
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPhelp();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

