
/*******************************************************************************
       CPdetailedInfo.c
       (Generated from interface file CPdetailedInfo.i)
       Associated Header file: CPdetailedInfo.h
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
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <X11/Shell.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPdetailedInfo_i[] = "@(#)CPdetailedInfo.i	1.8 96/07/16 16:50:05";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPdetailedInfo.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPdetailedInfo;
Widget	CPdetailedMainWindow;
Widget	CPdetailedForm;
Widget	CPdetailedJobId;
Widget	CPdetailedSubsys;
Widget	CPdetailedScrolledWin;
Widget	CPdetailedOdlText;
Widget	CPdetailedInputFiles;
Widget	CPdetailedOutputFiles;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_menu1_p1_b1( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPdetailedInfo      *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPdetailedInfoContext;
	UxCPdetailedInfoContext = UxContext =
			(_UxCCPdetailedInfo *) UxGetContext( UxWidget );
	UxPopdownInterface(CPdetailedInfo);
	UxCPdetailedInfoContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPdetailedInfo()
{
	Widget		_UxParent;
	Widget		menu1_p1_shell;


	/* Creation of CPdetailedInfo */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	CPdetailedInfo = XtVaCreatePopupShell( "CPdetailedInfo",
			topLevelShellWidgetClass,
			_UxParent,
			XmNx, 470,
			XmNy, 120,
			XmNwidth, 470,
			XmNheight, 510,
			XmNdeleteResponse, XmUNMAP,
			XmNiconName, "SPS Job Detailed Information",
			XmNallowShellResize, TRUE,
			NULL );
	UxPutContext( CPdetailedInfo, (char *) UxCPdetailedInfoContext );
	UxPutClassCode( CPdetailedInfo, _UxIfClassId );


	/* Creation of CPdetailedMainWindow */
	CPdetailedMainWindow = XtVaCreateManagedWidget( "CPdetailedMainWindow",
			xmMainWindowWidgetClass,
			CPdetailedInfo,
			XmNunitType, XmPIXELS,
			XmNx, 350,
			XmNy, 130,
			NULL );
	UxPutContext( CPdetailedMainWindow, (char *) UxCPdetailedInfoContext );


	/* Creation of menu1 */
	menu1 = XtVaCreateManagedWidget( "menu1",
			xmRowColumnWidgetClass,
			CPdetailedMainWindow,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( menu1, (char *) UxCPdetailedInfoContext );


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
	UxPutContext( menu1_p1, (char *) UxCPdetailedInfoContext );


	/* Creation of menu1_p1_b1 */
	menu1_p1_b1 = XtVaCreateManagedWidget( "menu1_p1_b1",
			xmPushButtonGadgetClass,
			menu1_p1,
			RES_CONVERT( XmNlabelString, "Exit" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( menu1_p1_b1, XmNactivateCallback,
		(XtCallbackProc) activateCB_menu1_p1_b1,
		(XtPointer) UxCPdetailedInfoContext );

	UxPutContext( menu1_p1_b1, (char *) UxCPdetailedInfoContext );


	/* Creation of menu1_top_b1 */
	menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
			xmCascadeButtonWidgetClass,
			menu1,
			RES_CONVERT( XmNlabelString, "File" ),
			XmNsubMenuId, menu1_p1,
			RES_CONVERT( XmNmnemonic, "F" ),
			NULL );
	UxPutContext( menu1_top_b1, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedForm */
	CPdetailedForm = XtVaCreateManagedWidget( "CPdetailedForm",
			xmFormWidgetClass,
			CPdetailedMainWindow,
			XmNhorizontalSpacing, 5,
			XmNverticalSpacing, 5,
			NULL );
	UxPutContext( CPdetailedForm, (char *) UxCPdetailedInfoContext );


	/* Creation of DetailedJob_RC */
	DetailedJob_RC = XtVaCreateManagedWidget( "DetailedJob_RC",
			xmRowColumnWidgetClass,
			CPdetailedForm,
			XmNwidth, 171,
			XmNnumColumns, 2,
			XmNpacking, XmPACK_COLUMN,
			XmNtopOffset, 20,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 30,
			NULL );
	UxPutContext( DetailedJob_RC, (char *) UxCPdetailedInfoContext );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			DetailedJob_RC,
			XmNx, -10,
			XmNy, 10,
			XmNwidth, 110,
			XmNheight, 20,
			XmNalignment, XmALIGNMENT_END,
			RES_CONVERT( XmNlabelString, "Job Id" ),
			NULL );
	UxPutContext( label1, (char *) UxCPdetailedInfoContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			DetailedJob_RC,
			XmNx, -10,
			XmNy, 10,
			XmNwidth, 110,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Subsystem" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label2, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedJobId */
	CPdetailedJobId = XtVaCreateManagedWidget( "CPdetailedJobId",
			xmLabelWidgetClass,
			DetailedJob_RC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "1234" ),
			NULL );
	UxPutContext( CPdetailedJobId, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedSubsys */
	CPdetailedSubsys = XtVaCreateManagedWidget( "CPdetailedSubsys",
			xmLabelWidgetClass,
			DetailedJob_RC,
			XmNx, 13,
			XmNy, 33,
			XmNwidth, 110,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "ASP" ),
			NULL );
	UxPutContext( CPdetailedSubsys, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdecodedFiles_label */
	CPdecodedFiles_label = XtVaCreateManagedWidget( "CPdecodedFiles_label",
			xmLabelWidgetClass,
			CPdetailedForm,
			XmNx, 22,
			XmNy, 73,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Decoded Files:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, DetailedJob_RC,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 10,
			NULL );
	UxPutContext( CPdecodedFiles_label, (char *) UxCPdetailedInfoContext );


	/* Creation of CPproductFiles_label */
	CPproductFiles_label = XtVaCreateManagedWidget( "CPproductFiles_label",
			xmLabelWidgetClass,
			CPdetailedForm,
			XmNx, 23,
			XmNy, 93,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Product Files:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPdecodedFiles_label,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightOffset, 0,
			XmNleftWidget, CPdecodedFiles_label,
			XmNleftOffset, 0,
			NULL );
	UxPutContext( CPproductFiles_label, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedScrolledWin */
	CPdetailedScrolledWin = XtVaCreateManagedWidget( "CPdetailedScrolledWin",
			xmScrolledWindowWidgetClass,
			CPdetailedForm,
			XmNx, 40,
			XmNy, 100,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPproductFiles_label,
			NULL );
	UxPutContext( CPdetailedScrolledWin, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedOdlText */
	CPdetailedOdlText = XtVaCreateManagedWidget( "CPdetailedOdlText",
			xmTextWidgetClass,
			CPdetailedScrolledWin,
			XmNwidth, 360,
			XmNheight, 380,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( CPdetailedOdlText, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedInputFiles */
	CPdetailedInputFiles = XtVaCreateManagedWidget( "CPdetailedInputFiles",
			xmLabelWidgetClass,
			CPdetailedForm,
			XmNx, 147,
			XmNy, 69,
			XmNwidth, 110,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "None" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, CPdecodedFiles_label,
			XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget, CPdecodedFiles_label,
			XmNtopOffset, 0,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 20,
			NULL );
	UxPutContext( CPdetailedInputFiles, (char *) UxCPdetailedInfoContext );


	/* Creation of CPdetailedOutputFiles */
	CPdetailedOutputFiles = XtVaCreateManagedWidget( "CPdetailedOutputFiles",
			xmLabelWidgetClass,
			CPdetailedForm,
			XmNx, 169,
			XmNy, 87,
			XmNwidth, 110,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "None" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, CPproductFiles_label,
			XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget, CPproductFiles_label,
			XmNtopOffset, 0,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 20,
			NULL );
	UxPutContext( CPdetailedOutputFiles, (char *) UxCPdetailedInfoContext );

	XtVaSetValues(label2,
			XmNpositionIndex, 1,
			NULL );

	XtVaSetValues(CPdetailedJobId,
			XmNpositionIndex, 2,
			NULL );

	XtVaSetValues(CPdetailedSubsys,
			XmNpositionIndex, XmLAST_POSITION,
			NULL );


	XtAddCallback( CPdetailedInfo, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPdetailedInfoContext);

	XmMainWindowSetAreas( CPdetailedMainWindow, menu1, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, CPdetailedForm );

	return ( CPdetailedInfo );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPdetailedInfo( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPdetailedInfo      *UxContext;
	static int		_Uxinit = 0;

	UxCPdetailedInfoContext = UxContext =
		(_UxCCPdetailedInfo *) UxNewContext( sizeof(_UxCCPdetailedInfo), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPdetailedInfo();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

