
/*******************************************************************************
	ims_opDLtoDTK.c

       Associated Header file: ims_opDLtoDTK.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/TextF.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
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
#include "ims_opDLtoDTK.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_DLtoDTK()
{
	Widget		_UxParent;
	Widget		gotoP_shell;
	Widget		helpP_shell;
	Widget		downlinkP_shell;
	Widget		datatakeP_shell;
	Widget		screenFuncP_shell;


	/* Creation of DLtoDTK */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "DLtoDTK_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 120,
			XmNy, 60,
			XmNwidth, 891,
			XmNheight, 817,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "DLtoDTK",
			XmNiconName, "DLtoDTK",
			NULL );

	}

	DLtoDTK = XtVaCreateManagedWidget( "DLtoDTK",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 891,
			XmNheight, 817,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNnoResize, TRUE,
			NULL );
	UxPutContext( DLtoDTK, (char *) UxDLtoDTKContext );
	UxPutClassCode( DLtoDTK, _UxIfClassId );


	/* Creation of downlinkMB */
	downlinkMB = XtVaCreateManagedWidget( "downlinkMB",
			xmRowColumnWidgetClass,
			DLtoDTK,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 0,
			XmNy, 1,
			XmNmenuAccelerator, "<KeyUp>F10",
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 3,
			XmNwidth, 1500,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( downlinkMB, (char *) UxDLtoDTKContext );


	/* Creation of gotoP */
	gotoP_shell = XtVaCreatePopupShell ("gotoP_shell",
			xmMenuShellWidgetClass, downlinkMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	gotoP = XtVaCreateWidget( "gotoP",
			xmRowColumnWidgetClass,
			gotoP_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( gotoP, (char *) UxDLtoDTKContext );


	/* Creation of welcomeScreenMPB */
	welcomeScreenMPB = XtVaCreateManagedWidget( "welcomeScreenMPB",
			xmPushButtonWidgetClass,
			gotoP,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( welcomeScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_goto_welcomeCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( welcomeScreenMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p1_b3 */
	downlinkMB_p1_b3 = XtVaCreateManagedWidget( "downlinkMB_p1_b3",
			xmSeparatorWidgetClass,
			gotoP,
			NULL );
	UxPutContext( downlinkMB_p1_b3, (char *) UxDLtoDTKContext );


	/* Creation of searchScreenMPB */
	searchScreenMPB = XtVaCreateManagedWidget( "searchScreenMPB",
			xmPushButtonWidgetClass,
			gotoP,
			RES_CONVERT( XmNlabelString, "Downlink Search Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			NULL );
	XtAddCallback( searchScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_goto_searchCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( searchScreenMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p1_b4 */
	downlinkMB_p1_b4 = XtVaCreateManagedWidget( "downlinkMB_p1_b4",
			xmSeparatorWidgetClass,
			gotoP,
			NULL );
	UxPutContext( downlinkMB_p1_b4, (char *) UxDLtoDTKContext );


	/* Creation of closeScreenMPB */
	closeScreenMPB = XtVaCreateManagedWidget( "closeScreenMPB",
			xmPushButtonWidgetClass,
			gotoP,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	XtAddCallback( closeScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_closeCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( closeScreenMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p1_top_b2 */
	downlinkMB_p1_top_b2 = XtVaCreateManagedWidget( "downlinkMB_p1_top_b2",
			xmCascadeButtonWidgetClass,
			downlinkMB,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, gotoP,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNmarginWidth, 12,
			RES_CONVERT( XmNmnemonic, "G" ),
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( downlinkMB_p1_top_b2, (char *) UxDLtoDTKContext );


	/* Creation of helpP */
	helpP_shell = XtVaCreatePopupShell ("helpP_shell",
			xmMenuShellWidgetClass, downlinkMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	helpP = XtVaCreateWidget( "helpP",
			xmRowColumnWidgetClass,
			helpP_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( helpP, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p3_b2 */
	downlinkMB_p3_b2 = XtVaCreateManagedWidget( "downlinkMB_p3_b2",
			xmPushButtonWidgetClass,
			helpP,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			RES_CONVERT( XmNmnemonic, "N" ),
			NULL );
	UxPutContext( downlinkMB_p3_b2, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p3_top_b2 */
	downlinkMB_p3_top_b2 = XtVaCreateManagedWidget( "downlinkMB_p3_top_b2",
			xmCascadeButtonWidgetClass,
			downlinkMB,
			RES_CONVERT( XmNlabelString, "Help" ),
			XmNsubMenuId, helpP,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNx, 0,
			XmNy, 0,
			RES_CONVERT( XmNmnemonic, "H" ),
			NULL );
	UxPutContext( downlinkMB_p3_top_b2, (char *) UxDLtoDTKContext );


	/* Creation of downlinkP */
	downlinkP_shell = XtVaCreatePopupShell ("downlinkP_shell",
			xmMenuShellWidgetClass, downlinkMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	downlinkP = XtVaCreateWidget( "downlinkP",
			xmRowColumnWidgetClass,
			downlinkP_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( downlinkP, (char *) UxDLtoDTKContext );


	/* Creation of viewDownlinkDetailsMPB */
	viewDownlinkDetailsMPB = XtVaCreateManagedWidget( "viewDownlinkDetailsMPB",
			xmPushButtonWidgetClass,
			downlinkP,
			RES_CONVERT( XmNlabelString, "View Downlink Details" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			RES_CONVERT( XmNmnemonic, "V" ),
			NULL );
	XtAddCallback( viewDownlinkDetailsMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_view_DL_detailsCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( viewDownlinkDetailsMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkP_Sep1 */
	downlinkP_Sep1 = XtVaCreateManagedWidget( "downlinkP_Sep1",
			xmSeparatorWidgetClass,
			downlinkP,
			NULL );
	UxPutContext( downlinkP_Sep1, (char *) UxDLtoDTKContext );


	/* Creation of viewDTKsMPB */
	viewDTKsMPB = XtVaCreateManagedWidget( "viewDTKsMPB",
			xmPushButtonWidgetClass,
			downlinkP,
			RES_CONVERT( XmNlabelString, "List Data-takes" ),
			RES_CONVERT( XmNmnemonic, "L" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( viewDTKsMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( viewDTKsMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_top_b3 */
	downlinkMB_top_b3 = XtVaCreateManagedWidget( "downlinkMB_top_b3",
			xmCascadeButtonGadgetClass,
			downlinkMB,
			RES_CONVERT( XmNlabelString, "Downlink Functions" ),
			XmNsubMenuId, downlinkP,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "o" ),
			XmNmarginWidth, 20,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( downlinkMB_top_b3, (char *) UxDLtoDTKContext );


	/* Creation of datatakeP */
	datatakeP_shell = XtVaCreatePopupShell ("datatakeP_shell",
			xmMenuShellWidgetClass, downlinkMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	datatakeP = XtVaCreateWidget( "datatakeP",
			xmRowColumnWidgetClass,
			datatakeP_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( datatakeP, (char *) UxDLtoDTKContext );


	/* Creation of viewDTKDetailsMPB */
	viewDTKDetailsMPB = XtVaCreateManagedWidget( "viewDTKDetailsMPB",
			xmPushButtonWidgetClass,
			datatakeP,
			RES_CONVERT( XmNlabelString, "View Data-take Details" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			RES_CONVERT( XmNmnemonic, "V" ),
			NULL );
	XtAddCallback( viewDTKDetailsMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( viewDTKDetailsMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p5_b1 */
	downlinkMB_p5_b1 = XtVaCreateManagedWidget( "downlinkMB_p5_b1",
			xmSeparatorWidgetClass,
			datatakeP,
			NULL );
	UxPutContext( downlinkMB_p5_b1, (char *) UxDLtoDTKContext );


	/* Creation of toggleProcAuthFlagMPB */
	toggleProcAuthFlagMPB = XtVaCreateManagedWidget( "toggleProcAuthFlagMPB",
			xmPushButtonWidgetClass,
			datatakeP,
			RES_CONVERT( XmNlabelString, "Toggle Proc. Auth." ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			RES_CONVERT( XmNmnemonic, "T" ),
			NULL );
	XtAddCallback( toggleProcAuthFlagMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_toggle_proc_auth_flagCb,
		(XtPointer) 0 );

	UxPutContext( toggleProcAuthFlagMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p5_b2 */
	downlinkMB_p5_b2 = XtVaCreateManagedWidget( "downlinkMB_p5_b2",
			xmSeparatorWidgetClass,
			datatakeP,
			NULL );
	UxPutContext( downlinkMB_p5_b2, (char *) UxDLtoDTKContext );


	/* Creation of resetDTK_MPB */
	resetDTK_MPB = XtVaCreateManagedWidget( "resetDTK_MPB",
			xmPushButtonWidgetClass,
			datatakeP,
			RES_CONVERT( XmNlabelString, "Reset Data-take(s)" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( resetDTK_MPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_reset_dtkCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( resetDTK_MPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_top_b5 */
	downlinkMB_top_b5 = XtVaCreateManagedWidget( "downlinkMB_top_b5",
			xmCascadeButtonGadgetClass,
			downlinkMB,
			RES_CONVERT( XmNlabelString, "Data-take Functions" ),
			RES_CONVERT( XmNmnemonic, "a" ),
			XmNsubMenuId, datatakeP,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( downlinkMB_top_b5, (char *) UxDLtoDTKContext );


	/* Creation of screenFuncP */
	screenFuncP_shell = XtVaCreatePopupShell ("screenFuncP_shell",
			xmMenuShellWidgetClass, downlinkMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	screenFuncP = XtVaCreateWidget( "screenFuncP",
			xmRowColumnWidgetClass,
			screenFuncP_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNwidth, 114,
			NULL );
	UxPutContext( screenFuncP, (char *) UxDLtoDTKContext );


	/* Creation of refreshSearchMPB */
	refreshSearchMPB = XtVaCreateManagedWidget( "refreshSearchMPB",
			xmPushButtonWidgetClass,
			screenFuncP,
			RES_CONVERT( XmNlabelString, "Refresh Search" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( refreshSearchMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_refreshSearchCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( refreshSearchMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p10_b1 */
	downlinkMB_p10_b1 = XtVaCreateManagedWidget( "downlinkMB_p10_b1",
			xmSeparatorWidgetClass,
			screenFuncP,
			NULL );
	UxPutContext( downlinkMB_p10_b1, (char *) UxDLtoDTKContext );


	/* Creation of saveChangesMPB */
	saveChangesMPB = XtVaCreateManagedWidget( "saveChangesMPB",
			xmPushButtonWidgetClass,
			screenFuncP,
			RES_CONVERT( XmNlabelString, "Update" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNmarginWidth, 5,
			RES_CONVERT( XmNmnemonic, "U" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( saveChangesMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_updateCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( saveChangesMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_p10_b3 */
	downlinkMB_p10_b3 = XtVaCreateManagedWidget( "downlinkMB_p10_b3",
			xmSeparatorWidgetClass,
			screenFuncP,
			NULL );
	UxPutContext( downlinkMB_p10_b3, (char *) UxDLtoDTKContext );


	/* Creation of printScreenMPB */
	printScreenMPB = XtVaCreateManagedWidget( "printScreenMPB",
			xmPushButtonWidgetClass,
			screenFuncP,
			RES_CONVERT( XmNlabelString, "Print Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( printScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_printScreenCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( printScreenMPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkMB_top_b6 */
	downlinkMB_top_b6 = XtVaCreateManagedWidget( "downlinkMB_top_b6",
			xmCascadeButtonGadgetClass,
			downlinkMB,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			XmNsubMenuId, screenFuncP,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 155,
			XmNx, 400,
			XmNmarginLeft, 0,
			XmNmarginWidth, 20,
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNy, 0,
			NULL );
	UxPutContext( downlinkMB_top_b6, (char *) UxDLtoDTKContext );


	/* Creation of label155 */
	label155 = XtVaCreateManagedWidget( "label155",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 296,
			XmNy, 42,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Downlink to Datatake Screen" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label155, (char *) UxDLtoDTKContext );


	/* Creation of downlinkIdLBL */
	downlinkIdLBL = XtVaCreateManagedWidget( "downlinkIdLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 58,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Downlink ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( downlinkIdLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlActivityLBL */
	dlActivityLBL = XtVaCreateManagedWidget( "dlActivityLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 188,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Act." ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlActivityLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlStationLBL */
	dlStationLBL = XtVaCreateManagedWidget( "dlStationLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 226,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Stn" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlStationLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlAntennaLBL */
	dlAntennaLBL = XtVaCreateManagedWidget( "dlAntennaLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 270,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Antenna" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlAntennaLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOnLBL */
	dlTimeOnLBL = XtVaCreateManagedWidget( "dlTimeOnLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 390,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time On" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlTimeOnLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOffLBL */
	dlTimeOffLBL = XtVaCreateManagedWidget( "dlTimeOffLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 542,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time Off" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlTimeOffLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlStatusLBL */
	dlStatusLBL = XtVaCreateManagedWidget( "dlStatusLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 680,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlStatusLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlNumDTKsLBL */
	dlNumDTKsLBL = XtVaCreateManagedWidget( "dlNumDTKsLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 768,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "#DTKs" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlNumDTKsLBL, (char *) UxDLtoDTKContext );


	/* Creation of dlPlatformSW */
	dlPlatformSW = XtVaCreateManagedWidget( "dlPlatformSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 58,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 27,
			NULL );
	UxPutContext( dlPlatformSW, (char *) UxDLtoDTKContext );


	/* Creation of dlPlatformLIST */
	dlPlatformLIST = XtVaCreateManagedWidget( "dlPlatformLIST",
			xmListWidgetClass,
			dlPlatformSW,
			XmNwidth, 27,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlPlatformLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlPlatformLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlPlatformLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlSensorSW */
	dlSensorSW = XtVaCreateManagedWidget( "dlSensorSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 86,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 16,
			NULL );
	UxPutContext( dlSensorSW, (char *) UxDLtoDTKContext );


	/* Creation of dlSensorLIST */
	dlSensorLIST = XtVaCreateManagedWidget( "dlSensorLIST",
			xmListWidgetClass,
			dlSensorSW,
			XmNwidth, 16,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlSensorLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlSensorLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlSensorLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlRevSW */
	dlRevSW = XtVaCreateManagedWidget( "dlRevSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 103,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 45,
			NULL );
	UxPutContext( dlRevSW, (char *) UxDLtoDTKContext );


	/* Creation of dlRevLIST */
	dlRevLIST = XtVaCreateManagedWidget( "dlRevLIST",
			xmListWidgetClass,
			dlRevSW,
			XmNwidth, 45,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlRevLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlRevLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlRevLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlSequenceSW */
	dlSequenceSW = XtVaCreateManagedWidget( "dlSequenceSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 149,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 20,
			NULL );
	UxPutContext( dlSequenceSW, (char *) UxDLtoDTKContext );


	/* Creation of dlSequenceLIST */
	dlSequenceLIST = XtVaCreateManagedWidget( "dlSequenceLIST",
			xmListWidgetClass,
			dlSequenceSW,
			XmNwidth, 20,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlSequenceLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlSequenceLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlSequenceLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlActivitySW */
	dlActivitySW = XtVaCreateManagedWidget( "dlActivitySW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 183,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 43,
			NULL );
	UxPutContext( dlActivitySW, (char *) UxDLtoDTKContext );


	/* Creation of dlActivityLIST */
	dlActivityLIST = XtVaCreateManagedWidget( "dlActivityLIST",
			xmListWidgetClass,
			dlActivitySW,
			XmNwidth, 43,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlActivityLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlActivityLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlActivityLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlStationSW */
	dlStationSW = XtVaCreateManagedWidget( "dlStationSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 228,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 27,
			NULL );
	UxPutContext( dlStationSW, (char *) UxDLtoDTKContext );


	/* Creation of dlStationLIST */
	dlStationLIST = XtVaCreateManagedWidget( "dlStationLIST",
			xmListWidgetClass,
			dlStationSW,
			XmNwidth, 27,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlStationLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlStationLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlStationLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlAntennaSW */
	dlAntennaSW = XtVaCreateManagedWidget( "dlAntennaSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 257,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 95,
			NULL );
	UxPutContext( dlAntennaSW, (char *) UxDLtoDTKContext );


	/* Creation of dlAntennaLIST */
	dlAntennaLIST = XtVaCreateManagedWidget( "dlAntennaLIST",
			xmListWidgetClass,
			dlAntennaSW,
			XmNwidth, 94,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlAntennaLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlAntennaLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlAntennaLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOnSW */
	dlTimeOnSW = XtVaCreateManagedWidget( "dlTimeOnSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 353,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 150,
			NULL );
	UxPutContext( dlTimeOnSW, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOnLIST */
	dlTimeOnLIST = XtVaCreateManagedWidget( "dlTimeOnLIST",
			xmListWidgetClass,
			dlTimeOnSW,
			XmNwidth, 150,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlTimeOnLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlTimeOnLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlTimeOnLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOffSW */
	dlTimeOffSW = XtVaCreateManagedWidget( "dlTimeOffSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 506,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 150,
			NULL );
	UxPutContext( dlTimeOffSW, (char *) UxDLtoDTKContext );


	/* Creation of dlTimeOffLIST */
	dlTimeOffLIST = XtVaCreateManagedWidget( "dlTimeOffLIST",
			xmListWidgetClass,
			dlTimeOffSW,
			XmNwidth, 150,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlTimeOffLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlTimeOffLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlTimeOffLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlStatusSW */
	dlStatusSW = XtVaCreateManagedWidget( "dlStatusSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 659,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 95,
			NULL );
	UxPutContext( dlStatusSW, (char *) UxDLtoDTKContext );


	/* Creation of dlStatusLIST */
	dlStatusLIST = XtVaCreateManagedWidget( "dlStatusLIST",
			xmListWidgetClass,
			dlStatusSW,
			XmNwidth, 94,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlStatusLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlStatusLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlStatusLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlNumDTKsSW */
	dlNumDTKsSW = XtVaCreateManagedWidget( "dlNumDTKsSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 770,
			XmNy, 156,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 50,
			NULL );
	UxPutContext( dlNumDTKsSW, (char *) UxDLtoDTKContext );


	/* Creation of dlNumDTKsLIST */
	dlNumDTKsLIST = XtVaCreateManagedWidget( "dlNumDTKsLIST",
			xmListWidgetClass,
			dlNumDTKsSW,
			XmNwidth, 50,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( dlNumDTKsLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_show_downlinkDTKsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dlNumDTKsLIST, XmNbrowseSelectionCallback,
		(XtCallbackProc) dl2dtk_dlLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dlNumDTKsLIST, (char *) UxDLtoDTKContext );


	/* Creation of dlDummySW */
	dlDummySW = XtVaCreateManagedWidget( "dlDummySW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 822,
			XmNy, 156,
			XmNheight, 246,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlDummySW, (char *) UxDLtoDTKContext );


	/* Creation of dlDummyLIST */
	dlDummyLIST = XtVaCreateManagedWidget( "dlDummyLIST",
			xmListWidgetClass,
			dlDummySW,
			XmNwidth, 2,
			XmNheight, 230,
			XmNlistSizePolicy, XmCONSTANT,
			XmNmappedWhenManaged, FALSE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisibleItemCount, 13,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNshadowThickness, 1,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNdoubleClickInterval, 0,
			NULL );
	UxPutContext( dlDummyLIST, (char *) UxDLtoDTKContext );


	/* Creation of separator12 */
	separator12 = XtVaCreateManagedWidget( "separator12",
			xmSeparatorWidgetClass,
			DLtoDTK,
			XmNx, 8,
			XmNy, 405,
			XmNwidth, 875,
			XmNheight, 9,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator12, (char *) UxDLtoDTKContext );


	/* Creation of dtkIdLBL */
	dtkIdLBL = XtVaCreateManagedWidget( "dtkIdLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 28,
			XmNy, 492,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Datatake ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dtkIdLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkSensorModeLBL */
	dtkSensorModeLBL = XtVaCreateManagedWidget( "dtkSensorModeLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 145,
			XmNy, 477,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Sensor\nMode" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 35,
			NULL );
	UxPutContext( dtkSensorModeLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOnLBL */
	dtkTimeOnLBL = XtVaCreateManagedWidget( "dtkTimeOnLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 231,
			XmNy, 492,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time On" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dtkTimeOnLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOffLBL */
	dtkTimeOffLBL = XtVaCreateManagedWidget( "dtkTimeOffLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 381,
			XmNy, 492,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time Off" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dtkTimeOffLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkSiteNameLBL */
	dtkSiteNameLBL = XtVaCreateManagedWidget( "dtkSiteNameLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 536,
			XmNy, 492,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Site Name" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dtkSiteNameLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkQuicklookLBL */
	dtkQuicklookLBL = XtVaCreateManagedWidget( "dtkQuicklookLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 756,
			XmNy, 492,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "QLK" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dtkQuicklookLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkFrameModeLBL */
	dtkFrameModeLBL = XtVaCreateManagedWidget( "dtkFrameModeLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 685,
			XmNy, 477,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Frame\nMode" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 35,
			NULL );
	UxPutContext( dtkFrameModeLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtkProcAuthLBL */
	dtkProcAuthLBL = XtVaCreateManagedWidget( "dtkProcAuthLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 806,
			XmNy, 477,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Proc\nAuth" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 35,
			NULL );
	UxPutContext( dtkProcAuthLBL, (char *) UxDLtoDTKContext );


	/* Creation of dtklPlatformSW */
	dtklPlatformSW = XtVaCreateManagedWidget( "dtklPlatformSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 28,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 27,
			NULL );
	UxPutContext( dtklPlatformSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkPlatformLIST */
	dtkPlatformLIST = XtVaCreateManagedWidget( "dtkPlatformLIST",
			xmListWidgetClass,
			dtklPlatformSW,
			XmNwidth, 27,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkPlatformLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkPlatformLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkPlatformLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkSensorSW */
	dtkSensorSW = XtVaCreateManagedWidget( "dtkSensorSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 56,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 16,
			NULL );
	UxPutContext( dtkSensorSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkSensorLIST */
	dtkSensorLIST = XtVaCreateManagedWidget( "dtkSensorLIST",
			xmListWidgetClass,
			dtkSensorSW,
			XmNwidth, 16,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkSensorLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkSensorLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkSensorLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkRevSW */
	dtkRevSW = XtVaCreateManagedWidget( "dtkRevSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 73,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 45,
			NULL );
	UxPutContext( dtkRevSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkRevLIST */
	dtkRevLIST = XtVaCreateManagedWidget( "dtkRevLIST",
			xmListWidgetClass,
			dtkRevSW,
			XmNwidth, 45,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkRevLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkRevLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkRevLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkSequenceSW */
	dtkSequenceSW = XtVaCreateManagedWidget( "dtkSequenceSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 119,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 20,
			NULL );
	UxPutContext( dtkSequenceSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkSequenceLIST */
	dtkSequenceLIST = XtVaCreateManagedWidget( "dtkSequenceLIST",
			xmListWidgetClass,
			dtkSequenceSW,
			XmNwidth, 20,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkSequenceLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkSequenceLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkSequenceLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkSensorModeSW */
	dtkSensorModeSW = XtVaCreateManagedWidget( "dtkSensorModeSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 153,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 40,
			NULL );
	UxPutContext( dtkSensorModeSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkSensorModeLIST */
	dtkSensorModeLIST = XtVaCreateManagedWidget( "dtkSensorModeLIST",
			xmListWidgetClass,
			dtkSensorModeSW,
			XmNwidth, 40,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkSensorModeLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkSensorModeLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkSensorModeLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOnSW */
	dtkTimeOnSW = XtVaCreateManagedWidget( "dtkTimeOnSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 194,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 150,
			NULL );
	UxPutContext( dtkTimeOnSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOnLIST */
	dtkTimeOnLIST = XtVaCreateManagedWidget( "dtkTimeOnLIST",
			xmListWidgetClass,
			dtkTimeOnSW,
			XmNwidth, 150,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkTimeOnLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkTimeOnLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkTimeOnLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOffSW */
	dtkTimeOffSW = XtVaCreateManagedWidget( "dtkTimeOffSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 345,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 150,
			NULL );
	UxPutContext( dtkTimeOffSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkTimeOffLIST */
	dtkTimeOffLIST = XtVaCreateManagedWidget( "dtkTimeOffLIST",
			xmListWidgetClass,
			dtkTimeOffSW,
			XmNwidth, 150,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkTimeOffLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkTimeOffLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkTimeOffLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkSiteNameSW */
	dtkSiteNameSW = XtVaCreateManagedWidget( "dtkSiteNameSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 496,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 170,
			NULL );
	UxPutContext( dtkSiteNameSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkSiteNameLIST */
	dtkSiteNameLIST = XtVaCreateManagedWidget( "dtkSiteNameLIST",
			xmListWidgetClass,
			dtkSiteNameSW,
			XmNwidth, 170,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkSiteNameLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkSiteNameLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkSiteNameLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkFrameModeSW */
	dtkFrameModeSW = XtVaCreateManagedWidget( "dtkFrameModeSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 667,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 90,
			NULL );
	UxPutContext( dtkFrameModeSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkFrameModeLIST */
	dtkFrameModeLIST = XtVaCreateManagedWidget( "dtkFrameModeLIST",
			xmListWidgetClass,
			dtkFrameModeSW,
			XmNwidth, 90,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkFrameModeLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkFrameModeLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkFrameModeLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkQuicklookSW */
	dtkQuicklookSW = XtVaCreateManagedWidget( "dtkQuicklookSW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 758,
			XmNy, 515,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 35,
			NULL );
	UxPutContext( dtkQuicklookSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkQuicklookLIST */
	dtkQuicklookLIST = XtVaCreateManagedWidget( "dtkQuicklookLIST",
			xmListWidgetClass,
			dtkQuicklookSW,
			XmNwidth, 35,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkQuicklookLIST, XmNdefaultActionCallback,
		(XtCallbackProc) dl2dtk_view_DTK_detailsCb,
		(XtPointer) UxDLtoDTKContext );
	XtAddCallback( dtkQuicklookLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_dtkLists_selectionCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkQuicklookLIST, (char *) UxDLtoDTKContext );


	/* Creation of dtkProcAuthFRAME */
	dtkProcAuthFRAME = XtVaCreateManagedWidget( "dtkProcAuthFRAME",
			xmFrameWidgetClass,
			DLtoDTK,
			XmNwidth, 27,
			XmNheight, 234,
			XmNx, 809,
			XmNy, 513,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( dtkProcAuthFRAME, (char *) UxDLtoDTKContext );


	/* Creation of dtkProcAuthSW */
	dtkProcAuthSW = XtVaCreateManagedWidget( "dtkProcAuthSW",
			xmScrolledWindowWidgetClass,
			dtkProcAuthFRAME,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNx, 2,
			XmNy, 2,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 230,
			XmNwidth, 30,
			RES_CONVERT( XmNtopShadowColor, "#d4d4e4e4eaea" ),
			NULL );
	UxPutContext( dtkProcAuthSW, (char *) UxDLtoDTKContext );


	/* Creation of dtkProcAuthLIST */
	dtkProcAuthLIST = XtVaCreateManagedWidget( "dtkProcAuthLIST",
			xmListWidgetClass,
			dtkProcAuthSW,
			XmNwidth, 23,
			XmNheight, 230,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( dtkProcAuthLIST, XmNextendedSelectionCallback,
		(XtCallbackProc) dl2dtk_toggle_proc_auth_flagCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dtkProcAuthLIST, (char *) UxDLtoDTKContext );


	/* Creation of ProcFlagFootnote */
	ProcFlagFootnote = XtVaCreateManagedWidget( "ProcFlagFootnote",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 705,
			XmNy, 751,
			XmNwidth, 148,
			XmNheight, 17,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, "*Proc Auth CANNOT be changed" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-i-normal--11-80-100-100-p-57-iso8859-1" ),
			XmNmappedWhenManaged, FALSE,
			XmNrightAttachment, XmATTACH_NONE,
			NULL );
	UxPutContext( ProcFlagFootnote, (char *) UxDLtoDTKContext );


	/* Creation of dtkDummySW */
	dtkDummySW = XtVaCreateManagedWidget( "dtkDummySW",
			xmScrolledWindowWidgetClass,
			DLtoDTK,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 838,
			XmNy, 515,
			XmNheight, 246,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dtkDummySW, (char *) UxDLtoDTKContext );


	/* Creation of dtkDummyLIST */
	dtkDummyLIST = XtVaCreateManagedWidget( "dtkDummyLIST",
			xmListWidgetClass,
			dtkDummySW,
			XmNwidth, 2,
			XmNheight, 230,
			XmNlistSizePolicy, XmCONSTANT,
			XmNmappedWhenManaged, FALSE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisibleItemCount, 13,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNshadowThickness, 1,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNdoubleClickInterval, 0,
			NULL );
	UxPutContext( dtkDummyLIST, (char *) UxDLtoDTKContext );


	/* Creation of dl2dtkUpdatePB */
	dl2dtkUpdatePB = XtVaCreateManagedWidget( "dl2dtkUpdatePB",
			xmPushButtonWidgetClass,
			DLtoDTK,
			XmNx, 24,
			XmNy, 773,
			XmNwidth, 185,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "UPDATE" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( dl2dtkUpdatePB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_updateCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dl2dtkUpdatePB, (char *) UxDLtoDTKContext );


	/* Creation of dl2dtkRefreshSearchPB */
	dl2dtkRefreshSearchPB = XtVaCreateManagedWidget( "dl2dtkRefreshSearchPB",
			xmPushButtonWidgetClass,
			DLtoDTK,
			XmNx, 241,
			XmNy, 773,
			XmNwidth, 185,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "REFRESH   SEARCH" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( dl2dtkRefreshSearchPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_refreshSearchCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dl2dtkRefreshSearchPB, (char *) UxDLtoDTKContext );


	/* Creation of dl2dtkPrintScreenPB */
	dl2dtkPrintScreenPB = XtVaCreateManagedWidget( "dl2dtkPrintScreenPB",
			xmPushButtonWidgetClass,
			DLtoDTK,
			XmNx, 459,
			XmNy, 773,
			XmNwidth, 185,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "PRINT   SCREEN" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dl2dtkPrintScreenPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_printScreenCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dl2dtkPrintScreenPB, (char *) UxDLtoDTKContext );


	/* Creation of dl2dtkCloseScreenPB */
	dl2dtkCloseScreenPB = XtVaCreateManagedWidget( "dl2dtkCloseScreenPB",
			xmPushButtonWidgetClass,
			DLtoDTK,
			XmNx, 676,
			XmNy, 773,
			XmNwidth, 185,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dl2dtkCloseScreenPB, XmNactivateCallback,
		(XtCallbackProc) dl2dtk_closeCb,
		(XtPointer) UxDLtoDTKContext );

	UxPutContext( dl2dtkCloseScreenPB, (char *) UxDLtoDTKContext );


	/* Creation of downlinkIdLBL1 */
	downlinkIdLBL1 = XtVaCreateManagedWidget( "downlinkIdLBL1",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 58,
			XmNy, 133,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Downlink ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( downlinkIdLBL1, (char *) UxDLtoDTKContext );


	/* Creation of dlTotalNumDlksLBL */
	dlTotalNumDlksLBL = XtVaCreateManagedWidget( "dlTotalNumDlksLBL",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 30,
			XmNy, 88,
			XmNwidth, 240,
			XmNheight, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Number of Downlinks:" ),
			NULL );
	UxPutContext( dlTotalNumDlksLBL, (char *) UxDLtoDTKContext );


	/* Creation of label21 */
	label21 = XtVaCreateManagedWidget( "label21",
			xmLabelWidgetClass,
			DLtoDTK,
			XmNx, 30,
			XmNy, 439,
			XmNwidth, 245,
			XmNheight, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Number of Datatakes:" ),
			NULL );
	UxPutContext( label21, (char *) UxDLtoDTKContext );


	/* Creation of dlTotalDlksTF */
	dlTotalDlksTF = XtVaCreateManagedWidget( "dlTotalDlksTF",
			xmTextFieldWidgetClass,
			DLtoDTK,
			XmNwidth, 110,
			XmNx, 292,
			XmNy, 81,
			XmNheight, 36,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNeditable, FALSE,
			NULL );
	UxPutContext( dlTotalDlksTF, (char *) UxDLtoDTKContext );


	/* Creation of dtkTotalDksTF */
	dtkTotalDksTF = XtVaCreateManagedWidget( "dtkTotalDksTF",
			xmTextFieldWidgetClass,
			DLtoDTK,
			XmNwidth, 110,
			XmNx, 292,
			XmNy, 435,
			XmNheight, 36,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNeditable, FALSE,
			NULL );
	UxPutContext( dtkTotalDksTF, (char *) UxDLtoDTKContext );

	XtVaSetValues(DLtoDTK,
			XmNinitialFocus, dtkProcAuthFRAME,
			NULL );

	XtVaSetValues(downlinkMB,
			XmNmenuHelpWidget, downlinkMB_p3_top_b2,
			NULL );

	XtVaSetValues(refreshSearchMPB,
			XmNpositionIndex, 0,
			NULL );

	XtVaSetValues(saveChangesMPB,
			XmNpositionIndex, 2,
			NULL );


	XtAddCallback( DLtoDTK, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxDLtoDTKContext);


	return ( DLtoDTK );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_DLtoDTK( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCDLtoDTK             *UxContext;
	static int		_Uxinit = 0;

	UxDLtoDTKContext = UxContext =
		(_UxCDLtoDTK *) UxNewContext( sizeof(_UxCDLtoDTK), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_DLtoDTK();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

