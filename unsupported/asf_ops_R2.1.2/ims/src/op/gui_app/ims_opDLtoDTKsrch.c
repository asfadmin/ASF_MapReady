
/*******************************************************************************
	ims_opDLtoDTKsrch.c

       Associated Header file: ims_opDLtoDTKsrch.h
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
#include <Xm/Text.h>
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
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
#include "ims_opDLtoDTKsrch.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_DLtoDTKsearch()
{
	Widget		_UxParent;
	Widget		menuBar_p1_shell;
	Widget		menuBar1_p2_shell;
	Widget		menuBar1_p3_shell;


	/* Creation of DLtoDTKsearch */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "DLtoDTKsearch_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 51,
			XmNy, 140,
			XmNwidth, 1067,
			XmNheight, 616,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "DLtoDTKsearch",
			XmNiconName, "DLtoDTKsearch",
			NULL );

	}

	DLtoDTKsearch = XtVaCreateManagedWidget( "DLtoDTKsearch",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1067,
			XmNheight, 616,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "Downlink (to Datatake) Search Screen" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNnoResize, TRUE,
			NULL );
	UxPutContext( DLtoDTKsearch, (char *) UxDLtoDTKsearchContext );
	UxPutClassCode( DLtoDTKsearch, _UxIfClassId );


	/* Creation of menuBar1 */
	menuBar1 = XtVaCreateManagedWidget( "menuBar1",
			xmRowColumnWidgetClass,
			DLtoDTKsearch,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 916,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNheight, 36,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( menuBar1, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar_p1 */
	menuBar_p1_shell = XtVaCreatePopupShell ("menuBar_p1_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p1 = XtVaCreateWidget( "menuBar_p1",
			xmRowColumnWidgetClass,
			menuBar_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar_p1, (char *) UxDLtoDTKsearchContext );


	/* Creation of srchWelcomeMPB */
	srchWelcomeMPB = XtVaCreateManagedWidget( "srchWelcomeMPB",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( srchWelcomeMPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_goto_welcomeCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( srchWelcomeMPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar_p1_b2 */
	menuBar_p1_b2 = XtVaCreateManagedWidget( "menuBar_p1_b2",
			xmSeparatorWidgetClass,
			menuBar_p1,
			NULL );
	UxPutContext( menuBar_p1_b2, (char *) UxDLtoDTKsearchContext );


	/* Creation of gotoDL2DTK_MPB */
	gotoDL2DTK_MPB = XtVaCreateManagedWidget( "gotoDL2DTK_MPB",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Downlink to Data-take Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( gotoDL2DTK_MPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_goto_dl2dtkCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( gotoDL2DTK_MPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar_p1_b4 */
	menuBar_p1_b4 = XtVaCreateManagedWidget( "menuBar_p1_b4",
			xmSeparatorWidgetClass,
			menuBar_p1,
			NULL );
	UxPutContext( menuBar_p1_b4, (char *) UxDLtoDTKsearchContext );


	/* Creation of closeScreenMPB */
	closeScreenMPB = XtVaCreateManagedWidget( "closeScreenMPB",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( closeScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_closeCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( closeScreenMPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar_top_b1 */
	menuBar_top_b1 = XtVaCreateManagedWidget( "menuBar_top_b1",
			xmCascadeButtonWidgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, menuBar_p1,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 10,
			NULL );
	UxPutContext( menuBar_top_b1, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_p2 */
	menuBar1_p2_shell = XtVaCreatePopupShell ("menuBar1_p2_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p2 = XtVaCreateWidget( "menuBar1_p2",
			xmRowColumnWidgetClass,
			menuBar1_p2_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p2, (char *) UxDLtoDTKsearchContext );


	/* Creation of srchExecuteSearchMPB */
	srchExecuteSearchMPB = XtVaCreateManagedWidget( "srchExecuteSearchMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Execute Search" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( srchExecuteSearchMPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_executeCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( srchExecuteSearchMPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_p2_b7 */
	menuBar1_p2_b7 = XtVaCreateManagedWidget( "menuBar1_p2_b7",
			xmSeparatorWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNbackground, "#7e88ab" ),
			NULL );
	UxPutContext( menuBar1_p2_b7, (char *) UxDLtoDTKsearchContext );


	/* Creation of srchClearSearchMPB */
	srchClearSearchMPB = XtVaCreateManagedWidget( "srchClearSearchMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Clear  Search" ),
			RES_CONVERT( XmNmnemonic, "l" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( srchClearSearchMPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_clearCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( srchClearSearchMPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_p2_b8 */
	menuBar1_p2_b8 = XtVaCreateManagedWidget( "menuBar1_p2_b8",
			xmSeparatorWidgetClass,
			menuBar1_p2,
			NULL );
	UxPutContext( menuBar1_p2_b8, (char *) UxDLtoDTKsearchContext );


	/* Creation of srchPrintScreenMPB */
	srchPrintScreenMPB = XtVaCreateManagedWidget( "srchPrintScreenMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Print  Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( srchPrintScreenMPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_printScreenCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( srchPrintScreenMPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_top_b1 */
	menuBar1_top_b1 = XtVaCreateManagedWidget( "menuBar1_top_b1",
			xmCascadeButtonGadgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, menuBar1_p2,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			NULL );
	UxPutContext( menuBar1_top_b1, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_p3 */
	menuBar1_p3_shell = XtVaCreatePopupShell ("menuBar1_p3_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p3 = XtVaCreateWidget( "menuBar1_p3",
			xmRowColumnWidgetClass,
			menuBar1_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p3, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_p3_b1 */
	menuBar1_p3_b1 = XtVaCreateManagedWidget( "menuBar1_p3_b1",
			xmPushButtonWidgetClass,
			menuBar1_p3,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( menuBar1_p3_b1, (char *) UxDLtoDTKsearchContext );


	/* Creation of menuBar1_top_b2 */
	menuBar1_top_b2 = XtVaCreateManagedWidget( "menuBar1_top_b2",
			xmCascadeButtonWidgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, menuBar1_p3,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( menuBar1_top_b2, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchPlatformLBL */
	dlSearchPlatformLBL = XtVaCreateManagedWidget( "dlSearchPlatformLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 38,
			XmNy, 85,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Platform" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 20,
			NULL );
	UxPutContext( dlSearchPlatformLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSensorLBL */
	dlSearchSensorLBL = XtVaCreateManagedWidget( "dlSearchSensorLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 153,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Sensor" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchSensorLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchActivityLBL */
	dlSearchActivityLBL = XtVaCreateManagedWidget( "dlSearchActivityLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 253,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Activity ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchActivityLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchStationLBL */
	dlSearchStationLBL = XtVaCreateManagedWidget( "dlSearchStationLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 370,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Station ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchStationLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dk2dtkAntennaLBL */
	dk2dtkAntennaLBL = XtVaCreateManagedWidget( "dk2dtkAntennaLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 509,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Antenna ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dk2dtkAntennaLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTransmitterLBL */
	dlSearchTransmitterLBL = XtVaCreateManagedWidget( "dlSearchTransmitterLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 694,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Transmitter ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTransmitterLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchPlatformSW */
	dlSearchPlatformSW = XtVaCreateManagedWidget( "dlSearchPlatformSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 91,
			XmNheight, 230,
			XmNx, 30,
			XmNy, 113,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNspacing, 4,
			XmNleftOffset, 30,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchPlatformSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of platformRC */
	platformRC = XtVaCreateManagedWidget( "platformRC",
			xmRowColumnWidgetClass,
			dlSearchPlatformSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 60,
			XmNheight, 226,
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( platformRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dummyTB */
	dummyTB = XtVaCreateManagedWidget( "dummyTB",
			xmToggleButtonWidgetClass,
			platformRC,
			XmNx, 3,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "E2" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 74,
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dummyTB, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSensorSW */
	dlSearchSensorSW = XtVaCreateManagedWidget( "dlSearchSensorSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 81,
			XmNheight, 230,
			XmNx, 141,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchSensorSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of sensorRC */
	sensorRC = XtVaCreateManagedWidget( "sensorRC",
			xmRowColumnWidgetClass,
			dlSearchSensorSW,
			XmNy, 0,
			XmNwidth, 50,
			XmNheight, 226,
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( sensorRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchActivitySW */
	dlSearchActivitySW = XtVaCreateManagedWidget( "dlSearchActivitySW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 105,
			XmNheight, 230,
			XmNx, 240,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchActivitySW, (char *) UxDLtoDTKsearchContext );


	/* Creation of activityRC */
	activityRC = XtVaCreateManagedWidget( "activityRC",
			xmRowColumnWidgetClass,
			dlSearchActivitySW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 74,
			XmNheight, 226,
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( activityRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchStationSW */
	dlSearchStationSW = XtVaCreateManagedWidget( "dlSearchStationSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 95,
			XmNheight, 230,
			XmNx, 362,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchStationSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of stationRC */
	stationRC = XtVaCreateManagedWidget( "stationRC",
			xmRowColumnWidgetClass,
			dlSearchStationSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 64,
			XmNheight, 226,
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( stationRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchAntennaSW */
	dlSearchAntennaSW = XtVaCreateManagedWidget( "dlSearchAntennaSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNheight, 230,
			XmNx, 475,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 156,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchAntennaSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of antennaRC */
	antennaRC = XtVaCreateManagedWidget( "antennaRC",
			xmRowColumnWidgetClass,
			dlSearchAntennaSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 125,
			XmNheight, 226,
			XmNresizeWidth, FALSE,
			XmNmarginWidth, 5,
			XmNspacing, 0,
			XmNresizeHeight, TRUE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( antennaRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTransmitterSW */
	dlSearchTransmitterSW = XtVaCreateManagedWidget( "dlSearchTransmitterSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 208,
			XmNheight, 230,
			XmNx, 648,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchTransmitterSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of transmitterRC */
	transmitterRC = XtVaCreateManagedWidget( "transmitterRC",
			xmRowColumnWidgetClass,
			dlSearchTransmitterSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 177,
			XmNheight, 226,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNresizeHeight, TRUE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( transmitterRC, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSeparator1 */
	dlSearchSeparator1 = XtVaCreateManagedWidget( "dlSearchSeparator1",
			xmSeparatorWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 1015,
			XmNheight, 8,
			XmNx, 26,
			XmNy, 353,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( dlSearchSeparator1, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchExecuteSearchPB */
	dlSearchExecuteSearchPB = XtVaCreateManagedWidget( "dlSearchExecuteSearchPB",
			xmPushButtonWidgetClass,
			DLtoDTKsearch,
			XmNx, 59,
			XmNy, 554,
			XmNwidth, 195,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, " EXECUTE   SEARCH " ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dlSearchExecuteSearchPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_executeCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchExecuteSearchPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchClearSearchPB */
	dlSearchClearSearchPB = XtVaCreateManagedWidget( "dlSearchClearSearchPB",
			xmPushButtonWidgetClass,
			DLtoDTKsearch,
			XmNx, 306,
			XmNy, 554,
			XmNwidth, 195,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLEAR    SEARCH" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dlSearchClearSearchPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_clearCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchClearSearchPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchCloseSearchPB */
	dlSearchCloseSearchPB = XtVaCreateManagedWidget( "dlSearchCloseSearchPB",
			xmPushButtonWidgetClass,
			DLtoDTKsearch,
			XmNx, 799,
			XmNy, 554,
			XmNwidth, 195,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dlSearchCloseSearchPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_closeCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchCloseSearchPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchPrintSearchPB */
	dlSearchPrintSearchPB = XtVaCreateManagedWidget( "dlSearchPrintSearchPB",
			xmPushButtonWidgetClass,
			DLtoDTKsearch,
			XmNx, 554,
			XmNy, 554,
			XmNwidth, 195,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "PRINT     SCREEN" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( dlSearchPrintSearchPB, XmNactivateCallback,
		(XtCallbackProc) dl_search_printScreenCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchPrintSearchPB, (char *) UxDLtoDTKsearchContext );


	/* Creation of DownlinkSearchLBL */
	DownlinkSearchLBL = XtVaCreateManagedWidget( "DownlinkSearchLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 439,
			XmNy, 47,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Downlink     Search" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 28,
			NULL );
	UxPutContext( DownlinkSearchLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnFrame */
	dlSearchTimeOnFrame = XtVaCreateManagedWidget( "dlSearchTimeOnFrame",
			xmFrameWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 455,
			XmNheight, 46,
			XmNx, 546,
			XmNy, 418,
			XmNshadowType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( dlSearchTimeOnFrame, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnForm */
	dlSearchTimeOnForm = XtVaCreateManagedWidget( "dlSearchTimeOnForm",
			xmFormWidgetClass,
			dlSearchTimeOnFrame,
			XmNwidth, 451,
			XmNheight, 46,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_OUT,
			XmNshadowThickness, 0,
			NULL );
	UxPutContext( dlSearchTimeOnForm, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnStartLBL */
	dlSearchTimeOnStartLBL = XtVaCreateManagedWidget( "dlSearchTimeOnStartLBL",
			xmLabelWidgetClass,
			dlSearchTimeOnForm,
			XmNx, 6,
			XmNy, 11,
			RES_CONVERT( XmNlabelString, "Start:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchTimeOnStartLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnEndLBL */
	dlSearchTimeOnEndLBL = XtVaCreateManagedWidget( "dlSearchTimeOnEndLBL",
			xmLabelWidgetClass,
			dlSearchTimeOnForm,
			XmNx, 230,
			XmNy, 11,
			RES_CONVERT( XmNlabelString, "End:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchTimeOnEndLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnStartText */
	dlSearchTimeOnStartText = XtVaCreateManagedWidget( "dlSearchTimeOnStartText",
			xmTextWidgetClass,
			dlSearchTimeOnForm,
			XmNwidth, 160,
			XmNx, 62,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchTimeOnStartText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_time_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchTimeOnStartText, XmNactivateCallback,
		(XtCallbackProc) dl_search_time_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchTimeOnStartText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnEndText */
	dlSearchTimeOnEndText = XtVaCreateManagedWidget( "dlSearchTimeOnEndText",
			xmTextWidgetClass,
			dlSearchTimeOnForm,
			XmNwidth, 160,
			XmNx, 278,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNcursorPosition, 0,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchTimeOnEndText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_time_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchTimeOnEndText, XmNactivateCallback,
		(XtCallbackProc) dl_search_time_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchTimeOnEndText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOnLBL */
	dlSearchTimeOnLBL = XtVaCreateManagedWidget( "dlSearchTimeOnLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 459,
			XmNy, 431,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time On:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTimeOnLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffLBL */
	dlSearchTimeOffLBL = XtVaCreateManagedWidget( "dlSearchTimeOffLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 457,
			XmNy, 483,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time Off:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTimeOffLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffFrame */
	dlSearchTimeOffFrame = XtVaCreateManagedWidget( "dlSearchTimeOffFrame",
			xmFrameWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 455,
			XmNheight, 46,
			XmNx, 546,
			XmNy, 466,
			XmNshadowType, XmSHADOW_ETCHED_IN,
			NULL );
	UxPutContext( dlSearchTimeOffFrame, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffForm */
	dlSearchTimeOffForm = XtVaCreateManagedWidget( "dlSearchTimeOffForm",
			xmFormWidgetClass,
			dlSearchTimeOffFrame,
			XmNwidth, 405,
			XmNheight, 42,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTimeOffForm, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffStartLBL */
	dlSearchTimeOffStartLBL = XtVaCreateManagedWidget( "dlSearchTimeOffStartLBL",
			xmLabelWidgetClass,
			dlSearchTimeOffForm,
			XmNx, 6,
			XmNy, 11,
			RES_CONVERT( XmNlabelString, "Start:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTimeOffStartLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffEndLBL */
	dlSearchTimeOffEndLBL = XtVaCreateManagedWidget( "dlSearchTimeOffEndLBL",
			xmLabelWidgetClass,
			dlSearchTimeOffForm,
			XmNx, 230,
			XmNy, 10,
			RES_CONVERT( XmNlabelString, "End:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 45,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchTimeOffEndLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffStartText */
	dlSearchTimeOffStartText = XtVaCreateManagedWidget( "dlSearchTimeOffStartText",
			xmTextWidgetClass,
			dlSearchTimeOffForm,
			XmNwidth, 160,
			XmNx, 62,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			NULL );
	XtAddCallback( dlSearchTimeOffStartText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_time_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchTimeOffStartText, XmNactivateCallback,
		(XtCallbackProc) dl_search_time_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchTimeOffStartText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchTimeOffEndText */
	dlSearchTimeOffEndText = XtVaCreateManagedWidget( "dlSearchTimeOffEndText",
			xmTextWidgetClass,
			dlSearchTimeOffForm,
			XmNwidth, 160,
			XmNx, 277,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			NULL );
	XtAddCallback( dlSearchTimeOffEndText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_time_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchTimeOffEndText, XmNactivateCallback,
		(XtCallbackProc) dl_search_time_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchTimeOffEndText, (char *) UxDLtoDTKsearchContext );


	/* Creation of frame2 */
	frame2 = XtVaCreateManagedWidget( "frame2",
			xmFrameWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 455,
			XmNheight, 47,
			XmNx, 546,
			XmNy, 370,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( frame2, (char *) UxDLtoDTKsearchContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			frame2,
			XmNx, 3,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " Date Formats:  YYYY-DDDTHH:MM:SS.fff\n               YYYY-MM-DD:HH:MM:SS.fff" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 449,
			XmNheight, 40,
			NULL );
	UxPutContext( label2, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSeparator2 */
	dlSearchSeparator2 = XtVaCreateManagedWidget( "dlSearchSeparator2",
			xmSeparatorWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 1015,
			XmNheight, 8,
			XmNx, 26,
			XmNy, 524,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( dlSearchSeparator2, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchScheduleLinkLBL */
	dlSearchScheduleLinkLBL = XtVaCreateManagedWidget( "dlSearchScheduleLinkLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 65,
			XmNy, 377,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "FA Schedule Link:" ),
			XmNwidth, 163,
			XmNheight, 23,
			NULL );
	UxPutContext( dlSearchScheduleLinkLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchScheduleLinkTF */
	dlSearchScheduleLinkTF = XtVaCreateManagedWidget( "dlSearchScheduleLinkTF",
			xmTextFieldWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 200,
			XmNx, 236,
			XmNy, 371,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, TRUE,
			NULL );
	XtAddCallback( dlSearchScheduleLinkTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( dlSearchScheduleLinkTF, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevolutionLBL */
	dlSearchRevolutionLBL = XtVaCreateManagedWidget( "dlSearchRevolutionLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 58,
			XmNy, 430,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Revolution:" ),
			XmNwidth, 95,
			XmNheight, 23,
			NULL );
	UxPutContext( dlSearchRevolutionLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevFrame */
	dlSearchRevFrame = XtVaCreateManagedWidget( "dlSearchRevFrame",
			xmFrameWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 282,
			XmNheight, 46,
			XmNx, 156,
			XmNy, 418,
			XmNshadowType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( dlSearchRevFrame, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevForm */
	dlSearchRevForm = XtVaCreateManagedWidget( "dlSearchRevForm",
			xmFormWidgetClass,
			dlSearchRevFrame,
			XmNwidth, 285,
			XmNheight, 47,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_OUT,
			XmNshadowThickness, 0,
			NULL );
	UxPutContext( dlSearchRevForm, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevStartLBL */
	dlSearchRevStartLBL = XtVaCreateManagedWidget( "dlSearchRevStartLBL",
			xmLabelWidgetClass,
			dlSearchRevForm,
			XmNx, 3,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "Start:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchRevStartLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevEndLBL */
	dlSearchRevEndLBL = XtVaCreateManagedWidget( "dlSearchRevEndLBL",
			xmLabelWidgetClass,
			dlSearchRevForm,
			XmNx, 146,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "End:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchRevEndLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevStartText */
	dlSearchRevStartText = XtVaCreateManagedWidget( "dlSearchRevStartText",
			xmTextWidgetClass,
			dlSearchRevForm,
			XmNwidth, 70,
			XmNx, 59,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchRevStartText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_rev_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchRevStartText, XmNactivateCallback,
		(XtCallbackProc) dl_search_rev_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchRevStartText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchRevEndText */
	dlSearchRevEndText = XtVaCreateManagedWidget( "dlSearchRevEndText",
			xmTextWidgetClass,
			dlSearchRevForm,
			XmNwidth, 70,
			XmNx, 194,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNcursorPosition, 0,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchRevEndText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_rev_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchRevEndText, XmNactivateCallback,
		(XtCallbackProc) dl_search_rev_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchRevEndText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceLBL */
	dlSearchSequenceLBL = XtVaCreateManagedWidget( "dlSearchSequenceLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 65,
			XmNy, 478,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Sequence:" ),
			XmNwidth, 88,
			XmNheight, 23,
			NULL );
	UxPutContext( dlSearchSequenceLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceFrame */
	dlSearchSequenceFrame = XtVaCreateManagedWidget( "dlSearchSequenceFrame",
			xmFrameWidgetClass,
			DLtoDTKsearch,
			XmNwidth, 282,
			XmNheight, 46,
			XmNx, 156,
			XmNy, 466,
			XmNshadowType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( dlSearchSequenceFrame, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceForm */
	dlSearchSequenceForm = XtVaCreateManagedWidget( "dlSearchSequenceForm",
			xmFormWidgetClass,
			dlSearchSequenceFrame,
			XmNwidth, 285,
			XmNheight, 47,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_OUT,
			XmNshadowThickness, 0,
			NULL );
	UxPutContext( dlSearchSequenceForm, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceStartLBL */
	dlSearchSequenceStartLBL = XtVaCreateManagedWidget( "dlSearchSequenceStartLBL",
			xmLabelWidgetClass,
			dlSearchSequenceForm,
			XmNx, 2,
			XmNy, 11,
			RES_CONVERT( XmNlabelString, "Start:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchSequenceStartLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceEndLBL */
	dlSearchSequenceEndLBL = XtVaCreateManagedWidget( "dlSearchSequenceEndLBL",
			xmLabelWidgetClass,
			dlSearchSequenceForm,
			XmNx, 145,
			XmNy, 11,
			RES_CONVERT( XmNlabelString, "End:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 22,
			NULL );
	UxPutContext( dlSearchSequenceEndLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceStartText */
	dlSearchSequenceStartText = XtVaCreateManagedWidget( "dlSearchSequenceStartText",
			xmTextWidgetClass,
			dlSearchSequenceForm,
			XmNwidth, 70,
			XmNx, 58,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchSequenceStartText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_sequence_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchSequenceStartText, XmNactivateCallback,
		(XtCallbackProc) dl_search_sequence_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchSequenceStartText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchSequenceEndText */
	dlSearchSequenceEndText = XtVaCreateManagedWidget( "dlSearchSequenceEndText",
			xmTextWidgetClass,
			dlSearchSequenceForm,
			XmNwidth, 70,
			XmNx, 193,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcolumns, 10,
			XmNcursorPosition, 0,
			XmNvalue, "",
			NULL );
	XtAddCallback( dlSearchSequenceEndText, XmNmodifyVerifyCallback,
		(XtCallbackProc) dl_search_sequence_modifyVerifyCb,
		(XtPointer) UxDLtoDTKsearchContext );
	XtAddCallback( dlSearchSequenceEndText, XmNactivateCallback,
		(XtCallbackProc) dl_search_sequence_loseFocusCb,
		(XtPointer) UxDLtoDTKsearchContext );

	UxPutContext( dlSearchSequenceEndText, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchStatusLBL */
	dlSearchStatusLBL = XtVaCreateManagedWidget( "dlSearchStatusLBL",
			xmLabelWidgetClass,
			DLtoDTKsearch,
			XmNx, 887,
			XmNy, 84,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Downlink Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( dlSearchStatusLBL, (char *) UxDLtoDTKsearchContext );


	/* Creation of dlSearchStatusSW */
	dlSearchStatusSW = XtVaCreateManagedWidget( "dlSearchStatusSW",
			xmScrolledWindowWidgetClass,
			DLtoDTKsearch,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 165,
			XmNheight, 230,
			XmNx, 872,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( dlSearchStatusSW, (char *) UxDLtoDTKsearchContext );


	/* Creation of statusRC */
	statusRC = XtVaCreateManagedWidget( "statusRC",
			xmRowColumnWidgetClass,
			dlSearchStatusSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 134,
			XmNheight, 226,
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( statusRC, (char *) UxDLtoDTKsearchContext );

	XtVaSetValues(menuBar1,
			XmNmenuHelpWidget, menuBar1_top_b2,
			NULL );


	XtAddCallback( DLtoDTKsearch, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxDLtoDTKsearchContext);


	return ( DLtoDTKsearch );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_DLtoDTKsearch( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCDLtoDTKsearch       *UxContext;
	static int		_Uxinit = 0;

	UxDLtoDTKsearchContext = UxContext =
		(_UxCDLtoDTKsearch *) UxNewContext( sizeof(_UxCDLtoDTKsearch), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_DLtoDTKsearch();

	XtAddCallback( dlSearchTimeOnStartText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchTimeOnEndText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchTimeOffStartText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchTimeOffEndText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchRevStartText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchRevEndText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchSequenceStartText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	XtAddCallback( dlSearchSequenceEndText, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

