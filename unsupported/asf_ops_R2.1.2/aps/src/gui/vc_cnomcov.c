
/*******************************************************************************
	vc_cnomcov.c

       Associated Header file: vc_cnomcov.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

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
#pragma ident   "@(#)vc_cnomcov.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_cnomcov.c"

#include <stdlib.h>

#include "cb_cnomcov.h"
#include "cb_datetime.h"
#include "satmenus.h"

extern Widget cnomcov_form ;
extern Widget datetime_form ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_cnomcov.h"
#undef CONTEXT_MACRO_ACCESS

Widget	subMenu_covtype_STN;
Widget	subMenu_covtype_GBL;
Widget	optionMenu_covtype;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_cnomcov_quit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCCreateNominalCoverage *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxCreateNominalCoverageContext;
	UxCreateNominalCoverageContext = UxContext =
			(_UxCCreateNominalCoverage *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(cnomcov_form)) ;
	}
	UxCreateNominalCoverageContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CreateNominalCoverage()
{
	Widget		_UxParent;
	Widget		subMenu_ncov_sensor_shell;
	Widget		subMenu_ncov_sat_shell;
	Widget		subMenu_covtype_shell;


	/* Creation of CreateNominalCoverage */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "CreateNominalCoverage_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 309,
			XmNy, 90,
			XmNwidth, 631,
			XmNheight, 709,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CreateNominalCoverage",
			XmNiconName, "CreateNominalCoverage",
			NULL );

	}

	CreateNominalCoverage = XtVaCreateManagedWidget( "CreateNominalCoverage",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 631,
			XmNheight, 709,
			XmNresizePolicy, XmRESIZE_ANY,
			XmNunitType, XmPIXELS,
			XmNnoResize, TRUE,
			RES_CONVERT( XmNdialogTitle, "APS:CREATE NOMINAL COVERAGE" ),
			NULL );
	UxPutContext( CreateNominalCoverage, (char *) UxCreateNominalCoverageContext );
	UxPutClassCode( CreateNominalCoverage, _UxIfClassId );


	/* Creation of label29 */
	label29 = XtVaCreateManagedWidget( "label29",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 10,
			XmNy, 10,
			XmNwidth, 600,
			XmNheight, 45,
			RES_CONVERT( XmNlabelString, "CREATE  NOMINAL  COVERAGE" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label29, (char *) UxCreateNominalCoverageContext );


	/* Creation of label30 */
	label30 = XtVaCreateManagedWidget( "label30",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 65,
			XmNy, 70,
			XmNwidth, 520,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "EPHEMERIS          SAT     PHASE      START TIME       START REV" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label30, (char *) UxCreateNominalCoverageContext );


	/* Creation of label39 */
	label39 = XtVaCreateManagedWidget( "label39",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 320,
			XmNy, 359,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label39, (char *) UxCreateNominalCoverageContext );


	/* Creation of label41 */
	label41 = XtVaCreateManagedWidget( "label41",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 225,
			XmNwidth, 125,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "COVERAGE PERIOD" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label41, (char *) UxCreateNominalCoverageContext );


	/* Creation of TF_ephemeris_file */
	TF_ephemeris_file = XtVaCreateManagedWidget( "TF_ephemeris_file",
			xmTextFieldWidgetClass,
			CreateNominalCoverage,
			XmNx, 95,
			XmNy, 258,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNwidth, 170,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_ephemeris_file, (char *) UxCreateNominalCoverageContext );


	/* Creation of label40 */
	label40 = XtVaCreateManagedWidget( "label40",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 21,
			XmNy, 259,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SELECTED \nEPHEMERIS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label40, (char *) UxCreateNominalCoverageContext );


	/* Creation of separator3 */
	separator3 = XtVaCreateManagedWidget( "separator3",
			xmSeparatorWidgetClass,
			CreateNominalCoverage,
			XmNwidth, 633,
			XmNheight, 10,
			XmNx, 0,
			XmNy, 210,
			NULL );
	UxPutContext( separator3, (char *) UxCreateNominalCoverageContext );


	/* Creation of separator4 */
	separator4 = XtVaCreateManagedWidget( "separator4",
			xmSeparatorWidgetClass,
			CreateNominalCoverage,
			XmNwidth, 633,
			XmNheight, 14,
			XmNx, 0,
			XmNy, 493,
			NULL );
	UxPutContext( separator4, (char *) UxCreateNominalCoverageContext );


	/* Creation of label43 */
	label43 = XtVaCreateManagedWidget( "label43",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 94,
			XmNy, 225,
			XmNwidth, 165,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "COVERAGE INFORMATION" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label43, (char *) UxCreateNominalCoverageContext );


	/* Creation of toggleButton_stoptime */
	toggleButton_stoptime = XtVaCreateManagedWidget( "toggleButton_stoptime",
			xmToggleButtonWidgetClass,
			CreateNominalCoverage,
			XmNx, 335,
			XmNy, 400,
			RES_CONVERT( XmNlabelString, "REPLICATE ORBITS FROM EPHEMERIS FILE" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNset, TRUE,
			XmNindicatorSize, 20,
			NULL );
	XtAddCallback( toggleButton_stoptime, XmNvalueChangedCallback,
		(XtCallbackProc) cb_adjust_phase_stoptime,
		(XtPointer) UxCreateNominalCoverageContext );

	UxPutContext( toggleButton_stoptime, (char *) UxCreateNominalCoverageContext );


	/* Creation of pushButton_cnomcov_quit */
	pushButton_cnomcov_quit = XtVaCreateManagedWidget( "pushButton_cnomcov_quit",
			xmPushButtonWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 450,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_cnomcov_quit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_cnomcov_quit,
		(XtPointer) UxCreateNominalCoverageContext );

	UxPutContext( pushButton_cnomcov_quit, (char *) UxCreateNominalCoverageContext );


	/* Creation of pushButton_create_coverage */
	pushButton_create_coverage = XtVaCreateManagedWidget( "pushButton_create_coverage",
			xmPushButtonWidgetClass,
			CreateNominalCoverage,
			XmNx, 95,
			XmNy, 450,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_create_coverage, XmNactivateCallback,
		(XtCallbackProc) cb_do_create_coverage,
		(XtPointer) UxCreateNominalCoverageContext );

	UxPutContext( pushButton_create_coverage, (char *) UxCreateNominalCoverageContext );


	/* Creation of TF_total_days */
	TF_total_days = XtVaCreateManagedWidget( "TF_total_days",
			xmTextFieldWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 358,
			XmNheight, 32,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "00000.00",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNwidth, 83,
			NULL );
	XtAddCallback( TF_total_days, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_float_chars );
	XtAddCallback( TF_total_days, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_total_days, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_total_days, (char *) UxCreateNominalCoverageContext );


	/* Creation of label44 */
	label44 = XtVaCreateManagedWidget( "label44",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 21,
			XmNy, 508,
			XmNwidth, 590,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label44, (char *) UxCreateNominalCoverageContext );


	/* Creation of scrolledWindowText1 */
	scrolledWindowText1 = XtVaCreateManagedWidget( "scrolledWindowText1",
			xmScrolledWindowWidgetClass,
			CreateNominalCoverage,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 28,
			XmNy, 532,
			XmNwidth, 590,
			XmNheight, 160,
			NULL );
	UxPutContext( scrolledWindowText1, (char *) UxCreateNominalCoverageContext );


	/* Creation of scrolledText_cnomcov_status */
	scrolledText_cnomcov_status = XtVaCreateManagedWidget( "scrolledText_cnomcov_status",
			xmTextWidgetClass,
			scrolledWindowText1,
			XmNwidth, 571,
			XmNheight, 150,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( scrolledText_cnomcov_status, (char *) UxCreateNominalCoverageContext );


	/* Creation of pushButton_refresh */
	pushButton_refresh = XtVaCreateManagedWidget( "pushButton_refresh",
			xmPushButtonWidgetClass,
			CreateNominalCoverage,
			XmNx, 35,
			XmNy, 90,
			XmNwidth, 25,
			XmNheight, 110,
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( pushButton_refresh, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_ncov_sensor */
	subMenu_ncov_sensor_shell = XtVaCreatePopupShell ("subMenu_ncov_sensor_shell",
			xmMenuShellWidgetClass, CreateNominalCoverage,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_ncov_sensor = XtVaCreateWidget( "subMenu_ncov_sensor",
			xmRowColumnWidgetClass,
			subMenu_ncov_sensor_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			NULL );
	XtAddCallback( subMenu_ncov_sensor, XmNentryCallback,
		(XtCallbackProc) cb_update_coverage_filename,
		(XtPointer) "SENSOR" );

	UxPutContext( subMenu_ncov_sensor, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_ncov_sensor_SAR */
	subMenu_ncov_sensor_SAR = XtVaCreateManagedWidget( "subMenu_ncov_sensor_SAR",
			xmPushButtonWidgetClass,
			subMenu_ncov_sensor,
			RES_CONVERT( XmNlabelString, "SAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( subMenu_ncov_sensor_SAR, (char *) UxCreateNominalCoverageContext );

	cb_build_cvrg_allowed_sensor_option_menu( subMenu_ncov_sensor_SAR,
			(XtPointer) UxCreateNominalCoverageContext, (XtPointer) NULL );


	/* Creation of optionMenu_ncov_sensor */
	optionMenu_ncov_sensor = XtVaCreateManagedWidget( "optionMenu_ncov_sensor",
			xmRowColumnWidgetClass,
			CreateNominalCoverage,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_ncov_sensor,
			XmNx, 198,
			XmNy, 303,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "/" ),
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( optionMenu_ncov_sensor, (char *) UxCreateNominalCoverageContext );


	/* Creation of scrolledWindowList2 */
	scrolledWindowList2 = XtVaCreateManagedWidget( "scrolledWindowList2",
			xmScrolledWindowWidgetClass,
			CreateNominalCoverage,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 66,
			XmNy, 95,
			XmNwidth, 515,
			NULL );
	UxPutContext( scrolledWindowList2, (char *) UxCreateNominalCoverageContext );


	/* Creation of scrolledList_ephm */
	scrolledList_ephm = XtVaCreateManagedWidget( "scrolledList_ephm",
			xmListWidgetClass,
			scrolledWindowList2,
			XmNwidth, 460,
			XmNheight, 100,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE,
			XmNvisibleItemCount, 5,
			XmNautomaticSelection, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			NULL );
	XtAddCallback( scrolledList_ephm, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_cnomcov_form,
		(XtPointer) UxCreateNominalCoverageContext );

	UxPutContext( scrolledList_ephm, (char *) UxCreateNominalCoverageContext );


	/* Creation of LBL_coverage_filename */
	LBL_coverage_filename = XtVaCreateManagedWidget( "LBL_coverage_filename",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 21,
			XmNy, 399,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "OUTPUT \nFILENAME:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( LBL_coverage_filename, (char *) UxCreateNominalCoverageContext );


	/* Creation of TF_coverage_filename */
	TF_coverage_filename = XtVaCreateManagedWidget( "TF_coverage_filename",
			xmTextFieldWidgetClass,
			CreateNominalCoverage,
			XmNx, 95,
			XmNy, 398,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNwidth, 215,
			XmNtraversalOn, FALSE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( TF_coverage_filename, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_ncov_sat */
	subMenu_ncov_sat_shell = XtVaCreatePopupShell ("subMenu_ncov_sat_shell",
			xmMenuShellWidgetClass, CreateNominalCoverage,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_ncov_sat = XtVaCreateWidget( "subMenu_ncov_sat",
			xmRowColumnWidgetClass,
			subMenu_ncov_sat_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			XmNmenuPost, "",
			NULL );
	UxPutContext( subMenu_ncov_sat, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_ncov_sat_ERS1 */
	subMenu_ncov_sat_ERS1 = XtVaCreateManagedWidget( "subMenu_ncov_sat_ERS1",
			xmPushButtonWidgetClass,
			subMenu_ncov_sat,
			RES_CONVERT( XmNlabelString, "RADARSAT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( subMenu_ncov_sat_ERS1, (char *) UxCreateNominalCoverageContext );

	cb_build_cvrg_allowed_satellite_option_menu( subMenu_ncov_sat_ERS1,
			(XtPointer) UxCreateNominalCoverageContext, (XtPointer) NULL );


	/* Creation of optionMenu_ncov_sat */
	optionMenu_ncov_sat = XtVaCreateManagedWidget( "optionMenu_ncov_sat",
			xmRowColumnWidgetClass,
			CreateNominalCoverage,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_ncov_sat,
			XmNx, 25,
			XmNy, 302,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "SATELLITE\n  /SENSOR:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_ncov_sat, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_covtype */
	subMenu_covtype_shell = XtVaCreatePopupShell ("subMenu_covtype_shell",
			xmMenuShellWidgetClass, CreateNominalCoverage,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_covtype = XtVaCreateWidget( "subMenu_covtype",
			xmRowColumnWidgetClass,
			subMenu_covtype_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 48,
			XmNresizeHeight, FALSE,
			XmNx, 0,
			XmNy, 327,
			XmNsensitive, TRUE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	XtAddCallback( subMenu_covtype, XmNentryCallback,
		(XtCallbackProc) cb_set_coverage_type,
		(XtPointer) UxCreateNominalCoverageContext );

	UxPutContext( subMenu_covtype, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_covtype_STN */
	subMenu_covtype_STN = XtVaCreateManagedWidget( "subMenu_covtype_STN",
			xmPushButtonWidgetClass,
			subMenu_covtype,
			RES_CONVERT( XmNlabelString, "STN MASK" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 353,
			NULL );
	UxPutContext( subMenu_covtype_STN, (char *) UxCreateNominalCoverageContext );


	/* Creation of subMenu_covtype_GBL */
	subMenu_covtype_GBL = XtVaCreateManagedWidget( "subMenu_covtype_GBL",
			xmPushButtonWidgetClass,
			subMenu_covtype,
			RES_CONVERT( XmNlabelString, "GLOBAL" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 353,
			NULL );
	UxPutContext( subMenu_covtype_GBL, (char *) UxCreateNominalCoverageContext );


	/* Creation of optionMenu_covtype */
	optionMenu_covtype = XtVaCreateManagedWidget( "optionMenu_covtype",
			xmRowColumnWidgetClass,
			CreateNominalCoverage,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_covtype,
			XmNx, 31,
			XmNy, 351,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "COVERAGE\n    TYPE:" ),
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( optionMenu_covtype, (char *) UxCreateNominalCoverageContext );


	/* Creation of T_PHASE_END */
	T_PHASE_END = XtVaCreateManagedWidget( "T_PHASE_END",
			xmTextFieldWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 310,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			NULL );
	XtAddCallback( T_PHASE_END, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );
	XtAddCallback( T_PHASE_END, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Coverage Stop Time" );
	XtAddCallback( T_PHASE_END, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( T_PHASE_END, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( T_PHASE_END, (char *) UxCreateNominalCoverageContext );


	/* Creation of T_PHASE_START */
	T_PHASE_START = XtVaCreateManagedWidget( "T_PHASE_START",
			xmTextFieldWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 260,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			NULL );
	XtAddCallback( T_PHASE_START, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );
	XtAddCallback( T_PHASE_START, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Coverage Start Time" );
	XtAddCallback( T_PHASE_START, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( T_PHASE_START, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( T_PHASE_START, (char *) UxCreateNominalCoverageContext );


	/* Creation of label28 */
	label28 = XtVaCreateManagedWidget( "label28",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 292,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss:ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label28, (char *) UxCreateNominalCoverageContext );


	/* Creation of label31 */
	label31 = XtVaCreateManagedWidget( "label31",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 395,
			XmNy, 341,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss:ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label31, (char *) UxCreateNominalCoverageContext );


	/* Creation of label32 */
	label32 = XtVaCreateManagedWidget( "label32",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 320,
			XmNy, 261,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label32, (char *) UxCreateNominalCoverageContext );


	/* Creation of label35 */
	label35 = XtVaCreateManagedWidget( "label35",
			xmLabelWidgetClass,
			CreateNominalCoverage,
			XmNx, 320,
			XmNy, 311,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, " STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label35, (char *) UxCreateNominalCoverageContext );


	XtAddCallback( CreateNominalCoverage, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCreateNominalCoverageContext);


	return ( CreateNominalCoverage );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CreateNominalCoverage( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCCreateNominalCoverage *UxContext;
	static int		_Uxinit = 0;

	UxCreateNominalCoverageContext = UxContext =
		(_UxCCreateNominalCoverage *) UxNewContext( sizeof(_UxCCreateNominalCoverage), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		OPTION_MENU_WIDGETS *sensor_menu ;
		PERIOD_WIDGETS *ASFperiod ;
		
		Widget selected_sat ;
		char *sat_name ;
		char *sat_code ;
		rtrn = _Uxbuild_CreateNominalCoverage();

		sensor_menu =
			(OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;
		ASFperiod = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		ASFperiod->start = (Widget) T_PHASE_START ;
		ASFperiod->stop = (Widget) T_PHASE_END ;
		
		sensor_menu->optionmenu = (Widget) optionMenu_ncov_sensor ;
		sensor_menu->submenu = (Widget) subMenu_ncov_sensor ;
		
		/* set the coverage type based on which sat is selected */
		XtVaGetValues( subMenu_ncov_sat,
			XmNmenuHistory, &selected_sat,
			NULL ) ;
		sat_name = XtName( selected_sat ) ;
		sat_code = get_satellite_code( sat_name ) ;
		/* if can't set coverage type, leave it as is */
		if (sat_code != NULL)
			(void) set_coverage_type( sat_code ) ;
		
		XtUnmanageChild(toggleButton_stoptime) ;
		
#ifndef DEBUG
		/* not DEBUG: don't create a coverage file (see the cb_*.c file also) */
		XtUnmanageChild( LBL_coverage_filename ) ;
		XtUnmanageChild( TF_coverage_filename ) ;
#endif
		 
		/* 
		-- add the callback for the refresh button here
		-- this ensures the ScrolledList widget has been 
		-- created and can be properly passed as client
		-- data
		*/
		XtAddCallback( pushButton_refresh, XmNactivateCallback,
		        (XtCallbackProc) cb_show_coverage_relations,
		        (XtPointer) scrolledList_ephm );
		 
		XtAddCallback(subMenu_ncov_sat, XmNentryCallback,
			(XtCallbackProc) cb_set_cvrg_allowed_sensor_menus,
			(XtPointer) sensor_menu) ;
		
		XtAddCallback(TF_total_days, XmNactivateCallback,
			(XtCallbackProc) cb_adjust_ASF_datetimes,
			(XtPointer) ASFperiod) ;
		
		/* add cb for popup */
		XtAddCallback(XtParent(rtrn), XtNpopupCallback,
			cb_show_coverage_relations, (XtPointer *) scrolledList_ephm) ;
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

