
/*******************************************************************************
	vc_crtapsfile.c

       Associated Header file: vc_crtapsfile.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/ToggleB.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
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
#pragma ident   "@(#)vc_crtapsfile.c	5.2 98/03/12 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_crtapsfile.c"
#include <stdlib.h>

#include "cb_datetime.h"
#include "cb_crtapsfile.h"
#include "cb_fileviewer.h"

extern Widget	apsfilegen_form ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_crtapsfile.h"
#undef CONTEXT_MACRO_ACCESS

Widget	scrolledText_create_report;
Widget	ExtendTimes_tb;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSFileGeneration()
{
	Widget		_UxParent;


	/* Creation of APSFileGeneration */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "APSFileGeneration_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 464,
			XmNy, 20,
			XmNwidth, 623,
			XmNheight, 720,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSFileGeneration",
			XmNiconName, "APSFileGeneration",
			NULL );

	}

	APSFileGeneration = XtVaCreateManagedWidget( "APSFileGeneration",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 623,
			XmNheight, 720,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:File Generation" ),
			NULL );
	UxPutContext( APSFileGeneration, (char *) UxAPSFileGenerationContext );
	UxPutClassCode( APSFileGeneration, _UxIfClassId );


	/* Creation of label46 */
	label46 = XtVaCreateManagedWidget( "label46",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 10,
			XmNy, 5,
			XmNwidth, 590,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "APS  FILE  GENERATION" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label46, (char *) UxAPSFileGenerationContext );


	/* Creation of scrolledWindowList4 */
	scrolledWindowList4 = XtVaCreateManagedWidget( "scrolledWindowList4",
			xmScrolledWindowWidgetClass,
			APSFileGeneration,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 61,
			XmNy, 64,
			XmNwidth, 500,
			XmNheight, 190,
			NULL );
	UxPutContext( scrolledWindowList4, (char *) UxAPSFileGenerationContext );


	/* Creation of scrolledList_reports */
	scrolledList_reports = XtVaCreateManagedWidget( "scrolledList_reports",
			xmListWidgetClass,
			scrolledWindowList4,
			XmNwidth, 484,
			XmNheight, 190,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNlistSizePolicy, XmCONSTANT,
			XmNautomaticSelection, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNitemCount, 16,
			RES_CONVERT( XmNitems, "AWOS           ASF Weekly Operation Schedule,MWOS           McMurdo Weekly Operation Schedule,MWOS           McMurdo Weekly Operation Schedule,MWO3           McMurdo Weekly Operation Schedule,MWO4           McMurdo Weekly Operation Schedule,MWO5           McMurdo Weekly Operation Schedule,MWO6           McMurdo Weekly Operation Schedule,MWO7           McMurdo Weekly Operation Schedule,MWO8           McMurdo Weekly Operation Schedule,MWO9           McMurdo Weekly Operation Schedule,MW10           McMurdo Weekly Operation Schedule,MW11           McMurdo Weekly Operation Schedule,MW12           McMurdo Weekly Operation Schedule,MW13           McMurdo Weekly Operation Schedule,MW14           McMurdo Weekly Operation Schedule,MW15           McMurdo Weekly Operation Schedule" ),
			XmNlistMarginHeight, 0,
			NULL );
	XtAddCallback( scrolledList_reports, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_capsfile_form,
		(XtPointer) UxAPSFileGenerationContext );

	UxPutContext( scrolledList_reports, (char *) UxAPSFileGenerationContext );


	/* Creation of separator6 */
	separator6 = XtVaCreateManagedWidget( "separator6",
			xmSeparatorWidgetClass,
			APSFileGeneration,
			XmNwidth, 619,
			XmNheight, 10,
			XmNx, 2,
			XmNy, 265,
			NULL );
	UxPutContext( separator6, (char *) UxAPSFileGenerationContext );


	/* Creation of label64 */
	label64 = XtVaCreateManagedWidget( "label64",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 75,
			XmNy, 323,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			NULL );
	UxPutContext( label64, (char *) UxAPSFileGenerationContext );


	/* Creation of label69 */
	label69 = XtVaCreateManagedWidget( "label69",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 75,
			XmNy, 373,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, " STOP TIME:" ),
			NULL );
	UxPutContext( label69, (char *) UxAPSFileGenerationContext );


	/* Creation of pushButton_CreateReportFile */
	pushButton_CreateReportFile = XtVaCreateManagedWidget( "pushButton_CreateReportFile",
			xmPushButtonWidgetClass,
			APSFileGeneration,
			XmNx, 44,
			XmNy, 471,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_CreateReportFile, XmNactivateCallback,
		(XtCallbackProc) cb_create_report,
		(XtPointer) UxAPSFileGenerationContext );

	UxPutContext( pushButton_CreateReportFile, (char *) UxAPSFileGenerationContext );


	/* Creation of pushButton_APSFileGenQuit */
	pushButton_APSFileGenQuit = XtVaCreateManagedWidget( "pushButton_APSFileGenQuit",
			xmPushButtonWidgetClass,
			APSFileGeneration,
			XmNx, 455,
			XmNy, 471,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNsensitive, TRUE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_APSFileGenQuit, XmNactivateCallback,
		(XtCallbackProc) cb_quit_filegen,
		(XtPointer) UxAPSFileGenerationContext );

	UxPutContext( pushButton_APSFileGenQuit, (char *) UxAPSFileGenerationContext );


	/* Creation of scrolledWindowText4 */
	scrolledWindowText4 = XtVaCreateManagedWidget( "scrolledWindowText4",
			xmScrolledWindowWidgetClass,
			APSFileGeneration,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 11,
			XmNy, 551,
			XmNwidth, 590,
			XmNheight, 160,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( scrolledWindowText4, (char *) UxAPSFileGenerationContext );


	/* Creation of scrolledText_create_report */
	scrolledText_create_report = XtVaCreateManagedWidget( "scrolledText_create_report",
			xmTextWidgetClass,
			scrolledWindowText4,
			XmNwidth, 485,
			XmNheight, 141,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( scrolledText_create_report, (char *) UxAPSFileGenerationContext );


	/* Creation of label75 */
	label75 = XtVaCreateManagedWidget( "label75",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 75,
			XmNy, 423,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			NULL );
	UxPutContext( label75, (char *) UxAPSFileGenerationContext );


	/* Creation of textField_reportname */
	textField_reportname = XtVaCreateManagedWidget( "textField_reportname",
			xmTextFieldWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 281,
			XmNheight, 32,
			XmNcursorPositionVisible, TRUE,
			XmNeditable, TRUE,
			XmNvalue, "",
			XmNcolumns, 30,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 1000,
			XmNwidth, 410,
			NULL );
	UxPutContext( textField_reportname, (char *) UxAPSFileGenerationContext );


	/* Creation of label77 */
	label77 = XtVaCreateManagedWidget( "label77",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 64,
			XmNy, 46,
			XmNwidth, 505,
			XmNheight, 16,
			RES_CONVERT( XmNlabelString, "REPORT ID      REPORT NAME" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label77, (char *) UxAPSFileGenerationContext );


	/* Creation of TF_report_total_days */
	TF_report_total_days = XtVaCreateManagedWidget( "TF_report_total_days",
			xmTextFieldWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 421,
			XmNheight, 31,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "00000.00",
			XmNcursorPositionVisible, TRUE,
			XmNeditable, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNwidth, 83,
			NULL );
	XtAddCallback( TF_report_total_days, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_float_chars );
	XtAddCallback( TF_report_total_days, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_report_total_days, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_report_total_days, (char *) UxAPSFileGenerationContext );


	/* Creation of label71 */
	label71 = XtVaCreateManagedWidget( "label71",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 401,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label71, (char *) UxAPSFileGenerationContext );


	/* Creation of TF_report_stop */
	TF_report_stop = XtVaCreateManagedWidget( "TF_report_stop",
			xmTextFieldWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 371,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, TRUE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "1999:001:00:00:00.000",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( TF_report_stop, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Create File Stop Time" );
	XtAddCallback( TF_report_stop, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_report_stop, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_report_stop, (char *) UxAPSFileGenerationContext );


	/* Creation of label70 */
	label70 = XtVaCreateManagedWidget( "label70",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 351,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label70, (char *) UxAPSFileGenerationContext );


	/* Creation of TF_report_start */
	TF_report_start = XtVaCreateManagedWidget( "TF_report_start",
			xmTextFieldWidgetClass,
			APSFileGeneration,
			XmNx, 150,
			XmNy, 321,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, TRUE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "1990:001:00:00:00.000",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( TF_report_start, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Create File Start Time" );
	XtAddCallback( TF_report_start, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_report_start, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_report_start, (char *) UxAPSFileGenerationContext );


	/* Creation of pushButton_input_file1 */
	pushButton_input_file1 = XtVaCreateManagedWidget( "pushButton_input_file1",
			xmPushButtonWidgetClass,
			APSFileGeneration,
			XmNx, 52,
			XmNy, 282,
			XmNwidth, 93,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SELECT FILE" ),
			XmNsensitive, TRUE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_input_file1, XmNactivateCallback,
		(XtCallbackProc) cb_capsfile_select_input,
		(XtPointer) scrolledList_reports );

	UxPutContext( pushButton_input_file1, (char *) UxAPSFileGenerationContext );


	/* Creation of pushButton_view */
	pushButton_view = XtVaCreateManagedWidget( "pushButton_view",
			xmPushButtonWidgetClass,
			APSFileGeneration,
			XmNx, 189,
			XmNy, 471,
			XmNwidth, 100,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "VIEW" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_view, XmNactivateCallback,
		(XtCallbackProc) cb_FileViewer_popup,
		(XtPointer) textField_reportname );

	UxPutContext( pushButton_view, (char *) UxAPSFileGenerationContext );


	/* Creation of pushButton_transfer */
	pushButton_transfer = XtVaCreateManagedWidget( "pushButton_transfer",
			xmPushButtonWidgetClass,
			APSFileGeneration,
			XmNx, 311,
			XmNy, 471,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "TRANSFER" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNhighlightOnEnter, TRUE,
			NULL );
	UxPutContext( pushButton_transfer, (char *) UxAPSFileGenerationContext );


	/* Creation of separator7 */
	separator7 = XtVaCreateManagedWidget( "separator7",
			xmSeparatorWidgetClass,
			APSFileGeneration,
			XmNwidth, 622,
			XmNheight, 9,
			XmNx, 1,
			XmNy, 516,
			NULL );
	UxPutContext( separator7, (char *) UxAPSFileGenerationContext );


	/* Creation of label6 */
	label6 = XtVaCreateManagedWidget( "label6",
			xmLabelWidgetClass,
			APSFileGeneration,
			XmNx, 11,
			XmNy, 526,
			XmNwidth, 590,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label6, (char *) UxAPSFileGenerationContext );


	/* Creation of ExtendTimes_tb */
	ExtendTimes_tb = XtVaCreateWidget( "ExtendTimes_tb",
			xmToggleButtonWidgetClass,
			APSFileGeneration,
			XmNwidth, 180,
			XmNheight, 32,
			XmNindicatorSize, 20,
			XmNindicatorType, XmN_OF_MANY,
			RES_CONVERT( XmNlabelString, "Extend Start/Stop Times\n(to day boundaries)" ),
			XmNmarginLeft, 2,
			XmNset, TRUE,
			XmNspacing, 4,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNx, 359,
			XmNy, 321,
			XmNindicatorOn, TRUE,
			NULL );
	UxPutContext( ExtendTimes_tb, (char *) UxAPSFileGenerationContext );


	XtAddCallback( APSFileGeneration, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSFileGenerationContext);


	return ( APSFileGeneration );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSFileGeneration( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSFileGeneration   *UxContext;
	static int		_Uxinit = 0;

	UxAPSFileGenerationContext = UxContext =
		(_UxCAPSFileGeneration *) UxNewContext( sizeof(_UxCAPSFileGeneration), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		PERIOD_WIDGETS *rptgen_times ;
		rtrn = _Uxbuild_APSFileGeneration();

		rptgen_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		rptgen_times->start = (Widget) TF_report_start ;
		rptgen_times->stop = (Widget) TF_report_stop ;
		
		XtAddCallback(TF_report_total_days, XmNactivateCallback,
		    (XtCallbackProc) cb_adjust_ASF_datetimes,
		    (XtPointer) rptgen_times) ;
		
		XtAddCallback(XtParent(rtrn), XtNpopupCallback,
			cb_show_reports, (XtPointer *) scrolledList_reports) ;
		 
		cb_init_extended_times( ExtendTimes_tb, NULL, NULL ) ;
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

