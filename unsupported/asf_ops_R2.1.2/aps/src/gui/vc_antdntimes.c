
/*******************************************************************************
	vc_antdntimes.c

       Associated Header file: vc_antdntimes.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
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
#pragma ident   "@(#)vc_antdntimes.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_antdntimes.c"

#include <stdlib.h>

#include <Xm/DialogS.h>

#include "db_sybint.h"
#include "aps_db_table.h"

#include "satmenus.h"
#include "cb_sortform.h"
#include "cb_searchform.h"
#include "cb_antdntimes.h"
#include "cb_datetime.h"

extern Widget AntennaDownTime_manager;
extern Widget filebox ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_antdntimes.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_QuitDownTime(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAntennaDownTimeManager *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAntennaDownTimeManagerContext;
	UxAntennaDownTimeManagerContext = UxContext =
			(_UxCAntennaDownTimeManager *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(AntennaDownTime_manager)) ;
	}
	UxAntennaDownTimeManagerContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_AntennaDownTimeManager()
{
	Widget		_UxParent;
	Widget		subMenu_antenna_down_stn_id_shell;
	Widget		subMenu_antenna_down_antenna_shell;


	/* Creation of AntennaDownTimeManager */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "AntennaDownTimeManager_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 400,
			XmNy, 5,
			XmNwidth, 732,
			XmNheight, 639,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "AntennaDownTimeManager",
			XmNiconName, "AntennaDownTimeManager",
			NULL );

	}

	AntennaDownTimeManager = XtVaCreateManagedWidget( "AntennaDownTimeManager",
			xmFormWidgetClass,
			_UxParent,
			XmNheight, 639,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:ANTENNA Down Time" ),
			XmNwidth, 732,
			NULL );
	UxPutContext( AntennaDownTimeManager, (char *) UxAntennaDownTimeManagerContext );
	UxPutClassCode( AntennaDownTimeManager, _UxIfClassId );


	/* Creation of label156 */
	label156 = XtVaCreateManagedWidget( "label156",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 232,
			XmNy, 14,
			XmNwidth, 268,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "ANTENNA  DOWN  TIMES" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label156, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of scrolledWindowList5 */
	scrolledWindowList5 = XtVaCreateManagedWidget( "scrolledWindowList5",
			xmScrolledWindowWidgetClass,
			AntennaDownTimeManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 60,
			XmNy, 90,
			XmNwidth, 645,
			NULL );
	UxPutContext( scrolledWindowList5, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of scrolledList_DownTimes */
	scrolledList_DownTimes = XtVaCreateManagedWidget( "scrolledList_DownTimes",
			xmListWidgetClass,
			scrolledWindowList5,
			XmNwidth, 640,
			XmNheight, 150,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "ASF      1       1994:266:11:22:33    1994:109:11:22:33" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 9,
			XmNx, 0,
			XmNy, 90,
			NULL );
	XtAddCallback( scrolledList_DownTimes, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_antdntime_form,
		(XtPointer) UxAntennaDownTimeManagerContext );

	UxPutContext( scrolledList_DownTimes, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_QuitDownTime */
	pushButton_QuitDownTime = XtVaCreateManagedWidget( "pushButton_QuitDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 617,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_QuitDownTime, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_QuitDownTime,
		(XtPointer) UxAntennaDownTimeManagerContext );

	UxPutContext( pushButton_QuitDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_SortDownTime */
	pushButton_SortDownTime = XtVaCreateManagedWidget( "pushButton_SortDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 326,
			RES_CONVERT( XmNlabelString, "SORT BY" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNy, 275,
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SortDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label151 */
	label151 = XtVaCreateManagedWidget( "label151",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 57,
			XmNy, 59,
			RES_CONVERT( XmNlabelString, " STN   ANTENNA\n ID      ID          START TIME           STOP TIME" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNrecomputeSize, TRUE,
			NULL );
	UxPutContext( label151, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of separator9 */
	separator9 = XtVaCreateManagedWidget( "separator9",
			xmSeparatorWidgetClass,
			AntennaDownTimeManager,
			XmNwidth, 730,
			XmNheight, 10,
			XmNx, 1,
			XmNy, 328,
			NULL );
	UxPutContext( separator9, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label153 */
	label153 = XtVaCreateManagedWidget( "label153",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 36,
			XmNy, 398,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label153, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label154 */
	label154 = XtVaCreateManagedWidget( "label154",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 42,
			XmNy, 453,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label154, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_EditDownTime */
	pushButton_EditDownTime = XtVaCreateManagedWidget( "pushButton_EditDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 27,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "EDIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_EditDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_set_antdntimes_editability,
		(XtPointer) True );

	UxPutContext( pushButton_EditDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_SearchDownTime */
	pushButton_SearchDownTime = XtVaCreateManagedWidget( "pushButton_SearchDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 56,
			XmNy, 275,
			RES_CONVERT( XmNlabelString, "SEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SearchDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_DeleteDownTime */
	pushButton_DeleteDownTime = XtVaCreateManagedWidget( "pushButton_DeleteDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 142,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "DELETE\nDOWN TIME" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_DeleteDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_delete_antdntime_record,
		(XtPointer) UxAntennaDownTimeManagerContext );

	UxPutContext( pushButton_DeleteDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_DownTime_sortclause */
	TF_DownTime_sortclause = XtVaCreateManagedWidget( "TF_DownTime_sortclause",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 406,
			XmNy, 278,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "station_id asc, antenna_id asc, strttime asc",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			XmNvalueWcs, UxConvertValueWcs("station_id asc, antenna_id asc, strttime asc" ),
			NULL );
	XtAddCallback( TF_DownTime_sortclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_antdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( TF_DownTime_sortclause, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_DownTime_recordcount */
	TF_DownTime_recordcount = XtVaCreateManagedWidget( "TF_DownTime_recordcount",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 631,
			XmNy, 279,
			XmNheight, 32,
			XmNcolumns, 5,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "1",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DownTime_recordcount, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label152 */
	label152 = XtVaCreateManagedWidget( "label152",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 591,
			XmNy, 279,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "RECORD\nCOUNT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label152, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_SaveDownTimeChanges */
	pushButton_SaveDownTimeChanges = XtVaCreateWidget( "pushButton_SaveDownTimeChanges",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 387,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "SAVE\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_SaveDownTimeChanges, XmNactivateCallback,
		(XtCallbackProc) cb_save_antdntime_changes,
		(XtPointer) False );

	UxPutContext( pushButton_SaveDownTimeChanges, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_CancelDownTimeChanges */
	pushButton_CancelDownTimeChanges = XtVaCreateWidget( "pushButton_CancelDownTimeChanges",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 507,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CANCEL\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_CancelDownTimeChanges, XmNactivateCallback,
		(XtCallbackProc) cb_set_antdntimes_editability,
		(XtPointer) False );

	UxPutContext( pushButton_CancelDownTimeChanges, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_DownTime_searchclause */
	TF_DownTime_searchclause = XtVaCreateManagedWidget( "TF_DownTime_searchclause",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 136,
			XmNy, 278,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "where strtime > today",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			XmNvalueWcs, UxConvertValueWcs("where strttime > ' '" ),
			NULL );
	XtAddCallback( TF_DownTime_searchclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_antdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( TF_DownTime_searchclause, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_ANTDNTIME_refresh */
	pushButton_ANTDNTIME_refresh = XtVaCreateManagedWidget( "pushButton_ANTDNTIME_refresh",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 22,
			XmNy, 100,
			XmNwidth, 30,
			XmNheight, 130,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( pushButton_ANTDNTIME_refresh, XmNactivateCallback,
		(XtCallbackProc) cb_show_antdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( pushButton_ANTDNTIME_refresh, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of pushButton_CreateDownTime */
	pushButton_CreateDownTime = XtVaCreateManagedWidget( "pushButton_CreateDownTime",
			xmPushButtonWidgetClass,
			AntennaDownTimeManager,
			XmNx, 252,
			XmNy, 573,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_CreateDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_create_new_antdntime_record,
		(XtPointer) UxAntennaDownTimeManagerContext );

	UxPutContext( pushButton_CreateDownTime, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label155 */
	label155 = XtVaCreateManagedWidget( "label155",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 36,
			XmNy, 508,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label155, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_ANT_DN_TIMES_STRTTIME */
	TF_ANT_DN_TIMES_STRTTIME = XtVaCreateManagedWidget( "TF_ANT_DN_TIMES_STRTTIME",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 111,
			XmNy, 397,
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
	XtAddCallback( TF_ANT_DN_TIMES_STRTTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Antenna Down Start Time" );
	XtAddCallback( TF_ANT_DN_TIMES_STRTTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_ANT_DN_TIMES_STRTTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_ANT_DN_TIMES_STRTTIME, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_ANT_DN_TIMES_STOPTIME */
	TF_ANT_DN_TIMES_STOPTIME = XtVaCreateManagedWidget( "TF_ANT_DN_TIMES_STOPTIME",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 111,
			XmNy, 452,
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
	XtAddCallback( TF_ANT_DN_TIMES_STOPTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Antenna Down Stop Time" );
	XtAddCallback( TF_ANT_DN_TIMES_STOPTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_ANT_DN_TIMES_STOPTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_ANT_DN_TIMES_STOPTIME, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of TF_DownTime_total_days */
	TF_DownTime_total_days = XtVaCreateManagedWidget( "TF_DownTime_total_days",
			xmTextFieldWidgetClass,
			AntennaDownTimeManager,
			XmNx, 111,
			XmNy, 507,
			XmNheight, 32,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNwidth, 83,
			NULL );
	XtAddCallback( TF_DownTime_total_days, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_float_chars );
	XtAddCallback( TF_DownTime_total_days, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DownTime_total_days, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_DownTime_total_days, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label122 */
	label122 = XtVaCreateManagedWidget( "label122",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 111,
			XmNy, 428,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label122, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label123 */
	label123 = XtVaCreateManagedWidget( "label123",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 111,
			XmNy, 483,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label123, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of T_ANT_DN_TIMES_COMMENTS */
	T_ANT_DN_TIMES_COMMENTS = XtVaCreateManagedWidget( "T_ANT_DN_TIMES_COMMENTS",
			xmTextWidgetClass,
			AntennaDownTimeManager,
			XmNx, 391,
			XmNy, 399,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 60,
			XmNrows, 3,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNcolumns, 30,
			XmNwordWrap, TRUE,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNheight, 79,
			XmNwidth, 207,
			NULL );
	XtAddCallback( T_ANT_DN_TIMES_COMMENTS, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) True );
	XtAddCallback( T_ANT_DN_TIMES_COMMENTS, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) False );

	UxPutContext( T_ANT_DN_TIMES_COMMENTS, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of label124 */
	label124 = XtVaCreateManagedWidget( "label124",
			xmLabelWidgetClass,
			AntennaDownTimeManager,
			XmNx, 329,
			XmNy, 398,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "COMMENTS:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 62,
			NULL );
	UxPutContext( label124, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of subMenu_antenna_down_stn_id */
	subMenu_antenna_down_stn_id_shell = XtVaCreatePopupShell ("subMenu_antenna_down_stn_id_shell",
			xmMenuShellWidgetClass, AntennaDownTimeManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_antenna_down_stn_id = XtVaCreateWidget( "subMenu_antenna_down_stn_id",
			xmRowColumnWidgetClass,
			subMenu_antenna_down_stn_id_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 48,
			XmNresizeHeight, FALSE,
			XmNx, 0,
			XmNy, 349,
			XmNsensitive, TRUE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_antenna_down_stn_id, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of subMenu_antenna_down_stn_id_pb */
	subMenu_antenna_down_stn_id_pb = XtVaCreateManagedWidget( "subMenu_antenna_down_stn_id_pb",
			xmPushButtonWidgetClass,
			subMenu_antenna_down_stn_id,
			RES_CONVERT( XmNlabelString, "ASF" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 362,
			NULL );
	UxPutContext( subMenu_antenna_down_stn_id_pb, (char *) UxAntennaDownTimeManagerContext );

	cb_build_station_option_menu( subMenu_antenna_down_stn_id_pb,
			(XtPointer) UxAntennaDownTimeManagerContext, (XtPointer) NULL );


	/* Creation of optionMenu_station_id */
	optionMenu_station_id = XtVaCreateManagedWidget( "optionMenu_station_id",
			xmRowColumnWidgetClass,
			AntennaDownTimeManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_antenna_down_stn_id,
			XmNx, 40,
			XmNy, 348,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "STATION ID\n/ANTENNA:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_station_id, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of subMenu_antenna_down_antenna */
	subMenu_antenna_down_antenna_shell = XtVaCreatePopupShell ("subMenu_antenna_down_antenna_shell",
			xmMenuShellWidgetClass, AntennaDownTimeManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_antenna_down_antenna = XtVaCreateWidget( "subMenu_antenna_down_antenna",
			xmRowColumnWidgetClass,
			subMenu_antenna_down_antenna_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 360,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_antenna_down_antenna, (char *) UxAntennaDownTimeManagerContext );


	/* Creation of subMenu_antenna_down_antenna_pb */
	subMenu_antenna_down_antenna_pb = XtVaCreateManagedWidget( "subMenu_antenna_down_antenna_pb",
			xmPushButtonWidgetClass,
			subMenu_antenna_down_antenna,
			RES_CONVERT( XmNlabelString, "1" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 362,
			NULL );
	UxPutContext( subMenu_antenna_down_antenna_pb, (char *) UxAntennaDownTimeManagerContext );

	cb_build_antenna_option_menu( subMenu_antenna_down_antenna_pb,
			(XtPointer) False, (XtPointer) NULL );


	/* Creation of optionMenu_antenna */
	optionMenu_antenna = XtVaCreateManagedWidget( "optionMenu_antenna",
			xmRowColumnWidgetClass,
			AntennaDownTimeManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_antenna_down_antenna,
			XmNx, 166,
			XmNy, 347,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "/" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_antenna, (char *) UxAntennaDownTimeManagerContext );


	XtAddCallback( AntennaDownTimeManager, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAntennaDownTimeManagerContext);


	return ( AntennaDownTimeManager );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_AntennaDownTimeManager( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAntennaDownTimeManager *UxContext;
	static int		_Uxinit = 0;

	UxAntennaDownTimeManagerContext = UxContext =
		(_UxCAntennaDownTimeManager *) UxNewContext( sizeof(_UxCAntennaDownTimeManager), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		SORT_INFO *sortinfo ;
		SEARCH_INFO *searchinfo ;
		PERIOD_WIDGETS *down_times ;
		ANTENNA_CLIENT_DATA *antenna_menu ;
		rtrn = _Uxbuild_AntennaDownTimeManager();

		sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;
		searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;
		antenna_menu =
			(ANTENNA_CLIENT_DATA *) malloc(sizeof(ANTENNA_CLIENT_DATA));
		
		sortinfo->table_name = APS_TABLE(ANTENNA_DOWN_TIMES) ;
		sortinfo->field_to_update = (Widget) TF_DownTime_sortclause ;
		
		searchinfo->table_name = APS_TABLE(ANTENNA_DOWN_TIMES) ;
		searchinfo->field_to_update  = (Widget) TF_DownTime_searchclause ;
		
		down_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		down_times->start = (Widget) TF_ANT_DN_TIMES_STRTTIME ;
		down_times->stop = (Widget) TF_ANT_DN_TIMES_STOPTIME ;
		
		antenna_menu->noAntenna_flag = False ;
		antenna_menu->menuWidgets.optionmenu = (Widget) optionMenu_antenna ;
		antenna_menu->menuWidgets.submenu = (Widget) subMenu_antenna_down_antenna ;
		
		XtAddCallback( subMenu_antenna_down_stn_id, XmNentryCallback,
			(XtCallbackProc) cb_set_antenna_menus,
			(XtPointer ) antenna_menu );
		
		XtAddCallback( pushButton_SortDownTime, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_sort_columns,
		    (XtPointer) sortinfo );
		 
		XtAddCallback( pushButton_SearchDownTime, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_search_columns,
		    (XtPointer) searchinfo );
		
		XtAddCallback(TF_DownTime_total_days, XmNactivateCallback,
			(XtCallbackProc) cb_adjust_ASF_datetimes,
			(XtPointer) down_times) ;
		
		XtAddCallback(XtParent(rtrn), XtNpopupCallback,
			cb_show_antdntime_records, (XtPointer *) scrolledList_DownTimes) ;
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

