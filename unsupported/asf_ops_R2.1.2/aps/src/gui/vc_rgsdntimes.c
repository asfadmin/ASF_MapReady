
/*******************************************************************************
	vc_rgsdntimes.c

       Associated Header file: vc_rgsdntimes.h
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
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
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
#pragma ident   "@(#)vc_rgsdntimes.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_rgsdntimes.c"

#include <stdlib.h>
 
#include <Xm/DialogS.h>

#include "db_sybint.h"
#include "aps_db_table.h"

#include "satmenus.h"
#include "cb_sortform.h"
#include "cb_searchform.h"
#include "cb_rgsdntimes.h"
#include "cb_datetime.h"

extern Widget DownTime_manager;
extern Widget filebox ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_rgsdntimes.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_RGSDownTimeManager()
{
	Widget		_UxParent;
	Widget		subMenu_ASFdown_type_shell;
	Widget		subMenu_ASFdown_reason_shell;
	Widget		subMenu_rgs_down_stn_id_shell;


	/* Creation of RGSDownTimeManager */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "RGSDownTimeManager_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 395,
			XmNy, -12,
			XmNwidth, 741,
			XmNheight, 679,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "RGSDownTimeManager",
			XmNiconName, "RGSDownTimeManager",
			NULL );

	}

	RGSDownTimeManager = XtVaCreateManagedWidget( "RGSDownTimeManager",
			xmFormWidgetClass,
			_UxParent,
			XmNheight, 679,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:RGS Down Time" ),
			XmNwidth, 741,
			NULL );
	UxPutContext( RGSDownTimeManager, (char *) UxRGSDownTimeManagerContext );
	UxPutClassCode( RGSDownTimeManager, _UxIfClassId );


	/* Creation of label156 */
	label156 = XtVaCreateManagedWidget( "label156",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 268,
			XmNy, 15,
			XmNwidth, 208,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "RGS  DOWN  TIMES" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label156, (char *) UxRGSDownTimeManagerContext );


	/* Creation of scrolledWindowList5 */
	scrolledWindowList5 = XtVaCreateManagedWidget( "scrolledWindowList5",
			xmScrolledWindowWidgetClass,
			RGSDownTimeManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 65,
			XmNy, 90,
			XmNwidth, 645,
			XmNheight, 145,
			XmNresizable, FALSE,
			NULL );
	UxPutContext( scrolledWindowList5, (char *) UxRGSDownTimeManagerContext );


	/* Creation of scrolledList_DownTimes */
	scrolledList_DownTimes = XtVaCreateManagedWidget( "scrolledList_DownTimes",
			xmListWidgetClass,
			scrolledWindowList5,
			XmNwidth, 640,
			XmNheight, 160,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "ASF 1994:266:11:22:33  1994:109:11:22:33  UNPLANNED MAINTENANCE   CANCEL  Y" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisibleItemCount, 8,
			NULL );
	XtAddCallback( scrolledList_DownTimes, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_asfdntime_form,
		(XtPointer) UxRGSDownTimeManagerContext );

	UxPutContext( scrolledList_DownTimes, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_QuitDownTime */
	pushButton_QuitDownTime = XtVaCreateManagedWidget( "pushButton_QuitDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 615,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_QuitDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_popdown_asfdntime_form,
		(XtPointer) UxRGSDownTimeManagerContext );

	UxPutContext( pushButton_QuitDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_SortDownTime */
	pushButton_SortDownTime = XtVaCreateManagedWidget( "pushButton_SortDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 331,
			RES_CONVERT( XmNlabelString, "SORT BY" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNy, 255,
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SortDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label151 */
	label151 = XtVaCreateManagedWidget( "label151",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 55,
			XmNy, 60,
			RES_CONVERT( XmNlabelString, " STN                                                                         FA\n ID   START TIME         STOP TIME          TYPE      REASON       STATUS  REPORT" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNrecomputeSize, TRUE,
			XmNheight, 30,
			NULL );
	UxPutContext( label151, (char *) UxRGSDownTimeManagerContext );


	/* Creation of separator9 */
	separator9 = XtVaCreateManagedWidget( "separator9",
			xmSeparatorWidgetClass,
			RGSDownTimeManager,
			XmNwidth, 739,
			XmNheight, 9,
			XmNx, 1,
			XmNy, 310,
			NULL );
	UxPutContext( separator9, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label153 */
	label153 = XtVaCreateManagedWidget( "label153",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 35,
			XmNy, 405,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label153, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label154 */
	label154 = XtVaCreateManagedWidget( "label154",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 41,
			XmNy, 460,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label154, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_EditDownTime */
	pushButton_EditDownTime = XtVaCreateManagedWidget( "pushButton_EditDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 25,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "EDIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_EditDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_set_asfdntimes_editability,
		(XtPointer) True );

	UxPutContext( pushButton_EditDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_SearchDownTime */
	pushButton_SearchDownTime = XtVaCreateManagedWidget( "pushButton_SearchDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 61,
			XmNy, 255,
			RES_CONVERT( XmNlabelString, "SEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SearchDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_DeleteDownTime */
	pushButton_DeleteDownTime = XtVaCreateManagedWidget( "pushButton_DeleteDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 140,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CANCEL\nDOWN TIME" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_DeleteDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_delete_asfdntime_record,
		(XtPointer) UxRGSDownTimeManagerContext );

	UxPutContext( pushButton_DeleteDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_DownTime_sortclause */
	TF_DownTime_sortclause = XtVaCreateManagedWidget( "TF_DownTime_sortclause",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 411,
			XmNy, 258,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "disposition desc, station_id asc, strttime asc",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			XmNvalueWcs, UxConvertValueWcs("disposition desc, station_id asc, strttime asc" ),
			NULL );
	XtAddCallback( TF_DownTime_sortclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_asfdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( TF_DownTime_sortclause, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_DownTime_recordcount */
	TF_DownTime_recordcount = XtVaCreateManagedWidget( "TF_DownTime_recordcount",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 636,
			XmNy, 258,
			XmNheight, 32,
			XmNcolumns, 5,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DownTime_recordcount, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label152 */
	label152 = XtVaCreateManagedWidget( "label152",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 596,
			XmNy, 259,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "RECORD\nCOUNT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label152, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_SaveDownTimeChanges */
	pushButton_SaveDownTimeChanges = XtVaCreateWidget( "pushButton_SaveDownTimeChanges",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 385,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "SAVE\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_SaveDownTimeChanges, XmNactivateCallback,
		(XtCallbackProc) cb_save_asfdntime_changes,
		(XtPointer) False );

	UxPutContext( pushButton_SaveDownTimeChanges, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_CancelDownTimeChanges */
	pushButton_CancelDownTimeChanges = XtVaCreateWidget( "pushButton_CancelDownTimeChanges",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 505,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CANCEL\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_CancelDownTimeChanges, XmNactivateCallback,
		(XtCallbackProc) cb_set_asfdntimes_editability,
		(XtPointer) False );

	UxPutContext( pushButton_CancelDownTimeChanges, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_DownTime_searchclause */
	TF_DownTime_searchclause = XtVaCreateManagedWidget( "TF_DownTime_searchclause",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 141,
			XmNy, 258,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "where strtime > today",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			XmNvalueWcs, UxConvertValueWcs("where strttime > ' '" ),
			NULL );
	XtAddCallback( TF_DownTime_searchclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_asfdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( TF_DownTime_searchclause, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_ASFDNTIME_refresh */
	pushButton_ASFDNTIME_refresh = XtVaCreateManagedWidget( "pushButton_ASFDNTIME_refresh",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 20,
			XmNy, 100,
			XmNwidth, 30,
			XmNheight, 130,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( pushButton_ASFDNTIME_refresh, XmNactivateCallback,
		(XtCallbackProc) cb_show_asfdntime_records,
		(XtPointer) scrolledList_DownTimes );

	UxPutContext( pushButton_ASFDNTIME_refresh, (char *) UxRGSDownTimeManagerContext );


	/* Creation of subMenu_ASFdown_type */
	subMenu_ASFdown_type_shell = XtVaCreatePopupShell ("subMenu_ASFdown_type_shell",
			xmMenuShellWidgetClass, RGSDownTimeManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_ASFdown_type = XtVaCreateWidget( "subMenu_ASFdown_type",
			xmRowColumnWidgetClass,
			subMenu_ASFdown_type_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			XmNmenuPost, "",
			NULL );
	UxPutContext( subMenu_ASFdown_type, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_type_PLANNED */
	ASFdown_type_PLANNED = XtVaCreateManagedWidget( "ASFdown_type_PLANNED",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_type,
			RES_CONVERT( XmNlabelString, "PLANNED" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( ASFdown_type_PLANNED, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_type_UNPLANNED */
	ASFdown_type_UNPLANNED = XtVaCreateManagedWidget( "ASFdown_type_UNPLANNED",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_type,
			RES_CONVERT( XmNlabelString, "UNPLANNED" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( ASFdown_type_UNPLANNED, (char *) UxRGSDownTimeManagerContext );


	/* Creation of optionMenu_ASFdown_type */
	optionMenu_ASFdown_type = XtVaCreateManagedWidget( "optionMenu_ASFdown_type",
			xmRowColumnWidgetClass,
			RGSDownTimeManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_ASFdown_type,
			XmNx, 350,
			XmNy, 400,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "TYPE:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_ASFdown_type, (char *) UxRGSDownTimeManagerContext );


	/* Creation of subMenu_ASFdown_reason */
	subMenu_ASFdown_reason_shell = XtVaCreatePopupShell ("subMenu_ASFdown_reason_shell",
			xmMenuShellWidgetClass, RGSDownTimeManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_ASFdown_reason = XtVaCreateWidget( "subMenu_ASFdown_reason",
			xmRowColumnWidgetClass,
			subMenu_ASFdown_reason_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_ASFdown_reason, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_reason_CONFLICT */
	ASFdown_reason_CONFLICT = XtVaCreateManagedWidget( "ASFdown_reason_CONFLICT",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_reason,
			RES_CONVERT( XmNlabelString, "CONFLICT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( ASFdown_reason_CONFLICT, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_reason_MAINTENANCE */
	ASFdown_reason_MAINTENANCE = XtVaCreateManagedWidget( "ASFdown_reason_MAINTENANCE",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_reason,
			RES_CONVERT( XmNlabelString, "MAINTENANCE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( ASFdown_reason_MAINTENANCE, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_reason_REPAIR */
	ASFdown_reason_REPAIR = XtVaCreateManagedWidget( "ASFdown_reason_REPAIR",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_reason,
			RES_CONVERT( XmNlabelString, "REPAIR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( ASFdown_reason_REPAIR, (char *) UxRGSDownTimeManagerContext );


	/* Creation of ASFdown_reason_UPGRADE */
	ASFdown_reason_UPGRADE = XtVaCreateManagedWidget( "ASFdown_reason_UPGRADE",
			xmPushButtonWidgetClass,
			subMenu_ASFdown_reason,
			RES_CONVERT( XmNlabelString, "UPGRADE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( ASFdown_reason_UPGRADE, (char *) UxRGSDownTimeManagerContext );


	/* Creation of optionMenu_ASFdown_reason */
	optionMenu_ASFdown_reason = XtVaCreateManagedWidget( "optionMenu_ASFdown_reason",
			xmRowColumnWidgetClass,
			RGSDownTimeManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_ASFdown_reason,
			XmNx, 510,
			XmNy, 400,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "REASON" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_ASFdown_reason, (char *) UxRGSDownTimeManagerContext );


	/* Creation of pushButton_CreateDownTime */
	pushButton_CreateDownTime = XtVaCreateManagedWidget( "pushButton_CreateDownTime",
			xmPushButtonWidgetClass,
			RGSDownTimeManager,
			XmNx, 250,
			XmNy, 605,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_CreateDownTime, XmNactivateCallback,
		(XtCallbackProc) cb_create_new_down_time,
		(XtPointer) UxRGSDownTimeManagerContext );

	UxPutContext( pushButton_CreateDownTime, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label155 */
	label155 = XtVaCreateManagedWidget( "label155",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 35,
			XmNy, 525,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label155, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_ASF_DN_TIMES_STRTTIME */
	TF_ASF_DN_TIMES_STRTTIME = XtVaCreateManagedWidget( "TF_ASF_DN_TIMES_STRTTIME",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 110,
			XmNy, 405,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			NULL );
	XtAddCallback( TF_ASF_DN_TIMES_STRTTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "RGS Down Start Time" );
	XtAddCallback( TF_ASF_DN_TIMES_STRTTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_ASF_DN_TIMES_STRTTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_ASF_DN_TIMES_STRTTIME, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_ASF_DN_TIMES_STOPTIME */
	TF_ASF_DN_TIMES_STOPTIME = XtVaCreateManagedWidget( "TF_ASF_DN_TIMES_STOPTIME",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 110,
			XmNy, 455,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, TRUE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			NULL );
	XtAddCallback( TF_ASF_DN_TIMES_STOPTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "RGS Down Stop Time" );
	XtAddCallback( TF_ASF_DN_TIMES_STOPTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_ASF_DN_TIMES_STOPTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_ASF_DN_TIMES_STOPTIME, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_DownTime_total_days */
	TF_DownTime_total_days = XtVaCreateManagedWidget( "TF_DownTime_total_days",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 110,
			XmNy, 524,
			XmNheight, 31,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, TRUE,
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

	UxPutContext( TF_DownTime_total_days, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label122 */
	label122 = XtVaCreateManagedWidget( "label122",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 110,
			XmNy, 435,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label122, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label123 */
	label123 = XtVaCreateManagedWidget( "label123",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 110,
			XmNy, 490,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label123, (char *) UxRGSDownTimeManagerContext );


	/* Creation of T_ASF_DN_TIMES_REMARKS */
	T_ASF_DN_TIMES_REMARKS = XtVaCreateManagedWidget( "T_ASF_DN_TIMES_REMARKS",
			xmTextWidgetClass,
			RGSDownTimeManager,
			XmNx, 390,
			XmNy, 455,
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
			NULL );
	XtAddCallback( T_ASF_DN_TIMES_REMARKS, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) True );
	XtAddCallback( T_ASF_DN_TIMES_REMARKS, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) False );

	UxPutContext( T_ASF_DN_TIMES_REMARKS, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label124 */
	label124 = XtVaCreateManagedWidget( "label124",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 335,
			XmNy, 455,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "REMARKS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label124, (char *) UxRGSDownTimeManagerContext );


	/* Creation of rowColumn4 */
	rowColumn4 = XtVaCreateManagedWidget( "rowColumn4",
			xmRowColumnWidgetClass,
			RGSDownTimeManager,
			XmNwidth, 290,
			XmNheight, 37,
			XmNx, 390,
			XmNy, 355,
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( rowColumn4, (char *) UxRGSDownTimeManagerContext );


	/* Creation of toggleButton_DownTimeScheduled */
	toggleButton_DownTimeScheduled = XtVaCreateManagedWidget( "toggleButton_DownTimeScheduled",
			xmToggleButtonWidgetClass,
			rowColumn4,
			XmNx, 3,
			XmNy, 355,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "ACTIVE" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8" ),
			XmNset, TRUE,
			NULL );
	UxPutContext( toggleButton_DownTimeScheduled, (char *) UxRGSDownTimeManagerContext );


	/* Creation of toggleButton_DownTimeCancelled */
	toggleButton_DownTimeCancelled = XtVaCreateManagedWidget( "toggleButton_DownTimeCancelled",
			xmToggleButtonWidgetClass,
			rowColumn4,
			XmNx, 92,
			XmNy, 355,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "CANCELLED" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8" ),
			NULL );
	UxPutContext( toggleButton_DownTimeCancelled, (char *) UxRGSDownTimeManagerContext );


	/* Creation of subMenu_rgs_down_stn_id */
	subMenu_rgs_down_stn_id_shell = XtVaCreatePopupShell ("subMenu_rgs_down_stn_id_shell",
			xmMenuShellWidgetClass, RGSDownTimeManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_rgs_down_stn_id = XtVaCreateWidget( "subMenu_rgs_down_stn_id",
			xmRowColumnWidgetClass,
			subMenu_rgs_down_stn_id_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 48,
			XmNresizeHeight, FALSE,
			XmNx, 0,
			XmNy, 355,
			XmNsensitive, TRUE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_rgs_down_stn_id, (char *) UxRGSDownTimeManagerContext );


	/* Creation of subMenu_rgs_down_stn_id_pb */
	subMenu_rgs_down_stn_id_pb = XtVaCreateManagedWidget( "subMenu_rgs_down_stn_id_pb",
			xmPushButtonWidgetClass,
			subMenu_rgs_down_stn_id,
			RES_CONVERT( XmNlabelString, "ASF" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 355,
			NULL );
	UxPutContext( subMenu_rgs_down_stn_id_pb, (char *) UxRGSDownTimeManagerContext );

	cb_build_station_option_menu( subMenu_rgs_down_stn_id_pb,
			(XtPointer) UxRGSDownTimeManagerContext, (XtPointer) NULL );


	/* Creation of optionMenu_station_id */
	optionMenu_station_id = XtVaCreateManagedWidget( "optionMenu_station_id",
			xmRowColumnWidgetClass,
			RGSDownTimeManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_rgs_down_stn_id,
			XmNx, 60,
			XmNy, 355,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "STATION\nID:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_station_id, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label22 */
	label22 = XtVaCreateManagedWidget( "label22",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 345,
			XmNy, 355,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STATUS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label22, (char *) UxRGSDownTimeManagerContext );


	/* Creation of label27 */
	label27 = XtVaCreateManagedWidget( "label27",
			xmLabelWidgetClass,
			RGSDownTimeManager,
			XmNx, 285,
			XmNy, 525,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "FA Unavailibilty\nReport Created:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label27, (char *) UxRGSDownTimeManagerContext );


	/* Creation of TF_FA_Notification */
	TF_FA_Notification = XtVaCreateManagedWidget( "TF_FA_Notification",
			xmTextFieldWidgetClass,
			RGSDownTimeManager,
			XmNx, 390,
			XmNy, 525,
			XmNheight, 31,
			XmNcolumns, 3,
			XmNresizeWidth, FALSE,
			XmNvalue, "NO",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( TF_FA_Notification, (char *) UxRGSDownTimeManagerContext );


	XtAddCallback( RGSDownTimeManager, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxRGSDownTimeManagerContext);


	return ( RGSDownTimeManager );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_DownTimeManager( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCRGSDownTimeManager  *UxContext;
	static int		_Uxinit = 0;

	UxRGSDownTimeManagerContext = UxContext =
		(_UxCRGSDownTimeManager *) UxNewContext( sizeof(_UxCRGSDownTimeManager), False );

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
		rtrn = _Uxbuild_RGSDownTimeManager();

		sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;
		searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;
		
		sortinfo->table_name = APS_TABLE(RGS_DOWN_TIMES) ;
		sortinfo->field_to_update = (Widget) TF_DownTime_sortclause ;
		
		searchinfo->table_name = APS_TABLE(RGS_DOWN_TIMES) ;
		searchinfo->field_to_update  = (Widget) TF_DownTime_searchclause ;
		
		down_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		down_times->start = (Widget) TF_ASF_DN_TIMES_STRTTIME ;
		down_times->stop = (Widget) TF_ASF_DN_TIMES_STOPTIME ;
		
		
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
			cb_show_asfdntime_records, (XtPointer *) scrolledList_DownTimes) ;
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

