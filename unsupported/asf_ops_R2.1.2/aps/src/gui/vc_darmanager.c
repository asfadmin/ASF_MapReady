
/*******************************************************************************
	vc_darmanager.c

       Associated Header file: vc_darmanager.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/SeparatoG.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
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
#pragma ident   "@(#)vc_darmanager.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_darmanager.c"

#include <stdlib.h>

#include <Xm/FileSB.h>
#include <Xm/DialogS.h>

#include "db_sybint.h"
#include "aps_db_table.h"
#include "DARconversions.h"

#include "gui_utils.h"
#include "cb_sortform.h"
#include "cb_searchform.h"
#include "cb_datetime.h"
#include "cb_darmanager.h"

extern Widget DAR_manager;
extern Widget filebox ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_darmanager.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_DARManager()
{
	Widget		_UxParent;
	Widget		DAR_status_options_shell;
	Widget		menuBar_p1_shell;
	Widget		menuBar_FILE_shell;
	Widget		PANE_SAVE_DAR_RPT_shell;
	Widget		PANE_PRINT_DAR_RPT_shell;


	/* Creation of DARManager */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "DARManager_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 129,
			XmNy, 6,
			XmNwidth, 755,
			XmNheight, 846,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "DARManager",
			XmNiconName, "DARManager",
			NULL );

	}

	DARManager = XtVaCreateManagedWidget( "DARManager",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 755,
			XmNheight, 846,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:DAR Manager" ),
			NULL );
	UxPutContext( DARManager, (char *) UxDARManagerContext );
	UxPutClassCode( DARManager, _UxIfClassId );


	/* Creation of label78 */
	label78 = XtVaCreateManagedWidget( "label78",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 15,
			XmNy, 15,
			XmNwidth, 730,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "DAR  MANAGER" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label78, (char *) UxDARManagerContext );


	/* Creation of scrolledWindow_DARS */
	scrolledWindow_DARS = XtVaCreateManagedWidget( "scrolledWindow_DARS",
			xmScrolledWindowWidgetClass,
			DARManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 65,
			XmNy, 90,
			XmNwidth, 660,
			NULL );
	UxPutContext( scrolledWindow_DARS, (char *) UxDARManagerContext );


	/* Creation of scrolledList_DARS */
	scrolledList_DARS = XtVaCreateManagedWidget( "scrolledList_DARS",
			xmListWidgetClass,
			scrolledWindow_DARS,
			XmNwidth, 660,
			XmNheight, 140,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "1234567 QUE RADARSAT/SAR 1990:001:00:00:00 1991:001:00:00:00 COLDEST PLACE ON EARTH" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNy, 1,
			XmNx, 1,
			NULL );
	XtAddCallback( scrolledList_DARS, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_darmanager_form,
		(XtPointer) UxDARManagerContext );

	UxPutContext( scrolledList_DARS, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_DARID */
	TF_DAR_DARID = XtVaCreateManagedWidget( "TF_DAR_DARID",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 101,
			XmNy, 333,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DAR_DARID, (char *) UxDARManagerContext );


	/* Creation of pushButton_QuitDAR */
	pushButton_QuitDAR = XtVaCreateManagedWidget( "pushButton_QuitDAR",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 629,
			XmNy, 775,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_QuitDAR, XmNactivateCallback,
		(XtCallbackProc) cb_quit_darmanager,
		(XtPointer) UxDARManagerContext );

	UxPutContext( pushButton_QuitDAR, (char *) UxDARManagerContext );


	/* Creation of pushButton_SortDAR */
	pushButton_SortDAR = XtVaCreateManagedWidget( "pushButton_SortDAR",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 375,
			RES_CONVERT( XmNlabelString, "SORT BY" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNy, 265,
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SortDAR, (char *) UxDARManagerContext );


	/* Creation of label133 */
	label133 = XtVaCreateManagedWidget( "label133",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 65,
			XmNy, 60,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "  DARID STATUS SAT/SENSOR    START TIME        STOP TIME          SITE NAME" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNwidth, 675,
			NULL );
	UxPutContext( label133, (char *) UxDARManagerContext );


	/* Creation of form_DARcircle */
	form_DARcircle = XtVaCreateWidget( "form_DARcircle",
			xmFormWidgetClass,
			DARManager,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 775,
			XmNy, 445,
			XmNwidth, 420,
			XmNheight, 80,
			NULL );
	UxPutContext( form_DARcircle, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_center_lon */
	TF_DAR_center_lon = XtVaCreateManagedWidget( "TF_DAR_center_lon",
			xmTextFieldWidgetClass,
			form_DARcircle,
			XmNx, 140,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_DAR_center_lon, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_center_lat */
	TF_DAR_center_lat = XtVaCreateManagedWidget( "TF_DAR_center_lat",
			xmTextFieldWidgetClass,
			form_DARcircle,
			XmNx, 50,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_DAR_center_lat, (char *) UxDARManagerContext );


	/* Creation of label92 */
	label92 = XtVaCreateManagedWidget( "label92",
			xmLabelWidgetClass,
			form_DARcircle,
			XmNx, 125,
			XmNy, 30,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label92, (char *) UxDARManagerContext );


	/* Creation of label93 */
	label93 = XtVaCreateManagedWidget( "label93",
			xmLabelWidgetClass,
			form_DARcircle,
			XmNx, 5,
			XmNy, 25,
			XmNwidth, 46,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "CENTER:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label93, (char *) UxDARManagerContext );


	/* Creation of label94 */
	label94 = XtVaCreateManagedWidget( "label94",
			xmLabelWidgetClass,
			form_DARcircle,
			XmNx, 220,
			XmNy, 25,
			XmNwidth, 51,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "RADIUS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label94, (char *) UxDARManagerContext );


	/* Creation of TF_RADIUS */
	TF_RADIUS = XtVaCreateManagedWidget( "TF_RADIUS",
			xmTextFieldWidgetClass,
			form_DARcircle,
			XmNx, 270,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_RADIUS, (char *) UxDARManagerContext );


	/* Creation of label95 */
	label95 = XtVaCreateManagedWidget( "label95",
			xmLabelWidgetClass,
			form_DARcircle,
			XmNx, 345,
			XmNy, 25,
			XmNwidth, 21,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "km" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label95, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_STRTTIME */
	TF_DAR_STRTTIME = XtVaCreateManagedWidget( "TF_DAR_STRTTIME",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 101,
			XmNy, 405,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "1990:001:00:00:00.000",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNvalueWcs, UxConvertValueWcs("" ),
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DAR_STRTTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "DAR Start Time" );
	XtAddCallback( TF_DAR_STRTTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DAR_STRTTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_DAR_STRTTIME, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_ENDTIME */
	TF_DAR_ENDTIME = XtVaCreateManagedWidget( "TF_DAR_ENDTIME",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 101,
			XmNy, 442,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "1999:001:00:00:00.000",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNvalueWcs, UxConvertValueWcs("" ),
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DAR_ENDTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "DAR Stop Time" );
	XtAddCallback( TF_DAR_ENDTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DAR_ENDTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_DAR_ENDTIME, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_total_days */
	TF_DAR_total_days = XtVaCreateManagedWidget( "TF_DAR_total_days",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 101,
			XmNy, 484,
			XmNheight, 32,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNwidth, 83,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DAR_total_days, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_float_chars );
	XtAddCallback( TF_DAR_total_days, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DAR_total_days, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_DAR_total_days, (char *) UxDARManagerContext );


	/* Creation of label99 */
	label99 = XtVaCreateManagedWidget( "label99",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 26,
			XmNy, 484,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label99, (char *) UxDARManagerContext );


	/* Creation of form_DARquad */
	form_DARquad = XtVaCreateManagedWidget( "form_DARquad",
			xmFormWidgetClass,
			DARManager,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNwidth, 401,
			XmNheight, 74,
			XmNx, 346,
			XmNy, 484,
			NULL );
	UxPutContext( form_DARquad, (char *) UxDARManagerContext );


	/* Creation of TF_NWLON */
	TF_NWLON = XtVaCreateManagedWidget( "TF_NWLON",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 120,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_NWLON, (char *) UxDARManagerContext );


	/* Creation of label134 */
	label134 = XtVaCreateManagedWidget( "label134",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 105,
			XmNy, 10,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label134, (char *) UxDARManagerContext );


	/* Creation of TF_NELON */
	TF_NELON = XtVaCreateManagedWidget( "TF_NELON",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 325,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_NELON, (char *) UxDARManagerContext );


	/* Creation of label131 */
	label131 = XtVaCreateManagedWidget( "label131",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 310,
			XmNy, 10,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label131, (char *) UxDARManagerContext );


	/* Creation of label_NE1 */
	label_NE1 = XtVaCreateManagedWidget( "label_NE1",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 210,
			XmNy, 5,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_NE1, (char *) UxDARManagerContext );


	/* Creation of TF_SWLON */
	TF_SWLON = XtVaCreateManagedWidget( "TF_SWLON",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 120,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SWLON, (char *) UxDARManagerContext );


	/* Creation of label130 */
	label130 = XtVaCreateManagedWidget( "label130",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 105,
			XmNy, 45,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label130, (char *) UxDARManagerContext );


	/* Creation of label89 */
	label89 = XtVaCreateManagedWidget( "label89",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 2,
			XmNy, 40,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SW:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label89, (char *) UxDARManagerContext );


	/* Creation of TF_SELAT */
	TF_SELAT = XtVaCreateManagedWidget( "TF_SELAT",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 235,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SELAT, (char *) UxDARManagerContext );


	/* Creation of TF_SELON */
	TF_SELON = XtVaCreateManagedWidget( "TF_SELON",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 325,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SELON, (char *) UxDARManagerContext );


	/* Creation of label132 */
	label132 = XtVaCreateManagedWidget( "label132",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 310,
			XmNy, 45,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label132, (char *) UxDARManagerContext );


	/* Creation of label91 */
	label91 = XtVaCreateManagedWidget( "label91",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 210,
			XmNy, 40,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label91, (char *) UxDARManagerContext );


	/* Creation of label_NW1 */
	label_NW1 = XtVaCreateManagedWidget( "label_NW1",
			xmLabelWidgetClass,
			form_DARquad,
			XmNx, 2,
			XmNy, 5,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NW:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_NW1, (char *) UxDARManagerContext );


	/* Creation of TF_NELAT */
	TF_NELAT = XtVaCreateManagedWidget( "TF_NELAT",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 235,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_NELAT, (char *) UxDARManagerContext );


	/* Creation of TF_NWLAT */
	TF_NWLAT = XtVaCreateManagedWidget( "TF_NWLAT",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 29,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_NWLAT, (char *) UxDARManagerContext );


	/* Creation of TF_SWLAT */
	TF_SWLAT = XtVaCreateManagedWidget( "TF_SWLAT",
			xmTextFieldWidgetClass,
			form_DARquad,
			XmNx, 29,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SWLAT, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_SITENAME */
	TF_DAR_SITENAME = XtVaCreateManagedWidget( "TF_DAR_SITENAME",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 473,
			XmNy, 442,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 32,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DAR_SITENAME, (char *) UxDARManagerContext );


	/* Creation of label80 */
	label80 = XtVaCreateManagedWidget( "label80",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 440,
			XmNy, 441,
			XmNwidth, 36,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SITE \nNAME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label80, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_SHAPE */
	TF_DAR_SHAPE = XtVaCreateManagedWidget( "TF_DAR_SHAPE",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 375,
			XmNy, 442,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 6,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DAR_SHAPE, (char *) UxDARManagerContext );


	/* Creation of label81 */
	label81 = XtVaCreateManagedWidget( "label81",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 334,
			XmNy, 443,
			XmNwidth, 40,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SHAPE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label81, (char *) UxDARManagerContext );


	/* Creation of TF_USERID */
	TF_USERID = XtVaCreateManagedWidget( "TF_USERID",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 625,
			XmNy, 333,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 13,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 13,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_USERID, (char *) UxDARManagerContext );


	/* Creation of TF_SAT */
	TF_SAT = XtVaCreateManagedWidget( "TF_SAT",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 375,
			XmNy, 369,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 9,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SAT, (char *) UxDARManagerContext );


	/* Creation of label101 */
	label101 = XtVaCreateManagedWidget( "label101",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 323,
			XmNy, 370,
			XmNwidth, 51,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SAT/\nSENSOR:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label101, (char *) UxDARManagerContext );


	/* Creation of TF_SENSOR */
	TF_SENSOR = XtVaCreateManagedWidget( "TF_SENSOR",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 475,
			XmNy, 369,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 6,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_SENSOR, (char *) UxDARManagerContext );


	/* Creation of label102 */
	label102 = XtVaCreateManagedWidget( "label102",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 314,
			XmNy, 334,
			XmNwidth, 60,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "REQUESTED" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label102, (char *) UxDARManagerContext );


	/* Creation of TF_REQTIME */
	TF_REQTIME = XtVaCreateManagedWidget( "TF_REQTIME",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 375,
			XmNy, 333,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_REQTIME, (char *) UxDARManagerContext );


	/* Creation of separator13 */
	separator13 = XtVaCreateManagedWidget( "separator13",
			xmSeparatorWidgetClass,
			DARManager,
			XmNwidth, 744,
			XmNheight, 15,
			XmNx, 10,
			XmNy, 310,
			NULL );
	UxPutContext( separator13, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_REV */
	TF_DAR_REV = XtVaCreateManagedWidget( "TF_DAR_REV",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 216,
			XmNy, 484,
			XmNheight, 32,
			XmNcolumns, 6,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DAR_REV, (char *) UxDARManagerContext );


	/* Creation of label128 */
	label128 = XtVaCreateManagedWidget( "label128",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 186,
			XmNy, 484,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "REV:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label128, (char *) UxDARManagerContext );


	/* Creation of label100 */
	label100 = XtVaCreateManagedWidget( "label100",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 26,
			XmNy, 406,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label100, (char *) UxDARManagerContext );


	/* Creation of label104 */
	label104 = XtVaCreateManagedWidget( "label104",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 32,
			XmNy, 443,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label104, (char *) UxDARManagerContext );


	/* Creation of T_PLNRCMNT */
	T_PLNRCMNT = XtVaCreateManagedWidget( "T_PLNRCMNT",
			xmTextWidgetClass,
			DARManager,
			XmNx, 375,
			XmNy, 656,
			XmNwidth, 300,
			XmNcolumns, 200,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNrows, 4,
			XmNvalue, "",
			XmNwordWrap, TRUE,
			XmNeditable, FALSE,
			XmNheight, 112,
			XmNvalueWcs, UxConvertValueWcs("" ),
			NULL );
	UxPutContext( T_PLNRCMNT, (char *) UxDARManagerContext );


	/* Creation of TF_NOBS */
	TF_NOBS = XtVaCreateManagedWidget( "TF_NOBS",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 101,
			XmNy, 524,
			XmNheight, 32,
			XmNcolumns, 2,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_NOBS, (char *) UxDARManagerContext );


	/* Creation of label97 */
	label97 = XtVaCreateManagedWidget( "label97",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 16,
			XmNy, 524,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "OBSERVATIONS/\nFREQUENCY:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label97, (char *) UxDARManagerContext );


	/* Creation of label105 */
	label105 = XtVaCreateManagedWidget( "label105",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 133,
			XmNy, 524,
			XmNwidth, 15,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "/" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label105, (char *) UxDARManagerContext );


	/* Creation of TF_ASCDSC */
	TF_ASCDSC = XtVaCreateManagedWidget( "TF_ASCDSC",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 375,
			XmNy, 405,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 2,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 2,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_ASCDSC, (char *) UxDARManagerContext );


	/* Creation of label84 */
	label84 = XtVaCreateManagedWidget( "label84",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 346,
			XmNy, 406,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "ASC/\nDSC:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label84, (char *) UxDARManagerContext );


	/* Creation of label96 */
	label96 = XtVaCreateManagedWidget( "label96",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 47,
			XmNy, 630,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, " USER COMMENTS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label96, (char *) UxDARManagerContext );


	/* Creation of T_USERCMNT */
	T_USERCMNT = XtVaCreateManagedWidget( "T_USERCMNT",
			xmTextWidgetClass,
			DARManager,
			XmNx, 50,
			XmNy, 656,
			XmNwidth, 300,
			XmNcolumns, 200,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNrows, 4,
			XmNvalue, "",
			XmNwordWrap, TRUE,
			XmNtraversalOn, FALSE,
			XmNheight, 112,
			XmNeditable, FALSE,
			XmNvalueWcs, UxConvertValueWcs("" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8" ),
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( T_USERCMNT, (char *) UxDARManagerContext );


	/* Creation of TF_REQSTAT */
	TF_REQSTAT = XtVaCreateManagedWidget( "TF_REQSTAT",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 166,
			XmNy, 333,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 4,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 4,
			XmNmarginWidth, 4,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_REQSTAT, (char *) UxDARManagerContext );


	/* Creation of DAR_status_options */
	DAR_status_options_shell = XtVaCreatePopupShell ("DAR_status_options_shell",
			xmMenuShellWidgetClass, DARManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	DAR_status_options = XtVaCreateWidget( "DAR_status_options",
			xmRowColumnWidgetClass,
			DAR_status_options_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 0,
			XmNy, 545,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( DAR_status_options, (char *) UxDARManagerContext );


	/* Creation of DAR_status_QUE */
	DAR_status_QUE = XtVaCreateManagedWidget( "DAR_status_QUE",
			xmPushButtonWidgetClass,
			DAR_status_options,
			RES_CONVERT( XmNlabelString, "QUE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 545,
			NULL );
	UxPutContext( DAR_status_QUE, (char *) UxDARManagerContext );


	/* Creation of DAR_status_PLN */
	DAR_status_PLN = XtVaCreateManagedWidget( "DAR_status_PLN",
			xmPushButtonWidgetClass,
			DAR_status_options,
			RES_CONVERT( XmNlabelString, "PLN" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( DAR_status_PLN, (char *) UxDARManagerContext );


	/* Creation of DAR_menu_status */
	DAR_menu_status = XtVaCreateManagedWidget( "DAR_menu_status",
			xmRowColumnWidgetClass,
			DARManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, DAR_status_options,
			XmNx, 374,
			XmNy, 623,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "PLANNER COMMENTS/STATUS:" ),
			NULL );
	UxPutContext( DAR_menu_status, (char *) UxDARManagerContext );


	/* Creation of pushButton_EditDAR */
	pushButton_EditDAR = XtVaCreateManagedWidget( "pushButton_EditDAR",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 29,
			XmNy, 775,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "EDIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_EditDAR, XmNactivateCallback,
		(XtCallbackProc) cb_set_darmanager_editability,
		(XtPointer) True );

	UxPutContext( pushButton_EditDAR, (char *) UxDARManagerContext );


	/* Creation of pushButton_SearchDAR */
	pushButton_SearchDAR = XtVaCreateManagedWidget( "pushButton_SearchDAR",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 105,
			XmNy, 265,
			RES_CONVERT( XmNlabelString, "SEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SearchDAR, (char *) UxDARManagerContext );


	/* Creation of menuBar_DELETE */
	menuBar_DELETE = XtVaCreateManagedWidget( "menuBar_DELETE",
			xmRowColumnWidgetClass,
			DARManager,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 133,
			XmNy, 775,
			XmNwidth, 90,
			XmNheight, 40,
			XmNresizable, TRUE,
			XmNmarginHeight, 5,
			XmNmarginWidth, 8,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( menuBar_DELETE, (char *) UxDARManagerContext );


	/* Creation of menuBar_p1 */
	menuBar_p1_shell = XtVaCreatePopupShell ("menuBar_p1_shell",
			xmMenuShellWidgetClass, menuBar_DELETE,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p1 = XtVaCreateWidget( "menuBar_p1",
			xmRowColumnWidgetClass,
			menuBar_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNheight, 40,
			XmNwidth, 90,
			XmNx, 0,
			XmNy, 704,
			NULL );
	UxPutContext( menuBar_p1, (char *) UxDARManagerContext );


	/* Creation of menuBar_p_b_REJECTED */
	menuBar_p_b_REJECTED = XtVaCreateManagedWidget( "menuBar_p_b_REJECTED",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "REJECTED" ),
			XmNx, 2,
			XmNy, 729,
			NULL );
	XtAddCallback( menuBar_p_b_REJECTED, XmNactivateCallback,
		(XtCallbackProc) cb_delete_dar_record,
		(XtPointer) APS_REJECTED_STATUS );

	UxPutContext( menuBar_p_b_REJECTED, (char *) UxDARManagerContext );


	/* Creation of menuBar_p_b_COMPLETED */
	menuBar_p_b_COMPLETED = XtVaCreateManagedWidget( "menuBar_p_b_COMPLETED",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "COMPLETED" ),
			XmNx, 2,
			XmNy, 729,
			NULL );
	XtAddCallback( menuBar_p_b_COMPLETED, XmNactivateCallback,
		(XtCallbackProc) cb_delete_dar_record,
		(XtPointer) APS_COMPLETED_STATUS );

	UxPutContext( menuBar_p_b_COMPLETED, (char *) UxDARManagerContext );


	/* Creation of menuBar_top_b1 */
	menuBar_top_b1 = XtVaCreateManagedWidget( "menuBar_top_b1",
			xmCascadeButtonWidgetClass,
			menuBar_DELETE,
			RES_CONVERT( XmNlabelString, "DELETE" ),
			XmNsubMenuId, menuBar_p1,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 40,
			XmNwidth, 90,
			XmNx, 10,
			XmNy, 728,
			XmNmarginHeight, 5,
			NULL );
	UxPutContext( menuBar_top_b1, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_sortclause */
	TF_DAR_sortclause = XtVaCreateManagedWidget( "TF_DAR_sortclause",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 455,
			XmNy, 268,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "reqtime desc",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DAR_sortclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_dar_records,
		(XtPointer) scrolledList_DARS );

	UxPutContext( TF_DAR_sortclause, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_recordcount */
	TF_DAR_recordcount = XtVaCreateManagedWidget( "TF_DAR_recordcount",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 680,
			XmNy, 269,
			XmNheight, 30,
			XmNcolumns, 5,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DAR_recordcount, (char *) UxDARManagerContext );


	/* Creation of label108 */
	label108 = XtVaCreateManagedWidget( "label108",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 640,
			XmNy, 269,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "RECORD\nCOUNT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label108, (char *) UxDARManagerContext );


	/* Creation of pushButton_SaveDARChanges */
	pushButton_SaveDARChanges = XtVaCreateWidget( "pushButton_SaveDARChanges",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 369,
			XmNy, 775,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "SAVE\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_SaveDARChanges, XmNactivateCallback,
		(XtCallbackProc) cb_save_dar_changes,
		(XtPointer) False );

	UxPutContext( pushButton_SaveDARChanges, (char *) UxDARManagerContext );


	/* Creation of menuBar_LOAD_DAR */
	menuBar_LOAD_DAR = XtVaCreateManagedWidget( "menuBar_LOAD_DAR",
			xmRowColumnWidgetClass,
			DARManager,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 12,
			XmNy, 267,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNpacking, XmPACK_TIGHT,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNmarginHeight, 2,
			XmNmarginWidth, 0,
			XmNwidth, 85,
			XmNheight, 35,
			NULL );
	UxPutContext( menuBar_LOAD_DAR, (char *) UxDARManagerContext );


	/* Creation of menuBar_FILE */
	menuBar_FILE_shell = XtVaCreatePopupShell ("menuBar_FILE_shell",
			xmMenuShellWidgetClass, menuBar_LOAD_DAR,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_FILE = XtVaCreateWidget( "menuBar_FILE",
			xmRowColumnWidgetClass,
			menuBar_FILE_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNpacking, XmPACK_COLUMN,
			XmNwidth, 200,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNx, 0,
			XmNy, 249,
			NULL );
	UxPutContext( menuBar_FILE, (char *) UxDARManagerContext );


	/* Creation of pushButton_LOAD_DARS */
	pushButton_LOAD_DARS = XtVaCreateManagedWidget( "pushButton_LOAD_DARS",
			xmPushButtonWidgetClass,
			menuBar_FILE,
			RES_CONVERT( XmNlabelString, "LOAD IMS DARS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	XtAddCallback( pushButton_LOAD_DARS, XmNactivateCallback,
		(XtCallbackProc) cb_load_dars_from_ims,
		(XtPointer) LOAD_DARS_APPEND );

	UxPutContext( pushButton_LOAD_DARS, (char *) UxDARManagerContext );


	/* Creation of PANE_SAVE_DAR_RPT */
	PANE_SAVE_DAR_RPT_shell = XtVaCreatePopupShell ("PANE_SAVE_DAR_RPT_shell",
			xmMenuShellWidgetClass, menuBar_FILE,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	PANE_SAVE_DAR_RPT = XtVaCreateWidget( "PANE_SAVE_DAR_RPT",
			xmRowColumnWidgetClass,
			PANE_SAVE_DAR_RPT_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 249,
			NULL );
	UxPutContext( PANE_SAVE_DAR_RPT, (char *) UxDARManagerContext );


	/* Creation of RPT_SELECTED_DAR */
	RPT_SELECTED_DAR = XtVaCreateManagedWidget( "RPT_SELECTED_DAR",
			xmPushButtonWidgetClass,
			PANE_SAVE_DAR_RPT,
			RES_CONVERT( XmNlabelString, "SELECTED DAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	XtAddCallback( RPT_SELECTED_DAR, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dars_to_file_cb,
		(XtPointer) PRINT_DARS_SELECTED_TO_FILE );

	UxPutContext( RPT_SELECTED_DAR, (char *) UxDARManagerContext );


	/* Creation of RPT_CURRENT_DARS */
	RPT_CURRENT_DARS = XtVaCreateManagedWidget( "RPT_CURRENT_DARS",
			xmPushButtonWidgetClass,
			PANE_SAVE_DAR_RPT,
			RES_CONVERT( XmNlabelString, "CURRENT DARS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	XtAddCallback( RPT_CURRENT_DARS, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dars_to_file_cb,
		(XtPointer) PRINT_DARS_CURRENT_TO_FILE );

	UxPutContext( RPT_CURRENT_DARS, (char *) UxDARManagerContext );


	/* Creation of RPT_ALL_DARS */
	RPT_ALL_DARS = XtVaCreateManagedWidget( "RPT_ALL_DARS",
			xmPushButtonWidgetClass,
			PANE_SAVE_DAR_RPT,
			RES_CONVERT( XmNlabelString, "ALL DARS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	XtAddCallback( RPT_ALL_DARS, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dars_to_file_cb,
		(XtPointer) PRINT_DARS_ALL_TO_FILE );

	UxPutContext( RPT_ALL_DARS, (char *) UxDARManagerContext );


	/* Creation of cascadeButton_SAVE_DAR_RPT */
	cascadeButton_SAVE_DAR_RPT = XtVaCreateManagedWidget( "cascadeButton_SAVE_DAR_RPT",
			xmCascadeButtonWidgetClass,
			menuBar_FILE,
			RES_CONVERT( XmNlabelString, "SAVE DAR REPORT" ),
			XmNsubMenuId, PANE_SAVE_DAR_RPT,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	UxPutContext( cascadeButton_SAVE_DAR_RPT, (char *) UxDARManagerContext );


	/* Creation of PANE_PRINT_DAR_RPT */
	PANE_PRINT_DAR_RPT_shell = XtVaCreatePopupShell ("PANE_PRINT_DAR_RPT_shell",
			xmMenuShellWidgetClass, menuBar_FILE,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	PANE_PRINT_DAR_RPT = XtVaCreateWidget( "PANE_PRINT_DAR_RPT",
			xmRowColumnWidgetClass,
			PANE_PRINT_DAR_RPT_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 250,
			NULL );
	UxPutContext( PANE_PRINT_DAR_RPT, (char *) UxDARManagerContext );


	/* Creation of PANE_PRINT_DARS_b4 */
	PANE_PRINT_DARS_b4 = XtVaCreateManagedWidget( "PANE_PRINT_DARS_b4",
			xmSeparatorGadgetClass,
			PANE_PRINT_DAR_RPT,
			XmNx, 2,
			XmNy, 283,
			NULL );
	UxPutContext( PANE_PRINT_DARS_b4, (char *) UxDARManagerContext );


	/* Creation of PRINT_SELECTED_DAR */
	PRINT_SELECTED_DAR = XtVaCreateManagedWidget( "PRINT_SELECTED_DAR",
			xmPushButtonWidgetClass,
			PANE_PRINT_DAR_RPT,
			RES_CONVERT( XmNlabelString, "SELECTED DAR" ),
			XmNx, 2,
			XmNy, 274,
			NULL );
	XtAddCallback( PRINT_SELECTED_DAR, XmNactivateCallback,
		(XtCallbackProc) cb_print_dars,
		(XtPointer) PRINT_DARS_SELECTED );

	UxPutContext( PRINT_SELECTED_DAR, (char *) UxDARManagerContext );


	/* Creation of PRINT_CURRENT_DARS */
	PRINT_CURRENT_DARS = XtVaCreateManagedWidget( "PRINT_CURRENT_DARS",
			xmPushButtonWidgetClass,
			PANE_PRINT_DAR_RPT,
			RES_CONVERT( XmNlabelString, "CURRENT DARS" ),
			XmNx, 2,
			XmNy, 274,
			NULL );
	XtAddCallback( PRINT_CURRENT_DARS, XmNactivateCallback,
		(XtCallbackProc) cb_print_dars,
		(XtPointer) PRINT_DARS_CURRENT );

	UxPutContext( PRINT_CURRENT_DARS, (char *) UxDARManagerContext );


	/* Creation of PRINT_ALL_DARS */
	PRINT_ALL_DARS = XtVaCreateManagedWidget( "PRINT_ALL_DARS",
			xmPushButtonWidgetClass,
			PANE_PRINT_DAR_RPT,
			RES_CONVERT( XmNlabelString, "ALL DARS" ),
			XmNx, 2,
			XmNy, 274,
			NULL );
	XtAddCallback( PRINT_ALL_DARS, XmNactivateCallback,
		(XtCallbackProc) cb_print_dars,
		(XtPointer) PRINT_DARS_ALL );

	UxPutContext( PRINT_ALL_DARS, (char *) UxDARManagerContext );


	/* Creation of cascadeButton_PRINT_DAR_RPT */
	cascadeButton_PRINT_DAR_RPT = XtVaCreateManagedWidget( "cascadeButton_PRINT_DAR_RPT",
			xmCascadeButtonWidgetClass,
			menuBar_FILE,
			RES_CONVERT( XmNlabelString, "PRINT DAR REPORT" ),
			XmNsubMenuId, PANE_PRINT_DAR_RPT,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 273,
			NULL );
	UxPutContext( cascadeButton_PRINT_DAR_RPT, (char *) UxDARManagerContext );


	/* Creation of menuBar_top_b4 */
	menuBar_top_b4 = XtVaCreateManagedWidget( "menuBar_top_b4",
			xmCascadeButtonWidgetClass,
			menuBar_LOAD_DAR,
			RES_CONVERT( XmNlabelString, "FILE" ),
			XmNsubMenuId, menuBar_FILE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 270,
			XmNalignment, XmALIGNMENT_CENTER,
			XmNrecomputeSize, FALSE,
			XmNwidth, 80,
			XmNmarginWidth, 0,
			XmNmarginHeight, 0,
			XmNheight, 25,
			NULL );
	UxPutContext( menuBar_top_b4, (char *) UxDARManagerContext );


	/* Creation of pushButton_CancelDARChanges */
	pushButton_CancelDARChanges = XtVaCreateWidget( "pushButton_CancelDARChanges",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 499,
			XmNy, 775,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CANCEL\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_CancelDARChanges, XmNactivateCallback,
		(XtCallbackProc) cb_save_dar_changes,
		(XtPointer) True );

	UxPutContext( pushButton_CancelDARChanges, (char *) UxDARManagerContext );


	/* Creation of TF_DAR_searchclause */
	TF_DAR_searchclause = XtVaCreateManagedWidget( "TF_DAR_searchclause",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 185,
			XmNy, 268,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "where darid > 0",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DAR_searchclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_dar_records,
		(XtPointer) scrolledList_DARS );

	UxPutContext( TF_DAR_searchclause, (char *) UxDARManagerContext );


	/* Creation of scrolledWindowText7 */
	scrolledWindowText7 = XtVaCreateManagedWidget( "scrolledWindowText7",
			xmScrolledWindowWidgetClass,
			DARManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 770,
			XmNy, 35,
			NULL );
	UxPutContext( scrolledWindowText7, (char *) UxDARManagerContext );


	/* Creation of scrolledWindowText8 */
	scrolledWindowText8 = XtVaCreateManagedWidget( "scrolledWindowText8",
			xmScrolledWindowWidgetClass,
			DARManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 770,
			XmNy, 125,
			NULL );
	UxPutContext( scrolledWindowText8, (char *) UxDARManagerContext );


	/* Creation of pushButton2 */
	pushButton2 = XtVaCreateManagedWidget( "pushButton2",
			xmPushButtonWidgetClass,
			DARManager,
			XmNx, 20,
			XmNy, 100,
			XmNwidth, 30,
			XmNheight, 130,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( pushButton2, XmNactivateCallback,
		(XtCallbackProc) cb_show_dar_records,
		(XtPointer) scrolledList_DARS );

	UxPutContext( pushButton2, (char *) UxDARManagerContext );


	/* Creation of label125 */
	label125 = XtVaCreateManagedWidget( "label125",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 50,
			XmNy, 334,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DAR ID/\nSTATUS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label125, (char *) UxDARManagerContext );


	/* Creation of label126 */
	label126 = XtVaCreateManagedWidget( "label126",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 592,
			XmNy, 334,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "USER\n ID " ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label126, (char *) UxDARManagerContext );


	/* Creation of label49 */
	label49 = XtVaCreateManagedWidget( "label49",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 31,
			XmNy, 365,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "QUICKLOOK:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 65,
			NULL );
	UxPutContext( label49, (char *) UxDARManagerContext );


	/* Creation of LABEL_QUICKLOOK */
	LABEL_QUICKLOOK = XtVaCreateManagedWidget( "LABEL_QUICKLOOK",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 97,
			XmNy, 370,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "No" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 30,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( LABEL_QUICKLOOK, (char *) UxDARManagerContext );


	/* Creation of T_FOBS */
	T_FOBS = XtVaCreateManagedWidget( "T_FOBS",
			xmTextWidgetClass,
			DARManager,
			XmNwidth, 168,
			XmNx, 149,
			XmNy, 524,
			XmNheight, 45,
			XmNcursorPositionVisible, FALSE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNvalue, "",
			XmNtraversalOn, FALSE,
			XmNwordWrap, TRUE,
			XmNvalueWcs, UxConvertValueWcs("" ),
			XmNnavigationType, XmTAB_GROUP,
			NULL );
	UxPutContext( T_FOBS, (char *) UxDARManagerContext );


	/* Creation of label79 */
	label79 = XtVaCreateManagedWidget( "label79",
			xmLabelWidgetClass,
			DARManager,
			XmNx, 4,
			XmNy, 573,
			XmNwidth, 91,
			XmNheight, 47,
			RES_CONVERT( XmNlabelString, "J1 OBSERVATION\nFREQUENCY\n(IN DAYS):" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label79, (char *) UxDARManagerContext );


	/* Creation of TF_J1OBS */
	TF_J1OBS = XtVaCreateManagedWidget( "TF_J1OBS",
			xmTextFieldWidgetClass,
			DARManager,
			XmNx, 100,
			XmNy, 580,
			XmNheight, 32,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, TRUE,
			XmNeditable, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNtraversalOn, TRUE,
			XmNwidth, 83,
			NULL );
	UxPutContext( TF_J1OBS, (char *) UxDARManagerContext );


	XtAddCallback( DARManager, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxDARManagerContext);


	return ( DARManager );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_DARManager( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCDARManager          *UxContext;
	static int		_Uxinit = 0;

	UxDARManagerContext = UxContext =
		(_UxCDARManager *) UxNewContext( sizeof(_UxCDARManager), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		Position x, y ;
		SORT_INFO *sortinfo ;
		SEARCH_INFO *searchinfo ;
		PERIOD_WIDGETS *DAR_times ;
		rtrn = _Uxbuild_DARManager();

		sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;
		searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;
		
		sortinfo->table_name =  (char *) APS_TABLE(DAR) ;
		sortinfo->field_to_update = (Widget) TF_DAR_sortclause ;
		
		searchinfo->table_name = APS_TABLE(DAR) ;
		searchinfo->field_to_update  = (Widget) TF_DAR_searchclause ;
		
		DAR_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		DAR_times->start = (Widget) TF_DAR_STRTTIME ;
		DAR_times->stop = (Widget) TF_DAR_ENDTIME ;
		
		XtAddCallback( pushButton_SortDAR, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_sort_columns,
		    (XtPointer) sortinfo );
		 
		XtAddCallback( pushButton_SearchDAR, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_search_columns,
		    (XtPointer) searchinfo );
		 
		XtAddCallback(TF_DAR_total_days, XmNactivateCallback,
		    (XtCallbackProc) cb_adjust_ASF_datetimes,
		    (XtPointer) DAR_times) ;
		
		 /*
		    -- the form_quad is the default shape form
		    -- at start up get its (x,y) position for use
		    -- by the form_circle
		    --
		    -- the circle form is constructed to the right
		    -- of the quad form using VC.  It is not placed
		    -- directly on top because may think its a form
		    -- within a form (form_quad --> form_circle)
		    */
		    XtVaGetValues(form_DARquad,
		        XmNx, &x,
		        XmNy, &y,
		        NULL) ;
		
		     XtVaSetValues(form_DARcircle,
		        XmNx, x,
		        XmNy, y,
		        NULL) ;
		
			XtAddCallback(XtParent(rtrn), XtNpopupCallback,
				cb_show_dar_records, (XtPointer *) scrolledList_DARS) ;
		 
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

