
/*******************************************************************************
	vc_dtkmanager.c

       Associated Header file: vc_dtkmanager.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
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
#pragma ident   "@(#)DTKManager.i	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.DTKManager.i"

#include <stdlib.h>

#include <Xm/FileSB.h>
#include <Xm/DialogS.h>

#include "db_sybint.h"
#include "aps_db_table.h"

#include "gui_utils.h"
#include "satmenus.h"
#include "cb_datetime.h"
#include "cb_sortform.h"
#include "cb_searchform.h"
#include "cb_dtkmanager.h"

extern void cb_format_float() ;

extern Widget DTK_manager;
extern Widget filebox ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_dtkmanager.h"
#undef CONTEXT_MACRO_ACCESS

Widget	DTKManager;
Widget	pushButton_CancelDTKChanges;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_QuitDTK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCDTKManager          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxDTKManagerContext;
	UxDTKManagerContext = UxContext =
			(_UxCDTKManager *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(DTK_manager)) ;
	}
	UxDTKManagerContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_DTKManager()
{
	Widget		_UxParent;
	Widget		subMenu_dtk_status_shell;
	Widget		menuBar_DTK_FILE_pane_shell;
	Widget		PANE_SAVE_DTK_RPT_shell;
	Widget		PANE_PRINT_DTK_RPT_shell;
	Widget		subMenu_sat_shell;
	Widget		subMenu_sensor_shell;
	Widget		subMenu_dtk_direction_shell;
	Widget		subMenu_J1_DLinkChannel_shell;
	Widget		subMenu_dtkm_stnid_shell;
	Widget		subMenu_antenna_shell;
	Widget		subMenu_PlanQuicklook_shell;


	/* Creation of DTKManager */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "DTKManager_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 246,
			XmNy, 94,
			XmNwidth, 887,
			XmNheight, 752,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "DTKManager",
			XmNiconName, "DTKManager",
			NULL );

	}

	DTKManager = XtVaCreateManagedWidget( "DTKManager",
			xmFormWidgetClass,
			_UxParent,
			XmNheight, 752,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:DTK Manager" ),
			XmNwidth, 887,
			NULL );
	UxPutContext( DTKManager, (char *) UxDTKManagerContext );
	UxPutClassCode( DTKManager, _UxIfClassId );


	/* Creation of label148 */
	label148 = XtVaCreateManagedWidget( "label148",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 17,
			XmNy, 9,
			XmNwidth, 828,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "DOWN-LINK  and  DATA-TAKE  MANAGER" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label148, (char *) UxDTKManagerContext );


	/* Creation of scrolledWindowList_DTKS */
	scrolledWindowList_DTKS = XtVaCreateManagedWidget( "scrolledWindowList_DTKS",
			xmScrolledWindowWidgetClass,
			DTKManager,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 73,
			XmNy, 79,
			XmNwidth, 770,
			NULL );
	UxPutContext( scrolledWindowList_DTKS, (char *) UxDTKManagerContext );


	/* Creation of scrolledList_DTKS */
	scrolledList_DTKS = XtVaCreateManagedWidget( "scrolledList_DTKS",
			xmListWidgetClass,
			scrolledWindowList_DTKS,
			XmNwidth, 770,
			XmNheight, 144,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "RADARSAT/SAR  4500  1 1994:161  1994:266:11:22:33  1994:109:11:22:33  RAG   078106/018/000001" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 89,
			XmNvisibleItemCount, 9,
			NULL );
	XtAddCallback( scrolledList_DTKS, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_dtkmanager_form,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( scrolledList_DTKS, (char *) UxDTKManagerContext );


	/* Creation of TF_DTKID */
	TF_DTKID = XtVaCreateManagedWidget( "TF_DTKID",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 528,
			XmNy, 324,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "0",
			XmNcolumns, 3,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 3,
			NULL );
	UxPutContext( TF_DTKID, (char *) UxDTKManagerContext );


	/* Creation of pushButton_SearchDTK */
	pushButton_SearchDTK = XtVaCreateManagedWidget( "pushButton_SearchDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 144,
			XmNy, 260,
			RES_CONVERT( XmNlabelString, "SEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SearchDTK, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_searchclause */
	TF_DTK_searchclause = XtVaCreateManagedWidget( "TF_DTK_searchclause",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 224,
			XmNy, 263,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "where dtkid > 0",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DTK_searchclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_dtk_records,
		(XtPointer) scrolledList_DTKS );

	UxPutContext( TF_DTK_searchclause, (char *) UxDTKManagerContext );


	/* Creation of pushButton_SortDTK */
	pushButton_SortDTK = XtVaCreateManagedWidget( "pushButton_SortDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 414,
			RES_CONVERT( XmNlabelString, "SORT BY" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNy, 260,
			XmNheight, 38,
			XmNwidth, 80,
			NULL );
	UxPutContext( pushButton_SortDTK, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_sortclause */
	TF_DTK_sortclause = XtVaCreateManagedWidget( "TF_DTK_sortclause",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 494,
			XmNy, 263,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "strttime",
			XmNcolumns, 256,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 180,
			XmNtraversalOn, FALSE,
			NULL );
	XtAddCallback( TF_DTK_sortclause, XmNvalueChangedCallback,
		(XtCallbackProc) cb_show_dtk_records,
		(XtPointer) scrolledList_DTKS );

	UxPutContext( TF_DTK_sortclause, (char *) UxDTKManagerContext );


	/* Creation of label136 */
	label136 = XtVaCreateManagedWidget( "label136",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 73,
			XmNy, 49,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "   SAT/SENSOR   REV ID LAST MOD     START TIME         STOP TIME    STATUS  FA DTK ID" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNwidth, 710,
			NULL );
	UxPutContext( label136, (char *) UxDTKManagerContext );


	/* Creation of form_DTKquad */
	form_DTKquad = XtVaCreateManagedWidget( "form_DTKquad",
			xmFormWidgetClass,
			DTKManager,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 429,
			XmNy, 608,
			XmNwidth, 445,
			XmNheight, 80,
			NULL );
	UxPutContext( form_DTKquad, (char *) UxDTKManagerContext );


	/* Creation of TF_NRLON1 */
	TF_NRLON1 = XtVaCreateManagedWidget( "TF_NRLON1",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 135,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_NRLON1, (char *) UxDTKManagerContext );


	/* Creation of label143 */
	label143 = XtVaCreateManagedWidget( "label143",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 125,
			XmNy, 10,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label143, (char *) UxDTKManagerContext );


	/* Creation of label144 */
	label144 = XtVaCreateManagedWidget( "label144",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 235,
			XmNy, 5,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "FAR  \nPT 1:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label144, (char *) UxDTKManagerContext );


	/* Creation of TF_NRLON2 */
	TF_NRLON2 = XtVaCreateManagedWidget( "TF_NRLON2",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 135,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_NRLON2, (char *) UxDTKManagerContext );


	/* Creation of label142 */
	label142 = XtVaCreateManagedWidget( "label142",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 125,
			XmNy, 45,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label142, (char *) UxDTKManagerContext );


	/* Creation of label147 */
	label147 = XtVaCreateManagedWidget( "label147",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 10,
			XmNy, 5,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NEAR \nPT 1:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label147, (char *) UxDTKManagerContext );


	/* Creation of TF_NRLAT1 */
	TF_NRLAT1 = XtVaCreateManagedWidget( "TF_NRLAT1",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 45,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_NRLAT1, (char *) UxDTKManagerContext );


	/* Creation of TF_NRLAT2 */
	TF_NRLAT2 = XtVaCreateManagedWidget( "TF_NRLAT2",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 45,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_NRLAT2, (char *) UxDTKManagerContext );


	/* Creation of label_NW2 */
	label_NW2 = XtVaCreateManagedWidget( "label_NW2",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 10,
			XmNy, 40,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NEAR \nPT 2:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_NW2, (char *) UxDTKManagerContext );


	/* Creation of label145 */
	label145 = XtVaCreateManagedWidget( "label145",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 235,
			XmNy, 40,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "FAR  \nPT 2:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label145, (char *) UxDTKManagerContext );


	/* Creation of TF_FARLON1 */
	TF_FARLON1 = XtVaCreateManagedWidget( "TF_FARLON1",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 360,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_FARLON1, (char *) UxDTKManagerContext );


	/* Creation of label87 */
	label87 = XtVaCreateManagedWidget( "label87",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 350,
			XmNy, 10,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label87, (char *) UxDTKManagerContext );


	/* Creation of TF_FARLAT2 */
	TF_FARLAT2 = XtVaCreateManagedWidget( "TF_FARLAT2",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 270,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_FARLAT2, (char *) UxDTKManagerContext );


	/* Creation of TF_FARLON2 */
	TF_FARLON2 = XtVaCreateManagedWidget( "TF_FARLON2",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 360,
			XmNy, 40,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_FARLON2, (char *) UxDTKManagerContext );


	/* Creation of label141 */
	label141 = XtVaCreateManagedWidget( "label141",
			xmLabelWidgetClass,
			form_DTKquad,
			XmNx, 350,
			XmNy, 45,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label141, (char *) UxDTKManagerContext );


	/* Creation of TF_FARLAT1 */
	TF_FARLAT1 = XtVaCreateManagedWidget( "TF_FARLAT1",
			xmTextFieldWidgetClass,
			form_DTKquad,
			XmNx, 270,
			XmNy, 5,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_FARLAT1, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_SITENAME */
	TF_DTK_SITENAME = XtVaCreateManagedWidget( "TF_DTK_SITENAME",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 362,
			XmNy, 521,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 32,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 32,
			NULL );
	UxPutContext( TF_DTK_SITENAME, (char *) UxDTKManagerContext );


	/* Creation of label140 */
	label140 = XtVaCreateManagedWidget( "label140",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 322,
			XmNy, 522,
			XmNwidth, 35,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SITE \nNAME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label140, (char *) UxDTKManagerContext );


	/* Creation of label83 */
	label83 = XtVaCreateManagedWidget( "label83",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 616,
			XmNy, 441,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "FA DTK ID:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label83, (char *) UxDTKManagerContext );


	/* Creation of TF_FADTKID */
	TF_FADTKID = XtVaCreateManagedWidget( "TF_FADTKID",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 683,
			XmNy, 440,
			XmNheight, 32,
			XmNcolumns, 20,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 20,
			XmNwidth, 186,
			NULL );
	UxPutContext( TF_FADTKID, (char *) UxDTKManagerContext );


	/* Creation of separator12 */
	separator12 = XtVaCreateManagedWidget( "separator12",
			xmSeparatorWidgetClass,
			DTKManager,
			XmNwidth, 888,
			XmNheight, 15,
			XmNx, -1,
			XmNy, 305,
			NULL );
	UxPutContext( separator12, (char *) UxDTKManagerContext );


	/* Creation of TF_REV */
	TF_REV = XtVaCreateManagedWidget( "TF_REV",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 362,
			XmNy, 324,
			XmNheight, 30,
			XmNcolumns, 6,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( TF_REV, (char *) UxDTKManagerContext );


	/* Creation of label103 */
	label103 = XtVaCreateManagedWidget( "label103",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 333,
			XmNy, 324,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "REV:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label103, (char *) UxDTKManagerContext );


	/* Creation of label139 */
	label139 = XtVaCreateManagedWidget( "label139",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 36,
			XmNy, 415,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label139, (char *) UxDTKManagerContext );


	/* Creation of label138 */
	label138 = XtVaCreateManagedWidget( "label138",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 42,
			XmNy, 459,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label138, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_DARID */
	TF_DTK_DARID = XtVaCreateManagedWidget( "TF_DTK_DARID",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 811,
			XmNy, 324,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_DTK_DARID, (char *) UxDTKManagerContext );


	/* Creation of label106 */
	label106 = XtVaCreateManagedWidget( "label106",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 774,
			XmNy, 324,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DARID:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 38,
			NULL );
	UxPutContext( label106, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status */
	subMenu_dtk_status_shell = XtVaCreatePopupShell ("subMenu_dtk_status_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_dtk_status = XtVaCreateWidget( "subMenu_dtk_status",
			xmRowColumnWidgetClass,
			subMenu_dtk_status_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 796,
			XmNy, 0,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_dtk_status, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_PLN */
	subMenu_dtk_status_PLN = XtVaCreateManagedWidget( "subMenu_dtk_status_PLN",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "PLN" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 24,
			NULL );
	UxPutContext( subMenu_dtk_status_PLN, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_QUE */
	subMenu_dtk_status_QUE = XtVaCreateManagedWidget( "subMenu_dtk_status_QUE",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "QUE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 2,
			NULL );
	UxPutContext( subMenu_dtk_status_QUE, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_SUB */
	subMenu_dtk_status_SUB = XtVaCreateManagedWidget( "subMenu_dtk_status_SUB",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "SUB" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 112,
			NULL );
	UxPutContext( subMenu_dtk_status_SUB, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_SCH */
	subMenu_dtk_status_SCH = XtVaCreateManagedWidget( "subMenu_dtk_status_SCH",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "SCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 90,
			NULL );
	UxPutContext( subMenu_dtk_status_SCH, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_CON */
	subMenu_dtk_status_CON = XtVaCreateManagedWidget( "subMenu_dtk_status_CON",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "CON" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( subMenu_dtk_status_CON, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_REJ */
	subMenu_dtk_status_REJ = XtVaCreateManagedWidget( "subMenu_dtk_status_REJ",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "REJ" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 46,
			NULL );
	UxPutContext( subMenu_dtk_status_REJ, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_DEL */
	subMenu_dtk_status_DEL = XtVaCreateManagedWidget( "subMenu_dtk_status_DEL",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "DEL" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 800,
			XmNy, 68,
			NULL );
	UxPutContext( subMenu_dtk_status_DEL, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_status_INV */
	subMenu_dtk_status_INV = XtVaCreateManagedWidget( "subMenu_dtk_status_INV",
			xmPushButtonWidgetClass,
			subMenu_dtk_status,
			RES_CONVERT( XmNlabelString, "INV" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( subMenu_dtk_status_INV, (char *) UxDTKManagerContext );


	/* Creation of optionMenu_dtk_status */
	optionMenu_dtk_status = XtVaCreateManagedWidget( "optionMenu_dtk_status",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_dtk_status,
			XmNx, 57,
			XmNy, 364,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "STATUS:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_dtk_status, (char *) UxDTKManagerContext );


	/* Creation of pushButton_EditDTK */
	pushButton_EditDTK = XtVaCreateManagedWidget( "pushButton_EditDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 38,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "EDIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_EditDTK, XmNactivateCallback,
		(XtCallbackProc) cb_set_dtkmanager_editability,
		(XtPointer) DTK_EDIT );

	UxPutContext( pushButton_EditDTK, (char *) UxDTKManagerContext );


	/* Creation of pushButton_DeleteDTK */
	pushButton_DeleteDTK = XtVaCreateManagedWidget( "pushButton_DeleteDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 149,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "DELETE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_DeleteDTK, XmNactivateCallback,
		(XtCallbackProc) cb_delete_dtk_record,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( pushButton_DeleteDTK, (char *) UxDTKManagerContext );


	/* Creation of pushButton_QuitDTK */
	pushButton_QuitDTK = XtVaCreateManagedWidget( "pushButton_QuitDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 749,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_QuitDTK, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_QuitDTK,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( pushButton_QuitDTK, (char *) UxDTKManagerContext );


	/* Creation of pushButton_ClearDTKForm */
	pushButton_ClearDTKForm = XtVaCreateWidget( "pushButton_ClearDTKForm",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 399,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CLEAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_ClearDTKForm, XmNactivateCallback,
		(XtCallbackProc) cb_clear_dtkmanager_form,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( pushButton_ClearDTKForm, (char *) UxDTKManagerContext );


	/* Creation of pushButton_SaveDTKChanges */
	pushButton_SaveDTKChanges = XtVaCreateWidget( "pushButton_SaveDTKChanges",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 508,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "SAVE\nCHANGES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_SaveDTKChanges, XmNactivateCallback,
		(XtCallbackProc) cb_save_dtk_changes,
		(XtPointer) False );

	UxPutContext( pushButton_SaveDTKChanges, (char *) UxDTKManagerContext );


	/* Creation of pushButton_CancelDTKChanges */
	pushButton_CancelDTKChanges = XtVaCreateWidget( "pushButton_CancelDTKChanges",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 617,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CANCEL\nEDIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_CancelDTKChanges, XmNactivateCallback,
		(XtCallbackProc) cb_set_dtkmanager_editability,
		(XtPointer) DTK_RESET );

	UxPutContext( pushButton_CancelDTKChanges, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_recordcount */
	TF_DTK_recordcount = XtVaCreateManagedWidget( "TF_DTK_recordcount",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 719,
			XmNy, 263,
			XmNheight, 32,
			XmNcolumns, 5,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DTK_recordcount, (char *) UxDTKManagerContext );


	/* Creation of label137 */
	label137 = XtVaCreateManagedWidget( "label137",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 679,
			XmNy, 264,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "RECORD\nCOUNT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label137, (char *) UxDTKManagerContext );


	/* Creation of menuBar_DTK_FILE */
	menuBar_DTK_FILE = XtVaCreateManagedWidget( "menuBar_DTK_FILE",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 49,
			XmNy, 262,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNpacking, XmPACK_COLUMN,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNmarginHeight, 2,
			XmNmarginWidth, 0,
			XmNwidth, 80,
			XmNheight, 34,
			NULL );
	UxPutContext( menuBar_DTK_FILE, (char *) UxDTKManagerContext );


	/* Creation of menuBar_DTK_FILE_pane */
	menuBar_DTK_FILE_pane_shell = XtVaCreatePopupShell ("menuBar_DTK_FILE_pane_shell",
			xmMenuShellWidgetClass, menuBar_DTK_FILE,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_DTK_FILE_pane = XtVaCreateWidget( "menuBar_DTK_FILE_pane",
			xmRowColumnWidgetClass,
			menuBar_DTK_FILE_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNpacking, XmPACK_COLUMN,
			XmNwidth, 200,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNx, 0,
			XmNy, 244,
			NULL );
	UxPutContext( menuBar_DTK_FILE_pane, (char *) UxDTKManagerContext );


	/* Creation of PANE_SAVE_DTK_RPT */
	PANE_SAVE_DTK_RPT_shell = XtVaCreatePopupShell ("PANE_SAVE_DTK_RPT_shell",
			xmMenuShellWidgetClass, menuBar_DTK_FILE_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	PANE_SAVE_DTK_RPT = XtVaCreateWidget( "PANE_SAVE_DTK_RPT",
			xmRowColumnWidgetClass,
			PANE_SAVE_DTK_RPT_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 244,
			NULL );
	UxPutContext( PANE_SAVE_DTK_RPT, (char *) UxDTKManagerContext );


	/* Creation of RPT_SELECTED_DTK */
	RPT_SELECTED_DTK = XtVaCreateManagedWidget( "RPT_SELECTED_DTK",
			xmPushButtonWidgetClass,
			PANE_SAVE_DTK_RPT,
			RES_CONVERT( XmNlabelString, "SELECTED DTK" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNx, 2,
			XmNy, 268,
			NULL );
	XtAddCallback( RPT_SELECTED_DTK, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dtks_to_file_cb,
		(XtPointer) PRINT_DTKS_SELECTED_TO_FILE );

	UxPutContext( RPT_SELECTED_DTK, (char *) UxDTKManagerContext );


	/* Creation of RPT_CURRENT_DTKS */
	RPT_CURRENT_DTKS = XtVaCreateManagedWidget( "RPT_CURRENT_DTKS",
			xmPushButtonWidgetClass,
			PANE_SAVE_DTK_RPT,
			RES_CONVERT( XmNlabelString, "CURRENT DTKS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNx, 2,
			XmNy, 268,
			NULL );
	XtAddCallback( RPT_CURRENT_DTKS, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dtks_to_file_cb,
		(XtPointer) PRINT_DTKS_CURRENT_TO_FILE );

	UxPutContext( RPT_CURRENT_DTKS, (char *) UxDTKManagerContext );


	/* Creation of RPT_ALL_DTKS */
	RPT_ALL_DTKS = XtVaCreateManagedWidget( "RPT_ALL_DTKS",
			xmPushButtonWidgetClass,
			PANE_SAVE_DTK_RPT,
			RES_CONVERT( XmNlabelString, "ALL DTKS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNx, 2,
			XmNy, 268,
			NULL );
	XtAddCallback( RPT_ALL_DTKS, XmNactivateCallback,
		(XtCallbackProc) cb_set_print_dtks_to_file_cb,
		(XtPointer) PRINT_DTKS_ALL_TO_FILE );

	UxPutContext( RPT_ALL_DTKS, (char *) UxDTKManagerContext );


	/* Creation of cascadeButton_DTK_SAVE */
	cascadeButton_DTK_SAVE = XtVaCreateManagedWidget( "cascadeButton_DTK_SAVE",
			xmCascadeButtonWidgetClass,
			menuBar_DTK_FILE_pane,
			RES_CONVERT( XmNlabelString, "SAVE DTK REPORT" ),
			XmNsubMenuId, PANE_SAVE_DTK_RPT,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 268,
			NULL );
	UxPutContext( cascadeButton_DTK_SAVE, (char *) UxDTKManagerContext );


	/* Creation of PANE_PRINT_DTK_RPT */
	PANE_PRINT_DTK_RPT_shell = XtVaCreatePopupShell ("PANE_PRINT_DTK_RPT_shell",
			xmMenuShellWidgetClass, menuBar_DTK_FILE_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	PANE_PRINT_DTK_RPT = XtVaCreateWidget( "PANE_PRINT_DTK_RPT",
			xmRowColumnWidgetClass,
			PANE_PRINT_DTK_RPT_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 246,
			NULL );
	UxPutContext( PANE_PRINT_DTK_RPT, (char *) UxDTKManagerContext );


	/* Creation of PRINT_SELECTED_DTK */
	PRINT_SELECTED_DTK = XtVaCreateManagedWidget( "PRINT_SELECTED_DTK",
			xmPushButtonWidgetClass,
			PANE_PRINT_DTK_RPT,
			RES_CONVERT( XmNlabelString, "SELECTED DTK" ),
			XmNx, 2,
			XmNy, 269,
			NULL );
	XtAddCallback( PRINT_SELECTED_DTK, XmNactivateCallback,
		(XtCallbackProc) cb_print_dtks,
		(XtPointer) PRINT_DTKS_SELECTED );

	UxPutContext( PRINT_SELECTED_DTK, (char *) UxDTKManagerContext );


	/* Creation of PRINT_CURRENT_DTKS */
	PRINT_CURRENT_DTKS = XtVaCreateManagedWidget( "PRINT_CURRENT_DTKS",
			xmPushButtonWidgetClass,
			PANE_PRINT_DTK_RPT,
			RES_CONVERT( XmNlabelString, "CURRENT DTKS" ),
			XmNx, 2,
			XmNy, 269,
			NULL );
	XtAddCallback( PRINT_CURRENT_DTKS, XmNactivateCallback,
		(XtCallbackProc) cb_print_dtks,
		(XtPointer) PRINT_DTKS_CURRENT );

	UxPutContext( PRINT_CURRENT_DTKS, (char *) UxDTKManagerContext );


	/* Creation of PRINT_ALL_DTKS */
	PRINT_ALL_DTKS = XtVaCreateManagedWidget( "PRINT_ALL_DTKS",
			xmPushButtonWidgetClass,
			PANE_PRINT_DTK_RPT,
			RES_CONVERT( XmNlabelString, "ALL DTKS" ),
			XmNx, 2,
			XmNy, 269,
			NULL );
	XtAddCallback( PRINT_ALL_DTKS, XmNactivateCallback,
		(XtCallbackProc) cb_print_dtks,
		(XtPointer) PRINT_DTKS_ALL );

	UxPutContext( PRINT_ALL_DTKS, (char *) UxDTKManagerContext );


	/* Creation of cascadeButton_DTK_PRINT */
	cascadeButton_DTK_PRINT = XtVaCreateManagedWidget( "cascadeButton_DTK_PRINT",
			xmCascadeButtonWidgetClass,
			menuBar_DTK_FILE_pane,
			RES_CONVERT( XmNlabelString, "PRINT DTK REPORT" ),
			XmNsubMenuId, PANE_PRINT_DTK_RPT,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 268,
			NULL );
	UxPutContext( cascadeButton_DTK_PRINT, (char *) UxDTKManagerContext );


	/* Creation of menuBar_cascade_button_DTK_FILE */
	menuBar_cascade_button_DTK_FILE = XtVaCreateManagedWidget( "menuBar_cascade_button_DTK_FILE",
			xmCascadeButtonWidgetClass,
			menuBar_DTK_FILE,
			RES_CONVERT( XmNlabelString, "FILE" ),
			XmNsubMenuId, menuBar_DTK_FILE_pane,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 266,
			XmNalignment, XmALIGNMENT_CENTER,
			XmNrecomputeSize, FALSE,
			XmNwidth, 76,
			XmNmarginWidth, 0,
			XmNmarginHeight, 0,
			XmNheight, 32,
			NULL );
	UxPutContext( menuBar_cascade_button_DTK_FILE, (char *) UxDTKManagerContext );


	/* Creation of pushButton_DTK_refresh */
	pushButton_DTK_refresh = XtVaCreateManagedWidget( "pushButton_DTK_refresh",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 26,
			XmNy, 79,
			XmNwidth, 30,
			XmNheight, 130,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( pushButton_DTK_refresh, XmNactivateCallback,
		(XtCallbackProc) cb_show_dtk_records,
		(XtPointer) scrolledList_DTKS );

	UxPutContext( pushButton_DTK_refresh, (char *) UxDTKManagerContext );


	/* Creation of TF_STOPLAT */
	TF_STOPLAT = XtVaCreateManagedWidget( "TF_STOPLAT",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 344,
			XmNy, 648,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_STOPLAT, (char *) UxDTKManagerContext );


	/* Creation of TF_STRTLAT */
	TF_STRTLAT = XtVaCreateManagedWidget( "TF_STRTLAT",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 344,
			XmNy, 613,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 8,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 8,
			NULL );
	UxPutContext( TF_STRTLAT, (char *) UxDTKManagerContext );


	/* Creation of label146 */
	label146 = XtVaCreateManagedWidget( "label146",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 279,
			XmNy, 613,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START LAT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label146, (char *) UxDTKManagerContext );


	/* Creation of label112 */
	label112 = XtVaCreateManagedWidget( "label112",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 279,
			XmNy, 648,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, " STOP LAT:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label112, (char *) UxDTKManagerContext );


	/* Creation of TF_NOTES */
	TF_NOTES = XtVaCreateManagedWidget( "TF_NOTES",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 362,
			XmNy, 561,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 40,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 40,
			NULL );
	UxPutContext( TF_NOTES, (char *) UxDTKManagerContext );


	/* Creation of label113 */
	label113 = XtVaCreateManagedWidget( "label113",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 317,
			XmNy, 562,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NOTES:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 40,
			NULL );
	UxPutContext( label113, (char *) UxDTKManagerContext );


	/* Creation of subMenu_sat */
	subMenu_sat_shell = XtVaCreatePopupShell ("subMenu_sat_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_sat = XtVaCreateWidget( "subMenu_sat",
			xmRowColumnWidgetClass,
			subMenu_sat_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 0,
			XmNy, 326,
			XmNmappedWhenManaged, TRUE,
			XmNmenuPost, "",
			NULL );
	UxPutContext( subMenu_sat, (char *) UxDTKManagerContext );


	/* Creation of subMenu_sat_ERS */
	subMenu_sat_ERS = XtVaCreateManagedWidget( "subMenu_sat_ERS",
			xmPushButtonWidgetClass,
			subMenu_sat,
			RES_CONVERT( XmNlabelString, "RADARSAT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 328,
			NULL );
	UxPutContext( subMenu_sat_ERS, (char *) UxDTKManagerContext );

	cb_build_satellite_option_menu( subMenu_sat_ERS,
			(XtPointer) UxDTKManagerContext, (XtPointer) NULL );


	/* Creation of optionMenu_sat */
	optionMenu_sat = XtVaCreateManagedWidget( "optionMenu_sat",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_sat,
			XmNx, 46,
			XmNy, 321,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "SATELLITE\n/SENSOR:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_sat, (char *) UxDTKManagerContext );


	/* Creation of subMenu_sensor */
	subMenu_sensor_shell = XtVaCreatePopupShell ("subMenu_sensor_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_sensor = XtVaCreateWidget( "subMenu_sensor",
			xmRowColumnWidgetClass,
			subMenu_sensor_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 0,
			XmNy, 326,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_sensor, (char *) UxDTKManagerContext );


	/* Creation of subMenu_sensor_SAR */
	subMenu_sensor_SAR = XtVaCreateManagedWidget( "subMenu_sensor_SAR",
			xmPushButtonWidgetClass,
			subMenu_sensor,
			RES_CONVERT( XmNlabelString, "SAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 328,
			NULL );
	UxPutContext( subMenu_sensor_SAR, (char *) UxDTKManagerContext );

	cb_build_sensor_option_menu( subMenu_sensor_SAR,
			(XtPointer) UxDTKManagerContext, (XtPointer) NULL );


	/* Creation of optionMenu_sensor */
	optionMenu_sensor = XtVaCreateManagedWidget( "optionMenu_sensor",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_sensor,
			XmNx, 216,
			XmNy, 321,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "/" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_sensor, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_direction */
	subMenu_dtk_direction_shell = XtVaCreatePopupShell ("subMenu_dtk_direction_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_dtk_direction = XtVaCreateWidget( "subMenu_dtk_direction",
			xmRowColumnWidgetClass,
			subMenu_dtk_direction_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, FALSE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_dtk_direction, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_direction_ascend */
	subMenu_dtk_direction_ascend = XtVaCreateManagedWidget( "subMenu_dtk_direction_ascend",
			xmPushButtonWidgetClass,
			subMenu_dtk_direction,
			RES_CONVERT( XmNlabelString, "ASC" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( subMenu_dtk_direction_ascend, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_direction_descend */
	subMenu_dtk_direction_descend = XtVaCreateManagedWidget( "subMenu_dtk_direction_descend",
			xmPushButtonWidgetClass,
			subMenu_dtk_direction,
			RES_CONVERT( XmNlabelString, "DSC" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( subMenu_dtk_direction_descend, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtk_direction_cvrgNotAllowed */
	subMenu_dtk_direction_cvrgNotAllowed = XtVaCreateManagedWidget( "subMenu_dtk_direction_cvrgNotAllowed",
			xmPushButtonWidgetClass,
			subMenu_dtk_direction,
			RES_CONVERT( XmNlabelString, "N/A" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	UxPutContext( subMenu_dtk_direction_cvrgNotAllowed, (char *) UxDTKManagerContext );


	/* Creation of optionMenu_dtk_direction */
	optionMenu_dtk_direction = XtVaCreateManagedWidget( "optionMenu_dtk_direction",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_dtk_direction,
			XmNx, 129,
			XmNy, 613,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "DIRECTION:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_dtk_direction, (char *) UxDTKManagerContext );


	/* Creation of pushButton_CreateDTK */
	pushButton_CreateDTK = XtVaCreateManagedWidget( "pushButton_CreateDTK",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 262,
			XmNy, 697,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_CreateDTK, XmNactivateCallback,
		(XtCallbackProc) cb_create_dtk,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( pushButton_CreateDTK, (char *) UxDTKManagerContext );


	/* Creation of TF_DTKDATE */
	TF_DTKDATE = XtVaCreateManagedWidget( "TF_DTKDATE",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 107,
			XmNy, 561,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNtraversalOn, FALSE,
			XmNwidth, 185,
			XmNleftOffset, 107,
			NULL );
	UxPutContext( TF_DTKDATE, (char *) UxDTKManagerContext );


	/* Creation of label116 */
	label116 = XtVaCreateManagedWidget( "label116",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 46,
			XmNy, 562,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "  LAST  \nMODIFIED:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 60,
			NULL );
	UxPutContext( label116, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_STRTTIME */
	TF_DTK_STRTTIME = XtVaCreateManagedWidget( "TF_DTK_STRTTIME",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 107,
			XmNy, 414,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNwidth, 185,
			NULL );
	XtAddCallback( TF_DTK_STRTTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "DTK Start Time" );
	XtAddCallback( TF_DTK_STRTTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DTK_STRTTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );
	XtAddCallback( TF_DTK_STRTTIME, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );

	UxPutContext( TF_DTK_STRTTIME, (char *) UxDTKManagerContext );


	/* Creation of TF_DTK_STOPTIME */
	TF_DTK_STOPTIME = XtVaCreateManagedWidget( "TF_DTK_STOPTIME",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNx, 107,
			XmNy, 458,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNwidth, 185,
			NULL );
	XtAddCallback( TF_DTK_STOPTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "DTK Stop Time" );
	XtAddCallback( TF_DTK_STOPTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_DTK_STOPTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );
	XtAddCallback( TF_DTK_STOPTIME, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );

	UxPutContext( TF_DTK_STOPTIME, (char *) UxDTKManagerContext );


	/* Creation of scrolledWindow_FATapes */
	scrolledWindow_FATapes = XtVaCreateManagedWidget( "scrolledWindow_FATapes",
			xmScrolledWindowWidgetClass,
			DTKManager,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 100,
			XmNheight, 145,
			XmNx, 467,
			XmNy, 367,
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( scrolledWindow_FATapes, (char *) UxDTKManagerContext );


	/* Creation of rowColumn2 */
	rowColumn2 = XtVaCreateManagedWidget( "rowColumn2",
			xmRowColumnWidgetClass,
			scrolledWindow_FATapes,
			XmNwidth, 170,
			XmNheight, 65,
			XmNx, 18,
			XmNy, -12,
			XmNorientation, XmVERTICAL,
			NULL );
	UxPutContext( rowColumn2, (char *) UxDTKManagerContext );


	/* Creation of FATape_ASF */
	FATape_ASF = XtVaCreateManagedWidget( "FATape_ASF",
			xmToggleButtonWidgetClass,
			rowColumn2,
			XmNx, 30,
			XmNy, 10,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "ASF" ),
			NULL );
	UxPutContext( FATape_ASF, (char *) UxDTKManagerContext );


	/* Creation of FATape_ESA */
	FATape_ESA = XtVaCreateManagedWidget( "FATape_ESA",
			xmToggleButtonWidgetClass,
			rowColumn2,
			XmNx, 35,
			XmNy, 10,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "ESA" ),
			NULL );
	UxPutContext( FATape_ESA, (char *) UxDTKManagerContext );


	/* Creation of FATape_NASDA */
	FATape_NASDA = XtVaCreateManagedWidget( "FATape_NASDA",
			xmToggleButtonWidgetClass,
			rowColumn2,
			XmNx, 45,
			XmNy, 40,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "NASDA" ),
			NULL );
	UxPutContext( FATape_NASDA, (char *) UxDTKManagerContext );


	/* Creation of FATape_CSA */
	FATape_CSA = XtVaCreateManagedWidget( "FATape_CSA",
			xmToggleButtonWidgetClass,
			rowColumn2,
			XmNx, 13,
			XmNy, 47,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "CSA" ),
			NULL );
	UxPutContext( FATape_CSA, (char *) UxDTKManagerContext );


	/* Creation of label25 */
	label25 = XtVaCreateManagedWidget( "label25",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 299,
			XmNy, 365,
			RES_CONVERT( XmNlabelString, "ACTIVITY/\nFA TAPES:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label25, (char *) UxDTKManagerContext );


	/* Creation of frameActivity */
	frameActivity = XtVaCreateManagedWidget( "frameActivity",
			xmFrameWidgetClass,
			DTKManager,
			XmNwidth, 100,
			XmNheight, 145,
			XmNx, 364,
			XmNy, 367,
			XmNshadowType, XmSHADOW_IN,
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( frameActivity, (char *) UxDTKManagerContext );


	/* Creation of rowColumn1 */
	rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
			xmRowColumnWidgetClass,
			frameActivity,
			XmNwidth, 170,
			XmNheight, 130,
			XmNx, 400,
			XmNy, 2,
			XmNorientation, XmVERTICAL,
			XmNradioBehavior, TRUE,
			XmNspacing, 0,
			XmNpacking, XmPACK_COLUMN,
			XmNresizeHeight, TRUE,
			NULL );
	UxPutContext( rowColumn1, (char *) UxDTKManagerContext );


	/* Creation of Activity_Downlink */
	Activity_Downlink = XtVaCreateManagedWidget( "Activity_Downlink",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 400,
			XmNy, 3,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "RT DOWNLK" ),
			XmNset, TRUE,
			NULL );
	UxPutContext( Activity_Downlink, (char *) UxDTKManagerContext );


	/* Creation of Activity_Observe */
	Activity_Observe = XtVaCreateManagedWidget( "Activity_Observe",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 400,
			XmNy, 34,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "RT OBSERV" ),
			NULL );
	XtAddCallback( Activity_Observe, XmNvalueChangedCallback,
		(XtCallbackProc) cb_rtobservation_toggle,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( Activity_Observe, (char *) UxDTKManagerContext );


	/* Creation of Activity_Dump */
	Activity_Dump = XtVaCreateManagedWidget( "Activity_Dump",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 400,
			XmNy, 65,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "DUMP" ),
			NULL );
	UxPutContext( Activity_Dump, (char *) UxDTKManagerContext );


	/* Creation of Activity_Record */
	Activity_Record = XtVaCreateManagedWidget( "Activity_Record",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 400,
			XmNy, 95,
			XmNindicatorSize, 20,
			RES_CONVERT( XmNlabelString, "TAPE REC" ),
			NULL );
	UxPutContext( Activity_Record, (char *) UxDTKManagerContext );


	/* Creation of subMenu_J1_DLinkChannel */
	subMenu_J1_DLinkChannel_shell = XtVaCreatePopupShell ("subMenu_J1_DLinkChannel_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_J1_DLinkChannel = XtVaCreateWidget( "subMenu_J1_DLinkChannel",
			xmRowColumnWidgetClass,
			subMenu_J1_DLinkChannel_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 661,
			XmNy, 0,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_J1_DLinkChannel, (char *) UxDTKManagerContext );


	/* Creation of subMenu_J1_DLink_00 */
	subMenu_J1_DLink_00 = XtVaCreateManagedWidget( "subMenu_J1_DLink_00",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "00" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 2,
			NULL );
	UxPutContext( subMenu_J1_DLink_00, (char *) UxDTKManagerContext );


	/* Creation of subMenu_J1_DLink_F1 */
	subMenu_J1_DLink_F1 = XtVaCreateManagedWidget( "subMenu_J1_DLink_F1",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F1" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 24,
			NULL );
	UxPutContext( subMenu_J1_DLink_F1, (char *) UxDTKManagerContext );


	/* Creation of subMenu_J1_DLink_F2 */
	subMenu_J1_DLink_F2 = XtVaCreateManagedWidget( "subMenu_J1_DLink_F2",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F2" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 46,
			NULL );
	UxPutContext( subMenu_J1_DLink_F2, (char *) UxDTKManagerContext );


	/* Creation of subMenu_J1_DLink_CB */
	subMenu_J1_DLink_CB = XtVaCreateManagedWidget( "subMenu_J1_DLink_CB",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "CB" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 68,
			NULL );
	UxPutContext( subMenu_J1_DLink_CB, (char *) UxDTKManagerContext );


	/* Creation of subMenu_R1_DLink_F3 */
	subMenu_R1_DLink_F3 = XtVaCreateManagedWidget( "subMenu_R1_DLink_F3",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F3" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 90,
			NULL );
	UxPutContext( subMenu_R1_DLink_F3, (char *) UxDTKManagerContext );


	/* Creation of subMenu_R1_DLink_F4 */
	subMenu_R1_DLink_F4 = XtVaCreateManagedWidget( "subMenu_R1_DLink_F4",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F4" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 112,
			NULL );
	UxPutContext( subMenu_R1_DLink_F4, (char *) UxDTKManagerContext );


	/* Creation of subMenu_A1_DLink_F5 */
	subMenu_A1_DLink_F5 = XtVaCreateManagedWidget( "subMenu_A1_DLink_F5",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F5" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 134,
			NULL );
	UxPutContext( subMenu_A1_DLink_F5, (char *) UxDTKManagerContext );


	/* Creation of subMenu_A1_DLink_F6 */
	subMenu_A1_DLink_F6 = XtVaCreateManagedWidget( "subMenu_A1_DLink_F6",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F6" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 156,
			NULL );
	UxPutContext( subMenu_A1_DLink_F6, (char *) UxDTKManagerContext );


	/* Creation of subMenu_A1_DLink_F7 */
	subMenu_A1_DLink_F7 = XtVaCreateManagedWidget( "subMenu_A1_DLink_F7",
			xmPushButtonWidgetClass,
			subMenu_J1_DLinkChannel,
			RES_CONVERT( XmNlabelString, "F7" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 178,
			NULL );
	UxPutContext( subMenu_A1_DLink_F7, (char *) UxDTKManagerContext );


	/* Creation of optionMenu_J1_DLinkChannel */
	optionMenu_J1_DLinkChannel = XtVaCreateManagedWidget( "optionMenu_J1_DLinkChannel",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_J1_DLinkChannel,
			XmNx, 591,
			XmNy, 363,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "DOWNLINK\nCHANNEL:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_J1_DLinkChannel, (char *) UxDTKManagerContext );


	/* Creation of pushButton_GetCvrgPoints */
	pushButton_GetCvrgPoints = XtVaCreateManagedWidget( "pushButton_GetCvrgPoints",
			xmPushButtonWidgetClass,
			DTKManager,
			XmNx, 63,
			XmNy, 608,
			XmNwidth, 65,
			XmNheight, 70,
			RES_CONVERT( XmNlabelString, "GET\nCVRG\nPOINTS" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( pushButton_GetCvrgPoints, XmNactivateCallback,
		(XtCallbackProc) cb_get_cvrg_points,
		(XtPointer) UxDTKManagerContext );

	UxPutContext( pushButton_GetCvrgPoints, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtkm_stnid */
	subMenu_dtkm_stnid_shell = XtVaCreatePopupShell ("subMenu_dtkm_stnid_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_dtkm_stnid = XtVaCreateWidget( "subMenu_dtkm_stnid",
			xmRowColumnWidgetClass,
			subMenu_dtkm_stnid_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 48,
			XmNresizeHeight, FALSE,
			XmNx, 0,
			XmNy, 315,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_dtkm_stnid, (char *) UxDTKManagerContext );


	/* Creation of subMenu_dtkm_stnid_asf */
	subMenu_dtkm_stnid_asf = XtVaCreateManagedWidget( "subMenu_dtkm_stnid_asf",
			xmPushButtonWidgetClass,
			subMenu_dtkm_stnid,
			RES_CONVERT( XmNlabelString, "ASF" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 328,
			NULL );
	UxPutContext( subMenu_dtkm_stnid_asf, (char *) UxDTKManagerContext );

	cb_build_station_option_menu( subMenu_dtkm_stnid_asf,
			(XtPointer) UxDTKManagerContext, (XtPointer) NULL );


	/* Creation of optionMenu_dtkm_station_id */
	optionMenu_dtkm_station_id = XtVaCreateManagedWidget( "optionMenu_dtkm_station_id",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_dtkm_stnid,
			XmNx, 583,
			XmNy, 318,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "STATION ID\n/ANTENNA:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_dtkm_station_id, (char *) UxDTKManagerContext );


	/* Creation of label34 */
	label34 = XtVaCreateManagedWidget( "label34",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 728,
			XmNy, 363,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SCIENCE QUICKLOOK:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 114,
			NULL );
	UxPutContext( label34, (char *) UxDTKManagerContext );


	/* Creation of label_SciQuicklook */
	label_SciQuicklook = XtVaCreateManagedWidget( "label_SciQuicklook",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 843,
			XmNy, 368,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "No" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 25,
			NULL );
	UxPutContext( label_SciQuicklook, (char *) UxDTKManagerContext );


	/* Creation of subMenu_antenna */
	subMenu_antenna_shell = XtVaCreatePopupShell ("subMenu_antenna_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_antenna = XtVaCreateWidget( "subMenu_antenna",
			xmRowColumnWidgetClass,
			subMenu_antenna_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 0,
			XmNy, 326,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_antenna, (char *) UxDTKManagerContext );


	/* Creation of subMenu_antenna_1 */
	subMenu_antenna_1 = XtVaCreateManagedWidget( "subMenu_antenna_1",
			xmPushButtonWidgetClass,
			subMenu_antenna,
			RES_CONVERT( XmNlabelString, "1" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 328,
			NULL );
	UxPutContext( subMenu_antenna_1, (char *) UxDTKManagerContext );

	cb_build_antenna_option_menu( subMenu_antenna_1,
			(XtPointer) True, (XtPointer) NULL );


	/* Creation of optionMenu_antenna */
	optionMenu_antenna = XtVaCreateManagedWidget( "optionMenu_antenna",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_antenna,
			XmNx, 705,
			XmNy, 318,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "/" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_antenna, (char *) UxDTKManagerContext );


	/* Creation of label_FA_Schedule_Link */
	label_FA_Schedule_Link = XtVaCreateManagedWidget( "label_FA_Schedule_Link",
			xmLabelWidgetClass,
			DTKManager,
			XmNx, 574,
			XmNy, 481,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "FA SCHEDULE LINK:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_FA_Schedule_Link, (char *) UxDTKManagerContext );


	/* Creation of TF_FA_SCHEDULE_LINK */
	TF_FA_SCHEDULE_LINK = XtVaCreateManagedWidget( "TF_FA_SCHEDULE_LINK",
			xmTextFieldWidgetClass,
			DTKManager,
			XmNwidth, 186,
			XmNx, 683,
			XmNy, 480,
			XmNheight, 32,
			XmNmaxLength, 20,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( TF_FA_SCHEDULE_LINK, (char *) UxDTKManagerContext );


	/* Creation of separator8 */
	separator8 = XtVaCreateManagedWidget( "separator8",
			xmSeparatorWidgetClass,
			DTKManager,
			XmNwidth, 886,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 594,
			NULL );
	UxPutContext( separator8, (char *) UxDTKManagerContext );


	/* Creation of label63 */
	label63 = XtVaCreateManagedWidget( "label63",
			xmLabelWidgetClass,
			DTKManager,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, "DTK ID:" ),
			XmNx, 472,
			XmNy, 326,
			XmNtopOffset, 324,
			NULL );
	UxPutContext( label63, (char *) UxDTKManagerContext );


	/* Creation of subMenu_PlanQuicklook */
	subMenu_PlanQuicklook_shell = XtVaCreatePopupShell ("subMenu_PlanQuicklook_shell",
			xmMenuShellWidgetClass, DTKManager,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_PlanQuicklook = XtVaCreateWidget( "subMenu_PlanQuicklook",
			xmRowColumnWidgetClass,
			subMenu_PlanQuicklook_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNx, 661,
			XmNy, 0,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_PlanQuicklook, (char *) UxDTKManagerContext );


	/* Creation of subMenu_PlanQuicklook_no */
	subMenu_PlanQuicklook_no = XtVaCreateManagedWidget( "subMenu_PlanQuicklook_no",
			xmPushButtonWidgetClass,
			subMenu_PlanQuicklook,
			RES_CONVERT( XmNlabelString, "NO" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 24,
			NULL );
	UxPutContext( subMenu_PlanQuicklook_no, (char *) UxDTKManagerContext );


	/* Creation of subMenu_PlanQuicklook_yes */
	subMenu_PlanQuicklook_yes = XtVaCreateManagedWidget( "subMenu_PlanQuicklook_yes",
			xmPushButtonWidgetClass,
			subMenu_PlanQuicklook,
			RES_CONVERT( XmNlabelString, "YES" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 665,
			XmNy, 2,
			NULL );
	UxPutContext( subMenu_PlanQuicklook_yes, (char *) UxDTKManagerContext );


	/* Creation of optionMenu_PlanQuicklook */
	optionMenu_PlanQuicklook = XtVaCreateManagedWidget( "optionMenu_PlanQuicklook",
			xmRowColumnWidgetClass,
			DTKManager,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_PlanQuicklook,
			XmNx, 701,
			XmNy, 398,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "PLANNER QUICKLOOK:" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_PlanQuicklook, (char *) UxDTKManagerContext );


	XtAddCallback( DTKManager, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxDTKManagerContext);


	return ( DTKManager );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_DTKManager( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCDTKManager          *UxContext;
	static int		_Uxinit = 0;

	UxDTKManagerContext = UxContext =
		(_UxCDTKManager *) UxNewContext( sizeof(_UxCDTKManager), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		SORT_INFO *sortinfo ;
		SEARCH_INFO *searchinfo ;
		OPTION_MENU_WIDGETS *sensor_menu ;
		ANTENNA_CLIENT_DATA *antenna_menu ;
		rtrn = _Uxbuild_DTKManager();

		sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;
		searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;
		sensor_menu = (OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;
		antenna_menu = (ANTENNA_CLIENT_DATA *) malloc(sizeof(ANTENNA_CLIENT_DATA)) ;
		
		sortinfo->table_name = APS_TABLE(DTK) ;
		sortinfo->field_to_update = (Widget) TF_DTK_sortclause ;
		
		searchinfo->table_name = APS_TABLE(DTK) ;
		searchinfo->field_to_update  = (Widget) TF_DTK_searchclause ;
		sensor_menu->optionmenu = (Widget) optionMenu_sensor ;
		sensor_menu->submenu = (Widget) subMenu_sensor ;
		antenna_menu->noAntenna_flag = True ;
		antenna_menu->menuWidgets.optionmenu = (Widget) optionMenu_antenna ;
		antenna_menu->menuWidgets.submenu = (Widget) subMenu_antenna ;
		
		XtAddCallback( pushButton_SortDTK, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_sort_columns,
		    (XtPointer) sortinfo );
		 
		XtAddCallback( pushButton_SearchDTK, XmNactivateCallback,
		    (XtCallbackProc) cb_edit_search_columns,
		    (XtPointer) searchinfo );
		
		XtAddCallback( subMenu_sat, XmNentryCallback,
			(XtCallbackProc) cb_set_sensor_menus,
			(XtPointer ) sensor_menu );
		
		XtAddCallback( subMenu_sat, XmNentryCallback,
			(XtCallbackProc) cb_set_dtk_status_menus,
			(XtPointer ) NULL );
		
		XtAddCallback( subMenu_dtkm_stnid, XmNentryCallback,
			(XtCallbackProc) cb_set_antenna_menus,
			(XtPointer ) antenna_menu );
		
		XtAddCallback(XtParent(rtrn), XtNpopupCallback,
			cb_show_dtk_records, (XtPointer *) scrolledList_DTKS) ;
		
		cb_init_dtk_search_clause( TF_DTK_searchclause, NULL, NULL ) ;
		
		XtCallActionProc( XtNameToWidget(subMenu_sat, "ERS-1" ),
		        "ArmAndActivate", NULL, NULL, 0 );
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

