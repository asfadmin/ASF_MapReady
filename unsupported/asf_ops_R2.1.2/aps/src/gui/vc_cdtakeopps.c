
/*******************************************************************************
	vc_cdtakeopps.c

       Associated Header file: vc_cdtakeopps.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
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
	**************************************************************************
	* Create Specific Site Coverage USED TO BE Create Datatake Opportunities *
	* so some of the names are still relics from the old title               *
	**************************************************************************
 
==============================================================================*/
#pragma ident   "@(#)vc_cdtakeopps.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_cdtakeopps.c"

#include <stdlib.h>

#include "cb_cdtakeopps.h"
#include "cb_datetime.h"
#include "satmenus.h"

extern XtCallbackProc check ;
extern void cb_set_sensor_menu();
extern Widget cdtakeopps_form ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_cdtakeopps.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_cdtakeopps_quit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCCreateDatatakeOpps  *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxCreateDatatakeOppsContext;
	UxCreateDatatakeOppsContext = UxContext =
			(_UxCCreateDatatakeOpps *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(cdtakeopps_form)) ;
	}
	UxCreateDatatakeOppsContext = UxSaveCtx;
}

static void  activateCB_subMenu_cdtk_sat_ERS(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCCreateDatatakeOpps  *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxCreateDatatakeOppsContext;
	UxCreateDatatakeOppsContext = UxContext =
			(_UxCCreateDatatakeOpps *) UxGetContext( UxWidget );
	{
	(void) printf("ok\n") ;
	}
	UxCreateDatatakeOppsContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CreateDatatakeOpps()
{
	Widget		_UxParent;
	Widget		menu1_p1_shell;
	Widget		subMenu_cdtk_sensor_shell;
	Widget		subMenu_cdtk_sat_shell;


	/* Creation of CreateDatatakeOpps */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "CreateDatatakeOpps_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 286,
			XmNy, -14,
			XmNwidth, 856,
			XmNheight, 828,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CreateDatatakeOpps",
			XmNiconName, "CreateDatatakeOpps",
			NULL );

	}

	CreateDatatakeOpps = XtVaCreateManagedWidget( "CreateDatatakeOpps",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 856,
			XmNheight, 828,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS:Create Specific Site Coverage" ),
			NULL );
	UxPutContext( CreateDatatakeOpps, (char *) UxCreateDatatakeOppsContext );
	UxPutClassCode( CreateDatatakeOpps, _UxIfClassId );


	/* Creation of label37 */
	label37 = XtVaCreateManagedWidget( "label37",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 69,
			XmNy, 12,
			XmNwidth, 700,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE  SPECIFIC  SITE  COVERAGE" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label37, (char *) UxCreateDatatakeOppsContext );


	/* Creation of scrolledWindowList3 */
	scrolledWindowList3 = XtVaCreateManagedWidget( "scrolledWindowList3",
			xmScrolledWindowWidgetClass,
			CreateDatatakeOpps,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 75,
			XmNy, 100,
			XmNwidth, 275,
			NULL );
	UxPutContext( scrolledWindowList3, (char *) UxCreateDatatakeOppsContext );


	/* Creation of scrolledList_sites */
	scrolledList_sites = XtVaCreateManagedWidget( "scrolledList_sites",
			xmListWidgetClass,
			scrolledWindowList3,
			XmNwidth, 310,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "12345 12345678901234567890123456789012" ),
			XmNvisibleItemCount, 13,
			XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE,
			NULL );
	XtAddCallback( scrolledList_sites, XmNdefaultActionCallback,
		(XtCallbackProc) cb_update_cdtakeopps_form,
		(XtPointer) UxCreateDatatakeOppsContext );
	XtAddCallback( scrolledList_sites, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_cdtakeopps_form,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( scrolledList_sites, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_COMMENTS */
	TF_COMMENTS = XtVaCreateManagedWidget( "TF_COMMENTS",
			xmTextFieldWidgetClass,
			CreateDatatakeOpps,
			XmNx, 419,
			XmNy, 163,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 40,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 365,
			NULL );
	UxPutContext( TF_COMMENTS, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_DARID */
	TF_DARID = XtVaCreateManagedWidget( "TF_DARID",
			xmTextFieldWidgetClass,
			CreateDatatakeOpps,
			XmNx, 419,
			XmNy, 76,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 5,
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( TF_DARID, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label53 */
	label53 = XtVaCreateManagedWidget( "label53",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 390,
			XmNy, 77,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DAR\nID:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label53, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_QUICKLOOK */
	TF_QUICKLOOK = XtVaCreateManagedWidget( "TF_QUICKLOOK",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 560,
			XmNy, 82,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "No" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 25,
			NULL );
	UxPutContext( TF_QUICKLOOK, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label5 */
	label5 = XtVaCreateManagedWidget( "label5",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 494,
			XmNy, 77,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "QUICKLOOK:" ),
			XmNalignment, XmALIGNMENT_END,
			XmNwidth, 65,
			NULL );
	UxPutContext( label5, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SITENAME */
	TF_SITENAME = XtVaCreateManagedWidget( "TF_SITENAME",
			xmTextFieldWidgetClass,
			CreateDatatakeOpps,
			XmNx, 419,
			XmNy, 117,
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
	UxPutContext( TF_SITENAME, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label54 */
	label54 = XtVaCreateManagedWidget( "label54",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 384,
			XmNy, 118,
			XmNwidth, 35,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SITE \nNAME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label54, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SHAPE */
	TF_SHAPE = XtVaCreateManagedWidget( "TF_SHAPE",
			xmTextFieldWidgetClass,
			CreateDatatakeOpps,
			XmNx, 419,
			XmNy, 204,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 6,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_SHAPE, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label55 */
	label55 = XtVaCreateManagedWidget( "label55",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 378,
			XmNy, 204,
			XmNwidth, 40,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SHAPE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label55, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label56 */
	label56 = XtVaCreateManagedWidget( "label56",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 359,
			XmNy, 164,
			XmNwidth, 60,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "COMMENTS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label56, (char *) UxCreateDatatakeOppsContext );


	/* Creation of rowColumn3 */
	rowColumn3 = XtVaCreateManagedWidget( "rowColumn3",
			xmRowColumnWidgetClass,
			CreateDatatakeOpps,
			XmNwidth, 137,
			XmNheight, 20,
			XmNx, 134,
			XmNy, 320,
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			RES_CONVERT( XmNlabelString, "" ),
			XmNnumColumns, 1,
			XmNpacking, XmPACK_TIGHT,
			XmNwhichButton, 1,
			NULL );
	UxPutContext( rowColumn3, (char *) UxCreateDatatakeOppsContext );


	/* Creation of toggleButton_DARSites */
	toggleButton_DARSites = XtVaCreateManagedWidget( "toggleButton_DARSites",
			xmToggleButtonWidgetClass,
			rowColumn3,
			XmNx, 3,
			XmNy, 282,
			XmNwidth, 67,
			XmNheight, 12,
			RES_CONVERT( XmNlabelString, "DAR" ),
			XmNset, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	XtAddCallback( toggleButton_DARSites, XmNvalueChangedCallback,
		(XtCallbackProc) cb_display_dar_sites,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( toggleButton_DARSites, (char *) UxCreateDatatakeOppsContext );


	/* Creation of toggleButton_HypoSites */
	toggleButton_HypoSites = XtVaCreateManagedWidget( "toggleButton_HypoSites",
			xmToggleButtonWidgetClass,
			rowColumn3,
			XmNx, 56,
			XmNy, 282,
			XmNwidth, 61,
			XmNheight, 17,
			RES_CONVERT( XmNlabelString, "Hypothetical" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	XtAddCallback( toggleButton_HypoSites, XmNvalueChangedCallback,
		(XtCallbackProc) cb_display_hypo_sites,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( toggleButton_HypoSites, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label60 */
	label60 = XtVaCreateManagedWidget( "label60",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 80,
			XmNy, 327,
			XmNwidth, 49,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DISPLAY\nSITES:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label60, (char *) UxCreateDatatakeOppsContext );


	/* Creation of scrolledWindowText2 */
	scrolledWindowText2 = XtVaCreateManagedWidget( "scrolledWindowText2",
			xmScrolledWindowWidgetClass,
			CreateDatatakeOpps,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 27,
			XmNy, 645,
			XmNwidth, 800,
			XmNheight, 160,
			NULL );
	UxPutContext( scrolledWindowText2, (char *) UxCreateDatatakeOppsContext );


	/* Creation of scrolledText_create_dtkopps */
	scrolledText_create_dtkopps = XtVaCreateManagedWidget( "scrolledText_create_dtkopps",
			xmTextWidgetClass,
			scrolledWindowText2,
			XmNheight, 141,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNcursorPositionVisible, TRUE,
			NULL );
	UxPutContext( scrolledText_create_dtkopps, (char *) UxCreateDatatakeOppsContext );


	/* Creation of separator5 */
	separator5 = XtVaCreateManagedWidget( "separator5",
			xmSeparatorWidgetClass,
			CreateDatatakeOpps,
			XmNwidth, 855,
			XmNheight, 15,
			XmNx, 0,
			XmNy, 604,
			NULL );
	UxPutContext( separator5, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label52 */
	label52 = XtVaCreateManagedWidget( "label52",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 134,
			XmNy, 623,
			XmNwidth, 590,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label52, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_cdtakeopps_quit */
	pushButton_cdtakeopps_quit = XtVaCreateManagedWidget( "pushButton_cdtakeopps_quit",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNx, 630,
			XmNy, 559,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_cdtakeopps_quit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_cdtakeopps_quit,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( pushButton_cdtakeopps_quit, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_create_dtkopps */
	pushButton_create_dtkopps = XtVaCreateManagedWidget( "pushButton_create_dtkopps",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNwidth, 130,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE\nSITE  COVERAGE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			XmNx, 100,
			XmNy, 559,
			NULL );
	XtAddCallback( pushButton_create_dtkopps, XmNactivateCallback,
		(XtCallbackProc) cb_do_create_dtk_opps,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( pushButton_create_dtkopps, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label67 */
	label67 = XtVaCreateManagedWidget( "label67",
			xmLabelWidgetClass,
			CreateDatatakeOpps,
			XmNx, 75,
			XmNy, 70,
			XmNwidth, 270,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "DAR ID      SITE NAME" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label67, (char *) UxCreateDatatakeOppsContext );


	/* Creation of form_quad */
	form_quad = XtVaCreateManagedWidget( "form_quad",
			xmFormWidgetClass,
			CreateDatatakeOpps,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 370,
			XmNy, 238,
			XmNwidth, 420,
			XmNheight, 80,
			NULL );
	UxPutContext( form_quad, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_NW_LON */
	TF_NW_LON = XtVaCreateManagedWidget( "TF_NW_LON",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 140,
			XmNy, 10,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_NW_LON, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label38 */
	label38 = XtVaCreateManagedWidget( "label38",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 125,
			XmNy, 15,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label38, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_NE_LON */
	TF_NE_LON = XtVaCreateManagedWidget( "TF_NE_LON",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 345,
			XmNy, 10,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_NE_LON, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label45 */
	label45 = XtVaCreateManagedWidget( "label45",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 330,
			XmNy, 15,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label45, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label_NE */
	label_NE = XtVaCreateManagedWidget( "label_NE",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 220,
			XmNy, 10,
			XmNwidth, 36,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_NE, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SW_LON */
	TF_SW_LON = XtVaCreateManagedWidget( "TF_SW_LON",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 140,
			XmNy, 45,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_SW_LON, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label47 */
	label47 = XtVaCreateManagedWidget( "label47",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 125,
			XmNy, 50,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label47, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label48 */
	label48 = XtVaCreateManagedWidget( "label48",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 20,
			XmNy, 45,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SW:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label48, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SE_LAT */
	TF_SE_LAT = XtVaCreateManagedWidget( "TF_SE_LAT",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 255,
			XmNy, 45,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_SE_LAT, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SE_LON */
	TF_SE_LON = XtVaCreateManagedWidget( "TF_SE_LON",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 345,
			XmNy, 45,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_SE_LON, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label50 */
	label50 = XtVaCreateManagedWidget( "label50",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 330,
			XmNy, 50,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label50, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label51 */
	label51 = XtVaCreateManagedWidget( "label51",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 230,
			XmNy, 45,
			XmNwidth, 26,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label51, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label_NW */
	label_NW = XtVaCreateManagedWidget( "label_NW",
			xmLabelWidgetClass,
			form_quad,
			XmNx, 10,
			XmNy, 10,
			XmNwidth, 36,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "NW:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label_NW, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_NE_LAT */
	TF_NE_LAT = XtVaCreateManagedWidget( "TF_NE_LAT",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 255,
			XmNy, 10,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_NE_LAT, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_NW_LAT */
	TF_NW_LAT = XtVaCreateManagedWidget( "TF_NW_LAT",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 50,
			XmNy, 10,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_NW_LAT, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_SW_LAT */
	TF_SW_LAT = XtVaCreateManagedWidget( "TF_SW_LAT",
			xmTextFieldWidgetClass,
			form_quad,
			XmNx, 50,
			XmNy, 45,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_SW_LAT, (char *) UxCreateDatatakeOppsContext );


	/* Creation of form_circle */
	form_circle = XtVaCreateWidget( "form_circle",
			xmFormWidgetClass,
			CreateDatatakeOpps,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 371,
			XmNy, 238,
			XmNwidth, 423,
			XmNheight, 80,
			NULL );
	UxPutContext( form_circle, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_center_lon */
	TF_center_lon = XtVaCreateManagedWidget( "TF_center_lon",
			xmTextFieldWidgetClass,
			form_circle,
			XmNx, 143,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNwidth, 71,
			NULL );
	UxPutContext( TF_center_lon, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_center_lat */
	TF_center_lat = XtVaCreateManagedWidget( "TF_center_lat",
			xmTextFieldWidgetClass,
			form_circle,
			XmNx, 50,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( TF_center_lat, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			form_circle,
			XmNx, 125,
			XmNy, 30,
			XmNwidth, 15,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "/" ),
			NULL );
	UxPutContext( label1, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			form_circle,
			XmNx, 5,
			XmNy, 25,
			XmNwidth, 46,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "CENTER:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label2, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label3 */
	label3 = XtVaCreateManagedWidget( "label3",
			xmLabelWidgetClass,
			form_circle,
			XmNx, 222,
			XmNy, 28,
			XmNwidth, 49,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, "RADIUS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label3, (char *) UxCreateDatatakeOppsContext );


	/* Creation of textField_radius */
	textField_radius = XtVaCreateManagedWidget( "textField_radius",
			xmTextFieldWidgetClass,
			form_circle,
			XmNx, 268,
			XmNy, 25,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 7,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( textField_radius, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label4 */
	label4 = XtVaCreateManagedWidget( "label4",
			xmLabelWidgetClass,
			form_circle,
			XmNx, 345,
			XmNy, 25,
			XmNwidth, 21,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "km" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label4, (char *) UxCreateDatatakeOppsContext );


	/* Creation of menuCreateHypoSite */
	menuCreateHypoSite = XtVaCreateManagedWidget( "menuCreateHypoSite",
			xmRowColumnWidgetClass,
			CreateDatatakeOpps,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 420,
			XmNy, 324,
			XmNheight, 28,
			XmNpacking, XmPACK_TIGHT,
			XmNresizeHeight, FALSE,
			NULL );
	UxPutContext( menuCreateHypoSite, (char *) UxCreateDatatakeOppsContext );


	/* Creation of menu1_p1 */
	menu1_p1_shell = XtVaCreatePopupShell ("menu1_p1_shell",
			xmMenuShellWidgetClass, menuCreateHypoSite,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menu1_p1 = XtVaCreateWidget( "menu1_p1",
			xmRowColumnWidgetClass,
			menu1_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 260,
			NULL );
	UxPutContext( menu1_p1, (char *) UxCreateDatatakeOppsContext );


	/* Creation of menuCreateHypoSite_Circle */
	menuCreateHypoSite_Circle = XtVaCreateManagedWidget( "menuCreateHypoSite_Circle",
			xmPushButtonWidgetClass,
			menu1_p1,
			RES_CONVERT( XmNlabelString, "Circular" ),
			XmNx, 2,
			XmNy, 286,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( menuCreateHypoSite_Circle, XmNactivateCallback,
		(XtCallbackProc) cb_edit_new_HypoSite,
		"P" );

	UxPutContext( menuCreateHypoSite_Circle, (char *) UxCreateDatatakeOppsContext );


	/* Creation of menuCreateHypoSite_Quad */
	menuCreateHypoSite_Quad = XtVaCreateManagedWidget( "menuCreateHypoSite_Quad",
			xmPushButtonWidgetClass,
			menu1_p1,
			RES_CONVERT( XmNlabelString, "Quadrilateral" ),
			XmNx, 2,
			XmNy, 286,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( menuCreateHypoSite_Quad, XmNactivateCallback,
		(XtCallbackProc) cb_edit_new_HypoSite,
		"Q" );

	UxPutContext( menuCreateHypoSite_Quad, (char *) UxCreateDatatakeOppsContext );


	/* Creation of menu1_top_b1 */
	menu1_top_b1 = XtVaCreateManagedWidget( "menu1_top_b1",
			xmCascadeButtonWidgetClass,
			menuCreateHypoSite,
			RES_CONVERT( XmNlabelString, "CREATE SITE" ),
			XmNsubMenuId, menu1_p1,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 5,
			XmNy, 290,
			NULL );
	UxPutContext( menu1_top_b1, (char *) UxCreateDatatakeOppsContext );


	/* Creation of form_dtk_info */
	form_dtk_info = XtVaCreateManagedWidget( "form_dtk_info",
			xmFormWidgetClass,
			CreateDatatakeOpps,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 90,
			XmNy, 369,
			XmNwidth, 680,
			XmNheight, 190,
			NULL );
	UxPutContext( form_dtk_info, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label57 */
	label57 = XtVaCreateManagedWidget( "label57",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 0,
			XmNy, 5,
			XmNwidth, 85,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "SAT/SENSOR\n COVERAGE:" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label57, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label59 */
	label59 = XtVaCreateManagedWidget( "label59",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 420,
			XmNy, 10,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DIRECTION:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label59, (char *) UxCreateDatatakeOppsContext );


	/* Creation of subMenu_cdtk_sensor */
	subMenu_cdtk_sensor_shell = XtVaCreatePopupShell ("subMenu_cdtk_sensor_shell",
			xmMenuShellWidgetClass, form_dtk_info,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_cdtk_sensor = XtVaCreateWidget( "subMenu_cdtk_sensor",
			xmRowColumnWidgetClass,
			subMenu_cdtk_sensor_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_cdtk_sensor, (char *) UxCreateDatatakeOppsContext );


	/* Creation of subMenu_cdtk_ensor_SAR */
	subMenu_cdtk_ensor_SAR = XtVaCreateManagedWidget( "subMenu_cdtk_ensor_SAR",
			xmPushButtonWidgetClass,
			subMenu_cdtk_sensor,
			RES_CONVERT( XmNlabelString, "SAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	UxPutContext( subMenu_cdtk_ensor_SAR, (char *) UxCreateDatatakeOppsContext );

	cb_build_cvrg_allowed_sensor_option_menu( subMenu_cdtk_ensor_SAR,
			(XtPointer) UxCreateDatatakeOppsContext, (XtPointer) NULL );


	/* Creation of optionMenu_cdtk_sensor */
	optionMenu_cdtk_sensor = XtVaCreateManagedWidget( "optionMenu_cdtk_sensor",
			xmRowColumnWidgetClass,
			form_dtk_info,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_cdtk_sensor,
			XmNx, 265,
			XmNy, 5,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "SENSOR:" ),
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( optionMenu_cdtk_sensor, (char *) UxCreateDatatakeOppsContext );


	/* Creation of rc_CoverageType1 */
	rc_CoverageType1 = XtVaCreateManagedWidget( "rc_CoverageType1",
			xmRowColumnWidgetClass,
			form_dtk_info,
			XmNwidth, 137,
			XmNheight, 20,
			XmNx, 486,
			XmNy, 4,
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, FALSE,
			RES_CONVERT( XmNlabelString, "" ),
			XmNnumColumns, 1,
			XmNpacking, XmPACK_TIGHT,
			XmNwhichButton, 1,
			XmNsensitive, TRUE,
			XmNradioAlwaysOne, FALSE,
			NULL );
	UxPutContext( rc_CoverageType1, (char *) UxCreateDatatakeOppsContext );


	/* Creation of toggleButton_Ascending */
	toggleButton_Ascending = XtVaCreateManagedWidget( "toggleButton_Ascending",
			xmToggleButtonWidgetClass,
			rc_CoverageType1,
			XmNx, 0,
			XmNy, 0,
			XmNwidth, 67,
			XmNheight, 12,
			RES_CONVERT( XmNlabelString, "ASCENDING" ),
			XmNset, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	UxPutContext( toggleButton_Ascending, (char *) UxCreateDatatakeOppsContext );


	/* Creation of toggleButton_Descending */
	toggleButton_Descending = XtVaCreateManagedWidget( "toggleButton_Descending",
			xmToggleButtonWidgetClass,
			rc_CoverageType1,
			XmNx, 73,
			XmNy, 3,
			XmNwidth, 61,
			XmNheight, 12,
			RES_CONVERT( XmNlabelString, "DESCENDING" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	UxPutContext( toggleButton_Descending, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_start_rev */
	TF_start_rev = XtVaCreateManagedWidget( "TF_start_rev",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 490,
			XmNy, 55,
			XmNheight, 30,
			XmNcolumns, 6,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	XtAddCallback( TF_start_rev, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) True );
	XtAddCallback( TF_start_rev, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) False );
	XtAddCallback( TF_start_rev, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_numeric_chars );

	UxPutContext( TF_start_rev, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_total_revs */
	TF_total_revs = XtVaCreateManagedWidget( "TF_total_revs",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 490,
			XmNy, 145,
			XmNheight, 31,
			XmNcolumns, 6,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 6,
			NULL );
	XtAddCallback( TF_total_revs, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) True );
	XtAddCallback( TF_total_revs, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) False );
	XtAddCallback( TF_total_revs, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_numeric_chars );

	UxPutContext( TF_total_revs, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label65 */
	label65 = XtVaCreateManagedWidget( "label65",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 415,
			XmNy, 145,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL REVS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label65, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label68 */
	label68 = XtVaCreateManagedWidget( "label68",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 10,
			XmNy, 60,
			XmNwidth, 65,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "ANALYSIS\n PERIOD:" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label68, (char *) UxCreateDatatakeOppsContext );


	/* Creation of subMenu_cdtk_sat */
	subMenu_cdtk_sat_shell = XtVaCreatePopupShell ("subMenu_cdtk_sat_shell",
			xmMenuShellWidgetClass, form_dtk_info,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_cdtk_sat = XtVaCreateWidget( "subMenu_cdtk_sat",
			xmRowColumnWidgetClass,
			subMenu_cdtk_sat_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNsensitive, TRUE,
			XmNx, 0,
			XmNy, 335,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_cdtk_sat, (char *) UxCreateDatatakeOppsContext );


	/* Creation of subMenu_cdtk_sat_ERS */
	subMenu_cdtk_sat_ERS = XtVaCreateManagedWidget( "subMenu_cdtk_sat_ERS",
			xmPushButtonWidgetClass,
			subMenu_cdtk_sat,
			RES_CONVERT( XmNlabelString, "RADARSAT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 335,
			NULL );
	XtAddCallback( subMenu_cdtk_sat_ERS, XmNactivateCallback,
		(XtCallbackProc) activateCB_subMenu_cdtk_sat_ERS,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( subMenu_cdtk_sat_ERS, (char *) UxCreateDatatakeOppsContext );

	cb_build_cvrg_allowed_satellite_option_menu( subMenu_cdtk_sat_ERS,
			(XtPointer) UxCreateDatatakeOppsContext, (XtPointer) NULL );


	/* Creation of optionMenu_cdtk_sat */
	optionMenu_cdtk_sat = XtVaCreateManagedWidget( "optionMenu_cdtk_sat",
			xmRowColumnWidgetClass,
			form_dtk_info,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_cdtk_sat,
			XmNx, 90,
			XmNy, 5,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "SATELLITE:" ),
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( optionMenu_cdtk_sat, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label117 */
	label117 = XtVaCreateManagedWidget( "label117",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 83,
			XmNy, 146,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "TOTAL DAYS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label117, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_dtkopps_total_days */
	TF_dtkopps_total_days = XtVaCreateManagedWidget( "TF_dtkopps_total_days",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 160,
			XmNy, 145,
			XmNheight, 31,
			XmNcolumns, 8,
			XmNresizeWidth, FALSE,
			XmNvalue, "00000.00",
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 8,
			XmNwidth, 83,
			NULL );
	XtAddCallback( TF_dtkopps_total_days, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_float_chars );
	XtAddCallback( TF_dtkopps_total_days, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_dtkopps_total_days, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_dtkopps_total_days, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label119 */
	label119 = XtVaCreateManagedWidget( "label119",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 158,
			XmNy, 114,
			XmNwidth, 185,
			XmNheight, 15,
			RES_CONVERT( XmNlabelString, "yyyy:ddd:hh:mm:ss.ccc" ),
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			NULL );
	UxPutContext( label119, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_dtkopps_end */
	TF_dtkopps_end = XtVaCreateManagedWidget( "TF_dtkopps_end",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 160,
			XmNy, 85,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( TF_dtkopps_end, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );
	XtAddCallback( TF_dtkopps_end, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Site Coverage Stop Time" );
	XtAddCallback( TF_dtkopps_end, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_dtkopps_end, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_dtkopps_end, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label121 */
	label121 = XtVaCreateManagedWidget( "label121",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 88,
			XmNy, 84,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, " STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label121, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label120 */
	label120 = XtVaCreateManagedWidget( "label120",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 88,
			XmNy, 54,
			XmNwidth, 70,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label120, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_dtkopps_start */
	TF_dtkopps_start = XtVaCreateManagedWidget( "TF_dtkopps_start",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 160,
			XmNy, 55,
			XmNheight, 30,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( TF_dtkopps_start, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );
	XtAddCallback( TF_dtkopps_start, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "Site Coverage Start Time" );
	XtAddCallback( TF_dtkopps_start, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_dtkopps_start, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );

	UxPutContext( TF_dtkopps_start, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label66 */
	label66 = XtVaCreateManagedWidget( "label66",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 420,
			XmNy, 90,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, " STOP REV:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label66, (char *) UxCreateDatatakeOppsContext );


	/* Creation of label118 */
	label118 = XtVaCreateManagedWidget( "label118",
			xmLabelWidgetClass,
			form_dtk_info,
			XmNx, 420,
			XmNy, 55,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START REV:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label118, (char *) UxCreateDatatakeOppsContext );


	/* Creation of TF_stop_rev */
	TF_stop_rev = XtVaCreateManagedWidget( "TF_stop_rev",
			xmTextFieldWidgetClass,
			form_dtk_info,
			XmNx, 490,
			XmNy, 85,
			XmNheight, 30,
			XmNcolumns, 6,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	XtAddCallback( TF_stop_rev, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) True );
	XtAddCallback( TF_stop_rev, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) False );
	XtAddCallback( TF_stop_rev, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_numeric_chars );

	UxPutContext( TF_stop_rev, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_cancel_create_site */
	pushButton_cancel_create_site = XtVaCreateWidget( "pushButton_cancel_create_site",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNx, 680,
			XmNy, 322,
			XmNwidth, 106,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "CANCEL" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_cancel_create_site, XmNactivateCallback,
		(XtCallbackProc) cb_cancel_edit_new_HypoSite,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( pushButton_cancel_create_site, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_delete_site */
	pushButton_delete_site = XtVaCreateManagedWidget( "pushButton_delete_site",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNx, 550,
			XmNy, 324,
			XmNwidth, 106,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DELETE SITE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_delete_site, XmNactivateCallback,
		(XtCallbackProc) cb_delete_HypoSite,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( pushButton_delete_site, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_done_create_site */
	pushButton_done_create_site = XtVaCreateWidget( "pushButton_done_create_site",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNx, 550,
			XmNy, 324,
			XmNwidth, 106,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "DONE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_done_create_site, XmNactivateCallback,
		(XtCallbackProc) cb_add_HypoSite,
		(XtPointer) UxCreateDatatakeOppsContext );

	UxPutContext( pushButton_done_create_site, (char *) UxCreateDatatakeOppsContext );


	/* Creation of pushButton_Refresh */
	pushButton_Refresh = XtVaCreateManagedWidget( "pushButton_Refresh",
			xmPushButtonWidgetClass,
			CreateDatatakeOpps,
			XmNx, 33,
			XmNy, 133,
			XmNwidth, 30,
			XmNheight, 132,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( pushButton_Refresh, XmNactivateCallback,
		(XtCallbackProc) cb_show_dar_relations,
		(XtPointer) scrolledList_sites );

	UxPutContext( pushButton_Refresh, (char *) UxCreateDatatakeOppsContext );


	/* Creation of separator10 */
	separator10 = XtVaCreateManagedWidget( "separator10",
			xmSeparatorWidgetClass,
			CreateDatatakeOpps,
			XmNwidth, 855,
			XmNheight, 15,
			XmNx, 0,
			XmNy, 359,
			NULL );
	UxPutContext( separator10, (char *) UxCreateDatatakeOppsContext );


	XtAddCallback( CreateDatatakeOpps, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCreateDatatakeOppsContext);


	return ( CreateDatatakeOpps );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CreateDatatakeOpps( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCCreateDatatakeOpps  *UxContext;
	static int		_Uxinit = 0;

	UxCreateDatatakeOppsContext = UxContext =
		(_UxCCreateDatatakeOpps *) UxNewContext( sizeof(_UxCCreateDatatakeOpps), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		Position x, y ;
		OPTION_MENU_WIDGETS *sensor_menu ;
		PERIOD_WIDGETS *dtkopps_times ;
		PERIOD_WIDGETS *dtkopps_revs ;
		rtrn = _Uxbuild_CreateDatatakeOpps();

		sensor_menu =
			(OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;
		sensor_menu->optionmenu = (Widget) optionMenu_cdtk_sensor ;
		sensor_menu->submenu = (Widget) subMenu_cdtk_sensor ;
		
		dtkopps_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		dtkopps_times->start = (Widget) TF_dtkopps_start ;
		dtkopps_times->stop = (Widget) TF_dtkopps_end ;
		
		dtkopps_revs = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;
		dtkopps_revs->start = (Widget) TF_start_rev ;
		dtkopps_revs->stop = (Widget) TF_stop_rev ;
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
		    XtVaGetValues(form_quad,
		        XmNx, &x,
		        XmNy, &y,
		        NULL) ;
		
		     XtVaSetValues(form_circle,
		        XmNx, x,
		        XmNy, y,
		        NULL) ;
		
		XtAddCallback( subMenu_cdtk_sat, XmNentryCallback,
		    (XtCallbackProc) cb_set_cvrg_allowed_sensor_menus,
		    (XtPointer ) sensor_menu );
		
		XtAddCallback(TF_dtkopps_total_days, XmNactivateCallback,
			(XtCallbackProc) cb_adjust_ASF_datetimes,
			(XtPointer) dtkopps_times) ;
		
		
		XtAddCallback(TF_total_revs, XmNactivateCallback,
			(XtCallbackProc) cb_adjust_revs,
			(XtPointer) dtkopps_revs) ;
		
		
		XtAddCallback(XtParent(rtrn), XtNpopupCallback,
			cb_show_dar_relations, (XtPointer *) scrolledList_sites) ;
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

