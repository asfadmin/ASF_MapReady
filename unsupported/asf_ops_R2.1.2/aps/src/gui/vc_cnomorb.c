
/*******************************************************************************
	vc_cnomorb.c

       Associated Header file: vc_cnomorb.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
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
#pragma ident   "@(#)vc_cnomorb.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_cnomorb.c"

#include "cb_cnomorb.h"

extern Widget cnomorb_form ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_cnomorb.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_cnomorb_quit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCCreateNominalOrbit  *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxCreateNominalOrbitContext;
	UxCreateNominalOrbitContext = UxContext =
			(_UxCCreateNominalOrbit *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(cnomorb_form)) ;
	}
	UxCreateNominalOrbitContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CreateNominalOrbit()
{
	Widget		_UxParent;


	/* Creation of CreateNominalOrbit */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "CreateNominalOrbit_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 419,
			XmNy, 2,
			XmNwidth, 667,
			XmNheight, 694,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CreateNominalOrbit",
			XmNiconName, "CreateNominalOrbit",
			NULL );

	}

	CreateNominalOrbit = XtVaCreateManagedWidget( "CreateNominalOrbit",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 667,
			XmNheight, 694,
			XmNresizePolicy, XmRESIZE_ANY,
			XmNunitType, XmPIXELS,
			XmNnoResize, TRUE,
			RES_CONVERT( XmNdialogTitle, "APS:CREATE NOMINAL ORBIT" ),
			NULL );
	UxPutContext( CreateNominalOrbit, (char *) UxCreateNominalOrbitContext );
	UxPutClassCode( CreateNominalOrbit, _UxIfClassId );


	/* Creation of scrolledWindowList1 */
	scrolledWindowList1 = XtVaCreateManagedWidget( "scrolledWindowList1",
			xmScrolledWindowWidgetClass,
			CreateNominalOrbit,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 45,
			XmNy, 115,
			XmNwidth, 600,
			XmNresizable, TRUE,
			NULL );
	UxPutContext( scrolledWindowList1, (char *) UxCreateNominalOrbitContext );


	/* Creation of scrolledList2 */
	scrolledList2 = XtVaCreateManagedWidget( "scrolledList2",
			xmListWidgetClass,
			scrolledWindowList1,
			XmNwidth, 620,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE,
			XmNvisibleItemCount, 6,
			XmNautomaticSelection, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "E1        A" ),
			NULL );
	XtAddCallback( scrolledList2, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_update_cnomorb_form,
		(XtPointer) UxCreateNominalOrbitContext );

	UxPutContext( scrolledList2, (char *) UxCreateNominalOrbitContext );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 72,
			XmNy, 10,
			XmNwidth, 540,
			XmNheight, 50,
			RES_CONVERT( XmNlabelString, "CREATE  NOMINAL  STATE  VECTOR  AND  ORBIT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label1, (char *) UxCreateNominalOrbitContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 45,
			XmNy, 95,
			XmNwidth, 600,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "SATELLITE  PHASE      START TIME       #DAYS    -DAYS  REVS-    LONGITUDE" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label2, (char *) UxCreateNominalOrbitContext );


	/* Creation of separator1 */
	separator1 = XtVaCreateManagedWidget( "separator1",
			xmSeparatorWidgetClass,
			CreateNominalOrbit,
			XmNwidth, 667,
			XmNheight, 10,
			XmNx, 0,
			XmNy, 215,
			NULL );
	UxPutContext( separator1, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_sat */
	textField_sat = XtVaCreateManagedWidget( "textField_sat",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 43,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNvalue, "",
			XmNcolumns, 9,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 91,
			NULL );
	UxPutContext( textField_sat, (char *) UxCreateNominalOrbitContext );


	/* Creation of label5 */
	label5 = XtVaCreateManagedWidget( "label5",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 70,
			XmNy, 410,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "LAST REV:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label5, (char *) UxCreateNominalOrbitContext );


	/* Creation of label7 */
	label7 = XtVaCreateManagedWidget( "label7",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 60,
			XmNy, 290,
			XmNwidth, 90,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "PHASE LENGTH:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label7, (char *) UxCreateNominalOrbitContext );


	/* Creation of label8 */
	label8 = XtVaCreateManagedWidget( "label8",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 54,
			XmNy, 380,
			XmNwidth, 96,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "#COMPLETE REVS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label8, (char *) UxCreateNominalOrbitContext );


	/* Creation of label9 */
	label9 = XtVaCreateManagedWidget( "label9",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 70,
			XmNy, 320,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "REPEAT CYCLE:" ),
			NULL );
	UxPutContext( label9, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_name */
	textField_phase_name = XtVaCreateManagedWidget( "textField_phase_name",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 135,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 2,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_phase_name, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_cycle_days */
	textField_cycle_days = XtVaCreateManagedWidget( "textField_cycle_days",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 421,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNresizeWidth, FALSE,
			XmNmaxLength, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 59,
			NULL );
	UxPutContext( textField_cycle_days, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_cycle_revs */
	textField_cycle_revs = XtVaCreateManagedWidget( "textField_cycle_revs",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 482,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 59,
			NULL );
	UxPutContext( textField_cycle_revs, (char *) UxCreateNominalOrbitContext );


	/* Creation of label10 */
	label10 = XtVaCreateManagedWidget( "label10",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 210,
			XmNy, 320,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "days" ),
			NULL );
	UxPutContext( label10, (char *) UxCreateNominalOrbitContext );


	/* Creation of label11 */
	label11 = XtVaCreateManagedWidget( "label11",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 210,
			XmNy, 350,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "revs" ),
			NULL );
	UxPutContext( label11, (char *) UxCreateNominalOrbitContext );


	/* Creation of label12 */
	label12 = XtVaCreateManagedWidget( "label12",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 350,
			XmNy, 290,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SEMI-MAJOR AXIS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label12, (char *) UxCreateNominalOrbitContext );


	/* Creation of label13 */
	label13 = XtVaCreateManagedWidget( "label13",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 360,
			XmNy, 320,
			XmNwidth, 90,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "ECCENTRICITY:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label13, (char *) UxCreateNominalOrbitContext );


	/* Creation of label14 */
	label14 = XtVaCreateManagedWidget( "label14",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 360,
			XmNy, 350,
			XmNwidth, 90,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_END,
			RES_CONVERT( XmNlabelString, "INCLINATION:" ),
			NULL );
	UxPutContext( label14, (char *) UxCreateNominalOrbitContext );


	/* Creation of label15 */
	label15 = XtVaCreateManagedWidget( "label15",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 330,
			XmNy, 380,
			XmNwidth, 120,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "SUBSAT LONGITUDE:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label15, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_start */
	textField_phase_start = XtVaCreateManagedWidget( "textField_phase_start",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 172,
			XmNy, 227,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 190,
			NULL );
	UxPutContext( textField_phase_start, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_days */
	textField_phase_days = XtVaCreateManagedWidget( "textField_phase_days",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 362,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 59,
			NULL );
	UxPutContext( textField_phase_days, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_orbits */
	textField_phase_orbits = XtVaCreateManagedWidget( "textField_phase_orbits",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 150,
			XmNy, 380,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_phase_orbits, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_last_rev */
	textField_last_rev = XtVaCreateManagedWidget( "textField_last_rev",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 150,
			XmNy, 410,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_last_rev, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_orb_a */
	textField_orb_a = XtVaCreateManagedWidget( "textField_orb_a",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 290,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, TRUE,
			XmNshadowThickness, 2,
			XmNtraversalOn, FALSE,
			XmNwidth, 94,
			NULL );
	UxPutContext( textField_orb_a, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_orb_e */
	textField_orb_e = XtVaCreateManagedWidget( "textField_orb_e",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 320,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_orb_e, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_orb_i */
	textField_orb_i = XtVaCreateManagedWidget( "textField_orb_i",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 350,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_orb_i, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_lon */
	textField_phase_lon = XtVaCreateManagedWidget( "textField_phase_lon",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 543,
			XmNy, 227,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			XmNwidth, 100,
			NULL );
	UxPutContext( textField_phase_lon, (char *) UxCreateNominalOrbitContext );


	/* Creation of label16 */
	label16 = XtVaCreateManagedWidget( "label16",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 310,
			XmNy, 410,
			XmNwidth, 140,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "ARGUMENT OF PERIAPSIS:" ),
			NULL );
	UxPutContext( label16, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_orb_arg_peri */
	textField_orb_arg_peri = XtVaCreateManagedWidget( "textField_orb_arg_peri",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 410,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_orb_arg_peri, (char *) UxCreateNominalOrbitContext );


	/* Creation of label17 */
	label17 = XtVaCreateManagedWidget( "label17",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 550,
			XmNy, 290,
			XmNwidth, 20,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "km" ),
			NULL );
	UxPutContext( label17, (char *) UxCreateNominalOrbitContext );


	/* Creation of label18 */
	label18 = XtVaCreateManagedWidget( "label18",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 550,
			XmNy, 350,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "deg" ),
			NULL );
	UxPutContext( label18, (char *) UxCreateNominalOrbitContext );


	/* Creation of label19 */
	label19 = XtVaCreateManagedWidget( "label19",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 550,
			XmNy, 380,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "deg" ),
			NULL );
	UxPutContext( label19, (char *) UxCreateNominalOrbitContext );


	/* Creation of label20 */
	label20 = XtVaCreateManagedWidget( "label20",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 550,
			XmNy, 410,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "deg" ),
			NULL );
	UxPutContext( label20, (char *) UxCreateNominalOrbitContext );


	/* Creation of label21 */
	label21 = XtVaCreateManagedWidget( "label21",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 210,
			XmNy, 290,
			XmNwidth, 30,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "days" ),
			NULL );
	UxPutContext( label21, (char *) UxCreateNominalOrbitContext );


	/* Creation of pushButton_CreateVectorFile */
	pushButton_CreateVectorFile = XtVaCreateManagedWidget( "pushButton_CreateVectorFile",
			xmPushButtonWidgetClass,
			CreateNominalOrbit,
			XmNx, 84,
			XmNy, 450,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_CreateVectorFile, XmNactivateCallback,
		(XtCallbackProc) cb_do_cnom,
		(XtPointer) UxCreateNominalOrbitContext );

	UxPutContext( pushButton_CreateVectorFile, (char *) UxCreateNominalOrbitContext );


	/* Creation of pushButton_cnomorb_quit */
	pushButton_cnomorb_quit = XtVaCreateManagedWidget( "pushButton_cnomorb_quit",
			xmPushButtonWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 450,
			XmNwidth, 120,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_cnomorb_quit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_cnomorb_quit,
		(XtPointer) UxCreateNominalOrbitContext );

	UxPutContext( pushButton_cnomorb_quit, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_days2 */
	textField_phase_days2 = XtVaCreateManagedWidget( "textField_phase_days2",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 150,
			XmNy, 290,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_phase_days2, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_cycle_days2 */
	textField_cycle_days2 = XtVaCreateManagedWidget( "textField_cycle_days2",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 150,
			XmNy, 320,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNresizeWidth, FALSE,
			XmNmaxLength, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_cycle_days2, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_cycle_revs2 */
	textField_cycle_revs2 = XtVaCreateManagedWidget( "textField_cycle_revs2",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 150,
			XmNy, 350,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 5,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_cycle_revs2, (char *) UxCreateNominalOrbitContext );


	/* Creation of label23 */
	label23 = XtVaCreateManagedWidget( "label23",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 45,
			XmNy, 70,
			XmNwidth, 600,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "                                                REPEAT CYCLE" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label23, (char *) UxCreateNominalOrbitContext );


	/* Creation of textField_phase_lon2 */
	textField_phase_lon2 = XtVaCreateManagedWidget( "textField_phase_lon2",
			xmTextFieldWidgetClass,
			CreateNominalOrbit,
			XmNx, 450,
			XmNy, 380,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNsensitive, FALSE,
			XmNcolumns, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNtraversalOn, FALSE,
			NULL );
	UxPutContext( textField_phase_lon2, (char *) UxCreateNominalOrbitContext );


	/* Creation of label24 */
	label24 = XtVaCreateManagedWidget( "label24",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 70,
			XmNy, 270,
			XmNwidth, 160,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "PHASE INFORMATION" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label24, (char *) UxCreateNominalOrbitContext );


	/* Creation of label26 */
	label26 = XtVaCreateManagedWidget( "label26",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 260,
			XmNy, 270,
			XmNwidth, 355,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MEAN ORBITAL ELEMENTS AT 1ST ASCENDING NODE" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label26, (char *) UxCreateNominalOrbitContext );


	/* Creation of separator2 */
	separator2 = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass,
			CreateNominalOrbit,
			XmNwidth, 667,
			XmNheight, 10,
			XmNx, 0,
			XmNy, 490,
			NULL );
	UxPutContext( separator2, (char *) UxCreateNominalOrbitContext );


	/* Creation of label3 */
	label3 = XtVaCreateManagedWidget( "label3",
			xmLabelWidgetClass,
			CreateNominalOrbit,
			XmNx, 35,
			XmNy, 500,
			XmNwidth, 590,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label3, (char *) UxCreateNominalOrbitContext );


	/* Creation of scrolledWindowText3 */
	scrolledWindowText3 = XtVaCreateManagedWidget( "scrolledWindowText3",
			xmScrolledWindowWidgetClass,
			CreateNominalOrbit,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 35,
			XmNy, 520,
			XmNwidth, 590,
			XmNheight, 160,
			NULL );
	UxPutContext( scrolledWindowText3, (char *) UxCreateNominalOrbitContext );


	/* Creation of scrolledText_cnomorb_status */
	scrolledText_cnomorb_status = XtVaCreateManagedWidget( "scrolledText_cnomorb_status",
			xmTextWidgetClass,
			scrolledWindowText3,
			XmNwidth, 571,
			XmNheight, 150,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( scrolledText_cnomorb_status, (char *) UxCreateNominalOrbitContext );


	/* Creation of pushButton_refresh1 */
	pushButton_refresh1 = XtVaCreateManagedWidget( "pushButton_refresh1",
			xmPushButtonWidgetClass,
			CreateNominalOrbit,
			XmNx, 12,
			XmNy, 97,
			XmNwidth, 25,
			XmNheight, 110,
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_refresh1, XmNactivateCallback,
		(XtCallbackProc) cb_show_orbit_relations,
		(XtPointer) scrolledList2 );

	UxPutContext( pushButton_refresh1, (char *) UxCreateNominalOrbitContext );


	XtAddCallback( CreateNominalOrbit, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCreateNominalOrbitContext);


	return ( CreateNominalOrbit );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CreateNominalOrbit( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCCreateNominalOrbit  *UxContext;
	static int		_Uxinit = 0;

	UxCreateNominalOrbitContext = UxContext =
		(_UxCCreateNominalOrbit *) UxNewContext( sizeof(_UxCCreateNominalOrbit), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CreateNominalOrbit();

	XtAddCallback(XtParent(rtrn), XtNpopupCallback,
		cb_show_orbit_relations, (XtPointer *) scrolledList2) ;
	 
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

