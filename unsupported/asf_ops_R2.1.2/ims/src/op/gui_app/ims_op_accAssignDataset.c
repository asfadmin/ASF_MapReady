
/*******************************************************************************
	ims_op_accAssignDataset.c

       Associated Header file: ims_op_accAssignDataset.h
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
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_op_accCb.h>


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAssignDataset.h"
#undef CONTEXT_MACRO_ACCESS

Widget	assign_datasets;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_assign_datasets()
{
	Widget		_UxParent;


	/* Creation of assign_datasets */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "assign_datasets_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 9,
			XmNy, 82,
			XmNwidth, 1036,
			XmNheight, 770,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "assign_datasets",
			NULL );

	}

	assign_datasets = XtVaCreateManagedWidget( "assign_datasets",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1036,
			XmNheight, 770,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNdialogTitle, "Assign Datasets" ),
			XmNsensitive, TRUE,
			XmNautoUnmanage, TRUE,
			NULL );
	UxPutContext( assign_datasets, (char *) UxAssign_datasetsContext );
	UxPutClassCode( assign_datasets, _UxIfClassId );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			assign_datasets,
			XmNx, 382,
			XmNy, 18,
			XmNwidth, 282,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Assign Datasets to Account" ),
			NULL );
	UxPutContext( label1, (char *) UxAssign_datasetsContext );


	/* Creation of frame1 */
	frame1 = XtVaCreateManagedWidget( "frame1",
			xmFrameWidgetClass,
			assign_datasets,
			XmNwidth, 608,
			XmNheight, 516,
			XmNx, 402,
			XmNy, 204,
			XmNshadowThickness, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame1, (char *) UxAssign_datasetsContext );


	/* Creation of form1 */
	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass,
			frame1,
			XmNwidth, 599,
			XmNheight, 510,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 30,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form1, (char *) UxAssign_datasetsContext );


	/* Creation of assigned_datasetsSW */
	assigned_datasetsSW = XtVaCreateManagedWidget( "assigned_datasetsSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 20,
			XmNy, 40,
			XmNwidth, 290,
			XmNheight, 398,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assigned_datasetsSW, (char *) UxAssign_datasetsContext );


	/* Creation of assigned_datasetsSL */
	assigned_datasetsSL = XtVaCreateManagedWidget( "assigned_datasetsSL",
			xmListWidgetClass,
			assigned_datasetsSW,
			XmNwidth, 600,
			XmNheight, 369,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( assigned_datasetsSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( assigned_datasetsSL, (char *) UxAssign_datasetsContext );


	/* Creation of scrolledWindowList4 */
	scrolledWindowList4 = XtVaCreateManagedWidget( "scrolledWindowList4",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 322,
			XmNy, 40,
			XmNwidth, 39,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( scrolledWindowList4, (char *) UxAssign_datasetsContext );


	/* Creation of orderSL */
	orderSL = XtVaCreateManagedWidget( "orderSL",
			xmListWidgetClass,
			scrolledWindowList4,
			XmNwidth, 36,
			XmNheight, 371,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( orderSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) 1 );

	UxPutContext( orderSL, (char *) UxAssign_datasetsContext );


	/* Creation of scrolledWindowList5 */
	scrolledWindowList5 = XtVaCreateManagedWidget( "scrolledWindowList5",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 372,
			XmNy, 40,
			XmNwidth, 39,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( scrolledWindowList5, (char *) UxAssign_datasetsContext );


	/* Creation of addSL */
	addSL = XtVaCreateManagedWidget( "addSL",
			xmListWidgetClass,
			scrolledWindowList5,
			XmNwidth, 36,
			XmNheight, 371,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( addSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) 2 );

	UxPutContext( addSL, (char *) UxAssign_datasetsContext );


	/* Creation of scrolledWindowList6 */
	scrolledWindowList6 = XtVaCreateManagedWidget( "scrolledWindowList6",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 422,
			XmNy, 40,
			XmNwidth, 39,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( scrolledWindowList6, (char *) UxAssign_datasetsContext );


	/* Creation of getSL */
	getSL = XtVaCreateManagedWidget( "getSL",
			xmListWidgetClass,
			scrolledWindowList6,
			XmNwidth, 36,
			XmNheight, 371,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( getSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) 3 );

	UxPutContext( getSL, (char *) UxAssign_datasetsContext );


	/* Creation of scrolledWindowList7 */
	scrolledWindowList7 = XtVaCreateManagedWidget( "scrolledWindowList7",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 472,
			XmNy, 40,
			XmNwidth, 39,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( scrolledWindowList7, (char *) UxAssign_datasetsContext );


	/* Creation of deleteSL */
	deleteSL = XtVaCreateManagedWidget( "deleteSL",
			xmListWidgetClass,
			scrolledWindowList7,
			XmNwidth, 36,
			XmNheight, 371,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( deleteSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) 4 );

	UxPutContext( deleteSL, (char *) UxAssign_datasetsContext );


	/* Creation of replaceSW */
	replaceSW = XtVaCreateManagedWidget( "replaceSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 522,
			XmNy, 40,
			XmNwidth, 39,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( replaceSW, (char *) UxAssign_datasetsContext );


	/* Creation of replaceSL */
	replaceSL = XtVaCreateManagedWidget( "replaceSL",
			xmListWidgetClass,
			replaceSW,
			XmNwidth, 36,
			XmNheight, 371,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	XtAddCallback( replaceSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_assigned_listCb,
		(XtPointer) 5 );

	UxPutContext( replaceSL, (char *) UxAssign_datasetsContext );


	/* Creation of guide_sbSW */
	guide_sbSW = XtVaCreateManagedWidget( "guide_sbSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 1,
			XmNx, 570,
			XmNy, 40,
			XmNwidth, 15,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( guide_sbSW, (char *) UxAssign_datasetsContext );


	/* Creation of guide_sbSL */
	guide_sbSL = XtVaCreateManagedWidget( "guide_sbSL",
			xmListWidgetClass,
			guide_sbSW,
			XmNwidth, 2,
			XmNheight, 378,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	UxPutContext( guide_sbSL, (char *) UxAssign_datasetsContext );

	assign_datasets_assigned_sbCb( guide_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of label63 */
	label63 = XtVaCreateManagedWidget( "label63",
			xmLabelWidgetClass,
			form1,
			XmNx, 332,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "O" ),
			NULL );
	UxPutContext( label63, (char *) UxAssign_datasetsContext );


	/* Creation of label64 */
	label64 = XtVaCreateManagedWidget( "label64",
			xmLabelWidgetClass,
			form1,
			XmNx, 384,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "A" ),
			NULL );
	UxPutContext( label64, (char *) UxAssign_datasetsContext );


	/* Creation of label65 */
	label65 = XtVaCreateManagedWidget( "label65",
			xmLabelWidgetClass,
			form1,
			XmNx, 432,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "G" ),
			NULL );
	UxPutContext( label65, (char *) UxAssign_datasetsContext );


	/* Creation of label66 */
	label66 = XtVaCreateManagedWidget( "label66",
			xmLabelWidgetClass,
			form1,
			XmNx, 480,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "D" ),
			NULL );
	UxPutContext( label66, (char *) UxAssign_datasetsContext );


	/* Creation of label67 */
	label67 = XtVaCreateManagedWidget( "label67",
			xmLabelWidgetClass,
			form1,
			XmNx, 532,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "R" ),
			NULL );
	UxPutContext( label67, (char *) UxAssign_datasetsContext );


	/* Creation of deletePB */
	deletePB = XtVaCreateManagedWidget( "deletePB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 104,
			XmNy, 456,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( deletePB, XmNactivateCallback,
		(XtCallbackProc) assign_datasets_delete_datasetsCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( deletePB, (char *) UxAssign_datasetsContext );


	/* Creation of label75 */
	label75 = XtVaCreateManagedWidget( "label75",
			xmLabelWidgetClass,
			form1,
			XmNx, 32,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "DataSet," ),
			NULL );
	UxPutContext( label75, (char *) UxAssign_datasetsContext );


	/* Creation of label76 */
	label76 = XtVaCreateManagedWidget( "label76",
			xmLabelWidgetClass,
			form1,
			XmNx, 140,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Platform," ),
			NULL );
	UxPutContext( label76, (char *) UxAssign_datasetsContext );


	/* Creation of label77 */
	label77 = XtVaCreateManagedWidget( "label77",
			xmLabelWidgetClass,
			form1,
			XmNx, 246,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Sensor" ),
			NULL );
	UxPutContext( label77, (char *) UxAssign_datasetsContext );


	/* Creation of frame7 */
	frame7 = XtVaCreateManagedWidget( "frame7",
			xmFrameWidgetClass,
			form1,
			XmNwidth, 266,
			XmNheight, 62,
			XmNx, 322,
			XmNy, 434,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame7, (char *) UxAssign_datasetsContext );


	/* Creation of form7 */
	form7 = XtVaCreateManagedWidget( "form7",
			xmFormWidgetClass,
			frame7,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, -2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form7, (char *) UxAssign_datasetsContext );


	/* Creation of label68 */
	label68 = XtVaCreateManagedWidget( "label68",
			xmLabelWidgetClass,
			form7,
			XmNx, 6,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "O: Order" ),
			NULL );
	UxPutContext( label68, (char *) UxAssign_datasetsContext );


	/* Creation of label69 */
	label69 = XtVaCreateManagedWidget( "label69",
			xmLabelWidgetClass,
			form7,
			XmNx, 106,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "A: Add" ),
			NULL );
	UxPutContext( label69, (char *) UxAssign_datasetsContext );


	/* Creation of label70 */
	label70 = XtVaCreateManagedWidget( "label70",
			xmLabelWidgetClass,
			form7,
			XmNx, 196,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "G: Get" ),
			NULL );
	UxPutContext( label70, (char *) UxAssign_datasetsContext );


	/* Creation of label71 */
	label71 = XtVaCreateManagedWidget( "label71",
			xmLabelWidgetClass,
			form7,
			XmNx, 0,
			XmNy, 32,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "D: Delete" ),
			XmNwidth, 92,
			NULL );
	UxPutContext( label71, (char *) UxAssign_datasetsContext );


	/* Creation of label72 */
	label72 = XtVaCreateManagedWidget( "label72",
			xmLabelWidgetClass,
			form7,
			XmNx, 106,
			XmNy, 32,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "R: Replace" ),
			NULL );
	UxPutContext( label72, (char *) UxAssign_datasetsContext );


	/* Creation of closeButton */
	closeButton = XtVaCreateManagedWidget( "closeButton",
			xmPushButtonWidgetClass,
			assign_datasets,
			XmNx, 870,
			XmNy, 728,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closeButton, XmNactivateCallback,
		(XtCallbackProc) assign_datasets_closeCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( closeButton, (char *) UxAssign_datasetsContext );


	/* Creation of frame4 */
	frame4 = XtVaCreateManagedWidget( "frame4",
			xmFrameWidgetClass,
			assign_datasets,
			XmNwidth, 988,
			XmNheight, 104,
			XmNx, 24,
			XmNy, 58,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( frame4, (char *) UxAssign_datasetsContext );


	/* Creation of form6 */
	form6 = XtVaCreateManagedWidget( "form6",
			xmFormWidgetClass,
			frame4,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form6, (char *) UxAssign_datasetsContext );


	/* Creation of label44 */
	label44 = XtVaCreateManagedWidget( "label44",
			xmLabelWidgetClass,
			form6,
			XmNx, 78,
			XmNy, 18,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID:" ),
			NULL );
	UxPutContext( label44, (char *) UxAssign_datasetsContext );


	/* Creation of account_idTF */
	account_idTF = XtVaCreateManagedWidget( "account_idTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 192,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( account_idTF, (char *) UxAssign_datasetsContext );


	/* Creation of label56 */
	label56 = XtVaCreateManagedWidget( "label56",
			xmLabelWidgetClass,
			form6,
			XmNx, 532,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Current Balance:" ),
			NULL );
	UxPutContext( label56, (char *) UxAssign_datasetsContext );


	/* Creation of current_balanceTF */
	current_balanceTF = XtVaCreateManagedWidget( "current_balanceTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 684,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( current_balanceTF, (char *) UxAssign_datasetsContext );


	/* Creation of label59 */
	label59 = XtVaCreateManagedWidget( "label59",
			xmLabelWidgetClass,
			form6,
			XmNx, 78,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Creation:" ),
			NULL );
	UxPutContext( label59, (char *) UxAssign_datasetsContext );


	/* Creation of creationTF */
	creationTF = XtVaCreateManagedWidget( "creationTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 192,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( creationTF, (char *) UxAssign_datasetsContext );


	/* Creation of expirationTF */
	expirationTF = XtVaCreateManagedWidget( "expirationTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 684,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( expirationTF, (char *) UxAssign_datasetsContext );


	/* Creation of label60 */
	label60 = XtVaCreateManagedWidget( "label60",
			xmLabelWidgetClass,
			form6,
			XmNx, 532,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Expiration:" ),
			NULL );
	UxPutContext( label60, (char *) UxAssign_datasetsContext );


	/* Creation of frame5 */
	frame5 = XtVaCreateManagedWidget( "frame5",
			xmFrameWidgetClass,
			assign_datasets,
			XmNwidth, 355,
			XmNheight, 516,
			XmNx, 25,
			XmNy, 204,
			XmNshadowThickness, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame5, (char *) UxAssign_datasetsContext );


	/* Creation of form4 */
	form4 = XtVaCreateManagedWidget( "form4",
			xmFormWidgetClass,
			frame5,
			XmNwidth, 349,
			XmNheight, 510,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 3,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form4, (char *) UxAssign_datasetsContext );


	/* Creation of label57 */
	label57 = XtVaCreateManagedWidget( "label57",
			xmLabelWidgetClass,
			form4,
			XmNx, 26,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "DataSet," ),
			NULL );
	UxPutContext( label57, (char *) UxAssign_datasetsContext );


	/* Creation of assign_datasetsSW */
	assign_datasetsSW = XtVaCreateManagedWidget( "assign_datasetsSW",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 14,
			XmNy, 40,
			XmNwidth, 290,
			XmNheight, 398,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assign_datasetsSW, (char *) UxAssign_datasetsContext );


	/* Creation of assign_datasetsSL */
	assign_datasetsSL = XtVaCreateManagedWidget( "assign_datasetsSL",
			xmListWidgetClass,
			assign_datasetsSW,
			XmNwidth, 598,
			XmNheight, 368,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNdoubleClickInterval, 200,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( assign_datasetsSL, XmNdefaultActionCallback,
		(XtCallbackProc) assign_datasets_datasets_listCb,
		(XtPointer) UxAssign_datasetsContext );
	XtAddCallback( assign_datasetsSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_datasets_datasets_listCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( assign_datasetsSL, (char *) UxAssign_datasetsContext );


	/* Creation of assign_datasets_sbSW */
	assign_datasets_sbSW = XtVaCreateManagedWidget( "assign_datasets_sbSW",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 1,
			XmNx, 314,
			XmNy, 40,
			XmNwidth, 15,
			XmNheight, 373,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assign_datasets_sbSW, (char *) UxAssign_datasetsContext );


	/* Creation of assign_datasets_sbSL */
	assign_datasets_sbSL = XtVaCreateManagedWidget( "assign_datasets_sbSL",
			xmListWidgetClass,
			assign_datasets_sbSW,
			XmNwidth, 2,
			XmNheight, 378,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	UxPutContext( assign_datasets_sbSL, (char *) UxAssign_datasetsContext );

	assign_datasets_datasets_sbCb( assign_datasets_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of addPB */
	addPB = XtVaCreateManagedWidget( "addPB",
			xmPushButtonWidgetClass,
			form4,
			XmNx, 96,
			XmNy, 456,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Add" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( addPB, XmNactivateCallback,
		(XtCallbackProc) assign_datasets_add_datasetsCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( addPB, (char *) UxAssign_datasetsContext );


	/* Creation of label73 */
	label73 = XtVaCreateManagedWidget( "label73",
			xmLabelWidgetClass,
			form4,
			XmNx, 132,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Platform," ),
			NULL );
	UxPutContext( label73, (char *) UxAssign_datasetsContext );


	/* Creation of label74 */
	label74 = XtVaCreateManagedWidget( "label74",
			xmLabelWidgetClass,
			form4,
			XmNx, 242,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Sensor" ),
			NULL );
	UxPutContext( label74, (char *) UxAssign_datasetsContext );


	/* Creation of label61 */
	label61 = XtVaCreateManagedWidget( "label61",
			xmLabelWidgetClass,
			assign_datasets,
			XmNx, 158,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Datasets" ),
			NULL );
	UxPutContext( label61, (char *) UxAssign_datasetsContext );


	/* Creation of label62 */
	label62 = XtVaCreateManagedWidget( "label62",
			xmLabelWidgetClass,
			assign_datasets,
			XmNx, 584,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Datasets Assigned to Account" ),
			NULL );
	UxPutContext( label62, (char *) UxAssign_datasetsContext );


	/* Creation of updateButton */
	updateButton = XtVaCreateManagedWidget( "updateButton",
			xmPushButtonWidgetClass,
			assign_datasets,
			XmNx, 26,
			XmNy, 728,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Update" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( updateButton, XmNactivateCallback,
		(XtCallbackProc) assign_datasets_updateCb,
		(XtPointer) UxAssign_datasetsContext );

	UxPutContext( updateButton, (char *) UxAssign_datasetsContext );


	XtAddCallback( assign_datasets, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAssign_datasetsContext);


	return ( assign_datasets );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_assign_datasets( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCassign_datasets     *UxContext;
	static int		_Uxinit = 0;

	UxAssign_datasetsContext = UxContext =
		(_UxCassign_datasets *) UxNewContext( sizeof(_UxCassign_datasets), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_assign_datasets();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

