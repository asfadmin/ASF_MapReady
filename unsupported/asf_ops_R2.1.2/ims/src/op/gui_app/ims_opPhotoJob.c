
/*******************************************************************************
	ims_opPhotoJob.c

       Associated Header file: ims_opPhotoJob.h
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
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
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
#include "ims_opPhotoJob.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_photoJob()
{
	Widget		_UxParent;
	Widget		menuBar_p3_shell;
	Widget		menuBar1_p5_shell;
	Widget		menuBar1_p6_shell;
	Widget		statusOM_pane_shell;


	/* Creation of photoJob */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "photoJob_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 60,
			XmNy, 50,
			XmNwidth, 1047,
			XmNheight, 790,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "photoJob",
			NULL );

	}

	photoJob = XtVaCreateManagedWidget( "photoJob",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1047,
			XmNheight, 790,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( photoJob, (char *) UxPhotoJobContext );
	UxPutClassCode( photoJob, _UxIfClassId );


	/* Creation of menuBar3 */
	menuBar3 = XtVaCreateManagedWidget( "menuBar3",
			xmRowColumnWidgetClass,
			photoJob,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 0,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNheight, 36,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( menuBar3, (char *) UxPhotoJobContext );


	/* Creation of menuBar_p3 */
	menuBar_p3_shell = XtVaCreatePopupShell ("menuBar_p3_shell",
			xmMenuShellWidgetClass, menuBar3,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p3 = XtVaCreateWidget( "menuBar_p3",
			xmRowColumnWidgetClass,
			menuBar_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar_p3, (char *) UxPhotoJobContext );


	/* Creation of welcomeMPB */
	welcomeMPB = XtVaCreateManagedWidget( "welcomeMPB",
			xmPushButtonWidgetClass,
			menuBar_p3,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( welcomeMPB, XmNactivateCallback,
		(XtCallbackProc) goto_welcomeScreen,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( welcomeMPB, (char *) UxPhotoJobContext );


	/* Creation of menuBar_p1_b19 */
	menuBar_p1_b19 = XtVaCreateManagedWidget( "menuBar_p1_b19",
			xmSeparatorWidgetClass,
			menuBar_p3,
			NULL );
	UxPutContext( menuBar_p1_b19, (char *) UxPhotoJobContext );


	/* Creation of gotoPhotoOrderMPB */
	gotoPhotoOrderMPB = XtVaCreateManagedWidget( "gotoPhotoOrderMPB",
			xmPushButtonWidgetClass,
			menuBar_p3,
			RES_CONVERT( XmNlabelString, "Create Photo Jobs Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( gotoPhotoOrderMPB, XmNactivateCallback,
		(XtCallbackProc) goto_photoOrderScreen,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( gotoPhotoOrderMPB, (char *) UxPhotoJobContext );


	/* Creation of menuBar_p1_b22 */
	menuBar_p1_b22 = XtVaCreateManagedWidget( "menuBar_p1_b22",
			xmSeparatorWidgetClass,
			menuBar_p3,
			NULL );
	UxPutContext( menuBar_p1_b22, (char *) UxPhotoJobContext );


	/* Creation of menuBar_p1_b27 */
	menuBar_p1_b27 = XtVaCreateManagedWidget( "menuBar_p1_b27",
			xmPushButtonWidgetClass,
			menuBar_p3,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( menuBar_p1_b27, XmNactivateCallback,
		(XtCallbackProc) photoJob_closeCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( menuBar_p1_b27, (char *) UxPhotoJobContext );


	/* Creation of menuBar_top_b3 */
	menuBar_top_b3 = XtVaCreateManagedWidget( "menuBar_top_b3",
			xmCascadeButtonWidgetClass,
			menuBar3,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, menuBar_p3,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 10,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar_top_b3, (char *) UxPhotoJobContext );


	/* Creation of menuBar1_p5 */
	menuBar1_p5_shell = XtVaCreatePopupShell ("menuBar1_p5_shell",
			xmMenuShellWidgetClass, menuBar3,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p5 = XtVaCreateWidget( "menuBar1_p5",
			xmRowColumnWidgetClass,
			menuBar1_p5_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p5, (char *) UxPhotoJobContext );


	/* Creation of photoJobsPrintMPB */
	photoJobsPrintMPB = XtVaCreateManagedWidget( "photoJobsPrintMPB",
			xmPushButtonWidgetClass,
			menuBar1_p5,
			RES_CONVERT( XmNlabelString, "Print  Screen" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( photoJobsPrintMPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_printCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( photoJobsPrintMPB, (char *) UxPhotoJobContext );


	/* Creation of menuBar1_top_b5 */
	menuBar1_top_b5 = XtVaCreateManagedWidget( "menuBar1_top_b5",
			xmCascadeButtonGadgetClass,
			menuBar3,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, menuBar1_p5,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b5, (char *) UxPhotoJobContext );


	/* Creation of menuBar1_p6 */
	menuBar1_p6_shell = XtVaCreatePopupShell ("menuBar1_p6_shell",
			xmMenuShellWidgetClass, menuBar3,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p6 = XtVaCreateWidget( "menuBar1_p6",
			xmRowColumnWidgetClass,
			menuBar1_p6_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p6, (char *) UxPhotoJobContext );


	/* Creation of menuBar1_p3_b3 */
	menuBar1_p3_b3 = XtVaCreateManagedWidget( "menuBar1_p3_b3",
			xmPushButtonWidgetClass,
			menuBar1_p6,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( menuBar1_p3_b3, (char *) UxPhotoJobContext );


	/* Creation of menuBar1_top_b6 */
	menuBar1_top_b6 = XtVaCreateManagedWidget( "menuBar1_top_b6",
			xmCascadeButtonWidgetClass,
			menuBar3,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, menuBar1_p6,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b6, (char *) UxPhotoJobContext );


	/* Creation of label48 */
	label48 = XtVaCreateManagedWidget( "label48",
			xmLabelWidgetClass,
			photoJob,
			XmNx, 375,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Complete   Photo   Jobs   Screen" ),
			XmNheight, 32,
			NULL );
	UxPutContext( label48, (char *) UxPhotoJobContext );


	/* Creation of separator7 */
	separator7 = XtVaCreateManagedWidget( "separator7",
			xmSeparatorWidgetClass,
			photoJob,
			XmNwidth, 1048,
			XmNheight, 4,
			XmNx, 0,
			XmNy, 732,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator7, (char *) UxPhotoJobContext );


	/* Creation of frame4 */
	frame4 = XtVaCreateManagedWidget( "frame4",
			xmFrameWidgetClass,
			photoJob,
			XmNwidth, 416,
			XmNheight, 652,
			XmNx, 8,
			XmNy, 72,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame4, (char *) UxPhotoJobContext );


	/* Creation of form6 */
	form6 = XtVaCreateManagedWidget( "form6",
			xmFormWidgetClass,
			frame4,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, -44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form6, (char *) UxPhotoJobContext );


	/* Creation of jobIdSW1 */
	jobIdSW1 = XtVaCreateManagedWidget( "jobIdSW1",
			xmScrolledWindowWidgetClass,
			form6,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 16,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 336,
			XmNwidth, 80,
			NULL );
	UxPutContext( jobIdSW1, (char *) UxPhotoJobContext );


	/* Creation of jobIdSL1 */
	jobIdSL1 = XtVaCreateManagedWidget( "jobIdSL1",
			xmListWidgetClass,
			jobIdSW1,
			XmNwidth, 80,
			XmNheight, 336,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 93,
			XmNlistSpacing, 0,
			NULL );
	XtAddCallback( jobIdSL1, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_jobLists_selectionCb,
		(XtPointer) 1 );
	XtAddCallback( jobIdSL1, XmNdefaultActionCallback,
		(XtCallbackProc) photoJob_viewCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( jobIdSL1, (char *) UxPhotoJobContext );


	/* Creation of label51 */
	label51 = XtVaCreateManagedWidget( "label51",
			xmLabelWidgetClass,
			form6,
			XmNx, 176,
			XmNy, 232,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Start Date" ),
			NULL );
	UxPutContext( label51, (char *) UxPhotoJobContext );


	/* Creation of startDateSW1 */
	startDateSW1 = XtVaCreateManagedWidget( "startDateSW1",
			xmScrolledWindowWidgetClass,
			form6,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 176,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 336,
			XmNwidth, 100,
			NULL );
	UxPutContext( startDateSW1, (char *) UxPhotoJobContext );


	/* Creation of startDateSL1 */
	startDateSL1 = XtVaCreateManagedWidget( "startDateSL1",
			xmListWidgetClass,
			startDateSW1,
			XmNwidth, 100,
			XmNheight, 336,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 93,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			NULL );
	XtAddCallback( startDateSL1, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_jobLists_selectionCb,
		(XtPointer) 3 );
	XtAddCallback( startDateSL1, XmNdefaultActionCallback,
		(XtCallbackProc) photoJob_viewCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( startDateSL1, (char *) UxPhotoJobContext );


	/* Creation of photoJobDummySW1 */
	photoJobDummySW1 = XtVaCreateManagedWidget( "photoJobDummySW1",
			xmScrolledWindowWidgetClass,
			form6,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 376,
			XmNy, 256,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 332,
			NULL );
	UxPutContext( photoJobDummySW1, (char *) UxPhotoJobContext );


	/* Creation of photoJobDummySL1 */
	photoJobDummySL1 = XtVaCreateManagedWidget( "photoJobDummySL1",
			xmListWidgetClass,
			photoJobDummySW1,
			XmNwidth, 2,
			XmNheight, 320,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 93,
			XmNvisibleItemCount, 19,
			NULL );
	UxPutContext( photoJobDummySL1, (char *) UxPhotoJobContext );


	/* Creation of label53 */
	label53 = XtVaCreateManagedWidget( "label53",
			xmLabelWidgetClass,
			form6,
			XmNx, 16,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Photo Job ID:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label53, (char *) UxPhotoJobContext );


	/* Creation of jobIdTF1 */
	jobIdTF1 = XtVaCreateManagedWidget( "jobIdTF1",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 141,
			XmNx, 188,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( jobIdTF1, (char *) UxPhotoJobContext );


	/* Creation of jobIdListPB */
	jobIdListPB = XtVaCreateManagedWidget( "jobIdListPB",
			xmPushButtonWidgetClass,
			form6,
			XmNx, 332,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNheight, 36,
			XmNshadowThickness, 3,
			XmNwidth, 60,
			NULL );
	XtAddCallback( jobIdListPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_jobId_validsCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( jobIdListPB, (char *) UxPhotoJobContext );


	/* Creation of searchPB */
	searchPB = XtVaCreateManagedWidget( "searchPB",
			xmPushButtonWidgetClass,
			form6,
			XmNx, 40,
			XmNy, 602,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNwidth, 108,
			NULL );
	XtAddCallback( searchPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_searchCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( searchPB, (char *) UxPhotoJobContext );


	/* Creation of separator9 */
	separator9 = XtVaCreateManagedWidget( "separator9",
			xmSeparatorWidgetClass,
			form6,
			XmNwidth, 412,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator9, (char *) UxPhotoJobContext );


	/* Creation of label50 */
	label50 = XtVaCreateManagedWidget( "label50",
			xmLabelWidgetClass,
			form6,
			XmNx, 16,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Processing Type:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label50, (char *) UxPhotoJobContext );


	/* Creation of photoTypeTF */
	photoTypeTF = XtVaCreateManagedWidget( "photoTypeTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 141,
			XmNx, 188,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( photoTypeTF, (char *) UxPhotoJobContext );


	/* Creation of photoTypeListPB */
	photoTypeListPB = XtVaCreateManagedWidget( "photoTypeListPB",
			xmPushButtonWidgetClass,
			form6,
			XmNx, 332,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNheight, 36,
			XmNshadowThickness, 3,
			XmNwidth, 60,
			NULL );
	XtAddCallback( photoTypeListPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_photoType_validsCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( photoTypeListPB, (char *) UxPhotoJobContext );


	/* Creation of label67 */
	label67 = XtVaCreateManagedWidget( "label67",
			xmLabelWidgetClass,
			form6,
			XmNx, 140,
			XmNy, 138,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "From" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label67, (char *) UxPhotoJobContext );


	/* Creation of label68 */
	label68 = XtVaCreateManagedWidget( "label68",
			xmLabelWidgetClass,
			form6,
			XmNx, 280,
			XmNy, 138,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "To" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label68, (char *) UxPhotoJobContext );


	/* Creation of photoTypeSW1 */
	photoTypeSW1 = XtVaCreateManagedWidget( "photoTypeSW1",
			xmScrolledWindowWidgetClass,
			form6,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 100,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 336,
			XmNwidth, 70,
			NULL );
	UxPutContext( photoTypeSW1, (char *) UxPhotoJobContext );


	/* Creation of photoTypeSL1 */
	photoTypeSL1 = XtVaCreateManagedWidget( "photoTypeSL1",
			xmListWidgetClass,
			photoTypeSW1,
			XmNwidth, 70,
			XmNheight, 336,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 93,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( photoTypeSL1, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_jobLists_selectionCb,
		(XtPointer) 2 );
	XtAddCallback( photoTypeSL1, XmNdefaultActionCallback,
		(XtCallbackProc) photoJob_viewCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( photoTypeSL1, (char *) UxPhotoJobContext );


	/* Creation of statusSW1 */
	statusSW1 = XtVaCreateManagedWidget( "statusSW1",
			xmScrolledWindowWidgetClass,
			form6,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 280,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 336,
			XmNwidth, 96,
			NULL );
	UxPutContext( statusSW1, (char *) UxPhotoJobContext );


	/* Creation of statusSL1 */
	statusSL1 = XtVaCreateManagedWidget( "statusSL1",
			xmListWidgetClass,
			statusSW1,
			XmNwidth, 96,
			XmNheight, 336,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 93,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			NULL );
	XtAddCallback( statusSL1, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_jobLists_selectionCb,
		(XtPointer) 4 );
	XtAddCallback( statusSL1, XmNdefaultActionCallback,
		(XtCallbackProc) photoJob_viewCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( statusSL1, (char *) UxPhotoJobContext );


	/* Creation of label69 */
	label69 = XtVaCreateManagedWidget( "label69",
			xmLabelWidgetClass,
			form6,
			XmNx, 296,
			XmNy, 232,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			NULL );
	UxPutContext( label69, (char *) UxPhotoJobContext );


	/* Creation of label70 */
	label70 = XtVaCreateManagedWidget( "label70",
			xmLabelWidgetClass,
			form6,
			XmNx, 108,
			XmNy, 232,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type" ),
			NULL );
	UxPutContext( label70, (char *) UxPhotoJobContext );


	/* Creation of fromDateText */
	fromDateText = XtVaCreateManagedWidget( "fromDateText",
			xmTextWidgetClass,
			form6,
			XmNwidth, 92,
			XmNx, 188,
			XmNy, 132,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNvalue, "",
			XmNmarginWidth, 1,
			NULL );
	XtAddCallback( fromDateText, XmNfocusCallback,
		(XtCallbackProc) photoJob_date_looseFocusCb,
		(XtPointer) UxPhotoJobContext );
	XtAddCallback( fromDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) photoJob_check_date,
		(XtPointer) UxPhotoJobContext );
	XtAddCallback( fromDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) photoJob_check_date,
		(XtPointer) UxPhotoJobContext );
	XtAddCallback( fromDateText, XmNlosingFocusCallback,
		(XtCallbackProc) photoJob_date_looseFocusCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( fromDateText, (char *) UxPhotoJobContext );


	/* Creation of toDateText */
	toDateText = XtVaCreateManagedWidget( "toDateText",
			xmTextWidgetClass,
			form6,
			XmNwidth, 92,
			XmNx, 304,
			XmNy, 132,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 1,
			NULL );
	XtAddCallback( toDateText, XmNlosingFocusCallback,
		(XtCallbackProc) photoJob_date_looseFocusCb,
		(XtPointer) UxPhotoJobContext );
	XtAddCallback( toDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) photoJob_check_date,
		(XtPointer) UxPhotoJobContext );
	XtAddCallback( toDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) photoJob_check_date,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( toDateText, (char *) UxPhotoJobContext );


	/* Creation of label71 */
	label71 = XtVaCreateManagedWidget( "label71",
			xmLabelWidgetClass,
			form6,
			XmNx, 16,
			XmNy, 92,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Photo Job Status:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label71, (char *) UxPhotoJobContext );


	/* Creation of frame6 */
	frame6 = XtVaCreateManagedWidget( "frame6",
			xmFrameWidgetClass,
			form6,
			XmNwidth, 125,
			XmNheight, 50,
			XmNx, 10,
			XmNy, 120,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame6, (char *) UxPhotoJobContext );


	/* Creation of label64 */
	label64 = XtVaCreateManagedWidget( "label64",
			xmLabelWidgetClass,
			frame6,
			XmNx, 0,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Start Date:\nYYYY-MM-DD" ),
			XmNheight, 54,
			XmNwidth, 110,
			NULL );
	UxPutContext( label64, (char *) UxPhotoJobContext );


	/* Creation of separator11 */
	separator11 = XtVaCreateManagedWidget( "separator11",
			xmSeparatorWidgetClass,
			form6,
			XmNwidth, 412,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 216,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator11, (char *) UxPhotoJobContext );


	/* Creation of label65 */
	label65 = XtVaCreateManagedWidget( "label65",
			xmLabelWidgetClass,
			form6,
			XmNx, 76,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Jobs:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label65, (char *) UxPhotoJobContext );


	/* Creation of totalJobsTF */
	totalJobsTF = XtVaCreateManagedWidget( "totalJobsTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 92,
			XmNx, 188,
			XmNy, 182,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNheight, 32,
			XmNmarginHeight, 2,
			XmNmarginWidth, 5,
			NULL );
	UxPutContext( totalJobsTF, (char *) UxPhotoJobContext );


	/* Creation of clearPB */
	clearPB = XtVaCreateManagedWidget( "clearPB",
			xmPushButtonWidgetClass,
			form6,
			XmNx, 260,
			XmNy, 602,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Clear" ),
			XmNwidth, 108,
			NULL );
	XtAddCallback( clearPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_clearCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( clearPB, (char *) UxPhotoJobContext );


	/* Creation of label49 */
	label49 = XtVaCreateManagedWidget( "label49",
			xmLabelWidgetClass,
			form6,
			XmNx, 20,
			XmNy, 232,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Job ID" ),
			NULL );
	UxPutContext( label49, (char *) UxPhotoJobContext );


	/* Creation of statusOM_pane */
	statusOM_pane_shell = XtVaCreatePopupShell ("statusOM_pane_shell",
			xmMenuShellWidgetClass, form6,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	statusOM_pane = XtVaCreateWidget( "statusOM_pane",
			xmRowColumnWidgetClass,
			statusOM_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 150,
			XmNuserData, (XtPointer) 1,
			NULL );
	UxPutContext( statusOM_pane, (char *) UxPhotoJobContext );


	/* Creation of allPB */
	allPB = XtVaCreateManagedWidget( "allPB",
			xmPushButtonWidgetClass,
			statusOM_pane,
			RES_CONVERT( XmNlabelString, "ALL" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( allPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_optionmenu_toggledCb,
		(XtPointer) 0 );

	UxPutContext( allPB, (char *) UxPhotoJobContext );


	/* Creation of statusOptionMenu */
	statusOptionMenu = XtVaCreateManagedWidget( "statusOptionMenu",
			xmRowColumnWidgetClass,
			form6,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, statusOM_pane,
			XmNx, 173,
			XmNy, 88,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 200,
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( statusOptionMenu, (char *) UxPhotoJobContext );


	/* Creation of frame5 */
	frame5 = XtVaCreateManagedWidget( "frame5",
			xmFrameWidgetClass,
			photoJob,
			XmNwidth, 530,
			XmNheight, 651,
			XmNx, 505,
			XmNy, 73,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame5, (char *) UxPhotoJobContext );


	/* Creation of form7 */
	form7 = XtVaCreateManagedWidget( "form7",
			xmFormWidgetClass,
			frame5,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, -88,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form7, (char *) UxPhotoJobContext );


	/* Creation of label54 */
	label54 = XtVaCreateManagedWidget( "label54",
			xmLabelWidgetClass,
			form7,
			XmNx, 16,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			NULL );
	UxPutContext( label54, (char *) UxPhotoJobContext );


	/* Creation of orderIdSW2 */
	orderIdSW2 = XtVaCreateManagedWidget( "orderIdSW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 12,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 108,
			NULL );
	UxPutContext( orderIdSW2, (char *) UxPhotoJobContext );


	/* Creation of orderIdSL2 */
	orderIdSL2 = XtVaCreateManagedWidget( "orderIdSL2",
			xmListWidgetClass,
			orderIdSW2,
			XmNwidth, 108,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNitemCount, 0,
			XmNx, 0,
			XmNy, 176,
			NULL );
	XtAddCallback( orderIdSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_queueLists_selectionCb,
		(XtPointer) 1 );

	UxPutContext( orderIdSL2, (char *) UxPhotoJobContext );


	/* Creation of label55 */
	label55 = XtVaCreateManagedWidget( "label55",
			xmLabelWidgetClass,
			form7,
			XmNx, 272,
			XmNy, 64,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date:" ),
			NULL );
	UxPutContext( label55, (char *) UxPhotoJobContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form7,
			XmNwidth, 116,
			XmNx, 384,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNmarginWidth, 1,
			NULL );
	UxPutContext( orderDateTF, (char *) UxPhotoJobContext );


	/* Creation of label56 */
	label56 = XtVaCreateManagedWidget( "label56",
			xmLabelWidgetClass,
			form7,
			XmNx, 16,
			XmNy, 64,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Work Order:" ),
			NULL );
	UxPutContext( label56, (char *) UxPhotoJobContext );


	/* Creation of workOrderTF */
	workOrderTF = XtVaCreateManagedWidget( "workOrderTF",
			xmTextFieldWidgetClass,
			form7,
			XmNwidth, 88,
			XmNx, 124,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNheight, 36,
			NULL );
	UxPutContext( workOrderTF, (char *) UxPhotoJobContext );


	/* Creation of label57 */
	label57 = XtVaCreateManagedWidget( "label57",
			xmLabelWidgetClass,
			form7,
			XmNx, 100,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Photo Job ID:" ),
			NULL );
	UxPutContext( label57, (char *) UxPhotoJobContext );


	/* Creation of jobIdTF2 */
	jobIdTF2 = XtVaCreateManagedWidget( "jobIdTF2",
			xmTextFieldWidgetClass,
			form7,
			XmNwidth, 152,
			XmNx, 240,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			NULL );
	UxPutContext( jobIdTF2, (char *) UxPhotoJobContext );


	/* Creation of label58 */
	label58 = XtVaCreateManagedWidget( "label58",
			xmLabelWidgetClass,
			form7,
			XmNx, 16,
			XmNy, 104,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Prints:" ),
			NULL );
	UxPutContext( label58, (char *) UxPhotoJobContext );


	/* Creation of label59 */
	label59 = XtVaCreateManagedWidget( "label59",
			xmLabelWidgetClass,
			form7,
			XmNx, 272,
			XmNy, 104,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Cost($):" ),
			NULL );
	UxPutContext( label59, (char *) UxPhotoJobContext );


	/* Creation of totalPrintsTF */
	totalPrintsTF = XtVaCreateManagedWidget( "totalPrintsTF",
			xmTextFieldWidgetClass,
			form7,
			XmNwidth, 88,
			XmNx, 124,
			XmNy, 96,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, TRUE,
			NULL );
	UxPutContext( totalPrintsTF, (char *) UxPhotoJobContext );


	/* Creation of totalCostTF */
	totalCostTF = XtVaCreateManagedWidget( "totalCostTF",
			xmTextFieldWidgetClass,
			form7,
			XmNwidth, 116,
			XmNx, 384,
			XmNy, 96,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, TRUE,
			NULL );
	UxPutContext( totalCostTF, (char *) UxPhotoJobContext );


	/* Creation of label60 */
	label60 = XtVaCreateManagedWidget( "label60",
			xmLabelWidgetClass,
			form7,
			XmNx, 120,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label60, (char *) UxPhotoJobContext );


	/* Creation of label61 */
	label61 = XtVaCreateManagedWidget( "label61",
			xmLabelWidgetClass,
			form7,
			XmNx, 172,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Product ID" ),
			NULL );
	UxPutContext( label61, (char *) UxPhotoJobContext );


	/* Creation of label62 */
	label62 = XtVaCreateManagedWidget( "label62",
			xmLabelWidgetClass,
			form7,
			XmNx, 304,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			XmNwidth, 32,
			NULL );
	UxPutContext( label62, (char *) UxPhotoJobContext );


	/* Creation of itemIdSW2 */
	itemIdSW2 = XtVaCreateManagedWidget( "itemIdSW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 124,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 37,
			NULL );
	UxPutContext( itemIdSW2, (char *) UxPhotoJobContext );


	/* Creation of itemIdSL2 */
	itemIdSL2 = XtVaCreateManagedWidget( "itemIdSL2",
			xmListWidgetClass,
			itemIdSW2,
			XmNwidth, 37,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			NULL );
	XtAddCallback( itemIdSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_queueLists_selectionCb,
		(XtPointer) 2 );

	UxPutContext( itemIdSL2, (char *) UxPhotoJobContext );


	/* Creation of productIdSW2 */
	productIdSW2 = XtVaCreateManagedWidget( "productIdSW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 164,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 135,
			NULL );
	UxPutContext( productIdSW2, (char *) UxPhotoJobContext );


	/* Creation of productIdSL2 */
	productIdSL2 = XtVaCreateManagedWidget( "productIdSL2",
			xmListWidgetClass,
			productIdSW2,
			XmNwidth, 135,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNx, 0,
			XmNy, 176,
			NULL );
	XtAddCallback( productIdSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_queueLists_selectionCb,
		(XtPointer) 3 );

	UxPutContext( productIdSL2, (char *) UxPhotoJobContext );


	/* Creation of qtySW2 */
	qtySW2 = XtVaCreateManagedWidget( "qtySW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 304,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 36,
			NULL );
	UxPutContext( qtySW2, (char *) UxPhotoJobContext );


	/* Creation of qtySL2 */
	qtySL2 = XtVaCreateManagedWidget( "qtySL2",
			xmListWidgetClass,
			qtySW2,
			XmNwidth, 36,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			NULL );
	XtAddCallback( qtySL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_queueLists_selectionCb,
		(XtPointer) 4 );

	UxPutContext( qtySL2, (char *) UxPhotoJobContext );


	/* Creation of photoJobDummySW2 */
	photoJobDummySW2 = XtVaCreateManagedWidget( "photoJobDummySW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 500,
			XmNy, 184,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 403,
			NULL );
	UxPutContext( photoJobDummySW2, (char *) UxPhotoJobContext );


	/* Creation of photoJobDummySL2 */
	photoJobDummySL2 = XtVaCreateManagedWidget( "photoJobDummySL2",
			xmListWidgetClass,
			photoJobDummySW2,
			XmNwidth, 2,
			XmNheight, 380,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			XmNvisibleItemCount, 22,
			NULL );
	UxPutContext( photoJobDummySL2, (char *) UxPhotoJobContext );


	/* Creation of processPB */
	processPB = XtVaCreateManagedWidget( "processPB",
			xmPushButtonWidgetClass,
			form7,
			XmNx, 24,
			XmNy, 600,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Process Job" ),
			XmNwidth, 136,
			XmNsensitive, FALSE,
			XmNmarginWidth, 0,
			NULL );
	XtAddCallback( processPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_processCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( processPB, (char *) UxPhotoJobContext );


	/* Creation of separator8 */
	separator8 = XtVaCreateManagedWidget( "separator8",
			xmSeparatorWidgetClass,
			form7,
			XmNwidth, 524,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator8, (char *) UxPhotoJobContext );


	/* Creation of label52 */
	label52 = XtVaCreateManagedWidget( "label52",
			xmLabelWidgetClass,
			form7,
			XmNx, 344,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Quality" ),
			NULL );
	UxPutContext( label52, (char *) UxPhotoJobContext );


	/* Creation of qualitySW2 */
	qualitySW2 = XtVaCreateManagedWidget( "qualitySW2",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 344,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 69,
			NULL );
	UxPutContext( qualitySW2, (char *) UxPhotoJobContext );


	/* Creation of qualitySL2 */
	qualitySL2 = XtVaCreateManagedWidget( "qualitySL2",
			xmListWidgetClass,
			qualitySW2,
			XmNwidth, 69,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			NULL );
	XtAddCallback( qualitySL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_queueLists_selectionCb,
		(XtPointer) 5 );

	UxPutContext( qualitySL2, (char *) UxPhotoJobContext );


	/* Creation of completePB */
	completePB = XtVaCreateManagedWidget( "completePB",
			xmPushButtonWidgetClass,
			form7,
			XmNx, 196,
			XmNy, 600,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Complete Job" ),
			XmNwidth, 130,
			XmNsensitive, FALSE,
			XmNmarginWidth, 0,
			NULL );
	XtAddCallback( completePB, XmNactivateCallback,
		(XtCallbackProc) photoJob_completeCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( completePB, (char *) UxPhotoJobContext );


	/* Creation of qualitySW1 */
	qualitySW1 = XtVaCreateManagedWidget( "qualitySW1",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 428,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 22,
			NULL );
	UxPutContext( qualitySW1, (char *) UxPhotoJobContext );


	/* Creation of goodSL2 */
	goodSL2 = XtVaCreateManagedWidget( "goodSL2",
			xmListWidgetClass,
			qualitySW1,
			XmNwidth, 22,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			RES_CONVERT( XmNforeground, "MediumBlue" ),
			NULL );
	XtAddCallback( goodSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_quality_validsCb,
		(XtPointer) 6 );

	UxPutContext( goodSL2, (char *) UxPhotoJobContext );


	/* Creation of qualitySW3 */
	qualitySW3 = XtVaCreateManagedWidget( "qualitySW3",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 452,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 22,
			NULL );
	UxPutContext( qualitySW3, (char *) UxPhotoJobContext );


	/* Creation of regenSL2 */
	regenSL2 = XtVaCreateManagedWidget( "regenSL2",
			xmListWidgetClass,
			qualitySW3,
			XmNwidth, 22,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			RES_CONVERT( XmNforeground, "MediumBlue" ),
			NULL );
	XtAddCallback( regenSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_quality_validsCb,
		(XtPointer) 7 );

	UxPutContext( regenSL2, (char *) UxPhotoJobContext );


	/* Creation of qualitySW4 */
	qualitySW4 = XtVaCreateManagedWidget( "qualitySW4",
			xmScrolledWindowWidgetClass,
			form7,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 476,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 405,
			XmNwidth, 22,
			NULL );
	UxPutContext( qualitySW4, (char *) UxPhotoJobContext );


	/* Creation of cancelSL2 */
	cancelSL2 = XtVaCreateManagedWidget( "cancelSL2",
			xmListWidgetClass,
			qualitySW4,
			XmNwidth, 22,
			XmNheight, 405,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 176,
			RES_CONVERT( XmNforeground, "MediumBlue" ),
			NULL );
	XtAddCallback( cancelSL2, XmNbrowseSelectionCallback,
		(XtCallbackProc) photoJob_quality_validsCb,
		(XtPointer) 8 );

	UxPutContext( cancelSL2, (char *) UxPhotoJobContext );


	/* Creation of label42 */
	label42 = XtVaCreateManagedWidget( "label42",
			xmLabelWidgetClass,
			form7,
			XmNx, 420,
			XmNy, 161,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Quality Valids" ),
			NULL );
	UxPutContext( label42, (char *) UxPhotoJobContext );


	/* Creation of commentPB */
	commentPB = XtVaCreateManagedWidget( "commentPB",
			xmPushButtonWidgetClass,
			form7,
			XmNx, 363,
			XmNy, 600,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Edit Comment" ),
			XmNwidth, 136,
			XmNsensitive, FALSE,
			XmNrecomputeSize, FALSE,
			XmNresizable, FALSE,
			XmNmarginWidth, 0,
			NULL );
	XtAddCallback( commentPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_commentCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( commentPB, (char *) UxPhotoJobContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			photoJob,
			XmNx, 800,
			XmNy, 744,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) photoJob_closeCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( closePB, (char *) UxPhotoJobContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			photoJob,
			XmNx, 60,
			XmNy, 744,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "PRINT    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_printCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( printPB, (char *) UxPhotoJobContext );


	/* Creation of viewPB */
	viewPB = XtVaCreateManagedWidget( "viewPB",
			xmPushButtonWidgetClass,
			photoJob,
			XmNx, 424,
			XmNy, 300,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNlabelType, XmPIXMAP,
			XmNshadowThickness, 4,
			XmNsensitive, FALSE,
			XmNrecomputeSize, FALSE,
			XmNheight, 81,
			XmNwidth, 81,
			XmNhighlightThickness, 2,
			NULL );
	XtAddCallback( viewPB, XmNactivateCallback,
		(XtCallbackProc) photoJob_viewCb,
		(XtPointer) UxPhotoJobContext );

	UxPutContext( viewPB, (char *) UxPhotoJobContext );

	XtVaSetValues(menuBar3,
			XmNmenuHelpWidget, menuBar1_top_b6,
			NULL );

	XtVaSetValues(viewPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/arrowV" ),
			RES_CONVERT( XmNlabelInsensitivePixmap, "/local/imsdads/app-defaults/pixmaps/cal.xbm" ),
			NULL );


	XtAddCallback( photoJob, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPhotoJobContext);


	return ( photoJob );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_photoJob( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCphotoJob            *UxContext;
	static int		_Uxinit = 0;

	UxPhotoJobContext = UxContext =
		(_UxCphotoJob *) UxNewContext( sizeof(_UxCphotoJob), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_photoJob();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

