
/*******************************************************************************
	ims_opFilmGen.c

       Associated Header file: ims_opFilmGen.h
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

#include <ims_opCb.h>


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opFilmGen.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_filmGeneration()
{
	Widget		_UxParent;
	Widget		menuBar_p5_shell;
	Widget		menuBar1_p9_shell;
	Widget		menuBar5_p4_shell;
	Widget		menuBar5_p5_shell;
	Widget		menuBar1_p10_shell;
	Widget		fireSearchStatusOM_pane_shell;
	Widget		laserSearchStatusOM_pane_shell;


	/* Creation of filmGeneration */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "filmGeneration_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 99,
			XmNy, 50,
			XmNwidth, 1023,
			XmNheight, 790,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "filmGeneration",
			NULL );

	}

	filmGeneration = XtVaCreateManagedWidget( "filmGeneration",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1023,
			XmNheight, 790,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( filmGeneration, (char *) UxFilmGenerationContext );
	UxPutClassCode( filmGeneration, _UxIfClassId );


	/* Creation of menuBar5 */
	menuBar5 = XtVaCreateManagedWidget( "menuBar5",
			xmRowColumnWidgetClass,
			filmGeneration,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, -1,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNheight, 36,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( menuBar5, (char *) UxFilmGenerationContext );


	/* Creation of menuBar_p5 */
	menuBar_p5_shell = XtVaCreatePopupShell ("menuBar_p5_shell",
			xmMenuShellWidgetClass, menuBar5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p5 = XtVaCreateWidget( "menuBar_p5",
			xmRowColumnWidgetClass,
			menuBar_p5_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar_p5, (char *) UxFilmGenerationContext );


	/* Creation of welcomeMPB */
	welcomeMPB = XtVaCreateManagedWidget( "welcomeMPB",
			xmPushButtonWidgetClass,
			menuBar_p5,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( welcomeMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_goto_welcomeCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( welcomeMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar_p1_b13 */
	menuBar_p1_b13 = XtVaCreateManagedWidget( "menuBar_p1_b13",
			xmSeparatorWidgetClass,
			menuBar_p5,
			NULL );
	UxPutContext( menuBar_p1_b13, (char *) UxFilmGenerationContext );


	/* Creation of closeMPB */
	closeMPB = XtVaCreateManagedWidget( "closeMPB",
			xmPushButtonWidgetClass,
			menuBar_p5,
			RES_CONVERT( XmNlabelString, "Close   Screen" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( closeMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_closeCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( closeMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar_top_b5 */
	menuBar_top_b5 = XtVaCreateManagedWidget( "menuBar_top_b5",
			xmCascadeButtonWidgetClass,
			menuBar5,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, menuBar_p5,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 10,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar_top_b5, (char *) UxFilmGenerationContext );


	/* Creation of menuBar1_p9 */
	menuBar1_p9_shell = XtVaCreatePopupShell ("menuBar1_p9_shell",
			xmMenuShellWidgetClass, menuBar5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p9 = XtVaCreateWidget( "menuBar1_p9",
			xmRowColumnWidgetClass,
			menuBar1_p9_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p9, (char *) UxFilmGenerationContext );


	/* Creation of printMPB */
	printMPB = XtVaCreateManagedWidget( "printMPB",
			xmPushButtonWidgetClass,
			menuBar1_p9,
			RES_CONVERT( XmNlabelString, "Print  Screen" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( printMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_printCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( printMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar1_top_b9 */
	menuBar1_top_b9 = XtVaCreateManagedWidget( "menuBar1_top_b9",
			xmCascadeButtonGadgetClass,
			menuBar5,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, menuBar1_p9,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b9, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p4 */
	menuBar5_p4_shell = XtVaCreatePopupShell ("menuBar5_p4_shell",
			xmMenuShellWidgetClass, menuBar5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar5_p4 = XtVaCreateWidget( "menuBar5_p4",
			xmRowColumnWidgetClass,
			menuBar5_p4_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar5_p4, (char *) UxFilmGenerationContext );


	/* Creation of updateFireStatusMPB */
	updateFireStatusMPB = XtVaCreateManagedWidget( "updateFireStatusMPB",
			xmPushButtonWidgetClass,
			menuBar5_p4,
			RES_CONVERT( XmNlabelString, "Update Fire Item Status" ),
			RES_CONVERT( XmNmnemonic, "i" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateFireStatusMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fireStatus_updateCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( updateFireStatusMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p4_b2 */
	menuBar5_p4_b2 = XtVaCreateManagedWidget( "menuBar5_p4_b2",
			xmSeparatorWidgetClass,
			menuBar5_p4,
			NULL );
	UxPutContext( menuBar5_p4_b2, (char *) UxFilmGenerationContext );


	/* Creation of editFireCommentMPB */
	editFireCommentMPB = XtVaCreateManagedWidget( "editFireCommentMPB",
			xmPushButtonWidgetClass,
			menuBar5_p4,
			RES_CONVERT( XmNlabelString, "Edit Fire Item Comment" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( editFireCommentMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fire_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( editFireCommentMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p4_b6 */
	menuBar5_p4_b6 = XtVaCreateManagedWidget( "menuBar5_p4_b6",
			xmSeparatorWidgetClass,
			menuBar5_p4,
			NULL );
	UxPutContext( menuBar5_p4_b6, (char *) UxFilmGenerationContext );


	/* Creation of fireItemRegenMPB */
	fireItemRegenMPB = XtVaCreateManagedWidget( "fireItemRegenMPB",
			xmPushButtonWidgetClass,
			menuBar5_p4,
			RES_CONVERT( XmNlabelString, "Fire Item PPS Regenerate" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( fireItemRegenMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fire_regenCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireItemRegenMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_top_b1 */
	menuBar5_top_b1 = XtVaCreateManagedWidget( "menuBar5_top_b1",
			xmCascadeButtonWidgetClass,
			menuBar5,
			RES_CONVERT( XmNlabelString, "Fire Queue Functions" ),
			RES_CONVERT( XmNmnemonic, "F" ),
			XmNsubMenuId, menuBar5_p4,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginLeft, 12,
			XmNmarginRight, 12,
			NULL );
	UxPutContext( menuBar5_top_b1, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p5 */
	menuBar5_p5_shell = XtVaCreatePopupShell ("menuBar5_p5_shell",
			xmMenuShellWidgetClass, menuBar5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar5_p5 = XtVaCreateWidget( "menuBar5_p5",
			xmRowColumnWidgetClass,
			menuBar5_p5_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar5_p5, (char *) UxFilmGenerationContext );


	/* Creation of updateLaserStatusMPB */
	updateLaserStatusMPB = XtVaCreateManagedWidget( "updateLaserStatusMPB",
			xmPushButtonWidgetClass,
			menuBar5_p5,
			RES_CONVERT( XmNlabelString, "Update Laser Item Status" ),
			RES_CONVERT( XmNmnemonic, "s" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateLaserStatusMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laserStatus_updateCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( updateLaserStatusMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p5_b2 */
	menuBar5_p5_b2 = XtVaCreateManagedWidget( "menuBar5_p5_b2",
			xmSeparatorWidgetClass,
			menuBar5_p5,
			NULL );
	UxPutContext( menuBar5_p5_b2, (char *) UxFilmGenerationContext );


	/* Creation of editLaserCommentMPB */
	editLaserCommentMPB = XtVaCreateManagedWidget( "editLaserCommentMPB",
			xmPushButtonWidgetClass,
			menuBar5_p5,
			RES_CONVERT( XmNlabelString, "Edit Laser Item Comment" ),
			RES_CONVERT( XmNmnemonic, "a" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( editLaserCommentMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laser_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( editLaserCommentMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_p5_b4 */
	menuBar5_p5_b4 = XtVaCreateManagedWidget( "menuBar5_p5_b4",
			xmSeparatorWidgetClass,
			menuBar5_p5,
			NULL );
	UxPutContext( menuBar5_p5_b4, (char *) UxFilmGenerationContext );


	/* Creation of laserItemRegenMPB */
	laserItemRegenMPB = XtVaCreateManagedWidget( "laserItemRegenMPB",
			xmPushButtonWidgetClass,
			menuBar5_p5,
			RES_CONVERT( XmNlabelString, "Laser Item PPS Regenerate" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( laserItemRegenMPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laser_regenCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserItemRegenMPB, (char *) UxFilmGenerationContext );


	/* Creation of menuBar5_top_b2 */
	menuBar5_top_b2 = XtVaCreateManagedWidget( "menuBar5_top_b2",
			xmCascadeButtonWidgetClass,
			menuBar5,
			RES_CONVERT( XmNlabelString, "Laser Queue Functions" ),
			RES_CONVERT( XmNmnemonic, "L" ),
			XmNsubMenuId, menuBar5_p5,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginLeft, 12,
			XmNmarginWidth, 10,
			XmNmarginRight, 12,
			NULL );
	UxPutContext( menuBar5_top_b2, (char *) UxFilmGenerationContext );


	/* Creation of menuBar1_p10 */
	menuBar1_p10_shell = XtVaCreatePopupShell ("menuBar1_p10_shell",
			xmMenuShellWidgetClass, menuBar5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p10 = XtVaCreateWidget( "menuBar1_p10",
			xmRowColumnWidgetClass,
			menuBar1_p10_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p10, (char *) UxFilmGenerationContext );


	/* Creation of menuBar1_p3_b5 */
	menuBar1_p3_b5 = XtVaCreateManagedWidget( "menuBar1_p3_b5",
			xmPushButtonWidgetClass,
			menuBar1_p10,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( menuBar1_p3_b5, (char *) UxFilmGenerationContext );


	/* Creation of menuBar1_top_b10 */
	menuBar1_top_b10 = XtVaCreateManagedWidget( "menuBar1_top_b10",
			xmCascadeButtonWidgetClass,
			menuBar5,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, menuBar1_p10,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b10, (char *) UxFilmGenerationContext );


	/* Creation of label73 */
	label73 = XtVaCreateManagedWidget( "label73",
			xmLabelWidgetClass,
			filmGeneration,
			XmNx, 363,
			XmNy, 38,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create     Film    TTDL    Screen" ),
			XmNheight, 32,
			NULL );
	UxPutContext( label73, (char *) UxFilmGenerationContext );


	/* Creation of separator15 */
	separator15 = XtVaCreateManagedWidget( "separator15",
			xmSeparatorWidgetClass,
			filmGeneration,
			XmNwidth, 1024,
			XmNheight, 4,
			XmNx, 0,
			XmNy, 732,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator15, (char *) UxFilmGenerationContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			filmGeneration,
			XmNx, 48,
			XmNy, 744,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "PRINT    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_printCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( printPB, (char *) UxFilmGenerationContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			filmGeneration,
			XmNx, 796,
			XmNy, 744,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) filmGen_closeCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( closePB, (char *) UxFilmGenerationContext );


	/* Creation of frame7 */
	frame7 = XtVaCreateManagedWidget( "frame7",
			xmFrameWidgetClass,
			filmGeneration,
			XmNwidth, 315,
			XmNheight, 640,
			XmNx, 20,
			XmNy, 81,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame7, (char *) UxFilmGenerationContext );


	/* Creation of form2 */
	form2 = XtVaCreateManagedWidget( "form2",
			xmFormWidgetClass,
			frame7,
			XmNwidth, 450,
			XmNheight, 720,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 4,
			XmNy, 85,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form2, (char *) UxFilmGenerationContext );


	/* Creation of label74 */
	label74 = XtVaCreateManagedWidget( "label74",
			xmLabelWidgetClass,
			form2,
			XmNx, 126,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label74, (char *) UxFilmGenerationContext );


	/* Creation of label75 */
	label75 = XtVaCreateManagedWidget( "label75",
			xmLabelWidgetClass,
			form2,
			XmNx, 192,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			NULL );
	UxPutContext( label75, (char *) UxFilmGenerationContext );


	/* Creation of fireDummySW */
	fireDummySW = XtVaCreateManagedWidget( "fireDummySW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 280,
			XmNy, 176,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 383,
			NULL );
	UxPutContext( fireDummySW, (char *) UxFilmGenerationContext );


	/* Creation of fireDummySL */
	fireDummySL = XtVaCreateManagedWidget( "fireDummySL",
			xmListWidgetClass,
			fireDummySW,
			XmNwidth, 2,
			XmNheight, 366,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNmappedWhenManaged, TRUE,
			XmNvisibleItemCount, 22,
			XmNlistSpacing, 1,
			NULL );
	UxPutContext( fireDummySL, (char *) UxFilmGenerationContext );


	/* Creation of label77 */
	label77 = XtVaCreateManagedWidget( "label77",
			xmLabelWidgetClass,
			form2,
			XmNx, 52,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Fire  Recorder  Queue" ),
			NULL );
	UxPutContext( label77, (char *) UxFilmGenerationContext );


	/* Creation of fireClearPB */
	fireClearPB = XtVaCreateManagedWidget( "fireClearPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 8,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Clear" ),
			XmNwidth, 108,
			NULL );
	XtAddCallback( fireClearPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fire_clearCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireClearPB, (char *) UxFilmGenerationContext );


	/* Creation of separator14 */
	separator14 = XtVaCreateManagedWidget( "separator14",
			xmSeparatorWidgetClass,
			form2,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator14, (char *) UxFilmGenerationContext );


	/* Creation of label79 */
	label79 = XtVaCreateManagedWidget( "label79",
			xmLabelWidgetClass,
			form2,
			XmNx, 44,
			XmNy, 108,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Items:" ),
			NULL );
	UxPutContext( label79, (char *) UxFilmGenerationContext );


	/* Creation of fireTotalItemsTF */
	fireTotalItemsTF = XtVaCreateManagedWidget( "fireTotalItemsTF",
			xmTextFieldWidgetClass,
			form2,
			XmNwidth, 90,
			XmNx, 156,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNheight, 32,
			XmNmarginHeight, 2,
			XmNmarginWidth, 8,
			NULL );
	UxPutContext( fireTotalItemsTF, (char *) UxFilmGenerationContext );


	/* Creation of label72 */
	label72 = XtVaCreateManagedWidget( "label72",
			xmLabelWidgetClass,
			form2,
			XmNx, 24,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 20,
			NULL );
	UxPutContext( label72, (char *) UxFilmGenerationContext );


	/* Creation of separator18 */
	separator18 = XtVaCreateManagedWidget( "separator18",
			xmSeparatorWidgetClass,
			form2,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 88,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator18, (char *) UxFilmGenerationContext );


	/* Creation of separator19 */
	separator19 = XtVaCreateManagedWidget( "separator19",
			xmSeparatorWidgetClass,
			form2,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, -4,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator19, (char *) UxFilmGenerationContext );


	/* Creation of label89 */
	label89 = XtVaCreateManagedWidget( "label89",
			xmLabelWidgetClass,
			form2,
			XmNx, 100,
			XmNy, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label89, (char *) UxFilmGenerationContext );


	/* Creation of separator21 */
	separator21 = XtVaCreateManagedWidget( "separator21",
			xmSeparatorWidgetClass,
			form2,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, -4,
			XmNy, 580,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator21, (char *) UxFilmGenerationContext );


	/* Creation of fireAddPB */
	fireAddPB = XtVaCreateManagedWidget( "fireAddPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 189,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Add >>" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( fireAddPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fire_addCb,
		(XtPointer) 0 );

	UxPutContext( fireAddPB, (char *) UxFilmGenerationContext );


	/* Creation of fireSearchStatusOM_pane */
	fireSearchStatusOM_pane_shell = XtVaCreatePopupShell ("fireSearchStatusOM_pane_shell",
			xmMenuShellWidgetClass, form2,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	fireSearchStatusOM_pane = XtVaCreateWidget( "fireSearchStatusOM_pane",
			xmRowColumnWidgetClass,
			fireSearchStatusOM_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 150,
			XmNuserData, (XtPointer) 1,
			RES_CONVERT( XmNlabelString, "" ),
			NULL );
	UxPutContext( fireSearchStatusOM_pane, (char *) UxFilmGenerationContext );


	/* Creation of fireDummyPB */
	fireDummyPB = XtVaCreateManagedWidget( "fireDummyPB",
			xmPushButtonWidgetClass,
			fireSearchStatusOM_pane,
			RES_CONVERT( XmNlabelString, "ALL" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( fireDummyPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_optionmenu_toggledCb,
		(XtPointer) 0 );

	UxPutContext( fireDummyPB, (char *) UxFilmGenerationContext );


	/* Creation of fireSearchStatusOM */
	fireSearchStatusOM = XtVaCreateManagedWidget( "fireSearchStatusOM",
			xmRowColumnWidgetClass,
			form2,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, fireSearchStatusOM_pane,
			XmNx, 154,
			XmNy, 46,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 200,
			XmNmarginWidth, 0,
			RES_CONVERT( XmNlabelString, "" ),
			NULL );
	UxPutContext( fireSearchStatusOM, (char *) UxFilmGenerationContext );


	/* Creation of fireSearchPB */
	fireSearchPB = XtVaCreateManagedWidget( "fireSearchPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 8,
			XmNy, 46,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNwidth, 85,
			NULL );
	XtAddCallback( fireSearchPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_fire_searchCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireSearchPB, (char *) UxFilmGenerationContext );


	/* Creation of fireOrderIdSW */
	fireOrderIdSW = XtVaCreateManagedWidget( "fireOrderIdSW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 16,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 108,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomWidget, NULL,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 176,
			XmNresizable, FALSE,
			XmNbottomOffset, 69,
			NULL );
	UxPutContext( fireOrderIdSW, (char *) UxFilmGenerationContext );


	/* Creation of fireOrderIdSL */
	fireOrderIdSL = XtVaCreateManagedWidget( "fireOrderIdSL",
			xmListWidgetClass,
			fireOrderIdSW,
			XmNwidth, 108,
			XmNheight, 387,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			XmNitemCount, 0,
			XmNunitType, XmPIXELS,
			NULL );
	XtAddCallback( fireOrderIdSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_fireLists_selectionCb,
		(XtPointer) 1 );
	XtAddCallback( fireOrderIdSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_fire_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireOrderIdSL, (char *) UxFilmGenerationContext );


	/* Creation of fireItemSW */
	fireItemSW = XtVaCreateManagedWidget( "fireItemSW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 128,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 37,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomWidget, NULL,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 176,
			XmNresizable, FALSE,
			XmNbottomOffset, 69,
			NULL );
	UxPutContext( fireItemSW, (char *) UxFilmGenerationContext );


	/* Creation of fireItemSL */
	fireItemSL = XtVaCreateManagedWidget( "fireItemSL",
			xmListWidgetClass,
			fireItemSW,
			XmNwidth, 37,
			XmNheight, 387,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( fireItemSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_fireLists_selectionCb,
		(XtPointer) 2 );
	XtAddCallback( fireItemSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_fire_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireItemSL, (char *) UxFilmGenerationContext );


	/* Creation of fireStatusSW */
	fireStatusSW = XtVaCreateManagedWidget( "fireStatusSW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 168,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 110,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 176,
			XmNbottomPosition, 0,
			XmNbottomOffset, 69,
			XmNresizable, FALSE,
			NULL );
	UxPutContext( fireStatusSW, (char *) UxFilmGenerationContext );


	/* Creation of fireStatusSL */
	fireStatusSL = XtVaCreateManagedWidget( "fireStatusSL",
			xmListWidgetClass,
			fireStatusSW,
			XmNwidth, 110,
			XmNheight, 387,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			XmNitemCount, 0,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			NULL );
	XtAddCallback( fireStatusSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_fireLists_selectionCb,
		(XtPointer) 3 );
	XtAddCallback( fireStatusSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_fire_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( fireStatusSL, (char *) UxFilmGenerationContext );


	/* Creation of frame8 */
	frame8 = XtVaCreateManagedWidget( "frame8",
			xmFrameWidgetClass,
			filmGeneration,
			XmNwidth, 315,
			XmNheight, 640,
			XmNx, 689,
			XmNy, 81,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame8, (char *) UxFilmGenerationContext );


	/* Creation of form5 */
	form5 = XtVaCreateManagedWidget( "form5",
			xmFormWidgetClass,
			frame8,
			XmNwidth, 450,
			XmNheight, 720,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 4,
			XmNy, 85,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form5, (char *) UxFilmGenerationContext );


	/* Creation of laserOrderIdSW */
	laserOrderIdSW = XtVaCreateManagedWidget( "laserOrderIdSW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 16,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 108,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 69,
			NULL );
	UxPutContext( laserOrderIdSW, (char *) UxFilmGenerationContext );


	/* Creation of laserOrderIdSL */
	laserOrderIdSL = XtVaCreateManagedWidget( "laserOrderIdSL",
			xmListWidgetClass,
			laserOrderIdSW,
			XmNwidth, 108,
			XmNheight, 388,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			NULL );
	XtAddCallback( laserOrderIdSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_laserLists_selectionCb,
		(XtPointer) 1 );
	XtAddCallback( laserOrderIdSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_laser_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserOrderIdSL, (char *) UxFilmGenerationContext );


	/* Creation of laserItemSW */
	laserItemSW = XtVaCreateManagedWidget( "laserItemSW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 128,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 37,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 69,
			NULL );
	UxPutContext( laserItemSW, (char *) UxFilmGenerationContext );


	/* Creation of laserItemSL */
	laserItemSL = XtVaCreateManagedWidget( "laserItemSL",
			xmListWidgetClass,
			laserItemSW,
			XmNwidth, 37,
			XmNheight, 388,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			NULL );
	XtAddCallback( laserItemSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_laserLists_selectionCb,
		(XtPointer) 2 );
	XtAddCallback( laserItemSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_laser_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserItemSL, (char *) UxFilmGenerationContext );


	/* Creation of laserStatusSW */
	laserStatusSW = XtVaCreateManagedWidget( "laserStatusSW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 168,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 388,
			XmNwidth, 110,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 69,
			NULL );
	UxPutContext( laserStatusSW, (char *) UxFilmGenerationContext );


	/* Creation of laserStatusSL */
	laserStatusSL = XtVaCreateManagedWidget( "laserStatusSL",
			xmListWidgetClass,
			laserStatusSW,
			XmNwidth, 110,
			XmNheight, 388,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 21,
			NULL );
	XtAddCallback( laserStatusSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_laserLists_selectionCb,
		(XtPointer) 3 );
	XtAddCallback( laserStatusSL, XmNdefaultActionCallback,
		(XtCallbackProc) filmGen_laser_commentCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserStatusSL, (char *) UxFilmGenerationContext );


	/* Creation of laserDummySW */
	laserDummySW = XtVaCreateManagedWidget( "laserDummySW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 280,
			XmNy, 176,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 383,
			NULL );
	UxPutContext( laserDummySW, (char *) UxFilmGenerationContext );


	/* Creation of laserDummySL */
	laserDummySL = XtVaCreateManagedWidget( "laserDummySL",
			xmListWidgetClass,
			laserDummySW,
			XmNwidth, 1,
			XmNheight, 366,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNmappedWhenManaged, TRUE,
			XmNvisibleItemCount, 24,
			XmNlistSpacing, 1,
			NULL );
	UxPutContext( laserDummySL, (char *) UxFilmGenerationContext );


	/* Creation of separator16 */
	separator16 = XtVaCreateManagedWidget( "separator16",
			xmSeparatorWidgetClass,
			form5,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator16, (char *) UxFilmGenerationContext );


	/* Creation of label82 */
	label82 = XtVaCreateManagedWidget( "label82",
			xmLabelWidgetClass,
			form5,
			XmNx, 40,
			XmNy, 108,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Items:" ),
			NULL );
	UxPutContext( label82, (char *) UxFilmGenerationContext );


	/* Creation of label81 */
	label81 = XtVaCreateManagedWidget( "label81",
			xmLabelWidgetClass,
			form5,
			XmNx, 68,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Laser  Tech  Queue" ),
			NULL );
	UxPutContext( label81, (char *) UxFilmGenerationContext );


	/* Creation of laserTotalItemsTF */
	laserTotalItemsTF = XtVaCreateManagedWidget( "laserTotalItemsTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 90,
			XmNx, 152,
			XmNy, 101,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNheight, 32,
			XmNmarginHeight, 2,
			XmNmarginWidth, 8,
			NULL );
	UxPutContext( laserTotalItemsTF, (char *) UxFilmGenerationContext );


	/* Creation of laserAddPB */
	laserAddPB = XtVaCreateManagedWidget( "laserAddPB",
			xmPushButtonWidgetClass,
			form5,
			XmNx, 16,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "<< Add" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( laserAddPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laser_addCb,
		(XtPointer) 0 );

	UxPutContext( laserAddPB, (char *) UxFilmGenerationContext );


	/* Creation of label76 */
	label76 = XtVaCreateManagedWidget( "label76",
			xmLabelWidgetClass,
			form5,
			XmNx, 24,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 20,
			NULL );
	UxPutContext( label76, (char *) UxFilmGenerationContext );


	/* Creation of label78 */
	label78 = XtVaCreateManagedWidget( "label78",
			xmLabelWidgetClass,
			form5,
			XmNx, 128,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label78, (char *) UxFilmGenerationContext );


	/* Creation of label80 */
	label80 = XtVaCreateManagedWidget( "label80",
			xmLabelWidgetClass,
			form5,
			XmNx, 196,
			XmNy, 154,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			NULL );
	UxPutContext( label80, (char *) UxFilmGenerationContext );


	/* Creation of laserClearPB */
	laserClearPB = XtVaCreateManagedWidget( "laserClearPB",
			xmPushButtonWidgetClass,
			form5,
			XmNx, 187,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Clear" ),
			XmNwidth, 108,
			NULL );
	XtAddCallback( laserClearPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laser_clearCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserClearPB, (char *) UxFilmGenerationContext );


	/* Creation of label90 */
	label90 = XtVaCreateManagedWidget( "label90",
			xmLabelWidgetClass,
			form5,
			XmNx, 12,
			XmNy, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status:" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label90, (char *) UxFilmGenerationContext );


	/* Creation of laserSearchStatusOM_pane */
	laserSearchStatusOM_pane_shell = XtVaCreatePopupShell ("laserSearchStatusOM_pane_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	laserSearchStatusOM_pane = XtVaCreateWidget( "laserSearchStatusOM_pane",
			xmRowColumnWidgetClass,
			laserSearchStatusOM_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 150,
			XmNuserData, (XtPointer) 3,
			NULL );
	UxPutContext( laserSearchStatusOM_pane, (char *) UxFilmGenerationContext );


	/* Creation of laserDummyPB */
	laserDummyPB = XtVaCreateManagedWidget( "laserDummyPB",
			xmPushButtonWidgetClass,
			laserSearchStatusOM_pane,
			RES_CONVERT( XmNlabelString, "ALL" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( laserDummyPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_optionmenu_toggledCb,
		(XtPointer) 0 );

	UxPutContext( laserDummyPB, (char *) UxFilmGenerationContext );


	/* Creation of laserSearchStatusOM */
	laserSearchStatusOM = XtVaCreateManagedWidget( "laserSearchStatusOM",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, laserSearchStatusOM_pane,
			XmNx, 68,
			XmNy, 46,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 200,
			XmNmarginWidth, 0,
			RES_CONVERT( XmNlabelString, "" ),
			NULL );
	UxPutContext( laserSearchStatusOM, (char *) UxFilmGenerationContext );


	/* Creation of laserSearchPB */
	laserSearchPB = XtVaCreateManagedWidget( "laserSearchPB",
			xmPushButtonWidgetClass,
			form5,
			XmNx, 214,
			XmNy, 46,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNwidth, 85,
			NULL );
	XtAddCallback( laserSearchPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_laser_searchCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( laserSearchPB, (char *) UxFilmGenerationContext );


	/* Creation of separator23 */
	separator23 = XtVaCreateManagedWidget( "separator23",
			xmSeparatorWidgetClass,
			form5,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 88,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator23, (char *) UxFilmGenerationContext );


	/* Creation of separator24 */
	separator24 = XtVaCreateManagedWidget( "separator24",
			xmSeparatorWidgetClass,
			form5,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator24, (char *) UxFilmGenerationContext );


	/* Creation of separator26 */
	separator26 = XtVaCreateManagedWidget( "separator26",
			xmSeparatorWidgetClass,
			form5,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 1,
			XmNy, 580,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator26, (char *) UxFilmGenerationContext );


	/* Creation of frame9 */
	frame9 = XtVaCreateManagedWidget( "frame9",
			xmFrameWidgetClass,
			filmGeneration,
			XmNwidth, 318,
			XmNheight, 640,
			XmNx, 354,
			XmNy, 81,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame9, (char *) UxFilmGenerationContext );


	/* Creation of form8 */
	form8 = XtVaCreateManagedWidget( "form8",
			xmFormWidgetClass,
			frame9,
			XmNwidth, 308,
			XmNheight, 632,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 16,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form8, (char *) UxFilmGenerationContext );


	/* Creation of label83 */
	label83 = XtVaCreateManagedWidget( "label83",
			xmLabelWidgetClass,
			form8,
			XmNx, 21,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 20,
			NULL );
	UxPutContext( label83, (char *) UxFilmGenerationContext );


	/* Creation of ttdlOrderIdSW */
	ttdlOrderIdSW = XtVaCreateManagedWidget( "ttdlOrderIdSW",
			xmScrolledWindowWidgetClass,
			form8,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 10,
			XmNy, 124,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 106,
			NULL );
	UxPutContext( ttdlOrderIdSW, (char *) UxFilmGenerationContext );


	/* Creation of ttdlOrderIdSL */
	ttdlOrderIdSL = XtVaCreateManagedWidget( "ttdlOrderIdSL",
			xmListWidgetClass,
			ttdlOrderIdSW,
			XmNwidth, 106,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( ttdlOrderIdSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_ttdlLists_selectionCb,
		(XtPointer) 1 );

	UxPutContext( ttdlOrderIdSL, (char *) UxFilmGenerationContext );


	/* Creation of label84 */
	label84 = XtVaCreateManagedWidget( "label84",
			xmLabelWidgetClass,
			form8,
			XmNx, 120,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label84, (char *) UxFilmGenerationContext );


	/* Creation of ttdlItemSW */
	ttdlItemSW = XtVaCreateManagedWidget( "ttdlItemSW",
			xmScrolledWindowWidgetClass,
			form8,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 120,
			XmNy, 124,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 37,
			NULL );
	UxPutContext( ttdlItemSW, (char *) UxFilmGenerationContext );


	/* Creation of ttdlItemSL */
	ttdlItemSL = XtVaCreateManagedWidget( "ttdlItemSL",
			xmListWidgetClass,
			ttdlItemSW,
			XmNwidth, 37,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			NULL );
	XtAddCallback( ttdlItemSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_ttdlLists_selectionCb,
		(XtPointer) 2 );

	UxPutContext( ttdlItemSL, (char *) UxFilmGenerationContext );


	/* Creation of label85 */
	label85 = XtVaCreateManagedWidget( "label85",
			xmLabelWidgetClass,
			form8,
			XmNx, 174,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Queue Type" ),
			NULL );
	UxPutContext( label85, (char *) UxFilmGenerationContext );


	/* Creation of ttdlQueueTypeSW */
	ttdlQueueTypeSW = XtVaCreateManagedWidget( "ttdlQueueTypeSW",
			xmScrolledWindowWidgetClass,
			form8,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 160,
			XmNy, 124,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 126,
			NULL );
	UxPutContext( ttdlQueueTypeSW, (char *) UxFilmGenerationContext );


	/* Creation of ttdlQueueTypeSL */
	ttdlQueueTypeSL = XtVaCreateManagedWidget( "ttdlQueueTypeSL",
			xmListWidgetClass,
			ttdlQueueTypeSW,
			XmNwidth, 126,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( ttdlQueueTypeSL, XmNextendedSelectionCallback,
		(XtCallbackProc) filmGen_ttdlLists_selectionCb,
		(XtPointer) 3 );

	UxPutContext( ttdlQueueTypeSL, (char *) UxFilmGenerationContext );


	/* Creation of ttdlDummySW */
	ttdlDummySW = XtVaCreateManagedWidget( "ttdlDummySW",
			xmScrolledWindowWidgetClass,
			form8,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 288,
			XmNy, 124,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 436,
			NULL );
	UxPutContext( ttdlDummySW, (char *) UxFilmGenerationContext );


	/* Creation of ttdlDummySL */
	ttdlDummySL = XtVaCreateManagedWidget( "ttdlDummySL",
			xmListWidgetClass,
			ttdlDummySW,
			XmNwidth, 1,
			XmNheight, 420,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 181,
			XmNmappedWhenManaged, TRUE,
			XmNvisibleItemCount, 24,
			XmNlistSpacing, 1,
			NULL );
	UxPutContext( ttdlDummySL, (char *) UxFilmGenerationContext );


	/* Creation of label86 */
	label86 = XtVaCreateManagedWidget( "label86",
			xmLabelWidgetClass,
			form8,
			XmNx, 88,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "TTDL     Queue" ),
			NULL );
	UxPutContext( label86, (char *) UxFilmGenerationContext );


	/* Creation of separator17 */
	separator17 = XtVaCreateManagedWidget( "separator17",
			xmSeparatorWidgetClass,
			form8,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator17, (char *) UxFilmGenerationContext );


	/* Creation of label87 */
	label87 = XtVaCreateManagedWidget( "label87",
			xmLabelWidgetClass,
			form8,
			XmNx, 40,
			XmNy, 54,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Items:" ),
			NULL );
	UxPutContext( label87, (char *) UxFilmGenerationContext );


	/* Creation of ttdlTotalItemsTF */
	ttdlTotalItemsTF = XtVaCreateManagedWidget( "ttdlTotalItemsTF",
			xmTextFieldWidgetClass,
			form8,
			XmNwidth, 90,
			XmNx, 156,
			XmNy, 47,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNheight, 32,
			XmNmarginWidth, 8,
			XmNmarginHeight, 2,
			NULL );
	UxPutContext( ttdlTotalItemsTF, (char *) UxFilmGenerationContext );


	/* Creation of ttdlDeletePB */
	ttdlDeletePB = XtVaCreateManagedWidget( "ttdlDeletePB",
			xmPushButtonWidgetClass,
			form8,
			XmNx, 20,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( ttdlDeletePB, XmNactivateCallback,
		(XtCallbackProc) filmGen_ttdl_deleteCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( ttdlDeletePB, (char *) UxFilmGenerationContext );


	/* Creation of ttdlProcessPB */
	ttdlProcessPB = XtVaCreateManagedWidget( "ttdlProcessPB",
			xmPushButtonWidgetClass,
			form8,
			XmNx, 182,
			XmNy, 590,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "Process" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( ttdlProcessPB, XmNactivateCallback,
		(XtCallbackProc) filmGen_ttdl_processCb,
		(XtPointer) UxFilmGenerationContext );

	UxPutContext( ttdlProcessPB, (char *) UxFilmGenerationContext );


	/* Creation of separator22 */
	separator22 = XtVaCreateManagedWidget( "separator22",
			xmSeparatorWidgetClass,
			form8,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 88,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator22, (char *) UxFilmGenerationContext );


	/* Creation of separator27 */
	separator27 = XtVaCreateManagedWidget( "separator27",
			xmSeparatorWidgetClass,
			form8,
			XmNwidth, 312,
			XmNheight, 5,
			XmNx, -3,
			XmNy, 580,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator27, (char *) UxFilmGenerationContext );

	XtVaSetValues(menuBar5,
			XmNmenuHelpWidget, menuBar1_top_b10,
			NULL );

	XtVaSetValues(fireAddPB,
			RES_CONVERT( XmNlabelPixmap, "/usr/openwin/include/X11/bitmaps/right_ptrmsk" ),
			NULL );


	XtAddCallback( filmGeneration, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxFilmGenerationContext);


	return ( filmGeneration );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_filmGeneration( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCfilmGeneration      *UxContext;
	static int		_Uxinit = 0;

	UxFilmGenerationContext = UxContext =
		(_UxCfilmGeneration *) UxNewContext( sizeof(_UxCfilmGeneration), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_filmGeneration();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

