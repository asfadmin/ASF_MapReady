
/*******************************************************************************
	ims_opPhotoOrder.c

       Associated Header file: ims_opPhotoOrder.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
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
#include "ims_opPhotoOrder.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_photoOrder()
{
	Widget		_UxParent;
	Widget		menuBar_p2_shell;
	Widget		menuBar1_p1_shell;
	Widget		menuBar1_p4_shell;


	/* Creation of photoOrder */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "photoOrder_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 50,
			XmNy, 20,
			XmNwidth, 1040,
			XmNheight, 790,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "photoOrder",
			NULL );

	}

	photoOrder = XtVaCreateManagedWidget( "photoOrder",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1040,
			XmNheight, 790,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( photoOrder, (char *) UxPhotoOrderContext );
	UxPutClassCode( photoOrder, _UxIfClassId );


	/* Creation of label20 */
	label20 = XtVaCreateManagedWidget( "label20",
			xmLabelWidgetClass,
			photoOrder,
			XmNx, 368,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create   Photo   Jobs   Screen" ),
			XmNheight, 32,
			NULL );
	UxPutContext( label20, (char *) UxPhotoOrderContext );


	/* Creation of frame2 */
	frame2 = XtVaCreateManagedWidget( "frame2",
			xmFrameWidgetClass,
			photoOrder,
			XmNwidth, 490,
			XmNheight, 640,
			XmNx, 8,
			XmNy, 74,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame2, (char *) UxPhotoOrderContext );


	/* Creation of form3 */
	form3 = XtVaCreateManagedWidget( "form3",
			xmFormWidgetClass,
			frame2,
			XmNwidth, 450,
			XmNheight, 720,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 336,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form3, (char *) UxPhotoOrderContext );


	/* Creation of label22 */
	label22 = XtVaCreateManagedWidget( "label22",
			xmLabelWidgetClass,
			form3,
			XmNx, 132,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label22, (char *) UxPhotoOrderContext );


	/* Creation of orderIdSW1 */
	orderIdSW1 = XtVaCreateManagedWidget( "orderIdSW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 124,
			XmNy, 136,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 108,
			NULL );
	UxPutContext( orderIdSW1, (char *) UxPhotoOrderContext );


	/* Creation of orderIdSL1 */
	orderIdSL1 = XtVaCreateManagedWidget( "orderIdSL1",
			xmListWidgetClass,
			orderIdSW1,
			XmNwidth, 108,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 136,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			NULL );
	XtAddCallback( orderIdSL1, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_queueLists_selectionCb,
		(XtPointer) 2 );

	UxPutContext( orderIdSL1, (char *) UxPhotoOrderContext );


	/* Creation of label28 */
	label28 = XtVaCreateManagedWidget( "label28",
			xmLabelWidgetClass,
			form3,
			XmNx, 232,
			XmNy, 114,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label28, (char *) UxPhotoOrderContext );


	/* Creation of itemIdSW1 */
	itemIdSW1 = XtVaCreateManagedWidget( "itemIdSW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 236,
			XmNy, 136,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 37,
			NULL );
	UxPutContext( itemIdSW1, (char *) UxPhotoOrderContext );


	/* Creation of itemIdSL1 */
	itemIdSL1 = XtVaCreateManagedWidget( "itemIdSL1",
			xmListWidgetClass,
			itemIdSW1,
			XmNwidth, 37,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 136,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			NULL );
	XtAddCallback( itemIdSL1, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_queueLists_selectionCb,
		(XtPointer) 3 );

	UxPutContext( itemIdSL1, (char *) UxPhotoOrderContext );


	/* Creation of label29 */
	label29 = XtVaCreateManagedWidget( "label29",
			xmLabelWidgetClass,
			form3,
			XmNx, 286,
			XmNy, 114,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Product ID" ),
			NULL );
	UxPutContext( label29, (char *) UxPhotoOrderContext );


	/* Creation of productIdSW1 */
	productIdSW1 = XtVaCreateManagedWidget( "productIdSW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 276,
			XmNy, 136,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 135,
			NULL );
	UxPutContext( productIdSW1, (char *) UxPhotoOrderContext );


	/* Creation of productIdSL1 */
	productIdSL1 = XtVaCreateManagedWidget( "productIdSL1",
			xmListWidgetClass,
			productIdSW1,
			XmNwidth, 135,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 136,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( productIdSL1, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_queueLists_selectionCb,
		(XtPointer) 4 );

	UxPutContext( productIdSL1, (char *) UxPhotoOrderContext );


	/* Creation of label30 */
	label30 = XtVaCreateManagedWidget( "label30",
			xmLabelWidgetClass,
			form3,
			XmNx, 422,
			XmNy, 114,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			NULL );
	UxPutContext( label30, (char *) UxPhotoOrderContext );


	/* Creation of qtySW1 */
	qtySW1 = XtVaCreateManagedWidget( "qtySW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 416,
			XmNy, 136,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 36,
			NULL );
	UxPutContext( qtySW1, (char *) UxPhotoOrderContext );


	/* Creation of qtySL1 */
	qtySL1 = XtVaCreateManagedWidget( "qtySL1",
			xmListWidgetClass,
			qtySW1,
			XmNwidth, 36,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 136,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			NULL );
	XtAddCallback( qtySL1, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_queueLists_selectionCb,
		(XtPointer) 5 );

	UxPutContext( qtySL1, (char *) UxPhotoOrderContext );


	/* Creation of dummySW1 */
	dummySW1 = XtVaCreateManagedWidget( "dummySW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 452,
			XmNy, 136,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 435,
			NULL );
	UxPutContext( dummySW1, (char *) UxPhotoOrderContext );


	/* Creation of dummySL1 */
	dummySL1 = XtVaCreateManagedWidget( "dummySL1",
			xmListWidgetClass,
			dummySW1,
			XmNwidth, 1,
			XmNheight, 420,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 92,
			XmNmappedWhenManaged, TRUE,
			XmNvisibleItemCount, 24,
			XmNlistSpacing, 1,
			NULL );
	UxPutContext( dummySL1, (char *) UxPhotoOrderContext );


	/* Creation of label44 */
	label44 = XtVaCreateManagedWidget( "label44",
			xmLabelWidgetClass,
			form3,
			XmNx, 56,
			XmNy, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Processing Type:" ),
			NULL );
	UxPutContext( label44, (char *) UxPhotoOrderContext );


	/* Creation of photoTypeTF */
	photoTypeTF = XtVaCreateManagedWidget( "photoTypeTF",
			xmTextFieldWidgetClass,
			form3,
			XmNwidth, 112,
			XmNx, 196,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( photoTypeTF, (char *) UxPhotoOrderContext );


	/* Creation of photoTypePB */
	photoTypePB = XtVaCreateManagedWidget( "photoTypePB",
			xmPushButtonWidgetClass,
			form3,
			XmNx, 308,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNshadowThickness, 3,
			XmNheight, 36,
			XmNwidth, 56,
			XmNlabelType, XmSTRING,
			NULL );
	XtAddCallback( photoTypePB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_photoType_validsCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( photoTypePB, (char *) UxPhotoOrderContext );


	/* Creation of searchPB */
	searchPB = XtVaCreateManagedWidget( "searchPB",
			xmPushButtonWidgetClass,
			form3,
			XmNx, 40,
			XmNy, 588,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "SEARCH" ),
			XmNwidth, 108,
			NULL );
	XtAddCallback( searchPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_searchCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( searchPB, (char *) UxPhotoOrderContext );


	/* Creation of label63 */
	label63 = XtVaCreateManagedWidget( "label63",
			xmLabelWidgetClass,
			form3,
			XmNx, 20,
			XmNy, 112,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID" ),
			XmNheight, 24,
			NULL );
	UxPutContext( label63, (char *) UxPhotoOrderContext );


	/* Creation of userIdSW1 */
	userIdSW1 = XtVaCreateManagedWidget( "userIdSW1",
			xmScrolledWindowWidgetClass,
			form3,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 12,
			XmNy, 136,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 440,
			XmNwidth, 110,
			NULL );
	UxPutContext( userIdSW1, (char *) UxPhotoOrderContext );


	/* Creation of userIdSL1 */
	userIdSL1 = XtVaCreateManagedWidget( "userIdSL1",
			xmListWidgetClass,
			userIdSW1,
			XmNwidth, 110,
			XmNheight, 440,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 136,
			XmNlistSpacing, 1,
			XmNvisibleItemCount, 24,
			NULL );
	XtAddCallback( userIdSL1, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_queueLists_selectionCb,
		(XtPointer) 1 );

	UxPutContext( userIdSL1, (char *) UxPhotoOrderContext );


	/* Creation of clearPB */
	clearPB = XtVaCreateManagedWidget( "clearPB",
			xmPushButtonWidgetClass,
			form3,
			XmNx, 336,
			XmNy, 588,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CLEAR" ),
			XmNwidth, 108,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( clearPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_clearCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( clearPB, (char *) UxPhotoOrderContext );


	/* Creation of separator10 */
	separator10 = XtVaCreateManagedWidget( "separator10",
			xmSeparatorWidgetClass,
			form3,
			XmNwidth, 484,
			XmNheight, 5,
			XmNx, 0,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator10, (char *) UxPhotoOrderContext );


	/* Creation of label66 */
	label66 = XtVaCreateManagedWidget( "label66",
			xmLabelWidgetClass,
			form3,
			XmNx, 89,
			XmNy, 76,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Items:" ),
			NULL );
	UxPutContext( label66, (char *) UxPhotoOrderContext );


	/* Creation of totalItemsTF */
	totalItemsTF = XtVaCreateManagedWidget( "totalItemsTF",
			xmTextFieldWidgetClass,
			form3,
			XmNwidth, 112,
			XmNx, 196,
			XmNy, 68,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( totalItemsTF, (char *) UxPhotoOrderContext );


	/* Creation of frame3 */
	frame3 = XtVaCreateManagedWidget( "frame3",
			xmFrameWidgetClass,
			photoOrder,
			XmNwidth, 448,
			XmNheight, 640,
			XmNx, 580,
			XmNy, 74,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame3, (char *) UxPhotoOrderContext );


	/* Creation of form4 */
	form4 = XtVaCreateManagedWidget( "form4",
			xmFormWidgetClass,
			frame3,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, -88,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form4, (char *) UxPhotoOrderContext );


	/* Creation of label21 */
	label21 = XtVaCreateManagedWidget( "label21",
			xmLabelWidgetClass,
			form4,
			XmNx, 32,
			XmNy, 148,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			NULL );
	UxPutContext( label21, (char *) UxPhotoOrderContext );


	/* Creation of orderIdSW2 */
	orderIdSW2 = XtVaCreateManagedWidget( "orderIdSW2",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 28,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 406,
			XmNwidth, 140,
			NULL );
	UxPutContext( orderIdSW2, (char *) UxPhotoOrderContext );


	/* Creation of orderIdSL2 */
	orderIdSL2 = XtVaCreateManagedWidget( "orderIdSL2",
			xmListWidgetClass,
			orderIdSW2,
			XmNwidth, 140,
			XmNheight, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 172,
			XmNvisibleItemCount, 22,
			XmNitemCount, 0,
			XmNlistSpacing, 1,
			NULL );
	XtAddCallback( orderIdSL2, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_jobLists_selectionCb,
		(XtPointer) 1 );

	UxPutContext( orderIdSL2, (char *) UxPhotoOrderContext );


	/* Creation of label25 */
	label25 = XtVaCreateManagedWidget( "label25",
			xmLabelWidgetClass,
			form4,
			XmNx, 200,
			XmNy, 60,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date:" ),
			NULL );
	UxPutContext( label25, (char *) UxPhotoOrderContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form4,
			XmNwidth, 116,
			XmNx, 312,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNvalue, "",
			XmNmarginWidth, 1,
			NULL );
	UxPutContext( orderDateTF, (char *) UxPhotoOrderContext );


	/* Creation of label24 */
	label24 = XtVaCreateManagedWidget( "label24",
			xmLabelWidgetClass,
			form4,
			XmNx, 8,
			XmNy, 60,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Work Order:" ),
			NULL );
	UxPutContext( label24, (char *) UxPhotoOrderContext );


	/* Creation of workOrderTF */
	workOrderTF = XtVaCreateManagedWidget( "workOrderTF",
			xmTextFieldWidgetClass,
			form4,
			XmNwidth, 75,
			XmNx, 116,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( workOrderTF, (char *) UxPhotoOrderContext );


	/* Creation of label23 */
	label23 = XtVaCreateManagedWidget( "label23",
			xmLabelWidgetClass,
			form4,
			XmNx, 68,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Photo Job ID:" ),
			NULL );
	UxPutContext( label23, (char *) UxPhotoOrderContext );


	/* Creation of jobIdTF */
	jobIdTF = XtVaCreateManagedWidget( "jobIdTF",
			xmTextFieldWidgetClass,
			form4,
			XmNwidth, 152,
			XmNx, 196,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( jobIdTF, (char *) UxPhotoOrderContext );


	/* Creation of label31 */
	label31 = XtVaCreateManagedWidget( "label31",
			xmLabelWidgetClass,
			form4,
			XmNx, 8,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Prints:" ),
			NULL );
	UxPutContext( label31, (char *) UxPhotoOrderContext );


	/* Creation of label32 */
	label32 = XtVaCreateManagedWidget( "label32",
			xmLabelWidgetClass,
			form4,
			XmNx, 200,
			XmNy, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Cost($):" ),
			NULL );
	UxPutContext( label32, (char *) UxPhotoOrderContext );


	/* Creation of totalPrintsTF */
	totalPrintsTF = XtVaCreateManagedWidget( "totalPrintsTF",
			xmTextFieldWidgetClass,
			form4,
			XmNwidth, 75,
			XmNx, 116,
			XmNy, 92,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( totalPrintsTF, (char *) UxPhotoOrderContext );


	/* Creation of totalCostTF */
	totalCostTF = XtVaCreateManagedWidget( "totalCostTF",
			xmTextFieldWidgetClass,
			form4,
			XmNwidth, 116,
			XmNx, 312,
			XmNy, 92,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( totalCostTF, (char *) UxPhotoOrderContext );


	/* Creation of label45 */
	label45 = XtVaCreateManagedWidget( "label45",
			xmLabelWidgetClass,
			form4,
			XmNx, 168,
			XmNy, 148,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			NULL );
	UxPutContext( label45, (char *) UxPhotoOrderContext );


	/* Creation of label46 */
	label46 = XtVaCreateManagedWidget( "label46",
			xmLabelWidgetClass,
			form4,
			XmNx, 224,
			XmNy, 148,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Product ID" ),
			NULL );
	UxPutContext( label46, (char *) UxPhotoOrderContext );


	/* Creation of label47 */
	label47 = XtVaCreateManagedWidget( "label47",
			xmLabelWidgetClass,
			form4,
			XmNx, 360,
			XmNy, 148,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			XmNwidth, 32,
			NULL );
	UxPutContext( label47, (char *) UxPhotoOrderContext );


	/* Creation of itemIdSW2 */
	itemIdSW2 = XtVaCreateManagedWidget( "itemIdSW2",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 172,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 406,
			XmNwidth, 40,
			NULL );
	UxPutContext( itemIdSW2, (char *) UxPhotoOrderContext );


	/* Creation of itemIdSL2 */
	itemIdSL2 = XtVaCreateManagedWidget( "itemIdSL2",
			xmListWidgetClass,
			itemIdSW2,
			XmNwidth, 40,
			XmNheight, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 172,
			XmNvisibleItemCount, 22,
			XmNlistSpacing, 1,
			NULL );
	XtAddCallback( itemIdSL2, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_jobLists_selectionCb,
		(XtPointer) 2 );

	UxPutContext( itemIdSL2, (char *) UxPhotoOrderContext );


	/* Creation of productIdSW2 */
	productIdSW2 = XtVaCreateManagedWidget( "productIdSW2",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 216,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 406,
			XmNwidth, 140,
			NULL );
	UxPutContext( productIdSW2, (char *) UxPhotoOrderContext );


	/* Creation of productIdSL2 */
	productIdSL2 = XtVaCreateManagedWidget( "productIdSL2",
			xmListWidgetClass,
			productIdSW2,
			XmNwidth, 140,
			XmNheight, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 172,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNvisibleItemCount, 22,
			XmNitemCount, 0,
			XmNlistSpacing, 1,
			NULL );
	XtAddCallback( productIdSL2, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_jobLists_selectionCb,
		(XtPointer) 3 );

	UxPutContext( productIdSL2, (char *) UxPhotoOrderContext );


	/* Creation of qtySW2 */
	qtySW2 = XtVaCreateManagedWidget( "qtySW2",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 360,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 406,
			XmNwidth, 40,
			NULL );
	UxPutContext( qtySW2, (char *) UxPhotoOrderContext );


	/* Creation of qtySL2 */
	qtySL2 = XtVaCreateManagedWidget( "qtySL2",
			xmListWidgetClass,
			qtySW2,
			XmNwidth, 40,
			XmNheight, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 172,
			XmNvisibleItemCount, 22,
			XmNlistSpacing, 1,
			NULL );
	XtAddCallback( qtySL2, XmNextendedSelectionCallback,
		(XtCallbackProc) photoOrder_jobLists_selectionCb,
		(XtPointer) 4 );

	UxPutContext( qtySL2, (char *) UxPhotoOrderContext );


	/* Creation of dummySW2 */
	dummySW2 = XtVaCreateManagedWidget( "dummySW2",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 400,
			XmNy, 172,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 400,
			NULL );
	UxPutContext( dummySW2, (char *) UxPhotoOrderContext );


	/* Creation of dummySL2 */
	dummySL2 = XtVaCreateManagedWidget( "dummySL2",
			xmListWidgetClass,
			dummySW2,
			XmNwidth, 2,
			XmNheight, 380,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNx, 0,
			XmNy, 172,
			XmNvisibleItemCount, 22,
			NULL );
	UxPutContext( dummySL2, (char *) UxPhotoOrderContext );


	/* Creation of createPB */
	createPB = XtVaCreateManagedWidget( "createPB",
			xmPushButtonWidgetClass,
			form4,
			XmNx, 28,
			XmNy, 588,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CREATE" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( createPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_createCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( createPB, (char *) UxPhotoOrderContext );


	/* Creation of separator6 */
	separator6 = XtVaCreateManagedWidget( "separator6",
			xmSeparatorWidgetClass,
			form4,
			XmNwidth, 440,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 132,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator6, (char *) UxPhotoOrderContext );


	/* Creation of cancelPB */
	cancelPB = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			form4,
			XmNx, 308,
			XmNy, 588,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CANCEL" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( cancelPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_cancelCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( cancelPB, (char *) UxPhotoOrderContext );


	/* Creation of deletePB */
	deletePB = XtVaCreateManagedWidget( "deletePB",
			xmPushButtonWidgetClass,
			form4,
			XmNx, 168,
			XmNy, 588,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "DELETE" ),
			XmNwidth, 108,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( deletePB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_deleteCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( deletePB, (char *) UxPhotoOrderContext );


	/* Creation of separator4 */
	separator4 = XtVaCreateManagedWidget( "separator4",
			xmSeparatorWidgetClass,
			photoOrder,
			XmNwidth, 1040,
			XmNheight, 4,
			XmNx, 0,
			XmNy, 724,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator4, (char *) UxPhotoOrderContext );


	/* Creation of menuBar2 */
	menuBar2 = XtVaCreateManagedWidget( "menuBar2",
			xmRowColumnWidgetClass,
			photoOrder,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 229,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNheight, 36,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( menuBar2, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_p2 */
	menuBar_p2_shell = XtVaCreatePopupShell ("menuBar_p2_shell",
			xmMenuShellWidgetClass, menuBar2,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p2 = XtVaCreateWidget( "menuBar_p2",
			xmRowColumnWidgetClass,
			menuBar_p2_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar_p2, (char *) UxPhotoOrderContext );


	/* Creation of welcomeMPB */
	welcomeMPB = XtVaCreateManagedWidget( "welcomeMPB",
			xmPushButtonWidgetClass,
			menuBar_p2,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( welcomeMPB, XmNactivateCallback,
		(XtCallbackProc) goto_welcomeScreen,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( welcomeMPB, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_p1_b1 */
	menuBar_p1_b1 = XtVaCreateManagedWidget( "menuBar_p1_b1",
			xmSeparatorWidgetClass,
			menuBar_p2,
			NULL );
	UxPutContext( menuBar_p1_b1, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_p2_b4 */
	menuBar_p2_b4 = XtVaCreateManagedWidget( "menuBar_p2_b4",
			xmPushButtonWidgetClass,
			menuBar_p2,
			RES_CONVERT( XmNlabelString, "Complete Photo Jobs Screen" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( menuBar_p2_b4, XmNactivateCallback,
		(XtCallbackProc) goto_photoJobScreen,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( menuBar_p2_b4, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_p2_b5 */
	menuBar_p2_b5 = XtVaCreateManagedWidget( "menuBar_p2_b5",
			xmSeparatorWidgetClass,
			menuBar_p2,
			NULL );
	UxPutContext( menuBar_p2_b5, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_p1_b18 */
	menuBar_p1_b18 = XtVaCreateManagedWidget( "menuBar_p1_b18",
			xmPushButtonWidgetClass,
			menuBar_p2,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( menuBar_p1_b18, XmNactivateCallback,
		(XtCallbackProc) photoOrder_closeCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( menuBar_p1_b18, (char *) UxPhotoOrderContext );


	/* Creation of menuBar_top_b2 */
	menuBar_top_b2 = XtVaCreateManagedWidget( "menuBar_top_b2",
			xmCascadeButtonWidgetClass,
			menuBar2,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, menuBar_p2,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 10,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar_top_b2, (char *) UxPhotoOrderContext );


	/* Creation of menuBar1_p1 */
	menuBar1_p1_shell = XtVaCreatePopupShell ("menuBar1_p1_shell",
			xmMenuShellWidgetClass, menuBar2,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p1 = XtVaCreateWidget( "menuBar1_p1",
			xmRowColumnWidgetClass,
			menuBar1_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p1, (char *) UxPhotoOrderContext );


	/* Creation of printMPB */
	printMPB = XtVaCreateManagedWidget( "printMPB",
			xmPushButtonWidgetClass,
			menuBar1_p1,
			RES_CONVERT( XmNlabelString, "Print  Screen" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( printMPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_printCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( printMPB, (char *) UxPhotoOrderContext );


	/* Creation of menuBar1_top_b3 */
	menuBar1_top_b3 = XtVaCreateManagedWidget( "menuBar1_top_b3",
			xmCascadeButtonGadgetClass,
			menuBar2,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, menuBar1_p1,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b3, (char *) UxPhotoOrderContext );


	/* Creation of menuBar1_p4 */
	menuBar1_p4_shell = XtVaCreatePopupShell ("menuBar1_p4_shell",
			xmMenuShellWidgetClass, menuBar2,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p4 = XtVaCreateWidget( "menuBar1_p4",
			xmRowColumnWidgetClass,
			menuBar1_p4_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p4, (char *) UxPhotoOrderContext );


	/* Creation of menuBar1_p3_b2 */
	menuBar1_p3_b2 = XtVaCreateManagedWidget( "menuBar1_p3_b2",
			xmPushButtonWidgetClass,
			menuBar1_p4,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( menuBar1_p3_b2, (char *) UxPhotoOrderContext );


	/* Creation of menuBar1_top_b4 */
	menuBar1_top_b4 = XtVaCreateManagedWidget( "menuBar1_top_b4",
			xmCascadeButtonWidgetClass,
			menuBar2,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, menuBar1_p4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( menuBar1_top_b4, (char *) UxPhotoOrderContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			photoOrder,
			XmNx, 64,
			XmNy, 740,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "PRINT    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_printCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( printPB, (char *) UxPhotoOrderContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			photoOrder,
			XmNx, 796,
			XmNy, 740,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			XmNheight, 36,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNwidth, 180,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_closeCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( closePB, (char *) UxPhotoOrderContext );


	/* Creation of addPB */
	addPB = XtVaCreateManagedWidget( "addPB",
			xmPushButtonWidgetClass,
			photoOrder,
			XmNx, 500,
			XmNy, 312,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNlabelType, XmPIXMAP,
			XmNshadowThickness, 4,
			XmNresizable, FALSE,
			XmNsensitive, FALSE,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, frame2,
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, frame3,
			XmNbottomAttachment, XmATTACH_NONE,
			XmNbottomOffset, 0,
			XmNrecomputeSize, FALSE,
			XmNheight, 81,
			XmNwidth, 81,
			NULL );
	XtAddCallback( addPB, XmNactivateCallback,
		(XtCallbackProc) photoOrder_addCb,
		(XtPointer) UxPhotoOrderContext );

	UxPutContext( addPB, (char *) UxPhotoOrderContext );

	XtVaSetValues(menuBar2,
			XmNmenuHelpWidget, menuBar1_top_b4,
			NULL );

	XtVaSetValues(addPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/arrow" ),
			RES_CONVERT( XmNlabelInsensitivePixmap, "/local/imsdads/app-defaults/pixmaps/hobbes.xbm" ),
			NULL );


	XtAddCallback( photoOrder, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPhotoOrderContext);


	return ( photoOrder );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_photoOrder( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCphotoOrder          *UxContext;
	static int		_Uxinit = 0;

	UxPhotoOrderContext = UxContext =
		(_UxCphotoOrder *) UxNewContext( sizeof(_UxCphotoOrder), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_photoOrder();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

