
/*******************************************************************************
	ims_opShipping.c

       Associated Header file: ims_opShipping.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
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
#include "ims_opShipping.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_shipping()
{
	Widget		_UxParent;


	/* Creation of shipping */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "shipping_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 415,
			XmNy, 33,
			XmNwidth, 784,
			XmNheight, 810,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "shipping",
			NULL );

	}

	shipping = XtVaCreateManagedWidget( "shipping",
			xmFormWidgetClass,
			_UxParent,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 810,
			XmNwidth, 784,
			XmNdefaultPosition, TRUE,
			NULL );
	UxPutContext( shipping, (char *) UxShippingContext );
	UxPutClassCode( shipping, _UxIfClassId );


	/* Creation of frame10 */
	frame10 = XtVaCreateManagedWidget( "frame10",
			xmFrameWidgetClass,
			shipping,
			XmNwidth, 743,
			XmNheight, 728,
			XmNx, 19,
			XmNy, 62,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNshadowThickness, 5,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame10, (char *) UxShippingContext );


	/* Creation of form11 */
	form11 = XtVaCreateManagedWidget( "form11",
			xmFormWidgetClass,
			frame10,
			XmNwidth, 557,
			XmNheight, 671,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 5,
			XmNy, 5,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form11, (char *) UxShippingContext );


	/* Creation of label108 */
	label108 = XtVaCreateManagedWidget( "label108",
			xmLabelWidgetClass,
			form11,
			XmNx, 36,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "SlateGray4" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, " Ship To: " ),
			XmNheight, 24,
			XmNwidth, 94,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "white" ),
			NULL );
	UxPutContext( label108, (char *) UxShippingContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			form11,
			XmNx, 44,
			XmNy, 668,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) shipping_printFormCb,
		(XtPointer) 1 );

	UxPutContext( printPB, (char *) UxShippingContext );


	/* Creation of cancelPB */
	cancelPB = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			form11,
			XmNx, 548,
			XmNy, 668,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( cancelPB, XmNactivateCallback,
		(XtCallbackProc) shipping_cancelFormCb,
		(XtPointer) UxShippingContext );

	UxPutContext( cancelPB, (char *) UxShippingContext );


	/* Creation of separator */
	separator = XtVaCreateManagedWidget( "separator",
			xmSeparatorWidgetClass,
			form11,
			XmNwidth, 736,
			XmNheight, 10,
			XmNx, 0,
			XmNy, 648,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator, (char *) UxShippingContext );


	/* Creation of label109 */
	label109 = XtVaCreateManagedWidget( "label109",
			xmLabelWidgetClass,
			form11,
			XmNx, 12,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			XmNheight, 20,
			XmNwidth, 48,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label109, (char *) UxShippingContext );


	/* Creation of label110 */
	label110 = XtVaCreateManagedWidget( "label110",
			xmLabelWidgetClass,
			form11,
			XmNx, 428,
			XmNy, 157,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 24,
			XmNwidth, 84,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label110, (char *) UxShippingContext );


	/* Creation of orderIdTF */
	orderIdTF = XtVaCreateManagedWidget( "orderIdTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 155,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderIdTF, (char *) UxShippingContext );


	/* Creation of label111 */
	label111 = XtVaCreateManagedWidget( "label111",
			xmLabelWidgetClass,
			form11,
			XmNx, 240,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Name" ),
			XmNheight, 20,
			XmNwidth, 56,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label111, (char *) UxShippingContext );


	/* Creation of label112 */
	label112 = XtVaCreateManagedWidget( "label112",
			xmLabelWidgetClass,
			form11,
			XmNx, 404,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label112, (char *) UxShippingContext );


	/* Creation of label113 */
	label113 = XtVaCreateManagedWidget( "label113",
			xmLabelWidgetClass,
			form11,
			XmNx, 606,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			XmNheight, 20,
			XmNwidth, 32,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label113, (char *) UxShippingContext );


	/* Creation of label114 */
	label114 = XtVaCreateManagedWidget( "label114",
			xmLabelWidgetClass,
			form11,
			XmNx, 648,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cost" ),
			XmNheight, 20,
			XmNwidth, 40,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label114, (char *) UxShippingContext );


	/* Creation of label115 */
	label115 = XtVaCreateManagedWidget( "label115",
			xmLabelWidgetClass,
			form11,
			XmNx, 40,
			XmNy, 196,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID" ),
			XmNheight, 24,
			XmNwidth, 96,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label115, (char *) UxShippingContext );


	/* Creation of shippingIdTF */
	shippingIdTF = XtVaCreateManagedWidget( "shippingIdTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 156,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( shippingIdTF, (char *) UxShippingContext );


	/* Creation of accountIdTF */
	accountIdTF = XtVaCreateManagedWidget( "accountIdTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 193,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( accountIdTF, (char *) UxShippingContext );


	/* Creation of carrierTF */
	carrierTF = XtVaCreateManagedWidget( "carrierTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 192,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNheight, 30,
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( carrierTF, (char *) UxShippingContext );


	/* Creation of label127 */
	label127 = XtVaCreateManagedWidget( "label127",
			xmLabelWidgetClass,
			form11,
			XmNx, 428,
			XmNy, 230,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date" ),
			XmNheight, 24,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label127, (char *) UxShippingContext );


	/* Creation of shipDateTF */
	shipDateTF = XtVaCreateManagedWidget( "shipDateTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 228,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( shipDateTF, (char *) UxShippingContext );


	/* Creation of totalQtyTF */
	totalQtyTF = XtVaCreateManagedWidget( "totalQtyTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 266,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( totalQtyTF, (char *) UxShippingContext );


	/* Creation of label128 */
	label128 = XtVaCreateManagedWidget( "label128",
			xmLabelWidgetClass,
			form11,
			XmNx, 428,
			XmNy, 195,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship Via" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label128, (char *) UxShippingContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 227,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderDateTF, (char *) UxShippingContext );


	/* Creation of totalCostTF */
	totalCostTF = XtVaCreateManagedWidget( "totalCostTF",
			xmTextFieldWidgetClass,
			form11,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 265,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( totalCostTF, (char *) UxShippingContext );


	/* Creation of shipToSW */
	shipToSW = XtVaCreateManagedWidget( "shipToSW",
			xmScrolledWindowWidgetClass,
			form11,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 168,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 520,
			XmNheight, 115,
			NULL );
	UxPutContext( shipToSW, (char *) UxShippingContext );


	/* Creation of shipToST */
	shipToST = XtVaCreateManagedWidget( "shipToST",
			xmTextWidgetClass,
			shipToSW,
			XmNwidth, 424,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollHorizontal, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNheight, 120,
			NULL );
	UxPutContext( shipToST, (char *) UxShippingContext );


	/* Creation of label129 */
	label129 = XtVaCreateManagedWidget( "label129",
			xmLabelWidgetClass,
			form11,
			XmNx, 40,
			XmNy, 231,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship Date" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label129, (char *) UxShippingContext );


	/* Creation of label130 */
	label130 = XtVaCreateManagedWidget( "label130",
			xmLabelWidgetClass,
			form11,
			XmNx, 40,
			XmNy, 160,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Shipping ID" ),
			XmNheight, 24,
			XmNwidth, 104,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label130, (char *) UxShippingContext );


	/* Creation of label132 */
	label132 = XtVaCreateManagedWidget( "label132",
			xmLabelWidgetClass,
			form11,
			XmNx, 64,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Description" ),
			XmNheight, 20,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label132, (char *) UxShippingContext );


	/* Creation of shipItemSW */
	shipItemSW = XtVaCreateManagedWidget( "shipItemSW",
			xmScrolledWindowWidgetClass,
			form11,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 12,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 580,
			XmNheight, 300,
			XmNresizable, FALSE,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 8,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 10,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 80,
			NULL );
	UxPutContext( shipItemSW, (char *) UxShippingContext );


	/* Creation of shipItemSL */
	shipItemSL = XtVaCreateManagedWidget( "shipItemSL",
			xmListWidgetClass,
			shipItemSW,
			XmNwidth, 520,
			XmNheight, 290,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNitemCount, 0,
			XmNlistSizePolicy, XmVARIABLE,
			NULL );
	UxPutContext( shipItemSL, (char *) UxShippingContext );


	/* Creation of editCommentPB */
	editCommentPB = XtVaCreateManagedWidget( "editCommentPB",
			xmPushButtonWidgetClass,
			form11,
			XmNx, 292,
			XmNy, 668,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Edit Comments" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( editCommentPB, XmNactivateCallback,
		(XtCallbackProc) shipping_editCommentsCb,
		(XtPointer) UxShippingContext );

	UxPutContext( editCommentPB, (char *) UxShippingContext );


	/* Creation of label116 */
	label116 = XtVaCreateManagedWidget( "label116",
			xmLabelWidgetClass,
			form11,
			XmNx, 40,
			XmNy, 269,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Qty" ),
			XmNheight, 24,
			XmNwidth, 83,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label116, (char *) UxShippingContext );


	/* Creation of label117 */
	label117 = XtVaCreateManagedWidget( "label117",
			xmLabelWidgetClass,
			form11,
			XmNx, 428,
			XmNy, 268,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Cost" ),
			XmNheight, 24,
			XmNwidth, 89,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label117, (char *) UxShippingContext );


	/* Creation of label209 */
	label209 = XtVaCreateManagedWidget( "label209",
			xmLabelWidgetClass,
			form11,
			XmNx, 520,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label209, (char *) UxShippingContext );


	/* Creation of label107 */
	label107 = XtVaCreateManagedWidget( "label107",
			xmLabelWidgetClass,
			shipping,
			XmNx, 308,
			XmNy, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Shipping            Screen" ),
			XmNheight, 28,
			XmNwidth, 224,
			NULL );
	UxPutContext( label107, (char *) UxShippingContext );


	/* Creation of label118 */
	label118 = XtVaCreateManagedWidget( "label118",
			xmLabelWidgetClass,
			shipping,
			XmNx, 20,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNlabelType, XmPIXMAP,
			XmNheight, 48,
			XmNwidth, 52,
			NULL );
	UxPutContext( label118, (char *) UxShippingContext );

	XtVaSetValues(label118,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/post.xpm" ),
			NULL );


	XtAddCallback( shipping, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxShippingContext);


	return ( shipping );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_shipping( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCshipping            *UxContext;
	static int		_Uxinit = 0;

	UxShippingContext = UxContext =
		(_UxCshipping *) UxNewContext( sizeof(_UxCshipping), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_shipping();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

