
/*******************************************************************************
	ims_opBilling.c

       Associated Header file: ims_opBilling.h
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
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
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
#include "ims_opBilling.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_billing()
{
	Widget		_UxParent;


	/* Creation of billing */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "billing_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 91,
			XmNy, 40,
			XmNwidth, 784,
			XmNheight, 815,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "billing",
			NULL );

	}

	billing = XtVaCreateManagedWidget( "billing",
			xmFormWidgetClass,
			_UxParent,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 784,
			XmNheight, 815,
			NULL );
	UxPutContext( billing, (char *) UxBillingContext );
	UxPutClassCode( billing, _UxIfClassId );


	/* Creation of label94 */
	label94 = XtVaCreateManagedWidget( "label94",
			xmLabelWidgetClass,
			billing,
			XmNx, 288,
			XmNy, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Billing             Screen" ),
			XmNheight, 36,
			XmNwidth, 264,
			NULL );
	UxPutContext( label94, (char *) UxBillingContext );


	/* Creation of frame11 */
	frame11 = XtVaCreateManagedWidget( "frame11",
			xmFrameWidgetClass,
			billing,
			XmNwidth, 744,
			XmNheight, 728,
			XmNx, 22,
			XmNy, 72,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNshadowThickness, 5,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame11, (char *) UxBillingContext );


	/* Creation of form12 */
	form12 = XtVaCreateManagedWidget( "form12",
			xmFormWidgetClass,
			frame11,
			XmNwidth, 327,
			XmNheight, 612,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, -8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form12, (char *) UxBillingContext );


	/* Creation of separator31 */
	separator31 = XtVaCreateManagedWidget( "separator31",
			xmSeparatorWidgetClass,
			form12,
			XmNwidth, 736,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 656,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator31, (char *) UxBillingContext );


	/* Creation of okPB */
	okPB = XtVaCreateManagedWidget( "okPB",
			xmPushButtonWidgetClass,
			form12,
			XmNx, 56,
			XmNy, 672,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( okPB, XmNactivateCallback,
		(XtCallbackProc) billing_printFormCb,
		(XtPointer) 1 );

	UxPutContext( okPB, (char *) UxBillingContext );


	/* Creation of cancelPB */
	cancelPB = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			form12,
			XmNx, 544,
			XmNy, 672,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( cancelPB, XmNactivateCallback,
		(XtCallbackProc) billing_cancelFormCb,
		(XtPointer) UxBillingContext );

	UxPutContext( cancelPB, (char *) UxBillingContext );


	/* Creation of label102 */
	label102 = XtVaCreateManagedWidget( "label102",
			xmLabelWidgetClass,
			form12,
			XmNx, 22,
			XmNy, 18,
			RES_CONVERT( XmNbackground, "SlateGray4" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, " Bill To: " ),
			XmNheight, 24,
			XmNwidth, 90,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "white" ),
			NULL );
	UxPutContext( label102, (char *) UxBillingContext );


	/* Creation of label103 */
	label103 = XtVaCreateManagedWidget( "label103",
			xmLabelWidgetClass,
			form12,
			XmNx, 28,
			XmNy, 258,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date" ),
			XmNheight, 24,
			XmNwidth, 95,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label103, (char *) UxBillingContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 144,
			XmNx, 176,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderDateTF, (char *) UxBillingContext );


	/* Creation of label105 */
	label105 = XtVaCreateManagedWidget( "label105",
			xmLabelWidgetClass,
			form12,
			XmNx, 10,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			XmNheight, 20,
			XmNwidth, 48,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label105, (char *) UxBillingContext );


	/* Creation of label119 */
	label119 = XtVaCreateManagedWidget( "label119",
			xmLabelWidgetClass,
			form12,
			XmNx, 238,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Name" ),
			XmNheight, 20,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label119, (char *) UxBillingContext );


	/* Creation of label120 */
	label120 = XtVaCreateManagedWidget( "label120",
			xmLabelWidgetClass,
			form12,
			XmNx, 400,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label120, (char *) UxBillingContext );


	/* Creation of label121 */
	label121 = XtVaCreateManagedWidget( "label121",
			xmLabelWidgetClass,
			form12,
			XmNx, 606,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			XmNheight, 20,
			XmNwidth, 32,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label121, (char *) UxBillingContext );


	/* Creation of label122 */
	label122 = XtVaCreateManagedWidget( "label122",
			xmLabelWidgetClass,
			form12,
			XmNx, 654,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cost" ),
			XmNheight, 20,
			XmNwidth, 40,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label122, (char *) UxBillingContext );


	/* Creation of label106 */
	label106 = XtVaCreateManagedWidget( "label106",
			xmLabelWidgetClass,
			form12,
			XmNx, 28,
			XmNy, 187,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID" ),
			XmNheight, 24,
			XmNwidth, 96,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label106, (char *) UxBillingContext );


	/* Creation of label123 */
	label123 = XtVaCreateManagedWidget( "label123",
			xmLabelWidgetClass,
			form12,
			XmNx, 28,
			XmNy, 150,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice ID" ),
			XmNheight, 24,
			XmNwidth, 92,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label123, (char *) UxBillingContext );


	/* Creation of label124 */
	label124 = XtVaCreateManagedWidget( "label124",
			xmLabelWidgetClass,
			form12,
			XmNx, 408,
			XmNy, 150,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice Amount" ),
			XmNheight, 24,
			XmNwidth, 144,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label124, (char *) UxBillingContext );


	/* Creation of label125 */
	label125 = XtVaCreateManagedWidget( "label125",
			xmLabelWidgetClass,
			form12,
			XmNx, 408,
			XmNy, 187,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Resource Type" ),
			XmNheight, 24,
			XmNwidth, 128,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label125, (char *) UxBillingContext );


	/* Creation of label126 */
	label126 = XtVaCreateManagedWidget( "label126",
			xmLabelWidgetClass,
			form12,
			XmNx, 408,
			XmNy, 258,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice Date" ),
			XmNheight, 24,
			XmNwidth, 112,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label126, (char *) UxBillingContext );


	/* Creation of label104 */
	label104 = XtVaCreateManagedWidget( "label104",
			xmLabelWidgetClass,
			form12,
			XmNx, 408,
			XmNy, 223,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 24,
			XmNwidth, 84,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label104, (char *) UxBillingContext );


	/* Creation of label131 */
	label131 = XtVaCreateManagedWidget( "label131",
			xmLabelWidgetClass,
			form12,
			XmNx, 62,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Description" ),
			XmNheight, 20,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label131, (char *) UxBillingContext );


	/* Creation of commentPB */
	commentPB = XtVaCreateManagedWidget( "commentPB",
			xmPushButtonWidgetClass,
			form12,
			XmNx, 300,
			XmNy, 672,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Edit Comments" ),
			XmNwidth, 144,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( commentPB, XmNactivateCallback,
		(XtCallbackProc) billing_editCommentsCb,
		(XtPointer) UxBillingContext );

	UxPutContext( commentPB, (char *) UxBillingContext );


	/* Creation of billItemSW */
	billItemSW = XtVaCreateManagedWidget( "billItemSW",
			xmScrolledWindowWidgetClass,
			form12,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 8,
			XmNy, 360,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 580,
			XmNheight, 290,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 10,
			NULL );
	UxPutContext( billItemSW, (char *) UxBillingContext );


	/* Creation of billItemSL */
	billItemSL = XtVaCreateManagedWidget( "billItemSL",
			xmListWidgetClass,
			billItemSW,
			XmNwidth, 520,
			XmNheight, 290,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNitemCount, 0,
			XmNlistSizePolicy, XmVARIABLE,
			NULL );
	UxPutContext( billItemSL, (char *) UxBillingContext );


	/* Creation of invoiceIdTF */
	invoiceIdTF = XtVaCreateManagedWidget( "invoiceIdTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 176,
			XmNy, 147,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( invoiceIdTF, (char *) UxBillingContext );


	/* Creation of accountIdTF */
	accountIdTF = XtVaCreateManagedWidget( "accountIdTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 176,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( accountIdTF, (char *) UxBillingContext );


	/* Creation of amountTF */
	amountTF = XtVaCreateManagedWidget( "amountTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 556,
			XmNy, 147,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( amountTF, (char *) UxBillingContext );


	/* Creation of orderIdTF */
	orderIdTF = XtVaCreateManagedWidget( "orderIdTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 556,
			XmNy, 220,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderIdTF, (char *) UxBillingContext );


	/* Creation of balanceTF */
	balanceTF = XtVaCreateManagedWidget( "balanceTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 176,
			XmNy, 220,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( balanceTF, (char *) UxBillingContext );


	/* Creation of billDateTF */
	billDateTF = XtVaCreateManagedWidget( "billDateTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 556,
			XmNy, 255,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( billDateTF, (char *) UxBillingContext );


	/* Creation of billToSW */
	billToSW = XtVaCreateManagedWidget( "billToSW",
			xmScrolledWindowWidgetClass,
			form12,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 176,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 520,
			XmNheight, 115,
			NULL );
	UxPutContext( billToSW, (char *) UxBillingContext );


	/* Creation of billToST */
	billToST = XtVaCreateManagedWidget( "billToST",
			xmTextWidgetClass,
			billToSW,
			XmNwidth, 424,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollHorizontal, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			XmNheight, 120,
			NULL );
	UxPutContext( billToST, (char *) UxBillingContext );


	/* Creation of label203 */
	label203 = XtVaCreateManagedWidget( "label203",
			xmLabelWidgetClass,
			form12,
			XmNx, 524,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label203, (char *) UxBillingContext );


	/* Creation of label204 */
	label204 = XtVaCreateManagedWidget( "label204",
			xmLabelWidgetClass,
			form12,
			XmNx, 28,
			XmNy, 223,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account Balance" ),
			XmNheight, 24,
			XmNwidth, 140,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label204, (char *) UxBillingContext );


	/* Creation of resourceTF */
	resourceTF = XtVaCreateManagedWidget( "resourceTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 145,
			XmNx, 556,
			XmNy, 184,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( resourceTF, (char *) UxBillingContext );


	/* Creation of label205 */
	label205 = XtVaCreateManagedWidget( "label205",
			xmLabelWidgetClass,
			form12,
			XmNx, 28,
			XmNy, 292,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name" ),
			XmNheight, 24,
			XmNwidth, 95,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label205, (char *) UxBillingContext );


	/* Creation of userNameTF */
	userNameTF = XtVaCreateManagedWidget( "userNameTF",
			xmTextFieldWidgetClass,
			form12,
			XmNwidth, 525,
			XmNx, 176,
			XmNy, 292,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( userNameTF, (char *) UxBillingContext );


	/* Creation of label100 */
	label100 = XtVaCreateManagedWidget( "label100",
			xmLabelWidgetClass,
			billing,
			XmNx, 88,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNlabelType, XmPIXMAP,
			XmNwidth, 56,
			XmNheight, 52,
			NULL );
	UxPutContext( label100, (char *) UxBillingContext );

	XtVaSetValues(label100,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/calc.xpm" ),
			NULL );


	XtAddCallback( billing, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxBillingContext);


	return ( billing );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_billing( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCbilling             *UxContext;
	static int		_Uxinit = 0;

	UxBillingContext = UxContext =
		(_UxCbilling *) UxNewContext( sizeof(_UxCbilling), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_billing();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

