
/*******************************************************************************
	ims_opBillView.c

       Associated Header file: ims_opBillView.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
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
#include "ims_opBillView.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_billView()
{
	Widget		_UxParent;


	/* Creation of billView */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "billView_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 828,
			XmNy, 16,
			XmNwidth, 784,
			XmNheight, 800,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "billView",
			NULL );

	}

	billView = XtVaCreateManagedWidget( "billView",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 784,
			XmNheight, 800,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( billView, (char *) UxBillViewContext );
	UxPutClassCode( billView, _UxIfClassId );


	/* Creation of frame13 */
	frame13 = XtVaCreateManagedWidget( "frame13",
			xmFrameWidgetClass,
			billView,
			XmNwidth, 748,
			XmNheight, 560,
			XmNx, 16,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame13, (char *) UxBillViewContext );


	/* Creation of form15 */
	form15 = XtVaCreateManagedWidget( "form15",
			xmFormWidgetClass,
			frame13,
			XmNwidth, 578,
			XmNheight, 580,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form15, (char *) UxBillViewContext );


	/* Creation of label135 */
	label135 = XtVaCreateManagedWidget( "label135",
			xmLabelWidgetClass,
			form15,
			XmNx, 32,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "SlateGray4" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, " Bill To: " ),
			XmNheight, 24,
			XmNwidth, 90,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "white" ),
			NULL );
	UxPutContext( label135, (char *) UxBillViewContext );


	/* Creation of billItemSW */
	billItemSW = XtVaCreateManagedWidget( "billItemSW",
			xmScrolledWindowWidgetClass,
			form15,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 8,
			XmNy, 340,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 580,
			XmNheight, 200,
			XmNleftOffset, 10,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( billItemSW, (char *) UxBillViewContext );


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
	UxPutContext( billItemSL, (char *) UxBillViewContext );


	/* Creation of billToSW */
	billToSW = XtVaCreateManagedWidget( "billToSW",
			xmScrolledWindowWidgetClass,
			form15,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 183,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 522,
			XmNheight, 100,
			NULL );
	UxPutContext( billToSW, (char *) UxBillViewContext );


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
	UxPutContext( billToST, (char *) UxBillViewContext );


	/* Creation of label141 */
	label141 = XtVaCreateManagedWidget( "label141",
			xmLabelWidgetClass,
			form15,
			XmNx, 28,
			XmNy, 239,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date" ),
			XmNheight, 24,
			XmNwidth, 95,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label141, (char *) UxBillViewContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 180,
			XmNy, 236,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderDateTF, (char *) UxBillViewContext );


	/* Creation of label143 */
	label143 = XtVaCreateManagedWidget( "label143",
			xmLabelWidgetClass,
			form15,
			XmNx, 28,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID" ),
			XmNheight, 24,
			XmNwidth, 96,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label143, (char *) UxBillViewContext );


	/* Creation of label144 */
	label144 = XtVaCreateManagedWidget( "label144",
			xmLabelWidgetClass,
			form15,
			XmNx, 28,
			XmNy, 144,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice ID" ),
			XmNheight, 24,
			XmNwidth, 92,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label144, (char *) UxBillViewContext );


	/* Creation of label158 */
	label158 = XtVaCreateManagedWidget( "label158",
			xmLabelWidgetClass,
			form15,
			XmNx, 416,
			XmNy, 143,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice Amount" ),
			XmNheight, 24,
			XmNwidth, 144,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label158, (char *) UxBillViewContext );


	/* Creation of label159 */
	label159 = XtVaCreateManagedWidget( "label159",
			xmLabelWidgetClass,
			form15,
			XmNx, 416,
			XmNy, 175,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Resource Type" ),
			XmNheight, 24,
			XmNwidth, 128,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label159, (char *) UxBillViewContext );


	/* Creation of label165 */
	label165 = XtVaCreateManagedWidget( "label165",
			xmLabelWidgetClass,
			form15,
			XmNx, 416,
			XmNy, 238,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice Date" ),
			XmNheight, 24,
			XmNwidth, 112,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label165, (char *) UxBillViewContext );


	/* Creation of label166 */
	label166 = XtVaCreateManagedWidget( "label166",
			xmLabelWidgetClass,
			form15,
			XmNx, 416,
			XmNy, 207,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 24,
			XmNwidth, 84,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label166, (char *) UxBillViewContext );


	/* Creation of invoiceIdTF */
	invoiceIdTF = XtVaCreateManagedWidget( "invoiceIdTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 180,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( invoiceIdTF, (char *) UxBillViewContext );


	/* Creation of accountIdTF */
	accountIdTF = XtVaCreateManagedWidget( "accountIdTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 180,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( accountIdTF, (char *) UxBillViewContext );


	/* Creation of amountTF */
	amountTF = XtVaCreateManagedWidget( "amountTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 564,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( amountTF, (char *) UxBillViewContext );


	/* Creation of orderIdTF */
	orderIdTF = XtVaCreateManagedWidget( "orderIdTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 564,
			XmNy, 204,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderIdTF, (char *) UxBillViewContext );


	/* Creation of balanceTF */
	balanceTF = XtVaCreateManagedWidget( "balanceTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 180,
			XmNy, 204,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( balanceTF, (char *) UxBillViewContext );


	/* Creation of billDateTF */
	billDateTF = XtVaCreateManagedWidget( "billDateTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 564,
			XmNy, 235,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( billDateTF, (char *) UxBillViewContext );


	/* Creation of label207 */
	label207 = XtVaCreateManagedWidget( "label207",
			xmLabelWidgetClass,
			form15,
			XmNx, 28,
			XmNy, 208,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account Balance" ),
			XmNheight, 24,
			XmNwidth, 140,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label207, (char *) UxBillViewContext );


	/* Creation of resourceTF */
	resourceTF = XtVaCreateManagedWidget( "resourceTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 145,
			XmNx, 564,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( resourceTF, (char *) UxBillViewContext );


	/* Creation of label208 */
	label208 = XtVaCreateManagedWidget( "label208",
			xmLabelWidgetClass,
			form15,
			XmNx, 28,
			XmNy, 271,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name" ),
			XmNheight, 24,
			XmNwidth, 95,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label208, (char *) UxBillViewContext );


	/* Creation of userNameTF */
	userNameTF = XtVaCreateManagedWidget( "userNameTF",
			xmTextFieldWidgetClass,
			form15,
			XmNwidth, 531,
			XmNx, 180,
			XmNy, 268,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( userNameTF, (char *) UxBillViewContext );


	/* Creation of label172 */
	label172 = XtVaCreateManagedWidget( "label172",
			xmLabelWidgetClass,
			form15,
			XmNx, 10,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			XmNheight, 20,
			XmNwidth, 48,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label172, (char *) UxBillViewContext );


	/* Creation of label173 */
	label173 = XtVaCreateManagedWidget( "label173",
			xmLabelWidgetClass,
			form15,
			XmNx, 62,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Description" ),
			XmNheight, 20,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label173, (char *) UxBillViewContext );


	/* Creation of label179 */
	label179 = XtVaCreateManagedWidget( "label179",
			xmLabelWidgetClass,
			form15,
			XmNx, 240,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Name" ),
			XmNheight, 20,
			XmNwidth, 98,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label179, (char *) UxBillViewContext );


	/* Creation of label187 */
	label187 = XtVaCreateManagedWidget( "label187",
			xmLabelWidgetClass,
			form15,
			XmNx, 408,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNheight, 20,
			XmNwidth, 50,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label187, (char *) UxBillViewContext );


	/* Creation of label197 */
	label197 = XtVaCreateManagedWidget( "label197",
			xmLabelWidgetClass,
			form15,
			XmNx, 528,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label197, (char *) UxBillViewContext );


	/* Creation of label198 */
	label198 = XtVaCreateManagedWidget( "label198",
			xmLabelWidgetClass,
			form15,
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
	UxPutContext( label198, (char *) UxBillViewContext );


	/* Creation of label202 */
	label202 = XtVaCreateManagedWidget( "label202",
			xmLabelWidgetClass,
			form15,
			XmNx, 654,
			XmNy, 316,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cost" ),
			XmNheight, 20,
			XmNwidth, 40,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label202, (char *) UxBillViewContext );


	/* Creation of label180 */
	label180 = XtVaCreateManagedWidget( "label180",
			xmLabelWidgetClass,
			billView,
			XmNx, 208,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Invoice ID" ),
			XmNheight, 24,
			XmNwidth, 104,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label180, (char *) UxBillViewContext );


	/* Creation of label186 */
	label186 = XtVaCreateManagedWidget( "label186",
			xmLabelWidgetClass,
			billView,
			XmNx, 424,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Bill Date" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label186, (char *) UxBillViewContext );


	/* Creation of viewPB */
	viewPB = XtVaCreateManagedWidget( "viewPB",
			xmPushButtonWidgetClass,
			billView,
			XmNx, 60,
			XmNy, 76,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNlabelType, XmPIXMAP,
			XmNshadowThickness, 4,
			XmNsensitive, TRUE,
			XmNrecomputeSize, FALSE,
			XmNheight, 81,
			XmNwidth, 81,
			XmNhighlightThickness, 2,
			NULL );
	XtAddCallback( viewPB, XmNactivateCallback,
		(XtCallbackProc) billView_viewCb,
		(XtPointer) UxBillViewContext );

	UxPutContext( viewPB, (char *) UxBillViewContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			billView,
			XmNx, 644,
			XmNy, 76,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNlabelType, XmPIXMAP,
			XmNshadowThickness, 4,
			XmNsensitive, FALSE,
			XmNrecomputeSize, FALSE,
			XmNheight, 81,
			XmNwidth, 81,
			XmNhighlightThickness, 2,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 0,
			XmNmarginWidth, 0,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) billing_printFormCb,
		(XtPointer) 0 );

	UxPutContext( printPB, (char *) UxBillViewContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			billView,
			XmNx, 308,
			XmNy, 752,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE" ),
			XmNwidth, 158,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) billView_closeCb,
		(XtPointer) UxBillViewContext );

	UxPutContext( closePB, (char *) UxBillViewContext );


	/* Creation of billViewLB */
	billViewLB = XtVaCreateManagedWidget( "billViewLB",
			xmLabelWidgetClass,
			billView,
			XmNx, 232,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 28,
			XmNwidth, 376,
			XmNleftOffset, 100,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 100,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( billViewLB, (char *) UxBillViewContext );


	/* Creation of separator33 */
	separator33 = XtVaCreateManagedWidget( "separator33",
			xmSeparatorWidgetClass,
			billView,
			XmNwidth, 788,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 736,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator33, (char *) UxBillViewContext );


	/* Creation of billIdSW */
	billIdSW = XtVaCreateManagedWidget( "billIdSW",
			xmScrolledWindowWidgetClass,
			billView,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 204,
			XmNy, 76,
			XmNwidth, 390,
			XmNheight, 85,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( billIdSW, (char *) UxBillViewContext );


	/* Creation of billIdSL */
	billIdSL = XtVaCreateManagedWidget( "billIdSL",
			xmListWidgetClass,
			billIdSW,
			XmNwidth, 363,
			XmNheight, 200,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			NULL );
	XtAddCallback( billIdSL, XmNdefaultActionCallback,
		(XtCallbackProc) billView_viewCb,
		(XtPointer) UxBillViewContext );
	XtAddCallback( billIdSL, XmNbrowseSelectionCallback,
		(XtCallbackProc) billView_browseSelectCb,
		(XtPointer) UxBillViewContext );

	UxPutContext( billIdSL, (char *) UxBillViewContext );

	XtVaSetValues(viewPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/arrowD" ),
			NULL );

	XtVaSetValues(printPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/printD" ),
			RES_CONVERT( XmNlabelInsensitivePixmap, "/local/imsdads/app-defaults/pixmaps/printer.xpm" ),
			NULL );


	XtAddCallback( billView, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxBillViewContext);


	return ( billView );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_billView( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCbillView            *UxContext;
	static int		_Uxinit = 0;

	UxBillViewContext = UxContext =
		(_UxCbillView *) UxNewContext( sizeof(_UxCbillView), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_billView();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

