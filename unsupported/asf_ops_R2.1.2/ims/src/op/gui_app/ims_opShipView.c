
/*******************************************************************************
	ims_opShipView.c

       Associated Header file: ims_opShipView.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
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
#include "ims_opShipView.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_shipView()
{
	Widget		_UxParent;


	/* Creation of shipView */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "shipView_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 436,
			XmNy, 72,
			XmNwidth, 784,
			XmNheight, 800,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "shipView",
			NULL );

	}

	shipView = XtVaCreateManagedWidget( "shipView",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 784,
			XmNheight, 800,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( shipView, (char *) UxShipViewContext );
	UxPutClassCode( shipView, _UxIfClassId );


	/* Creation of shipViewLB */
	shipViewLB = XtVaCreateManagedWidget( "shipViewLB",
			xmLabelWidgetClass,
			shipView,
			XmNx, 184,
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
	UxPutContext( shipViewLB, (char *) UxShipViewContext );


	/* Creation of shipIdSW */
	shipIdSW = XtVaCreateManagedWidget( "shipIdSW",
			xmScrolledWindowWidgetClass,
			shipView,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 192,
			XmNy, 76,
			XmNwidth, 390,
			XmNheight, 85,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNresizable, FALSE,
			NULL );
	UxPutContext( shipIdSW, (char *) UxShipViewContext );


	/* Creation of shipIdSL */
	shipIdSL = XtVaCreateManagedWidget( "shipIdSL",
			xmListWidgetClass,
			shipIdSW,
			XmNwidth, 363,
			XmNheight, 200,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			NULL );
	XtAddCallback( shipIdSL, XmNdefaultActionCallback,
		(XtCallbackProc) shipView_viewCb,
		(XtPointer) UxShipViewContext );
	XtAddCallback( shipIdSL, XmNbrowseSelectionCallback,
		(XtCallbackProc) shipView_browseSelectCb,
		(XtPointer) UxShipViewContext );

	UxPutContext( shipIdSL, (char *) UxShipViewContext );


	/* Creation of frame12 */
	frame12 = XtVaCreateManagedWidget( "frame12",
			xmFrameWidgetClass,
			shipView,
			XmNwidth, 748,
			XmNheight, 548,
			XmNx, 16,
			XmNy, 172,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNshadowThickness, 4,
			XmNshadowType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( frame12, (char *) UxShipViewContext );


	/* Creation of form13 */
	form13 = XtVaCreateManagedWidget( "form13",
			xmFormWidgetClass,
			frame12,
			XmNwidth, 578,
			XmNheight, 580,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNnoResize, TRUE,
			NULL );
	UxPutContext( form13, (char *) UxShipViewContext );


	/* Creation of label133 */
	label133 = XtVaCreateManagedWidget( "label133",
			xmLabelWidgetClass,
			form13,
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
	UxPutContext( label133, (char *) UxShipViewContext );


	/* Creation of label134 */
	label134 = XtVaCreateManagedWidget( "label134",
			xmLabelWidgetClass,
			form13,
			XmNx, 16,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			XmNheight, 20,
			XmNwidth, 48,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label134, (char *) UxShipViewContext );


	/* Creation of label136 */
	label136 = XtVaCreateManagedWidget( "label136",
			xmLabelWidgetClass,
			form13,
			XmNx, 424,
			XmNy, 143,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			XmNheight, 24,
			XmNwidth, 84,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label136, (char *) UxShipViewContext );


	/* Creation of orderIdTF */
	orderIdTF = XtVaCreateManagedWidget( "orderIdTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderIdTF, (char *) UxShipViewContext );


	/* Creation of label137 */
	label137 = XtVaCreateManagedWidget( "label137",
			xmLabelWidgetClass,
			form13,
			XmNx, 244,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Name" ),
			XmNheight, 20,
			XmNwidth, 56,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label137, (char *) UxShipViewContext );


	/* Creation of label138 */
	label138 = XtVaCreateManagedWidget( "label138",
			xmLabelWidgetClass,
			form13,
			XmNx, 408,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label138, (char *) UxShipViewContext );


	/* Creation of label139 */
	label139 = XtVaCreateManagedWidget( "label139",
			xmLabelWidgetClass,
			form13,
			XmNx, 606,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qty" ),
			XmNheight, 20,
			XmNwidth, 32,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label139, (char *) UxShipViewContext );


	/* Creation of label145 */
	label145 = XtVaCreateManagedWidget( "label145",
			xmLabelWidgetClass,
			form13,
			XmNx, 648,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cost" ),
			XmNheight, 20,
			XmNwidth, 40,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label145, (char *) UxShipViewContext );


	/* Creation of label146 */
	label146 = XtVaCreateManagedWidget( "label146",
			xmLabelWidgetClass,
			form13,
			XmNx, 40,
			XmNy, 179,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID" ),
			XmNheight, 24,
			XmNwidth, 96,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label146, (char *) UxShipViewContext );


	/* Creation of accountIdTF */
	accountIdTF = XtVaCreateManagedWidget( "accountIdTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( accountIdTF, (char *) UxShipViewContext );


	/* Creation of carrierTF */
	carrierTF = XtVaCreateManagedWidget( "carrierTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNheight, 30,
			XmNmarginHeight, 3,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( carrierTF, (char *) UxShipViewContext );


	/* Creation of label147 */
	label147 = XtVaCreateManagedWidget( "label147",
			xmLabelWidgetClass,
			form13,
			XmNx, 424,
			XmNy, 215,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Date" ),
			XmNheight, 24,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label147, (char *) UxShipViewContext );


	/* Creation of totalQtyTF */
	totalQtyTF = XtVaCreateManagedWidget( "totalQtyTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 248,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( totalQtyTF, (char *) UxShipViewContext );


	/* Creation of label188 */
	label188 = XtVaCreateManagedWidget( "label188",
			xmLabelWidgetClass,
			form13,
			XmNx, 424,
			XmNy, 179,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship Via" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label188, (char *) UxShipViewContext );


	/* Creation of orderDateTF */
	orderDateTF = XtVaCreateManagedWidget( "orderDateTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 212,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( orderDateTF, (char *) UxShipViewContext );


	/* Creation of totalCostTF */
	totalCostTF = XtVaCreateManagedWidget( "totalCostTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 548,
			XmNy, 248,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( totalCostTF, (char *) UxShipViewContext );


	/* Creation of label199 */
	label199 = XtVaCreateManagedWidget( "label199",
			xmLabelWidgetClass,
			form13,
			XmNx, 68,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Description" ),
			XmNheight, 20,
			XmNwidth, 100,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label199, (char *) UxShipViewContext );


	/* Creation of shipItemSW */
	shipItemSW = XtVaCreateManagedWidget( "shipItemSW",
			xmScrolledWindowWidgetClass,
			form13,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 16,
			XmNy, 324,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 580,
			XmNheight, 200,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 16,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 16,
			XmNbottomAttachment, XmATTACH_NONE,
			XmNbottomOffset, 0,
			XmNresizable, FALSE,
			NULL );
	UxPutContext( shipItemSW, (char *) UxShipViewContext );


	/* Creation of shipItemSL */
	shipItemSL = XtVaCreateManagedWidget( "shipItemSL",
			xmListWidgetClass,
			shipItemSW,
			XmNwidth, 553,
			XmNheight, 290,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNitemCount, 0,
			XmNlistSizePolicy, XmVARIABLE,
			NULL );
	UxPutContext( shipItemSL, (char *) UxShipViewContext );


	/* Creation of label200 */
	label200 = XtVaCreateManagedWidget( "label200",
			xmLabelWidgetClass,
			form13,
			XmNx, 40,
			XmNy, 251,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Qty" ),
			XmNheight, 24,
			XmNwidth, 83,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label200, (char *) UxShipViewContext );


	/* Creation of label201 */
	label201 = XtVaCreateManagedWidget( "label201",
			xmLabelWidgetClass,
			form13,
			XmNx, 424,
			XmNy, 251,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Cost" ),
			XmNheight, 24,
			XmNwidth, 89,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label201, (char *) UxShipViewContext );


	/* Creation of label140 */
	label140 = XtVaCreateManagedWidget( "label140",
			xmLabelWidgetClass,
			form13,
			XmNx, 40,
			XmNy, 143,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Shipping ID" ),
			XmNheight, 24,
			XmNwidth, 104,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label140, (char *) UxShipViewContext );


	/* Creation of shippingIdTF */
	shippingIdTF = XtVaCreateManagedWidget( "shippingIdTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( shippingIdTF, (char *) UxShipViewContext );


	/* Creation of label142 */
	label142 = XtVaCreateManagedWidget( "label142",
			xmLabelWidgetClass,
			form13,
			XmNx, 40,
			XmNy, 215,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship Date" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label142, (char *) UxShipViewContext );


	/* Creation of shipDateTF */
	shipDateTF = XtVaCreateManagedWidget( "shipDateTF",
			xmTextFieldWidgetClass,
			form13,
			XmNwidth, 145,
			XmNx, 168,
			XmNy, 212,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			XmNvalue, "",
			XmNmarginHeight, 3,
			XmNheight, 30,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( shipDateTF, (char *) UxShipViewContext );


	/* Creation of shipToSW */
	shipToSW = XtVaCreateManagedWidget( "shipToSW",
			xmScrolledWindowWidgetClass,
			form13,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 168,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 520,
			XmNheight, 100,
			NULL );
	UxPutContext( shipToSW, (char *) UxShipViewContext );


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
	UxPutContext( shipToST, (char *) UxShipViewContext );


	/* Creation of label210 */
	label210 = XtVaCreateManagedWidget( "label210",
			xmLabelWidgetClass,
			form13,
			XmNx, 528,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNheight, 20,
			XmNwidth, 52,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label210, (char *) UxShipViewContext );


	/* Creation of label148 */
	label148 = XtVaCreateManagedWidget( "label148",
			xmLabelWidgetClass,
			shipView,
			XmNx, 200,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Shipping ID" ),
			XmNheight, 24,
			XmNwidth, 104,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label148, (char *) UxShipViewContext );


	/* Creation of label149 */
	label149 = XtVaCreateManagedWidget( "label149",
			xmLabelWidgetClass,
			shipView,
			XmNx, 420,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship Date" ),
			XmNheight, 24,
			XmNwidth, 85,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNforeground, "black" ),
			NULL );
	UxPutContext( label149, (char *) UxShipViewContext );


	/* Creation of separator29 */
	separator29 = XtVaCreateManagedWidget( "separator29",
			xmSeparatorWidgetClass,
			shipView,
			XmNwidth, 788,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 732,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( separator29, (char *) UxShipViewContext );


	/* Creation of viewPB */
	viewPB = XtVaCreateManagedWidget( "viewPB",
			xmPushButtonWidgetClass,
			shipView,
			XmNx, 56,
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
		(XtCallbackProc) shipView_viewCb,
		(XtPointer) UxShipViewContext );

	UxPutContext( viewPB, (char *) UxShipViewContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			shipView,
			XmNx, 628,
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
			XmNmarginWidth, 0,
			XmNmarginHeight, 0,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) shipping_printFormCb,
		(XtPointer) 0 );

	UxPutContext( printPB, (char *) UxShipViewContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			shipView,
			XmNx, 316,
			XmNy, 748,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE" ),
			XmNwidth, 158,
			XmNshadowThickness, 4,
			XmNheight, 36,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) shipView_closeCb,
		(XtPointer) UxShipViewContext );

	UxPutContext( closePB, (char *) UxShipViewContext );

	XtVaSetValues(viewPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/arrowD" ),
			NULL );

	XtVaSetValues(printPB,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/printD" ),
			RES_CONVERT( XmNlabelInsensitivePixmap, "/local/imsdads/app-defaults/pixmaps/printer.xpm" ),
			NULL );


	XtAddCallback( shipView, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxShipViewContext);


	return ( shipView );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_shipView( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCshipView            *UxContext;
	static int		_Uxinit = 0;

	UxShipViewContext = UxContext =
		(_UxCshipView *) UxNewContext( sizeof(_UxCshipView), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_shipView();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

