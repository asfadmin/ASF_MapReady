
/*******************************************************************************
	ims_opOrder.c

       Associated Header file: ims_opOrder.h
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
#include "ims_opOrder.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_order()
{
	Widget		_UxParent;
	Widget		orderMB_p1_shell;
	Widget		orderMB_p3_shell;
	Widget		orderMB_p4_shell;
	Widget		orderMB_p6_shell;
	Widget		orderMB_p7_shell;
	Widget		orderMB_p5_shell;
	Widget		orderMB_p8_shell;
	Widget		orderMB_p9_shell;
	Widget		orderMB_p10_shell;


	/* Creation of order */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "order_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 12,
			XmNy, 2,
			XmNwidth, 1060,
			XmNheight, 790,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "order",
			NULL );

	}

	order = XtVaCreateManagedWidget( "order",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1060,
			XmNheight, 790,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNdialogTitle, "Order Production Screen" ),
			NULL );
	UxPutContext( order, (char *) UxOrderContext );
	UxPutClassCode( order, _UxIfClassId );


	/* Creation of orderMB */
	orderMB = XtVaCreateManagedWidget( "orderMB",
			xmRowColumnWidgetClass,
			order,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 0,
			XmNy, 0,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNmenuAccelerator, "<KeyUp>F10",
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( orderMB, (char *) UxOrderContext );


	/* Creation of orderMB_p1 */
	orderMB_p1_shell = XtVaCreatePopupShell ("orderMB_p1_shell",
			xmMenuShellWidgetClass, orderMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p1 = XtVaCreateWidget( "orderMB_p1",
			xmRowColumnWidgetClass,
			orderMB_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p1, (char *) UxOrderContext );


	/* Creation of orderMB_p1_b1 */
	orderMB_p1_b1 = XtVaCreateManagedWidget( "orderMB_p1_b1",
			xmPushButtonWidgetClass,
			orderMB_p1,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p1_b1, XmNactivateCallback,
		(XtCallbackProc) order_goto_welcomeCb,
		(XtPointer) UxOrderContext );

	UxPutContext( orderMB_p1_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p1_b8 */
	orderMB_p1_b8 = XtVaCreateManagedWidget( "orderMB_p1_b8",
			xmSeparatorWidgetClass,
			orderMB_p1,
			NULL );
	UxPutContext( orderMB_p1_b8, (char *) UxOrderContext );


	/* Creation of searchPB */
	searchPB = XtVaCreateManagedWidget( "searchPB",
			xmPushButtonWidgetClass,
			orderMB_p1,
			RES_CONVERT( XmNlabelString, "Order Search Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( searchPB, XmNactivateCallback,
		(XtCallbackProc) order_goto_searchCb,
		(XtPointer) UxOrderContext );

	UxPutContext( searchPB, (char *) UxOrderContext );


	/* Creation of orderMB_p1_b9 */
	orderMB_p1_b9 = XtVaCreateManagedWidget( "orderMB_p1_b9",
			xmSeparatorWidgetClass,
			orderMB_p1,
			NULL );
	UxPutContext( orderMB_p1_b9, (char *) UxOrderContext );


	/* Creation of orderMB_p1_b6 */
	orderMB_p1_b6 = XtVaCreateManagedWidget( "orderMB_p1_b6",
			xmPushButtonWidgetClass,
			orderMB_p1,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p1_b6, XmNactivateCallback,
		(XtCallbackProc) order_closeCb,
		(XtPointer) UxOrderContext );

	UxPutContext( orderMB_p1_b6, (char *) UxOrderContext );


	/* Creation of orderMB_p1_top_b1 */
	orderMB_p1_top_b1 = XtVaCreateManagedWidget( "orderMB_p1_top_b1",
			xmCascadeButtonWidgetClass,
			orderMB,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, orderMB_p1,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNmarginWidth, 12,
			RES_CONVERT( XmNmnemonic, "G" ),
			NULL );
	UxPutContext( orderMB_p1_top_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p3 */
	orderMB_p3_shell = XtVaCreatePopupShell ("orderMB_p3_shell",
			xmMenuShellWidgetClass, orderMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p3 = XtVaCreateWidget( "orderMB_p3",
			xmRowColumnWidgetClass,
			orderMB_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p3, (char *) UxOrderContext );


	/* Creation of orderMB_p3_b1 */
	orderMB_p3_b1 = XtVaCreateManagedWidget( "orderMB_p3_b1",
			xmPushButtonWidgetClass,
			orderMB_p3,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			NULL );
	UxPutContext( orderMB_p3_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p3_top_b1 */
	orderMB_p3_top_b1 = XtVaCreateManagedWidget( "orderMB_p3_top_b1",
			xmCascadeButtonWidgetClass,
			orderMB,
			RES_CONVERT( XmNlabelString, "Help" ),
			XmNsubMenuId, orderMB_p3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( orderMB_p3_top_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p4 */
	orderMB_p4_shell = XtVaCreatePopupShell ("orderMB_p4_shell",
			xmMenuShellWidgetClass, orderMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p4 = XtVaCreateWidget( "orderMB_p4",
			xmRowColumnWidgetClass,
			orderMB_p4_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p4, (char *) UxOrderContext );


	/* Creation of viewOrderDetailsMPB */
	viewOrderDetailsMPB = XtVaCreateManagedWidget( "viewOrderDetailsMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "View Order Details" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( viewOrderDetailsMPB, XmNactivateCallback,
		(XtCallbackProc) order_browse_orderDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( viewOrderDetailsMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b4 */
	orderMB_p4_b4 = XtVaCreateManagedWidget( "orderMB_p4_b4",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b4, (char *) UxOrderContext );


	/* Creation of validateOrderMPB */
	validateOrderMPB = XtVaCreateManagedWidget( "validateOrderMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Validate Order" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( validateOrderMPB, XmNactivateCallback,
		(XtCallbackProc) order_validate_orderCb,
		(XtPointer) 0 );

	UxPutContext( validateOrderMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b13 */
	orderMB_p4_b13 = XtVaCreateManagedWidget( "orderMB_p4_b13",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b13, (char *) UxOrderContext );


	/* Creation of unvalidateOrderMPB */
	unvalidateOrderMPB = XtVaCreateManagedWidget( "unvalidateOrderMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Unvalidate Order" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( unvalidateOrderMPB, XmNactivateCallback,
		(XtCallbackProc) order_validate_orderCb,
		(XtPointer) 1 );

	UxPutContext( unvalidateOrderMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b14 */
	orderMB_p4_b14 = XtVaCreateManagedWidget( "orderMB_p4_b14",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b14, (char *) UxOrderContext );


	/* Creation of updateOrderPriorityMPB */
	updateOrderPriorityMPB = XtVaCreateManagedWidget( "updateOrderPriorityMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Update Order Priority" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateOrderPriorityMPB, XmNactivateCallback,
		(XtCallbackProc) order_update_orderPriorityCb,
		(XtPointer) UxOrderContext );

	UxPutContext( updateOrderPriorityMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b12 */
	orderMB_p4_b12 = XtVaCreateManagedWidget( "orderMB_p4_b12",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b12, (char *) UxOrderContext );


	/* Creation of updateOrderStatusMPB */
	updateOrderStatusMPB = XtVaCreateManagedWidget( "updateOrderStatusMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Update Order Status" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateOrderStatusMPB, XmNactivateCallback,
		(XtCallbackProc) order_update_orderStatusCb,
		(XtPointer) UxOrderContext );

	UxPutContext( updateOrderStatusMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b6 */
	orderMB_p4_b6 = XtVaCreateManagedWidget( "orderMB_p4_b6",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b6, (char *) UxOrderContext );


	/* Creation of editOrderCommentMPB */
	editOrderCommentMPB = XtVaCreateManagedWidget( "editOrderCommentMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Edit Order Comments" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( editOrderCommentMPB, XmNactivateCallback,
		(XtCallbackProc) order_edit_orderCommentCb,
		(XtPointer) UxOrderContext );

	UxPutContext( editOrderCommentMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b11 */
	orderMB_p4_b11 = XtVaCreateManagedWidget( "orderMB_p4_b11",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b11, (char *) UxOrderContext );


	/* Creation of processMediaMPB */
	processMediaMPB = XtVaCreateManagedWidget( "processMediaMPB",
			xmPushButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Process Media" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( processMediaMPB, XmNactivateCallback,
		(XtCallbackProc) order_processMediaCb,
		(XtPointer) 1 );

	UxPutContext( processMediaMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b15 */
	orderMB_p4_b15 = XtVaCreateManagedWidget( "orderMB_p4_b15",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b15, (char *) UxOrderContext );


	/* Creation of orderMB_p6 */
	orderMB_p6_shell = XtVaCreatePopupShell ("orderMB_p6_shell",
			xmMenuShellWidgetClass, orderMB_p4,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p6 = XtVaCreateWidget( "orderMB_p6",
			xmRowColumnWidgetClass,
			orderMB_p6_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p6, (char *) UxOrderContext );


	/* Creation of orderMB_p6_b1 */
	orderMB_p6_b1 = XtVaCreateManagedWidget( "orderMB_p6_b1",
			xmPushButtonWidgetClass,
			orderMB_p6,
			RES_CONVERT( XmNlabelString, "Create New Shipment" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p6_b1, XmNactivateCallback,
		(XtCallbackProc) order_create_shipmentCb,
		(XtPointer) 1 );

	UxPutContext( orderMB_p6_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p6_b2 */
	orderMB_p6_b2 = XtVaCreateManagedWidget( "orderMB_p6_b2",
			xmSeparatorWidgetClass,
			orderMB_p6,
			NULL );
	UxPutContext( orderMB_p6_b2, (char *) UxOrderContext );


	/* Creation of orderMB_p6_b3 */
	orderMB_p6_b3 = XtVaCreateManagedWidget( "orderMB_p6_b3",
			xmPushButtonWidgetClass,
			orderMB_p6,
			RES_CONVERT( XmNlabelString, "View Shipping Reports" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p6_b3, XmNactivateCallback,
		(XtCallbackProc) order_view_shippingReportsCb,
		(XtPointer) 1 );

	UxPutContext( orderMB_p6_b3, (char *) UxOrderContext );


	/* Creation of shipOrderMPB */
	shipOrderMPB = XtVaCreateManagedWidget( "shipOrderMPB",
			xmCascadeButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Ship Order" ),
			XmNsubMenuId, orderMB_p6,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( shipOrderMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p4_b17 */
	orderMB_p4_b17 = XtVaCreateManagedWidget( "orderMB_p4_b17",
			xmSeparatorWidgetClass,
			orderMB_p4,
			NULL );
	UxPutContext( orderMB_p4_b17, (char *) UxOrderContext );


	/* Creation of orderMB_p7 */
	orderMB_p7_shell = XtVaCreatePopupShell ("orderMB_p7_shell",
			xmMenuShellWidgetClass, orderMB_p4,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p7 = XtVaCreateWidget( "orderMB_p7",
			xmRowColumnWidgetClass,
			orderMB_p7_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p7, (char *) UxOrderContext );


	/* Creation of orderMB_p7_b1 */
	orderMB_p7_b1 = XtVaCreateManagedWidget( "orderMB_p7_b1",
			xmPushButtonWidgetClass,
			orderMB_p7,
			RES_CONVERT( XmNlabelString, "Create New Invoice" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p7_b1, XmNactivateCallback,
		(XtCallbackProc) order_create_invoiceCb,
		(XtPointer) 1 );

	UxPutContext( orderMB_p7_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p7_b2 */
	orderMB_p7_b2 = XtVaCreateManagedWidget( "orderMB_p7_b2",
			xmSeparatorWidgetClass,
			orderMB_p7,
			NULL );
	UxPutContext( orderMB_p7_b2, (char *) UxOrderContext );


	/* Creation of orderMB_p7_b3 */
	orderMB_p7_b3 = XtVaCreateManagedWidget( "orderMB_p7_b3",
			xmPushButtonWidgetClass,
			orderMB_p7,
			RES_CONVERT( XmNlabelString, "View Existing Invoices" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p7_b3, XmNactivateCallback,
		(XtCallbackProc) order_view_invoiceCb,
		(XtPointer) 1 );

	UxPutContext( orderMB_p7_b3, (char *) UxOrderContext );


	/* Creation of billOrderMPB */
	billOrderMPB = XtVaCreateManagedWidget( "billOrderMPB",
			xmCascadeButtonWidgetClass,
			orderMB_p4,
			RES_CONVERT( XmNlabelString, "Bill Order" ),
			XmNsubMenuId, orderMB_p7,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( billOrderMPB, (char *) UxOrderContext );


	/* Creation of orderMB_top_b1 */
	orderMB_top_b1 = XtVaCreateManagedWidget( "orderMB_top_b1",
			xmCascadeButtonGadgetClass,
			orderMB,
			RES_CONVERT( XmNlabelString, "Order Functions" ),
			XmNsubMenuId, orderMB_p4,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "O" ),
			XmNmarginWidth, 20,
			NULL );
	UxPutContext( orderMB_top_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p5 */
	orderMB_p5_shell = XtVaCreatePopupShell ("orderMB_p5_shell",
			xmMenuShellWidgetClass, orderMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p5 = XtVaCreateWidget( "orderMB_p5",
			xmRowColumnWidgetClass,
			orderMB_p5_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p5, (char *) UxOrderContext );


	/* Creation of viewItemDetailsMPB */
	viewItemDetailsMPB = XtVaCreateManagedWidget( "viewItemDetailsMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "View Item Details" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( viewItemDetailsMPB, XmNactivateCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( viewItemDetailsMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b3 */
	orderMB_p5_b3 = XtVaCreateManagedWidget( "orderMB_p5_b3",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b3, (char *) UxOrderContext );


	/* Creation of validateItemMPB */
	validateItemMPB = XtVaCreateManagedWidget( "validateItemMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Validate Item" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( validateItemMPB, XmNactivateCallback,
		(XtCallbackProc) order_validate_itemCb,
		(XtPointer) 0 );

	UxPutContext( validateItemMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b8 */
	orderMB_p5_b8 = XtVaCreateManagedWidget( "orderMB_p5_b8",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b8, (char *) UxOrderContext );


	/* Creation of unvalidateItemMPB */
	unvalidateItemMPB = XtVaCreateManagedWidget( "unvalidateItemMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Unvalidate Item" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( unvalidateItemMPB, XmNactivateCallback,
		(XtCallbackProc) order_validate_itemCb,
		(XtPointer) 1 );

	UxPutContext( unvalidateItemMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b13 */
	orderMB_p5_b13 = XtVaCreateManagedWidget( "orderMB_p5_b13",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b13, (char *) UxOrderContext );


	/* Creation of updateItemPriorityMPB */
	updateItemPriorityMPB = XtVaCreateManagedWidget( "updateItemPriorityMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Update Item Priority" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateItemPriorityMPB, XmNactivateCallback,
		(XtCallbackProc) order_update_itemPriorityCb,
		(XtPointer) UxOrderContext );

	UxPutContext( updateItemPriorityMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b11 */
	orderMB_p5_b11 = XtVaCreateManagedWidget( "orderMB_p5_b11",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b11, (char *) UxOrderContext );


	/* Creation of updateItemStatusMPB */
	updateItemStatusMPB = XtVaCreateManagedWidget( "updateItemStatusMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Update Item Status" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( updateItemStatusMPB, XmNactivateCallback,
		(XtCallbackProc) order_update_itemStatusCb,
		(XtPointer) UxOrderContext );

	UxPutContext( updateItemStatusMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b5 */
	orderMB_p5_b5 = XtVaCreateManagedWidget( "orderMB_p5_b5",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b5, (char *) UxOrderContext );


	/* Creation of editItemCommentMPB */
	editItemCommentMPB = XtVaCreateManagedWidget( "editItemCommentMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Edit Item Comments" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( editItemCommentMPB, XmNactivateCallback,
		(XtCallbackProc) order_edit_itemCommentCb,
		(XtPointer) UxOrderContext );

	UxPutContext( editItemCommentMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b9 */
	orderMB_p5_b9 = XtVaCreateManagedWidget( "orderMB_p5_b9",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b9, (char *) UxOrderContext );


	/* Creation of itemProcessMediaMPB */
	itemProcessMediaMPB = XtVaCreateManagedWidget( "itemProcessMediaMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Process Media" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( itemProcessMediaMPB, XmNactivateCallback,
		(XtCallbackProc) order_processMediaCb,
		(XtPointer) 2 );

	UxPutContext( itemProcessMediaMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b15 */
	orderMB_p5_b15 = XtVaCreateManagedWidget( "orderMB_p5_b15",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b15, (char *) UxOrderContext );


	/* Creation of orderMB_p8 */
	orderMB_p8_shell = XtVaCreatePopupShell ("orderMB_p8_shell",
			xmMenuShellWidgetClass, orderMB_p5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p8 = XtVaCreateWidget( "orderMB_p8",
			xmRowColumnWidgetClass,
			orderMB_p8_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p8, (char *) UxOrderContext );


	/* Creation of orderMB_p8_b1 */
	orderMB_p8_b1 = XtVaCreateManagedWidget( "orderMB_p8_b1",
			xmPushButtonWidgetClass,
			orderMB_p8,
			RES_CONVERT( XmNlabelString, "Create New Shipment" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p8_b1, XmNactivateCallback,
		(XtCallbackProc) order_create_shipmentCb,
		(XtPointer) 2 );

	UxPutContext( orderMB_p8_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p8_b2 */
	orderMB_p8_b2 = XtVaCreateManagedWidget( "orderMB_p8_b2",
			xmSeparatorWidgetClass,
			orderMB_p8,
			NULL );
	UxPutContext( orderMB_p8_b2, (char *) UxOrderContext );


	/* Creation of orderMB_p8_b3 */
	orderMB_p8_b3 = XtVaCreateManagedWidget( "orderMB_p8_b3",
			xmPushButtonWidgetClass,
			orderMB_p8,
			RES_CONVERT( XmNlabelString, "View Shipping Reports" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p8_b3, XmNactivateCallback,
		(XtCallbackProc) order_view_shippingReportsCb,
		(XtPointer) 2 );

	UxPutContext( orderMB_p8_b3, (char *) UxOrderContext );


	/* Creation of itemShipItemsMPB */
	itemShipItemsMPB = XtVaCreateManagedWidget( "itemShipItemsMPB",
			xmCascadeButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Ship Items" ),
			XmNsubMenuId, orderMB_p8,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( itemShipItemsMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b17 */
	orderMB_p5_b17 = XtVaCreateManagedWidget( "orderMB_p5_b17",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b17, (char *) UxOrderContext );


	/* Creation of orderMB_p9 */
	orderMB_p9_shell = XtVaCreatePopupShell ("orderMB_p9_shell",
			xmMenuShellWidgetClass, orderMB_p5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p9 = XtVaCreateWidget( "orderMB_p9",
			xmRowColumnWidgetClass,
			orderMB_p9_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( orderMB_p9, (char *) UxOrderContext );


	/* Creation of orderMB_p9_b1 */
	orderMB_p9_b1 = XtVaCreateManagedWidget( "orderMB_p9_b1",
			xmPushButtonWidgetClass,
			orderMB_p9,
			RES_CONVERT( XmNlabelString, "Create New Invoice" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p9_b1, XmNactivateCallback,
		(XtCallbackProc) order_create_invoiceCb,
		(XtPointer) 2 );

	UxPutContext( orderMB_p9_b1, (char *) UxOrderContext );


	/* Creation of orderMB_p9_b2 */
	orderMB_p9_b2 = XtVaCreateManagedWidget( "orderMB_p9_b2",
			xmSeparatorWidgetClass,
			orderMB_p9,
			NULL );
	UxPutContext( orderMB_p9_b2, (char *) UxOrderContext );


	/* Creation of orderMB_p9_b3 */
	orderMB_p9_b3 = XtVaCreateManagedWidget( "orderMB_p9_b3",
			xmPushButtonWidgetClass,
			orderMB_p9,
			RES_CONVERT( XmNlabelString, "View Existing Invoices" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( orderMB_p9_b3, XmNactivateCallback,
		(XtCallbackProc) order_view_invoiceCb,
		(XtPointer) 2 );

	UxPutContext( orderMB_p9_b3, (char *) UxOrderContext );


	/* Creation of itemBillItemsMPB */
	itemBillItemsMPB = XtVaCreateManagedWidget( "itemBillItemsMPB",
			xmCascadeButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Bill Items" ),
			XmNsubMenuId, orderMB_p9,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( itemBillItemsMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p5_b19 */
	orderMB_p5_b19 = XtVaCreateManagedWidget( "orderMB_p5_b19",
			xmSeparatorWidgetClass,
			orderMB_p5,
			NULL );
	UxPutContext( orderMB_p5_b19, (char *) UxOrderContext );


	/* Creation of restartItemMPB */
	restartItemMPB = XtVaCreateManagedWidget( "restartItemMPB",
			xmPushButtonWidgetClass,
			orderMB_p5,
			RES_CONVERT( XmNlabelString, "Restart Item" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( restartItemMPB, XmNactivateCallback,
		(XtCallbackProc) order_restart_itemCb,
		(XtPointer) UxOrderContext );

	UxPutContext( restartItemMPB, (char *) UxOrderContext );


	/* Creation of orderMB_top_b4 */
	orderMB_top_b4 = XtVaCreateManagedWidget( "orderMB_top_b4",
			xmCascadeButtonGadgetClass,
			orderMB,
			RES_CONVERT( XmNlabelString, "Item Functions" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			XmNsubMenuId, orderMB_p5,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			NULL );
	UxPutContext( orderMB_top_b4, (char *) UxOrderContext );


	/* Creation of orderMB_p10 */
	orderMB_p10_shell = XtVaCreatePopupShell ("orderMB_p10_shell",
			xmMenuShellWidgetClass, orderMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	orderMB_p10 = XtVaCreateWidget( "orderMB_p10",
			xmRowColumnWidgetClass,
			orderMB_p10_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNwidth, 114,
			NULL );
	UxPutContext( orderMB_p10, (char *) UxOrderContext );


	/* Creation of saveResultsMPB */
	saveResultsMPB = XtVaCreateManagedWidget( "saveResultsMPB",
			xmPushButtonWidgetClass,
			orderMB_p10,
			RES_CONVERT( XmNlabelString, "Save Results" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNmarginWidth, 5,
			NULL );
	XtAddCallback( saveResultsMPB, XmNactivateCallback,
		(XtCallbackProc) order_save_resultsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( saveResultsMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p10_b2 */
	orderMB_p10_b2 = XtVaCreateManagedWidget( "orderMB_p10_b2",
			xmSeparatorWidgetClass,
			orderMB_p10,
			NULL );
	UxPutContext( orderMB_p10_b2, (char *) UxOrderContext );


	/* Creation of refreshSearchMPB */
	refreshSearchMPB = XtVaCreateManagedWidget( "refreshSearchMPB",
			xmPushButtonWidgetClass,
			orderMB_p10,
			RES_CONVERT( XmNlabelString, "Refresh Search" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( refreshSearchMPB, XmNactivateCallback,
		(XtCallbackProc) order_refreshCb,
		(XtPointer) UxOrderContext );

	UxPutContext( refreshSearchMPB, (char *) UxOrderContext );


	/* Creation of orderMB_p10_b4 */
	orderMB_p10_b4 = XtVaCreateManagedWidget( "orderMB_p10_b4",
			xmSeparatorWidgetClass,
			orderMB_p10,
			NULL );
	UxPutContext( orderMB_p10_b4, (char *) UxOrderContext );


	/* Creation of printScreenMPB */
	printScreenMPB = XtVaCreateManagedWidget( "printScreenMPB",
			xmPushButtonWidgetClass,
			orderMB_p10,
			RES_CONVERT( XmNlabelString, "Print Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( printScreenMPB, XmNactivateCallback,
		(XtCallbackProc) order_printScreenCb,
		(XtPointer) UxOrderContext );

	UxPutContext( printScreenMPB, (char *) UxOrderContext );


	/* Creation of orderMB_top_b2 */
	orderMB_top_b2 = XtVaCreateManagedWidget( "orderMB_top_b2",
			xmCascadeButtonGadgetClass,
			orderMB,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			XmNsubMenuId, orderMB_p10,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 155,
			XmNx, 400,
			XmNmarginLeft, 0,
			XmNmarginWidth, 20,
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	UxPutContext( orderMB_top_b2, (char *) UxOrderContext );


	/* Creation of separator2 */
	separator2 = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass,
			order,
			XmNwidth, 800,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 472,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 3,
			XmNseparatorType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( separator2, (char *) UxOrderContext );


	/* Creation of separator3 */
	separator3 = XtVaCreateManagedWidget( "separator3",
			xmSeparatorWidgetClass,
			order,
			XmNwidth, 1024,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 732,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 3,
			XmNseparatorType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( separator3, (char *) UxOrderContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			order,
			XmNx, 836,
			XmNy, 748,
			XmNwidth, 186,
			XmNheight, 32,
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) order_closeCb,
		(XtPointer) UxOrderContext );

	UxPutContext( closePB, (char *) UxOrderContext );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			order,
			XmNx, 16,
			XmNy, 500,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Details For Order ID:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label1, (char *) UxOrderContext );


	/* Creation of orderIdTF */
	orderIdTF = XtVaCreateManagedWidget( "orderIdTF",
			xmTextFieldWidgetClass,
			order,
			XmNwidth, 110,
			XmNx, 240,
			XmNy, 492,
			XmNheight, 36,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	UxPutContext( orderIdTF, (char *) UxOrderContext );


	/* Creation of label3 */
	label3 = XtVaCreateManagedWidget( "label3",
			xmLabelWidgetClass,
			order,
			XmNx, 910,
			XmNy, 543,
			XmNwidth, 44,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Vald" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label3, (char *) UxOrderContext );


	/* Creation of label4 */
	label4 = XtVaCreateManagedWidget( "label4",
			xmLabelWidgetClass,
			order,
			XmNx, 127,
			XmNy, 543,
			XmNwidth, 100,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Name" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label4, (char *) UxOrderContext );


	/* Creation of label6 */
	label6 = XtVaCreateManagedWidget( "label6",
			xmLabelWidgetClass,
			order,
			XmNx, 480,
			XmNy, 543,
			XmNwidth, 66,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Priority" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label6, (char *) UxOrderContext );


	/* Creation of label7 */
	label7 = XtVaCreateManagedWidget( "label7",
			xmLabelWidgetClass,
			order,
			XmNx, 622,
			XmNy, 543,
			XmNwidth, 66,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label7, (char *) UxOrderContext );


	/* Creation of label13 */
	label13 = XtVaCreateManagedWidget( "label13",
			xmLabelWidgetClass,
			order,
			XmNx, 16,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label13, (char *) UxOrderContext );


	/* Creation of label15 */
	label15 = XtVaCreateManagedWidget( "label15",
			xmLabelWidgetClass,
			order,
			XmNx, 729,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label15, (char *) UxOrderContext );


	/* Creation of label17 */
	label17 = XtVaCreateManagedWidget( "label17",
			xmLabelWidgetClass,
			order,
			XmNx, 586,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label17, (char *) UxOrderContext );


	/* Creation of label18 */
	label18 = XtVaCreateManagedWidget( "label18",
			xmLabelWidgetClass,
			order,
			XmNx, 118,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label18, (char *) UxOrderContext );


	/* Creation of label26 */
	label26 = XtVaCreateManagedWidget( "label26",
			xmLabelWidgetClass,
			order,
			XmNx, 280,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Time Received" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label26, (char *) UxOrderContext );


	/* Creation of label36 */
	label36 = XtVaCreateManagedWidget( "label36",
			xmLabelWidgetClass,
			order,
			XmNx, 7,
			XmNy, 543,
			XmNwidth, 48,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label36, (char *) UxOrderContext );


	/* Creation of label38 */
	label38 = XtVaCreateManagedWidget( "label38",
			xmLabelWidgetClass,
			order,
			XmNx, 954,
			XmNy, 543,
			XmNwidth, 44,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Ship" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label38, (char *) UxOrderContext );


	/* Creation of label41 */
	label41 = XtVaCreateManagedWidget( "label41",
			xmLabelWidgetClass,
			order,
			XmNx, 994,
			XmNy, 543,
			XmNwidth, 44,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Bill" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label41, (char *) UxOrderContext );


	/* Creation of label27 */
	label27 = XtVaCreateManagedWidget( "label27",
			xmLabelWidgetClass,
			order,
			XmNx, 16,
			XmNy, 240,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Number of Orders:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label27, (char *) UxOrderContext );


	/* Creation of totalOrdersTF */
	totalOrdersTF = XtVaCreateManagedWidget( "totalOrdersTF",
			xmTextFieldWidgetClass,
			order,
			XmNwidth, 110,
			XmNx, 240,
			XmNy, 232,
			XmNheight, 36,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	UxPutContext( totalOrdersTF, (char *) UxOrderContext );


	/* Creation of separator1 */
	separator1 = XtVaCreateManagedWidget( "separator1",
			xmSeparatorWidgetClass,
			order,
			XmNwidth, 1064,
			XmNheight, 12,
			XmNx, 0,
			XmNy, 216,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 3,
			XmNseparatorType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( separator1, (char *) UxOrderContext );


	/* Creation of orderSearchParamLBL */
	orderSearchParamLBL = XtVaCreateManagedWidget( "orderSearchParamLBL",
			xmLabelWidgetClass,
			order,
			XmNx, 16,
			XmNy, 72,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Search Parameters:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( orderSearchParamLBL, (char *) UxOrderContext );


	/* Creation of refreshPB */
	refreshPB = XtVaCreateManagedWidget( "refreshPB",
			xmPushButtonWidgetClass,
			order,
			XmNx, 304,
			XmNy, 748,
			XmNwidth, 188,
			XmNheight, 32,
			RES_CONVERT( XmNlabelString, "REFRESH    SEARCH" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( refreshPB, XmNactivateCallback,
		(XtCallbackProc) order_refreshCb,
		(XtPointer) UxOrderContext );

	UxPutContext( refreshPB, (char *) UxOrderContext );


	/* Creation of viewItemsPB */
	viewItemsPB = XtVaCreateManagedWidget( "viewItemsPB",
			xmPushButtonWidgetClass,
			order,
			XmNx, 40,
			XmNy, 748,
			XmNwidth, 186,
			XmNheight, 32,
			RES_CONVERT( XmNlabelString, "VIEW     ITEMS" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( viewItemsPB, XmNactivateCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( viewItemsPB, (char *) UxOrderContext );


	/* Creation of printPB */
	printPB = XtVaCreateManagedWidget( "printPB",
			xmPushButtonWidgetClass,
			order,
			XmNx, 572,
			XmNy, 748,
			XmNwidth, 186,
			XmNheight, 32,
			RES_CONVERT( XmNlabelString, "PRINT    SCREEN" ),
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( printPB, XmNactivateCallback,
		(XtCallbackProc) order_printScreenCb,
		(XtPointer) UxOrderContext );

	UxPutContext( printPB, (char *) UxOrderContext );


	/* Creation of receivedSW */
	receivedSW = XtVaCreateManagedWidget( "receivedSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 277,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 142,
			NULL );
	UxPutContext( receivedSW, (char *) UxOrderContext );


	/* Creation of receivedList */
	receivedList = XtVaCreateManagedWidget( "receivedList",
			xmListWidgetClass,
			receivedSW,
			XmNwidth, 142,
			XmNheight, 152,
			XmNlistSizePolicy, XmCONSTANT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNx, 0,
			XmNy, 248,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( receivedList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( receivedList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 3 );

	UxPutContext( receivedList, (char *) UxOrderContext );


	/* Creation of onlineSW */
	onlineSW = XtVaCreateManagedWidget( "onlineSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 848,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 50,
			NULL );
	UxPutContext( onlineSW, (char *) UxOrderContext );


	/* Creation of onlineList */
	onlineList = XtVaCreateManagedWidget( "onlineList",
			xmListWidgetClass,
			onlineSW,
			XmNwidth, 50,
			XmNheight, 152,
			XmNx, 0,
			XmNy, 248,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			NULL );
	XtAddCallback( onlineList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( onlineList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 9 );

	UxPutContext( onlineList, (char *) UxOrderContext );


	/* Creation of orderIdSW */
	orderIdSW = XtVaCreateManagedWidget( "orderIdSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 16,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 92,
			NULL );
	UxPutContext( orderIdSW, (char *) UxOrderContext );


	/* Creation of orderIdList */
	orderIdList = XtVaCreateManagedWidget( "orderIdList",
			xmListWidgetClass,
			orderIdSW,
			XmNwidth, 92,
			XmNheight, 152,
			XmNx, 0,
			XmNy, 248,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			NULL );
	XtAddCallback( orderIdList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( orderIdList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 1 );

	UxPutContext( orderIdList, (char *) UxOrderContext );


	/* Creation of userIdSW */
	userIdSW = XtVaCreateManagedWidget( "userIdSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 114,
			XmNy, 304,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 156,
			NULL );
	UxPutContext( userIdSW, (char *) UxOrderContext );


	/* Creation of userIdList */
	userIdList = XtVaCreateManagedWidget( "userIdList",
			xmListWidgetClass,
			userIdSW,
			XmNwidth, 156,
			XmNheight, 152,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			NULL );
	XtAddCallback( userIdList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( userIdList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 2 );

	UxPutContext( userIdList, (char *) UxOrderContext );


	/* Creation of itemNoSW */
	itemNoSW = XtVaCreateManagedWidget( "itemNoSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 12,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 36,
			NULL );
	UxPutContext( itemNoSW, (char *) UxOrderContext );


	/* Creation of itemNoList */
	itemNoList = XtVaCreateManagedWidget( "itemNoList",
			xmListWidgetClass,
			itemNoSW,
			XmNwidth, 36,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNx, 0,
			XmNy, 570,
			NULL );
	XtAddCallback( itemNoList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 1 );
	XtAddCallback( itemNoList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( itemNoList, (char *) UxOrderContext );


	/* Creation of frameIdSW */
	frameIdSW = XtVaCreateManagedWidget( "frameIdSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 114,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 135,
			NULL );
	UxPutContext( frameIdSW, (char *) UxOrderContext );


	/* Creation of frameIdList */
	frameIdList = XtVaCreateManagedWidget( "frameIdList",
			xmListWidgetClass,
			frameIdSW,
			XmNwidth, 135,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( frameIdList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 2 );
	XtAddCallback( frameIdList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( frameIdList, (char *) UxOrderContext );


	/* Creation of itemStatusSW */
	itemStatusSW = XtVaCreateManagedWidget( "itemStatusSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 600,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 120,
			NULL );
	UxPutContext( itemStatusSW, (char *) UxOrderContext );


	/* Creation of itemStatusList */
	itemStatusList = XtVaCreateManagedWidget( "itemStatusList",
			xmListWidgetClass,
			itemStatusSW,
			XmNwidth, 120,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			NULL );
	XtAddCallback( itemStatusList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 7 );
	XtAddCallback( itemStatusList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( itemStatusList, (char *) UxOrderContext );


	/* Creation of procTypeSW */
	procTypeSW = XtVaCreateManagedWidget( "procTypeSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 295,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 172,
			NULL );
	UxPutContext( procTypeSW, (char *) UxOrderContext );


	/* Creation of procTypeList */
	procTypeList = XtVaCreateManagedWidget( "procTypeList",
			xmListWidgetClass,
			procTypeSW,
			XmNwidth, 172,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			NULL );
	XtAddCallback( procTypeList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 4 );
	XtAddCallback( procTypeList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( procTypeList, (char *) UxOrderContext );


	/* Creation of validateSW */
	validateSW = XtVaCreateManagedWidget( "validateSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 917,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 35,
			NULL );
	UxPutContext( validateSW, (char *) UxOrderContext );


	/* Creation of validateList */
	validateList = XtVaCreateManagedWidget( "validateList",
			xmListWidgetClass,
			validateSW,
			XmNwidth, 35,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( validateList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 11 );
	XtAddCallback( validateList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( validateList, (char *) UxOrderContext );


	/* Creation of shipSW */
	shipSW = XtVaCreateManagedWidget( "shipSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 957,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 35,
			XmNheight, 152,
			NULL );
	UxPutContext( shipSW, (char *) UxOrderContext );


	/* Creation of shipList */
	shipList = XtVaCreateManagedWidget( "shipList",
			xmListWidgetClass,
			shipSW,
			XmNwidth, 35,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( shipList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 12 );
	XtAddCallback( shipList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( shipList, (char *) UxOrderContext );


	/* Creation of billSW */
	billSW = XtVaCreateManagedWidget( "billSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 997,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 35,
			NULL );
	UxPutContext( billSW, (char *) UxOrderContext );


	/* Creation of billList */
	billList = XtVaCreateManagedWidget( "billList",
			xmListWidgetClass,
			billSW,
			XmNwidth, 35,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( billList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 13 );
	XtAddCallback( billList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( billList, (char *) UxOrderContext );


	/* Creation of orderDummySW */
	orderDummySW = XtVaCreateManagedWidget( "orderDummySW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 1018,
			XmNy, 304,
			XmNheight, 166,
			XmNwidth, 15,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( orderDummySW, (char *) UxOrderContext );


	/* Creation of orderDummyList */
	orderDummyList = XtVaCreateManagedWidget( "orderDummyList",
			xmListWidgetClass,
			orderDummySW,
			XmNwidth, 2,
			XmNheight, 152,
			XmNlistSizePolicy, XmCONSTANT,
			XmNmappedWhenManaged, FALSE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNshadowThickness, 1,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNdoubleClickInterval, 0,
			NULL );
	UxPutContext( orderDummyList, (char *) UxOrderContext );


	/* Creation of itemDummySW */
	itemDummySW = XtVaCreateManagedWidget( "itemDummySW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 1033,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 167,
			XmNwidth, 15,
			NULL );
	UxPutContext( itemDummySW, (char *) UxOrderContext );


	/* Creation of itemDummyList */
	itemDummyList = XtVaCreateManagedWidget( "itemDummyList",
			xmListWidgetClass,
			itemDummySW,
			XmNwidth, 2,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNmappedWhenManaged, FALSE,
			XmNdoubleClickInterval, 0,
			NULL );
	UxPutContext( itemDummyList, (char *) UxOrderContext );


	/* Creation of label8 */
	label8 = XtVaCreateManagedWidget( "label8",
			xmLabelWidgetClass,
			order,
			XmNx, 600,
			XmNy, 500,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Total Number of Items:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label8, (char *) UxOrderContext );


	/* Creation of totalItemsTF */
	totalItemsTF = XtVaCreateManagedWidget( "totalItemsTF",
			xmTextFieldWidgetClass,
			order,
			XmNwidth, 96,
			XmNx, 816,
			XmNy, 492,
			XmNheight, 36,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	UxPutContext( totalItemsTF, (char *) UxOrderContext );


	/* Creation of searchParamSW */
	searchParamSW = XtVaCreateManagedWidget( "searchParamSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 16,
			XmNy, 96,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 112,
			XmNwidth, 460,
			NULL );
	UxPutContext( searchParamSW, (char *) UxOrderContext );


	/* Creation of searchParamST */
	searchParamST = XtVaCreateManagedWidget( "searchParamST",
			xmTextWidgetClass,
			searchParamSW,
			XmNwidth, 460,
			XmNheight, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNscrollHorizontal, FALSE,
			XmNeditable, FALSE,
			XmNcolumns, 20,
			XmNwordWrap, TRUE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			NULL );
	UxPutContext( searchParamST, (char *) UxOrderContext );


	/* Creation of userInfoLBL */
	userInfoLBL = XtVaCreateManagedWidget( "userInfoLBL",
			xmLabelWidgetClass,
			order,
			XmNx, 580,
			XmNy, 72,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order User & Account Information:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( userInfoLBL, (char *) UxOrderContext );


	/* Creation of userInfoSW */
	userInfoSW = XtVaCreateManagedWidget( "userInfoSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 580,
			XmNy, 96,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 460,
			NULL );
	UxPutContext( userInfoSW, (char *) UxOrderContext );


	/* Creation of userInfoST */
	userInfoST = XtVaCreateManagedWidget( "userInfoST",
			xmTextWidgetClass,
			userInfoSW,
			XmNwidth, 460,
			XmNheight, 112,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollHorizontal, FALSE,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( userInfoST, (char *) UxOrderContext );


	/* Creation of generatedSW */
	generatedSW = XtVaCreateManagedWidget( "generatedSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 788,
			XmNy, 304,
			XmNwidth, 50,
			XmNheight, 152,
			NULL );
	UxPutContext( generatedSW, (char *) UxOrderContext );


	/* Creation of generatedList */
	generatedList = XtVaCreateManagedWidget( "generatedList",
			xmListWidgetClass,
			generatedSW,
			XmNwidth, 50,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( generatedList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( generatedList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 8 );

	UxPutContext( generatedList, (char *) UxOrderContext );


	/* Creation of orderQlkSW */
	orderQlkSW = XtVaCreateManagedWidget( "orderQlkSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 520,
			XmNy, 304,
			XmNwidth, 30,
			XmNheight, 152,
			NULL );
	UxPutContext( orderQlkSW, (char *) UxOrderContext );


	/* Creation of orderQlkList */
	orderQlkList = XtVaCreateManagedWidget( "orderQlkList",
			xmListWidgetClass,
			orderQlkSW,
			XmNwidth, 30,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			NULL );
	XtAddCallback( orderQlkList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( orderQlkList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 5 );

	UxPutContext( orderQlkList, (char *) UxOrderContext );


	/* Creation of holdSW */
	holdSW = XtVaCreateManagedWidget( "holdSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 907,
			XmNy, 304,
			XmNwidth, 50,
			XmNheight, 152,
			NULL );
	UxPutContext( holdSW, (char *) UxOrderContext );


	/* Creation of holdList */
	holdList = XtVaCreateManagedWidget( "holdList",
			xmListWidgetClass,
			holdSW,
			XmNwidth, 50,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( holdList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( holdList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 10 );

	UxPutContext( holdList, (char *) UxOrderContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			order,
			XmNx, 846,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label2, (char *) UxOrderContext );


	/* Creation of label9 */
	label9 = XtVaCreateManagedWidget( "label9",
			xmLabelWidgetClass,
			order,
			XmNx, 793,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Gen" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label9, (char *) UxOrderContext );


	/* Creation of label10 */
	label10 = XtVaCreateManagedWidget( "label10",
			xmLabelWidgetClass,
			order,
			XmNx, 434,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Priority" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label10, (char *) UxOrderContext );


	/* Creation of orderPriSW */
	orderPriSW = XtVaCreateManagedWidget( "orderPriSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 426,
			XmNy, 304,
			XmNheight, 152,
			XmNwidth, 86,
			NULL );
	UxPutContext( orderPriSW, (char *) UxOrderContext );


	/* Creation of orderPriorityList */
	orderPriorityList = XtVaCreateManagedWidget( "orderPriorityList",
			xmListWidgetClass,
			orderPriSW,
			XmNwidth, 86,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			NULL );
	XtAddCallback( orderPriorityList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( orderPriorityList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 4 );

	UxPutContext( orderPriorityList, (char *) UxOrderContext );


	/* Creation of totalSW */
	totalSW = XtVaCreateManagedWidget( "totalSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 728,
			XmNy, 304,
			XmNheight, 152,
			XmNwidth, 50,
			NULL );
	UxPutContext( totalSW, (char *) UxOrderContext );


	/* Creation of totalList */
	totalList = XtVaCreateManagedWidget( "totalList",
			xmListWidgetClass,
			totalSW,
			XmNwidth, 50,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( totalList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( totalList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 7 );

	UxPutContext( totalList, (char *) UxOrderContext );


	/* Creation of label12 */
	label12 = XtVaCreateManagedWidget( "label12",
			xmLabelWidgetClass,
			order,
			XmNx, 810,
			XmNy, 240,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item Statistics" ),
			RES_CONVERT( XmNbackground, "DarkSlateBlue" ),
			RES_CONVERT( XmNforeground, "WhiteSmoke" ),
			NULL );
	UxPutContext( label12, (char *) UxOrderContext );


	/* Creation of label14 */
	label14 = XtVaCreateManagedWidget( "label14",
			xmLabelWidgetClass,
			order,
			XmNx, 516,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "QLK" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label14, (char *) UxOrderContext );


	/* Creation of label16 */
	label16 = XtVaCreateManagedWidget( "label16",
			xmLabelWidgetClass,
			order,
			XmNx, 900,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cancld" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label16, (char *) UxOrderContext );


	/* Creation of label19 */
	label19 = XtVaCreateManagedWidget( "label19",
			xmLabelWidgetClass,
			order,
			XmNx, 962,
			XmNy, 280,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Err/Fail" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label19, (char *) UxOrderContext );


	/* Creation of label33 */
	label33 = XtVaCreateManagedWidget( "label33",
			xmLabelWidgetClass,
			order,
			XmNx, 248,
			XmNy, 543,
			XmNwidth, 44,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "SAT" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label33, (char *) UxOrderContext );


	/* Creation of itemPriSW */
	itemPriSW = XtVaCreateManagedWidget( "itemPriSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 473,
			XmNy, 568,
			XmNheight, 152,
			XmNwidth, 86,
			NULL );
	UxPutContext( itemPriSW, (char *) UxOrderContext );


	/* Creation of itemPriorityList */
	itemPriorityList = XtVaCreateManagedWidget( "itemPriorityList",
			xmListWidgetClass,
			itemPriSW,
			XmNwidth, 86,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 570,
			NULL );
	XtAddCallback( itemPriorityList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 5 );
	XtAddCallback( itemPriorityList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( itemPriorityList, (char *) UxOrderContext );


	/* Creation of label34 */
	label34 = XtVaCreateManagedWidget( "label34",
			xmLabelWidgetClass,
			order,
			XmNx, 307,
			XmNy, 543,
			XmNwidth, 144,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Processing Type" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label34, (char *) UxOrderContext );


	/* Creation of satSW */
	satSW = XtVaCreateManagedWidget( "satSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 254,
			XmNy, 568,
			XmNheight, 152,
			XmNwidth, 35,
			NULL );
	UxPutContext( satSW, (char *) UxOrderContext );


	/* Creation of satList */
	satList = XtVaCreateManagedWidget( "satList",
			xmListWidgetClass,
			satSW,
			XmNwidth, 35,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 570,
			NULL );
	XtAddCallback( satList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 3 );
	XtAddCallback( satList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( satList, (char *) UxOrderContext );


	/* Creation of label11 */
	label11 = XtVaCreateManagedWidget( "label11",
			xmLabelWidgetClass,
			order,
			XmNx, 557,
			XmNy, 543,
			XmNwidth, 43,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "QLK" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label11, (char *) UxOrderContext );


	/* Creation of itemQlkSW */
	itemQlkSW = XtVaCreateManagedWidget( "itemQlkSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 565,
			XmNy, 568,
			XmNheight, 152,
			XmNwidth, 29,
			NULL );
	UxPutContext( itemQlkSW, (char *) UxOrderContext );


	/* Creation of itemQlkList */
	itemQlkList = XtVaCreateManagedWidget( "itemQlkList",
			xmListWidgetClass,
			itemQlkSW,
			XmNwidth, 29,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 570,
			NULL );
	XtAddCallback( itemQlkList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 6 );
	XtAddCallback( itemQlkList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( itemQlkList, (char *) UxOrderContext );


	/* Creation of separator5 */
	separator5 = XtVaCreateManagedWidget( "separator5",
			xmSeparatorWidgetClass,
			order,
			XmNwidth, 12,
			XmNheight, 256,
			XmNx, 692,
			XmNy, 220,
			XmNorientation, XmVERTICAL,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_OUT,
			XmNshadowThickness, 4,
			NULL );
	UxPutContext( separator5, (char *) UxOrderContext );


	/* Creation of errorSW */
	errorSW = XtVaCreateManagedWidget( "errorSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 966,
			XmNy, 304,
			XmNheight, 152,
			XmNwidth, 50,
			NULL );
	UxPutContext( errorSW, (char *) UxOrderContext );


	/* Creation of errorList */
	errorList = XtVaCreateManagedWidget( "errorList",
			xmListWidgetClass,
			errorSW,
			XmNwidth, 50,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( errorList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( errorList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 11 );

	UxPutContext( errorList, (char *) UxOrderContext );


	/* Creation of orderStatusSW */
	orderStatusSW = XtVaCreateManagedWidget( "orderStatusSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 560,
			XmNy, 304,
			XmNheight, 152,
			XmNwidth, 110,
			NULL );
	UxPutContext( orderStatusSW, (char *) UxOrderContext );


	/* Creation of orderStatusList */
	orderStatusList = XtVaCreateManagedWidget( "orderStatusList",
			xmListWidgetClass,
			orderStatusSW,
			XmNwidth, 110,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( orderStatusList, XmNdefaultActionCallback,
		(XtCallbackProc) order_show_orderItemsCb,
		(XtPointer) UxOrderContext );
	XtAddCallback( orderStatusList, XmNbrowseSelectionCallback,
		(XtCallbackProc) order_orderLists_selectionCb,
		(XtPointer) 6 );

	UxPutContext( orderStatusList, (char *) UxOrderContext );


	/* Creation of label39 */
	label39 = XtVaCreateManagedWidget( "label39",
			xmLabelWidgetClass,
			order,
			XmNx, 730,
			XmNy, 543,
			XmNwidth, 104,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media Type" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label39, (char *) UxOrderContext );


	/* Creation of mediaSW */
	mediaSW = XtVaCreateManagedWidget( "mediaSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 726,
			XmNy, 568,
			XmNwidth, 117,
			XmNheight, 152,
			NULL );
	UxPutContext( mediaSW, (char *) UxOrderContext );


	/* Creation of mediaList */
	mediaList = XtVaCreateManagedWidget( "mediaList",
			xmListWidgetClass,
			mediaSW,
			XmNwidth, 117,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 570,
			XmNitemCount, 0,
			NULL );
	XtAddCallback( mediaList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 8 );
	XtAddCallback( mediaList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( mediaList, (char *) UxOrderContext );


	/* Creation of label35 */
	label35 = XtVaCreateManagedWidget( "label35",
			xmLabelWidgetClass,
			order,
			XmNx, 851,
			XmNy, 543,
			XmNwidth, 52,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cost" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label35, (char *) UxOrderContext );


	/* Creation of costSW */
	costSW = XtVaCreateManagedWidget( "costSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 849,
			XmNy, 568,
			XmNheight, 152,
			XmNwidth, 62,
			NULL );
	UxPutContext( costSW, (char *) UxOrderContext );


	/* Creation of costList */
	costList = XtVaCreateManagedWidget( "costList",
			xmListWidgetClass,
			costSW,
			XmNwidth, 62,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNvisibleItemCount, 8,
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			XmNy, 570,
			NULL );
	XtAddCallback( costList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 10 );
	XtAddCallback( costList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( costList, (char *) UxOrderContext );


	/* Creation of label43 */
	label43 = XtVaCreateManagedWidget( "label43",
			xmLabelWidgetClass,
			order,
			XmNx, 392,
			XmNy, 40,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order   Production   Screen" ),
			XmNrightAttachment, XmATTACH_NONE,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 400,
			NULL );
	UxPutContext( label43, (char *) UxOrderContext );


	/* Creation of label5 */
	label5 = XtVaCreateManagedWidget( "label5",
			xmLabelWidgetClass,
			order,
			XmNx, 53,
			XmNy, 543,
			XmNwidth, 56,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( label5, (char *) UxOrderContext );


	/* Creation of typeSW */
	typeSW = XtVaCreateManagedWidget( "typeSW",
			xmScrolledWindowWidgetClass,
			order,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 0,
			XmNx, 54,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 152,
			XmNwidth, 55,
			NULL );
	UxPutContext( typeSW, (char *) UxOrderContext );


	/* Creation of typeList */
	typeList = XtVaCreateManagedWidget( "typeList",
			xmListWidgetClass,
			typeSW,
			XmNwidth, 55,
			XmNheight, 152,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNx, 0,
			XmNy, 570,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 8,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNitemCount, 0,
			NULL );
	XtAddCallback( typeList, XmNextendedSelectionCallback,
		(XtCallbackProc) order_itemLists_selectionCb,
		(XtPointer) 9 );
	XtAddCallback( typeList, XmNdefaultActionCallback,
		(XtCallbackProc) order_browse_itemDetailsCb,
		(XtPointer) UxOrderContext );

	UxPutContext( typeList, (char *) UxOrderContext );

	XtVaSetValues(orderMB,
			XmNmenuHelpWidget, orderMB_p3_top_b1,
			NULL );


	XtAddCallback( order, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxOrderContext);


	return ( order );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_order( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCorder               *UxContext;
	static int		_Uxinit = 0;

	UxOrderContext = UxContext =
		(_UxCorder *) UxNewContext( sizeof(_UxCorder), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_order();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

