! UIMX ascii 2.9 key: 4638                                                      

*order.class: form
*order.classinc:
*order.classspec:
*order.classmembers:
*order.classconstructor:
*order.classdestructor:
*order.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*order.ispecdecl:
*order.funcdecl: swidget create_order(swidget UxParent)
*order.funcname: create_order
*order.funcdef: "swidget", "<create_order>(%)"
*order.argdecl: swidget UxParent;
*order.arglist: UxParent
*order.arglist.UxParent: "swidget", "%UxParent%"
*order.icode:
*order.fcode: return(rtrn);\

*order.auxdecl:
*order.static: true
*order.name: order
*order.parent: NO_PARENT
*order.parentExpression: UxParent
*order.defaultShell: transientShell
*order.width: 1060
*order.height: 790
*order.resizePolicy: "resize_none"
*order.isCompound: "true"
*order.compoundIcon: "form.xpm"
*order.compoundName: "form_"
*order.x: 12
*order.y: 2
*order.unitType: "pixels"
*order.allowShellResize: "false"
*order.background: "#9ac0cd"
*order.dialogTitle: "Order Production Screen"

*orderMB.class: rowColumn
*orderMB.static: true
*orderMB.name: orderMB
*orderMB.parent: order
*orderMB.rowColumnType: "menu_bar"
*orderMB.isCompound: "true"
*orderMB.compoundIcon: "pulldownM.xpm"
*orderMB.compoundName: "menu_Bar"
*orderMB.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*orderMB.x: 0
*orderMB.y: 0
*orderMB.rightAttachment: "attach_form"
*orderMB.leftAttachment: "attach_form"
*orderMB.menuAccelerator: "<KeyUp>F10"
*orderMB.menuHelpWidget: "orderMB_p3_top_b1"
*orderMB.background: "CadetBlue"
*orderMB.shadowThickness: 3

*orderMB_p1.class: rowColumn
*orderMB_p1.static: true
*orderMB_p1.name: orderMB_p1
*orderMB_p1.parent: orderMB
*orderMB_p1.rowColumnType: "menu_pulldown"

*orderMB_p1_b1.class: pushButton
*orderMB_p1_b1.static: true
*orderMB_p1_b1.name: orderMB_p1_b1
*orderMB_p1_b1.parent: orderMB_p1
*orderMB_p1_b1.labelString: "Welcome Screen"
*orderMB_p1_b1.background: "CadetBlue"
*orderMB_p1_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p1_b1.activateCallback.source: public
*orderMB_p1_b1.activateCallback: order_goto_welcomeCb

*orderMB_p1_b8.class: separator
*orderMB_p1_b8.static: true
*orderMB_p1_b8.name: orderMB_p1_b8
*orderMB_p1_b8.parent: orderMB_p1

*searchPB.class: pushButton
*searchPB.static: true
*searchPB.name: searchPB
*searchPB.parent: orderMB_p1
*searchPB.labelString: "Order Search Screen"
*searchPB.background: "CadetBlue"
*searchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*searchPB.activateCallback.source: public
*searchPB.activateCallback: order_goto_searchCb

*orderMB_p1_b9.class: separator
*orderMB_p1_b9.static: true
*orderMB_p1_b9.name: orderMB_p1_b9
*orderMB_p1_b9.parent: orderMB_p1

*orderMB_p1_b6.class: pushButton
*orderMB_p1_b6.static: true
*orderMB_p1_b6.name: orderMB_p1_b6
*orderMB_p1_b6.parent: orderMB_p1
*orderMB_p1_b6.labelString: "Close  Screen"
*orderMB_p1_b6.background: "CadetBlue"
*orderMB_p1_b6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p1_b6.activateCallback.source: public
*orderMB_p1_b6.activateCallback: order_closeCb

*orderMB_p3.class: rowColumn
*orderMB_p3.static: true
*orderMB_p3.name: orderMB_p3
*orderMB_p3.parent: orderMB
*orderMB_p3.rowColumnType: "menu_pulldown"

*orderMB_p3_b1.class: pushButton
*orderMB_p3_b1.static: true
*orderMB_p3_b1.name: orderMB_p3_b1
*orderMB_p3_b1.parent: orderMB_p3
*orderMB_p3_b1.labelString: "No Help Available"
*orderMB_p3_b1.background: "cadetBlue"
*orderMB_p3_b1.activateCallback.source: public
*orderMB_p3_b1.activateCallback: 

*orderMB_p4.class: rowColumn
*orderMB_p4.static: true
*orderMB_p4.name: orderMB_p4
*orderMB_p4.parent: orderMB
*orderMB_p4.rowColumnType: "menu_pulldown"

*viewOrderDetailsMPB.class: pushButton
*viewOrderDetailsMPB.static: true
*viewOrderDetailsMPB.name: viewOrderDetailsMPB
*viewOrderDetailsMPB.parent: orderMB_p4
*viewOrderDetailsMPB.labelString: "View Order Details"
*viewOrderDetailsMPB.background: "CadetBlue"
*viewOrderDetailsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*viewOrderDetailsMPB.activateCallback.source: public
*viewOrderDetailsMPB.activateCallback: order_browse_orderDetailsCb
*viewOrderDetailsMPB.sensitive: "false"

*orderMB_p4_b4.class: separator
*orderMB_p4_b4.static: true
*orderMB_p4_b4.name: orderMB_p4_b4
*orderMB_p4_b4.parent: orderMB_p4

*validateOrderMPB.class: pushButton
*validateOrderMPB.static: true
*validateOrderMPB.name: validateOrderMPB
*validateOrderMPB.parent: orderMB_p4
*validateOrderMPB.labelString: "Validate Order"
*validateOrderMPB.background: "CadetBlue"
*validateOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*validateOrderMPB.activateCallback.source: public
*validateOrderMPB.activateCallback: order_validate_orderCb
*validateOrderMPB.activateCallbackClientData: (XtPointer) 0
*validateOrderMPB.sensitive: "false"

*orderMB_p4_b13.class: separator
*orderMB_p4_b13.static: true
*orderMB_p4_b13.name: orderMB_p4_b13
*orderMB_p4_b13.parent: orderMB_p4

*unvalidateOrderMPB.class: pushButton
*unvalidateOrderMPB.static: true
*unvalidateOrderMPB.name: unvalidateOrderMPB
*unvalidateOrderMPB.parent: orderMB_p4
*unvalidateOrderMPB.labelString: "Unvalidate Order"
*unvalidateOrderMPB.activateCallback.source: public
*unvalidateOrderMPB.activateCallback: order_validate_orderCb
*unvalidateOrderMPB.activateCallbackClientData: (XtPointer) 1
*unvalidateOrderMPB.background: "CadetBlue"
*unvalidateOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*unvalidateOrderMPB.sensitive: "false"

*orderMB_p4_b14.class: separator
*orderMB_p4_b14.static: true
*orderMB_p4_b14.name: orderMB_p4_b14
*orderMB_p4_b14.parent: orderMB_p4

*updateOrderPriorityMPB.class: pushButton
*updateOrderPriorityMPB.static: true
*updateOrderPriorityMPB.name: updateOrderPriorityMPB
*updateOrderPriorityMPB.parent: orderMB_p4
*updateOrderPriorityMPB.labelString: "Update Order Priority"
*updateOrderPriorityMPB.background: "CadetBlue"
*updateOrderPriorityMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateOrderPriorityMPB.activateCallback.source: public
*updateOrderPriorityMPB.activateCallback: order_update_orderPriorityCb
*updateOrderPriorityMPB.sensitive: "false"

*orderMB_p4_b12.class: separator
*orderMB_p4_b12.static: true
*orderMB_p4_b12.name: orderMB_p4_b12
*orderMB_p4_b12.parent: orderMB_p4

*updateOrderStatusMPB.class: pushButton
*updateOrderStatusMPB.static: true
*updateOrderStatusMPB.name: updateOrderStatusMPB
*updateOrderStatusMPB.parent: orderMB_p4
*updateOrderStatusMPB.labelString: "Update Order Status"
*updateOrderStatusMPB.background: "CadetBlue"
*updateOrderStatusMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateOrderStatusMPB.activateCallback.source: public
*updateOrderStatusMPB.activateCallback: order_update_orderStatusCb
*updateOrderStatusMPB.sensitive: "false"

*orderMB_p4_b6.class: separator
*orderMB_p4_b6.static: true
*orderMB_p4_b6.name: orderMB_p4_b6
*orderMB_p4_b6.parent: orderMB_p4

*editOrderCommentMPB.class: pushButton
*editOrderCommentMPB.static: true
*editOrderCommentMPB.name: editOrderCommentMPB
*editOrderCommentMPB.parent: orderMB_p4
*editOrderCommentMPB.labelString: "Edit Order Comments"
*editOrderCommentMPB.background: "CadetBlue"
*editOrderCommentMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*editOrderCommentMPB.activateCallback.source: public
*editOrderCommentMPB.activateCallback: order_edit_orderCommentCb
*editOrderCommentMPB.sensitive: "false"

*orderMB_p4_b11.class: separator
*orderMB_p4_b11.static: true
*orderMB_p4_b11.name: orderMB_p4_b11
*orderMB_p4_b11.parent: orderMB_p4

*processMediaMPB.class: pushButton
*processMediaMPB.static: true
*processMediaMPB.name: processMediaMPB
*processMediaMPB.parent: orderMB_p4
*processMediaMPB.labelString: "Process Media"
*processMediaMPB.background: "CadetBlue"
*processMediaMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*processMediaMPB.activateCallback.source: public
*processMediaMPB.activateCallback: order_processMediaCb
*processMediaMPB.activateCallbackClientData: (XtPointer) 1
*processMediaMPB.sensitive: "false"

*orderMB_p4_b15.class: separator
*orderMB_p4_b15.static: true
*orderMB_p4_b15.name: orderMB_p4_b15
*orderMB_p4_b15.parent: orderMB_p4

*shipOrderMPB.class: cascadeButton
*shipOrderMPB.static: true
*shipOrderMPB.name: shipOrderMPB
*shipOrderMPB.parent: orderMB_p4
*shipOrderMPB.labelString: "Ship Order"
*shipOrderMPB.subMenuId: "orderMB_p6"
*shipOrderMPB.background: "cadetBlue"
*shipOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*shipOrderMPB.activateCallback.source: public
*shipOrderMPB.activateCallback: 
*shipOrderMPB.sensitive: "false"

*orderMB_p4_b17.class: separator
*orderMB_p4_b17.static: true
*orderMB_p4_b17.name: orderMB_p4_b17
*orderMB_p4_b17.parent: orderMB_p4

*billOrderMPB.class: cascadeButton
*billOrderMPB.static: true
*billOrderMPB.name: billOrderMPB
*billOrderMPB.parent: orderMB_p4
*billOrderMPB.labelString: "Bill Order"
*billOrderMPB.subMenuId: "orderMB_p7"
*billOrderMPB.background: "cadetBlue"
*billOrderMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*billOrderMPB.sensitive: "false"

*orderMB_p6.class: rowColumn
*orderMB_p6.static: true
*orderMB_p6.name: orderMB_p6
*orderMB_p6.parent: orderMB_p4
*orderMB_p6.rowColumnType: "menu_pulldown"

*orderMB_p6_b1.class: pushButton
*orderMB_p6_b1.static: true
*orderMB_p6_b1.name: orderMB_p6_b1
*orderMB_p6_b1.parent: orderMB_p6
*orderMB_p6_b1.labelString: "Create New Shipment"
*orderMB_p6_b1.background: "cadetBlue"
*orderMB_p6_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p6_b1.activateCallback.source: public
*orderMB_p6_b1.activateCallback: order_create_shipmentCb
*orderMB_p6_b1.activateCallbackClientData: (XtPointer) 1

*orderMB_p6_b2.class: separator
*orderMB_p6_b2.static: true
*orderMB_p6_b2.name: orderMB_p6_b2
*orderMB_p6_b2.parent: orderMB_p6

*orderMB_p6_b3.class: pushButton
*orderMB_p6_b3.static: true
*orderMB_p6_b3.name: orderMB_p6_b3
*orderMB_p6_b3.parent: orderMB_p6
*orderMB_p6_b3.labelString: "View Shipping Reports"
*orderMB_p6_b3.background: "cadetBlue"
*orderMB_p6_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p6_b3.activateCallback.source: public
*orderMB_p6_b3.activateCallback: order_view_shippingReportsCb
*orderMB_p6_b3.activateCallbackClientData: (XtPointer) 1

*orderMB_p7.class: rowColumn
*orderMB_p7.static: true
*orderMB_p7.name: orderMB_p7
*orderMB_p7.parent: orderMB_p4
*orderMB_p7.rowColumnType: "menu_pulldown"

*orderMB_p7_b1.class: pushButton
*orderMB_p7_b1.static: true
*orderMB_p7_b1.name: orderMB_p7_b1
*orderMB_p7_b1.parent: orderMB_p7
*orderMB_p7_b1.labelString: "Create New Invoice"
*orderMB_p7_b1.background: "cadetBlue"
*orderMB_p7_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p7_b1.activateCallback.source: public
*orderMB_p7_b1.activateCallback: order_create_invoiceCb
*orderMB_p7_b1.activateCallbackClientData: (XtPointer) 1

*orderMB_p7_b2.class: separator
*orderMB_p7_b2.static: true
*orderMB_p7_b2.name: orderMB_p7_b2
*orderMB_p7_b2.parent: orderMB_p7

*orderMB_p7_b3.class: pushButton
*orderMB_p7_b3.static: true
*orderMB_p7_b3.name: orderMB_p7_b3
*orderMB_p7_b3.parent: orderMB_p7
*orderMB_p7_b3.labelString: "View Existing Invoices"
*orderMB_p7_b3.background: "cadetBlue"
*orderMB_p7_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p7_b3.activateCallback.source: public
*orderMB_p7_b3.activateCallback: order_view_invoiceCb
*orderMB_p7_b3.activateCallbackClientData: (XtPointer) 1

*orderMB_p5.class: rowColumn
*orderMB_p5.static: true
*orderMB_p5.name: orderMB_p5
*orderMB_p5.parent: orderMB
*orderMB_p5.rowColumnType: "menu_pulldown"

*viewItemDetailsMPB.class: pushButton
*viewItemDetailsMPB.static: true
*viewItemDetailsMPB.name: viewItemDetailsMPB
*viewItemDetailsMPB.parent: orderMB_p5
*viewItemDetailsMPB.labelString: "View Item Details"
*viewItemDetailsMPB.background: "CadetBlue"
*viewItemDetailsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*viewItemDetailsMPB.sensitive: "false"
*viewItemDetailsMPB.activateCallback.source: public
*viewItemDetailsMPB.activateCallback: order_browse_itemDetailsCb

*orderMB_p5_b3.class: separator
*orderMB_p5_b3.static: true
*orderMB_p5_b3.name: orderMB_p5_b3
*orderMB_p5_b3.parent: orderMB_p5

*validateItemMPB.class: pushButton
*validateItemMPB.static: true
*validateItemMPB.name: validateItemMPB
*validateItemMPB.parent: orderMB_p5
*validateItemMPB.labelString: "Validate Item"
*validateItemMPB.background: "cadetBlue"
*validateItemMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*validateItemMPB.sensitive: "false"
*validateItemMPB.activateCallback.source: public
*validateItemMPB.activateCallback: order_validate_itemCb
*validateItemMPB.activateCallbackClientData: (XtPointer) 0

*orderMB_p5_b8.class: separator
*orderMB_p5_b8.static: true
*orderMB_p5_b8.name: orderMB_p5_b8
*orderMB_p5_b8.parent: orderMB_p5

*unvalidateItemMPB.class: pushButton
*unvalidateItemMPB.static: true
*unvalidateItemMPB.name: unvalidateItemMPB
*unvalidateItemMPB.parent: orderMB_p5
*unvalidateItemMPB.labelString: "Unvalidate Item"
*unvalidateItemMPB.activateCallback.source: public
*unvalidateItemMPB.activateCallback: order_validate_itemCb
*unvalidateItemMPB.background: "CadetBlue"
*unvalidateItemMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*unvalidateItemMPB.activateCallbackClientData: (XtPointer) 1
*unvalidateItemMPB.sensitive: "false"

*orderMB_p5_b13.class: separator
*orderMB_p5_b13.static: true
*orderMB_p5_b13.name: orderMB_p5_b13
*orderMB_p5_b13.parent: orderMB_p5

*updateItemPriorityMPB.class: pushButton
*updateItemPriorityMPB.static: true
*updateItemPriorityMPB.name: updateItemPriorityMPB
*updateItemPriorityMPB.parent: orderMB_p5
*updateItemPriorityMPB.labelString: "Update Item Priority"
*updateItemPriorityMPB.background: "CadetBlue"
*updateItemPriorityMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateItemPriorityMPB.sensitive: "false"
*updateItemPriorityMPB.activateCallback.source: public
*updateItemPriorityMPB.activateCallback: order_update_itemPriorityCb

*orderMB_p5_b11.class: separator
*orderMB_p5_b11.static: true
*orderMB_p5_b11.name: orderMB_p5_b11
*orderMB_p5_b11.parent: orderMB_p5

*updateItemStatusMPB.class: pushButton
*updateItemStatusMPB.static: true
*updateItemStatusMPB.name: updateItemStatusMPB
*updateItemStatusMPB.parent: orderMB_p5
*updateItemStatusMPB.labelString: "Update Item Status"
*updateItemStatusMPB.background: "CadetBlue"
*updateItemStatusMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*updateItemStatusMPB.sensitive: "false"
*updateItemStatusMPB.activateCallback.source: public
*updateItemStatusMPB.activateCallback: order_update_itemStatusCb

*orderMB_p5_b5.class: separator
*orderMB_p5_b5.static: true
*orderMB_p5_b5.name: orderMB_p5_b5
*orderMB_p5_b5.parent: orderMB_p5

*editItemCommentMPB.class: pushButton
*editItemCommentMPB.static: true
*editItemCommentMPB.name: editItemCommentMPB
*editItemCommentMPB.parent: orderMB_p5
*editItemCommentMPB.labelString: "Edit Item Comments"
*editItemCommentMPB.background: "CadetBlue"
*editItemCommentMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*editItemCommentMPB.activateCallback.source: public
*editItemCommentMPB.activateCallback: order_edit_itemCommentCb
*editItemCommentMPB.sensitive: "false"

*orderMB_p5_b9.class: separator
*orderMB_p5_b9.static: true
*orderMB_p5_b9.name: orderMB_p5_b9
*orderMB_p5_b9.parent: orderMB_p5

*itemProcessMediaMPB.class: pushButton
*itemProcessMediaMPB.static: true
*itemProcessMediaMPB.name: itemProcessMediaMPB
*itemProcessMediaMPB.parent: orderMB_p5
*itemProcessMediaMPB.labelString: "Process Media"
*itemProcessMediaMPB.background: "CadetBlue"
*itemProcessMediaMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*itemProcessMediaMPB.sensitive: "false"
*itemProcessMediaMPB.activateCallback.source: public
*itemProcessMediaMPB.activateCallback: order_processMediaCb
*itemProcessMediaMPB.activateCallbackClientData: (XtPointer) 2

*orderMB_p5_b15.class: separator
*orderMB_p5_b15.static: true
*orderMB_p5_b15.name: orderMB_p5_b15
*orderMB_p5_b15.parent: orderMB_p5

*itemShipItemsMPB.class: cascadeButton
*itemShipItemsMPB.static: true
*itemShipItemsMPB.name: itemShipItemsMPB
*itemShipItemsMPB.parent: orderMB_p5
*itemShipItemsMPB.labelString: "Ship Items"
*itemShipItemsMPB.subMenuId: "orderMB_p8"
*itemShipItemsMPB.background: "cadetBlue"
*itemShipItemsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*itemShipItemsMPB.sensitive: "false"

*orderMB_p5_b17.class: separator
*orderMB_p5_b17.static: true
*orderMB_p5_b17.name: orderMB_p5_b17
*orderMB_p5_b17.parent: orderMB_p5

*itemBillItemsMPB.class: cascadeButton
*itemBillItemsMPB.static: true
*itemBillItemsMPB.name: itemBillItemsMPB
*itemBillItemsMPB.parent: orderMB_p5
*itemBillItemsMPB.labelString: "Bill Items"
*itemBillItemsMPB.subMenuId: "orderMB_p9"
*itemBillItemsMPB.background: "cadetBlue"
*itemBillItemsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*itemBillItemsMPB.sensitive: "false"

*orderMB_p5_b19.class: separator
*orderMB_p5_b19.static: true
*orderMB_p5_b19.name: orderMB_p5_b19
*orderMB_p5_b19.parent: orderMB_p5

*restartItemMPB.class: pushButton
*restartItemMPB.static: true
*restartItemMPB.name: restartItemMPB
*restartItemMPB.parent: orderMB_p5
*restartItemMPB.labelString: "Restart Item"
*restartItemMPB.background: "cadetBlue"
*restartItemMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*restartItemMPB.activateCallback.source: public
*restartItemMPB.activateCallback: order_restart_itemCb
*restartItemMPB.sensitive: "false"

*orderMB_p8.class: rowColumn
*orderMB_p8.static: true
*orderMB_p8.name: orderMB_p8
*orderMB_p8.parent: orderMB_p5
*orderMB_p8.rowColumnType: "menu_pulldown"

*orderMB_p8_b1.class: pushButton
*orderMB_p8_b1.static: true
*orderMB_p8_b1.name: orderMB_p8_b1
*orderMB_p8_b1.parent: orderMB_p8
*orderMB_p8_b1.labelString: "Create New Shipment"
*orderMB_p8_b1.background: "cadetBlue"
*orderMB_p8_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p8_b1.activateCallback.source: public
*orderMB_p8_b1.activateCallback: order_create_shipmentCb
*orderMB_p8_b1.activateCallbackClientData: (XtPointer) 2

*orderMB_p8_b2.class: separator
*orderMB_p8_b2.static: true
*orderMB_p8_b2.name: orderMB_p8_b2
*orderMB_p8_b2.parent: orderMB_p8

*orderMB_p8_b3.class: pushButton
*orderMB_p8_b3.static: true
*orderMB_p8_b3.name: orderMB_p8_b3
*orderMB_p8_b3.parent: orderMB_p8
*orderMB_p8_b3.labelString: "View Shipping Reports"
*orderMB_p8_b3.background: "cadetBlue"
*orderMB_p8_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p8_b3.activateCallback.source: public
*orderMB_p8_b3.activateCallback: order_view_shippingReportsCb
*orderMB_p8_b3.activateCallbackClientData: (XtPointer) 2

*orderMB_p9.class: rowColumn
*orderMB_p9.static: true
*orderMB_p9.name: orderMB_p9
*orderMB_p9.parent: orderMB_p5
*orderMB_p9.rowColumnType: "menu_pulldown"

*orderMB_p9_b1.class: pushButton
*orderMB_p9_b1.static: true
*orderMB_p9_b1.name: orderMB_p9_b1
*orderMB_p9_b1.parent: orderMB_p9
*orderMB_p9_b1.labelString: "Create New Invoice"
*orderMB_p9_b1.background: "cadetBlue"
*orderMB_p9_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p9_b1.activateCallback.source: public
*orderMB_p9_b1.activateCallback: order_create_invoiceCb
*orderMB_p9_b1.activateCallbackClientData: (XtPointer) 2

*orderMB_p9_b2.class: separator
*orderMB_p9_b2.static: true
*orderMB_p9_b2.name: orderMB_p9_b2
*orderMB_p9_b2.parent: orderMB_p9

*orderMB_p9_b3.class: pushButton
*orderMB_p9_b3.static: true
*orderMB_p9_b3.name: orderMB_p9_b3
*orderMB_p9_b3.parent: orderMB_p9
*orderMB_p9_b3.labelString: "View Existing Invoices"
*orderMB_p9_b3.background: "cadetBlue"
*orderMB_p9_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p9_b3.activateCallback.source: public
*orderMB_p9_b3.activateCallback: order_view_invoiceCb
*orderMB_p9_b3.activateCallbackClientData: (XtPointer) 2

*orderMB_p10.class: rowColumn
*orderMB_p10.static: true
*orderMB_p10.name: orderMB_p10
*orderMB_p10.parent: orderMB
*orderMB_p10.rowColumnType: "menu_pulldown"
*orderMB_p10.width: 114

*saveResultsMPB.class: pushButton
*saveResultsMPB.static: true
*saveResultsMPB.name: saveResultsMPB
*saveResultsMPB.parent: orderMB_p10
*saveResultsMPB.labelString: "Save Results"
*saveResultsMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*saveResultsMPB.background: "cadetBlue"
*saveResultsMPB.marginWidth: 5
*saveResultsMPB.activateCallback.source: public
*saveResultsMPB.activateCallback: order_save_resultsCb

*orderMB_p10_b2.class: separator
*orderMB_p10_b2.static: true
*orderMB_p10_b2.name: orderMB_p10_b2
*orderMB_p10_b2.parent: orderMB_p10

*refreshSearchMPB.class: pushButton
*refreshSearchMPB.static: true
*refreshSearchMPB.name: refreshSearchMPB
*refreshSearchMPB.parent: orderMB_p10
*refreshSearchMPB.labelString: "Refresh Search"
*refreshSearchMPB.background: "cadetBlue"
*refreshSearchMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*refreshSearchMPB.activateCallback.source: public
*refreshSearchMPB.activateCallback: order_refreshCb

*orderMB_p10_b4.class: separator
*orderMB_p10_b4.static: true
*orderMB_p10_b4.name: orderMB_p10_b4
*orderMB_p10_b4.parent: orderMB_p10

*printScreenMPB.class: pushButton
*printScreenMPB.static: true
*printScreenMPB.name: printScreenMPB
*printScreenMPB.parent: orderMB_p10
*printScreenMPB.labelString: "Print Screen"
*printScreenMPB.background: "cadetBlue"
*printScreenMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*printScreenMPB.activateCallback.source: public
*printScreenMPB.activateCallback: order_printScreenCb

*orderMB_p1_top_b1.class: cascadeButton
*orderMB_p1_top_b1.static: true
*orderMB_p1_top_b1.name: orderMB_p1_top_b1
*orderMB_p1_top_b1.parent: orderMB
*orderMB_p1_top_b1.labelString: "Go To"
*orderMB_p1_top_b1.subMenuId: "orderMB_p1"
*orderMB_p1_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p1_top_b1.background: "CadetBlue"
*orderMB_p1_top_b1.marginWidth: 12
*orderMB_p1_top_b1.mnemonic: "G"

*orderMB_p3_top_b1.class: cascadeButton
*orderMB_p3_top_b1.static: true
*orderMB_p3_top_b1.name: orderMB_p3_top_b1
*orderMB_p3_top_b1.parent: orderMB
*orderMB_p3_top_b1.labelString: "Help"
*orderMB_p3_top_b1.subMenuId: "orderMB_p3"
*orderMB_p3_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_p3_top_b1.background: "CadetBlue"

*orderMB_top_b1.class: cascadeButtonGadget
*orderMB_top_b1.static: true
*orderMB_top_b1.name: orderMB_top_b1
*orderMB_top_b1.parent: orderMB
*orderMB_top_b1.labelString: "Order Functions"
*orderMB_top_b1.subMenuId: "orderMB_p4"
*orderMB_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_top_b1.mnemonic: "O"
*orderMB_top_b1.marginWidth: 20

*orderMB_top_b4.class: cascadeButtonGadget
*orderMB_top_b4.static: true
*orderMB_top_b4.name: orderMB_top_b4
*orderMB_top_b4.parent: orderMB
*orderMB_top_b4.labelString: "Item Functions"
*orderMB_top_b4.mnemonic: "I"
*orderMB_top_b4.subMenuId: "orderMB_p5"
*orderMB_top_b4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_top_b4.marginWidth: 20

*orderMB_top_b2.class: cascadeButtonGadget
*orderMB_top_b2.static: true
*orderMB_top_b2.name: orderMB_top_b2
*orderMB_top_b2.parent: orderMB
*orderMB_top_b2.labelString: "Screen Functions"
*orderMB_top_b2.subMenuId: "orderMB_p10"
*orderMB_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderMB_top_b2.width: 155
*orderMB_top_b2.x: 400
*orderMB_top_b2.marginLeft: 0
*orderMB_top_b2.marginWidth: 20
*orderMB_top_b2.mnemonic: "S"

*separator2.class: separator
*separator2.static: true
*separator2.name: separator2
*separator2.parent: order
*separator2.width: 800
*separator2.height: 12
*separator2.isCompound: "true"
*separator2.compoundIcon: "sep.xpm"
*separator2.compoundName: "separator_"
*separator2.x: 0
*separator2.y: 472
*separator2.rightAttachment: "attach_form"
*separator2.leftAttachment: "attach_form"
*separator2.background: "#9ac0cd"
*separator2.shadowThickness: 3
*separator2.separatorType: "shadow_etched_out"

*separator3.class: separator
*separator3.static: true
*separator3.name: separator3
*separator3.parent: order
*separator3.width: 1024
*separator3.height: 12
*separator3.isCompound: "true"
*separator3.compoundIcon: "sep.xpm"
*separator3.compoundName: "separator_"
*separator3.x: 0
*separator3.y: 732
*separator3.rightAttachment: "attach_form"
*separator3.leftAttachment: "attach_form"
*separator3.background: "#9ac0cd"
*separator3.shadowThickness: 3
*separator3.separatorType: "shadow_etched_out"

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: order
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 836
*closePB.y: 748
*closePB.width: 186
*closePB.height: 32
*closePB.labelString: "CLOSE    SCREEN"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.activateCallback.source: public
*closePB.activateCallback: order_closeCb
*closePB.background: "CadetBlue"
*closePB.shadowThickness: 4

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: order
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 16
*label1.y: 500
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label1.labelString: "Details For Order ID:"
*label1.background: "#9ac0cd"

*orderIdTF.class: textField
*orderIdTF.static: true
*orderIdTF.name: orderIdTF
*orderIdTF.parent: order
*orderIdTF.width: 110
*orderIdTF.isCompound: "true"
*orderIdTF.compoundIcon: "textfield.xpm"
*orderIdTF.compoundName: "text_Field"
*orderIdTF.x: 240
*orderIdTF.y: 492
*orderIdTF.height: 36
*orderIdTF.cursorPositionVisible: "false"
*orderIdTF.editable: "false"
*orderIdTF.background: "LightSkyBlue3"
*orderIdTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*label3.class: label
*label3.static: true
*label3.name: label3
*label3.parent: order
*label3.isCompound: "true"
*label3.compoundIcon: "label.xpm"
*label3.compoundName: "label_"
*label3.x: 910
*label3.y: 543
*label3.width: 44
*label3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label3.labelString: "Vald"
*label3.background: "#9ac0cd"

*label4.class: label
*label4.static: true
*label4.name: label4
*label4.parent: order
*label4.isCompound: "true"
*label4.compoundIcon: "label.xpm"
*label4.compoundName: "label_"
*label4.x: 127
*label4.y: 543
*label4.width: 100
*label4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label4.labelString: "Name"
*label4.background: "#9ac0cd"

*label6.class: label
*label6.static: true
*label6.name: label6
*label6.parent: order
*label6.isCompound: "true"
*label6.compoundIcon: "label.xpm"
*label6.compoundName: "label_"
*label6.x: 480
*label6.y: 543
*label6.width: 66
*label6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label6.labelString: "Priority"
*label6.background: "#9ac0cd"

*label7.class: label
*label7.static: true
*label7.name: label7
*label7.parent: order
*label7.isCompound: "true"
*label7.compoundIcon: "label.xpm"
*label7.compoundName: "label_"
*label7.x: 622
*label7.y: 543
*label7.width: 66
*label7.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label7.labelString: "Status"
*label7.background: "#9ac0cd"

*label13.class: label
*label13.static: true
*label13.name: label13
*label13.parent: order
*label13.isCompound: "true"
*label13.compoundIcon: "label.xpm"
*label13.compoundName: "label_"
*label13.x: 16
*label13.y: 280
*label13.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label13.labelString: "Order ID"
*label13.background: "#9ac0cd"

*label15.class: label
*label15.static: true
*label15.name: label15
*label15.parent: order
*label15.isCompound: "true"
*label15.compoundIcon: "label.xpm"
*label15.compoundName: "label_"
*label15.x: 729
*label15.y: 280
*label15.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label15.labelString: "Total"
*label15.background: "#9ac0cd"

*label17.class: label
*label17.static: true
*label17.name: label17
*label17.parent: order
*label17.isCompound: "true"
*label17.compoundIcon: "label.xpm"
*label17.compoundName: "label_"
*label17.x: 586
*label17.y: 280
*label17.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label17.labelString: "Status"
*label17.background: "#9ac0cd"

*label18.class: label
*label18.static: true
*label18.name: label18
*label18.parent: order
*label18.isCompound: "true"
*label18.compoundIcon: "label.xpm"
*label18.compoundName: "label_"
*label18.x: 118
*label18.y: 280
*label18.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label18.labelString: "User ID"
*label18.background: "#9ac0cd"

*label26.class: label
*label26.static: true
*label26.name: label26
*label26.parent: order
*label26.isCompound: "true"
*label26.compoundIcon: "label.xpm"
*label26.compoundName: "label_"
*label26.x: 280
*label26.y: 280
*label26.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label26.labelString: "Time Received"
*label26.background: "#9ac0cd"

*label36.class: label
*label36.static: true
*label36.name: label36
*label36.parent: order
*label36.isCompound: "true"
*label36.compoundIcon: "label.xpm"
*label36.compoundName: "label_"
*label36.x: 7
*label36.y: 543
*label36.width: 48
*label36.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label36.labelString: "Item"
*label36.background: "#9ac0cd"

*label38.class: label
*label38.static: true
*label38.name: label38
*label38.parent: order
*label38.isCompound: "true"
*label38.compoundIcon: "label.xpm"
*label38.compoundName: "label_"
*label38.x: 954
*label38.y: 543
*label38.width: 44
*label38.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label38.labelString: "Ship"
*label38.background: "#9ac0cd"

*label41.class: label
*label41.static: true
*label41.name: label41
*label41.parent: order
*label41.isCompound: "true"
*label41.compoundIcon: "label.xpm"
*label41.compoundName: "label_"
*label41.x: 994
*label41.y: 543
*label41.width: 44
*label41.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label41.labelString: "Bill"
*label41.background: "#9ac0cd"

*label27.class: label
*label27.static: true
*label27.name: label27
*label27.parent: order
*label27.isCompound: "true"
*label27.compoundIcon: "label.xpm"
*label27.compoundName: "label_"
*label27.x: 16
*label27.y: 240
*label27.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label27.labelString: "Total Number of Orders:"
*label27.background: "#9ac0cd"

*totalOrdersTF.class: textField
*totalOrdersTF.static: true
*totalOrdersTF.name: totalOrdersTF
*totalOrdersTF.parent: order
*totalOrdersTF.width: 110
*totalOrdersTF.isCompound: "true"
*totalOrdersTF.compoundIcon: "textfield.xpm"
*totalOrdersTF.compoundName: "text_Field"
*totalOrdersTF.x: 240
*totalOrdersTF.y: 232
*totalOrdersTF.height: 36
*totalOrdersTF.cursorPositionVisible: "false"
*totalOrdersTF.editable: "false"
*totalOrdersTF.background: "LightSkyBlue3"
*totalOrdersTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*separator1.class: separator
*separator1.static: true
*separator1.name: separator1
*separator1.parent: order
*separator1.width: 1064
*separator1.height: 12
*separator1.isCompound: "true"
*separator1.compoundIcon: "sep.xpm"
*separator1.compoundName: "separator_"
*separator1.x: 0
*separator1.y: 216
*separator1.background: "#9ac0cd"
*separator1.shadowThickness: 3
*separator1.separatorType: "shadow_etched_out"

*orderSearchParamLBL.class: label
*orderSearchParamLBL.static: true
*orderSearchParamLBL.name: orderSearchParamLBL
*orderSearchParamLBL.parent: order
*orderSearchParamLBL.isCompound: "true"
*orderSearchParamLBL.compoundIcon: "label.xpm"
*orderSearchParamLBL.compoundName: "label_"
*orderSearchParamLBL.x: 16
*orderSearchParamLBL.y: 72
*orderSearchParamLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderSearchParamLBL.labelString: "Order Search Parameters:"
*orderSearchParamLBL.background: "#9ac0cd"

*refreshPB.class: pushButton
*refreshPB.static: true
*refreshPB.name: refreshPB
*refreshPB.parent: order
*refreshPB.isCompound: "true"
*refreshPB.compoundIcon: "push.xpm"
*refreshPB.compoundName: "push_Button"
*refreshPB.x: 304
*refreshPB.y: 748
*refreshPB.width: 188
*refreshPB.height: 32
*refreshPB.labelString: "REFRESH    SEARCH"
*refreshPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*refreshPB.background: "CadetBlue"
*refreshPB.shadowThickness: 4
*refreshPB.activateCallback.source: public
*refreshPB.activateCallback: order_refreshCb

*viewItemsPB.class: pushButton
*viewItemsPB.static: true
*viewItemsPB.name: viewItemsPB
*viewItemsPB.parent: order
*viewItemsPB.isCompound: "true"
*viewItemsPB.compoundIcon: "push.xpm"
*viewItemsPB.compoundName: "push_Button"
*viewItemsPB.x: 40
*viewItemsPB.y: 748
*viewItemsPB.width: 186
*viewItemsPB.height: 32
*viewItemsPB.labelString: "VIEW     ITEMS"
*viewItemsPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*viewItemsPB.background: "CadetBlue"
*viewItemsPB.shadowThickness: 4
*viewItemsPB.activateCallback.source: public
*viewItemsPB.activateCallback: order_show_orderItemsCb
*viewItemsPB.sensitive: "false"

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: order
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 572
*printPB.y: 748
*printPB.width: 186
*printPB.height: 32
*printPB.labelString: "PRINT    SCREEN"
*printPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*printPB.background: "CadetBlue"
*printPB.shadowThickness: 4
*printPB.activateCallback.source: public
*printPB.activateCallback: order_printScreenCb

*receivedSW.class: scrolledWindow
*receivedSW.static: true
*receivedSW.name: receivedSW
*receivedSW.parent: order
*receivedSW.scrollingPolicy: "automatic"
*receivedSW.visualPolicy: "constant"
*receivedSW.scrollBarDisplayPolicy: "as_needed"
*receivedSW.shadowThickness: 0
*receivedSW.isCompound: "true"
*receivedSW.compoundIcon: "scrllist.xpm"
*receivedSW.compoundName: "scrolled_List"
*receivedSW.x: 277
*receivedSW.y: 304
*receivedSW.background: "#9ac0cd"
*receivedSW.height: 152
*receivedSW.width: 142

*receivedList.class: scrolledList
*receivedList.static: true
*receivedList.name: receivedList
*receivedList.parent: receivedSW
*receivedList.width: 142
*receivedList.height: 152
*receivedList.listSizePolicy: "constant"
*receivedList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*receivedList.x: 0
*receivedList.y: 248
*receivedList.background: "LightSkyBlue3"
*receivedList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*receivedList.selectionPolicy: "browse_select"
*receivedList.defaultActionCallback.source: public
*receivedList.defaultActionCallback: order_show_orderItemsCb
*receivedList.singleSelectionCallbackClientData: (XtPointer) 0x0
*receivedList.browseSelectionCallback.source: public
*receivedList.browseSelectionCallback: order_orderLists_selectionCb
*receivedList.browseSelectionCallbackClientData: (XtPointer) 3
*receivedList.itemCount: 0

*onlineSW.class: scrolledWindow
*onlineSW.static: true
*onlineSW.name: onlineSW
*onlineSW.parent: order
*onlineSW.scrollingPolicy: "automatic"
*onlineSW.visualPolicy: "constant"
*onlineSW.scrollBarDisplayPolicy: "as_needed"
*onlineSW.shadowThickness: 0
*onlineSW.isCompound: "true"
*onlineSW.compoundIcon: "scrllist.xpm"
*onlineSW.compoundName: "scrolled_List"
*onlineSW.x: 848
*onlineSW.y: 304
*onlineSW.background: "#9ac0cd"
*onlineSW.height: 152
*onlineSW.width: 50

*onlineList.class: scrolledList
*onlineList.static: true
*onlineList.name: onlineList
*onlineList.parent: onlineSW
*onlineList.width: 50
*onlineList.height: 152
*onlineList.x: 0
*onlineList.y: 248
*onlineList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*onlineList.background: "LightSkyBlue3"
*onlineList.listSizePolicy: "constant"
*onlineList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*onlineList.selectionPolicy: "browse_select"
*onlineList.defaultActionCallback.source: public
*onlineList.defaultActionCallback: order_show_orderItemsCb
*onlineList.visibleItemCount: 8
*onlineList.singleSelectionCallbackClientData: (XtPointer) 0x0
*onlineList.browseSelectionCallback.source: public
*onlineList.browseSelectionCallback: order_orderLists_selectionCb
*onlineList.browseSelectionCallbackClientData: (XtPointer) 9

*orderIdSW.class: scrolledWindow
*orderIdSW.static: true
*orderIdSW.name: orderIdSW
*orderIdSW.parent: order
*orderIdSW.scrollingPolicy: "automatic"
*orderIdSW.visualPolicy: "constant"
*orderIdSW.scrollBarDisplayPolicy: "as_needed"
*orderIdSW.shadowThickness: 0
*orderIdSW.isCompound: "true"
*orderIdSW.compoundIcon: "scrllist.xpm"
*orderIdSW.compoundName: "scrolled_List"
*orderIdSW.x: 16
*orderIdSW.y: 304
*orderIdSW.background: "#9ac0cd"
*orderIdSW.height: 152
*orderIdSW.width: 92

*orderIdList.class: scrolledList
*orderIdList.static: true
*orderIdList.name: orderIdList
*orderIdList.parent: orderIdSW
*orderIdList.width: 92
*orderIdList.height: 152
*orderIdList.x: 0
*orderIdList.y: 248
*orderIdList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderIdList.background: "LightSkyBlue3"
*orderIdList.listSizePolicy: "constant"
*orderIdList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*orderIdList.selectionPolicy: "browse_select"
*orderIdList.defaultActionCallback.source: public
*orderIdList.defaultActionCallback: order_show_orderItemsCb
*orderIdList.singleSelectionCallbackClientData: (XtPointer) 0x0
*orderIdList.browseSelectionCallback.source: public
*orderIdList.browseSelectionCallback: order_orderLists_selectionCb
*orderIdList.browseSelectionCallbackClientData: (XtPointer) 1

*userIdSW.class: scrolledWindow
*userIdSW.static: true
*userIdSW.name: userIdSW
*userIdSW.parent: order
*userIdSW.scrollingPolicy: "automatic"
*userIdSW.visualPolicy: "variable"
*userIdSW.scrollBarDisplayPolicy: "as_needed"
*userIdSW.shadowThickness: 0
*userIdSW.isCompound: "true"
*userIdSW.compoundIcon: "scrllist.xpm"
*userIdSW.compoundName: "scrolled_List"
*userIdSW.x: 114
*userIdSW.y: 304
*userIdSW.background: "#9ac0cd"
*userIdSW.height: 152
*userIdSW.width: 156

*userIdList.class: scrolledList
*userIdList.static: true
*userIdList.name: userIdList
*userIdList.parent: userIdSW
*userIdList.width: 156
*userIdList.height: 152
*userIdList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*userIdList.itemCount: 0
*userIdList.background: "LightSkyBlue3"
*userIdList.listSizePolicy: "constant"
*userIdList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*userIdList.selectionPolicy: "browse_select"
*userIdList.defaultActionCallback.source: public
*userIdList.defaultActionCallback: order_show_orderItemsCb
*userIdList.singleSelectionCallbackClientData: (XtPointer) 0x0
*userIdList.browseSelectionCallback.source: public
*userIdList.browseSelectionCallback: order_orderLists_selectionCb
*userIdList.browseSelectionCallbackClientData: (XtPointer) 2

*itemNoSW.class: scrolledWindow
*itemNoSW.static: true
*itemNoSW.name: itemNoSW
*itemNoSW.parent: order
*itemNoSW.scrollingPolicy: "automatic"
*itemNoSW.visualPolicy: "constant"
*itemNoSW.scrollBarDisplayPolicy: "as_needed"
*itemNoSW.shadowThickness: 0
*itemNoSW.isCompound: "true"
*itemNoSW.compoundIcon: "scrllist.xpm"
*itemNoSW.compoundName: "scrolled_List"
*itemNoSW.x: 12
*itemNoSW.y: 568
*itemNoSW.background: "#9ac0cd"
*itemNoSW.height: 152
*itemNoSW.width: 36

*itemNoList.class: scrolledList
*itemNoList.static: true
*itemNoList.name: itemNoList
*itemNoList.parent: itemNoSW
*itemNoList.width: 36
*itemNoList.height: 152
*itemNoList.background: "LightSkyBlue3"
*itemNoList.listSizePolicy: "constant"
*itemNoList.visibleItemCount: 8
*itemNoList.selectionPolicy: "extended_select"
*itemNoList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemNoList.extendedSelectionCallback.source: public
*itemNoList.extendedSelectionCallback: order_itemLists_selectionCb
*itemNoList.extendedSelectionCallbackClientData: (XtPointer) 1
*itemNoList.defaultActionCallback.source: public
*itemNoList.defaultActionCallback: order_browse_itemDetailsCb
*itemNoList.x: 0
*itemNoList.y: 570

*frameIdSW.class: scrolledWindow
*frameIdSW.static: true
*frameIdSW.name: frameIdSW
*frameIdSW.parent: order
*frameIdSW.scrollingPolicy: "automatic"
*frameIdSW.visualPolicy: "constant"
*frameIdSW.scrollBarDisplayPolicy: "as_needed"
*frameIdSW.shadowThickness: 0
*frameIdSW.isCompound: "true"
*frameIdSW.compoundIcon: "scrllist.xpm"
*frameIdSW.compoundName: "scrolled_List"
*frameIdSW.x: 114
*frameIdSW.y: 568
*frameIdSW.background: "#9ac0cd"
*frameIdSW.height: 152
*frameIdSW.width: 135

*frameIdList.class: scrolledList
*frameIdList.static: true
*frameIdList.name: frameIdList
*frameIdList.parent: frameIdSW
*frameIdList.width: 135
*frameIdList.height: 152
*frameIdList.background: "LightSkyBlue3"
*frameIdList.x: 0
*frameIdList.y: 570
*frameIdList.listSizePolicy: "constant"
*frameIdList.visibleItemCount: 8
*frameIdList.selectionPolicy: "extended_select"
*frameIdList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*frameIdList.extendedSelectionCallback.source: public
*frameIdList.extendedSelectionCallback: order_itemLists_selectionCb
*frameIdList.extendedSelectionCallbackClientData: (XtPointer) 2
*frameIdList.defaultActionCallback.source: public
*frameIdList.defaultActionCallback: order_browse_itemDetailsCb

*itemStatusSW.class: scrolledWindow
*itemStatusSW.static: true
*itemStatusSW.name: itemStatusSW
*itemStatusSW.parent: order
*itemStatusSW.scrollingPolicy: "automatic"
*itemStatusSW.visualPolicy: "constant"
*itemStatusSW.scrollBarDisplayPolicy: "as_needed"
*itemStatusSW.shadowThickness: 0
*itemStatusSW.isCompound: "true"
*itemStatusSW.compoundIcon: "scrllist.xpm"
*itemStatusSW.compoundName: "scrolled_List"
*itemStatusSW.x: 600
*itemStatusSW.y: 568
*itemStatusSW.background: "#9ac0cd"
*itemStatusSW.height: 152
*itemStatusSW.width: 120

*itemStatusList.class: scrolledList
*itemStatusList.static: true
*itemStatusList.name: itemStatusList
*itemStatusList.parent: itemStatusSW
*itemStatusList.width: 120
*itemStatusList.height: 152
*itemStatusList.background: "LightSkyBlue3"
*itemStatusList.x: 0
*itemStatusList.y: 570
*itemStatusList.listSizePolicy: "constant"
*itemStatusList.visibleItemCount: 8
*itemStatusList.selectionPolicy: "extended_select"
*itemStatusList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemStatusList.extendedSelectionCallback.source: public
*itemStatusList.extendedSelectionCallback: order_itemLists_selectionCb
*itemStatusList.extendedSelectionCallbackClientData: (XtPointer) 7
*itemStatusList.defaultActionCallback.source: public
*itemStatusList.defaultActionCallback: order_browse_itemDetailsCb
*itemStatusList.itemCount: 0

*procTypeSW.class: scrolledWindow
*procTypeSW.static: true
*procTypeSW.name: procTypeSW
*procTypeSW.parent: order
*procTypeSW.scrollingPolicy: "automatic"
*procTypeSW.visualPolicy: "constant"
*procTypeSW.scrollBarDisplayPolicy: "as_needed"
*procTypeSW.shadowThickness: 0
*procTypeSW.isCompound: "true"
*procTypeSW.compoundIcon: "scrllist.xpm"
*procTypeSW.compoundName: "scrolled_List"
*procTypeSW.x: 295
*procTypeSW.y: 568
*procTypeSW.background: "#9ac0cd"
*procTypeSW.height: 152
*procTypeSW.width: 172

*procTypeList.class: scrolledList
*procTypeList.static: true
*procTypeList.name: procTypeList
*procTypeList.parent: procTypeSW
*procTypeList.width: 172
*procTypeList.height: 152
*procTypeList.background: "LightSkyBlue3"
*procTypeList.x: 0
*procTypeList.y: 570
*procTypeList.listSizePolicy: "constant"
*procTypeList.visibleItemCount: 8
*procTypeList.selectionPolicy: "extended_select"
*procTypeList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*procTypeList.extendedSelectionCallback.source: public
*procTypeList.extendedSelectionCallback: order_itemLists_selectionCb
*procTypeList.extendedSelectionCallbackClientData: (XtPointer) 4
*procTypeList.defaultActionCallback.source: public
*procTypeList.defaultActionCallback: order_browse_itemDetailsCb
*procTypeList.itemCount: 0

*validateSW.class: scrolledWindow
*validateSW.static: true
*validateSW.name: validateSW
*validateSW.parent: order
*validateSW.scrollingPolicy: "automatic"
*validateSW.visualPolicy: "constant"
*validateSW.scrollBarDisplayPolicy: "as_needed"
*validateSW.shadowThickness: 0
*validateSW.isCompound: "true"
*validateSW.compoundIcon: "scrllist.xpm"
*validateSW.compoundName: "scrolled_List"
*validateSW.x: 917
*validateSW.y: 568
*validateSW.background: "#9ac0cd"
*validateSW.height: 152
*validateSW.width: 35

*validateList.class: scrolledList
*validateList.static: true
*validateList.name: validateList
*validateList.parent: validateSW
*validateList.width: 35
*validateList.height: 152
*validateList.background: "LightSkyBlue3"
*validateList.x: 0
*validateList.y: 570
*validateList.listSizePolicy: "constant"
*validateList.visibleItemCount: 8
*validateList.selectionPolicy: "extended_select"
*validateList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*validateList.extendedSelectionCallback.source: public
*validateList.extendedSelectionCallback: order_itemLists_selectionCb
*validateList.extendedSelectionCallbackClientData: (XtPointer) 11
*validateList.defaultActionCallback.source: public
*validateList.defaultActionCallback: order_browse_itemDetailsCb

*shipSW.class: scrolledWindow
*shipSW.static: true
*shipSW.name: shipSW
*shipSW.parent: order
*shipSW.scrollingPolicy: "automatic"
*shipSW.visualPolicy: "constant"
*shipSW.scrollBarDisplayPolicy: "as_needed"
*shipSW.shadowThickness: 0
*shipSW.isCompound: "true"
*shipSW.compoundIcon: "scrllist.xpm"
*shipSW.compoundName: "scrolled_List"
*shipSW.x: 957
*shipSW.y: 568
*shipSW.background: "#9ac0cd"
*shipSW.width: 35
*shipSW.height: 152

*shipList.class: scrolledList
*shipList.static: true
*shipList.name: shipList
*shipList.parent: shipSW
*shipList.width: 35
*shipList.height: 152
*shipList.background: "LightSkyBlue3"
*shipList.x: 0
*shipList.y: 570
*shipList.listSizePolicy: "constant"
*shipList.visibleItemCount: 8
*shipList.selectionPolicy: "extended_select"
*shipList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*shipList.extendedSelectionCallback.source: public
*shipList.extendedSelectionCallback: order_itemLists_selectionCb
*shipList.extendedSelectionCallbackClientData: (XtPointer) 12
*shipList.defaultActionCallback.source: public
*shipList.defaultActionCallback: order_browse_itemDetailsCb

*billSW.class: scrolledWindow
*billSW.static: true
*billSW.name: billSW
*billSW.parent: order
*billSW.scrollingPolicy: "automatic"
*billSW.visualPolicy: "constant"
*billSW.scrollBarDisplayPolicy: "as_needed"
*billSW.shadowThickness: 0
*billSW.isCompound: "true"
*billSW.compoundIcon: "scrllist.xpm"
*billSW.compoundName: "scrolled_List"
*billSW.x: 997
*billSW.y: 568
*billSW.background: "#9ac0cd"
*billSW.height: 152
*billSW.width: 35

*billList.class: scrolledList
*billList.static: true
*billList.name: billList
*billList.parent: billSW
*billList.width: 35
*billList.height: 152
*billList.background: "LightSkyBlue3"
*billList.x: 0
*billList.y: 570
*billList.listSizePolicy: "constant"
*billList.visibleItemCount: 8
*billList.selectionPolicy: "extended_select"
*billList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*billList.extendedSelectionCallback.source: public
*billList.extendedSelectionCallback: order_itemLists_selectionCb
*billList.extendedSelectionCallbackClientData: (XtPointer) 13
*billList.defaultActionCallback.source: public
*billList.defaultActionCallback: order_browse_itemDetailsCb

*orderDummySW.class: scrolledWindow
*orderDummySW.static: true
*orderDummySW.name: orderDummySW
*orderDummySW.parent: order
*orderDummySW.scrollingPolicy: "application_defined"
*orderDummySW.visualPolicy: "variable"
*orderDummySW.scrollBarDisplayPolicy: "static"
*orderDummySW.shadowThickness: 0
*orderDummySW.isCompound: "true"
*orderDummySW.compoundIcon: "scrllist.xpm"
*orderDummySW.compoundName: "scrolled_List"
*orderDummySW.x: 1018
*orderDummySW.y: 304
*orderDummySW.height: 166
*orderDummySW.width: 15
*orderDummySW.background: "#9ac0cd"

*orderDummyList.class: scrolledList
*orderDummyList.static: true
*orderDummyList.name: orderDummyList
*orderDummyList.parent: orderDummySW
*orderDummyList.width: 2
*orderDummyList.height: 152
*orderDummyList.listSizePolicy: "constant"
*orderDummyList.mappedWhenManaged: "false"
*orderDummyList.scrollBarDisplayPolicy: "static"
*orderDummyList.visibleItemCount: 8
*orderDummyList.selectionPolicy: "browse_select"
*orderDummyList.shadowThickness: 1
*orderDummyList.background: "LightSkyBlue3"
*orderDummyList.doubleClickInterval: 0

*itemDummySW.class: scrolledWindow
*itemDummySW.static: true
*itemDummySW.name: itemDummySW
*itemDummySW.parent: order
*itemDummySW.scrollingPolicy: "application_defined"
*itemDummySW.visualPolicy: "variable"
*itemDummySW.scrollBarDisplayPolicy: "static"
*itemDummySW.shadowThickness: 0
*itemDummySW.isCompound: "true"
*itemDummySW.compoundIcon: "scrllist.xpm"
*itemDummySW.compoundName: "scrolled_List"
*itemDummySW.x: 1033
*itemDummySW.y: 568
*itemDummySW.background: "#9ac0cd"
*itemDummySW.height: 167
*itemDummySW.width: 15

*itemDummyList.class: scrolledList
*itemDummyList.static: true
*itemDummyList.name: itemDummyList
*itemDummyList.parent: itemDummySW
*itemDummyList.width: 2
*itemDummyList.height: 152
*itemDummyList.background: "LightSkyBlue3"
*itemDummyList.scrollBarDisplayPolicy: "static"
*itemDummyList.visibleItemCount: 8
*itemDummyList.listSizePolicy: "constant"
*itemDummyList.selectionPolicy: "extended_select"
*itemDummyList.mappedWhenManaged: "false"
*itemDummyList.doubleClickInterval: 0

*label8.class: label
*label8.static: true
*label8.name: label8
*label8.parent: order
*label8.isCompound: "true"
*label8.compoundIcon: "label.xpm"
*label8.compoundName: "label_"
*label8.x: 600
*label8.y: 500
*label8.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label8.labelString: "Total Number of Items:"
*label8.background: "#9ac0cd"

*totalItemsTF.class: textField
*totalItemsTF.static: true
*totalItemsTF.name: totalItemsTF
*totalItemsTF.parent: order
*totalItemsTF.width: 96
*totalItemsTF.isCompound: "true"
*totalItemsTF.compoundIcon: "textfield.xpm"
*totalItemsTF.compoundName: "text_Field"
*totalItemsTF.x: 816
*totalItemsTF.y: 492
*totalItemsTF.height: 36
*totalItemsTF.cursorPositionVisible: "false"
*totalItemsTF.editable: "false"
*totalItemsTF.background: "LightSkyBlue3"
*totalItemsTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*searchParamSW.class: scrolledWindow
*searchParamSW.static: true
*searchParamSW.name: searchParamSW
*searchParamSW.parent: order
*searchParamSW.scrollingPolicy: "application_defined"
*searchParamSW.visualPolicy: "variable"
*searchParamSW.scrollBarDisplayPolicy: "static"
*searchParamSW.isCompound: "true"
*searchParamSW.compoundIcon: "scrltext.xpm"
*searchParamSW.compoundName: "scrolled_Text"
*searchParamSW.x: 16
*searchParamSW.y: 96
*searchParamSW.background: "LightSkyBlue3"
*searchParamSW.height: 112
*searchParamSW.width: 460

*searchParamST.class: scrolledText
*searchParamST.static: true
*searchParamST.name: searchParamST
*searchParamST.parent: searchParamSW
*searchParamST.width: 460
*searchParamST.height: 112
*searchParamST.background: "LightSkyBlue3"
*searchParamST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*searchParamST.scrollHorizontal: "false"
*searchParamST.editable: "false"
*searchParamST.columns: 20
*searchParamST.wordWrap: "true"
*searchParamST.editMode: "multi_line_edit"

*userInfoLBL.class: label
*userInfoLBL.static: true
*userInfoLBL.name: userInfoLBL
*userInfoLBL.parent: order
*userInfoLBL.isCompound: "true"
*userInfoLBL.compoundIcon: "label.xpm"
*userInfoLBL.compoundName: "label_"
*userInfoLBL.x: 580
*userInfoLBL.y: 72
*userInfoLBL.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*userInfoLBL.labelString: "Order User & Account Information:"
*userInfoLBL.background: "#9ac0cd"

*userInfoSW.class: scrolledWindow
*userInfoSW.static: true
*userInfoSW.name: userInfoSW
*userInfoSW.parent: order
*userInfoSW.scrollingPolicy: "application_defined"
*userInfoSW.visualPolicy: "variable"
*userInfoSW.scrollBarDisplayPolicy: "static"
*userInfoSW.isCompound: "true"
*userInfoSW.compoundIcon: "scrltext.xpm"
*userInfoSW.compoundName: "scrolled_Text"
*userInfoSW.x: 580
*userInfoSW.y: 96
*userInfoSW.background: "LightSkyBlue3"
*userInfoSW.width: 460

*userInfoST.class: scrolledText
*userInfoST.static: true
*userInfoST.name: userInfoST
*userInfoST.parent: userInfoSW
*userInfoST.width: 460
*userInfoST.height: 112
*userInfoST.background: "LightSkyBlue3"
*userInfoST.scrollHorizontal: "false"
*userInfoST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*userInfoST.editMode: "multi_line_edit"
*userInfoST.editable: "false"

*generatedSW.class: scrolledWindow
*generatedSW.static: true
*generatedSW.name: generatedSW
*generatedSW.parent: order
*generatedSW.scrollingPolicy: "automatic"
*generatedSW.visualPolicy: "constant"
*generatedSW.scrollBarDisplayPolicy: "as_needed"
*generatedSW.shadowThickness: 0
*generatedSW.isCompound: "true"
*generatedSW.compoundIcon: "scrllist.xpm"
*generatedSW.compoundName: "scrolled_List"
*generatedSW.x: 788
*generatedSW.y: 304
*generatedSW.width: 50
*generatedSW.height: 152

*generatedList.class: scrolledList
*generatedList.static: true
*generatedList.name: generatedList
*generatedList.parent: generatedSW
*generatedList.width: 50
*generatedList.height: 152
*generatedList.background: "LightSkyBlue3"
*generatedList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*generatedList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*generatedList.selectionPolicy: "browse_select"
*generatedList.visibleItemCount: 8
*generatedList.listSizePolicy: "constant"
*generatedList.defaultActionCallback.source: public
*generatedList.defaultActionCallback: order_show_orderItemsCb
*generatedList.singleSelectionCallbackClientData: (XtPointer) 0x0
*generatedList.browseSelectionCallback.source: public
*generatedList.browseSelectionCallback: order_orderLists_selectionCb
*generatedList.browseSelectionCallbackClientData: (XtPointer) 8

*orderQlkSW.class: scrolledWindow
*orderQlkSW.static: true
*orderQlkSW.name: orderQlkSW
*orderQlkSW.parent: order
*orderQlkSW.scrollingPolicy: "automatic"
*orderQlkSW.visualPolicy: "constant"
*orderQlkSW.scrollBarDisplayPolicy: "as_needed"
*orderQlkSW.shadowThickness: 0
*orderQlkSW.isCompound: "true"
*orderQlkSW.compoundIcon: "scrllist.xpm"
*orderQlkSW.compoundName: "scrolled_List"
*orderQlkSW.x: 520
*orderQlkSW.y: 304
*orderQlkSW.width: 30
*orderQlkSW.height: 152

*orderQlkList.class: scrolledList
*orderQlkList.static: true
*orderQlkList.name: orderQlkList
*orderQlkList.parent: orderQlkSW
*orderQlkList.width: 30
*orderQlkList.height: 152
*orderQlkList.background: "LightSkyBlue3"
*orderQlkList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderQlkList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*orderQlkList.listSizePolicy: "constant"
*orderQlkList.selectionPolicy: "browse_select"
*orderQlkList.visibleItemCount: 8
*orderQlkList.defaultActionCallback.source: public
*orderQlkList.defaultActionCallback: order_show_orderItemsCb
*orderQlkList.singleSelectionCallbackClientData: (XtPointer) 0x0
*orderQlkList.browseSelectionCallback.source: public
*orderQlkList.browseSelectionCallback: order_orderLists_selectionCb
*orderQlkList.browseSelectionCallbackClientData: (XtPointer) 5

*holdSW.class: scrolledWindow
*holdSW.static: true
*holdSW.name: holdSW
*holdSW.parent: order
*holdSW.scrollingPolicy: "automatic"
*holdSW.visualPolicy: "constant"
*holdSW.scrollBarDisplayPolicy: "as_needed"
*holdSW.shadowThickness: 0
*holdSW.isCompound: "true"
*holdSW.compoundIcon: "scrllist.xpm"
*holdSW.compoundName: "scrolled_List"
*holdSW.x: 907
*holdSW.y: 304
*holdSW.width: 50
*holdSW.height: 152

*holdList.class: scrolledList
*holdList.static: true
*holdList.name: holdList
*holdList.parent: holdSW
*holdList.width: 50
*holdList.height: 152
*holdList.background: "LightSkyBlue3"
*holdList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*holdList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*holdList.selectionPolicy: "browse_select"
*holdList.visibleItemCount: 8
*holdList.listSizePolicy: "constant"
*holdList.defaultActionCallback.source: public
*holdList.defaultActionCallback: order_show_orderItemsCb
*holdList.singleSelectionCallbackClientData: (XtPointer) 0x0
*holdList.browseSelectionCallback.source: public
*holdList.browseSelectionCallback: order_orderLists_selectionCb
*holdList.browseSelectionCallbackClientData: (XtPointer) 10

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: order
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 846
*label2.y: 280
*label2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label2.labelString: "Media"
*label2.background: "#9ac0cd"

*label9.class: label
*label9.static: true
*label9.name: label9
*label9.parent: order
*label9.isCompound: "true"
*label9.compoundIcon: "label.xpm"
*label9.compoundName: "label_"
*label9.x: 793
*label9.y: 280
*label9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label9.labelString: "Gen"
*label9.background: "#9ac0cd"

*label10.class: label
*label10.static: true
*label10.name: label10
*label10.parent: order
*label10.isCompound: "true"
*label10.compoundIcon: "label.xpm"
*label10.compoundName: "label_"
*label10.x: 434
*label10.y: 280
*label10.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label10.labelString: "Priority"
*label10.background: "#9ac0cd"

*orderPriSW.class: scrolledWindow
*orderPriSW.static: true
*orderPriSW.name: orderPriSW
*orderPriSW.parent: order
*orderPriSW.scrollingPolicy: "automatic"
*orderPriSW.visualPolicy: "constant"
*orderPriSW.scrollBarDisplayPolicy: "as_needed"
*orderPriSW.shadowThickness: 0
*orderPriSW.isCompound: "true"
*orderPriSW.compoundIcon: "scrllist.xpm"
*orderPriSW.compoundName: "scrolled_List"
*orderPriSW.x: 426
*orderPriSW.y: 304
*orderPriSW.height: 152
*orderPriSW.width: 86

*orderPriorityList.class: scrolledList
*orderPriorityList.static: true
*orderPriorityList.name: orderPriorityList
*orderPriorityList.parent: orderPriSW
*orderPriorityList.width: 86
*orderPriorityList.height: 152
*orderPriorityList.background: "LightSkyBlue3"
*orderPriorityList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderPriorityList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*orderPriorityList.listSizePolicy: "constant"
*orderPriorityList.selectionPolicy: "browse_select"
*orderPriorityList.visibleItemCount: 8
*orderPriorityList.defaultActionCallback.source: public
*orderPriorityList.defaultActionCallback: order_show_orderItemsCb
*orderPriorityList.singleSelectionCallbackClientData: (XtPointer) 0x0
*orderPriorityList.browseSelectionCallback.source: public
*orderPriorityList.browseSelectionCallback: order_orderLists_selectionCb
*orderPriorityList.browseSelectionCallbackClientData: (XtPointer) 4

*totalSW.class: scrolledWindow
*totalSW.static: true
*totalSW.name: totalSW
*totalSW.parent: order
*totalSW.scrollingPolicy: "automatic"
*totalSW.visualPolicy: "constant"
*totalSW.scrollBarDisplayPolicy: "as_needed"
*totalSW.shadowThickness: 0
*totalSW.isCompound: "true"
*totalSW.compoundIcon: "scrllist.xpm"
*totalSW.compoundName: "scrolled_List"
*totalSW.x: 728
*totalSW.y: 304
*totalSW.height: 152
*totalSW.width: 50

*totalList.class: scrolledList
*totalList.static: true
*totalList.name: totalList
*totalList.parent: totalSW
*totalList.width: 50
*totalList.height: 152
*totalList.background: "LightSkyBlue3"
*totalList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*totalList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*totalList.selectionPolicy: "browse_select"
*totalList.visibleItemCount: 8
*totalList.listSizePolicy: "constant"
*totalList.defaultActionCallback.source: public
*totalList.defaultActionCallback: order_show_orderItemsCb
*totalList.singleSelectionCallbackClientData: (XtPointer) 0x0
*totalList.browseSelectionCallback.source: public
*totalList.browseSelectionCallback: order_orderLists_selectionCb
*totalList.browseSelectionCallbackClientData: (XtPointer) 7

*label12.class: label
*label12.static: true
*label12.name: label12
*label12.parent: order
*label12.isCompound: "true"
*label12.compoundIcon: "label.xpm"
*label12.compoundName: "label_"
*label12.x: 810
*label12.y: 240
*label12.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label12.labelString: "Item Statistics"
*label12.background: "DarkSlateBlue"
*label12.foreground: "WhiteSmoke"

*label14.class: label
*label14.static: true
*label14.name: label14
*label14.parent: order
*label14.isCompound: "true"
*label14.compoundIcon: "label.xpm"
*label14.compoundName: "label_"
*label14.x: 516
*label14.y: 280
*label14.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label14.labelString: "QLK"
*label14.background: "#9ac0cd"

*label16.class: label
*label16.static: true
*label16.name: label16
*label16.parent: order
*label16.isCompound: "true"
*label16.compoundIcon: "label.xpm"
*label16.compoundName: "label_"
*label16.x: 900
*label16.y: 280
*label16.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label16.labelString: "Cancld"
*label16.background: "#9ac0cd"

*label19.class: label
*label19.static: true
*label19.name: label19
*label19.parent: order
*label19.isCompound: "true"
*label19.compoundIcon: "label.xpm"
*label19.compoundName: "label_"
*label19.x: 962
*label19.y: 280
*label19.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label19.labelString: "Err/Fail"
*label19.background: "#9ac0cd"

*label33.class: label
*label33.static: true
*label33.name: label33
*label33.parent: order
*label33.isCompound: "true"
*label33.compoundIcon: "label.xpm"
*label33.compoundName: "label_"
*label33.x: 248
*label33.y: 543
*label33.width: 44
*label33.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label33.labelString: "SAT"
*label33.background: "#9ac0cd"

*itemPriSW.class: scrolledWindow
*itemPriSW.static: true
*itemPriSW.name: itemPriSW
*itemPriSW.parent: order
*itemPriSW.scrollingPolicy: "automatic"
*itemPriSW.visualPolicy: "constant"
*itemPriSW.scrollBarDisplayPolicy: "as_needed"
*itemPriSW.shadowThickness: 0
*itemPriSW.isCompound: "true"
*itemPriSW.compoundIcon: "scrllist.xpm"
*itemPriSW.compoundName: "scrolled_List"
*itemPriSW.x: 473
*itemPriSW.y: 568
*itemPriSW.height: 152
*itemPriSW.width: 86

*itemPriorityList.class: scrolledList
*itemPriorityList.static: true
*itemPriorityList.name: itemPriorityList
*itemPriorityList.parent: itemPriSW
*itemPriorityList.width: 86
*itemPriorityList.height: 152
*itemPriorityList.background: "LightSkyBlue3"
*itemPriorityList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemPriorityList.selectionPolicy: "extended_select"
*itemPriorityList.visibleItemCount: 8
*itemPriorityList.extendedSelectionCallback.source: public
*itemPriorityList.extendedSelectionCallback: order_itemLists_selectionCb
*itemPriorityList.extendedSelectionCallbackClientData: (XtPointer) 5
*itemPriorityList.listSizePolicy: "constant"
*itemPriorityList.defaultActionCallback.source: public
*itemPriorityList.defaultActionCallback: order_browse_itemDetailsCb
*itemPriorityList.x: 0
*itemPriorityList.y: 570

*label34.class: label
*label34.static: true
*label34.name: label34
*label34.parent: order
*label34.isCompound: "true"
*label34.compoundIcon: "label.xpm"
*label34.compoundName: "label_"
*label34.x: 307
*label34.y: 543
*label34.width: 144
*label34.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label34.labelString: "Processing Type"
*label34.background: "#9ac0cd"

*satSW.class: scrolledWindow
*satSW.static: true
*satSW.name: satSW
*satSW.parent: order
*satSW.scrollingPolicy: "automatic"
*satSW.visualPolicy: "constant"
*satSW.scrollBarDisplayPolicy: "as_needed"
*satSW.shadowThickness: 0
*satSW.isCompound: "true"
*satSW.compoundIcon: "scrllist.xpm"
*satSW.compoundName: "scrolled_List"
*satSW.x: 254
*satSW.y: 568
*satSW.height: 152
*satSW.width: 35

*satList.class: scrolledList
*satList.static: true
*satList.name: satList
*satList.parent: satSW
*satList.width: 35
*satList.height: 152
*satList.background: "LightSkyBlue3"
*satList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*satList.selectionPolicy: "extended_select"
*satList.visibleItemCount: 8
*satList.extendedSelectionCallback.source: public
*satList.extendedSelectionCallback: order_itemLists_selectionCb
*satList.extendedSelectionCallbackClientData: (XtPointer) 3
*satList.listSizePolicy: "constant"
*satList.defaultActionCallback.source: public
*satList.defaultActionCallback: order_browse_itemDetailsCb
*satList.x: 0
*satList.y: 570

*label11.class: label
*label11.static: true
*label11.name: label11
*label11.parent: order
*label11.isCompound: "true"
*label11.compoundIcon: "label.xpm"
*label11.compoundName: "label_"
*label11.x: 557
*label11.y: 543
*label11.width: 43
*label11.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label11.labelString: "QLK"
*label11.background: "#9ac0cd"

*itemQlkSW.class: scrolledWindow
*itemQlkSW.static: true
*itemQlkSW.name: itemQlkSW
*itemQlkSW.parent: order
*itemQlkSW.scrollingPolicy: "automatic"
*itemQlkSW.visualPolicy: "constant"
*itemQlkSW.scrollBarDisplayPolicy: "as_needed"
*itemQlkSW.shadowThickness: 0
*itemQlkSW.isCompound: "true"
*itemQlkSW.compoundIcon: "scrllist.xpm"
*itemQlkSW.compoundName: "scrolled_List"
*itemQlkSW.x: 565
*itemQlkSW.y: 568
*itemQlkSW.height: 152
*itemQlkSW.width: 29

*itemQlkList.class: scrolledList
*itemQlkList.static: true
*itemQlkList.name: itemQlkList
*itemQlkList.parent: itemQlkSW
*itemQlkList.width: 29
*itemQlkList.height: 152
*itemQlkList.background: "LightSkyBlue3"
*itemQlkList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemQlkList.selectionPolicy: "extended_select"
*itemQlkList.visibleItemCount: 8
*itemQlkList.extendedSelectionCallback.source: public
*itemQlkList.extendedSelectionCallback: order_itemLists_selectionCb
*itemQlkList.extendedSelectionCallbackClientData: (XtPointer) 6
*itemQlkList.listSizePolicy: "constant"
*itemQlkList.defaultActionCallback.source: public
*itemQlkList.defaultActionCallback: order_browse_itemDetailsCb
*itemQlkList.x: 0
*itemQlkList.y: 570

*separator5.class: separator
*separator5.static: true
*separator5.name: separator5
*separator5.parent: order
*separator5.width: 12
*separator5.height: 256
*separator5.isCompound: "true"
*separator5.compoundIcon: "sep.xpm"
*separator5.compoundName: "separator_"
*separator5.x: 692
*separator5.y: 220
*separator5.orientation: "vertical"
*separator5.background: "#9ac0cd"
*separator5.separatorType: "shadow_etched_out"
*separator5.shadowThickness: 4

*errorSW.class: scrolledWindow
*errorSW.static: true
*errorSW.name: errorSW
*errorSW.parent: order
*errorSW.scrollingPolicy: "automatic"
*errorSW.visualPolicy: "constant"
*errorSW.scrollBarDisplayPolicy: "as_needed"
*errorSW.shadowThickness: 0
*errorSW.isCompound: "true"
*errorSW.compoundIcon: "scrllist.xpm"
*errorSW.compoundName: "scrolled_List"
*errorSW.x: 966
*errorSW.y: 304
*errorSW.height: 152
*errorSW.width: 50

*errorList.class: scrolledList
*errorList.static: true
*errorList.name: errorList
*errorList.parent: errorSW
*errorList.width: 50
*errorList.height: 152
*errorList.background: "LightSkyBlue3"
*errorList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*errorList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*errorList.selectionPolicy: "browse_select"
*errorList.visibleItemCount: 8
*errorList.listSizePolicy: "constant"
*errorList.defaultActionCallback.source: public
*errorList.defaultActionCallback: order_show_orderItemsCb
*errorList.singleSelectionCallbackClientData: (XtPointer) 0x0
*errorList.browseSelectionCallback.source: public
*errorList.browseSelectionCallback: order_orderLists_selectionCb
*errorList.browseSelectionCallbackClientData: (XtPointer) 11

*orderStatusSW.class: scrolledWindow
*orderStatusSW.static: true
*orderStatusSW.name: orderStatusSW
*orderStatusSW.parent: order
*orderStatusSW.scrollingPolicy: "automatic"
*orderStatusSW.visualPolicy: "constant"
*orderStatusSW.scrollBarDisplayPolicy: "as_needed"
*orderStatusSW.shadowThickness: 0
*orderStatusSW.isCompound: "true"
*orderStatusSW.compoundIcon: "scrllist.xpm"
*orderStatusSW.compoundName: "scrolled_List"
*orderStatusSW.x: 560
*orderStatusSW.y: 304
*orderStatusSW.height: 152
*orderStatusSW.width: 110

*orderStatusList.class: scrolledList
*orderStatusList.static: true
*orderStatusList.name: orderStatusList
*orderStatusList.parent: orderStatusSW
*orderStatusList.width: 110
*orderStatusList.height: 152
*orderStatusList.background: "LightSkyBlue3"
*orderStatusList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderStatusList.extendedSelectionCallbackClientData: (XtPointer) 0x0
*orderStatusList.selectionPolicy: "browse_select"
*orderStatusList.visibleItemCount: 8
*orderStatusList.listSizePolicy: "constant"
*orderStatusList.defaultActionCallback.source: public
*orderStatusList.defaultActionCallback: order_show_orderItemsCb
*orderStatusList.singleSelectionCallbackClientData: (XtPointer) 0x0
*orderStatusList.browseSelectionCallback.source: public
*orderStatusList.browseSelectionCallback: order_orderLists_selectionCb
*orderStatusList.browseSelectionCallbackClientData: (XtPointer) 6

*label39.class: label
*label39.static: true
*label39.name: label39
*label39.parent: order
*label39.isCompound: "true"
*label39.compoundIcon: "label.xpm"
*label39.compoundName: "label_"
*label39.x: 730
*label39.y: 543
*label39.width: 104
*label39.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label39.labelString: "Media Type"
*label39.background: "#9ac0cd"

*mediaSW.class: scrolledWindow
*mediaSW.static: true
*mediaSW.name: mediaSW
*mediaSW.parent: order
*mediaSW.scrollingPolicy: "automatic"
*mediaSW.visualPolicy: "constant"
*mediaSW.scrollBarDisplayPolicy: "as_needed"
*mediaSW.shadowThickness: 0
*mediaSW.isCompound: "true"
*mediaSW.compoundIcon: "scrllist.xpm"
*mediaSW.compoundName: "scrolled_List"
*mediaSW.x: 726
*mediaSW.y: 568
*mediaSW.width: 117
*mediaSW.height: 152

*mediaList.class: scrolledList
*mediaList.static: true
*mediaList.name: mediaList
*mediaList.parent: mediaSW
*mediaList.width: 117
*mediaList.height: 152
*mediaList.background: "LightSkyBlue3"
*mediaList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*mediaList.selectionPolicy: "extended_select"
*mediaList.visibleItemCount: 8
*mediaList.extendedSelectionCallback.source: public
*mediaList.extendedSelectionCallback: order_itemLists_selectionCb
*mediaList.extendedSelectionCallbackClientData: (XtPointer) 8
*mediaList.listSizePolicy: "constant"
*mediaList.defaultActionCallback.source: public
*mediaList.defaultActionCallback: order_browse_itemDetailsCb
*mediaList.x: 0
*mediaList.y: 570
*mediaList.itemCount: 0

*label35.class: label
*label35.static: true
*label35.name: label35
*label35.parent: order
*label35.isCompound: "true"
*label35.compoundIcon: "label.xpm"
*label35.compoundName: "label_"
*label35.x: 851
*label35.y: 543
*label35.width: 52
*label35.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label35.labelString: "Cost"
*label35.background: "#9ac0cd"

*costSW.class: scrolledWindow
*costSW.static: true
*costSW.name: costSW
*costSW.parent: order
*costSW.scrollingPolicy: "automatic"
*costSW.visualPolicy: "constant"
*costSW.scrollBarDisplayPolicy: "as_needed"
*costSW.shadowThickness: 0
*costSW.isCompound: "true"
*costSW.compoundIcon: "scrllist.xpm"
*costSW.compoundName: "scrolled_List"
*costSW.x: 849
*costSW.y: 568
*costSW.height: 152
*costSW.width: 62

*costList.class: scrolledList
*costList.static: true
*costList.name: costList
*costList.parent: costSW
*costList.width: 62
*costList.height: 152
*costList.background: "LightSkyBlue3"
*costList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*costList.selectionPolicy: "extended_select"
*costList.visibleItemCount: 8
*costList.extendedSelectionCallback.source: public
*costList.extendedSelectionCallback: order_itemLists_selectionCb
*costList.extendedSelectionCallbackClientData: (XtPointer) 10
*costList.listSizePolicy: "constant"
*costList.defaultActionCallback.source: public
*costList.defaultActionCallback: order_browse_itemDetailsCb
*costList.x: 0
*costList.y: 570

*label43.class: label
*label43.static: true
*label43.name: label43
*label43.parent: order
*label43.isCompound: "true"
*label43.compoundIcon: "label.xpm"
*label43.compoundName: "label_"
*label43.x: 392
*label43.y: 40
*label43.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label43.labelString: "Order   Production   Screen"
*label43.rightAttachment: "attach_none"
*label43.background: "#9ac0cd"
*label43.leftAttachment: "attach_form"
*label43.leftOffset: 400

*label5.class: label
*label5.static: true
*label5.name: label5
*label5.parent: order
*label5.isCompound: "true"
*label5.compoundIcon: "label.xpm"
*label5.compoundName: "label_"
*label5.x: 53
*label5.y: 543
*label5.width: 56
*label5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label5.labelString: "Type"
*label5.background: "#9ac0cd"

*typeSW.class: scrolledWindow
*typeSW.static: true
*typeSW.name: typeSW
*typeSW.parent: order
*typeSW.scrollingPolicy: "automatic"
*typeSW.visualPolicy: "constant"
*typeSW.scrollBarDisplayPolicy: "as_needed"
*typeSW.shadowThickness: 0
*typeSW.isCompound: "true"
*typeSW.compoundIcon: "scrllist.xpm"
*typeSW.compoundName: "scrolled_List"
*typeSW.x: 54
*typeSW.y: 568
*typeSW.background: "#9ac0cd"
*typeSW.height: 152
*typeSW.width: 55

*typeList.class: scrolledList
*typeList.static: true
*typeList.name: typeList
*typeList.parent: typeSW
*typeList.width: 55
*typeList.height: 152
*typeList.background: "LightSkyBlue3"
*typeList.x: 0
*typeList.y: 570
*typeList.listSizePolicy: "constant"
*typeList.visibleItemCount: 8
*typeList.selectionPolicy: "extended_select"
*typeList.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*typeList.extendedSelectionCallback.source: public
*typeList.extendedSelectionCallback: order_itemLists_selectionCb
*typeList.extendedSelectionCallbackClientData: (XtPointer) 9
*typeList.defaultActionCallback.source: public
*typeList.defaultActionCallback: order_browse_itemDetailsCb
*typeList.itemCount: 0

