
/*******************************************************************************
       ims_opOrder.h
       This header file is included by ims_opOrder.c

*******************************************************************************/

#ifndef	_IMS_OPORDER_INCLUDED
#define	_IMS_OPORDER_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	Uxorder;
	Widget	UxorderMB;
	Widget	UxorderMB_p1;
	Widget	UxorderMB_p1_b1;
	Widget	UxorderMB_p1_b8;
	Widget	UxsearchPB;
	Widget	UxorderMB_p1_b9;
	Widget	UxorderMB_p1_b6;
	Widget	UxorderMB_p1_top_b1;
	Widget	UxorderMB_p3;
	Widget	UxorderMB_p3_b1;
	Widget	UxorderMB_p3_top_b1;
	Widget	UxorderMB_p4;
	Widget	UxviewOrderDetailsMPB;
	Widget	UxorderMB_p4_b4;
	Widget	UxvalidateOrderMPB;
	Widget	UxorderMB_p4_b13;
	Widget	UxunvalidateOrderMPB;
	Widget	UxorderMB_p4_b14;
	Widget	UxupdateOrderPriorityMPB;
	Widget	UxorderMB_p4_b12;
	Widget	UxupdateOrderStatusMPB;
	Widget	UxorderMB_p4_b6;
	Widget	UxeditOrderCommentMPB;
	Widget	UxorderMB_p4_b11;
	Widget	UxprocessMediaMPB;
	Widget	UxorderMB_p4_b15;
	Widget	UxorderMB_p6;
	Widget	UxorderMB_p6_b1;
	Widget	UxorderMB_p6_b2;
	Widget	UxorderMB_p6_b3;
	Widget	UxshipOrderMPB;
	Widget	UxorderMB_p4_b17;
	Widget	UxorderMB_p7;
	Widget	UxorderMB_p7_b1;
	Widget	UxorderMB_p7_b2;
	Widget	UxorderMB_p7_b3;
	Widget	UxbillOrderMPB;
	Widget	UxorderMB_top_b1;
	Widget	UxorderMB_p5;
	Widget	UxviewItemDetailsMPB;
	Widget	UxorderMB_p5_b3;
	Widget	UxvalidateItemMPB;
	Widget	UxorderMB_p5_b8;
	Widget	UxunvalidateItemMPB;
	Widget	UxorderMB_p5_b13;
	Widget	UxupdateItemPriorityMPB;
	Widget	UxorderMB_p5_b11;
	Widget	UxupdateItemStatusMPB;
	Widget	UxorderMB_p5_b5;
	Widget	UxeditItemCommentMPB;
	Widget	UxorderMB_p5_b9;
	Widget	UxitemProcessMediaMPB;
	Widget	UxorderMB_p5_b15;
	Widget	UxorderMB_p8;
	Widget	UxorderMB_p8_b1;
	Widget	UxorderMB_p8_b2;
	Widget	UxorderMB_p8_b3;
	Widget	UxitemShipItemsMPB;
	Widget	UxorderMB_p5_b17;
	Widget	UxorderMB_p9;
	Widget	UxorderMB_p9_b1;
	Widget	UxorderMB_p9_b2;
	Widget	UxorderMB_p9_b3;
	Widget	UxitemBillItemsMPB;
	Widget	UxorderMB_p5_b19;
	Widget	UxrestartItemMPB;
	Widget	UxorderMB_top_b4;
	Widget	UxorderMB_p10;
	Widget	UxsaveResultsMPB;
	Widget	UxorderMB_p10_b2;
	Widget	UxrefreshSearchMPB;
	Widget	UxorderMB_p10_b4;
	Widget	UxprintScreenMPB;
	Widget	UxorderMB_top_b2;
	Widget	Uxseparator2;
	Widget	Uxseparator3;
	Widget	UxclosePB;
	Widget	Uxlabel1;
	Widget	UxorderIdTF;
	Widget	Uxlabel3;
	Widget	Uxlabel4;
	Widget	Uxlabel6;
	Widget	Uxlabel7;
	Widget	Uxlabel13;
	Widget	Uxlabel15;
	Widget	Uxlabel17;
	Widget	Uxlabel18;
	Widget	Uxlabel26;
	Widget	Uxlabel36;
	Widget	Uxlabel38;
	Widget	Uxlabel41;
	Widget	Uxlabel27;
	Widget	UxtotalOrdersTF;
	Widget	Uxseparator1;
	Widget	UxorderSearchParamLBL;
	Widget	UxrefreshPB;
	Widget	UxviewItemsPB;
	Widget	UxprintPB;
	Widget	UxreceivedSW;
	Widget	UxreceivedList;
	Widget	UxonlineSW;
	Widget	UxonlineList;
	Widget	UxorderIdSW;
	Widget	UxorderIdList;
	Widget	UxuserIdSW;
	Widget	UxuserIdList;
	Widget	UxitemNoSW;
	Widget	UxitemNoList;
	Widget	UxframeIdSW;
	Widget	UxframeIdList;
	Widget	UxitemStatusSW;
	Widget	UxitemStatusList;
	Widget	UxprocTypeSW;
	Widget	UxprocTypeList;
	Widget	UxvalidateSW;
	Widget	UxvalidateList;
	Widget	UxshipSW;
	Widget	UxshipList;
	Widget	UxbillSW;
	Widget	UxbillList;
	Widget	UxorderDummySW;
	Widget	UxorderDummyList;
	Widget	UxitemDummySW;
	Widget	UxitemDummyList;
	Widget	Uxlabel8;
	Widget	UxtotalItemsTF;
	Widget	UxsearchParamSW;
	Widget	UxsearchParamST;
	Widget	UxuserInfoLBL;
	Widget	UxuserInfoSW;
	Widget	UxuserInfoST;
	Widget	UxgeneratedSW;
	Widget	UxgeneratedList;
	Widget	UxorderQlkSW;
	Widget	UxorderQlkList;
	Widget	UxholdSW;
	Widget	UxholdList;
	Widget	Uxlabel2;
	Widget	Uxlabel9;
	Widget	Uxlabel10;
	Widget	UxorderPriSW;
	Widget	UxorderPriorityList;
	Widget	UxtotalSW;
	Widget	UxtotalList;
	Widget	Uxlabel12;
	Widget	Uxlabel14;
	Widget	Uxlabel16;
	Widget	Uxlabel19;
	Widget	Uxlabel33;
	Widget	UxitemPriSW;
	Widget	UxitemPriorityList;
	Widget	Uxlabel34;
	Widget	UxsatSW;
	Widget	UxsatList;
	Widget	Uxlabel11;
	Widget	UxitemQlkSW;
	Widget	UxitemQlkList;
	Widget	Uxseparator5;
	Widget	UxerrorSW;
	Widget	UxerrorList;
	Widget	UxorderStatusSW;
	Widget	UxorderStatusList;
	Widget	Uxlabel39;
	Widget	UxmediaSW;
	Widget	UxmediaList;
	Widget	Uxlabel35;
	Widget	UxcostSW;
	Widget	UxcostList;
	Widget	Uxlabel43;
	Widget	Uxlabel5;
	Widget	UxtypeSW;
	Widget	UxtypeList;
	swidget	UxUxParent;
} _UxCorder;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCorder               *UxOrderContext;
#define order                   UxOrderContext->Uxorder
#define orderMB                 UxOrderContext->UxorderMB
#define orderMB_p1              UxOrderContext->UxorderMB_p1
#define orderMB_p1_b1           UxOrderContext->UxorderMB_p1_b1
#define orderMB_p1_b8           UxOrderContext->UxorderMB_p1_b8
#define searchPB                UxOrderContext->UxsearchPB
#define orderMB_p1_b9           UxOrderContext->UxorderMB_p1_b9
#define orderMB_p1_b6           UxOrderContext->UxorderMB_p1_b6
#define orderMB_p1_top_b1       UxOrderContext->UxorderMB_p1_top_b1
#define orderMB_p3              UxOrderContext->UxorderMB_p3
#define orderMB_p3_b1           UxOrderContext->UxorderMB_p3_b1
#define orderMB_p3_top_b1       UxOrderContext->UxorderMB_p3_top_b1
#define orderMB_p4              UxOrderContext->UxorderMB_p4
#define viewOrderDetailsMPB     UxOrderContext->UxviewOrderDetailsMPB
#define orderMB_p4_b4           UxOrderContext->UxorderMB_p4_b4
#define validateOrderMPB        UxOrderContext->UxvalidateOrderMPB
#define orderMB_p4_b13          UxOrderContext->UxorderMB_p4_b13
#define unvalidateOrderMPB      UxOrderContext->UxunvalidateOrderMPB
#define orderMB_p4_b14          UxOrderContext->UxorderMB_p4_b14
#define updateOrderPriorityMPB  UxOrderContext->UxupdateOrderPriorityMPB
#define orderMB_p4_b12          UxOrderContext->UxorderMB_p4_b12
#define updateOrderStatusMPB    UxOrderContext->UxupdateOrderStatusMPB
#define orderMB_p4_b6           UxOrderContext->UxorderMB_p4_b6
#define editOrderCommentMPB     UxOrderContext->UxeditOrderCommentMPB
#define orderMB_p4_b11          UxOrderContext->UxorderMB_p4_b11
#define processMediaMPB         UxOrderContext->UxprocessMediaMPB
#define orderMB_p4_b15          UxOrderContext->UxorderMB_p4_b15
#define orderMB_p6              UxOrderContext->UxorderMB_p6
#define orderMB_p6_b1           UxOrderContext->UxorderMB_p6_b1
#define orderMB_p6_b2           UxOrderContext->UxorderMB_p6_b2
#define orderMB_p6_b3           UxOrderContext->UxorderMB_p6_b3
#define shipOrderMPB            UxOrderContext->UxshipOrderMPB
#define orderMB_p4_b17          UxOrderContext->UxorderMB_p4_b17
#define orderMB_p7              UxOrderContext->UxorderMB_p7
#define orderMB_p7_b1           UxOrderContext->UxorderMB_p7_b1
#define orderMB_p7_b2           UxOrderContext->UxorderMB_p7_b2
#define orderMB_p7_b3           UxOrderContext->UxorderMB_p7_b3
#define billOrderMPB            UxOrderContext->UxbillOrderMPB
#define orderMB_top_b1          UxOrderContext->UxorderMB_top_b1
#define orderMB_p5              UxOrderContext->UxorderMB_p5
#define viewItemDetailsMPB      UxOrderContext->UxviewItemDetailsMPB
#define orderMB_p5_b3           UxOrderContext->UxorderMB_p5_b3
#define validateItemMPB         UxOrderContext->UxvalidateItemMPB
#define orderMB_p5_b8           UxOrderContext->UxorderMB_p5_b8
#define unvalidateItemMPB       UxOrderContext->UxunvalidateItemMPB
#define orderMB_p5_b13          UxOrderContext->UxorderMB_p5_b13
#define updateItemPriorityMPB   UxOrderContext->UxupdateItemPriorityMPB
#define orderMB_p5_b11          UxOrderContext->UxorderMB_p5_b11
#define updateItemStatusMPB     UxOrderContext->UxupdateItemStatusMPB
#define orderMB_p5_b5           UxOrderContext->UxorderMB_p5_b5
#define editItemCommentMPB      UxOrderContext->UxeditItemCommentMPB
#define orderMB_p5_b9           UxOrderContext->UxorderMB_p5_b9
#define itemProcessMediaMPB     UxOrderContext->UxitemProcessMediaMPB
#define orderMB_p5_b15          UxOrderContext->UxorderMB_p5_b15
#define orderMB_p8              UxOrderContext->UxorderMB_p8
#define orderMB_p8_b1           UxOrderContext->UxorderMB_p8_b1
#define orderMB_p8_b2           UxOrderContext->UxorderMB_p8_b2
#define orderMB_p8_b3           UxOrderContext->UxorderMB_p8_b3
#define itemShipItemsMPB        UxOrderContext->UxitemShipItemsMPB
#define orderMB_p5_b17          UxOrderContext->UxorderMB_p5_b17
#define orderMB_p9              UxOrderContext->UxorderMB_p9
#define orderMB_p9_b1           UxOrderContext->UxorderMB_p9_b1
#define orderMB_p9_b2           UxOrderContext->UxorderMB_p9_b2
#define orderMB_p9_b3           UxOrderContext->UxorderMB_p9_b3
#define itemBillItemsMPB        UxOrderContext->UxitemBillItemsMPB
#define orderMB_p5_b19          UxOrderContext->UxorderMB_p5_b19
#define restartItemMPB          UxOrderContext->UxrestartItemMPB
#define orderMB_top_b4          UxOrderContext->UxorderMB_top_b4
#define orderMB_p10             UxOrderContext->UxorderMB_p10
#define saveResultsMPB          UxOrderContext->UxsaveResultsMPB
#define orderMB_p10_b2          UxOrderContext->UxorderMB_p10_b2
#define refreshSearchMPB        UxOrderContext->UxrefreshSearchMPB
#define orderMB_p10_b4          UxOrderContext->UxorderMB_p10_b4
#define printScreenMPB          UxOrderContext->UxprintScreenMPB
#define orderMB_top_b2          UxOrderContext->UxorderMB_top_b2
#define separator2              UxOrderContext->Uxseparator2
#define separator3              UxOrderContext->Uxseparator3
#define closePB                 UxOrderContext->UxclosePB
#define label1                  UxOrderContext->Uxlabel1
#define orderIdTF               UxOrderContext->UxorderIdTF
#define label3                  UxOrderContext->Uxlabel3
#define label4                  UxOrderContext->Uxlabel4
#define label6                  UxOrderContext->Uxlabel6
#define label7                  UxOrderContext->Uxlabel7
#define label13                 UxOrderContext->Uxlabel13
#define label15                 UxOrderContext->Uxlabel15
#define label17                 UxOrderContext->Uxlabel17
#define label18                 UxOrderContext->Uxlabel18
#define label26                 UxOrderContext->Uxlabel26
#define label36                 UxOrderContext->Uxlabel36
#define label38                 UxOrderContext->Uxlabel38
#define label41                 UxOrderContext->Uxlabel41
#define label27                 UxOrderContext->Uxlabel27
#define totalOrdersTF           UxOrderContext->UxtotalOrdersTF
#define separator1              UxOrderContext->Uxseparator1
#define orderSearchParamLBL     UxOrderContext->UxorderSearchParamLBL
#define refreshPB               UxOrderContext->UxrefreshPB
#define viewItemsPB             UxOrderContext->UxviewItemsPB
#define printPB                 UxOrderContext->UxprintPB
#define receivedSW              UxOrderContext->UxreceivedSW
#define receivedList            UxOrderContext->UxreceivedList
#define onlineSW                UxOrderContext->UxonlineSW
#define onlineList              UxOrderContext->UxonlineList
#define orderIdSW               UxOrderContext->UxorderIdSW
#define orderIdList             UxOrderContext->UxorderIdList
#define userIdSW                UxOrderContext->UxuserIdSW
#define userIdList              UxOrderContext->UxuserIdList
#define itemNoSW                UxOrderContext->UxitemNoSW
#define itemNoList              UxOrderContext->UxitemNoList
#define frameIdSW               UxOrderContext->UxframeIdSW
#define frameIdList             UxOrderContext->UxframeIdList
#define itemStatusSW            UxOrderContext->UxitemStatusSW
#define itemStatusList          UxOrderContext->UxitemStatusList
#define procTypeSW              UxOrderContext->UxprocTypeSW
#define procTypeList            UxOrderContext->UxprocTypeList
#define validateSW              UxOrderContext->UxvalidateSW
#define validateList            UxOrderContext->UxvalidateList
#define shipSW                  UxOrderContext->UxshipSW
#define shipList                UxOrderContext->UxshipList
#define billSW                  UxOrderContext->UxbillSW
#define billList                UxOrderContext->UxbillList
#define orderDummySW            UxOrderContext->UxorderDummySW
#define orderDummyList          UxOrderContext->UxorderDummyList
#define itemDummySW             UxOrderContext->UxitemDummySW
#define itemDummyList           UxOrderContext->UxitemDummyList
#define label8                  UxOrderContext->Uxlabel8
#define totalItemsTF            UxOrderContext->UxtotalItemsTF
#define searchParamSW           UxOrderContext->UxsearchParamSW
#define searchParamST           UxOrderContext->UxsearchParamST
#define userInfoLBL             UxOrderContext->UxuserInfoLBL
#define userInfoSW              UxOrderContext->UxuserInfoSW
#define userInfoST              UxOrderContext->UxuserInfoST
#define generatedSW             UxOrderContext->UxgeneratedSW
#define generatedList           UxOrderContext->UxgeneratedList
#define orderQlkSW              UxOrderContext->UxorderQlkSW
#define orderQlkList            UxOrderContext->UxorderQlkList
#define holdSW                  UxOrderContext->UxholdSW
#define holdList                UxOrderContext->UxholdList
#define label2                  UxOrderContext->Uxlabel2
#define label9                  UxOrderContext->Uxlabel9
#define label10                 UxOrderContext->Uxlabel10
#define orderPriSW              UxOrderContext->UxorderPriSW
#define orderPriorityList       UxOrderContext->UxorderPriorityList
#define totalSW                 UxOrderContext->UxtotalSW
#define totalList               UxOrderContext->UxtotalList
#define label12                 UxOrderContext->Uxlabel12
#define label14                 UxOrderContext->Uxlabel14
#define label16                 UxOrderContext->Uxlabel16
#define label19                 UxOrderContext->Uxlabel19
#define label33                 UxOrderContext->Uxlabel33
#define itemPriSW               UxOrderContext->UxitemPriSW
#define itemPriorityList        UxOrderContext->UxitemPriorityList
#define label34                 UxOrderContext->Uxlabel34
#define satSW                   UxOrderContext->UxsatSW
#define satList                 UxOrderContext->UxsatList
#define label11                 UxOrderContext->Uxlabel11
#define itemQlkSW               UxOrderContext->UxitemQlkSW
#define itemQlkList             UxOrderContext->UxitemQlkList
#define separator5              UxOrderContext->Uxseparator5
#define errorSW                 UxOrderContext->UxerrorSW
#define errorList               UxOrderContext->UxerrorList
#define orderStatusSW           UxOrderContext->UxorderStatusSW
#define orderStatusList         UxOrderContext->UxorderStatusList
#define label39                 UxOrderContext->Uxlabel39
#define mediaSW                 UxOrderContext->UxmediaSW
#define mediaList               UxOrderContext->UxmediaList
#define label35                 UxOrderContext->Uxlabel35
#define costSW                  UxOrderContext->UxcostSW
#define costList                UxOrderContext->UxcostList
#define label43                 UxOrderContext->Uxlabel43
#define label5                  UxOrderContext->Uxlabel5
#define typeSW                  UxOrderContext->UxtypeSW
#define typeList                UxOrderContext->UxtypeList
#define UxParent                UxOrderContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_order( swidget _UxUxParent );

#endif	/* _IMS_OPORDER_INCLUDED */
