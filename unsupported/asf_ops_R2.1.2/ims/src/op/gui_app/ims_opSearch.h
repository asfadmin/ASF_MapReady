
/*******************************************************************************
       ims_opSearch.h
       This header file is included by ims_opSearch.c

*******************************************************************************/

#ifndef	_IMS_OPSEARCH_INCLUDED
#define	_IMS_OPSEARCH_INCLUDED

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
	Widget	Uxsearch;
	Widget	UxsrchOrderSearchLB;
	Widget	UxsrchUserIdLB;
	Widget	UxsrchUserNameLB;
	Widget	UxsrchOrderReceivedFrame;
	Widget	UxsrchOrderReceivedForm;
	Widget	UxsrchOrderReceivedStartDateLB;
	Widget	UxsrchOrderReceivedEndDateLB;
	Widget	UxordrRecStartDateText;
	Widget	UxordrRecEndDateText;
	Widget	UxsrchOrderCompletedLB;
	Widget	UxsrchOrderCompletedFrame;
	Widget	UxsrchOrderCompletedForm;
	Widget	UxsrchOrderCompletedStartDateLB;
	Widget	UxsrchOrderCompletedEndDateLB;
	Widget	UxordrComEndDateText;
	Widget	UxordrComStartDateText;
	Widget	UxsrchUserNameFrame;
	Widget	UxsrchUserNameForm;
	Widget	UxsrchLastNameLB;
	Widget	UxsrchLastNameTF;
	Widget	UxsrchFirstNameLB;
	Widget	UxsrchFirstNameTF;
	Widget	UxsrchFirstNameLB1;
	Widget	UxmiddleNameTF;
	Widget	UxsrchItemStatusLB;
	Widget	UxsrchMediaTypeLB;
	Widget	UxsrchSeparator2;
	Widget	UxsrchProductionStatusLB;
	Widget	UxsrchSeparator3;
	Widget	UxsrchExecuteSearchPB;
	Widget	UxmenuBar1;
	Widget	UxmenuBar_p1;
	Widget	UxsrchWelcomeMPB;
	Widget	UxmenuBar_p1_b2;
	Widget	UxgotoOrderMPB;
	Widget	UxmenuBar_p1_b4;
	Widget	UxmenuBar_p1_b11;
	Widget	UxmenuBar_top_b1;
	Widget	UxmenuBar1_p2;
	Widget	UxsrchExecuteSearchMPB;
	Widget	UxmenuBar1_p2_b7;
	Widget	UxsrchClearSearchMPB;
	Widget	UxmenuBar1_p2_b8;
	Widget	UxsrchPrintScreenMPB;
	Widget	UxmenuBar1_top_b1;
	Widget	UxmenuBar1_p3;
	Widget	UxmenuBar1_p3_b1;
	Widget	UxmenuBar1_top_b2;
	Widget	UxsrchOrderIdLB;
	Widget	UxsrchProcessingOptionLB;
	Widget	UxsrchClearSearchPB;
	Widget	UxsrchCloseSearchPB;
	Widget	UxsrchOrderStatusLB;
	Widget	UxsrchSeparator1;
	Widget	UxsrchOrderReceivedLB;
	Widget	UxsrchAccountIdLB;
	Widget	UxsrchSeparator4;
	Widget	UxsrchOrderItemLB;
	Widget	UxsrchValidatedLB;
	Widget	UxsrchDebitedLB;
	Widget	UxsrchShippedLB;
	Widget	UxsrchBilledLB;
	Widget	UxsrchValidatedFrame;
	Widget	UxsrchValidatedRC;
	Widget	UxsrchValidatedYesTB;
	Widget	UxsrchValidatedNoTB;
	Widget	UxsrchValidatedBothTB;
	Widget	UxsrchShippedFrame;
	Widget	UxsrchShippedRC;
	Widget	UxsrchShippedYesTB;
	Widget	UxsrchShippedNoTB;
	Widget	UxsrchShippedBothTB;
	Widget	UxsrchDebitedFrame;
	Widget	UxsrchDebitedRC;
	Widget	UxsrchDebitedYesTB;
	Widget	UxsrchDebitedNoTB;
	Widget	UxsrchDebitedBothTB;
	Widget	UxsrchBilledFrame;
	Widget	UxsrchBilledRC;
	Widget	UxsrchBilledYesTB;
	Widget	UxsrchBilledNoTB;
	Widget	UxsrchBilledBothTB;
	Widget	UxmediaTypeSW;
	Widget	UxmediaTypeRC;
	Widget	UxprocessStatusSW;
	Widget	UxprocessStatusRC;
	Widget	UxsrchItemStatusSW;
	Widget	UxitemStatusRC;
	Widget	UxsrchOrderStatusSW;
	Widget	UxorderStatusRC;
	Widget	UxdummyTB;
	Widget	UxprocessOptionSW;
	Widget	UxprocessOptionRC;
	Widget	UxsrchOrderIdTF;
	Widget	UxaccountIdTF;
	Widget	UxuserIdTF;
	Widget	UxuserIdPB;
	Widget	UxaccountIdPB;
	Widget	UxoptionMenu_p1;
	Widget	UxprocessTypeAllPB;
	Widget	UxoptionMenu_p1_b4;
	Widget	UxoptionMenu_p_b1;
	Widget	UxoptionMenu_p1_b3;
	Widget	UxoptionMenu_p1_b2;
	Widget	UxprocessingTypeOM;
	Widget	UxsrchPrintSearchPB;
	Widget	UxsrchOrderItemLB1;
	Widget	UxsrchOrderItemLB2;
	Widget	UxpriorityOM_pane;
	Widget	UxorderPriorityAllPB;
	Widget	UxorderPriorityOM;
	Widget	Uxframe1;
	Widget	Uxlabel40;
	Widget	UxsrchItemStatusLB1;
	Widget	UxsrchItemTypeSW;
	Widget	UxitemTypeRC;
	swidget	UxUxParent;
} _UxCsearch;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCsearch              *UxSearchContext;
#define search                  UxSearchContext->Uxsearch
#define srchOrderSearchLB       UxSearchContext->UxsrchOrderSearchLB
#define srchUserIdLB            UxSearchContext->UxsrchUserIdLB
#define srchUserNameLB          UxSearchContext->UxsrchUserNameLB
#define srchOrderReceivedFrame  UxSearchContext->UxsrchOrderReceivedFrame
#define srchOrderReceivedForm   UxSearchContext->UxsrchOrderReceivedForm
#define srchOrderReceivedStartDateLB UxSearchContext->UxsrchOrderReceivedStartDateLB
#define srchOrderReceivedEndDateLB UxSearchContext->UxsrchOrderReceivedEndDateLB
#define ordrRecStartDateText    UxSearchContext->UxordrRecStartDateText
#define ordrRecEndDateText      UxSearchContext->UxordrRecEndDateText
#define srchOrderCompletedLB    UxSearchContext->UxsrchOrderCompletedLB
#define srchOrderCompletedFrame UxSearchContext->UxsrchOrderCompletedFrame
#define srchOrderCompletedForm  UxSearchContext->UxsrchOrderCompletedForm
#define srchOrderCompletedStartDateLB UxSearchContext->UxsrchOrderCompletedStartDateLB
#define srchOrderCompletedEndDateLB UxSearchContext->UxsrchOrderCompletedEndDateLB
#define ordrComEndDateText      UxSearchContext->UxordrComEndDateText
#define ordrComStartDateText    UxSearchContext->UxordrComStartDateText
#define srchUserNameFrame       UxSearchContext->UxsrchUserNameFrame
#define srchUserNameForm        UxSearchContext->UxsrchUserNameForm
#define srchLastNameLB          UxSearchContext->UxsrchLastNameLB
#define srchLastNameTF          UxSearchContext->UxsrchLastNameTF
#define srchFirstNameLB         UxSearchContext->UxsrchFirstNameLB
#define srchFirstNameTF         UxSearchContext->UxsrchFirstNameTF
#define srchFirstNameLB1        UxSearchContext->UxsrchFirstNameLB1
#define middleNameTF            UxSearchContext->UxmiddleNameTF
#define srchItemStatusLB        UxSearchContext->UxsrchItemStatusLB
#define srchMediaTypeLB         UxSearchContext->UxsrchMediaTypeLB
#define srchSeparator2          UxSearchContext->UxsrchSeparator2
#define srchProductionStatusLB  UxSearchContext->UxsrchProductionStatusLB
#define srchSeparator3          UxSearchContext->UxsrchSeparator3
#define srchExecuteSearchPB     UxSearchContext->UxsrchExecuteSearchPB
#define menuBar1                UxSearchContext->UxmenuBar1
#define menuBar_p1              UxSearchContext->UxmenuBar_p1
#define srchWelcomeMPB          UxSearchContext->UxsrchWelcomeMPB
#define menuBar_p1_b2           UxSearchContext->UxmenuBar_p1_b2
#define gotoOrderMPB            UxSearchContext->UxgotoOrderMPB
#define menuBar_p1_b4           UxSearchContext->UxmenuBar_p1_b4
#define menuBar_p1_b11          UxSearchContext->UxmenuBar_p1_b11
#define menuBar_top_b1          UxSearchContext->UxmenuBar_top_b1
#define menuBar1_p2             UxSearchContext->UxmenuBar1_p2
#define srchExecuteSearchMPB    UxSearchContext->UxsrchExecuteSearchMPB
#define menuBar1_p2_b7          UxSearchContext->UxmenuBar1_p2_b7
#define srchClearSearchMPB      UxSearchContext->UxsrchClearSearchMPB
#define menuBar1_p2_b8          UxSearchContext->UxmenuBar1_p2_b8
#define srchPrintScreenMPB      UxSearchContext->UxsrchPrintScreenMPB
#define menuBar1_top_b1         UxSearchContext->UxmenuBar1_top_b1
#define menuBar1_p3             UxSearchContext->UxmenuBar1_p3
#define menuBar1_p3_b1          UxSearchContext->UxmenuBar1_p3_b1
#define menuBar1_top_b2         UxSearchContext->UxmenuBar1_top_b2
#define srchOrderIdLB           UxSearchContext->UxsrchOrderIdLB
#define srchProcessingOptionLB  UxSearchContext->UxsrchProcessingOptionLB
#define srchClearSearchPB       UxSearchContext->UxsrchClearSearchPB
#define srchCloseSearchPB       UxSearchContext->UxsrchCloseSearchPB
#define srchOrderStatusLB       UxSearchContext->UxsrchOrderStatusLB
#define srchSeparator1          UxSearchContext->UxsrchSeparator1
#define srchOrderReceivedLB     UxSearchContext->UxsrchOrderReceivedLB
#define srchAccountIdLB         UxSearchContext->UxsrchAccountIdLB
#define srchSeparator4          UxSearchContext->UxsrchSeparator4
#define srchOrderItemLB         UxSearchContext->UxsrchOrderItemLB
#define srchValidatedLB         UxSearchContext->UxsrchValidatedLB
#define srchDebitedLB           UxSearchContext->UxsrchDebitedLB
#define srchShippedLB           UxSearchContext->UxsrchShippedLB
#define srchBilledLB            UxSearchContext->UxsrchBilledLB
#define srchValidatedFrame      UxSearchContext->UxsrchValidatedFrame
#define srchValidatedRC         UxSearchContext->UxsrchValidatedRC
#define srchValidatedYesTB      UxSearchContext->UxsrchValidatedYesTB
#define srchValidatedNoTB       UxSearchContext->UxsrchValidatedNoTB
#define srchValidatedBothTB     UxSearchContext->UxsrchValidatedBothTB
#define srchShippedFrame        UxSearchContext->UxsrchShippedFrame
#define srchShippedRC           UxSearchContext->UxsrchShippedRC
#define srchShippedYesTB        UxSearchContext->UxsrchShippedYesTB
#define srchShippedNoTB         UxSearchContext->UxsrchShippedNoTB
#define srchShippedBothTB       UxSearchContext->UxsrchShippedBothTB
#define srchDebitedFrame        UxSearchContext->UxsrchDebitedFrame
#define srchDebitedRC           UxSearchContext->UxsrchDebitedRC
#define srchDebitedYesTB        UxSearchContext->UxsrchDebitedYesTB
#define srchDebitedNoTB         UxSearchContext->UxsrchDebitedNoTB
#define srchDebitedBothTB       UxSearchContext->UxsrchDebitedBothTB
#define srchBilledFrame         UxSearchContext->UxsrchBilledFrame
#define srchBilledRC            UxSearchContext->UxsrchBilledRC
#define srchBilledYesTB         UxSearchContext->UxsrchBilledYesTB
#define srchBilledNoTB          UxSearchContext->UxsrchBilledNoTB
#define srchBilledBothTB        UxSearchContext->UxsrchBilledBothTB
#define mediaTypeSW             UxSearchContext->UxmediaTypeSW
#define mediaTypeRC             UxSearchContext->UxmediaTypeRC
#define processStatusSW         UxSearchContext->UxprocessStatusSW
#define processStatusRC         UxSearchContext->UxprocessStatusRC
#define srchItemStatusSW        UxSearchContext->UxsrchItemStatusSW
#define itemStatusRC            UxSearchContext->UxitemStatusRC
#define srchOrderStatusSW       UxSearchContext->UxsrchOrderStatusSW
#define orderStatusRC           UxSearchContext->UxorderStatusRC
#define dummyTB                 UxSearchContext->UxdummyTB
#define processOptionSW         UxSearchContext->UxprocessOptionSW
#define processOptionRC         UxSearchContext->UxprocessOptionRC
#define srchOrderIdTF           UxSearchContext->UxsrchOrderIdTF
#define accountIdTF             UxSearchContext->UxaccountIdTF
#define userIdTF                UxSearchContext->UxuserIdTF
#define userIdPB                UxSearchContext->UxuserIdPB
#define accountIdPB             UxSearchContext->UxaccountIdPB
#define optionMenu_p1           UxSearchContext->UxoptionMenu_p1
#define processTypeAllPB        UxSearchContext->UxprocessTypeAllPB
#define optionMenu_p1_b4        UxSearchContext->UxoptionMenu_p1_b4
#define optionMenu_p_b1         UxSearchContext->UxoptionMenu_p_b1
#define optionMenu_p1_b3        UxSearchContext->UxoptionMenu_p1_b3
#define optionMenu_p1_b2        UxSearchContext->UxoptionMenu_p1_b2
#define processingTypeOM        UxSearchContext->UxprocessingTypeOM
#define srchPrintSearchPB       UxSearchContext->UxsrchPrintSearchPB
#define srchOrderItemLB1        UxSearchContext->UxsrchOrderItemLB1
#define srchOrderItemLB2        UxSearchContext->UxsrchOrderItemLB2
#define priorityOM_pane         UxSearchContext->UxpriorityOM_pane
#define orderPriorityAllPB      UxSearchContext->UxorderPriorityAllPB
#define orderPriorityOM         UxSearchContext->UxorderPriorityOM
#define frame1                  UxSearchContext->Uxframe1
#define label40                 UxSearchContext->Uxlabel40
#define srchItemStatusLB1       UxSearchContext->UxsrchItemStatusLB1
#define srchItemTypeSW          UxSearchContext->UxsrchItemTypeSW
#define itemTypeRC              UxSearchContext->UxitemTypeRC
#define UxParent                UxSearchContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_search( swidget _UxUxParent );

#endif	/* _IMS_OPSEARCH_INCLUDED */
