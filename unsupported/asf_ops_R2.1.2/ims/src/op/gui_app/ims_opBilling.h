
/*******************************************************************************
       ims_opBilling.h
       This header file is included by ims_opBilling.c

*******************************************************************************/

#ifndef	_IMS_OPBILLING_INCLUDED
#define	_IMS_OPBILLING_INCLUDED

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
	Widget	Uxbilling;
	Widget	Uxlabel94;
	Widget	Uxframe11;
	Widget	Uxform12;
	Widget	Uxseparator31;
	Widget	UxokPB;
	Widget	UxcancelPB;
	Widget	Uxlabel102;
	Widget	Uxlabel103;
	Widget	UxorderDateTF;
	Widget	Uxlabel105;
	Widget	Uxlabel119;
	Widget	Uxlabel120;
	Widget	Uxlabel121;
	Widget	Uxlabel122;
	Widget	Uxlabel106;
	Widget	Uxlabel123;
	Widget	Uxlabel124;
	Widget	Uxlabel125;
	Widget	Uxlabel126;
	Widget	Uxlabel104;
	Widget	Uxlabel131;
	Widget	UxcommentPB;
	Widget	UxbillItemSW;
	Widget	UxbillItemSL;
	Widget	UxinvoiceIdTF;
	Widget	UxaccountIdTF;
	Widget	UxamountTF;
	Widget	UxorderIdTF;
	Widget	UxbalanceTF;
	Widget	UxbillDateTF;
	Widget	UxbillToSW;
	Widget	UxbillToST;
	Widget	Uxlabel203;
	Widget	Uxlabel204;
	Widget	UxresourceTF;
	Widget	Uxlabel205;
	Widget	UxuserNameTF;
	Widget	Uxlabel100;
	swidget	UxUxParent;
} _UxCbilling;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCbilling             *UxBillingContext;
#define billing                 UxBillingContext->Uxbilling
#define label94                 UxBillingContext->Uxlabel94
#define frame11                 UxBillingContext->Uxframe11
#define form12                  UxBillingContext->Uxform12
#define separator31             UxBillingContext->Uxseparator31
#define okPB                    UxBillingContext->UxokPB
#define cancelPB                UxBillingContext->UxcancelPB
#define label102                UxBillingContext->Uxlabel102
#define label103                UxBillingContext->Uxlabel103
#define orderDateTF             UxBillingContext->UxorderDateTF
#define label105                UxBillingContext->Uxlabel105
#define label119                UxBillingContext->Uxlabel119
#define label120                UxBillingContext->Uxlabel120
#define label121                UxBillingContext->Uxlabel121
#define label122                UxBillingContext->Uxlabel122
#define label106                UxBillingContext->Uxlabel106
#define label123                UxBillingContext->Uxlabel123
#define label124                UxBillingContext->Uxlabel124
#define label125                UxBillingContext->Uxlabel125
#define label126                UxBillingContext->Uxlabel126
#define label104                UxBillingContext->Uxlabel104
#define label131                UxBillingContext->Uxlabel131
#define commentPB               UxBillingContext->UxcommentPB
#define billItemSW              UxBillingContext->UxbillItemSW
#define billItemSL              UxBillingContext->UxbillItemSL
#define invoiceIdTF             UxBillingContext->UxinvoiceIdTF
#define accountIdTF             UxBillingContext->UxaccountIdTF
#define amountTF                UxBillingContext->UxamountTF
#define orderIdTF               UxBillingContext->UxorderIdTF
#define balanceTF               UxBillingContext->UxbalanceTF
#define billDateTF              UxBillingContext->UxbillDateTF
#define billToSW                UxBillingContext->UxbillToSW
#define billToST                UxBillingContext->UxbillToST
#define label203                UxBillingContext->Uxlabel203
#define label204                UxBillingContext->Uxlabel204
#define resourceTF              UxBillingContext->UxresourceTF
#define label205                UxBillingContext->Uxlabel205
#define userNameTF              UxBillingContext->UxuserNameTF
#define label100                UxBillingContext->Uxlabel100
#define UxParent                UxBillingContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_billing( swidget _UxUxParent );

#endif	/* _IMS_OPBILLING_INCLUDED */
