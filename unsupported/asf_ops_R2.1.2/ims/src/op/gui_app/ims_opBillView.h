
/*******************************************************************************
       ims_opBillView.h
       This header file is included by ims_opBillView.c

*******************************************************************************/

#ifndef	_IMS_OPBILLVIEW_INCLUDED
#define	_IMS_OPBILLVIEW_INCLUDED

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
	Widget	UxbillView;
	Widget	Uxframe13;
	Widget	Uxform15;
	Widget	Uxlabel135;
	Widget	UxbillItemSW;
	Widget	UxbillItemSL;
	Widget	UxbillToSW;
	Widget	UxbillToST;
	Widget	Uxlabel141;
	Widget	UxorderDateTF;
	Widget	Uxlabel143;
	Widget	Uxlabel144;
	Widget	Uxlabel158;
	Widget	Uxlabel159;
	Widget	Uxlabel165;
	Widget	Uxlabel166;
	Widget	UxinvoiceIdTF;
	Widget	UxaccountIdTF;
	Widget	UxamountTF;
	Widget	UxorderIdTF;
	Widget	UxbalanceTF;
	Widget	UxbillDateTF;
	Widget	Uxlabel207;
	Widget	UxresourceTF;
	Widget	Uxlabel208;
	Widget	UxuserNameTF;
	Widget	Uxlabel172;
	Widget	Uxlabel173;
	Widget	Uxlabel179;
	Widget	Uxlabel187;
	Widget	Uxlabel197;
	Widget	Uxlabel198;
	Widget	Uxlabel202;
	Widget	Uxlabel180;
	Widget	Uxlabel186;
	Widget	UxviewPB;
	Widget	UxprintPB;
	Widget	UxclosePB;
	Widget	UxbillViewLB;
	Widget	Uxseparator33;
	Widget	UxbillIdSW;
	Widget	UxbillIdSL;
	swidget	UxUxParent;
} _UxCbillView;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCbillView            *UxBillViewContext;
#define billView                UxBillViewContext->UxbillView
#define frame13                 UxBillViewContext->Uxframe13
#define form15                  UxBillViewContext->Uxform15
#define label135                UxBillViewContext->Uxlabel135
#define billItemSW              UxBillViewContext->UxbillItemSW
#define billItemSL              UxBillViewContext->UxbillItemSL
#define billToSW                UxBillViewContext->UxbillToSW
#define billToST                UxBillViewContext->UxbillToST
#define label141                UxBillViewContext->Uxlabel141
#define orderDateTF             UxBillViewContext->UxorderDateTF
#define label143                UxBillViewContext->Uxlabel143
#define label144                UxBillViewContext->Uxlabel144
#define label158                UxBillViewContext->Uxlabel158
#define label159                UxBillViewContext->Uxlabel159
#define label165                UxBillViewContext->Uxlabel165
#define label166                UxBillViewContext->Uxlabel166
#define invoiceIdTF             UxBillViewContext->UxinvoiceIdTF
#define accountIdTF             UxBillViewContext->UxaccountIdTF
#define amountTF                UxBillViewContext->UxamountTF
#define orderIdTF               UxBillViewContext->UxorderIdTF
#define balanceTF               UxBillViewContext->UxbalanceTF
#define billDateTF              UxBillViewContext->UxbillDateTF
#define label207                UxBillViewContext->Uxlabel207
#define resourceTF              UxBillViewContext->UxresourceTF
#define label208                UxBillViewContext->Uxlabel208
#define userNameTF              UxBillViewContext->UxuserNameTF
#define label172                UxBillViewContext->Uxlabel172
#define label173                UxBillViewContext->Uxlabel173
#define label179                UxBillViewContext->Uxlabel179
#define label187                UxBillViewContext->Uxlabel187
#define label197                UxBillViewContext->Uxlabel197
#define label198                UxBillViewContext->Uxlabel198
#define label202                UxBillViewContext->Uxlabel202
#define label180                UxBillViewContext->Uxlabel180
#define label186                UxBillViewContext->Uxlabel186
#define viewPB                  UxBillViewContext->UxviewPB
#define printPB                 UxBillViewContext->UxprintPB
#define closePB                 UxBillViewContext->UxclosePB
#define billViewLB              UxBillViewContext->UxbillViewLB
#define separator33             UxBillViewContext->Uxseparator33
#define billIdSW                UxBillViewContext->UxbillIdSW
#define billIdSL                UxBillViewContext->UxbillIdSL
#define UxParent                UxBillViewContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_billView( swidget _UxUxParent );

#endif	/* _IMS_OPBILLVIEW_INCLUDED */
