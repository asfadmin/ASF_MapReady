
/*******************************************************************************
       ims_opShipping.h
       This header file is included by ims_opShipping.c

*******************************************************************************/

#ifndef	_IMS_OPSHIPPING_INCLUDED
#define	_IMS_OPSHIPPING_INCLUDED

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
	Widget	Uxshipping;
	Widget	Uxframe10;
	Widget	Uxform11;
	Widget	Uxlabel108;
	Widget	UxprintPB;
	Widget	UxcancelPB;
	Widget	Uxseparator;
	Widget	Uxlabel109;
	Widget	Uxlabel110;
	Widget	UxorderIdTF;
	Widget	Uxlabel111;
	Widget	Uxlabel112;
	Widget	Uxlabel113;
	Widget	Uxlabel114;
	Widget	Uxlabel115;
	Widget	UxshippingIdTF;
	Widget	UxaccountIdTF;
	Widget	UxcarrierTF;
	Widget	Uxlabel127;
	Widget	UxshipDateTF;
	Widget	UxtotalQtyTF;
	Widget	Uxlabel128;
	Widget	UxorderDateTF;
	Widget	UxtotalCostTF;
	Widget	UxshipToSW;
	Widget	UxshipToST;
	Widget	Uxlabel129;
	Widget	Uxlabel130;
	Widget	Uxlabel132;
	Widget	UxshipItemSW;
	Widget	UxshipItemSL;
	Widget	UxeditCommentPB;
	Widget	Uxlabel116;
	Widget	Uxlabel117;
	Widget	Uxlabel209;
	Widget	Uxlabel107;
	Widget	Uxlabel118;
	swidget	UxUxParent;
} _UxCshipping;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCshipping            *UxShippingContext;
#define shipping                UxShippingContext->Uxshipping
#define frame10                 UxShippingContext->Uxframe10
#define form11                  UxShippingContext->Uxform11
#define label108                UxShippingContext->Uxlabel108
#define printPB                 UxShippingContext->UxprintPB
#define cancelPB                UxShippingContext->UxcancelPB
#define separator               UxShippingContext->Uxseparator
#define label109                UxShippingContext->Uxlabel109
#define label110                UxShippingContext->Uxlabel110
#define orderIdTF               UxShippingContext->UxorderIdTF
#define label111                UxShippingContext->Uxlabel111
#define label112                UxShippingContext->Uxlabel112
#define label113                UxShippingContext->Uxlabel113
#define label114                UxShippingContext->Uxlabel114
#define label115                UxShippingContext->Uxlabel115
#define shippingIdTF            UxShippingContext->UxshippingIdTF
#define accountIdTF             UxShippingContext->UxaccountIdTF
#define carrierTF               UxShippingContext->UxcarrierTF
#define label127                UxShippingContext->Uxlabel127
#define shipDateTF              UxShippingContext->UxshipDateTF
#define totalQtyTF              UxShippingContext->UxtotalQtyTF
#define label128                UxShippingContext->Uxlabel128
#define orderDateTF             UxShippingContext->UxorderDateTF
#define totalCostTF             UxShippingContext->UxtotalCostTF
#define shipToSW                UxShippingContext->UxshipToSW
#define shipToST                UxShippingContext->UxshipToST
#define label129                UxShippingContext->Uxlabel129
#define label130                UxShippingContext->Uxlabel130
#define label132                UxShippingContext->Uxlabel132
#define shipItemSW              UxShippingContext->UxshipItemSW
#define shipItemSL              UxShippingContext->UxshipItemSL
#define editCommentPB           UxShippingContext->UxeditCommentPB
#define label116                UxShippingContext->Uxlabel116
#define label117                UxShippingContext->Uxlabel117
#define label209                UxShippingContext->Uxlabel209
#define label107                UxShippingContext->Uxlabel107
#define label118                UxShippingContext->Uxlabel118
#define UxParent                UxShippingContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_shipping( swidget _UxUxParent );

#endif	/* _IMS_OPSHIPPING_INCLUDED */
