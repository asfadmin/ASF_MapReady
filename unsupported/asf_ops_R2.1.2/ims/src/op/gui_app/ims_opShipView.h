
/*******************************************************************************
       ims_opShipView.h
       This header file is included by ims_opShipView.c

*******************************************************************************/

#ifndef	_IMS_OPSHIPVIEW_INCLUDED
#define	_IMS_OPSHIPVIEW_INCLUDED

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
	Widget	UxshipView;
	Widget	UxshipViewLB;
	Widget	UxshipIdSW;
	Widget	UxshipIdSL;
	Widget	Uxframe12;
	Widget	Uxform13;
	Widget	Uxlabel133;
	Widget	Uxlabel134;
	Widget	Uxlabel136;
	Widget	UxorderIdTF;
	Widget	Uxlabel137;
	Widget	Uxlabel138;
	Widget	Uxlabel139;
	Widget	Uxlabel145;
	Widget	Uxlabel146;
	Widget	UxaccountIdTF;
	Widget	UxcarrierTF;
	Widget	Uxlabel147;
	Widget	UxtotalQtyTF;
	Widget	Uxlabel188;
	Widget	UxorderDateTF;
	Widget	UxtotalCostTF;
	Widget	Uxlabel199;
	Widget	UxshipItemSW;
	Widget	UxshipItemSL;
	Widget	Uxlabel200;
	Widget	Uxlabel201;
	Widget	Uxlabel140;
	Widget	UxshippingIdTF;
	Widget	Uxlabel142;
	Widget	UxshipDateTF;
	Widget	UxshipToSW;
	Widget	UxshipToST;
	Widget	Uxlabel210;
	Widget	Uxlabel148;
	Widget	Uxlabel149;
	Widget	Uxseparator29;
	Widget	UxviewPB;
	Widget	UxprintPB;
	Widget	UxclosePB;
	swidget	UxUxParent;
} _UxCshipView;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCshipView            *UxShipViewContext;
#define shipView                UxShipViewContext->UxshipView
#define shipViewLB              UxShipViewContext->UxshipViewLB
#define shipIdSW                UxShipViewContext->UxshipIdSW
#define shipIdSL                UxShipViewContext->UxshipIdSL
#define frame12                 UxShipViewContext->Uxframe12
#define form13                  UxShipViewContext->Uxform13
#define label133                UxShipViewContext->Uxlabel133
#define label134                UxShipViewContext->Uxlabel134
#define label136                UxShipViewContext->Uxlabel136
#define orderIdTF               UxShipViewContext->UxorderIdTF
#define label137                UxShipViewContext->Uxlabel137
#define label138                UxShipViewContext->Uxlabel138
#define label139                UxShipViewContext->Uxlabel139
#define label145                UxShipViewContext->Uxlabel145
#define label146                UxShipViewContext->Uxlabel146
#define accountIdTF             UxShipViewContext->UxaccountIdTF
#define carrierTF               UxShipViewContext->UxcarrierTF
#define label147                UxShipViewContext->Uxlabel147
#define totalQtyTF              UxShipViewContext->UxtotalQtyTF
#define label188                UxShipViewContext->Uxlabel188
#define orderDateTF             UxShipViewContext->UxorderDateTF
#define totalCostTF             UxShipViewContext->UxtotalCostTF
#define label199                UxShipViewContext->Uxlabel199
#define shipItemSW              UxShipViewContext->UxshipItemSW
#define shipItemSL              UxShipViewContext->UxshipItemSL
#define label200                UxShipViewContext->Uxlabel200
#define label201                UxShipViewContext->Uxlabel201
#define label140                UxShipViewContext->Uxlabel140
#define shippingIdTF            UxShipViewContext->UxshippingIdTF
#define label142                UxShipViewContext->Uxlabel142
#define shipDateTF              UxShipViewContext->UxshipDateTF
#define shipToSW                UxShipViewContext->UxshipToSW
#define shipToST                UxShipViewContext->UxshipToST
#define label210                UxShipViewContext->Uxlabel210
#define label148                UxShipViewContext->Uxlabel148
#define label149                UxShipViewContext->Uxlabel149
#define separator29             UxShipViewContext->Uxseparator29
#define viewPB                  UxShipViewContext->UxviewPB
#define printPB                 UxShipViewContext->UxprintPB
#define closePB                 UxShipViewContext->UxclosePB
#define UxParent                UxShipViewContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_shipView( swidget _UxUxParent );

#endif	/* _IMS_OPSHIPVIEW_INCLUDED */
