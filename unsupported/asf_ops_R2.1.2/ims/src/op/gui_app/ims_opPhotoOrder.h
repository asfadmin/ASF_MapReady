
/*******************************************************************************
       ims_opPhotoOrder.h
       This header file is included by ims_opPhotoOrder.c

*******************************************************************************/

#ifndef	_IMS_OPPHOTOORDER_INCLUDED
#define	_IMS_OPPHOTOORDER_INCLUDED

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
	Widget	UxphotoOrder;
	Widget	Uxlabel20;
	Widget	Uxframe2;
	Widget	Uxform3;
	Widget	Uxlabel22;
	Widget	UxorderIdSW1;
	Widget	UxorderIdSL1;
	Widget	Uxlabel28;
	Widget	UxitemIdSW1;
	Widget	UxitemIdSL1;
	Widget	Uxlabel29;
	Widget	UxproductIdSW1;
	Widget	UxproductIdSL1;
	Widget	Uxlabel30;
	Widget	UxqtySW1;
	Widget	UxqtySL1;
	Widget	UxdummySW1;
	Widget	UxdummySL1;
	Widget	Uxlabel44;
	Widget	UxphotoTypeTF;
	Widget	UxphotoTypePB;
	Widget	UxsearchPB;
	Widget	Uxlabel63;
	Widget	UxuserIdSW1;
	Widget	UxuserIdSL1;
	Widget	UxclearPB;
	Widget	Uxseparator10;
	Widget	Uxlabel66;
	Widget	UxtotalItemsTF;
	Widget	Uxframe3;
	Widget	Uxform4;
	Widget	Uxlabel21;
	Widget	UxorderIdSW2;
	Widget	UxorderIdSL2;
	Widget	Uxlabel25;
	Widget	UxorderDateTF;
	Widget	Uxlabel24;
	Widget	UxworkOrderTF;
	Widget	Uxlabel23;
	Widget	UxjobIdTF;
	Widget	Uxlabel31;
	Widget	Uxlabel32;
	Widget	UxtotalPrintsTF;
	Widget	UxtotalCostTF;
	Widget	Uxlabel45;
	Widget	Uxlabel46;
	Widget	Uxlabel47;
	Widget	UxitemIdSW2;
	Widget	UxitemIdSL2;
	Widget	UxproductIdSW2;
	Widget	UxproductIdSL2;
	Widget	UxqtySW2;
	Widget	UxqtySL2;
	Widget	UxdummySW2;
	Widget	UxdummySL2;
	Widget	UxcreatePB;
	Widget	Uxseparator6;
	Widget	UxcancelPB;
	Widget	UxdeletePB;
	Widget	Uxseparator4;
	Widget	UxmenuBar2;
	Widget	UxmenuBar_p2;
	Widget	UxwelcomeMPB;
	Widget	UxmenuBar_p1_b1;
	Widget	UxmenuBar_p2_b4;
	Widget	UxmenuBar_p2_b5;
	Widget	UxmenuBar_p1_b18;
	Widget	UxmenuBar_top_b2;
	Widget	UxmenuBar1_p1;
	Widget	UxprintMPB;
	Widget	UxmenuBar1_top_b3;
	Widget	UxmenuBar1_p4;
	Widget	UxmenuBar1_p3_b2;
	Widget	UxmenuBar1_top_b4;
	Widget	UxprintPB;
	Widget	UxclosePB;
	Widget	UxaddPB;
	swidget	UxUxParent;
} _UxCphotoOrder;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCphotoOrder          *UxPhotoOrderContext;
#define photoOrder              UxPhotoOrderContext->UxphotoOrder
#define label20                 UxPhotoOrderContext->Uxlabel20
#define frame2                  UxPhotoOrderContext->Uxframe2
#define form3                   UxPhotoOrderContext->Uxform3
#define label22                 UxPhotoOrderContext->Uxlabel22
#define orderIdSW1              UxPhotoOrderContext->UxorderIdSW1
#define orderIdSL1              UxPhotoOrderContext->UxorderIdSL1
#define label28                 UxPhotoOrderContext->Uxlabel28
#define itemIdSW1               UxPhotoOrderContext->UxitemIdSW1
#define itemIdSL1               UxPhotoOrderContext->UxitemIdSL1
#define label29                 UxPhotoOrderContext->Uxlabel29
#define productIdSW1            UxPhotoOrderContext->UxproductIdSW1
#define productIdSL1            UxPhotoOrderContext->UxproductIdSL1
#define label30                 UxPhotoOrderContext->Uxlabel30
#define qtySW1                  UxPhotoOrderContext->UxqtySW1
#define qtySL1                  UxPhotoOrderContext->UxqtySL1
#define dummySW1                UxPhotoOrderContext->UxdummySW1
#define dummySL1                UxPhotoOrderContext->UxdummySL1
#define label44                 UxPhotoOrderContext->Uxlabel44
#define photoTypeTF             UxPhotoOrderContext->UxphotoTypeTF
#define photoTypePB             UxPhotoOrderContext->UxphotoTypePB
#define searchPB                UxPhotoOrderContext->UxsearchPB
#define label63                 UxPhotoOrderContext->Uxlabel63
#define userIdSW1               UxPhotoOrderContext->UxuserIdSW1
#define userIdSL1               UxPhotoOrderContext->UxuserIdSL1
#define clearPB                 UxPhotoOrderContext->UxclearPB
#define separator10             UxPhotoOrderContext->Uxseparator10
#define label66                 UxPhotoOrderContext->Uxlabel66
#define totalItemsTF            UxPhotoOrderContext->UxtotalItemsTF
#define frame3                  UxPhotoOrderContext->Uxframe3
#define form4                   UxPhotoOrderContext->Uxform4
#define label21                 UxPhotoOrderContext->Uxlabel21
#define orderIdSW2              UxPhotoOrderContext->UxorderIdSW2
#define orderIdSL2              UxPhotoOrderContext->UxorderIdSL2
#define label25                 UxPhotoOrderContext->Uxlabel25
#define orderDateTF             UxPhotoOrderContext->UxorderDateTF
#define label24                 UxPhotoOrderContext->Uxlabel24
#define workOrderTF             UxPhotoOrderContext->UxworkOrderTF
#define label23                 UxPhotoOrderContext->Uxlabel23
#define jobIdTF                 UxPhotoOrderContext->UxjobIdTF
#define label31                 UxPhotoOrderContext->Uxlabel31
#define label32                 UxPhotoOrderContext->Uxlabel32
#define totalPrintsTF           UxPhotoOrderContext->UxtotalPrintsTF
#define totalCostTF             UxPhotoOrderContext->UxtotalCostTF
#define label45                 UxPhotoOrderContext->Uxlabel45
#define label46                 UxPhotoOrderContext->Uxlabel46
#define label47                 UxPhotoOrderContext->Uxlabel47
#define itemIdSW2               UxPhotoOrderContext->UxitemIdSW2
#define itemIdSL2               UxPhotoOrderContext->UxitemIdSL2
#define productIdSW2            UxPhotoOrderContext->UxproductIdSW2
#define productIdSL2            UxPhotoOrderContext->UxproductIdSL2
#define qtySW2                  UxPhotoOrderContext->UxqtySW2
#define qtySL2                  UxPhotoOrderContext->UxqtySL2
#define dummySW2                UxPhotoOrderContext->UxdummySW2
#define dummySL2                UxPhotoOrderContext->UxdummySL2
#define createPB                UxPhotoOrderContext->UxcreatePB
#define separator6              UxPhotoOrderContext->Uxseparator6
#define cancelPB                UxPhotoOrderContext->UxcancelPB
#define deletePB                UxPhotoOrderContext->UxdeletePB
#define separator4              UxPhotoOrderContext->Uxseparator4
#define menuBar2                UxPhotoOrderContext->UxmenuBar2
#define menuBar_p2              UxPhotoOrderContext->UxmenuBar_p2
#define welcomeMPB              UxPhotoOrderContext->UxwelcomeMPB
#define menuBar_p1_b1           UxPhotoOrderContext->UxmenuBar_p1_b1
#define menuBar_p2_b4           UxPhotoOrderContext->UxmenuBar_p2_b4
#define menuBar_p2_b5           UxPhotoOrderContext->UxmenuBar_p2_b5
#define menuBar_p1_b18          UxPhotoOrderContext->UxmenuBar_p1_b18
#define menuBar_top_b2          UxPhotoOrderContext->UxmenuBar_top_b2
#define menuBar1_p1             UxPhotoOrderContext->UxmenuBar1_p1
#define printMPB                UxPhotoOrderContext->UxprintMPB
#define menuBar1_top_b3         UxPhotoOrderContext->UxmenuBar1_top_b3
#define menuBar1_p4             UxPhotoOrderContext->UxmenuBar1_p4
#define menuBar1_p3_b2          UxPhotoOrderContext->UxmenuBar1_p3_b2
#define menuBar1_top_b4         UxPhotoOrderContext->UxmenuBar1_top_b4
#define printPB                 UxPhotoOrderContext->UxprintPB
#define closePB                 UxPhotoOrderContext->UxclosePB
#define addPB                   UxPhotoOrderContext->UxaddPB
#define UxParent                UxPhotoOrderContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_photoOrder( swidget _UxUxParent );

#endif	/* _IMS_OPPHOTOORDER_INCLUDED */
