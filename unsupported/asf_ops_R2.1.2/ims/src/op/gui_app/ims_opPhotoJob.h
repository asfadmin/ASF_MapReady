
/*******************************************************************************
       ims_opPhotoJob.h
       This header file is included by ims_opPhotoJob.c

*******************************************************************************/

#ifndef	_IMS_OPPHOTOJOB_INCLUDED
#define	_IMS_OPPHOTOJOB_INCLUDED

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
	Widget	UxphotoJob;
	Widget	UxmenuBar3;
	Widget	UxmenuBar_p3;
	Widget	UxwelcomeMPB;
	Widget	UxmenuBar_p1_b19;
	Widget	UxgotoPhotoOrderMPB;
	Widget	UxmenuBar_p1_b22;
	Widget	UxmenuBar_p1_b27;
	Widget	UxmenuBar_top_b3;
	Widget	UxmenuBar1_p5;
	Widget	UxphotoJobsPrintMPB;
	Widget	UxmenuBar1_top_b5;
	Widget	UxmenuBar1_p6;
	Widget	UxmenuBar1_p3_b3;
	Widget	UxmenuBar1_top_b6;
	Widget	Uxlabel48;
	Widget	Uxseparator7;
	Widget	Uxframe4;
	Widget	Uxform6;
	Widget	UxjobIdSW1;
	Widget	UxjobIdSL1;
	Widget	Uxlabel51;
	Widget	UxstartDateSW1;
	Widget	UxstartDateSL1;
	Widget	UxphotoJobDummySW1;
	Widget	UxphotoJobDummySL1;
	Widget	Uxlabel53;
	Widget	UxjobIdTF1;
	Widget	UxjobIdListPB;
	Widget	UxsearchPB;
	Widget	Uxseparator9;
	Widget	Uxlabel50;
	Widget	UxphotoTypeTF;
	Widget	UxphotoTypeListPB;
	Widget	Uxlabel67;
	Widget	Uxlabel68;
	Widget	UxphotoTypeSW1;
	Widget	UxphotoTypeSL1;
	Widget	UxstatusSW1;
	Widget	UxstatusSL1;
	Widget	Uxlabel69;
	Widget	Uxlabel70;
	Widget	UxfromDateText;
	Widget	UxtoDateText;
	Widget	Uxlabel71;
	Widget	Uxframe6;
	Widget	Uxlabel64;
	Widget	Uxseparator11;
	Widget	Uxlabel65;
	Widget	UxtotalJobsTF;
	Widget	UxclearPB;
	Widget	Uxlabel49;
	Widget	UxstatusOM_pane;
	Widget	UxallPB;
	Widget	UxstatusOptionMenu;
	Widget	Uxframe5;
	Widget	Uxform7;
	Widget	Uxlabel54;
	Widget	UxorderIdSW2;
	Widget	UxorderIdSL2;
	Widget	Uxlabel55;
	Widget	UxorderDateTF;
	Widget	Uxlabel56;
	Widget	UxworkOrderTF;
	Widget	Uxlabel57;
	Widget	UxjobIdTF2;
	Widget	Uxlabel58;
	Widget	Uxlabel59;
	Widget	UxtotalPrintsTF;
	Widget	UxtotalCostTF;
	Widget	Uxlabel60;
	Widget	Uxlabel61;
	Widget	Uxlabel62;
	Widget	UxitemIdSW2;
	Widget	UxitemIdSL2;
	Widget	UxproductIdSW2;
	Widget	UxproductIdSL2;
	Widget	UxqtySW2;
	Widget	UxqtySL2;
	Widget	UxphotoJobDummySW2;
	Widget	UxphotoJobDummySL2;
	Widget	UxprocessPB;
	Widget	Uxseparator8;
	Widget	Uxlabel52;
	Widget	UxqualitySW2;
	Widget	UxqualitySL2;
	Widget	UxcompletePB;
	Widget	UxqualitySW1;
	Widget	UxgoodSL2;
	Widget	UxqualitySW3;
	Widget	UxregenSL2;
	Widget	UxqualitySW4;
	Widget	UxcancelSL2;
	Widget	Uxlabel42;
	Widget	UxcommentPB;
	Widget	UxclosePB;
	Widget	UxprintPB;
	Widget	UxviewPB;
	swidget	UxUxParent;
} _UxCphotoJob;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCphotoJob            *UxPhotoJobContext;
#define photoJob                UxPhotoJobContext->UxphotoJob
#define menuBar3                UxPhotoJobContext->UxmenuBar3
#define menuBar_p3              UxPhotoJobContext->UxmenuBar_p3
#define welcomeMPB              UxPhotoJobContext->UxwelcomeMPB
#define menuBar_p1_b19          UxPhotoJobContext->UxmenuBar_p1_b19
#define gotoPhotoOrderMPB       UxPhotoJobContext->UxgotoPhotoOrderMPB
#define menuBar_p1_b22          UxPhotoJobContext->UxmenuBar_p1_b22
#define menuBar_p1_b27          UxPhotoJobContext->UxmenuBar_p1_b27
#define menuBar_top_b3          UxPhotoJobContext->UxmenuBar_top_b3
#define menuBar1_p5             UxPhotoJobContext->UxmenuBar1_p5
#define photoJobsPrintMPB       UxPhotoJobContext->UxphotoJobsPrintMPB
#define menuBar1_top_b5         UxPhotoJobContext->UxmenuBar1_top_b5
#define menuBar1_p6             UxPhotoJobContext->UxmenuBar1_p6
#define menuBar1_p3_b3          UxPhotoJobContext->UxmenuBar1_p3_b3
#define menuBar1_top_b6         UxPhotoJobContext->UxmenuBar1_top_b6
#define label48                 UxPhotoJobContext->Uxlabel48
#define separator7              UxPhotoJobContext->Uxseparator7
#define frame4                  UxPhotoJobContext->Uxframe4
#define form6                   UxPhotoJobContext->Uxform6
#define jobIdSW1                UxPhotoJobContext->UxjobIdSW1
#define jobIdSL1                UxPhotoJobContext->UxjobIdSL1
#define label51                 UxPhotoJobContext->Uxlabel51
#define startDateSW1            UxPhotoJobContext->UxstartDateSW1
#define startDateSL1            UxPhotoJobContext->UxstartDateSL1
#define photoJobDummySW1        UxPhotoJobContext->UxphotoJobDummySW1
#define photoJobDummySL1        UxPhotoJobContext->UxphotoJobDummySL1
#define label53                 UxPhotoJobContext->Uxlabel53
#define jobIdTF1                UxPhotoJobContext->UxjobIdTF1
#define jobIdListPB             UxPhotoJobContext->UxjobIdListPB
#define searchPB                UxPhotoJobContext->UxsearchPB
#define separator9              UxPhotoJobContext->Uxseparator9
#define label50                 UxPhotoJobContext->Uxlabel50
#define photoTypeTF             UxPhotoJobContext->UxphotoTypeTF
#define photoTypeListPB         UxPhotoJobContext->UxphotoTypeListPB
#define label67                 UxPhotoJobContext->Uxlabel67
#define label68                 UxPhotoJobContext->Uxlabel68
#define photoTypeSW1            UxPhotoJobContext->UxphotoTypeSW1
#define photoTypeSL1            UxPhotoJobContext->UxphotoTypeSL1
#define statusSW1               UxPhotoJobContext->UxstatusSW1
#define statusSL1               UxPhotoJobContext->UxstatusSL1
#define label69                 UxPhotoJobContext->Uxlabel69
#define label70                 UxPhotoJobContext->Uxlabel70
#define fromDateText            UxPhotoJobContext->UxfromDateText
#define toDateText              UxPhotoJobContext->UxtoDateText
#define label71                 UxPhotoJobContext->Uxlabel71
#define frame6                  UxPhotoJobContext->Uxframe6
#define label64                 UxPhotoJobContext->Uxlabel64
#define separator11             UxPhotoJobContext->Uxseparator11
#define label65                 UxPhotoJobContext->Uxlabel65
#define totalJobsTF             UxPhotoJobContext->UxtotalJobsTF
#define clearPB                 UxPhotoJobContext->UxclearPB
#define label49                 UxPhotoJobContext->Uxlabel49
#define statusOM_pane           UxPhotoJobContext->UxstatusOM_pane
#define allPB                   UxPhotoJobContext->UxallPB
#define statusOptionMenu        UxPhotoJobContext->UxstatusOptionMenu
#define frame5                  UxPhotoJobContext->Uxframe5
#define form7                   UxPhotoJobContext->Uxform7
#define label54                 UxPhotoJobContext->Uxlabel54
#define orderIdSW2              UxPhotoJobContext->UxorderIdSW2
#define orderIdSL2              UxPhotoJobContext->UxorderIdSL2
#define label55                 UxPhotoJobContext->Uxlabel55
#define orderDateTF             UxPhotoJobContext->UxorderDateTF
#define label56                 UxPhotoJobContext->Uxlabel56
#define workOrderTF             UxPhotoJobContext->UxworkOrderTF
#define label57                 UxPhotoJobContext->Uxlabel57
#define jobIdTF2                UxPhotoJobContext->UxjobIdTF2
#define label58                 UxPhotoJobContext->Uxlabel58
#define label59                 UxPhotoJobContext->Uxlabel59
#define totalPrintsTF           UxPhotoJobContext->UxtotalPrintsTF
#define totalCostTF             UxPhotoJobContext->UxtotalCostTF
#define label60                 UxPhotoJobContext->Uxlabel60
#define label61                 UxPhotoJobContext->Uxlabel61
#define label62                 UxPhotoJobContext->Uxlabel62
#define itemIdSW2               UxPhotoJobContext->UxitemIdSW2
#define itemIdSL2               UxPhotoJobContext->UxitemIdSL2
#define productIdSW2            UxPhotoJobContext->UxproductIdSW2
#define productIdSL2            UxPhotoJobContext->UxproductIdSL2
#define qtySW2                  UxPhotoJobContext->UxqtySW2
#define qtySL2                  UxPhotoJobContext->UxqtySL2
#define photoJobDummySW2        UxPhotoJobContext->UxphotoJobDummySW2
#define photoJobDummySL2        UxPhotoJobContext->UxphotoJobDummySL2
#define processPB               UxPhotoJobContext->UxprocessPB
#define separator8              UxPhotoJobContext->Uxseparator8
#define label52                 UxPhotoJobContext->Uxlabel52
#define qualitySW2              UxPhotoJobContext->UxqualitySW2
#define qualitySL2              UxPhotoJobContext->UxqualitySL2
#define completePB              UxPhotoJobContext->UxcompletePB
#define qualitySW1              UxPhotoJobContext->UxqualitySW1
#define goodSL2                 UxPhotoJobContext->UxgoodSL2
#define qualitySW3              UxPhotoJobContext->UxqualitySW3
#define regenSL2                UxPhotoJobContext->UxregenSL2
#define qualitySW4              UxPhotoJobContext->UxqualitySW4
#define cancelSL2               UxPhotoJobContext->UxcancelSL2
#define label42                 UxPhotoJobContext->Uxlabel42
#define commentPB               UxPhotoJobContext->UxcommentPB
#define closePB                 UxPhotoJobContext->UxclosePB
#define printPB                 UxPhotoJobContext->UxprintPB
#define viewPB                  UxPhotoJobContext->UxviewPB
#define UxParent                UxPhotoJobContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_photoJob( swidget _UxUxParent );

#endif	/* _IMS_OPPHOTOJOB_INCLUDED */
