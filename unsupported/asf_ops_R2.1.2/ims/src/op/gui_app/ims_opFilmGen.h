
/*******************************************************************************
       ims_opFilmGen.h
       This header file is included by ims_opFilmGen.c

*******************************************************************************/

#ifndef	_IMS_OPFILMGEN_INCLUDED
#define	_IMS_OPFILMGEN_INCLUDED

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
	Widget	UxfilmGeneration;
	Widget	UxmenuBar5;
	Widget	UxmenuBar_p5;
	Widget	UxwelcomeMPB;
	Widget	UxmenuBar_p1_b13;
	Widget	UxcloseMPB;
	Widget	UxmenuBar_top_b5;
	Widget	UxmenuBar1_p9;
	Widget	UxprintMPB;
	Widget	UxmenuBar1_top_b9;
	Widget	UxmenuBar5_p4;
	Widget	UxupdateFireStatusMPB;
	Widget	UxmenuBar5_p4_b2;
	Widget	UxeditFireCommentMPB;
	Widget	UxmenuBar5_p4_b6;
	Widget	UxfireItemRegenMPB;
	Widget	UxmenuBar5_top_b1;
	Widget	UxmenuBar5_p5;
	Widget	UxupdateLaserStatusMPB;
	Widget	UxmenuBar5_p5_b2;
	Widget	UxeditLaserCommentMPB;
	Widget	UxmenuBar5_p5_b4;
	Widget	UxlaserItemRegenMPB;
	Widget	UxmenuBar5_top_b2;
	Widget	UxmenuBar1_p10;
	Widget	UxmenuBar1_p3_b5;
	Widget	UxmenuBar1_top_b10;
	Widget	Uxlabel73;
	Widget	Uxseparator15;
	Widget	UxprintPB;
	Widget	UxclosePB;
	Widget	Uxframe7;
	Widget	Uxform2;
	Widget	Uxlabel74;
	Widget	Uxlabel75;
	Widget	UxfireDummySW;
	Widget	UxfireDummySL;
	Widget	Uxlabel77;
	Widget	UxfireClearPB;
	Widget	Uxseparator14;
	Widget	Uxlabel79;
	Widget	UxfireTotalItemsTF;
	Widget	Uxlabel72;
	Widget	Uxseparator18;
	Widget	Uxseparator19;
	Widget	Uxlabel89;
	Widget	Uxseparator21;
	Widget	UxfireAddPB;
	Widget	UxfireSearchStatusOM_pane;
	Widget	UxfireDummyPB;
	Widget	UxfireSearchStatusOM;
	Widget	UxfireSearchPB;
	Widget	UxfireOrderIdSW;
	Widget	UxfireOrderIdSL;
	Widget	UxfireItemSW;
	Widget	UxfireItemSL;
	Widget	UxfireStatusSW;
	Widget	UxfireStatusSL;
	Widget	Uxframe8;
	Widget	Uxform5;
	Widget	UxlaserOrderIdSW;
	Widget	UxlaserOrderIdSL;
	Widget	UxlaserItemSW;
	Widget	UxlaserItemSL;
	Widget	UxlaserStatusSW;
	Widget	UxlaserStatusSL;
	Widget	UxlaserDummySW;
	Widget	UxlaserDummySL;
	Widget	Uxseparator16;
	Widget	Uxlabel82;
	Widget	Uxlabel81;
	Widget	UxlaserTotalItemsTF;
	Widget	UxlaserAddPB;
	Widget	Uxlabel76;
	Widget	Uxlabel78;
	Widget	Uxlabel80;
	Widget	UxlaserClearPB;
	Widget	Uxlabel90;
	Widget	UxlaserSearchStatusOM_pane;
	Widget	UxlaserDummyPB;
	Widget	UxlaserSearchStatusOM;
	Widget	UxlaserSearchPB;
	Widget	Uxseparator23;
	Widget	Uxseparator24;
	Widget	Uxseparator26;
	Widget	Uxframe9;
	Widget	Uxform8;
	Widget	Uxlabel83;
	Widget	UxttdlOrderIdSW;
	Widget	UxttdlOrderIdSL;
	Widget	Uxlabel84;
	Widget	UxttdlItemSW;
	Widget	UxttdlItemSL;
	Widget	Uxlabel85;
	Widget	UxttdlQueueTypeSW;
	Widget	UxttdlQueueTypeSL;
	Widget	UxttdlDummySW;
	Widget	UxttdlDummySL;
	Widget	Uxlabel86;
	Widget	Uxseparator17;
	Widget	Uxlabel87;
	Widget	UxttdlTotalItemsTF;
	Widget	UxttdlDeletePB;
	Widget	UxttdlProcessPB;
	Widget	Uxseparator22;
	Widget	Uxseparator27;
	swidget	UxUxParent;
} _UxCfilmGeneration;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCfilmGeneration      *UxFilmGenerationContext;
#define filmGeneration          UxFilmGenerationContext->UxfilmGeneration
#define menuBar5                UxFilmGenerationContext->UxmenuBar5
#define menuBar_p5              UxFilmGenerationContext->UxmenuBar_p5
#define welcomeMPB              UxFilmGenerationContext->UxwelcomeMPB
#define menuBar_p1_b13          UxFilmGenerationContext->UxmenuBar_p1_b13
#define closeMPB                UxFilmGenerationContext->UxcloseMPB
#define menuBar_top_b5          UxFilmGenerationContext->UxmenuBar_top_b5
#define menuBar1_p9             UxFilmGenerationContext->UxmenuBar1_p9
#define printMPB                UxFilmGenerationContext->UxprintMPB
#define menuBar1_top_b9         UxFilmGenerationContext->UxmenuBar1_top_b9
#define menuBar5_p4             UxFilmGenerationContext->UxmenuBar5_p4
#define updateFireStatusMPB     UxFilmGenerationContext->UxupdateFireStatusMPB
#define menuBar5_p4_b2          UxFilmGenerationContext->UxmenuBar5_p4_b2
#define editFireCommentMPB      UxFilmGenerationContext->UxeditFireCommentMPB
#define menuBar5_p4_b6          UxFilmGenerationContext->UxmenuBar5_p4_b6
#define fireItemRegenMPB        UxFilmGenerationContext->UxfireItemRegenMPB
#define menuBar5_top_b1         UxFilmGenerationContext->UxmenuBar5_top_b1
#define menuBar5_p5             UxFilmGenerationContext->UxmenuBar5_p5
#define updateLaserStatusMPB    UxFilmGenerationContext->UxupdateLaserStatusMPB
#define menuBar5_p5_b2          UxFilmGenerationContext->UxmenuBar5_p5_b2
#define editLaserCommentMPB     UxFilmGenerationContext->UxeditLaserCommentMPB
#define menuBar5_p5_b4          UxFilmGenerationContext->UxmenuBar5_p5_b4
#define laserItemRegenMPB       UxFilmGenerationContext->UxlaserItemRegenMPB
#define menuBar5_top_b2         UxFilmGenerationContext->UxmenuBar5_top_b2
#define menuBar1_p10            UxFilmGenerationContext->UxmenuBar1_p10
#define menuBar1_p3_b5          UxFilmGenerationContext->UxmenuBar1_p3_b5
#define menuBar1_top_b10        UxFilmGenerationContext->UxmenuBar1_top_b10
#define label73                 UxFilmGenerationContext->Uxlabel73
#define separator15             UxFilmGenerationContext->Uxseparator15
#define printPB                 UxFilmGenerationContext->UxprintPB
#define closePB                 UxFilmGenerationContext->UxclosePB
#define frame7                  UxFilmGenerationContext->Uxframe7
#define form2                   UxFilmGenerationContext->Uxform2
#define label74                 UxFilmGenerationContext->Uxlabel74
#define label75                 UxFilmGenerationContext->Uxlabel75
#define fireDummySW             UxFilmGenerationContext->UxfireDummySW
#define fireDummySL             UxFilmGenerationContext->UxfireDummySL
#define label77                 UxFilmGenerationContext->Uxlabel77
#define fireClearPB             UxFilmGenerationContext->UxfireClearPB
#define separator14             UxFilmGenerationContext->Uxseparator14
#define label79                 UxFilmGenerationContext->Uxlabel79
#define fireTotalItemsTF        UxFilmGenerationContext->UxfireTotalItemsTF
#define label72                 UxFilmGenerationContext->Uxlabel72
#define separator18             UxFilmGenerationContext->Uxseparator18
#define separator19             UxFilmGenerationContext->Uxseparator19
#define label89                 UxFilmGenerationContext->Uxlabel89
#define separator21             UxFilmGenerationContext->Uxseparator21
#define fireAddPB               UxFilmGenerationContext->UxfireAddPB
#define fireSearchStatusOM_pane UxFilmGenerationContext->UxfireSearchStatusOM_pane
#define fireDummyPB             UxFilmGenerationContext->UxfireDummyPB
#define fireSearchStatusOM      UxFilmGenerationContext->UxfireSearchStatusOM
#define fireSearchPB            UxFilmGenerationContext->UxfireSearchPB
#define fireOrderIdSW           UxFilmGenerationContext->UxfireOrderIdSW
#define fireOrderIdSL           UxFilmGenerationContext->UxfireOrderIdSL
#define fireItemSW              UxFilmGenerationContext->UxfireItemSW
#define fireItemSL              UxFilmGenerationContext->UxfireItemSL
#define fireStatusSW            UxFilmGenerationContext->UxfireStatusSW
#define fireStatusSL            UxFilmGenerationContext->UxfireStatusSL
#define frame8                  UxFilmGenerationContext->Uxframe8
#define form5                   UxFilmGenerationContext->Uxform5
#define laserOrderIdSW          UxFilmGenerationContext->UxlaserOrderIdSW
#define laserOrderIdSL          UxFilmGenerationContext->UxlaserOrderIdSL
#define laserItemSW             UxFilmGenerationContext->UxlaserItemSW
#define laserItemSL             UxFilmGenerationContext->UxlaserItemSL
#define laserStatusSW           UxFilmGenerationContext->UxlaserStatusSW
#define laserStatusSL           UxFilmGenerationContext->UxlaserStatusSL
#define laserDummySW            UxFilmGenerationContext->UxlaserDummySW
#define laserDummySL            UxFilmGenerationContext->UxlaserDummySL
#define separator16             UxFilmGenerationContext->Uxseparator16
#define label82                 UxFilmGenerationContext->Uxlabel82
#define label81                 UxFilmGenerationContext->Uxlabel81
#define laserTotalItemsTF       UxFilmGenerationContext->UxlaserTotalItemsTF
#define laserAddPB              UxFilmGenerationContext->UxlaserAddPB
#define label76                 UxFilmGenerationContext->Uxlabel76
#define label78                 UxFilmGenerationContext->Uxlabel78
#define label80                 UxFilmGenerationContext->Uxlabel80
#define laserClearPB            UxFilmGenerationContext->UxlaserClearPB
#define label90                 UxFilmGenerationContext->Uxlabel90
#define laserSearchStatusOM_pane UxFilmGenerationContext->UxlaserSearchStatusOM_pane
#define laserDummyPB            UxFilmGenerationContext->UxlaserDummyPB
#define laserSearchStatusOM     UxFilmGenerationContext->UxlaserSearchStatusOM
#define laserSearchPB           UxFilmGenerationContext->UxlaserSearchPB
#define separator23             UxFilmGenerationContext->Uxseparator23
#define separator24             UxFilmGenerationContext->Uxseparator24
#define separator26             UxFilmGenerationContext->Uxseparator26
#define frame9                  UxFilmGenerationContext->Uxframe9
#define form8                   UxFilmGenerationContext->Uxform8
#define label83                 UxFilmGenerationContext->Uxlabel83
#define ttdlOrderIdSW           UxFilmGenerationContext->UxttdlOrderIdSW
#define ttdlOrderIdSL           UxFilmGenerationContext->UxttdlOrderIdSL
#define label84                 UxFilmGenerationContext->Uxlabel84
#define ttdlItemSW              UxFilmGenerationContext->UxttdlItemSW
#define ttdlItemSL              UxFilmGenerationContext->UxttdlItemSL
#define label85                 UxFilmGenerationContext->Uxlabel85
#define ttdlQueueTypeSW         UxFilmGenerationContext->UxttdlQueueTypeSW
#define ttdlQueueTypeSL         UxFilmGenerationContext->UxttdlQueueTypeSL
#define ttdlDummySW             UxFilmGenerationContext->UxttdlDummySW
#define ttdlDummySL             UxFilmGenerationContext->UxttdlDummySL
#define label86                 UxFilmGenerationContext->Uxlabel86
#define separator17             UxFilmGenerationContext->Uxseparator17
#define label87                 UxFilmGenerationContext->Uxlabel87
#define ttdlTotalItemsTF        UxFilmGenerationContext->UxttdlTotalItemsTF
#define ttdlDeletePB            UxFilmGenerationContext->UxttdlDeletePB
#define ttdlProcessPB           UxFilmGenerationContext->UxttdlProcessPB
#define separator22             UxFilmGenerationContext->Uxseparator22
#define separator27             UxFilmGenerationContext->Uxseparator27
#define UxParent                UxFilmGenerationContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_filmGeneration( swidget _UxUxParent );

#endif	/* _IMS_OPFILMGEN_INCLUDED */
