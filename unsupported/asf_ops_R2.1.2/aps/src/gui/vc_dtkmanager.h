
/*******************************************************************************
       vc_dtkmanager.h
       This header file is included by vc_dtkmanager.c

*******************************************************************************/

#ifndef	_VC_DTKMANAGER_INCLUDED
#define	_VC_DTKMANAGER_INCLUDED

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
	Widget	Uxlabel148;
	Widget	UxscrolledWindowList_DTKS;
	Widget	UxscrolledList_DTKS;
	Widget	UxTF_DTKID;
	Widget	UxpushButton_SearchDTK;
	Widget	UxTF_DTK_searchclause;
	Widget	UxpushButton_SortDTK;
	Widget	UxTF_DTK_sortclause;
	Widget	Uxlabel136;
	Widget	Uxform_DTKquad;
	Widget	UxTF_NRLON1;
	Widget	Uxlabel143;
	Widget	Uxlabel144;
	Widget	UxTF_NRLON2;
	Widget	Uxlabel142;
	Widget	Uxlabel147;
	Widget	UxTF_NRLAT1;
	Widget	UxTF_NRLAT2;
	Widget	Uxlabel_NW2;
	Widget	Uxlabel145;
	Widget	UxTF_FARLON1;
	Widget	Uxlabel87;
	Widget	UxTF_FARLAT2;
	Widget	UxTF_FARLON2;
	Widget	Uxlabel141;
	Widget	UxTF_FARLAT1;
	Widget	UxTF_DTK_SITENAME;
	Widget	Uxlabel140;
	Widget	Uxlabel83;
	Widget	UxTF_FADTKID;
	Widget	Uxseparator12;
	Widget	UxTF_REV;
	Widget	Uxlabel103;
	Widget	Uxlabel139;
	Widget	Uxlabel138;
	Widget	UxTF_DTK_DARID;
	Widget	Uxlabel106;
	Widget	UxsubMenu_dtk_status;
	Widget	UxsubMenu_dtk_status_PLN;
	Widget	UxsubMenu_dtk_status_QUE;
	Widget	UxsubMenu_dtk_status_SUB;
	Widget	UxsubMenu_dtk_status_SCH;
	Widget	UxsubMenu_dtk_status_CON;
	Widget	UxsubMenu_dtk_status_REJ;
	Widget	UxsubMenu_dtk_status_DEL;
	Widget	UxsubMenu_dtk_status_INV;
	Widget	UxoptionMenu_dtk_status;
	Widget	UxpushButton_EditDTK;
	Widget	UxpushButton_DeleteDTK;
	Widget	UxpushButton_QuitDTK;
	Widget	UxpushButton_ClearDTKForm;
	Widget	UxpushButton_SaveDTKChanges;
	Widget	UxTF_DTK_recordcount;
	Widget	Uxlabel137;
	Widget	UxmenuBar_DTK_FILE;
	Widget	UxmenuBar_DTK_FILE_pane;
	Widget	UxPANE_SAVE_DTK_RPT;
	Widget	UxRPT_SELECTED_DTK;
	Widget	UxRPT_CURRENT_DTKS;
	Widget	UxRPT_ALL_DTKS;
	Widget	UxcascadeButton_DTK_SAVE;
	Widget	UxPANE_PRINT_DTK_RPT;
	Widget	UxPRINT_SELECTED_DTK;
	Widget	UxPRINT_CURRENT_DTKS;
	Widget	UxPRINT_ALL_DTKS;
	Widget	UxcascadeButton_DTK_PRINT;
	Widget	UxmenuBar_cascade_button_DTK_FILE;
	Widget	UxpushButton_DTK_refresh;
	Widget	UxTF_STOPLAT;
	Widget	UxTF_STRTLAT;
	Widget	Uxlabel146;
	Widget	Uxlabel112;
	Widget	UxTF_NOTES;
	Widget	Uxlabel113;
	Widget	UxsubMenu_sat;
	Widget	UxsubMenu_sat_ERS;
	Widget	UxoptionMenu_sat;
	Widget	UxsubMenu_sensor;
	Widget	UxsubMenu_sensor_SAR;
	Widget	UxoptionMenu_sensor;
	Widget	UxsubMenu_dtk_direction;
	Widget	UxsubMenu_dtk_direction_ascend;
	Widget	UxsubMenu_dtk_direction_descend;
	Widget	UxsubMenu_dtk_direction_cvrgNotAllowed;
	Widget	UxoptionMenu_dtk_direction;
	Widget	UxpushButton_CreateDTK;
	Widget	UxTF_DTKDATE;
	Widget	Uxlabel116;
	Widget	UxTF_DTK_STRTTIME;
	Widget	UxTF_DTK_STOPTIME;
	Widget	UxscrolledWindow_FATapes;
	Widget	UxrowColumn2;
	Widget	UxFATape_ASF;
	Widget	UxFATape_ESA;
	Widget	UxFATape_NASDA;
	Widget	UxFATape_CSA;
	Widget	Uxlabel25;
	Widget	UxframeActivity;
	Widget	UxrowColumn1;
	Widget	UxActivity_Downlink;
	Widget	UxActivity_Observe;
	Widget	UxActivity_Dump;
	Widget	UxActivity_Record;
	Widget	UxsubMenu_J1_DLinkChannel;
	Widget	UxsubMenu_J1_DLink_00;
	Widget	UxsubMenu_J1_DLink_F1;
	Widget	UxsubMenu_J1_DLink_F2;
	Widget	UxsubMenu_J1_DLink_CB;
	Widget	UxsubMenu_R1_DLink_F3;
	Widget	UxsubMenu_R1_DLink_F4;
	Widget	UxsubMenu_A1_DLink_F5;
	Widget	UxsubMenu_A1_DLink_F6;
	Widget	UxsubMenu_A1_DLink_F7;
	Widget	UxoptionMenu_J1_DLinkChannel;
	Widget	UxpushButton_GetCvrgPoints;
	Widget	UxsubMenu_dtkm_stnid;
	Widget	UxsubMenu_dtkm_stnid_asf;
	Widget	UxoptionMenu_dtkm_station_id;
	Widget	Uxlabel34;
	Widget	Uxlabel_SciQuicklook;
	Widget	UxsubMenu_antenna;
	Widget	UxsubMenu_antenna_1;
	Widget	UxoptionMenu_antenna;
	Widget	Uxlabel_FA_Schedule_Link;
	Widget	UxTF_FA_SCHEDULE_LINK;
	Widget	Uxseparator8;
	Widget	Uxlabel63;
	Widget	UxsubMenu_PlanQuicklook;
	Widget	UxsubMenu_PlanQuicklook_no;
	Widget	UxsubMenu_PlanQuicklook_yes;
	Widget	UxoptionMenu_PlanQuicklook;
	swidget	UxUxParent;
} _UxCDTKManager;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCDTKManager          *UxDTKManagerContext;
#define label148                UxDTKManagerContext->Uxlabel148
#define scrolledWindowList_DTKS UxDTKManagerContext->UxscrolledWindowList_DTKS
#define scrolledList_DTKS       UxDTKManagerContext->UxscrolledList_DTKS
#define TF_DTKID                UxDTKManagerContext->UxTF_DTKID
#define pushButton_SearchDTK    UxDTKManagerContext->UxpushButton_SearchDTK
#define TF_DTK_searchclause     UxDTKManagerContext->UxTF_DTK_searchclause
#define pushButton_SortDTK      UxDTKManagerContext->UxpushButton_SortDTK
#define TF_DTK_sortclause       UxDTKManagerContext->UxTF_DTK_sortclause
#define label136                UxDTKManagerContext->Uxlabel136
#define form_DTKquad            UxDTKManagerContext->Uxform_DTKquad
#define TF_NRLON1               UxDTKManagerContext->UxTF_NRLON1
#define label143                UxDTKManagerContext->Uxlabel143
#define label144                UxDTKManagerContext->Uxlabel144
#define TF_NRLON2               UxDTKManagerContext->UxTF_NRLON2
#define label142                UxDTKManagerContext->Uxlabel142
#define label147                UxDTKManagerContext->Uxlabel147
#define TF_NRLAT1               UxDTKManagerContext->UxTF_NRLAT1
#define TF_NRLAT2               UxDTKManagerContext->UxTF_NRLAT2
#define label_NW2               UxDTKManagerContext->Uxlabel_NW2
#define label145                UxDTKManagerContext->Uxlabel145
#define TF_FARLON1              UxDTKManagerContext->UxTF_FARLON1
#define label87                 UxDTKManagerContext->Uxlabel87
#define TF_FARLAT2              UxDTKManagerContext->UxTF_FARLAT2
#define TF_FARLON2              UxDTKManagerContext->UxTF_FARLON2
#define label141                UxDTKManagerContext->Uxlabel141
#define TF_FARLAT1              UxDTKManagerContext->UxTF_FARLAT1
#define TF_DTK_SITENAME         UxDTKManagerContext->UxTF_DTK_SITENAME
#define label140                UxDTKManagerContext->Uxlabel140
#define label83                 UxDTKManagerContext->Uxlabel83
#define TF_FADTKID              UxDTKManagerContext->UxTF_FADTKID
#define separator12             UxDTKManagerContext->Uxseparator12
#define TF_REV                  UxDTKManagerContext->UxTF_REV
#define label103                UxDTKManagerContext->Uxlabel103
#define label139                UxDTKManagerContext->Uxlabel139
#define label138                UxDTKManagerContext->Uxlabel138
#define TF_DTK_DARID            UxDTKManagerContext->UxTF_DTK_DARID
#define label106                UxDTKManagerContext->Uxlabel106
#define subMenu_dtk_status      UxDTKManagerContext->UxsubMenu_dtk_status
#define subMenu_dtk_status_PLN  UxDTKManagerContext->UxsubMenu_dtk_status_PLN
#define subMenu_dtk_status_QUE  UxDTKManagerContext->UxsubMenu_dtk_status_QUE
#define subMenu_dtk_status_SUB  UxDTKManagerContext->UxsubMenu_dtk_status_SUB
#define subMenu_dtk_status_SCH  UxDTKManagerContext->UxsubMenu_dtk_status_SCH
#define subMenu_dtk_status_CON  UxDTKManagerContext->UxsubMenu_dtk_status_CON
#define subMenu_dtk_status_REJ  UxDTKManagerContext->UxsubMenu_dtk_status_REJ
#define subMenu_dtk_status_DEL  UxDTKManagerContext->UxsubMenu_dtk_status_DEL
#define subMenu_dtk_status_INV  UxDTKManagerContext->UxsubMenu_dtk_status_INV
#define optionMenu_dtk_status   UxDTKManagerContext->UxoptionMenu_dtk_status
#define pushButton_EditDTK      UxDTKManagerContext->UxpushButton_EditDTK
#define pushButton_DeleteDTK    UxDTKManagerContext->UxpushButton_DeleteDTK
#define pushButton_QuitDTK      UxDTKManagerContext->UxpushButton_QuitDTK
#define pushButton_ClearDTKForm UxDTKManagerContext->UxpushButton_ClearDTKForm
#define pushButton_SaveDTKChanges UxDTKManagerContext->UxpushButton_SaveDTKChanges
#define TF_DTK_recordcount      UxDTKManagerContext->UxTF_DTK_recordcount
#define label137                UxDTKManagerContext->Uxlabel137
#define menuBar_DTK_FILE        UxDTKManagerContext->UxmenuBar_DTK_FILE
#define menuBar_DTK_FILE_pane   UxDTKManagerContext->UxmenuBar_DTK_FILE_pane
#define PANE_SAVE_DTK_RPT       UxDTKManagerContext->UxPANE_SAVE_DTK_RPT
#define RPT_SELECTED_DTK        UxDTKManagerContext->UxRPT_SELECTED_DTK
#define RPT_CURRENT_DTKS        UxDTKManagerContext->UxRPT_CURRENT_DTKS
#define RPT_ALL_DTKS            UxDTKManagerContext->UxRPT_ALL_DTKS
#define cascadeButton_DTK_SAVE  UxDTKManagerContext->UxcascadeButton_DTK_SAVE
#define PANE_PRINT_DTK_RPT      UxDTKManagerContext->UxPANE_PRINT_DTK_RPT
#define PRINT_SELECTED_DTK      UxDTKManagerContext->UxPRINT_SELECTED_DTK
#define PRINT_CURRENT_DTKS      UxDTKManagerContext->UxPRINT_CURRENT_DTKS
#define PRINT_ALL_DTKS          UxDTKManagerContext->UxPRINT_ALL_DTKS
#define cascadeButton_DTK_PRINT UxDTKManagerContext->UxcascadeButton_DTK_PRINT
#define menuBar_cascade_button_DTK_FILE UxDTKManagerContext->UxmenuBar_cascade_button_DTK_FILE
#define pushButton_DTK_refresh  UxDTKManagerContext->UxpushButton_DTK_refresh
#define TF_STOPLAT              UxDTKManagerContext->UxTF_STOPLAT
#define TF_STRTLAT              UxDTKManagerContext->UxTF_STRTLAT
#define label146                UxDTKManagerContext->Uxlabel146
#define label112                UxDTKManagerContext->Uxlabel112
#define TF_NOTES                UxDTKManagerContext->UxTF_NOTES
#define label113                UxDTKManagerContext->Uxlabel113
#define subMenu_sat             UxDTKManagerContext->UxsubMenu_sat
#define subMenu_sat_ERS         UxDTKManagerContext->UxsubMenu_sat_ERS
#define optionMenu_sat          UxDTKManagerContext->UxoptionMenu_sat
#define subMenu_sensor          UxDTKManagerContext->UxsubMenu_sensor
#define subMenu_sensor_SAR      UxDTKManagerContext->UxsubMenu_sensor_SAR
#define optionMenu_sensor       UxDTKManagerContext->UxoptionMenu_sensor
#define subMenu_dtk_direction   UxDTKManagerContext->UxsubMenu_dtk_direction
#define subMenu_dtk_direction_ascend UxDTKManagerContext->UxsubMenu_dtk_direction_ascend
#define subMenu_dtk_direction_descend UxDTKManagerContext->UxsubMenu_dtk_direction_descend
#define subMenu_dtk_direction_cvrgNotAllowed UxDTKManagerContext->UxsubMenu_dtk_direction_cvrgNotAllowed
#define optionMenu_dtk_direction UxDTKManagerContext->UxoptionMenu_dtk_direction
#define pushButton_CreateDTK    UxDTKManagerContext->UxpushButton_CreateDTK
#define TF_DTKDATE              UxDTKManagerContext->UxTF_DTKDATE
#define label116                UxDTKManagerContext->Uxlabel116
#define TF_DTK_STRTTIME         UxDTKManagerContext->UxTF_DTK_STRTTIME
#define TF_DTK_STOPTIME         UxDTKManagerContext->UxTF_DTK_STOPTIME
#define scrolledWindow_FATapes  UxDTKManagerContext->UxscrolledWindow_FATapes
#define rowColumn2              UxDTKManagerContext->UxrowColumn2
#define FATape_ASF              UxDTKManagerContext->UxFATape_ASF
#define FATape_ESA              UxDTKManagerContext->UxFATape_ESA
#define FATape_NASDA            UxDTKManagerContext->UxFATape_NASDA
#define FATape_CSA              UxDTKManagerContext->UxFATape_CSA
#define label25                 UxDTKManagerContext->Uxlabel25
#define frameActivity           UxDTKManagerContext->UxframeActivity
#define rowColumn1              UxDTKManagerContext->UxrowColumn1
#define Activity_Downlink       UxDTKManagerContext->UxActivity_Downlink
#define Activity_Observe        UxDTKManagerContext->UxActivity_Observe
#define Activity_Dump           UxDTKManagerContext->UxActivity_Dump
#define Activity_Record         UxDTKManagerContext->UxActivity_Record
#define subMenu_J1_DLinkChannel UxDTKManagerContext->UxsubMenu_J1_DLinkChannel
#define subMenu_J1_DLink_00     UxDTKManagerContext->UxsubMenu_J1_DLink_00
#define subMenu_J1_DLink_F1     UxDTKManagerContext->UxsubMenu_J1_DLink_F1
#define subMenu_J1_DLink_F2     UxDTKManagerContext->UxsubMenu_J1_DLink_F2
#define subMenu_J1_DLink_CB     UxDTKManagerContext->UxsubMenu_J1_DLink_CB
#define subMenu_R1_DLink_F3     UxDTKManagerContext->UxsubMenu_R1_DLink_F3
#define subMenu_R1_DLink_F4     UxDTKManagerContext->UxsubMenu_R1_DLink_F4
#define subMenu_A1_DLink_F5     UxDTKManagerContext->UxsubMenu_A1_DLink_F5
#define subMenu_A1_DLink_F6     UxDTKManagerContext->UxsubMenu_A1_DLink_F6
#define subMenu_A1_DLink_F7     UxDTKManagerContext->UxsubMenu_A1_DLink_F7
#define optionMenu_J1_DLinkChannel UxDTKManagerContext->UxoptionMenu_J1_DLinkChannel
#define pushButton_GetCvrgPoints UxDTKManagerContext->UxpushButton_GetCvrgPoints
#define subMenu_dtkm_stnid      UxDTKManagerContext->UxsubMenu_dtkm_stnid
#define subMenu_dtkm_stnid_asf  UxDTKManagerContext->UxsubMenu_dtkm_stnid_asf
#define optionMenu_dtkm_station_id UxDTKManagerContext->UxoptionMenu_dtkm_station_id
#define label34                 UxDTKManagerContext->Uxlabel34
#define label_SciQuicklook      UxDTKManagerContext->Uxlabel_SciQuicklook
#define subMenu_antenna         UxDTKManagerContext->UxsubMenu_antenna
#define subMenu_antenna_1       UxDTKManagerContext->UxsubMenu_antenna_1
#define optionMenu_antenna      UxDTKManagerContext->UxoptionMenu_antenna
#define label_FA_Schedule_Link  UxDTKManagerContext->Uxlabel_FA_Schedule_Link
#define TF_FA_SCHEDULE_LINK     UxDTKManagerContext->UxTF_FA_SCHEDULE_LINK
#define separator8              UxDTKManagerContext->Uxseparator8
#define label63                 UxDTKManagerContext->Uxlabel63
#define subMenu_PlanQuicklook   UxDTKManagerContext->UxsubMenu_PlanQuicklook
#define subMenu_PlanQuicklook_no UxDTKManagerContext->UxsubMenu_PlanQuicklook_no
#define subMenu_PlanQuicklook_yes UxDTKManagerContext->UxsubMenu_PlanQuicklook_yes
#define optionMenu_PlanQuicklook UxDTKManagerContext->UxoptionMenu_PlanQuicklook
#define UxParent                UxDTKManagerContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	DTKManager;
extern Widget	pushButton_CancelDTKChanges;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_DTKManager( swidget _UxUxParent );

#endif	/* _VC_DTKMANAGER_INCLUDED */
