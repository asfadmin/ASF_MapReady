
/*******************************************************************************
       vc_rgsdntimes.h
       This header file is included by vc_rgsdntimes.c

*******************************************************************************/

#ifndef	_VC_RGSDNTIMES_INCLUDED
#define	_VC_RGSDNTIMES_INCLUDED

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
	Widget	UxRGSDownTimeManager;
	Widget	Uxlabel156;
	Widget	UxscrolledWindowList5;
	Widget	UxscrolledList_DownTimes;
	Widget	UxpushButton_QuitDownTime;
	Widget	UxpushButton_SortDownTime;
	Widget	Uxlabel151;
	Widget	Uxseparator9;
	Widget	Uxlabel153;
	Widget	Uxlabel154;
	Widget	UxpushButton_EditDownTime;
	Widget	UxpushButton_SearchDownTime;
	Widget	UxpushButton_DeleteDownTime;
	Widget	UxTF_DownTime_sortclause;
	Widget	UxTF_DownTime_recordcount;
	Widget	Uxlabel152;
	Widget	UxpushButton_SaveDownTimeChanges;
	Widget	UxpushButton_CancelDownTimeChanges;
	Widget	UxTF_DownTime_searchclause;
	Widget	UxpushButton_ASFDNTIME_refresh;
	Widget	UxsubMenu_ASFdown_type;
	Widget	UxASFdown_type_PLANNED;
	Widget	UxASFdown_type_UNPLANNED;
	Widget	UxoptionMenu_ASFdown_type;
	Widget	UxsubMenu_ASFdown_reason;
	Widget	UxASFdown_reason_CONFLICT;
	Widget	UxASFdown_reason_MAINTENANCE;
	Widget	UxASFdown_reason_REPAIR;
	Widget	UxASFdown_reason_UPGRADE;
	Widget	UxoptionMenu_ASFdown_reason;
	Widget	UxpushButton_CreateDownTime;
	Widget	Uxlabel155;
	Widget	UxTF_ASF_DN_TIMES_STRTTIME;
	Widget	UxTF_ASF_DN_TIMES_STOPTIME;
	Widget	UxTF_DownTime_total_days;
	Widget	Uxlabel122;
	Widget	Uxlabel123;
	Widget	UxT_ASF_DN_TIMES_REMARKS;
	Widget	Uxlabel124;
	Widget	UxrowColumn4;
	Widget	UxtoggleButton_DownTimeScheduled;
	Widget	UxtoggleButton_DownTimeCancelled;
	Widget	UxsubMenu_rgs_down_stn_id;
	Widget	UxsubMenu_rgs_down_stn_id_pb;
	Widget	UxoptionMenu_station_id;
	Widget	Uxlabel22;
	Widget	Uxlabel27;
	Widget	UxTF_FA_Notification;
	swidget	UxUxParent;
} _UxCRGSDownTimeManager;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCRGSDownTimeManager  *UxRGSDownTimeManagerContext;
#define RGSDownTimeManager      UxRGSDownTimeManagerContext->UxRGSDownTimeManager
#define label156                UxRGSDownTimeManagerContext->Uxlabel156
#define scrolledWindowList5     UxRGSDownTimeManagerContext->UxscrolledWindowList5
#define scrolledList_DownTimes  UxRGSDownTimeManagerContext->UxscrolledList_DownTimes
#define pushButton_QuitDownTime UxRGSDownTimeManagerContext->UxpushButton_QuitDownTime
#define pushButton_SortDownTime UxRGSDownTimeManagerContext->UxpushButton_SortDownTime
#define label151                UxRGSDownTimeManagerContext->Uxlabel151
#define separator9              UxRGSDownTimeManagerContext->Uxseparator9
#define label153                UxRGSDownTimeManagerContext->Uxlabel153
#define label154                UxRGSDownTimeManagerContext->Uxlabel154
#define pushButton_EditDownTime UxRGSDownTimeManagerContext->UxpushButton_EditDownTime
#define pushButton_SearchDownTime UxRGSDownTimeManagerContext->UxpushButton_SearchDownTime
#define pushButton_DeleteDownTime UxRGSDownTimeManagerContext->UxpushButton_DeleteDownTime
#define TF_DownTime_sortclause  UxRGSDownTimeManagerContext->UxTF_DownTime_sortclause
#define TF_DownTime_recordcount UxRGSDownTimeManagerContext->UxTF_DownTime_recordcount
#define label152                UxRGSDownTimeManagerContext->Uxlabel152
#define pushButton_SaveDownTimeChanges UxRGSDownTimeManagerContext->UxpushButton_SaveDownTimeChanges
#define pushButton_CancelDownTimeChanges UxRGSDownTimeManagerContext->UxpushButton_CancelDownTimeChanges
#define TF_DownTime_searchclause UxRGSDownTimeManagerContext->UxTF_DownTime_searchclause
#define pushButton_ASFDNTIME_refresh UxRGSDownTimeManagerContext->UxpushButton_ASFDNTIME_refresh
#define subMenu_ASFdown_type    UxRGSDownTimeManagerContext->UxsubMenu_ASFdown_type
#define ASFdown_type_PLANNED    UxRGSDownTimeManagerContext->UxASFdown_type_PLANNED
#define ASFdown_type_UNPLANNED  UxRGSDownTimeManagerContext->UxASFdown_type_UNPLANNED
#define optionMenu_ASFdown_type UxRGSDownTimeManagerContext->UxoptionMenu_ASFdown_type
#define subMenu_ASFdown_reason  UxRGSDownTimeManagerContext->UxsubMenu_ASFdown_reason
#define ASFdown_reason_CONFLICT UxRGSDownTimeManagerContext->UxASFdown_reason_CONFLICT
#define ASFdown_reason_MAINTENANCE UxRGSDownTimeManagerContext->UxASFdown_reason_MAINTENANCE
#define ASFdown_reason_REPAIR   UxRGSDownTimeManagerContext->UxASFdown_reason_REPAIR
#define ASFdown_reason_UPGRADE  UxRGSDownTimeManagerContext->UxASFdown_reason_UPGRADE
#define optionMenu_ASFdown_reason UxRGSDownTimeManagerContext->UxoptionMenu_ASFdown_reason
#define pushButton_CreateDownTime UxRGSDownTimeManagerContext->UxpushButton_CreateDownTime
#define label155                UxRGSDownTimeManagerContext->Uxlabel155
#define TF_ASF_DN_TIMES_STRTTIME UxRGSDownTimeManagerContext->UxTF_ASF_DN_TIMES_STRTTIME
#define TF_ASF_DN_TIMES_STOPTIME UxRGSDownTimeManagerContext->UxTF_ASF_DN_TIMES_STOPTIME
#define TF_DownTime_total_days  UxRGSDownTimeManagerContext->UxTF_DownTime_total_days
#define label122                UxRGSDownTimeManagerContext->Uxlabel122
#define label123                UxRGSDownTimeManagerContext->Uxlabel123
#define T_ASF_DN_TIMES_REMARKS  UxRGSDownTimeManagerContext->UxT_ASF_DN_TIMES_REMARKS
#define label124                UxRGSDownTimeManagerContext->Uxlabel124
#define rowColumn4              UxRGSDownTimeManagerContext->UxrowColumn4
#define toggleButton_DownTimeScheduled UxRGSDownTimeManagerContext->UxtoggleButton_DownTimeScheduled
#define toggleButton_DownTimeCancelled UxRGSDownTimeManagerContext->UxtoggleButton_DownTimeCancelled
#define subMenu_rgs_down_stn_id UxRGSDownTimeManagerContext->UxsubMenu_rgs_down_stn_id
#define subMenu_rgs_down_stn_id_pb UxRGSDownTimeManagerContext->UxsubMenu_rgs_down_stn_id_pb
#define optionMenu_station_id   UxRGSDownTimeManagerContext->UxoptionMenu_station_id
#define label22                 UxRGSDownTimeManagerContext->Uxlabel22
#define label27                 UxRGSDownTimeManagerContext->Uxlabel27
#define TF_FA_Notification      UxRGSDownTimeManagerContext->UxTF_FA_Notification
#define UxParent                UxRGSDownTimeManagerContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_DownTimeManager( swidget _UxUxParent );

#endif	/* _VC_RGSDNTIMES_INCLUDED */
