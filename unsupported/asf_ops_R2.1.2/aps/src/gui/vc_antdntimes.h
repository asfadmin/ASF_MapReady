
/*******************************************************************************
       vc_antdntimes.h
       This header file is included by vc_antdntimes.c

*******************************************************************************/

#ifndef	_VC_ANTDNTIMES_INCLUDED
#define	_VC_ANTDNTIMES_INCLUDED

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
	Widget	UxAntennaDownTimeManager;
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
	Widget	UxpushButton_ANTDNTIME_refresh;
	Widget	UxpushButton_CreateDownTime;
	Widget	Uxlabel155;
	Widget	UxTF_ANT_DN_TIMES_STRTTIME;
	Widget	UxTF_ANT_DN_TIMES_STOPTIME;
	Widget	UxTF_DownTime_total_days;
	Widget	Uxlabel122;
	Widget	Uxlabel123;
	Widget	UxT_ANT_DN_TIMES_COMMENTS;
	Widget	Uxlabel124;
	Widget	UxsubMenu_antenna_down_stn_id;
	Widget	UxsubMenu_antenna_down_stn_id_pb;
	Widget	UxoptionMenu_station_id;
	Widget	UxsubMenu_antenna_down_antenna;
	Widget	UxsubMenu_antenna_down_antenna_pb;
	Widget	UxoptionMenu_antenna;
	swidget	UxUxParent;
} _UxCAntennaDownTimeManager;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAntennaDownTimeManager *UxAntennaDownTimeManagerContext;
#define AntennaDownTimeManager  UxAntennaDownTimeManagerContext->UxAntennaDownTimeManager
#define label156                UxAntennaDownTimeManagerContext->Uxlabel156
#define scrolledWindowList5     UxAntennaDownTimeManagerContext->UxscrolledWindowList5
#define scrolledList_DownTimes  UxAntennaDownTimeManagerContext->UxscrolledList_DownTimes
#define pushButton_QuitDownTime UxAntennaDownTimeManagerContext->UxpushButton_QuitDownTime
#define pushButton_SortDownTime UxAntennaDownTimeManagerContext->UxpushButton_SortDownTime
#define label151                UxAntennaDownTimeManagerContext->Uxlabel151
#define separator9              UxAntennaDownTimeManagerContext->Uxseparator9
#define label153                UxAntennaDownTimeManagerContext->Uxlabel153
#define label154                UxAntennaDownTimeManagerContext->Uxlabel154
#define pushButton_EditDownTime UxAntennaDownTimeManagerContext->UxpushButton_EditDownTime
#define pushButton_SearchDownTime UxAntennaDownTimeManagerContext->UxpushButton_SearchDownTime
#define pushButton_DeleteDownTime UxAntennaDownTimeManagerContext->UxpushButton_DeleteDownTime
#define TF_DownTime_sortclause  UxAntennaDownTimeManagerContext->UxTF_DownTime_sortclause
#define TF_DownTime_recordcount UxAntennaDownTimeManagerContext->UxTF_DownTime_recordcount
#define label152                UxAntennaDownTimeManagerContext->Uxlabel152
#define pushButton_SaveDownTimeChanges UxAntennaDownTimeManagerContext->UxpushButton_SaveDownTimeChanges
#define pushButton_CancelDownTimeChanges UxAntennaDownTimeManagerContext->UxpushButton_CancelDownTimeChanges
#define TF_DownTime_searchclause UxAntennaDownTimeManagerContext->UxTF_DownTime_searchclause
#define pushButton_ANTDNTIME_refresh UxAntennaDownTimeManagerContext->UxpushButton_ANTDNTIME_refresh
#define pushButton_CreateDownTime UxAntennaDownTimeManagerContext->UxpushButton_CreateDownTime
#define label155                UxAntennaDownTimeManagerContext->Uxlabel155
#define TF_ANT_DN_TIMES_STRTTIME UxAntennaDownTimeManagerContext->UxTF_ANT_DN_TIMES_STRTTIME
#define TF_ANT_DN_TIMES_STOPTIME UxAntennaDownTimeManagerContext->UxTF_ANT_DN_TIMES_STOPTIME
#define TF_DownTime_total_days  UxAntennaDownTimeManagerContext->UxTF_DownTime_total_days
#define label122                UxAntennaDownTimeManagerContext->Uxlabel122
#define label123                UxAntennaDownTimeManagerContext->Uxlabel123
#define T_ANT_DN_TIMES_COMMENTS UxAntennaDownTimeManagerContext->UxT_ANT_DN_TIMES_COMMENTS
#define label124                UxAntennaDownTimeManagerContext->Uxlabel124
#define subMenu_antenna_down_stn_id UxAntennaDownTimeManagerContext->UxsubMenu_antenna_down_stn_id
#define subMenu_antenna_down_stn_id_pb UxAntennaDownTimeManagerContext->UxsubMenu_antenna_down_stn_id_pb
#define optionMenu_station_id   UxAntennaDownTimeManagerContext->UxoptionMenu_station_id
#define subMenu_antenna_down_antenna UxAntennaDownTimeManagerContext->UxsubMenu_antenna_down_antenna
#define subMenu_antenna_down_antenna_pb UxAntennaDownTimeManagerContext->UxsubMenu_antenna_down_antenna_pb
#define optionMenu_antenna      UxAntennaDownTimeManagerContext->UxoptionMenu_antenna
#define UxParent                UxAntennaDownTimeManagerContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_AntennaDownTimeManager( swidget _UxUxParent );

#endif	/* _VC_ANTDNTIMES_INCLUDED */
