
/*******************************************************************************
       vc_darmanager.h
       This header file is included by vc_darmanager.c

*******************************************************************************/

#ifndef	_VC_DARMANAGER_INCLUDED
#define	_VC_DARMANAGER_INCLUDED

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
	Widget	UxDARManager;
	Widget	Uxlabel78;
	Widget	UxscrolledWindow_DARS;
	Widget	UxscrolledList_DARS;
	Widget	UxTF_DAR_DARID;
	Widget	UxpushButton_QuitDAR;
	Widget	UxpushButton_SortDAR;
	Widget	Uxlabel133;
	Widget	Uxform_DARcircle;
	Widget	UxTF_DAR_center_lon;
	Widget	UxTF_DAR_center_lat;
	Widget	Uxlabel92;
	Widget	Uxlabel93;
	Widget	Uxlabel94;
	Widget	UxTF_RADIUS;
	Widget	Uxlabel95;
	Widget	UxTF_DAR_STRTTIME;
	Widget	UxTF_DAR_ENDTIME;
	Widget	UxTF_DAR_total_days;
	Widget	Uxlabel99;
	Widget	Uxform_DARquad;
	Widget	UxTF_NWLON;
	Widget	Uxlabel134;
	Widget	UxTF_NELON;
	Widget	Uxlabel131;
	Widget	Uxlabel_NE1;
	Widget	UxTF_SWLON;
	Widget	Uxlabel130;
	Widget	Uxlabel89;
	Widget	UxTF_SELAT;
	Widget	UxTF_SELON;
	Widget	Uxlabel132;
	Widget	Uxlabel91;
	Widget	Uxlabel_NW1;
	Widget	UxTF_NELAT;
	Widget	UxTF_NWLAT;
	Widget	UxTF_SWLAT;
	Widget	UxTF_DAR_SITENAME;
	Widget	Uxlabel80;
	Widget	UxTF_DAR_SHAPE;
	Widget	Uxlabel81;
	Widget	UxTF_USERID;
	Widget	UxTF_SAT;
	Widget	Uxlabel101;
	Widget	UxTF_SENSOR;
	Widget	Uxlabel102;
	Widget	UxTF_REQTIME;
	Widget	Uxseparator13;
	Widget	UxTF_DAR_REV;
	Widget	Uxlabel128;
	Widget	Uxlabel100;
	Widget	Uxlabel104;
	Widget	UxT_PLNRCMNT;
	Widget	UxTF_NOBS;
	Widget	Uxlabel97;
	Widget	Uxlabel105;
	Widget	UxTF_ASCDSC;
	Widget	Uxlabel84;
	Widget	Uxlabel96;
	Widget	UxT_USERCMNT;
	Widget	UxTF_REQSTAT;
	Widget	UxDAR_status_options;
	Widget	UxDAR_status_QUE;
	Widget	UxDAR_status_PLN;
	Widget	UxDAR_menu_status;
	Widget	UxpushButton_EditDAR;
	Widget	UxpushButton_SearchDAR;
	Widget	UxmenuBar_DELETE;
	Widget	UxmenuBar_p1;
	Widget	UxmenuBar_p_b_REJECTED;
	Widget	UxmenuBar_p_b_COMPLETED;
	Widget	UxmenuBar_top_b1;
	Widget	UxTF_DAR_sortclause;
	Widget	UxTF_DAR_recordcount;
	Widget	Uxlabel108;
	Widget	UxpushButton_SaveDARChanges;
	Widget	UxmenuBar_LOAD_DAR;
	Widget	UxmenuBar_FILE;
	Widget	UxpushButton_LOAD_DARS;
	Widget	UxPANE_SAVE_DAR_RPT;
	Widget	UxRPT_SELECTED_DAR;
	Widget	UxRPT_CURRENT_DARS;
	Widget	UxRPT_ALL_DARS;
	Widget	UxcascadeButton_SAVE_DAR_RPT;
	Widget	UxPANE_PRINT_DAR_RPT;
	Widget	UxPANE_PRINT_DARS_b4;
	Widget	UxPRINT_SELECTED_DAR;
	Widget	UxPRINT_CURRENT_DARS;
	Widget	UxPRINT_ALL_DARS;
	Widget	UxcascadeButton_PRINT_DAR_RPT;
	Widget	UxmenuBar_top_b4;
	Widget	UxpushButton_CancelDARChanges;
	Widget	UxTF_DAR_searchclause;
	Widget	UxscrolledWindowText7;
	Widget	UxscrolledWindowText8;
	Widget	UxpushButton2;
	Widget	Uxlabel125;
	Widget	Uxlabel126;
	Widget	Uxlabel49;
	Widget	UxLABEL_QUICKLOOK;
	Widget	UxT_FOBS;
	Widget	Uxlabel79;
	Widget	UxTF_J1OBS;
	swidget	UxUxParent;
} _UxCDARManager;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCDARManager          *UxDARManagerContext;
#define DARManager              UxDARManagerContext->UxDARManager
#define label78                 UxDARManagerContext->Uxlabel78
#define scrolledWindow_DARS     UxDARManagerContext->UxscrolledWindow_DARS
#define scrolledList_DARS       UxDARManagerContext->UxscrolledList_DARS
#define TF_DAR_DARID            UxDARManagerContext->UxTF_DAR_DARID
#define pushButton_QuitDAR      UxDARManagerContext->UxpushButton_QuitDAR
#define pushButton_SortDAR      UxDARManagerContext->UxpushButton_SortDAR
#define label133                UxDARManagerContext->Uxlabel133
#define form_DARcircle          UxDARManagerContext->Uxform_DARcircle
#define TF_DAR_center_lon       UxDARManagerContext->UxTF_DAR_center_lon
#define TF_DAR_center_lat       UxDARManagerContext->UxTF_DAR_center_lat
#define label92                 UxDARManagerContext->Uxlabel92
#define label93                 UxDARManagerContext->Uxlabel93
#define label94                 UxDARManagerContext->Uxlabel94
#define TF_RADIUS               UxDARManagerContext->UxTF_RADIUS
#define label95                 UxDARManagerContext->Uxlabel95
#define TF_DAR_STRTTIME         UxDARManagerContext->UxTF_DAR_STRTTIME
#define TF_DAR_ENDTIME          UxDARManagerContext->UxTF_DAR_ENDTIME
#define TF_DAR_total_days       UxDARManagerContext->UxTF_DAR_total_days
#define label99                 UxDARManagerContext->Uxlabel99
#define form_DARquad            UxDARManagerContext->Uxform_DARquad
#define TF_NWLON                UxDARManagerContext->UxTF_NWLON
#define label134                UxDARManagerContext->Uxlabel134
#define TF_NELON                UxDARManagerContext->UxTF_NELON
#define label131                UxDARManagerContext->Uxlabel131
#define label_NE1               UxDARManagerContext->Uxlabel_NE1
#define TF_SWLON                UxDARManagerContext->UxTF_SWLON
#define label130                UxDARManagerContext->Uxlabel130
#define label89                 UxDARManagerContext->Uxlabel89
#define TF_SELAT                UxDARManagerContext->UxTF_SELAT
#define TF_SELON                UxDARManagerContext->UxTF_SELON
#define label132                UxDARManagerContext->Uxlabel132
#define label91                 UxDARManagerContext->Uxlabel91
#define label_NW1               UxDARManagerContext->Uxlabel_NW1
#define TF_NELAT                UxDARManagerContext->UxTF_NELAT
#define TF_NWLAT                UxDARManagerContext->UxTF_NWLAT
#define TF_SWLAT                UxDARManagerContext->UxTF_SWLAT
#define TF_DAR_SITENAME         UxDARManagerContext->UxTF_DAR_SITENAME
#define label80                 UxDARManagerContext->Uxlabel80
#define TF_DAR_SHAPE            UxDARManagerContext->UxTF_DAR_SHAPE
#define label81                 UxDARManagerContext->Uxlabel81
#define TF_USERID               UxDARManagerContext->UxTF_USERID
#define TF_SAT                  UxDARManagerContext->UxTF_SAT
#define label101                UxDARManagerContext->Uxlabel101
#define TF_SENSOR               UxDARManagerContext->UxTF_SENSOR
#define label102                UxDARManagerContext->Uxlabel102
#define TF_REQTIME              UxDARManagerContext->UxTF_REQTIME
#define separator13             UxDARManagerContext->Uxseparator13
#define TF_DAR_REV              UxDARManagerContext->UxTF_DAR_REV
#define label128                UxDARManagerContext->Uxlabel128
#define label100                UxDARManagerContext->Uxlabel100
#define label104                UxDARManagerContext->Uxlabel104
#define T_PLNRCMNT              UxDARManagerContext->UxT_PLNRCMNT
#define TF_NOBS                 UxDARManagerContext->UxTF_NOBS
#define label97                 UxDARManagerContext->Uxlabel97
#define label105                UxDARManagerContext->Uxlabel105
#define TF_ASCDSC               UxDARManagerContext->UxTF_ASCDSC
#define label84                 UxDARManagerContext->Uxlabel84
#define label96                 UxDARManagerContext->Uxlabel96
#define T_USERCMNT              UxDARManagerContext->UxT_USERCMNT
#define TF_REQSTAT              UxDARManagerContext->UxTF_REQSTAT
#define DAR_status_options      UxDARManagerContext->UxDAR_status_options
#define DAR_status_QUE          UxDARManagerContext->UxDAR_status_QUE
#define DAR_status_PLN          UxDARManagerContext->UxDAR_status_PLN
#define DAR_menu_status         UxDARManagerContext->UxDAR_menu_status
#define pushButton_EditDAR      UxDARManagerContext->UxpushButton_EditDAR
#define pushButton_SearchDAR    UxDARManagerContext->UxpushButton_SearchDAR
#define menuBar_DELETE          UxDARManagerContext->UxmenuBar_DELETE
#define menuBar_p1              UxDARManagerContext->UxmenuBar_p1
#define menuBar_p_b_REJECTED    UxDARManagerContext->UxmenuBar_p_b_REJECTED
#define menuBar_p_b_COMPLETED   UxDARManagerContext->UxmenuBar_p_b_COMPLETED
#define menuBar_top_b1          UxDARManagerContext->UxmenuBar_top_b1
#define TF_DAR_sortclause       UxDARManagerContext->UxTF_DAR_sortclause
#define TF_DAR_recordcount      UxDARManagerContext->UxTF_DAR_recordcount
#define label108                UxDARManagerContext->Uxlabel108
#define pushButton_SaveDARChanges UxDARManagerContext->UxpushButton_SaveDARChanges
#define menuBar_LOAD_DAR        UxDARManagerContext->UxmenuBar_LOAD_DAR
#define menuBar_FILE            UxDARManagerContext->UxmenuBar_FILE
#define pushButton_LOAD_DARS    UxDARManagerContext->UxpushButton_LOAD_DARS
#define PANE_SAVE_DAR_RPT       UxDARManagerContext->UxPANE_SAVE_DAR_RPT
#define RPT_SELECTED_DAR        UxDARManagerContext->UxRPT_SELECTED_DAR
#define RPT_CURRENT_DARS        UxDARManagerContext->UxRPT_CURRENT_DARS
#define RPT_ALL_DARS            UxDARManagerContext->UxRPT_ALL_DARS
#define cascadeButton_SAVE_DAR_RPT UxDARManagerContext->UxcascadeButton_SAVE_DAR_RPT
#define PANE_PRINT_DAR_RPT      UxDARManagerContext->UxPANE_PRINT_DAR_RPT
#define PANE_PRINT_DARS_b4      UxDARManagerContext->UxPANE_PRINT_DARS_b4
#define PRINT_SELECTED_DAR      UxDARManagerContext->UxPRINT_SELECTED_DAR
#define PRINT_CURRENT_DARS      UxDARManagerContext->UxPRINT_CURRENT_DARS
#define PRINT_ALL_DARS          UxDARManagerContext->UxPRINT_ALL_DARS
#define cascadeButton_PRINT_DAR_RPT UxDARManagerContext->UxcascadeButton_PRINT_DAR_RPT
#define menuBar_top_b4          UxDARManagerContext->UxmenuBar_top_b4
#define pushButton_CancelDARChanges UxDARManagerContext->UxpushButton_CancelDARChanges
#define TF_DAR_searchclause     UxDARManagerContext->UxTF_DAR_searchclause
#define scrolledWindowText7     UxDARManagerContext->UxscrolledWindowText7
#define scrolledWindowText8     UxDARManagerContext->UxscrolledWindowText8
#define pushButton2             UxDARManagerContext->UxpushButton2
#define label125                UxDARManagerContext->Uxlabel125
#define label126                UxDARManagerContext->Uxlabel126
#define label49                 UxDARManagerContext->Uxlabel49
#define LABEL_QUICKLOOK         UxDARManagerContext->UxLABEL_QUICKLOOK
#define T_FOBS                  UxDARManagerContext->UxT_FOBS
#define label79                 UxDARManagerContext->Uxlabel79
#define TF_J1OBS                UxDARManagerContext->UxTF_J1OBS
#define UxParent                UxDARManagerContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_DARManager( swidget _UxUxParent );

#endif	/* _VC_DARMANAGER_INCLUDED */
