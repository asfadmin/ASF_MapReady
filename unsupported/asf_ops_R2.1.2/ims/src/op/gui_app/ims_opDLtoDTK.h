
/*******************************************************************************
       ims_opDLtoDTK.h
       This header file is included by ims_opDLtoDTK.c

*******************************************************************************/

#ifndef	_IMS_OPDLTODTK_INCLUDED
#define	_IMS_OPDLTODTK_INCLUDED

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
	Widget	UxDLtoDTK;
	Widget	UxdownlinkMB;
	Widget	UxgotoP;
	Widget	UxwelcomeScreenMPB;
	Widget	UxdownlinkMB_p1_b3;
	Widget	UxsearchScreenMPB;
	Widget	UxdownlinkMB_p1_b4;
	Widget	UxcloseScreenMPB;
	Widget	UxdownlinkMB_p1_top_b2;
	Widget	UxhelpP;
	Widget	UxdownlinkMB_p3_b2;
	Widget	UxdownlinkMB_p3_top_b2;
	Widget	UxdownlinkP;
	Widget	UxviewDownlinkDetailsMPB;
	Widget	UxdownlinkP_Sep1;
	Widget	UxviewDTKsMPB;
	Widget	UxdownlinkMB_top_b3;
	Widget	UxdatatakeP;
	Widget	UxviewDTKDetailsMPB;
	Widget	UxdownlinkMB_p5_b1;
	Widget	UxtoggleProcAuthFlagMPB;
	Widget	UxdownlinkMB_p5_b2;
	Widget	UxresetDTK_MPB;
	Widget	UxdownlinkMB_top_b5;
	Widget	UxscreenFuncP;
	Widget	UxrefreshSearchMPB;
	Widget	UxdownlinkMB_p10_b1;
	Widget	UxsaveChangesMPB;
	Widget	UxdownlinkMB_p10_b3;
	Widget	UxprintScreenMPB;
	Widget	UxdownlinkMB_top_b6;
	Widget	Uxlabel155;
	Widget	UxdownlinkIdLBL;
	Widget	UxdlActivityLBL;
	Widget	UxdlStationLBL;
	Widget	UxdlAntennaLBL;
	Widget	UxdlTimeOnLBL;
	Widget	UxdlTimeOffLBL;
	Widget	UxdlStatusLBL;
	Widget	UxdlNumDTKsLBL;
	Widget	UxdlPlatformSW;
	Widget	UxdlPlatformLIST;
	Widget	UxdlSensorSW;
	Widget	UxdlSensorLIST;
	Widget	UxdlRevSW;
	Widget	UxdlRevLIST;
	Widget	UxdlSequenceSW;
	Widget	UxdlSequenceLIST;
	Widget	UxdlActivitySW;
	Widget	UxdlActivityLIST;
	Widget	UxdlStationSW;
	Widget	UxdlStationLIST;
	Widget	UxdlAntennaSW;
	Widget	UxdlAntennaLIST;
	Widget	UxdlTimeOnSW;
	Widget	UxdlTimeOnLIST;
	Widget	UxdlTimeOffSW;
	Widget	UxdlTimeOffLIST;
	Widget	UxdlStatusSW;
	Widget	UxdlStatusLIST;
	Widget	UxdlNumDTKsSW;
	Widget	UxdlNumDTKsLIST;
	Widget	UxdlDummySW;
	Widget	UxdlDummyLIST;
	Widget	Uxseparator12;
	Widget	UxdtkIdLBL;
	Widget	UxdtkSensorModeLBL;
	Widget	UxdtkTimeOnLBL;
	Widget	UxdtkTimeOffLBL;
	Widget	UxdtkSiteNameLBL;
	Widget	UxdtkQuicklookLBL;
	Widget	UxdtkFrameModeLBL;
	Widget	UxdtkProcAuthLBL;
	Widget	UxdtklPlatformSW;
	Widget	UxdtkPlatformLIST;
	Widget	UxdtkSensorSW;
	Widget	UxdtkSensorLIST;
	Widget	UxdtkRevSW;
	Widget	UxdtkRevLIST;
	Widget	UxdtkSequenceSW;
	Widget	UxdtkSequenceLIST;
	Widget	UxdtkSensorModeSW;
	Widget	UxdtkSensorModeLIST;
	Widget	UxdtkTimeOnSW;
	Widget	UxdtkTimeOnLIST;
	Widget	UxdtkTimeOffSW;
	Widget	UxdtkTimeOffLIST;
	Widget	UxdtkSiteNameSW;
	Widget	UxdtkSiteNameLIST;
	Widget	UxdtkFrameModeSW;
	Widget	UxdtkFrameModeLIST;
	Widget	UxdtkQuicklookSW;
	Widget	UxdtkQuicklookLIST;
	Widget	UxdtkProcAuthFRAME;
	Widget	UxdtkProcAuthSW;
	Widget	UxdtkProcAuthLIST;
	Widget	UxProcFlagFootnote;
	Widget	UxdtkDummySW;
	Widget	UxdtkDummyLIST;
	Widget	Uxdl2dtkUpdatePB;
	Widget	Uxdl2dtkRefreshSearchPB;
	Widget	Uxdl2dtkPrintScreenPB;
	Widget	Uxdl2dtkCloseScreenPB;
	Widget	UxdownlinkIdLBL1;
	Widget	UxdlTotalNumDlksLBL;
	Widget	Uxlabel21;
	Widget	UxdlTotalDlksTF;
	Widget	UxdtkTotalDksTF;
	swidget	UxUxParent;
} _UxCDLtoDTK;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCDLtoDTK             *UxDLtoDTKContext;
#define DLtoDTK                 UxDLtoDTKContext->UxDLtoDTK
#define downlinkMB              UxDLtoDTKContext->UxdownlinkMB
#define gotoP                   UxDLtoDTKContext->UxgotoP
#define welcomeScreenMPB        UxDLtoDTKContext->UxwelcomeScreenMPB
#define downlinkMB_p1_b3        UxDLtoDTKContext->UxdownlinkMB_p1_b3
#define searchScreenMPB         UxDLtoDTKContext->UxsearchScreenMPB
#define downlinkMB_p1_b4        UxDLtoDTKContext->UxdownlinkMB_p1_b4
#define closeScreenMPB          UxDLtoDTKContext->UxcloseScreenMPB
#define downlinkMB_p1_top_b2    UxDLtoDTKContext->UxdownlinkMB_p1_top_b2
#define helpP                   UxDLtoDTKContext->UxhelpP
#define downlinkMB_p3_b2        UxDLtoDTKContext->UxdownlinkMB_p3_b2
#define downlinkMB_p3_top_b2    UxDLtoDTKContext->UxdownlinkMB_p3_top_b2
#define downlinkP               UxDLtoDTKContext->UxdownlinkP
#define viewDownlinkDetailsMPB  UxDLtoDTKContext->UxviewDownlinkDetailsMPB
#define downlinkP_Sep1          UxDLtoDTKContext->UxdownlinkP_Sep1
#define viewDTKsMPB             UxDLtoDTKContext->UxviewDTKsMPB
#define downlinkMB_top_b3       UxDLtoDTKContext->UxdownlinkMB_top_b3
#define datatakeP               UxDLtoDTKContext->UxdatatakeP
#define viewDTKDetailsMPB       UxDLtoDTKContext->UxviewDTKDetailsMPB
#define downlinkMB_p5_b1        UxDLtoDTKContext->UxdownlinkMB_p5_b1
#define toggleProcAuthFlagMPB   UxDLtoDTKContext->UxtoggleProcAuthFlagMPB
#define downlinkMB_p5_b2        UxDLtoDTKContext->UxdownlinkMB_p5_b2
#define resetDTK_MPB            UxDLtoDTKContext->UxresetDTK_MPB
#define downlinkMB_top_b5       UxDLtoDTKContext->UxdownlinkMB_top_b5
#define screenFuncP             UxDLtoDTKContext->UxscreenFuncP
#define refreshSearchMPB        UxDLtoDTKContext->UxrefreshSearchMPB
#define downlinkMB_p10_b1       UxDLtoDTKContext->UxdownlinkMB_p10_b1
#define saveChangesMPB          UxDLtoDTKContext->UxsaveChangesMPB
#define downlinkMB_p10_b3       UxDLtoDTKContext->UxdownlinkMB_p10_b3
#define printScreenMPB          UxDLtoDTKContext->UxprintScreenMPB
#define downlinkMB_top_b6       UxDLtoDTKContext->UxdownlinkMB_top_b6
#define label155                UxDLtoDTKContext->Uxlabel155
#define downlinkIdLBL           UxDLtoDTKContext->UxdownlinkIdLBL
#define dlActivityLBL           UxDLtoDTKContext->UxdlActivityLBL
#define dlStationLBL            UxDLtoDTKContext->UxdlStationLBL
#define dlAntennaLBL            UxDLtoDTKContext->UxdlAntennaLBL
#define dlTimeOnLBL             UxDLtoDTKContext->UxdlTimeOnLBL
#define dlTimeOffLBL            UxDLtoDTKContext->UxdlTimeOffLBL
#define dlStatusLBL             UxDLtoDTKContext->UxdlStatusLBL
#define dlNumDTKsLBL            UxDLtoDTKContext->UxdlNumDTKsLBL
#define dlPlatformSW            UxDLtoDTKContext->UxdlPlatformSW
#define dlPlatformLIST          UxDLtoDTKContext->UxdlPlatformLIST
#define dlSensorSW              UxDLtoDTKContext->UxdlSensorSW
#define dlSensorLIST            UxDLtoDTKContext->UxdlSensorLIST
#define dlRevSW                 UxDLtoDTKContext->UxdlRevSW
#define dlRevLIST               UxDLtoDTKContext->UxdlRevLIST
#define dlSequenceSW            UxDLtoDTKContext->UxdlSequenceSW
#define dlSequenceLIST          UxDLtoDTKContext->UxdlSequenceLIST
#define dlActivitySW            UxDLtoDTKContext->UxdlActivitySW
#define dlActivityLIST          UxDLtoDTKContext->UxdlActivityLIST
#define dlStationSW             UxDLtoDTKContext->UxdlStationSW
#define dlStationLIST           UxDLtoDTKContext->UxdlStationLIST
#define dlAntennaSW             UxDLtoDTKContext->UxdlAntennaSW
#define dlAntennaLIST           UxDLtoDTKContext->UxdlAntennaLIST
#define dlTimeOnSW              UxDLtoDTKContext->UxdlTimeOnSW
#define dlTimeOnLIST            UxDLtoDTKContext->UxdlTimeOnLIST
#define dlTimeOffSW             UxDLtoDTKContext->UxdlTimeOffSW
#define dlTimeOffLIST           UxDLtoDTKContext->UxdlTimeOffLIST
#define dlStatusSW              UxDLtoDTKContext->UxdlStatusSW
#define dlStatusLIST            UxDLtoDTKContext->UxdlStatusLIST
#define dlNumDTKsSW             UxDLtoDTKContext->UxdlNumDTKsSW
#define dlNumDTKsLIST           UxDLtoDTKContext->UxdlNumDTKsLIST
#define dlDummySW               UxDLtoDTKContext->UxdlDummySW
#define dlDummyLIST             UxDLtoDTKContext->UxdlDummyLIST
#define separator12             UxDLtoDTKContext->Uxseparator12
#define dtkIdLBL                UxDLtoDTKContext->UxdtkIdLBL
#define dtkSensorModeLBL        UxDLtoDTKContext->UxdtkSensorModeLBL
#define dtkTimeOnLBL            UxDLtoDTKContext->UxdtkTimeOnLBL
#define dtkTimeOffLBL           UxDLtoDTKContext->UxdtkTimeOffLBL
#define dtkSiteNameLBL          UxDLtoDTKContext->UxdtkSiteNameLBL
#define dtkQuicklookLBL         UxDLtoDTKContext->UxdtkQuicklookLBL
#define dtkFrameModeLBL         UxDLtoDTKContext->UxdtkFrameModeLBL
#define dtkProcAuthLBL          UxDLtoDTKContext->UxdtkProcAuthLBL
#define dtklPlatformSW          UxDLtoDTKContext->UxdtklPlatformSW
#define dtkPlatformLIST         UxDLtoDTKContext->UxdtkPlatformLIST
#define dtkSensorSW             UxDLtoDTKContext->UxdtkSensorSW
#define dtkSensorLIST           UxDLtoDTKContext->UxdtkSensorLIST
#define dtkRevSW                UxDLtoDTKContext->UxdtkRevSW
#define dtkRevLIST              UxDLtoDTKContext->UxdtkRevLIST
#define dtkSequenceSW           UxDLtoDTKContext->UxdtkSequenceSW
#define dtkSequenceLIST         UxDLtoDTKContext->UxdtkSequenceLIST
#define dtkSensorModeSW         UxDLtoDTKContext->UxdtkSensorModeSW
#define dtkSensorModeLIST       UxDLtoDTKContext->UxdtkSensorModeLIST
#define dtkTimeOnSW             UxDLtoDTKContext->UxdtkTimeOnSW
#define dtkTimeOnLIST           UxDLtoDTKContext->UxdtkTimeOnLIST
#define dtkTimeOffSW            UxDLtoDTKContext->UxdtkTimeOffSW
#define dtkTimeOffLIST          UxDLtoDTKContext->UxdtkTimeOffLIST
#define dtkSiteNameSW           UxDLtoDTKContext->UxdtkSiteNameSW
#define dtkSiteNameLIST         UxDLtoDTKContext->UxdtkSiteNameLIST
#define dtkFrameModeSW          UxDLtoDTKContext->UxdtkFrameModeSW
#define dtkFrameModeLIST        UxDLtoDTKContext->UxdtkFrameModeLIST
#define dtkQuicklookSW          UxDLtoDTKContext->UxdtkQuicklookSW
#define dtkQuicklookLIST        UxDLtoDTKContext->UxdtkQuicklookLIST
#define dtkProcAuthFRAME        UxDLtoDTKContext->UxdtkProcAuthFRAME
#define dtkProcAuthSW           UxDLtoDTKContext->UxdtkProcAuthSW
#define dtkProcAuthLIST         UxDLtoDTKContext->UxdtkProcAuthLIST
#define ProcFlagFootnote        UxDLtoDTKContext->UxProcFlagFootnote
#define dtkDummySW              UxDLtoDTKContext->UxdtkDummySW
#define dtkDummyLIST            UxDLtoDTKContext->UxdtkDummyLIST
#define dl2dtkUpdatePB          UxDLtoDTKContext->Uxdl2dtkUpdatePB
#define dl2dtkRefreshSearchPB   UxDLtoDTKContext->Uxdl2dtkRefreshSearchPB
#define dl2dtkPrintScreenPB     UxDLtoDTKContext->Uxdl2dtkPrintScreenPB
#define dl2dtkCloseScreenPB     UxDLtoDTKContext->Uxdl2dtkCloseScreenPB
#define downlinkIdLBL1          UxDLtoDTKContext->UxdownlinkIdLBL1
#define dlTotalNumDlksLBL       UxDLtoDTKContext->UxdlTotalNumDlksLBL
#define label21                 UxDLtoDTKContext->Uxlabel21
#define dlTotalDlksTF           UxDLtoDTKContext->UxdlTotalDlksTF
#define dtkTotalDksTF           UxDLtoDTKContext->UxdtkTotalDksTF
#define UxParent                UxDLtoDTKContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_DLtoDTK( swidget _UxUxParent );

#endif	/* _IMS_OPDLTODTK_INCLUDED */
