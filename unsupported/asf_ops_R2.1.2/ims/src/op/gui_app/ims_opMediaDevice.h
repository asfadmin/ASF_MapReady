
/*******************************************************************************
       ims_opMediaDevice.h
       This header file is included by ims_opMediaDevice.c

*******************************************************************************/

#ifndef	_IMS_OPMEDIADEVICE_INCLUDED
#define	_IMS_OPMEDIADEVICE_INCLUDED

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
	Widget	UxmediaDevice;
	Widget	UxmediaSW;
	Widget	UxmediaRC;
	Widget	UxftpPB;
	Widget	UxftpAvailLBL;
	Widget	UxftpAllocLBL;
	Widget	UxftpJobStartLBL;
	Widget	UxftpQualLBL;
	Widget	UxftpJobDoneLBL;
	Widget	UxftpJobFailLBL;
	Widget	UxftpOfflineLBL;
	Widget	UxftpSW;
	Widget	UxftpST;
	Widget	Uxlabel189;
	Widget	Uxlabel190;
	Widget	Uxlabel191;
	Widget	Uxlabel192;
	Widget	Uxlabel193;
	Widget	Uxlabel194;
	Widget	Uxlabel195;
	Widget	Uxlabel196;
	Widget	UxclosePB;
	swidget	UxUxParent;
} _UxCmediaDevice;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCmediaDevice         *UxMediaDeviceContext;
#define mediaDevice             UxMediaDeviceContext->UxmediaDevice
#define mediaSW                 UxMediaDeviceContext->UxmediaSW
#define mediaRC                 UxMediaDeviceContext->UxmediaRC
#define ftpPB                   UxMediaDeviceContext->UxftpPB
#define ftpAvailLBL             UxMediaDeviceContext->UxftpAvailLBL
#define ftpAllocLBL             UxMediaDeviceContext->UxftpAllocLBL
#define ftpJobStartLBL          UxMediaDeviceContext->UxftpJobStartLBL
#define ftpQualLBL              UxMediaDeviceContext->UxftpQualLBL
#define ftpJobDoneLBL           UxMediaDeviceContext->UxftpJobDoneLBL
#define ftpJobFailLBL           UxMediaDeviceContext->UxftpJobFailLBL
#define ftpOfflineLBL           UxMediaDeviceContext->UxftpOfflineLBL
#define ftpSW                   UxMediaDeviceContext->UxftpSW
#define ftpST                   UxMediaDeviceContext->UxftpST
#define label189                UxMediaDeviceContext->Uxlabel189
#define label190                UxMediaDeviceContext->Uxlabel190
#define label191                UxMediaDeviceContext->Uxlabel191
#define label192                UxMediaDeviceContext->Uxlabel192
#define label193                UxMediaDeviceContext->Uxlabel193
#define label194                UxMediaDeviceContext->Uxlabel194
#define label195                UxMediaDeviceContext->Uxlabel195
#define label196                UxMediaDeviceContext->Uxlabel196
#define closePB                 UxMediaDeviceContext->UxclosePB
#define UxParent                UxMediaDeviceContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_mediaDevice( swidget _UxUxParent );

#endif	/* _IMS_OPMEDIADEVICE_INCLUDED */
