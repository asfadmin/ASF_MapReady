
/*******************************************************************************
       vc_prcapsfile.h
       This header file is included by vc_prcapsfile.c

*******************************************************************************/

#ifndef	_VC_PRCAPSFILE_INCLUDED
#define	_VC_PRCAPSFILE_INCLUDED

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
	Widget	Uxlabel161;
	Widget	UxscrolledWindowList8;
	Widget	UxscrolledList_reports_inbound;
	Widget	UxpushButton_APSFileProcQuit;
	Widget	Uxlabel160;
	Widget	UxpushButton_process;
	Widget	UxscrolledWindowText9;
	Widget	UxscrolledText_procfile;
	Widget	Uxseparator11;
	Widget	Uxlabel166;
	Widget	Uxlabel33;
	Widget	UxpushButton_stop;
	Widget	UxgetList_menuBar;
	Widget	UxgetList_menuBar_p;
	Widget	UxgetList_menuBar_top_b;
	swidget	UxUxParent;
} _UxCAPSFileProcessing;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSFileProcessing   *UxAPSFileProcessingContext;
#define label161                UxAPSFileProcessingContext->Uxlabel161
#define scrolledWindowList8     UxAPSFileProcessingContext->UxscrolledWindowList8
#define scrolledList_reports_inbound UxAPSFileProcessingContext->UxscrolledList_reports_inbound
#define pushButton_APSFileProcQuit UxAPSFileProcessingContext->UxpushButton_APSFileProcQuit
#define label160                UxAPSFileProcessingContext->Uxlabel160
#define pushButton_process      UxAPSFileProcessingContext->UxpushButton_process
#define scrolledWindowText9     UxAPSFileProcessingContext->UxscrolledWindowText9
#define scrolledText_procfile   UxAPSFileProcessingContext->UxscrolledText_procfile
#define separator11             UxAPSFileProcessingContext->Uxseparator11
#define label166                UxAPSFileProcessingContext->Uxlabel166
#define label33                 UxAPSFileProcessingContext->Uxlabel33
#define pushButton_stop         UxAPSFileProcessingContext->UxpushButton_stop
#define getList_menuBar         UxAPSFileProcessingContext->UxgetList_menuBar
#define getList_menuBar_p       UxAPSFileProcessingContext->UxgetList_menuBar_p
#define getList_menuBar_top_b   UxAPSFileProcessingContext->UxgetList_menuBar_top_b
#define UxParent                UxAPSFileProcessingContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	APSFileProcessing;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSFileProcessing( swidget _UxUxParent );

#endif	/* _VC_PRCAPSFILE_INCLUDED */
