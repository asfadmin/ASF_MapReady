
/*******************************************************************************
       vc_crtapsfile.h
       This header file is included by vc_crtapsfile.c

*******************************************************************************/

#ifndef	_VC_CRTAPSFILE_INCLUDED
#define	_VC_CRTAPSFILE_INCLUDED

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
	Widget	UxAPSFileGeneration;
	Widget	Uxlabel46;
	Widget	UxscrolledWindowList4;
	Widget	UxscrolledList_reports;
	Widget	Uxseparator6;
	Widget	Uxlabel64;
	Widget	Uxlabel69;
	Widget	UxpushButton_CreateReportFile;
	Widget	UxpushButton_APSFileGenQuit;
	Widget	UxscrolledWindowText4;
	Widget	Uxlabel75;
	Widget	UxtextField_reportname;
	Widget	Uxlabel77;
	Widget	UxTF_report_total_days;
	Widget	Uxlabel71;
	Widget	UxTF_report_stop;
	Widget	Uxlabel70;
	Widget	UxTF_report_start;
	Widget	UxpushButton_input_file1;
	Widget	UxpushButton_view;
	Widget	UxpushButton_transfer;
	Widget	Uxseparator7;
	Widget	Uxlabel6;
	swidget	UxUxParent;
} _UxCAPSFileGeneration;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSFileGeneration   *UxAPSFileGenerationContext;
#define APSFileGeneration       UxAPSFileGenerationContext->UxAPSFileGeneration
#define label46                 UxAPSFileGenerationContext->Uxlabel46
#define scrolledWindowList4     UxAPSFileGenerationContext->UxscrolledWindowList4
#define scrolledList_reports    UxAPSFileGenerationContext->UxscrolledList_reports
#define separator6              UxAPSFileGenerationContext->Uxseparator6
#define label64                 UxAPSFileGenerationContext->Uxlabel64
#define label69                 UxAPSFileGenerationContext->Uxlabel69
#define pushButton_CreateReportFile UxAPSFileGenerationContext->UxpushButton_CreateReportFile
#define pushButton_APSFileGenQuit UxAPSFileGenerationContext->UxpushButton_APSFileGenQuit
#define scrolledWindowText4     UxAPSFileGenerationContext->UxscrolledWindowText4
#define label75                 UxAPSFileGenerationContext->Uxlabel75
#define textField_reportname    UxAPSFileGenerationContext->UxtextField_reportname
#define label77                 UxAPSFileGenerationContext->Uxlabel77
#define TF_report_total_days    UxAPSFileGenerationContext->UxTF_report_total_days
#define label71                 UxAPSFileGenerationContext->Uxlabel71
#define TF_report_stop          UxAPSFileGenerationContext->UxTF_report_stop
#define label70                 UxAPSFileGenerationContext->Uxlabel70
#define TF_report_start         UxAPSFileGenerationContext->UxTF_report_start
#define pushButton_input_file1  UxAPSFileGenerationContext->UxpushButton_input_file1
#define pushButton_view         UxAPSFileGenerationContext->UxpushButton_view
#define pushButton_transfer     UxAPSFileGenerationContext->UxpushButton_transfer
#define separator7              UxAPSFileGenerationContext->Uxseparator7
#define label6                  UxAPSFileGenerationContext->Uxlabel6
#define UxParent                UxAPSFileGenerationContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	scrolledText_create_report;
extern Widget	ExtendTimes_tb;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSFileGeneration( swidget _UxUxParent );

#endif	/* _VC_CRTAPSFILE_INCLUDED */
