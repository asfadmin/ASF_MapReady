
/*******************************************************************************
       vc_cnomcov.h
       This header file is included by vc_cnomcov.c

*******************************************************************************/

#ifndef	_VC_CNOMCOV_INCLUDED
#define	_VC_CNOMCOV_INCLUDED

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
	Widget	UxCreateNominalCoverage;
	Widget	Uxlabel29;
	Widget	Uxlabel30;
	Widget	Uxlabel39;
	Widget	Uxlabel41;
	Widget	UxTF_ephemeris_file;
	Widget	Uxlabel40;
	Widget	Uxseparator3;
	Widget	Uxseparator4;
	Widget	Uxlabel43;
	Widget	UxtoggleButton_stoptime;
	Widget	UxpushButton_cnomcov_quit;
	Widget	UxpushButton_create_coverage;
	Widget	UxTF_total_days;
	Widget	Uxlabel44;
	Widget	UxscrolledWindowText1;
	Widget	UxscrolledText_cnomcov_status;
	Widget	UxpushButton_refresh;
	Widget	UxsubMenu_ncov_sensor;
	Widget	UxsubMenu_ncov_sensor_SAR;
	Widget	UxoptionMenu_ncov_sensor;
	Widget	UxscrolledWindowList2;
	Widget	UxscrolledList_ephm;
	Widget	UxLBL_coverage_filename;
	Widget	UxTF_coverage_filename;
	Widget	UxsubMenu_ncov_sat;
	Widget	UxsubMenu_ncov_sat_ERS1;
	Widget	UxoptionMenu_ncov_sat;
	Widget	UxsubMenu_covtype;
	Widget	UxT_PHASE_END;
	Widget	UxT_PHASE_START;
	Widget	Uxlabel28;
	Widget	Uxlabel31;
	Widget	Uxlabel32;
	Widget	Uxlabel35;
	swidget	UxUxParent;
} _UxCCreateNominalCoverage;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCreateNominalCoverage *UxCreateNominalCoverageContext;
#define CreateNominalCoverage   UxCreateNominalCoverageContext->UxCreateNominalCoverage
#define label29                 UxCreateNominalCoverageContext->Uxlabel29
#define label30                 UxCreateNominalCoverageContext->Uxlabel30
#define label39                 UxCreateNominalCoverageContext->Uxlabel39
#define label41                 UxCreateNominalCoverageContext->Uxlabel41
#define TF_ephemeris_file       UxCreateNominalCoverageContext->UxTF_ephemeris_file
#define label40                 UxCreateNominalCoverageContext->Uxlabel40
#define separator3              UxCreateNominalCoverageContext->Uxseparator3
#define separator4              UxCreateNominalCoverageContext->Uxseparator4
#define label43                 UxCreateNominalCoverageContext->Uxlabel43
#define toggleButton_stoptime   UxCreateNominalCoverageContext->UxtoggleButton_stoptime
#define pushButton_cnomcov_quit UxCreateNominalCoverageContext->UxpushButton_cnomcov_quit
#define pushButton_create_coverage UxCreateNominalCoverageContext->UxpushButton_create_coverage
#define TF_total_days           UxCreateNominalCoverageContext->UxTF_total_days
#define label44                 UxCreateNominalCoverageContext->Uxlabel44
#define scrolledWindowText1     UxCreateNominalCoverageContext->UxscrolledWindowText1
#define scrolledText_cnomcov_status UxCreateNominalCoverageContext->UxscrolledText_cnomcov_status
#define pushButton_refresh      UxCreateNominalCoverageContext->UxpushButton_refresh
#define subMenu_ncov_sensor     UxCreateNominalCoverageContext->UxsubMenu_ncov_sensor
#define subMenu_ncov_sensor_SAR UxCreateNominalCoverageContext->UxsubMenu_ncov_sensor_SAR
#define optionMenu_ncov_sensor  UxCreateNominalCoverageContext->UxoptionMenu_ncov_sensor
#define scrolledWindowList2     UxCreateNominalCoverageContext->UxscrolledWindowList2
#define scrolledList_ephm       UxCreateNominalCoverageContext->UxscrolledList_ephm
#define LBL_coverage_filename   UxCreateNominalCoverageContext->UxLBL_coverage_filename
#define TF_coverage_filename    UxCreateNominalCoverageContext->UxTF_coverage_filename
#define subMenu_ncov_sat        UxCreateNominalCoverageContext->UxsubMenu_ncov_sat
#define subMenu_ncov_sat_ERS1   UxCreateNominalCoverageContext->UxsubMenu_ncov_sat_ERS1
#define optionMenu_ncov_sat     UxCreateNominalCoverageContext->UxoptionMenu_ncov_sat
#define subMenu_covtype         UxCreateNominalCoverageContext->UxsubMenu_covtype
#define T_PHASE_END             UxCreateNominalCoverageContext->UxT_PHASE_END
#define T_PHASE_START           UxCreateNominalCoverageContext->UxT_PHASE_START
#define label28                 UxCreateNominalCoverageContext->Uxlabel28
#define label31                 UxCreateNominalCoverageContext->Uxlabel31
#define label32                 UxCreateNominalCoverageContext->Uxlabel32
#define label35                 UxCreateNominalCoverageContext->Uxlabel35
#define UxParent                UxCreateNominalCoverageContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	subMenu_covtype_STN;
extern Widget	subMenu_covtype_GBL;
extern Widget	optionMenu_covtype;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CreateNominalCoverage( swidget _UxUxParent );

#endif	/* _VC_CNOMCOV_INCLUDED */
