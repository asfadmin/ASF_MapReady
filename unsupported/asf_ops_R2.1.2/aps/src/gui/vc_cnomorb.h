
/*******************************************************************************
       vc_cnomorb.h
       This header file is included by vc_cnomorb.c

*******************************************************************************/

#ifndef	_VC_CNOMORB_INCLUDED
#define	_VC_CNOMORB_INCLUDED

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
	Widget	UxCreateNominalOrbit;
	Widget	UxscrolledWindowList1;
	Widget	UxscrolledList2;
	Widget	Uxlabel1;
	Widget	Uxlabel2;
	Widget	Uxseparator1;
	Widget	UxtextField_sat;
	Widget	Uxlabel5;
	Widget	Uxlabel7;
	Widget	Uxlabel8;
	Widget	Uxlabel9;
	Widget	UxtextField_phase_name;
	Widget	UxtextField_cycle_days;
	Widget	UxtextField_cycle_revs;
	Widget	Uxlabel10;
	Widget	Uxlabel11;
	Widget	Uxlabel12;
	Widget	Uxlabel13;
	Widget	Uxlabel14;
	Widget	Uxlabel15;
	Widget	UxtextField_phase_start;
	Widget	UxtextField_phase_days;
	Widget	UxtextField_phase_orbits;
	Widget	UxtextField_last_rev;
	Widget	UxtextField_orb_a;
	Widget	UxtextField_orb_e;
	Widget	UxtextField_orb_i;
	Widget	UxtextField_phase_lon;
	Widget	Uxlabel16;
	Widget	UxtextField_orb_arg_peri;
	Widget	Uxlabel17;
	Widget	Uxlabel18;
	Widget	Uxlabel19;
	Widget	Uxlabel20;
	Widget	Uxlabel21;
	Widget	UxpushButton_CreateVectorFile;
	Widget	UxpushButton_cnomorb_quit;
	Widget	UxtextField_phase_days2;
	Widget	UxtextField_cycle_days2;
	Widget	UxtextField_cycle_revs2;
	Widget	Uxlabel23;
	Widget	UxtextField_phase_lon2;
	Widget	Uxlabel24;
	Widget	Uxlabel26;
	Widget	Uxseparator2;
	Widget	Uxlabel3;
	Widget	UxscrolledWindowText3;
	Widget	UxscrolledText_cnomorb_status;
	Widget	UxpushButton_refresh1;
	swidget	UxUxParent;
} _UxCCreateNominalOrbit;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCreateNominalOrbit  *UxCreateNominalOrbitContext;
#define CreateNominalOrbit      UxCreateNominalOrbitContext->UxCreateNominalOrbit
#define scrolledWindowList1     UxCreateNominalOrbitContext->UxscrolledWindowList1
#define scrolledList2           UxCreateNominalOrbitContext->UxscrolledList2
#define label1                  UxCreateNominalOrbitContext->Uxlabel1
#define label2                  UxCreateNominalOrbitContext->Uxlabel2
#define separator1              UxCreateNominalOrbitContext->Uxseparator1
#define textField_sat           UxCreateNominalOrbitContext->UxtextField_sat
#define label5                  UxCreateNominalOrbitContext->Uxlabel5
#define label7                  UxCreateNominalOrbitContext->Uxlabel7
#define label8                  UxCreateNominalOrbitContext->Uxlabel8
#define label9                  UxCreateNominalOrbitContext->Uxlabel9
#define textField_phase_name    UxCreateNominalOrbitContext->UxtextField_phase_name
#define textField_cycle_days    UxCreateNominalOrbitContext->UxtextField_cycle_days
#define textField_cycle_revs    UxCreateNominalOrbitContext->UxtextField_cycle_revs
#define label10                 UxCreateNominalOrbitContext->Uxlabel10
#define label11                 UxCreateNominalOrbitContext->Uxlabel11
#define label12                 UxCreateNominalOrbitContext->Uxlabel12
#define label13                 UxCreateNominalOrbitContext->Uxlabel13
#define label14                 UxCreateNominalOrbitContext->Uxlabel14
#define label15                 UxCreateNominalOrbitContext->Uxlabel15
#define textField_phase_start   UxCreateNominalOrbitContext->UxtextField_phase_start
#define textField_phase_days    UxCreateNominalOrbitContext->UxtextField_phase_days
#define textField_phase_orbits  UxCreateNominalOrbitContext->UxtextField_phase_orbits
#define textField_last_rev      UxCreateNominalOrbitContext->UxtextField_last_rev
#define textField_orb_a         UxCreateNominalOrbitContext->UxtextField_orb_a
#define textField_orb_e         UxCreateNominalOrbitContext->UxtextField_orb_e
#define textField_orb_i         UxCreateNominalOrbitContext->UxtextField_orb_i
#define textField_phase_lon     UxCreateNominalOrbitContext->UxtextField_phase_lon
#define label16                 UxCreateNominalOrbitContext->Uxlabel16
#define textField_orb_arg_peri  UxCreateNominalOrbitContext->UxtextField_orb_arg_peri
#define label17                 UxCreateNominalOrbitContext->Uxlabel17
#define label18                 UxCreateNominalOrbitContext->Uxlabel18
#define label19                 UxCreateNominalOrbitContext->Uxlabel19
#define label20                 UxCreateNominalOrbitContext->Uxlabel20
#define label21                 UxCreateNominalOrbitContext->Uxlabel21
#define pushButton_CreateVectorFile UxCreateNominalOrbitContext->UxpushButton_CreateVectorFile
#define pushButton_cnomorb_quit UxCreateNominalOrbitContext->UxpushButton_cnomorb_quit
#define textField_phase_days2   UxCreateNominalOrbitContext->UxtextField_phase_days2
#define textField_cycle_days2   UxCreateNominalOrbitContext->UxtextField_cycle_days2
#define textField_cycle_revs2   UxCreateNominalOrbitContext->UxtextField_cycle_revs2
#define label23                 UxCreateNominalOrbitContext->Uxlabel23
#define textField_phase_lon2    UxCreateNominalOrbitContext->UxtextField_phase_lon2
#define label24                 UxCreateNominalOrbitContext->Uxlabel24
#define label26                 UxCreateNominalOrbitContext->Uxlabel26
#define separator2              UxCreateNominalOrbitContext->Uxseparator2
#define label3                  UxCreateNominalOrbitContext->Uxlabel3
#define scrolledWindowText3     UxCreateNominalOrbitContext->UxscrolledWindowText3
#define scrolledText_cnomorb_status UxCreateNominalOrbitContext->UxscrolledText_cnomorb_status
#define pushButton_refresh1     UxCreateNominalOrbitContext->UxpushButton_refresh1
#define UxParent                UxCreateNominalOrbitContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CreateNominalOrbit( swidget _UxUxParent );

#endif	/* _VC_CNOMORB_INCLUDED */
