
/*******************************************************************************
       ims_op_accAssignDataset.h
       This header file is included by ims_op_accAssignDataset.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCASSIGNDATASET_INCLUDED
#define	_IMS_OP_ACCASSIGNDATASET_INCLUDED

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
	Widget	Uxlabel1;
	Widget	Uxframe1;
	Widget	Uxform1;
	Widget	Uxassigned_datasetsSW;
	Widget	Uxassigned_datasetsSL;
	Widget	UxscrolledWindowList4;
	Widget	UxorderSL;
	Widget	UxscrolledWindowList5;
	Widget	UxaddSL;
	Widget	UxscrolledWindowList6;
	Widget	UxgetSL;
	Widget	UxscrolledWindowList7;
	Widget	UxdeleteSL;
	Widget	UxreplaceSW;
	Widget	UxreplaceSL;
	Widget	Uxguide_sbSW;
	Widget	Uxguide_sbSL;
	Widget	Uxlabel63;
	Widget	Uxlabel64;
	Widget	Uxlabel65;
	Widget	Uxlabel66;
	Widget	Uxlabel67;
	Widget	UxdeletePB;
	Widget	Uxlabel75;
	Widget	Uxlabel76;
	Widget	Uxlabel77;
	Widget	Uxframe7;
	Widget	Uxform7;
	Widget	Uxlabel68;
	Widget	Uxlabel69;
	Widget	Uxlabel70;
	Widget	Uxlabel71;
	Widget	Uxlabel72;
	Widget	UxcloseButton;
	Widget	Uxframe4;
	Widget	Uxform6;
	Widget	Uxlabel44;
	Widget	Uxaccount_idTF;
	Widget	Uxlabel56;
	Widget	Uxcurrent_balanceTF;
	Widget	Uxlabel59;
	Widget	UxcreationTF;
	Widget	UxexpirationTF;
	Widget	Uxlabel60;
	Widget	Uxframe5;
	Widget	Uxform4;
	Widget	Uxlabel57;
	Widget	Uxassign_datasetsSW;
	Widget	Uxassign_datasetsSL;
	Widget	Uxassign_datasets_sbSW;
	Widget	Uxassign_datasets_sbSL;
	Widget	UxaddPB;
	Widget	Uxlabel73;
	Widget	Uxlabel74;
	Widget	Uxlabel61;
	Widget	Uxlabel62;
	Widget	UxupdateButton;
	swidget	UxUxParent;
} _UxCassign_datasets;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCassign_datasets     *UxAssign_datasetsContext;
#define label1                  UxAssign_datasetsContext->Uxlabel1
#define frame1                  UxAssign_datasetsContext->Uxframe1
#define form1                   UxAssign_datasetsContext->Uxform1
#define assigned_datasetsSW     UxAssign_datasetsContext->Uxassigned_datasetsSW
#define assigned_datasetsSL     UxAssign_datasetsContext->Uxassigned_datasetsSL
#define scrolledWindowList4     UxAssign_datasetsContext->UxscrolledWindowList4
#define orderSL                 UxAssign_datasetsContext->UxorderSL
#define scrolledWindowList5     UxAssign_datasetsContext->UxscrolledWindowList5
#define addSL                   UxAssign_datasetsContext->UxaddSL
#define scrolledWindowList6     UxAssign_datasetsContext->UxscrolledWindowList6
#define getSL                   UxAssign_datasetsContext->UxgetSL
#define scrolledWindowList7     UxAssign_datasetsContext->UxscrolledWindowList7
#define deleteSL                UxAssign_datasetsContext->UxdeleteSL
#define replaceSW               UxAssign_datasetsContext->UxreplaceSW
#define replaceSL               UxAssign_datasetsContext->UxreplaceSL
#define guide_sbSW              UxAssign_datasetsContext->Uxguide_sbSW
#define guide_sbSL              UxAssign_datasetsContext->Uxguide_sbSL
#define label63                 UxAssign_datasetsContext->Uxlabel63
#define label64                 UxAssign_datasetsContext->Uxlabel64
#define label65                 UxAssign_datasetsContext->Uxlabel65
#define label66                 UxAssign_datasetsContext->Uxlabel66
#define label67                 UxAssign_datasetsContext->Uxlabel67
#define deletePB                UxAssign_datasetsContext->UxdeletePB
#define label75                 UxAssign_datasetsContext->Uxlabel75
#define label76                 UxAssign_datasetsContext->Uxlabel76
#define label77                 UxAssign_datasetsContext->Uxlabel77
#define frame7                  UxAssign_datasetsContext->Uxframe7
#define form7                   UxAssign_datasetsContext->Uxform7
#define label68                 UxAssign_datasetsContext->Uxlabel68
#define label69                 UxAssign_datasetsContext->Uxlabel69
#define label70                 UxAssign_datasetsContext->Uxlabel70
#define label71                 UxAssign_datasetsContext->Uxlabel71
#define label72                 UxAssign_datasetsContext->Uxlabel72
#define closeButton             UxAssign_datasetsContext->UxcloseButton
#define frame4                  UxAssign_datasetsContext->Uxframe4
#define form6                   UxAssign_datasetsContext->Uxform6
#define label44                 UxAssign_datasetsContext->Uxlabel44
#define account_idTF            UxAssign_datasetsContext->Uxaccount_idTF
#define label56                 UxAssign_datasetsContext->Uxlabel56
#define current_balanceTF       UxAssign_datasetsContext->Uxcurrent_balanceTF
#define label59                 UxAssign_datasetsContext->Uxlabel59
#define creationTF              UxAssign_datasetsContext->UxcreationTF
#define expirationTF            UxAssign_datasetsContext->UxexpirationTF
#define label60                 UxAssign_datasetsContext->Uxlabel60
#define frame5                  UxAssign_datasetsContext->Uxframe5
#define form4                   UxAssign_datasetsContext->Uxform4
#define label57                 UxAssign_datasetsContext->Uxlabel57
#define assign_datasetsSW       UxAssign_datasetsContext->Uxassign_datasetsSW
#define assign_datasetsSL       UxAssign_datasetsContext->Uxassign_datasetsSL
#define assign_datasets_sbSW    UxAssign_datasetsContext->Uxassign_datasets_sbSW
#define assign_datasets_sbSL    UxAssign_datasetsContext->Uxassign_datasets_sbSL
#define addPB                   UxAssign_datasetsContext->UxaddPB
#define label73                 UxAssign_datasetsContext->Uxlabel73
#define label74                 UxAssign_datasetsContext->Uxlabel74
#define label61                 UxAssign_datasetsContext->Uxlabel61
#define label62                 UxAssign_datasetsContext->Uxlabel62
#define updateButton            UxAssign_datasetsContext->UxupdateButton
#define UxParent                UxAssign_datasetsContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	assign_datasets;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_assign_datasets( swidget _UxUxParent );

#endif	/* _IMS_OP_ACCASSIGNDATASET_INCLUDED */
