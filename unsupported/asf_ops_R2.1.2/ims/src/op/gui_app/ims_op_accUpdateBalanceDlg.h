
/*******************************************************************************
       ims_op_accUpdateBalanceDlg.h
       This header file is included by ims_op_accUpdateBalanceDlg.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCUPDATEBALANCEDLG_INCLUDED
#define	_IMS_OP_ACCUPDATEBALANCEDLG_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
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
	swidget	UxUxParent;
	Widget	Uxaccount_param;
	Widget	Uxbegin_param;
	Widget	Uxcurrent_param;
	Widget	Uxhold_param;
} _UxCupdate_balance_dlg;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCupdate_balance_dlg  *UxUpdate_balance_dlgContext;
#define UxParent                UxUpdate_balance_dlgContext->UxUxParent
#define account_param           UxUpdate_balance_dlgContext->Uxaccount_param
#define begin_param             UxUpdate_balance_dlgContext->Uxbegin_param
#define current_param           UxUpdate_balance_dlgContext->Uxcurrent_param
#define hold_param              UxUpdate_balance_dlgContext->Uxhold_param

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	update_balance_dlg;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_update_balance_dlg( swidget _UxUxParent, Widget _Uxaccount_param, Widget _Uxbegin_param, Widget _Uxcurrent_param, Widget _Uxhold_param );

#endif	/* _IMS_OP_ACCUPDATEBALANCEDLG_INCLUDED */
