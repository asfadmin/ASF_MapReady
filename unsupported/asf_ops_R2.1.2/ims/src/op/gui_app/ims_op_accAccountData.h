
/*******************************************************************************
       ims_op_accAccountData.h
       This header file is included by ims_op_accAccountData.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCACCOUNTDATA_INCLUDED
#define	_IMS_OP_ACCACCOUNTDATA_INCLUDED

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
	Widget	UxscrolledWindow2;
	Widget	Uxform5;
	Widget	UxtypeTF;
	Widget	Uxlabel27;
	Widget	Uxlabel35;
	Widget	UxresourceTF;
	Widget	UxprocTF;
	Widget	UxoptionMenu_p2;
	Widget	UxoptionMenu_p_b2;
	Widget	UxoptionMenu2;
	Widget	Uxoption_menu_pane_type;
	Widget	Uxoption_menu_typePB;
	Widget	Uxoption_menu_type;
	Widget	Uxoption_menu_pane_resource;
	Widget	Uxoption_menu_resourcePB;
	Widget	Uxoption_menu_resource;
	Widget	Uxlabel42;
	Widget	UxscrolledWindowText2;
	Widget	UxcommentsST;
	Widget	Uxlabel9;
	Widget	UxcreationTF;
	Widget	Uxlabel11;
	Widget	Uxlabel12;
	Widget	Uxlabel13;
	Widget	Uxlabel14;
	Widget	Uxlabel15;
	Widget	Uxlabel16;
	Widget	Uxlabel18;
	Widget	Uxframe3;
	Widget	UxrowColumn1;
	Widget	UxyesTB;
	Widget	UxnoTB;
	Widget	UxidTF;
	Widget	Uxlabel84;
	Widget	Uxframe6;
	Widget	Uxform8;
	Widget	Uxlabel85;
	Widget	UxexpirationTF;
	Widget	UxrateTF;
	Widget	Uxcurrent_balanceTF;
	Widget	Uxon_holdTF;
	Widget	Uxbegin_balanceTF;
	Widget	Uxlabel151;
	Widget	UxmanagerTF;
	Widget	UxpushButton1;
	Widget	UxsavePB;
	Widget	Uxupdate_balancePB;
	Widget	UxclosePB;
	swidget	UxUxParent;
	int	Uxcreate;
} _UxCaccount_data;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCaccount_data        *UxAccount_dataContext;
#define scrolledWindow2         UxAccount_dataContext->UxscrolledWindow2
#define form5                   UxAccount_dataContext->Uxform5
#define typeTF                  UxAccount_dataContext->UxtypeTF
#define label27                 UxAccount_dataContext->Uxlabel27
#define label35                 UxAccount_dataContext->Uxlabel35
#define resourceTF              UxAccount_dataContext->UxresourceTF
#define procTF                  UxAccount_dataContext->UxprocTF
#define optionMenu_p2           UxAccount_dataContext->UxoptionMenu_p2
#define optionMenu_p_b2         UxAccount_dataContext->UxoptionMenu_p_b2
#define optionMenu2             UxAccount_dataContext->UxoptionMenu2
#define option_menu_pane_type   UxAccount_dataContext->Uxoption_menu_pane_type
#define option_menu_typePB      UxAccount_dataContext->Uxoption_menu_typePB
#define option_menu_type        UxAccount_dataContext->Uxoption_menu_type
#define option_menu_pane_resource UxAccount_dataContext->Uxoption_menu_pane_resource
#define option_menu_resourcePB  UxAccount_dataContext->Uxoption_menu_resourcePB
#define option_menu_resource    UxAccount_dataContext->Uxoption_menu_resource
#define label42                 UxAccount_dataContext->Uxlabel42
#define scrolledWindowText2     UxAccount_dataContext->UxscrolledWindowText2
#define commentsST              UxAccount_dataContext->UxcommentsST
#define label9                  UxAccount_dataContext->Uxlabel9
#define creationTF              UxAccount_dataContext->UxcreationTF
#define label11                 UxAccount_dataContext->Uxlabel11
#define label12                 UxAccount_dataContext->Uxlabel12
#define label13                 UxAccount_dataContext->Uxlabel13
#define label14                 UxAccount_dataContext->Uxlabel14
#define label15                 UxAccount_dataContext->Uxlabel15
#define label16                 UxAccount_dataContext->Uxlabel16
#define label18                 UxAccount_dataContext->Uxlabel18
#define frame3                  UxAccount_dataContext->Uxframe3
#define rowColumn1              UxAccount_dataContext->UxrowColumn1
#define yesTB                   UxAccount_dataContext->UxyesTB
#define noTB                    UxAccount_dataContext->UxnoTB
#define idTF                    UxAccount_dataContext->UxidTF
#define label84                 UxAccount_dataContext->Uxlabel84
#define frame6                  UxAccount_dataContext->Uxframe6
#define form8                   UxAccount_dataContext->Uxform8
#define label85                 UxAccount_dataContext->Uxlabel85
#define expirationTF            UxAccount_dataContext->UxexpirationTF
#define rateTF                  UxAccount_dataContext->UxrateTF
#define current_balanceTF       UxAccount_dataContext->Uxcurrent_balanceTF
#define on_holdTF               UxAccount_dataContext->Uxon_holdTF
#define begin_balanceTF         UxAccount_dataContext->Uxbegin_balanceTF
#define label151                UxAccount_dataContext->Uxlabel151
#define managerTF               UxAccount_dataContext->UxmanagerTF
#define pushButton1             UxAccount_dataContext->UxpushButton1
#define savePB                  UxAccount_dataContext->UxsavePB
#define update_balancePB        UxAccount_dataContext->Uxupdate_balancePB
#define closePB                 UxAccount_dataContext->UxclosePB
#define UxParent                UxAccount_dataContext->UxUxParent
#define create                  UxAccount_dataContext->Uxcreate

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	account_data;
extern Widget	account_dataLB1;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_account_data( swidget _UxUxParent, int _Uxcreate );

#endif	/* _IMS_OP_ACCACCOUNTDATA_INCLUDED */
