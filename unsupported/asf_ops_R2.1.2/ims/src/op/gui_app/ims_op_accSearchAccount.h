
/*******************************************************************************
       ims_op_accSearchAccount.h
       This header file is included by ims_op_accSearchAccount.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCSEARCHACCOUNT_INCLUDED
#define	_IMS_OP_ACCSEARCHACCOUNT_INCLUDED

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
	Widget	Uxoption_menu_pane_resource;
	Widget	Uxoption_menu_resourcePB;
	Widget	Uxoption_menu_resource;
	Widget	UxscrolledWindowText2;
	Widget	UxcommentsST;
	Widget	Uxlabel9;
	Widget	Uxlabel10;
	Widget	Uxlabel12;
	Widget	Uxlabel13;
	Widget	Uxlabel15;
	Widget	UxidTF;
	Widget	Uxlabel78;
	Widget	Uxlabel79;
	Widget	Uxoption_menu_pane_type;
	Widget	Uxoption_menu_typePB;
	Widget	Uxoption_menu_type;
	Widget	Uxlabel80;
	Widget	Uxlabel81;
	Widget	Uxlabel82;
	Widget	Uxlabel83;
	Widget	Uxstart_creationTF;
	Widget	Uxend_creationTF;
	Widget	Uxstart_expirationTF;
	Widget	Uxend_expirationTF;
	Widget	Uxstart_balanceTF;
	Widget	Uxend_balanceTF;
	Widget	Uxframe8;
	Widget	Uxform9;
	Widget	Uxlabel5;
	Widget	Uxlabel152;
	Widget	UxmanagerTF;
	Widget	UxpushButton2;
	Widget	Uxlabel41;
	Widget	UxcloseButton;
	Widget	UxpushButton3;
	swidget	UxUxParent;
} _UxCsearch_accounts;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCsearch_accounts     *UxSearch_accountsContext;
#define scrolledWindow2         UxSearch_accountsContext->UxscrolledWindow2
#define form5                   UxSearch_accountsContext->Uxform5
#define typeTF                  UxSearch_accountsContext->UxtypeTF
#define label27                 UxSearch_accountsContext->Uxlabel27
#define label35                 UxSearch_accountsContext->Uxlabel35
#define resourceTF              UxSearch_accountsContext->UxresourceTF
#define option_menu_pane_resource UxSearch_accountsContext->Uxoption_menu_pane_resource
#define option_menu_resourcePB  UxSearch_accountsContext->Uxoption_menu_resourcePB
#define option_menu_resource    UxSearch_accountsContext->Uxoption_menu_resource
#define scrolledWindowText2     UxSearch_accountsContext->UxscrolledWindowText2
#define commentsST              UxSearch_accountsContext->UxcommentsST
#define label9                  UxSearch_accountsContext->Uxlabel9
#define label10                 UxSearch_accountsContext->Uxlabel10
#define label12                 UxSearch_accountsContext->Uxlabel12
#define label13                 UxSearch_accountsContext->Uxlabel13
#define label15                 UxSearch_accountsContext->Uxlabel15
#define idTF                    UxSearch_accountsContext->UxidTF
#define label78                 UxSearch_accountsContext->Uxlabel78
#define label79                 UxSearch_accountsContext->Uxlabel79
#define option_menu_pane_type   UxSearch_accountsContext->Uxoption_menu_pane_type
#define option_menu_typePB      UxSearch_accountsContext->Uxoption_menu_typePB
#define option_menu_type        UxSearch_accountsContext->Uxoption_menu_type
#define label80                 UxSearch_accountsContext->Uxlabel80
#define label81                 UxSearch_accountsContext->Uxlabel81
#define label82                 UxSearch_accountsContext->Uxlabel82
#define label83                 UxSearch_accountsContext->Uxlabel83
#define start_creationTF        UxSearch_accountsContext->Uxstart_creationTF
#define end_creationTF          UxSearch_accountsContext->Uxend_creationTF
#define start_expirationTF      UxSearch_accountsContext->Uxstart_expirationTF
#define end_expirationTF        UxSearch_accountsContext->Uxend_expirationTF
#define start_balanceTF         UxSearch_accountsContext->Uxstart_balanceTF
#define end_balanceTF           UxSearch_accountsContext->Uxend_balanceTF
#define frame8                  UxSearch_accountsContext->Uxframe8
#define form9                   UxSearch_accountsContext->Uxform9
#define label5                  UxSearch_accountsContext->Uxlabel5
#define label152                UxSearch_accountsContext->Uxlabel152
#define managerTF               UxSearch_accountsContext->UxmanagerTF
#define pushButton2             UxSearch_accountsContext->UxpushButton2
#define label41                 UxSearch_accountsContext->Uxlabel41
#define closeButton             UxSearch_accountsContext->UxcloseButton
#define pushButton3             UxSearch_accountsContext->UxpushButton3
#define UxParent                UxSearch_accountsContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	search_accounts;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_search_accounts( swidget _UxUxParent );

#endif	/* _IMS_OP_ACCSEARCHACCOUNT_INCLUDED */
