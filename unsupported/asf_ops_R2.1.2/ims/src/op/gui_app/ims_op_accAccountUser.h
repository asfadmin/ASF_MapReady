
/*******************************************************************************
       ims_op_accAccountUser.h
       This header file is included by ims_op_accAccountUser.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCACCOUNTUSER_INCLUDED
#define	_IMS_OP_ACCACCOUNTUSER_INCLUDED

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
	Widget	Uxaccounts_users;
	Widget	Uxacc_usrMB;
	Widget	Uxacc_usrMB_p1;
	Widget	Uxacc_usrMB_p1_b1;
	Widget	Uxacc_usrMB_top_b1;
	Widget	Uxacc_usrMB_p3;
	Widget	Uxacc_usrMB_p3_b2;
	Widget	Uxacc_usrMB_top_b3;
	Widget	Uxacc_usrMB_p10;
	Widget	Uxacc_usrMB_p10_b1;
	Widget	Uxacc_usrMB_p10_b3;
	Widget	Uxacc_usrMB_p10_b4;
	Widget	Uxacc_usrMB_p10_b7;
	Widget	Uxacc_usrMB_p10_b8;
	Widget	Uxacc_usrMB_p10_b9;
	Widget	Uxacc_usrMB_top_b8;
	Widget	Uxacc_usrMB_p4;
	Widget	Uxacc_usrMB_p4_b2;
	Widget	Uxacc_usrMB_p4_b3;
	Widget	Uxacc_usrMB_p4_b4;
	Widget	Uxacc_usrMB_p4_b5;
	Widget	Uxacc_usrMB_p4_b10;
	Widget	Uxacc_usrMB_p4_b6;
	Widget	Uxacc_usrMB_p4_b7;
	Widget	Uxacc_usrMB_p4_b8;
	Widget	Uxacc_usrMB_p4_b9;
	Widget	Uxacc_usrMB_top_b2;
	Widget	Uxlabel1;
	Widget	Uxlabel3;
	Widget	Uxframe1;
	Widget	Uxform1;
	Widget	Uxlabel6;
	Widget	Uxlabel7;
	Widget	Uxlabel24;
	Widget	UxaccountsSW;
	Widget	UxaccountsSL;
	Widget	Uxaccounts_sbSL;
	Widget	Uxcreate_accountPB;
	Widget	Uxaccount_dataPB;
	Widget	Uxdelete_accountPB;
	Widget	UxsearchPB;
	Widget	Uxassign_usersButton;
	Widget	Uxassign_datasetsButton;
	Widget	Uxframe2;
	Widget	Uxform2;
	Widget	Uxlabel4;
	Widget	UxusersSW;
	Widget	UxusersSL;
	Widget	Uxcreate_userPB;
	Widget	Uxuser_dataPB;
	Widget	Uxdelete_userPB;
	Widget	Uxsearch_usersButton;
	Widget	Uxlabel8;
	Widget	Uxusers_sbSL;
	Widget	Uxlabel86;
	Widget	Uxlabel2;
	Widget	UxpushButton5;
	Widget	UxcloseButton;
	swidget	UxUxParent;
} _UxCaccounts_users;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCaccounts_users      *UxAccounts_usersContext;
#define accounts_users          UxAccounts_usersContext->Uxaccounts_users
#define acc_usrMB               UxAccounts_usersContext->Uxacc_usrMB
#define acc_usrMB_p1            UxAccounts_usersContext->Uxacc_usrMB_p1
#define acc_usrMB_p1_b1         UxAccounts_usersContext->Uxacc_usrMB_p1_b1
#define acc_usrMB_top_b1        UxAccounts_usersContext->Uxacc_usrMB_top_b1
#define acc_usrMB_p3            UxAccounts_usersContext->Uxacc_usrMB_p3
#define acc_usrMB_p3_b2         UxAccounts_usersContext->Uxacc_usrMB_p3_b2
#define acc_usrMB_top_b3        UxAccounts_usersContext->Uxacc_usrMB_top_b3
#define acc_usrMB_p10           UxAccounts_usersContext->Uxacc_usrMB_p10
#define acc_usrMB_p10_b1        UxAccounts_usersContext->Uxacc_usrMB_p10_b1
#define acc_usrMB_p10_b3        UxAccounts_usersContext->Uxacc_usrMB_p10_b3
#define acc_usrMB_p10_b4        UxAccounts_usersContext->Uxacc_usrMB_p10_b4
#define acc_usrMB_p10_b7        UxAccounts_usersContext->Uxacc_usrMB_p10_b7
#define acc_usrMB_p10_b8        UxAccounts_usersContext->Uxacc_usrMB_p10_b8
#define acc_usrMB_p10_b9        UxAccounts_usersContext->Uxacc_usrMB_p10_b9
#define acc_usrMB_top_b8        UxAccounts_usersContext->Uxacc_usrMB_top_b8
#define acc_usrMB_p4            UxAccounts_usersContext->Uxacc_usrMB_p4
#define acc_usrMB_p4_b2         UxAccounts_usersContext->Uxacc_usrMB_p4_b2
#define acc_usrMB_p4_b3         UxAccounts_usersContext->Uxacc_usrMB_p4_b3
#define acc_usrMB_p4_b4         UxAccounts_usersContext->Uxacc_usrMB_p4_b4
#define acc_usrMB_p4_b5         UxAccounts_usersContext->Uxacc_usrMB_p4_b5
#define acc_usrMB_p4_b10        UxAccounts_usersContext->Uxacc_usrMB_p4_b10
#define acc_usrMB_p4_b6         UxAccounts_usersContext->Uxacc_usrMB_p4_b6
#define acc_usrMB_p4_b7         UxAccounts_usersContext->Uxacc_usrMB_p4_b7
#define acc_usrMB_p4_b8         UxAccounts_usersContext->Uxacc_usrMB_p4_b8
#define acc_usrMB_p4_b9         UxAccounts_usersContext->Uxacc_usrMB_p4_b9
#define acc_usrMB_top_b2        UxAccounts_usersContext->Uxacc_usrMB_top_b2
#define label1                  UxAccounts_usersContext->Uxlabel1
#define label3                  UxAccounts_usersContext->Uxlabel3
#define frame1                  UxAccounts_usersContext->Uxframe1
#define form1                   UxAccounts_usersContext->Uxform1
#define label6                  UxAccounts_usersContext->Uxlabel6
#define label7                  UxAccounts_usersContext->Uxlabel7
#define label24                 UxAccounts_usersContext->Uxlabel24
#define accountsSW              UxAccounts_usersContext->UxaccountsSW
#define accountsSL              UxAccounts_usersContext->UxaccountsSL
#define accounts_sbSL           UxAccounts_usersContext->Uxaccounts_sbSL
#define create_accountPB        UxAccounts_usersContext->Uxcreate_accountPB
#define account_dataPB          UxAccounts_usersContext->Uxaccount_dataPB
#define delete_accountPB        UxAccounts_usersContext->Uxdelete_accountPB
#define searchPB                UxAccounts_usersContext->UxsearchPB
#define assign_usersButton      UxAccounts_usersContext->Uxassign_usersButton
#define assign_datasetsButton   UxAccounts_usersContext->Uxassign_datasetsButton
#define frame2                  UxAccounts_usersContext->Uxframe2
#define form2                   UxAccounts_usersContext->Uxform2
#define label4                  UxAccounts_usersContext->Uxlabel4
#define usersSW                 UxAccounts_usersContext->UxusersSW
#define usersSL                 UxAccounts_usersContext->UxusersSL
#define create_userPB           UxAccounts_usersContext->Uxcreate_userPB
#define user_dataPB             UxAccounts_usersContext->Uxuser_dataPB
#define delete_userPB           UxAccounts_usersContext->Uxdelete_userPB
#define search_usersButton      UxAccounts_usersContext->Uxsearch_usersButton
#define label8                  UxAccounts_usersContext->Uxlabel8
#define users_sbSL              UxAccounts_usersContext->Uxusers_sbSL
#define label86                 UxAccounts_usersContext->Uxlabel86
#define label2                  UxAccounts_usersContext->Uxlabel2
#define pushButton5             UxAccounts_usersContext->UxpushButton5
#define closeButton             UxAccounts_usersContext->UxcloseButton
#define UxParent                UxAccounts_usersContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	accounts_sbSW;
extern Widget	users_sbSW;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_accounts_users( swidget _UxUxParent );

#endif	/* _IMS_OP_ACCACCOUNTUSER_INCLUDED */
