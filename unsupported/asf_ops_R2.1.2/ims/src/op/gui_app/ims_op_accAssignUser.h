
/*******************************************************************************
       ims_op_accAssignUser.h
       This header file is included by ims_op_accAssignUser.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCASSIGNUSER_INCLUDED
#define	_IMS_OP_ACCASSIGNUSER_INCLUDED

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
	Widget	Uxlabel6;
	Widget	Uxlabel7;
	Widget	UxdeletePB;
	Widget	Uxassigned_usersSW;
	Widget	Uxassigned_usersSL;
	Widget	Uxassigned_users_sbSW;
	Widget	Uxassigned_users_sbSL;
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
	Widget	Uxlabel58;
	Widget	Uxassign_usersSW;
	Widget	Uxassign_usersSL;
	Widget	UxaddPB;
	Widget	Uxassign_users_sbSW;
	Widget	Uxassign_users_sbSL;
	Widget	Uxlabel61;
	Widget	Uxlabel62;
	Widget	UxupdateButton;
	swidget	UxUxParent;
} _UxCassign_users;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCassign_users        *UxAssign_usersContext;
#define label1                  UxAssign_usersContext->Uxlabel1
#define frame1                  UxAssign_usersContext->Uxframe1
#define form1                   UxAssign_usersContext->Uxform1
#define label6                  UxAssign_usersContext->Uxlabel6
#define label7                  UxAssign_usersContext->Uxlabel7
#define deletePB                UxAssign_usersContext->UxdeletePB
#define assigned_usersSW        UxAssign_usersContext->Uxassigned_usersSW
#define assigned_usersSL        UxAssign_usersContext->Uxassigned_usersSL
#define assigned_users_sbSW     UxAssign_usersContext->Uxassigned_users_sbSW
#define assigned_users_sbSL     UxAssign_usersContext->Uxassigned_users_sbSL
#define closeButton             UxAssign_usersContext->UxcloseButton
#define frame4                  UxAssign_usersContext->Uxframe4
#define form6                   UxAssign_usersContext->Uxform6
#define label44                 UxAssign_usersContext->Uxlabel44
#define account_idTF            UxAssign_usersContext->Uxaccount_idTF
#define label56                 UxAssign_usersContext->Uxlabel56
#define current_balanceTF       UxAssign_usersContext->Uxcurrent_balanceTF
#define label59                 UxAssign_usersContext->Uxlabel59
#define creationTF              UxAssign_usersContext->UxcreationTF
#define expirationTF            UxAssign_usersContext->UxexpirationTF
#define label60                 UxAssign_usersContext->Uxlabel60
#define frame5                  UxAssign_usersContext->Uxframe5
#define form4                   UxAssign_usersContext->Uxform4
#define label57                 UxAssign_usersContext->Uxlabel57
#define label58                 UxAssign_usersContext->Uxlabel58
#define assign_usersSW          UxAssign_usersContext->Uxassign_usersSW
#define assign_usersSL          UxAssign_usersContext->Uxassign_usersSL
#define addPB                   UxAssign_usersContext->UxaddPB
#define assign_users_sbSW       UxAssign_usersContext->Uxassign_users_sbSW
#define assign_users_sbSL       UxAssign_usersContext->Uxassign_users_sbSL
#define label61                 UxAssign_usersContext->Uxlabel61
#define label62                 UxAssign_usersContext->Uxlabel62
#define updateButton            UxAssign_usersContext->UxupdateButton
#define UxParent                UxAssign_usersContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	assign_users;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_assign_users( swidget _UxUxParent );

#endif	/* _IMS_OP_ACCASSIGNUSER_INCLUDED */
