
/*******************************************************************************
       ims_op_accSearchUser.h
       This header file is included by ims_op_accSearchUser.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCSEARCHUSER_INCLUDED
#define	_IMS_OP_ACCSEARCHUSER_INCLUDED

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
	Widget	UxscrolledWindow3;
	Widget	Uxform10;
	Widget	Uxlabel87;
	Widget	Uxlabel88;
	Widget	UxidTF;
	Widget	Uxm_iTF;
	Widget	Uxlabel89;
	Widget	Uxlabel90;
	Widget	Uxlabel91;
	Widget	Uxlabel92;
	Widget	Uxlabel93;
	Widget	Uxlabel94;
	Widget	Uxlabel95;
	Widget	Uxlabel96;
	Widget	Uxlabel97;
	Widget	Uxfirst_nameTF;
	Widget	Uxlast_nameTF;
	Widget	UxtypeTF;
	Widget	Uxlabel98;
	Widget	UxorganizationTF;
	Widget	UxtitleTF;
	Widget	UxcityTF;
	Widget	UxstateTF;
	Widget	UxcountryTF;
	Widget	UxzipTF;
	Widget	UxphoneTF;
	Widget	UxfaxTF;
	Widget	UxemailTF;
	Widget	Uxlabel99;
	Widget	Uxlabel100;
	Widget	Uxoption_menu_pane_type;
	Widget	Uxoption_menu_typePB;
	Widget	Uxoption_menu_type;
	Widget	Uxlabel101;
	Widget	Uxftp_dirSW;
	Widget	Uxftp_dirST;
	Widget	Uxlabel154;
	Widget	Uxaccess_dirSW;
	Widget	Uxaccess_dirST;
	Widget	Uxlabel102;
	Widget	UxcloseButton;
	Widget	Uxstart_searchPB;
	swidget	UxUxParent;
	int	Uxcreate;
} _UxCsearch_users;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCsearch_users        *UxSearch_usersContext;
#define scrolledWindow3         UxSearch_usersContext->UxscrolledWindow3
#define form10                  UxSearch_usersContext->Uxform10
#define label87                 UxSearch_usersContext->Uxlabel87
#define label88                 UxSearch_usersContext->Uxlabel88
#define idTF                    UxSearch_usersContext->UxidTF
#define m_iTF                   UxSearch_usersContext->Uxm_iTF
#define label89                 UxSearch_usersContext->Uxlabel89
#define label90                 UxSearch_usersContext->Uxlabel90
#define label91                 UxSearch_usersContext->Uxlabel91
#define label92                 UxSearch_usersContext->Uxlabel92
#define label93                 UxSearch_usersContext->Uxlabel93
#define label94                 UxSearch_usersContext->Uxlabel94
#define label95                 UxSearch_usersContext->Uxlabel95
#define label96                 UxSearch_usersContext->Uxlabel96
#define label97                 UxSearch_usersContext->Uxlabel97
#define first_nameTF            UxSearch_usersContext->Uxfirst_nameTF
#define last_nameTF             UxSearch_usersContext->Uxlast_nameTF
#define typeTF                  UxSearch_usersContext->UxtypeTF
#define label98                 UxSearch_usersContext->Uxlabel98
#define organizationTF          UxSearch_usersContext->UxorganizationTF
#define titleTF                 UxSearch_usersContext->UxtitleTF
#define cityTF                  UxSearch_usersContext->UxcityTF
#define stateTF                 UxSearch_usersContext->UxstateTF
#define countryTF               UxSearch_usersContext->UxcountryTF
#define zipTF                   UxSearch_usersContext->UxzipTF
#define phoneTF                 UxSearch_usersContext->UxphoneTF
#define faxTF                   UxSearch_usersContext->UxfaxTF
#define emailTF                 UxSearch_usersContext->UxemailTF
#define label99                 UxSearch_usersContext->Uxlabel99
#define label100                UxSearch_usersContext->Uxlabel100
#define option_menu_pane_type   UxSearch_usersContext->Uxoption_menu_pane_type
#define option_menu_typePB      UxSearch_usersContext->Uxoption_menu_typePB
#define option_menu_type        UxSearch_usersContext->Uxoption_menu_type
#define label101                UxSearch_usersContext->Uxlabel101
#define ftp_dirSW               UxSearch_usersContext->Uxftp_dirSW
#define ftp_dirST               UxSearch_usersContext->Uxftp_dirST
#define label154                UxSearch_usersContext->Uxlabel154
#define access_dirSW            UxSearch_usersContext->Uxaccess_dirSW
#define access_dirST            UxSearch_usersContext->Uxaccess_dirST
#define label102                UxSearch_usersContext->Uxlabel102
#define closeButton             UxSearch_usersContext->UxcloseButton
#define start_searchPB          UxSearch_usersContext->Uxstart_searchPB
#define UxParent                UxSearch_usersContext->UxUxParent
#define create                  UxSearch_usersContext->Uxcreate

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	search_users;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_search_users( swidget _UxUxParent, int _Uxcreate );

#endif	/* _IMS_OP_ACCSEARCHUSER_INCLUDED */
