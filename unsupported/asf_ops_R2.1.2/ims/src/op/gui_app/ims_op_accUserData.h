
/*******************************************************************************
       ims_op_accUserData.h
       This header file is included by ims_op_accUserData.c

*******************************************************************************/

#ifndef	_IMS_OP_ACCUSERDATA_INCLUDED
#define	_IMS_OP_ACCUSERDATA_INCLUDED

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
	Widget	Uxlabel25;
	Widget	Uxlabel26;
	Widget	UxidTF;
	Widget	Uxm_iTF;
	Widget	Uxlabel27;
	Widget	Uxlabel28;
	Widget	Uxlabel29;
	Widget	Uxlabel30;
	Widget	Uxlabel31;
	Widget	Uxlabel32;
	Widget	Uxlabel33;
	Widget	Uxlabel34;
	Widget	UxIDLB;
	Widget	Uxfirst_nameLB;
	Widget	Uxfirst_nameTF;
	Widget	Uxlast_nameTF;
	Widget	Uxlabel37;
	Widget	UxorganizationTF;
	Widget	UxtitleTF;
	Widget	UxaddressTF;
	Widget	UxcityTF;
	Widget	UxstateTF;
	Widget	UxcountryTF;
	Widget	UxzipTF;
	Widget	UxphoneTF;
	Widget	UxfaxTF;
	Widget	UxemailTF;
	Widget	Uxlabel38;
	Widget	Uxlabel40;
	Widget	UxpriorityTF;
	Widget	UxtypeTF;
	Widget	Uxlabel42;
	Widget	UxpasswordTF;
	Widget	Uxlabel43;
	Widget	Uxftp_dirSW;
	Widget	Uxftp_dirST;
	Widget	Uxlabel55;
	Widget	Uxoption_menu_pane_type;
	Widget	Uxoption_menu_typePB;
	Widget	Uxoption_menu_type;
	Widget	Uxoption_menu_pane_priority;
	Widget	Uxoption_menu_priorityPB;
	Widget	Uxoption_menu_priority;
	Widget	Uxaccess_dirSW;
	Widget	Uxaccess_dirST;
	Widget	Uxlabel153;
	Widget	UxcreatePB;
	Widget	UxclosePB;
	swidget	UxUxParent;
	int	Uxcreate;
} _UxCuser_data;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCuser_data           *UxUser_dataContext;
#define scrolledWindow2         UxUser_dataContext->UxscrolledWindow2
#define form5                   UxUser_dataContext->Uxform5
#define label25                 UxUser_dataContext->Uxlabel25
#define label26                 UxUser_dataContext->Uxlabel26
#define idTF                    UxUser_dataContext->UxidTF
#define m_iTF                   UxUser_dataContext->Uxm_iTF
#define label27                 UxUser_dataContext->Uxlabel27
#define label28                 UxUser_dataContext->Uxlabel28
#define label29                 UxUser_dataContext->Uxlabel29
#define label30                 UxUser_dataContext->Uxlabel30
#define label31                 UxUser_dataContext->Uxlabel31
#define label32                 UxUser_dataContext->Uxlabel32
#define label33                 UxUser_dataContext->Uxlabel33
#define label34                 UxUser_dataContext->Uxlabel34
#define IDLB                    UxUser_dataContext->UxIDLB
#define first_nameLB            UxUser_dataContext->Uxfirst_nameLB
#define first_nameTF            UxUser_dataContext->Uxfirst_nameTF
#define last_nameTF             UxUser_dataContext->Uxlast_nameTF
#define label37                 UxUser_dataContext->Uxlabel37
#define organizationTF          UxUser_dataContext->UxorganizationTF
#define titleTF                 UxUser_dataContext->UxtitleTF
#define addressTF               UxUser_dataContext->UxaddressTF
#define cityTF                  UxUser_dataContext->UxcityTF
#define stateTF                 UxUser_dataContext->UxstateTF
#define countryTF               UxUser_dataContext->UxcountryTF
#define zipTF                   UxUser_dataContext->UxzipTF
#define phoneTF                 UxUser_dataContext->UxphoneTF
#define faxTF                   UxUser_dataContext->UxfaxTF
#define emailTF                 UxUser_dataContext->UxemailTF
#define label38                 UxUser_dataContext->Uxlabel38
#define label40                 UxUser_dataContext->Uxlabel40
#define priorityTF              UxUser_dataContext->UxpriorityTF
#define typeTF                  UxUser_dataContext->UxtypeTF
#define label42                 UxUser_dataContext->Uxlabel42
#define passwordTF              UxUser_dataContext->UxpasswordTF
#define label43                 UxUser_dataContext->Uxlabel43
#define ftp_dirSW               UxUser_dataContext->Uxftp_dirSW
#define ftp_dirST               UxUser_dataContext->Uxftp_dirST
#define label55                 UxUser_dataContext->Uxlabel55
#define option_menu_pane_type   UxUser_dataContext->Uxoption_menu_pane_type
#define option_menu_typePB      UxUser_dataContext->Uxoption_menu_typePB
#define option_menu_type        UxUser_dataContext->Uxoption_menu_type
#define option_menu_pane_priority UxUser_dataContext->Uxoption_menu_pane_priority
#define option_menu_priorityPB  UxUser_dataContext->Uxoption_menu_priorityPB
#define option_menu_priority    UxUser_dataContext->Uxoption_menu_priority
#define access_dirSW            UxUser_dataContext->Uxaccess_dirSW
#define access_dirST            UxUser_dataContext->Uxaccess_dirST
#define label153                UxUser_dataContext->Uxlabel153
#define createPB                UxUser_dataContext->UxcreatePB
#define closePB                 UxUser_dataContext->UxclosePB
#define UxParent                UxUser_dataContext->UxUxParent
#define create                  UxUser_dataContext->Uxcreate

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	user_data;
extern Widget	user_dataLB;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_user_data( swidget _UxUxParent, int _Uxcreate );

#endif	/* _IMS_OP_ACCUSERDATA_INCLUDED */
