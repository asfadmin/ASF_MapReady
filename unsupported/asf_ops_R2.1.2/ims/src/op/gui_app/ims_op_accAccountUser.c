
/*******************************************************************************
	ims_op_accAccountUser.c

       Associated Header file: ims_op_accAccountUser.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_op_accCb.h>


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAccountUser.h"
#undef CONTEXT_MACRO_ACCESS

Widget	accounts_sbSW;
Widget	users_sbSW;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton5(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCaccounts_users      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAccounts_usersContext;
	UxAccounts_usersContext = UxContext =
			(_UxCaccounts_users *) UxGetContext( UxWidget );
	{
	
	}
	UxAccounts_usersContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_accounts_users()
{
	Widget		_UxParent;
	Widget		acc_usrMB_p1_shell;
	Widget		acc_usrMB_p3_shell;
	Widget		acc_usrMB_p10_shell;
	Widget		acc_usrMB_p4_shell;


	/* Creation of accounts_users */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "accounts_users_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 100,
			XmNy, 5,
			XmNwidth, 1023,
			XmNheight, 770,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "accounts_users",
			NULL );

	}

	accounts_users = XtVaCreateManagedWidget( "accounts_users",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1023,
			XmNheight, 770,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNdialogTitle, "Users and Accounts Management" ),
			XmNsensitive, TRUE,
			XmNautoUnmanage, TRUE,
			NULL );
	UxPutContext( accounts_users, (char *) UxAccounts_usersContext );
	UxPutClassCode( accounts_users, _UxIfClassId );


	/* Creation of acc_usrMB */
	acc_usrMB = XtVaCreateManagedWidget( "acc_usrMB",
			xmRowColumnWidgetClass,
			accounts_users,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 0,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 3,
			XmNmenuAccelerator, "<KeyUp>F10",
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( acc_usrMB, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p1 */
	acc_usrMB_p1_shell = XtVaCreatePopupShell ("acc_usrMB_p1_shell",
			xmMenuShellWidgetClass, acc_usrMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	acc_usrMB_p1 = XtVaCreateWidget( "acc_usrMB_p1",
			xmRowColumnWidgetClass,
			acc_usrMB_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNmarginWidth, 12,
			NULL );
	UxPutContext( acc_usrMB_p1, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p1_b1 */
	acc_usrMB_p1_b1 = XtVaCreateManagedWidget( "acc_usrMB_p1_b1",
			xmPushButtonWidgetClass,
			acc_usrMB_p1,
			RES_CONVERT( XmNlabelString, "Welcome" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p1_b1, XmNactivateCallback,
		(XtCallbackProc) accounts_users_goto_welcomeCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p1_b1, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_top_b1 */
	acc_usrMB_top_b1 = XtVaCreateManagedWidget( "acc_usrMB_top_b1",
			xmCascadeButtonWidgetClass,
			acc_usrMB,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, acc_usrMB_p1,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 12,
			NULL );
	UxPutContext( acc_usrMB_top_b1, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p3 */
	acc_usrMB_p3_shell = XtVaCreatePopupShell ("acc_usrMB_p3_shell",
			xmMenuShellWidgetClass, acc_usrMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	acc_usrMB_p3 = XtVaCreateWidget( "acc_usrMB_p3",
			xmRowColumnWidgetClass,
			acc_usrMB_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( acc_usrMB_p3, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p3_b2 */
	acc_usrMB_p3_b2 = XtVaCreateManagedWidget( "acc_usrMB_p3_b2",
			xmPushButtonWidgetClass,
			acc_usrMB_p3,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNbackground, "Cadet Blue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( acc_usrMB_p3_b2, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_top_b3 */
	acc_usrMB_top_b3 = XtVaCreateManagedWidget( "acc_usrMB_top_b3",
			xmCascadeButtonWidgetClass,
			acc_usrMB,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, acc_usrMB_p3,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 12,
			NULL );
	UxPutContext( acc_usrMB_top_b3, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10 */
	acc_usrMB_p10_shell = XtVaCreatePopupShell ("acc_usrMB_p10_shell",
			xmMenuShellWidgetClass, acc_usrMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	acc_usrMB_p10 = XtVaCreateWidget( "acc_usrMB_p10",
			xmRowColumnWidgetClass,
			acc_usrMB_p10_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( acc_usrMB_p10, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b1 */
	acc_usrMB_p10_b1 = XtVaCreateManagedWidget( "acc_usrMB_p10_b1",
			xmPushButtonGadgetClass,
			acc_usrMB_p10,
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p10_b1, XmNactivateCallback,
		(XtCallbackProc) accounts_users_search_usersCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p10_b1, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b3 */
	acc_usrMB_p10_b3 = XtVaCreateManagedWidget( "acc_usrMB_p10_b3",
			xmSeparatorGadgetClass,
			acc_usrMB_p10,
			NULL );
	UxPutContext( acc_usrMB_p10_b3, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b4 */
	acc_usrMB_p10_b4 = XtVaCreateManagedWidget( "acc_usrMB_p10_b4",
			xmPushButtonGadgetClass,
			acc_usrMB_p10,
			RES_CONVERT( XmNlabelString, "User Data" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p10_b4, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_userCb,
		(XtPointer) 0 );

	UxPutContext( acc_usrMB_p10_b4, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b7 */
	acc_usrMB_p10_b7 = XtVaCreateManagedWidget( "acc_usrMB_p10_b7",
			xmSeparatorGadgetClass,
			acc_usrMB_p10,
			NULL );
	UxPutContext( acc_usrMB_p10_b7, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b8 */
	acc_usrMB_p10_b8 = XtVaCreateManagedWidget( "acc_usrMB_p10_b8",
			xmPushButtonGadgetClass,
			acc_usrMB_p10,
			RES_CONVERT( XmNlabelString, "Create" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p10_b8, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_userCb,
		(XtPointer) 1 );

	UxPutContext( acc_usrMB_p10_b8, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p10_b9 */
	acc_usrMB_p10_b9 = XtVaCreateManagedWidget( "acc_usrMB_p10_b9",
			xmPushButtonGadgetClass,
			acc_usrMB_p10,
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p10_b9, XmNactivateCallback,
		(XtCallbackProc) accounts_users_delete_userCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p10_b9, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_top_b8 */
	acc_usrMB_top_b8 = XtVaCreateManagedWidget( "acc_usrMB_top_b8",
			xmCascadeButtonWidgetClass,
			acc_usrMB,
			RES_CONVERT( XmNlabelString, "Users" ),
			RES_CONVERT( XmNmnemonic, "U" ),
			XmNsubMenuId, acc_usrMB_p10,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( acc_usrMB_top_b8, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4 */
	acc_usrMB_p4_shell = XtVaCreatePopupShell ("acc_usrMB_p4_shell",
			xmMenuShellWidgetClass, acc_usrMB,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	acc_usrMB_p4 = XtVaCreateWidget( "acc_usrMB_p4",
			xmRowColumnWidgetClass,
			acc_usrMB_p4_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( acc_usrMB_p4, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b2 */
	acc_usrMB_p4_b2 = XtVaCreateManagedWidget( "acc_usrMB_p4_b2",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b2, XmNactivateCallback,
		(XtCallbackProc) accounts_users_search_accountsCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p4_b2, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b3 */
	acc_usrMB_p4_b3 = XtVaCreateManagedWidget( "acc_usrMB_p4_b3",
			xmSeparatorGadgetClass,
			acc_usrMB_p4,
			NULL );
	UxPutContext( acc_usrMB_p4_b3, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b4 */
	acc_usrMB_p4_b4 = XtVaCreateManagedWidget( "acc_usrMB_p4_b4",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Account Data" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b4, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_accountCb,
		(XtPointer) 0 );

	UxPutContext( acc_usrMB_p4_b4, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b5 */
	acc_usrMB_p4_b5 = XtVaCreateManagedWidget( "acc_usrMB_p4_b5",
			xmSeparatorGadgetClass,
			acc_usrMB_p4,
			NULL );
	UxPutContext( acc_usrMB_p4_b5, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b10 */
	acc_usrMB_p4_b10 = XtVaCreateManagedWidget( "acc_usrMB_p4_b10",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Assign DataSet" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b10, XmNactivateCallback,
		(XtCallbackProc) accounts_users_assign_datasetsCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p4_b10, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b6 */
	acc_usrMB_p4_b6 = XtVaCreateManagedWidget( "acc_usrMB_p4_b6",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Assign Users" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b6, XmNactivateCallback,
		(XtCallbackProc) accounts_users_assign_usersCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p4_b6, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b7 */
	acc_usrMB_p4_b7 = XtVaCreateManagedWidget( "acc_usrMB_p4_b7",
			xmSeparatorGadgetClass,
			acc_usrMB_p4,
			NULL );
	UxPutContext( acc_usrMB_p4_b7, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b8 */
	acc_usrMB_p4_b8 = XtVaCreateManagedWidget( "acc_usrMB_p4_b8",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Create" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b8, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_accountCb,
		(XtPointer) 1 );

	UxPutContext( acc_usrMB_p4_b8, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_p4_b9 */
	acc_usrMB_p4_b9 = XtVaCreateManagedWidget( "acc_usrMB_p4_b9",
			xmPushButtonGadgetClass,
			acc_usrMB_p4,
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( acc_usrMB_p4_b9, XmNactivateCallback,
		(XtCallbackProc) accounts_users_delete_accountCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( acc_usrMB_p4_b9, (char *) UxAccounts_usersContext );


	/* Creation of acc_usrMB_top_b2 */
	acc_usrMB_top_b2 = XtVaCreateManagedWidget( "acc_usrMB_top_b2",
			xmCascadeButtonWidgetClass,
			acc_usrMB,
			RES_CONVERT( XmNlabelString, "Accounts" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			XmNsubMenuId, acc_usrMB_p4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( acc_usrMB_top_b2, (char *) UxAccounts_usersContext );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			accounts_users,
			XmNx, 672,
			XmNy, 76,
			XmNwidth, 152,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ACCOUNTS" ),
			NULL );
	UxPutContext( label1, (char *) UxAccounts_usersContext );


	/* Creation of label3 */
	label3 = XtVaCreateManagedWidget( "label3",
			xmLabelWidgetClass,
			accounts_users,
			XmNx, 224,
			XmNy, 42,
			XmNwidth, 604,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "USERS AND ACCOUNTS MANAGEMENT" ),
			NULL );
	UxPutContext( label3, (char *) UxAccounts_usersContext );


	/* Creation of frame1 */
	frame1 = XtVaCreateManagedWidget( "frame1",
			xmFrameWidgetClass,
			accounts_users,
			XmNwidth, 483,
			XmNheight, 616,
			XmNx, 516,
			XmNy, 112,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame1, (char *) UxAccounts_usersContext );


	/* Creation of form1 */
	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass,
			frame1,
			XmNwidth, 475,
			XmNheight, 628,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 4,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form1, (char *) UxAccounts_usersContext );


	/* Creation of label6 */
	label6 = XtVaCreateManagedWidget( "label6",
			xmLabelWidgetClass,
			form1,
			XmNx, 30,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID" ),
			NULL );
	UxPutContext( label6, (char *) UxAccounts_usersContext );


	/* Creation of label7 */
	label7 = XtVaCreateManagedWidget( "label7",
			xmLabelWidgetClass,
			form1,
			XmNx, 190,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account Type" ),
			NULL );
	UxPutContext( label7, (char *) UxAccounts_usersContext );


	/* Creation of label24 */
	label24 = XtVaCreateManagedWidget( "label24",
			xmLabelWidgetClass,
			form1,
			XmNx, 328,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Resource" ),
			NULL );
	UxPutContext( label24, (char *) UxAccounts_usersContext );


	/* Creation of accountsSW */
	accountsSW = XtVaCreateManagedWidget( "accountsSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 18,
			XmNy, 52,
			XmNwidth, 416,
			XmNheight, 461,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( accountsSW, (char *) UxAccounts_usersContext );


	/* Creation of accountsSL */
	accountsSL = XtVaCreateManagedWidget( "accountsSL",
			xmListWidgetClass,
			accountsSW,
			XmNwidth, 414,
			XmNheight, 458,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmSINGLE_SELECT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 2,
			RES_CONVERT( XmNhighlightColor, "black" ),
			RES_CONVERT( XmNforeground, "black" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( accountsSL, XmNdefaultActionCallback,
		(XtCallbackProc) accounts_users_create_accountCb,
		(XtPointer) 0 );
	XtAddCallback( accountsSL, XmNsingleSelectionCallback,
		(XtCallbackProc) accounts_users_accounts_listCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( accountsSL, (char *) UxAccounts_usersContext );


	/* Creation of accounts_sbSW */
	accounts_sbSW = XtVaCreateManagedWidget( "accounts_sbSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 440,
			XmNy, 52,
			XmNheight, 461,
			XmNwidth, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( accounts_sbSW, (char *) UxAccounts_usersContext );


	/* Creation of accounts_sbSL */
	accounts_sbSL = XtVaCreateManagedWidget( "accounts_sbSL",
			xmListWidgetClass,
			accounts_sbSW,
			XmNwidth, 2,
			XmNheight, 441,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNlistSizePolicy, XmVARIABLE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNvisibleItemCount, 25,
			NULL );
	UxPutContext( accounts_sbSL, (char *) UxAccounts_usersContext );

	accounts_users_accounts_sbCb( accounts_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of create_accountPB */
	create_accountPB = XtVaCreateManagedWidget( "create_accountPB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 18,
			XmNy, 564,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( create_accountPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_accountCb,
		(XtPointer) 1 );

	UxPutContext( create_accountPB, (char *) UxAccounts_usersContext );


	/* Creation of account_dataPB */
	account_dataPB = XtVaCreateManagedWidget( "account_dataPB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 318,
			XmNy, 526,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account Data" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( account_dataPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_accountCb,
		(XtPointer) 0 );

	UxPutContext( account_dataPB, (char *) UxAccounts_usersContext );


	/* Creation of delete_accountPB */
	delete_accountPB = XtVaCreateManagedWidget( "delete_accountPB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 320,
			XmNy, 562,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( delete_accountPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_delete_accountCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( delete_accountPB, (char *) UxAccounts_usersContext );


	/* Creation of searchPB */
	searchPB = XtVaCreateManagedWidget( "searchPB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 18,
			XmNy, 526,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( searchPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_search_accountsCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( searchPB, (char *) UxAccounts_usersContext );


	/* Creation of assign_usersButton */
	assign_usersButton = XtVaCreateManagedWidget( "assign_usersButton",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 172,
			XmNy, 562,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Assign Users" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( assign_usersButton, XmNactivateCallback,
		(XtCallbackProc) accounts_users_assign_usersCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( assign_usersButton, (char *) UxAccounts_usersContext );


	/* Creation of assign_datasetsButton */
	assign_datasetsButton = XtVaCreateManagedWidget( "assign_datasetsButton",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 172,
			XmNy, 526,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Assign Dataset" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( assign_datasetsButton, XmNactivateCallback,
		(XtCallbackProc) accounts_users_assign_datasetsCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( assign_datasetsButton, (char *) UxAccounts_usersContext );


	/* Creation of frame2 */
	frame2 = XtVaCreateManagedWidget( "frame2",
			xmFrameWidgetClass,
			accounts_users,
			XmNwidth, 483,
			XmNheight, 616,
			XmNx, 21,
			XmNy, 112,
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( frame2, (char *) UxAccounts_usersContext );


	/* Creation of form2 */
	form2 = XtVaCreateManagedWidget( "form2",
			xmFormWidgetClass,
			frame2,
			XmNwidth, 477,
			XmNheight, 501,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 4,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form2, (char *) UxAccounts_usersContext );


	/* Creation of label4 */
	label4 = XtVaCreateManagedWidget( "label4",
			xmLabelWidgetClass,
			form2,
			XmNx, 32,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID" ),
			NULL );
	UxPutContext( label4, (char *) UxAccounts_usersContext );


	/* Creation of usersSW */
	usersSW = XtVaCreateManagedWidget( "usersSW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 18,
			XmNy, 52,
			XmNwidth, 416,
			XmNheight, 461,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( usersSW, (char *) UxAccounts_usersContext );


	/* Creation of usersSL */
	usersSL = XtVaCreateManagedWidget( "usersSL",
			xmListWidgetClass,
			usersSW,
			XmNwidth, 414,
			XmNheight, 458,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmSINGLE_SELECT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( usersSL, XmNdefaultActionCallback,
		(XtCallbackProc) accounts_users_create_userCb,
		(XtPointer) 0 );
	XtAddCallback( usersSL, XmNsingleSelectionCallback,
		(XtCallbackProc) accounts_users_users_listCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( usersSL, (char *) UxAccounts_usersContext );


	/* Creation of create_userPB */
	create_userPB = XtVaCreateManagedWidget( "create_userPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 18,
			XmNy, 562,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( create_userPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_userCb,
		(XtPointer) 1 );

	UxPutContext( create_userPB, (char *) UxAccounts_usersContext );


	/* Creation of user_dataPB */
	user_dataPB = XtVaCreateManagedWidget( "user_dataPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 318,
			XmNy, 526,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Data" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( user_dataPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_create_userCb,
		(XtPointer) 0 );

	UxPutContext( user_dataPB, (char *) UxAccounts_usersContext );


	/* Creation of delete_userPB */
	delete_userPB = XtVaCreateManagedWidget( "delete_userPB",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 318,
			XmNy, 564,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( delete_userPB, XmNactivateCallback,
		(XtCallbackProc) accounts_users_delete_userCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( delete_userPB, (char *) UxAccounts_usersContext );


	/* Creation of search_usersButton */
	search_usersButton = XtVaCreateManagedWidget( "search_usersButton",
			xmPushButtonWidgetClass,
			form2,
			XmNx, 18,
			XmNy, 526,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( search_usersButton, XmNactivateCallback,
		(XtCallbackProc) accounts_users_search_usersCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( search_usersButton, (char *) UxAccounts_usersContext );


	/* Creation of label8 */
	label8 = XtVaCreateManagedWidget( "label8",
			xmLabelWidgetClass,
			form2,
			XmNx, 174,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Type" ),
			NULL );
	UxPutContext( label8, (char *) UxAccounts_usersContext );


	/* Creation of users_sbSW */
	users_sbSW = XtVaCreateManagedWidget( "users_sbSW",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 440,
			XmNy, 52,
			XmNheight, 461,
			XmNwidth, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( users_sbSW, (char *) UxAccounts_usersContext );


	/* Creation of users_sbSL */
	users_sbSL = XtVaCreateManagedWidget( "users_sbSL",
			xmListWidgetClass,
			users_sbSW,
			XmNwidth, 2,
			XmNheight, 441,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNlistSizePolicy, XmVARIABLE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNvisibleItemCount, 25,
			NULL );
	UxPutContext( users_sbSL, (char *) UxAccounts_usersContext );

	accounts_users_users_sbCb( users_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of label86 */
	label86 = XtVaCreateManagedWidget( "label86",
			xmLabelWidgetClass,
			form2,
			XmNx, 292,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name" ),
			NULL );
	UxPutContext( label86, (char *) UxAccounts_usersContext );


	/* Creation of label2 */
	label2 = XtVaCreateManagedWidget( "label2",
			xmLabelWidgetClass,
			accounts_users,
			XmNx, 192,
			XmNy, 76,
			XmNwidth, 152,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "USERS" ),
			NULL );
	UxPutContext( label2, (char *) UxAccounts_usersContext );


	/* Creation of pushButton5 */
	pushButton5 = XtVaCreateManagedWidget( "pushButton5",
			xmPushButtonWidgetClass,
			accounts_users,
			XmNx, 22,
			XmNy, 732,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Help" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( pushButton5, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton5,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( pushButton5, (char *) UxAccounts_usersContext );


	/* Creation of closeButton */
	closeButton = XtVaCreateManagedWidget( "closeButton",
			xmPushButtonWidgetClass,
			accounts_users,
			XmNx, 858,
			XmNy, 732,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Close" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closeButton, XmNactivateCallback,
		(XtCallbackProc) accounts_users_closeCb,
		(XtPointer) UxAccounts_usersContext );

	UxPutContext( closeButton, (char *) UxAccounts_usersContext );

	XtVaSetValues(acc_usrMB,
			XmNmenuHelpWidget, acc_usrMB_top_b3,
			NULL );


	XtAddCallback( accounts_users, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAccounts_usersContext);


	return ( accounts_users );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_accounts_users( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCaccounts_users      *UxContext;
	static int		_Uxinit = 0;

	UxAccounts_usersContext = UxContext =
		(_UxCaccounts_users *) UxNewContext( sizeof(_UxCaccounts_users), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_accounts_users();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

