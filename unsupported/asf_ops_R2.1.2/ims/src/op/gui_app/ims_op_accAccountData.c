
/*******************************************************************************
	ims_op_accAccountData.c

       Associated Header file: ims_op_accAccountData.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_op_accCb.h>
/*typedef enum account_user_data { CREATE = 1, DATA = 2 } ACCOUNT_USER_DATA ;*/


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accAccountData.h"
#undef CONTEXT_MACRO_ACCESS

Widget	account_data;
Widget	account_dataLB1;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_account_data()
{
	Widget		_UxParent;
	Widget		optionMenu_p2_shell;
	Widget		option_menu_pane_type_shell;
	Widget		option_menu_pane_resource_shell;


	/* Creation of account_data */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "account_data_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 484,
			XmNy, 30,
			XmNwidth, 585,
			XmNheight, 740,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "account_data",
			NULL );

	}

	account_data = XtVaCreateManagedWidget( "account_data",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 585,
			XmNheight, 740,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( account_data, (char *) UxAccount_dataContext );
	UxPutClassCode( account_data, _UxIfClassId );


	/* Creation of scrolledWindow2 */
	scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
			xmScrolledWindowWidgetClass,
			account_data,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 531,
			XmNheight, 619,
			XmNx, 28,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( scrolledWindow2, (char *) UxAccount_dataContext );


	/* Creation of form5 */
	form5 = XtVaCreateManagedWidget( "form5",
			xmFormWidgetClass,
			scrolledWindow2,
			XmNwidth, 527,
			XmNheight, 615,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form5, (char *) UxAccount_dataContext );


	/* Creation of typeTF */
	typeTF = XtVaCreateManagedWidget( "typeTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 50,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 15,
			XmNcolumns, 15,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( typeTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( typeTF, (char *) UxAccount_dataContext );


	/* Creation of label27 */
	label27 = XtVaCreateManagedWidget( "label27",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 59,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type:" ),
			NULL );
	UxPutContext( label27, (char *) UxAccount_dataContext );


	/* Creation of label35 */
	label35 = XtVaCreateManagedWidget( "label35",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 19,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ID:" ),
			NULL );
	UxPutContext( label35, (char *) UxAccount_dataContext );


	/* Creation of resourceTF */
	resourceTF = XtVaCreateManagedWidget( "resourceTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 92,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( resourceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( resourceTF, (char *) UxAccount_dataContext );


	/* Creation of procTF */
	procTF = XtVaCreateManagedWidget( "procTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 496,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( procTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( procTF, (char *) UxAccount_dataContext );


	/* Creation of optionMenu_p2 */
	optionMenu_p2_shell = XtVaCreatePopupShell ("optionMenu_p2_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	optionMenu_p2 = XtVaCreateWidget( "optionMenu_p2",
			xmRowColumnWidgetClass,
			optionMenu_p2_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 94,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( optionMenu_p2, (char *) UxAccount_dataContext );


	/* Creation of optionMenu_p_b2 */
	optionMenu_p_b2 = XtVaCreateManagedWidget( "optionMenu_p_b2",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "None" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 90,
			NULL );
	XtAddCallback( optionMenu_p_b2, XmNactivateCallback,
		(XtCallbackProc) account_data_option_special_procCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( optionMenu_p_b2, (char *) UxAccount_dataContext );

	account_data_option_special_procCb( optionMenu_p_b2,
			(XtPointer) UxAccount_dataContext, (XtPointer) NULL );


	/* Creation of optionMenu2 */
	optionMenu2 = XtVaCreateManagedWidget( "optionMenu2",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, optionMenu_p2,
			XmNx, 374,
			XmNy, 494,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( optionMenu2, (char *) UxAccount_dataContext );


	/* Creation of option_menu_pane_type */
	option_menu_pane_type_shell = XtVaCreatePopupShell ("option_menu_pane_type_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	option_menu_pane_type = XtVaCreateWidget( "option_menu_pane_type",
			xmRowColumnWidgetClass,
			option_menu_pane_type_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNresizeHeight, TRUE,
			XmNwidth, 94,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( option_menu_pane_type, (char *) UxAccount_dataContext );


	/* Creation of option_menu_typePB */
	option_menu_typePB = XtVaCreateManagedWidget( "option_menu_typePB",
			xmPushButtonWidgetClass,
			option_menu_pane_type,
			RES_CONVERT( XmNlabelString, "None" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 90,
			NULL );
	XtAddCallback( option_menu_typePB, XmNactivateCallback,
		(XtCallbackProc) account_data_option_typeCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_typePB, (char *) UxAccount_dataContext );

	account_data_option_typeCb( option_menu_typePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_type */
	option_menu_type = XtVaCreateManagedWidget( "option_menu_type",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_type,
			XmNx, 370,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( option_menu_type, (char *) UxAccount_dataContext );


	/* Creation of option_menu_pane_resource */
	option_menu_pane_resource_shell = XtVaCreatePopupShell ("option_menu_pane_resource_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	option_menu_pane_resource = XtVaCreateWidget( "option_menu_pane_resource",
			xmRowColumnWidgetClass,
			option_menu_pane_resource_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 94,
			XmNpacking, XmPACK_TIGHT,
			XmNorientation, XmVERTICAL,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( option_menu_pane_resource, (char *) UxAccount_dataContext );


	/* Creation of option_menu_resourcePB */
	option_menu_resourcePB = XtVaCreateManagedWidget( "option_menu_resourcePB",
			xmPushButtonWidgetClass,
			option_menu_pane_resource,
			RES_CONVERT( XmNlabelString, "None" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 90,
			NULL );
	XtAddCallback( option_menu_resourcePB, XmNactivateCallback,
		(XtCallbackProc) account_data_option_resourcesCb,
		(XtPointer)0 );

	UxPutContext( option_menu_resourcePB, (char *) UxAccount_dataContext );

	account_data_option_resourcesCb( option_menu_resourcePB,
			(XtPointer)1, (XtPointer) NULL );


	/* Creation of option_menu_resource */
	option_menu_resource = XtVaCreateManagedWidget( "option_menu_resource",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_resource,
			XmNx, 370,
			XmNy, 90,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			XmNwidth, 123,
			NULL );
	UxPutContext( option_menu_resource, (char *) UxAccount_dataContext );


	/* Creation of label42 */
	label42 = XtVaCreateManagedWidget( "label42",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 183,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Current Balance:" ),
			NULL );
	UxPutContext( label42, (char *) UxAccount_dataContext );


	/* Creation of scrolledWindowText2 */
	scrolledWindowText2 = XtVaCreateManagedWidget( "scrolledWindowText2",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNx, 150,
			XmNy, 538,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 366,
			XmNheight, 64,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( scrolledWindowText2, (char *) UxAccount_dataContext );


	/* Creation of commentsST */
	commentsST = XtVaCreateManagedWidget( "commentsST",
			xmTextWidgetClass,
			scrolledWindowText2,
			XmNwidth, 340,
			XmNheight, 35,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNwordWrap, FALSE,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 29,
			XmNrows, 2,
			XmNscrollHorizontal, FALSE,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNscrollVertical, TRUE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( commentsST, (char *) UxAccount_dataContext );


	/* Creation of label9 */
	label9 = XtVaCreateManagedWidget( "label9",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 97,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Resource:" ),
			NULL );
	UxPutContext( label9, (char *) UxAccount_dataContext );


	/* Creation of creationTF */
	creationTF = XtVaCreateManagedWidget( "creationTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 264,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( creationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( creationTF, XmNfocusCallback,
		(XtCallbackProc) account_data_creation_focusCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( creationTF, (char *) UxAccount_dataContext );


	/* Creation of label11 */
	label11 = XtVaCreateManagedWidget( "label11",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 137,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Begin Balance:" ),
			NULL );
	UxPutContext( label11, (char *) UxAccount_dataContext );


	/* Creation of label12 */
	label12 = XtVaCreateManagedWidget( "label12",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 552,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Comments:" ),
			NULL );
	UxPutContext( label12, (char *) UxAccount_dataContext );


	/* Creation of label13 */
	label13 = XtVaCreateManagedWidget( "label13",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 359,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Rate Multiplier:" ),
			NULL );
	UxPutContext( label13, (char *) UxAccount_dataContext );


	/* Creation of label14 */
	label14 = XtVaCreateManagedWidget( "label14",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 229,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "On Hold:" ),
			NULL );
	UxPutContext( label14, (char *) UxAccount_dataContext );


	/* Creation of label15 */
	label15 = XtVaCreateManagedWidget( "label15",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 317,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Expiration Time:" ),
			NULL );
	UxPutContext( label15, (char *) UxAccount_dataContext );


	/* Creation of label16 */
	label16 = XtVaCreateManagedWidget( "label16",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 494,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Special\nProcessing:" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNwidth, 93,
			NULL );
	UxPutContext( label16, (char *) UxAccount_dataContext );


	/* Creation of label18 */
	label18 = XtVaCreateManagedWidget( "label18",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 454,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Manual Validate:" ),
			NULL );
	UxPutContext( label18, (char *) UxAccount_dataContext );


	/* Creation of frame3 */
	frame3 = XtVaCreateManagedWidget( "frame3",
			xmFrameWidgetClass,
			form5,
			XmNwidth, 122,
			XmNheight, 44,
			XmNx, 152,
			XmNy, 442,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame3, (char *) UxAccount_dataContext );


	/* Creation of rowColumn1 */
	rowColumn1 = XtVaCreateManagedWidget( "rowColumn1",
			xmRowColumnWidgetClass,
			frame3,
			XmNwidth, 115,
			XmNheight, 40,
			XmNx, 140,
			XmNy, 2,
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			XmNshadowThickness, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNradioAlwaysOne, TRUE,
			XmNwhichButton, 2,
			NULL );
	UxPutContext( rowColumn1, (char *) UxAccount_dataContext );


	/* Creation of yesTB */
	yesTB = XtVaCreateManagedWidget( "yesTB",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 140,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Yes" ),
			XmNset, TRUE,
			XmNwidth, 52,
			XmNheight, 34,
			NULL );
	XtAddCallback( yesTB, XmNvalueChangedCallback,
		(XtCallbackProc) account_data_validationCb,
		(XtPointer) 0x1 );

	UxPutContext( yesTB, (char *) UxAccount_dataContext );


	/* Creation of noTB */
	noTB = XtVaCreateManagedWidget( "noTB",
			xmToggleButtonWidgetClass,
			rowColumn1,
			XmNx, 140,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "No" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNset, FALSE,
			XmNwidth, 52,
			XmNheight, 34,
			NULL );
	XtAddCallback( noTB, XmNvalueChangedCallback,
		(XtCallbackProc) account_data_validationCb,
		(XtPointer) 0x2 );

	UxPutContext( noTB, (char *) UxAccount_dataContext );


	/* Creation of idTF */
	idTF = XtVaCreateManagedWidget( "idTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			NULL );
	XtAddCallback( idTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( idTF, (char *) UxAccount_dataContext );

	account_data_account_idCb( idTF,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of label84 */
	label84 = XtVaCreateManagedWidget( "label84",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 273,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Creation Time:" ),
			NULL );
	UxPutContext( label84, (char *) UxAccount_dataContext );


	/* Creation of frame6 */
	frame6 = XtVaCreateManagedWidget( "frame6",
			xmFrameWidgetClass,
			form5,
			XmNwidth, 118,
			XmNheight, 74,
			XmNx, 387,
			XmNy, 267,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame6, (char *) UxAccount_dataContext );


	/* Creation of form8 */
	form8 = XtVaCreateManagedWidget( "form8",
			xmFormWidgetClass,
			frame6,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 56,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form8, (char *) UxAccount_dataContext );


	/* Creation of label85 */
	label85 = XtVaCreateManagedWidget( "label85",
			xmLabelWidgetClass,
			form8,
			XmNx, -1,
			XmNy, -2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "YYYY-MM-DD\nHH:MM:SS" ),
			XmNheight, 74,
			NULL );
	UxPutContext( label85, (char *) UxAccount_dataContext );


	/* Creation of expirationTF */
	expirationTF = XtVaCreateManagedWidget( "expirationTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 308,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNheight, 35,
			XmNmaxLength, 25,
			NULL );
	XtAddCallback( expirationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( expirationTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) account_data_check_dateCb,
		(XtPointer) 0 );
	XtAddCallback( expirationTF, XmNmotionVerifyCallback,
		(XtCallbackProc) account_data_check_dateCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( expirationTF, (char *) UxAccount_dataContext );


	/* Creation of rateTF */
	rateTF = XtVaCreateManagedWidget( "rateTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 350,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNheight, 35,
			XmNmaxLength, 25,
			NULL );
	XtAddCallback( rateTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( rateTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) 0 );
	XtAddCallback( rateTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxAccount_dataContext );
	XtAddCallback( rateTF, XmNfocusCallback,
		(XtCallbackProc) account_data_rate_focusCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( rateTF, (char *) UxAccount_dataContext );


	/* Creation of current_balanceTF */
	current_balanceTF = XtVaCreateManagedWidget( "current_balanceTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 176,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNheight, 35,
			XmNmaxLength, 25,
			NULL );
	XtAddCallback( current_balanceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( current_balanceTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) 0 );
	XtAddCallback( current_balanceTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxAccount_dataContext );
	XtAddCallback( current_balanceTF, XmNfocusCallback,
		(XtCallbackProc) account_data_current_balance_focusCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( current_balanceTF, (char *) UxAccount_dataContext );

	account_data_current_balanceCb( current_balanceTF,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of on_holdTF */
	on_holdTF = XtVaCreateManagedWidget( "on_holdTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 220,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNheight, 35,
			XmNmaxLength, 25,
			NULL );
	XtAddCallback( on_holdTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( on_holdTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) 0 );
	XtAddCallback( on_holdTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxAccount_dataContext );
	XtAddCallback( on_holdTF, XmNfocusCallback,
		(XtCallbackProc) account_data_on_hold_focusCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( on_holdTF, (char *) UxAccount_dataContext );

	account_data_on_holdCb( on_holdTF,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of begin_balanceTF */
	begin_balanceTF = XtVaCreateManagedWidget( "begin_balanceTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 134,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNheight, 35,
			XmNvalue, "",
			XmNmaxLength, 25,
			NULL );
	XtAddCallback( begin_balanceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( begin_balanceTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) 0 );
	XtAddCallback( begin_balanceTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( begin_balanceTF, (char *) UxAccount_dataContext );

	account_data_begin_balanceCb( begin_balanceTF,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of label151 */
	label151 = XtVaCreateManagedWidget( "label151",
			xmLabelWidgetClass,
			form5,
			XmNx, 7,
			XmNy, 400,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Manager:" ),
			XmNwidth, 79,
			XmNheight, 23,
			NULL );
	UxPutContext( label151, (char *) UxAccount_dataContext );


	/* Creation of managerTF */
	managerTF = XtVaCreateManagedWidget( "managerTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 150,
			XmNy, 394,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( managerTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( managerTF, (char *) UxAccount_dataContext );


	/* Creation of pushButton1 */
	pushButton1 = XtVaCreateManagedWidget( "pushButton1",
			xmPushButtonWidgetClass,
			form5,
			XmNx, 390,
			XmNy, 394,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNwidth, 120,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( pushButton1, XmNactivateCallback,
		(XtCallbackProc) account_manager_select_dlg_popupCb,
		(XtPointer) 0 );

	UxPutContext( pushButton1, (char *) UxAccount_dataContext );


	/* Creation of account_dataLB1 */
	account_dataLB1 = XtVaCreateManagedWidget( "account_dataLB1",
			xmLabelWidgetClass,
			account_data,
			XmNx, 210,
			XmNy, 13,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create   Account" ),
			XmNwidth, 168,
			NULL );
	UxPutContext( account_dataLB1, (char *) UxAccount_dataContext );

	account_data_labelCb( account_dataLB1,
			(XtPointer) UxAccount_dataContext, (XtPointer) NULL );


	/* Creation of savePB */
	savePB = XtVaCreateManagedWidget( "savePB",
			xmPushButtonWidgetClass,
			account_data,
			XmNx, 14,
			XmNy, 685,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Save" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNrecomputeSize, FALSE,
			NULL );
	XtAddCallback( savePB, XmNactivateCallback,
		(XtCallbackProc) account_data_createCb,
		(XtPointer) 0 );

	UxPutContext( savePB, (char *) UxAccount_dataContext );

	account_data_createCb( savePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of update_balancePB */
	update_balancePB = XtVaCreateManagedWidget( "update_balancePB",
			xmPushButtonWidgetClass,
			account_data,
			XmNx, 220,
			XmNy, 685,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Update Balance" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( update_balancePB, XmNactivateCallback,
		(XtCallbackProc) account_data_update_balanceCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( update_balancePB, (char *) UxAccount_dataContext );

	account_data_update_balanceCb( update_balancePB,
			(XtPointer) 0x1, (XtPointer) NULL );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			account_data,
			XmNx, 428,
			XmNy, 685,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Close" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) account_data_closeCb,
		(XtPointer) UxAccount_dataContext );

	UxPutContext( closePB, (char *) UxAccount_dataContext );


	XtAddCallback( account_data, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAccount_dataContext);


	return ( account_data );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_account_data( swidget _UxUxParent, int _Uxcreate )
{
	Widget                  rtrn;
	_UxCaccount_data        *UxContext;
	static int		_Uxinit = 0;

	UxAccount_dataContext = UxContext =
		(_UxCaccount_data *) UxNewContext( sizeof(_UxCaccount_data), False );

	UxParent = _UxUxParent;
	create = _Uxcreate;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_account_data();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

