
/*******************************************************************************
	ims_op_accSearchAccount.c

       Associated Header file: ims_op_accSearchAccount.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Frame.h>
#include <Xm/Text.h>
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


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accSearchAccount.h"
#undef CONTEXT_MACRO_ACCESS

Widget	search_accounts;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_search_accounts(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_accounts     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_accountsContext;
	UxContext = UxSearch_accountsContext;
	{
	
	}
	UxSearch_accountsContext = UxSaveCtx;
}

static void  createCB_scrolledWindow2(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCsearch_accounts     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearch_accountsContext;
	UxContext = UxSearch_accountsContext;
	{
	Widget sb ;
	
	/*XtVaGetValues(UxWidget,XmNverticalScrollBar, &sb,NULL ) ;
	XtVaSetValues (sb,XmNbackground,"#9ac0cd",NULL ) ;*/
	}
	UxSearch_accountsContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_search_accounts()
{
	Widget		_UxParent;
	Widget		option_menu_pane_resource_shell;
	Widget		option_menu_pane_type_shell;


	/* Creation of search_accounts */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "search_accounts_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 378,
			XmNy, 130,
			XmNwidth, 585,
			XmNheight, 612,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "search_accounts",
			NULL );

	}

	search_accounts = XtVaCreateManagedWidget( "search_accounts",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 585,
			XmNheight, 612,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( search_accounts, (char *) UxSearch_accountsContext );
	UxPutClassCode( search_accounts, _UxIfClassId );

	createCB_search_accounts( search_accounts,
			(XtPointer) UxSearch_accountsContext, (XtPointer) NULL );


	/* Creation of scrolledWindow2 */
	scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
			xmScrolledWindowWidgetClass,
			search_accounts,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 531,
			XmNheight, 486,
			XmNx, 27,
			XmNy, 60,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( scrolledWindow2, (char *) UxSearch_accountsContext );

	createCB_scrolledWindow2( scrolledWindow2,
			(XtPointer) UxSearch_accountsContext, (XtPointer) NULL );


	/* Creation of form5 */
	form5 = XtVaCreateManagedWidget( "form5",
			xmFormWidgetClass,
			scrolledWindow2,
			XmNwidth, 527,
			XmNheight, 482,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, -180,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form5, (char *) UxSearch_accountsContext );


	/* Creation of typeTF */
	typeTF = XtVaCreateManagedWidget( "typeTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 133,
			XmNy, 57,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 15,
			XmNcolumns, 15,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( typeTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer)XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( typeTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 2 );
	XtAddCallback( typeTF, XmNvalueChangedCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 2 );

	UxPutContext( typeTF, (char *) UxSearch_accountsContext );


	/* Creation of label27 */
	label27 = XtVaCreateManagedWidget( "label27",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 65,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type:" ),
			NULL );
	UxPutContext( label27, (char *) UxSearch_accountsContext );


	/* Creation of label35 */
	label35 = XtVaCreateManagedWidget( "label35",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 25,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ID:" ),
			NULL );
	UxPutContext( label35, (char *) UxSearch_accountsContext );


	/* Creation of resourceTF */
	resourceTF = XtVaCreateManagedWidget( "resourceTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 133,
			XmNy, 99,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( resourceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer)XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( resourceTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 3 );
	XtAddCallback( resourceTF, XmNvalueChangedCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 3 );

	UxPutContext( resourceTF, (char *) UxSearch_accountsContext );


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
			XmNwidth, 90,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( option_menu_pane_resource, (char *) UxSearch_accountsContext );


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
		(XtCallbackProc) search_accounts_option_resourcesCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_resourcePB, (char *) UxSearch_accountsContext );

	search_accounts_option_resourcesCb( option_menu_resourcePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_resource */
	option_menu_resource = XtVaCreateManagedWidget( "option_menu_resource",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_resource,
			XmNx, 364,
			XmNy, 98,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			XmNentryAlignment, XmALIGNMENT_BEGINNING,
			XmNresizeWidth, TRUE,
			XmNwidth, 138,
			NULL );
	UxPutContext( option_menu_resource, (char *) UxSearch_accountsContext );


	/* Creation of scrolledWindowText2 */
	scrolledWindowText2 = XtVaCreateManagedWidget( "scrolledWindowText2",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNx, 137,
			XmNy, 405,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 366,
			XmNheight, 64,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( scrolledWindowText2, (char *) UxSearch_accountsContext );


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
	XtAddCallback( commentsST, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 10 );
	XtAddCallback( commentsST, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( commentsST, (char *) UxSearch_accountsContext );


	/* Creation of label9 */
	label9 = XtVaCreateManagedWidget( "label9",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 105,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Resource:" ),
			NULL );
	UxPutContext( label9, (char *) UxSearch_accountsContext );


	/* Creation of label10 */
	label10 = XtVaCreateManagedWidget( "label10",
			xmLabelWidgetClass,
			form5,
			XmNx, 169,
			XmNy, 358,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Begin:" ),
			NULL );
	UxPutContext( label10, (char *) UxSearch_accountsContext );


	/* Creation of label12 */
	label12 = XtVaCreateManagedWidget( "label12",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 411,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Comments:" ),
			NULL );
	UxPutContext( label12, (char *) UxSearch_accountsContext );


	/* Creation of label13 */
	label13 = XtVaCreateManagedWidget( "label13",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 356,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Current Balance:" ),
			NULL );
	UxPutContext( label13, (char *) UxSearch_accountsContext );


	/* Creation of label15 */
	label15 = XtVaCreateManagedWidget( "label15",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 300,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Expiration Date:" ),
			NULL );
	UxPutContext( label15, (char *) UxSearch_accountsContext );


	/* Creation of idTF */
	idTF = XtVaCreateManagedWidget( "idTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 133,
			XmNy, 15,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			NULL );
	XtAddCallback( idTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer)XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( idTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 1 );

	UxPutContext( idTF, (char *) UxSearch_accountsContext );


	/* Creation of label78 */
	label78 = XtVaCreateManagedWidget( "label78",
			xmLabelWidgetClass,
			form5,
			XmNx, 343,
			XmNy, 359,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "End:" ),
			NULL );
	UxPutContext( label78, (char *) UxSearch_accountsContext );


	/* Creation of label79 */
	label79 = XtVaCreateManagedWidget( "label79",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 259,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Creation Date:" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label79, (char *) UxSearch_accountsContext );


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
			XmNresizeWidth, FALSE,
			XmNwidth, 90,
			NULL );
	UxPutContext( option_menu_pane_type, (char *) UxSearch_accountsContext );


	/* Creation of option_menu_typePB */
	option_menu_typePB = XtVaCreateManagedWidget( "option_menu_typePB",
			xmPushButtonWidgetClass,
			option_menu_pane_type,
			RES_CONVERT( XmNlabelString, "None" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( option_menu_typePB, XmNactivateCallback,
		(XtCallbackProc) search_accounts_option_typeCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_typePB, (char *) UxSearch_accountsContext );

	search_accounts_option_typeCb( option_menu_typePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_type */
	option_menu_type = XtVaCreateManagedWidget( "option_menu_type",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_type,
			XmNx, 364,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( option_menu_type, (char *) UxSearch_accountsContext );


	/* Creation of label80 */
	label80 = XtVaCreateManagedWidget( "label80",
			xmLabelWidgetClass,
			form5,
			XmNx, 169,
			XmNy, 259,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Start:" ),
			NULL );
	UxPutContext( label80, (char *) UxSearch_accountsContext );


	/* Creation of label81 */
	label81 = XtVaCreateManagedWidget( "label81",
			xmLabelWidgetClass,
			form5,
			XmNx, 343,
			XmNy, 260,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "End:" ),
			NULL );
	UxPutContext( label81, (char *) UxSearch_accountsContext );


	/* Creation of label82 */
	label82 = XtVaCreateManagedWidget( "label82",
			xmLabelWidgetClass,
			form5,
			XmNx, 169,
			XmNy, 300,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Start:" ),
			NULL );
	UxPutContext( label82, (char *) UxSearch_accountsContext );


	/* Creation of label83 */
	label83 = XtVaCreateManagedWidget( "label83",
			xmLabelWidgetClass,
			form5,
			XmNx, 343,
			XmNy, 301,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "End:" ),
			NULL );
	UxPutContext( label83, (char *) UxSearch_accountsContext );


	/* Creation of start_creationTF */
	start_creationTF = XtVaCreateManagedWidget( "start_creationTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 223,
			XmNy, 255,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( start_creationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( start_creationTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 4 );
	XtAddCallback( start_creationTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( start_creationTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( start_creationTF, (char *) UxSearch_accountsContext );


	/* Creation of end_creationTF */
	end_creationTF = XtVaCreateManagedWidget( "end_creationTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 389,
			XmNy, 255,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( end_creationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( end_creationTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 5 );
	XtAddCallback( end_creationTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( end_creationTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( end_creationTF, (char *) UxSearch_accountsContext );


	/* Creation of start_expirationTF */
	start_expirationTF = XtVaCreateManagedWidget( "start_expirationTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 223,
			XmNy, 296,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( start_expirationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( start_expirationTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 6 );
	XtAddCallback( start_expirationTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( start_expirationTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( start_expirationTF, (char *) UxSearch_accountsContext );


	/* Creation of end_expirationTF */
	end_expirationTF = XtVaCreateManagedWidget( "end_expirationTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 389,
			XmNy, 296,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( end_expirationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( end_expirationTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 7 );
	XtAddCallback( end_expirationTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( end_expirationTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_dateCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( end_expirationTF, (char *) UxSearch_accountsContext );


	/* Creation of start_balanceTF */
	start_balanceTF = XtVaCreateManagedWidget( "start_balanceTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 223,
			XmNy, 354,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( start_balanceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( start_balanceTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 8 );
	XtAddCallback( start_balanceTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( start_balanceTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( start_balanceTF, (char *) UxSearch_accountsContext );


	/* Creation of end_balanceTF */
	end_balanceTF = XtVaCreateManagedWidget( "end_balanceTF",
			xmTextWidgetClass,
			form5,
			XmNwidth, 114,
			XmNx, 389,
			XmNy, 354,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( end_balanceTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( end_balanceTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 9 );
	XtAddCallback( end_balanceTF, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxSearch_accountsContext );
	XtAddCallback( end_balanceTF, XmNmotionVerifyCallback,
		(XtCallbackProc) search_accounts_check_balanceCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( end_balanceTF, (char *) UxSearch_accountsContext );


	/* Creation of frame8 */
	frame8 = XtVaCreateManagedWidget( "frame8",
			xmFrameWidgetClass,
			form5,
			XmNwidth, 278,
			XmNheight, 38,
			XmNx, 223,
			XmNy, 206,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame8, (char *) UxSearch_accountsContext );


	/* Creation of form9 */
	form9 = XtVaCreateManagedWidget( "form9",
			xmFormWidgetClass,
			frame8,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 56,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form9, (char *) UxSearch_accountsContext );


	/* Creation of label5 */
	label5 = XtVaCreateManagedWidget( "label5",
			xmLabelWidgetClass,
			form9,
			XmNx, 8,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Date Format : YYYY-MM-DD" ),
			XmNheight, 32,
			XmNwidth, 258,
			NULL );
	UxPutContext( label5, (char *) UxSearch_accountsContext );


	/* Creation of label152 */
	label152 = XtVaCreateManagedWidget( "label152",
			xmLabelWidgetClass,
			form5,
			XmNx, 18,
			XmNy, 147,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Manager:" ),
			XmNwidth, 79,
			XmNheight, 23,
			NULL );
	UxPutContext( label152, (char *) UxSearch_accountsContext );


	/* Creation of managerTF */
	managerTF = XtVaCreateManagedWidget( "managerTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 133,
			XmNy, 142,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, TRUE,
			NULL );
	XtAddCallback( managerTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( managerTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 11 );
	XtAddCallback( managerTF, XmNvalueChangedCallback,
		(XtCallbackProc) search_accounts_losing_focusCb,
		(XtPointer) 11 );

	UxPutContext( managerTF, (char *) UxSearch_accountsContext );


	/* Creation of pushButton2 */
	pushButton2 = XtVaCreateManagedWidget( "pushButton2",
			xmPushButtonWidgetClass,
			form5,
			XmNx, 380,
			XmNy, 144,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 32,
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNwidth, 115,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( pushButton2, XmNactivateCallback,
		(XtCallbackProc) account_manager_select_dlg_popupCb,
		(XtPointer) 1 );

	UxPutContext( pushButton2, (char *) UxSearch_accountsContext );


	/* Creation of label41 */
	label41 = XtVaCreateManagedWidget( "label41",
			xmLabelWidgetClass,
			search_accounts,
			XmNx, 146,
			XmNy, 23,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Search   Accounts   Parameters" ),
			NULL );
	UxPutContext( label41, (char *) UxSearch_accountsContext );


	/* Creation of closeButton */
	closeButton = XtVaCreateManagedWidget( "closeButton",
			xmPushButtonWidgetClass,
			search_accounts,
			XmNx, 401,
			XmNy, 563,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Close" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closeButton, XmNactivateCallback,
		(XtCallbackProc) search_accounts_closeCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( closeButton, (char *) UxSearch_accountsContext );


	/* Creation of pushButton3 */
	pushButton3 = XtVaCreateManagedWidget( "pushButton3",
			xmPushButtonWidgetClass,
			search_accounts,
			XmNx, 48,
			XmNy, 563,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Search" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( pushButton3, XmNactivateCallback,
		(XtCallbackProc) search_accounts_start_searchCb,
		(XtPointer) UxSearch_accountsContext );

	UxPutContext( pushButton3, (char *) UxSearch_accountsContext );


	XtAddCallback( search_accounts, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSearch_accountsContext);


	return ( search_accounts );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_search_accounts( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCsearch_accounts     *UxContext;
	static int		_Uxinit = 0;

	UxSearch_accountsContext = UxContext =
		(_UxCsearch_accounts *) UxNewContext( sizeof(_UxCsearch_accounts), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_search_accounts();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

