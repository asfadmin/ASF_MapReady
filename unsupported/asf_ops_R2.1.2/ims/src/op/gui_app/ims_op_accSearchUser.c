
/*******************************************************************************
	ims_op_accSearchUser.c

       Associated Header file: ims_op_accSearchUser.h
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
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
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
#include "ims_op_accSearchUser.h"
#undef CONTEXT_MACRO_ACCESS

Widget	search_users;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_search_users()
{
	Widget		_UxParent;
	Widget		option_menu_pane_type_shell;


	/* Creation of search_users */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "search_users_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 538,
			XmNy, 48,
			XmNwidth, 559,
			XmNheight, 671,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "search_users",
			NULL );

	}

	search_users = XtVaCreateManagedWidget( "search_users",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 559,
			XmNheight, 671,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( search_users, (char *) UxSearch_usersContext );
	UxPutClassCode( search_users, _UxIfClassId );


	/* Creation of scrolledWindow3 */
	scrolledWindow3 = XtVaCreateManagedWidget( "scrolledWindow3",
			xmScrolledWindowWidgetClass,
			search_users,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 530,
			XmNheight, 562,
			XmNx, 16,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( scrolledWindow3, (char *) UxSearch_usersContext );


	/* Creation of form10 */
	form10 = XtVaCreateManagedWidget( "form10",
			xmFormWidgetClass,
			scrolledWindow3,
			XmNwidth, 496,
			XmNheight, 625,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form10, (char *) UxSearch_usersContext );


	/* Creation of label87 */
	label87 = XtVaCreateManagedWidget( "label87",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 90,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Last Name:" ),
			NULL );
	UxPutContext( label87, (char *) UxSearch_usersContext );


	/* Creation of label88 */
	label88 = XtVaCreateManagedWidget( "label88",
			xmLabelWidgetClass,
			form10,
			XmNx, 352,
			XmNy, 50,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "M.I.:" ),
			NULL );
	UxPutContext( label88, (char *) UxSearch_usersContext );


	/* Creation of idTF */
	idTF = XtVaCreateManagedWidget( "idTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 15,
			XmNcolumns, 15,
			NULL );
	XtAddCallback( idTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( idTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 0x1 );

	UxPutContext( idTF, (char *) UxSearch_usersContext );


	/* Creation of m_iTF */
	m_iTF = XtVaCreateManagedWidget( "m_iTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 44,
			XmNx, 402,
			XmNy, 44,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 3,
			XmNmaxLength, 3,
			NULL );
	XtAddCallback( m_iTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( m_iTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 3 );

	UxPutContext( m_iTF, (char *) UxSearch_usersContext );


	/* Creation of label89 */
	label89 = XtVaCreateManagedWidget( "label89",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 132,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type:" ),
			NULL );
	UxPutContext( label89, (char *) UxSearch_usersContext );


	/* Creation of label90 */
	label90 = XtVaCreateManagedWidget( "label90",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 174,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Title:" ),
			NULL );
	UxPutContext( label90, (char *) UxSearch_usersContext );


	/* Creation of label91 */
	label91 = XtVaCreateManagedWidget( "label91",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 246,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "City:" ),
			NULL );
	UxPutContext( label91, (char *) UxSearch_usersContext );


	/* Creation of label92 */
	label92 = XtVaCreateManagedWidget( "label92",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 288,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "State:" ),
			XmNheight, 28,
			NULL );
	UxPutContext( label92, (char *) UxSearch_usersContext );


	/* Creation of label93 */
	label93 = XtVaCreateManagedWidget( "label93",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 370,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Zip:" ),
			NULL );
	UxPutContext( label93, (char *) UxSearch_usersContext );


	/* Creation of label94 */
	label94 = XtVaCreateManagedWidget( "label94",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 330,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Country:" ),
			NULL );
	UxPutContext( label94, (char *) UxSearch_usersContext );


	/* Creation of label95 */
	label95 = XtVaCreateManagedWidget( "label95",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 408,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Phone:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label95, (char *) UxSearch_usersContext );


	/* Creation of label96 */
	label96 = XtVaCreateManagedWidget( "label96",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ID:" ),
			NULL );
	UxPutContext( label96, (char *) UxSearch_usersContext );


	/* Creation of label97 */
	label97 = XtVaCreateManagedWidget( "label97",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 46,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "First Name:" ),
			NULL );
	UxPutContext( label97, (char *) UxSearch_usersContext );


	/* Creation of first_nameTF */
	first_nameTF = XtVaCreateManagedWidget( "first_nameTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 44,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			NULL );
	XtAddCallback( first_nameTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( first_nameTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 0x2 );

	UxPutContext( first_nameTF, (char *) UxSearch_usersContext );


	/* Creation of last_nameTF */
	last_nameTF = XtVaCreateManagedWidget( "last_nameTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 84,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNvalue, "",
			NULL );
	XtAddCallback( last_nameTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( last_nameTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 4 );

	UxPutContext( last_nameTF, (char *) UxSearch_usersContext );


	/* Creation of typeTF */
	typeTF = XtVaCreateManagedWidget( "typeTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 126,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNeditable, FALSE,
			XmNheight, 35,
			NULL );
	XtAddCallback( typeTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( typeTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)5 );
	XtAddCallback( typeTF, XmNvalueChangedCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 5 );

	UxPutContext( typeTF, (char *) UxSearch_usersContext );


	/* Creation of label98 */
	label98 = XtVaCreateManagedWidget( "label98",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 212,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Organization:" ),
			NULL );
	UxPutContext( label98, (char *) UxSearch_usersContext );


	/* Creation of organizationTF */
	organizationTF = XtVaCreateManagedWidget( "organizationTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 208,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 32,
			XmNcolumns, 32,
			XmNvalue, "",
			NULL );
	XtAddCallback( organizationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( organizationTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)7 );

	UxPutContext( organizationTF, (char *) UxSearch_usersContext );


	/* Creation of titleTF */
	titleTF = XtVaCreateManagedWidget( "titleTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 168,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 10,
			XmNcolumns, 10,
			NULL );
	XtAddCallback( titleTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( titleTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 6 );

	UxPutContext( titleTF, (char *) UxSearch_usersContext );


	/* Creation of cityTF */
	cityTF = XtVaCreateManagedWidget( "cityTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 244,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 30,
			XmNcolumns, 30,
			NULL );
	XtAddCallback( cityTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( cityTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)8 );

	UxPutContext( cityTF, (char *) UxSearch_usersContext );


	/* Creation of stateTF */
	stateTF = XtVaCreateManagedWidget( "stateTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 286,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			NULL );
	XtAddCallback( stateTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( stateTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 9 );

	UxPutContext( stateTF, (char *) UxSearch_usersContext );


	/* Creation of countryTF */
	countryTF = XtVaCreateManagedWidget( "countryTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 326,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			NULL );
	XtAddCallback( countryTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( countryTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)10 );

	UxPutContext( countryTF, (char *) UxSearch_usersContext );


	/* Creation of zipTF */
	zipTF = XtVaCreateManagedWidget( "zipTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 366,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 10,
			XmNcolumns, 10,
			NULL );
	XtAddCallback( zipTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( zipTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)11 );

	UxPutContext( zipTF, (char *) UxSearch_usersContext );


	/* Creation of phoneTF */
	phoneTF = XtVaCreateManagedWidget( "phoneTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 21,
			XmNcolumns, 21,
			NULL );
	XtAddCallback( phoneTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( phoneTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 12 );

	UxPutContext( phoneTF, (char *) UxSearch_usersContext );


	/* Creation of faxTF */
	faxTF = XtVaCreateManagedWidget( "faxTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 448,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 21,
			XmNcolumns, 21,
			NULL );
	XtAddCallback( faxTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( faxTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)13 );

	UxPutContext( faxTF, (char *) UxSearch_usersContext );


	/* Creation of emailTF */
	emailTF = XtVaCreateManagedWidget( "emailTF",
			xmTextFieldWidgetClass,
			form10,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 488,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 128,
			XmNcolumns, 128,
			NULL );
	XtAddCallback( emailTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( emailTF, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer)14 );

	UxPutContext( emailTF, (char *) UxSearch_usersContext );


	/* Creation of label99 */
	label99 = XtVaCreateManagedWidget( "label99",
			xmLabelWidgetClass,
			form10,
			XmNx, 0,
			XmNy, 450,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Fax:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label99, (char *) UxSearch_usersContext );


	/* Creation of label100 */
	label100 = XtVaCreateManagedWidget( "label100",
			xmLabelWidgetClass,
			form10,
			XmNx, 4,
			XmNy, 542,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "FTP Dir:" ),
			XmNwidth, 82,
			NULL );
	UxPutContext( label100, (char *) UxSearch_usersContext );


	/* Creation of option_menu_pane_type */
	option_menu_pane_type_shell = XtVaCreatePopupShell ("option_menu_pane_type_shell",
			xmMenuShellWidgetClass, form10,
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
	UxPutContext( option_menu_pane_type, (char *) UxSearch_usersContext );


	/* Creation of option_menu_typePB */
	option_menu_typePB = XtVaCreateManagedWidget( "option_menu_typePB",
			xmPushButtonWidgetClass,
			option_menu_pane_type,
			RES_CONVERT( XmNlabelString, "None" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 79,
			NULL );
	XtAddCallback( option_menu_typePB, XmNactivateCallback,
		(XtCallbackProc) search_users_option_typeCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_typePB, (char *) UxSearch_usersContext );

	search_users_option_typeCb( option_menu_typePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_type */
	option_menu_type = XtVaCreateManagedWidget( "option_menu_type",
			xmRowColumnWidgetClass,
			form10,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_type,
			XmNx, 336,
			XmNy, 124,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			XmNwidth, 90,
			XmNresizeWidth, TRUE,
			NULL );
	UxPutContext( option_menu_type, (char *) UxSearch_usersContext );


	/* Creation of label101 */
	label101 = XtVaCreateManagedWidget( "label101",
			xmLabelWidgetClass,
			form10,
			XmNx, 8,
			XmNy, 488,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Email:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label101, (char *) UxSearch_usersContext );


	/* Creation of ftp_dirSW */
	ftp_dirSW = XtVaCreateManagedWidget( "ftp_dirSW",
			xmScrolledWindowWidgetClass,
			form10,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 114,
			XmNy, 535,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 362,
			XmNheight, 35,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( ftp_dirSW, (char *) UxSearch_usersContext );


	/* Creation of ftp_dirST */
	ftp_dirST = XtVaCreateManagedWidget( "ftp_dirST",
			xmTextWidgetClass,
			ftp_dirSW,
			XmNwidth, 340,
			XmNheight, 35,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNwordWrap, TRUE,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 29,
			XmNrows, 2,
			XmNscrollHorizontal, FALSE,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNscrollVertical, TRUE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( ftp_dirST, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( ftp_dirST, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 15 );

	UxPutContext( ftp_dirST, (char *) UxSearch_usersContext );


	/* Creation of label154 */
	label154 = XtVaCreateManagedWidget( "label154",
			xmLabelWidgetClass,
			form10,
			XmNx, 7,
			XmNy, 585,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Access Dir:" ),
			XmNwidth, 96,
			NULL );
	UxPutContext( label154, (char *) UxSearch_usersContext );


	/* Creation of access_dirSW */
	access_dirSW = XtVaCreateManagedWidget( "access_dirSW",
			xmScrolledWindowWidgetClass,
			form10,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 114,
			XmNy, 579,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 362,
			XmNheight, 35,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( access_dirSW, (char *) UxSearch_usersContext );


	/* Creation of access_dirST */
	access_dirST = XtVaCreateManagedWidget( "access_dirST",
			xmTextWidgetClass,
			access_dirSW,
			XmNwidth, 340,
			XmNheight, 35,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNwordWrap, TRUE,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 29,
			XmNrows, 2,
			XmNscrollHorizontal, FALSE,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNscrollVertical, TRUE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( access_dirST, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );
	XtAddCallback( access_dirST, XmNlosingFocusCallback,
		(XtCallbackProc) search_users_losing_focusCb,
		(XtPointer) 16 );

	UxPutContext( access_dirST, (char *) UxSearch_usersContext );


	/* Creation of label102 */
	label102 = XtVaCreateManagedWidget( "label102",
			xmLabelWidgetClass,
			search_users,
			XmNx, 172,
			XmNy, 20,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Search Users Parameters" ),
			NULL );
	UxPutContext( label102, (char *) UxSearch_usersContext );


	/* Creation of closeButton */
	closeButton = XtVaCreateManagedWidget( "closeButton",
			xmPushButtonWidgetClass,
			search_users,
			XmNx, 402,
			XmNy, 624,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Close" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closeButton, XmNactivateCallback,
		(XtCallbackProc) search_users_closeCb,
		(XtPointer) UxSearch_usersContext );

	UxPutContext( closeButton, (char *) UxSearch_usersContext );


	/* Creation of start_searchPB */
	start_searchPB = XtVaCreateManagedWidget( "start_searchPB",
			xmPushButtonWidgetClass,
			search_users,
			XmNx, 12,
			XmNy, 624,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Start Search" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( start_searchPB, XmNactivateCallback,
		(XtCallbackProc) search_users_start_searchCb,
		(XtPointer) UxSearch_usersContext );

	UxPutContext( start_searchPB, (char *) UxSearch_usersContext );


	XtAddCallback( search_users, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSearch_usersContext);


	return ( search_users );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_search_users( swidget _UxUxParent, int _Uxcreate )
{
	Widget                  rtrn;
	_UxCsearch_users        *UxContext;
	static int		_Uxinit = 0;

	UxSearch_usersContext = UxContext =
		(_UxCsearch_users *) UxNewContext( sizeof(_UxCsearch_users), False );

	UxParent = _UxUxParent;
	create = _Uxcreate;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_search_users();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

