
/*******************************************************************************
	vc_searchform.c

       Associated Header file: vc_searchform.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_searchform.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_searchform.c"

#include "cb_searchform.h"

extern Widget searchform ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_searchform.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_QuitSearchForm(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCSearchForm          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSearchFormContext;
	UxSearchFormContext = UxContext =
			(_UxCSearchForm *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(searchform)) ;
	}
	UxSearchFormContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_SearchForm()
{
	Widget		_UxParent;
	Widget		optionMenu_p2_shell;
	Widget		optionMenu_p3_shell;


	/* Creation of SearchForm */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "SearchForm_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 162,
			XmNy, 191,
			XmNwidth, 670,
			XmNheight, 458,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "SearchForm",
			XmNiconName, "SearchForm",
			NULL );

	}

	SearchForm = XtVaCreateManagedWidget( "SearchForm",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 670,
			XmNheight, 458,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "Search Form" ),
			NULL );
	UxPutContext( SearchForm, (char *) UxSearchFormContext );
	UxPutClassCode( SearchForm, _UxIfClassId );


	/* Creation of scrolledWindowText6 */
	scrolledWindowText6 = XtVaCreateManagedWidget( "scrolledWindowText6",
			xmScrolledWindowWidgetClass,
			SearchForm,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 45,
			XmNy, 80,
			XmNwidth, 545,
			NULL );
	UxPutContext( scrolledWindowText6, (char *) UxSearchFormContext );


	/* Creation of scrolledText_SearchClause */
	scrolledText_SearchClause = XtVaCreateManagedWidget( "scrolledText_SearchClause",
			xmTextWidgetClass,
			scrolledWindowText6,
			XmNwidth, 565,
			XmNheight, 105,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNvalue, "where ",
			XmNscrollHorizontal, FALSE,
			XmNwordWrap, TRUE,
			NULL );
	UxPutContext( scrolledText_SearchClause, (char *) UxSearchFormContext );


	/* Creation of scrolledWindowList7 */
	scrolledWindowList7 = XtVaCreateManagedWidget( "scrolledWindowList7",
			xmScrolledWindowWidgetClass,
			SearchForm,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 140,
			XmNy, 260,
			XmNheight, 180,
			XmNresizable, FALSE,
			XmNwidth, 120,
			NULL );
	UxPutContext( scrolledWindowList7, (char *) UxSearchFormContext );


	/* Creation of scrolledList_searchColumnNames */
	scrolledList_searchColumnNames = XtVaCreateManagedWidget( "scrolledList_searchColumnNames",
			xmListWidgetClass,
			scrolledWindowList7,
			XmNheight, 240,
			XmNx, 140,
			XmNy, 0,
			NULL );
	XtAddCallback( scrolledList_searchColumnNames, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_add_search_column,
		(XtPointer) UxSearchFormContext );
	XtAddCallback( scrolledList_searchColumnNames, XmNdefaultActionCallback,
		(XtCallbackProc) cb_add_search_column,
		(XtPointer) UxSearchFormContext );

	UxPutContext( scrolledList_searchColumnNames, (char *) UxSearchFormContext );


	/* Creation of textField_search_column */
	textField_search_column = XtVaCreateManagedWidget( "textField_search_column",
			xmTextFieldWidgetClass,
			SearchForm,
			XmNx, 140,
			XmNy, 218,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 12,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 32,
			XmNwidth, 120,
			NULL );
	UxPutContext( textField_search_column, (char *) UxSearchFormContext );


	/* Creation of optionMenu_p2 */
	optionMenu_p2_shell = XtVaCreatePopupShell ("optionMenu_p2_shell",
			xmMenuShellWidgetClass, SearchForm,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	optionMenu_p2 = XtVaCreateWidget( "optionMenu_p2",
			xmRowColumnWidgetClass,
			optionMenu_p2_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 95,
			NULL );
	UxPutContext( optionMenu_p2, (char *) UxSearchFormContext );


	/* Creation of pb_equal */
	pb_equal = XtVaCreateManagedWidget( "pb_equal",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(=) equal\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_equal, (char *) UxSearchFormContext );


	/* Creation of pb_not_equal */
	pb_not_equal = XtVaCreateManagedWidget( "pb_not_equal",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(!=) not equal\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			NULL );
	UxPutContext( pb_not_equal, (char *) UxSearchFormContext );


	/* Creation of pb_lessthan */
	pb_lessthan = XtVaCreateManagedWidget( "pb_lessthan",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(<) less than\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_lessthan, (char *) UxSearchFormContext );


	/* Creation of pb_greaterthan */
	pb_greaterthan = XtVaCreateManagedWidget( "pb_greaterthan",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(>) greater than\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_greaterthan, (char *) UxSearchFormContext );


	/* Creation of pb_lessequal */
	pb_lessequal = XtVaCreateManagedWidget( "pb_lessequal",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(<=) less than\nor equal to" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_lessequal, (char *) UxSearchFormContext );


	/* Creation of pb_greaterequal */
	pb_greaterequal = XtVaCreateManagedWidget( "pb_greaterequal",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "(>=) greater than\nor equal to" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_greaterequal, (char *) UxSearchFormContext );


	/* Creation of pb_like */
	pb_like = XtVaCreateManagedWidget( "pb_like",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "like\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( pb_like, (char *) UxSearchFormContext );


	/* Creation of pb_notlike */
	pb_notlike = XtVaCreateManagedWidget( "pb_notlike",
			xmPushButtonWidgetClass,
			optionMenu_p2,
			RES_CONVERT( XmNlabelString, "not like\n" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 223,
			NULL );
	UxPutContext( pb_notlike, (char *) UxSearchFormContext );


	/* Creation of optionMenu_operator */
	optionMenu_operator = XtVaCreateManagedWidget( "optionMenu_operator",
			xmRowColumnWidgetClass,
			SearchForm,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, optionMenu_p2,
			XmNx, 260,
			XmNy, 209,
			XmNwidth, 90,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, " " ),
			XmNentryAlignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( optionMenu_operator, (char *) UxSearchFormContext );


	/* Creation of textField_search_value */
	textField_search_value = XtVaCreateManagedWidget( "textField_search_value",
			xmTextFieldWidgetClass,
			SearchForm,
			XmNx, 430,
			XmNy, 218,
			XmNheight, 32,
			XmNcursorPositionVisible, TRUE,
			XmNeditable, TRUE,
			XmNvalue, "",
			XmNcolumns, 25,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 200,
			NULL );
	UxPutContext( textField_search_value, (char *) UxSearchFormContext );


	/* Creation of optionMenu_p3 */
	optionMenu_p3_shell = XtVaCreatePopupShell ("optionMenu_p3_shell",
			xmMenuShellWidgetClass, SearchForm,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	optionMenu_p3 = XtVaCreateWidget( "optionMenu_p3",
			xmRowColumnWidgetClass,
			optionMenu_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 217,
			NULL );
	UxPutContext( optionMenu_p3, (char *) UxSearchFormContext );


	/* Creation of pb_and */
	pb_and = XtVaCreateManagedWidget( "pb_and",
			xmPushButtonWidgetClass,
			optionMenu_p3,
			RES_CONVERT( XmNlabelString, "AND" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 230,
			NULL );
	UxPutContext( pb_and, (char *) UxSearchFormContext );


	/* Creation of pb_or */
	pb_or = XtVaCreateManagedWidget( "pb_or",
			xmPushButtonWidgetClass,
			optionMenu_p3,
			RES_CONVERT( XmNlabelString, "OR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 230,
			NULL );
	UxPutContext( pb_or, (char *) UxSearchFormContext );


	/* Creation of optionMenu_connector */
	optionMenu_connector = XtVaCreateManagedWidget( "optionMenu_connector",
			xmRowColumnWidgetClass,
			SearchForm,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, optionMenu_p3,
			XmNx, 25,
			XmNy, 216,
			XmNwidth, 90,
			XmNheight, 25,
			RES_CONVERT( XmNlabelString, " " ),
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( optionMenu_connector, (char *) UxSearchFormContext );


	/* Creation of label109 */
	label109 = XtVaCreateManagedWidget( "label109",
			xmLabelWidgetClass,
			SearchForm,
			XmNx, 5,
			XmNy, 25,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "TABLE:" ),
			NULL );
	UxPutContext( label109, (char *) UxSearchFormContext );


	/* Creation of TF_search_table_name */
	TF_search_table_name = XtVaCreateManagedWidget( "TF_search_table_name",
			xmTextFieldWidgetClass,
			SearchForm,
			XmNx, 45,
			XmNy, 20,
			XmNheight, 32,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNvalue, "",
			XmNcolumns, 12,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNresizeWidth, FALSE,
			XmNmaxLength, 12,
			NULL );
	UxPutContext( TF_search_table_name, (char *) UxSearchFormContext );


	/* Creation of label110 */
	label110 = XtVaCreateManagedWidget( "label110",
			xmLabelWidgetClass,
			SearchForm,
			XmNx, 44,
			XmNy, 59,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "SEARCH CRITERIA:" ),
			NULL );
	UxPutContext( label110, (char *) UxSearchFormContext );


	/* Creation of pushButton_QuitSearchForm */
	pushButton_QuitSearchForm = XtVaCreateManagedWidget( "pushButton_QuitSearchForm",
			xmPushButtonWidgetClass,
			SearchForm,
			XmNx, 510,
			XmNy, 400,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_QuitSearchForm, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_QuitSearchForm,
		(XtPointer) UxSearchFormContext );

	UxPutContext( pushButton_QuitSearchForm, (char *) UxSearchFormContext );


	/* Creation of pushButton_AddPhrase */
	pushButton_AddPhrase = XtVaCreateManagedWidget( "pushButton_AddPhrase",
			xmPushButtonWidgetClass,
			SearchForm,
			XmNx, 275,
			XmNy, 295,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "ADD\nPHRASE" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_AddPhrase, XmNactivateCallback,
		(XtCallbackProc) cb_add_search_phrase,
		(XtPointer) UxSearchFormContext );

	UxPutContext( pushButton_AddPhrase, (char *) UxSearchFormContext );


	/* Creation of pushButton_ApplySearch */
	pushButton_ApplySearch = XtVaCreateManagedWidget( "pushButton_ApplySearch",
			xmPushButtonWidgetClass,
			SearchForm,
			XmNx, 275,
			XmNy, 400,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "APPLY\nSEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_ApplySearch, XmNactivateCallback,
		(XtCallbackProc) cb_update_search_field,
		(XtPointer) UxSearchFormContext );

	UxPutContext( pushButton_ApplySearch, (char *) UxSearchFormContext );


	/* Creation of textField_matched_records */
	textField_matched_records = XtVaCreateManagedWidget( "textField_matched_records",
			xmTextFieldWidgetClass,
			SearchForm,
			XmNx, 455,
			XmNy, 300,
			XmNheight, 30,
			XmNcolumns, 5,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			XmNresizeWidth, FALSE,
			XmNvalue, "0",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( textField_matched_records, (char *) UxSearchFormContext );


	/* Creation of label111 */
	label111 = XtVaCreateManagedWidget( "label111",
			xmLabelWidgetClass,
			SearchForm,
			XmNx, 400,
			XmNy, 300,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "MATCHING\nRECORDS:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label111, (char *) UxSearchFormContext );


	/* Creation of pushButton_CancelSearch */
	pushButton_CancelSearch = XtVaCreateManagedWidget( "pushButton_CancelSearch",
			xmPushButtonWidgetClass,
			SearchForm,
			XmNx, 395,
			XmNy, 400,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CLEAR\nSEARCH" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( pushButton_CancelSearch, XmNactivateCallback,
		(XtCallbackProc) cb_clear_search_form,
		(XtPointer) UxSearchFormContext );

	UxPutContext( pushButton_CancelSearch, (char *) UxSearchFormContext );

	XtVaSetValues(optionMenu_operator,
			XmNmenuHelpWidget, NULL,
			NULL );

	XtVaSetValues(optionMenu_connector,
			XmNmenuHelpWidget, NULL,
			NULL );


	XtAddCallback( SearchForm, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSearchFormContext);


	return ( SearchForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_SearchForm( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCSearchForm          *UxContext;
	static int		_Uxinit = 0;

	UxSearchFormContext = UxContext =
		(_UxCSearchForm *) UxNewContext( sizeof(_UxCSearchForm), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_SearchForm();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

