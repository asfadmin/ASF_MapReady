
/*******************************************************************************
	vc_sortform.c

       Associated Header file: vc_sortform.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/List.h>
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
#pragma ident   "@(#)vc_sortform.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_sortform.c"

#include "cb_sortform.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_sortform.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_SortOrder_Cancel(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCSortForm            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxSortFormContext;
	UxSortFormContext = UxContext =
			(_UxCSortForm *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(XtParent(UxWidget))) ;
	}
	UxSortFormContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_SortForm()
{
	Widget		_UxParent;


	/* Creation of SortForm */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "SortForm_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 408,
			XmNy, 95,
			XmNwidth, 334,
			XmNheight, 670,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "SortForm",
			XmNiconName, "SortForm",
			NULL );

	}

	SortForm = XtVaCreateManagedWidget( "SortForm",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 334,
			XmNheight, 670,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "Sort Order" ),
			NULL );
	UxPutContext( SortForm, (char *) UxSortFormContext );
	UxPutClassCode( SortForm, _UxIfClassId );


	/* Creation of scrolledWindowList6 */
	scrolledWindowList6 = XtVaCreateManagedWidget( "scrolledWindowList6",
			xmScrolledWindowWidgetClass,
			SortForm,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 35,
			XmNy, 315,
			XmNwidth, 245,
			XmNheight, 255,
			XmNresizable, FALSE,
			NULL );
	UxPutContext( scrolledWindowList6, (char *) UxSortFormContext );


	/* Creation of scrolledList_column_names */
	scrolledList_column_names = XtVaCreateManagedWidget( "scrolledList_column_names",
			xmListWidgetClass,
			scrolledWindowList6,
			XmNwidth, 245,
			XmNvisibleItemCount, 10,
			XmNlistSizePolicy, XmVARIABLE,
			XmNheight, 245,
			NULL );
	XtAddCallback( scrolledList_column_names, XmNdefaultActionCallback,
		(XtCallbackProc) cb_add_sort_column,
		(XtPointer) UxSortFormContext );
	XtAddCallback( scrolledList_column_names, XmNbrowseSelectionCallback,
		(XtCallbackProc) cb_add_sort_column,
		(XtPointer) UxSortFormContext );

	UxPutContext( scrolledList_column_names, (char *) UxSortFormContext );


	/* Creation of scrolledWindowText5 */
	scrolledWindowText5 = XtVaCreateManagedWidget( "scrolledWindowText5",
			xmScrolledWindowWidgetClass,
			SortForm,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 35,
			XmNy, 85,
			XmNwidth, 270,
			XmNheight, 130,
			NULL );
	UxPutContext( scrolledWindowText5, (char *) UxSortFormContext );


	/* Creation of scrolledText_SortClause */
	scrolledText_SortClause = XtVaCreateManagedWidget( "scrolledText_SortClause",
			xmTextWidgetClass,
			scrolledWindowText5,
			XmNwidth, 265,
			XmNheight, 115,
			XmNeditable, FALSE,
			XmNwordWrap, TRUE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNscrollHorizontal, FALSE,
			NULL );
	UxPutContext( scrolledText_SortClause, (char *) UxSortFormContext );


	/* Creation of label82 */
	label82 = XtVaCreateManagedWidget( "label82",
			xmLabelWidgetClass,
			SortForm,
			XmNx, 10,
			XmNy, 25,
			XmNwidth, 60,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "TABLE:" ),
			NULL );
	UxPutContext( label82, (char *) UxSortFormContext );


	/* Creation of textField_table_name */
	textField_table_name = XtVaCreateManagedWidget( "textField_table_name",
			xmTextFieldWidgetClass,
			SortForm,
			XmNx, 70,
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
	UxPutContext( textField_table_name, (char *) UxSortFormContext );


	/* Creation of label98 */
	label98 = XtVaCreateManagedWidget( "label98",
			xmLabelWidgetClass,
			SortForm,
			XmNx, 20,
			XmNy, 290,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "COLUMN NAMES:" ),
			NULL );
	UxPutContext( label98, (char *) UxSortFormContext );


	/* Creation of pushButton_SortOrder_OK */
	pushButton_SortOrder_OK = XtVaCreateManagedWidget( "pushButton_SortOrder_OK",
			xmPushButtonWidgetClass,
			SortForm,
			XmNx, 20,
			XmNy, 600,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_SortOrder_OK, XmNactivateCallback,
		(XtCallbackProc) cb_update_sort_order_field,
		(XtPointer) UxSortFormContext );

	UxPutContext( pushButton_SortOrder_OK, (char *) UxSortFormContext );


	/* Creation of pushButton_SortOrder_Cancel */
	pushButton_SortOrder_Cancel = XtVaCreateManagedWidget( "pushButton_SortOrder_Cancel",
			xmPushButtonWidgetClass,
			SortForm,
			XmNx, 230,
			XmNy, 600,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_SortOrder_Cancel, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_SortOrder_Cancel,
		(XtPointer) UxSortFormContext );

	UxPutContext( pushButton_SortOrder_Cancel, (char *) UxSortFormContext );


	/* Creation of pushButton_SortOrder_Clear */
	pushButton_SortOrder_Clear = XtVaCreateManagedWidget( "pushButton_SortOrder_Clear",
			xmPushButtonWidgetClass,
			SortForm,
			XmNx, 125,
			XmNy, 600,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CLEAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_SortOrder_Clear, XmNactivateCallback,
		(XtCallbackProc) cb_clear_sort_order_form,
		(XtPointer) UxSortFormContext );

	UxPutContext( pushButton_SortOrder_Clear, (char *) UxSortFormContext );


	/* Creation of label107 */
	label107 = XtVaCreateManagedWidget( "label107",
			xmLabelWidgetClass,
			SortForm,
			XmNx, 20,
			XmNy, 60,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "ORDER BY" ),
			NULL );
	UxPutContext( label107, (char *) UxSortFormContext );


	/* Creation of rc_SortOrder */
	rc_SortOrder = XtVaCreateManagedWidget( "rc_SortOrder",
			xmRowColumnWidgetClass,
			SortForm,
			XmNwidth, 137,
			XmNheight, 20,
			XmNx, 117,
			XmNy, 236,
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			RES_CONVERT( XmNlabelString, "" ),
			XmNnumColumns, 1,
			XmNpacking, XmPACK_TIGHT,
			XmNwhichButton, 1,
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( rc_SortOrder, (char *) UxSortFormContext );


	/* Creation of toggleButton_ASCENDING */
	toggleButton_ASCENDING = XtVaCreateManagedWidget( "toggleButton_ASCENDING",
			xmToggleButtonWidgetClass,
			rc_SortOrder,
			XmNx, 3,
			XmNy, 531,
			XmNwidth, 67,
			XmNheight, 12,
			RES_CONVERT( XmNlabelString, "ASCENDING" ),
			XmNset, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	UxPutContext( toggleButton_ASCENDING, (char *) UxSortFormContext );


	/* Creation of toggleButton_DESCENDING */
	toggleButton_DESCENDING = XtVaCreateManagedWidget( "toggleButton_DESCENDING",
			xmToggleButtonWidgetClass,
			rc_SortOrder,
			XmNx, 92,
			XmNy, 531,
			XmNwidth, 61,
			XmNheight, 12,
			RES_CONVERT( XmNlabelString, "DESCENDING" ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1" ),
			XmNindicatorSize, 20,
			NULL );
	UxPutContext( toggleButton_DESCENDING, (char *) UxSortFormContext );


	/* Creation of label115 */
	label115 = XtVaCreateManagedWidget( "label115",
			xmLabelWidgetClass,
			SortForm,
			XmNx, 32,
			XmNy, 246,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "COLUMN ORDER:" ),
			NULL );
	UxPutContext( label115, (char *) UxSortFormContext );


	XtAddCallback( SortForm, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSortFormContext);


	return ( SortForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_SortForm( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCSortForm            *UxContext;
	static int		_Uxinit = 0;

	UxSortFormContext = UxContext =
		(_UxCSortForm *) UxNewContext( sizeof(_UxCSortForm), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_SortForm();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

