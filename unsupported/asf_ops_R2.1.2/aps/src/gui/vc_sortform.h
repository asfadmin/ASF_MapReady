
/*******************************************************************************
       vc_sortform.h
       This header file is included by vc_sortform.c

*******************************************************************************/

#ifndef	_VC_SORTFORM_INCLUDED
#define	_VC_SORTFORM_INCLUDED

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
	Widget	UxSortForm;
	Widget	UxscrolledWindowList6;
	Widget	UxscrolledList_column_names;
	Widget	UxscrolledWindowText5;
	Widget	UxscrolledText_SortClause;
	Widget	Uxlabel82;
	Widget	UxtextField_table_name;
	Widget	Uxlabel98;
	Widget	UxpushButton_SortOrder_OK;
	Widget	UxpushButton_SortOrder_Cancel;
	Widget	UxpushButton_SortOrder_Clear;
	Widget	Uxlabel107;
	Widget	Uxrc_SortOrder;
	Widget	UxtoggleButton_ASCENDING;
	Widget	UxtoggleButton_DESCENDING;
	Widget	Uxlabel115;
	swidget	UxUxParent;
} _UxCSortForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCSortForm            *UxSortFormContext;
#define SortForm                UxSortFormContext->UxSortForm
#define scrolledWindowList6     UxSortFormContext->UxscrolledWindowList6
#define scrolledList_column_names UxSortFormContext->UxscrolledList_column_names
#define scrolledWindowText5     UxSortFormContext->UxscrolledWindowText5
#define scrolledText_SortClause UxSortFormContext->UxscrolledText_SortClause
#define label82                 UxSortFormContext->Uxlabel82
#define textField_table_name    UxSortFormContext->UxtextField_table_name
#define label98                 UxSortFormContext->Uxlabel98
#define pushButton_SortOrder_OK UxSortFormContext->UxpushButton_SortOrder_OK
#define pushButton_SortOrder_Cancel UxSortFormContext->UxpushButton_SortOrder_Cancel
#define pushButton_SortOrder_Clear UxSortFormContext->UxpushButton_SortOrder_Clear
#define label107                UxSortFormContext->Uxlabel107
#define rc_SortOrder            UxSortFormContext->Uxrc_SortOrder
#define toggleButton_ASCENDING  UxSortFormContext->UxtoggleButton_ASCENDING
#define toggleButton_DESCENDING UxSortFormContext->UxtoggleButton_DESCENDING
#define label115                UxSortFormContext->Uxlabel115
#define UxParent                UxSortFormContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_SortForm( swidget _UxUxParent );

#endif	/* _VC_SORTFORM_INCLUDED */
