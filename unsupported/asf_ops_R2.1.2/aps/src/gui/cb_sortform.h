#ifndef CB_SORTFORM_H
#define CB_SORTFORM_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_sortform.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_sortform.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_sortform.h"

typedef
	struct SORT_INFO
	{
		char *table_name ;
		Widget field_to_update ;
	} SORT_INFO ;

void cb_edit_sort_columns(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_add_sort_column(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_update_sort_order_field(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_clear_sort_order_form(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif	/* CB_SORTFORM_H */
