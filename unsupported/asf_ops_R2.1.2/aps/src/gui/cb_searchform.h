#ifndef CB_SEARCHFORM_H
#define CB_SEARCHFORM_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_searchform.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_searchform.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_searchform.h"

typedef
	struct _SEARCH_INFO
	{
		char *table_name ;
		Widget field_to_update ;
	} SEARCH_INFO ;

void cb_edit_search_columns(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_add_search_column(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_add_search_phrase(
    Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_update_search_field(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_clear_search_form(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif	/* CB_SEARCHFORM_H */
