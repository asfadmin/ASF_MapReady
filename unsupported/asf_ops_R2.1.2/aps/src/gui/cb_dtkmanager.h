#ifndef CB_DTKMANAGER_H
#define CB_DTKMANAGER_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_dtkmanager.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_dtkmanager.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_dtkmanager.h"

#define SAVE_DTK_FILE_ERROR  -1
#define SAVE_DTK_OK	         2

#define SAVE_DTKS_ALL        3
#define SAVE_DTKS_CURRENT    4	
#define SAVE_DTKS_SELECTED   5	

#define LOAD_DTK_FILE_ERROR  1
#define LOAD_DTK_CANCEL      2
#define LOAD_DTK_OK          3

#define LOAD_DTKS_REPLACE     1
#define LOAD_DTKS_APPEND      2

#define PRINT_DTKS_SELECTED   1
#define PRINT_DTKS_CURRENT    2
#define PRINT_DTKS_ALL        3
#define PRINT_DTKS_SELECTED_TO_FILE     4
#define PRINT_DTKS_CURRENT_TO_FILE      5
#define PRINT_DTKS_ALL_TO_FILE          6

#define DTK_RESET   0
#define DTK_EDIT    1
#define DTK_CREATE  2

void cb_init_dtk_search_clause(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_show_dtk_records(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_clear_dtkmanager_form(
	Widget widget, XtPointer client_data_UNUSED, XmListCallbackStruct *cbs ) ;

void cb_update_dtkmanager_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_delete_dtk_record(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_dtkmanager_editability(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_get_cvrg_points(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_save_dtk_changes(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_create_dtk(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

int load_dtk_records(llist *dtks, int load_type) ;

void cb_print_dtks(Widget widget,
	XtPointer client_data, XtPointer cbs) ;

void cb_rtobservation_toggle( Widget widget, XtPointer client_data_UNUSED, 
	XmListCallbackStruct *cbs) ;

void cb_set_print_dtks_to_file_cb(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_dtk_status_menus(
	Widget widget_UNUSED, XtPointer client_data_UNUSED,
	XmRowColumnCallbackStruct *cbs ) ;

#endif	/* CB_DTKMANAGER_H */
