#ifndef CB_DARMANAGER_H
#define CB_DARMANAGER_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_darmanager.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_darmanager.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_darmanager.h"

#define SAVE_DAR_FILE_ERROR				-1
#define SAVE_DAR_METAFILE_ERROR			-2
#define SAVE_DAR_OK						2

#define SAVE_DARS_ALL					3
#define SAVE_DARS_CURRENT				4	
#define SAVE_DARS_SELECTED				5	

#define LOAD_DAR_FILE_ERROR				1
#define LOAD_DAR_CANCEL					2
#define LOAD_DAR_OK						3

#define LOAD_DARS_REPLACE				1
#define LOAD_DARS_APPEND				2

#define PRINT_DARS_SELECTED				1
#define PRINT_DARS_CURRENT				2
#define PRINT_DARS_ALL					3
#define PRINT_DARS_SELECTED_TO_FILE		4
#define PRINT_DARS_CURRENT_TO_FILE		5
#define PRINT_DARS_ALL_TO_FILE			6

#define XFER_FILE_FROM_ACS				0
#define XFER_FILE_TO_ACS				1


void cb_quit_darmanager(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_show_dar_records(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_update_darmanager_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_set_darmanager_editability(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_save_dar_changes(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_delete_dar_record(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_save_dars_to_file_cb(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_load_dars_from_ims(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_print_dars(Widget widget, 
	XtPointer client_data, XtPointer cbs) ;

void cb_set_print_dars_to_file_cb(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif	/* CB_DARMANAGER_H */
