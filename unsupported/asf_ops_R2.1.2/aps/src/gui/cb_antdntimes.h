#ifndef CB_ANTDNTIMES_H
#define CB_ANTDNTIMES_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_antdntimes.h
Description:	
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_antdntimes.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_antdntimes.h"

void cb_update_antdntime_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_delete_antdntime_record(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_antdntimes_editability(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_show_antdntime_records(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_save_antdntime_changes(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_create_new_antdntime_record(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif /*CB_ANTDNTIMES_H*/
