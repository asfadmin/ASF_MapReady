#ifndef CB_RGSDNTIMES_H
#define CB_RGSDNTIMES_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_rgsdntimes.h
Description:	
Creator:	unknown
Notes:		5/30/96: file renamed from cb_asfdntimes.h to cb_rgsdntimes.h.
			some names still have "asf" instead of "rgs".
==============================================================================*/
#pragma ident	"@(#)cb_rgsdntimes.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_rgsdntimes.h"

void cb_show_asfdntime_records(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_update_asfdntime_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_delete_asfdntime_record(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_asfdntimes_editability(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_save_asfdntime_changes(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_create_new_down_time(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_popup_asfdntime_form(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_popdown_asfdntime_form(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif	/* CB_RGSDNTIMES_H */
