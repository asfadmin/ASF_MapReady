#ifndef CB_CDTAKEOPPS_H
#define CB_CDTAKEOPPS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_cdtakeopps.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_cdtakeopps.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cdtakeopps.h"

void cb_display_hypo_sites(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_display_dar_sites(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_update_cdtakeopps_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_do_create_dtk_opps(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_show_dar_relations(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_add_HypoSite(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_delete_HypoSite(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_edit_new_HypoSite(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_cancel_edit_new_HypoSite(
	Widget widget, XtPointer client_data, XtPointer cbs) ;


#endif	/* CB_CDTAKEOPPS_H */
