#ifndef CB_CNOMCOV_H
#define CB_CNOMCOV_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_cnomcov.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_cnomcov.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cnomcov.h"

Widget set_coverage_type( char *satcode ) ;

void cb_show_coverage_relations(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_set_coverage_type(
    Widget widget, XtPointer client_data, XmRowColumnCallbackStruct *cbs) ;

void cb_update_coverage_filename(
    Widget widget, XtPointer client_data, XmRowColumnCallbackStruct *cbs) ;

void cb_update_cnomcov_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs) ;

void cb_adjust_phase_stoptime(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

void cb_check_phase_time_range(
	Widget widget, XtPointer client_data, XmTextVerifyCallbackStruct *cbs) ;

void cb_do_create_coverage(
	Widget widget, XtPointer client_data, XtPointer cbs) ;

#endif	/* CB_CNOMCOV_H */
