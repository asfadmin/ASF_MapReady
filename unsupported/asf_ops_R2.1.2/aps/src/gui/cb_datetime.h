#ifndef CB_DATETIME_H
#define CB_DATETIME_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	cb_datetime.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)cb_datetime.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_datetime.h"

void numeric_only(
	Widget text_w, XtPointer client_data, XmTextVerifyCallbackStruct *cbs );

void cb_format_float(
	Widget text_w, XtPointer client_data, XmTextVerifyCallbackStruct *cbs );

void cb_filter_text(
    Widget text_w, XtPointer client_data , XmTextVerifyCallbackStruct *cbs) ;

void cb_adjust_ASF_datetimes(
	Widget text_w, XtPointer client_data, XtPointer cbs );
 
void cb_adjust_revs(
    Widget text_w, XtPointer client_data, XtPointer cbs) ;

void cb_datefield_filter(
	Widget text_w, XtPointer client_data, XmTextVerifyCallbackStruct *cbs );

void cb_timefield_filter(
	Widget text_w, XtPointer client_data, XmTextVerifyCallbackStruct *cbs );

void time_error( char *msg );

int isleapyear( int year );

int daysinmonth( int year, int month );

char *format_asf_datetime( char *datestr, char *timestr );

void cb_validate_ASF_datetime(
	Widget text_w, XtPointer client_data, XtPointer cbs );

int time_range_error( char *start_time, char *stop_time );

void cb_toggle_cursor(
	Widget text_w, XtPointer client_data, XtPointer *cbs );


typedef 
	struct _PERIOD_WIDGETS
	{
		Widget start ;
		Widget stop ;
	} PERIOD_WIDGETS ;

typedef
	struct _MIN_MAX
	{
		int min ;
		int max ;
		int min_digits ;
	} MIN_MAX ;

typedef
	struct _VERIFY_TIME
	{
		char *min_time ;
		char *max_time ;
		Widget *update_field ;
	} VERIFY_TIME ;

				
extern char valid_ASF_datetime_chars[] ;
extern char valid_float_chars[]        ;
extern char valid_numeric_chars[]      ;
extern  ASF_datetime_err ;

#endif	/* CB_DATETIME_H */
