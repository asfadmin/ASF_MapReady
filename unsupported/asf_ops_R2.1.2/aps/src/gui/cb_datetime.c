#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	cb_datetime.c

Description:	

External Functions Defined:
			numeric_only
			isleapyear
			daysinmonth
			time_error
			time_range_error
			format_asf_datetime
			cb_format_float
			cb_filter_text
			cb_adjust_ASF_datetimes
			cb_adjust_revs
			cb_datefield_filter
			cb_timefield_filter
			cb_validate_ASF_datetime
			cb_toggle_cursor
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_datetime.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_datetime.c"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "dapps_defs.h"
#include "cb_datetime.h"
#include "gui_utils.h"
#include "timeconv.h"

static char time_separaters[] = ":." ;
static char date_separaters[] = "/-" ;

char valid_ASF_datetime_chars[] = "0123456789:." ;
char valid_float_chars[]        = "0123456789-." ;
char valid_numeric_chars[]      = "0123456789-" ;

extern void		popup_message() ;

extern Widget	datetime_form ;
extern char		display_string[] ;

int ASF_datetime_err;


/*==============================================================================
Function:		numeric_only
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
   sample code from the V6 Motif Programming Manual Book 
   check zip function on page 516.. Text Modification

==============================================================================*/
void numeric_only(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
	int len ;

	if (cbs->text->ptr == NULL)  /* backspace */
		return ;

	for (len = 0; len < cbs->text->length; len++) 
		if (!isdigit(cbs->text->ptr[len]))
		{
			/* 
			-- not a digit; move all chars down one
			-- and decrement cbs->text->length
			*/
			int i ;

			for (i = len ; (i+1) < cbs->text->length; i++)
				cbs->text->ptr[i] = cbs->text->ptr[i+1] ;
			cbs->text->length-- ;
			len-- ;
		}

	if (cbs->text->length == 0)
		cbs->doit = False ;
}



/*==============================================================================
Function:		cb_format_float
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
   sample code from the V6 Motif Programming Manual Book 
   check zip function on page 516.. Text Modification

==============================================================================*/
void cb_format_float(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
	int len ;

	if (cbs->text->ptr == NULL)  /* backspace */
		return ;

	for (len = 0; len < cbs->text->length; len++) 
		if (!isdigit(cbs->text->ptr[len]) && cbs->text->ptr[len] != '.')
		{
			/* 
			-- not a digit; move all chars down one
			-- and decrement cbs->text->length
			*/
			int i ;

			for (i = len ; (i+1) < cbs->text->length; i++)
				cbs->text->ptr[i] = cbs->text->ptr[i+1] ;
			cbs->text->length-- ;
			len-- ;
		}

	if (cbs->text->length == 0)
		cbs->doit = False ;
}



/*==============================================================================
Function:		cb_filter_text
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	11/23/1994
Notes:		
   sample code from the V6 Motif Programming Manual Book 
   check zip function on page 516.. Text Modification

==============================================================================*/
void cb_filter_text(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
	int len ;
	char *valid_chars ;

	valid_chars = (char *) client_data ;

	/* 
	-- verify the text only when actual typing is done
	-- if no event is associated with this cb, then the update
	-- is a result of a convience function like XmTextSetString
	*/
	if (!cbs->event)  
		return ;

	if (cbs->text->ptr == NULL)  /* backspace */
		return ;

	for (len = 0; len < cbs->text->length; len++) 
		if (!strchr(valid_chars, cbs->text->ptr[len]))
		{
			/* 
			-- not a valid character; move all chars down one
			-- and decrement cbs->text->length
			*/
			int i ;

			for (i = len ; (i+1) < cbs->text->length; i++)
				cbs->text->ptr[i] = cbs->text->ptr[i+1] ;
			cbs->text->length-- ;
			len-- ;
		}

	if (cbs->text->length == 0)
		cbs->doit = False ;
}




/*==============================================================================
Function:		cb_adjust_ASF_datetimes

Description:	Depending on the value of the total days text field widget
                set the start or stop time to the appropiate value

Parameters:		Standard X Callback Parameters

Returns:     	none

Creator:		Ron Green

Creation Date:	11/26/1994

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void cb_adjust_ASF_datetimes(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XtPointer cbs ;
{
	char			*start_time ;
	char			*stop_time ;
	char			*days ;
	double			ndays ;

	char			asf_time[22] ;

	PERIOD_WIDGETS	*ASFperiod ;
 
	ASFperiod = (PERIOD_WIDGETS *) client_data ;

	days = gui_TF_string(text_w) ;
	ndays = atof(days) ;
	XtFree(days) ;

	if (ndays > 0)  
	{
		/* make sure the start time is valid */
		XtCallActionProc(ASFperiod->start, "activate", NULL, NULL, 0) ;
		if (ASF_datetime_err)
			return ;

		start_time = gui_TF_string(ASFperiod->start) ;

		/* get the new stop time */
		if (tc_asf_add_ndays( start_time, ndays, asf_time ) == FALSE)
		{
			(void) sprintf( display_string,
					"Can't add %8.2f days to %s;\n\t%8.2f might be too large\n",
					ndays, start_time, ndays ) ;
			time_error( display_string ) ;
			XtFree(start_time) ;
			return ;
		}

		XmTextSetString(ASFperiod->stop, asf_time) ;
		XtFree(start_time) ;
	}
	else 
	{
		/* days assumed to be negative. make sure stop time is valid */
		XtCallActionProc(ASFperiod->stop, "activate", NULL, NULL, 0) ;
		if (ASF_datetime_err)
			return ;

		stop_time = gui_TF_string(ASFperiod->stop) ;

		/* get the new start time */
		if (tc_asf_add_ndays( stop_time, ndays, asf_time ) == FALSE)
		{
			(void) sprintf( display_string,
					"Can't subtract %8.2f days from %s;\n\t%8.2f might be too small\n",
					ndays, stop_time, ndays ) ;
			time_error( display_string ) ;
			XtFree(stop_time) ;
			return ;
		}

		XmTextSetString(ASFperiod->start, asf_time) ;
		XtFree(stop_time) ;
	}

	/* update total days display */
	(void) sprintf(asf_time, "%8.2f", fabs(ndays)) ;
	XmTextSetString(text_w, asf_time) ;

	return ;
}



/*==============================================================================
Function:		cb_adjust_revs

Description:	Depending on the value of the widget set the start or
            	stop rev to the appropiate value

Parameters:		Standard X Callback Parameters

Returns:     	none

Creator:		Ron Green

Creation Date:	11/28/1994

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void cb_adjust_revs(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XtPointer cbs ;
{
	char *start_rev ;
	char *stop_rev ;
	char *revs ;
	int nrevs ;

	int start ;
	int stop ;

	PERIOD_WIDGETS *ASFperiod ;
 
	ASFperiod = (PERIOD_WIDGETS *) client_data ;

	revs = gui_TF_string(text_w) ;
	nrevs = atoi(revs) ;
	XtFree(revs) ;

	if (nrevs > 0)  
	{
		/* get the current start rev */
		start_rev = gui_TF_string(ASFperiod->start) ;

		/* convert to ephemeris time and add the # days to the start time */
		start = atoi(start_rev) ;
		stop = start + nrevs ;

		(void) sprintf(display_string, "%d", stop) ;
		XmTextSetString(ASFperiod->stop, display_string) ;

		XtFree(start_rev) ;
	}
	else 
	{
		/* get the current stop rev */
		stop_rev = gui_TF_string(ASFperiod->stop) ;
		stop = atoi(stop_rev) ;

		start = stop + nrevs ;

		(void) sprintf(display_string, "%d", start) ;
		XmTextSetString(ASFperiod->start, display_string) ;

		XtFree(stop_rev) ;
	}

	/* update total days display */
	nrevs = stop - start ;
	(void) sprintf(display_string, "%d", nrevs) ;
	XmTextSetString(text_w, display_string) ;
}


void cb_datefield_filter(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
	int len ;

	if (cbs->text->ptr == NULL)  /* backspace */
		return ;

	for (len = 0; len < cbs->text->length; len++) 
		if ((!isdigit(cbs->text->ptr[len]))
		&& (cbs->text->ptr[len] != '-')
		&& (cbs->text->ptr[len] != '/'))
		{
			/* 
			-- not a digit; move all chars down one
			-- and decrement cbs->text->length
			*/
			int i ;

			for (i = len ; (i+1) < cbs->text->length; i++)
				cbs->text->ptr[i] = cbs->text->ptr[i+1] ;
			cbs->text->length-- ;
			len-- ;
		}

	if (cbs->text->length == 0)
		cbs->doit = False ;
}


void cb_timefield_filter(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
	int len ;

	if (cbs->text->ptr == NULL)  /* backspace */
		return ;

	for (len = 0; len < cbs->text->length; len++) 
		if ((!isdigit(cbs->text->ptr[len]))
		&& (cbs->text->ptr[len] != ':')
		&& (cbs->text->ptr[len] != '.'))
		{
			/* 
			-- not a digit; move all chars down one
			-- and decrement cbs->text->length
			*/
			int i ;

			for (i = len ; (i+1) < cbs->text->length; i++)
				cbs->text->ptr[i] = cbs->text->ptr[i+1] ;
			cbs->text->length-- ;
			len-- ;
		}

	if (cbs->text->length == 0)
		cbs->doit = False ;
}


/*==============================================================================
Function:		time_error
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/

void time_error(msg)
	char *msg ;
	
{
	popup_message(XmDIALOG_ERROR, "APS:DateTime", msg, XtGrabNone) ;
}



int isleapyear(year)
	int year ;
{
	/* determine if year is a leap year */

	int leap = 0 ;
	if ((year % 4) == 0) leap = 1 ;
	if ((year % 100) == 0) leap = 0 ;
	if ((year % 400) == 0) leap = 1 ;


	return(leap) ;
}


int daysinmonth(year, month)
   int year, month ;
{
	int days ;
	switch (month)
	{
		case 1 : case 3 :
		case 5 : case 7 :
		case 8 : case 10 :
		case 12 :
			days = 31 ;
			break ;
		case 4 : case 6 :
		case 9 : case 11 :
			days = 30 ;
			break ;
		case 2 :
			days = 28 + isleapyear(year) ;
		break ;
		default :
			days = 0 ;
			break ;
	}
	return(days) ;
}


char *format_asf_datetime(char *datestr, char *timestr)
{
	static char default_msec[] = "6" ;

	char *ptr ;
	char *year, *month, *day ;
	char *hour, *min, *sec, *msec ;

	int i ;
	int imonth ;
	int doy ;

	hour = strtok(timestr, time_separaters) ;
	min = strtok(NULL, time_separaters) ;
	sec = strtok(NULL, time_separaters) ;
	msec = strtok(NULL, time_separaters) ;

	month = strtok(datestr, date_separaters) ;
	day = strtok(NULL, date_separaters) ;
	year = strtok(NULL, date_separaters) ;

	if ((!month) || (!day) || (!year)
	|| (!hour) || (!min) || (!sec))
		return(NULL) ;

	if (!msec)
		msec = default_msec ;

	/* 
	-- convert the day of the month to the day of the year by
	-- adding all the days of the previous month plus the number
	-- of days of the current month
	*/ 
	doy = 0 ;
	imonth = atoi(month) ;
	for (i = 1 ; i < imonth ; i++)
		doy = doy + daysinmonth(atoi(year), i) ;
	doy = doy + atoi(day) ;

	/* 
	-- get some space to hold the date time string 
	-- the format of the ASF DateTime String is 
	-- 1994:201:12:00:00.000
	*/
	ptr = (char *) malloc( ASF_TIME_STR_LENGTH + 1 ) ;  

	(void) sprintf(ptr, "%4s:%03d:%02d:%02d:%02d.%03d", 
		year, doy, atoi(hour), atoi(min), atoi(sec), atoi(msec)) ;
	return(ptr) ;
}



/*==============================================================================
Function:		cb_validate_ASF_datetime

Description:	

Parameters:		Standard X Callback Parameters

Returns:     	none

Creator:		Ron Green

Creation Date:	11/23/1994

Notes:		
				10/18/95	Teresa McKillop		check the ASF time str length
==============================================================================*/
/* ARGSUSED2 */
void
cb_validate_ASF_datetime(
	Widget text_w, XtPointer client_data, XtPointer cbs)
{
	char	*ASF_datetime ;
	int		ValidateRetStatus ;

	ASF_datetime_err = False ;

	/* get the contents of the field for processing */
	ASF_datetime = XmTextGetString(text_w) ;

	ValidateRetStatus = tc_validate_asf_datetime( ASF_datetime ) ;

	*display_string = STREND ;
	switch( ValidateRetStatus )
	{
		case TC_VALID_YEAR_ERR :
			(void) sprintf( display_string, "%s:\n\nIncorrect year",
				client_data ) ;
			break ;
		case TC_VALID_DOY_ERR :
			(void) sprintf( display_string, 
				"%s:\n\nIncorrect day-of-year", client_data ) ;
			break ;
		case TC_VALID_HOUR_ERR :
			(void) sprintf( display_string, 
				"%s:\n\nInvalid hour value\nRange is 00-23", client_data ) ;
			break ;
		case TC_VALID_MIN_ERR :
			(void) sprintf( display_string, 
				"%s:\n\nInvalid minutes value\nRange is 00-59", client_data ) ;
			break ;
		case TC_VALID_SEC_ERR :
			(void) sprintf( display_string, 
				"%s:\n\nInvalid seconds value\nRange is 00-59", client_data ) ;
			break ;
		case TC_VALID_TIME_ERR :
			(void) sprintf( display_string,
				"%s:\n\nInvalid date format", client_data ) ;
			break ;
		default :
			break ;
	}

	if (strlen( display_string ))
	{
		time_error( display_string ) ;
		ASF_datetime_err = True ;
	}	

	XtFree(ASF_datetime) ;

	return ;
}


int time_range_error(start_time, stop_time)
	char *start_time ;
	char *stop_time ;
{

	if (strcmp(start_time, stop_time) >= 0)
	{
		(void) sprintf(display_string,
			"Start Time\n   %s\nmust come before Stop Time\n   %s\nRespecify Start/Stop Times\n", start_time, stop_time) ;
		time_error(display_string) ;
		return(TRUE) ;
	}
	return(FALSE) ;
}



/* ARGSUSED2 */
void cb_toggle_cursor(text_w, client_data, cbs)
	Widget text_w ;
	XtPointer client_data ;
	XtPointer *cbs ;
{
	char *text ;

	XtVaSetValues(text_w,
		XmNcursorPositionVisible, client_data,
		NULL) ;

	/* refresh text in the window */
	if ( XmIsText( text_w ) )
	{
		text = XmTextGetString(text_w) ;
		XmTextSetString(text_w, text) ;
	}
	else
	{
		text = XmTextFieldGetString(text_w) ;
		XmTextFieldSetString(text_w, text) ;
	}
	XtFree(text) ;
}
