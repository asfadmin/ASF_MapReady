static char *sccs = "@(#)ims_timeConv.c	5.6  04/16/97";
/******************************************************************************
**
** File:        ims_timeConv.c
**
** Function:    Functions for parsing and formating ASCII date/time strings.
**
** Author:      J. Rector
**
** Date:        6/29/89
**
** Modified:    12/3/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files string.h, memory.h, and stdlib.h.
**              Replaced the include files sybfront.h and sybdb.h with
**              cdb_dbms.h.
**
**              5/8/95 - S. Hardman - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              5/12/95 - D. Crichton - R1B
**              Add new ASF supported date/time formats.  Update 
**              message facility calls to ASF standards.
**
**              6/23/95 - D. Crichton - R1B
**              Added new functions, ims_getCurrentDate() to return the
**              current date to the caller and ims_numericDateToCSA().
**
**              7/10/95 - D. Crichton - R1B
**              Add new function to format date/time in the TTDL format.
**
**              8/30/95 - J. Wang - R1B
**              Add new function to format date/time in the V0 format.
**
**              9/14/95 - D. Crichton - R1B
**              Add new function to calculate date differences.
**
**              10/19/95 - D. Crichton - R1B'
**              Add new function to format date/time in the ESAA format.
**
**              10/20/95 - D. Pass - R1B'
**              Add new function to format date/time in the GMT format.
**
**              02/06/96 - D. Crichton - R1B'
**              Add new function to convert msecs, days to ASCII string.
**
**              4/3/97 - T. McKillop - R2.1
**              modified ims_timeToNumericDate() to check for valid separators.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_timeConv.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_timeConv.h.
** They are listed here for reference.
**
**	int ims_timeToNumericDate (IMS_MSG_STRUCT *, char *,
**		IMS_NUMERIC_DATE *);
**	void ims_numericDateToDBMSA (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToIMSA  (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToASFA  (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToACSA  (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToCSAA  (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToTTDLA (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToV0A   (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToESAA  (IMS_NUMERIC_DATE *, char *);
**	void ims_numericDateToESAI  (IMS_NUMERIC_DATE *, int *, int *);
**	void ims_numericDatetoGMTA  (IMS_NUMERIC_DATE *, char *);
**	int ims_getCurrentDate (IMS_MSG_STRUCT *, IMS_NUMERIC_DATE *);
**	int ims_numericDateDiff (IMS_MSG_STRUCT *, char *, char *, int *, int *);
**	int ims_numericDateDiff2 (IMS_MSG_STRUCT *, char *, char *, int *, int *,
**	  int *);
*/

/*
** A table of the cummulative days from the start of the year to the
** beginning of each month in the year.  Row one is for non-leap years and
** row two is for leap years.  Calculations are based on the following
** calendar
**                 J  F  M  A  M  J  J  A  S  O  N  D
**                 0  1  2  3  4  5  6  7  8  9 10 11
**                 - -- -- -- -- -- -- -- -- -- -- --
** Non-leap year - 0 31 28 31 30 31 30 31 31 30 31 30 [31]
** Leap year -     0 31 29 31 30 31 30 31 31 30 31 30 [31]
*/
static unsigned int monthTbl[2][13] =
{
	{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
	{ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366},
};

/*
** An array of months.
*/
static char *months[] = 
{
	"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
	"JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
};

/*
** A string of legal separators in a year-month-day date.
*/
#define YMD_SEPARATORS				"/-: ."	/* note: includes "space" */

/******************************************************************************
**
** ims_timeToNumericDate ()
**
** Convert date/time from ASCII format to number unsigned int format,
** where each date part is held in the structure of type
** IMS_NUMERIC_DATE.
**
** The year must be supplied, but the string can stop
** at any point after that.  The assumption is that all undefined components
** of the date are 1 and all undefined components of time are 0.
**
** Must be able to parse and accept the following formats along with their
** month-day options (01-01 instead of 001). 
**
**  1995-001T12:30:45.500[Z]
**  1995/001T12:30:45.500[Z]
**  1995:001:12:30:45.500[Z]
**  1995 001:12:30:45.500[Z]
**
******************************************************************************/

int ims_timeToNumericDate (
	IMS_MSG_STRUCT *msgDesc,
	char *inDateString,
	IMS_NUMERIC_DATE *tbl)
{
	register char *p;
	register char *q;
	char dateString[32];
	char *terminator;
	register int delta;
	int dateLen;                /* Length of the input date string. */
	enum ims_timeTypes type;    /* Type of time specified. */
	unsigned int dayOfYear;     /* Temporary storage for day-of-year. */
	unsigned int maxDayOfMonth; /* Max day value for given month. */
	int i;
	int leap;                   /* Is this a leap year? */

	(void) strncpy (dateString, inDateString, sizeof (dateString)-1);

	/*
	** Find the length of the string, and its terminator.
	*/
	if ((terminator = strchr (dateString, 'Z')) == (char *) NULL)
	{
		dateLen = strlen (dateString);
		terminator = dateString + dateLen;
	}
	else
	{
		dateLen = terminator - dateString;
	}

	/*
	** Now, determine the type, and parse the string.
	*/
	switch (dateLen)
	{
	case 4:
		/* Year only. */
		type = IMS_YMD;
		break;
	case 7:
		/* Year and month only. */
		type = IMS_YMD;
		break;
	case 8:
		/* Year and day-of-year only. */
		type = IMS_DOY;
		break;
	case 10:
		/* Year, month and day only. */
		type = IMS_YMD;
		break;
	default:
		/* All other cases less than 10 characters in length. */
		if (dateLen < 10)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"Invalid date format.");
			return (IMS_ERROR);
		}

		/*
		** If the length is greater than 10 characters 
		** check date/time delimeter location to determine type.
		*/
		if ((*(dateString + 8) == 'T') ||
			(*(dateString + 8) == ':'))
		{
			/* Year and day-of-year with time. */
			type = IMS_DOY;
			break;
		}
		else if ((*(dateString + 10) == 'T') ||
				 (*(dateString + 10) == ':'))
		{
			/*  Year, month and day with time. */
			type = IMS_YMD;
			break;
		}
		else
		{
			/* Invalid or missing delimeter. */
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Invalid or missing delimiter between date and time representations.");
			return (IMS_ERROR);
		}
	}
	
	/*
	** Initialize the numeric time structure. Note: If no month is given,
	** then assume the first month. Also Note: we don't initialize the year.
	** If no year is given, then we have no date to work with and that is an
	** error.
	*/
	tbl->doy = 1;
	tbl->month = 1;
	tbl->day = 1;
	tbl->hours = 0;
	tbl->minutes = 0;
	tbl->seconds = 0;
	tbl->msecs = 0;

	/*
	** Parse the string and find the beginning and the ending of the year
	** part of the string.  Convert that to an unsigned int.
	** If we encounter a terminator or if the string is not 4 characters
	** in length then we have an error.
	*/
	q = p = dateString;
	while (*p >= '0' && *p <= '9' && p < terminator)
	{
		p++;
	}
	delta = p - q;

	/*
	** The year must be 4 digits in length.
	*/
	if (delta != 4)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The year value must be 4 digits in length.");
		return (IMS_ERROR);	
	}

	if (p != terminator && strchr( YMD_SEPARATORS, *p ) == NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The character following the year is an invalid separator.");
		return (IMS_ERROR);	
	}
			
	*p = '\0';
	tbl->year = (unsigned) atoi (q);

	/*
	** If this is it, we are finished and the default values will fill
	** out the rest of the date/time fields.
	*/
	if (p == terminator)
	{
		return (IMS_OK);
	}

	/*
	** Now, check the type and either get the month and day, or convert
	** the number of days in a year to month and day.  Of course, if we
	** reach the terminator at any time, then we're done and we leave the
	** defaults in our structure alone.
	*/
	if (type == IMS_YMD)
	{
		/*
		** Converts string month to unsigned int value.
		** Increment p before starting to skip the demarcation character
		*/
		q = ++p;
		while (*p >= '0' && *p <= '9' && p < terminator)
		{
			p++;
		}

		delta = p - q;

		/*
		** The month must be 2 digits in length.
		*/
		if (delta != 2)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The month value must be 2 digits in length.");
			return (IMS_ERROR);
		}

		if (p != terminator && strchr( YMD_SEPARATORS, *p ) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The character following the month is an invalid separator.");
			return (IMS_ERROR);	
		}

		*p = '\0';
		tbl->month = (unsigned) atoi (q);

		/*
		** Make sure the month is between 1 and 12, inclusive.
		*/
		if ((tbl->month < 1) || (tbl->month > 12))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The month value must be from 01 to 12.");
			return (IMS_ERROR);
		}

		if (IMS_LEAP_YEAR (tbl->year) == IMS_TRUE)
		{
			leap = 1;
			tbl->doy = monthTbl[leap][tbl->month - 1] + 1;
		}
		else
		{
			leap = 0;
			tbl->doy = monthTbl[leap][tbl->month - 1] + 1;
		}

		/*
		** If this is it, we are finished and the default values will fill
		** out the rest of the date/time fields.
		*/
		if (p == terminator)
		{
			return (IMS_OK);
		}

		/*
		** Convert string day to unsigned int value.
		*/
		q = ++p;
		while (*p >= '0' && *p <= '9' && p < terminator)
		{
			p++;
		}

		delta = p - q;

		/*
		** The day must be 2 digits in length.
		*/
		if (delta != 2)
		{
			
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The day value must be 2 digits in length.");
			return (IMS_ERROR);
		}

		/*
		** The separator between the date and the time was already checked,
		** so no need to do the check here.
		*/
		*p = '\0';
		tbl->day = (unsigned) atoi (q);
		tbl->doy += tbl->day - 1;

		/*
		** Make sure the day is in the range of days for the given month.
		*/
		maxDayOfMonth = monthTbl[leap][tbl->month] - monthTbl[leap][tbl->month - 1];
		if ((tbl->day < 0) || (tbl->day > maxDayOfMonth))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The day value must be from 01 to %d, for the given month.",
				maxDayOfMonth);
			return (IMS_ERROR);
		}

		/*
		** If this is it, we are finished and the default values will fill
		** out the rest of the date/time fields.
		*/
		if (p == terminator)
		{
			return (IMS_OK);
		}
	}
	else
	{
		/*
		** Type is IMS_DOY, so we get the dayOfTheYear, and figure out
		** the month and the day of the month from it.
		**
		** First, convert string day to unsigned int value.  In this
		** case, the day will be 3-characters long.
		*/
		q = ++p;
		while (*p >= '0' && *p <= '9' && p < terminator)
		{
			p++;
		}

		delta = p - q;

		/*
		** The day-of-year must be 3 digits in length.
		*/
		if (delta != 3)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The day-of-year value must be 3 digits in length.");
			return (IMS_ERROR);
		}

		/*
		** The separator between the date and the time was already checked,
		** so no need to do the check here.
		*/
		*p = '\0';
		dayOfYear = (unsigned) atoi (q);
		tbl->doy = dayOfYear;

		/*
		** See if it is a leap year and check the day-of-year range.
		*/
		if (IMS_LEAP_YEAR (tbl->year) == IMS_TRUE)
		{
			leap = 1;
			if ((tbl->doy < 1) || (tbl->doy > 366))
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The day-of-year value must be from 001 to 366, for the given year.");
				return (IMS_ERROR);
			}
		}
		else
		{
			leap = 0;
			if ((tbl->doy < 1) || (tbl->doy > 365))
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The day-of-year value must be from 001 to 365, for the given year.");
				return (IMS_ERROR);
			}
		}

		/*
		** Determine the month and day based on the day-of-year.
		*/
		for (i=0; i<13; i++)
		{
			if (monthTbl[leap] [i] >= dayOfYear)
			{
				break;
			}
		}

		if (i >= 13)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not determine month and day from day-of-year given.");
			return (IMS_FATAL);
		}

		tbl->day = dayOfYear - monthTbl[leap] [i - 1];
		tbl->month = i;

		/*
		** If this is it, we are finished and the default values will fill
		** out the rest of the date/time fields.
		*/
		if (p == terminator)
		{
			return (IMS_OK);
		}

	}

	/*
	** Convert string hour to unsigned int value.
	*/
	q = ++p;
	while (*p >= '0' && *p <= '9' && p < terminator)
	{
		p++;
	}

	delta = p - q;

	/*
	** The hour must be 2 digits in length.
	*/
	if (delta != 2)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The hour value must be 2 digits in length.");
		return (IMS_ERROR);
	}

	if (*p != ':' && p != terminator)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The character following the hour is not a colon.");
		return (IMS_ERROR);	
	}

	*p = '\0';
	tbl->hours = (unsigned) atoi (q);

	/*
	** Make sure the hour is between 0 and 23, inclusive.
	*/
	if ((tbl->hours < 0) || (tbl->hours > 23))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The hour value must be from 00 to 23.");
		return (IMS_ERROR);
	}

	/*
	** If this is it, we are finished and the default values will fill
	** out the rest of the date/time fields.
	*/
	if (p == terminator)
	{
		return (IMS_OK);
	}

	/*
	** Convert string minutes to unsigned int value.
	*/
	q = ++p;
	while (*p >= '0' && *p <= '9' && p < terminator)
	{
		p++;
	}

	delta = p - q;

	/*
	** The minute must be 2 digits in length.
	*/
	if (delta != 2)
	{
		(void *) ims_msg (msgDesc, IMS_ERROR,
			"The minute value must be 2 digits in length.");
		return (IMS_ERROR);
	}

	if (*p != ':' && p != terminator)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The character following the minutes is not a colon.");
		return (IMS_ERROR);	
	}

	*p = '\0';
	tbl->minutes = (unsigned) atoi (q);

	/*
	** Make sure the minute is between 0 and 59, inclusive.
	*/
	if ((tbl->minutes < 0) || (tbl->minutes > 59))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The minute value must be from 00 to 59.");
		return (IMS_ERROR);
	}

	/*
	** If this is it, we are finished and the default values will fill
	** out the rest of the date/time fields.
	*/
	if (p == terminator)
	{
		return (IMS_OK);
	}

	/*
	** Convert string seconds to unsigned int value.
	*/
	q = ++p;
	while (*p >= '0' && *p <= '9' && p < terminator)
	{
		p++;
	}

	delta = p - q;

	/*
	** The second must be 2 digits in length.
	*/
	if (delta != 2)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The second value must be 2 digits in length.");
		return (IMS_ERROR);
	}

	if (*p != '.' && p != terminator)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The character following the seconds is not a period.");
		return (IMS_ERROR);	
	}

	*p = '\0';
	tbl->seconds = (unsigned) atoi (q);

	/*
	** Make sure the second is between 0 and 59, inclusive.
	*/
	if ((tbl->seconds < 0) || (tbl->seconds > 59))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The second value must be from 00 to 59.");
		return (IMS_ERROR);
	}

	/*
	** If this is it, we are finished and the default values will fill
	** out the rest of the date/time fields.
	*/
	if (p == terminator)
	{
		return (IMS_OK);
	}

	/*
	** Convert string msecs to unsigned int value.
	*/
	q = ++p;
	while (*p >= '0' && *p <= '9' && p < terminator)
	{
		p++;
	}

	delta = p - q;

	/*
	** The millisecond must be 3 digits in length.
	*/
	if (delta != 3)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The millisecond value must be 3 digits in length.");
		return (IMS_ERROR);
	}

	/*
	** The range for millisecond is 0 to 999.  Since this cannot be
	** exceeded in 3 digits we do not check for it.
	*/
	*p = '\0';
	tbl->msecs = (unsigned) atoi (q);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_numericDateToASFA ()
**
** This function converts the numeric date to a character string
** in ASF readable format (YYYY DOY:hh:mm:ss.fff).
**
******************************************************************************/

void ims_numericDateToASFA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%4d %03d:%02d:%02d:%02d.%03d",
		tbl->year, tbl->doy, 
		tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateToACSA ()
**
** This function converts the numeric date to a character string
** in ACS readable format (YYYY:DOY:hh:mm:ss.fff).
**
******************************************************************************/

void ims_numericDateToACSA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%4d:%03d:%02d:%02d:%02d.%03d",
		tbl->year, tbl->doy, 
		tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}
/******************************************************************************
**
** ims_numericDateToIMSA ()
**
** This function converts the numeric date to a character string
** in IMS readable format (YYYY-DOYThh:mm:ss.fff).
**
******************************************************************************/

void ims_numericDateToIMSA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%4d-%03dT%02d:%02d:%02d.%03d",
		tbl->year, tbl->doy, 
		tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateToDBMSA ()
**
** This function converts the numeric date to a character string
** in DBMS readable format (MM/DD/YYYY hh:mm:ss.fff).
**
******************************************************************************/

void ims_numericDateToDBMSA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%02d/%02d/%4d %02d:%02d:%02d.%03d",
		tbl->month, tbl->day, tbl->year,
		tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateToCSA ()
**
** This function converts the numeric date to a character string
** in CSA readable format (YYYY-DOY-hh:mm:ss.fff).
**
******************************************************************************/

void ims_numericDateToCSAA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%4d-%03d-%02d:%02d:%02d.%03d",
		tbl->year, tbl->doy, 
		tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateToTTDLA ()
**
** This function converts the numeric date to a character string
** in TTDL readable format (<DD><MON><YEAR> as in 01JAN1994).
**
******************************************************************************/

void ims_numericDateToTTDLA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%02d%s%d",
		tbl->day, months[tbl->month-1], tbl->year);
}

/******************************************************************************
**
** ims_numericDateToV0A ()
**
** This function converts the numeric date to a character string
** in IMS V0 client readable format (YYYY-MM-DDThh:mm:ss).
**
******************************************************************************/

void ims_numericDateToV0A (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%04d-%02d-%02dT%02d:%02d:%02d",
		tbl->year, tbl->month, tbl->day,
		tbl->hours, tbl->minutes, tbl->seconds);
}

/******************************************************************************
**
** ims_numericDateToESAA ()
**
** This function converts the numeric date to an ESA character string in 
** format DD-MON-YYYY hh:mm:ss.mss
**
******************************************************************************/

void ims_numericDateToESAA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf(asciiDate, "%02d-%s-%04d %02d:%02d:%02d.%03d",
		tbl->day, months[tbl->month-1], tbl->year, tbl->hours, 
		tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateToESAI ()
**
** This function converts the numeric date to an ESA integer format
** reflecting the number of days since Jan 1, 1950 and the
** the number of milliseconds that have passed to the time
** reflected in the date/time table.
**
******************************************************************************/

void ims_numericDateToESAI (
	IMS_NUMERIC_DATE *tbl,
	int *days,
	int *msecs)
{
	int i;

	*msecs = 0;
	*days = 0;

	/*
	** If the given year is less than the reference year
	** just return with zeros.
	*/
	if (tbl->year < 1950)
	{
		return;
	}

	*msecs = (tbl->hours * 3600 * 1000) + (tbl->minutes * 60 * 1000) +
		(tbl->seconds * 1000) + tbl->msecs; 
	

	for (i = 1950; i < tbl->year; i++)
	{
		if (IMS_LEAP_YEAR (i) == IMS_TRUE)
		{
			*days += 366;
		}
		else
		{
			*days += 365;
		}

	}
	*days += (tbl->doy - 1);

}

/******************************************************************************
**
** ims_dateItoDef ()
**
** This function will convert the number of days and msecs to a string in the
** IMS ASF format.
**
** Jan 1, 1950 @ 00:00:00.000: days = 1.  msecs = 0.0 
******************************************************************************/

void ims_dateItoDef(
	int days,
	int msecs,
	char *date)
{
	int year = 1950;
	int doy;
	int hours, minutes, seconds;


	while (((days > 365) && (!IMS_LEAP_YEAR(year))) ||
		     ((days > 366) && (IMS_LEAP_YEAR(year))))
	{
		if (IMS_LEAP_YEAR(year))
		{
			days -= 366;
		}
		else
			days -= 365;

		year ++;

	}
	doy = days;

	hours = (int) (msecs/(3600 * 1000));
	msecs = msecs - (hours * 3600 * 1000);
	minutes = (int) (msecs/(60 * 1000));
	msecs = msecs - (minutes * 60 * 1000);
	seconds = (int) (msecs/1000);
	msecs = msecs - seconds * 1000;

	sprintf(date, "%04d-%03dT%02d:%02d:%02d.%03d",
		year, doy, hours, minutes, seconds, msecs);


}


/******************************************************************************
**
** ims_numericDateToGMTA ()
**
** This function converts the numeric date to a character string
** in GMT readable format (DOY:hh:mm:ss.fff).
**
******************************************************************************/
 
void ims_numericDateToGMTA (
	IMS_NUMERIC_DATE *tbl,
	char *asciiDate)
{
	(void) sprintf (asciiDate, "%03d:%02d:%02d:%02d.%03d ",
		tbl->doy, tbl->hours, tbl->minutes, tbl->seconds, tbl->msecs);
}

/******************************************************************************
**
** ims_numericDateDiff ()
**
** Calculates the difference between two dates and returns the difference
** in days and milliseconds.   The routine will always take the absolute 
** value of the difference so the results are always non-negative integers.
**
******************************************************************************/

int ims_numericDateDiff (
	IMS_MSG_STRUCT *msgDesc,
	char *date1,
	char *date2,
	int *days,
	int *msecs)

{
	IMS_NUMERIC_DATE tbl1;
	IMS_NUMERIC_DATE tbl2;
	int days1, days2;
	int msecs1, msecs2;
	int status;

	/*
	** Check parameter date 1.
	*/

	if ((status = ims_timeToNumericDate(msgDesc, date1, &tbl1)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Date Parameter 1 %s is invalid.",
			date1);
		return(IMS_ERROR);
	}

	/*
	** Check parameter date 2.
	*/
	
	if ((status = ims_timeToNumericDate(msgDesc, date2, &tbl2)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Date Parameter 2 %s is invalid.",
			date2);
		return(IMS_ERROR);
	}

	/*
	** Use  ESA integer format to convert the days, and msecs.
	*/

	ims_numericDateToESAI (&tbl1, &days1, &msecs1);
	ims_numericDateToESAI (&tbl2, &days2, &msecs2);

	*days = abs(days2 - days1);
	*msecs = abs(msecs2 - msecs1);	
	return(IMS_OK);
}

/******************************************************************************
**
** ims_numericDateDiff2 ()
**
** Calculates the difference between two dates and returns the difference
** in days and milliseconds.   The routine will always take the absolute
** value of the difference so the results are always non-negative integers.
** In addition, the variable compare is -1 if date1 is less than date2,
** 0 if they are equal, and 1 if date1 is greater than date2.
**
******************************************************************************/

int ims_numericDateDiff2 (
	IMS_MSG_STRUCT *msgDesc,
	char *date1,
	char *date2,
	int *days,
	int *msecs,
	int *compare )
{
	IMS_NUMERIC_DATE tbl1;
	IMS_NUMERIC_DATE tbl2;
	int days1, days2;
	int msecs1, msecs2;
	int status;

	/*
	** Check parameter date 1.
	*/

	if ((status = ims_timeToNumericDate(msgDesc, date1, &tbl1)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Date Parameter 1 %s is invalid.",
			date1);
		return(IMS_ERROR);
	}

	/*
	** Check parameter date 2.
	*/
	
	if ((status = ims_timeToNumericDate(msgDesc, date2, &tbl2)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Date Parameter 2 %s is invalid.",
			date2);
		return(IMS_ERROR);
	}

	/*
	** Use  ESA integer format to convert the days, and msecs.
	*/

	ims_numericDateToESAI (&tbl1, &days1, &msecs1);
	ims_numericDateToESAI (&tbl2, &days2, &msecs2);

	*days = abs(days2 - days1);
	*msecs = abs(msecs2 - msecs1);	

	if(  days1  <  days2  )  *compare = 1;
	else  if(  days1  >  days2 )  *compare = -1;
	else{
		if(  msecs1  <  msecs2  )  *compare = 1;
		else  if(  msecs1  >  msecs2 )  *compare = -1;
		else  *compare = 0;
	}
	return(IMS_OK);
}   /*  ims_numericDateDiff2   */

/******************************************************************************
**
** ims_getCurrentDate ()
**
** Returns the current date to the caller.
**
******************************************************************************/

int ims_getCurrentDate (
	IMS_MSG_STRUCT *msgDesc,
	IMS_NUMERIC_DATE *tbl)
{
	time_t clock;
	struct tm *tmdef;
	char dateString[32];

	(void) time (&clock);

	tmdef = localtime (&clock);

	(void) sprintf (dateString, "%d:%03d:%02d:%02d:%02d.%03d",
		1900 + tmdef->tm_year,
		tmdef->tm_yday + 1, tmdef->tm_hour, tmdef->tm_min, tmdef->tm_sec, 0);
	
	return (ims_timeToNumericDate (msgDesc, dateString, tbl));
}
