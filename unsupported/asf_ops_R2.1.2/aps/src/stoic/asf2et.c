#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	Most of the routines in this module can be found in the libtimes.a 
	library; we will switch over to it very soon.
==============================================================================*/
#pragma ident	"@(#)asf2et.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/stoic/SCCS/s.asf2et.c"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>

#include "dapps_defs.h"
#include "timeconv.h"

static char asf_time_format[] = "%4d:%03d:%02d:%02d:%02d.%03d" ;
static char csa_time_format[] = "%4d-%03d-%02d:%02d:%02d.%03d" ;
static char esa_time_format[] = "%4d%02d%02d%02d%02d%02d" ;

static char colon[] = ":" ;
static char dot[] = "." ;

static int daysinmonth[2][12] =  
{
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
	31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
} ;



/*==============================================================================
Function:		tc_asf2et

Description:	Convert ASF time to ephemeris time

Parameters:		char *asftime

Returns:     	int TRUE/FALSE

Creator:		Gus Faist

Creation Date:	05/04/1995

Notes:		
==============================================================================*/
int tc_asf2et_(char *asftime, double *et)
{
	double utc ;

	/* check for valid asftime */

	if (tc_validate_asf_datetime_(asftime) != TRUE )
	{
		*et = 0.0 ;
		return (FALSE) ;
	}
	/* calculate ephemeris time */

	if (!tc_asf2julian_(asftime, &utc))
		return(FALSE) ;

	*et = tc_utc2et_(&utc) ;

	return (TRUE) ;
}
	

/*==============================================================================
Function:		tc_et2asf

Description:	Convert an ephemeris time to ASF time

Parameters:		double * et
            	char * asftime 

Returns:     	int TRUE/FALSE

Creator:		Gus Faist

Creation Date:	05/08/1995

Notes:		
==============================================================================*/
int tc_et2asf_(double et, char *asftime)
{
	double utc ;

	utc = tc_et2utc_(&et) ;
	if (!tc_julian2asf_(&utc, asftime))
		return(FALSE) ;

	if (!tc_validate_asf_datetime_(asftime)) 
		return(FALSE) ;

	return (TRUE) ;
}


/*=============================================================================
Function:    tc_leapyr

Description: This function determines if a given year is a leap year. Given an
             integer year number, the function tests the year and returns a TRUE
             value (1) if the given year is a leap year ; otherwise, it returns 
             a FALSE value (0).

Input argument:
        int year
        = a year number ; example: 1995

Returns:
        int:
        = 0 (FALSE) input year is not a leap year.
        = 1 (TRUE)  input year is     a leap year.

Creator:    Ted Pavlovitch

Creation date: 03/27/95
===========================================================================*/
int tc_leapyr_(int year)
{
	if( year % 4 == 0 && year % 100 != 0 || year % 400 == 0 )
		return(TRUE) ;
	else
		return(FALSE) ;
} /*  end function leapyr  */


/*==============================================================================
Function:		tc_validate_asf_datetime

Description:	Validate ASF time string for proper values, delimiters and length.

Parameters:		char asftime[22]            asftime  (yyy:ddd:hh:mm:ss.ccc)
          		boolean ASF_datetime_err    TRUE, FALSE

Returns:     	TRUE if asftime is a valid ASF time string, otherwise FALSE

Creator:		Gus Faist (Inspired by Ron Green's cb_validate_ASF_datetime)

Creation Date:	02/15/1995

Notes:		
				10/18/95	Teresa McKillop		added check for length
==============================================================================*/
int  tc_validate_asf_datetime_(asftime)
	char *asftime ;
{
	char *ptr ;
	char *ASF_datetime ;

	/* ASF datetime str tokens */
	char *year, *dayofyear ;
	char *hour, *min, *sec, *msec ;

	/* integer equivalents */
	int iyear, idayofyear ;
	int ihour, imin, isec, imsec ;
	int daysinyear ;

	/* Check separators & length of all fields */

	if ( (int) strlen( asftime ) != ASF_TIME_STR_LENGTH  )
		return (FALSE) ;

	/*
	-- this routine was allowing non-numerics 
	-- in the numeric fields.  atoi() does not 
	-- set errno when this happens.  
	*/
	if ( !isdigit(asftime[0])
	||   !isdigit(asftime[1])
	||   !isdigit(asftime[2])
	||   !isdigit(asftime[3])
	||   asftime[4]        != colon[0] 
	||   !isdigit(asftime[5])
	||   !isdigit(asftime[6])
	||   !isdigit(asftime[7])
	||   asftime[8]        != colon[0] 
	||   !isdigit(asftime[9])
	||   !isdigit(asftime[10])
	||   asftime[11]       != colon[0] 
	||   !isdigit(asftime[12])
	||   !isdigit(asftime[13])
	||   asftime[14]       != colon[0] 
	||   !isdigit(asftime[15])
	||   !isdigit(asftime[16])
	||   asftime[17]       !=   dot[0]  
	||   !isdigit(asftime[18])
	||   !isdigit(asftime[19])
	||   !isdigit(asftime[20])   )
		return (FALSE) ;

	/* get the contents of the field for processing */
	ASF_datetime = (char *) malloc(ASF_TIME_STR_LENGTH + 1) ;
	ptr = strcpy( ASF_datetime, asftime );


	/*
	-- NOTE:  year, dayofyear, etc are pointers onto the malloc 
	-- area. 
	-- For this reason, the free() must be delayed.  
	*/
	year      = strtok(ptr, colon) ;
	dayofyear = strtok(NULL, colon) ;
	hour      = strtok(NULL, colon) ;
	min       = strtok(NULL, colon) ;
	sec       = strtok(NULL, dot) ;
	msec      = strtok(NULL, dot) ;

	/* make sure we have data for each item */
	if (!year || !dayofyear || !hour || !min || !sec || !msec)
	{
		free(ASF_datetime) ;
		return (FALSE) ;
	}

	errno = 0 ;  /* reset errno before atoi calls */

	/* get integer equivelents */
	iyear       = atoi(year) ;
	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	idayofyear  = atoi(dayofyear) ;
	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	ihour       = atoi(hour) ;
	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	imin        = atoi(min) ;
	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	isec        = atoi(sec) ;
	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	imsec       = atoi(msec) ;

	if (errno) 
	{
		free(ASF_datetime) ;
		return(FALSE) ;
	}

	free(ASF_datetime) ;

	daysinyear = 365 + tc_leapyr_(iyear) ;

	if ((iyear < 1990)
	|| (idayofyear > daysinyear)
	|| (ihour < 0 || ihour > 23)
	|| (imin < 0 || imin > 59)
	|| (isec < 0 || isec > 59))
		return (FALSE) ;

	return(TRUE) ;
}



/********************************************************************
   Function tc_parse_asftime

   Purpose: To parse the 22 character asftime format and return
            year, decade, day, hour, min, sec, and msec as integers.
            The caller can use these components to build a time
            string in any desired format. This function will also
            parse csatime. 

   Functions called: tc_validate_asf_datetime

   Input Parameters:
        Name            Type       Definition
	filename        c*         Pointer to 22 char asf time string.

   Output Parameters:
	Name		Type	   Definition
	year		int*   Pointer to year (e.g., 1995)
	decade		"	   Pointer to decade (e.g., 95)
	day			"	   Pointer to day (e.g., 365)
	hour		"	   Pointer to hour (e.g., 23)
	min			"	   Pointer to minute (e.g., 59)
	sec			"	   Pointer to second (e.g., 59)
	msec		"	   Pointer to millisecond (e.g., 999)

   Return: TRUE  = OK
       	   FALSE = input datetime string in error.

   Created by: G. Faist 04/21/95
********************************************************************/
int tc_parse_asftime_(asftime, year, decade, day, hour, min, sec, msec)
	char *asftime ;
	int *year, *decade, *day, *hour, *min, *sec, *msec ;
{
	int status ;
	char *pasf ;

	if ((status = tc_validate_asf_datetime_(asftime)) != TRUE)
		return (FALSE) ;

	pasf = asftime ;

	*year   = atoi(pasf) ;
	pasf   += 2 ;
	*decade = atoi(pasf) ;
	pasf   += 3 ;
	*day    = atoi(pasf) ;
	pasf   += 4 ;
	*hour   = atoi(pasf) ;
	pasf   += 3 ;
	*min    = atoi(pasf) ;
	pasf   += 3 ;
	*sec    = atoi(pasf) ;
	pasf   += 3 ;
	*msec   = atoi(pasf) ;

	return (TRUE) ;
}


/*==============================================================================
Function:	tc_doy2cal()

Description: This function takes year and day of year and converts to month
             and day of month.  The input parameters are integers and pointers
             to integers.		

Parameters:	integer day, integer year, pointer to integer month, 
            pointer to integer day of month			

Returns:	TRUE  = successful conversion
			FALSE = error     	

Creator:    Gus Faist

Creation Date:	05/05/1995

Notes:		This function replaces the equivalent FORTRAN function tf_day2cal		
==============================================================================*/
int tc_doy2cal_(int year, int day, int *mon, int *mday)
{
	int i, j ;

	/* Check for leap year and process */

	j = tc_leapyr_(year) ;

	*mday = day ;

	for (i = 0; i <= 11; i++)
	{
		*mon = i+1 ;
		if(*mday > daysinmonth[j][i])
			*mday = *mday - daysinmonth[j][i] ;
		else 
			return (TRUE) ;
	}
	return (FALSE) ;
}


/*=============================================================================
Function:	tc_cal2doy()

Description:This function takes year, mon and day of month and returns day
			of year.

Parameters:	pointer to integer day, integer year, integer month, integer day
			of month.

Returns:	TRUE  = successful conversion
			FALSE = error     	

Creator:		Gus Faist

Creation Date:	05/05/1995

Notes:		This function replaces the equivalent FORTRAN function tf_cal2day
==============================================================================*/
int tc_cal2doy_(int year, int mon, int mday, int *doy)
{
	int i, j ;

	/* Check for leap year and process */

	j = tc_leapyr_(year) ;

	*doy = mday ;

	for (i = 0; i <= (mon - 2); i++)
			*doy = *doy + daysinmonth[j][i] ;
	return (TRUE) ;
}


/********************************************************************
*  Function tc_id_asftime
*
*  Purpose: Generate file name containing time stamp
*  for WFF bound files.
*          
*  $Logfile:   $
*
*  Functions called: tc_systime2asf()
*
*  Input Parameters:
*	Name		Type    Definition
*	ID		c*	Pointer to string to prepend to time tag.
*
*  Output Parameters:
*	None
*
*  Return:
*	Character pointer to generated name or NULL if error.
*
*  Locals :
*	year		i	integer year
*	decade		i		decade
*	day			i	  "     day
*	hour		i	  "     hour
*	min			i	  "     minute
*	sec			i	  "     second
*	msec		i	  "     millisecond
*	asftime		c       string space to hold new name
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
char *tc_id_asftime_(ID)
	char *ID ;
{
	int year, decade, day, hour, min, sec, msec ;
	char asftime[22] ;
	char *stored_filename ;

	/* Get Current ASF Time */
	tc_systime2asf_(asftime) ;

	/* 
	-- Parse the string 
	-- if string cannot be parsed then an error has occurred 
	*/
	if (!tc_parse_asftime_(
			asftime, &year, &decade, &day, &hour, &min, &sec, &msec))
		return NULL ;

	/* Build file name for WFF file */

	sprintf(asftime, "%s_%04d-%03dT%02d:%02d:%02d.%03d\0", 
		ID, year, day, hour, min, sec, msec) ;

	/* Get storage, copy, and return pointer */
	stored_filename = (char *) malloc(strlen(asftime) + 1) ;
	strcpy(stored_filename, asftime) ;

	return(stored_filename) ;
}


/********************************************************************
*  Function tc_asf2csa
*
*  Purpose: convert asf time to csa time
*          
*  $Logfile:   $
*
*  Functions called: tc_parse_asftime
*
*  Input Parameters:
*	Name		Type    Definition
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*
*  Output Parameters:
*	csatime		c*	csa time (yyyy-ddd-hh:mm:ss.ccc)
*
*  Locals :
*	year		i	integer year
*	decade		i	  "     decade
*	day			i	  "     day
*	hour		i	  "     hour
*	min			i	  "     minute
*	sec			i	  "     second
*	msec		i	  "     millisecond
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_asf2csa_(asftime, csatime)
	char *asftime, *csatime ;
{
	int year, decade, day, hour, min, sec, msec ;

	/* parse asftime */

	if (!tc_parse_asftime_(
			asftime, &year, &decade, &day, &hour, &min, &sec, &msec))
		return (FALSE) ;

	/* build the csa string */

	sprintf(csatime, csa_time_format, year, day, hour, min, sec, msec) ;

	return (TRUE) ;
}


/********************************************************************
*  Function tc_csa2asf
*
*  Purpose: convert csa time to asf time
*          
*  Functions called:tc_parse_asftime 
*
*  Input Parameters:
*	Name		Type    Definition
*	csatime		c*	csa time (yyyy-ddd-hh:mm:ss.ccc)
*
*  Output Parameters:
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*
*  Locals :
*	year		i	integer year
*	decade		i	  "	decade
*	day			i	  "     day
*	hour		i	  "     hour
*	min			i	  "     minute
*	sec			i	  "     second
*	msec		i	  "     millisecond
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_csa2asf_(csatime, asftime)
	char *csatime, *asftime ;
{
	static char colon = ':' ;

	int year, decade, day, hour, min, sec, msec ;

	strcpy(asftime, csatime) ;
	asftime[4] = colon ;
	asftime[8] = colon ;

	return(tc_validate_asf_datetime_(asftime)) ;

}


/********************************************************************
*  Function tc_asf2esa
*
*  Purpose: convert asf time to esa time
*          
*  Functions called: tc_parse_asftime, tc_doy2cal
*
*  Input Parameters:
*	Name		Type    Definition
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*
*  Output Parameters:
*	esatime		c*	esa time (yyyymmddhhmmss)
*
*  Locals :
*	year		i	integer year
*	decade		i	  "	decade 
*	day			i	  "     day
*	hour		i	  "     hour
*	min			i	  "     minute
*	sec			i	  "     second
*	msec		i	  "	millisecond
*	mon			i	  "     month
*	mday		i	  "     day of month
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_asf2esa_(asftime, esatime)
	char *asftime, *esatime ;
{
	int year, decade, day, hour, min, sec, msec ;
	int mon, mday ;

	if (!tc_parse_asftime_(
			asftime, &year, &decade, &day, &hour, &min, &sec, &msec))
		return(FALSE) ;

	/* convert the day of year to month and day */

	if (!tc_doy2cal_(year, day, &mon, &mday))
		return(FALSE) ; 

	sprintf(esatime, esa_time_format, year, mon, mday, hour, min, sec) ;

	return (TRUE) ;
}


/********************************************************************
*  Function tc_esa2asf
*  Purpose: convert esa time to asf time
*  Functions called: tc_cal2doy and tc_validate_asf_datetime
*  Input Parameters:
*	Name		Type    Definition
*	esatime		c*	esa time (yyyymmddhhmmss)
*  Output Parameters:
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*  Locals :
*	year		i	integer year
*	day			i	  "     day
*	hour		i	  "     hour
*	min			i	  "     minute
*	sec			i	  "     second
*	mon			i	  "     month
*	nday		i	  "     day of month
*	eyear		i	integer year
*	eday		i	  "     day
*	ehour		i	  "     hour
*	emin		i	  "     minute
*	esec		i	  "     second
*	emon		i	  "     month
*	nday		i	  "     day of month
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_esa2asf_(esatime, asftime)
	char *esatime ; 	
	char *asftime ;  
{
	char 
		eyear[] = "YYYY", emon[] = "MM", eday[3] = "DD", 
		ehour[] = "HH", emin[] = "MM", esec[] = "SS" ;

	int year, mon, day, hour, min, sec ;
	int doy ;

	/* format the esa time string */

	sprintf(eyear, "%.4s", esatime) ;
	sprintf(emon,  "%.2s", esatime + 4) ;
	sprintf(eday,  "%.2s", esatime + 6) ;
	sprintf(ehour, "%.2s", esatime + 8) ;
	sprintf(emin,  "%.2s", esatime + 10) ;
	sprintf(esec,  "%.2s", esatime + 12) ;


	/* convert the strings to int */
	year  = atoi(eyear) ;
	mon   = atoi(emon) ;
	day   = atoi(eday) ;
	hour  = atoi(ehour) ;
	min   = atoi(emin) ;
	sec   = atoi(esec) ;

#ifdef DEBUG	
	printf("year = %d\n", year) ;
	printf("mon  = %d\n", mon) ;
	printf("day  = %d\n", day) ;
	printf("hour = %d\n", hour) ;
	printf("min  = %d\n", min ) ;
	printf("sec  = %d\n", sec ) ;
#endif

	if ((mon < 1 || mon > 12)
	|| (day < 1  || day > 31)
	|| (hour < 0 || hour > 23)
	|| (min < 0  || min > 59)
	|| (sec < 0  || sec > 59))
		return (FALSE) ;

	/* 
	-- convert month and day to day of year (DOY) 
	-- input : yyyy mm dd   output: ddd 
	*/
	if (!tc_cal2doy_(year, mon, day, &doy)) 
		return(FALSE) ;

	/* write out the asf format time */
	sprintf(asftime, asf_time_format, 
	    year, doy, hour, min, sec, 0) ;

	return(tc_validate_asf_datetime_(asftime)) ;
}


/*****************************************************************
* Function: tc_esadaytime2asf
*
* Purpose:
*	Convert ESA format day and time to ASF format
*	dd-MMM-yyyy hh:mm:ss.ccc -> yyyy:ddd:hh:mm:ss.ccc
*
* Functions called: 
* 	tc_cal2doy 
*	tc_validate_asf_datetime
*
* Written by Craig K. Fujimoto
*
* Input:
*	esa_daytime	c*	ESA format day/time
* Output:
*	asftime		c*	ASF format time
* Return:
*			int	= 0 if no errors, 1 if there is an error.
* Internal:
*	eyear, emon, 
*	eday, ehour, 
*	emin, esec, 
*	etsec		c	parsed elements of esa time string
*	hmst		c	hours, mins, sec, tsecs combined
*	year, mon, 
*	day, hour, 
*	min, sec, tsec	i	elements converted to integer
*	doy		i	day of year
*	stat		i	return status
*
* $Date$ $Revision$ $Author$
*           8 Feb 1995                                            Gus
*****************************************************************/
int tc_esadaytime2asf_(
	char *esa_daytime, 
	char *asftime)
{
	char 
		eyear[] = "YYYY", emon[] = "MMM", eday[3] = "DD", 
		ehour[] = "HH", emin[] = "MM", esec[] = "SSS", etsec[] = "ccc" ;

	char hmst[] = "hh:mm:ss.ccc" ;

	int year, mon, day, hour, min, sec, tsec, i ;
	int doy ;
	char *month[] = 
	{
		"JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
		"JUL", "AUG", "SEP", "OCT", "NOV", "DEC", 
		NULL
	} ;

	/* Parse the ESA day/time string */
	sprintf(eday , "%.2s", esa_daytime) ;
	sprintf(emon , "%.3s", esa_daytime +  3) ;
	sprintf(eyear, "%.4s", esa_daytime +  7) ;
	sprintf(ehour, "%.2s", esa_daytime + 12) ;
	sprintf(emin , "%.2s", esa_daytime + 15) ;
	sprintf(esec , "%.2s", esa_daytime + 18) ;
	sprintf(etsec, "%.3s", esa_daytime + 21) ;

	/* Save the hh:mm:ss.ccc portion of the ESA day/time */
	sprintf(hmst, "%.12s", esa_daytime+12) ;	

	/* force the dot in the position before the msec... */
	hmst[8] = '.' ;

	/* Determine the month number */
	mon = 0 ;

	i = 0 ;
	while(month[i])
	{
		if(strncmp(emon, month[i], 3) == 0)
		{
			mon = i + 1 ;
			break ;
		}
		i = i + 1 ;
	}
	if (!mon) 
		return (FALSE) ;

	/* Convert the strings to int */
	year  = atoi(eyear) ;
	day   = atoi(eday) ;
	hour  = atoi(ehour) ;
	min   = atoi(emin) ;
	sec   = atoi(esec) ;
	tsec  = atoi(etsec) ;

	/* Error check */
	if ((year < 1970 || year > 2050) 
	|| (day  < 1  || day  > 31)  
	|| (hour < 0  || hour > 23) 
	|| (min  < 0  || min  > 59) 
	|| (sec  < 0  || sec  > 59) 
	|| (tsec < 0  || tsec > 999))
		return (FALSE) ;

	/* 
	-- convert month and day to day of year 
	-- input : yyyy mm dd   output: ddd 
	*/
	if (!tc_cal2doy_(year, mon, day, &doy))
		return(FALSE) ;

	/* write out the asf format time - yyyy:ddd:hh:mm:ss.ccc */
	sprintf(asftime, "%4d:%03d:%.12s", year, doy, hmst) ;

	return(tc_validate_asf_datetime_(asftime)) ;
}


/********************************************************************
*  Function tc_asf2yyyymmdd
*  Purpose: convert asf time to J1date format YYYYmmDD
*  Functions called: tc_asf2esa
*  Input Parameters:
*	Name		Type    Definition
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*  Output Parameters:
*	J1date		c*	J1 format date (yyyymmdd)
*  Locals :
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_asf2yyyymmdd_ (asftime, datestr)
	char *asftime, *datestr ;
{
	char esatime[] = "YYYYMMDDHHMMSS" ;

	/* convert to esatime format */
	if (!tc_asf2esa_(asftime, esatime)) 
		return(FALSE) ;

	/* Just copy YYYYMMDD */
	sprintf(datestr, "%.8s", esatime) ; 

	return (TRUE) ;
}


/********************************************************************
*  Function : tc_yyyymmdd2asf
*  Module Type: integer function Language: C
*  Purpose:
*	Compute ASF time from J1 date.  ASF time ends with 00:00:00.000.  
*
*  Functions called: tc_esa2asf 
*  Input Parameters:
*  Name         Type    Definition
*  datestr	*CHAR	J1 format date:  YYYYMMDD
*  Output Parameters:
*  asftime	*CHAR	ASF format:	 YYYY:ddd:00:00:00.000
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*           08 Feb 1995                                            GUS    $
*********************************************************************/
int tc_yyyymmdd2asf_(
	char 	*datestr, 
	char 	*asftime)
{
	char esatime[15] ;

	sprintf(esatime, "%.8s000000", datestr) ;

	if (!tc_esa2asf_(esatime, asftime)) 
		return(FALSE) ;

	return(tc_validate_asf_datetime_(asftime)) ;
}


/********************************************************************
*  Function : tc_yyyymmdd_check
*  Purpose:
*	Check datestr for values.  yyyymmdd
*
*  Input Parameters:
*  Name         Type    Definition
*  datestr	*CHAR	J1 format date:  YYYYMMDD
*  Output Parameters:
*  [returned value]	TRUE means O.K. ; FALSE means an error occurred 
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_yyyymmdd_check_(datestr)
	char *datestr ;
{
	char buf[10] ;
	int year, month, day ;

	/* get the year */
	sprintf(buf, "%.4s", datestr) ;
	year = atoi(buf) ;

	/* get the month */
	sprintf(buf, "%.2s", datestr + 4) ;
	month = atoi(buf) ;

	/* get the day */
	sprintf(buf, "%.2s", datestr + 6) ;
	day = atoi (buf) ;

	/* do simple checks first */
	if ((year < 1980)
	|| (month < 1)
	|| (month > 12)
	|| (day < 1)
	|| (day > 31))
		return (FALSE) ;

	/* now check days in the month */
	if (month == 2)
	{
		if ((tc_leapyr_(year)  &&   day > 29) 
		||  (!tc_leapyr_(year)  &&   day > 28))
			return (FALSE) ;
	}

	if ((day > 30)
	&& (month == 4 || month == 6 
	|| month == 9 || month == 11))
		return (FALSE) ;

	return (TRUE)  ;
}


/********************************************************************
*  Function tc_asf_add_ndays
*  Purpose: add one day to the input asf time.  
*  Functions called: 
*
*  Input Parameters:
*	Name		Type    Definition
*	asftime		c*	asf time (yyyy:ddd:hh:mm:ss.ccc)
*
*  Output Parameters:
*	asftime1	c*	asf time for the input time plus one day.  
*
*  Locals :
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_asf_add_ndays_(
	char 	*asftime,            /*  input asf time	  */
	double  ndays,               /*  n days to add    */
	char	*asftime_plusndays ) /*  asf time + ndays */
{
	double et ;
	char buf[22] ;

	/*  Convert asftime to et */

	if (!tc_asf2et_(asftime, &et)) 
		return (FALSE) ;

	et = et + ndays ;
	if (!tc_et2asf_(et, buf)) 
		return (FALSE) ;

	sprintf(asftime_plusndays, "%s", buf) ;
		
	return (TRUE) ;
}


/********************************************************************
*  Function: tc_systime2asf
*
*  Purpose: Get the current system time and store as a character string
*           like this: "yyyy:ddd:hh:mm:ss.000"
*          
*  Input Parameters:                                                
*                                                                   
*  Name          Type        Description                           
*  NONE
*                                                                   
*                                                                   
*  Output Parameters:                                               
*                                                                   
*  Name          Type        Description                            
*  curr_time     *char       Pointer to character string with current time
*                            in yyyy:ddd:hh:mm:ss.sss format.
*                                    ddd is from 001-366.
*                                                                   
*                                                                   
*  Modification History:                                            
*                                                                   
*  $Author$    $Revision$        $Date$
* ported from VAX to unix by Larry Stevens 3/94
*                                                                   
*********************************************************************/
int tc_systime2asf_(curr_time)
char	*curr_time ;
{
	time_t sec_1970 ;
	struct tm *time_ptr ;

	/*  Get the current time, seconds since 1 Jan 1970. */
	time(&sec_1970) ;

	/* compute time fields from sec_1970 */
	time_ptr = localtime (&sec_1970) ;

	/* 
	-- Store time fields into buffer.. 
	-- Use asftime format
	*/
	sprintf(curr_time, asf_time_format, 
		1900 + time_ptr->tm_year, 
		time_ptr->tm_yday + 1,    /* tm_yday starts at 0 */
		time_ptr->tm_hour, 
		time_ptr->tm_min, 
 		time_ptr->tm_sec, 
		0) ;

	return(TRUE) ;
}


/********************************************************************
*  Function tc_systime2yr_date
*
*  Purpose: get the current system time in asf format
*          
*
*  Functions called:
*	tc_systime2asf  user routine which calls:
*	time
*	localtime
*
*  Input Parameters:
*	Name		Type    Definition
*	year		c*	year pointer
*	date		c*	date pointer
*
*  Output Parameters:
*	year		c*	year (yyyy)
*	date		c*	date (ddd:hh:mm:ss.ccc)
*
*  Locals :
*	timebuf		c[]	temp time buffer
*	stat		i	return status
*
*  $Date$ $Revision$ $Author$
* ported from the VAX to unix by Larry Stevens 3/1994                    
*           08 Feb 1995                                            FAIST  $
*********************************************************************/
int tc_systime2yr_date_(year, date)
	char *year, *date ;
{
	char timebuf[] = "1995:001:00:00:00.000" ;

	if (!tc_systime2asf_(timebuf))
		return(FALSE) ;

	sprintf(year, "%.4s", timebuf) ;
	sprintf(date, "%.16s", timebuf + 5) ; 

	return (TRUE) ;
}


/********************************************************************
*  Function: tc_systime2yyyycddd
*
*  Purpose: gets the current date in the form yyyy:ddd
*          
*  Functions called: tc_systime2asf
*
*  Input:
*  Name         Type    Definition
*  datestr		c*	pointer to character string to hold date
*
*  Output:
*  datestr      c*      current date
*
*  Locals :
*  stat		i	return status from get_date
*
*  Return :
*  TRUE - success
*  FALSE - failure
*
*********************************************************************/
int tc_systime2yyyycddd_(datestr)
	char *datestr ;
{
	char asftime[22] ;

	if (!tc_systime2asf_(asftime))
		return (FALSE) ;

	sprintf(datestr, "%.8s", asftime) ;

	return (TRUE) ;
}


/********************************************************************
*  Function : tc_yyyymmdd_hhmmss2asf
*
*  Purpose:
*	Compute ASF time from J1 date & time.  
*
*  Functions called: esa2asf 
*
*  Input Parameters:
*  Name         Type    Definition
*  datestr	*CHAR	J1 format date:  YYYYMMDD
*  timestr	*CHAR	J1 format time:  hh:mm:ss
*
*  Output Parameters:
*  asftime	*CHAR	ASF format:	 YYYY:ddd:hh:mm:ss.000
*
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
int tc_yyyymmdd_hhmmss2asf_(
	char 	*datestr, 
	char	*timestr, 
	char	*asftime )
{
	char esatime[] = "yyyymmddhhmmss" ;

	sprintf(esatime, "%.8s%.2s%.2s%.2s", 
		datestr, timestr, timestr+3, timestr+6) ;

	return(tc_esa2asf_(esatime, asftime)) ;
}


/*==============================================================================
Function:    tc_cal2julian

Description: This function calculates a julian day, given a calendar year, month
             and day. A julian day is a method of measuring time as a time
             interval from some agreed arbitrary reference time, which is 
             12:00 hours U.T. January 1, 4713 B.C.  

Input arguments:
             int y
             = the calendar year - example:1995
             int m
             = the month of the year (1 - 12)
             int d
             = the day of the month  (1 - 31)

Returns:
             int:
             = integer days since noon, Jan 1, 4713 B.C.

Creator:     Ted Pavlovitch

Creation date: 04/25/95
==============================================================================*/
int tc_cal2julian_(int y, int m, int d)
{

	int   jd ;

	jd = d - 32075 + 1461*(y + 4800 + (m - 14)/12)/4   
	   +  367*(m - 2 - (m-14)/12*12)/12                 
	   -  3*((y + 4900 + (m - 14)/12)/100)/4 ;

	return(jd) ;
}


/*==============================================================================
Function:		tc_asf2julian

Description:	Convert ASF time to julian day

Parameters:		char *asftime
            	double *julian_day

Returns:     	int TRUE/FALSE

Creator:		Gus Faist

Creation Date:	05/10/1995

Notes:		
==============================================================================*/
int tc_asf2julian_(char *asftime, double *julian_day)
{
	int iyear, idecade, iday, ihour, imin, isec, imsec ;
	int iqd, iaft ;
	long double const t1961 = 2437300.5 ;

	if (!tc_parse_asftime_(asftime, 
			&iyear, &idecade, &iday, &ihour, &imin, &isec, &imsec))
		return(FALSE) ;

	iqd  = ((iyear - 1961) / 4) * 1461 ;
	iaft = ((iyear - 1961) % 4) *  365 ;
	*julian_day = iqd + iaft + (iday-1) + (long double) ihour/24L 
		+ (long double) imin/1440L + (long double) isec/86400L
		+ (long double) imsec/86400000L + t1961 ;

	if( iday == 1 && ihour == 0 && imin == 0 && isec == 0 && imsec == 0 )
	{
		/*	
		-- A partial second fudge for the start of the year.
		-- In the case of the user inputing 
		-- 		yyyy:001:00:00:00.000
		-- This is the addition of a partial millisecond to yield an et
		-- after a possible leap second rather than before.
		-- This addition is less than the resolution of the user input, 
		-- which is 0.001 seconds.
		*/

		*julian_day = *julian_day + 0.0001 / (24 * 3600) ;
	}
	return(TRUE) ;
}


/*==============================================================================
Function:		tc_julian2asf

Description:	Convert a julian day to ASF time format

Parameters:		double *julian_day
	        	char *asftime

Returns:     	int TRUE/FALSE

Creator:		Gus Faist/Ron Green

Creation Date:	05/16/1995

Notes:	Conversion from a FORTRAN function to a C subroutine.		
==============================================================================*/
int tc_julian2asf_(double* julian_day, char* asftime)
{
	int ileap, iyear, iday, ihour, imin, isec, imils ;
	long double ttime, t1961 = 2437300.5L ;

        printf("asftime: %s\n",asftime);

	ttime = (long double) *julian_day - t1961 ; 

	/*	
	-- Now the result is eventually going to be in milliseconds rounded, 
	-- Let's add 1/2 millisec now, and then use truncation the rest of the
	-- way. ttime is in days.
	*/

	ttime = ttime + 0.5L/1000.0L/86400.0L ;	
	ileap = (int) ttime / 1461 ;

	/*i ileap = number of leap days, there are 1461 dais in four years */

	iyear = (ttime - ileap * 1461) / 365 ;
	
	if (iyear == 4) 
		iyear = 3 ;

	iyear = iyear + 1961 +4 * ileap ;
	iday  = ttime - ileap * 1461 - (iyear - 1961 - ileap * 4) * 365 ;
	iday = iday + 1 ; /* Jan 1 is day 1 not day 0 */

	/*	Work on hours, minutes, seconds and milliseconds. */

	ttime = ttime - (int) ttime ;
	ttime = ttime * 24.0L ; /* Convert days to hours */

	ihour = (int) ttime ;
	ttime = ttime - (int) ttime ;
	ttime = ttime * 60.0L ; /* Convert hours to minutes */ 

	imin  = (int) ttime ;
	ttime = ttime - (int) ttime ;
	ttime = ttime * 60.0L ; /* Convert minutes to seconds */

	isec  = (int) ttime ;
	ttime = ttime - (int) ttime ;
	ttime = ttime * 1000.0L ; /* Convert seconds to milliseconds */

	imils = (int) ttime ;

	/*	Build asftime */

	sprintf(asftime, asf_time_format, 
		iyear, iday, ihour, imin, isec, imils) ;

	return (TRUE) ;
}


/*==============================================================================
Function:   	tc_et_ASF_datetime_diff

Description:	calculate the difference of two ASF times and report in ephemer
            	time.

Parameters:		char *strttime
            	char *stoptime
            	double *deltatime 

Returns:     	int TRUE/FALSE

Creator:		Larry Stevens

Creation Date:	05/18/1995

Notes:		
==============================================================================*/
int tc_et_ASF_datetime_diff_(strttime, stoptime, deltatime)
	char *strttime ; 
	char *stoptime ;
	double *deltatime ;
{
	double et_start ;
	double et_stop ;

	if (!tc_asf2et_(strttime, &et_start))
		return(FALSE) ;

	if (!tc_asf2et_(stoptime, &et_stop))
		return(FALSE) ;

	*deltatime = et_stop - et_start ;
	return(TRUE) ;
}
/*==============================================================================
Function:       tc_asf2odl

Description:    convert asf time to odl time

Parameters:     
Name        Type    Definition
asftime     c*  	asf time (yyyy:ddd:hh:mm:ss.ccc)
odltime     c*  	odl time (yyyy-dddThh:mm:ss.ccc)

Returns: 	always  returns TRUE

Creator:        Miguel Siu

Creation Date:  Thu Jul  6 13:58:28 PDT 1995

Notes:		
==============================================================================*/
int tc_asf2odl_(
	char *asftime, 
	char *odltime)
{
	/*
	-- strncpy ( odltime, asftime, strlen(asftime) );
	-- a more robust copy, in case source and 
	-- destination overlap or are identical.
	-- also copy the terminating null character:
	*/
	memmove(odltime, asftime, strlen(asftime)+1 ) ;
	odltime[4] = '-';
	odltime[8] = 'T';

#ifdef DEBUG
	printf("in tc_asf2odl_: asftime %s  odltime %s\n",asftime,odltime);
#endif

	return(TRUE);
}

/*==============================================================================
Function:       tc_odl2asf

Description:    convert odl time to asf time

Parameters:     
				Name        Type    Definition
				odltime     char*  	odl time (yyyy-dddThh:mm:ss.ccc)
				asftime     char*  	asf time (yyyy:ddd:hh:mm:ss.ccc)

Returns: 		always  returns TRUE

Creator:    	Teresa McKillop

Creation Date:  01/11/96

Notes:		
==============================================================================*/
int tc_odl2asf_( char *odltime, char *asftime)
{
	/*
	-- using memmove, because it is
	-- a more robust copy (in case source and 
	-- destination overlap or are identical),
	-- to copy the string
	*/
	memmove( asftime, odltime, strlen( odltime ) + 1 ) ;
	asftime[4] = asftime[8] = ':';

#ifdef DEBUG
	(void) printf( "in tc_odl2asf_: odltime %s asftime %s\n", odltime, asftime );
#endif

	return(TRUE);
}

/*==============================================================================
Function:	tc_time_pad

Description:	
			compute a new time bracket by padding the input time 
			bracket by the indicated numnber of minutes. 

Parameters:		
	input:
	char *strttime
	char *stoptime
	float minutes

	output:
	char *padded_strttime
	char *padded_stoptime

Returns:   

	TRUE   =  no error.  

	FALSE  =  an error in an input time was encountered.

Creator:		Lawrence Stevens

Creation Date:	02/01/1995

Notes:	Modified by Faist 05/18/95	

==============================================================================*/
int tc_time_pad_(
	char *strttime, 
	char *stoptime, 
	float minutes, 
	char *padded_strttime, 
	char *padded_stoptime ) 
{
	double et_days ;

#ifdef DEBUG
	printf("debug: %.8f %s - %s\n", minutes, padded_strttime, padded_stoptime) ;
#endif
	/* initialize the output times in case of an early return.  */
	strcpy(padded_strttime, "") ;
	strcpy(padded_stoptime, "") ;

	/* converting minutes to days.  */
	et_days = minutes/60.0/24.0 ;

	/* pad the time at the start; subtract the days.  */
	/* pad the time at the start; add the days.  */
	if ((!tc_asf_add_ndays_(strttime, -1 * et_days, padded_strttime))
	|| (!tc_asf_add_ndays_(stoptime,       et_days, padded_stoptime)))
		return(FALSE) ;

	return (TRUE) ;
}


/* #define MAIN */
#ifdef MAIN
/*==============================================================================
Function:       main

Description:    Test Driver for functions above

Parameters:     

Returns:        none

Creator:        Ron Green

Creation Date:  Tue May 23 10:54:54 PDT 1995

Notes:		
==============================================================================*/
void main (argc, argv)
	int argc ;
	char *argv[] ;
{
	char asftimed1[] = "1995:001:20:56:34.020" ;
	char asftimed2[] = "1995:002:04:56:34.020" ;
	char asftime1[] = "1995:002:04:56:34.020" ;
	char asftime2[] = "1995:002:04:56:34.020" ;
	char csatime[] = "1995:001:00:00:00.000" ;
	char j1date[] = "1995:011" ;
	char j1date2[] = "1995:366" ;
	char j1time[] = "23:00:44" ;
	char esatime[] = "19950228232300" ;
	char esadaytime[] = "28-FEB-1995 02:28:23:230" ;

	char cyear[] = "1995" ;
	char cdate[] = "001:00:00:00.000" ;

	char asftime4[] = "1995:001:00:00:00.000" ;
	char asftime5[] = "1995:001:00:00:00.000" ;
	char asftime6[] = "1995:001:00:00:00.000" ;

	int 
		year, doy, decade, mon, mday, day,
		hour, min, sec, msec ;

	double et ;

	static char sv[] = "SV" ;
	char *sv_filename ;
	int status ;

#define PRINT_STATUS(status) if (status) printf("STATUS: OK\n\n") ; else printf("STATUS: ERR\n\n") ;

	
	printf("tc_parse_asftime_(char *asftime, \
		\n\t&year, &decade &doy, &mon,  &hour, &min, &sec)\n") ;
	status = tc_parse_asftime_(asftime1, 
		&year, &decade, &doy, &mon,  &hour, &min, &sec) ;
	printf("%s --> %d %d %d %d %d %d %d\n", 
		asftime1, year, decade, doy, mon,  hour, min, sec) ;
	PRINT_STATUS(status) ;


	printf("char * tc_id_asftime_(char *id)") ;
	sv_filename = tc_id_asftime_(sv) ;
	if (!sv_filename)
	{
		printf("Unable to print filename...\n") ;
		PRINT_STATUS(FALSE) ;
	}
	printf("%s --> %s\n", sv, sv_filename) ;


	printf("tc_asf2csa_(asftime1, csatime)\n") ;
	status = tc_asf2csa_(asftime1, csatime) ;
	printf("%s --> %s\n", asftime1, csatime) ;
	PRINT_STATUS(status) ;


	printf("tc_csa2asf_(csatime, asftime1)\n") ;
	status = tc_csa2asf_(csatime, asftime1) ;
	printf("%s --> %s\n", csatime, asftime1) ;
	PRINT_STATUS(status) ;


	printf("tc_asf2yyyymmdd_(asftime1, j1date)\n") ;
	status = tc_asf2yyyymmdd_(asftime1, j1date) ;
	printf("%s --> %s\n", asftime1, j1date) ;
	PRINT_STATUS(status) ;


	printf("tc_yyyymmdd2asf_(j1date, asftime1)\n") ;
	status = tc_yyyymmdd2asf_(j1date, asftime1) ;
	printf("%s --> %s\n", j1date, asftime1) ;
	PRINT_STATUS(status) ;


	printf("tc_asf_add_ndays_(asftime1, 5.5, asftime2)\n") ;
	status = tc_asf_add_ndays_(asftime1, 5.5, asftime2) ;
	printf("%s --> %s\n", asftime1, asftime2) ;
	PRINT_STATUS(status) ;


	printf("tc_esa2asf_(esatime, asftime1)\n") ;
	status = tc_esa2asf_(esatime, asftime1) ;
	printf("%s --> %s\n", esatime, asftime1) ;
	PRINT_STATUS(status) ;

	printf("tc_esadaytime2asf_(esadaytime, asftime1)\n") ;
	status = tc_esadaytime2asf_(esadaytime, asftime1) ;
	printf("%s --> %s\n", esadaytime, asftime1) ;
	PRINT_STATUS(status) ;

	printf("tc_systime2yr_date_(cyear, cdate)\n") ;
	status = tc_systime2yr_date_(cyear, cdate) ;
	printf("SYSTIME --> %s %s\n",  cyear, cdate) ;
	PRINT_STATUS(status) ;


	printf("tc_systime2asf_(asftime1)\n") ;
	status = tc_systime2asf_(asftime1) ;
	printf("SYSTIME --> %s\n",  asftime1) ;
	PRINT_STATUS(status) ;


	printf("tc_esadaytime2asf_(esadaytime, asftime1)\n") ;
	status = tc_esadaytime2asf_(esadaytime, asftime1) ;
	printf("%s --> %s\n", esadaytime, asftime1) ;
	PRINT_STATUS(status) ;


	printf("tc_yyyymmdd_check_(j1date)\n") ;
	status = tc_yyyymmdd_check_(j1date) ;
	printf("%s --> %s\n", j1date, j1date) ;
	PRINT_STATUS(status) ;

	printf("tc_yyyymmdd_check_(j1date2)\n") ;
	status = tc_yyyymmdd_check_(j1date2) ;
	printf("%s --> %s\n", j1date2, j1date2) ;
	PRINT_STATUS(status) ;


	printf("tc_yyyymmdd_hhmmss2asf_(j1date, j1time, asftime)\n") ;
	status = tc_yyyymmdd_hhmmss2asf_(j1date, j1time, asftime1) ;
	printf("%s  %s --> %s\n", j1date, j1time, asftime1) ;
	PRINT_STATUS(status) ;

	printf("tc_leapyr_(1992)\n") ;
	status = tc_leapyr_(1992) ;
	PRINT_STATUS(status) ;


	printf("tc_leapyr_(1993)\n") ;
	status = tc_leapyr_(1993) ;
	PRINT_STATUS(status) ;

	printf("tc_doy2cal_(1993, 60, &year, &mday )\n") ;
	status = tc_doy2cal_(1993, 60, &year, &mday) ;
	printf("YEAR: %d DOY: %d --> month: %d MDay: %d\n", 1992, 60, year, mday) ;
	PRINT_STATUS(status) ;


	printf("tc_cal2doy_(1992, 3, 01, &doy)\n") ;
	status = tc_cal2doy_(1992, 3, 01, &doy) ;
	printf("DOY: %d\n", doy) ;
	PRINT_STATUS(status) ;

	printf("tc_cal2doy_(1993, 2, 29, &doy)\n") ;
	status = tc_cal2doy_(1993, 2, 29, &doy) ;
	printf("DOY: %d\n", doy) ;
	PRINT_STATUS(status) ;


	printf("tc_doy2cal_(1993, 60, &mon, &day)\n") ;
	status = tc_doy2cal_(1993, 60, &mon, &day) ;
	printf("Month: %d Day:%d\n", mon, day) ;
	PRINT_STATUS(status) ;


	printf("tc_doy2cal_(1992, 60, &mon, &day)\n") ;
	status = tc_doy2cal_(1992, 60, &mon, &day) ;
	printf("Month: %d Day:%d\n", mon, day) ;
	PRINT_STATUS(status) ;

	printf("tc_asf2et_(asftime2, &et)\n") ;
	status = tc_asf2et_(asftime2, &et) ;
	printf("ASFT: %s ET: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;

	status = asf2et_(asftime2, &et, &status) ;
	printf("ORIGINAL: ASFT: %s ET: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;
	

	printf("tc_et2asf_(et, asftime2)\n") ;
	status = tc_et2asf_(et, asftime2) ;
	printf("ASFT: %s ET: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;

	asftime2[0] = NULL ;
	status = et2asf_(&et, asftime2) ;
	printf("ORIGINAL: ASFT: %s ET: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;
	

	printf("tc_asf2julian_(asftime2, &julian)\n") ;
	status = tc_asf2julian_(asftime2, &et) ;
	printf("ASFT: %s julian: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;

	asftime2[0] = NULL ;
	printf("tc_julian2asf_(&et, asftime2)\n") ;
	status = tc_julian2asf_(&et, asftime2) ;
	printf("ASFT: %s julian: %.8f \n", asftime2, et) ;
	PRINT_STATUS(status) ;

	printf("tc_et_ASF_datetime_diff_(asftimed1, asftimed2)\n") ;
	status = tc_et_ASF_datetime_diff_(asftimed1, asftimed2, &et) ;
	printf("ASFT: %s - %s: %.8f \n", asftimed1, asftimed2, et) ;
	PRINT_STATUS(status) ;

	printf("tc_time_pad_(asftime1, asftime2, 63, asftime4, asftime5)\n") ;
	printf("ASFT: %s - %s \n ASFT: %s - %s \n ", 
		asftime1, asftime2, asftime4, asftime5) ;
	status = tc_time_pad_(asftime1, asftime2, 63, asftime4, asftime5) ;
	printf("ASFT: %s - %s \n ASFT: %s - %s \n ", 
		asftime1, asftime2, asftime4, asftime5) ;
	PRINT_STATUS(status) ;


}
#endif

