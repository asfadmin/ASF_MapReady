#ifndef _DATE_UTIL_H_
#define _DATE_UTIL_H_

/********************************************
Constants:
*/
#define DAY2SEC (24.0*60*60)
#define SEC2MSEC (1000.0)

/*********************************************
Structures:
*/
typedef struct {
	int year;/*Gregorian year (e.g. 1998)*/
	int jd;/*Julian day of year (e.g. 33, for February 2nd.)*/
} julian_date;

typedef struct {
	int year;/*Gregorian year (e.g. 1998)*/
	int month;/*1-based month of year (e.g. 3 for March).*/
	int day;/*1-based day of month.*/
} ymd_date;

typedef struct {
	int hour;/*Military-time hour of day (e.g. 00 for midnight hour.)*/
	int min;/*Minute of hour, from 0 to 59*/
	double sec;/*Second of minute, from 0.000... to 59.999...*/
} hms_time;

/*********************************************
Calender/Date Conversion utilities:*/
/*Get number of days in given Gregorian Year.*/
int date_getDaysInYear(int year);

/*Get month/day given year and julian day of year; vice versa.
 ------------------------------------------------------------*/
void date_jd2ymd(julian_date *in,ymd_date *out);
void date_ymd2jd(ymd_date *in,julian_date *out);

/*Compute the number of seconds since midnight, Jan 1, 1900.
 ----------------------------------------------------------*/
double date2sec(julian_date *date,hms_time *time);

/*Compute the date corresponding to this number of seconds 
  since midnight, Jan 1, 1900.
 --------------------------------------------------------*/
void sec2date(double secs,julian_date *date,hms_time *time);

/*Get Julian Date (in fractional days since "creation"):
 ------------------------------------------------------*/
double date_getJD(ymd_date *in);

/*Get (ESA-Modified) Julian Day given conventional year and julian day.
 ---------------------------------------------------------------------*/
int date_getMJD(julian_date *in);

/*Convert hour/minute/second to seconds in day; vice versa.
 ---------------------------------------------------------*/
double date_hms2sec(hms_time *in);
void date_sec2hms(double sec,hms_time *out);

/* Convert a year, month, day into a gmt_day (ie day of the year)
 ---------------------------------------------------------------*/
int date_ymd2gmt(ymd_date *date);


/************************************************
     Date/Time parsing routines:
 ***********************************************/

/* Date Utility Routine: Convert ODL style date to structure.
   Input format is YYYY-DDDTHH:MM:SS.TTT
 -----------------------------------------------------------*/
void parse_odlTime(const char *str, ymd_date *date, hms_time *time);


/*Date Utility routine: Convert LZ style date to structure.
  Input format is YYYYMMDDHHMMSSTTT
 ---------------------------------------------------------*/
void parse_ymdTime(const char *inStr,ymd_date *date,hms_time *time);


/* Parse a reference time string of the form YYYYDDDHHMMSS[TTT]
 -------------------------------------------------------------*/
void parse_refTime(const char *refTime, julian_date *julDay, hms_time *time);


/************************************************
     Date/Time mathematical routines:
 ***********************************************/

/* Calculate seconds elapsed from given refYear to the seekDate, seekTime
 -----------------------------------------------------------------------*/
double timeSince(ymd_date *seekDate, hms_time *seekTime, int refYear);


/* Calculate absolute seconds difference from
   date1,time1 to date2,time2
 ----------------------------------------------*/
double date_difference(ymd_date *date1, hms_time *time1,
                       ymd_date *date2, hms_time *time2);


/*-------------------------------------------
  Compare date1,time1 to date2,time2;
     return -1 if date1,time1 < date2,time2
     return  1 if date1,time1 > date2,time2
     return  0 if date1,time1 = date2,time2
 ------------------------------------------*/
int compare_time(ymd_date *date1, hms_time *time1,
                 ymd_date *date2, hms_time *time2);


/* Add delta seconds onto the the given date,time
 -----------------------------------------------*/
void add_time(double delta, ymd_date *date, hms_time *time);


/* Subtract delta seconds from the the given date,time
 ----------------------------------------------------*/
void sub_time(double delta, ymd_date *date, hms_time *time);



/****************************************
Date I/O Utilites:
	These write characters into a raw buffer.
They don't even append a '/0' to the end of their
inputs, because they are intended for use with fixed-
size ASCII structures.
	They return a pointer to just past the end of
what they wrote.
*/
/*Extract a DSSR-style date from inStr=YYYYMMDDhhmmssttt */
void date_dssr2date(const char *inStr,ymd_date *date,hms_time *time);

/*Extract second DSSR-style date from instr="DD-MMM-YYYY hh:mm:ss.ttt" */
void date_dssr2time(const char *inStr,hms_time *time);

// Extract ALOS summary style data from instr="YYYYMMDD hh:mm:ss.ttt"
void date_alos2date(const char *inStr,ymd_date *date,hms_time *time);

// Extract SIR-C summary style data from instr="YYYY/MM/DD hh:mm:ss.ttt"
void date_sirc2date(const char *inStr,ymd_date *date,hms_time *time);

// Converts a DDSR style date to a time stamp
void date_dssr2time_stamp(const char *inStr, char *t_stamp);

// Converts a SHR style date to a data stamp
void date_shr2date_stamp(const char *inStr, char *d_stamp);

/*Writes YY&MM&DD, where sep==&*/
char * date_printY2Kdate(ymd_date *in,char sep,char *dest);

/*Writes YYYY&MM&DD, where sep==&*/
char *date_printDate(ymd_date *in,char sep,char *dest);

/*Writes HH&MM&SS.CCCC,
where sep==& (no spaces if sep=='\0')
where there are prec "C"'s. (no decimal if prec==0).*/
char *date_printTime(hms_time *in,int prec,char sep,char *dest);

#endif
