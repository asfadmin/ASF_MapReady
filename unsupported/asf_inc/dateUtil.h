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

/*Get month/day given year and julian day of year; vice versa.*/
void date_jd2ymd(julian_date *in,ymd_date *out);
void date_ymd2jd(ymd_date *in,julian_date *out);

/*Compute the number of seconds since midnight, Jan 1, 1900.*/
double date2sec(julian_date *date,hms_time *time);

/*Compute the date corresponding to this number of seconds 
since midnight, Jan 1, 1900.*/
void sec2date(double secs,julian_date *date,hms_time *time);

/*Get Julian Date (in fractional days since "creation"):*/
double date_getJD(ymd_date *in);

/*Get (ESA-Modified) Julian Day given conventional year and julian day.*/
int date_getMJD(julian_date *in);

/*Convert hour/minute/second to seconds in day; vice versa.*/
double date_hms2sec(hms_time *in);
void date_sec2hms(double sec,hms_time *out);


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

/*Writes YY&MM&DD, where sep==&*/
char * date_printY2Kdate(ymd_date *in,char sep,char *dest);

/*Writes YYYY&MM&DD, where sep==&*/
char *date_printDate(ymd_date *in,char sep,char *dest);

/*Writes HH&MM&SS.CCCC,
where sep==& (no spaces if sep=='\0')
where there are prec "C"'s. (no decimal if prec==0).*/
char *date_printTime(hms_time *in,int prec,char sep,char *dest);
