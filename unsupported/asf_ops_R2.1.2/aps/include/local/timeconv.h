#ifndef _TIMCONV_
#define _TIMCONV_

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   	timconnvh.h

Description:	Prototypes for time lib functions

Creator:    	Gus Faist/Ron Green

Notes:		
This .h file contains prototypes for the "c" time conversion routines in the
time conversion library libtimes.a - GAF 08/02/95

==============================================================================*/
#pragma ident	"@(#)timeconv.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.timeconv.h"

/* C Function Return Codes: NOTE error codes must be negative */
#define TC_VALID_YEAR_ERR	-1		/* invalid year value */
#define TC_VALID_DOY_ERR	-6		/* invalid doy value */
#define TC_VALID_HOUR_ERR	-9		/* invalid hour value */
#define TC_VALID_MIN_ERR	-12		/* invalid minutes value */
#define TC_VALID_SEC_ERR	-15		/* invalid seconds value */
#define TC_VALID_TIME_ERR	-99		/* time format error */

/* external FORTRAN declarations */

extern double tc_utc2et_ (double*) ;
extern double tc_et2utc_ (double*) ;
extern void tc_julian_ (double*, double*) ;

/* C Function Prototypes */

int tc_asf2et(char *asftime, double *et) ;
int tc_et2asf(double et, char *asftime) ;

int tc_leapyr(int year)  ;
int tc_validate_asf_datetime(char *asftime) ;
int tc_parse_asftime(char *asftime, int *year, int *decade, int *day, 
                     int *hour, int *min, int *sec, int *msec) ;

int tc_doy2cal(int year, int day, int *mon, int *mday) ;
int tc_cal2doy(int year, int mon, int mday, int *doy) ;

char* tc_id_asftime(char *id) ;

int tc_asf2odl(char *asftime, char *odltime) ;
int tc_odl2asf( char *odltime, char *asftime );

int tc_asf2csa(char *asftime, char *csatime) ;
int tc_csa2asf(char *csatime, char *asftime) ;

int tc_asf2esa(char *asftime, char *esatime) ;
int tc_esa2asf(char *esatime, char *asftime) ;
int tc_esadaytime2asf(char *esa_daytime, char *asftime) ;

int tc_asf2yyyymmdd(char *asftime, char *datestr) ;
int tc_yyyymmdd2asf(char *datestr, char *asftime) ;
int tc_yyyymmdd_check(char *time) ;

int tc_asf_add_ndays(char *asftime, double ndays, char *asftime_plusndays) ;

int tc_systime2asf(char *curr_time) ;
int tc_systime2yr_date(char *year, char *date) ;
int tc_systime2yyyycddd(char *datestr) ;

int tc_yyyymmdd_hhmmss2asf(char *j1date, char *j1time, char *asftime) ;

int tc_cal2julian(int year, int month, int day) ;
int tc_asf2julian(char *asftime, double *julian_day) ;
int tc_julian2asf(double *julian_day, char *asftime) ;

int tc_et_ASF_datetime_diff(
    char *strttime, char *stoptime, double *deltatime) ;

int tc_time_pad( char *strttime, char *stoptime, float minutes,
    char *padded_strttime, char *padded_stoptime) ;


int tc_j1date2asf       (char *j1date, char *asftime) ;
int tc_j1date_check     (char *jidate) ;

#endif /* _TIMCONV_ */
