/*****************************************************************************
	NAME:  C_DEGDMS

	PURPOSE:  To convert total degrees, total minutes, or total seconds to 
		  packed degress,minutes,seconds.

	PROGRAM HISTORY:
	PROGRAMMER	DATE	REASON
	----------	----	------
	B.Ailts		9/87	Original development 
	B. Ailts	12/87	Place bridge routine is seperate source file
				change include file directory specifications
				replace DESC arguements with char. strings
	B. Ailts	05/88	Change return status to E_SUCC and E_FAIL
				Changed newlas.h to worgen.h
				Standardized error messages
				Changed the name from c_deg2dms to c_degdms
	D. Steinwand 	07/89	Corrected some numerical problems--some
				angles were comming out as xxx060000.0
				(some degrees, 60 minutes, etc)
				Added find_deg, find_min, find_sec routines
	D. Etrheim	07/90	Standardized error message handling
 	S. Nelson	08/92	Changed "static" statements on find_deg,
				find_min, find_sec functions

	COMPUTER HARDWARE AND/OR SOFTWRE LIMITATIONS:
		Must be run under TAE

	PROJECT:	LAS

	ALGORITHM:
       	Receive an angle in seconds, minutes, or degrees
	Convert it to DMS.  
	The angle is then checked to be sure it is within the limits 
	of its use(LAT, LON, or DEGREES).

	ALGORITHM REFERENCES		none

******************************************************************************/
#include "asf.h"




#include "worgen.h"

FUNCTION int find_deg(double angle);
FUNCTION int find_min(double angle);
FUNCTION double find_sec(double angle);

int c_degdms(deg,dms,code,check)
double *deg;
double *dms;
char *code;
char *check;

{ /* degdms */

double tsec;
double MAXDMS;
double MAXMIN = 60060;
double MAXSEC = 60;
double MINDMS;
double MINMIN = -60060;
double MINSEC = -60;
double tempmin;
double tempsec;

int tdeg;
int tmin;
int sign;
int status = E_SUCC;

if (strcmp(check,"LAT") == 0)
   {
   MAXDMS = 90000000;
   MINDMS = -90000000;
   }
else if (strcmp(check,"LON") == 0)
   {
   MAXDMS = 180000000;
   MINDMS = -180000000;
   }
else
   {
   MAXDMS = 360000000;
   MINDMS = 0;
   }

if (strcmp(code,"DMS") != 0)
   {
   if (strcmp(code,"MIN") == 0)
      {
      *deg = *deg / 60.0;
      }
   if (strcmp(code,"SEC") == 0)
      {
      *deg = *deg / 3600.0;
      }
   tdeg = (int)find_deg(*deg);
   tmin = (int)find_min(*deg);
   tsec = find_sec(*deg);
   sign = 1;
   if (tdeg < 0) sign = -1;
   tdeg = abs(tdeg);
   *dms = ((tdeg * 1000000) + (tmin * 1000) + tsec) * sign;

/*  Check to be sure coordinate is valid
----------------------------------------*/
   if ((*dms > MAXDMS) || (*dms < MINDMS))
      {
      c_errmsg("Invalid coordinate value","degdms-coord",NON_FATAL);
      status = E_FAIL;
      }
   }
else
   {
   *dms = *deg;

/*  Check to be sure coordinate is valid
----------------------------------------*/
   if ((*dms > MAXDMS) || (*dms < MINDMS))
      {
      c_errmsg("Invalid coordinate value","degdms-coord",NON_FATAL);
      status = E_FAIL;
      }

   if ((strcmp(check,"LAT") != 0) && (strcmp(check,"LON") !=0))
      {
      if (*dms <= 0)
         {
         c_errmsg("Invalid coordinate value","degdms-coord",NON_FATAL);
         status = E_FAIL;
         }
      }

/*  parse out the minutes value from DMS and check against MAXMIN
-----------------------------------------------------------------*/
   tempmin = *dms - (((int)(*dms/1000000)) * 1000000);
   if ((tempmin > MAXMIN) || (tempmin < MINMIN))
      {
      c_errmsg("Invalid coordinate value","degdms-coord",NON_FATAL);
      status = E_FAIL;
      }

/*  parse out the seconds value from DMS and check against MAXSEC
-----------------------------------------------------------------*/
   tempsec = *dms - (((int)(*dms/1000)) * 1000);
   if ((tempsec > MAXSEC) || (tempsec < MINSEC))
      {
      c_errmsg("Invalid coordinate value","degdms-coord",NON_FATAL);
      status = E_FAIL;
      }
   }

return(status);
} /* degdms */

/****************************************************************************
NAME:			find_deg, find_min, find_sec	

PURPOSE:  Extracts deg, min, or sec portions of an angle

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 5/89	D. Steinwand  CSB      LAS 5.0 (original) Development

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Extract portion of angle
	Return
*****************************************************************************/
FUNCTION  int find_deg(angle)
double angle;			/* Angle in total degrees */
{
int sign;			/* Sign of angle */
int deg;			/* Degrees portion of angle */
int minute;			/* Minutes portion of angle */
double sec;			/* Seconds portion of angle */
deg = (int)angle;
sign = 1;
if (deg < 0) sign = -1;
deg = fabs(angle);
minute =  (fabs(angle) - deg) * 60.0;
sec =  (((fabs(angle) - deg) * 60.0) - minute) * 60.0;
if (sec >= 59.999) minute++;
if (minute >= 60) deg++;
deg *= sign;
return(deg);
}

FUNCTION  int find_min(angle)
double angle;			/* Angle in total degrees */
{
double sec;			/* Seconds portion of angle */
int minute;			/* Minutes portion of angle */
angle = fabs(angle);
angle -= (int)angle;
minute = angle * 60.0;
sec = ((angle * 60.0) - minute) * 60.0;
if (sec > 59.999) minute++;
if (minute >= 60) minute -= 60;
return(minute);
}

FUNCTION  double find_sec(double angle)
{
int temp_angle;
angle = fabs(angle);
angle -= (int)angle;
angle *= 60.0;
angle -= (int)angle;
angle *= 60.0;
if (angle > 59.999) angle -= 60.0;
temp_angle = angle * 1000;	/* Truncate to 0.001 sec */
angle = temp_angle / 1000.0;
return(angle);
}
