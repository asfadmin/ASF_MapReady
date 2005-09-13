/*****************************************************************************
	NAME:  DECDEG

	PURPOSE:  To convert angles to total degrees.

	PROGRAM HISTORY:
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.Steinwand		Original development
	B.Ailts		9/87	Convert to C and add
				UNIX bridge routines
	B. Ailts	12/87	Place bridge routines in a seperate file
				replace DESC arguments with char. arrays
				change include file directory specifications
	B. Ailts	05/87	Changed return status to E_SUCC and E_FAIL
				Changed newlas.h to worgen.h
				Standardized error messages
	D. Etrheim	07/90	Standardized error message handling

	COMPUTER HARDWARE AND/OR SOFTWRE LIMITATIONS:
		Must be run under TAE

	PROJECT:	LAS

	ALGORITHM:	
       	Recieve an anlge in seconds, minutes, or DMS
	Convert it to total degrees.
	The angle is then checked to be sure it is within the limits of 
	its use(LAT, LON, or DEGREES).

	ALGORITHM REFERENCES		none

******************************************************************************/

#include "asf.h"



#include "worgen.h"

int c_decdeg(angle,coform,type)
double *angle;
char *coform;
char *type;

{ /* main */

float second;
float upper;
float lower;

int degree;
int minute;
int status;
short sign;

if (strncmp(type,"LAT",3) == 0) 
   { /* LAT */
   upper = 90.0;
   lower = -90.0;
   } /* LAT */
else if (strncmp(type,"LON",3) == 0)
   { /* LON */
   upper = 180.0;
   lower = -180.0;
   } /* LON */
else
   {
   upper = 360.0;
   lower = 0.0;
   }

if (strncmp(coform,"MIN",3) == 0)
   {
   *angle = *angle / 60.0;
   }
else if (strncmp(coform,"SEC",3) == 0)
   {
   *angle = *angle / 3600.0;
   }
else if (strncmp(coform,"DMS",3) == 0)
   { /* DMS */
   if (*angle < 0)
      {
      sign = -1;
      *angle = *angle * -1;
      }
   else
      {
      sign = 1;
      }
   degree = *angle / 1000000;
   *angle = *angle - (degree * 1000000);
   minute = *angle/1000;
   second = *angle - (minute * 1000);
   *angle = sign * (degree + (minute/60.0) + (second/3600.0));
   } /* DMS */

if ((*angle > upper) || (*angle < lower))
   {
   c_errmsg("Illegal coordinate value","decdeg-coor",NON_FATAL);
   status = E_FAIL;
   }
else
   {
   status = E_SUCC;
   }

return(status);
} /* main */
