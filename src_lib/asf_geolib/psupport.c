/*******************************************************************************
NAME			      PSUPPORT

PURPOSE	     Package-specific support routines

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Nov, 1990	Original Development

*******************************************************************************/
#include "asf.h"
#include "cproj.h"

/* Print header with projection name 
 ----------------------------------*/
void report_type(text)
char text[75];		/* Projection name */
{
printf("\n%s PROJECTION PARAMETERS:\n",text);
return;
}

/* Report the string (text) with the numeric value (val) 
 -------------------------------------------------------*/
void report_gen(val, text)
double val;
char *text;
{
printf(" %s =    %f\n", text, val);
return;
}

/* Reports a TEXT = DMS value parameter 
 ---------------------------------*/
void report_deg(angle, text) 
double angle;
char *text;
{

/* Convert from radians to degrees and report angle in decimal degrees
  -------------------------------------------------------------------*/
angle *= 57.2957795131;
printf(" %s = %12.7f degrees\n", text, angle);
return;
}

/* Reports an ellipsoidal earth model 
 ------------------------------------*/
void report_spheroid(major, e2)
double major;
double e2;
{

if(e2 == 0)
	{
	report_rad(major);
	return;
	}
else
	{
	printf(" Semi-major axis of ellipsoid  =    %f meters\n", major);
	if(e2 < 1)
	   printf(" Eccentricity squared          =    %.12f\n", e2);
	else
	   printf(" Semi-minor axis               =    %f\n", e2);
	}
return;
} 

/* Reports a spherical earth model
 --------------------------------*/
void report_rad(double radius)
{
printf(" Radius of Sphere              =    %f meters\n", radius);
return;
}

/* Report projection zone code
 ----------------------------*/
void report_zone(zone)
int zone;
{
printf(" Zone Code = %d\n", zone);
return;
}

/* Produce a line-feed for nicely spaced output
 ---------------------------------------------*/
void report_lf()
{
printf("\n");
return;
}
