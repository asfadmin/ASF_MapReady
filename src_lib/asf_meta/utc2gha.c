/*
	utc2gha.c:part of JPL's earthloc software.
	Translated to C by Orion Lawlor, ASF, 6/97

	utc2gha:
		Converts a universal time to an Hour Angle of Greenwich (gha).
	The universal time is expressed in year, 
c                                    
c*/
#include "asf.h"


#include "geolocate.h"

const double jd1900=2415020.0; /* Julian day for noon 1/1/1900 */

int getDaysInYear(int year);
int getDaysInYear(int year)
{
	if (((year%4)==0)&&(((year%100)!=0)||((year%400)==0)))
		return 366;
	else
		return 365;
}
double getJulianYear(int targetYear);
double getJulianYear(int targetYear)
{
	double totalDays=2442412.5; /*Start at 1975.*/
	int year;
	if (targetYear<1975)
		for (year=1975;year>targetYear;year--)
			totalDays-=getDaysInYear(year);
	else
		for (year=1975;year<targetYear;year++)
			totalDays+=getDaysInYear(year);
	return totalDays;
}

const double 
/*ta=99.6910 */
ta=99.691716
,tb=36000.7689,tc=0.0004;

double utc2gha (int year,int julianDay, int hour,int min, double sec)
{
	double greenwich_hour_angle; /*Greenwich Hour Angle.*/
	double fracDay,day,century,subDayDegrees;
	fracDay=julianDay+hour/24.0+min/1440.0+sec/86400.0;
	day=getJulianYear(year)+fracDay-jd1900;
	century=day/36525; /*Julian century, with respect to jd1900*/
	subDayDegrees=(360.0/86400.0)*(hour*3600.0+min*60.0+sec);
	greenwich_hour_angle=(ta+tb*century+tc*century*century+subDayDegrees);
	return fmod(greenwich_hour_angle,360.0);
}

