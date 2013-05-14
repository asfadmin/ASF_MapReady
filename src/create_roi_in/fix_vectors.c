#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "seasat.h"

int getDaysInYear(int year);
double getJulianYear(int targetYear);
double utc2gha (int year,int julianDay, int hour,int min, double sec);
void ecitoecef( double gha,
		double x, double y, double z, double x_vel, double y_vel, double z_vel, 
  		double *output_X_pos, double *output_Y_pos, double *output_Z_pos, 
		double *output_X_vel, double *output_Y_vel, double *output_Z_vel);

# define M_PI		3.14159265358979323846	/* pi */


void fix_state_vectors(int year, int julianDay, int hour, int min, double sec)
{
   double time, x, y, z, xv, yv, zv;
   double gha;
   double otime, ox, oy, oz, oxv, oyv, ozv;
   FILE *fp, *fpo;
   
   fp = fopen("propagated_state_vector.txt","r");
   fpo = fopen("fixed_state_vector.txt","w");
   while (fscanf(fp,"%lf %lf %lf %lf %lf %lf %lf",&time,&x,&y,&z,&xv,&yv,&zv)==7)
     {
          otime = time*60.0;   /* convert to seconds */
	  gha = utc2gha(year,julianDay,hour,min,sec+otime);
          ecitoecef(gha, x, y, z, xv, yv, zv, &ox, &oy, &oz, &oxv, &oyv, &ozv);
	  fprintf(fpo,"%14.8lf %14.8lf %14.8lf %14.8lf %14.9lf %14.9lf %14.9lf \n",
	  	otime, ox*1000.0, oy*1000.0, oz*1000.0, oxv*1000.0, oyv*1000.0, ozv*1000.0);
      }
    fclose(fp);
    fclose(fpo);
}

void ecitoecef( double gha,
		double x, double y, double z, double x_vel, double y_vel, double z_vel, 
  		double *output_X_pos, double *output_Y_pos, double *output_Z_pos, 
		double *output_X_vel, double *output_Y_vel, double *output_Z_vel)
{
	double angularVelocity, ghaRads, phi, phiVel, radius;

	angularVelocity=(366.225/365.225)*2.0*M_PI/86400.0;
	ghaRads=gha*M_PI/180.0;

	radius = sqrt(x*x + y*y);
	phi = atan2(y,x);
	phi-=ghaRads;
	*output_X_pos = cos(phi)*radius;
	*output_Y_pos = sin(phi)*radius;
	*output_Z_pos = z;

	radius = sqrt(x_vel*x_vel + y_vel*y_vel);
	phiVel = atan2(y_vel,x_vel);
	phiVel-=ghaRads;

	*output_X_vel = cos(phiVel)*radius+angularVelocity*(*output_Y_pos);
	*output_Y_vel = sin(phiVel)*radius-angularVelocity*(*output_X_pos);
	*output_Z_vel = z_vel;
}

const double jd1900=2415020.0; /* Julian day for noon 1/1/1900 */
const double ta=99.691716,tb=36000.7689,tc=0.0004;

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

int getDaysInYear(int year)
{
        if (((year%4)==0)&&(((year%100)!=0)||((year%400)==0)))
                return 366;
        else
                return 365;
}
