static char sccsid_minpath_c[] =
    "@(#)minpath.c	1.2 96/04/09 19:13:29";

/* float minpath(coarse,fine,a,twod) ------------------

  done by quyen dinh nguyen
  7/7/89
  minpath.f --- to calculate the min of the path, given
                by the equation:
                  path = coarse+fine+ az(z+twod)
                where z can be from -1/2 to 1/2
 */

#include <math.h>

float minpath(coarse,fine,a,twod)
float coarse,fine,a,twod;
{
     float xpos,xneg;
     float xhalf,xmin;
     float zpeak,xpeak;

     zpeak = -twod/2.0;
     if(fabs(zpeak) > 0.5) zpeak = 0.0;
     xpeak = coarse+fine + a*zpeak*(zpeak + twod);
     xpos = coarse+fine + 0.5*a*(0.5+twod);
     xneg = coarse+fine + (-0.5)*a*((-0.5) + twod);

     xhalf = xpos < xneg ? xpos : xneg;
     xmin =  xpeak < xhalf ? xpeak : xhalf ;
     return (xmin);
}
