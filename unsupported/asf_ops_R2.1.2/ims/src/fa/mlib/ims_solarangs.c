static char *sccs = "@(#)ims_solarangs.c	5.2 25 Apr 1996";


#include <math.h>
#include <stdio.h>

/* solarangs
 * Corey Porter
 * 12/18/95
 */


#define RAD(x) (x / 57.29577)
#define ARAD(x) (x * 57.29577)

/*
#define RAD(x) (x)
#define ARAD(x) (x)
*/

extern double ims_fa_abs(double);



/*
 * Name: solarangs
 * Module Type: Subroutine
 * Language: C
 * Purpose: Give lat/lon and asf time, computes horizon angle
 *          and azimuth angle of the sun.  Approxinates to 1 degree.
 * Input parameters:
 * Name         Type         Def
 * afstime      char[]       time in asf format: yyyy:ddd:hh:mm:ss.sss
 * xlat         double      latitude
 * xlon          double      longitude
 * Output parameters:
 * horiz        long*        90  = zenith, 0 = horizon, < 0 = below horizon
 * iazmuth      long*        azmuth angle.  if sun is due south from input
 *                           lodation, then value = 180. if due west, 270.
 *                          due east = 90. etc.
 * Linking:
 *   asf2gha
 *  asf2jd
 *  sephem
 *  solazd
 *  -lm -lsunmath
 * History:
 *  Originally coded in FORTRAN 10/3/91, 
 *    Larry Stevens  larry@bobby.jpl.nasa.gov
 *  Translated to C 1/29/96, 
 *    Corey Porter cporter@impala.jpl.nasa.gov
 */

#include <ims_cpfort.h>

void ims_solarangs(
    char *asftime,
    double xlat,
    double xlon,
    long *horiz,
    long *iazmuth)
{
  double gha,v[3],ssv[10],e2s[3],u1[3],u2[3],xjd,cosang,azmuth,vmag;
  int i;


  gha = asf2gha(asftime);


  v[0] = cos(RAD(xlat)) * cos(RAD(xlon) + RAD(gha));
  v[1] = cos(RAD(xlat)) * sin(RAD(xlon) + RAD(gha)) * 6378;
  v[2] = sin(RAD(xlat)) * 6378;

  v[0] = v[0] * 6378;

  xjd = asf2jd(asftime);


  sephem(xjd,ssv);


  for(i=0;i<3;i++)
  {
    e2s[i] = ssv[i] - v[i];
  }

  vmag = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);

  for(i=0;i<3;i++)
  {
    u1[i] = v[i] / vmag;
  }

  vmag = sqrt(e2s[0]*e2s[0]+e2s[1]*e2s[1]+e2s[2]*e2s[2]);

  for(i=0;i<3;i++)
  {
    u2[i] = e2s[i] / vmag;
  }

  cosang = u1[0]*u2[0] + u1[1]*u2[1] + u1[2]*u2[2];

  *horiz = 90 - floor(ARAD(acos(cosang)) + 0.5);


  if(ims_fa_abs(xlat) > 89.9 || ims_fa_abs(*horiz) >= 90)
  {
    *iazmuth = 0.0;
  }
  else
  {
    vmag = sqrt(ssv[0]*ssv[0]+ssv[1]*ssv[1]+ssv[2]*ssv[2]);
    for(i=0;i<3;i++)
    {
      u2[i] = ssv[i] / vmag;
    }

    *iazmuth = floor(solazd(u2,u1) + 0.50);


    if(*iazmuth >= 360) *iazmuth -= 360;

  }
}
