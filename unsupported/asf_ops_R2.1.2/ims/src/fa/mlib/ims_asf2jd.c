static char *sccs = "@(#)ims_asf2jd.c	5.1 17 Mar 1996";

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* asf time to julian date
 * Corey Porter
 * 12/18/95
 *
 * 2/6/96 - Modifed ASF date format.
 * D. Crichton
 *
 */

#include <ims_cpfort.h>

long yddd2jd(long iyear,long iday)
{
  long jmonth,kday,jd,i,j,k,iydays;

  j = k = jmonth = kday = 1;
  i = iyear;

  jd = k - 32075 + 1461 * (i + 4800 + (j - 14)/12)/4
       + 367 * (j - 2 - (j-14)/12*12)/12
       - 3 * ((i+4900+(j-14)/12)/100)/4
			 + iday - 1;
  
	return jd;
}

double asf2jd(char asftime[])
{
  double xjd, xsec, xfrac, t, utdeg;
  long   iyear, iday, ihour, imin, jd;
  int i;

  (void)
  sscanf(asftime,"%d-%dT%d:%d:%lf",&iyear,&iday,&ihour,&imin,&xsec);

/*  while(xsec >= 1) xsec /= 10; */

  if(iyear < 1950 || iyear > 2100)
  {
    printf("year %ld out of range\n",iyear);
    return -1;
  }

  jd = yddd2jd(iyear,iday);

  xjd = jd;
  xjd -= 0.5;

  xfrac = ((double)ihour) / 24.0
        + (((double)imin) / 60.0) / 24.0
        + (((double)xsec) / 3600.0) / 24.0;
  
  xjd += xfrac;

  return xjd;
}
