static char *sccs = "@(#)ims_asf2gha.c	5.1 17 Mar 1996";

#include <math.h>

/* asf time to gha time
 * Corey Porter
 * 12/18/95
 */

#include <ims_cpfort.h>

double asf2gha(char asftime[])
{
  double gha,xsec,xfrac,xjd,t,utdeg;
  long        iyear,iday,ihour,imin,jd;
  int         i;

  xjd = asf2jd(asftime);

  xfrac = xjd - floor(xjd) + 0.5;

  if(xfrac >= 1.0)
  {
    xfrac -= 1.0;
  }

  t = xjd - 2415020.0;
	t /= 36525.0;

  utdeg = xfrac * 360.0;

  gha = 99.691 + 36000.7689 * t + 0.0004 * t*t + utdeg;

  gha = dmod(gha,(double)360);

  return gha;
}
