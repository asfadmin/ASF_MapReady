static char *sccs = "@(#)ims_asf2jday.c	5.1 17 Mar 1996";


/*
 * asf2jday
 * Corey Porter
 * 1/9/96
 *
 * 2/6/96 - Modifed ASF date format.
 * D. Crichton
 */

#include <stdio.h>

#include <ims_cpfort.h>

double asf2jday(char asftime[])
{
	double secs, tj, t1961 = 2437300.5;
	int    iyear, iday, ihour, imn, iqd, iaft;

	sscanf(asftime,"%d-%dT%d:%d:%lf",&iyear,&iday,&ihour,&imn,&secs);

	iqd  = ((iyear - 1961) / 4) * 1461;
	iaft = ((iyear - 1961) % 4) * 365;
	tj   = iqd + iaft + iday - 1 + (double)ihour/(double)24.0 +
				 (double)imn/(double)144.0 + secs / (double)86400 + t1961;

	if(iday == 1 && ihour == 0 && imn == 0 && secs == 0)
	{
		tj += 0.0001 / (double)(24 * 3600);
	}

	return tj;
}
