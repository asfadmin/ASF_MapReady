static char *sccs = "@(#)ims_solazd.c	5.2 25 Apr 1996";


#define RAD(x) (x / 57.29577)
#define ARAD(x) (x * 57.29577)


#include <math.h>

#include <ims_cpfort.h>

extern double ims_fa_abs(double);

double solazd(double p1[3],double p2[3])
{
	double p3[3],u12[3],u23[3],xvec[3],d,adeg,dotmag,xmag;
	int    i;

	/* p3 is the north pole :-) */
	p3[0] = 0;
	p3[1] = 0;
	p3[2] = 1.0;
	adeg  = 0;

	ucross(u12,p2,p1);

	d = dot(p3,u12,3);

	if(d >= 0.0) i =  1;
	else         i = -1;

	ucross(u23,p2,p3);

	ucrosm(&xmag,xvec,u12,u23);

	dotmag = dot(u12,u23,3);

	adeg = i * ims_fa_abs(ARAD(atan2(xmag,dotmag)));

	if(adeg < 0) adeg += 360;

	return adeg;
}
