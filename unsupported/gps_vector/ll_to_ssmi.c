/******************************************************************************

NAME: 	ll_to_ssmi

DESCRIPTION:	Convert the longitute and latitude into SSM/I projection.

HISTORY:	Shusun Li.
****************************************************************************/

#include	<stdio.h>
#include	<math.h>

void ll_to_ssmi (alat, along, x, y)
double	alat, along, *x, *y;
{
	int	ii;
	double	t[2], cm, rho;
	double	cdr, re, e2, e, pi, slat, sn, xlam, rlat;

	/* initialization */
	pi = atan(1.0) * 4.0;
	cdr = 180. / pi;	/* conversion const from degrees to radians */
	re = 6378.273;		/* radius of the earth, unit km */
	e2 = 0.006693883;	
	e = sqrt(e2);		/* eccentricity of the earth */
	slat = 70.0;		/* standard parallel for ssmi */
	xlam = -45.0;
	sn = 1.0;		/* set cnostant for north hemisphere */

	if (alat < 0.) {	/* if south hemisphere */
		sn = -1.0;
		xlam = 0.0;
	}

	alat = sn * alat;
	along = sn * along;

	/* computer x and y in grid coordinates */
	rlat = alat;

	for (ii = 0; ii < 2; ii++) {
		if (ii == 1)
			rlat = slat;
		t[ii] = tan((pi/4.0) - (rlat/(2.0 * cdr))) / 
			pow (((1.0 - e * sin(rlat/cdr)) /
			(1.0 + e * sin(rlat/cdr))), (e/2.0));
	}
	cm = cos(slat/cdr) / sqrt(1.0 - e2 * (sin(slat/cdr) * sin(slat/cdr)));
	rho = re * cm * t[0] / t[1];
	*x = rho * sn * sin((along-xlam)/cdr);
	*y = -1.0 * (rho * sn * cos((along-xlam)/cdr));
	return;
}
