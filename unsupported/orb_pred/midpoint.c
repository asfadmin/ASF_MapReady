/*
	Algorithm:	Calculate geographic coordinates of the mid point
			given the coordinates of the two end points.

	Date:		March 10, 1992.

	Developed by:	Shusun Li
*/

#include "asf.h"

static	int	already = 0;
static	double	*midpt;

double	*midpoint (p1, p2)
double	*p1, *p2;
{
	double	v1[3], v2[3], midv[3], vlength;
	double	temp1, temp2, temp3;

	if (!already) {
	  if ((midpt = (double *) malloc(2 * sizeof(double))) == NULL)
            {
	      fprintf (stderr, "Can't allocate space for Midpt\n");
	      exit(1);
 	    }
	  already = 1;
	}
	/*	calculate the x-y-z coordinates of vec1	*/
	v1[0] = cos(p1[0]*D2R) * cos(p1[1]*D2R);
	v1[1] = cos(p1[0]*D2R) * sin(p1[1]*D2R);
	v1[2] = sin(p1[0]*D2R);

	/*	calculate the x-y-z coordinates of vec2	*/
	v2[0] = cos(p2[0]*D2R) * cos(p2[1]*D2R);
	v2[1] = cos(p2[0]*D2R) * sin(p2[1]*D2R);
	v2[2] = sin(p2[0]*D2R);

	/*	calculate the sum of vec1 and vec2 to get mid-vector	*/
	midv[0] = v1[0] + v2[0];
	midv[1] = v1[1] + v2[1];
	midv[2] = v1[2] + v2[2];

	/* calculate the length of mid-vector	*/
	vlength = sqrt (midv[0]*midv[0] + midv[1]*midv[1] + midv[2]*midv[2]);

	/* normalize the mid-vector	*/
	midv[0] /= vlength;
	midv[1] /= vlength;
	midv[2] /= vlength;

	/*	calculate latitude and longitude of the mid-point	*/
	temp1 = asin(midv[2]);	/* latitude in radians	*/
	temp2 = midv[1]/cos(temp1);
	temp3 = midv[0]/cos(temp1);

	midpt[0] = temp1 * R2D;
	midpt[1] = atan2(temp2, temp3) * R2D;

	return (midpt);
}
