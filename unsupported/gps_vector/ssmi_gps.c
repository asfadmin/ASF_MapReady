/*****************************************************************************

FUNCTION:	change the SSM/I projection into gps locations.

AUTHOR  :	Shusun Li.

*******************************************************************************/
#include	<stdio.h>

int	ssmi_gps (x, x0, dx)
double	x, x0, dx;
{
	int	n;
        double zz;

	if (dx == 0.) {
		fprintf (stderr, "dx = 0. in ssmi_gps.\n");
		exit (1);
	}
        

	zz = ((x - x0) / dx + 0.51);
        n = (int) zz;
/*	printf("x = %lf\tx0 = %lf\tZZ = %lf\tn = %d\n",x,x0,zz,n); */
	return (n);
}
