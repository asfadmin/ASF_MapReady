static char *sccs = "@(#)ims_vector.c	5.1 17 Mar 1996";


#include <math.h>

/* useful vector utility things
 * Corey Porter 
 * 12/18/95
 */

#include <ims_cpfort.h>

double dot(double a[],double b[],int n)
{
  double ret = 0;
  int i;

  for(i=0;i<n;i++)
  {
    ret += a[i] * b[i];
  }

  return ret;
}

void ucross(double x[],double p1[],double p2[])
{
  x[2] = p1[0] * p2[1] - p1[1] * p2[0];
  x[0] = p1[1] * p2[2] - p1[2] * p2[1];
  x[1] = p1[2] * p2[0] - p1[0] * p2[2];
}

void ucrosm(double *s,double x[],double p1[],double p2[])
{
	double d1,d2,d3;

	x[2] = p1[0] * p2[1] - p1[1] * p2[0];
	x[0] = p1[1] * p2[2] - p1[2] * p2[1];
	x[1] = p1[2] * p2[0] - p1[0] * p2[2];

	d1 = x[0];
	d2 = x[1];
	d3 = x[2];

	*s = sqrt(d1*d1 + d2*d2 + d3*d3);

	if(*s > 0)
	{
		x[0] /= *s;
		x[1] /= *s;
		x[2] /= *s;
	}
}

double d_sign(double a, double b)
{
  double x = (a >= 0 ? a : - a);
  return( b >= 0 ? x : -x);
}
 
double dmod(double a,double b)
{
  double quotient;
  if((quotient = a/b) >= 0)
  {
    quotient = floor(quotient);
  }
  else
  {
    quotient = -floor(-quotient);
  }
  return(a - b * quotient);
}

