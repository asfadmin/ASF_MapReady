#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);


double *dvector(int nl, int nh)
{
	double *v;

	v=(double *)MALLOC((unsigned) (nh-nl+1)*sizeof(double));
	return v-nl;
}

void free_dvector(v,nl,nh)
double *v;
int nl,nh;
{
	free((v+nl));
}
