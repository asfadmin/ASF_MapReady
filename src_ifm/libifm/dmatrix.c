#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);

double **dmatrix(int nrl, int nrh, int ncl, int nch)
{
	int i,nelem;
	double **m;

	nelem = nrh-nrl+1;
	m=(double **)MALLOC(nelem * sizeof(double*));
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(double *)MALLOC((unsigned)(nch-ncl+1) * sizeof(double));
		m[i] -= ncl;
	}
	return m;
}

void free_dmatrix(m,nrl,nrh,ncl,nch)
double **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((double*) (m[i]+ncl));
	free((double**) (m+nrl));
}
