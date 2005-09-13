#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);

float **
matrix(int nrl, int nrh, int ncl, int nch)
{
   int i,nelem;
	float **m;

	nelem = nrh-nrl+1;
	m=(float **)MALLOC(nelem * sizeof(float*));
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(float *)MALLOC((unsigned)(nch-ncl+1) * sizeof(float));
		m[i] -= ncl;
	}
	return m;
}

void free_matrix(m,nrl,nrh,ncl,nch)
float **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((float*) (m[i]+ncl));
	free((float**) (m+nrl));
}
