#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);

int **imatrix(int nrl, int nrh, int ncl, int nch)
{
	int i,nelem;
	int **m;

	nelem = nrh-nrl+1;
	m=(int **)MALLOC(nelem * sizeof(int*));
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(int *)MALLOC((unsigned)(nch-ncl+1) * sizeof(int));
		m[i] -= ncl;
	}
	return m;
}

void free_imatrix(m,nrl,nrh,ncl,nch)
int **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((int*) (m[i]+ncl));
	free((int**) (m+nrl));
}
