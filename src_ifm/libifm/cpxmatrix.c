#include "asf.h"

#include "ifm.h"

FComplex **cpxmatrix(int nrl, int nrh, int ncl, int nch)
{
	int i,nelem;
	FComplex **m;

	nelem = nrh-nrl+1;
	m=(FComplex **)MALLOC(nelem * sizeof(FComplex *));
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(FComplex *)MALLOC((unsigned)(nch-ncl+1)*sizeof(FComplex));
		m[i] -= ncl;
	}
	return m;
}

void free_cpxmatrix(m,nrl,nrh,ncl,nch)
FComplex **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((FComplex*) (m[i]+ncl));
	free((FComplex**) (m+nrl));
}
