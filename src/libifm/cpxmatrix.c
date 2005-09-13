#include "asf.h"

#include "ifm.h"

complexFloat **cpxmatrix(int nrl, int nrh, int ncl, int nch)
{
	int i,nelem;
	complexFloat **m;

	nelem = nrh-nrl+1;
	m=(complexFloat **)MALLOC(nelem * sizeof(complexFloat *));
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(complexFloat *)MALLOC((unsigned)(nch-ncl+1)*sizeof(complexFloat));
		m[i] -= ncl;
	}
	return m;
}

void free_cpxmatrix(m,nrl,nrh,ncl,nch)
complexFloat **m;
int nrl,nrh,ncl,nch;
{
	int i;

	for(i=nrh;i>=nrl;i--) free((complexFloat*) (m[i]+ncl));
	free((complexFloat**) (m+nrl));
}
