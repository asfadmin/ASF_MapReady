#include "asf.h"

#include "ifm.h"

FComplex *cpxvector(int nl, int nh)
{
	FComplex *v;

	v=(FComplex *)MALLOC((unsigned) (nh-nl+1)*sizeof(FComplex));
	return v-nl;
}

void free_cpxvector(v,nl,nh)
FComplex *v;
int nl,nh;
{
	free((v+nl));
}
