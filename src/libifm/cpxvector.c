#include "asf.h"

#include "ifm.h"

complexFloat *cpxvector(int nl, int nh)
{
	complexFloat *v;

	v=(complexFloat *)MALLOC((unsigned) (nh-nl+1)*sizeof(complexFloat));
	return v-nl;
}

void free_cpxvector(v,nl,nh)
complexFloat *v;
int nl,nh;
{
	free((v+nl));
}
