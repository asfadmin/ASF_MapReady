#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);

int *ivector(nl,nh)
int nl,nh;
{
	int *v;

	v=(int *)MALLOC((unsigned) (nh-nl+1)*sizeof(int));
	return v-nl;
}

void free_ivector(v,nl,nh)
int *v,nl,nh;
{
	free((v+nl));
}
