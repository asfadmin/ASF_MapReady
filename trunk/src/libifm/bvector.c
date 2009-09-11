#include "asf.h"


/* function declaration */
void nrerror(char *fmt, ...);

unsigned char *bvector(nl,nh)
int nl,nh;
{
  unsigned char *v;

  v=(unsigned char *)MALLOC((nh-nl+1)*sizeof(unsigned char));
  return v-nl;
}

void free_bvector(v,nl,nh)
unsigned char *v;
int nl,nh;
{
  free((v+nl));
}
