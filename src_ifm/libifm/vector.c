#include "asf.h"

#include "ifm.h"

float *
alloc_vector(int nl, int nh)
{
   float *v;

   v=(float *)MALLOC((unsigned) (nh-nl+1)*sizeof(float));
   return v-nl;
}


void free_vector(float *v, int nl, int nh)
{
   free((char*) (v+nl));
}
