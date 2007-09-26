#include "matrix.h"
#include "asf.h"

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

void free_matrix(float **m,int nrl,int nrh,int ncl,int nch)
{
	int i;

	for(i=nrh;i>=nrl;i--) free((float*) (m[i]+ncl));
	free((float**) (m+nrl));
}

void 
matrix_multiply(float **a, float **b, float **c, int r1, int c1, int c2)
{
  int i, j, k;
  float sum;
  
  for (i = 1; i <= r1; i++) {
    for (j = 1; j <= c2; j++) {
      for (k = 1, sum = 0.0; k <= c1; k++) {
        sum += a[i][k]*b[k][j];
      }
      c[i][j] = sum;
    }
  }

  return;
}
