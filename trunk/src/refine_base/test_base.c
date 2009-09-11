#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"

int test_base(char *basefile, char *matfile, char *vecfile)
{
  int i, j;
  int m, n = 4;   /* m and n are rows and columns of matrix A */
  double sum, rms, errsum;
  double **A;
  double *b, *x, *Ax;
  FILE *fp;

  /* 
   * Part I:  Singular Value Decomposition of matrix A 
   */

  /* determine number of rows 'm' */
  if (!fileExist(matfile)) Exit("   test_base:  <A> dne");
  if (!fileExist(basefile)) Exit("   test_base:  <x> dne");
  if (!fileExist(vecfile)) Exit("   test_base:  <b> dne");
  m = fileNumLines(matfile);
  if (fileNumLines(vecfile) != m) 
     Exit("   test_base: Number of rows in matrix A and vector B do not match");

  /* establish matrix A and vector b */
  A   = dmatrix(1, m, 1, n);
  b   = dvector(1, m);
  x   = dvector(1, n);
  Ax  = dvector(1, m);

  /* read data */
/*  printf("Reading MatrixA from file %s\n",matfile);*/
  fp = FOPEN(matfile, "r");
  for (i = 1; i <= m; i++) { 
    for (j = 1; j <= n; j++) {
      fscanf(fp, "%lf", &A[i][j]); 
    }
  }
  fclose(fp);

/*  printf("Reading baseline from file %s\n",basefile);*/
  fp = FOPEN(basefile, "r");
  for (i = 1; i <= n; i++) { fscanf(fp, "%lf", &x[i]); }
  fclose(fp);

/*  printf("Reading vectorB from file %s\n",vecfile);*/
  fp = FOPEN(vecfile, "r");
  for (i = 1; i <= m; i++) { fscanf(fp, "%lf", &b[i]); }
  fclose(fp);

  /* multiply A * x = Ax */
  sum    = 0.0;
  errsum = 0.0;
  for (i = 1; i <= m; i++) {
    Ax[i] = 0.0;
    for (j = 1; j <= n; j++) {
      Ax[i] += A[i][j] * x[j];
    }
    sprintf (logbuf, "   %3d  b = %9.3f   A * x = %9.3f   diff = %9.3f  \n", i, b[i], Ax[i], b[i] - Ax[i]);
    if (!quietflag) printf(logbuf);
    if (logflag && !quietflag) printLog(logbuf);
    sum += (b[i] - Ax[i])*(b[i] - Ax[i]);
    errsum += (b[i] - Ax[i]);
  }
  rms = sqrt(sum/(double)(m));
  sprintf(logbuf, "\n   rms diff(b, A*x) = %f, avg diff(b, A*x) = %f\n\n", rms, errsum/(double)m);
  if (!quietflag) printf(logbuf);
  if (logflag && !quietflag) printLog(logbuf);

  return(0);
}


