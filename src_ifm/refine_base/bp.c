/******************************************************************************
NAME: bp

SYNOPSIS: bp <matrix> <vector> <newbase>

DESCRIPTION:
        Bp is called by refine_base, and only refine_base.

           Bp (solve Baseline Problem) is used during baseline
        refinement to convert the output of genab into an
        actual baseline file (newbase), suitable for use with
        deramp

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1997  Orion Lawlor,   ASF 

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   bp:  solve the baseline problem					    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "ifm.h"
#include "ddr.h"
#include "asf_meta.h"

/* local function declaration */
int get_matrix_rows(char *, char *);

int bp(char *matfile, char *vecfile, char *newbase)
{
  int i, j=0;
  int m, n = 4;   /* m and n are rows and columns of matrix A */
  float sum, rms;
  float **A, **Acp, **At; 
  float **U, **Ut;
  float **V, **Vt;
  float **S, **St, **Si;
  float *Sv;
  float *b, *bT, *db, *x, *dx, *x0;
  float *Utb, *SiUtb, *VSiUtb;
  float **UUt, **VVt, **VtV, **US, **SVt, **USVt;
  FILE *fp;

  /* 
   * Part I:  Singular Value Decomposition of matrix A 
   */
/*  printf("Beginning singular value decomposition of matrix A...\n");*/

  /* determine number of rows 'm' */
  m = get_matrix_rows(matfile,vecfile);

  /* establish matrix A and vector b */
  b = alloc_vector(1, m);
  db = alloc_vector(1, m);
  bT = alloc_vector(1, m);
  x = alloc_vector(1, n);
  dx = alloc_vector(1, n);
  x0 = alloc_vector(1, n);
  A = matrix(1, m, 1, n);
  Acp = matrix(1, m, 1, n);
  At = matrix(1, n, 1, m);

  /* establish decomposition matrices */
  U  = matrix(1, m, 1, n);
  Ut = matrix(1, n, 1, m);
  S  = matrix(1, n, 1, n);
  St = matrix(1, n, 1, n);
  Si = matrix(1, n, 1, n);
  V  = matrix(1, n, 1, n);
  Vt = matrix(1, n, 1, n);

  /* establish product matrices */
  UUt  = matrix(1, n, 1, n);
  VVt  = matrix(1, n, 1, n);
  VtV  = matrix(1, n, 1, n);
  US   = matrix(1, m, 1, n);
  SVt  = matrix(1, n, 1, n);

  /* establish SVD product matrices */
  USVt = matrix(1, m, 1, n);

  /* vector version of diagonal matrix S */
  Sv = alloc_vector(1, m);

  /* vector products */
  Utb = alloc_vector(1, n);
  SiUtb = alloc_vector(1, n);
  VSiUtb = alloc_vector(1, n);

  /* read matrix and vector from input files */
  fp = FOPEN(matfile,"r");
  for (i = 1; i <= m; i++) { 
    for (j = 1; j <= n; j++) {
      fscanf(fp, "%f", &A[i][j]); 
    }
  }
  fclose(fp);

  fp = FOPEN(vecfile, "r");
  for (i = 1; i <= m; i++) 
    fscanf(fp, "%f", &b[i]); 
  fclose(fp);

  /* copy A into Acp */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      Acp[i][j] = A[i][j];
    }
  }

  /* transpose A into At */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      At[j][i] = A[i][j];
    }
  }

  /* NR fn to decompose A = U x S x Vt, where U is written into A */
  svdcmp(A, m, n, Sv, V);

  /* copy Sv into the diagonal of S and St */
  for (i = 1; i <= 4; i++) 
    St[i][i] = S[i][i] = Sv[i]; 

  /* copy A into U where it belongs, copy Acp back into A */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      U[i][j] = A[i][j];
      A[i][j] = Acp[i][j];
    }
  }
  
  /* establish Ut and Vt */
  for (i = 1; i <= m; i++) {
    for (j = 1; j <= n; j++) {
      Ut[j][i] = U[i][j];
    }
  }

  for (i = 1; i <= n; i++) {
    for (j = 1; j <= n; j++) {
      Vt[j][i] = V[i][j];
    }
  }

  /* check that SVD of A == A */
  matrix_multiply(U, S, US, m, n, n);
  matrix_multiply(US, Vt, USVt, m, n, n);
  for (i = 1; i <= m; i++){
    for (i = 1; i <= m; i++){
      if (fabs(A[i][j] - USVt[i][j]) > 1e-12) {
        Exit("   reconstruction of A from SVD failed");
      }
    }
  }

  /* invert S into Si, automatically fixing small singular values */
  for (i = 1; i <= n; i++) {
    if (fabs(S[i][i]) < 0.0) { 
      Exit("svdcmp() found a negative singular value"); 
    }
    if (S[i][i] < 1e-6) {
      printf("   singular value %d = %f; auto-set inverse to zero\n", i, S[i][i]);
      Si[i][i] = 0.0;
    }
    else { 
      Si[i][i] = 1.0 / S[i][i]; 
    }
  }

  /* breathe sigh of relief having gotten through SVD */
/*  printf("\nSVD of A is ok\n\n");*/


  /*
   * Part II:  Solve for n-vector x0
   */

  /* multiply matrix Ut x vector b = vector Utb */
  for (i = 1; i <= n; i++) {
    for (j = 1, sum = 0.0; j <= m; j++) {
      sum += Ut[i][j] * b[j];
    }
    Utb[i] = sum;
  }

  /* multiply matrix Si x vector Utb = vector SiUtb */
  for (i = 1; i <= n; i++) {
    SiUtb[i] = Si[i][i] * Utb[i];
  }

  /* multiply matrix V x vector SiUtb = vector VSiUtb */
  for (i = 1; i <= n; i++) {
    for (j = 1, sum = 0.0; j <= n; j++) {
      sum += V[i][j] * SiUtb[j];
    }
    VSiUtb[i] = sum;
  }

  /* copy VSiUtb into x0 */
  for (i = 1; i <= n; i++) { 
    x0[i] = VSiUtb[i]; 
  } 

  /* calculate A x x0 */
  for (i = 1; i <= m; i++) {
    for (j = 1, sum = 0.0; j <= n; j++) {
      sum += A[i][j] * x0[j];
    }
    bT[i] = sum;
  }

  /*print_vector(bT, 1, m, "b check, compare with ...");
  print_vector(b, 1, m, "b");
  print_vector(x0, 1, n, "x0");*/

  for (i = 1, sum = 0.0; i <= m; i++) {
    sum += (bT[i] - b[i])*(bT[i] - b[i]);
  }
  rms = sqrt(sum/(float)(m));
  if (!quietflag) printf("   RMS of b-reconstructed and b = %f\n\n", rms);

  /* test for sign of deltas */
  
  printf("   New Baseline:  Normal: %f, delta: %f\n"
         "                  Parallel: %f, delta: %f\n\n", x0[1], x0[2], x0[3], x0[4]);
  if (logflag) {
    sprintf(logbuf,"   New Baseline:  Normal: %f, delta: %f\n"
                   "                  Parallel: %f, delta: %f\n\n", x0[1], x0[2], x0[3], x0[4]);
    printLog(logbuf);
  }

  fp = FOPEN(newbase,"w");
  fprintf(fp, "%14.7f  %14.7f  %14.7f  %14.7f\n", 
    x0[1], x0[2], x0[3], x0[4]);
  fclose(fp);

  /* free memory */
  free_vector(b,1,m);
  free_vector(db,1,m);
  free_vector(bT,1,m);
  free_vector(x,1,m);
  free_vector(dx,1,m);
  free_vector(x0,1,m);
  free_matrix(A,1,n,1,m);
  free_matrix(Acp,1,n,1,m);
  free_matrix(At,1,n,1,m);
  free_matrix(U,1,m,1,n);
  free_matrix(Ut,1,n,1,m);
  free_matrix(S,1,n,1,n);
  free_matrix(St,1,n,1,n);
  free_matrix(Si,1,n,1,n);
  free_matrix(V,1,n,1,n);
  free_matrix(Vt,1,n,1,n);
  free_matrix(UUt,1,n,1,n);
  free_matrix(VVt,1,n,1,n);
  free_matrix(VtV,1,n,1,n);
  free_matrix(US,1,n,1,n);
  free_matrix(SVt,1,n,1,n);
  free_matrix(USVt,1,m,1,n);
  free_vector(Sv,1,m);
  free_vector(Utb,1,n);
  free_vector(SiUtb,1,n);
  free_vector(VSiUtb,1,n);
  return(0);
}

int get_matrix_rows(char *f1, char *f2)
{
  int m1, m2;

  if (!fileExist(f1)) 
     Exit("get_matrix_rows(): %s does not exist",f1);
  if (!fileExist(f2)) 
     Exit("get_matrix_rows(): %s does not exist",f2);

  m1 = fileNumLines(f1);
  m2 = fileNumLines(f2);

  if (m1 != m2) Exit("get_matrix_rows():  A and b are not same row-dimension");
  if (m1 < 4) Exit("bp:  number of rows less than number of colums");

  return(m1);
}

