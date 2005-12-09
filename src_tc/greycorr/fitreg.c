/****************************************************************************
NAME:				FITREG

PURPOSE:  Fits a quadratic surface to the neighborhood of the correlation peak 
	  and from it determine the best-fit registration offsets and their 
          estimated errors

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

REFERENCES:

1.0  LAS 4.0 GREYCORR & EDGECORR by R. White 6/83
*****************************************************************************/
#include "correlate.h"
#include "asf_reporting.h"

#define MAX_DIM  6		/* Maximum dimension (set at compile time) */
#define U_OK     -99		/* Satisfactory return from subroutine 	   */
#define U_FAIL    -1		/* Unsatisfactory return from subroutine   */

#define GMAX(A, B)      ((A) > (B) ? (A) : (B)) /* assign maximum of a and b */
#define TINY 1.0e-20;

void fitreg(
	float *cpval,   /* 5 by 5 array of xcorr vals, in units of standard 
                           dev above background, centered on corr peak      */
	int mfit,       /* Method of surface fit: 1 - Elliptical paraboloid
                                                  2 - Elliptical Gaussian
                                                  3 - Reciprocal Paraboloid */
	float *pkoffs,  /* Best-fit horiz and vertical offsets of correlation 
                           peak relative to center of 5 by 5 array          */
	float *tlerrs   /* Estimated horizontal error, vertical error, and h-v 
			   cross term in best-fit offsets                   */
)
{
int i, j;		/* Loop counters 				    */
float B[6][6];		/* Matrix of sums of polynomial terms 		    */
float A[6][6];		/* Inverse of Matrix B 				    */
float coeffs[6];	/* Coefs of best-fit quadratic surface 		    */
float vector[6];	/* Right side Col vector to solve for coefs 	    */
float wghts[25];	/* Weight assigned to each value in area of peak    */
float z[25];		/* Xcorr coefficients to which surface is to be fit */
float denom;		/* Denominator of equation for best-fit offsets     */

/* Compute elements of matrix and column vector
  ---------------------------------------------*/
sums(cpval, mfit, z, wghts, B, vector);

/* Invert matrix to get best-fit peak offsets
  -------------------------------------------*/
invert(B, A, 6);
for(i=0;i<6;i++) 
   {
   for(coeffs[i]=0.0,j=0;j<6;j++)
      coeffs[i] += A[i][j]*vector[j];
   }
denom = 4.0*coeffs[3]*coeffs[5] - coeffs[4]*coeffs[4];
pkoffs[0] = (coeffs[2]*coeffs[4] - 2.0*coeffs[1]*coeffs[5])/denom;
pkoffs[1] = (coeffs[1]*coeffs[4] - 2.0*coeffs[2]*coeffs[3])/denom;

/* Estimate errors in best-fit offsets
  ------------------------------------*/
esterr(z, wghts, A, coeffs, pkoffs, tlerrs);
}

/****************************************************************************
NAME:				SUMS

PURPOSE:  Compute the sums of terms for the matrix elements and column vector 
          used for determining the best-fit surface to the cross-correlation 
          values in the neighborhood of the peak

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

*****************************************************************************/
void sums(
	float *cpval,    /* 5 by 5 array of xcorr values, in std dev        */
	int mfit,        /* Method of fitting surface                       */
	float *z,        /* Function of xcorr to which quadratic is fit     */
	float *wghts,    /* Weights to be assigned to points in 5 by 5 array*/
	float b[6][6],   /* Matrix of sums of polynomial terms              */
	float *vector    /* Col vector obtained by summing products of poly 
                            terms and an approp. function of xcorr value    */
)
{
int i,ic,ir,j;		/* Loop counters 				   */
int ivalpt;		/* Index of xcorr vals and weights in peak area    */
float term[6];		/* Terms in approximating quadratic polynomial     */
float val;		/* Value of one element in CPVAL array 		   */
 
/* Initialize arrays and constants
  --------------------------------*/
for(i=0; i<6; i++)
   {
   for(j=0; j<6; j++) b[i][j]=0.0;
   vector[i] = 0.0;
   }
 
/* Compute function of correlation coefficient and assign weight for each 
   location in neighborhood of peak
  --------------------------------*/
term[0] = 1.0;
for(ivalpt=0,ir=0; ir<5; ir++)
   for(ic=0; ic<5; ic++,ivalpt++)
      {
      val = GMAX(cpval[ivalpt], 1.0);
      if (mfit == 1) { z[ivalpt] = val; wghts[ivalpt] = 1.0; }
      else if (mfit == 2) { z[ivalpt] = -(log(val)); wghts[ivalpt] = val*val; }
      else { z[ivalpt] = 1.0/val; wghts[ivalpt] = val*val*val*val; }
 
/* Compute matrix and vector elements in linear equations for best-fit 
  -------------------------------------------------------------------*/
      term[1] = ic - 2;
      term[2] = ir - 2;
      term[3] = term[1]*term[1];
      term[4] = term[1]*term[2];
      term[5] = term[2]*term[2];
      for(i=0; i<6; i++)
          {
          vector[i] += wghts[ivalpt]*term[i]*z[ivalpt];
          for(j=0; j<6; j++) b[i][j] += wghts[ivalpt]*term[i]*term[j];
          }
      }
}

/****************************************************************************

NAME:				INVERT			

PURPOSE:  Driver program for Gaussian Elimination (LU factorization) using 
          scaled column pivoting.  All arrays are zero relative--the first 
          element of matrix A is A[0][0].  Solves Ax = b where A is an n x n 
          matrix and x and b are n dimensional vectors.

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

*****************************************************************************/
void invert(
	float (*matrix)[MAX_DIM], /* Input--Matrix A     */
	float (*outA)[MAX_DIM],   /* Output--Matrix A    */
	int n                     /* Dimension of matrix */
)
{
int ipvt[MAX_DIM];		/* Array of pivots 		*/
float b[MAX_DIM];		/* b & resulting x vectors 	*/
int i,j;			/* Loop counter 		*/
int status;			/* Return status flag 		*/

/* Factor matrix 
  -------------*/
status = lu_fact(n, matrix, ipvt);

/* If factored sucessfully, solve
  ------------------------------*/
for(i=0;i<n;i++)
   {
   for(j=0;j<n;j++)b[j]=0.0;
   b[i]=1.0;
   if (status == U_OK) back_solve(n, matrix, ipvt, b);
   else asfPrintWarning("No solution--Matrix not invertible:  %d\n",status);
   for(j=0;j<n;j++)outA[j][i]=b[j];
   }
}

/* Routine to perfrom the LU factorization of a matrix.  Upon input, "matrix"
   contains matrix A and upon output "matrix" contains A's LU factorization.
   The pivoting method used is scaled column pivoting.
  ---------------------------------------------------*/
int lu_fact(
	int n,                     /* Input--Dimension of matrix         */
	float (*matrix)[MAX_DIM],  /* Input/Ouput--Matrix A              */
	int *ipvt                  /* Output--Record of pivots performed */
)
{
float s[MAX_DIM];	/* Vector containing abs() of max val for each row */
float factor;		/* Factor used in zeroing entries (stored) 	*/
int n_1;		/* Parameter n minus 1 				*/
int i, j, k;		/* Loop counters 				*/
float temp;		/* Temporary variable for row swaping 		*/

if ((n <= 1) && (matrix[0][0] == 0.0)) return(1);
n_1 = n - 1;

/* Initialize scaled column pivoting (calculate the s vector).
  -----------------------------------------------------------*/
j = init_pivot(n, matrix, s);
if (j == U_FAIL) return(0);

/* Factor the matrix
  -----------------*/
for (k = 0; k < n_1; k++)
   {
   ipvt[k] = find_pivot(n, matrix, k, s);

/* Perform row interchange, if necessary
  -------------------------------------*/
   if (ipvt[k] != k)
      for (i = k; i < n; i++)
	{
	temp = matrix[k][i];
	matrix[k][i] = matrix[ipvt[k]][i];
	matrix[ipvt[k]][i] = temp;
	}
   if (matrix[k][k] == 0.0) return(k);

/* Factor the row
  --------------*/
   for (i = k + 1; i < n; i++)
      {
      factor = -matrix[i][k] / matrix[k][k];
      matrix[i][k] = factor;
      for (j = k + 1; j < n; j++)
	 matrix[i][j] += factor * matrix[k][j];
      }
   }
if (matrix[n_1][n_1] == 0.0) return(n);

return(U_OK);
}

/* Routine to solve the LU factorized matrix A, given a vector b.  Upon output,
   vector b is really vector x.
  ----------------------------*/
void back_solve(
	int n,                    /* Input--dimension of matrix            */
	float (*matrix)[MAX_DIM], /* Input--LU factorization of matrix A   */
	int *ipvt,                /* Input--Record of pivots               */
	float *b                  /* Input/Output--b (in) & x (out) vector */
)
{
int k, j, i;			/* Loop counters 			*/
int n_1;			/* Dimension of matrix - 1 		*/
int kp1;			/* Var to reduce computations 		*/
float temp;			/* Temporary variable for interchanges 	*/

n_1 = n - 1;

/* Align vector b so that it corresponds to the row interchanges made during
   the factorization process.
  --------------------------*/
for (k = 0; k < n_1; k++)
  {
  if (k != ipvt[k]) { temp = b[k]; b[k] = b[ipvt[k]]; b[ipvt[k]] = temp; }

/* Solve Ly = b
  ------------*/
  for (i = k + 1; i < n; i++) b[i] = b[i] + matrix[i][k] * b[k];
  }

/* Backsolve--Solve Ux = y
  -----------------------*/
b[n_1] = b[n_1] / matrix[n_1][n_1];
for (k = n_1; k--;)
   {
   kp1 = k + 1;
   for (j = 0; j < kp1; j++) b[j] = b[j] - matrix[j][kp1] * b[kp1];
   b[k] = b[k] / matrix[k][k];
   }

return;
}

/* Scaled column pivoting.  The first routine (init_pivot) sets up the 
   s array.  The second routine (find_pivot) finds the pivot element and
   returns it to the caller.  These routines are isolated so that different
   pivoting methods can be used with little re-coding (although the s array
   may be unused).
  ---------------*/
int init_pivot(
	int n,                    /* Input--dimension of matrix */
	float (*matrix)[MAX_DIM], /* Input--matrix A            */
	float *s                  /* Output--S array            */
)
{
int i,j;		/* Loop counters */
float test;		/* Temporary variable for maximum test */

/* Find the value of the abs(largest) element in a row.
  ----------------------------------------------------*/
for (i = 0; i < n; i++)
   {
   for (s[i] = 0.0, j = 0; j < n; j++)
	 { test = fabs(matrix[i][j]); if (test > s[i]) s[i] = test; }

/* If s[i] is zero, all elements in the row are zero--something this routine
   won`t deal with...
  -----------------*/
   if (s[i] == 0.0) return(U_FAIL);
   }
return(U_OK);
}

int find_pivot(
	int n,                    /* Input--dimension of matrix         */
	float (*matrix)[MAX_DIM], /* Input/Output--matrix A             */
	int k,                    /* Input--Current row (zero-relative) */
	float *s                  /* Input--S array                     */
)
{
float max;		/* Maximum value found 				*/
int j;			/* Loop counter 				*/
float test;		/* Temporary value used in max value test 	*/
int max_row=0;		/* Row containing the maximum value 		*/

/* Find the row (zero relative) which have the largest value after being
   scaled with the corresponding element of the s array.
  -----------------------------------------------------*/
max = 0.0;
for (j = k; j < n; j++)
   {
   test = matrix[j][k] / s[j];
   test = fabs(test);
   if (test > max) { max = test; max_row = j; }
   }

return(max_row);
}

/****************************************************************************
NAME:				ESTERR

PURPOSE:  Estimate the probable errors in the best-fit location of the 
          correlation peak

VERSION	 DATE	AUTHOR	  
-------	 ----	------	   
  1.0	 8/92	D. Steinwand 

*****************************************************************************/
void esterr(
	float *z,          /* Function of xcorr val to which quadratic is fit   */
	float *wghts,      /* Weights for each point in 5 by 5 array of values  */
	float bnvrs[6][6], /* Inverse of matrix B                               */
	float *coeffs,     /* Coefs of best-fit quad surface to values of Z     */
	float *pkoffs,     /* Best-fit offset of peak to center of 5 by 5 array */
	float *tlerrs      /* Est horiz & vert error & h-v cross term           */
)
{
int i,	j, x, y;	/* Loop counters 				     */
int ivalpt;		/* Index to values det from measured corr coeffs     */
float du[6];		/* Partial derivatives of horiz best-fit peak offset 
			   with respect to coefs of polynomial surface 	     */
float dv[6];		/* Partial derivatives of vertical offset 	     */
float c;		/* Constant for computing weights or errors 	     */
float denom;		/* Denominator used for partial derivatives 	     */
float f;		/* Value of function of corr coefficient computed 
                           using best-fit coefficients 			     */
float usum;		/* Accum. for sum of terms to determine est errors in 
			   horizontal offset 				     */
float var;		/* Residual variance of fit			     */
float vsum;		/* Sum of terms to estimate vertical offset error    */
float xsum;		/* Accum. for sum of cross terms to est errors when 
                           search chip was rotated 			     */

/* Compute residual variance for elements of peak array
  -----------------------------------------------------*/
ivalpt = 0;
var = 0.0;
for(y= -2;y<=2;y++)
 for(x= -2;x<=2;x++)
 {
 f=coeffs[0]+coeffs[1]*x+coeffs[2]*y+coeffs[3]*x*x+coeffs[4]*x*y+coeffs[5]*y*y;
 var += wghts[ivalpt]*(f - z[ivalpt])*(f - z[ivalpt]);
 ivalpt++;
 }

/* Compute constant to use with weights
  -------------------------------------*/
c = var/19.0;

/* Compute partial derivatives of peak offsets with respect to poly coefs
  ----------------------------------------------------------------------*/
denom = 4.0*coeffs[3]*coeffs[5] - coeffs[4]*coeffs[4];
du[0] = 0.0;
du[1] = -2.0*coeffs[5]/denom;
du[2] = coeffs[4]/denom;
du[3] = -4.0*coeffs[5]*pkoffs[0]/denom;
du[4] = (coeffs[2] + 2.0*coeffs[4]*pkoffs[0])/denom;
du[5] = (-2.0*coeffs[1] - 4.0*coeffs[3]*pkoffs[0])/denom;
dv[0] = 0.0;
dv[1] = du[2];
dv[2] = -2.0*coeffs[3]/denom;
dv[3] = (-2.0*coeffs[2] - 4.0*coeffs[5]*pkoffs[1])/denom;
dv[4] = (coeffs[1] + 2.0*coeffs[4]*pkoffs[1])/denom;
dv[5] = -4.0*coeffs[3]*pkoffs[1]/denom;

/* Compute estimated errors in offsets
  ------------------------------------*/
usum = vsum = xsum = 0.0;
for(i=0;i<6;i++)
   for(j=0;j<6;j++)
      {
      usum += du[i]*du[j]*bnvrs[i][j];
      vsum += dv[i]*dv[j]*bnvrs[i][j];
      xsum += du[i]*dv[j]*bnvrs[i][j];
      }
tlerrs[0] = sqrt(fabs(c*usum));
tlerrs[1] = sqrt(fabs(c*vsum));
tlerrs[2] = c*xsum;
}
