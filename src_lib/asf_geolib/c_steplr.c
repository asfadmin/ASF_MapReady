/****************************************************************************
NAME:				c_steplr

PURPOSE:  Computes transformation coefficients based on a stepwise
	  regression procedure.

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development
  5.1    7/89   D. Steinwand  CSB      Added error messages & fixed a 
				       bug in partial_f
  5.2    11/89  D. Steinwand  CSB      Fixed a numerical error
  5.3    7/90   D. Etrheim    CSB      Standardized error message handling
  5.4    8/90   D. Etrheim    CSB      Added check to ensure that the first 
				       variable enters the regression. 

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Initialize variables
	Calculate correlations of independent variables with dependent vars
	Select the first variable to enter the regression
	Enter the first variable into the regression
	Test regression to see if it is significant
	If not significant, return(fail)
	Loop
	   Find the next variable to enter the regression
	   If no variables remain, break out of loop
	   Test the selected variable for significance
	   If not significant, break out of loop
	   Enter the variable into the regression
	   Find the smallest partial-F value and test for significance
	   If not significant
		Remove the variable from the model
		Find the smallest .....
	Find the coefficients of the best regression equation
	Return (success)

   Additional notes:

	This procedure is modeled after Draper & Smith's "Applied Regression
	Analysis", section 6.8 entitled "Computational Method for Stepwise
  	Regression."  This stepwise procedure was originated by Efroymson.

	The Augmented Correlation Matrix, a[][], is initially defined as 
	follows:

		--------------------------------------------
		|  R(k x k)   |  T'(K x 1)   |   I(k x k)  |
		--------------------------------------------
	 	|  T(1 x k)   |  S(1 x 1)    |   0(1 x k)  |
		--------------------------------------------
		| -I(k x k)   |  0(k x 1)    |   0(k x k)  |
		--------------------------------------------

	where:
		 R(k x k)  is the correlation matrix for the k independent vars
		 T(1 x k)  is the correlation of the k independent variables
				with the dependent variable
		 T'(k x 1) is the transpose of T
		 S(1 x 1)  is the correlation of the dependent variable with
				itself (approximately = 1.0)
		 I(k x k)  is the identity matrix
		-I(k x k)  is the negitive identity matrix
	

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.,
	 and portions of chapters 1 & 2.

	Draper, N.R., Smith, H.  1981.  "Applied Regression Analysis"
	 Second Edition (New York:  John Wiley & Sons, Inc.). pp. 307-312.

	Press, William H., Flannery, Brian P., Teukolsky, Saul A., and
	 Vetterling, William T.  1986, "Numerical Recipes--The Art of
  	 Scientific Computing" (New York:  Cambridge University Press).
	 Chapter 6.
*****************************************************************************/
#define NOT_IN	 100		/* Independent variable not in model */
#define IN_MODEL 200		/* Independent variable in model */
#define REMOVED	 300		/* Independent variable removed from model */

#define MAX_INDP  15		/* Maximum number of independent variables */
#define AUG_MAT	  31   		/* (MAX_INDP * 2) + 1  */
#define NO_PTS_IN_MODEL 400	/* Flag set when no points remain in model */

#define ENTER_TEST	0	/* Array index of alpha -- enter model test */
#define EXIT_TEST 	1	/* Array index of alpha -- exit model test */

#define BEFORE_FIT	101	/* Flag for partial-f test; before fit */
#define AFTER_FIT	102	/* Flag for partial-f test; after fit */

#define NEAR_ZERO 1.0e-30

#include "worgen.h"
#include "geompak.h"

#include "typlim.h"
#include "proj.h"

 static void corr_coef (double (*raw)[16], int indep, int npts, double *sum2x, double *sum2y, double *sumx, double *sumy, double (*a)[31]); 
 static double partial_f (int selected, int rd_free, int y_val, double (*a)[31], int flag); 
 static void calc_coef (int *in_model, int n_indep, double (*a)[31], double sum2y, double *sum2x, double sumy, double *sumx, int npts, double *b); 
 static int select_var (int y_val, int *in_model, double (*a)[31]); 
 static void enter_model (int n_indep, int selected, double (*a)[31]); 
 static void exit_model (int n_indep, int selected, double (*a)[31]); 
 static void smallest_f (int *in_model, int rd_free, int n_indep, double (*a)[31], int *selected, double *min_f); 
 static int f_test (double f, int rd_free, double alpha); 
 static double icbetaf (double a, double b, double x); 
 static double fraction (double a, double b, double x); 
 static double log_gam (double arg); 

int c_steplr(npts, n_indep, raw_data, coefs, alpha)

double (*raw_data)[MAX_COEF];	/* Raw data--independent & dependent vars */
double *coefs;			/* Returned (calculated) coefficients */
double alpha[];			/* Entry & exit significance test parameters */
int *npts;			/* Number of data records */
int *n_indep;			/* Number of independent variables */
{
double sum2x[MAX_INDP];		/* Sum of X's sqr; X = indpendent variables */
double sum2y;			/* Sum of Y squared; the dependent variable */
double sumx[MAX_INDP];		/* Sum of X's; X = independent variables */
double sumy;			/* Sum of Y; the dependent variable */
double f;			/* F value (partial or overall) for F-test */
double a[AUG_MAT][AUG_MAT];	/* Augmented correlation matrix */
int count;			/* Loop index */
int in_model[MAX_INDP];	/* Flags--is given independent var in model? */
int selected;			/* Variable selected to enter or exit model */
int x;				/* Number of points in model (indep + dep) */

/* Initialize the in_model array of flags--no variables in the model
  -----------------------------------------------------------------*/
for (count = 0; count < MAX_INDP; in_model[count++] = NOT_IN);

/* Calculate correlations of the independent variables with the dependent
   variables.  Store the results in a, the augmented correlation matrix
  --------------------------------------------------------------------*/
corr_coef(raw_data, *n_indep, *npts, sum2x, &sum2y, sumx, &sumy, a);

/* Select the first variable to enter the regression.  This variable is the
   variable which has the largest correlation with the dependent variable.
   Enter the variable into the regression.
 ----------------------------------------*/
selected = select_var(*n_indep, in_model, a);
if (selected == NO_PTS_IN_MODEL) 
   {
   c_errmsg("Zero variables entered into model","steplr-model",NON_FATAL);
   c_errmsg("Modeling process failed","steplr-fail",NON_FATAL);
   return(E_FAIL);
   }
enter_model(*n_indep, selected, a);
in_model[selected] = IN_MODEL;

/* Calculate the partial f-test for the variable in the regression.  
  ----------------------------------------------------------------*/
f = partial_f(selected, (*npts - 2), *n_indep, a, AFTER_FIT);

/* Check if the regression is significant.  If not, return a fail condition
  ------------------------------------------------------------------------*/
if (f_test(f, (*npts - 2), alpha[EXIT_TEST]) == E_FAIL) 
   {
   c_errmsg("Zero variables entered into model","steplr-model",NON_FATAL);
   c_errmsg("Modeling process failed","steplr-fail",NON_FATAL);
   return(E_FAIL);
   }

/* The regression is significant.  Continue looping until all variables have
   been considered for use in the model.
  -------------------------------------*/
for (x = 2;;)
   {

/* Select the next variable to enter the regression
  ------------------------------------------------*/
   selected = select_var(*n_indep, in_model, a);

/* If a variable was not selected, there are no more variables significant
   enough to enter the regression--break out of this loop, the process is
   complete.
  ---------*/
   if (selected == NO_PTS_IN_MODEL) break;

/* Perform a partial f-test on the variable selected.  This is an enterance
   test to see if the variable is significant enough to enter the model.
   If the f-test fails, break out of this loop--the process is complete.
  ---------------------------------------------------------------------*/
   f = partial_f(selected, (*npts - (x + 1)), *n_indep, a, BEFORE_FIT);
   if (f_test(f, (*npts - (x + 1)), alpha[ENTER_TEST]) == E_FAIL) break;

/* Enter the selected variable into the regression and flag it as in the
   regression (model).
  ------------------*/
   x++;
   enter_model(*n_indep, selected, a);
   in_model[selected] = IN_MODEL;

/* Perform exit tests on the variables in the regression.  This is accomplished
   by finding the variable with the smallest f-value and testing it for
   significance.  If the variable with the smallest f-value is found to be
   significant, break out of the loop and processing continues.  If the 
   variable is found to be not significant, the variable is removed from the
   regression (model) and is flaged as such.  Then, the remaining variables
   are tested by finding the variable with the smallest f-value...  The process
   continues until a "smallest f-value" passes or there are no more variables
   in the regression.  This procedure is recursive in nature, but is implemented
   non-recursively for machine efficiency purposes.  (An thus the ugly infinite
   loop and break commands...)
  ---------------------------*/
   for (;;)
	{
	smallest_f(in_model, (*npts - x), *n_indep, a, &selected, &f);
	if (selected == NO_PTS_IN_MODEL) break;
   	if (f_test(f, (*npts - x), alpha[EXIT_TEST]) == E_FAIL) 
	   {
	   exit_model(*n_indep, selected, a);
	   in_model[selected] = REMOVED;
	   x--;
	   }
	else break;
	}
   }
/* When x == 1, there are no points in the regression.  If this is the case,
   signal an error condition--return fail .
  ---------------------------------------*/
if (x == 1)
   {
   c_errmsg("Zero variables remain in model","steplr-model",NON_FATAL);
   c_errmsg("Modeling process failed","steplr-fail",NON_FATAL);
   return(E_FAIL);
   }

/* A successful stepwise regression has occured.  Extract the transformation
   coefficients from the augmented correlation matrix, a.
  ------------------------------------------------------*/
calc_coef(in_model, *n_indep, a, sum2y, sum2x, sumy, sumx, *npts, coefs);
return(E_SUCC);
}

/****************************************************************************
NAME:				corr_coef

PURPOSE:  Calculates the correlations of all independent variables with
  	  the dependent variable.

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Initialize variables
	Calculate uncorrected sum of squares
	Calculate the corrected sum of squares and corrected sums of 
	  cross-products matrix
	Calculate correlation coefficients and place directly in the
   	  augmented correlation matrix
	Return

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 178-180.
*****************************************************************************/
static void corr_coef(raw, indep, npts, sum2x, sum2y, sumx, sumy, a)

int indep;			/* Number of independent variables */
int npts;			/* Number of data points */
double (*raw)[MAX_COEF];	/* Raw (input) data array */
double *sum2x;			/* Adjusted sum of squares--independent vars */
double *sum2y;			/* Adjusted sum of squares--dependent var */
double *sumx;			/* Sum of the independent variables */
double *sumy;			/* Sum of the dependent variable */
double (*a)[AUG_MAT];		/* Augmented correlation matrix */
{
double cross[MAX_COEF][MAX_COEF]; /* Cross products matrix */
register int cnt, cnt2, cnt3;	/* Loop counters */
/*int upper;*/			/* Upper bounds for loop */

/* Zero matries and other variables
  --------------------------------*/
*sumy = *sum2y = 0.0;
for (cnt = indep; cnt--;)
   {
   sumx[cnt] = 0.0;
   sum2x[cnt] = 0.0;
   }
for (cnt = (indep + 1); cnt--;)
   for (cnt2 = (indep + 1); cnt2--;)
	cross[cnt][cnt2] = 0.0;

/* Calculate the uncorrected sum of squares (SS)
  ---------------------------------------------*/
for (cnt = 0; cnt < npts; cnt++, raw++)
   {
   for (cnt2 = 0; cnt2 < indep; cnt2++)
      sumx[cnt2] += (*raw)[cnt2];
   *sumy += (*raw)[indep];

   for (cnt2 = 0; cnt2 < (indep + 1); cnt2++)
      for (cnt3 = cnt2; cnt3 < (indep + 1); cnt3++)
	cross[cnt2][cnt3] += (*raw)[cnt2] * (*raw)[cnt3];
   }
for (cnt = 0; cnt < indep; cnt++)
   sum2x[cnt] = cross[cnt][cnt];
*sum2y = cross[indep][indep];

/* Calculate the corrected sum of squares (SS) and corrected sums of 
   cross-products matrix
  ---------------------*/
for (cnt = 0; cnt < indep; cnt++)
   sum2x[cnt] -= (sumx[cnt] * sumx[cnt] / npts);
*sum2y -= (*sumy * *sumy / npts);

for (cnt = 0; cnt < indep; cnt++)
   for (cnt2 = cnt; cnt2 < indep; cnt2++)
	cross[cnt][cnt2] -= (sumx[cnt] * sumx[cnt2] / npts);

for (cnt = 0; cnt < indep; cnt++)
   cross[cnt][indep] -= (sumx[cnt] * *sumy) / npts;

/* Store sum2x[] and sum2y as the square root of their values
  ----------------------------------------------------------*/
for (cnt = 0; cnt < indep; cnt++)
   sum2x[cnt] = sqrt(sum2x[cnt]);
*sum2y = sqrt(*sum2y);

/* Calcaulate correlation coefficients R (but place directly in A)
  ---------------------------------------------------------------*/
for (cnt = AUG_MAT; cnt--;)
   for (cnt2 = AUG_MAT; cnt2--;)
	a[cnt][cnt2] = 0.0;

for (cnt = (indep + 1); cnt--;)
   a[cnt][cnt] = 1.0;

for (cnt = 0; cnt < indep; cnt++)
   {
   for (cnt2 = (cnt + 1); cnt2 < indep; cnt2++)
	a[cnt][cnt2] = cross[cnt][cnt2] / (sum2x[cnt] * sum2x[cnt2]);
   a[cnt][indep] = cross[cnt][indep] / (sum2x[cnt] * *sum2y);
   }

for (cnt = 1; cnt < (indep + 1); cnt++)
   for (cnt2 = 0; cnt2 < cnt; cnt2++)
	a[cnt][cnt2] = a[cnt2][cnt];

for(/*upper = (2 * indep),*/ cnt = 0, cnt2 = (indep + 1); cnt < indep; cnt++,cnt2++)
	{
	a[cnt][cnt2] = 1.0;
	a[cnt2][cnt] = -1.0;
	}
return;
}

/****************************************************************************
NAME:				partial-f

PURPOSE:  Calculates partial-f statistic either before or after a variable
	  has been entered into the regression (model)

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development
  5.01   7/89   D. Steinwand  CSB      Added the fabs call to the F value

ALGORITHM DESCRIPTION:
	If after the variable has been entered into the regression,
	   Calculate index into the augmented correlation matrix
	   Calculate numerator
	   Calculate denominator
	   Return numerator/denominator
	If before the variable has been entered into the regression,
	   Calculate index into the augmented correlation matrix
	   Calculate numerator
	   Calculate denominator
	   Return (numerator * residual degrees of freedom) /denominator

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.
*****************************************************************************/
static double partial_f(selected, rd_free, y_val, a, flag)

int selected;		/* Independent variable selected to test */
int rd_free;		/* Residual degrees of freedom */
int y_val;		/* Matrix index of dependent variable */
int flag;		/* Before / After fit flag */
double a[][AUG_MAT];	/* Augmented correlation matrix */
{
double num, denom;	/* Numerator & denominator of partial-f equation */
int index;		/* Temporary index value */

if (flag == AFTER_FIT)
   {
   index = y_val + selected + 1;
   num = rd_free * a[selected][y_val] * a[selected][y_val];
   denom = a[y_val][y_val] * a[index][index];
   if (denom < NEAR_ZERO) denom = NEAR_ZERO;
   return (fabs(num/denom));
   }
else
   {
   index = y_val + selected + 1;
   num = a[selected][y_val] * a[selected][y_val] / a[selected][selected];
   denom = a[y_val][y_val] - num;
   if (denom < NEAR_ZERO) denom = NEAR_ZERO;
   return (fabs((num * rd_free)/denom));
   }
}

/****************************************************************************
NAME:				calc_coef

PURPOSE:  Extracts tranformation coefficients from regression matrix

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Initialize variables  (Coefficient[0] = 0.0 -- used as an accumulator)
	Loop on the number of independent variables (index)
	   If the independent variable is in the model,
		Extract Coefficient[index]
		Adjust Coefficient[0]
	   Else Coefficient[index] = 0.0
	Adjust Coefficient[0]
	Return

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.
*****************************************************************************/
static void calc_coef(in_model, n_indep, a, sum2y, sum2x, sumy, sumx, npts, b)

double a[][AUG_MAT];	/* Augmented correlation matrix */
double sum2y;		/* Sum of squares of dependent variable */
double *sum2x;		/* Sum of squares of independent variables */
double sumy;		/* Sum of dependent variable */
double *sumx;		/* Sum of independent variables */
double *b;		/* Coeffient array */
int *in_model;		/* Flags--is given independent var in model? */
int n_indep;		/* Number of independent variables */
int npts;		/* Number of data points */
{
int count, index;	/* Loop counters */

for (b[0] = 0.0, count = 0, index = 1; count < n_indep; count++, index++)
   if (in_model[count] == IN_MODEL)
	{
	b[index] = a[count][n_indep] * (sum2y / sum2x[count]);
	b[0] += (b[index] * sumx[count] / npts);
	}
   else
	b[index] = 0.0;
b[0] = (sumy / npts) - b[0];
return;
}

/****************************************************************************
NAME:				select_var

PURPOSE:  Selects the independent variable with the largest correlation value

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Initialize variables, maximum set to zero
	Loop on each independent variable
	   If the independent variable is not in the model,
		Calculate val
		If val is greater than the maximum,
		   Set maximum = val
		   Record variable number
	Return the number of the independent variable with the largest val
		
ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.
*****************************************************************************/
static int select_var(y_val, in_model, a)

int y_val;		/* Position of y value in matrix;  y_val = n_indep */
int *in_model;		/* Boolean values for each indep var: TRUE = in model */
double a[][AUG_MAT];	/* Augmented correlation matrix */
{
double max_v;	/* Maximum value */
double val;	/* Temporary test value */
int count;	/* Loop counter */
int max_count;	/* Number of the independent variable with the maximum value */

for(max_count = NO_PTS_IN_MODEL, max_v = 0.0, count = 0; count < y_val; count++)
   if (in_model[count] == NOT_IN)
	{
	val = a[count][y_val] * a[y_val][count] / a[count][count];
	if (val > max_v)
	   {
	   max_v = val;
	   max_count = count;
	   }
	}
return(max_count);
}

/****************************************************************************
NAME:				enter_model

PURPOSE:  Enters a selected independent variable into the regression

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Calculate size of augmented correlation matrix
	Adjust the augmented correlation matrix for the entering of the 
	  selected variable, placing results in a temporary buffer
	Place the resulting matrix residing in the temporary buffer back 
	  into a, the augmented correlation matrix
	Return

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.
*****************************************************************************/
static void enter_model(n_indep, selected, a)

int n_indep;		/* Number of independent variables */
int selected;		/* Independent variable selected to enter model */
double a[][AUG_MAT];	/* Augmented correlation matrix */
{
int size;			/* Size of correlation matrix */
int count, count2;		/* Loop counters */
double temp[AUG_MAT][AUG_MAT];	/* Temporary matrix buffer */

size = (2 * n_indep) + 1;

for (count = 0; count < size; count++)
   for (count2 = 0; count2 < size; count2++)
	temp[count][count2] = a[count][count2] - ((a[count][selected] *
			      a[selected][count2]) / a[selected][selected]);
for (count2 = 0; count2 < size; count2++)
   temp[selected][count2] = a[selected][count2] / a[selected][selected];

for (count = 0; count < size; count++)
   for (count2 = 0; count2 < size; count2++)
	a[count][count2] = temp[count][count2];

return;
}

/****************************************************************************
NAME:				exit_model

PURPOSE:  Removes a selected independent variable from the regression

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Calculate size of augmented correlation matrix
	Calculate index into augmented correlation matrix for the independent
	  variable to be removed from the regression (model)
	Adjust the augmented correlation matrix for removal of the selected
	  variable, placing results in a temporary buffer
	Place the resulting matrix residing in the temporary buffer back 
	  into a, the augmented correlation matrix
	Return

ALGORITHM REFERENCES:
	Draper, N.R., Smith, H.  1966.  "Applied Regression Analysis"
	 (New York:  John Wiley & Sons, Inc.). pp. 171-172 & pp. 178-195.
*****************************************************************************/
static void exit_model(n_indep, selected, a)

int n_indep;		/* Number of independent variables */
int selected;		/* Independent variable selected to enter model */
double a[][AUG_MAT];	/* Augmented correlation matrix */
{
double temp[AUG_MAT][AUG_MAT];	/* Temporary matrix buffer */
int index;			/* Additional array index */
int size;			/* Size of correlation matrix */
int count, count2;		/* Loop counters */

size = (2 * n_indep) + 1;
index = n_indep + selected + 1;

for (count = 0; count < size; count++)
   for (count2 = 0; count2 < size; count2++)
	temp[count][count2] = a[count][count2] - ((a[count][index] *
			      a[selected][count2]) / a[index][index]);
for (count2 = 0; count2 < size; count2++)
   temp[selected][count2] = a[selected][count2] / a[index][index];

for (count = 0; count < size; count++)
   for (count2 = 0; count2 < size; count2++)
	a[count][count2] = temp[count][count2];

return;
}

/****************************************************************************
NAME:				smallest_f

PURPOSE:  Finds the variable in the regression (model) with the smallest
	  partial-f value

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development

ALGORITHM DESCRIPTION:
	Initialize the selected variable to "no points found"
	Initialize the smallest f-value "found" (min_f) to the maximum real 
		for this machine
	Do for each independent variable
	   If the variable is in the model,
		Calculate the variable's partial-f value
		If the partial-f value is less than the smallest-so-far (min_f),
		   Set min_f to the partial-f value just calculated
		   Set selected to this variable
	Return
*****************************************************************************/
static void smallest_f(in_model, rd_free, n_indep, a, selected, min_f)

double a[][AUG_MAT];	/* Augmented correlation matrix */
double *min_f;		/* Minimum f value found */
int *in_model;		/* Flags--is given independent var in model? */
int rd_free;		/* Residual degrees of freedom */
int n_indep;		/* Number of independent variables (dependent index) */
int *selected;		/* Independent variable with the smallest partial-f */
{
double f;		/* Partial-f value */
int count;		/* Loop index */

*selected = NO_PTS_IN_MODEL;
*min_f = MAXREAL;
for (count = 0; count < n_indep; count++)
   if (in_model[count] == IN_MODEL)
	{
	f = partial_f(count, rd_free, n_indep, a, AFTER_FIT);
	if (f < *min_f)
	   {
	   *min_f = f;
	   *selected = count;
	   }
	}
return;
}

/****************************************************************************
NAME:				f_test

PURPOSE:  Tests the significance of an entered f-value based on the
	  residual degrees of freedom and alpha value entered.

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development
  5.02	 11/89	D. Steinwand  CSB      Fixed numerical error

ALGORITHM DESCRIPTION:
	Adjust the f value for use in the incomplete beta function
	Adjust the residual degrees of freedom for use in the incomplete
	   beta function
	Call the incomplete beta function; it returns the f-distribution
	  percentage value for the degrees of freedom and f-value entered.
	If the value returned is larger than alpha, return FAIL
	return SUCCESS

ALGORITHM REFERENCES:
	Press, William H., Flannery, Brian P., Teukolsky, Saul A., and
	 Vetterling, William T.  1986, "Numerical Recipes--The Art of
  	 Scientific Computing" (New York:  Cambridge University Press).
	 Page 169, equation (6.3.11), the incomplete beta function's 
	 use in evaluating the F-Distribution probability function.
*****************************************************************************/
static int f_test(f, rd_free, alpha)

double f;	/* f value for F-test */
int rd_free;	/* Residual degrees of freedom */
double alpha;	/* F-test significance criterian */
{
double val;		/* F-Distribution percentage returned from icbetaf */
double half = 0.5;	/* Constant of 1/2 */
double df;		/* Residual degrees of freedom / 2 */

if ((rd_free + f) < NEAR_ZERO) f = 1.1;
else if (f < NEAR_ZERO) f = 1.0;
else f = rd_free / (rd_free + f);
df = (float)rd_free / 2.0;
val = icbetaf(df, half, f);
if (val > alpha) return(E_FAIL);
return(E_SUCC);
}

/****************************************************************************
NAME:				icbetaf

PURPOSE:  Returns the incomplete beta function

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
         1986   Numerical Recipes FORTRAN book
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development & Conversion to C

ALGORITHM DESCRIPTION:
	If argument x is less than zero or greater than 1, return with
		a bad argument status
	If argument x is equal to either zero or one, set factor to zero
	Else compute factors in front of the continued fraction
	If argument x is less than the ratio (a + 1)/(a+b+2) then use the
		continued fraction directly
	Else, make symmetry transformation, then use continued fraction
	Return

ALGORITHM REFERENCES:
	This function was derived from Numerical Recipes by Press, Flannery,
	  Teukolsky, & Vetterling; Cambridge University Press, 1986, p. 167.
*****************************************************************************/
static double icbetaf(a, b, x)

double a,b,x;		/* Arguments */
{
double factor;		/* Factor in front of continued fraction */

if ((x < 0.0) || (x > 1.0)) return(1.0);
if ((x == 0.0) || (x == 1.0)) factor = 0.0;
else factor = exp(log_gam(a+b) - log_gam(a) - log_gam(b) + a * log(x) 
              + b * log(1.0-x));
if (x < ((a + 1.0) / (a + b + 2.0))) return(factor * fraction(a,b,x)/a);
return(1.0 - factor * fraction(b,a,1.0-x)/b);
}

/****************************************************************************
NAME:				fraction

PURPOSE:  Returns continued fraction for the incomplete beta function 

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
         1986   Numerical Recipes FORTRAN book
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development & Conversion to C

ALGORITHM DESCRIPTION:
	Initialize variables
	Initialize temporary variables used in factors which occur in 
	   coefficients in equation (6.3.6) in the text
	Iterate until a solution is found or until maximum iterations exceeded
	   Calculate the even recurrence
	   Calculate the odd recurrence
	   Save old result
	   Re-normalize to prevent floating point overflow
	   Check for completion and return is successful
	Return error condition

ALGORITHM REFERENCES:
	This function was derived from Numerical Recipes by Press, Flannery,
	  Teukolsky, & Vetterling; Cambridge University Press, 1986, p. 168.
*****************************************************************************/
static double fraction(a,b,x)

double a,b,x;
{
int itmax = 101;		/* Function constant--Max iterations + 1 */
double eps = 0.0000003;		/* Function constant */
int m;
double tem, qap, qam, qab, em, d;
double bz, bpp, bp, bm, az, app;
double am, aold, ap;

/* Initialize variables
  --------------------*/
am = bm = az = 1.0;

/* Temporary variables used in factors which occur in coefficients in equation
   (6.3.6) in the text
  -------------------*/
qab = a + b;
qap = a + 1.0;
qam = a - 1.0;

/* The continued fraction is evaluated by the recurrence method described in
   equation (5.2.5) in the text.  This method is linear in the a's and b's
   and may be rescaled at any point to avoid floating-point overflow or 
   underflow
  ---------*/
bz = 1.0 - qab * x / qap;
for (m = 1 ; m < itmax; m++)
   {
   em = (double)m;
   tem = em + em;
   d = em * (b - m) * x / ((qam + tem) * (a + tem));

/* Calculate the even step of the recurrence
  -----------------------------------------*/
   ap = az + d * am;
   bp = bz + d * bm;
   d = -(a + em) * (qab + em) * x / ((a + tem) * (qap + tem));

/* Calculate the odd step of the recurrence
  ----------------------------------------*/
   app = ap + d * az;
   bpp = bp + d * bz;

/* The old result is saved and the a's and b's are renormalized to prevent
   floating point overflow
  -----------------------*/
   aold = az;
   am = ap/bpp;
   bm = bp/bpp;
   az = app / bpp;
   bz = 1.0;

/* Check for satisfactory completion
  ---------------------------------*/
   if (fabs(az - aold) < (eps * fabs(az))) return(az);
   }

/* Error--Maximum iterations performed without satisfactory completion
  -------------------------------------------------------------------*/
return(-99999);
}

/****************************************************************************
NAME:				log_gam

PURPOSE:  Returns the logarithm of the gamma function

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
         1986   Numerical Recipes FORTRAN book
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 Development & Conversion to C

ALGORITHM DESCRIPTION:
	Initialize coefficients for evaluation of integral
	Decrement the argument; store in another variable
	Compute portion outside summation
	Initialize the accumulator for series evaluation
	Loop five times
	   Add to the series accumulator the coefficient for this iteration
	   divided by the increment of (argument - 1)
	Return portion outside summation + the log of the series * a constant

ALGORITHM REFERENCES:
	This function was derived from Numerical Recipes by Press, Flannery,
	  Teukolsky, & Vetterling; Cambridge University Press, 1986, p. 157.
*****************************************************************************/
static double log_gam(arg)

double arg;		/* Argument */
{
double coef[6];		/* Coefficients for series (by Lanczos) */
double x;		/* Argument - 1.0 */
double temp;		/* Portion of calculation not in series evaluation */
double series;		/* Accumulator for series evaluation (integration) */
int count;		/* Loop counter */

coef[0] =  76.18009173;
coef[1] = -86.50532033;
coef[2] = 24.01409822;
coef[3] = -1.231739516;
coef[4] = 0.00120858003;
coef[5] = -0.00000536382;

x = arg - 1.0;
temp = x + 5.5;
temp = (x + 0.5) * log(temp) - temp;
series = 1.0;
for (count = 0; count < 6; count++)
   series += coef[count]/(++x);
return(temp + log(2.50662827465 * series));
}
