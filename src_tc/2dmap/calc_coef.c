/****************************************************************************
NAME:				calc_coef
 
PURPOSE:  Calculates transformation coefficients from a set of tie points
 
PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    3/82   R. Wenger     CSC      N/A    (Old name:  TIEPOINTS)
  4.0    5/85   S. Kondal     SAR      20-472 (Old name:  TIEFIT)
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 C conversion
 
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   must be run under TAE
 
PROJECT:  LAS
 
ALGORITHM DESCRIPTION:
	Calculate the number of independent variables for degree chosen
	Loop on tie point pairs
	   If tie point is active,
		Calculate independent variables in x & y for degree chosen
		Assign reference samples to dependent variable
	Calculate transformation coefficients in x
	Loop on tie point pairs
	   If tie point is active,
		Assign reference lines to dependent variable
	Calculate transformation coefficients in y
	Record the number of coefficients in x and in y
	Record the degrees of freedom in x and in y
	Return
*****************************************************************************/


#include "proj.h"
#include "2dmap.h"
 
int calc_coef(tie_data, fit, coef)
 
struct TPLDATA *tie_data;	/* Tie Point Location records */
struct FIT *fit;		/* Structure containing fitting parameters */
struct COEF *coef;		/* Structure to contain coefficients & stats */
{
struct TPLDATA *tt;	/* Temp pointer to tie point data records */
static double *data;	/* Raw data array for model routine */
double *data_ptr;	/* Pointer to data */
double bx, by;		/* Temporary variable to hold x & y tie points */
int nvar;		/* Number of independent variables */
int count;	/* Loop counters */
int cnt;	/* Loop accumulators */
 
nvar = ((coef->degree + 1) * (coef->degree + 2) / 2) - 1;
if (data==NULL)
    data = (double *)MALLOC((coef->npts * MAX_COEF * sizeof(double)));
 
/* Loop on tie point pairs.  If the tie point is active, use it in the modeling
   calculations; first for x, then for y.
  --------------------------------------*/
coef->npts_fit = 0;
for(tt = tie_data, cnt = 0, count = 0; count < coef->npts; count++, tt++)
   {
   data_ptr = data + (MAX_COEF * cnt);
   if((tt->active == 0)||(tt->active == 1)||(tt->active > 9))
	{
	coef->npts_fit++;
	bx = tt->ref_coord[1];
	by = tt->ref_coord[0];
 
	*data_ptr++ = bx;
	*data_ptr++ = by;
 
	if (coef->degree > 1)
	   {
	   *data_ptr++ = bx * bx;
	   *data_ptr++ = bx * by;
	   *data_ptr++ = by * by;
	   }
	if (coef->degree > 2)
	   {
	   *data_ptr++ = bx * bx * bx;
	   *data_ptr++ = bx * bx * by;
	   *data_ptr++ = by * by * bx;
	   *data_ptr++ = by * by * by;
	   }
	if (coef->degree > 3)
	   {
	   *data_ptr++ = bx * bx * bx * bx;
	   *data_ptr++ = bx * bx * bx * by;
	   *data_ptr++ = by * by * bx * bx;
	   *data_ptr++ = bx * by * by * by;
	   *data_ptr++ = by * by * by * by;
	   }
	*data_ptr = tt->sea_coord[1];
	cnt++;
	}
   }
 
if(c_steplr(&(coef->npts_fit), &nvar, (double (*)[16])data, coef->x_coef, fit->alpha) != E_SUCC)
   { printf("Can not derive a fit in the X-direction\n"); exit(1); }
 
for(tt = tie_data, cnt = 0, count = 0; count < coef->npts; count++, tt++)
   {
     data_ptr = data + (MAX_COEF * cnt) + nvar;
     if((tt->active == 0)||(tt->active == 1)||(tt->active > 9))
	{
          *data_ptr = tt->sea_coord[0];
   	  cnt++;
	}
   }
if(c_steplr(&(coef->npts_fit), &nvar, (double (*)[16])data, coef->y_coef, fit->alpha) != E_SUCC)
   { printf("Can not derive a fit in the Y-direction\n"); exit(1); }
 
/* Record the number of coefficients in X and in Y
  -----------------------------------------------*/
for (count = 0, coef->x_ncoef = 0; count < (nvar + 1); count++)
   if(coef->x_coef[count] != 0.0) coef->x_ncoef++;
for (count = 0, coef->y_ncoef = 0; count < (nvar + 1); count++)
   if(coef->y_coef[count] != 0.0) coef->y_ncoef++;
 
/* Record the degrees of freedom in X and in Y
  -------------------------------------------*/
coef->x_freedom = coef->npts_fit - coef->x_ncoef;
coef->y_freedom = coef->npts_fit - coef->y_ncoef;
 
return(E_SUCC);
}

