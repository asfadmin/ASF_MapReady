/****************************************************************************
NAME:				calc_residuals
 
PURPOSE:  Calculates residual errors & related statistics for a given set
	  of tie points and transformation coefficients
 
PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    3/82   R. Wenger     CSC      N/A    (Old name:  TIEPOINTS)
  4.0    5/85   S. Kondal     SAR      20-472 (Old name:  TIEFIT)
  5.0	 1/89	D. Steinwand  CSB      LAS 5.0 C conversion
 
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   must be run under TAE
 
PROJECT:  LAS
 
ALGORITHM DESCRIPTION:
	Initialize variables
	Loop on tie point pairs
	   If tie point is active,
		Evaluate the polynomial at the tie point
		Calculate residual error
		Calculate RES error
		Find maximum errors in x and in y
	Store maximum error in x
	Store maximum error in y
	Store RES error of maximum error in x
	Store RES error of maximum error in y
	Store the point id of the point with the maximum error in x
	Store the point id of the point with the maximum error in y
	Calculate & store the average x residual error
	Calculate & store the average y residual error
	Calculate & store the average RMS error
	Return
*****************************************************************************/

#include "2dmap.h"

 
void calc_residuals(tp, coef, residual)
 
struct TPLDATA *tp;		/* Tie Point Location data records */
struct COEF *coef;		/* Coefficient & stats from model */
struct RESIDUAL *residual;	/* Residual errors from fit */
{
struct TPLDATA *tt;		/* Temp pointer to tie point data records */
double x,y;			/* Calculated x & y locactions */
double delta_x,delta_y;		/* Absolute value of x & y residuals */
double rms_acc, x_acc, y_acc;	/* Accumulators */
double rms_sqr;			/* RMS square */
double max_err_x;		/* Maximum error in x or y */
double max_err_y;		/* Maximum error in x or y */
int acc;			/* Accumulator */
int count;			/* Loop counter */
int x_max_index, y_max_index;	/* Count values of max residuals in x & y */
 
/* Initialize function variables
  -----------------------------*/
tt = tp;
rms_acc = x_acc = y_acc = 0.0;
max_err_x = max_err_y = 0.0;
x_max_index = y_max_index = MAX_TIE_POINTS + 1;
 
/* Loop on tie point pairs.  If the tie point is active, evaluate the
   polynomial and  calculate residuals & rms errors
  ------------------------------------------------*/
for (acc = 0, count = 0; count < coef->npts; count++, tt++)
   {
   if((tt->active == 0)||(tt->active == 1)||(tt->active > 9))
	{
	x = c_eval(&(coef->degree),coef->x_coef,&(tt->ref_coord[1]),
                   &(tt->ref_coord[0]));
	y = c_eval(&(coef->degree),coef->y_coef,&(tt->ref_coord[1]),
                   &(tt->ref_coord[0]));
	residual->x_residual[count] = tt->sea_coord[1] - x;
	residual->y_residual[count] = tt->sea_coord[0] - y;
	delta_x = fabs(residual->x_residual[count]);
	delta_y = fabs(residual->y_residual[count]);
	rms_sqr = (delta_x * delta_x) + (delta_y * delta_y);
	residual->rms_residual[count] = sqrt(rms_sqr);
	x_acc += delta_x;
	y_acc += delta_y;
	rms_acc += rms_sqr;
 
        /* Store maximum errors in x & y
          -----------------------------*/
	if (delta_x > max_err_x) {max_err_x=delta_x; x_max_index=count;}
	if (delta_y > max_err_y) {max_err_y=delta_y; y_max_index=count;}
	acc++;
	}
   }
 
/* Pack maximum error fields not packed in processing loop
  -------------------------------------------------------*/
residual->x_max_err = residual->x_residual[x_max_index];
residual->y_max_err = residual->y_residual[y_max_index];
residual->x_max_rms = residual->rms_residual[x_max_index];
residual->y_max_rms = residual->rms_residual[y_max_index];
strcpy(residual->x_max_ptid, (tp + x_max_index)->pt_id);
strcpy(residual->y_max_ptid, (tp + y_max_index)->pt_id);
 
/* Calculate average residuals
  ---------------------------*/
residual->x_ave_err = x_acc / acc;
residual->y_ave_err = y_acc / acc;
residual->ave_rmse = sqrt(rms_acc / acc);
 
return;
}

