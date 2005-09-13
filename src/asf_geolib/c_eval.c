/****************************************************************************
NAME:				c_eval

PURPOSE:  Evaluates a polynomial at a given point

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
  5.0		1/89	D. Steinwand	   CSB      LAS 5.0 Development

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Determine degree of polynomial
	Return evaluation
*****************************************************************************/
#include "worgen.h"

double FUNCTION c_eval(
	int *degree,		/* Degree of polynomial */
	double *a,		/* Array of polynomial coefficients */
	double *x,double *y)		/* X & Y coordinates to be evaluated */
{
if(*degree == 4)return(a[0]+ *x*(a[1]+ *x*(a[3]+ *x*(a[6]+a[10]* *x+a[11]* 
                   *y)+a[7]* *y)+a[4]* *y) + *y*(a[2]+ *y*(a[5]+ *y*(a[9]+
                   a[14]* *y+a[13]* *x)+a[8]* *x+a[12]* *x* *x)));
if(*degree == 3)return(a[0]+ *x*(a[1]+ *x*(a[3]+a[6]* *x+a[7]* *y)+a[4]* *y)
                  + *y*(a[2]+ *y*(a[5]+a[9]* *y+a[8]* *x)));
if(*degree == 2)return(a[0]+ *x*(a[1]+a[3]* *x+a[4]* *y)+ *y*(a[2]+a[5]* *y));
return(a[0]+a[1]* *x+a[2]* *y);
}
