static char sccsid_linear_lsq_c[] =
    "@(#)linear_lsq.c	1.2 96/04/09 19:13:28";

/* linear_lsq(f_vec,e_vec,n,num_smpl,f_coff)  -----------------

****************************************************************
*                                                              *
*			linear_lsq.c		               *
*						               *
****************************************************************

This is a "least squares fit to a polynomial of the first degree"
routine.  It attempts to fit the  linear curve y = a + bx
to the 'n' data points supplied, using a subset of 'num_smpl' data
points evenly spaced through the data.

The results of this routine are the coefficients a and b of the
linear equation that best fits these points.

INPUTS:
variable:	f_vec
type:		float
description:   	vector for  y=f(x) from  x=0 to x=(n-1)	

variable:       e_vec
type:		integer
description:	validity vector corresponding to f_vec.  any
		non-zero value in this vector means the corres-
		ponding element in f_vec is not valid.

variable:	n
type:		integer
description:	number of elements in f_vec and e_vec.

variable:	num_smpl
type:		integer
description:	number of samples to take from f_vec to do the curve 
		fitting.  Points will be spaced n/num_smpl elements
		apart, starting with element 0.

OUTPUTS:
variable:	f_coff
type:		float
description:	a two element vector corresponding to the 
		coefficients a and b for the above mentioned
		linear equation.

Algorithm:		|Syi		Sxi
		a =(1/D)|Sxi.yi 	Sxi**2

			|N		Syi
		b =(1/D)|Sxi		Sxi.yi

			|N		Sxi
		D =	|Sxi		Sxi**2

		where Sxi equals the summation of xi from i=0 to (n-1)
		D is the determinant of the matrix.
		N is the total number of points.
*/


#include<stdio.h>

linear_lsq(f_vec,e_vec,n,num_smpl,f_coff)
    float     *f_vec, *f_coff;
    long int  *e_vec,n,num_smpl;

{
    long int  i,inc_size,np;
    double    sum_y,sum_x,sum_x2,sum_xy,determnt,matrix;

    if (num_smpl == 0) printf("num_smpl is 0\n");
    inc_size = n/num_smpl;

    sum_y  = 0;
    sum_x  = 0;
    sum_x2 = 0;
    sum_xy = 0;
    np     = 0;
    for(i=1; i<(n+1);i=i+inc_size){
       if (e_vec[i-1] == 0) {
	   np++;
	   sum_y 	= sum_y  + f_vec[i-1];
	   sum_x 	= sum_x  + i;
	   sum_x2	= sum_x2 + (i*i);
	   sum_xy	= sum_xy + (i*f_vec[i-1]);
       }
    }

    determnt  = (np * sum_x2) - (sum_x * sum_x);

    if(determnt == 0.) {
	printf("determnt == 0\n");
	if (abob()) exit(1);
    }
    matrix    = (sum_y * sum_x2) - (sum_x * sum_xy);
    f_coff[0] = matrix/determnt;

    matrix    = (np * sum_xy) - (sum_y * sum_x);
    f_coff[1] = matrix/determnt;
}
