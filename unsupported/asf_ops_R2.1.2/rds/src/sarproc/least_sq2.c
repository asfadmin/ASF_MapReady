static char sccsid_least_sq2_c[] =
    "@(#)least_sq2.c	1.2 96/04/09 19:13:28";

/*  least_sq2(f_vec,e_vec,n,num_smpl,f_coff)  ---------------

****************************************************************
*			least_sq2.c 		               *
****************************************************************

This is a least squares fit to a polynomial of the second degree 
routine.  It attempts to fit the  quadratic curve y = a+bx+cx**2
to the 'n' data points supplied using a sampling size of 'num_smpl'.

The results of this routine are the coefficients a,b and c of the 
quadratic equation that best fits these points.

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
description:	size of vector

variable:	num_smpl
type:		integer
description:	number of samples taken to do the curve fitting.

OUTPUTS:
variable:	f_coff
type:		float
description:	a three element vector corresponding to the 
		coefficients a, b and c for the above mentioned
		quadratic equation.

Algorithm:		|Syi		Sxi		Sxi**2|
		a =(1/D)|Sxi.yi 	Sxi**2  	Sxi**3|
			|Sxi**2.yi	Sxi**3		Sxi**4|

			|N		Syi		Sxi**2|
		b =(1/D)|Sxi		Sxi.yi		Sxi**3|
			|Sxi**2		Sxi**2.yi	Sxi**4|

			|N		Sxi		Syi   |
		c =(1/D)|Sxi		Sxi**2		Sxi.yi|
			|Sxi**2		Sxi**3	     Sxi**2.yi|

			|N		Sxi		Sxi**2|
		D =	|Sxi		Sxi**2		Sxi**3|
			|Sxi**2		Sxi**3		Sxi**4|

		where Sxi equals the summation of xi from i=0 to (n-1)
		D is the determinant of the matrix.
		N is the total number of points.
*/


#include <stdio.h>

least_sq2(f_vec,e_vec,n,num_smpl,f_coff)
float  *f_vec, *f_coff;
long int  *e_vec,n,num_smpl;

{
long int  i,inc_size,np;
double  sum_y,sum_x,sum_x2,sum_x3,sum_x4,sum_xy,sum_x2y,determnt;
double  matrix;

inc_size= n/num_smpl;

sum_y 	= 0;
sum_x 	= 0;
sum_x2	= 0;
sum_x3	= 0;
sum_x4	= 0;
sum_xy  = 0;
sum_x2y = 0;
np      = 0;
for(i=1; i<(n+1);i=i+inc_size){
   if (e_vec[i-1] == 0) {
       np++;
       sum_y 	= sum_y  + f_vec[i-1];
       sum_x 	= sum_x  + i;
       sum_x2	= sum_x2 + (i*i);
       sum_x3	= sum_x3 + (i*i*i);
       sum_x4	= sum_x4 + (i*i*i*i);
       sum_xy	= sum_xy + (i*f_vec[i-1]);
       sum_x2y	= sum_x2y+ (i*i*f_vec[i-1]);
   }
}

determnt=(np*sum_x2*sum_x4)+(sum_x*sum_x3*sum_x2)+
	 (sum_x2*sum_x3*sum_x)-(sum_x2*sum_x2*sum_x2)-
	 (sum_x3*sum_x3*np)-(sum_x4*sum_x*sum_x);

matrix =   (sum_y*sum_x2*sum_x4)+(sum_x*sum_x3*sum_x2y)+
	   (sum_x2*sum_x3*sum_xy)-(sum_x2y*sum_x2*sum_x2)-
	   (sum_x3*sum_x3*sum_y)-(sum_x4*sum_x*sum_xy);
f_coff[0] = matrix/determnt;


matrix    =(np*sum_xy*sum_x4)+(sum_y*sum_x3*sum_x2)+
	   (sum_x2*sum_x2y*sum_x)-(sum_x2*sum_xy*sum_x2)-
	   (sum_x2y*sum_x3*np)-(sum_x4*sum_y*sum_x);

f_coff[1] = matrix/determnt;

matrix     =(np*sum_x2*sum_x2y)+(sum_x*sum_xy*sum_x2)+
	    (sum_y*sum_x3*sum_x)-(sum_x2*sum_x2*sum_y)-
	    (sum_x3*sum_xy*np)-(sum_x2y*sum_x*sum_x);
f_coff[2]  = matrix/determnt;

}
