/****************************************************************************
*								            *
*   fix_defs.c              Miscellaneous routines used by others           *
*   Copyright (C) 1997  ASF STEP LAB 			   	    	    *
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
*   ASF STEP LAB Contacts:						    *
*	Lab Coordinator   - Rick Guritz		rguritz@images.alaska.edu   *
*	Software Engineer - Tom Logan		tlogan@images.alaska.edu    *
* 									    *
*	Alaska SAR Facility			STEP Lab Web Site:	    *	
*	Geophysical Institute			www.images.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
/****************************************************************************
FUNCTION NAME:	General Routines that are used by the ASP software

SYNTAX, DESCRIPTION, and RETURN VALUES:

void	yax2bxc(float[], float[], int, float*, float*, float*)
        --Finds quadratic fit of input data using least squares regression.

****************************************************************************/
/* #include "asf.h"
 * 
 * 
 * #include <sys/time.h>
 * #include "aisp_defs.h"
 * #include "las.h"
 * #include "locinc.h"
 */
/******************************************************************************
NAME: 		yax2bxc.c
DESCRIPTION:	Computes a, b, and c for y = ax^2 + bx + c using quadratic
		regression (least squares fit) given float vectors y and x
		of length n.
PARAMETERS:	x_vec	float[]		Input vector of X values
		y_vec	float[]		Input vector of Y values
		n	int		Length of input vectors
		a	float*		Return x^2 coefficient 
		b	float*		Return x coefficient
		c	float*		Return offset factor
HISTORY:      	Ver 1.0   2/97   T. Logan    Initial Creation
                Ver 1.1   5/98   O. Lawlor   Changed to doubles
                                 floats become unstable for thousands
                                 of elements.
ALGORITHM REF: 	Cheney, Ward & D. Kincaid, Numerical Mathematics and Computing,
	       	2nd Editn. pp 360-362. Brooks/Cole Pub. Co., Pacific Grove, Ca.
******************************************************************************/
void yax2bxc(float x_vec[],float y_vec[],int n,float *a,float *b,float *c)
{
 double x1, x2, x3, x4,		/* Sum of x, x^2, x^3, x^4 */
       y1, yx, yx2;		/* Sum of y, y*x, y*x^2    */
 double d1, d2, d3, d4, d5;      /* Intermediate Values     */
 double t1, t2, t3;		/* Equation Solutions      */
 int   i;

 /* Calculate all of the first order sums */
 x1 = x2 = x3 = x4 = y1 = yx = yx2 = 0.0;

 for (i=0; i<n; i++) {
    x1  += x_vec[i];
    x2  += x_vec[i] * x_vec[i];
    x3  += x_vec[i] * x_vec[i] * x_vec[i];
    x4  += x_vec[i] * x_vec[i] * x_vec[i] * x_vec[i];
    y1  += y_vec[i];
    yx  += y_vec[i] * x_vec[i];
    yx2 += y_vec[i] * x_vec[i] * x_vec[i];
  }

 d1 = n*x2  - x1*x1;
 d2 = n*x3  - x1*x2;
 d3 = n*x4  - x2*x2;
 d4 = n*yx  - x1*y1;
 d5 = n*yx2 - x2*y1;

 t1 = (d1*d5 - d2*d4) / (d1*d3 - d2*d2);
 t2 = (d4 - d2*t1) / d1;
 t3 = (y1 - x2*t1 - x1*t2) / n;

 *a = t1;
 *b = t2;
 *c = t3;

 return;
}
