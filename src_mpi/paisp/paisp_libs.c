/****************************************************************************
*								            *
*   aisp_libs.c              Miscellaneous routines used by others           *
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

float	Cabs(FCMPLX a)			Returns Scalar Magnitude of a
FCMPLX	Cconj(FCMPLX a)			Returns Complex Conjugate of a
FCMPLX	Czero()				Returns Complex Zero
FCMPLX	Cadd(FCMPLX a, FCMPLX b)	Returns Complex Sum of a + b
FCMPLX	Cmplx(float a, float b)		Returns Complex a + bi
FCMPLX	Csmul(float s, FCMPLX a)	Returns Complex s times a
FCMPLX	Cmul(FCMPLX a, FCMPLX b)	Returns Complex a times b

void 	elapse(int fnc, const char *msg)	Elapsed wall clock timer

void	yaxb(float[], float[], int, float*, float*)
   	--Computes a linear regression based on least squares fit

void	yax2bxc(float[], float[], int, float*, float*, float*)
        --Finds quadratic fit of input data using least squares regression.

void 	save_ddr(fname, nl, ns, sl, ss, pdx, pdy, li)
	--Creates a valid ddr with windowing information

SPECIAL CONSIDERATIONS:
PROGRAM HISTORY:  Ver 1.0  T. Logan - Most routines are new
****************************************************************************/
#include "asf.h"


#include <sys/time.h>
#include "paisp_defs.h"
#include "las.h"
#include "locinc.h"

/* Global Variables used by the FCMPLX Arithmetic Routines */
FCMPLX a,b,x;
float d;
int i;

float  Cabs(FCMPLX a)   { d = sqrt (a.r*a.r + a.i*a.i); return d; }

FCMPLX Cconj(FCMPLX a)  { x.r = a.r; x.i = -a.i; return x; }

FCMPLX Czero()          { x.r = 0.0;  x.i = 0.0;   return x; }

FCMPLX Cadd (FCMPLX a, FCMPLX b) { x.r = a.r+b.r; x.i=a.i+b.i; return x; }

FCMPLX Cmplx(float a,  float b)  { x.r = a; x.i = b; return x; }

FCMPLX Csmul(float s,  FCMPLX a) { x.r=s*a.r; x.i=s*a.i; return x; }

FCMPLX Cmul (FCMPLX a, FCMPLX b)
	{ x.r = a.r*b.r-a.i*b.i; x.i = a.r*b.i+a.i*b.r;  return x; }

/****************************************************************
FUNCTION NAME: elapse - an elapsed time wall clock timer
PARAMETER:   fnc  int	start (0) / stop (!0) switch
DESCRIPTION:
    The input parameter is the start/stop button.  If mode = 0, the timer
    is started.  If mode != 0, the timer is stopped and the elapsed time
    is displayed.
HISTORY: 1.0 - Tom Logan   4/97  Modified from stopwatch functions
****************************************************************/
void elapse(int fnc, const char *msg)
  {
    struct timeval tp2;
    struct timezone tzp2,tzp1;
    static struct timeval tp1;

    if (fnc == 0)
      gettimeofday(&tp1,&tzp1);
    else
      { 
	gettimeofday(&tp2,&tzp2);
        printf("%s: elapsed time = %i seconds.\n",msg,(int)(tp2.tv_sec-tp1.tv_sec));
      }
  }

/******************************************************************************
NAME: 		yaxb.c
DESCRIPTION:	Computes a and b for y = ax + b using linear regression
		given float vectors y and x of length n.
PARAMETERS:	x_vec	float[]		Input vector of X values
		y_vec	float[]		Input vector of Y values
		n	int		Length of input vectors
		a	float*		Return slope factor
		b	float*		Return offset factor
HISTORY: 	Ver 1.0   2/97   T. Logan    Initial Creation 
                Ver 1.1   5/98   O. Lawlor   Changed to doubles
                                 floats become unstable for thousands
                                 of elements.
ALGORITHM REF:  Cheney, Ward & D. Kincaid, Numerical Mathematics and Computing,
		2nd Editn. pp 360-362. Brooks/Cole Pub. Co., Pacific Grove, Ca.
******************************************************************************/
void yaxb(float x_vec[], float y_vec[], int n, float *a, float *b)
  {
   double sum_x=0.0, sum_xx=0.0, sum_xy=0.0, sum_y=0.0;
   double d, at, bt;
   int   i;

   for (i=0; i<n; i++)
    {
      sum_x  += x_vec[i];
      sum_y  += y_vec[i];
      sum_xx += x_vec[i] * x_vec[i];
      sum_xy += x_vec[i] * y_vec[i];
     }
   d =  n*sum_xx - sum_x*sum_x;
   at = n*sum_xy - sum_x*sum_y;
   bt = sum_xx * sum_y - sum_x * sum_xy;

   *a = at/d;
   *b = bt/d;

   return;
  }

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

/***************************************************************************
FUNCTION NAME:  save_ddr -- make a LAS 6.0 ddr metadata file

PARAMETERS: 	fname	 char *	  Name of file to create a ddr for
		nl, ns	 int	  Number of lines and samples in file
		sl, ss   int	  Master file start line and sample (windowing)
		pdx, pdy float	  Projection distance in x and y 
		li	 int	  Line Increment
DESCRIPTION:
    This functions creates a LAS ddr structure and save it to a file. The
 purpose here is to retain the image metadata in a convenient fashion.  The
 ddr file is initialized with the size of the image (nl,ns), image data type,
 the coordinates of the window from the original image (sl,ss), and the size
 of the image pixels.

SPECIAL CONSIDERATIONS:
    Sets data type to REAL (4) which is not actually correct for cpx files.

PROGRAM HISTORY:
  VERSION   DATE   AUTHOR       PURPOSE
  -------   ----   ------       -------
   1.0      4/95   T. Logan     Retain image metadata after resampling
   1.1      6/95   T. Logan     Validate DDR fields that are valid
   1.2     10/95   M. Shindle   Now accepts data type as a parameter
   2.0      4/97   T. Logan     Copied from create_ddr
***************************************************************************/
void save_ddr(const char *fname,int nl,int ns,int sl,int ss,
	      double pdx,double pdy, int li)
{
  struct DDR ddr;
  int stati;
  char system[40];

  c_intddr(&ddr);
  ddr.nl = (int) nl;
  ddr.ns = (int) ns;
  ddr.master_line = (int) sl;
  ddr.master_sample = (int) ss;
  ddr.pdist_x = (double) pdx;
  ddr.pdist_y = (double) pdy;
  ddr.nbands = 1;
  ddr.dtype = 4;
  ddr.line_inc = (int) li;
  ddr.sample_inc = 1.0;
  strcpy(ddr.proj_units,"METERS");
  ddr.valid[4] = VALID;          /* Projection Units    */
  ddr.valid[5] = VALID;          /* Projection Distance */
  ddr.valid[7] = VALID;          /* Increment           */
  strcpy(ddr.system,system);
  stati = c_putddr(fname, &ddr);
  if (stati!=E_SUCC) { printf("Error returned from putddr\n"); exit(1); }

  return;
}
