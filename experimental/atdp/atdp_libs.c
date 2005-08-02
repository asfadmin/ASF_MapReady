/****************************************************************************
*								            *
*   aisp_libs.c              Miscellaneous routines used by others          *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
/****************************************************************************
FUNCTION NAME:	General Routines that are used by the ASP software

SYNTAX, DESCRIPTION, and RETURN VALUES:

float	Cabs(complexFloat a)			Returns Scalar Magnitude of a
complexFloat	Cconj(complexFloat a)			Returns Complex Conjugate of a
complexFloat	Czero()				Returns Complex Zero
complexFloat	Cadd(complexFloat a, complexFloat b)	Returns Complex Sum of a + b
complexFloat	Cmplx(float a, float b)		Returns Complex a + bi
complexFloat	Csmul(float s, complexFloat a)	Returns Complex s times a
complexFloat	Cmul(complexFloat a, complexFloat b)	Returns Complex a times b

void 	elapse(int fnc)			Elapsed wall clock timer

void	yaxb(float[], float[], int, float*, float*)
   	--Computes a linear regression based on least squares fit

void	yax2bxc(float[], float[], int, float*, float*, float*)
        --Finds quadratic fit of input data using least squares regression.

void 	save_meta(meta, fname, nl, ns, sl, ss, pdx, pdy, li)
	--Creates a valid meta file with windowing information

SPECIAL CONSIDERATIONS:
PROGRAM HISTORY:  Ver 1.0  T. Logan - Most routines are new
****************************************************************************/
#include "asf.h"
#include <sys/time.h>
#include "atdp_defs.h"
#include "las.h"
#include "locinc.h"

/* Global Variables used by the complexFloat Arithmetic Routines */
complexFloat a,b,x;
float d;
int i;

float  Cabs(complexFloat a)
{
  d = sqrt (a.real*a.real + a.imag*a.imag);
  return d;
}

complexFloat Cconj(complexFloat a)
{
  x.real = a.real;
  x.imag = -a.imag;
  return x;
}

complexFloat Czero()
{
  x.real = 0.0;
  x.imag = 0.0;
  return x;
}

complexFloat Cadd (complexFloat a, complexFloat b)
{
  x.real = a.real+b.real;
  x.imag = a.imag+b.imag;
  return x;
}

complexFloat Cmplx(float a, float b)
{
  x.real = a;
  x.imag = b;
  return x;
}

complexFloat Csmul(float s, complexFloat a)
{
  x.real=s*a.real;
  x.imag=s*a.imag;
  return x;
}

complexFloat Cmul (complexFloat a, complexFloat b)
{
  x.real = a.real*b.real - a.imag*b.imag;
  x.imag = a.real*b.imag + a.imag*b.real;
  return x;
}

/****************************************************************
FUNCTION NAME: elapse - an elapsed time wall clock timer
PARAMETER:   fnc  int	start (0) / stop (!0) switch
DESCRIPTION:
    The input parameter is the start/stop button.  If mode = 0, the timer
    is started.  If mode != 0, the timer is stopped and the elapsed time
    is displayed.
HISTORY: 1.0 - Tom Logan   4/97  Modified from stopwatch functions
****************************************************************/
void elapse(int fnc)
{
  struct timeval tp2;
  struct timezone tzp2,tzp1;
  static struct timeval tp1;
  
  if (fnc == 0)
    gettimeofday(&tp1,&tzp1);
  else
    { 
      gettimeofday(&tp2,&tzp2);
      printf("   elapsed time = %i seconds.\n\n",(int)(tp2.tv_sec-tp1.tv_sec));
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
FUNCTION NAME:  save_meta -- make a metadata file

PARAMETERS: 	meta_parameters meta  Metadata structure pointer
		fname	 char *	  Name of file to create a .meta for
		nl, ns	 int	  Number of lines and samples in file
		sl, ss   int	  Master file start line and sample (windowing)
		pdx, pdy float	  Projection distance in x and y 
		li	 int	  Line Increment
DESCRIPTION:
    The purpose here is to retain the image metadata.

SPECIAL CONSIDERATIONS:
    Sets data type to REAL32 (float) which is not actually correct for cpx files.

PROGRAM HISTORY:
  VERSION   DATE   AUTHOR       PURPOSE
  -------   ----   ------       -------
   1.0      4/95   T. Logan     Retain image metadata after resampling
   1.1      6/95   T. Logan     Validate DDR fields that are valid
   1.2     10/95   M. Shindle   Now accepts data type as a parameter
   2.0      4/97   T. Logan     Copied from create_ddr
   2.5      1/03   P. Denny     Changed to .meta format in effort to NUKE
                                  the DDR
***************************************************************************/
void save_meta(meta_parameters *meta, const char *fname,
               int nl,int ns,int sl,int ss,
               double pdx,double pdy, int li)
{
  meta->general->line_count = nl;
  meta->general->sample_count = ns;
  meta->general->start_line = sl;
  meta->general->start_sample = ss;
  meta->general->x_pixel_size = pdx;
  meta->general->y_pixel_size = pdy;
  meta->sar->line_increment = (double) li;
  meta->sar->sample_increment = 1.0;
  meta_write(meta, fname);
  
  return;
}
