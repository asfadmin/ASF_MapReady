/***********************************************************************
NAME:	    projfit --	determines quadratic coefficients given both the
			near and far swath coordinate vectors

SYNOPSIS:   projfit <inPROJfile> <outCOEFfile>

DESCRIPTION:
    uses the 'yax2bxc' routine to compute coefficients for a
    quadratic fit of the near and far edge swath coordinates, 
    then writes the computed coefficents to outCOEF file

FILE FORMATS
    inPROJfile:
	year, julian day, total sec.
	number of points
	offset(sec.), nearNorthing, nearEasting, farNorthing, farEasting
	...
	
    outCOEFfile:
	nearA, nearB, nearC
	farA,  farB,  farC
	
PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:         PURPOSE
    --------------------------------------------------------------
    1.0      7/00   M. Jessop       Determine the coefficients of a UTM
				    coordinate swath.
    2.0     11/00   J. Badgley      Changed to allow the use of any
				    projection type.

***********************************************************/
/******************************************************************************
*                                                                             *
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

#include <asf.h>


void yax2bxc(double x_vec[],double y_vec[],int n,double *a,double *b,double *c)
/******************************************************************************
DESCRIPTION:    Computes a, b, and c for y = ax^2 + bx + c using quadratic
                regression (least squares fit) given float vectors y and x
                of length n.
PARAMETERS:     x_vec   float[]         Input vector of X values
                y_vec   float[]         Input vector of Y values
                n       int             Length of input vectors
                a       float*          Return x^2 coefficient 
                b       float*          Return x coefficient
                c       float*          Return offset factor

HISTORY:        Ver 1.0   2/97   T. Logan    Initial Creation
                Ver 1.1   5/98   O. Lawlor   Changed to doubles
                                 floats become unstable for thousands
                                 of elements.
ALGORITHM REF:  Cheney, Ward & D. Kincaid, Numerical Mathematics and Computing,
                2nd Editn. pp 360-362. Brooks/Cole Pub. Co., Pacific Grove, Ca.
******************************************************************************/
{
 double x1, x2, x3, x4,         /* Sum of x, x^2, x^3, x^4 */
       y1, yx, yx2;             /* Sum of y, y*x, y*x^2    */
 double d1, d2, d3, d4, d5;      /* Intermediate Values     */
 double t1, t2, t3;             /* Equation Solutions      */
 int   i;

 /* Calculate all of the first order sums */
 x1 = x2 = x3 = x4 = y1 = yx = yx2 = 0.0;

 for (i=1; i<=n; i++) {
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

int main (int argc, char *argv[]) {
    int	    i, n;
    double  a1, b1, c1, 
	    a2, b2, c2;
    double *nrth_vec1=NULL;
    double *east_vec1=NULL;
    double  *nrth_vec2=NULL;
    double  *east_vec2=NULL;
    double tmp;    
    FILE    *fpi, *fpo;
    
    printf("\n");
    /* Test for proper arguments */
    if (argc != 3) { 
        printf("Usage: %s <inPROJfile> <outCOEFfile>\n",argv[0]); exit(1); 
    }
    
    /* Open input and output vector files */
    fpi = fopen(argv[1], "r");
    if ( fpi == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", argv[1]);
        exit(1);
    }
    
    for (i=1; i<=4; i++) { fscanf (fpi, "%lf", &tmp); }
    n = (int) tmp;
    printf("Number of lines parsed: %i\n", n);
    
    /* Allocate memory for vectors */
    nrth_vec1 = (double *) MALLOC ((n+1)*sizeof(double));
    east_vec1 = (double *) MALLOC ((n+1)*sizeof(double));
    nrth_vec2 = (double *) MALLOC ((n+1)*sizeof(double));
    east_vec2 = (double *) MALLOC ((n+1)*sizeof(double));
    
    /* Read UTM northing and easting vectors for near and far edge */
    for (i=1; i<=n; i++) {
	fscanf (fpi, "%lf", &tmp);	    /* ignore the time */
	fscanf (fpi, "%lf", &nrth_vec1[i]);
	fscanf (fpi, "%lf", &east_vec1[i]);
	fscanf (fpi, "%lf", &nrth_vec2[i]);
	fscanf (fpi, "%lf", &east_vec2[i]);
    }
    fclose(fpi);
    
    /* Determine a, b, and c coefficients */
    
    yax2bxc(nrth_vec1, east_vec1, n-1, &a1, &b1, &c1);
    yax2bxc(nrth_vec1, east_vec2, n-1, &a2, &b2, &c2);
     
    /* Write to outCOEF file */
    fpo = fopen(argv[2], "w");
    fprintf(fpo, "%1.12e %1.12e %1.12e\n", a1, b1, c1);
    fprintf(fpo, "%1.12e %1.12e %1.12e\n", a2, b2, c2);
    fclose(fpo);
}
