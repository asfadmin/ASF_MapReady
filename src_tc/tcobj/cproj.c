/****************************************************************************
FUNCTION NAME:  Projection Support Routines
SYNTAX:       Variable, based on the routine required
PARAMETERS:   Variable, based on the routine required
DESCRIPTION:  A collection of one-line support routines from the LAS
              projection transformation package.  
RETURN VALUE: Variable, based on the routine required
SPECIAL CONSIDERATIONS: None
PROGRAM HISTORY:
   D. Steinwand    EROS   July, 1991    original implementation
   Tom Logan       ASF    Oct 21, 1994  additional commenting and clean up
****************************************************************************/
#include "asf.h"
#include "sarsim.h"
 
/* Functions to report projection parameters
  -----------------------------------------*/
void ptitle(A) char *A; { printf("\n%s Projection Parameters:\n\n",A); }
void radius(A) double A;{printf("   Radius of Sphere:     %f meters\n",A); }
void radius2(A,B) double A,B;{
   printf("   Semi-Major Axis of Ellipsoid:     %f meters\n",A);
   printf("   Semi-Minor Axis of Ellipsoid:     %f meters\n",B); }
void cenlon(A) double A;{printf("   Longitude of Center:     %f degrees\n",A*R2D);}
void cenlat(A) double A;{printf("   Latitude  of Center:     %f degrees\n",A*R2D);}
void origin(A) double A;{printf("   Latitude  of Origin:     %f degrees\n",A*R2D);}
void genrpt(A,S) double A; char *S;{printf("   %s %f\n", S, A);}
void genrpt_long(A,S) int A; char *S;{printf("   %s %d\n", S, A);}
void pblank() {printf("\n");}
 
#ifndef sunos
/* Function to calculate the sine and cosine in one call.  Some computer
   systems have implemented this function, resulting in a faster 
   implementation than calling each function separately.  It is provided
   here for those computer systems which don`t implement this function
  ----------------------------------------------------------------------*/
void sincos(val, sin_val, cos_val) double val; double *sin_val; double *cos_val;
   { *sin_val = sin(val); *cos_val = cos(val); }
#endif

/* Function to return the sign of an argument
  ------------------------------------------*/
int sign(x) double x; { if (x < 0.0) return(-1); else return(1);}
 
/* Function to adjust longitude to -180 to 180
  -------------------------------------------*/
double adjust_lon(x) double x;
{  x = (fabs(x)<PI) ? x : (x-(sign(x)*TWO_PI)); return(x);  }
 
/* Functions to compute constants e0,e1,e2, and M
  ----------------------------------------------*/
double e0fn(x) double x;{return(1.0-0.25*x*(1.0+x/16.0*(3.0+1.25*x)));}
double e1fn(x) double x;{return(0.375*x*(1.0+0.25*x*(1.0+0.46875*x)));}
double e2fn(x) double x;{return(0.05859375*x*x*(1.0+0.75*x));}
double mlfn(e0,e1,e2,phi) double e0,e1,e2,phi;
                        {return(e0*phi-e1*sin(2.0*phi)+e2*sin(4.0*phi));}
 
/* Function to calculate UTM zone number -- Longitude entered in DEGREES!!
  -----------------------------------------------------------------------*/
int calc_utm_zone(lon) double lon; {return((int)(((lon+180.0)/6.0)+1.0)); }
