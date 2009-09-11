#ifndef _SARSIM_H_
#define _SARSIM_H_

/****************************************************************
HEADER NAME:  sarsim.h
 
DESCRIPTION:  This include file defines constants and macros
              necessary for the SARSIM and SARGEOM routines.
 
PROGRAM HISTORY:
  Modified from LAS cproj.h and const.h files, T. Logan 10/93
****************************************************************/

 
/*---------------  Define "constants"  -----------------*/
#define PT      0
#define NORTH   1
#define SOUTH   2
#define EAST    3
#define WEST    4
#define ELEVDIM 5
#define HALF_PI 1.5707963268
#define TWO_PI 	6.2831853072
#define EPSLN	1.0e-10
#define OK	0               /* return value  */
#define ERROR  -1               /* return value  */
#define IN_BREAK -2             /* return value  */
 
/*---------------  Misc macros  -----------*/
#define SQUARE(x)       x * x                     /* x**2 */
#define CUBE(x)     x * x * x                     /* x**3 */
#define QUAD(x) x * x * x * x                     /* x**4 */
#define GMAX(A, B)      ((A) > (B) ? (A) : (B))   /* assign max of a and b */
#define GMIN(A, B)      ((A) < (B) ? (A) : (B))   /* assign min of a and b */
#define IMOD(A, B)      (A) - (((A) / (B)) * (B)) /* Integer mod function  */

/*Prototypes (for tcobj.a):*/
 void ptitle (char *A); 
 void radius (double A); 
 void radius2 (double A, double B); 
 void genrpt (double A, char *S); 
 void genrpt_long (int A, char *S); 
 int sign (double x); 
 void sincos (double val, double *sin_val, double *cos_val); 
 double adjust_lon (double x); 
 double mlfn (double e0, double e1, double e2, double phi); 
 int calc_utm_zone (double lon); 
 int mod_ddr (char *file, int newDataType); 
 int utm_init (int zone); 
 int tm_forward (double lon, double lat, double *x, double *y); 
 int tm_inverse (double x, double y, double *lon, double *lat); 
double e0fn(double x);
double e1fn(double x);
double e2fn(double x);
#endif
