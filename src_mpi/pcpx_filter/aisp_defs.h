/****************************************************************************
*								            *
*   aisp_def.h - Global type, function, and constants definitions            *
*   Copyright (C) 1997  ASF STEP LAB 			   	    	    *
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

/*-----------------------------------------------------*/
/* define complex variable type if not already defined */
/*-----------------------------------------------------*/
#include "asf.h"

#ifndef __complex_var
#define __complex_var
typedef struct {
   float r;
   float i;
} FCMPLX;
#endif


/*-----------------------------*/
/* Simple function definitions */
/*-----------------------------*/
#define   NINT(a)       ((a)>=0.0 ? (int)((a)+0.5):(int)((a)-0.5)) /* nearest int */
#define   MIN(a,b)      (((a) < (b)) ? (a) : (b))                    /* minimum     */
#define   MAX(a,b)      (((a) > (b)) ? (a) : (b))                    /* maximum     */



float  Cabs(FCMPLX); 
FCMPLX Cadd (FCMPLX,FCMPLX);
FCMPLX Cconj(FCMPLX);
FCMPLX Cmplx(float,float);
FCMPLX Czero();
FCMPLX Csmul(float,FCMPLX);
FCMPLX Cmul (FCMPLX,FCMPLX);

/*cfft1d: Perform FFT, 1 dimentional:
	dir=0 -> init; 
	dir<0 -> forward; 
	dir>0 -> backward*/
void cfft1d(int n, FCMPLX *c, int dir);

/*-----------------------*/
/* Constants Definitions */
/*-----------------------*/
#define   default_n_az 4096

#define   speedOfLight 299792458.0 /*Speed of light in vacuum, m/s */
#define   pi      	3.14159265358979323
#define   pi2     	(2*pi)

