static char sccsid_pregp4l_c[] =
    "@(#)pregp4l.c	1.2 96/04/09 19:13:30";

/*********************************************************************/
/*								     */
/*		pregp4l.c					     */
/*  12/9/89: Quyen dinh Nguyen                                       */
/*           This routine will compute the location of good points   */
/*           for preprocessing.                                      */
/*  5/9/90:  modified to put the offset to all 4looks.               */
/*           Using the SEASAT (beaufort sea scene),we need the offset*/
/*								     */
/*********************************************************************/


/*  pregp4l(forfftsize,fd,fdot,prf,pbw, --------------------------------
          ix0,ix1,ix2,ix3,vlpts,ngood_look)

This routine returns the 4 values which are the location of the 
starting bins within each of four looks in the preprocessing mode.

In fourlook mode, the four azimuth transfer functions are identical
for each look. In addition, the azimuth transfer functions only
contain the quadratic term and do not adjust for linear phase term.
For preprocessing mode, we need to align the fourlook  to study
the Doppler centroid.


INPUTS:
variable:       forfftsize
type:           int
description:    Forward FFT size

variable:       fd
type:           float
description:    Doppler centroid frequency (Hz).

variable:       fdot
type:           float
description:    Doppler rate frequency (Hz/sec).

variable:       prf 
type:           float
description:    Pulse duration Frequency (Hz).

variable:       pbw 
type:           float 
description:    the processing bandwidth (Hz). 


OUTPUTS:
variable:       ix0
type:           pointer of float
description:    the starting good point location for look 1. 

variable:       ix1
type:           pointer of float
description:    the starting good point location for look 2. 

variable:       ix2
type:           pointer of float
description:    the starting good point location for look 3. 

variable:       ix3
type:           pointer of float
description:    the starting good point location for look 4. 

variable:       vlpts 
type:           pointer of int 
description:    The number points to be saved from each look. 

variable:       ngood_look 
type:           pointer of int 
description:    The number good points after compression for 
                each look. 

*/

#include <stdio.h>
#include <math.h>


pregp4l(forfftsize,fd,fdot,prf,pbw,
          ix0,ix1,ix2,ix3,vlpts,ngood_look)

int    forfftsize,*vlpts,*ngood_look;
float  fd,fdot,prf,pbw;
float  *ix0,*ix1,*ix2,*ix3;

{

   float   fr,prf4,offset,xstart,fftsize4;
   int     n_gt_look,igoodpt;
   float   fd1,fd2,fd3,fd4,t1,t2,t3,t4;
   float   x0,x1,x2,x3,xpbw2;

   /* calculate the abs of fr */
   fr   = (float)(fabs((double)(fdot)));
   prf4 = prf/4.0;
   fftsize4 = (float)(forfftsize/4);
   n_gt_look = (int)( ((float)(forfftsize) - pbw/fr*prf)/4.0);
   offset = (pbw/fr)*prf4;
   xstart = (pbw/fr*prf/4.0/2.0);
   xstart = 3.0*(pbw/fr*prf/4.0/2.0);

 /* put xstart back 5/9/90 to overlay the look correctly */
   *ix0 = 3.0*offset + xstart;
   *ix1 = 2.0*offset + xstart;
   *ix2 = offset + xstart;
   *ix3 = 0.0 + xstart;

 /* Old way which is not working 
   *ix0 = 0.0 - xstart;
   *ix1 = -offset - xstart;
   *ix2 = -2.0*offset - xstart;
   *ix3 = -3.0*offset - xstart;
 */

  /*
   printf(" the xstart=%f offset=%f\n",xstart,offset);
   printf(" the fftsize4=%f\n",fftsize4);
   printf(" the good points (QDN) are: %f %f %f %f\n",
               *ix0,*ix1,*ix2,*ix3);
  */
   
   *ix0 = (float)(fmod((double)(*ix0), (double)(fftsize4)));
   *ix1 = (float)(fmod((double)(*ix1), (double)(fftsize4)));
   *ix2 = (float)(fmod((double)(*ix2), (double)(fftsize4)));
   *ix3 = (float)(fmod((double)(*ix3), (double)(fftsize4)));

   if(*ix0<0.0) *ix0  += fftsize4;
   if(*ix1<0.0) *ix1  += fftsize4;
   if(*ix2<0.0) *ix2  += fftsize4;
   if(*ix3<0.0) *ix3  += fftsize4;
  
   igoodpt = (int)((float)( n_gt_look ) - 3.5*offset);
   *vlpts = (igoodpt > forfftsize/8) ? forfftsize/8 : igoodpt;
   *ngood_look = igoodpt;
   
  /*
   printf(" points after compress =%d \n",n_gt_look);
   printf(" VLPTS= %d \n",*vlpts);
   printf(" the number of good points per look= %d \n",igoodpt);
   printf(" the good points (QDN) are: %f %f %f %f\n",
               *ix0,*ix1,*ix2,*ix3);
  */
  
   xpbw2 = pbw/2.0;
   fd1 = fd - 1.5*pbw - xpbw2;
   fd2 = fd - 0.5*pbw - xpbw2;
   fd3 = fd + 0.5*pbw - xpbw2;
   fd4 = fd + 1.5*pbw - xpbw2;
   t1 = fd1/fr;
   t2 = fd2/fr;
   t3 = fd3/fr;
   t4 = fd4/fr;
  
   x0 = fmod( t1*prf4, fftsize4);
   if(x0<0.0) x0  += fftsize4;
   x1 = fmod( t2*prf4, fftsize4);
   if(x1<0.0) x1  += fftsize4;
   x2 = fmod( t3*prf4, fftsize4);
   if(x2<0.0) x2  += fftsize4;
   x3 = fmod( t4*prf4, fftsize4);
   if(x3<0.0) x3  += fftsize4;
  /*
   printf(" the time = %f %f %f %f \n",t1,t2,t3,t4);
   printf(" the good points( RC) are: %f %f %f %f\n",
            x0,x1,x2,x3);
  */

}
