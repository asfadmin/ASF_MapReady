static char sccsid_az4lpre_c[] =
    "@(#)az4lpre.c	1.2 96/04/09 19:13:23";

#include <stdio.h>
#include <math.h>

/* done by quyen dinh nguyen
 * 9/19/89
 */


/* az4l_pre( forfftsize, roffset, prf, pbw, ----------------------------
      fdgrd, frgrd,
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, istat)

   This routine will generate the parameter for Azimuth Transfer
   Function Board (XFER), the Azimuth Interpolation Board (AZINT),
   and the Multilook Controller Board (MLC) for 4look processor 
   in the preprocessing mode

 INPUT:
   forfftsize    int*4    the forward fft size.

   roffset       int*4    The first azimuth line out of Range Migration
                          board. (This value is from the rm1l or rm4l
                          routine ).
   
   prf           float    the Pulse repitition frequency (Hz).
  
   pbw           float    the processing bandwidth (Hz).The  value
			  is for a 4look total bandwidth.

   fdgrd         vector of float  the three coefficient of the Doppler
                                  centroid as a function of slant range
                                  bin. The Doppler centroid of slant range 
                                  bin n is calculated as: 
                                     fdgrd(n)=(fdgrd[2]*n + fdgrd[1])*n 
                                              + fdgrd[0]
                                  where n is 0,1,2,...N.
  
   frgrd         vector of float  the three coefficent of the Doppler
                                  rate as a function of slant range bin.
                                  The Doppler centroid of slant range bin 
                                  n is calculated as: 
                                     frgrd(n) = (frgrd[2]*n+frgrd[1])*n 
                                             + frgrd[0]
                                  where n is 0,1,2,...N.

   istat         int       For input:
                              istat = 1 for verbal messages.
                              istat = 0 for no error messages.

 OUTPUT:
  aw        vector of int*2    the Azimuth weighting function
                               (XFER board).
  bm        vector of int*2    Rho value (spacecraft beam wavelength)
                               (XFER board).
  ls        vector of int*2    Lookstart (XFER board).

  qd        vector of int*2    gamma1 (quadratic offset) (XFER board).
  
  th        vector of int*2    gamma2 (theta offset) (XFER board).
 
  wt        vector of int*2    weightstart (starting point for Azimuth 
                               weight) (XFER board).

  lookpos   int*2              First nonzero point in a look,
                               zero in one look case (XFER board).

  looklen   int*2              number of "good" points in a look
                               (not used in onelook case) (XFER board).

  gp        vector of int*2    location of first good point (AZINT).

  pstspace  int*4              13 bit represent the fractional
                               interpolator point space. (AZINT).

  io        vector of int*2    Offset in Multi-look board (MLC).
  
  vlpts     int*4              valid points per look (VLPTS) (MLC).

  nov       int*4              non overlap between look (NOV) (MLC).

  azoff     int*4              frame offset (MLC,MLM).

  bufrdlen  int*4              range line offset between frames 
                               (have to be even) (INPUT,CTC).

  pbwx      float              the effective processing bandwidth .The
			       return value will be 4look total 
			       bandwidth

  istat     int*4              0 if no error.
                               >0 if parameter is overflow.
                               <0 if error in selecting parameters.

*/
az4l_pre( forfftsize, roffset, prf, pbw,  
      fdgrd, frgrd,
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, istat)

int       forfftsize, roffset;
int       *pstspace, *vlpts, *nov, *azoff, *bufrdlen, *istat;
short int aw[1], bm[1], ls[1], qd[1], th[1], wt[1], gp[1], io[1];
short int *lookpos,*looklen;
float     prf, pbw, *pbwx, fdgrd[1], frgrd[1];

{
 int        dmesg,i,j,k,idelbin,nw,kline,kstart,kend;
 int        ie,ifrac,imax50;
 int        ngdpt_look,ngdpt_far,ngdpt_near,ivlpts,inov;
 int        frameoffset,froff();
 int        maxdeskew;
 int        ngoodpt,ngoodpt_far,ngoodpt_near;
 short int  prho,gamma1,gamma2,theta;
 short int  weightstart,lookstart,ioffset,weight;
 short int  ix[4];
 float      ix0,ix1,ix2,ix3,fftsize4;
 float      xpbw,e,x, fd,fr, frabs, f2,fls,frac,temp,xifrac;
 float      xbin,fdot,fdnear,frnear,fdfar,frfar,prf4;
 float      fdmin,fdotmin,skewnear,skewfar;
 float      skewref,delerror,xskew,xoffset,xnov;
 float      xvlpts;
 float      f_value;
 double     fdummy,ltemp,itemp;

 /* init everything to zero */
 for(i=0; i<1024; i++)
    io[i] = 0;
 for(i=0; i<2048; i++)
    bm[i] = ls[i] = qd[i] = th[i] = wt[i] =  0;
 for( i=0; i<8192; i++)
    aw[i] = gp[i] = 0;

 /* for the error message */
 if( (*istat) == 1) dmesg = 1;
 else           dmesg = 0;
 fls = (float)forfftsize;
 *istat = 0;

 /* calculate the effective processing bandwidth */
 pbw = pbw/4; /* The bandwidth is for total 4looks */
 nw = (int)((pbw/prf)*(float)(forfftsize));
 xpbw = (float)(nw)*prf/((float)(forfftsize));
 *pbwx = xpbw*4; /* return the 4looks total bandwidth */

 if(dmesg)printf(" fd= %f %f %f \n",fdgrd[2],fdgrd[1],fdgrd[0]);
 if(dmesg)printf(" fr= %f %f %f \n", frgrd[2],frgrd[1],frgrd[0]);


 /* specify the Azweight array. Note that the equation for the
    nonzero in the weighting function for 1 look and 4 look are
    different. However, the equations are reduced to a function
    of prf, pbw and forward fftsize. (xpbw has the different
    values for 1look and 4look). 
  */
 weight = 16384;
 for(j=0; j<4; j++)
    for(i=0; i<nw; i++) aw[i+j*(forfftsize/4)] = weight;
 for(j=forfftsize; j<8192; j+=forfftsize)
    for(i=0; i<forfftsize; i++) aw[i+j] = aw[i];



 /* calculate the expansion factor and round off to 13 bit */
 e = 1.0;
 idelbin = 8;
 /* generate the azimuth board coefficient */
 kstart = 0;
 kend   = 1024;

 weightstart = 0;
 gamma1 = 0;
 theta = 0;
 wt[0] = weightstart;
 qd[0] = gamma1;
 th[0] = theta;
 *looklen = (short int)(nw & 0x07ff);
 *lookpos = (short int)(forfftsize/4 - nw/2);

 /* for other boards, no 12.5 meter interpolation */
 xifrac = 1.0;
 ifrac = (int)(xifrac*8192.0)& 0x3fff;
 *pstspace = ifrac;

 /* calculate the good points and nonoverlap as a function
    of the far range to be the constant value for the whole image.
    Note that the reference function length is increased with 
    the slant range. In addition, the vlpts and the nov variables
    are chosen as integers. 
  */
  xbin = (float)(kend*idelbin);
  fd   = (fdgrd[2]*xbin + fdgrd[1])*xbin + fdgrd[0];
  fdot = (frgrd[2]*xbin + frgrd[1])*xbin + frgrd[0];
  fdot = (float)(fabs((double)fdot));
  pregp4l(forfftsize, fd,fdot, prf, xpbw, 
          &ix0, &ix1, &ix2,  &ix3,  &ngoodpt_far, &ngdpt_far);


  xbin = 0;
  fdmin   = (fdgrd[2]*xbin + fdgrd[1])*xbin + fdgrd[0];
  fdotmin = (frgrd[2]*xbin + frgrd[1])*xbin + frgrd[0];
  fdotmin = (float)(fabs((double)fdotmin));
  pregp4l(forfftsize, fdmin,fdotmin, prf, xpbw,
          &ix0, &ix1, &ix2, &ix3, &ngoodpt_near, &ngdpt_near);


  if(dmesg)printf("the fdot=%f and fdotmin=%f\n",fdot,fdotmin);
  if(dmesg) printf(" the good points after compress (near)= %d\n",ngdpt_near);
  if(dmesg) printf(" the good points after compress (far)= %d\n",ngdpt_far);
  if(dmesg) printf(" ngoodpt_near %d ngoodpt_far=%d\n",ngoodpt_near,
                 ngoodpt_far);

  *vlpts = (ngoodpt_near < ngoodpt_far) ? ngoodpt_near: ngoodpt_far ;
  if(dmesg) printf(" the valid points per look(VLPTS)is %d\n", *vlpts);

  *nov = *vlpts;
  if(dmesg)printf(" the non overlap points (NOV) is %d\n",*nov);

  *bufrdlen = forfftsize/2;
  if(dmesg)printf(" the raw data line offset is %d\n",*bufrdlen);
 
  *azoff = (*vlpts)*4;
  if(dmesg)printf(" the frame offset (MLC) is %d\n", *azoff);

  prf4 = ((float)(prf))/4.0;
  fftsize4 = ((float)(forfftsize))/4.0;
  for(kline = kstart; kline < kend; kline++){
    i = kline;
    /* calculate fd and fr */
    x = (float)(i*idelbin + roffset);
    fd = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
    fr = (frgrd[2]*x + frgrd[1])*x + frgrd[0];

    /* calculate prho and lookstart  */
    prho = (short int)((prf*prf/fr)*2.0);
    itemp = (fd - (xpbw*4.0)/2.0)/prf;
    itemp = modf(itemp,&ltemp);
    if(itemp < 0.0) itemp += 1.0;
    lookstart = (int)(forfftsize *itemp);
    
    /* For preprocessing, there is no deskew correction */
    ioffset = 0;

    /* calculate the starting points location that will allow
       (vlpts - nov) overlap points between looks */
    pregp4l(forfftsize, fd, fr, prf, xpbw,
                &ix0,&ix1,&ix2,&ix3, &ngoodpt, &ngdpt_look);

   /*
    ix0 = (float)(fmod((double)(ix0), (double)(fftsize4)));
    ix1 = (float)(fmod((double)(ix1), (double)(fftsize4)));
    ix2 = (float)(fmod((double)(ix2), (double)(fftsize4)));
    ix3 = (float)(fmod((double)(ix3), (double)(fftsize4)));

    if(ix0 < 0.0) ix0 += fftsize4;
    if(ix1 < 0.0) ix1 += fftsize4;
    if(ix2 < 0.0) ix2 += fftsize4;
    if(ix3 < 0.0) ix3 += fftsize4;
   */

    ix[0] = (short int)((int)(ix0*16.0) & 0xffff);
    ix[1] = (short int)((int)(ix1*16.0) & 0xffff);
    ix[2] = (short int)((int)(ix2*16.0) & 0xffff);
    ix[3] = (short int)((int)(ix3*16.0) & 0xffff);
  
    /* assign the value to the array vector */
    bm[kline] = prho;
    ls[kline] = lookstart;
    io[kline] = ioffset;
    gp[kline*4] = ix[0];
    gp[kline*4 + 1] = ix[1];
    gp[kline*4 + 2] = ix[2];
    gp[kline*4 + 3] = ix[3];
  }

} 
 
