/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <math.h>
/* 
 * done by quyen dinh nguyen
 * 9/19/89
 * 3/13/90 note that for complex image, there is no deskew correction
 *         In addition, we can start to process far swath by 
 *         changing value of roffset parameter.
 */



/*  az1l( forfftsize, roffset, prf, pbw, fdslr, frslr, -----------------
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, err_offset, istat)

   This routine will generate the parameter for Azimuth Transfer
   Function Board (XFER), the Azimuth Interpolation Board (AZINT),
   and the Multilook Controller Board (MLC) for 1look processor 

 INPUT:
   forfftsize    int*4    the forward fft size.

   roffset       int*4    The first azimuth line out of Range Migration
                          board. (This value is from the rm1l or rm4l
                          routine and from the starting range bin 
                          offset (this value is for complex processing)).
                          This range bin offset is used to calculate
                          the fd and fr from fdslr and frslr
                          coefficients.
   
   prf           float    the Pulse repitition frequency (Hz).
  
   pbw           float    the processing bandwidth (Hz).

   fdslr         vector of float  the three coefficient of the Doppler
                                  centroid as a function of slant range
                                  bin. The Doppler centroid of slant range 
                                  bin n is calculated as: 
                                     fdslr(n) = (fdslr[2]*n + fdslr[1])*n 
                                                 + fdslr[0]
                                  where n is 0,1,2,...N.
  
   frslr         vector of float  the three coefficent of the Doppler
                                  rate as a function of slant range bin.
                                  The Doppler centroid of slant range bin 
                                  n is calculated as: 
                                     frslr(n) = (frslr[2]*n+frslr[1])*n 
                                                 + frslr[0]
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
                               (have to be even)(INPUT,CTC).

  pbwx      float              the effective processing bandwidth .

  err_offset float             the error accumulated after each frame.

  istat     int*4              0 if no error.
                               >0 if parameter is overflow.
                               <0 if error in selecting parameters.

*/
az1l( forfftsize, roffset, prf, pbw, fdslr, frslr,
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, err_offset, istat)

int       forfftsize, roffset;
int       *pstspace, *vlpts, *nov, *azoff, *bufrdlen, *istat;
short int aw[1], bm[1], ls[1], qd[1], th[1], wt[1], gp[1], io[1];
short int *lookpos,*looklen;
float     prf, pbw, *pbwx, fdslr[1], frslr[1],*err_offset;

{
 int        dmesg,i,j,k,idelbin,nw,kline,kstart,kend;
 int        ie,ifrac,cnt;
 unsigned   gamma2;
 short int  prho_o,prho,gamma1,theta;
 short int  weightstart,lookstart,ioffset,weight;
 float      xpbw,e,x, fd,fr,f2,fls,frac,temp,xifrac;
 double     fdummy;

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
 nw = (int)((pbw/prf)*forfftsize);
 xpbw = (float)(nw)*prf/((float)(forfftsize));
 *pbwx = xpbw;

 /* specify the Azweight array. Note that the equation for the
    nonzero in the weighting function for 1 look and 4 look are
    different. However, the equations are reduced to a function
    of prf, pbw and forward fftsize. (xpbw has the different
    values for 1look and 4look). 
  */
 for(i=0; i<forfftsize; i++) {
    weight = 16384;
    for(j=0; j<8192; j+= forfftsize)
        aw[i+j] = (i<nw) ? weight: 0;
 }

 /* calculate the expansion factor */
 e = 1.0;
 idelbin = 1;
 /* generate the azimuth board coefficient */
 kstart = 0;
 kend   = 2048;
 prho = 0;
 for(kline = kstart; kline < kend; kline++){
    i = kline;
    /* calculate fd and fr */
    x = (float)(i*idelbin + roffset);
    fd = (fdslr[2]*x + fdslr[1])*x + fdslr[0];
    fr = (frslr[2]*x + frslr[1])*x + frslr[0];

    /* calculate prho, gamma1, gamma2 */
    prho_o = prho;
    prho = (short int)((prf*prf/fr)*2.0);
    weightstart = (short int)(fls/prf*(xpbw/2.0 - fd));
    f2 = (fr>0.0) ? fd - xpbw/2.0 : fd + xpbw/2.0;
    temp = 65536.0*modf((double)(f2/prf), &fdummy);
    k = (int)(temp);
    gamma1 = (short int)( k & 0xffff);
    frac = modf((double)(-f2*f2/fr), &fdummy);
    if( frac < 0.0) frac += 2.0;
    k = (int)(frac*4096.0);
    gamma2 = (unsigned)( k& 0x0fff);
    if (prho_o != prho) cnt = 0;
    theta = 136 - (int)(3.78*cnt);
    cnt++;
  
    /* assign the value to the array vector */
    bm[kline] = prho;
    qd[kline] = gamma1;
    th[kline] = theta;
    wt[kline] = weightstart;
 }
 lookstart = 0;
 ls[0] = lookstart;
 *looklen = 0 & 0x07ff;
 *lookpos = 0;

 /* for other boards */
 xifrac = 1.0/e;
 ifrac = (int)(xifrac*8192.0)& 0x3fff;
 *pstspace = ifrac;

 *vlpts = forfftsize/2;

 *nov   = 0;
 
 *azoff = forfftsize/2;

 *bufrdlen = forfftsize/2;

 *err_offset = 0;

} 
 
