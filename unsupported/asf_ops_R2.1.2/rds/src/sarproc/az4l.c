static char sccsid_az4l_c[] =
    "@(#)az4l.c	1.2 96/04/09 19:13:22";

#include <stdio.h>
#include <math.h>

/* done by quyen dinh nguyen
 * 9/19/89
 * 12/28/89 modified for deskew algorithm
 * 1/3/90   modified to apply the azimuth weighting (include
 *          the inverse antenna pattern and cos^2 weighting)
 * 3/13/90  modified to skew the image if requested.
 * 5/29/90  modified to put the additional deskew according
 *          to T. Bicknell
 * 5/30/90  modified to change the sign of the additional
 *          deskew
 * 1/24/91  (geoloc)modified to return the total deskew value:
 *             zero if FD is positive.
 *             deskew amount if FD is negative 
 */


/* az4l(forfftsize,roffset,flag_azwgt,flag_deskew,prf,pbw,fzero2nul,h, -----
      azspacing, azsample, fdgrd, frgrd, skew_adj,
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, err_offset, totalskew, istat)

   This routine will generate the parameter for Azimuth Transfer
   Function Board (XFER), the Azimuth Interpolation Board (AZINT),
   and the Multilook Controller Board (MLC) for 4look processor 

 INPUT:
   forfftsize    int*4    the forward fft size.

   roffset       int*4    The first azimuth line out of Range Migration
                          board. (This value is from the rm1l or rm4l
                          routine ).
   
   flag_azwgt    int*4    The four LSB bits will used to zero out each 
                          weighting vector. If bit is zero, the weighting
                          vector of that look will be zero. 
                          Note that Bit 3 will be coresponding to look 1,
                          Bit 2 will be coresponding to look2, Bit 1 will
                          be coresponding to look 3 and Bit 0 will
                          be coresponding to look 4.

   flag_deskew   int*4    The flag is used to turn on the deskew
                          correction. If flag_deskew is zero, no deskew
                          correction will be made. Otherwise, the final
                          image will be deskewed.

   prf           float    the Pulse repitition frequency (Hz).
  
   pbw           float    the processing bandwidth (Hz).This value
			  is for 4looks total bandwidth

   fzero2nul     float    the null frequency for a sinc antenna pattern
                          This value is calculated by
                              fzero2nul = 2*vel/L
                          where vel is the azimuth velocity and L is
                          the physical azimuth antenna length.
  
   h             float    The pedestal level h of a standard cosine.
                          squared on a pedestal weighting function.
                          For ERS-1, the h value is 0.3.

   azspacing     double   the azimuth spacing between range lines (meter). 

   azsample      double   the azimuth sampling to 12.5 meter (meters).

   fdgrd         vector of float  the three coefficient of the Doppler
                                  centroid as a function of ground range
                                  bin. The Doppler centroid of ground range 
                                  bin n is calculated as: 
                                     fdgrd(n)=(fdgrd[2]*n + fdgrd[1])*n 
                                              + fdgrd[0]
                                  where n is 0,1,2,...N.
  
   frgrd         vector of float  the three coefficent of the Doppler
                                  rate as a function of ground range bin.
                                  The Doppler centroid of ground range bin 
                                  n is calculated as: 
                                     frgrd(n) = (frgrd[2]*n+frgrd[1])*n 
                                             + frgrd[0]
                                  where n is 0,1,2,...N.
   skew_adj      double    the azimuth skew adjust at far range (bin 1024)
                           The unit is meter. The adjustment for each 
                           bin is skew_adj/1024.0

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

  pbwx      float              the effective processing bandwidth .
			       The value will be a 4look total bandwidth

  err_offset  float            the error accumulated after each frame 

  totalskew  float            the maximum deskew amount for the whole
                               scene.

  istat     int*4              0 if no error.
                               >0 if parameter is overflow.
                               <0 if error in selecting parameters.

*/
az4l( forfftsize, roffset, flag_azwgt, flag_deskew, prf, pbw, fzero2nul, h,
      azspacing, azsample, fdgrd, frgrd, skew_adj,
      aw, bm, ls, qd, th, wt, lookpos, looklen, gp, 
      pstspace, io, vlpts, nov,
      azoff, bufrdlen, pbwx, err_offset, totalskew, istat)

int       forfftsize, roffset, flag_azwgt , flag_deskew;
int       *pstspace, *vlpts, *nov, *azoff, *bufrdlen, *istat;
short int aw[1], bm[1], ls[1], qd[1], th[1], wt[1], gp[1], io[1];
short int *lookpos,*looklen;
float     prf, pbw, *pbwx, fzero2nul, h, fdgrd[1], frgrd[1];
float     *err_offset,*totalskew;
double    azspacing, azsample,skew_adj;

{
 int        dmesg,i,j,k,idelbin,nw,kline,kstart,kend;
 int        ie,ifrac,imax50,imin;
 int        igoodpt,ivlpts,inov;
 int        iazsize,frameoffset,froff();
 int        ngoodpt,nplook4;
 int        ihalfref;
 int        npts_az;
 short int  prho,gamma1,gamma2,theta;
 short int  weightstart,lookstart,ioffset,weight;
 short int  ix[4];
 float      Az_max, Az_factor,Cos_squared[8192],Inv_ant[8192];
 float      Az_wgt[8192];
 float      ix0,ix1,ix2,ix3;
 float      xpbw,e,x, fd,fr,frac,temp,xifrac;
 float      xbin,afdot,fdot,prf4;
 float      fdmin,afdotmin,fdotmin,maxnov,minnov,skewnear,skewfar;
 float      skewref,delerror,xskew,xoffset,xnov,xstart;
 float      xvlpts;
 float      f_value;
 float      total_skew,total_offset_skew;
 double     fdummy,ltemp,itemp;
 double     skew_adj_time,skew_adj_b_time,total_sk_adj;
 double     skew_adj_bin;

 /* init everything to zero */
 for(i=0; i<1024; i++)
    io[i] = 0;
 for(i=0; i<2048; i++)
    bm[i] = ls[i] = qd[i] = th[i] = wt[i] =  0;
 for( i=0; i<8192; i++)
    aw[i] = gp[i] = 0;

 prf4 = ((float)(prf))/4.0;
 skew_adj_bin = skew_adj/1024;

 /* for the error message */
 if( (*istat) == 1) dmesg = 1;
 else           dmesg = 0;
 *istat = 0;

 /* calculate the effective processing bandwidth */
 pbw = pbw/4; /* pass data in as total 4looks bandwidth */
 nw = (int)((pbw/prf)*forfftsize);
 xpbw = (float)(nw)*prf/((float)(forfftsize));
 *pbwx = xpbw*4; /* need to return to total bandwidth */

 if(dmesg)printf(" fd= %f %f %f \n",fdgrd[2],fdgrd[1],fdgrd[0]);
 if(dmesg)printf(" fr= %f %f %f \n", frgrd[2],frgrd[1],frgrd[0]);


 /* specify the Azweight array. Note that the equation for the
    nonzero in the weighting function for 1 look and 4 look are
    different. However, the equations are reduced to a function
    of prf, pbw and forward fftsize. (xpbw has the different
    values for 1look and 4look). 
 weight = 16384;
 for(j=0; j<4; j++)
    for(i=0; i<nw; i++) aw[i+j*(forfftsize/4)] = weight;
 for(j=forfftsize; j<8192; j+= forfftsize)
    for(i=0; i<forfftsize; i++) aw[i+j] = aw[i];
  */
 npts_az = nw*4;
 ant_wgt(forfftsize, npts_az, prf, fzero2nul, Inv_ant);
 Cos_wgt(nw, h, Cos_squared);
 /*
 printf(" the antenna weighting= %f %f %f %f\n",
              Inv_ant[0],Inv_ant[1],Inv_ant[2],Inv_ant[3]);
 printf(" the size of Inv_ant=%d\n", sizeof(Inv_ant[0]));
 printf(" the cos weighting =%f %f %f %f\n",
              Cos_squared[0],Cos_squared[1],Cos_squared[2],
                    Cos_squared[3]);
 */
 for (i=0;i<nw; i++)  if(Cos_squared[i] < 0.0)
     if(dmesg)printf(" Cos wgt is negative at %d for %f\n",i,Cos_squared[i]);
 for(i=0; i<npts_az; i++) if(Inv_ant[i] < 0.0)
    if(dmesg)printf(" Inv _ant is negative at %d for %f\n",i, Inv_ant[i]);

 for(j=0; j<4 ; j++){
    Az_max = 0.0;
    for(i=0; i<nw; i++) {
       Az_wgt[i] = Inv_ant[i+j*nw]*Cos_squared[i];
       if( Az_wgt[i] > Az_max) Az_max = Az_wgt[i];
    }
    Az_factor = 0.0;
    if((flag_azwgt >> (3-j) ) & 0x1) Az_factor = 16384.0/Az_max;
    if(dmesg)printf(" for j= %d , the Az_max is %f and AZ_factor=%f\n",
            j, Az_max, Az_factor);
    for(i=0; i<nw; i++) 
       aw[i+j*(forfftsize/4)] = (short int)(Az_wgt[i]*Az_factor);
 }
 for(j=forfftsize; j<8192; j+=forfftsize)
    for(i=0; i<forfftsize; i++) aw[i+j] = aw[i];
    

 /* calculate the expansion factor and round off to 13 bit */
 e = (float)(azspacing/azsample);
 ie = (int)(8192.0/e);
 e = 1.0/((float)(ie)/8192.0);

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

 /* for other boards */
 xifrac = 1.0/e;
 ifrac = (int)(xifrac*8192.0)& 0x3fff;
 *pstspace = ifrac;

 /* calculate the good points and nonoverlap as a function
    of the far range to be the constant value for the whole image.
    Note that the reference function length is increased with 
    the slant range. In addition, the vlpts and the nov variables
    are chosen as integers. 
  */
  xbin = 0.0;
  fdmin   = (fdgrd[2]*xbin + fdgrd[1])*xbin + fdgrd[0];
  fdotmin = (frgrd[2]*xbin + frgrd[1])*xbin + frgrd[0];
  afdotmin = (float)(fabs((double)fdotmin));

  xbin = (float)(kend*idelbin);
  fd   = (fdgrd[2]*xbin + fdgrd[1])*xbin + fdgrd[0];
  fdot = (frgrd[2]*xbin + frgrd[1])*xbin + frgrd[0];
  afdot = (float)(fabs((double)fdot));
 
/* calculate the pixels offset in the far range */
  skew_adj_time = (skew_adj_bin*((double)kend)/azspacing)
                         /((double)prf4);
  skew_adj_b_time = (skew_adj_bin/azspacing)/((double)prf4);


  ihalfref = (int)( xpbw/afdot*prf/4.0/2.0);
  igoodpt = ((float)forfftsize - xpbw/afdot*prf)/4.0;
  maxnov = xpbw/afdot*prf/4.0;
  minnov = xpbw/afdotmin*prf/4.0;

  xnov = (float)(ceil((double)(maxnov * e)));
  *nov   = (int)(xnov);
  if(dmesg) printf(" the nonoverlap points (NOV) is %d\n", *nov);

  imax50 = forfftsize/8;
  imin   = (float)(ceil((double)(maxnov)));
  delerror = 0.04;
  /* 12/15/89 let bypass froff subroutine
  frameoffset = forfftsize/8;
   2/26/90 use froff routine 
  */
  frameoffset = froff( imax50,imin,delerror,e);
  if(dmesg) printf(" frame offset is %d\n",frameoffset);
  xvlpts = (float)(frameoffset)*e;
  *vlpts = (int)(xvlpts);
  if(dmesg)printf(" the valid points per look (VLPTS) is %d\n",*vlpts);
  *err_offset = xvlpts - (float)(*vlpts);
  if(dmesg) printf(" the error in frame offset = %f\n", *err_offset);

  *bufrdlen = frameoffset*4;
  if(dmesg) printf(" the raw data line offset is %d\n",*bufrdlen);
  iazsize = 2*( *bufrdlen );

  *azoff  = *vlpts;
  if(dmesg) printf(" the frame offset (MLC) is %d\n", *azoff);
  

  skewnear = fdmin/fdotmin;
  skewfar  = fd/fdot;
 /* for far range, with skew_adj input variable, we need to 
    adjust the time to be deskewed */
  skewfar = skewfar - skew_adj_time;

  total_skew = (float)(fabs((double)( (skewnear - skewfar )*prf4*e)));
  total_skew = (float)(ceil((double)(total_skew)));
  total_offset_skew = 0;
  /* 9/24/91: modified to make sure there is no line in the image due
	      to the deskew
  if( (skewnear > 0) && (skewfar > 0) ) total_offset_skew = total_skew;
  */
  if( (skewfar - skewnear)  > 0 ) total_offset_skew = total_skew;
  *totalskew = total_offset_skew;
  skewref = skewnear;

  printf(" skewnear = %f , skewfar=%f, totalskew=%f\n",
      skewnear,skewfar,*totalskew);
  printf(" total skew_ajust =%f meters,( %f bin)\n",
       skew_adj,skew_adj/azspacing);

  /* check if flag_deskew is zero, no deskew correction will be made */
  if(flag_deskew == 0){
       *totalskew = 0;
       total_offset_skew = 0;
       skewref = 0;
  }

 

  if(dmesg)printf("the near deskew %f and far deskew=%f\n",
                   skewnear,skewfar);
  if(dmesg)printf("the prf4 and e are= %f , %f\n",prf4,e);
  if(dmesg) printf(" the reference deskew = %f\n", skewref);
  if(dmesg) printf(" the total_skew = %f\n", *totalskew);
  if(dmesg) printf(" the total_offset_skew = %f\n",total_offset_skew);
  

  total_sk_adj = 0.0;
  for(kline = kstart; kline < kend; kline++){
    i = kline;
    /* calculate fd and fr */
    x = (float)(i*idelbin + roffset);
    fd = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
    fr = (frgrd[2]*x + frgrd[1])*x + frgrd[0];
    fdot = (float)(fabs((double)(fr)));

    /* calculate prho and lookstart  */
    prho = (short int)((prf*prf/fr)*2.0);
    itemp = (fd - (xpbw*4.0)/2.0)/prf;
    itemp = modf(itemp,&ltemp);
    if(itemp < 0.0) itemp += 1.0;
    lookstart = (int)(forfftsize *itemp);
    
    /*  For deskew the image, calculate deskew amount */ 

    xskew = (fd/fr - skewref)*prf4*e;
    /* for each bin, with the skew_adj input variable, the
       xskew variable should be account the addition deskew time */
    xskew = (fd/fr - total_sk_adj - skewref)*prf4*e;
    total_sk_adj += skew_adj_b_time; /* update the total skew time*/

    if(flag_deskew == 0) xskew = 0.0;
    ioffset = forfftsize - (int)(total_offset_skew) + (int)(xskew);
    xoffset = xskew - (float)((int)(xskew));


    /* calculate the good point location */
    xstart = (xpbw/fdot*prf/4.0/2.0)*e;

    /* calculate the starting points location that will allow
       (vlpts - nov) overlap points between looks */
    xnov = (float)(*nov) - (xpbw/fdot*prf)/4.0*e;

    /* check the number of valid points per look to make sure
       that in look4 we will not use the bad compressed data */
    ngoodpt = (int)( ((float)(iazsize)  - xpbw/fdot*prf)/4.0 );
    nplook4 = (int)( ( xoffset + 3.0*xnov + (*vlpts) )/e);
    if(ngoodpt < nplook4 && dmesg)
        printf(" Beware, there is not enough good points for look4\n");

    /* Fudge factor 128 for TOM, Ming 5/20/92 */
    xoffset -= 310.;

    /* calculate the starting location for each look.
       Note that so far, all the terms are in azsample spacing units.
       We need to convert to the original azimuth spacing. */

    ix0 = (xoffset - xstart)/e;
    ix1 = (xoffset - xstart + xnov)/e;
    ix2 = (xoffset - xstart + 2.0*xnov)/e;
    ix3 = (xoffset - xstart + 3.0*xnov)/e;

    ix0 = (float)(fmod((double)ix0, (double)(forfftsize/4)));
    ix1 = (float)(fmod((double)ix1, (double)(forfftsize/4)));
    ix2 = (float)(fmod((double)ix2, (double)(forfftsize/4)));
    ix3 = (float)(fmod((double)ix3, (double)(forfftsize/4)));

    if(ix0 < 0.0) ix0 += (float)(forfftsize/4);
    if(ix1 < 0.0) ix1 += (float)(forfftsize/4);
    if(ix2 < 0.0) ix2 += (float)(forfftsize/4);
    if(ix3 < 0.0) ix3 += (float)(forfftsize/4);

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
 
