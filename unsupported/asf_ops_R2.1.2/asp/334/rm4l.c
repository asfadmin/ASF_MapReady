/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <math.h>
#include "flagerror.h"

/* done by quyen dinh nguyen
 * 9/19/89
 * 12/28/89 modified to check the number of valid output lines
 *          from the RM board.
 * 1/12/90  modified to zero out the far range when there is
 *          no valid line from the corner turn
 * 3/13/90  modified to return the slant range, the fd and fr of
 *          3 slant range positions: near, midle and far.
 * 1/24/91  modified the index of sr2gr vector(it should be start
 *          from 0 as the C language required).
 */


/* rm4l( forfftsize, valid_lines_per_frame, prf, lambda, slrspace, -----
          fdslr, frslr,
          grdspace, rsc, r_earth, r_close,
          crs, fin, vnkl, vtwod, va, roffset, fdgrd, frgrd, rmlines,
          r3points, fd3points, fr3points, istat)

   This routine will generate the parameter for Range Migration board
   in the 1look processor
   
 INPUT: 
   forfftsize   int*4   the forward fft size.

   valid_lines_per_frame int*4  the valid output lines from the CTC.

   prf          float   Pulse repitition frequency (Hz).

   lambda       float   wavelength (meter).
 
   slrspace     double  the  slant range spacing (meter).

   fdslr        vec of float*4   the three coefficient  of the Doppler
                                 centroid. The Doppler centroid of range
                                 bin n is calculated as:
                                  fdslr(n) = (fdslr[2]*n + fdslr[1])*n 
                                                + fdslr[0]
                                 when n is 0,1,2,...N.

   frslr        vec of float*4   the Doppler rate is calculated as:
                                  frslr(n) = (frslr[2]*n + frslr[1])*n
                                                + frslr[0]
                                  where n is the range bin and n is
                                  0,1,2,....N.

  grdspace      double   the ground range spacing (meter).
  
  rsc           double   the distance from earth center to SAR (meter).

  r_earth       double   the earth radius at scence center (meter).
 
  r_close       double   the slant range to the first range bin (meter).

  istat         int      For INPUT:
                           istat = 1 for verbal messages.
                           istat = 0 for no error messages.

 OUTPUT:
   crs          vec of int*2   coarse coefficient

   fin          vec of int*2   fine coefficient
 
   vnkl         vec of int*2   range offset

   vtwod        vec of int*2   twod coefficient

   va           vec of int*2   a coefficent

   roffset      pointer of int*4    first azimuth line as output
 
   fdgrd        vec of float*4    the new three coefficient of the
                                  Doppler centroid as a function of
                                  the ground range bin. The Doppler
                                  centroid is calculated as: 
                                    fdgrd(m) = (fdgrd[2]*m + fdgrd[1])*m
                                                   + fdgrd[0]
                                  where m is the ground range bin and 
                                  m is 0,1,2,...M.

   frgrd        vec of float*4    the new three coefficient of the
                                  Doppler rate as a function of
                                  the ground range bin. The Doppler
                                  rate is calculated as: 
                                    frgrd(m) = (frgrd[2]*m + frgrd[1])*m
                                                   + frgrd[0]
                                  where m is the ground range bin and 
                                  m is 0,1,2,...M.

   rmlines      points of int*4   the valid output lines from the RM board.

   r3points     vector of double  the slant range vector (meter)
                                  r3points[0] for the near point.
                                  r3points[1] for the midle point.
                                  r3points[2] for the far point.

   fd3points    vector of double  the Doppler centroid frequency vector
                                  (Hz) 
                                  fd3points[0] for the near point.
                                  fd3points[1] for the midle point.
                                  fd3points[2] for the far point.

   fr3points    vector of double  the Doppler frequency rate vector
                                  (Hz/sec) 
                                  fr3points[0] for the near point.
                                  fr3points[1] for the midle point.
                                  fr3points[2] for the far point.

   istat        pointer of int*4     0 if no errors.
                                     otherwise, check with flagerror.h

*/

rm4l( forfftsize, valid_lines_per_frame, prf, lambda, slrspace, 
          fdslr, frslr,
          grdspace, rsc, r_earth, r_close,
          crs, fin, vnkl, vtwod, va, roffset, fdgrd, frgrd, rmlines,
          r3points, fd3points, fr3points, istat)

int         forfftsize, *roffset, *rmlines, *istat;
int         valid_lines_per_frame;
short int   crs[1], fin[1], vnkl[1], vtwod[1], va[1];
float       prf, lambda, fdslr[1], frslr[1], fdgrd[1], frgrd[1];
float       fd3points[1],fr3points[1];
double      slrspace,grdspace,rsc,r_earth, r_close, r3points[1];

{
 int     i,j,noutput,imax,dmesg;
 int     totalsample,nsample,icoarse,ncoarse,k,idelbin;
 int     iflag_a,iflag_twod,iflag_crs,iflag_fin;
 int     icountout;
 int     ifar_zero,ifar_coarse;
 short int  coarse,fine,a,twod,nkl;
 float   fd,fr,fls,p,xmin,xmax;
 float   ycoarse,yfine,ya,ytwod,pathval(),minpath();
 float   x,za,xnewtwod,xa,xfine,xtwod,xnkl;
 float   sr2gr[8320];
 double  rfirst;
 double  xtemp,ip;

 /* clear out the crs, fin, vnkl, vtwod and va vectors */
 for(i=0; i<8192; i++) crs[i]=fin[i]=0;
 for(i=0; i<2048; i++) vnkl[i]=vtwod[i]=va[i]=0;
 icountout = 0;
 *rmlines  = 0;
 ifar_zero = 0;
 ifar_coarse = 0;

 if( (*istat) == 1) dmesg = 1;
 else               dmesg = 0;
 /* find the first range bin that will have enough lines in the
    Range migration memory */
 *istat = 0;
 fls = (float) forfftsize;
 p = (float)(slrspace);
 imax = 100;
 xmin = -1.0;
 for(i=0; i<imax && xmin < 0.0; i++){
    /* calculate fd, fr, xa,xtwod,xfine and coarse parameter */
    x = (float)(i);
    fd = (fdslr[2]*x + fdslr[1])*x + fdslr[0];
    fr = (frslr[2]*x + frslr[1])*x + frslr[0];

    xa = - prf*prf*lambda/(4.0*p*fr);
    xtwod = 2.0*(fd/prf);
    xfine = 0.0;
    coarse = (short int)(i & 0x00001fff);

    ya      = xa;
    ytwod   = xtwod;
    yfine   = xfine;
    ycoarse = (float)(coarse);

    xmin = minpath(ycoarse, yfine, ya, ytwod) - 2.0;
    if(xmin> 0.0){
       *roffset = i;
       if(dmesg)printf(" Enough points for interpolation at %d\n",i);
    }
 }
 if(i >= imax){
    if(dmesg)printf(" ERROR: NO PATH FITS WITH FIRST %d RANGE CELLS\n",
                  imax);
    *istat |= OVRFL_PATH;
    return;
 }

 noutput = 1024;
 idelbin = 8;
 rfirst = r_close + (double)(*roffset)*slrspace;
 ncoarse = noutput*idelbin;
 totalsample = 8192;
 nsample = ncoarse;
 sltogr(rsc, r_earth, grdspace, r_close, lambda, prf, slrspace, rfirst,
            fdslr, frslr, totalsample,  nsample, 
            fdgrd, frgrd);
 /* generate the slant range to ground range vector */
 vecsr2gr( rsc, r_earth, r_close, rfirst,  slrspace, grdspace,
            sr2gr, ncoarse);

 iflag_a = iflag_twod = iflag_crs = iflag_fin = 0;
 /* Now compute the parameters for each range bin */
 for(i=0; i<ncoarse; i++){
    j = i;
    /* calculate fd , fr parameter */
    x = (float) ((j/idelbin)*idelbin);
    fd = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
    fr = (frgrd[2]*x + frgrd[1])*x + frgrd[0];


    xa = -prf*prf*lambda/(4.0*p*fr);
    if(abs((int)(xa*1024.0)) >= 32768){
       if(dmesg && iflag_a == 0)
           printf(" Beware the a value is overflow for %d\n",j);
       iflag_a = 1;
       *istat |= OVRFL_A ;
       if(xa > 0.0) a = 0x7fff;
       else         a = 0x8000;
    } else {
       a = (short int)( (int)(xa*1024.0) & 0xffff);
    }

    xtwod = 2.0*(fd/prf);
    if( abs((int)(xa*1024.0)) >= 32768){
       /* need to modify xtwod such that the product xtwod*xa 
          will equal to the original xtwod*xa */
       za = (float)(a)/1024.0;
       xnewtwod = (xa*xtwod)/za;
       xtwod = xnewtwod;
       xa = za;
    }
    if(abs((int) (xtwod*1024.0)) >= 32768){
       if(dmesg && iflag_twod == 0)
          printf(" Beware the twod is overflow for i=%d \n",j);
       iflag_twod = 1;
       *istat |= OVRFL_2D ;
       if(xtwod > 0.0) twod = 0x7fff;
       else            twod = 0x8000;
    } else {
       twod = (short int)( (int)(xtwod*1024.0) & 0xffff);
    }

    xtemp = fd/prf - 1.0/2.0;
    xtemp = modf(xtemp, &ip);
    if(xtemp < 0.0) xtemp = xtemp + 1.0;
    nkl = (short int)(fls - xtemp*fls) & 0x1fff;
    
    xfine = modf((double)(sr2gr[j]),&ip);
    coarse = (short int)( (int)(sr2gr[j]) );

    ya    = xa;
    ytwod = xtwod;
    yfine = xfine;
    ycoarse = (float)(coarse);

    xmax = pathval(ycoarse, yfine,ya, ytwod);
    xmax = ((float)((int)(xmax*16.0)))/16.0;

    icoarse = (int)(ceil((double)(xmax)));
    coarse = (short int)(icoarse + 2);
    icountout += 1;
    if( ((int)(coarse)) < valid_lines_per_frame) *rmlines = icountout;
    if( ((int)(coarse)) > valid_lines_per_frame) {
       ifar_zero = 1;
       ifar_coarse = valid_lines_per_frame + 4;
    }
    /* update the parameter if we pass the valid lines per frames */
    if( ifar_zero == 1) {
       coarse = (short int)(ifar_coarse);
       fine   = (short int)(-2*16);
       nkl    = 0;
       twod   = 0;
       a      = 0;
    }else{
       if(coarse > 0x1fff ){
          if(dmesg && iflag_crs == 0)
               printf(" Beware that coarse value is overflow for i=%d\n",j);
          *istat |= OVRFL_CRS ;
          iflag_crs = 1;
          coarse = 0x1fff;
       }

       xfine = sr2gr[j] - (float)(coarse);
       if(abs((int)(xfine*16)) > 0x07ff ){
          if(dmesg && iflag_fin == 0 )
              printf(" Beware that fine value is overflow for i=%d\n",j);
          *istat |= OVRFL_FIN;
          iflag_fin = 1;
          if(xfine > 0.0) fine = 0x07ff;
          else            fine = 0x0800;
       } else {
          fine = (short int)( (int)(xfine*16) & 0x0fff);
       }
    }

    crs[i] = coarse;
    fin[i] = fine;
    if( i%idelbin == 0) {
       k = i/idelbin;
       vnkl[k]  = nkl;
       vtwod[k] = twod;
       va[k]    = a; 
    }
 }
 /* calculate the fd, fr and slant range of 3 slant range points */
 x = 0;
 r3points[0] = rfirst;
 fd3points[0] = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
 fr3points[0] = (frgrd[2]*x + frgrd[1])*x + frgrd[0];
 
 x =(float)( (*rmlines)/2 - 1) ;
 r3points[1] = r_close + slrspace*( (double)( sr2gr[(*rmlines)/2 - 1] ) );
 fd3points[1] = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
 fr3points[1] = (frgrd[2]*x + frgrd[1])*x + frgrd[0];
 
 x = (float) ((*rmlines) - 1);
 r3points[2] = r_close + slrspace*( (double)( sr2gr[(*rmlines) - 1] ) );
 fd3points[2] = (fdgrd[2]*x + fdgrd[1])*x + fdgrd[0];
 fr3points[2] = (frgrd[2]*x + frgrd[1])*x + frgrd[0];
 
 

}

