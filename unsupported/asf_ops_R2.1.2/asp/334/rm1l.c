/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <math.h>
#include "flagerror.h"
/* done by quyen dinh nguyen
 * 9/19/89
 * 12/28/89 modified to check the last valid line output.
 * 3/12/90  modified to return 3 range locations: near, midle and
 *          far 
 * 4/25/90  modified the coarse values within 2048
 * 1/24/91  modified to correct the index in calculating 3 range
 *          locations (rmlines is starting from 1 not zero).
 * 4/30/91  modified to truncate the maximum of coarse to 2046
 */


/* rm1l( forfftsize, istart_rg_bin, valid_lines_per_frame, prf, --------
          lambda, slrspace,  fdslr, frslr, r_close, 
          crs, fin, vnkl, vtwod, va, 
          roffset, rmlines, r3points,fd3points,fr3points,istat)

   This routine will generate the parameter for Range Migration board
   in the 1look processor
   
 INPUT: 
   forfftsize   int*4   the forward fft size.

   istart_rg_bin int*4  the starting range bin number. This
                        value is used to calculate the correct fd and
                        fr based on fdslr and frslr coefficients.

   valid_lines_per_frame  int*4   the valid output lines (CT).
				  This number should be 2048 for
				  complex mode (not the valid number
				  from range compression module)

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

   r_close      double    the slant range to the first range bin (meter).

   istat        int       For input:
                           istat = 1 for verbal messages.
                           istat = 0 for no error messages.

 OUTPUT:
   crs          vec of int*2   coarse coefficient

   fin          vec of int*2   fine coefficient
 
   vnkl         vec of int*2   range offset

   vtwod        vec of int*2   twod coefficient

   va           vec of int*2   a coefficent

   roffset      pointer of int*4    first azimuth line as output

   rmlines      pointer of int*4    number of output valid lines (RM)

   r3points     vector of double   the slant range vector. (meter) 
                                   r3points[0] for the near point.
                                   r3points[1] for the midle point.
                                   r3points[2] for the far point.

   fd3points    vector of float   the Doppler centroid frequency vector 
                                  (Hz)
                                  fd3points[0] for the near point.
                                  fd3points[1] for the midle point.
                                  fd3points[2] for the far point.

   fr3points    vector of float   the Doppler frequency rate vector 
                                  (Hz/sec)
                                  fr3points[0] for the near point.
                                  fr3points[1] for the midle point.
                                  fr3points[2] for the far point.

   istat        pointer of int*4     0 if no errors.
                                     otherwise, check with errorflag.h

*/

rm1l( forfftsize, istart_rg_bin, valid_lines_per_frame, prf, 
          lambda, slrspace,  fdslr, frslr, r_close, 
          crs, fin, vnkl, vtwod, va, 
          roffset, rmlines, r3points,fd3points,fr3points,  istat)

int         forfftsize, valid_lines_per_frame, *roffset, *rmlines, *istat;
int         istart_rg_bin;
short int   crs[1], fin[1], vnkl[1], vtwod[1], va[1];
float       prf, lambda, fdslr[1], frslr[1], fd3points[1],fr3points[1];
double      slrspace,r_close, r3points[1];

{
 int     i,j,noutput,imax,dmesg;
 int     icoarse;
 int     iflag_a,iflag_twod,iflag_crs,iflag_fin;
 int     icountout;
 int     ifar_zero,ifar_coarse;
 int     valid_lines_from_CT;
 short int  coarse,fine,a,twod,nkl;
 float   fd,fr,fls,p,xmin,xmax;
 float   ycoarse,yfine,ya,ytwod,pathval(),minpath();
 float   x,za,xnewtwod,xa,xfine,xtwod,xnkl;
 float   sr2gr[8320];
 double  xtemp,ip;

 /* clear out the crs, fin, vnkl, vtwod and va vectors */
 for(i=0; i<8192; i++) crs[i] = fin[i] = 0;
 for(i=0; i<2048; i++) vnkl[i] = vtwod[i] = va[i] = 0;
 icountout = 0;
 *rmlines = 0;
 valid_lines_from_CT = istart_rg_bin + valid_lines_per_frame;
 ifar_zero = 0;
 ifar_coarse = 0;

 /* find the first range bin that will have enough lines in the
    Range migration memory */
 if( (*istat) == 1) dmesg = 1;
 else               dmesg = 0;
 *istat = 0;
 fls = (float) forfftsize;
 p = (float)(slrspace);
 imax = 100;
 xmin = -1.0;
 for(i=0; i<imax && xmin < 0.0; i++){
    /* calculate fd, fr, xa,xtwod,xfine and coarse parameter */
    x = (float)(i + istart_rg_bin );
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
 if( i>=imax) {
    if(dmesg)printf(" ERROR: NO PATH FITS WITH FIRST %d RANGE CELLS\n",
                    imax);
    *istat |= OVRFL_PATH;;
    return;
 }

 /* Now compute the parameters for each range bin */
 noutput = 2048;
 for(i=0; i<8320; i++) sr2gr[i] = (float)(i + *roffset);
 iflag_a = iflag_twod = iflag_crs = iflag_fin = 0;
 for(i=0; i<noutput; i++){
    j = i+ (*roffset) + istart_rg_bin ;
    /* calculate fd , fr parameter */
    x = (float) (j );
    fd = (fdslr[2]*x + fdslr[1])*x + fdslr[0];
    fr = (frslr[2]*x + frslr[1])*x + frslr[0];


    xa = -prf*prf*lambda/(4.0*p*fr);
    if(abs((int)(xa*1024.0)) >= 32768){
       if(iflag_a == 0 && dmesg)
           printf(" Beware the a value is overflow for %d\n",j);
       *istat |= OVRFL_A;
       iflag_a = 1;
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
       if(iflag_twod == 0 && dmesg)
            printf(" Beware the twod is overflow for i=%d \n",j);
       *istat |= OVRFL_2D ;
       iflag_twod = 1;
       if(xtwod > 0.0) twod = 0x7fff;
       else            twod = 0x8000;
    } else {
       twod = (short int)( (int)(xtwod*1024.0) & 0xffff);
    }

    xtemp =(double)( fd/prf - 1.0/2.0);
    xtemp = modf(xtemp, &ip);
    if(xtemp < 0.0) xtemp = xtemp + 1.0;
    nkl = (short int)(fls - ((float)(xtemp))*fls) & 0x1fff;
    
    xfine = modf((double)(sr2gr[j]),&ip);
    coarse = (short int)( (int)(sr2gr[j]) ) ;

    ya    = xa;
    ytwod = xtwod;
    yfine = xfine;
    ycoarse = (float)(coarse);

    xmax = pathval(ycoarse, yfine,ya, ytwod);
    xmax = ((float)((int)(xmax*16.0)))/16.0;

    icoarse = (int)(ceil((double)(xmax)));
    coarse = (short int)(icoarse + 2);
    icountout += 1;
    if(((int)(coarse)) < valid_lines_from_CT) *rmlines = icountout;
    coarse = coarse - istart_rg_bin; /* update coarse to be within
					2048 */

    if(((int)(coarse)) > 2046) {
       ifar_zero = 1;
       coarse = ifar_coarse = 2046;
    }
    /* zero the parameters if we pass to the limit of 2046 */
    if(ifar_zero == 1){
       coarse = (short int) ifar_coarse;
       fine   = (short int) (-2*16);
       nkl    = 0;
       twod   = 0;
       a      = 0;
    }else {
       if(coarse > 0x1fff){
          if(dmesg && iflag_crs == 0 )
               printf(" Beware that coarse value is overflow for i=%d\n",j);
          *istat |= OVRFL_CRS;
          iflag_crs = 1;
          coarse = 0x1fff;
       }

       xfine = (sr2gr[j] - istart_rg_bin) - (float)(coarse);
       if(abs((int)(xfine*16)) > 0x07ff){
          if(dmesg && iflag_fin ==  0 )
              printf(" Beware that fine  value is overflow for i=%d\n",j);
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
    vnkl[i] = nkl;
    vtwod[i] = twod;
    va[i] =   a; 
 }

 /* calculate the fd, fr and slant range of 3 slant range points */
 x = (*roffset) + istart_rg_bin ;
 r3points[0] = r_close + slrspace * ( (double) x  );
 fd3points[0] = (fdslr[2]*x + fdslr[1])*x + fdslr[0] ;
 fr3points[0] = (frslr[2]*x + frslr[1])*x + frslr[0] ;

 x = *roffset + (*rmlines)/2 - 1 + istart_rg_bin ;
 r3points[1] = r_close + slrspace * ( (double) x );
 fd3points[1] = (fdslr[2]*x + fdslr[1])*x + fdslr[0] ;
 fr3points[1] = (frslr[2]*x + frslr[1])*x + frslr[0] ;

 x = *roffset + (*rmlines) - 1 + istart_rg_bin ; 
 r3points[2] = r_close + slrspace * ( (double) x );
 fd3points[2] = (fdslr[2]*x + fdslr[1])*x + fdslr[0] ;
 fr3points[2] = (frslr[2]*x + frslr[1])*x + frslr[0] ;

}

