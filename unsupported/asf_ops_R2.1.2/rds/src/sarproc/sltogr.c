static char sccsid_sltogr_c[] =
    "@(#)sltogr.c	1.2 96/04/09 19:13:33";

/*********************************************************************/
/*								     */
/*		sl2gr_intg.c					     */
/*  8/30/89: modified by MrQ: to include the first slant pixel       */
/*								     */
/*********************************************************************/


/* sltogr(rsc,r_earth,grinc,r_close,lambda,prf,srinc,rfirst, -----------
     fd_sl_coff,
     fdot_sl_coff,totalsample,num_bins,fd_gnd_coff,fdot_gnd_coff)

This routine returns the three element ground range Doppler center
frequency coefficients as well as three element ground range Doppler
rate coefficients.

INPUTS:
variable:	rsc
type:		double
description:	Distance from earth center to SAR in meters.

variable:	r_earth
type:		double
description:  	Radius of earth at scene center in meters.

variable:	grinc
type:		double
description:	distance between output ground range bins in meters.
		For ASF = 12.5 meters.


variable:	r_close
type:		double
description:    near range of data, determined by the delay between
		transmission and the start of digitization of radar
		pulses. (Units in meters)

variable:	lambda
type:		float	
description:	radar wavelength in meters.

variable:	prf
type:		float	
description:	Pulse repitition frequency (Hz)

variable: 	srinc
type:		double	
description:	Distance between the slant range bins in meters. 

variable:       rfirst
type:           double
description:    the distance from spacecraft to the first slant range
                pixel for the range cell migration. 

variable:	fd_sl_coff
type:		float	
description:	3 element vector containing the Doppler center
		frequency of slant range line in Hz.  The three elements
		are the constant, linear and quadratic coefficients in
		slant range units.

variable:	fdot_sl_coff
type:		float	
description:	3 element vector containing the Doppler rate terms of
		slant range line in Hz/sec.  The three elements are the
		constant, linear and quadratic coefficients in slant
		range units.

variable:       totalsample	
type:		long integer
description:    The number of ground range bins that the Doppler terms
                fd and fdot used to calculate the coefficient. The
                acceptable value is 8192.	

variable:	num_bins
type:		long integer
description:	the number of output ground range points which is the
		number of elements in the slant range to ground range
		interpolation vector to be used in the range migration
		correction. (maximum = 8192)

OUTPUTS:
variable:	fd_gnd_coff
type:		float	
description:	3 element vector containing the Doppler center frequency
		coefficient of  ground range line in Hz.  The three
		elements are the constant, linear and quadratic
		coefficients in ground range units.

variable:	fdot_gnd_coff
type:		float	
description:	3 element vector containing the Doppler rate terms of
		the ground range line in Hz/sec.  The three elements
		are the constant, linear and quadratic coefficients
		in ground range units.
*/

#include <stdio.h>


sltogr(rsc,r_earth,grinc,r_close,lambda,prf,srinc,rfirst,
           fd_sl_coff,
	   fdot_sl_coff,totalsample,num_bins,fd_gnd_coff,fdot_gnd_coff)

double  rsc,r_earth,grinc,r_close,srinc,rfirst;
float 	lambda,prf;
float	*fd_sl_coff,*fdot_sl_coff,*fd_gnd_coff,*fdot_gnd_coff;
long int	totalsample,num_bins;

{
long int	i,total_smpl,num_points;
double	r_slant0;
float	sr2gr_vec[8192],fd_gnd_vec[8192],fdot_gnd_vec[8192];
float   x_vec[8192],xfactor;
float   frsc,fr_earth,fr_slant0,fsrinc,fgrinc;


num_points = num_bins;
total_smpl = totalsample;


r_slant0 = rfirst;
vecsr2gr(rsc, r_earth, r_close, r_slant0, srinc, grinc, 
               sr2gr_vec,total_smpl);


/*find fd_gnd_vec[i],fdot_gnd_vec for each corresponding sr2gr_vec[i] */

for(i = 0; i < total_smpl; i++){
   fd_gnd_vec[i]   = fd_sl_coff[0] +(fd_sl_coff[1]*sr2gr_vec[i])+
		     (fd_sl_coff[2]*sr2gr_vec[i]*sr2gr_vec[i]);
   fdot_gnd_vec[i] = fdot_sl_coff[0]+(fdot_sl_coff[1]*sr2gr_vec[i])+
		     (fdot_sl_coff[2]*sr2gr_vec[i]*sr2gr_vec[i]);
   x_vec[i] = (float)(i);
   }

/* use least squares fit to a polynomial algorithm */

lsq(fd_gnd_vec,x_vec,total_smpl,num_points,fd_gnd_coff);
lsq(fdot_gnd_vec,x_vec,total_smpl,num_points,fdot_gnd_coff);

}
