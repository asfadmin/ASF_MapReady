static char sccsid_sl2gr_intg_c[] =
    "@(#)sl2gr_intg.c	1.2 96/04/09 19:13:33";

/*********************************************************************/
/*								     */
/*		sl2gr_intg.c					     */
/*								     */
/*********************************************************************/

/* sl2gr_intg(rsc,r_earth,grinc,r_close,lambda,prf,csr,fd_sl_coff, -----
      fdot_sl_coff,istat,num_bins,fd_gnd_coff,fdot_gnd_coff)

This routine returns the three element ground range Doppler center
frequency coefficients as well as three element ground range Doppler
rate coefficients.

INPUTS:
variable:	rsc
type:		float	
description:	Distance from earth center to SAR in meters.

variable:	r_earth
type:		float	
description:  	Radius of earth at scene center in meters.

variable:	grinc
type:		float	
description:	distance between output ground range bins in meters.
		For ASF = 12.5 meters.


variable:	r_close
type:		float
description:    near range of data, determined by the delay between
		transmission and the start of digitization of radar
		pulses. (Units in meters)

variable:	lambda
type:		float	
description:	radar wavelength in meters.

variable:	prf
type:		float	
description:	Pulse repitition frequency (Hz)

variable:	csr
type:		float	
description:	Complex sampling rate of SAR (Mhz) used to determine
		slant range pixel spacing.

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

variable:	istat
type:		long integer
description:	As input, indicates number of range bins the Doppler
		terms fd and fdot are to be used for. Acceptable value
		is istat = 1 for one look processing, and istat = 8
		for four look processing.

OUTPUTS:
variable:	num_bins
type:		long integer
description:	the number of output ground range points which is the
		number of elements in the slant range to ground range
		interpolation vector to be used in the range migration
		correction. (maximum = 8192)

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

#define c 2.99792458e8
#define MAX_BIN 8192


sl2gr_intg(rsc,r_earth,grinc,r_close,lambda,prf,csr,fd_sl_coff,
	   fdot_sl_coff,istat,num_bins,fd_gnd_coff,fdot_gnd_coff)


float 	rsc,r_earth,grinc,r_close,lambda,prf,csr;
float	*fd_sl_coff,*fdot_sl_coff,*fd_gnd_coff,*fdot_gnd_coff;
long int	istat,num_bins;

{
long int	i,frst_bin,total_smpl,num_points;
float	srinc,r_slant0;
float	sr2gr_vec[8192],fd_gnd_vec[8192],fdot_gnd_vec[8192];


num_points = num_bins;
total_smpl = MAX_BIN;


fnd1st_(&r_close,&lambda,&prf,&csr,fd_sl_coff,fdot_sl_coff,&frst_bin,&istat);


if(istat < 0){
   printf("Unable to determine valid first range bin\n");
   return;
   }

srinc    = c/(2.0*csr*1e6);

r_slant0 = r_close + (frst_bin*srinc);

mksl2gr_(&rsc,&r_slant0,&r_earth,&srinc,&grinc,sr2gr_vec);

/*find fd_gnd_vec[i],fdot_gnd_vec for each corresponding sr2gr_vec[i] */

for(i = 0; i < MAX_BIN; i++){
   fd_gnd_vec[i]   = fd_sl_coff[0] +(fd_sl_coff[1]*sr2gr_vec[i])+
		     (fd_sl_coff[2]*sr2gr_vec[i]*sr2gr_vec[i]);
   fdot_gnd_vec[i] = fdot_sl_coff[0]+(fdot_sl_coff[1]*sr2gr_vec[i])+
		     (fdot_sl_coff[2]*sr2gr_vec[i]*sr2gr_vec[i]);
   
  printf("%f\n",fd_gnd_vec[i]); 
   }

/* use least squares fit to a polynomial algorithm */

least_sq(fd_gnd_vec,total_smpl,num_points,fd_gnd_coff);
least_sq(fdot_gnd_vec,total_smpl,num_points,fdot_gnd_coff);

}

