static char sccsid_noise_c[] =
    "@(#)noise.c	1.2 96/04/09 19:13:29";

/*  float noise(noise_vec,num_bytes,istat)  ----------------

***************************************************************
*                    					      *
*		float noise(noise_vec,num_bytes,istat)        *
*					                      *
***************************************************************

    This routine measures the noise by accepting  the  noise vector
    as inputs together with the number of bytes of I/Q pairs in one
    range line.


INPUTS:
variable:	noise_vec
type:		integer
description:	'noise_vec' is made up of a maximum of  8192 I/Q pairs
		of  byte elements which therefore constitute a maximum
		number of 2*8192=16384 bytes. Each 8 bit byte can have 
		an integer value from 0 to 255.

variable:       num_bytes	
type:		integer
description:	'num_bytes' is the total  number of bytes including 
		both I and Q. 
		Maximum = 8192*2 bytes.

OUTPUTS:
variable:	power
type:		float
description	'power' is the power calculated by the algorithm shown
		below.

variable:	istat
type:		char
description	'stat' returns a '0' if the 'npts = num_bytes/2' input
		is within the range from 1->8192, otherwise it returns
		a '-1' to signify an error condition.

ALGORITHM:	sum =SUMMATION[square of (ii[i])+square of (qi[i])]
		from i=0 to i=(npts-1)
		power = sum/npts
*/

#include<stdio.h>

#define MAXSIZE 8192  

float noise(noise_vec,num_bytes,istat)
unsigned int 	*noise_vec;
int		num_bytes;
char		*istat;
{
int	i,j,npts;
float	ii[MAXSIZE],qi[MAXSIZE],sum,power;		   
	
   npts = num_bytes/2;
   if((npts > MAXSIZE) || (npts <= 0)){
      istat[0] = '-';
      istat[1] = '1';
      istat[2] = '\0';
      return;
      }
      else{
      istat[0] = '0';
      istat[1] = '\0';
      }
  j = 0;
for(i = 0; i < npts; i++){
  ii[i] = noise_vec[j];
  qi[i] = noise_vec[j+1];
  j = j + 2;
  }
  sum = 0;
  for (i = 0; i < npts; i++)
      sum =((ii[i]*ii[i]) +(qi[i]*qi[i]));
  power = sum/npts;
return(power);
}

