static char sccsid_rate_chk_c[] =
    "@(#)rate_chk.c	1.2 96/04/09 19:13:31";

/*                     RATE CHECK FUNCTION                  */

/*  rate_chk(att1,att2,att3,tm,r_clse,avg,rte2,r_chg,istat) ------

This function is used by the calling program when processing
a scene centered at processing region "m".

Inputs:
variables:	att1,att2,att3 
type: 		float
description:	'attx' is a three element vector containing the
		spacecraft yaw, pitch and roll in degrees as
		determined by the preprocessing s/w for the 
		respective regions(m-1,m,m+1).
varibles:	tm
type:		float
description:	'tm' is the seconds portion of the GMT time of the
		respective preprocessing region.  It assumes that
		the provided information is in time increasing
		order such that if tm0 = 55.00 and tm1 = 10.00,
		the difference is 15.00 seconds.
variables:	r_clse
type:		float
description:	'r_clse' is the slant range in meters to the near
		edge of the swath for the designated preprocessing
		region.

Outouts:
variables:	avg
type:		float
description:	'avg' is a three element vector with the average of
		the yaw,pitch and roll rates between the three
		corresponding preprocessing regions (m-1,m,m+1).
variables:	rte2
type:		float
description:	'rte2' is a three element vector which gives the
		rate of the rate of change throughout the region
		namely the rate between regions m-1 and m and also
		the rate between region m and m+1.
variable:	r_chg
type:		integer
description:	'r_chg' is an integer value from 0 to 3 which is
		derived from the given slant range (r_clse) values.

variable:	istat
type		char
description:	'istat' gives the status of the input time conditions
		in order to avoid the situation where a divide by zero
		should occur. '-1' signifies an error condition where
		'0' denotes a valid time input value.

Algorithms:	
		rate12(i) = (attitude2(i)-attitude1(i))/(t2-t1)
		rate23(i) = (attitude3(i)-attitude2(i))/(t3-t2)
		avgrate(i)= 1/2(rate12(i)+rate23(i))

		raterate(i)=(rate23(i)-rate12(i))/(t3-t1)

		r_change = 0 if r_clse1 = r_clse2 = r_clse3
		(all same value, most frequent state)
		r_change = 1 if r_clse1 != r_clse2 = r_clse3
		(change between regions 'm-1' and 'm')
		r_change = 2 if r_clse1 = r_clse2 != r_clse3
		(change between regions 'm' and 'm+1')
		r_change = 3 if r_clse1 != r_clse2 != r_clse3
		(change between regions 'm-1' and 'm' AND also
		between regions 'm' and 'm+1'.  This is an
		error condition.)This includes the condition
		where r_clse1 = r_clse3 != r_clse2 also.


*/


#include <stdio.h>
rate_chk(att1,att2,att3,tm,r_clse,avg,rte2,r_chg,istat)
float *att1,*att2,*att3,*tm,*r_clse,*avg,*rte2;
int   *r_chg;
char  *istat;

{
float  yaw_12rate,pit_12rate,rol_12rate;
float  yaw_23rate,pit_23rate,rol_23rate;
float  t21,t32,t31;

/* check validity of time(sec) elements */
if (tm[0] == tm[1]){
   istat[0] = '-';
   istat[1] = '1';
   istat[2] = '\0';
   return;
   }
   else{
       istat[0] = '0';
       istat[1] = '\0';
       if(tm[0] > tm[1])
         t21 = (60.0-tm[0])+tm[1];
         else
         t21 = tm[1]-tm[0];

         yaw_12rate = (att2[0]-att1[0])/t21;
         pit_12rate = (att2[1]-att1[1])/t21;
         rol_12rate = (att2[2]-att1[2])/t21;
         }
if(tm[1] == tm[2]){
   istat[0] = '-';
   istat[1] = '1';
   istat[2] = '\0';
   return;
   }
   else{
      istat[0] = '0';
      istat[1] = '\0';
      if(tm[1] > tm[2])
        t32 = (60.0-tm[1])+tm[2];
      else
        t32 = tm[2]-tm[1];

        yaw_23rate = (att3[0]-att2[0])/t32;
        pit_23rate = (att3[1]-att2[1])/t32;
        rol_23rate = (att3[2]-att2[2])/t32;
	}

avg[0] = (yaw_12rate + yaw_23rate)/2.0;
avg[1] = (pit_12rate + pit_23rate)/2.0;
avg[2] = (rol_12rate + rol_23rate)/2.0;

t31 = t32 + t21;

rte2[0] = (yaw_23rate - yaw_12rate)/t31;
rte2[1] = (pit_23rate - pit_12rate)/t31;
rte2[2] = (rol_23rate - rol_12rate)/t31;

if (r_clse[0] == r_clse[1])
   {if (r_clse[0] == r_clse[2])
          r_chg[0] = 0;
     else r_chg[0] = 2;
   }
else
   {if (r_clse[1] == r_clse[2])
           r_chg[0] = 1;
      else r_chg[0] = 3;
   }

return;
}

