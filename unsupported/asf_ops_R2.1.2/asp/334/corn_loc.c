/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* corn_loc.c -- calculate image corner locations */

#include"math.h"
#include<stdio.h>

struct gmt {
   int	  year;
   int	  day;
   int	  hour;
   int	  min;
   float  sec;
};

/* corn_loc(sv_ref,t_ref,t_start,delta,nl,r3points, --------------------
        fd3points, fr3points,re,rp,
	lambda,zero_dop,fd_adj,a,b,c,d,e,istat)

*********************************************************************
*					                            *
*		 corn_loc.c				            *
*						                    *
* 3/13/90: modified to enter the slant range, the fd and fr of 3    *
*          different slant range points. (QDN)                      *
* 3/14/90: modified to pass the slant range , the fd and the fr     *
*          as vectors for 3 points. (QDN)                           *
* 5/18/90: modified to consolidate GMT routines with other ASP s/w. *
* 5/29/90: modified to adjust for skew in image. (APS)              *
*								    *
*********************************************************************

This routine calculates the four corners of the image
by returning the geocentric latitude, the geocentric longitude
as well as the radius of ellipsoidal earth at target.




			  NEAR RANGE
		+---------------------------+       ^
		|A                         D|       |
		|			    |       |
		|<---------nl lines-------->|       |
		|			    |       |
		|                           |    np pixels
		|                           |       |
         EARLY	|             C             | LATE  |
		|                           |       |
		|                           |       |
		|         deskewed          |       |
		|                           |       |
		|                           |       |
		|B                         E|       |
		+---------------------------+       X

 			FAR RANGE



INPUTS:
variable:	sv_ref
type:		double
description:	input state vectors consisting of six elements namely,
		the position vectors x,y,z in meters together with the
		corresponding velocity vectors vx,vy vz in meters/sec
		respectively.

variable:	t_ref
type:		struct 'gmt' where  year,day,hour,min are type int
		and  sec is of type  float.
description:	GMT time with state vector in year,day,hour,min,sec

variable:	t_start
type:		struct 'gmt' where year,day,hour,min are type int
		and  sec is of type float
description:	GMT of first range line

variable:	delta
type:		double 
description:	time between range lines in seconds

variable:	nl
type:		integer
description:	total number of range lines.

variable:       r3points
type:           vector of double with 3 elements.
description:    3 element vector containing the slant range to
                the near, the midle and the far points (meter).

variable:       fd3points
type:           vector of float with 3 elements.
description:    3 element vector containing the Doppler centroid
                frequencies of the near, the midle and the far
                points (Hz). 

variable:       fr3points
type:           vector of float with 3 elements.
description:    3 element vector containing the Doppler frequency 
                rates of the near, the midle and the far
                points (Hz). 

variable:	re
type:		double	
description:	earth equatorial radius in meters

variable:	rp
type:		double	
description:	earth polar radius in meters


variable:	lambda
type:		double
description:	wavelength in meters

variable:	zero_dop
type:		integer
description:	0 if image is deskewed
		1 if image is not deskewed

variable:	fd_adj
type:		double
description:	factor to calculate adjustment to locations due to 
		skew relative to zero doppler line throughout image.
		The skew is caused by change in spacecraft altitude.

OUTPUTS:
variable:	a
type:		double
description:	three elements consisting of latitude,longitude and
		radius of ellipsoidal earth at target 'A'

variable:	b
type:		double
description:	three elements consisting of latitude, longitude and
		radius of ellipsoidal earth at target 'B'

variable:	c
type:		double
description:	three elements consisting of latitude, longitude and
		radius of ellipsoidal earth at target 'C'

variable:	d
type:		double
description:	three elements consisting of latitude, longitude and
		radius of ellipsoidal earth at target 'D'

variable:	e
type:		double
description:	three elements consisting of latitude, longitude and
		radius of ellipsoidal earth at target 'E'

pointer variable:  *istat
type:		int
description:	-1 indicates an error
		0  indicates successful operation
*/


corn_loc(sv_ref,t_ref,t_start,delta,nl,r3points,
        fd3points, fr3points,re,rp,
	lambda,zero_dop,fd_adj,a,b,c,d,e,istat)

struct  gmt t_ref,t_start;

double	sv_ref[6],re,rp,lambda,r3points[3],fd_adj,
	a[3],b[3],c[3],d[3],e[3];

double	delta;
float   fd3points[3],fr3points[3];

int	nl,zero_dop,*istat;

{
struct gmt t_ab,t_cc,t_de,t_a,t_b,t_c,t_d,t_e,t_out;

double  a_propvec[6],b_propvec[6],c_propvec[6],d_propvec[6],
	e_propvec[6],fd_out,fdot_out,fd_a,fd_b,fd_c,fd_d,fd_e,
	fdot_a,fdot_b,fdot_c,dfot_d,fdot_e,	
	ta_diff,tb_diff,tc_diff,td_diff,te_diff,t_step,tt_sec;

double  r_near,r_mid,r_far;

double  get_gmt_diff();

float   fd_near,fd_mid,fd_far,fr_near,fr_mid,fr_far;

float	t_sec,sec_diff,secs;

int	i,years,days,hrs,mins;


r_near = r3points[0]; r_mid = r3points[1]; r_far = r3points[2];
fd_near = fd3points[0]; fd_mid = fd3points[1]; fd_far = fd3points[2];
fr_near = fr3points[0]; fr_mid = fr3points[1]; fr_far = fr3points[2];

/*
printf("x=%f\ty=%f\tz=%f\nvx=%f\tvy=%f\tvz=%f\n",sv_ref[0],sv_ref[1],
	sv_ref[2],sv_ref[3],sv_ref[4],sv_ref[5]);
printf("t_ref=%d\t%d\t%d\t%d\t%4.2f\n",t_ref.year,t_ref.day,t_ref.hour,
	t_ref.min,t_ref.sec);
printf("t_start=%d\t%d\t%d\t%d\t%4.2f\n",t_start.year,t_start.day,
	t_start.hour,t_start.min,t_start.sec);
printf("delta(millisec)=%f\nnl=%d\nr_near=%f\nr_mid=%f\nr_far=%f\n",
	delta,nl,r_near,r_mid,r_far);
printf("fd=%f\t%f\t%f\nfdot=%f\t%f\t%f\n",fd3points[0],fd3points[1],
	fd3points[2],fr3points[0],fr3points[1],fr3points[2]);
printf("re=%f\nrp=%f\n",re,rp);
printf("lambda=%f\nzero_dop=%d\n",lambda,zero_dop);
*/


/* Calculate time associated with each pixel locations */
t_ab = t_start;

t_sec = (float)(delta*nl)/2.0;
t_cc = t_start;
add_seconds(&t_cc,t_sec);

/*
printf("t_cc=%d\t%d\t%d\t%d\t%f\n",t_cc.year,t_cc.day,t_cc.hour,
	t_cc.min,t_cc.sec);
*/

t_sec = delta*nl;
t_de = t_start;
add_seconds(&t_de,t_sec);

/*
printf("t_de=%d\t%d\t%d\t%d\t%f\n",t_de.year,t_de.day,t_de.hour,
	t_de.min,t_de.sec);
*/

/* Calculate time pixel at beam center */
fd_a = fd_d = fd_near;
fd_c = fd_mid;
fd_b = fd_e = fd_far;

/*
printf("fd_a=%lf\nfd_b=%lf\nfd_c=%lf\nfd_d=%lf\nfd_e=%lf\n",fd_a,fd_b,
	fd_c,fd_d,fd_e);
*/
t_a = t_ab;
t_b = t_ab;
t_c = t_cc;
t_d = t_de;
t_e = t_de;
	
fdot_a = fr_near;
fdot_c = fr_mid;
fdot_b = fr_far;

if (zero_dop == 0) {
    t_sec = -fd_a/fabs(fdot_a);
    add_seconds(&t_a,t_sec);
    add_seconds(&t_d,t_sec);

    t_sec = -fd_c/fabs(fdot_c);
    add_seconds(&t_c,t_sec);

    t_sec = -fd_b/fabs(fdot_b);
    add_seconds(&t_b,t_sec);
    add_seconds(&t_e,t_sec);
}

/*
printf("t_a=%d\t%d\t%d\t%d\t%f\n",t_a.year,t_a.day,t_a.hour,t_a.min,
	t_a.sec);
printf("t_b=%d\t%d\t%d\t%d\t%f\n",t_b.year,t_b.day,t_b.hour,t_b.min,
	t_b.sec);
printf("t_c=%d\t%d\t%d\t%d\t%f\n",t_c.year,t_c.day,t_c.hour,t_c.min,
	t_c.sec);
printf("t_d=%d\t%d\t%d\t%d\t%f\n",t_d.year,t_d.day,t_d.hour,t_d.min,
	t_d.sec);
printf("t_e=%d\t%d\t%d\t%d\t%f\n",t_e.year,t_e.day,t_e.hour,t_e.min,
	t_e.sec);
*/

/* Calculate statevectors associated with the above times */
ta_diff = get_gmt_diff(&t_a,&t_ref);
tb_diff = get_gmt_diff(&t_b,&t_ref);
tc_diff = get_gmt_diff(&t_c,&t_ref);
td_diff = get_gmt_diff(&t_d,&t_ref);
te_diff = get_gmt_diff(&t_e,&t_ref);

t_step = 0.1;

newtprop_(sv_ref,&ta_diff,&t_step,a_propvec);
newtprop_(sv_ref,&tb_diff,&t_step,b_propvec);
newtprop_(sv_ref,&tc_diff,&t_step,c_propvec);
newtprop_(sv_ref,&td_diff,&t_step,d_propvec);
newtprop_(sv_ref,&te_diff,&t_step,e_propvec);

/*
printf("a_propvec=%lf\t%lf\t%lf\n%lf\t%lf\t%lf\n",a_propvec[0],
	a_propvec[1],
	a_propvec[2],a_propvec[3],a_propvec[4],a_propvec[5]);
printf("\nb_propvec=%lf\t%lf\t%lf\n%lf\t%lf\t%lf\n",b_propvec[0],
	b_propvec[1],
	b_propvec[2],b_propvec[3],b_propvec[4],b_propvec[5]);
printf("\nc_propvec=%lf\t%lf\t%lf\n%lf\t%lf\t%lf\n",c_propvec[0],
	c_propvec[1],
	c_propvec[2],c_propvec[3],c_propvec[4],c_propvec[5]);
printf("\nd_propvec=%lf\t%lf\t%lf\n%lf\t%lf\t%lf\n",d_propvec[0],
	d_propvec[1],
	d_propvec[2],d_propvec[3],d_propvec[4],d_propvec[5]);
printf("\ne_propvec=%lf\t%lf\t%lf\n%lf\t%lf\t%lf\n",e_propvec[0],
	e_propvec[1],
	e_propvec[2],e_propvec[3],e_propvec[4],e_propvec[5]);
*/

time_brk(&t_a,&years,&days,&hrs,&mins,&secs);
getlocc_(a_propvec,&r_near,&fd_a,&re,&rp,&lambda,&years,&days,&hrs,
	&mins,&secs,&a[0],&a[1],&a[2],istat);
if(*istat != 0)
   return ;


time_brk(&t_b,&years,&days,&hrs,&mins,&secs);
fd_b += fd_adj * fdot_b;
getlocc_(b_propvec,&r_far,&fd_b,&re,&rp,&lambda,&years,&days,&hrs,
	&mins,&secs,&b[0],&b[1],&b[2],istat);
if(*istat != 0)
   return ;


time_brk(&t_c,&years,&days,&hrs,&mins,&secs);
fd_c += 0.5 * fd_adj * fdot_b;
getlocc_(c_propvec,&r_mid,&fd_c,&re,&rp,&lambda,&years,&days,&hrs,
	&mins,&secs,&c[0],&c[1],&c[2],istat);
if(*istat != 0)
   return ;

/*
printf("fd_a=%lf\nfd_b=%lf\nfd_c=%lf\nfd_d=%lf\nfd_e=%lf\n",fd_a,fd_b,
	fd_c,fd_d,fd_e);
*/
time_brk(&t_d,&years,&days,&hrs,&mins,&secs);
getlocc_(d_propvec,&r_near,&fd_d,&re,&rp,&lambda,&years,&days,&hrs,
	&mins,&secs,&d[0],&d[1],&d[2],istat);
if(*istat != 0)
   return ;


time_brk(&t_e,&years,&days,&hrs,&mins,&secs);
fd_e += fd_adj * fdot_b;
/*
printf("fd_a=%lf\nfd_b=%lf\nfd_c=%lf\nfd_d=%lf\nfd_e=%lf\n",fd_a,fd_b,
	fd_c,fd_d,fd_e);
*/
getlocc_(e_propvec,&r_far,&fd_e,&re,&rp,&lambda,&years,&days,&hrs,
	&mins,&secs,&e[0],&e[1],&e[2],istat);
if(istat != 0)
   return ;

return;
}

/* time_brk(gmt_x,year,day,hour,min,second)  ---------------------------
	This routine breaks a standard GMT time into its component
	parts.
*/

time_brk(gmt_x,year,day,hour,min,second)
struct 	gmt *gmt_x;
int	*year,*day,*hour,*min;
float	*second;
{
/* printf("\nentering 'time_brk.c' routine........."); */
*year    = gmt_x->year;
*day     = gmt_x->day;
*hour    = gmt_x->hour;
*min     = gmt_x->min;
*second  = gmt_x->sec;
/* printf("\n%d\t%d\t%d\t%d\t%f\n",*year,*day,*hour,*min,*second); */
return;
}
