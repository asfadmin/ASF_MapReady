/******************************************************************************
NAME:  gen_oe - generates an orbital element input file for ASAP propagator

SYNOPSIS:  gen_oe <infile>

DESCRIPTION:  

	gen_oe reads an input vector file for propagation (see parse_lzp_par
    for file description) and creates a file called "5" that is input to the
    ASAP propagator.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     7/7/95 dcuddy	main routine to propagate a set of state vectors
    2.0     9/98   Tom Logan	make propagation routine easily usable

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   gen_oe - generates an orbital element input file for ASAP propagator    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "procfil.h"

#define LIMIT 2700 
#define VERSION 2.0

main(int argc,char **argv)
{
  /*static int markloc[] = { 13,14,15,16,17,18,21,22,23,24,25,26,27 };
    	int     i, j, k, loc_count, newline_count, mark[12], size, buf_size;
	int     nmarks = sizeof(markloc) / sizeof(int);
    	char    s[13][30];*/
  /*char   *file_name;*/  /*, temp[80], inbuf[255];
	char    buffer[LIMIT]; */
	FILE *fp;
	
	
	double  x, y, z, xv, yv, zv;
        int    refYear, refMonth, refDay;
        int    endYear, endMonth, endDay;
	double refSecs, endSecs;
  	int    nsteps;
	/*GMT    start_gmt, end_gmt;*/
    	double  result[6];
    	double  ctok ();
        double gmt2hms(double gmtSec);
	/*float fx;
	int fd;*/

    if (argc != 2) 
	{ 
	printf("\nUsage: %s <infile>\n",argv[0]);
	printf("\nVersion %.2f, ASF SAR TOOLS \n\n", VERSION);
	exit(1);
	}

    /* read the input parameters from file */
    fp = FOPEN(argv[1], "r");

    /* parse the input file */
    fscanf(fp,"%lf",&x);
    fscanf(fp,"%lf",&y);
    fscanf(fp,"%lf",&z);
    fscanf(fp,"%lf",&xv); xv /= 1000.0;
    fscanf(fp,"%lf",&yv); yv /= 1000.0;
    fscanf(fp,"%lf",&zv); zv /= 1000.0;

/*    printf("x  = %22.14E\n", x); 
    printf("y  = %22.14E\n", y); 
    printf("z  = %22.14E\n", z); 
    printf("xv = %22.14E\n", xv); 
    printf("yv = %22.14E\n", yv); 
    printf("zv = %22.14E\n", zv); */

    fscanf(fp,"%i",&refYear);
    fscanf(fp,"%i",&refMonth);
    fscanf(fp,"%i",&refDay);
    fscanf(fp,"%lf",&refSecs);

    fscanf(fp,"%i",&endYear);
    fscanf(fp,"%i",&endMonth);
    fscanf(fp,"%i",&endDay);
    fscanf(fp,"%lf",&endSecs);

    fscanf(fp,"%li",&nsteps);

/*    printf("start = %4i%.2i%.2i  %lf\n",refYear,refMonth,refDay,refSecs);
    printf("        %lf\n",gmt2hms(refSecs));
    printf("end   = %4i%.2i%.2i  %lf\n",endYear,endMonth,endDay,endSecs);
    printf("        %lf\n",gmt2hms(endSecs));
    printf("steps = %i\n",nsteps);
    printf("time_step = %lf\n",(endSecs-refSecs)/(double)nsteps);*/

    FCLOSE(fp); 

 /* calculate Kepler coordinates from given Cartesian state vector 
    printf("Calculating orbital elements ...\n");*/
    ctok (x, y, z, xv, yv, zv, result);


 /* write out the ASAP input file '5' */
   /* file_name = "5";*/
    fp=FOPEN("/var/tmp/5","w");
    
    fprintf(fp,
"20                            L\n"
"20                            M\n"
"0                             IRES\n"
"0                             ISUN\n"
"0                             IMOON\n"
"1                             IEPHEM\n"
"0                             IDRAG\n"
"1                             IDENS\n"
"0                             ISRP\n"
"0                             IORB\n"
"0                             IPRINT\n"
"0                             INODE\n"
"0                             IPLOT\n"
" %22.14lfD0     ORB(1), A\n"
" %22.14lfD0        (2), E\n"
" %22.14lfD0        (3), I\n"
" %22.14lfD0        (4), NODE\n"
" %22.14lfD0        (5), W\n"
" %22.14lfD0        (6), M\n"
"1.D-12                        RELERR\n"
"1.D-12                        ABSERR\n"
"%.6lfD0                    STEP\n"
"%4i%.2i%.2i.D0                   TINT(1)\n"
"%.7lfD0                   (2)\n"
"%4i%.2i%.2i.D0                   TFIN(1)\n"
"%.7lfD0                   (2)\n"
"%4i%.2i%.2i.D0                   TREF(1)\n"
"%.7lfD0                   (2)\n"
"3.9860045D5                   GE\n"
"6378.140D0                    RE\n"
".4178074216D-2                RATE\n"
"99.652865509D0                PM\n"
".8182D-1                      ELLIP\n"
"6468.14D0                     RATM\n"
"0.D0                          RDENS\n"
"0.D0                          RHT\n"
"0.D0                          SHT\n"
"1.D0                          ALTMAX\n"
"1.D0                          WT\n"
"10.D-6                        AREAD\n"
"10.D-6                        AREAS\n"
"2000.D0                       SCMASS\n"
"2.D0                          CDRAG\n"
"6.6D-3                        CSRP\n"
".13271244D12                  GS\n"
"0.D0                          ES(1)\n"
"0.D0                          ES(2)\n"
"0.D0                          ES(3)\n"
"0.D0                          ES(4)\n"
"0.D0                          ES(5)\n"
"0.D0                          ES(6)\n"
"0.D0                          ES(7)\n"
".490279D4                     GM\n"
"0.D0                          EM(1)\n"
"0.D0                          EM(2)\n"
"0.D0                          EM(3)\n"
"0.D0                          EM(4)\n"
"0.D0                          EM(5)\n"
"0.D0                          EM(6)\n"
"0.D0                          EM(7)\n"
"    2    0                 -.10826271D-2                          0.D0\n"
,result[0],result[1],result[2],result[3],result[4],result[5]
,(endSecs-refSecs)/(double)nsteps
,refYear,refMonth,refDay
, gmt2hms(refSecs)
, endYear,endMonth,endDay
, gmt2hms(endSecs)
, refYear,refMonth,refDay
, gmt2hms(refSecs));
    
    FCLOSE(fp);

/*    printf("Gen_oe:  Created ASAP input file\n\n");*/
}

double gmt2hms(double gmtSec)
 {
    int    hour, min;
    /* double ret = 0.0;*/
    double remaining;
    char   buf[256];

    hour = gmtSec / 3600;
    remaining = gmtSec - (double) (hour*3600.0);
    min = remaining / 60;
    remaining = remaining - (double) (min*60.0);
    sprintf(buf,"%.2i%.2i%013.10lf",hour,min,remaining);
    return(atof(buf));
 }


/*  double  ctok (rx, ry, rz, vx, vy, vz, kepler)  --------------

**********************************************************************
*                                                                    *
* 'ctok.c'  converts the Cartesian State Vector to Kepler elements   *
*                                                                    *
**********************************************************************

INPUTS:
variables:	rx,ry,rz
type:		double
description:	Cartesian coordinates in the x,y,z axis in Km.

variables:	vx,vy,vz
type:		double
description:	Velocity in the x,y,z direction in Km/sec.

OUTPUTS:
variable:	kepler[6]
type:		double
description:	the resultant Kepler transformation consisting of the
		six Kepler elements,namely, 
		'a' the semi-major axis in Km
		'e' the eccentricity
		'i' angle of inclination (deg)
		'Omega' longitude of ascending node (deg.)
		'w' argument of periapsis (deg.)
		'M' mean anomaly (deg.)
*/

double  ctok (rx, ry, rz, vx, vy, vz, kepler)
double  rx, ry, rz, vx, vy, vz, kepler[6];

{
    double  a, u, r, v, es, ec, e, E, M_deg, V2, H, i, i_deg;
    double  w_deg, cu, su, somega, comega, omega;
    double  pi, mu;
    double tanfix();

    pi = 3.141592653589793;
    mu = 3.9860045e+5;


 /* determine semi major axis 'a' */
    r = sqrt ((rx * rx) + (ry * ry) + (rz * rz));
    V2 = (vx * vx) + (vy * vy) + (vz * vz);
    a = (mu * r) / ((2.0 * mu) - (r * V2));

 /* determine eccentricity 'e' */
    es = ((rx * vx) + (ry * vy) + (rz * vz)) / sqrt (mu * a);
    ec = 1.0 - (r / a);
    e = sqrt ((es * es) + (ec * ec));

/* determine mean anomaly'M' */
    E = 2.0 * tanfix ((e - ec), es);
    M_deg = (180.0 / pi) * (E - es);
    if (M_deg < 0.0)
	M_deg = M_deg + 360.0;

/* determine angle of inclination 'i' */
    H = sqrt (mu * a * (1.0 - (e * e)));
    i = acos (((rx * vy) - (ry * vx)) / H);
    i_deg = i * (180.0 / pi);

/* determine omega */
    somega = ((ry * vz) - (rz * vy)) / (sin (i) * H);
    comega = ((rx * vz) - (rz * vx)) / (sin (i) * H);
    omega = (180.0 / pi) * 2.0 * tanfix ((1.0 - comega), somega);
    if (omega < 0.0)
	omega = omega + 360.0;

/* determine w_deg */
    su = rz / (r * sin (i));
    cu = ((ry / r) * somega) + ((rx / r) * comega);
    if (rz == 0.0)
	cu = 1.0;
    u = 2 * tanfix ((1.0 - cu), su);
    v = 2 * atan (sqrt ((1.0 + e) / (1.0 - e)) * tan (E / 2.0));
    w_deg = (180.0 / pi) * (u - v);
    if (w_deg < 0.0)
	w_deg = w_deg + 360.0;


    kepler[0] = a;
    kepler[1] = e;
    kepler[2] = i_deg;
    kepler[3] = omega;
    kepler[4] = w_deg;
    kepler[5] = M_deg;

    return (0);
}


/* tanfix(a,b) --------------------------------------------------------

	This routine calculates the tangent of a/b, protecting
	against b=0.
*/

double tanfix(a,b)
    double a,b;
{
    double pi = 3.141592653589793;

    if (b == 0.0) {
	if (a < 0.0)
	    return (-pi / 2.0);
	else if (a > 0.0)
	    return (pi / 2.0);
	else
	    return (0.0);
    }
    return (atan (a/b));
}




