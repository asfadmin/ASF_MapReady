static char sccsid_predict_c[] =
    "@(#)predict.c	1.3 96/04/12 16:59:48";

/*  		predict.c                                      */

/* predict (x,y,z,x_vel,y_vel,z_vel,gmt1,gmt2,prop_vec)  ----------


This routine accepts as inputs the state vectors and start/stop times
and dates in order to generate propagated state vectors at one minute
intervals for a period of 100 minutes(i.e., one orbit or 100 propagated
state vectors) using the supplied ASAP program to perform the
calculations.
The resulting outputs are passed back to the calling program in the
format "t,x,y,z,vx,vy,vz" respectively.  Also, this result is available
in a file called 'fort.7' for further display or printout.

File '5' contains the input data for ASAP which need to be modified
in order to incorporate the new initial state vectors and start/stop
times and dates associated with each run. 

ASAP accepts Kepler Coordinates instead of the Cartesian Coordinates
as given by the input state vectors.  Therefore, a conversion from
Cartesian to Kepler coordinates  must be performed prior to calling
ASAP.

INPUTS:
variables:	x,y,z
type:		double
description:	These are the Cartesian Coordinates in Km.  

variables:	x_vel,y_vel,z_vel
type:		double
description:	These are the velocities in Km/sec in the x,y and z
		Cartesian Coordinates respectively.

variables:	gmt1,gmt2
type:		GMT
description:	These are standard GMT structures that contain the 
		start date and time and the stop date and time
		respectively.

OUTPUTS:
pointer to array 'prop_vec' that contains the propagated state vectors
type		double
description:	The routine returns the 100 propagated state vectors at
		one minute intervals for the supplied orbit.

PROGRAM STRUCTURE:
		The program reads in the input state vectors and start/
		stop dates and times.
		Routine ctok.c is called in order to do the conversion
		from Cartesian to Kepler Coordinates.
		It then reads in the previous contents of file "5" into
		a buffer. Then the new input data in entered into the
		buffer at the precise predetermined locations.
		The data in the buffer is then used to recreate file
		"5" to be used as the new input for the ASAP program.
		ASAP is finally called to calculate the propagated state
		vectors.
ALGORITHMS:
		The equations for the transformation from Cartesian to
		Kepler Coordinates are obtained from  the ESA document
		NO: ER-RP-ESA-SY-004  Issue 1 Rev. 0 page 9-11 dated 
		June 28, 1987.

*/

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include "procfil.h"



#define LIMIT 2700 

predict (x, y, z, x_vel, y_vel, z_vel, gmt1, gmt2, prop_vec)
double  x, y, z, x_vel, y_vel, z_vel;
GMT_PTR gmt1, gmt2;
double *prop_vec;

{

    double  result[6];
    double  ctok ();
    double tint[2], tfin[2], tref[2];
    char    buffer[LIMIT];
    char    s[12][30];
    char   *file_name;
    int     i, j, loc_count, newline_count, mark[12], size, buf_size;
    int     month, day, sec, mills;
    int     fd;

/* data insertion line location table */
    /*
    static int markloc[] = { 13,14,15,16,17,18,22,23,24,25,26,27 };
    int     nmarks = sizeof(markloc) / sizeof(int);
    */

 /* reformat the dates and times in string buffers 6-11 */

 /* start time (TINT) */
    get_month_day(gmt1->yr,gmt1->day,&month,&day);

    /*
    sprintf(s[6],"%4.4d%2.2d%2.2d",gmt1->yr,month,day);
    sec = (int) gmt1->second;
    mills = (int) ((gmt1->second - sec) * 1000.0);
    sprintf(s[7],"%2.2d%2.2d%2.2d.%3.3d",gmt1->hr,gmt1->min,sec,mills);
    */

    tint[0] = gmt1->yr * 10000 + month * 100 + day;
    tint[1] = gmt1->hr * 10000 + gmt1->min * 100 + (double) gmt1->second;

 /* end time (TFIN) */
    get_month_day(gmt2->yr,gmt2->day,&month,&day);

    /*
    sprintf(s[8],"%4.4d%2.2d%2.2d",gmt2->yr,month,day);
    sec = (int) gmt2->second;
    mills = (int) ((gmt2->second - sec) * 1000.0);
    sprintf(s[9],"%2.2d%2.2d%2.2d.%3.3d",gmt2->hr,gmt2->min,sec,mills);
    */

    tfin[0] = gmt2->yr * 10000 + month * 100 + day;
    tfin[1] = gmt2->hr * 10000 + gmt2->min * 100 + (double) gmt2->second;

 /* copy TINT values to TREF */
    /*
    strcpy (s[10],s[6]);
    strcpy (s[11],s[7]);
    */
    tref[0] = tint[0];
    tref[1] = tint[1];

 /* calculate Kepler coordinates from given Cartesian state vector */
    ctok (x, y, z, x_vel, y_vel, z_vel, result);

 /* convert the Kepler coordinates to ascii strings in buffers 0-5 */
    /*
    for (j = 0; j < 6; j++)
	sprintf (s[j], "%22.14lfD0", result[j]);
    */

 /* read ASAP's input file'5' into buffer */

/*
    file_name = "5";
    buf_size = LIMIT;
    size = readfile (file_name, buffer, buf_size);
    buffer[size] = '\0';
*/

/* insert the orbit parameters and times into the buffer */

 /* locate the 12 data insertion points by counting newlines */

/*
    loc_count = 0;
    newline_count = 0;
    for (j = 0; j < nmarks; j++) {
	while (newline_count < markloc[j]) {
	    while (buffer[loc_count] != '\n') {
		loc_count++;
	    }
	    newline_count++;
	    loc_count++;
	}
	mark[j] = loc_count;
    }
*/

/* insert new data at determined locations marked by mark[i] */
/*
    for (j = 0; j < nmarks; j++)
	strncpy(&buffer[mark[j]],s[j],strlen(s[j]));
*/

 /* write out the modified ASAP input file '5' */
/*
    file_name = "5";
    fd = creat (file_name, 0777);
    write (fd, buffer, size);
    close (fd);
*/

 /* use ASAP to calculate propagated state vectors  */
    asap_(result, tint, tfin, tref, prop_vec);
/*
    file_name = "fort.7";
    read_cnvrt (file_name, prop_vec);
*/

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

    return;
}

/* readfile (f_name, storage, size)  ----------------------

**********************************************************************
*                                                                    *
*	readfile.c  	 function reads data from file '5'           *
*                                                                    *
**********************************************************************

INPUTS:
variable:	ASAP file '5'
type:		char
description:	This file has all the inputs required by ASAP as
		specified in the document called 'The Artificial
		Satellite Analysis Program' Version 2.3  by J.Kwok
		dated Aril 1987.
OUTPUTS:
variable:	readfile function value
type:		int
description:	The routine returns the number of bytes read.

variable:	storage[]
type:		char
description:	This is the buffer that will contain the the characters
		read from the ASAP input file '5'.

*/


readfile (f_name, storage, size)
int     size;
char   *f_name, storage[LIMIT];
{
    int     i;
    FILE   *fp, *fopen ();

    if ((fp = fopen (f_name, "r")) == NULL) {
	perror ("ASAP input file 5 can't be opened\n");
	exit (1);
    }
    i = fread(storage,1,size,fp);
    fclose (fp);

    return (i);
}

/*  read_cnvrt (f_name, out_buf)  ---------------------

**********************************************************************
*                                                                    *
*       read_cnvrt.c						     *
*								     *
**********************************************************************

	This routine reads the fort.7 file produced by ASAP, and
	converts its entries into double precision values in the array
	out_buf.  The fort.7 file contains an array of ascii-coded
	values; each group of 7 values constitutes one statevector.
	The 7 values are: 
			  time (hours, 1st statevector = 0)
			  x position (Km)
			  y position
			  z position
			  x velocity (Km/sec)
			  y velocity
			  z velocity
	This routine discards the time value.
*/
read_cnvrt (f_name, out_buf)
char   *f_name;
double *out_buf;
{
    int     i, j, n, flag, c;
    char    char_string[25];
    double  value;

    FILE   *fp, *fopen ();
    if ((fp = fopen (f_name, "r")) == NULL) {
	printf ("file cannot be opened!\n");
	exit (1);
    }
    i = 0;
    j = 0;
    n = 0;
    flag = 0;
    while ((c = getc (fp)) != EOF) {
	while (!isspace (c)) {
	    flag = 1;
	    if ((c == 'D') || (c == 'd'))
		c = 'E';
	    char_string[i] = c;
	    i++;
	    c = getc (fp);
	}
	if (flag == 1) {
	    ungetc (c, fp);
	    char_string[i] = '\0';
	    if (n % 7) {
		value = atof (char_string);
		out_buf[j++] = value;
	    }
	    n++;
	    i = 0;
	    flag = 0;
	}
    }
    fclose (fp);
    return;
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
