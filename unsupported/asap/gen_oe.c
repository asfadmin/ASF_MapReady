/* main routine to propagate a set of state vectors */
/* dcuddy, jpl, July 7, 1995 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "procfil.h"

#define LIMIT 2700 

main()
{
	static int markloc[] = { 13,14,15,16,17,18 };
    	int     i, j, k, loc_count, newline_count, mark[12], size, buf_size;
	int     nmarks = sizeof(markloc) / sizeof(int);
    	char    s[12][30];
   	char   *file_name, temp[80], inbuf[255];
    	char    buffer[LIMIT];
	FILE *fp, *fopen ();
	double  x, y, z, xv, yv, zv;
	GMT start_gmt, end_gmt;
    	double  result[6];
    	double  ctok ();
	float fx;
	int fd;

 /* input the start state vector from a file */

    if ((fp = fopen ("input_vect", "r")) == NULL) {
	perror ("input file input_vect can't be opened\n");
	exit (1);
    }

/*
    i = fread(inbuf,1,155,fp);
    fclose (fp);
/* parse the string into the six vectors (I hate C language) 
/* assume that time take 23 bytes at begin and each vector is in 22d format

    for (j=0; j<6; j++) {
        for (i=0; i<22; i++) {
          k = i + 23 + j*22;
          temp[i] = inbuf[k];
	  temp[22] = '\0';
	}
	if (j == 0) { x = atof(temp); printf("x  = %22.14E\n", x); }
	if (j == 1) { y = atof(temp); printf("y  = %22.14E\n", y); }
	if (j == 2) { z = atof(temp); printf("z  = %22.14E\n", z); }
	if (j == 3) { xv = atof(temp); printf("xv = %22.14E\n", xv); }
	if (j == 4) { yv = atof(temp); printf("yv = %22.14E\n", yv); }
	if (j == 5) { zv = atof(temp); printf("zv = %22.14E\n", zv); }
    }
*/
    fscanf(fp,"%lf",&x);
    fscanf(fp,"%lf",&y);
    fscanf(fp,"%lf",&z);
    fscanf(fp,"%lf",&xv); xv /= 1000.0;
    fscanf(fp,"%lf",&yv); yv /= 1000.0;
    fscanf(fp,"%lf",&zv); zv /= 1000.0;

    printf("x  = %22.14E\n", x); 
    printf("y  = %22.14E\n", y); 
    printf("z  = %22.14E\n", z); 
    printf("xv = %22.14E\n", xv); 
    printf("yv = %22.14E\n", yv); 
    printf("zv = %22.14E\n", zv); 

    fclose(fp); 

    printf("Calculating orbital elements ...\n");
 /* calculate Kepler coordinates from given Cartesian state vector */
    ctok (x, y, z, xv, yv, zv, result);

 /* convert the Kepler coordinates to ascii strings in buffers 0-5 */
    for (j = 0; j < 6; j++)
	sprintf (s[j], "%22.14lfD0", result[j]);

 /* read ASAP's input file'5' into buffer */
    file_name = "5";
    buf_size = LIMIT;
    size = readfile (file_name, buffer, buf_size);
    buffer[size] = '\0';

/* insert the orbit parameters and times into the buffer */

 /* locate the 12 data insertion points by counting newlines */
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

/* insert new data at determined locations marked by mark[i] */
    for (j = 0; j < nmarks; j++)
	strncpy(&buffer[mark[j]],s[j],strlen(s[j]));

 /* write out the modified ASAP input file '5' */
    file_name = "5.new";
    fd = creat (file_name, 0777);
    write (fd, buffer, size);
    close (fd);

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
    int     i, j, n, flag;
    char    c, char_string[25];
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
