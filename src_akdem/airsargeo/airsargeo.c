/****************************************************************************
NAME:	AIRSARGEO  -  creates tie point files of SAR image points
		       and corresponding geographic locations  

SYNOPSIS:       airsargeo SARfile DEMfile tiepointFile
        The SARfile is in the original AIRSAR format.
        The DEMfile is the digital elevation model in LAS format.
	The tiepointFile is the tie point file that is read by projectGeo.

DESCRIPTION:
	The program creates a 9x9 grid tie point file from AIRSAR DEM data.
	The tie points are defined by their latitude/longitude and their
	corresponding sample/line values.
	The algorithm transforms radar mapping coordinates (s, c, h) into
	Cartesian coordinates (X, Y, Z). In another step the Cartesian 
	coordinates are converted into geographic coordinates. Note that the
	ellipsoidal height in not used for the geocoding. 

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/01   R. Gens	Geocodes AIRSAR data


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:
Holecz, Lou and van Zyl, ????, Geocoding of AIRSAR/TOPSAR SAR data. 
Proceedings ???, pp. 35-42.

The formula for the longitude (lambda) in the paper is wrong. The arctan
is missing. It has to be 

	lambda = arctan (Y'/X')

BUGS:
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ifm.h"
#include "asf.h"
#include "airsar_io.h"
#include "asf_meta.h"

#define VERSION 1.0

 /* function declaration */
void print_usage();
char* get_airsar(char* fname, char* Header, char* Record);
char* linetail(char* strIn);

int main(argc,argv)
int argc;
char **argv;
{
	struct DDR ddr;
	double a = 6378137.0;		/* semi-major axis */
	double b = 6356752.3412;	/* semi-minor axis */
	double e2 = 0.00669437999014; 	/* ellipticity */
	double e12 = 0.00673949674228;	/* second eccentricity */
	double lat_peg, lon_peg, head_peg, height;
	double ra, re, rn, d, theta, c_lat, s_lon, c0, s0;
	double x, y, z;
	double lat, lon;
	char in_header[255], dem_ddr[255], dem_img[255], out_file[255];
	float **m1, **m2, **m;
	float *c, *o, *t;
	FILE *fp_in, *fp_out;
	short *img;
	int c_ns, c_nl, i, k;

	/* Process command line parameters */
	if (argc < 4) print_usage();
	
	/* Get the input facility data record file name */
	strcpy(in_header,argv[1]);
	sprintf(dem_ddr,"%s.ddr",argv[2]);
	sprintf(dem_img,"%s.img",argv[2]);
	strcpy(out_file,argv[3]);
	
	/* read dimensions of file out of the DDR file */
        c_getddr(dem_ddr, &ddr);
 	c_ns = ddr.ns / 9;
	c_nl = ddr.nl / 9;

	/* read lat/lon/heading of peg point from header */
	height = atof(get_airsar(in_header, "PARAMETER", "GPS ALTITUDE, M"));
	lat_peg = atof(get_airsar(in_header, "PARAMETER", "LATITUDE OF PEG POINT"));
	lon_peg = atof(get_airsar(in_header, "PARAMETER", "LONGITUDE OF PEG POINT"));
	head_peg = atof(get_airsar(in_header, "PARAMETER", "HEADING AT PEG POINT"));
	lat_peg = lat_peg*PI/180.0;
	lon_peg = lon_peg*PI/180.0;
	head_peg = head_peg*PI/180.0;

	/* read offset parameters */
	s0 = atof(get_airsar(in_header, "PARAMETER", "ALONG-TRACK OFFSET S0  (M)"));	
	c0 = atof(get_airsar(in_header, "PARAMETER", "CROSS-TRACK OFFSET C0  (M)"));

	/* calculating some radii needed later */
	re = a / sqrt(1-e2*sin(lat_peg)*sin(lat_peg));
	rn = (a*(1-e2)) / pow(1-e2*sin(lat_peg)*sin(lat_peg), 1.5);
	ra = (re*rn) / (re*cos(head_peg)*cos(head_peg)+rn*sin(head_peg)*sin(head_peg));

	/* initializing matrices */
	m1 = matrix(1,3,1,3);
	m2 = matrix(1,3,1,3);
	m  = matrix(1,3,1,3);
	o  = alloc_vector(1,3);
	t  = alloc_vector(1,3);
	c  = alloc_vector(1,3);

	/* M1 matrix */
	m1[1][1] = -sin(lon_peg);
	m1[1][2] = -sin(lat_peg)*cos(lon_peg);
	m1[1][3] = cos(lat_peg)*cos(lon_peg);
	m1[2][1] = cos(lon_peg);
	m1[2][2] = -sin(lat_peg)*sin(lon_peg);
	m1[2][3] = cos(lat_peg)*sin(lon_peg);
	m1[3][1] = 0.0;
	m1[3][2] = cos(lat_peg);
	m1[3][3] = sin(lat_peg);

	/* M2 matrix */
	m2[1][1] = 0.0;
	m2[1][2] = sin(head_peg);
	m2[1][3] = -cos(head_peg);
	m2[2][1] = 0.0;
	m2[2][2] = cos(head_peg);
	m2[2][3] = sin(head_peg);
	m2[3][1] = 1.0;
	m2[3][2] = 0.0;
	m2[3][3] = 0.0;

	/* O matrix */
	o[1] = re*cos(lat_peg)*cos(lon_peg)-ra*cos(lat_peg)*cos(lon_peg);
	o[2] = re*cos(lat_peg)*sin(lon_peg)-ra*cos(lat_peg)*sin(lon_peg);
	o[3] = re*(1-e2)*sin(lat_peg)-ra*sin(lat_peg);

	/* M = M1 * M2 */
	matrix_multiply(m1,m2,m,3,3,3);

	/* read points from original DEM */
	img = (short *)MALLOC(sizeof(short)*ddr.ns*ddr.nl);
	fp_in = FOPEN(dem_img, "rb");
	FREAD(img,1,sizeof(short)*ddr.ns*ddr.nl,fp_in);
	fp_out = FOPEN(out_file, "w");
	
	for (k=1; k<=ddr.nl; k=k+c_nl)
	  for (i=1; i<=ddr.ns; i=i+c_ns) { 

		/* radar coordinates */
		c_lat = (i*ddr.pdist_y+c0)/ra;
		s_lon = (k*ddr.pdist_x+s0)/ra;
		height = img[k*ddr.ns+i];

		/* radar coordinates in WGS84 */
		t[1] = (ra+height)*cos(c_lat)*cos(s_lon);
		t[2] = (ra+height)*cos(c_lat)*sin(s_lon);
		t[3] = (ra+height)*sin(c_lat);

		c[1] = m[1][1]*t[1] + m[1][2]*t[2] + m[1][3]*t[3];
		c[2] = m[2][1]*t[1] + m[2][2]*t[2] + m[2][3]*t[3];
		c[3] = m[3][1]*t[1] + m[3][2]*t[2] + m[3][3]*t[3];

		/* shift into local Cartesian coordinates */
		x = c[1] + o[1];// + 9.0;
		y = c[2] + o[2];// - 161.0;
		z = c[3] + o[3];// - 179.0;

		/* local Cartesian coordinates into geographic coordinates */
		d = sqrt(x*x+y*y);
		theta = atan2(z*a, d*b);
		lat = atan2(z+e12*b*sin(theta)*sin(theta)*sin(theta), d-e2*a*cos(theta)*cos(theta)*cos(theta));
		lon = atan2(y, x);
		fprintf(fp_out, "%lf %lf %i %i\n", lat*180.0/PI, lon*180.0/PI, i, k);

	}
	FCLOSE(fp_in);
	FCLOSE(fp_out);
        free_matrix(m1,1,3,1,3);
        free_matrix(m2,1,3,1,3);
        free_matrix(m,1,3,1,3);
        free_vector(o,1,3);
        free_vector(t,1,3);
        free_vector(c,1,3);

	return 0;
}


void print_usage() {
	fprintf(stderr,"Usage: airsargeo SARfile DEMfile tplfile\n");
	fprintf(stderr,"\tSARfile\t\timage name\n");
	fprintf(stderr,"\tDEMfile\t\tfile name\n");
	fprintf(stderr,"\ttplfile\t\t name\n");
	fprintf(stderr,"\nCreates two output files.  One contains a "
		"grid of SAR image points \n(line, sample) coordinates.  " 
		"The other contains calculated geographic \n(lat, lon) "
		"coordinates.  These files can be used to find a mapping "
		"from \nthe SAR image coordinates to geographic coordinates "
		"using a 2-D planar \nwarp mapping.");
	fprintf(stderr,"\n\nVersion %.2f, ASF STEP Tools\n",VERSION);
	exit(1);
}


char* get_airsar(char* fname, char* Header, char* Record)
{
        FILE* fp;
        char airsar_rec[50];
        int HDR=0, REC=0;
        char c;
        int i;
        int rl;
        char chOut[256];

        rl=strlen(Record);

        fp=fopen(fname, "r");
        if(fp==NULL){
          printf("ERROR!  NULL file pointer!\n");
          return NULL;
        }

        if( strncmp(Header,"FIRST",5)==0) HDR=1;

        while(!feof(fp) && !REC){
          FREAD(airsar_rec, 1, 50, fp);
          if(airsar_rec[0]==0){ 
                while(( c = getc(fp) ) == 0){};
                ungetc(c,fp);
          }
          strcpy(chOut, linetail(airsar_rec));

          if(!HDR)
            if( !strcmp(chOut, Header) ) HDR=1;
          if(HDR && !REC) {
            REC = 1;
            for(i=0; i<rl; i++) {
                if(airsar_rec[i]!=Record[i]){i = rl; REC=0;}
            }
          }
        }
        fclose(fp);
        return chOut;
}

char* linetail(char* strIn)
{
        int i=49;
        char chOut[50];

        while(strIn[i] != ' ') i--;
        strcpy(chOut, &strIn[++i]);

        return chOut;
}

