/******************************************************************
NAME:	    ll2proj.c -- converts lat/lon to the given projection 
coordinates

SYNOPSIS:   ll2proj <demFile> <inLATLONfile> <outPROJfile>

DESCRIPTION:

FILE FORMATS
    demFile:	(give w/out extension)
	{ddr file data structure}
    
    inLATLONfile:
	year, julian day, total sec.
	offset(sec), nearLat, nearLon, farLat, farLon
	...
	
    outPROJfile:
	year, julian day, total sec.
	number of points
	offset(sec), nearNorthing, nearEasting, farNorthing, farEasting
	...

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:	    PURPOSE
    --------------------------------------------------------------
    1.0      8/00   M. Jessop       Transform Lat/lon coordinates into UTM
				    coordinates.
    2.0	    11/00   J. Badgley	    Transform Lat/lon coordinates into
				    projection coordinates of the input
				    DEMfile; changed name of program to
				    ll2proj.
******************************************************************/
/***************** Copyright Notice ***********************
                        English: 
         You can freely use, modify, and re-distribute
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the
University of Alaska Technology Corporation, below.

Legalese:
                 COPYRIGHT NOTIFICATION
(C) COPYRIGHT 2000 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright
laws of the United States. Permission is hereby granted to
use, reproduce, and prepare derivative works for noncommercial
purposes at no charge, provided that this original copyright
notice, and disclaimer are retained and any changes are
clearly documented. Any entity desiring permission to
incorporate this software or a work based on the software
into a product for sale must contact the University of
Alaska Technology Corporation.


This software was authored by:
MIKE JESSOP
under the supervision of
RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195
Any questions or comments on the software may be directed
to one of the authors: Rick Guritz, and Tom Logan; or to
http://www.images.alaska.edu


NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF,
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/


#include "asf.h"
#include "ddr.h"
#include "proj.h"

#define RPD 0.017453293		/* Radians per Degree */

main (int argc, char *argv[]) {
    int	    i, n, cnt;	    /* counter and number of pts */
    double  lat1, lon1,	    /* near edge lat/lon coordinates */
	    lat2, lon2,	    /* far edge lat/lon coordinates */
	    x1, y1,	    /* near edge UTM coordinates (northing/easting) */
	    x2, y2,	    /* far edge UTM coordinates */
	    tmp, 	    /* temporary value holder */
	    hilat, lolat, 
	    maxlon;
    char    inDDR[255];
    struct  DDR ddr;
    FILE *fpi, *fpo;
    forward_transform latLon2proj[100];    
    int iflg=0;

    /* Initialize */
    n=0; cnt=0;
    hilat = 71.5;	/* Are these necessary? */
    lolat = 52.0;
    maxlon = -126.0;
    
    /* Test for proper arguments */
    if (argc != 4) { 
        printf("Usage: %s <demFile> <inLATLONfile> <outUTMfile>\n",argv[0]); exit(1); 
    }
    
    /* Read input DEM ddr file */
    strcat(strcpy(inDDR, argv[1]), ".ddr");
    if (c_getddr(inDDR, &ddr) != 0) {
	printf("Error returned from c_getddr:  unable to read file %s\n", inDDR);
	exit(1);
    }
        
    /* Open input and output vector files */
    fpi = fopen(argv[2], "r");
    if ( fpi == (FILE *) NULL) {
        printf("\n*** couldn't open data file: %s\n", argv[2]);
        exit(1);
    }
    fpo = fopen(argv[3], "w");
    
    /* Initialize conversion routines */

    for_init(ddr.proj_code,ddr.zone_code,ddr.proj_coef,ddr.datum_code,NULL,
    NULL,&iflg,latLon2proj);

    /* Read ref. time header info from input lat/lon data file */
    for (i=1; i<=3; i++) { fscanf(fpi, "%lf", &tmp); }
    
    /* Find number of points in file */
    while (!feof(fpi)) {
        n++;
        for (i=1; i<=2; i++) { fscanf (fpi, "%lf", &lat1); }
	for (i=1; i<=3; i++) { fscanf (fpi, "%lf", &lon2); }
	if (lat1<hilat && lat1>lolat && lon2 < maxlon) cnt++;
    }
    n--;
    printf("\nNumber of lines parsed: %i", n);
    if (cnt==0) { printf("\nERROR: no points are located near Alaska\n"); exit(1); }
    
    /* Reset file position to start & re-read ref. time header, write to output */
    fseek(fpi, 0L, 0);
    for (i=1; i<=3; i++) { 
	fscanf (fpi, "%lf", &tmp);
	fprintf (fpo, "%lf ", tmp);
    }
    fprintf (fpo, "\n%lf", (double) cnt );
    
    /* Read input file, convert, write to output */
    for (i=1; i<=n; i++) {
        fscanf (fpi, "%lf", &tmp);  /* Read ref. time */
	fscanf (fpi, "%lf%lf%lf%lf", &lat1, &lon1, &lat2, &lon2);
	if (lat1<hilat && lat1>lolat && lon2 < maxlon) {
	    lat1 *= RPD; lon1 *= RPD;
	    lat2 *= RPD; lon2 *= RPD;
	    latLon2proj[ddr.proj_code](lon1, lat1, &x1, &y1);
	    latLon2proj[ddr.proj_code](lon2, lat2, &x2, &y2);
	    fprintf(fpo, "\n%lf %lf %lf %lf %lf", tmp, y1, x1, y2, x2);
	}
    }
    fprintf(fpo, "\n");
    printf("\nNumber of lines written to file: %i\n", cnt);
    fclose(fpi);
    fclose(fpo);
}
