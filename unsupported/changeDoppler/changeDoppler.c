/****************************************************************
NAME: 		changeDoppler

SYNOPSIS:       changeDoppler <delta doppler> <CEOS> <OUTPUT> [-near | -bilin | -sinc]

DESCRIPTION:	Creates a translation grid (for use by fitplane) for a 
given change in the doppler of a geocoded scanSAR image.  This is used 
to correct a doppler PRF estimation problem for the RGPS.

EXTERNAL ASSOCIATES:
get_ifiledr.c      Process image file data record; Extracts size of SAR image.
get_raddr.c        Process radiometric data record; Extract radiometric
		    calibration coefficients.	
get_mpdr.c Process map projection data record; Extract geolocation for images.
xform.c    Transformation from geocoded image line,sample to SSM/I east,north.
ssmill.c   Transformation from SSM/I east,north to geographic lat,lon
twoway.c   Transformation from lat,lon to original image line,sample
	
FILE REFERENCES:
		inSAR 	input SAR image
		outSAR	output SAR image

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
	1.0	3/98	Correct doppler PRF for RGPS project.
	1.1	6/98	Updated for new FICO format.


ALGORITHM DESCRIPTION:

****************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

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

Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef sgi
#ifdef SYSV
#include <sys/ieeefp.h>
#include <floatingpoint.h>
#endif
#endif
#include <math.h>
#include "ceos.h"
#include "asf_las.h"
#include "asf_sar.h"

/* constants */
#define ERROR_IMAGEID	7000
#define VERSION		1.1 
#define stepX 100
#define stepY 100

double dopplerChange=0;
char *remapMethod="-near";

void changeCEOS(char *inName,char *outName);
void remapImage(char *inName, char *outName);

main(int	argc, char	**argv)
{
	char	inSAR[40],
	outSAR[40];
	FILE *grid;
	unsigned char	*ibuff, *obuff;	/* Input and output buffers	*/
	float	nrtmp, Dmax, Dmin;	/* Loop invariant, Min and Max
						   sigma0 calibration range	*/
	double	ptmp;		/* Loop invariant		*/
	double	tmp, frac;
	FILE            * fp0, *fp1;		/* input and output file ptrs	*/
	int modify_calib=0;/*Modify calibration coefficients? */
	int useLookTable=1;/*Use lookup table for Radarsat look angle? */
	int	x,y, j1, j2; 	/* loop counters, imterpolation */
	int	np, nl;		/* number lines and samples	*/
	int	look_np, look_nl;   /* size of look angle matrix    */
	int pointNo=1;
	int	pre;		/* bytes of prefix data         */
	int	ispace;		/* length of interpolation space
						   between radiometric correction
						   factors in rdr		*/
	int skipSamples=1;/*1/Fraction of samples to actually process.*/
	struct VRADDR rdr;		/* Radiometric data record	*/
	struct IOF_VFDR iof_vfdr;		/* Imagery option file, value of
						   file descriptor record	*/
	struct VMPDREC mpdr_data;           /* Map projection data record   */
	struct dataset_sum_rec dssr;	/* Data Set Summary Record      */
	long	type;			/* Type of input image     	*/
	double	lat, lon;			/* geodetic lat & lon of point	*/
	double	east, north;			/* SSM/I east,north coordinates */
	long	line, samp;			/* calculated image line,sample */
	long	dum1, dum2;			/* dummy arguments for twoway() */
	long	mode, era;
	double	zero = 0.0;
	double	a[3];                        /* Calibrate coeff's            */
	double	proj_x, proj_y, pd_x, pd_y, curr_x, curr_y;
	double	look_ang;
	void	ModifyParameter();
	double	proj_2_look();

	if (argc != 4 && argc!=5 ) {
		printf("Usage: %s <shift> <inSAR> <output> [-near | -bilin | -sinc] \n", argv[0]);
		printf("       Creates a doppler-shift grid for SAR images.\n");
		printf("       inputs:  <shift> new doppler, in PRF\n");
		printf("          <inSAR> ASF CEOS image to shift\n");
		printf("       outputs: <output>, a shifted image.\n");
		printf("       [-near | -bilin | -sinc]: remapping methods (default is -near).\n");
		printf("       Version %.2f,  ASF STEP TOOLS\n", VERSION);
		exit(1);
	}
	if (argc==5)
		remapMethod=argv[4];
	
	sscanf(argv[1],"%lf",&dopplerChange);
	era = set_era(argv[2], inSAR, 0);
	strcpy(outSAR,argv[3]);
	
	grid=fopen("grid","w");
	
	
	/* Determine format and size of input file */
	get_ifiledr(inSAR, &iof_vfdr);
	nl = iof_vfdr.numofrec;
	pre = (era) ? 192 : 12;
	np = iof_vfdr.reclen-pre;
	ispace = (np - pre) / 256;

	get_dssr(inSAR, &dssr);

	/* Check for SCANSAR data */
	if (strncmp(dssr.product_type, "SCANSAR", 7) == 0) {
		printf("\nImage %s is SCANSAR\n", inSAR);
		type = 5;
	} else {
		printf("This only works on SCANSAR data.\n");
	}
	printf("Output-Sigma0-FILE : %s\n", outSAR);

	/* get & assign map projection data values & initialize transformations */
	if (type == 3 || type == 4) {
		get_mpdr(inSAR, &mpdr_data);
		init_xform(&mpdr_data);

		/* Need to set length of interpolation space, since the rotated
		         images do not allow a simple calculation to be made here    */
		if (type == 4) 
			ispace = 4;
		else if (type == 3) 
			ispace = 32;

		/* Initialize the twoway transformation, mapping from lat,lon
			 to image line,sample for original image (non-geocoded)  */
		mode = 0;
		twoway(mode, type, inSAR, &dum1, &dum2, &line, &samp, zero, &lat, &lon);
		mode = 1;
	} else if (type == 5) {
		char	projection[256];
		get_mpdr(inSAR, &mpdr_data);
		printf("Map Proj mpdesig is %s\n", mpdr_data.mpdesig);

		if (strncmp(mpdr_data.mpdesig, "PS-SMM/I", 8) == 0) {
			strcpy(projection, "ssmi");
		} else if (strncmp(mpdr_data.mpdesig, "UTM", 3) == 0) {
			strcpy(projection, "utm");
		} else if (strncmp(mpdr_data.mpdesig, "LAMBERT", 7) == 0) {
			strcpy(projection, "lambert");
		} else { 
			printf("Can not match projection\n\n"); 
			exit(1); 
		}

		ssar_tool_init(inSAR, projection);

		proj_y = mpdr_data.tlcnorth;
		proj_x = mpdr_data.tlceast;
		pd_y   = (mpdr_data.blcnorth-mpdr_data.tlcnorth)/nl;
		pd_x   = (mpdr_data.trceast-mpdr_data.tlceast)/np;
	}

	printf("\n\n  nl, np = %i, %i\n", nl, np);

	/* allocate appropriate buffer space */
	ibuff = (unsigned char *) malloc ((unsigned) np * sizeof(char));
	
	/*  Open input and output files */
	if ( NULL == (fp0 = fopen(inSAR, "rb"))) {
		printf("\7\n Cannot Open File !! : %s\n\n", inSAR);
		printf("Program abnormally terminated\n\n"); 
		exit(1); 
	}

	/* Read input file, convert, and write to output file  */
	for (y = 0; y < nl; y +=stepY) {
		#define tableRes 40 /*Num. of samples between look angle table entries.*/
		double look_table[50000/tableRes];
		fseek(fp0, (np+pre) * (y+1), 0);
		/*Copy over CEOS header.*/
		fread(ibuff, pre, 1, fp0);
		/*Read image data.*/
		fread(ibuff,np,1,fp0);
		for (x = 0; x < np; x +=stepX)
		 if (ibuff[x]) {
			double new_posX,new_posY;
			double out_x,out_y;
			curr_y=proj_y+y*pd_y;
			curr_x=proj_x+x*pd_x;
			convert_for_doppler(curr_x,curr_y,&new_posX,&new_posY);
			out_x=(new_posX-proj_x)/pd_x;
			out_y=(new_posY-proj_y)/pd_y;
			fprintf(grid,"%6f %6f %8.5f %8.5f %4.2f\n",
				(float)out_x,(float)out_y,(float)x,(float)y,1.0);
			pointNo++;
		  }

		/* End for x */
		if ((y % 100) == 0) 
			printf(" Now Processing Line No = %d \n", y);
	}
	printf("Wrote %i lines of %i samples\n", nl, np);
	fclose(fp0);
	fclose(grid);
	printf("Remapping image..\n");
	remapImage(argv[2],argv[3]);
	changeCEOS(argv[2],argv[3]);
	
	printf("\n\n Conversion complete !!! \n\n");

	return(0);
}
void Execute(char *cmd)
{
	int ret;
	printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
	 "Executing:\n\t%s\n",cmd);
	ret=system(cmd);
	if (ret!=0)
	{ printf("Command ended in error %d./n",ret);exit(1);}
}
void remapImage(char *inName, char *outName)
{
	char cmd[255];
	Execute("fit_plane grid grid_matrix k 0.95");
	sprintf(cmd,"sarin %s tmp_%s",inName,outName);Execute(cmd);
	sprintf(cmd,"remap tmp_%s.img %s.img -map 0 255 -matrix grid_matrix -sameSize %s",outName,outName,remapMethod);Execute(cmd);
	sprintf(cmd,"/bin/cp tmp_%s.ddr %s.ddr",outName,outName);Execute(cmd);
	
	Execute("/bin/rm grid grid_matrix tmp_*.ddr tmp_*.img");
}
