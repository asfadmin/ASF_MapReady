/******************************************************************************
NAME:	fix_in_fromraw

SYNOPSIS:  fix_in_fromraw <infile.par> <infile.meta> <in/outfile.in> 
			  [# samples] [-log <file>] [-quiet]

DESCRIPTION:
	Calculates doppler polynomial based on .par, and .meta information. The 
  result is stored back into the given .in file.

EXTERNAL ASSOCIATES:
    NAME:			USAGE:
    -----------------------------------------------------------------

FILE REFERENCES:
    NAME:			USAGE:
    -----------------------------------------------------------------
    infile.par			Input par file
    infile.meta			Input meta file
    (infile/outfile).in		Input / Output .in file

PROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
     1.0     6/00      D.Koster   Initial Development.
     1.1     6/01      T. Logan   Modified to read ref. dimensions
				  & to read meta file structure
     1.2     7/01      R. Gens	  Added logfile and quiet switch

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*****************************************************************************/
/****************************************************************************
*								            *
*   fix_in_fromraw -- This program modifies a .in file so that the doppler  *
*		      quadratic is given correctly.			    *
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
#include "dateUtil.h"
#include "asf_meta.h"
#include "aisp_params.h"

#define VERSION 1.2

main(int argc, char *argv[])
{
	ymd_date date1;			/* Structures for calculating center time */
	hms_time time1;
	julian_date jDate;

	int ii, jj, kk, nSamples=100;	/* counter variables, and number of samples */

	char metaFile[255];		/* meta file name */	
	char parFile[255];		/* par file name */
	char dotInFile[255];		/* .in file name */
	char line[255], param[255];	
	char Date[255];

	float *dopVec, *sampVec;	/* floats for calculating the doppler polynomial */
	float currSamp = 0.0;
	float t1, t2, t3;

	double aVal[3][4];				/* doubles for calculating doppler */
	double firstDate = 0.0, lastDate = 0.0;
	double time_t = 0.0;
	double slantFirst = 0.0, slantRange = 0.0;
	double xPix = 0.0, origSamps = 0.0;
	double t_ref = 0.0,  r_ref = 0.0;
	double prf = 0.0;
	int    i,numCoef1 = 0, numCoef2 = 0;

	FILE *fptr;
	struct AISP_PARAMS aisp;	/* aisp structure to load .in file */
	meta_parameters *meta;                                                       

	logflag=quietflag=0;

/* Usage, shown if user does not give 3 or 4 arguments */
	if(argc < 3)
	{
		printf("\nUsage: \n");
		printf(" fix_in_fromraw <infile.par> <infile.meta> <in/outfile.in>"); 
		printf(" [# samples] [-log <file>] [-quiet]\n\n");
		printf("   <infile.par> is the input parameter file with the .par extension \n");
		printf("   <infile.meta> is the input meta file with the .meta extension \n");
		printf("   <in/outfile.in> is the .in file with the .in extension \n");
		printf("   [# Samples] is the number of range samples to be used in calculating\n");
		printf("    the doppler quadratic, default = 100\n");
		printf("   [-log <file>] allows the ouput to be written to a log file.\n");
		printf("   [-quiet] suppresses the output to the essential\n\n");
		printf("\nThis program modifies a .in file so that the doppler quadratic\n");
		printf("is given correctly.\n");
		printf("Version %.2f, ASF SAR Tools\n\n", VERSION);
		exit(1);
	}
        for (i=4; i<argc; i++) {
          if(i==4 && (!sscanf(argv[4], "%li", &nSamples))) {
	    sprintf(errbuf,"   ERROR: '%s' is not a number of samples\n", argv[4]);
	    printErr(errbuf);
	  }
          else if(strncmp(argv[i],"-log", 4)==0) {
            sscanf(argv[i+1], "%s", logFile);
            logflag=1;
            fLog = FOPEN(logFile, "a");
          }
	  else if(strncmp(argv[i],"-quiet", 6)==0) quietflag=1;
        }

/* Get file names from the command line. */
	strcpy(parFile,argv[1]);
	strcpy(metaFile,argv[2]);
	strcpy(dotInFile,argv[3]);

/* Allocate memory for sample & doppler vectors (nSamples)*/
	dopVec = (float *)MALLOC(sizeof(float)*nSamples);
	sampVec = (float *)MALLOC(sizeof(float)*nSamples);

	for (ii=0; ii<3; ii++)
	  for (jj=0; jj<4; jj++)
	     aVal[ii][jj] = 0;

/* read meta file */

        meta = meta_init(metaFile);

        slantFirst = meta->geo->slantFirst;
        xPix = meta->geo->xPix;
        origSamps = meta->ifm->orig_nSamples;

/* begin reading par file */
/* get first date from par file */
	fptr = FOPEN(parFile, "r");
	while( firstDate == 0.0 && NULL != fgets(line,255,fptr) )
	{
		sscanf(line, "%s",param);
		if(strncmp(param,"first_date",10)==0)
		{
			sscanf(line, "%s %s", param, Date);
			parse_ymdTime(Date, &date1, &time1);
			date_ymd2jd(&date1, &jDate);
			firstDate = date_hms2sec(&time1) + 
				(double)(date_getMJD(&jDate) * 3600 * 24);
		}
	}

/* Get last date from par file */
	FSEEK64(fptr,0,SEEK_SET);
	while( lastDate == 0.0 && NULL != fgets(line,255,fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "last_date", 9) == 0)
		{
			sscanf(line, "%s %s", param, Date);
			parse_ymdTime(Date, &date1, &time1);
			date_ymd2jd(&date1, &jDate);
			lastDate = date_hms2sec(&time1) +
				(double)(date_getMJD(&jDate) * 3600 * 24);
		}
	}

/* Calculate center time 't' for doppler */
	time_t = ((lastDate - firstDate)/2) + firstDate;

/* get t_ref (reference_second_dimension) from par file */
	FSEEK64(fptr,0,SEEK_SET);
	while( t_ref == 0.0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "reference_second_dimension", 26) == 0)
			sscanf(line, "%s %lf", param, &t_ref);
	}

/* get r_ref (reference_first_dimension) from par file */
	FSEEK64(fptr,0,SEEK_SET);
	while( r_ref == 0.0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "reference_first_dimension", 25) == 0)
			sscanf(line, "%s %lf", param, &r_ref);
	}


/* get number of coef in first dimension */
	FSEEK64(fptr,0,SEEK_SET);
	while( numCoef1 == 0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "number_of_coefficients_first_dimension", 38) == 0)
			sscanf(line, "%s %i", param, &numCoef1);
	}
	
/* get number of coef in second dimension */
	FSEEK64(fptr,0,SEEK_SET);
	while( numCoef2 == 0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "number_of_coefficients_second_dimension", 39) == 0)
			sscanf(line, "%s %i", param, &numCoef2);
	}

/* locate a values for a coefs in par file and load them. */
	FSEEK64(fptr, 0, SEEK_SET);
	aVal[0][0] = 0.0;
	while(aVal[0][0] == 0.0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "a00", 3) == 0)
		{
			for(ii = 0; ii < numCoef2 ; ii++)
				for(jj = 0; jj < numCoef1; jj++)
				{
					sscanf(line, "%s %lf",
						param, &aVal[ii][jj]);
					fgets(line, 255, fptr);
				}
		}		
	}

/* Obtain prf from par file */
	FSEEK64(fptr, 0, SEEK_SET);
	while(prf == 0.0 && NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "PRF", 3) == 0)
			sscanf(line, "%s %lf", param, &prf);
	}
/* closing par file */
	FCLOSE(fptr);

/* calculate slant range from near to far, with nSamples along the range */
	for(kk = 0; kk < nSamples; kk++)
	{
		sampVec[kk] = currSamp;
		slantRange = slantFirst + (currSamp * xPix);
		dopVec[kk] = 0.0;
		for(ii = 0; ii < numCoef2; ii++)
		{
			for(jj = 0; jj < numCoef1; jj++)
			{
				dopVec[kk] += aVal[ii][jj]
					     * pow( (time_t - t_ref), ii)
					     * pow( (slantRange - r_ref), jj);
			}
		} 
		currSamp += (origSamps/nSamples);
	}

/* Quadratic Regression. */	
	yax2bxc(sampVec, dopVec, nSamples, &t1, &t2, &t3);
	/* printf(" Quadratic Regression: y = %e x^2 + %f x +  %f\n", t1, t2, t3); */

/* read .in file into aisp strucute update the doppler polynomial and write it back out. */ 
	read_params(dotInFile, &aisp);
	aisp.fd = t3/prf;
	aisp.fdd = t2/prf;
	aisp.fddd = t1/prf;

        system("date");
        printf("Program: fix_in_fromraw\n\n");
	printf("   fd: %f , fdd: %f, fddd: %e \n\n", aisp.fd, aisp.fdd, aisp.fddd);
	if (logflag) {
	  StartWatchLog(fLog);
          printLog("Program: fix_in_fromraw\n\n");
  	  sprintf(logbuf,"   fd: %f , fdd: %f, fddd: %e \n\n", aisp.fd, aisp.fdd, aisp.fddd);
	  printLog(logbuf);
	}
	print_params(dotInFile, &aisp, "fix_in_fromraw");
}
