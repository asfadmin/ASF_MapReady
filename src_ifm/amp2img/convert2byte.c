/****************************************************************
NAME: convert2byte

SYNOPSIS: convert2byte [-look lxs] [-step lxs] [-log <log file>] [-quiet]
                       <infile> <outfile>

DESCRIPTION:
	Convert any ASF image into a viewable byte image.  Allows for
	specification of file size as well as a "look area" in which to
	generate a byte image.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    This program is a merging of amp2img and ui2byte. It converts any ASF image
    file to an image file made up of byte data (excluding complex data).
    Amp2img was a compilation of two programs written by Shusun Li and Tom
    Logan. It fulfilled the original purpose of Tom's with the added flexibility
    of Shusun's. Ui2byte created byte versions of RAMP (AMM-1) imagery.

    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     2/03     P. Denny     Merge amp2img and ui2byte.

HARDWARE/SOFTWARE LIMITATIONS:
	None known

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS: None known

*****************************************************************************
*								            *
*   Converts any non-complex data type image to a byte image                *
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
#include "asf_meta.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* For floating point comparisons */
#define MICRON 0.0000001
#define FLOAT_EQUIVALENT(a, b) (abs(a - b) < MICRON ? 1 : 0)

#define WINDOW_SIZE_MULTIPLIER 1

#define VERSION 1.0

int main(int argc, char **argv)
{
	char inFileName[256];
	char outFileName[256];
	char inMetaFileName[256];
	char outMetaFileName[256];
	int usingDefaultLS=1;      /* Has user overridden look line & sample? */
	int multilook=FALSE;
	int stepLine=5, stepSample=1;
	int lookLine=5, lookSample=1;
	int percent_complete;
	int line, sample;
	int num_lines, num_samples;
	double slope, offset;
	double *inBuffer;
	double *outBuffer;
	extern int currArg;        /* Pre-initialized to 1                    */
	meta_parameters *inMeta;
	meta_parameters *outMeta;
	FILE *inFilePtr;
	FILE *outFilePtr;

/* parse command line */
	logflag=quietflag=FALSE;
	while (currArg < (argc-2)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-multilook")) {
			multilook=TRUE;
		}
		else if (strmatch(key,"-look")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&lookLine,&lookSample)) {
				printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
				usage(argv[0]);
			}
			multilook=TRUE;
       			usingDefaultLS=FALSE;
		}
		else if (strmatch(key,"-step")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&stepLine,&stepSample)) {
				printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
				usage(argv[0]);
			}
			multilook=TRUE;
			usingDefaultLS=FALSE;
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag=TRUE;
		}
		else if (strmatch(key,"-quiet")) {
			quietflag=TRUE;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	strcpy(inFileName, argv[currArg++]);
	create_name(inMetaFileName,inFileName,".meta");

	strcpy(outFileName,argv[currArg]);
	create_name(outMetaFileName,outFileName,".meta");

/* Get metadata */
	inMeta = meta_read(inMetaFileName);
	if (inMeta->general->data_type==BYTE) {
		printf("Data type is already byte... Exitting.\n");
		return 0;
	}
	num_lines = inMeta->general->line_count;
	num_samples = inMeta->general->sample_count;

	outMeta = meta_copy(inMeta);
	outMeta->general->data_type = BYTE;

	if (usingDefaultLS) {
		/* We don't want to multilook any image twice */
		if (inMeta->sar->line_increment==inMeta->sar->look_count)
			stepLine=stepSample=lookLine=lookSample=1;
		else
			stepLine = inMeta->sar->look_count;
			stepSample = 1;
			lookLine = WINDOW_SIZE_MULTIPLIER * stepLine;
			lookSample = WINDOW_SIZE_MULTIPLIER * stepSample;
	}

/* Get statistics for input data (all this quiet and log flag business makes it
 * awfully messy, but hey whachya gonna do? */
	if (!inMeta->stats) {
		char command[1024];
		
		if (!quietflag)
			printf(" Theres is no statistics structure in the meta file.\n"
			       " To fix this the stats program will be run...\n");
		if (!quietflag && logflag) 
			fprintf(fLog,
			       " Theres is no statistics structure in the meta file.\n"
			       " To fix this the stats program will be run...\n");

		sprintf(command,"stats -nostat");
		if (quietflag) sprintf(command,"%s -quiet",command);
		if (logflag)   sprintf(command,"%s -log %s",command,logFile);
		sprintf(command,"%s %s",command,inFileName);

		if (!quietflag)
			printf(" Running command line:  %s\n",command);
		if (!quietflag && logflag)
			fprintf(fLog," Running command line:  %s\n",command);
		system(command);

		meta_free(inMeta);
		inMeta = meta_read(inMetaFileName);
		num_lines = inMeta->general->line_count;
		num_samples = inMeta->general->sample_count;
		meta_free(outMeta);
		outMeta = meta_copy(inMeta);
		outMeta->general->data_type = BYTE;
	}
	
	if (inMeta->stats->mask == inMeta->stats->mask) {
		char command[1024];
		
		if (!quietflag) 
			printf(" It appears that a mask was used in prior statisticas calculations\n"
			       " This program needs statistics without a mask, let's rectify that...\n"
			       " Moving %s to %s.old...\n",
				inMetaFileName, inMetaFileName);
		if (!quietflag && logflag) 
			fprintf(fLog,
			       " It appears that a mask was used in prior statisticas calculations\n"
			       " This program needs statistics without a mask, let's rectify that...\n"
			       " Moving %s to %s.old...\n",
				inMetaFileName, inMetaFileName);
		sprintf(command,"mv %s %s.old", inMetaFileName, inMetaFileName);
		system(command);

		sprintf(command,"stats -nostat");
		if (quietflag) sprintf(command,"%s -quiet",command);
		if (logflag)   sprintf(command,"%s -log %s",command,logFile);
		sprintf(command,"%s %s",command,inFileName);

		if (!quietflag)
			printf(" Running command line:  %s\n",command);
		if (!quietflag && logflag)
			fprintf(fLog," Running command line:  %s\n",command);
		system(command);

		meta_free(inMeta);
		inMeta = meta_read(inMetaFileName);
		num_lines = inMeta->general->line_count;
		num_samples = inMeta->general->sample_count;
		meta_free(outMeta);
		outMeta = meta_copy(inMeta);
		outMeta->general->data_type = BYTE;
	}

/* Read data and convert it to byte data */
	inFilePtr = fopenImage(inFileName, "r");
	inBuffer = (double *) MALLOC(num_samples*sizeof(double));
	outFilePtr = FOPEN(outFileName, "w");
	outBuffer = (double *) MALLOC(num_samples*sizeof(double));
	/* Factors to convert pixels fo [0..255]
	 * byte = slope * in + offset
	 * 0    = slope * min + offset
	 * 255  = slope * max + offset
	 * Therefore: */
	slope = 255.0 / (inMeta->stats->max - inMeta->stats->min);
	offset = -slope * inMeta->stats->min;

	percent_complete=0;
	for (line=0; line<num_lines; line++) {
		if (!quietflag && (line*100/num_lines==percent_complete)) {
			printf("\rConverting data to byte: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(inFilePtr, inMeta, line, inBuffer);
		for (sample=0; sample<num_samples; sample++) {
			outBuffer[sample] = slope*inBuffer[sample] + offset;
		}
		put_double_line(outFilePtr, outMeta, line, outBuffer);
	}
	if (!quietflag) printf("\rConverting data to byte: 100%% complete.\n\n");
	
	meta_write(outMeta,outMetaFileName);

	FREE(inBuffer);
	FREE(outBuffer);
	FCLOSE(inFilePtr);
	FCLOSE(outFilePtr);

	return 0;
}


void usage(char *name) {
 printf("\n"
	"USAGE:\n"
	"   %s [-look lxs] [-step lxs] [-log log_file] [-quiet]\n"
	"                <infile> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   infile   is an image file (WITH extension)\n"
	"              accompanied by a .meta file.\n"
	"   outfile  is the image file to be created, .img and .meta\n"
	"              (Do not include an extension on your outfile)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -look lxs      change number of look lines and samples (default = 5x1).\n"
	"   -step lxs      change number of step lines and samples (default = 5x1).\n"
	"   -log log_file  allows the output to be written to a log file\n"
	"   -quiet         Suppress terminal output.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts a float .amp file into a byte image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
