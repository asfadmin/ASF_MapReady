/****************************************************************
NAME: convert2byte

SYNOPSIS: convert2byte [-multilook] [-look lxs] [-step lxs] [-log <log file>]
                       [-quiet] <infile> <outfile>

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
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"

/* For floating point comparisons */
#define MICRON 0.0000001
#define FLOAT_EQUIVALENT(a, b) (abs(a - b) < MICRON ? 1 : 0)

#define WINDOW_SIZE_MULTIPLIER 1

#define VERSION 1.0

/* PROTOTYPES */
void linear_conversion(FILE *fpin, FILE *fpout, meta_parameters *inMeta,
                       meta_parameters *outMeta);
void multilook(FILE *fpin, FILE *fpout,
               meta_parameters *inMeta, meta_parameters *outMeta,
               int lookLine, int lookSample,
               int stepLine, int stepSample);


int main(int argc, char **argv)
{
	char inFileName[256];           /* Input data file name               */
	char inMetaFileName[256];       /* Input metadata file name           */
	char outFileName[256];          /* Output data file name              */
	char outMetaFileName[256];      /* Output metadata file name          */
	int defaultLookStep_flag=TRUE;  /* Use meta look line & sample?       */
	int multilook_flag=FALSE;       /* Multilook the data or not          */
	int stepLine=-1, stepSample=-1; /* Step line/samp for multilooking    */
	int lookLine=-1, lookSample=-1; /* Look line/samp for multilooking    */
	meta_parameters *inMeta;        /* Input metadata structure pointer   */
	meta_parameters *outMeta;       /* Output metadata structure pointer  */
	FILE *inFilePtr;                /* File pointer for input data        */
	FILE *outFilePtr;               /* File pointer for output data       */
	extern int currArg;             /* Pre-initialized to 1               */

/* parse command line */
	logflag=quietflag=FALSE;
	while (currArg < (argc-2)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-multilook")) {
			multilook_flag=TRUE;
		}
		else if (strmatch(key,"-look")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&lookLine,&lookSample)) {
				printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
				usage(argv[0]);
			}
			if (lookLine<1 || lookSample<1) {
				printf("You can't have an look area of less than 1!\n");
				usage(argv[0]);
			}
			if (stepLine==-1 || stepSample==-1) {
				stepLine = lookLine;
				stepSample = lookSample;
			}
			multilook_flag=TRUE;
       			defaultLookStep_flag=FALSE;
		}
		else if (strmatch(key,"-step")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&stepLine,&stepSample)) {
				printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
				usage(argv[0]);
			}
			if (stepLine<1 || stepSample<1) {
				printf("You can't step less than 1 line or sample!\n");
				usage(argv[0]);
			}
			if (lookLine==-1 || lookSample==-1) {
				lookLine = stepLine;
				lookSample = stepSample;
			}
			multilook_flag=TRUE;
			defaultLookStep_flag=FALSE;
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
	if (!findExt(outFileName))
		strncat(outFileName,".img",256);
	create_name(outMetaFileName,outFileName,".meta");

	system("date");
	printf("Program: convert2byte\n\n");
	if (logflag) {
		StartWatchLog(fLog);
		fprintf(fLog, "Program: convert2byte\n\n");
		fflush(fLog);
	}

/* Get metadata */
	inMeta = meta_read(inMetaFileName);
	if (inMeta->general->data_type==BYTE) {
		printf("Data type is already byte... Exiting.\n");
		if (logflag) {
			fprintf(fLog,"Data type is already byte... Exiting.\n");
			FCLOSE(fLog);
		}
		return 0;
	}

/* Get statistics for input data (all this -quiet and -log flag business makes
 * it awfully messy, but hey, whachya gonna do? */
	if (!inMeta->stats) {
		char command[1024];
		
		if (!quietflag)
			printf(" There is no statistics structure in the meta file.\n"
			       " To fix this the stats program will be run...\n");
		if (!quietflag && logflag) {
			fprintf(fLog,
			       " There is no statistics structure in the meta file.\n"
			       " To fix this the stats program will be run...\n");
			fflush(fLog);
		}
		sprintf(command,"stats -nostat");
		if (quietflag) sprintf(command,"%s -quiet",command);
		if (logflag)   sprintf(command,"%s -log %s",command,logFile);
		sprintf(command,"%s %s",command,inFileName);

		if (!quietflag)
			printf(" Running command line:  %s\n",command);
		if (!quietflag && logflag) {
			fprintf(fLog," Running command line:  %s\n",command);
			fflush(fLog);
		}
		system(command);

		meta_free(inMeta);
		inMeta = meta_read(inMetaFileName);
	}

	/* if a mask was used in prior stats, move them, and get new stats */
	if (inMeta->stats->mask == inMeta->stats->mask) {
		char command[1024];
		
		if (!quietflag) 
			printf(" It appears that a mask was used in prior statisticas calculations\n"
			       " This program needs statistics without a mask, let's rectify that...\n"
			       " Moving %s to %s.old...\n",
				inMetaFileName, inMetaFileName);
		if (!quietflag && logflag) {
			fprintf(fLog,
			       " It appears that a mask was used in prior statisticas calculations\n"
			       " This program needs statistics without a mask, let's rectify that...\n"
			       " Moving %s to %s.old...\n",
				inMetaFileName, inMetaFileName);
			fflush(fLog);
		}
		sprintf(command,"mv %s %s.old", inMetaFileName, inMetaFileName);
		system(command);

		sprintf(command,"stats -nostat");
		if (quietflag) sprintf(command,"%s -quiet",command);
		if (logflag)   sprintf(command,"%s -log %s",command,logFile);
		sprintf(command,"%s %s",command,inFileName);

		if (!quietflag)
			printf(" Running command line:  %s\n",command);
		if (!quietflag && logflag) {
			fprintf(fLog," Running command line:  %s\n",command);
			fflush(fLog);
		}
		system(command);

		meta_free(inMeta);
		inMeta = meta_read(inMetaFileName);
	}

/* Prepare output meta data for processing & writing */
	outMeta = meta_copy(inMeta);
	outMeta->general->data_type = BYTE;

/* Figure multilooking parameters if necessary */
	if (multilook_flag) {
	    if (defaultLookStep_flag) {
		/* We don't want to multilook any image twice */
		if (inMeta->sar->line_increment==inMeta->sar->look_count) {
		    stepLine = stepSample = lookLine = lookSample = 1;
		}
		else {
		    if (inMeta->sar->look_count == MAGIC_UNSET_INT) {
			printf( "Bad look_count value in %s. Unable to determine multilooking values.\n"
				"You can use the -look option to set the number of looks yourself. Exiting.\n" ,
				inMetaFileName);
			if (logflag) {
			    fprintf( fLog,
				"Bad look_count value in %s. Unable to determine multilooking values.\n"
				"You can use the -look option to set the number of looks yourself. Exiting.\n" ,
				inMetaFileName);
			}
			exit(EXIT_FAILURE);
		    }
		    else
			stepLine = inMeta->sar->look_count;
		    stepSample = 1;
		    lookLine = WINDOW_SIZE_MULTIPLIER * stepLine;
		    lookSample = WINDOW_SIZE_MULTIPLIER * stepSample;
		}
	    }
	    outMeta->general->sample_count = 
		    inMeta->general->sample_count / stepSample;
	    outMeta->general->line_count = 
		    inMeta->general->line_count / stepLine;
	    outMeta->sar->line_increment *= stepLine;
	    outMeta->sar->sample_increment *= stepSample;
	    outMeta->general->x_pixel_size *= stepSample;
	    outMeta->general->y_pixel_size *= stepLine;
	}

/* Read data and convert it to byte data */
	inFilePtr = fopenImage(inFileName, "r");
	outFilePtr = FOPEN(outFileName, "w");
	if (multilook_flag) {
		multilook(inFilePtr, outFilePtr, inMeta, outMeta,
		          lookLine, lookSample, stepLine,stepSample);
	}
	else {
		linear_conversion(inFilePtr, outFilePtr, inMeta, outMeta);
	}

	FCLOSE(inFilePtr);
	FCLOSE(outFilePtr);

	meta_write(outMeta,outMetaFileName);

	if (fLog) FCLOSE(fLog);

	return 0;
}


void usage(char *name) {
 printf("\n"
	"USAGE:\n"
	"   %s [-multilook] [-look lxs] [-step lxs] [-log log_file] [-quiet]\n"
	"                <infile> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   infile   Image file to be read in(WITH extension)\n"
	"              accompanied by a .meta file.\n"
	"   outfile  Output image file which will be byte data.\n"
	"              (No extension necessary.)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -multilook     Multilook the data as it is converted to byte.\n"
	"   -look (l)x(s)  Change number of look lines (l) and samples (s).\n"
	"                    (-multilook option is implied.)\n"
	"   -step (l)x(s)  Change number of step lines (l) and samples (s).\n"
	"                    (-multilook option is implied.)\n"
	"   -log log_file  Allows the output to be written to a log file\n"
	"   -quiet         Suppress terminal output to essential.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts any ASF image into a byte image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
