/****************************************************************
NAME: stats

SYNOPSIS: stats [-mask <value>] [-log <logFile>] [-quiet]
                [-overmeta] [-nometa] [-overstat] [-nostat]
		<sar_name>

DESCRIPTION:
	Takes statistics on an image file and prints them out to
	a .stat file as well as inserting a stats structure in the
	.meta file.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:

    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     3/03     P. Denny     Put sar image file statistics into
                                   meta file and stats file

HARDWARE/SOFTWARE LIMITATIONS:
	None known

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS: None known

*****************************************************************************
*								            *
*   Figures statistics for a SAR image file and dumps them to file          *
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
*   ASF Advanced Product Development Lab Contacts:			    *
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
#include "asf_nan.h"

#define VERSION 1.0

/* For floating point comparisons.  */
#define MICRON 0.00000001
#define FLOAT_EQUIVALENT(a, b) (fabs(a - b) < MICRON ? 1 : 0)

#define SQR(X) ((X)*(X))

int main(int argc, char **argv)
{
	double min, max;             /* Minimum & maximum sample values       */
	double mean;                 /* Average value of samples              */
	double sum_of_samples=0.0;   /* Sum of all samples accounted for      */
	double sum_of_squared_samples=0.0; /* Sum of all squared samples accounted for*/
	double diff_squared_sum=0.0; /* Summation of square of (sample - mean)*/
	double slope;                /* Slope of line to convert real to byte */
	double offset;               /* Offset of line to convert real to byte*/
	int histogram[256];          /* 256 bin histogram of data             */
	int ii;                      /* Loop index                            */
	int samples_counted=0;       /* Number of all samples accounted for   */
	double *data_line;           /* Buffer for a line of samples          */
	int line, sample;            /* Line and sample indices               */
	int num_lines, num_samples;  /* Number of lines and samples           */
	int percent_complete=0;      /* Percent of data sweep completed       */
	int overmeta_flag=FALSE;     /* If TRUE write over current .meta file */
	int overstat_flag=FALSE;     /* If TRUE write over current .stat file */
	int nometa_flag=FALSE;       /* If TRUE do not write .meta file       */
	int nostat_flag=FALSE;       /* If TRUE do not write .stat file       */
	int mask_flag=FALSE;         /* TRUE if user specifies a mask value   */
	double mask=NAN;             /* Value to ignore while caculating stats*/
	char meta_name[261];         /* Meta file name                        */
	meta_parameters *meta;       /* SAR meta data structure               */
	char sar_name[256];          /* SAR file name WITH extention          */
	FILE *sar_file;              /* SAR data file pointer to take stats on*/
	FILE *stat_file;             /* File pointer in which to put stats    */
	char stat_name[261];         /* Stats file name                       */
	extern int currArg;          /* Pre-initialized to 1                  */

/* parse command line */
	logflag=quietflag=FALSE;
	while (currArg < (argc-1)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-mask")) {
			CHECK_ARG(1);
			mask = atof(GET_ARG(1));
			mask_flag=TRUE;
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
		else if (strmatch(key,"-overmeta")) {
			overmeta_flag=TRUE;
		}
		else if (strmatch(key,"-overstat")) {
			overstat_flag=TRUE;
		}
		else if (strmatch(key,"-nometa")) {
			nometa_flag=TRUE;
		}
		else if (strmatch(key,"-nostat")) {
			nostat_flag=TRUE;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg)<1) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy (sar_name, argv[currArg]);
	create_name(meta_name, sar_name, ".meta");
	create_name(stat_name, sar_name, ".stat");

	sar_file = FOPEN(sar_name, "r");
	meta = meta_read(meta_name);
	num_lines = meta->general->line_count;
	num_samples = meta->general->sample_count;

/* Make sure we don't over write any files that we don't want to */
	if (meta->stats && !overmeta_flag && !nometa_flag) {
		printf(" ** The meta file already has a populated statistics structure.\n"
		       " ** If you want to run this program and replace that structure,\n"
		       " ** then use the -overmeta option to do so.\n");
		if (logflag) {
			fprintf(fLog,
			" ** The meta file already has a populated statistics structure.\n"
			" ** If you want to run this program and replace that structure,\n"
			" ** then use the -overmeta option to do so.\n");
		}
		exit(EXIT_FAILURE);
	}
 	if (fileExists(stat_name) && !overstat_flag && !nostat_flag) {
		printf(" ** The file, %s, already exists. If you want to\n"
		       " ** overwrite it, then use the -overstat option to do so.\n",
		       stat_name);
		if (logflag) {
			fprintf(fLog,
			" ** The file, %s, already exists. If you want to\n"
			" ** overwrite it, then use the -overstat option to do so.\n",
			stat_name);
		}
		exit(EXIT_FAILURE);
	}

/* Allocate line buffer */
 	data_line = (double *)MALLOC(sizeof(double)*num_samples);

/* Find min, max, and mean values */
	if (!quietflag) printf("\n");
	min = 100000000;
	max = -100000000;
	sum_of_samples=0.0;
	sum_of_squared_samples=0.0;
	percent_complete=0;
	for (line=0; line<num_lines; line++) {
		if ((line*100/num_lines==percent_complete) && !quietflag) {
			printf("\rFirst data sweep: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(sar_file, meta, line, data_line);
		for (sample=0; sample<num_samples; sample++) {
			if ( mask_flag && FLOAT_EQUIVALENT(data_line[sample],mask) )
				continue;
			if (data_line[sample] < min) min=data_line[sample];
			if (data_line[sample] > max) max=data_line[sample];
			sum_of_samples += data_line[sample];
			sum_of_squared_samples += SQR(data_line[sample]);
			samples_counted++;
		}
	}
	if (!quietflag) printf("\rFirst data sweep: 100%% complete.\n");

/* Create histogram of the data and get the summation of the
 * square of (sample-mean) to use in calculation of rmse & stdev */
	/* Initialize the histogram array */
	for (ii=0; ii<256; ii++) histogram[ii] = 0;

	/* Set slope and offset, to map pixels to [0..255].
	 * byte = slope * in + offset
	 * 0    = slope * min + offset
	 * 255  = slope * max + offset
	 * Therefore: */
	slope = 255.0 / (max-min);
	offset = -slope * min;

	/* Get histogram of the data.  If its byte data just slap it into the
	 * histogram; otherwise use slope & offset to scale the data */
	diff_squared_sum=0.0;
	mean = sum_of_samples / (double)samples_counted;
	percent_complete=0;
	for (line=0; line<num_lines; line++) {
		if ((line*100/num_lines==percent_complete) && !quietflag) {
			printf("\rSecond data sweep: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(sar_file, meta, line, data_line);
		for (sample=0; sample<num_samples; sample++) {
			int bin = (meta->general->data_type == BYTE)
				? (int)data_line[sample]
				: (int)(slope*data_line[sample]+offset);
			if ( mask_flag && FLOAT_EQUIVALENT(data_line[sample],mask) )
				continue;
			histogram[bin]++;
			diff_squared_sum = SQR(data_line[sample] - mean);
		}
	}
	if (!quietflag) printf("\rSecond data sweep: 100%% complete.\n");
	FREE(data_line);
	FCLOSE(sar_file);

/* Populate meta stats structure for writing */
	if (!meta->stats)
		meta->stats = meta_stats_init();
	meta->stats->min = min;
	meta->stats->max = max;
	meta->stats->mean = mean;
	meta->stats->rmse = sqrt( fabs(
		diff_squared_sum / (double)samples_counted ) );
	meta->stats->std_deviation = sqrt( fabs(
		diff_squared_sum / (double)(samples_counted-1) ) );
	meta->stats->mask = mask;

/* Write out .meta and .stat files */
	if (!nometa_flag) meta_write(meta, meta_name);
	if (!nostat_flag) {
		stat_file = FOPEN(stat_name, "w");
		fprintf(stat_file, "# Statistics for image file \"%s\".\n",
			sar_name);
		fprintf(stat_file, "%-16.11g\t# minimum\n",meta->stats->min);
		fprintf(stat_file, "%-16.11g\t# maximum\n",meta->stats->max);
		fprintf(stat_file, "%-16.11g\t# mean\n",meta->stats->mean);
		fprintf(stat_file, "%-16.11g\t# root mean squared error\n",
			meta->stats->rmse);
		fprintf(stat_file, "%-16.11g\t# standard deviation\n",
			meta->stats->std_deviation);
		fprintf(stat_file,
			"%-16.11g\t# masked value (none if NaN)\n",mask);
		fprintf(stat_file, "\n");
		fprintf(stat_file, "# Histogram %s\n",
			(meta->general->data_type==BYTE)
			? "" 
			: "(data is scaled and offset to fill [0..255])");
		for (ii=0; ii<256; ii++) {
			fprintf(stat_file, "% 4i: % 9i\n",ii, histogram[ii]);
		}
		FCLOSE(stat_file);
	}
	meta_free(meta);

/* Report */
	if (!quietflag) {
		printf("\n");
		printf("Statistics taken on image file %s.\n",sar_name);
		if (!nometa_flag)
			printf("Results written to the stats block in %s.\n",
				meta_name);
		if (!nostat_flag)
			printf("Results plus histogram written to %s.\n",
				stat_name);
		printf("\n");
	}
	if (logflag && !quietflag) {
		printf("\n");
		fprintf(fLog,"Statistics taken on image file '%s'\n",sar_name);
		if (!nometa_flag)
			fprintf(fLog,"Results written to the stats block in %s\n",
				meta_name);
		if (!nostat_flag)
			fprintf(fLog,"Results plus histogram written to %s\n",
				stat_name);
		printf("\n");
	}

	if (fLog) FCLOSE(fLog);
	return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-mask <value>] [-log <logFile>] [-quiet]\n"
	"         [-overmeta] [-nometa] [-overstat] [-nostat]"
	"         <sar_name>\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   sar_name   Name of input image file (including extension)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -mask      Value to ignore while taking statistics.\n"
	"   -log       Copy terminal output to <logFile>.\n"
	"   -quiet     Supress terminal output.\n"
	"   -overmeta  Force overwrite of existing .meta file.\n"
	"   -nometa    Do not write out a .meta file.\n"
	"   -overstat  Force overwrite of existing .stat file.\n"
	"   -nostat    Do not write out a .stat file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program takes statistics on a SAR data file and writes them\n"
	"   out to a .stat file. It also inserts a stats block in the .meta\n"
	"   file.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
