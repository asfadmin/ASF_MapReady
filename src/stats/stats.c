/****************************************************************
NAME: stats

SYNOPSIS: stats [-mask <value>] [-log <logFile>] [-quiet]
                [-overmeta] [-nometa] [-overstat] [-nostat]
                [-startline <line>] [-startsample <sample>]
                [-width <width>] [-height <height>] <sar_name>

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
    1.0     3/2003   P. Denny     Put sar image file statistics into
                                   meta file and stats file
    1.1     4/2003   B. Kerin     Added windowing functionality.
    1.2     5/2003   P. Denny     Fixed windowing bug,
                                    wrote stat_write and stat_read

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
#include "stats.h"

#define VERSION 1.2

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
	double slope=1.0;            /* Slope of line to convert real to byte */
	double offset=0.0;           /* Offset of line to convert real to byte*/
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
	stat_parameters stats;       /* Statistics structure                  */
	char stat_name[261];         /* Stats file name                       */
	extern int currArg;          /* Pre-initialized to 1                  */

	/* We initialize these to a magic number for checking. */
	int start_line = -1;         /* Window starting line.                 */
	int start_sample = -1;       /* Window starting sample.               */
	int window_height = -1;      /* Window height in lines.               */
	int window_width = -1;       /* Window width in samples.              */

/* parse command line */
	logflag=quietflag=FALSE;
	while (currArg < (argc-1)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-quiet")) {
			quietflag=TRUE;
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag=TRUE;
		}
		else if (strmatch(key,"-mask")) {
			CHECK_ARG(1);
			mask = atof(GET_ARG(1));
			mask_flag=TRUE;
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
		else if (strmatch(key,"-startline")) {
			CHECK_ARG(1);
			nometa_flag=TRUE; /* Implied.  */
			start_line = atol(GET_ARG(1));
			if ( start_line < 0 ) {
			  printf("error: -startline argument must be greater than or equal to zero\n");
			  usage(argv[0]);
			}
		}
		else if (strmatch(key,"-startsample")) {
			CHECK_ARG(1);
			nometa_flag=TRUE; /* Implied.  */
			start_sample = atol(GET_ARG(1));
			if ( start_sample < 0 ) {
			  printf("error: -startsample argument must be greater than or equal to zero\n");
			  usage(argv[0]);
			}
		}
		else if (strmatch(key,"-width")) {
			CHECK_ARG(1);
			nometa_flag=TRUE; /* Implied.  */
			window_width = atol(GET_ARG(1));
			if ( window_width < 0 ) {
			  printf("error: -width argument must be greater than or equal to zero\n");
			  usage(argv[0]);
			}
		}
		else if (strmatch(key,"-height")) {
			CHECK_ARG(1);
			nometa_flag=TRUE; /* Implied.  */
			window_height = atol(GET_ARG(1));
			if ( window_height < 0 ) {
			  printf("error: -height argument must be greater than or equal to zero\n");
			  usage(argv[0]);
			}
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}

	if ((argc-currArg)<1) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy (sar_name, argv[currArg]);
	create_name(meta_name, sar_name, ".meta");
	create_name(stat_name, sar_name, ".stat");

	StartWatch();
	printf("Date: ");
	fflush(NULL);
	system("date");
	printf("Program: stats\n\n");
	if (logflag) {
		StartWatchLog(fLog);
		fprintf(fLog, "Program: stats\n\n");
	}

	sar_file = FOPEN(sar_name, "r");
	meta = meta_read(meta_name);
	num_lines = meta->general->line_count;
	num_samples = meta->general->sample_count;

	if ( start_line == -1 ) start_line = 0;
	if ( start_line > num_lines ) {
	  printf("error: -startline argument is larger than index of last line in image\n"); 
	  exit(EXIT_FAILURE);
	}
	if ( start_sample == -1 ) start_sample = 0;
	if ( start_sample > num_samples ) {
	  printf("error: -startsample argument is larger than index of last sample in image\n"); 
	  exit(EXIT_FAILURE);
	}
	if ( window_height == -1 ) window_height = num_lines;
	if ( start_line + window_height > num_lines ) {
	  printf("warning: window specified with -startline, -height options doesn't fit in image\n");
	}
	if ( window_width == -1 ) window_width = num_samples;
	if ( start_sample + window_width > num_samples ) {
	  printf("warning: window specified with -startsample, -width options doesn't fit in image\n");
	}

/* Make sure we don't over write any files that we don't want to */
	if (meta->stats && !overmeta_flag && !nometa_flag) {
		printf(" ** The meta file already has a populated statistics structure.\n"
		       " ** If you want to run this program and replace that structure,\n"
		       " ** then use the -overmeta option to do so. If you want to run\n"
		       " ** this program, but don't want to replace the structure, use\n"
		       " ** the -nometa option.\n");
		if (logflag) {
			fprintf(fLog,
			" ** The meta file already has a populated statistics structure.\n"
			" ** If you want to run this program and replace that structure,\n"
			" ** then use the -overmeta option to do so. If you want to run\n"
			" ** this program, but don't want to replace the structure, use\n"
			" ** the -nometa option.\n");
		}
		exit(EXIT_FAILURE);
	}
 	if (fileExists(stat_name) && !overstat_flag && !nostat_flag) {
		printf(" ** The file, %s, already exists. If you want to\n"
		       " ** overwrite it, then use the -overstat option to do so.\n"
		       " ** If you want to run the progam but don't want to write\n"
		       " ** over the current file, then use the -nostat option.\n",
		       stat_name);
		if (logflag) {
			fprintf(fLog,
			" ** The file, %s, already exists. If you want to\n"
			" ** overwrite it, then use the -overstat option to do so.\n"
			" ** If you want to run the progam but don't want to write\n"
			" ** over the current file, then use the -nostat option.\n",
			stat_name);
		}
		exit(EXIT_FAILURE);
	}

/* Let user know the window in which the stats will be taken */
	if ((start_line!=0) || (start_sample!=0)
	    || (window_height!=num_lines) || (window_width!=num_samples)) {
	    	if (!quietflag) {
		  printf("Taking statistics on a window with upper right corner (%d,%d)\n"
			"  and lower left corner (%d,%d)\n",
			start_sample, start_line,
			window_width+start_sample, window_height+start_line);
		}
		if (logflag && !quietflag) {
		  fprintf(fLog,
		  	"Taking statistics on a window with upper right corner (%d,%d)\n"
			"  and lower left corner (%d,%d)\n",
			start_sample, start_line,
			window_width+start_sample, window_height+start_line);
		}
		
	}

/* Allocate line buffer */
 	data_line = (double *)MALLOC(sizeof(double)*num_samples); 

/* Find min, max, and mean values */
	if (!quietflag) printf("\n");
	if (logflag && !quietflag) fprintf(fLog,"\n");
	min = 100000000;
	max = -100000000;
	sum_of_samples=0.0;
	sum_of_squared_samples=0.0;
	percent_complete=0;
	for (line=start_line; line<start_line+window_height-1; line++) {
		if (((line-start_line)*100/window_height==percent_complete)
                    && !quietflag) {
			printf("\rFirst data sweep: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(sar_file, meta, line, data_line);
		for (sample=start_sample; sample<start_sample+window_width-1; 
		     sample++) {
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
	for (ii=0; ii<256; ii++) stats.histogram[ii] = 0;

	if (meta->general->data_type != BYTE) {
		/* Set slope and offset, to map pixels to [0..255].
		 * byte = slope * in + offset
		 * 0    = slope * min + offset
		 * 255  = slope * max + offset
		 * Therefore: */
		slope = 255.0 / (max-min);
		offset = -slope * min;
	}

	/* Get histogram of the data.  If its byte data just slap it into the
	 * histogram; otherwise use slope & offset to scale the data */
	diff_squared_sum=0.0;
	mean = sum_of_samples / (double)samples_counted;
	percent_complete=0;
	for (line=start_line; line<start_line+window_height; line++) {
		if (((line-start_line)*100/window_height==percent_complete)
                    && !quietflag) {
			printf("\rSecond data sweep: %3d%% complete.",
				percent_complete++);
			fflush(NULL);
		}
		get_double_line(sar_file, meta, line, data_line);
		for (sample=start_sample; sample<start_sample+window_width;
                     sample++) {
			int bin = (meta->general->data_type == BYTE)
				? (int)data_line[sample]
				: (int)(slope*data_line[sample]+offset);
			if ( mask_flag && FLOAT_EQUIVALENT(data_line[sample],mask) )
				continue;
			stats.histogram[bin]++;
			diff_squared_sum = SQR(data_line[sample] - mean);
		}
	}
	if (!quietflag) printf("\rSecond data sweep: 100%% complete.\n");
	FREE(data_line);
	FCLOSE(sar_file);

/* Populate stats structure */
	stats.min = min;
	stats.max = max;
	stats.mean = mean;
	stats.rmse =
		sqrt( fabs( diff_squared_sum / (double)samples_counted ) );
	stats.std_deviation =
		sqrt( fabs( diff_squared_sum / (double)(samples_counted-1) ) );
	stats.mask = mask;
	stats.slope = slope;
	stats.offset = offset;
	stats.upper_right_line = start_line;
	stats.upper_right_samp = start_sample;
	stats.lower_left_line = start_line + window_height;
	stats.lower_left_samp = start_sample + window_width;

/* Populate meta->stats structure */
	if (!meta->stats)
		meta->stats = meta_stats_init();
	meta->stats->min = stats.min;
	meta->stats->max = stats.max;
	meta->stats->mean = stats.mean;
	meta->stats->rmse = stats.rmse;
	meta->stats->std_deviation = stats.std_deviation;
	meta->stats->mask = stats.mask;

/* Print findings to the screen (and log file if applicable)*/
	if (!quietflag) {
		printf("\n");
		printf("Statistics found:\n");
		if (mask_flag)
			{ printf("Used mask %-16.11g\n",mask); }
		printf("Minimum = %-16.11g\n",stats.min);
		printf("Maximum = %-16.11g\n",stats.max);
		printf("Mean = %-16.11g\n",stats.mean);
		printf("Root mean squared error = %-16.11g\n",
			stats.rmse);
		printf("Standard deviation = %-16.11g\n",
			stats.std_deviation);
		printf("\n");
		printf("Data fit to [0..255] using equation:  byte = %g * sample + %g\n",
			stats.slope, stats.offset);
		printf("\n");
		printf("Histogram:\n");
		for (ii=0; ii<256; ii++) {
			if (ii%8 == 0) {
				printf("%s%3i-%3i:",
					(ii==0) ? "" : "\n",
					ii, ii+7);
			}
			printf(" %8i", stats.histogram[ii]);
		}
		printf("\n");
	}
	if (logflag && !quietflag) {
		fprintf(fLog,"Statistics found:\n");
		if (mask_flag)
			{ fprintf(fLog,"Used mask %-16.11g\n",mask); }
		fprintf(fLog,"Minimum = %-16.11g\n",stats.min);
		fprintf(fLog,"Maximum = %-16.11g\n",stats.max);
		fprintf(fLog,"Mean = %-16.11g\n",stats.mean);
		fprintf(fLog,"Root mean squared error = %-16.11g\n",
			stats.rmse);
		fprintf(fLog,"Standard deviation = %-16.11g\n",
			stats.std_deviation);
		fprintf(fLog,"\n");
		fprintf(fLog,
			"Data fit to [0..255] using equation:  byte = %g * sample + %g\n",
			stats.slope, stats.offset);
		fprintf(fLog,"\n");
		fprintf(fLog,"Histogram:\n");
		for (ii=0; ii<256; ii++) {
			if (ii%8 == 0) {
				fprintf(fLog,"%s%3i-%3i:",
					(ii==0) ? "" : "\n",
					ii, ii+7);
			}
			fprintf(fLog," %8i", stats.histogram[ii]);
		}
		fprintf(fLog,"\n");
	}

/* Write out .meta and .stat files */
	if (!nometa_flag) meta_write(meta, meta_name);
	meta_free(meta);
	if (!nostat_flag) stat_write(&stats, stat_name);

/* Report */
	if (!quietflag) {
		printf("\n");
		printf("Statistics taken on image file %s.\n",sar_name);
		if (!nometa_flag)
			printf("Statistics written to the stats block in %s.\n",
				meta_name);
		if (!nostat_flag)
			printf("Statistics plus histogram written to %s.\n",
				stat_name);
		printf("\n");
	}
	if (logflag && !quietflag) {
		fprintf(fLog,"\n");
		fprintf(fLog,"Statistics taken on image file '%s'\n",sar_name);
		if (!nometa_flag)
			fprintf(fLog,"Statistics written to the stats block in %s\n",
				meta_name);
		if (!nostat_flag)
			fprintf(fLog,"Statistics plus histogram written to %s\n",
				stat_name);
		fprintf(fLog,"\n");
	}

	StopWatch();
	if (logflag) StopWatchLog(fLog);
	if (fLog) FCLOSE(fLog);
	return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-mask <value>] [-log <logFile>] [-quiet]\n"
	"         [-overmeta] [-nometa] [-overstat] [-nostat]\n"
	"         [-startline <line>] [-startsample <sample>]\n"
	"         [-width <width>] [-height <height>] <sar_name>\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   sar_name   Name of input image file (including extension)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -mask         Value to ignore while taking statistics.\n"
	"   -log          Copy terminal output to <logFile>.\n"
	"   -quiet        Supress terminal output.\n"
	"   -overmeta     Force overwrite of existing .meta file.\n"
	"   -nometa       Do not write a .meta file.\n"
	"   -overstat     Force overwrite of existing .stat file.\n"
	"   -nostat       Do not write a .stat file.\n"
	"   -startline    Start counting at image line <line> (implies -nometa).\n"
	"   -startsample  Start counting at image sample <sample> (implies -nometa).\n"
	"   -height       Only process <height> lines (implies -nometa).\n"
	"   -width        Only process <width> samples per line (implies -nometa).\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program takes statistics on a SAR data file and writes them\n"
	"   with a histogram out to a .stat file. It also inserts a stats block\n"
	"   in the .meta file.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
