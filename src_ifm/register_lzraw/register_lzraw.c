/*******************************************************************************NAME:
	register_lzraw.c

SYNOPSIS:  register_lzraw <file 1> <file 2> <igram_out>
                [-g <grid_resolution>]
                [-line <start_line1> <start_line2> <end_line1> <end_line2>]
                [-lat <start_lat> <end_lat>]
                [-p [<num_processors>]]
                [-f]
                [-ml]
                [-nolz2raw]
DESCRIPTION:

	This program processes, correlates, and registers 
	two non-correlated L0 swath images.

ASSOCIATE PROGRAMS:
	lz2raw_flywheel(1)
	swath_offset(1)
	fix_in_fromraw(1)
	avg_in_dop(1)
	water_mask(1)
	aisp(1)
	paisp(1)
	resolve(1)
	fico(1)
	register_cpx(1)
	fit_line(1)
	calc_deltas(1)
	igram(1)
	pigram(1)
	coh(1)
	las_op(1)

FILE REFERENCES:
	NAME:		USAGE:
	---------------------------------------------------------------------
	swath_one	L0 binary swath input.
	swath_two	L0 binary swath input.
	igram_out	output interferogram file.

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	1.0	???	???		Initial development from existing
					register_lzceos script v2.0
	2.0	6/01	Patrick Denny	Replace script with C code
					Added calculation of swath overlap
					heavily based on register_akdemSwath.c

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	There are no known bugs.

*****************************************************************************
*                                                                           *
*   register_lzraw							    *
*   Copyright (C) 2000  ASF APD LAB					    *
*                                                                           *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.                                     *
*                                                                           *
*   This program is distributed in the hope that it will be useful,         *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).                                  *
*                                                                           *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software             *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*                                                                           *
*   ASF Advanced Product Development LAB Contacts:                          *
*       E-mail: apd@asf.alaska.edu                                          *
*                                                                           *
*       Alaska SAR Facility                     APD Lab Web Site:	    *
*       Geophysical Institute                   www.images.alaska.edu       *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/

#include "asf.h"
#include "aisp_params.h"
#include "register_lzraw_inc.h"
#include "swath_bound.h"

#define VERSION 2.0

void show_usage(void)
{
	printf("register_lzraw:\n\n");
	printf(" Usage: register_lzraw <file 1> <file 2> <igram_out>\n");
	printf("\t\t[-g <grid resolution> ]\n ");
	printf("\t\t[-line <start_line1> <start_line2> <end_line1> <end_line2>]\n");
	printf("\t\t[-lat <start_lat> <end_lat> ]\n");
	printf("\t\t[-p [<num_processors>]]\n");
	printf("\t\t[-f ]\n\t\t[-ml]\n\t\t[-nolz2raw]\n\n");
	printf(" <file 1>    an LZP Swath file \n");
	printf(" <file 2>    an LZP Swath file \n");
	printf(" <igram_out> output name for produced interferogram (.amp, .phase, .ddr)\n");
	printf(" [-g <grid resolution>] is the grid resolution for fico(1).\n");
	printf("             (default is 20 for a 20x20 grid)\n");
	printf(" [-line <start line 1> <end line 1> <start line 2> <end line 2>]\n");
	printf("	     <start line 1> start line for image 1 \n");
	printf("	     <end line 1>   end line for image 1\n");
	printf("	     <start line 2> start line for image 2\n");
	printf("	     <end line 2>   end line for image 2\n");
	printf(" [-lat <begin lat> <end lat> ] (defaults for start & end\n");
	printf("	     lines are determined by swath_offset(1))\n");
	printf(" [-p [<num_processors>]]  Number of processors to use.\n");
	printf(" [-f ]       use fft with fico.\n");
	printf(" [-ml ] multilooks igram 5x1 after processing, output name <igram_out>_ml\n");
	printf(" [-nolz2raw ] Disables call to lz2raw_flywheel,\n");
	printf("	     assumes input files are APD raw format\n\n");
	printf("register_lzraw will run the Alaska SAR processor\n");
	printf("on the two images, find the overlap in the two images\n");
	printf("then corregister the two images using  swath_ffset,\n");
	printf("resolve, fico, fit_line, and calc_deltas, then re-process the second\n");
	printf("image so that it matches up (coregistered) with the first.\n");
	printf("After this, you can generate a DEM with tandem_ifm.\n");
	printf("Version %.2f, ASF SAR TOOLS\n\n", VERSION);
	exit(1);
}

main(int argc, char *argv[])
{
	/*Declare variables*/

	char inOne[255];	/*first level zero file name*/
	char inTwo[255];	/*second level zero file name*/
	char outIgram[255];     /*Interferogram output file name*/

	int useSwathOffset = 1; /*flag for weather or not the swaths need to be lined up */
	char swathLat[255];	/*buffer for Latitude bounds sent to swath_offset */
	struct swathbounds sB;	/*Structure containing the info for clipping a swath */
	char grid[255];		/*buffer for a specified grid file */
	int numProcessors = 1;	/*Number of processers to use for parallel processing */
	int doML = 0;		/*Flag for automatic multilooking */
	int useFFT = 0;		/*Use fft match in fico*/
	int uselz2raw = 1;	/*Use lz2raw_flywheel program? */

	int numPatches1;	/*Number of patches to process for swath 1 */
	int numPatches2;	/*Number of patches to process for swath 2 */

	int ii;			/*Counter var.*/

	grid[0] = '\0';		/* Set the grid to NULL */

	/* Do we have enough arguments? */
	if( (argc < 4) || (argc > 16) )
		show_usage();
	else
	{
		/* The first three arguments are required, and must be in a
		 * specific order, read them in here.
		 */
		strcpy(inOne,  argv[1]);
		strcpy(inTwo,  argv[2]);
		strcpy(outIgram,  argv[3]);

		/*Parse cla's*/
		for(ii = 4; ii < argc; ii++)
		{
			/* If latitude constraints have been given for processing, 
			 * Read them in, and set the flag to line up the swaths based 
			 * on those latitudes by calling the swath_offset program.
			 */
			if(strncmp(argv[ii],"-lat", 4)==0)
			{
				if(argc >= ii+3)
				{
					sprintf(swathLat, " %s %s %s", argv[ii], 
							argv[ii+1], argv[ii+2]);
					printf("Found -lat, passing %s to swath_offset\n", swathLat);
					useSwathOffset = 1;
				}else
					show_usage();
			}
			/* Same thing as latitude constraints, except with lines instead */
			if(strncmp(argv[ii],"-line", 4)==0)
			{
				if(argc >= ii+5)
				{
					sscanf(argv[ii+1], "%d", &sB.start_offset1); 
					sscanf(argv[ii+2], "%d", &sB.start_offset2);
					sscanf(argv[ii+3], "%d", &sB.end_offset1);
					sscanf(argv[ii+4], "%d", &sB.end_offset2);
					useSwathOffset = 0;
					printf("Found -line, skipping swath_offset, using\n");
					printf("   %d %d %d %d\n", sB.start_offset1, sB.start_offset2,
								   sB.end_offset1, sB.end_offset2);
					/* set the maskes line values
					 * the same as the unmasked values
					 * since swath offset & watermask are
					 * being skipped.
					 */
					sB.masked_start_offset1 = sB.start_offset1;
					sB.masked_start_offset2 = sB.start_offset2;
					sB.masked_end_offset1 = sB.end_offset1;
					sB.masked_end_offset2 = sB.end_offset2;
				}else
					show_usage();	
			}
			/* if a grid has been specified read the filename in */
			if(strncmp(argv[ii],"-g", 2)==0)
			{
				if(argc >= ii+2)
				{
					sscanf(argv[ii+1], "%s", grid);
					printf("Found -g, using %s grid for fico", grid);
				}else
					show_usage();
	
			}
			/* Hidden option for parallel processing
			 * Set the number of processors if -p <n> has been specified.
			 * Hard coded for our machine at max = 16 processors.
			 */
			if(strncmp(argv[ii],"-p", 2)==0)
			{
				if(argc >= ii+2)
				{
					if(argv[ii+1][0] > '9')
						numProcessors = 8;
					else
						sscanf(argv[ii + 1], "%d", &numProcessors);
					printf("Found -p, using %d processors\n", numProcessors);
				}
				else
				{
					numProcessors = 8;
				}
				if( (numProcessors < 0) || (numProcessors > 16) )
					numProcessors = 8;
			}
			/* set flag to use fft match in fico */
			if(strncmp(argv[ii],"-f", 2)==0)
			{
				useFFT = 1;
				printf("Found -f, using fft for fico\n");
			}
			/* Set flag to automatically multilook the interferogram */
			if(strncmp(argv[ii],"-ml", 3)==0)
			{
				doML = 1;
				printf("Multi-look interferogram after completion.\n");
			}
			/* Switch to turn off level-zero pre-processing */
			if (strncmp(argv[ii],"-nolz2raw",8)==0)
			{
				uselz2raw=0;
				printf("Assuming input data are APD raw files named a & b\n");
			}
		}   /*Finish cla loop.*/
	}	/*Finish parse Cla's */


	/*Make a directory for intermediate files*/
	system("mkdir reg");

	/* Call lz2raw_flywheel for each swath */
	if (uselz2raw)
	{
		lz2raw_flywheel(inOne, "a");
		lz2raw_flywheel(inTwo, "b");
	}
	
	/* Calculate the doppler polynomial for each swath, write the result  
	 * out to each swath's .in file.
	 */
	fix_in_fromraw(inOne, "a");
	fix_in_fromraw(inTwo, "b");

	/* Constrain swaths to coincident areas, or specified 
	 * latitudes using swath_offset
	 */
	if(useSwathOffset == 1)
	{
		swath_offset("a", "b", inOne, inTwo, swathLat);
		/* Mask out large bodies of water using the 
		 * specified DEM with water_mask
		 */

		read_bound_file("boundlines", &sB);
		numPatches1 = sB.aisp_patches;
		numPatches2 = numPatches1;
	}else
	{
		struct AISP_PARAMS aisp;
		read_params("a.in", &aisp);
		numPatches1 = (((sB.end_offset1-sB.start_offset1)
				-4096)/aisp.na_valid) + 1;
		numPatches2 = (((sB.end_offset2-sB.start_offset2)
				-4096)/aisp.na_valid) + 1;
	}


	/* Calculate the average doppler polynomial for the 
	 * two swaths, and write it out to a file.
	 */
	avg_in_dop("a", "b", "reg/avedop");

	/* Process the first image through aisp 
	 * before any further processing.
	 */
printf ("/***------------------------------------------------------***/\n");
printf ("/***              Processing image 1                      ***/\n");
printf ("/***------------------------------------------------------***/\n");

	if(numProcessors == 1) /* if not parallel processing */
	    aisp("-d 1 -c reg/avedop ", sB.start_offset1, numPatches1, "a", "a");
	else /*do parallel processing */
	    paisp("-d 1 -c reg/avedop ", sB.start_offset1, numPatches1, numProcessors, "a", "a");

printf ("/***------------------------------------------------------***/\n");
printf ("/***  Register the top of image 1 to the top of image 2   ***/\n");
printf ("/***------------------------------------------------------***/\n");
	
	/* Process the top patch of each trimmed swath with aisp, 
	 * then using resolve, and fico co-register those two patches.
	 */
	aisp("-d 1 -c reg/avedop ", sB.masked_start_offset1, 1, "a", "reg/a_p1");
	aisp("-d 1 -c reg/avedop ", sB.masked_start_offset2, 1, "b", "reg/b_p1");
	
	resolve("reg/a_p1", "reg/b_p1", "reg/ctrl1");
	if(fico("reg/a_p1", "reg/b_p1", "reg/ctrl1", "reg/fico1", grid, useFFT) == 1)
		exit(1);

	fit_line("reg/fico1", "reg/line1");

printf ("/***------------------------------------------------------***/\n");
printf ("/***  Register bottom of image 1 to the bottom of image 2 ***/\n");
printf ("/***------------------------------------------------------***/\n");


	/* Process the bottom patch of each trimmed swath with aisp, 
	 * then using resolve, and fico co-register those two patches.
	 */
	aisp("-d 1 -c reg/avedop ", sB.masked_end_offset1-4096, 1, "a", "reg/a_pL");
	aisp("-d 1 -c reg/avedop ", sB.masked_end_offset2-4096, 1, "b", "reg/b_pL");
	
	resolve("reg/a_pL", "reg/b_pL", "reg/ctrlL");
	
	if(fico("reg/a_pL", "reg/b_pL", "reg/ctrlL", "reg/ficoL", grid, useFFT) == 1)
		exit(1);

	fit_line("reg/ficoL", "reg/lineL");

	calc_deltas("reg/line1", "reg/lineL", sB.masked_end_offset1-sB.masked_start_offset1-4096, "reg/deltas");


	/* Extrapolate the line if the actual start patch comes before
	 * the start patch that the line was calculated with.
	 */
	ii = sB.masked_start_offset1 - sB.start_offset1;
	if(ii != 0)
	{
		FILE *inFile;
		double a, b, c, d;
		double e, f, g, h;
		inFile = FOPEN("reg/deltas", "r");
			fscanf(inFile, "%lf%lf%lf%lf", &a, &b, &c, &d);
			fscanf(inFile, "%lf%lf%lf%lf", &e, &f, &g, &h);
		FCLOSE(inFile);
		inFile = FOPEN("reg/deltas", "w");
			fprintf(inFile, "%e %e %e %e\n",
				(a-(ii*e)),(b-(ii*f)),(c-(ii*g)),(d-(ii*h)));
			fprintf(inFile, "%e %e %e %e\n", e, f, g, h);
		FCLOSE(inFile); 
	}

/***-----------------------------***/
/*** Process Image, now lined up ***/
/***-----------------------------***/

	/* Process second patch lined up with the first patch.*/
	if(numProcessors == 1)
		aisp("-o reg/deltas -d 1 -c reg/avedop ", sB.start_offset2, 
						numPatches2, "b", "b_corr");
	else
		paisp("-o reg/deltas -d 1 -c reg/avedop ", sB.start_offset2, 
						numPatches2, numProcessors,
						"b", "b_corr");
	
	/* Produce the interferogram, using parallel if specified*/
	if(numProcessors == 1)
		igram("a", "b_corr", outIgram);
	else
		pigram("a", "b_corr", outIgram, numProcessors);

	/* Produce a coherence image, using parallel if specified */
	if(numProcessors == 1)
		coh("a", "b_corr");
	else
		pcoh("a", "b_corr", numProcessors);
	
	/* Multi-look the image if that option was specified */
	if(doML == 1)
	{
		/* Use parallel implementation if specified */
		if(numProcessors == 1)
			ml(outIgram, "a.meta");
		else
			pml(outIgram, numProcessors, "a.meta");
	}

	/* Produce the average power image */
	las_op("ave_pwr", " \"((a^2) + (b^2))/2\" a_amp b_corr_amp");
        printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
        printf("XXXXXXXXX      register_lzraw complete       XXXXXXXXXXXX\n");
        printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");

exit(1);
}
