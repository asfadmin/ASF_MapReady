/********************************************************************************
NAME:
	register_akdemSwath.c

SYNOPSIS:

DESCRIPTION:
	This program processes, correlates, and registers 
	two non-correlated L0 swath images.

FILE REFERENCES:
	NAME:		USAGE:
	---------------------------------------------------------------------
	swath_one	L0 binary swath input.
	swath_two	L0 binary swath input.
	dem_name	DEM for masking out large bodies of water.
	igram_out	output interferogram file.

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	2.0	11/00	Dave Koster, Development from csh script.
	2.1	6/01	T. Logan, Fixed last patch offset bug, 
			added -nolz2raw switch.
        2.2     6/01    T. Logan, Added -flywheel and -par switches
	2.21	7/01    R. Gens, Added -log and -quiet switches
	2.4     3/02    P. Denny, Rewrote command line parsing & usage

HARDWARE/SOFTWARE LIMITATIONS:
	The maximum number of processors is hard-coded at 16.
        The default number of processors for parallel processing 
        is 8.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	There are no known bugs.

ASSOCIATE PROGRAMS:
	lz2raw(1)
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

*********************************************************************************/

#include "asf.h"
#include "aisp_params.h"
#include "register_lzraw_inc.h"
#include "swath_bound.h"

#define VERSION 2.4

void usage(char *name);

int main(int argc, char *argv[])
{
	/*Declare variables*/

	char inOne[255];	/*first level zero file name*/
	char inTwo[255];	/*second level zero file name*/
	char outIgram[255];     /*Interferogram output file name*/

	char demName[255];	/*DEM name for water masking */
	int demSpecified = 0;	/*flag for weather or not watermasking should be done */
	int useSwathOffset = 1; /*flag for weather or not the swaths need to be lined up */
	char swathLat[255];	/*buffer for Latitude bounds sent to swath_offset */
	struct swathbounds sB;	/*Structure containing the info for clipping a swath */
	char grid[255];		/*buffer for a specified grid file */
	int numProcessors = 1;	/*Number of processers to use for parallel processing */
	int doML = 0;		/*Flag for automatic multilooking */
	int useFFT = 0;		/*Use fft match in fico*/
	int uselz2raw = 1;	/*Use lz2raw program? */
        int usePar = 0;         /*Use lz2raw_par program? */
        int useFlywheel = 0;    /*Use lz2raw_flywheel program? */
	float rngPercent = -1.0;/*percentage of swath that can be over water*/

	int numPatches1;	/*Number of patches to process for swath 1 */
	int numPatches2;	/*Number of patches to process for swath 2 */

	int offset_diff;	/*Difference between sB.masked_start_offset1 & sB.start_offset1 */

	grid[0] = '\0';		/* Set the grid to NULL */
	logflag=quietflag=0;	/* intialize log & quiet to false */

	StartWatch();
	printf("\nDate: ");
	system("date");
        printf("Program: register_akdemSwath\n\n");
	if (argc > 4) printf("   Options:\n");
	
	/* Parse command line args */
	currArg=1;	/* from cla.h which is in asf.h */
	while (currArg < (argc-3))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-dem")) {
		/* Set the demSpecified flag to use the water_mask program */
			CHECK_ARG(1) /*one string argument: dem file */
			strcpy(demName,GET_ARG(1));
			printf("   Found -dem, using %s as water masking DEM\n", demName);
			demSpecified = 1;
		}
		else if (strmatch(key,"-g")) {
			CHECK_ARG(1) /*one string argument: grid buffer */
			strcpy(grid,GET_ARG(1));
			printf("   Found -g, using %s grid for fico\n", grid);
		}
		else if (strmatch(key,"-par")) {
		/* set flag to use lz2raw_par */
			usePar = 1;
			printf("   Found -par, using lz2raw_par\n");
		}
		else if (strmatch(key,"-p")) {
		/* Set the number of processors if -p <n> has been specified. */
			CHECK_ARG(1) /*one integer argument: num of processors */
			numProcessors = atoi(GET_ARG(1));
			if( (numProcessors < 1) || (numProcessors > 16) )
			{
				printf("-p <# of processors> must be between 1 & 16 (inclusive).\n");
				usage(argv[0]);
			}
			printf("   Found -p, using %d processors\n", numProcessors);
		}
		else if(strmatch(key,"-line")) {
		/* Allow user to give lines to line images up manually. */
			CHECK_ARG(4) /*four int arguments:  sl1 sl2 el1 el2*/
			sB.start_offset1 = atoi(GET_ARG(4)); 
			sB.start_offset2 = atoi(GET_ARG(3));
			sB.end_offset1 = atoi(GET_ARG(2));
			sB.end_offset2 = atoi(GET_ARG(1));
			useSwathOffset = 0;
			printf("Found -line, skipping swath_offset, using:\n");
			printf("   %d %d %d %d\n", sB.start_offset1, sB.start_offset2,
						   sB.end_offset1, sB.end_offset2);
		/* set the maskes line values the same as the unmasked values
		 * since swath offset & watermask are being skipped. */
			sB.masked_start_offset1 = sB.start_offset1;
			sB.masked_start_offset2 = sB.start_offset2;
			sB.masked_end_offset1 = sB.end_offset1;
			sB.masked_end_offset2 = sB.end_offset2;
		}
		else if(strmatch(key,"-lat")) {
		/* read latitude constraints in, and set the flag to line up the swaths 
		 * based on those latitudes by calling the swath_offset program. */
			CHECK_ARG(2) /*two int arguments:  begin lat, end lat*/
			sprintf(swathLat, " -lat %s %s",GET_ARG(1),GET_ARG(2));
			printf("   Found -lat, passing \"%s\" to swath_offset\n", swathLat);
			useSwathOffset = 1;
		}
		else if (strmatch(key,"-log")) {
		/* Output copied to a log file */
			CHECK_ARG(1) /*one filename arguments:  log file*/
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag=1;
			printf("   Found -log, writing output to a log file: %s\n", logFile);
		}
		else if (strmatch(key,"-r")) {
		/* set a range percent for the water mask program */
			CHECK_ARG(1) /*one floating-point arguments:  range percent*/
			rngPercent = atof(GET_ARG(1));
                        if(rngPercent < 0)  {printf("**  -r <%.3f>: Cannot go below 0.0 (0%%)\n",rngPercent); usage(argv[0]);}
                        if(rngPercent > 1.0){printf("**  -r <%.3f>: Cannot exceed 1.0 (100%%)\n",rngPercent); usage(argv[0]);}
 			printf("   Found -r, using %f as range percent for water_mask\n", rngPercent);
		}
		else if (strmatch(key,"-flywheel")) {
		/* -flywheel (use lz2raw_flywheel) */
			useFlywheel = 1;
			printf("   Found -flywheel, using lz2raw_flywheel\n");
		}
		else if(strmatch(key,"-f")) {
		/* -f (set flag to use fftMatch in fico) */
			useFFT = 1;
			printf("   Found -f, using fft for fico\n");
		}
		else if(strmatch(key,"-ml")) {
		/* -ml (Flag to multilook the interferogram) */
			doML = 1;
			printf("   Found -ml, multi-look interferogram after completion.\n");
		}
		else if (strmatch(key,"-nolz2raw")) {
		 /* -nolz2raw (Turn off level-zero pre-processing) */
			uselz2raw=0;
			printf("   Found -nolz2raw, assuming input data are APD raw files named a & b\n");
		}
		else if (strmatch(key,"-quiet")) {
		 /* -quiet (Suppress some of the output) */
			quietflag=1;
			printf("   Found -quiet, suppressing the output to the essential\n");
		}
		else {printf("\n*****Unrecognized option keyword:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	/* Grab required arguments */
	strcpy(inOne,   argv[currArg++]);
	strcpy(inTwo,   argv[currArg++]);
	strcpy(outIgram,argv[currArg]);
	
	/*Make a directory for intermediate files*/
	system("mkdir reg");
	printf("\n");

        /* Call lz2raw for each swath */   
        if (!uselz2raw && (useFlywheel || usePar))
        {
                printErr("   ERROR: Invalid command line options given.\n   \
                		ERROR: -nolz2raw and -flywheel/-par are mutually exclusive\n");
        }
        else if (useFlywheel && usePar)
        { 
                printErr("   ERROR: Invalid command line options given.\n   \
                		ERROR: -flywheel and -par are mutually exclusive\n");
        }                     
        
        if (uselz2raw)
        {     
                if (useFlywheel)
                {
                    lz2raw_flywheel(inOne, "a");
                    lz2raw_flywheel(inTwo, "b");
                }
                else if (usePar)
                {
                    lz2raw_par(inOne, "a");
                    lz2raw_par(inTwo, "b");
                }
                else
                { 
                    lz2raw(inOne, "a");
                    lz2raw(inTwo, "b");
                }
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
		if(demSpecified == 1)
			water_mask(demName, "boundlines", rngPercent);

		read_bound_file("boundlines", &sB);
		numPatches1 = sB.aisp_patches;
		numPatches2 = numPatches1;
	}
	else
	{
		numPatches1 = (((sB.end_offset1-sB.start_offset1)
				-4096)/AISP_VALID_PATCH_LENGTH) + 1;
		numPatches2 = (((sB.end_offset2-sB.start_offset2)
				-4096)/AISP_VALID_PATCH_LENGTH) + 1;
	}


	/* Calculate the average doppler polynomial for the 
	 * two swaths, and write it out to a file.
	 */
	avg_in_dop("a", "b", "reg/avedop");

	/* Process the first image through aisp 
	 * before any further processing.
	 */

	if(numProcessors == 1) /* if not parallel processing */
	    aisp("-d 1 -c reg/avedop ", sB.start_offset1, numPatches1, "a", "a");
	else /*do parallel processing */
	    paisp("-d 1 -c reg/avedop ", sB.start_offset1, numPatches1, numProcessors, "a", "a");

	/* Process the top patch of each trimmed swath with aisp, 
	 * then using resolve, and fico co-register those two patches.
	 */
	aisp("-d 1 -c reg/avedop ", sB.masked_start_offset1, 1, "a", "reg/a_p1");
	aisp("-d 1 -c reg/avedop ", sB.masked_start_offset2, 1, "b", "reg/b_p1");
	
	resolve("reg/a_p1", "reg/b_p1", "reg/ctrl1");
	if(fico("reg/a_p1", "reg/b_p1", "reg/ctrl1", "reg/fico1", grid, useFFT) == 1)
		exit(1);

	fit_line("reg/fico1", "reg/line1");

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
	offset_diff = sB.masked_start_offset1 - sB.start_offset1;
	if(offset_diff != 0)
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
				(a-(offset_diff*e)),(b-(offset_diff*f)),(c-(offset_diff*g)),(d-(offset_diff*h)));
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

	/* Produce the average power image 
	las_op("ave_pwr", " \"((a^2) + (b^2))/2\" a_amp b_corr_amp");
	*/

	/* Produce the individual power images 
	las_op("a_pwr", " \"a*a\" a_amp");
	las_op("b_pwr", " \"a*a\" b_corr_amp");*/

/*        printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
        printf("XXXXXXXXX      register_lzraw complete       XXXXXXXXXXXX\n");
        printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");*/
	
	StopWatch();
	if (logflag) StopWatchLog(fLog);

	exit(0);
}


void usage(char *name)
{
fflush(NULL);
 printf("\n"
	"USAGE:\n"
	"   %s\n"
	"      [-dem <dem_name>] [-g <grid_resoloution>] [-p <#_of_Processors>]\n"
	"      [-line <start_line_1> <start_line_2> <end_line_1> <end_line_2>]\n"
	"      [-lat <begin_lat> <end_lat>] [-r <percent>] [-f] [-nolz2raw]\n"
	"      [-flywheel] [-par] [-ml] [-log <file>] [-quiet]\n"
	"      <file_1> <file_2> <igram_out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <file_1>    an LZP Swath file \n"
	"   <file_2>    an LZP Swath file \n"
	"   <igram_out> output name for produced interferogram (.amp, .phase, .ddr)\n");
 printf("\n"
	"OPTIONS:\n"
	"       -dem <dem_name>  is a DEM to use for masking out large bodies of water\n"
	"  -g <grid_resolution>  is the grid resolution for fico(1).\n"
	"                          (default is 20 for a 20x20 grid)\n"
	"     -p <#_Processors>  number of processors to use with paisp.\n"
	" -line <start_line_1> <end_line_1> <start_line_2> <end_line_2>\n"
	"        <start_line_1>  start line for image 1 \n"
	"          <end_line_1>  end line for image 1\n"
	"        <start_line_2>  start line for image 2\n"
	"          <end_line_2>  end line for image 2\n"
	" -lat <begin_lat> <end_lat>\n"
	"                        defaults for start & end lines are determined by\n"
	"                             swath_offset(1)\n"
	"          -r <percent>  defines the percent of range that must be land\n"
	"                             default = 1.0 (100%%)\n"
	"                    -f  use fft with fico.\n"
	"             -nolz2raw  Disables call to lz2raw, assumes input files are APD\n"
	"                             raw format\n"
	"             -flywheel  Replaces call to lz2raw with call to lz2raw_flywheel\n"
	"                  -par  Replaces call to lz2raw with call to lz2raw_par\n"  
	"                   -ml  multilooks igram 5x1 after processing, output name\n"
	"                             <igram_out>_ml\n"
	"           -log <file>  Allows the output to be written to a log file\n"  
	"                -quiet  Suppresses the output to the essential\n");  
 printf("\n"
	"DESCRIPTION:\n"
	"   This program will run the Alaska SAR processor on the two\n"
	"   images, find the overlap in the two images then corregister\n"
	"   the two images using swath_offset, resolve, fico, fit_line,\n"
	"   and calc_deltas, then re-process the second image so that\n"
	"   it matches up with (is coregistered with) the first.\n"
	"   After this, you can generate a DEM with tandem_ifm.\n");
 printf("\n"
	"VERSION %1.2f, ASF ISAR TOOLS\n"
	"\n", VERSION);

 exit(1);
}
