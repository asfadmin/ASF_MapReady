/******************************************************************************
NAME:	swath_offset.c

SYNOPSIS:

DESCRIPTION:
	   Calculates the overlap between two level zero swath files.

EXTERNAL ASSOCIATES:
    NAME:			USAGE:
    -----------------------------------------------------------------

FILE REFERENCES:
    NAME:			USAGE:
    -----------------------------------------------------------------
    infile1.meta		input meta file for swath 1
    infile2.meta		input meta file for swath 2
    infile1.par			input par file for swath 1
    infile2.par			input par file for swath 2

PROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
     1.0     6/00      D.Koster   Initial Development.
     1.1    7/01      R. Gens    Added logfile and quiet switch
     1.2    9/01      S. Watts   Check lat constraints 
				  and use lzFetch routines.
     1.3    9/01      S.W&T.L    quick fix; distRatio at most is 1.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
	The main algorithm in this program first calculates the start line
    of the image that has a higher start location if descending or lower start
    location if descending.  This is done by calculating the arc distance between
    the two locations on either side of the start location of the 'shorter' swath. 
    Then calculating the distance between the first location before the start of the
    second swath and the start of the second swath.  A ratio between the two is taken
    and then applied to the line lengths of each swath for a start line location.  The
    same sort of thing is done for the end of both of the swaths.  This cuts each of
    them up into the same size based on their location.

ALGORITHM REFERENCES:

BUGS:

*****************************************************************************/
#include "asf.h"
#include "aisp_params.h"
#include "swath_offset_inc.h"
#include "swath_bound.h"
#include "lzFetch.h"

#define VERSION 1.3
#define AISP_VALID_PATCH_LENGTH  3300


int calc_offset(int fromBlock, int toBlock, char *parFile, float *lat1, float *lon1, float lat2, float lon2, 
			int *retIndex, int descend, double azPixTime, double ERad)
{
	int dir, ii;			/* direction {-1, 1} and ii is a counter */
	int index = 0;			/* index into lat & lon vectors */
	/*int tblock;*/			/* temporary index */
	int startLineOffset;		/* line number of the calculated offset */

	double dist12, dist13;		/* arc-distances to calculate a ratio */
	double distRatio;		/* ratio of dist12 and dist 13. */
	double firstDate=0.0, lastDate=0.0;	/* first date in par file, and 
						   current location block date */
	double deltaDate, doubleTemp;	/* difference in dates, and a temporary 
					   double */
	double swathStart = 0.0;	/* used for calculating overlap based on 
					   more or less arbitrary lat & lons */

	char *dateFirst=NULL, *dateLast=NULL; /* character versions of above 
						double values, used for 
						conversions. */
	/*char line[255], param[255];*/	    /* temporaries for pulling data out 
					       of data files */
	char blockName[255];

	/*FILE *fptr;	*/		    /* file pointer for various files */

	/* start at the first block if it is first */
	if(fromBlock > toBlock)	
		dir = -1;
	else
		dir = 1;
		
	/* 
	 * go through the locations blocks and set the index when appropriate.
	 */
	for (ii = fromBlock; ((ii < toBlock) && (dir > 0)) || ((ii >= toBlock) && (dir < 0)); ii+=dir)
	{

		if( ((lat1[ii-1] > lat2) && (lat1[ii] < lat2) && (descend == 0))
		    	|| ((lat1[ii-1] < lat2) && (lat1[ii] > lat2) && (descend == 1)) )
			{
				index = ii;
			}
	}

	/* 
         * calculate the arc distance between  two adjacent locations blocks 
	 * based on the index.
	 */
	dist12 = calc_distance(ERad, (double)lon1[index-1], (double)lat1[index-1], 
				      (double)lon1[index], (double)lat1[index]);

	/* 
	 * estimate longitude if latitude restraints are given 
	 * Use a horribly inefficient but able brute-force algorithm
	 */
	if(lon2 == 361)
	{
		double dist14, dist15;
		int done = 0;

		if(lon1[index-1] > lon1[index])
		{ lon2 = lon1[index]; }
		else
		{ lon2 = lon1[index-1]; }
		

		dist14 = calc_distance(ERad, (double)lon1[index-1], (double)lat1[index-1],
				      (double)lon2, (double) lat2 );
		dist15 = calc_distance(ERad, (double) lon2, (double) lat2,
				      (double)lon1[index], (double)lat1[index]);

		do {
			lon2 += 0.00001;
			dist14 = calc_distance(ERad, (double)lon1[index-1], 
				      (double)lat1[index-1],
				      (double)lon2, (double)lat2);
			dist15 = calc_distance(ERad, (double)lon2, (double) lat2,
				      (double)lon1[index], (double)lat1[index]);

			/* printf("\t%f -> %f -> %f \r", lon1[index-1], lon2, lon1[index]); */
			if( (dist12 - (dist14+dist15) == 0.0) ||
			    ( (dist12 - (dist14+dist15) > -0.0001) && 
			    (dist12 - (dist14+dist15) < 0.0001) ) )
				done = 1;
		} while( done == 0);
	}

	printf("Lat: %f, Lon: %f \n", lat2, lon2);
	if (logflag) {
	  sprintf(logbuf,"Lat: %f, Lon: %f \n", lat2, lon2);
	  printLog(logbuf);
	}

	*retIndex = index;
	if (!quietflag) printf("   Location block: %i\n\n", index);

	dist13 = calc_distance(ERad, (double)lon1[index-1], (double)lat1[index-1],
				      (double)lon2, (double)lat2);

/*	printf("dist12: %lf\n", dist12);*/
	distRatio = dist13/dist12;
	/** A Quick fix. **/
	if (dist13>dist12) distRatio = 1;

	

/**Get the dates from the par file using lzFetch routines**/

	swathStart=lzDouble(parFile,"prep_block.first_date:",NULL);
 	sprintf(blockName,"prep_block.location[%d].line_date:",(index-1));
	dateFirst=lzStr(parFile,blockName,NULL);
	sprintf(blockName,"prep_block.location[%d].line_date:",index);
	dateLast=lzStr(parFile,blockName,NULL);

	if (dateFirst!=NULL)
		firstDate = getSecc(dateFirst);
	if (dateLast!=NULL)
		lastDate = getSecc(dateLast);

	deltaDate = (lastDate - firstDate)/(azPixTime);			
	startLineOffset = deltaDate * distRatio;

	doubleTemp = getSecd(swathStart);
		
	deltaDate = (firstDate - doubleTemp)/(azPixTime);

	return (int) deltaDate+startLineOffset;
}


main(int argc, char *argv[])
{
	/* Structure that contains, the various information to be placed into the 
		output file. Stuff like, start & end offset and filenames. */
	struct swathbounds sB;

	/* string variables */
	char outFile[255];
	char line[255], param[255];

	/* integer variables */
	int descendIs_0 = -1, ii = 0;
	int i, latsDef = 0;	

	/* vectors for lat/long from par file, and pointer for swapping */
	float *Lat1, *Lon1;
	float *Lat2, *Lon2;

	/* double variables */
	double azPixTime1 = 0.0;
	double azPixTime2 = 0.0;
	double ERad1=0.0, ERad2=0.0;
	float upperLat=361.0, lowerLat=361.0;
	float low_lat_limit, large_lat_limit;
	int num_blocks1 = 0, num_blocks2 = 0;

	FILE *fptr;

/* Usage, shown if user does not give 4 arguments */
	if(argc < 5)
	{
		printf("\nUsage: \n");
		printf(" swath_offset < 1.meta > < 2.meta > < 1.par > < 2.par > < out_file > \n");
		printf("		[ -lat <lower lat> <upper lat> -log <file> -quiet]\n\n");
		printf(" 	<1.meta> is the first .meta file of the pair being ");
		printf("compared\n");
		printf("	<2.meta> is the second .meta file of the pair being ");
		printf("compared\n");
		printf(" 	<1.par> is the first .par file of the pair being ");
		printf("compared\n");
		printf("	<2.par> is the second .par file of the pair being ");
		printf("compared\n");		
		printf("	<out_file> is the output file for swath_offset \n");
		printf("	[-lat <lower lat> <upper lat>] allows lat constraints\n");
		printf("	[-log <file>] allows the output to be written to a log file\n");
		printf("	[-quiet] suppresses the output to the essential\n");
		printf("\nThis program calculates the start and end lines of each");
		printf("swath\nand outputs the start & end lines for ");
		printf("each swath,\n"); 
		printf("and the number of patches covered by both.\n\n"); 
		printf("    Version %3.2f, ASF SAR TOOLS\n\n", VERSION);
		exit(1);
	}

/* Get file names from the command line. */
	sB.metaFile1 = (char *) MALLOC(sizeof(char)*strlen(argv[1]) );
	sB.metaFile2 = (char *) MALLOC(sizeof(char)*strlen(argv[2]) );
	sB.parFile1 = (char *) MALLOC(sizeof(char)*strlen(argv[3]) );
	sB.parFile2 = (char *) MALLOC(sizeof(char)*strlen(argv[4]) );
	
	strcpy(sB.metaFile1,argv[1]);
	strcpy(sB.metaFile2,argv[2]);
	strcpy(sB.parFile1,argv[3]);
	strcpy(sB.parFile2,argv[4]);
	strcpy(outFile, argv[5]);

	logflag=quietflag=0;

        for (i=6; i<argc; i++) {
	  if( strncmp(argv[i], "-lat", 4) == 0) {
 	    /* 
	     * Get values to bound the swaths with, if none 
	     * exist, use the start and end of the swath that goes
	     * further in that direction.
	     */
	    sscanf(argv[i+1], "%f", &lowerLat);
	    sscanf(argv[i+2], "%f", &upperLat);	
	    if(lowerLat > upperLat) {
		double dblTmp;
		dblTmp = lowerLat;
		lowerLat = upperLat;
		upperLat = dblTmp;
	    }			
	    latsDef = 1;
	    i+=2;
	  }
          else if(strncmp(argv[i],"-log", 4)==0) {
            sscanf(argv[i+1], "%s", logFile);
            logflag=1;
            fLog = FOPEN(logFile, "a");
	    i+=1;
          }
          else if(strncmp(argv[i],"-quiet", 6)==0) quietflag=1;
	  else {
	    sprintf(errbuf,"   ERROR: '%s' is not a valid option\n",argv[i]);
	    printErr(errbuf);
	  }
        }

        system("date");
        printf("Program: swath_offset\n");
	if (logflag) {
          StartWatchLog(fLog);
          printLog("Program: swath_offset\n");
	}

	fptr = FOPEN(sB.parFile1, "r");
/* read each location block to get the actual number of blocks (parFile1)*/
	sB.total_loc1 = 0;
	while(NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "location", 8) == 0)
			sB.total_loc1++;
	}
	sB.par_end_loc1 = sB.total_loc1;
	sB.par_start_loc1 = 0;
	FSEEK64(fptr, 0, SEEK_SET);

	FCLOSE(fptr);
	if (!quietflag) printf("\n   There %d Blocks in parfile 1\n", sB.total_loc1);

	fptr = FOPEN(sB.parFile2, "r");
/* for each location block to get the actual number of blocks (parFile2) */
	sB.total_loc2 = 0;
	while(NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "location", 8) == 0)
			sB.total_loc2++;
	}
	sB.par_end_loc2 = sB.total_loc2;
	sB.par_start_loc2 = 0;
	FSEEK64(fptr, 0, SEEK_SET);
	FCLOSE(fptr);

	if (!quietflag) printf("   There %d Blocks in parfile 2\n", sB.total_loc2);

/* Allocate Memory for lat/long stuff 
	printf("Allocating memory for lat/lon vectors\n");*/
	Lat1 = (float *) MALLOC(sizeof(float)*sB.total_loc1);
	Lon1 = (float *) MALLOC(sizeof(float)*sB.total_loc1);

	Lat2 = (float *) MALLOC(sizeof(float)*sB.total_loc2);
	Lon2 = (float *) MALLOC(sizeof(float)*sB.total_loc2);

/* get earth radius from both meta files. */
	fptr = FOPEN(sB.metaFile1, "r");
	ERad1 = getFromFiled(fptr, "er");

	FSEEK64(fptr, 0, SEEK_SET);
	azPixTime1 = getFromFiled(fptr, "azPixTime");
	FSEEK64(fptr, 0, SEEK_SET);
	sB.end_offset1 = getFromFilei(fptr, "orig_lines");

	FCLOSE(fptr);

	fptr = FOPEN(sB.metaFile2, "r");

	ERad2 = getFromFiled(fptr, "er");

	FSEEK64(fptr, 0, SEEK_SET);
	azPixTime2 = getFromFiled(fptr, "azPixTime");
	FSEEK64(fptr, 0, SEEK_SET);
	sB.end_offset2 = getFromFilei(fptr, "orig_lines");


	FCLOSE(fptr);

	if (!quietflag) {
	  printf("\n   earth radius 1: %lf\n", ERad1);
	  printf("   earth radius 2: %lf\n", ERad2);
	  printf("\n   azPixTime1: %e\n", azPixTime1);
	  printf("   azPixTime2: %e\n", azPixTime2);
	}

/* read in lat/lon for parfile 1 */
	fptr = FOPEN(sB.parFile1, "r");
	
	ii = 0;
	while(NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "first_pixel_ll", 14) == 0)
		{
			sscanf(line, "%s %f %f", param, &Lat1[ii], &Lon1[ii]);
			ii++;
		}
	num_blocks1 = ii - 1;
	}
	FCLOSE(fptr);
	fptr = FOPEN(sB.parFile2, "r");

/* read in lat/lon for parfile 2 */
	ii = 0;
	while(NULL != fgets(line, 255, fptr) )
	{
		sscanf(line, "%s", param);
		if(strncmp(param, "first_pixel_ll", 14) == 0)
		{
			sscanf(line, "%s %f %f", param, &Lat2[ii], &Lon2[ii]);
			ii++;
		}
	num_blocks2 = ii - 1;
	}

	FCLOSE(fptr);

/**********************************************************************
***if -lat flag set, make sure the given lat constraints are valid*****
***********************************************************************/
if (latsDef)
	{
	low_lat_limit = Lat1[0];
	if (Lat2[0] > Lat1[0])
	  low_lat_limit = Lat2[0];
	low_lat_limit+=.0031;

	large_lat_limit = Lat1[num_blocks1];
	if (Lat2[num_blocks2]<Lat1[num_blocks1])
	   large_lat_limit = Lat2[num_blocks2];
	large_lat_limit-=.003;

	/* If its descending need to swap limits */
	if (large_lat_limit < low_lat_limit)
	  {
		float tmp;
		tmp = low_lat_limit;
		low_lat_limit = large_lat_limit;
		large_lat_limit = tmp;
	  }


      	if (((lowerLat<=low_lat_limit)||(lowerLat>=large_lat_limit)) ||
	   ((upperLat<=low_lat_limit)||(upperLat>=large_lat_limit)))
	   {
	   printf("\nLat constraints given (%.4f and %.4f) are out of range.\n"
		  "Must be greater than %.4f and less than"
		  " %.4f\nExiting...\n\n",
	  lowerLat, upperLat, (low_lat_limit-=.0001), large_lat_limit);
	  exit(0);
	   }
	}

	printf("\n   Par File1 :\n");
	printf("   First location block - Lat: %lf, Lon: %lf \n", Lat1[0], Lon1[0]);
	printf("   Last location block - Lat: %lf, Lon: %lf \n", Lat1[sB.total_loc1-1], Lon1[sB.total_loc1-1]);
	printf("\n   Par File2:\n");
	printf("   First location block - Lat: %lf, Lon: %lf \n", Lat2[0], Lon2[0]);
	printf("   Last location block - Lat: %lf %lf, Lon: \n", Lat2[sB.total_loc2-1], Lon2[sB.total_loc2-1]);

	if (logflag) {
	  printLog("\n   Par File1 :\n");
	  sprintf(logbuf,"   First location block - Lat: %lf, Lon: %lf \n", Lat1[0], Lon1[0]);
	  printLog(logbuf);
	  sprintf(logbuf,"   Last location block - Lat: %lf, Lon: %lf \n", Lat1[sB.total_loc1-1], Lon1[sB.total_loc1-1]);
	  printLog(logbuf);
	  printLog("\n   Par File2:\n");
	  sprintf(logbuf,"   First location block - Lat: %lf, Lon: %lf \n", Lat2[0], Lon2[0]);
	  printLog(logbuf);
	  sprintf(logbuf,"   Last location block - Lat: %lf %lf, Lon: \n", Lat2[sB.total_loc2-1], Lon2[sB.total_loc2-1]);
	  printLog(logbuf);
	}

	if((Lat1[1] - Lat1[0]) < 0)
	{	
	  printf("\n   Descending pair\n\n"); 
	  if (logflag) printLog("\n   Descending pair\n\n"); 
	  descendIs_0 = 0;
	}
	else
	{	
	  printf("\n   Ascending pair\n\n"); 
	  if (logflag) printLog("\n   Ascending pair\n\n"); 
	  descendIs_0 = 1;
	}

/*
 *
 * Begin offset calculation for beginning & end of the swath given no latitude
 * constraints.
 *
 */
	if( latsDef == 0)
	{
	if( ((Lat1[0] < Lat2[0]) && (descendIs_0 == 0)) ||
		((Lat1[0] > Lat2[0]) && (descendIs_0 == 1)) )
	{
		printf("   Start image 2 - ");
		if (logflag) printLog("   Start image 2 - ");
		sB.start_offset2 = calc_offset(1, sB.total_loc2, sB.parFile2, Lat2, Lon2,
				Lat1[0], Lon1[0], &sB.par_start_loc2, descendIs_0, azPixTime2, ERad2);
	}
	else
	{
		printf("   Start image 1 - ");
		if (logflag) printLog("   Start image 1 - ");
		sB.start_offset1 = calc_offset(1, sB.total_loc1, sB.parFile1, Lat1, Lon1,
				Lat2[0], Lon2[0], &sB.par_start_loc1, descendIs_0, azPixTime1, ERad1);
	}
	if (!quietflag) {
	  printf("sB.start_offset1: %i \n", sB.start_offset1);
	  printf("sB.start_offset2: %i \n", sB.start_offset2);
	  printf("sB.par_start_loc1: %i \n", sB.par_start_loc1);
	  printf("sB.par_start_loc2: %i \n", sB.par_start_loc2);
	}

	if( ((Lat1[sB.total_loc1-1] > Lat2[sB.total_loc2-1]) && (descendIs_0 == 0)) ||
		((Lat1[sB.total_loc1-1] < Lat2[sB.total_loc2-1]) && (descendIs_0 == 1)) )

	{
		printf("   End image 2 - ");
		if (logflag) printLog("   End image 2 - ");
		sB.end_offset2 = calc_offset(sB.total_loc2-1, 1, sB.parFile2, Lat2, Lon2,
			Lat1[sB.total_loc1-1], Lon1[sB.total_loc1-1], &sB.par_end_loc2, 
			descendIs_0, azPixTime2, ERad2);		
		sB.par_end_loc1 -= 1;
	}
	else
	{
		printf("   End image 1 - ");
		if (logflag) printLog("   End image 1 - ");
		sB.end_offset1 = calc_offset(sB.total_loc1-1, 1, sB.parFile1, Lat1, Lon1,
			Lat2[sB.total_loc2-1], Lon2[sB.total_loc2-1], &sB.par_end_loc1, 
			descendIs_0, azPixTime1, ERad1);
		sB.par_end_loc2 -= 2;
	}
	if (!quietflag) {
	  printf("   sB.end_offset1: %i \n", sB.end_offset1);
	  printf("   sB.end_offset2: %i \n", sB.end_offset2);
	  printf("   sB.par_end_loc1: %i \n", sB.par_end_loc1);
	  printf("   sB.par_end_loc2: %i \n", sB.par_end_loc2); 
	}
	/*
 	 *
 	 * End Calculate swath 
 	 *
 	 */
	} /* End if latsDef == 0 */
	else
	{
	/* 
	 * The case where latitude constraints are given ->
	 */
		if(descendIs_0 == 0)
		{
			printf("   Start image 1 - ");
			if (logflag) printLog("   Start image 1 - ");
			sB.start_offset1 = calc_offset(1, sB.total_loc1,
					sB.parFile1, Lat1, Lon1, upperLat, 
					361, &sB.par_start_loc1, 
					descendIs_0, azPixTime1, ERad1);
			printf("   Start image 2 - ");
			if (logflag) printLog("   Start image 2 - ");
			sB.start_offset2 = calc_offset(1, sB.total_loc2,
					sB.parFile2, Lat2, Lon2, upperLat, 
					361, &sB.par_start_loc2, 
					descendIs_0, azPixTime2, ERad2);
			printf("   End image 1 - ");
			if (logflag) printLog("   End image 1 - ");
			sB.end_offset1 = calc_offset(sB.total_loc1-1, 1,
					sB.parFile1, Lat1, Lon1, lowerLat, 
					361, &sB.par_end_loc1, 
					descendIs_0, azPixTime1, ERad1);
			printf("   End image 2 - ");
			if (logflag) printLog("   End image 2 - ");
			sB.end_offset2 = calc_offset(sB.total_loc2-1, 1,
					sB.parFile2, Lat2, Lon2, lowerLat, 
					361, &sB.par_end_loc2, 
					descendIs_0, azPixTime2, ERad2);
			printf("\n");
			if (logflag) printLog("\n");
		}else
		{
			printf("   Start image 1 - ");
			if (logflag) printLog("   Start image 1 - ");
			sB.start_offset1 = calc_offset(1, sB.total_loc1,
					sB.parFile1, Lat1, Lon1, lowerLat, 
					361, &sB.par_start_loc1, 
					descendIs_0, azPixTime1, ERad1);
			printf("   Start image 2 - ");
			if (logflag) printLog("   Start image 2 - ");
			sB.start_offset2 = calc_offset(1, sB.total_loc2,
					sB.parFile2, Lat2, Lon2, lowerLat, 
					361, &sB.par_start_loc2, 
					descendIs_0, azPixTime2, ERad2);
			printf("   End image 1 - ");
			if (logflag) printLog("   End image 1 - ");
			sB.end_offset1 = calc_offset(sB.total_loc1-1, 1,
					sB.parFile1, Lat1, Lon1, upperLat, 
					361, &sB.par_end_loc1, 
					descendIs_0, azPixTime1, ERad1);
			printf("   End image 2 - ");
			if (logflag) printLog("   End image 2 - ");
			sB.end_offset2 = calc_offset(sB.total_loc2-1, 1,
					sB.parFile2, Lat2, Lon2, upperLat, 
					361, &sB.par_end_loc2, 
					descendIs_0, azPixTime2, ERad2);
			printf("\n");
			if (logflag) printLog("\n");
		}
	} /* end else for latDef == 0 */

	if( (sB.end_offset1 - sB.start_offset1) > (sB.end_offset2 - sB.start_offset2))
	{
		if (!quietflag) printf("   1 is longer than 2, clip it from the end\n\n");
		sB.end_offset1 = sB.end_offset1 - ((sB.end_offset1 - sB.start_offset1) - 
							(sB.end_offset2 - sB.start_offset2));
	}
	else if( (sB.end_offset1 - sB.start_offset1) < (sB.end_offset2 - sB.start_offset2))
	{
		if (!quietflag) printf("   2 is longer than 1, clip it from the end\n\n");
		sB.end_offset2 = sB.end_offset2 - ((sB.end_offset2 - sB.start_offset2) - 
							(sB.end_offset1 - sB.start_offset1));
	}
	if (!quietflag) {
	  printf("   image1 line %d -> %d\n", sB.start_offset1, sB.end_offset1);
	  printf("   image2 line %d -> %d\n\n", sB.start_offset2, sB.end_offset2);
	}

	if(sB.par_start_loc1 > sB.par_end_loc1)
	{ ii = sB.par_start_loc1; sB.par_start_loc1 = sB.par_end_loc1;sB.par_end_loc1 = ii; } 
	if(sB.par_start_loc2 > sB.par_end_loc2)
	{ ii = sB.par_start_loc2; sB.par_start_loc2 =sB.par_end_loc2;sB.par_end_loc2 = ii; } 

	/* write result out to a file */
	/*sB.start_offset1 = startOffset1;*/
	sB.aisp_patches = (int) (((sB.end_offset1-sB.start_offset1)-4096)/AISP_VALID_PATCH_LENGTH) + 1;

	sB.total_loc1 -= 1;
	sB.total_loc2 -= 1;
	sB.masked_patches = sB.aisp_patches;
	sB.masked_start_offset1 = sB.start_offset1;
	sB.masked_start_offset2 = sB.start_offset2;
	sB.masked_end_offset1 = sB.end_offset1;
	sB.masked_end_offset2 = sB.end_offset2;
	
	make_bound_file(outFile, sB);
	
	return 0;
}
