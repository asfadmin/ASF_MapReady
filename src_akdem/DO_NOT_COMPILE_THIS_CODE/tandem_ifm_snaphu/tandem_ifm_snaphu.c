/******************************************************************************
NAME:  tandem_ifm - generates a baseline-refined DEM from an interferogram

DESCRIPTION:

	Performs interferometric processing on input complex files,
     creating an igram, deramped phase, multilooked amp & phase, 
     and unwrapped phase files.
        The algorithm relies on several external programs, which it 
     calls as follows:

    --------------------------------------------------------------------------  
     *) Loop while baseline is changing: 
  	  a)  deramp	- Remove Earth's curvature using latest base line file
	  b)  ml	- Create multilooked igram using latest deramped phase
	  c)  escher      - Unwrap multilooked phase file using latest base line
	  d)  refine_base - Refine the baseline parameter estimates using seed pts.
    --------------------------------------------------------------------------  
     *) elev	- converts unwrapped phase into height elevations
     *) coh	    - calculates the image pair's phase coherence 
     *) deskew_dem - remaps a DEM to ground range.
     *) las2ppm    - converts unwrapped phase mask file into colorized ppm file
    --------------------------------------------------------------------------  

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    deramp		Removes phase change due to earth's curvature & baseline.
    ml			Multilooks igram amp and phase to 5x1 look
    escher		Unwraps phase files using Goldstein branch-cuts
    refine_base		Modifies baseline parameter estimates based on seeds

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    in1.cpx		Input float complex SAR image
    in2.cpx		Input float complex SAR image
    in1.L		Satellite metadata file for scene dependent params
    seeds		Seed point file of sample,line,elevation values

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    3/97   T. Logan     Automate the creation of ifsar DEMs
    1.1	    8/97   O. Lawlor    Go all the way to ground range.
    1.2	    8/97   O. Lawlor    Speed up baseline refinement.
    2.0     10/97  O. Lawlor    DEM-guided phase unwrapping.
    2.1     3/98   O. Lawlor    Added reverse deramping for higher quality.
    2.2     6/98   O. Lawlor    Run from interferogram only.
    2.3     6/98   O. Lawlor    Updated convergence test (test for cyclic convergence).
    2.5     8/98   O. Lawlor    Added phase filtering.
    2.6     2/99   O. Lawlor    Phase filtering bug fix (added zeroify).

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
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

RICK GURITZ      rguritz@images.alaska    (907)474-7886
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
#include "asf.h"

#include "las.h"
#include "ddr.h"
#define VERSION 2.5

/*External functions:*/
float get_seed(char[], int, int, int);	/* Get the seed phase used by Escher */
int check_refinement(char * newBaseline, char *oldBaseline, char *veryOldBaseline);	/* Check if refinement is converging */

/*Functions listed after main:*/
 float get_seed (char *fname, int wid, int seedX, int seedY); 
 char *base2str (int baseNo); 
 void derampIgram (char *baselineFile); 
 void multilook (char *ml_igram); 
 void unwrapPhase (void); 
 void execute (char *cmd); 
 void DISPLAY (char *msg); 
 void give_usage (char *name);

/*Globals:*/
#define multilookPixels 5
char  *ceos, *igram,*seeds;     /* Input complex files	& seed point file */
int escherX=0,escherY=0; /*Phase unwrapping starting point.*/
char *demPhase=NULL; /*DEM phase image.*/
float filterPower=-1.0;
char cmd[256]; /* Temporary command buffer. */

/*Base2str: returns a baseline string, given a baseline
number (i.e. 0 -> "base.00", 2 -> "base.02", 15 -> "base.15").*/
char *base2str(int baseNo)
{
	char *baseStr=(char *)MALLOC(sizeof(char)*50);
	if (baseNo<10) sprintf(baseStr,"base.0%i",baseNo);
	else sprintf(baseStr,"base.%i",baseNo);
	return baseStr;
}

/*DerampIgram: deramps igram".phase" into "igramd.phase" using
the specified baseline file.  Then copies over the amp and ddr.*/
void derampIgram(char *baselineFile)
{
	DISPLAY("    Performing Phase Deramping\n");
	sprintf(cmd,"deramp %s %s %s igramd\n",igram,ceos,baselineFile);
	execute(cmd);
}

/*Multilook: multilooks igram into "ml.*" */
/*void multilook(char *ml_igram)
{
	DISPLAY("    Creating a multilooked image file\n");
	sprintf(cmd,"ml -s %ix1 -l %ix2 %s ml\n",multilookPixels,
		2*multilookPixels,ml_igram);
	execute(cmd);
}*/
void multilook(char *ml_igram)
{
	DISPLAY("    Creating a multilooked image file\n");
	/*
	sprintf(cmd,"mpirun -np 8 pml -l %ix2 %s ml\n",2*multilookPixels,ml_igram);
	*/
	sprintf(cmd,"ml -l %ix2 %s ml\n",2*multilookPixels,ml_igram);
	execute(cmd);
}



void escher(char *unwrapBaseIn,char *unwrapBaseOut)
{
	char *unwrapThis=unwrapBaseIn;
/*Perform phase filtering*/
	if (filterPower>0.0)
	{
		unwrapThis="filtered_phase";
		DISPLAY("Filtering phase\n");
		sprintf(cmd,"phase_filter %s.phase %f %s.phase",
			unwrapBaseIn,filterPower,unwrapThis); execute(cmd);
	}

/*Add zeros where ml.phase does not exist*/
	if (0!=strcmp(unwrapThis,"ml"))/*Skip zero-ify if we're unwrapping ml*/
	{
		sprintf(cmd,"zeroify %s.phase ml.phase escher_in.phase",unwrapThis);
		execute(cmd);
		unwrapThis="escher_in";
	}
	
/*Assemble the unwrapping command*/
	sprintf(cmd,"escher %s %s ",unwrapThis,unwrapBaseOut);
	
	if (escherX != 0 || escherY !=0) /*Seed Specified*/
		sprintf(&(cmd[strlen(cmd)]),"%i %i\n",escherX,escherY);
	
	DISPLAY("Performing phase unwrapping\n");
	execute(cmd);
	
}

/*UnwrapPhase: unwraps "ml.phase" into "unwrap.phase",
 possibly using a dem-phase image.*/
void unwrapPhase(void)
{
	if (demPhase!=NULL)
	{ /*Use dem-guided phase unwrapping.*/
		char *unwrapThis="ml_dem";
		
		DISPLAY("Subtracting off Terrain-induced phase\n");
		/*
		sprintf(cmd,"las_op ml_dem.phase '(a-b)%%6.2831853-3.14159265' ml.phase %s.phase"
			,demPhase); execute(cmd);
			*/


/*sprintf(cmd,"/home/dkoster/testtools/tilesnaphu 3 3 400 400 --nproc 8 --path /home/dkoster/testtools/ ml.phase 4800 -m ml.amp --AA a_pwr.img b_pwr.img -f snaphu.conf -o unwrap.phase");
*/			           /* --quiet */

sprintf(cmd,"/home/dkoster/testtools/tilesnaphu 1 3 0 400 --nproc 8 --path /home/dkoster/testtools/ --demifm demargfile ml.phase 4800 -m ml.amp"); 
strcat(cmd, " --AA a_pwr.img b_pwr.img -E out_dem_phase.phase -f snaphu.conf -o unwrap.phase");

printf("\n\n\n%s\n\n\n\n",cmd);
system(cmd);
system("ln -s ml.ddr unwrap.ddr");
			/*
		escher("ml_dem","unwrap_dem");		
		system("ln -s unwrap_dem.phase.mask unwrap.phase.mask\n");
		*/


/*	
		DISPLAY("Adding terrain-induced phase back in.\n");
		sprintf(cmd,"las_op unwrap.phase '(a+b)*(a/a)*(b/b)' unwrap_dem.phase %s.phase"
			,demPhase); execute(cmd); */
		/*
		sprintf(cmd,"las_op unwrap.phase '(a+b)' unwrap_dem.phase %s.phase"
			,demPhase); execute(cmd); 

			*/		
	} else /*Use regular, non-DEM phase unwrapping.*/
	  {

				/* --quiet */
sprintf(cmd,"/3dsar1/home/home/dkoster/testools/tilesnaphu 3 3 400 400 --quiet --nproc 8  ml.phase 4800 -m ml.amp --AA a_pwr.img b_pwr.img -f snaphu.conf -o unwrap.phase");

printf("\n\n\n%s\n\n\n\n",cmd);
system(cmd);
system("ln -s ml.ddr unwrap.ddr");

	  }
}



int main(int argc,char *argv[])
{
	int   i;
	int  	still_going = 1,	/* For baseline convergence test	*/
		start = 0;		/* Starting base line file number       */
	char *newBaseline=NULL, *oldBaseline=NULL, *veryOldBaseline=NULL;
	int optind;

	DISPLAY("Tandem Interferometric DEM Generation Program\n");

	/* Process command line parameters */
	if (argc < 4) give_usage(argv[0]);
	igram=argv[1];
	ceos=argv[2];
	seeds=argv[3];

	optind=4;
	while (optind<argc)
	{
	  if (argv[optind][0]=='-')
		switch (argv[optind][1])
		{
		case 'd':
			if (!(++optind<argc)) give_usage(argv[0]);
			else demPhase=argv[optind++];
			printf("Removing elevation-induced phase with %s.phase\n",demPhase);
			break;
		case 's':
			if (!(++optind<argc)) give_usage(argv[0]);
			else start=atoi(argv[optind++]);
			printf("Starting iteration with baseline %i\n",start);
			break;
		case 'v':
			still_going=0;
			optind++;
			break;
		case 'x':
			if (!(++optind<argc)) give_usage(argv[0]);
			else escherX=atoi(argv[optind++]);
			break;
		case 'y':
			if (!(++optind<argc)) give_usage(argv[0]);
			else escherY=atoi(argv[optind++]);
			break;
		case 'f':
			if (!(++optind<argc)) give_usage(argv[0]);
			else filterPower=atof(argv[optind++]);
			break;
		default: give_usage(argv[0]);
	  } else give_usage(argv[0]);
	}
	/* Done processing command line arguments.*/
	
	oldBaseline=newBaseline;
        newBaseline=base2str(start);
        
	if (start==0)
	{
		derampIgram(newBaseline);
		multilook("igramd");
		unwrapPhase();
		sprintf(cmd,"deramp unwrap %s %s unwrap_nod -b\n",ceos,newBaseline);
		execute(cmd);
	}
	
	DISPLAY("    Begining the iterations to converge the baseline\n");
	if (still_going)
	{
	  for (i=start; still_going; i++) 
	  {
	  	veryOldBaseline=oldBaseline;
		oldBaseline=newBaseline;
		newBaseline=base2str(i+1);

		DISPLAY("    Refining the interferometric baseline\n");
		sprintf(cmd,"refine_base unwrap_nod.phase %s %s %s %s\n",
			seeds,ceos,oldBaseline,newBaseline);
		execute(cmd);
		if (i>0)
			if (check_refinement(newBaseline,oldBaseline,veryOldBaseline))
				still_going=0;/*We converged!*/
		if (i>90)
			still_going=0;/*Stop failures to converge after a while.*/
	  }

    
	  if (i > 90) 
	  {
		 fprintf(stderr,"\n\n\n");
		 fprintf(stderr,"*****  ITERATIONS FAILED TO CONVERGE  *****\n");
		 fprintf(stderr,"\n\n\n");
		 exit(1);
	  }

	  printf("\n\n");
	  DISPLAY("         Baseline Refinement Successfully Converged!!  \n");
	  
	}
	/* Create DEM file, coherence file, & colorized unwrapping mask... */

	sprintf(cmd,"deramp unwrap_nod %s %s unwrap\n",ceos,newBaseline);
	execute(cmd);
		
	DISPLAY("    Creating the Terrain Elevation File\n");
	sprintf(cmd,"elev unwrap.phase %s %s elevation_sr.ht %s\n",
			newBaseline,ceos, seeds);
	execute(cmd);
	
	DISPLAY("    Creating the Elevation Error Estimate Image\n");
	execute("cp unwrap.ddr unwrap.phase.ddr");
	/*
	sprintf(cmd,"eleverr %s_coh.img %s %s unwrap.phase.mask eleverr_sr.ht\n",
		 igram,newBaseline, ceos);
	execute(cmd);
	*/
	
	system("rm -r igramd.phase");

	DISPLAY("    Remapping the Height to Ground Range\n");
	sprintf(cmd,"deskew_dem elevation_sr.ht %s elevation.dem\n",ceos);
	execute(cmd);

	DISPLAY("    Remapping the Amplitude to Ground Range\n");
	sprintf(cmd,"deskew_dem elevation.dem %s amplitude_float.img ml.amp\n",ceos);
	execute(cmd);

	DISPLAY("    Remapping the Amplitude to Byte\n");
	sprintf(cmd,"amp2img -l 1x1 -s 1x1 amplitude_float.img amplitude.img\n");
	execute(cmd);

	/*
	DISPLAY("    Creating Colorized phase unwrapping mask (ppm format)\n");
	execute("cp unwrap.ddr unwrap.phase.ddr");
	sprintf(cmd,"las2ppm -m unwrap.phase.mask unwrap_mask.ppm\n");
	execute(cmd);
	*/

	DISPLAY("###   Tandem Interferometric DEM Generation Completed   ###\n");
	exit(0);
}

void execute(char cmd[]) { 
	int errorCode=0;
	printf("\n$$$$$$$$######__TANDEM_IFM__#####$$$$$$$$$\n\
Executing: %s\n\n",cmd); 
	fflush(stdout);
	errorCode=system(cmd);
	if (0!=errorCode)
	{
		fprintf(stderr,
"********** Fatal Error in tandem_ifm! *************\n\
****** Command %s returned error code %i! *********\n",cmd,errorCode);
		exit(errorCode);
	}
}
void DISPLAY(char msg[])
{ 
  printf(" #######################################################\n");
  printf("%s",msg);
  printf(" #######################################################\n");
  fflush(stdout);
}


void give_usage(char *name)
{
 printf("\nUsage: %s igram meta seeds [-s <start>][-d <dem_phase>] \n"
 "\t\t\t[-f <strength>] [-x <x> -y <y>]\n\n",name);
 printf(
"     igram        Non-multilooked Interferogram (.amp, .phase, .ddr)\n"
"     meta         Interferogram metadata (.meta file)\n"
"     seeds        Tie point elevation seed file name\n"
"\n"
"     -s start     If specified, baseline iteration will\n"
"                  start at base.<start> instead of base.0\n"
"\n"
"     -d dem_phase  A LAS phase image, probably created by\n"
"                  the demIFM script.  This is used for dem-guided\n"
"                  phase unwrapping.\n"
"\n"
"     -f strength  Filter the image (using phase_filter)\n"
"                  with the specified power (1.2 to 1.7).\n"
"\n"
"     -x x, -y y: Specify an alternate phase unwrapping start location.\n"
"                 The default is the image center.\n"
"\n"
"     This program takes an interferogram, refines the baseline based\n"
"  on seed points, and generates a ground-range DEM.\n"
"  The seed point file contains triplets of values in the form:\n\n"
"          (int samp, int line, float height (meters) )\n\n\n"
"  This is the other major program in the interferometry package--\n"
"  it is run after one of the register_* scripts (register_ccsd, \n"
"  register_sic,etc)\n"
"ASF Step Interferometry Tools, Version %.2f\n\n\n",VERSION);
 exit(1);
}

