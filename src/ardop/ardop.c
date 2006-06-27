/******************************************************************************
*								              *
*  ardop - ASF Sar Processor (SAR Software Correlator)			      *
*  Parts of this code are Copyright Howard Zebker at Stanford University      *
*  Modifications are Copyright Geophysical Institute, University of Alaska    *
*  Fairbanks. All rights reserved.                                            *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
/*Usage:*/

#include "asf.h"
#include "ardop_defs.h"

/* defined in parse_cla.c */
int parse_cla(int argc,char *argv[], struct INPUT_ARDOP_PARAMS *g);

void give_usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [options] <ifile> <ofile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"      ifile   input signal data file (.D & .L or .raw & .in)\n"
	"      ofile   output file name\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   Argument     Default  Description\n"
	"   ----------------------------------------------------------------\n"
	"   -l first_line   1     First line to process (from 0)\n"
	"   -p patches      8     Number of patches to process (@ 4K lines)\n"
	"   -v valid_lines  3000  Valid output lines per patch\n"
	"   -s skip_samp    0     range samples to skip (of INVALID samps)\n"
	"   -f first_samp   0     1st range samp to process (of VALID samps)\n"
	"   -e 1            0     remove doppler skew from image (flag).\n"
	"   -n num_samps    META  Number of range samples to process\n"
	"                         (Default is read from metadata)\n"
	"   -r output_res   8.0   Desired output azimuth resolution (m)\n"
	"   -c dfile        NO    Read doppler centroid from dfile\n"
	"   -o off_file     NO    Read resampling coeg.fs from off_file\n"
	"   -hamming 	   NO    Use a Hamming window instead of a rectangular one\n"
	"                         for the azimuth reference function weighting\n"
/*	"   -kaiser	   NO    Use a Kaiser window instead of a rectangular one\n"
 *	"                         for the azimuth reference function weighting\n" */
	"   -m CAL_PARAMS   NO    Read the Elevation Angle and Gain vectors from the\n"
	"			 CAL_PARAMS file to correct for the antenna gain\n"
	"   -debug dbg_flg  1     Debug: for options enter -debug 0\n"
	"   -log logfile	   NO	 Allows output to be written to a log file\n"
	"   -quiet	   NO	 Suppresses the output to the essential\n"
	"   -power	   NO	 Creates a power image\n"
	"   -sigma	   NO	 Creates a sigma image\n"
	"   -gamma	   NO	 Creates a gamma image\n"
	"   -beta	   NO	 Creates a beta image\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program creates a SAR image from SAR signal data.\n\n"
	"   The optional switches provide the ability to\n"
	"   override any or all of:\n"
	"        1) the default values given above,\n"
	"        2) parameters read from metadata (.D & .L),\n"
	"        3) parameters read from a parameter file (.raw and .in)\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}


/* If debug flag is set to 0, then show debug usage */

void give_debug_usage(void)
{
	printf("\n\n\n\n"
	"   Welcome to DEBUG!\n\n"
	"   You have entered '-debug 0', which shows the usage for the\n"
	"   debug option. The debug option has been expanded to help\n"
	"   you learn the inner workings of Range-Doppler processing.\n" 
	"   \n"
	"   You use the debugger by entering '-debug dbg_flg'.\n"
	"   'dbg_flg' is any combination of the flags listed below.\n"
	"   As an example, if you want to output a patch of the azimuth \n"
	"   reference function (time domain) and a patch of the raw data, \n"
	"   'dbg_flg' would be 16 + 8192 = 8208\n"
	"   \n"
	"   \n"
	"   Flag  Output ext.    Description\n"
	"   ---- -------------  ------------------------------------------------\n");
	printf("   %i	 *_range_raw_t	Raw data after ingestion but before processing\n",RANGE_RAW_T);
	printf("   %i	 *_range_raw_f	Raw data after range FFT\n",RANGE_RAW_F);
	printf("   %i	 *_range_ref_t	Range reference function in time domain\n",RANGE_REF_T);
	printf("   %i	 *_range_ref_f	Range reference function in freq domain\n",RANGE_REF_F);
	printf("   %i	 *_range_X_f	Data after range compression in freq domain\n",RANGE_X_F);
	printf("   %i	 n/a		If used, *_range_ref_map will be a patch,\n",RANGE_REF_MAP);
	printf("   			if not, *_range_ref_f will be a LUT\n"); 
	printf("   %i	 *_az_raw_t	Data after range compression in time domain\n",AZ_RAW_T);
	printf("   %i	 *_az_raw_f	Data after azimuth FFT in frequency domain\n",AZ_RAW_F);
	printf("   %i	 *_az_mig_f	Data after range cell migration in freq domain\n",AZ_MIG_F);
	printf("   %i	 *_az_ref_t	Azimuth reference function in time domain\n",AZ_REF_T);
	printf("   %i	 *_az_ref_f	Azimuth reference function in freq domain\n",AZ_REF_F);
	printf("   %i	 *_az_X_f	Data after azimuth compression in freq domain\n",AZ_X_F);
	printf("   %i	 *_az_X_t 	Data after azimuth compression in time domain\n",AZ_X_T);

	printf("   %i  n/a		No range compression\n",NO_RANGE);
	printf("   %i  n/a		No range cell migration\n",NO_RCM);
	printf("   %i  n/a		No azimuth compression\n",NO_AZIMUTH);
	printf("   ----	 -------------  ------------------------------------------------\n");
	exit(EXIT_FAILURE);
}

int main (int argc, char *argv [])
{
  int give_usage_action = 0;

  struct INPUT_ARDOP_PARAMS *params_in;

  logflag=quietflag=0;
  params_in = get_input_ardop_params_struct("", "");
  give_usage_action=parse_cla(argc,argv,params_in);
  if (give_usage_action==0) give_usage(argv[0]);
  if (give_usage_action==-1) give_debug_usage();

  system("date");
  printf("Program: ardop\n\n");

  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: ardop\n\n");
  }

  return ardop(params_in);
}
