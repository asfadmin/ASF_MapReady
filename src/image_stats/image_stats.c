/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"image_stats"

#define ASF_USAGE_STRING \
"[ -look ] [ -incidence ] [ -range ]\n"\
"[ -min <value> ] [ -max <value> ] [ -bins <value> ]\n"\
"[ -interval <value> ] <infile> <outfile>\n"\
"\n"\
"Additional option: -help"

#define ASF_DESCRIPTION_STRING \
"Image_stats calculates image statistics that help to analyze the\n"\
"radiometry of an image. The tool creates plot file with average, pixel\n"\
"count and standard deviation for the input image per a value range in\n"\
"look angle, incidence and/or slant range. "

#define ASF_INPUT_STRING \
"<infile>\n"\
"Image file that needs to be radiometrically analyzed. "

#define ASF_OUTPUT_STRING \
"<outfile>\n"\
"Basename of plot file. Extensions (_look.plot, _incid.plot and _range.plot) are\n"\
"appended when used with the respective flags."

#define ASF_OPTIONS_STRING \
"-look		Plot image values versus look angle.\n"\
"-incidence	Plot image values versus incidence angle.\n"\
"-range		Plot image values versus range.\n"\
"-min		Minimum value to be considered for statistics.\n"\
"-max		Maximum value to be considered for statistics.\n"\
"-bins		Number of bins for calculating the statistics (default: 256).\n"\
"-interval	Size of interval for binning the image values\n"\
"		(supersedes setting number of bins).\n"

#define ASF_EXAMPLES_STRING \
"image_stats -look rsat.img analysis"

#define ASF_LIMITATIONS_STRING \
"None known."

#define ASF_SEE_ALSO_STRING \
"offset_test, detect_cr"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"None."

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#include <unistd.h>
#include "asf.h"
#include "asf_meta.h"
#include "lzFetch.h"
#include "jpl_proj.h"
#include "image_stats.h"

#define VERSION 1.0

/* Global variables */
int lines, samples, bins=256;
double min, max, interval=0.0;

/* usage - enter here on command-line usage error*/
void usage(void)
{
  printf("\n"
         "USAGE:\n"
         ASF_NAME_STRING
         " "
         ASF_USAGE_STRING
         "\n\n");
  exit (EXIT_FAILURE);
}

/* help_page - go here when the -help option is specified */
void help_page()
{
  if(system("echo '"
            "\n\n\n"
            "Tool name:\n" ASF_NAME_STRING "\n\n\n"
            "Usage:\n" ASF_USAGE_STRING "\n\n\n"
            "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
            "Input:\n" ASF_INPUT_STRING "\n\n\n"
            "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
            "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
            "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
            "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
            "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
            "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
            "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
            "' | less") != -1)
    exit(EXIT_SUCCESS);
  
  else if(system("echo '"
                 "\n\n\n"
                 "Tool name:\n" ASF_NAME_STRING "\n\n\n"
                 "Usage:\n" ASF_USAGE_STRING "\n\n\n"
                 "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
                 "Input:\n" ASF_INPUT_STRING "\n\n\n"
                 "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
                 "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
                 "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
                 "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
                 "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
                 "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
                 "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
                 "' | more") != -1)
    exit(EXIT_SUCCESS);
  
  else
    printf("\n\n\n"
           "Tool name:\n" ASF_NAME_STRING "\n\n\n"
           "Usage:\n" ASF_USAGE_STRING "\n\n\n"
           "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
           "Input:\n" ASF_INPUT_STRING "\n\n\n"
           "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
           "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
           "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
           "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
           "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
           "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
           "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n");
  exit(EXIT_SUCCESS);
}


/* Main program */
int main(int argc, char *argv[])
{
  FILE *fpLook=NULL, *fpIncid=NULL, *fpRange=NULL, *fpIn=NULL, *fpMask=NULL;
  meta_parameters *meta, *metaMask;
  stateVector stVec;
  int ii, kk, ll, size;
  char inFile[255], metaFile[255], dataFile[255], outLook[255], *outIncid=NULL;
  char outRange[255], outBase[255], outFile[255], *maskFile=NULL; 
  char cmd[255];
  float *bufImage=NULL, *bufMask=NULL, *bufLook=NULL, *bufIncid=NULL, *bufRange=NULL;
  double latitude, longitude, time, doppler, earth_radius, satellite_height, range;
  double look_angle, incidence_angle;
  double line, sample, re=6378144.0, rp=6356754.9, px, py;
  double firstLook=0.0, firstIncid=0.0, firstRange=0.0;
  flag_indices_t flags[NUM_FLAGS];

  /* Set all flags to 'not set' */
  for (ii=0; ii<NUM_FLAGS; ii++) {
    flags[ii] = FLAG_NOT_SET;
  }
  
/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Check to see if any options were provided */
  if (checkForOption("-help", argc, argv) != -1) /* Most important */
    help_page();
  flags[f_LOOK] = checkForOption("-look", argc, argv);
  flags[f_INCIDENCE] = checkForOption("-incidence", argc, argv);
  flags[f_RANGE] = checkForOption("-range", argc, argv);
  flags[f_MIN] = checkForOption("-min", argc, argv);
  flags[f_MAX] = checkForOption("-max", argc, argv);
  flags[f_BINS] = checkForOption("-bins", argc, argv);
  flags[f_INTERVAL] = checkForOption("-interval", argc, argv);

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  { /* We need to make sure the user specified the proper number of arguments */
    int needed_args = 3;/*command & in_base & out_base */
    if (flags[f_LOOK] != FLAG_NOT_SET) needed_args += 1; /* option */
    if (flags[f_INCIDENCE] != FLAG_NOT_SET) needed_args += 1; /* option */
    if (flags[f_RANGE] != FLAG_NOT_SET) needed_args += 1; /* option */
    if (flags[f_MIN] != FLAG_NOT_SET) needed_args += 2; /* option & value */
    if (flags[f_MAX] != FLAG_NOT_SET) needed_args += 2; /* option & value */
    if (flags[f_BINS] != FLAG_NOT_SET) needed_args += 2; /* option & value */
    if (flags[f_INTERVAL] != FLAG_NOT_SET) needed_args += 2; /* option & value */

    /*Make sure we have enough arguments*/
    if (argc != needed_args)
      usage();/*This exits with a failure*/
  }

  /* We must be close to good enough at this point...start filling in fields 
     as needed */
  if (flags[f_MIN] != FLAG_NOT_SET)
    min = atof(argv[flags[f_MIN] + 1]);
  if (flags[f_MAX] != FLAG_NOT_SET)
    max = atof(argv[flags[f_MAX] + 1]);
  if (flags[f_BINS] != FLAG_NOT_SET)
    bins = atoi(argv[flags[f_BINS] + 1]);
  if (flags[f_INTERVAL] != FLAG_NOT_SET)
    interval = atof(argv[flags[f_INTERVAL] + 1]);

  if (flags[f_QUIET] == FLAG_NOT_SET)
    /* display splash screen if not quiet */
    print_splash_screen(argc, argv);
  if (flags[f_LOG] != FLAG_NOT_SET)
    strcpy(logFile, argv[flags[f_LOG] + 1]);
  else
    /* default behavior: log to tmp<pid>.log */
    sprintf(logFile, "tmp%i.log", (int)getpid());
  fLog = FOPEN(logFile, "a");

  /* Fetch required arguments */
  strcpy(inFile,argv[argc - 2]);
  strcpy(outBase,argv[argc - 1]);
/***********************END COMMAND LINE PARSING STUFF***********************/

  create_name(metaFile, inFile, ".meta");
  create_name(dataFile, inFile, ".img");
  
  /* Read metadata */
  meta = meta_read(inFile);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;

  /* Set some values */
  doppler = 0.0;

  if (meta->sar->image_type=='P') {
    /* Calculation in case the imagery is map projected */
    size = BUFSIZE;
    re = meta->projection->re_major;
    rp = meta->projection->re_minor;
    
    /* Prepare input image for reading */
    fpIn = fopenImage(inFile, "rb");
    bufImage = (float *) MALLOC(samples * sizeof(float) * BUFSIZE);

    /* Create a mask file for background fill */
    printf("   Generating mask file ...\n");
    maskFile = (char *) MALLOC(255*sizeof(char));
    sprintf(maskFile, "tmp%i.mask", (int)getpid());
    fpMask = fopenImage(maskFile, "wb");
    bufMask = (float *) MALLOC(samples * sizeof(char) * BUFSIZE);
    for (ii=0; ii<lines; ii++) {
      get_float_line(fpIn, meta, ii, bufImage);
      for (kk=0; kk<samples; kk++) {
	if (bufImage[kk]>0.0)
	  bufMask[kk] = 1;
	else
	  bufMask[kk] = 0;
      }      
      FWRITE(bufMask, sizeof(char), samples, fpMask);
    }

    /* Get the output images set up */
    if (flags[f_LOOK] != FLAG_NOT_SET) {
      fpLook = fopenImage(outLook,"wb");
      bufLook = (float *) MALLOC(samples * sizeof(float) * BUFSIZE);        
    }
    if (flags[f_INCIDENCE] != FLAG_NOT_SET) {
      fpIncid = fopenImage(outIncid,"wb");
      bufIncid = (float *) MALLOC(samples * sizeof(float) * BUFSIZE);
    }        
    if (flags[f_RANGE] != FLAG_NOT_SET) {
      fpRange = fopenImage(outRange,"wb");
      bufRange = (float *) MALLOC(samples * sizeof(float) * BUFSIZE);        
    }
    
    for (ii=0; ii<lines; ii+=size) {
      if ((lines-ii)<BUFSIZE) size = lines-ii;
      for (ll=0; ll<size; ll++) 
	for (kk=0; kk<samples; kk++) {
	  px = meta->projection->startX + meta->projection->perX * kk;
	  py = meta->projection->startY + meta->projection->perY * (ii+ll);
	  proj_to_ll(meta->projection, meta->sar->look_direction, px, py,
		     &latitude, &longitude);
	  latLon2timeSlant(meta, latitude, longitude, &time, &range, &doppler);
	  stVec = meta_get_stVec(meta, time);
	  earth_radius = get_earth_radius(time, stVec, re, rp);
	  satellite_height = get_satellite_height(time, stVec);
	  look_angle = get_look_angle(earth_radius, satellite_height, range);
	  incidence_angle = meta_incid(meta, ii+ll, kk);
	  
	  if (flags[f_LOOK] != FLAG_NOT_SET) 
	    bufLook[kk+ll*samples] = (float) look_angle*R2D;
	  if (flags[f_INCIDENCE] != FLAG_NOT_SET) 
	    bufIncid[kk+ll*samples] = (float) incidence_angle*R2D;
	  if (flags[f_RANGE] != FLAG_NOT_SET) 
	    bufRange[kk+ll*samples] = (float) range;
	}
      
      if (flags[f_LOOK] != FLAG_NOT_SET) 
	FWRITE(bufLook, sizeof(float), samples*size, fpLook);
      if (flags[f_INCIDENCE] != FLAG_NOT_SET) 
	FWRITE(bufIncid, sizeof(float), samples*size, fpIncid);
      if (flags[f_RANGE] != FLAG_NOT_SET)
	FWRITE(bufRange, sizeof(float), samples*size, fpRange);
    }
    if (flags[f_LOOK] != FLAG_NOT_SET) {
      FCLOSE(fpLook);
      FREE(bufLook);
    }
    if (flags[f_INCIDENCE] != FLAG_NOT_SET) {
      FCLOSE(fpIncid);
      FREE(bufIncid);
    }
    if (flags[f_RANGE] != FLAG_NOT_SET) {
      FCLOSE(fpRange);
      FREE(bufRange);
    }
  }
  
  else {
    /* Calculation in case the imagery is in slant range or ground range */
    printf("   Initialization ...\n");
    if (flags[f_LOOK] != FLAG_NOT_SET) {
      sprintf(outLook, "tmp%i.look", (int)getpid());
      fpLook = FOPEN(outLook, "w");
    }
    if (flags[f_INCIDENCE] != FLAG_NOT_SET ||
	meta->general->image_data_type == SIGMA_IMAGE ||
	meta->general->image_data_type == BETA_IMAGE) {
      outIncid = (char *) MALLOC(255 * sizeof(char)); 
      sprintf(outIncid, "tmp%i.incid", (int)getpid());
      fpIncid = FOPEN(outIncid, "w");
    }
    if (flags[f_RANGE] != FLAG_NOT_SET) {
      sprintf(outRange, "tmp%i.range", (int)getpid());
      fpRange = FOPEN(outRange, "w");
    }
    
    for (ll=0; ll<=RES_X; ll++)
      for (kk=0; kk<=RES_Y; kk++) {
	line = ll * lines / RES_Y;
	sample = kk * samples / RES_X;
	time = meta_get_time(meta, line, sample);
	stVec = meta_get_stVec(meta, time);
	earth_radius = get_earth_radius(time, stVec, re, rp);
	satellite_height = get_satellite_height(time, stVec);
	range = get_slant_range(meta, earth_radius, satellite_height, sample);
	look_angle = get_look_angle(earth_radius, satellite_height, range);
	incidence_angle = get_incidence_angle(earth_radius, satellite_height, range);
	
	if (ll==0 && kk==0) {
	  firstLook = look_angle * R2D;
	  firstIncid = incidence_angle * R2D;
	  firstRange = range;
  	  if (flags[f_LOOK] != FLAG_NOT_SET)
            fprintf(fpLook, "Look angle\n");
	  if (flags[f_INCIDENCE] != FLAG_NOT_SET ||
	      meta->general->image_data_type == SIGMA_IMAGE ||
	      meta->general->image_data_type == BETA_IMAGE) 
            fprintf(fpIncid, "Incidence angle\n");
	  if (flags[f_RANGE] != FLAG_NOT_SET)
            fprintf(fpRange, "Range\n"); 
	}

	if (flags[f_LOOK] != FLAG_NOT_SET)
	  fprintf(fpLook, "%.18f %.12f %.12f\n", (float)look_angle*R2D, 
		  line, sample);
	if (flags[f_INCIDENCE] != FLAG_NOT_SET ||
	    meta->general->image_data_type == SIGMA_IMAGE ||
	    meta->general->image_data_type == BETA_IMAGE) 
	  fprintf(fpIncid, "%.18f %.12f %.12f\n", (float)incidence_angle*R2D, 
		  line, sample);
	if (flags[f_RANGE] != FLAG_NOT_SET) 
	  fprintf(fpRange, "%.18f %.12f %.12f\n", (float)range, line, sample);
      }
    
    /* Close files for now */
    if (flags[f_LOOK] != FLAG_NOT_SET) {
      FCLOSE(fpLook);
      FREE(bufLook);
    }
    if (flags[f_INCIDENCE] != FLAG_NOT_SET ||
	meta->general->image_data_type == SIGMA_IMAGE ||
	meta->general->image_data_type == BETA_IMAGE) {
      FCLOSE(fpIncid);
      FREE(bufIncid);
    }
    if (flags[f_RANGE] != FLAG_NOT_SET) {
      FCLOSE(fpRange);
      FREE(bufLook);
    }

    /* Calculate plots */
    if (flags[f_LOOK] != FLAG_NOT_SET) {
      create_name(outFile, outBase, "_look.plot");
      calculate_plot(outLook, dataFile, maskFile, outFile, meta, firstLook);
    }
    if (flags[f_INCIDENCE] != FLAG_NOT_SET) {
      create_name(outFile, outBase, "_incid.plot");
      calculate_plot(outIncid, dataFile, maskFile, outFile, meta, firstIncid);
    }
    if (flags[f_RANGE] != FLAG_NOT_SET) {
      create_name(outFile, outBase, "_range.plot");
      calculate_plot(outRange, dataFile, maskFile, outFile, meta, firstRange);
    }
  }  

  /* Clean up */
  sprintf(cmd, "rm -rf tmp*");
  //system(cmd);

  exit(0);
}

