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
"pta"

#define ASF_USAGE_STRING \
"[ -chip ] [ -text ] <image> <corner reflector locations> <point target analyis file>\n"\
"\n"\
"Additional option: -help"

#define ASF_DESCRIPTION_STRING \
"Detect_cr takes the geolocation information of corner reflectors and\n"\
"searches in the image for the location of an amplitude peak in comparison\n"\
"to the estimate from the metadata."

#define ASF_INPUT_STRING \
"<image>\n"\
"Image file that contains hopefully contains amplitude peaks where\n"\
"corner reflectors are located. \n"\
"<corner reflector locations>\n"\
"Text file with corner reflector information (ID, lat, lon, elevation)."

#define ASF_OUTPUT_STRING \
"<peak search file>\n"\
"File containing original corner reflector information and the\n"\
"determined offset from the amplitude peak search."

#define ASF_OPTIONS_STRING \
"-chip	Stores the image chip used for determination of amplitude peaks.\n"\
"-text  Stores the image chip as a tab delimated text file."

#define ASF_EXAMPLES_STRING \
"detect_cr rsat.img cr.txt reflector.test"

#define ASF_LIMITATIONS_STRING \
"None known."

#define ASF_SEE_ALSO_STRING \
"offset_test, image_stats"

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

#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "fft.h"
#include "fft2d.h"
#include "pta.h"

#define VERSION 1.0
#define modX(x) ((x+srcSize)%srcSize)  /* Return x, wrapped to [0..srcSize-1] */
#define modY(y) ((y+srcSize)%srcSize)  /* Return y, wrapped to [0..srcSize-1] */

/*Read-only, informational globals:*/
int lines, samples;	     /* Lines and samples of source images. */
int srcSize = 64;
int oversampling_factor = 8;

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

/* Start of main progam */

int main(int argc, char *argv[])
{
  char szImg[255], buffer[1000], crID[10], szCrList[255], szOut[255];
  int ii, kk, mX, mY;
  int azimuth_window_size, range_window_size, bigSize=oversampling_factor*srcSize;
  int mainlobe_azimuth_min, mainlobe_azimuth_max, mainlobe_range_min, mainlobe_range_max;
  int sidelobe_azimuth_min, sidelobe_azimuth_max, sidelobe_range_min, sidelobe_range_max;
  int peak_line, peak_sample;
  float azimuth_processing_bandwidth, chirp_rate, pulse_duration, sampling_rate, prf;
  float srcPeakX, srcPeakY, bigPeakX, bigPeakY, clutter_power, peak_power, scr;
  float azimuth_resolution, range_resolution, azimuth_pslr, range_pslr, azimuth_islr;
  float range_islr;
  float azimuth_profile[bigSize], range_profile[bigSize];
  static float *s, *t;
  double lat, lon, elev, posX, posY;
  FILE *fpIn, *fpOut;
  meta_parameters *meta;
  flag_indices_t flags[NUM_FLAGS];

  /* Set all flags to 'not set' */
  for (ii=0; ii<NUM_FLAGS; ii++) {
    flags[ii] = FLAG_NOT_SET;
  }
  
/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Check to see if any options were provided */
  if(checkForOption("-help", argc, argv) != -1) /* Most important */
    help_page();
  flags[f_CHIP] = checkForOption("-chip", argc, argv);
  flags[f_TEXT] = checkForOption("-text", argc, argv);

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  { /* We need to make sure the user specified the proper number of arguments */
    int needed_args = 4;/*command & in_data & in_meta & out_base */
    if(flags[f_CHIP] != FLAG_NOT_SET) needed_args += 1; /* option */
    if(flags[f_TEXT] != FLAG_NOT_SET) needed_args += 1; /* option */

    /*Make sure we have enough arguments*/
    if(argc != needed_args)
      usage();/*This exits with a failure*/
  }

  /* We must be close to good enough at this point...start filling in fields 
     as needed */
  if(flags[f_QUIET] == FLAG_NOT_SET)
    /* display splash screen if not quiet */
    print_splash_screen(argc, argv);
  if(flags[f_LOG] != FLAG_NOT_SET)
    strcpy(logFile, argv[flags[f_LOG] + 1]);
  else
    /* default behavior: log to tmp<pid>.log */
    sprintf(logFile, "tmp%i.log", (int)getpid());
  fLog = FOPEN(logFile, "a");

  /* Fetch required arguments */
  strcpy(szImg,argv[argc - 3]);
  strcpy(szCrList,argv[argc - 2]);
  strcpy(szOut,argv[argc - 1]);
/***********************END COMMAND LINE PARSING STUFF***********************/

  /* DEFAULT VALUES:
     size of region to oversample - 64 pixels	
     mainlobe width factor - 2.6
     sidelobe width factor - 20.0
     maximum oversampling factor - 8
  */

  /* Read metadata */
  meta = meta_read(szImg);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;
 
  /* Handle input and output file */
  fpIn = FOPEN(szCrList, "r");
  fpOut = FOPEN(szOut, "w");
  fprintf(fpOut, "POINT TARGET ANALYSIS RESULTS\n");
  
  /* Loop through corner reflector location file */
  while (fgets(buffer, 1000, fpIn))
  {
    sscanf(buffer, "%s\t%lf\t%lf\t%lf", crID, &lat, &lon, &elev);
    meta_get_lineSamp(meta, lat, lon, elev, &posY, &posX);
    fprintf(fpOut, "\n   Target ID :         %s\n", crID);
	  
    /* Check bounds */	/* Get average spectra from chip in range direction */
    if (!(outOfBounds(posX, posY, srcSize)))
      {

	/* READ SUBSET FROM THE IMAGE WITH CORNER REFLECTOR IN THE CENTER */
	s = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
	t = (float *)(MALLOC(srcSize*srcSize*sizeof(float)*
			     oversampling_factor*oversampling_factor));
	readSubset(szImg, srcSize, srcSize, posX-srcSize/2+1, posY-srcSize/2+1, s);

	/****************************
        - special ScanSAR case: images are "projected" - need to be rotated back to
          allow analysis in azimuth and range direction (ss_extract.c)
        - same approach for geocoded imagery ???
	*********************/

	/* BASEBAND THE DATA IN EACH DIMENSION IN THE FREQUENCY DOMAIN */

	/* Determine azimuth and range window size */
	azimuth_processing_bandwidth = (float) meta->sar->azimuth_processing_bandwidth;
	prf = (float) meta->sar->prf;
	chirp_rate = (float) meta->sar->chirp_rate;
	pulse_duration = (float) meta->sar->pulse_duration;
	sampling_rate = (float) meta->sar->range_sampling_rate;
	azimuth_window_size = (int) (azimuth_processing_bandwidth / prf + 0.5);
	range_window_size = (int) (fabs(chirp_rate) * pulse_duration / sampling_rate + 0.5);
	/* for ScanSAR both 0.5 */
	/* run debugger to check units are correct! */

	/* Determine dimensions for FFT, initialize and get into frequency domain */
	mX = (int)(log(srcSize)/log(2.0)+0.5); /* Dimensions for FFT */
	mY = (int)(log(srcSize)/log(2.0)+0.5); /* in x and y */
       	fft2dInit(mY,mX); /* Initialization */
	rfft2d(s,mY,mX); /* FFT chip */

	/* Baseband image in range direction *
	   baseband(s, range_window_size);*/

	/* Transpose matrix to work in azimuth direction */
	transpose(s);

	/* Baseband image in azimuth direction *
	   baseband(s, azimuth_window_size);*/

	/* Oversample the basebanded data by a factor of 8:1 */
	for (kk=0; kk<srcSize/2; kk++)
	  for (ii=0; ii<srcSize/2; ii++)
	    t[ii*srcSize+kk] = s[ii*srcSize+kk];
	for (kk=0; kk<srcSize/2; kk++)
	  for (ii=0; ii<srcSize/2; ii++)
	    t[(bigSize-srcSize+ii)*srcSize+kk] = s[ii*srcSize+kk];
	for (kk=srcSize/2+1; kk<srcSize; kk++)
	  for (ii=srcSize/2+1; ii<srcSize; ii++)
	    t[ii*srcSize+bigSize-srcSize+kk] = s[ii*srcSize+kk];
	for (kk=srcSize/2+1; kk<srcSize; kk++)
	  for (ii=srcSize/2+1; ii<srcSize; ii++)
	    t[(bigSize-srcSize+ii)*srcSize+bigSize-srcSize+kk] = s[ii*srcSize+kk];

	/* Transpose matrix back into original orientation */
	transpose(t);

	/* Get everything back into time domain */
	rifft2d(t,mY,mX); /* Inverse FFT */

	/* FIND THE AMPLITUDE PEAK IN THE ORIGINAL AND OVERSAMPLED IMAGE CHIP */
	if (!findPeak(s, srcSize, &srcPeakX, &srcPeakY)) {
	  fprintf(fpOut, "   Could not find amplitude peak in original image chip!\n");
	  goto SKIP;
	}
	if (!findPeak(t, bigSize, &bigPeakX, &bigPeakY)) {
	  fprintf(fpOut, 
		  "   Could not find amplitude peak in oversampled image chip!\n");
	  goto SKIP;
	}
	peak_line = (int)(bigPeakX + 0.5);
	peak_sample = (int)(bigPeakY + 0.5);

	/* EXTRACTING PROFILES IN AZIMUTH AND RANGE THROUGH PEAK */
	for (ii=0; ii<bigSize; ii++) {
	  azimuth_profile[ii] = t[ii*bigSize+peak_sample];
	  range_profile[ii] = t[bigSize*peak_line+ii];
	}

        /* FINALLY GET TO THE IMAGE QUALITY PARAMETERS */
	clutter_power = 0.0;	

	/* Find main lobes in oversampled image */
	if (!find_mainlobe(t, azimuth_profile, bigSize, peak_line, clutter_power, 
			  &mainlobe_azimuth_min, &mainlobe_azimuth_max)) {
	  fprintf(fpOut, "   No mainlobes could be found in azimuth profile!\n");
	  goto SKIP;
	}
	if (!find_mainlobe(t, range_profile, bigSize, peak_sample, clutter_power,
			  &mainlobe_range_min, &mainlobe_range_max)) {
	  fprintf(fpOut, "   No mainlobes could be found in range profile!\n");
	  goto SKIP;
	}

	/* Calculate resolution in azimuth and range for profiles */
	if (!calc_resolution(azimuth_profile, mainlobe_azimuth_min, mainlobe_azimuth_max,
			     peak_line, bigSize, clutter_power, &azimuth_resolution))
	  fprintf(fpOut, "   Negative azimuth resolution - invalid result!\n");
	if (!calc_resolution(range_profile, mainlobe_range_min, mainlobe_range_max,
			     peak_sample, bigSize, clutter_power, &range_resolution))
	  fprintf(fpOut, "   Negative range resolution - invalid result!\n");

	/* Find peak of original data - thought we had that already: check !!! */

	/* Calculate the clutter power */
	azimuth_resolution /= lines;
	range_resolution /= samples;
	clutter_power = calc_clutter_power(s, srcSize, posX, posY, 
					   azimuth_resolution, range_resolution);
	fprintf(fpOut, "   Clutter power:      %8.3f\n", clutter_power);

	/* Calculate the resolution in azimuth and range with estimated clutter power */
	if (!calc_resolution(azimuth_profile, mainlobe_azimuth_min, mainlobe_azimuth_max,
			     peak_line, bigSize, clutter_power, &azimuth_resolution))
	  fprintf(fpOut, "   Negative azimuth resolution - invalid result!\n");
	if (!calc_resolution(range_profile, mainlobe_range_min, mainlobe_range_max,
			     peak_sample, bigSize, clutter_power, &range_resolution))
	  fprintf(fpOut, "   Negative range resolution - invalid result!\n");
	fprintf(fpOut, "   Azimuth resolution: %8.3f\n", azimuth_resolution);
	fprintf(fpOut, "   Range resolution:   %8.3f\n", range_resolution);

	/* Find sidelobes in oversampled image and calculate the point-to-sidelobe
	   ratio in azimuth and range direction */
	if (find_sidelobe(azimuth_profile, bigSize, 1, peak_line, mainlobe_azimuth_max,
			  &sidelobe_azimuth_max) &&
	    find_sidelobe(azimuth_profile, bigSize, -1, peak_line, mainlobe_azimuth_min,
			  &sidelobe_azimuth_min)) {
	  if (calc_pslr(azimuth_profile, bigSize, peak_line, sidelobe_azimuth_min, 
			sidelobe_azimuth_max, &azimuth_pslr))
	    fprintf(fpOut, "   Azimuth PSLR:       %8.3f\n", azimuth_pslr);
	  else
	    fprintf(fpOut, "   No valid PSLR in azimuth could be determined!\n");
	}
	else {
	  fprintf(fpOut, "   Problem in finding sidelobes in azimuth - invalid PSLR!\n");
	}
	if (find_sidelobe(range_profile, bigSize, 1, peak_line, mainlobe_range_max,
			  &sidelobe_range_max) &&
	    find_sidelobe(range_profile, bigSize, -1, peak_line, mainlobe_range_min,
			  &sidelobe_range_min)) {
	  if (calc_pslr(range_profile, bigSize, peak_sample, sidelobe_range_min, 
			sidelobe_range_max, &range_pslr))
	    fprintf(fpOut, "   Range PSLR:         %8.3f\n", range_pslr);
	  else
	    fprintf(fpOut, "   No valid PSLR in range could be determined!\n");
	}
	else {
	  fprintf(fpOut, "   Problem in finding sidelobes in range - invalid PSLR!\n");
	}

	/* Calculate the integrated sidelobe ratio (ISLR) */
	if (calc_islr())
	  fprintf(fpOut, "   Azimuth ISLR:       %8.3f\n", azimuth_islr);
	else
	  fprintf(fpOut, "   No valid ISLR in azimuth could be determined!\n");
	if (calc_islr())
	  fprintf(fpOut, "   Range ISLR:         %8.3f\n", range_islr);
	else
	  fprintf(fpOut, "   No valid ISLR in range could be determined!\n");

	/* Calculate the signal-to-clutter ratio (SCR) */
	peak_power = t[peak_line*bigSize+peak_sample] * t[peak_line*bigSize+peak_sample];
	if (clutter_power>0 && peak_power>clutter_power)
	  scr = 10 * log((peak_power - clutter_power)/clutter_power);
	else 
	  scr = 0.0;
	if (peak_power > 0.0) {
	  peak_power = 10 * log(peak_power);
	  fprintf(fpOut, "   Peak power:         %8.3f\n", peak_power);
	  fprintf(fpOut, "   SCR:                %8.3f\n", scr);
	}
	else 
	  fprintf(fpOut,"   Negative peak power - invalid result!\n");

      SKIP: continue;
      }
    else 
      fprintf(fpOut, "\n   WARNING: Target %s outside the image boundaries!\n", crID);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(meta);

  return(0);
}

/*      call islr_1d(peak_line, azimuth_cut, mwidth, swidth,
     &    clutter_power, n_clutter, mainlobes(1, 1), mainlobes(1, 2),
     &    box, OVERSAMP_SIZE, 'azimuth', azimuth_islr, err_azimuth_islr)

      subroutine islr_1d(peak, cut, mwidth, swidth, clutter_power,
     &    n_clutter, mainlobe_start, mainlobe_end, box, N, id, islr_db,
     &    err_islr_db)*/

bool calc_islr()
{
  //  integrate
}

bool calc_pslr(float *profile, int size, int peak, int sidelobe_min, int sidelobe_max,
	       float *pslr)
{
  int larger_lobe;
  float amplitude, peak_power, side_power;

  /* Select the larger of the two sidelobes */
  larger_lobe = sidelobe_max;
  if (profile[sidelobe_min] > profile[sidelobe_max])
    larger_lobe = sidelobe_min;

  /* Calculate peak power plus amplitude and power of the larger sidelobe */
  amplitude = profile[larger_lobe];
  peak_power = profile[peak]*profile[peak];
  side_power = amplitude*amplitude;

  /* Calculate PSLR */
  if (FLOAT_EQUIVALENT(side_power, 0.0)) {
    *pslr = 10*log(side_power/peak_power);
    return TRUE;
  }
  else {
    *pslr = -1.0;
    return FALSE;
  }
}

bool find_sidelobe(float *profile, int size, int sign, int peak_line, int boundary,
		   int *sidelobe)
{
  int i;
  float previous, current;
  float mainlobe_width = 2.6; /* Default values from PVS */
  bool done = FALSE;
  bool found = FALSE;

  /* Search for the first relative minimum in the region immediately outside
     the mainlobe region */
  i = peak_line + (int)((boundary - peak_line)*mainlobe_width + 0.5);
  if (i > size)
    i = size;
  else if (i < 1)
    i = 1;
  previous = profile[i];
  while (i<size && i>1 && !done) {
    i += sign;
    current = profile[i];
    done = (current > previous) ? TRUE : FALSE;
    previous = current;
  }

  /* Search for the first relative maximum */
  if (done) {
    done = FALSE;
    i += sign;
    while (i<size && i>1 && !done) {
      current = profile[i];
      if (current < previous)
	done = TRUE;
      else {
	previous = current;
	i += sign;
      }
    }
    found = done;
  }
  *sidelobe = i - sign;

  return found;
}

int modr(int number, int base)
{
  int tmp = number % base;

  if (tmp < 0)
    tmp += base;

  return tmp;
}

void integrate(float *s, int srcSize, int sample, int line, float azimuth_length, 
	       float range_length, float *sum, float *maximum)
{
  int ii, kk;
  float power;

  *sum = 0.0;
  *maximum = s[modr(sample,srcSize)*srcSize + modr(line,srcSize)] *
    s[modr(sample,srcSize)*srcSize + modr(line,srcSize)];
  /* check indices !!! */
  for (ii=sample; ii<sample+(int)(range_length+0.5)-1; ii++)
    for (kk=line; kk<line+(int)(azimuth_length+0.5)-1; kk++) {
      power = s[modr(ii,srcSize)*srcSize+modr(kk,srcSize)] *
	s[modr(ii,srcSize)*srcSize+modr(kk,srcSize)];
      *sum += power;
      if (power > *maximum)
	*maximum = power;
    }
}

float calc_clutter_power(float *s, int srcSize, int peak_sample, int peak_line,
			 float azimuth_resolution, float range_resolution)
{
  int na, nr, window_size, count, valid=0, box=20, nClutter;
  float sum=0.0, sum_tl, max_tl, sum_bl, max_bl, sum_tr, max_tr, sum_br, max_br;
  float clutter_power;

  na = (int)(box*azimuth_resolution + 0.5);
  nr = (int)(box*range_resolution + 0.5);
  window_size = 7;
  integrate(s, srcSize, peak_sample - nr/2, peak_line - na/2, 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_tl, &max_tl);
  integrate(s, srcSize, peak_sample - nr/2, 
	    (int)(peak_line + na/2 - window_size*azimuth_resolution + 0.5), 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_bl, &max_bl);
  integrate(s, srcSize, (int)(peak_sample + nr/2 - window_size*range_resolution + 0.5), 
	    peak_line-na/2, window_size*azimuth_resolution,
	    window_size*range_resolution, &sum_tr, &max_tr);
  integrate(s, srcSize, (int)(peak_sample + nr/2 - window_size*range_resolution + 0.5), 
	    (int)(peak_line - na/2 - window_size*azimuth_resolution + 0.5), 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_br, &max_br);
  count = (int)(window_size*azimuth_resolution + 0.5) *
    (int)(window_size*range_resolution + 0.5);
  if (max_tl < (8*sum_tl/count)) {
    sum += sum_tl;
    valid++;
  }
  if (max_bl < (8*sum_bl/count)) {
    sum += sum_bl;
    valid++;
  }
  if (max_tr < (8*sum_tr/count)) {
    sum += sum_tr;
    valid++;
  }
  if (max_br < (8*sum_br/count)) {
    sum += sum_br;
    valid++;
  }
  if (valid > 0) {
    nClutter = window_size*window_size*valid;
    clutter_power = sum / nClutter;
  }
  else
    clutter_power = 0.0;

  return clutter_power;
}



bool calc_resolution(float *profile, int mainlobe_min, int mainlobe_max, int max, 
		     int nCells, float clutter_power, float *resolution)
{
  int nPixels;
  float peak_power, error_estimate, uncertainty, res;

  nPixels = (mainlobe_max - mainlobe_min + 1) / oversampling_factor;
  res = nPixels * nCells;
  peak_power = profile[max] * profile[max] - clutter_power;
  if (peak_power > clutter_power) 
    uncertainty = sqrt(clutter_power/(peak_power - clutter_power) +
		       ((nCells/(res*res*oversampling_factor*oversampling_factor)) 
			/ 6.0));
  else
    uncertainty = -1.0;
  error_estimate = 100.0 * uncertainty;
  if (error_estimate > 0) {
    *resolution = res;
    return TRUE;
  }
  else {
    *resolution = -1.0;
    return FALSE;
  }
}

bool find_mainlobe(float *t, float *profile, int size, int peak, float clutter_power, 
		   int *mainlobe_min, int *mainlobe_max)
{
  bool found_min, found_max;
  int index;
  float cutoff;
  float criteria = -3.0; /* dB */

  /* Check whether we can calculate a reasonable result */
  if ((profile[peak]*profile[peak]) > clutter_power)
    cutoff = sqrt(profile[peak]*profile[peak] - clutter_power) * 
      pow(10, criteria/10.0);
  else
    return(FALSE);

  /* Search for maximum mainlobe */
  found_max = FALSE;
  index = peak;
  while (!found_max) {
    if (index<size && !found_max) {
      if (profile[index] > cutoff)
	index++;
      else
	found_max = TRUE;
    }
  }
  *mainlobe_max = index - 1;

  /* Search for minimum mainlobe */
  found_min = FALSE;
  index = peak;
  while (!found_min) {
    if (index>1 && !found_min) {
      if (profile[index] > cutoff)
	index--;
      else 
	found_min = TRUE;
    }
  }
  *mainlobe_min = index + 1;

  if (!found_min && !found_max)
    return(FALSE);

  return(TRUE);
}

void baseband(float *s, int window_size)
{
  int ii, kk, peak_bin, window_length, right_edge;
  float avg_spectra[srcSize], window_sums[srcSize], tmp[srcSize*srcSize];
  float peak_sum, previous_sum, sum;

  /* Get average spectra from chip in range direction */
  for (ii=0; ii<srcSize; ii++)
    avg_spectra[ii] = 0.0; /* Set to zero for initialization */
  for (kk=0; kk<srcSize; kk++)
    for (ii=0; ii<srcSize; ii++) 
      avg_spectra[ii] += s[ii*srcSize+kk];
  for (ii=0; ii<srcSize; ii++)
    avg_spectra[ii] /= srcSize;
  
  /* Get centroid of average spectra in range direction */
  window_length = window_size * srcSize;
  peak_bin = 0;
  peak_sum = 0.0;
  for (ii=0; ii<window_length; ii++)
    peak_sum += avg_spectra[ii];
  window_sums[0] = peak_sum;
  previous_sum = peak_sum;
  for (ii=1; ii<srcSize; ii++) {
    right_edge = (ii + window_length - 2) % srcSize + 1;
    sum = previous_sum - avg_spectra[ii-1];
    if (sum > peak_sum) {
      peak_sum = sum;
      peak_bin = ii;
    }
    window_sums[ii] = sum;
    previous_sum = sum;
  }
  peak_bin = (peak_bin + window_length/2 -1) % srcSize + 1;
  
  /* Rotate image to baseband */
  if (peak_bin != 1) {
    for (kk=0; kk<srcSize; kk++) {
      for (ii=peak_bin; ii<srcSize; ii++)
	tmp[ii*srcSize-peak_bin+kk+1] = s[ii*srcSize+kk];
      for (ii=0; ii<peak_bin; ii++)
	tmp[(ii+1)*srcSize-peak_bin+kk+1] = s[ii*srcSize+kk];
    }
    for (ii=0; ii<srcSize*srcSize; ii++)
      s[ii] = tmp[ii];
  }
}

void transpose(float *s)
{
  int ii, kk;
  float temp;

  for (ii=0; ii<srcSize-1; ii++)
    for (kk=ii+1; kk<srcSize; kk++) {
      temp = s[kk*srcSize+ii];
      s[kk*srcSize+ii] = s[ii*srcSize+kk];
      s[ii*srcSize+kk] = temp;
    } 
}

bool outOfBounds(int x, int y, int srcSize)
{
  if (x - srcSize/2 + 1 < 0) return TRUE;
  if (y - srcSize/2 + 1 < 0) return TRUE;
  if (x + srcSize/2  >= samples) return TRUE;
  if (y + srcSize/2  >= lines) return TRUE;
  return FALSE;
}


/*FindPeak: 
  This version of findPeak just determines the maxium amplitude value and checks
  whether it is actually the peak for the neighborhood.
*/
bool findPeak(float *s, int size, float *peakX, float *peakY)
{
  float max=-10000000.0;
  int ii, kk, bestX, bestY;
  float bestLocX, bestLocY;
  
  /* Search for the amplitude peak */
  for (ii=0; ii<size; ii++)
    for (kk=0; kk<size; kk++)
      if (s[ii*size+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*size+kk];
      }
  
  topOffPeak(s,bestX,bestY,size,&bestLocX,&bestLocY);
  

  /* Output our guess. */
  *peakX = bestLocX - size/2;
  *peakY = bestLocY - size/2;

  return TRUE;
}



/* TopOffPeak:
   Given an array of peak values, use trilinear interpolation to determine the 
   exact (i.e. float) top. This works by finding the peak of a parabola which 
   goes though the highest point, and the three points surrounding it.
*/
void topOffPeak(float *peaks,int i,int j,int maxI,float *di,float *dj)
{
        float a,b,c,d;
        a=peaks[modY(j)*maxI+modX(i-1)];
        b=peaks[modY(j)*maxI+modX(i)];
        c=peaks[modY(j)*maxI+modX(i+1)];
        d=4*((a+c)/2-b);
        if (d!=0)
                *di=i+(a-c)/d;
        else *di=i;
        a=peaks[modY(j-1)*maxI+modX(i)];
        b=peaks[modY(j)*maxI+modX(i)];
        c=peaks[modY(j+1)*maxI+modX(i)];
        d=4*((a+c)/2-b);
        if (d!=0)
                *dj=j+(a-c)/d;
        else *dj=j;
}
