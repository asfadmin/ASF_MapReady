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
"<image> <corner reflector locations> <point target analyis file>\n"\
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
""

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
  char szImg[255], szImage[255], buffer[1000], crID[10], szCrList[255], szOut[255];
  int ii, kk, size, bigSize=oversampling_factor*srcSize;
  int mainlobe_azimuth_min, mainlobe_azimuth_max, mainlobe_range_min;
  int mainlobe_range_max, sidelobe_azimuth_min, sidelobe_azimuth_max;
  int sidelobe_range_min, sidelobe_range_max, peak_line, peak_sample;
  float azimuth_processing_bandwidth, chirp_rate, pulse_duration, sampling_rate;
  float prf, srcPeakX, srcPeakY, bigPeakX, bigPeakY, clutter_power, peak_power, scr;
  float azimuth_resolution, range_resolution, azimuth_pslr, range_pslr;
  float azimuth_window_size, range_window_size;
  float azimuth_profile[bigSize], range_profile[bigSize];
  static complexFloat *s, *t;
  double lat, lon, elev, posX, posY, look_angle;
  FILE *fpIn, *fpOut, *fp, *fpText;
  meta_parameters *meta, *meta_debug, *metaText;
  float *original_amplitude, *amplitude, *phase;
  fcpx *src_fft, *trg_fft;
  int debug=FALSE;
  char *text=NULL;
  int overwrite=TRUE;

  if(argc != 4)
    usage();/*This exits with a failure*/
  
  /* Fetch required arguments */
  strcpy(szImg, argv[argc - 3]);
  strcpy(szCrList,argv[argc - 2]);
  strcpy(szOut,argv[argc - 1]);

  /* DEFAULT VALUES:
     size of region to oversample - 64 pixels	
     mainlobe width factor - 2.6
     sidelobe width factor - 20.0
     maximum oversampling factor - 8
  */

  // Read metadata
  sprintf(szImage, "%s.img", szImg);
  meta = meta_read(szImage);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;
  //printf("line_count = %d, sample_count = %d\n", lines, samples);
  text = (char *) MALLOC(255*sizeof(char));

  // Handle input and output file
  fpIn = FOPEN(szCrList, "r");
  fpOut = FOPEN(szOut, "w");
  fprintf(fpOut, "POINT TARGET ANALYSIS RESULTS\n\n");
  fprintf(fpOut, "CR\tLat\tLon\tElev\tAz peak\tRng peak\tLook\t"
	  "Az res\tRng res\tAz PSLR\tRng PSLR\tSCR\n");
  // RCS needs some more coding
  
  // Loop through corner reflector location file
  while (fgets(buffer, 1000, fpIn))
  {
    if (overwrite) {
      sscanf(buffer, "%s\t%lf\t%lf", crID, &posY, &posX);
      printf("  %s: posX = %.2lf, posY = %.2lf\n", crID, posX, posY);
    }
    else {
      sscanf(buffer, "%s\t%lf\t%lf\t%lf", crID, &lat, &lon, &elev);
      meta_get_lineSamp(meta, lat, lon, elev, &posY, &posX);
      printf("  %s: lat = %.4lf, lon = %.4lf, posX = %.2lf, posY = %.2lf\n", 
	     crID, lat, lon, posX, posY);
    }
	  
    // Check bounds - Get average spectra from chip in range direction
    if (!(outOfBounds(posX, posY, srcSize)))
      {
	
	// READ SUBSET FROM THE IMAGE WITH CORNER REFLECTOR IN THE CENTER
	size = srcSize*srcSize*sizeof(float);
	original_amplitude = (float *) MALLOC(size);
	phase = (float *) MALLOC(size);
	s = (complexFloat *) MALLOC(2*size);
	readComplexSubset(szImage, srcSize, srcSize, posX-srcSize/2, 
			  posY-srcSize/2, s);
	complex2polar(s, srcSize, srcSize, original_amplitude, phase);

	if (debug) { // Store original image for debugging
	  fp = FOPEN("original.img", "wb");
	  size = bigSize*bigSize*sizeof(float);
	  FWRITE(original_amplitude, size, 1, fp);
	  FCLOSE(fp);
	  meta_debug = meta_init(szImage);
	  meta_debug->general->line_count = 
	    meta_debug->general->sample_count = srcSize;
	  meta_debug->general->data_type = REAL32;
	  meta_debug->general->start_line = posY-srcSize/2;
	  meta_debug->general->start_sample = posX-srcSize/2;
	  meta_debug->general->center_latitude = lat;
	  meta_debug->general->center_longitude = lon;
	  meta_write(meta_debug, "original.meta");
	  meta_free(meta_debug);
	}

	// Find amplitude peak in original image chip
	if (!findPeak(original_amplitude, srcSize, &srcPeakX, &srcPeakY)) {
	  fprintf(fpOut, 
		  "   Could not find amplitude peak in original image chip!\n");
	  goto SKIP;
	}

	// Cut out the subset again around the peak to make sure we have data for
	// the analysis
	readComplexSubset(szImage, srcSize, srcSize, posX-srcSize+srcPeakX, 
			  posY-srcSize+srcPeakY, s);
	complex2polar(s, srcSize, srcSize, original_amplitude, phase);
	FREE(phase);
	findPeak(original_amplitude, srcSize, &srcPeakX, &srcPeakY);

	// Determine look angle
	look_angle = 
	  meta_look(meta, srcSize/2, srcSize/2);

	/****************************
        - special ScanSAR case: images are "projected" - need to be rotated back to
          allow analysis in azimuth and range direction (ss_extract.c)
	*********************/

	// BASEBAND THE DATA IN EACH DIMENSION IN THE FREQUENCY DOMAIN

	// Oversample image
	src_fft = forward_fft(s, srcSize, srcSize);
	trg_fft = oversample(src_fft, srcSize, oversampling_factor);

	// Determine azimuth and range window size
	azimuth_processing_bandwidth = 
	  (float) meta->sar->azimuth_processing_bandwidth;
	prf = (float) meta->sar->prf;
	chirp_rate = (float) meta->sar->chirp_rate;
	pulse_duration = (float) meta->sar->pulse_duration;
	sampling_rate = (float) meta->sar->range_sampling_rate;
	azimuth_window_size = azimuth_processing_bandwidth / prf;
	range_window_size = fabs(chirp_rate) * pulse_duration / sampling_rate;
	printf("azimuth window size: %.2f, range window size: %.2f\n",
	       azimuth_window_size, range_window_size);
	asfRequire(azimuth_window_size > 0.0 && azimuth_window_size < 1.0,
		   "azimuth window size out of range (0 to 1)!\n");
	asfRequire(range_window_size > 0.0 && range_window_size < 1.0,
		   "range window size out of range (0 to 1)!\n");
	if (range_window_size < 0.5)
	  range_window_size = 0.5;

	// for ScanSAR both 0.5
	// run debugger to check units are correct!

	// Baseband image in range direction
	//baseband(src_fft, range_window_size);

	/*
	// Transpose matrix to work in azimuth direction
	//transpose(s);

	// Baseband image in azimuth direction
	//	   baseband(s, azimuth_window_size);

	// Transpose matrix back into original orientation
	//transpose(s);

	*/
	t = inverse_fft(trg_fft, bigSize, bigSize);
	amplitude = (float *) MALLOC(sizeof(float)*bigSize*bigSize);
	phase = (float *) MALLOC(sizeof(float)*bigSize*bigSize);
	complex2polar(t, bigSize, bigSize, amplitude, phase);
	FREE(phase);

	if (debug) { // Store oversampled image for debugging
	  fp = FOPEN("oversample.img", "wb");
	  size = bigSize*bigSize*sizeof(float);
	  FWRITE(amplitude, size, 1, fp);
	  FCLOSE(fp);
	  meta_debug = meta_init("oversample.meta");
	  meta_debug->general->line_count = 
	    meta_debug->general->sample_count = bigSize;
	  meta_debug->general->data_type = REAL32;
	  meta_write(meta_debug, "oversample.meta");
	  meta_free(meta_debug);
	}

	// Find the amplitude peak in oversampled image
	if (!findPeak(amplitude, bigSize, &bigPeakX, &bigPeakY)) {
	  fprintf(fpOut, 
		  "   Could not find amplitude peak in oversampled image chip!\n");
	  goto SKIP;
	}
	peak_line = (int)(bigPeakX + 0.5);
	peak_sample = (int)(bigPeakY + 0.5);

	// Write text version of oversampled image
	metaText = meta_read(szImage);
	metaText->general->line_count = 64;
	metaText->general->sample_count = 64;
	metaText->general->start_line = posY-srcSize/2+1;
	metaText->general->start_sample = posX-srcSize/2+1;
	metaText->general->center_latitude = lat;
	metaText->general->center_longitude = lon;
	sprintf(text, "%s_%s", szImg, crID);
	meta_write(metaText, text);
	sprintf(text, "%s_%s_chip.txt", szImg, crID);
	fpText = FOPEN(text, "w");
	for (ii=peak_line-32; ii<peak_line+32; ii++) {
	  for (kk=peak_sample-32; kk<peak_sample+32; kk++)
	    fprintf(fpText, "%12.4f\t", amplitude[ii*bigSize+kk]);
	  fprintf(fpText, "\n");
	}
	FCLOSE(fpText);
	meta_free(metaText);

	// EXTRACTING PROFILES IN AZIMUTH AND RANGE THROUGH PEAK
	for (ii=0; ii<bigSize; ii++) {
	  azimuth_profile[ii] = amplitude[ii*bigSize+peak_sample];
	  range_profile[ii] = amplitude[bigSize*peak_line+ii];
	}

	sprintf(text, "%s_%s_azimuth.txt", szImg, crID);
	fp = FOPEN(text, "w");
	fprintf(fp, "Azimuth profile\n");
	for (ii=0; ii<bigSize; ii++)
	  fprintf(fp, "%.3f\n", azimuth_profile[ii]);
	FCLOSE(fp);
	sprintf(text, "%s_%s_range.txt", szImg, crID);
	fp = FOPEN(text, "w");
	fprintf(fp, "Range profile\n");
	for (ii=0; ii<bigSize; ii++)
	  fprintf(fp, "%.3f\n", range_profile[ii]);
	FCLOSE(fp);

        // FINALLY GET TO THE IMAGE QUALITY PARAMETERS
	clutter_power = 0.0;	

	// Find main lobes in oversampled image
	if (!find_mainlobe(amplitude, azimuth_profile, bigSize, peak_line, 
			   clutter_power, &mainlobe_azimuth_min, 
			   &mainlobe_azimuth_max)) {
	  fprintf(fpOut, "   No mainlobes could be found for %s in azimuth!\n", 
		  crID);
	  goto SKIP;
	}
	//printf("mainlobe azimuth: min = %d, max = %d\n", 
	//       mainlobe_azimuth_min, mainlobe_azimuth_max);
	if (!find_mainlobe(amplitude, range_profile, bigSize, peak_sample, 
			   clutter_power, &mainlobe_range_min, 
			   &mainlobe_range_max)) {
	  fprintf(fpOut, "   No mainlobes could be found for %s in range!\n", crID);
	  goto SKIP;
	}
	//printf("mainlobe range: min = %d, max = %d\n", 
	//       mainlobe_range_min, mainlobe_range_max);

	// Calculate resolution in azimuth and range for profiles 
	if (!calc_resolution(azimuth_profile, mainlobe_azimuth_min, 
			     mainlobe_azimuth_max, peak_line, 
			     meta->general->y_pixel_size, clutter_power, 
			     &azimuth_resolution))
	  fprintf(fpOut, "   Negative azimuth resolution for %s - invalid result!"
		  "\n", crID);
	//printf("azimuth resolution = %.2f\n", azimuth_resolution);
	if (!calc_resolution(range_profile, mainlobe_range_min, 
			     mainlobe_range_max, peak_sample, 
			     meta->general->x_pixel_size, clutter_power, 
			     &range_resolution))
	  fprintf(fpOut, "   Negative range resolution for %s - invalid result!\n",
		  crID);
	//printf("range resolution = %.2f\n", range_resolution);

	// Find peak of original data - thought we had that already: check !!! 

	// Calculate the clutter power
	azimuth_resolution /= meta->general->x_pixel_size;
	range_resolution /= meta->general->y_pixel_size;
	clutter_power = 
	  calc_clutter_power(original_amplitude, srcSize, peak_sample, peak_line, 
			     azimuth_resolution, range_resolution);
	//printf("   Clutter power:      %8.3f\n", clutter_power);

	// Calculate resolution in azimuth and range with estimated clutter power
	if (!calc_resolution(azimuth_profile, mainlobe_azimuth_min, 
			     mainlobe_azimuth_max, peak_line, 
			     meta->general->y_pixel_size, clutter_power, 
			     &azimuth_resolution))
	  fprintf(fpOut, "   Negative azimuth resolution for %s - invalid result!"
		  "\n", crID);
	if (!calc_resolution(range_profile, mainlobe_range_min, 
			     mainlobe_range_max, peak_sample, 
			     meta->general->x_pixel_size, clutter_power, 
			     &range_resolution))
	  fprintf(fpOut, "   Negative range resolution for %s - invalid result!\n",
		  crID);
	//printf("   Azimuth resolution: %.3f\n", azimuth_resolution);
	//printf("   Range resolution: %.3f\n", range_resolution);

	// Find sidelobes in oversampled image and calculate the point-to-sidelobe
	// ratio in azimuth and range direction
	if (find_sidelobe(azimuth_profile, bigSize, 1, peak_line, 
			  mainlobe_azimuth_max, &sidelobe_azimuth_max) &&
	    find_sidelobe(azimuth_profile, bigSize, -1, peak_line, 
			  mainlobe_azimuth_min, &sidelobe_azimuth_min)) {
	  if (!calc_pslr(azimuth_profile, bigSize, peak_line, sidelobe_azimuth_min, 
			 sidelobe_azimuth_max, &azimuth_pslr))
	    //printf("   Azimuth PSLR: %.3f\n", azimuth_pslr);
	    //printf("   No valid PSLR in azimuth could be determined!\n");
	    ;
	}
	else {
	  fprintf(fpOut, "   Problem in finding sidelobes for %s in azimuth - "
		  "invalid PSLR!\n", crID);
	}
	if (find_sidelobe(range_profile, bigSize, 1, peak_sample, 
			  mainlobe_range_max, &sidelobe_range_max) &&
	    find_sidelobe(range_profile, bigSize, -1, peak_sample, 
			  mainlobe_range_min, &sidelobe_range_min)) {
	  if (!calc_pslr(range_profile, bigSize, peak_sample, sidelobe_range_min, 
			 sidelobe_range_max, &range_pslr))
	    //printf("   Range PSLR: %.3f\n", range_pslr);
	    //printf("   No valid PSLR in range could be determined!\n");
	    ;
	}
	else {
	  fprintf(fpOut, "   Problem in finding sidelobes for %s in range -"
		  " invalid PSLR!\n", crID);
	}
	//printf("sidelobe_azimuth: min = %d, max = %d\n",
	//       sidelobe_azimuth_min, sidelobe_azimuth_max);
	//printf("sidelobe_range: min = %d, max = %d\n",
	//       sidelobe_range_min, sidelobe_range_max);

	// Calculate the signal-to-clutter ratio (SCR)
	peak_power = amplitude[peak_line*bigSize+peak_sample] * 
	  amplitude[peak_line*bigSize+peak_sample];
	if (clutter_power>0 && peak_power>clutter_power)
	  scr = 10 * log((peak_power - clutter_power)/clutter_power);
	else 
	  scr = 0.0;
	if (peak_power > 0.0) {
	  peak_power = 10 * log(peak_power);
	  //printf("   Peak power: %.3f\n", peak_power);
	  //printf("   SCR: %.3f\n", scr);
	}
	else 
	  fprintf(fpOut, "   Negative peak power - invalid result!\n");

	FREE(amplitude);
	FREE(original_amplitude);

	// Write values in output files
	fprintf(fpOut, "%s\t%.4lf\t%.4lf\t%.1lf\t%.4lf\t%.3f\t%.3f\t"
		"%.3f\t%.3f\t%.3f\t%.3f\t%.3f\n",
		crID, lat, lon, elev, srcPeakY, srcPeakX, look_angle*R2D, 
		azimuth_resolution, range_resolution, azimuth_pslr, range_pslr, 
		scr);

      SKIP: continue;
      }
    else 
      fprintf(fpOut, "\n   WARNING: Target %s outside the image boundaries!\n", 
	      crID);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(meta);

  return(0);
}

bool calc_pslr(float *profile, int size, int peak, int sidelobe_min, 
	       int sidelobe_max, float *pslr)
{
  int larger_lobe;
  float amplitude, peak_power, side_power;

  // Select the larger of the two sidelobes
  larger_lobe = sidelobe_max;
  if (profile[sidelobe_min] > profile[sidelobe_max])
    larger_lobe = sidelobe_min;

  // Calculate peak power plus amplitude and power of the larger sidelobe
  amplitude = profile[larger_lobe];
  peak_power = profile[peak]*profile[peak];
  side_power = amplitude*amplitude;

  // Calculate PSLR
  if (!FLOAT_EQUIVALENT(side_power, 0.0)) {
    *pslr = 10*log(side_power/peak_power);
    return TRUE;
  }
  else {
    *pslr = -1.0;
    return FALSE;
  }
}

bool find_sidelobe(float *profile, int size, int sign, int peak_pos, int boundary,
		   int *sidelobe)
{
  int i;
  float previous, current;
  float mainlobe_width = 2.6; // Default values from PVS
  bool done = FALSE;
  bool found = FALSE;

  // Search for the first relative minimum in the region immediately outside
  // the mainlobe region
  i = peak_pos + (int)((boundary - peak_pos)*mainlobe_width + 0.5);
  if (i > size)
    i = size - 1;
  else if (i < 0)
    i = 0;
  previous = profile[i];
  while (i<size && i>0 && !done) {
    i += sign;
    current = profile[i];
    done = (current > previous) ? TRUE : FALSE;
    previous = current;
  }

  // Search for the first relative maximum
  if (done) {
    done = FALSE;
    i += sign;
    while (i<size && i>0 && !done) {
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
  *maximum = //s[modr(line,srcSize)*srcSize + modr(sample,srcSize)] *
    s[modr(line,srcSize)*srcSize + modr(sample,srcSize)];
  for (ii=sample; ii<sample+(int)(range_length+0.5)-1; ii++)
    for (kk=line; kk<line+(int)(azimuth_length+0.5)-1; kk++) {
      power = //s[modr(kk,srcSize)*srcSize+modr(ii,srcSize)] *
	s[modr(kk,srcSize)*srcSize+modr(ii,srcSize)];
      *sum += power;
      if (power > *maximum)
	*maximum = power;
    }
}

float calc_clutter_power(float *s, int srcSize, int peak_sample, int peak_line,
			 float azimuth_resolution, float range_resolution)
{
  int na, nr, window_size=7, count, valid=0, box=20, nClutter;
  float sum=0.0, sum_tl, max_tl, sum_bl, max_bl, sum_tr, max_tr, sum_br, max_br;
  float clutter_power;

  na = (int)(box*azimuth_resolution + 0.5);
  nr = (int)(box*range_resolution + 0.5);
  integrate(s, srcSize, peak_sample - nr/2, peak_line - na/2, 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_tl, &max_tl);
  integrate(s, srcSize, peak_sample - nr/2, 
	    (int)(peak_line + na/2 - window_size*azimuth_resolution + 0.5), 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_bl, &max_bl);
  integrate(s, srcSize, 
	    (int)(peak_sample + nr/2 - window_size*range_resolution + 0.5), 
	    peak_line-na/2, window_size*azimuth_resolution,
	    window_size*range_resolution, &sum_tr, &max_tr);
  integrate(s, srcSize, 
	    (int)(peak_sample + nr/2 - window_size*range_resolution + 0.5), 
	    (int)(peak_line - na/2 - window_size*azimuth_resolution + 0.5), 
	    window_size*azimuth_resolution, window_size*range_resolution, 
	    &sum_br, &max_br);
  count = (int)(window_size*azimuth_resolution + 0.5) *
    (int)(window_size*range_resolution + 0.5);
  if (max_tl < sum_tl/count) {
    sum += sum_tl;
    valid++;
  }
  if (max_bl < sum_bl/count) {
    sum += sum_bl;
    valid++;
  }
  if (max_tr < sum_tr/count) {
    sum += sum_tr;
    valid++;
  }
  if (max_br < sum_br/count) {
    sum += sum_br;
    valid++;
  }
  if (valid > 0) {
    nClutter = window_size * window_size * valid;
    clutter_power = sum / nClutter;
  }
  else
    clutter_power = 0.0;

  return clutter_power;
}

bool calc_resolution(float *profile, int mainlobe_min, int mainlobe_max, int max, 
		     float pixel_spacing, float clutter_power, float *resolution)
{
  float pixels, peak_power, error_estimate, uncertainty, res;

  pixels = (float)(mainlobe_max - mainlobe_min + 1) / oversampling_factor;
  res = pixels * pixel_spacing;
  peak_power = profile[max] * profile[max] - clutter_power;
  if (peak_power > clutter_power) 
    uncertainty = sqrt(clutter_power/(peak_power - clutter_power) +
		       ((pixel_spacing/(res*res*
					oversampling_factor*oversampling_factor)) 
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

bool find_mainlobe(float *t, float *profile, int size, int peak, 
		   float clutter_power, int *mainlobe_min, int *mainlobe_max)
{
  bool found_min, found_max;
  int index;
  float cutoff;
  float criteria = -3.0; // dB

  // Check whether we can calculate a reasonable result
  if ((profile[peak]*profile[peak]) > clutter_power)
    cutoff = sqrt(profile[peak]*profile[peak] - clutter_power) * 
      pow(10, criteria/10.0);
  else
    return(FALSE);

  // Search for maximum mainlobe
  found_max = FALSE;
  index = peak;
  while (!found_max) {
    if (index<size && !found_max) {
      if (profile[index] >= cutoff)
	index++;
      else
	found_max = TRUE;
    }
  }
  *mainlobe_max = index - 1;

  // Search for minimum mainlobe
  found_min = FALSE;
  index = peak;
  while (!found_min) {

    if (index>0 && !found_min) {
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

void baseband(fcpx *s, int window_size)
{
  int ii, kk, peak_bin, window_length, right_edge;
  float avg_spectra[srcSize], window_sums[srcSize];
  float peak_sum, previous_sum, sum;

  // Get average spectra from chip in range direction 
  for (ii=0; ii<srcSize; ii++)
    avg_spectra[ii] = 0.0; // Set to zero for initialization
  for (ii=0; ii<srcSize; ii++)
    for (kk=0; kk<srcSize; kk++)
      avg_spectra[kk] += sqrt(s[ii*srcSize+kk][0]*s[ii*srcSize+kk][0] + 
			      s[ii*srcSize+kk][1]*s[ii*srcSize+kk][1]);
  for (ii=0; ii<srcSize; ii++)
    avg_spectra[ii] /= srcSize;
  
  // Get centroid of average spectra in range direction
  window_length = window_size * srcSize;
  peak_bin = 0;
  peak_sum = 0.0;
  for (ii=0; ii<window_length; ii++) {
    peak_sum += avg_spectra[ii];
  }
  window_sums[0] = peak_sum;
  previous_sum = peak_sum;
  for (ii=1; ii<srcSize; ii++) {
    right_edge = (ii + window_length - 1) % srcSize;
    sum = previous_sum - avg_spectra[ii-1];
    if (sum > peak_sum) {
      peak_sum = sum;
      peak_bin = ii;
    }
    window_sums[ii] = sum;
    previous_sum = sum;
  }
  peak_bin = (peak_bin + window_length/2) % srcSize;
  
  /*
  // Rotate image to baseband
  if (peak_bin != 0) {
    for (ii=peak_bin; ii<srcSize; ii++) {
      for (kk=0; kk<srcSize; kk++) {
	tmp[ii*srcSize-peak_bin+kk][0] = s[ii*srcSize+kk][0];
	tmp[ii*srcSize-peak_bin+kk][1] = s[ii*srcSize+kk][1];
      }
      for (ii=0; ii<peak_bin; ii++) {
	tmp[(ii+1)*srcSize-peak_bin+kk+1][0] = s[ii*srcSize+kk][0];
	tmp[(ii+1)*srcSize-peak_bin+kk+1][1] = s[ii*srcSize+kk][1];
      }
    }
    for (ii=0; ii<srcSize*srcSize; ii++) {
      s[ii][0] = tmp[ii][0];
      s[ii][1] = tmp[ii][1];
    }
  }
  */
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


// FindPeak: 
// This version of findPeak just determines the maxium amplitude value and checks
// whether it is actually the peak for the neighborhood
bool findPeak(float *s, int size, float *peakX, float *peakY)
{
  float max=-10000000.0;
  int ii, kk, bestX, bestY;
  float bestLocX, bestLocY;
  
  // Search for the amplitude peak
  for (ii=0; ii<size; ii++)
    for (kk=0; kk<size; kk++)
      if (s[ii*size+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*size+kk];
      }
  
  topOffPeak(s,bestX,bestY,size,&bestLocX,&bestLocY);

  // Output our guess.
  //*peakX = bestLocX - size/2;
  //*peakY = bestLocY - size/2;
  *peakX = bestLocX;
  *peakY = bestLocY;

  return TRUE;
}



// TopOffPeak:
// Given an array of peak values, use trilinear interpolation to determine the 
// exact (i.e. float) top. This works by finding the peak of a parabola which 
// goes though the highest point, and the three points surrounding it.
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
