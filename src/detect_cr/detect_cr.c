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
"detect_cr"

#define ASF_USAGE_STRING \
"[ -chips ] [ -text ] [ -geocode <projection parameter file> ]\n"\
"    <image basename> <corner reflector locations> <peak search file>\n"\
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
"-chips	    Stores the image chip used for determination of amplitude peaks.\n"\
"-text      Stores the image chip as a tab delimated text file.\n"\
"-geocode   Geocodes the individual image chips to evaluate whether the\n"\
"           point targets are in the correct location\n"\

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

#include "asf_nan.h"
#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "fft.h"
#include "fft2d.h"
#include "detect_cr.h"
#include "asf_geocode.h"

#define borderX 80	/* Distances from edge of image to start correlating.*/
#define borderY 80
#define maxDisp 1.8	/* Forward and reverse correlations which differ 
			   by more than this will be deleted.*/
#define minSNR 0.3
#define VERSION 1.0
#define modX(x) ((x+srcSize)%srcSize)  /* Return x, wrapped to [0..srcSize-1] */
#define modY(y) ((y+srcSize)%srcSize)  /* Return y, wrapped to [0..srcSize-1] */

#define MAX_OFFSET_SCANSAR 1000   /* 1000 m = 2x geolocation accuracy ScanSAR */
#define MAX_OFFSET_STANDARD 200   /* 200 m = 2x geolocation accuracy standard beam */
#define CHIP_DEFAULT_SIZE 128

/*Read-only, informational globals:*/
int lines, samples;	     /* Lines and samples of source images. */
int srcSize;                 /* Chip size for analysis */

/* usage - enter here on command-line usage error*/
void usage(char *name)
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
  char szImg[255], buffer[1000], crID[10], szCrList[255], szOut[255], tmp[255];
  char *chips=NULL, *text=NULL, *projFile=NULL;
  int ii;
  float dx_pix, dy_pix, dx_m, dy_m, max_dx_pix, max_dy_pix;
  double lat, lon, elev, posX, posY, magnitude;
  FILE *fpIn, *fpOut;
  meta_parameters *meta, *metaChip;
  project_parameters_t pps;
  projection_type_t proj_type;
  flag_indices_t flags[NUM_FLAGS];

  /* Set all flags to 'not set' */
  for (ii=0; ii<NUM_FLAGS; ii++) {
    flags[ii] = FLAG_NOT_SET;
  }
  
/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Check to see if any options were provided */
  if(checkForOption("-help", argc, argv) != -1) /* Most important */
    help_page();
  flags[f_CHIPS] = checkForOption("-chips", argc, argv);
  flags[f_TEXT] = checkForOption("-text", argc, argv);
  flags[f_GEOCODE] = checkForOption("-geocode", argc, argv);

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  { /* We need to make sure the user specified the proper number of arguments */
    int needed_args = 4;/*command & in_data & in_meta & out_base */
    if(flags[f_CHIPS] != FLAG_NOT_SET) needed_args += 1; /* option */
    if(flags[f_TEXT] != FLAG_NOT_SET) needed_args += 1; /* option */
    if(flags[f_GEOCODE] != FLAG_NOT_SET) needed_args += 2; //option & argument

    /*Make sure we have enough arguments*/
    if(argc != needed_args)
      usage(argv[0]);/*This exits with a failure*/
  }

  /* We must be close to good enough at this point...start filling in fields 
     as needed */
  if(flags[f_GEOCODE] != FLAG_NOT_SET) {
    projFile = (char *) MALLOC(1024*sizeof(char));
    strcpy(projFile, argv[flags[f_GEOCODE] + 1]);
  }
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

  /* Read metadata */
  meta = meta_read(szImg);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;

  /* Allocate memory for flags */
  if(flags[f_CHIPS] != FLAG_NOT_SET ||
     flags[f_GEOCODE] != FLAG_NOT_SET)
    chips = (char *) MALLOC(255*sizeof(char));
  if(flags[f_TEXT] != FLAG_NOT_SET)
    text = (char *) MALLOC(255*sizeof(char));

  /* Handle input and output file */
  fpIn = FOPEN(szCrList, "r");
  fpOut = FOPEN(szOut, "w");
  if (projFile)
    fprintf(fpOut, "ID\tReference Lat\tReference Lon\tElevation\tLine\tSample\t"
	    "Reference x [m]\tReference y [m]\tTarget x [m]\tTarget y [m]\t"
	    "Abs. error [m]\n");
  else
    fprintf(fpOut, "ID\tReference Lat\tReference Lon\tElevation\tLine\tSample\t"
	    "Target line\tTarget sample\tLine offset [m]\tSample offset [m]\t"
	    "Abs. error [m]\n");
  
  /* Establish maximum peak offsets */
  if (projFile) { // chips that need to be geocoded
    if (meta->sar->image_type=='P') { // ScanSAR imagery
      if (meta->projection->type==SCANSAR_PROJECTION) {
	max_dx_pix = MAX_OFFSET_SCANSAR;
	max_dy_pix = MAX_OFFSET_SCANSAR;
      }
      else { // regular standard/finebeam imagery
	max_dx_pix = MAX_OFFSET_STANDARD;
	max_dy_pix = MAX_OFFSET_STANDARD;
      }
    }
    else { // regular standard/finebeam imagery
      max_dx_pix = MAX_OFFSET_STANDARD;
      max_dy_pix = MAX_OFFSET_STANDARD;
    }
  }
  else { // regular case without geocoding
    if (meta->sar->image_type=='P') { // ScanSAR imagery
      if (meta->projection->type==SCANSAR_PROJECTION) {
	max_dx_pix = MAX_OFFSET_SCANSAR / meta->general->x_pixel_size;
	max_dy_pix = MAX_OFFSET_SCANSAR / meta->general->y_pixel_size;
      }
      else { // regular standard/finebeam imagery
	max_dx_pix = MAX_OFFSET_STANDARD / meta->general->x_pixel_size;
	max_dy_pix = MAX_OFFSET_STANDARD / meta->general->y_pixel_size;
      }
    }
    else { // regular standard/finebeam imagery
      max_dx_pix = MAX_OFFSET_STANDARD;
      max_dy_pix = MAX_OFFSET_STANDARD;
    }
  }

  printf ("Going through the list of point targets ...\n\n");

  /* Loop through corner reflector location file */
  while (fgets(buffer, 1000, fpIn))
  {
    srcSize = CHIP_DEFAULT_SIZE;
    sscanf(buffer, "%s\t%lf\t%lf\t%lf", crID, &lat, &lon, &elev);
    meta_get_lineSamp(meta, lat, lon, elev, &posY, &posX);
    if (chips || projFile)
      sprintf(chips, "%s_%s", szImg, crID);
    if (text)
      sprintf(text, "%s_%s", szImg, crID);

    /* Check bounds */
    if (!(outOfBounds(posX, posY, srcSize)))
      {
	// Find peak
	printf("Checking corner reflector %s ...\n", crID);
	findPeak(posX, posY, elev, szImg, &dy_pix, &dx_pix, chips, text, projFile);
	if (projFile) { // chips that needed geocoding
	  latLon2proj(lat, lon, elev, projFile, &posX, &posY);
	  dx_m = dx_pix;
	  dy_m = dy_pix;
	  magnitude = sqrt((posX-dx_m)*(posX-dx_m) + (posY-dy_m)*(posY-dy_m));

	  // Check peak offset
	  if (fabs(posX-dx_m) < max_dx_pix || fabs(posY-dy_m) < max_dy_pix) {
	    fprintf(fpOut,"%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t%10.1f"
		    "\t%10.1f\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posX, posY, dx_m, dy_m, posX-dx_m, posY-dy_m, magnitude);
	    fflush(fpOut);
	  }
	  else { // Do the analysis on a smaller window
	    fprintf(fpOut,"**%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t(%10.1f)"
		    "\t(%10.1f)\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posX, posY, dx_m, dy_m, posX-dx_m, posY-dy_m, magnitude);
	    fflush(fpOut);
	    srcSize = max_dx_pix * 2;
	    printf("   Warning: Corner reflector %s outside the accuracy "
		   "threshold.\n", crID);
	    printf("            Repeating analysis with smaller chip size "
		   "(%ix%i).\n", srcSize, srcSize);
	    findPeak(posX, posY, elev, szImg, &dy_m, &dx_m, chips, text, 
		     projFile);
	    magnitude = sqrt((posX-dx_m)*(posX-dx_m) + (posY-dy_m)*(posY-dy_m));
	    fprintf(fpOut,"%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t%10.1f"
		    "\t%10.1f\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posX, posY, dx_m, dy_m, posX-dx_m, posY-dy_m, magnitude);
	    fflush(fpOut);
	  }
	}
	else { // chips without geocoding
	  dx_m = dx_pix * meta->general->x_pixel_size;
	  dy_m = dy_pix * meta->general->y_pixel_size;
	  magnitude = sqrt(dx_m*dx_m + dy_m*dy_m);
	  
	  // Check peak offset
	  if (fabs(dx_pix) < max_dx_pix || fabs(dy_pix) < max_dy_pix) {
	    fprintf(fpOut,"%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t%10.1f"
		    "\t%10.1f\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posY, posX, posY+dy_pix, posX+dx_pix, dy_m, dx_m, magnitude);
	    fflush(fpOut);
	  }
	  else { // Do the analysis on a smaller window
	    fprintf(fpOut,"**%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t(%10.1f)"
		    "\t(%10.1f)\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posY, posX, posY+dy_pix, posX+dx_pix, dy_m, dx_m, magnitude);
	    fflush(fpOut);
	    srcSize = max_dx_pix * 2;
	    printf("   Warning: Corner reflector %s outside the accuracy "
		   "threshold.\n", crID);
	    printf("            Repeating analysis with smaller chip size "
		   "(%ix%i).\n", srcSize, srcSize);
	    findPeak(posX, posY, elev, szImg, &dy_pix, &dx_pix, chips, text, 
		     projFile);
	    dx_m = dx_pix * meta->general->x_pixel_size;
	    dy_m = dy_pix * meta->general->y_pixel_size;
	    magnitude = sqrt(dx_m*dx_m + dy_m*dy_m);
	    fprintf(fpOut,"%s\t%10.4lf\t%10.4lf\t%8.1lf\t%10.1f\t%10.1f\t%10.1f"
		    "\t%10.1f\t%10.1f\t%10.1f\t%10.1f\n", crID, lat, lon, elev, 
		    posY, posX, posY+dy_pix, posX+dx_pix, dy_m, dx_m, magnitude);
	    fflush(fpOut);
	  }
	}
      }
    else {
      sprintf(tmp, "   WARNING: Corner reflector %s outside the image boundaries!\n",
	      crID);
      printf(tmp);
      fprintf(fpOut, tmp);
    }
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(meta);

  /* Clean up */
  sprintf(tmp, "rm -rf tmp*");
  asfSystem(tmp);

  return(0);
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
bool findPeak(int x, int y, float elev, char *szImg, float *peakX, float *peakY, 
	      char *chip, char *text, char *projFile)
{
  FILE *fpChip, *fpText;
  meta_parameters *meta, *metaChip, *metaText;
  project_parameters_t pps;
  projection_type_t proj_type;
  static float *s=NULL;
  float max=-10000000.0;
  char szChip[255], szText[255], szChipGeo[255];
  int ii, kk, bestX, bestY, lines, samples;
  float bestLocX, bestLocY;
  double lat, lon;
  
  meta = meta_read(szImg); 

  /* Allocate working arrays */
  if (s == NULL) {
    s = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
  }  

  /* At each corner reflector location read in a chunk of the image */
  readSubset(szImg, srcSize, srcSize, x-srcSize/2+1, y-srcSize/2+1, s);
  lines = srcSize;
  samples = srcSize;

  /* Write out chip if name is given */
  if (chip && srcSize==CHIP_DEFAULT_SIZE) {
    metaChip = meta_read(szImg);
    metaChip->general->line_count = lines;
    metaChip->general->sample_count = samples;
    metaChip->general->start_line = y-srcSize/2+1;
    metaChip->general->start_sample = x-srcSize/2+1;
    meta_get_latLon(metaChip, lines/2, samples/2, elev, &lat, &lon);
    metaChip->general->center_latitude = lat;
    metaChip->general->center_longitude = lon;
    
    meta_write(metaChip, chip);
    sprintf(szChip, "%s.img", chip);
    fpChip = FOPEN(szChip, "wb");
    put_float_lines(fpChip, metaChip, 0, lines, s);
    FCLOSE(fpChip);
    meta_free(metaChip);
    printf("Wrote '%s' to disk\n", szChip);
  }

  /* Write out text file for chip if name is given */
  if (text && srcSize==CHIP_DEFAULT_SIZE) {
    metaText = meta_read(szImg);
    metaText->general->line_count = lines;
    metaText->general->sample_count = samples;
    metaText->general->start_line = y-srcSize/2+1;
    metaText->general->start_sample = x-srcSize/2+1;
    meta_get_latLon(metaText, lines/2, samples/2, elev, &lat, &lon);
    metaText->general->center_latitude = lat;
    metaText->general->center_longitude = lon;

    meta_write(metaText, text);
    sprintf(szText, "%s_chip.txt", text);
    fpText = FOPEN(szText, "w");
    for (ii=0; ii<srcSize; ii++) {
      for (kk=0; kk<srcSize; kk++)
	fprintf(fpText, "%12.4f\t", s[ii*srcSize+kk]);
      fprintf(fpText, "\n");
    }
    FCLOSE(fpText);
    meta_free(metaText);
    printf("Wrote '%s' to disk\n", szText);
  }

  // Geocode to the height of the point target before analyzing when projection 
  // parameter file is passed in. Geocoded chip needs to be read in again.
  if (projFile) {
    FREE(s);
    s = NULL;
    sprintf(szChipGeo, "%s_geo", chip);
    metaChip = meta_read(chip);
    parse_proj_args_file(projFile, &pps, &proj_type);
    // Note: Datum hardwired in here. Might want to consider to pass the entire
    // projection block into the function. Projection parameter files include
    // information about datum and spheroid. Certainly required for datum 
    // conversions.
    asf_geocode (&pps, proj_type, 0, RESAMPLE_BILINEAR, elev, WGS84_DATUM,
		 metaChip->general->x_pixel_size, chip, szChipGeo, 0.0);
    meta = meta_read(szChipGeo);
    lines = meta->general->line_count;
    samples= meta->general->sample_count;
    s = (float *)(MALLOC(lines*samples*sizeof(float)));
    sprintf(szChipGeo, "%s_geo.img", chip);    
    fpChip = FOPEN(szChipGeo, "rb");
    get_float_lines(fpChip, metaChip, 0, lines, s);
    FCLOSE(fpChip);
    meta_free(metaChip);
  }

  /* Search for the amplitude peak */
  for (ii=0; ii<lines; ii++)
    for (kk=0; kk<samples; kk++)
      if (s[ii*samples+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*samples+kk];
      }
  
  topOffPeak(s,bestX,bestY,lines,&bestLocX,&bestLocY);
  if (projFile) {
    metaChip = meta_read(szChipGeo);
    *peakX = metaChip->projection->startY +
      metaChip->projection->perY * bestLocX;
    *peakY = metaChip->projection->startX + 
      metaChip->projection->perX * bestLocY;
    meta_free(metaChip);
  }
  else {
    *peakX = bestLocX - srcSize/2;
    *peakY = bestLocY - srcSize/2;
  }

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
