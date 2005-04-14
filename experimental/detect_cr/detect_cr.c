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
"[ -chips ] [ chip_size <value> ] [ -text ] [ -profile ] <image> "\
"<corner reflector locations> <peak search file>\n"\
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
"-chip_size Defines the size of the chip used for the correlation (default: 128).\n"\
"-text      Stores the image chip as a tab delimated text file.\n"\
"-profile   Stores line and sample profiles through the peak in a text file."

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
#include "detect_cr.h"

#define borderX 80	/* Distances from edge of image to start correlating.*/
#define borderY 80
#define maxDisp 1.8	/* Forward and reverse correlations which differ 
			   by more than this will be deleted.*/
#define minSNR 0.3
#define VERSION 1.0
#define modX(x) ((x+srcSize)%srcSize)  /* Return x, wrapped to [0..srcSize-1] */
#define modY(y) ((y+srcSize)%srcSize)  /* Return y, wrapped to [0..srcSize-1] */


/*Read-only, informational globals:*/
int lines, samples;	     /* Lines and samples of source images. */
int srcSize=128;

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
  char szImg[255], buffer[1000], crID[10], szCrList[255], szOut[255], tmp[255];
  char *chips=NULL, *text=NULL, *profile=NULL;
  int ii;
  float dx_pix, dy_pix, dx_m, dy_m;
  double lat, lon, elev, posX, posY, magnitude;
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
  flags[f_CHIPS] = checkForOption("-chips", argc, argv);
  flags[f_CHIP_SIZE] = checkForOption("-chip_size", argc, argv);
  flags[f_TEXT] = checkForOption("-text", argc, argv);
  flags[f_PROFILE] = checkForOption("-profile", argc, argv);

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  { /* We need to make sure the user specified the proper number of arguments */
    int needed_args = 4;/*command & in_data & in_meta & out_base */
    if(flags[f_CHIPS] != FLAG_NOT_SET) needed_args += 1; /* option */
    if(flags[f_CHIP_SIZE] != FLAG_NOT_SET) needed_args += 2; /* option */
    if(flags[f_TEXT] != FLAG_NOT_SET) needed_args += 1; /* option */
    if(flags[f_PROFILE] != FLAG_NOT_SET) needed_args += 1; /* option */

    /*Make sure we have enough arguments*/
    if(argc != needed_args)
      usage();/*This exits with a failure*/
  }

  /* We must be close to good enough at this point...start filling in fields 
     as needed */
  if (flags[f_CHIP_SIZE] != FLAG_NOT_SET)
    srcSize = atoi(argv[flags[f_CHIP_SIZE] + 1]);
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

  /* Check chip size */ 
  if (srcSize < 32) {
    printf("   Chosen chip size too small! Chip size set to minimal chip size of 32");
    srcSize = 32;
  }

  /* Allocate memory for flags */
  if(flags[f_CHIPS] != FLAG_NOT_SET)
    chips = (char *) MALLOC(255*sizeof(char));
  if(flags[f_TEXT] != FLAG_NOT_SET)
    text = (char *) MALLOC(255*sizeof(char));
  if(flags[f_PROFILE] != FLAG_NOT_SET)
    profile = (char *) MALLOC(255*sizeof(char));

  /* Handle input and output file */
  fpIn = FOPEN(szCrList, "r");
  fpOut = FOPEN(szOut, "w");
  fprintf(fpOut, "ID\tReference Lat\tReference Lon\tElevation\tReference line"
	  "\tReference sample\tLine offset [m]\tSample offset [m]\tMagnitude [m]"
	  "\tLine offset [pix]\tSample offset [pix]\n");
  
  /* Loop through corner reflector location file */
  while (fgets(buffer, 1000, fpIn))
  {
    sscanf(buffer, "%s\t%lf\t%lf\t%lf", crID, &lat, &lon, &elev);
    meta_get_lineSamp(meta, lat, lon, elev, &posY, &posX);
    if (chips)
      sprintf(chips, "%s", crID);
    if (text)
      sprintf(text, "%s", crID);
    if (profile)
      sprintf(profile, "%s", crID);

    /* Check bounds */
    if (!(outOfBounds(posX, posY, srcSize)))
      {
	/* Find peak */ 
	findPeak(posX+0.5, posY+0.5, szImg, &dx_pix, &dy_pix, chips, text, profile);
	dx_m = dx_pix * meta->general->x_pixel_size;
	dy_m = dy_pix * meta->general->y_pixel_size;
	magnitude = sqrt(dx_m*dx_m + dy_m*dy_m);
	fprintf(fpOut,"%s\t%10.4lf\t%10.4lf\t%8.0lf\t%10.1f\t%10.1f\t%10.2f\t%10.2f"
		"\t%10.2f\t%10.2f\t%10.2f\n",
		crID, lat, lon, elev, posY, posX, dy_m, dx_m, magnitude, dy_pix, dx_pix);
	fflush(fpOut);
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
  system(tmp);

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
bool findPeak(int x, int y, char *szImg, float *peakX, float *peakY, 
	      char *chip, char *text, char *profile)
{
  FILE *fpChip, *fpText, *fpProfile;
  meta_parameters *meta, *metaChip, *metaText;
  static float *s=NULL;
  float max=-10000000.0;
  char szChip[255], szText[255], szProfile[255];
  int size, ii, kk, bestX, bestY;
  float bestLocX, bestLocY;
  
  meta = meta_read(szImg); 

  /* Allocate working arrays */
  if (s == NULL) {
    s = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
  }  

  /* At each corner reflector location read in a chunk of the image */
  readSubset(szImg, srcSize, srcSize, x-srcSize/2+1, y-srcSize/2+1, s);
  
  /* Write out chip if name is given */
  if (chip) {
    metaChip = meta_copy(meta);
    metaChip->general->line_count = srcSize;
    metaChip->general->sample_count = srcSize;
    meta_write(metaChip, chip);
    sprintf(szChip, "%s.img", chip);
    fpChip = FOPEN(szChip, "wb");
    size = srcSize*srcSize*sizeof(float);
    FWRITE(s, size, 1, fpChip);
    FCLOSE(fpChip);
  }

  /* Write out text file for chip if name is given */
  if (text) {
    metaText = meta_copy(meta);
    metaText->general->line_count = srcSize;
    metaText->general->sample_count = srcSize;
    meta_write(metaText, text);
    sprintf(szText, "%s_chip.txt", text);
    fpText = FOPEN(szText, "w");
    for (ii=0; ii<srcSize; ii++) {
      for (kk=0; kk<srcSize; kk++)
	fprintf(fpText, "%12.4f\t", s[ii*srcSize+kk]);
      fprintf(fpText, "\n");
    }
    FCLOSE(fpText);
  }

  /* Search for the amplitude peak */
  for (ii=0; ii<srcSize; ii++)
    for (kk=0; kk<srcSize; kk++)
      if (s[ii*srcSize+kk] > max) {
	bestX = ii;
	bestY = kk;
	max = s[ii*srcSize+kk];
      }
  
  topOffPeak(s,bestX,bestY,srcSize,&bestLocX,&bestLocY);
  
  /* Output our guess. */
  *peakX = bestLocX - srcSize/2;
  *peakY = bestLocY - srcSize/2;

  /* Write out text file for line/sample profiles */
  if (profile) {
    sprintf(szProfile, "%s_azimuth.txt", profile);
    fpProfile = FOPEN(szProfile, "w");
    for (ii=0; ii<srcSize; ii++)
      fprintf(fpProfile, "%3i\t%12.4f\n", x-srcSize/2+ii, s[ii*srcSize+bestY]);
    FCLOSE(fpProfile);
    sprintf(szProfile, "%s_range.txt", profile);
    fpProfile = FOPEN(szProfile, "w");
    for (ii=0; ii<srcSize; ii++)
      fprintf(fpProfile, "%3i\t%12.4f\n", y-srcSize/2+ii, s[bestX*srcSize+ii]);
    FCLOSE(fpProfile);
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
