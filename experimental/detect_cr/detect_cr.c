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
"<image> <corner reflector locations> <peak search file>\n"\
"\n"\
"Additional option: -help"

#define ASF_DESCRIPTION_STRING \
"Detect_cr takes the geolocation information of corner reflectors are\n"\
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
"-chip	Stores the image chip used for determination of amplitude peaks."

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
#include "ifm.h"
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
  char szImg[255], buffer[1000], crID[10], szCrList[255], szOut[255];
  int ii;
  float dx, dy;
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

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  { /* We need to make sure the user specified the proper number of arguments */
    int needed_args = 4;/*command & in_data & in_meta & out_base */
    if(flags[f_CHIP] != FLAG_NOT_SET) needed_args += 1; /* option */

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

  /* Read metadata */
  meta = meta_read(szImg);
  lines = meta->general->line_count;
  samples = meta->general->sample_count;
 
  /* Handle input and output file */
  fpIn = FOPEN(szCrList, "r");
  fpOut = FOPEN(szOut, "w");
  
  /* Loop through corner reflector location file */
  while (fgets(buffer, 1000, fpIn))
  {
    sscanf(buffer, "%s\t%lf\t%lf\t%lf", crID, &lat, &lon, &elev);
    meta_get_lineSamp(meta, lat, lon, elev, &posY, &posX);
    
    /* Check bounds */
    if (!(outOfBounds(posX, posY, srcSize)))
      {
	/* Find peak */
	if(flags[f_CHIP] != FLAG_NOT_SET) 
	  findPeak(posX+0.5, posY+0.5, szImg, &dx, &dy, crID);
	else 
	  findPeak(posX+0.5, posY+0.5, szImg, &dx, &dy, NULL);
	fprintf(fpOut,"%s %.4lf %.4lf %.0lf %.1f %.1f %.2f %.2f\n",
		crID, lat, lon, elev, posX, posY, dx, dy);
	fflush(fpOut);
      }
    else 
      printf("   WARNING: Corner reflector %s outside the image boundaries!\n", 
	     crID);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(meta);

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
bool findPeak(int x, int y, char *szImg, float *peakX, float *peakY, char *crID)
{
  FILE *fpChip;
  meta_parameters *meta, *metaChip;
  static float *s=NULL;
  float max=-10000000.0;
  char szChip[255];
  int size, ii, kk, bestX, bestY;
  float bestLocX, bestLocY;
  
  meta = meta_read(szImg); 

  /* Allocate working arrays */
  if (s == NULL) {
    s = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
  }  

  /* At each corner reflector location read in a chunk of the image */
  readSubset(szImg, srcSize, srcSize, x-srcSize/2+1, y-srcSize/2+1, s);
  
  /* Write out chip if reflector ID is given */
  if (crID) {
    metaChip = meta_copy(meta);
    metaChip->general->line_count = srcSize;
    metaChip->general->sample_count = srcSize;
    meta_write(metaChip, crID);
    sprintf(szChip, "%s.img", crID);
    fpChip = FOPEN(szChip, "wb");
    size = srcSize*srcSize*sizeof(float);
    FWRITE(s, size, 1, fpChip);
    FCLOSE(fpChip);
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
