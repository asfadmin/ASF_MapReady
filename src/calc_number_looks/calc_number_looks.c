/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_nan.h"
#include "asf_sar.h"
#include "asf_meta.h"
#include "asf_license.h"
#include "time.h"
#include <string.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "Usage:\n"
	 "   %s [-log <logFile>] [-image] [-chip-size <pixels] [-gis] <file>\n",
	 name);
  printf("\n"
	 "Required arguments:\n"
	 "   file   ASF internal SAR image.");
  printf("\n"
	 "Options:\n"
	 "   -image\n"
         "       Performs the calculation on the entire image instead of chips\n"
	 "   -chip-size <pixels>\n"
	 "       Set the chip size for the calculation.\nBy default the "
	 "       is determined for a chip to cover 500 x 500 m area.\n"
	 "   -gis\n"
	 "       The output is additionally stored in a shape file to analyze\n"
	 "       the result spatially.\n");
  printf("\n"
	 "Description:\n"
	 "   %s determines the number of looks from an image.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

static int strmatches(const char *key, ...)
{
  va_list ap;
  char *arg = NULL;
  int found = FALSE;
  
  va_start(ap, key);
  do {
    arg = va_arg(ap, char *);
    if (arg) {
      if (strcmp(key, arg) == 0) {
	found = TRUE;
	break;
      }
    }
  } while (arg);
  
  return found;
}

int main(int argc, char **argv)
{
  const int n=1024;
  char inFile[1024];
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  int NUM_ARGS = 1;
  logflag = FALSE;
  int imageFlag = FALSE;
  int chipSize = MAGIC_UNSET_INT;
  int gisFlag = FALSE; 

  // Parse command line args
  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key, "-log", "--log", NULL)) {
      CHECK_ARG(1);
      strncpy_safe(logFile, GET_ARG(1), n);
      logflag = TRUE;
    }
    else if (strmatches(key, "-image", "--image", NULL))
      imageFlag = TRUE;
    else if (strmatches(key, "-chip-size", "--chip-size", NULL)) {
      CHECK_ARG(1);
      chipSize = strtod(GET_ARG(1), NULL);
    }
    else if (strmatches(key, "-gis", "--gis", NULL))
      gisFlag = TRUE;
    else {
      printf("\n   ***Invalid option:  %s\n\n",
	     argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }
  
  strcpy(inFile, argv[currArg]);

  asfSplashScreen(argc, argv);

  calc_number_looks(inFile, imageFlag, chipSize, gisFlag);

  return FALSE;
}
