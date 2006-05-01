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
#include "asf_meta.h"

#define VERSION 1.0

void read_proj_file(char * file, project_parameters_t * pps,
		    projection_type_t * proj_type);

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <projX> <projY> <proj parameters input> <proj parameter output> |\n"
	 "      -list <coordinate list> <projection parameter file>",name);
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts coordinates from one map projection to another\n"
	 "   using predefined map projection parameter files\n",name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
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
  char *listFile, *projFile, *key, line[255];
  FILE *fp;
  project_parameters_t pps;
  projection_type_t proj_type;
  meta_projection *meta_proj;
  double lat, lon, projX, projY;
  int listFlag = FALSE;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */

  currArg = 1;

  if (argc < 4) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  key = argv[currArg];
  if (strmatches(key,"-list","--list",NULL)) {
    currArg++;
    CHECK_ARG(1);
    listFile = GET_ARG(1);
    projFile = argv[currArg];
    listFlag = TRUE;
  }
  else {
    projX = atof(argv[currArg++]);
    projY = atof(argv[currArg++]);
    projFile = argv[currArg];
  }

  system("date");
  printf("Program: proj2proj\n\n");

  // Read projection file
  read_proj_file(projFile, &pps, &proj_type);

  // Check whether UTM projection has a zone defined
  if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR &&
      (pps.utm.zone < 0 || pps.utm.zone > 60))
    asfPrintError("Undefined zone for UTM projection\n");

  // Report the conversion type
  switch(proj_type) 
    {
    case ALBERS_EQUAL_AREA:
      printf("Albers Equal Area to Lat/Lon\n\n");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      printf("Lambert Azimuthal Equal Area to Lat/Lon\n\n");
      break;
    case LAMBERT_CONFORMAL_CONIC:
      printf("Lambert Conformal Conic to Lat/Lon\n\n");
      break;
    case POLAR_STEREOGRAPHIC:
      printf("Polar Stereographic to Lat/Lon\n\n");
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      printf("UTM to Lat/Lon\n\n");
      break;
    }

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM;
  meta_proj->param = pps;

  // Read coordinates from list if needed and convert to map coordinates
  if (listFlag) {
    fp = FOPEN(listFile, "r");
    while (fgets(line, 255, fp) != NULL) {
      if (strlen(line) > 1) {
	sscanf(line, "%lf %lf", &projX, &projY);   
	proj_to_latlon(meta_proj, 'R', projX, projY, &lat, &lon);
	printf("%.3lf\t%.3lf\t%.4lf\t%.4lf\n", projX, projY, lat*R2D, lon*R2D);
      }
    }
    FCLOSE(fp);
  }
  else {
    proj_to_latlon(meta_proj, 'R', projX, projY, &lat, &lon);
    printf("%.3lf\t%.3lf\t%.4lf\t%.4lf\n", projX, projY, lat*R2D, lon*R2D);
  }

  printf("\n");
  return 0;
}
