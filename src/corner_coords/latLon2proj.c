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
	 "   %s <lat> <lon> <projection parameter file> |\n"
	 "      -list <coordinate list> <projection parameter file>",name);
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts geographic coordinates into map projected coordinates\n"
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

void fill_in_utm(double lat, double lon, project_parameters_t *pps)
{
  pps->utm.zone = (int) ((lon + 180.0) / 6.0 + 1.0);
  pps->utm.scale_factor = 0.9996;
  pps->utm.lon0 = (double) (pps->utm.zone - 1) * 6.0 - 177.0;
  pps->utm.lat0 = 0.0;
  pps->utm.false_easting = 500000.0;
  if (lat > 0.0)
    pps->utm.false_northing = 0.0;
  else
    pps->utm.false_northing = 10000000.0;
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
    lat = atof(argv[currArg++]);
    lon = atof(argv[currArg++]);
    projFile = argv[currArg];
  }

  system("date");
  printf("Program: latLon2proj\n\n");

  // Read projection file
  read_proj_file(projFile, &pps, &proj_type);

  // Report the conversion type
  switch(proj_type) 
    {
    case ALBERS_EQUAL_AREA:
      printf("Lat/Lon to Albers Equal Area\n\n");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      printf("Lat/Lon to Lambert Azimuthal Equal Area\n\n");
      break;
    case LAMBERT_CONFORMAL_CONIC:
      printf("Lat/Lon to Lambert Conformal Conic\n\n");
      break;
    case POLAR_STEREOGRAPHIC:
      printf("Lat/Lon to Polar Stereographic\n\n");
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      printf("Lat/Lon to UTM\n\n");
      break;
    }

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM;

  // Read lat/lon from list if needed and convert to map coordinates
  if (listFlag) {
    fp = FOPEN(listFile, "r");
    while (fgets(line, 255, fp) != NULL) {
      if (strlen(line) > 1) {
	sscanf(line, "%lf %lf", &lat, &lon);   
	if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR)
	  fill_in_utm(lat, lon, &meta_proj->param);
	else
	  meta_proj->param = pps;
	latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, &projX, &projY);
	printf("%.4lf\t%.4lf\t%.3lf\t%.3lf\n", lat, lon, projX, projY);
      }
    }
    FCLOSE(fp);
  }
  else {
    if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR)
      fill_in_utm(lat, lon, &meta_proj->param);
    else
      meta_proj->param = pps;
    latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, &projX, &projY);
    printf("%.4lf\t%.4lf\t%.3lf\t%.3lf\n", lat, lon, projX, projY);
  }

  printf("\n");
  return 0;
}
