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
#include "envi.h"
#include "asf_meta.h"
#include <ctype.h>
#include <string.h>

#define VERSION 1.0


void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <envi_name> <meta_name>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   envi_name   Base name of the ENVI header file.\n"
	 "   meta_name   Base name of the new style meta data.");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts an ENVI header file to a new style metadata.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}


int main(int argc, char **argv)
{
  char meta_name[255];
  char envi_name[255];
  meta_parameters *meta=NULL;
  envi_header *envi=NULL;
  FILE *fp;
  char line[255]="", key[25]="", value[25]="", *map_info_ptr=NULL;
  char *proj_info_ptr, proj_info[255]="", bla[25], map_info[255]="";
  double fTmp1, fTmp2;
  int projection_key;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  logflag = 0;

  /* Parse command line args */
  while (currArg < (argc-2))
    {
      char *key=argv[currArg++];
      if (strmatch(key,"-log")) {
	sprintf(logFile, "%s", argv[currArg]);
	logflag = 1;
      }
      else {
	printf("\n   ***Invalid option:  %s\n\n",
	       argv[currArg-1]);
	usage(argv[0]);
      }
    }
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  create_name(envi_name, argv[currArg], ".hdr");
  create_name(meta_name, argv[currArg+1], ".meta");

  asfSplashScreen(argc, argv);
  
  /* Allocate memory for ESRI header structure */
  envi = (envi_header *)MALLOC(sizeof(envi_header));

  /* Read .hdr and fill meta structures */ 
  fp = FOPEN(envi_name, "r");
  while (NULL != fgets(line, 255, fp)) {
    sscanf(line, "%s = %s", key, value);
    if (strncmp(key, "samples", 6)==0) envi->samples = atoi(value);
    else if (strncmp(key, "lines", 5)==0) envi->lines = atoi(value);
    else if (strncmp(key, "bands", 5)==0) envi->bands = atoi(value);
    else if (strncmp(key, "header", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "offset", 6)==0)
	envi->header_offset = atoi(value);
    }
    /*** ignore file type for the moment ***/
    else if (strncmp(key, "data", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
	envi->data_type = atoi(value);
    }
    else if (strncmp(key, "interleave", 10)==0) 
      sprintf(envi->interleave, "%s", value);
    else if (strncmp(key, "sensor", 6)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "type", 4)==0)
	sprintf(envi->sensor_type, "%s", value);
    }
    else if (strncmp(key, "byte", 4)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "order", 5)==0)
	envi->byte_order = atoi(value);
    }
    else if (strncmp(key, "map", 3)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
	map_info_ptr = strstr(line, ",");
	sprintf(map_info, "%s", map_info_ptr);
      }
    }
    else if (strncmp(key, "projection", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "info", 4)==0) {
	proj_info_ptr = strstr(line, ",");
        sprintf(proj_info, "%s", proj_info_ptr);      
	sscanf(value, "{%i,", &projection_key);
      }
    }
    else if (strncmp(key, "wavelength", 10)==0) {
      sscanf(line, "%s %s = %s", bla, key, value);
      if (strncmp(key, "units", 5)==0)
	sprintf(envi->wavelength_units, "%s", value);
    }
    /*** ignore wavelength for the moment ***/
    /*** ignore data ignore for the moment ***/
    /*** ignore default stretch for the moment ***/
  }
  FCLOSE(fp);

  switch(projection_key)
    {
    case 3: 
      sprintf(envi->projection, "UTM");
      sscanf(map_info, ", %i %i %lf %lf %lf %lf %i %s",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, 
	     &envi->pixel_easting, &envi->pixel_northing, 
	     &envi->proj_dist_x, &envi->proj_dist_y,
	     &envi->projection_zone, envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
	     &envi->center_lon, bla);
      break;
    case 4: 
      sprintf(envi->projection, "Lambert Conformal Conic");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, 
	     &envi->pixel_easting, &envi->pixel_northing, 
	     &envi->proj_dist_x, &envi->proj_dist_y,
	     envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
	     &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
	     &envi->standard_parallel2, bla);
      break;
    case 9: 
      sprintf(envi->projection, "Albers Conical Equal Area");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, 
	     &envi->pixel_easting, &envi->pixel_northing, 
	     &envi->proj_dist_x, &envi->proj_dist_y,
	     envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf %lf %lf %lf%s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
	     &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
	     &envi->standard_parallel2, bla);
      break;
    case 11: 
      sprintf(envi->projection, "Lambert Azimuthal Equal Area");
      sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, 
	     &envi->pixel_easting, &envi->pixel_northing, 
	     &envi->proj_dist_x, &envi->proj_dist_y,
	     envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
	     &envi->center_lon, bla);
      break;
    case 31: 
      sprintf(envi->projection, "Polar Stereographic");
      sscanf(map_info, ", %d, %d, %lf, %lf, %lf, %lf, %s}",
	     &envi->ref_pixel_x, &envi->ref_pixel_y, 
	     &envi->pixel_easting, &envi->pixel_northing, 
	     &envi->proj_dist_x, &envi->proj_dist_y,
	     envi->hemisphere);
      sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
	     &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
	     &envi->center_lon, bla);
      break;
    default:
      sprintf(errbuf, "\n   ERROR: unsupported map projection\n\n");
      printErr(errbuf);
      break;
    }

  /* Fill metadata structure with valid data */
  meta = envi2meta(envi);
  
  /* Write metadata file */
  meta_write(meta, meta_name);

  /* Clean and report */
  meta_free(meta);
  asfPrintStatus("   Converted ENVI header (%s) to metadata file (%s)\n\n",
		 envi_name, meta_name);
  
  return 0;
}
