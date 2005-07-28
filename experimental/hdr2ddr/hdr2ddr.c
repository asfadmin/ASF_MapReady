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
#include "las.h"
#include "hdr.h"

#define VERSION 1.0

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <hdr_name> <las_name>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   hdr_name   Base name of the hdr file.  I.E. if <hdr_name> is\n"
	"                \"test1\", then \"test1.hdr\" must be in the working directory\n"
 	"   las_name    Base name of the ddr file.  I.E. if <las_name> is\n"
	"                \"test2\", then both \"test2.ddr\" and \"test2.meta\" must be\n"
	"                in the working directory\n");
printf("\n"
	"DESCRIPTION:\n"
	"   %s converts HDR file to DDR file.\n", name);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fp;
  char las_name[255];
  char hdr_name[255];
  char line[255], value[255], key[255];
  struct DDR ddr;
  jpl_header *hdr;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  
  /* Parse command line */
  if (argc-currArg < 2)
    {printf("Insufficient arguments.\n"); usage(argv[0]);}
  if (argc-currArg > 2)
    {printf("Excessive arguments.\n"); usage(argv[0]);}
  create_name(hdr_name, argv[currArg], ".hdr");
  create_name(las_name, argv[currArg+1], ".ddr");
  
  /* Allocate memory for JPL header structure */
  hdr = (jpl_header *) MALLOC(sizeof(jpl_header));

  /* Read .hdr and fill meta structures */ 
  fp = FOPEN(hdr_name, "r");
  fgets(line, 255, fp);
  sscanf(line, "%d", &hdr->magnitude_bytes);
  fgets(line, 255, fp);
  sscanf(line, "%d", &hdr->elevation_bytes);
  fgets(line, 255, fp);
  sscanf(line, "%s", hdr->data_type, key);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->elevation_scale, &hdr->elevation_shift);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->line_increment, &hdr->sample_increment);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->start_lat, &hdr->start_lon);
  fgets(line, 255, fp);
  sscanf(line, "%d %d", &hdr->line_count, &hdr->sample_count);
  FCLOSE(fp);

  /*
  printf("Magnitude Bytes per Pixel: %d\n", hdr->magnitude_bytes);
  printf("Elevation Bytes per Pixel: %d\n", hdr->elevation_bytes);
  printf("Date file type: %s\n", hdr->data_type);
  printf("Elevation Scale and Shift(M): %.8lf %.8lf\n", hdr->elevation_scale, 
	 hdr->elevation_shift);
  printf("Post Spacing(Deg): %.8lf %.8lf\n", hdr->line_increment, 
	 hdr->sample_increment);
  printf("Starting corner position(s,c): %.6lf %.6lf\n", hdr->start_lat, 
	 hdr->start_lon);
  printf("Data file dimensions: %d %d\n", hdr->line_count, hdr->sample_count);
  */

  /* Fill ddr struct with valid data */
  hdr2ddr(hdr, &ddr);
  
  /* Write DDR file */
  c_putddr(las_name, &ddr);
  
  /* Clean and report */
  printf("   Wrote %s from %s.\n",
	 las_name, hdr_name);
  
  return 0;
}


