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

int main(int argc, char **argv)
{
  char meta_name[255], out_name[255];
  meta_parameters *meta=NULL;
  FILE *fp;
  double lat, lon;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  create_name(meta_name, argv[currArg], ".meta");
  create_name(out_name, argv[currArg], ".corners");

  system("date");
  printf("Program: corner_coords\n\n");
  
  meta = meta_read(meta_name);
  fp = FOPEN(out_name, "w");
  meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);
  printf("   UL: lat = %.4f, lon = %.4f\n", lat, lon);
  fprintf(fp, "UL: lat = %.4f, lon = %.4f\n", lat, lon);
  meta_get_latLon(meta, 0, meta->general->sample_count, 0.0, &lat, &lon);
  printf("   UR: lat = %.4f, lon = %.4f\n", lat, lon);
  fprintf(fp, "UR: lat = %.4f, lon = %.4f\n", lat, lon);
  meta_get_latLon(meta, meta->general->line_count, 0, 0.0, &lat, &lon);
  printf("   LL: lat = %.4f, lon = %.4f\n", lat, lon);
  fprintf(fp, "LL: lat = %.4f, lon = %.4f\n", lat, lon);
  meta_get_latLon(meta, meta->general->line_count, meta->general->sample_count,
                  0.0, &lat, &lon);
  printf("   LR: lat = %.4f, lon = %.4f\n\n", lat, lon);
  fprintf(fp, "LR: lat = %.4f, lon = %.4f\n", lat, lon);
  FCLOSE(fp);

  return 0;
}


void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s  <name>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   name   Base name of image file.");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s writes the geographic corner coordinates to a file.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}
