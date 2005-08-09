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
#include "asf_reporting.h"
#include "ddr.h"
#include "hdr.h"

#define VERSION 1.0

static
void usage(char *name)
{
 asfPrintStatus("\n"
	"USAGE:\n"
	"   %s <hdr_name> <las_name>\n",name);
 asfPrintStatus("\n"
	"REQUIRED ARGUMENTS:\n"
	"   hdr_name   Base name of the hdr file.  I.E. if <hdr_name> is\n"
	"                \"test1\", then \"test1.hdr\" must be in the working directory\n"
 	"   las_name   Base name of the ddr file.  I.E. if <las_name> is\n"
	"                \"test2\", then both \"test2.ddr\" and \"test2.meta\" must be\n"
	"                in the working directory\n");
 asfPrintStatus("\n"
	"DESCRIPTION:\n"
	"   %s converts DDR file to HDR file.\n", name);
 asfPrintStatus("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}


int main(int argc, char **argv)
{
  char las_name[255];
  char hdr_name[255];
  struct DDR ddr;
  jpl_header hdr;

  // Parse command line
  if (argc-currArg < 2)
    {asfPrintStatus("Insufficient arguments.\n"); usage(argv[0]);}
  if (argc-currArg > 2)
    {asfPrintStatus("Excessive arguments.\n"); usage(argv[0]);}
  create_name(las_name, argv[currArg], ".ddr");
  create_name(hdr_name, argv[currArg+1], ".hdr");

  // Read .hdr and fill meta structures
  c_getddr_old(las_name, &ddr);

  // Fill the hdr based on the ddr
  ddr2hdr(&ddr, &hdr);

  // Write HDR file
  put_hdr(hdr_name, &hdr);

  // Clean and report
  asfPrintStatus("   Wrote %s based on %s.\n", hdr_name, las_name);

  return 0;
}


