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

#define VERSION 1.0

int main(int argc, char **argv)
{
	char las_name[256];
	char meta_name[256];
	meta_parameters *meta;
	
/* Parse command line */
	currArg=1;
	if (argc-currArg < 2)
		{printf("Insufficient arguments.\n"); usage(argv[0]);}
	if (argc-currArg > 2)
		{printf("Excessive arguments.\n"); usage(argv[0]);}
	strcpy(las_name, argv[currArg]);
	strcpy(meta_name,argv[currArg+1]);
	
/* Read .ddr & .meta file info & put into meta structures */ 
	meta = meta_read(las_name);

/* write it out new style */
	meta_write(meta, meta_name);

/* Clean and report */
	meta_free(meta);
	printf("***Wrote %s.meta from %s.ddr and %s.meta.\n",
	       meta_name,las_name,las_name);

	return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <las_name> <meta_name>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   las_name    Base name of the old style meta data.  I.E. if <las_name> is\n"
	"                \"test1\", then both \"test1.ddr\" and \"test1.meta\" must be\n"
	"                in the working directory\n"
	"   meta_name   Base name of the new style meta data.  I.E. if <meta_name> is\n"
	"                \"test2\", then \"test2.meta\" must be in the working directory\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s coverts old ASF style metadata to new ASF style metadata.\n"
	"   Old metadata being a LAS DDR file and a pre-version 1.10 meta file.\n"
	"   New metadata is a meta file that is version 1.10 or greater. Current\n"
	"   version is %.2f. Make sure that <las_name> and <meta_name> are\n"
	"   different.  Otherwise the meta file that was read in will get written\n"
	"   over.\n",name,META_VERSION);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
