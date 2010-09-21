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
#include "asf_meta.h"
#include "asf_license.h"
#include "time.h"
#include <string.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ] <meta_name> <xml_name>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   meta_name   Base name of the meta data file.\n"
	 "   esri_name   Base name of the XML file.");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts an ASF .meta file to an ASF .xml file.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  char meta_name[1024], xml_name[1024];
  meta_parameters *meta=NULL;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  logflag = 0;

  // Parse command line args
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

  create_name(meta_name, argv[currArg], ".meta");
  create_name(xml_name, argv[currArg+1], ".xml");

  asfSplashScreen(argc, argv);

  meta = meta_read(meta_name);
  meta_write_xml(meta, xml_name);

  // Clean and report
  meta_free(meta);
  asfPrintStatus("   Converted metadata file (%s) to XML file (%s)\n\n",
		 meta_name, xml_name);

  return 0;
}

