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
#include "asf_license.h"
#include "envi2meta_help.h"
#include <ctype.h>
#include <string.h>

int main(int argc, char **argv)
{
  char meta_name[255];
  char envi_name[255];
  meta_parameters *meta=NULL;
  envi_header *envi=NULL;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  logflag = 0;

  if (argc > 1) {
      check_for_help(argc, argv);
      handle_license_and_version_args(argc, argv, TOOL_NAME);
  }
  if (argc < 3) {
      asfPrintStatus("\nNot enough arguments.\n");
      usage();
      return 1;
  }

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

  // Read ENVI header
  envi = read_envi(envi_name);

  // Fill metadata structure with valid data
  meta = envi2meta(envi);

  // Write metadata file
  meta_write(meta, meta_name);

  // Clean and report
  meta_free(meta);
  asfPrintStatus("   Converted ENVI header (%s) to metadata file (%s)\n\n",
         envi_name, meta_name);

  return 0;
}
