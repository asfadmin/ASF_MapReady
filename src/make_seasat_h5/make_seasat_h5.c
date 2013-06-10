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

#include "asf_export.h"
#include "asf_iso_meta.h"
#include "asf_vector.h"
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
	 "   %s [ -log <logFile> -gap <gapFile> ] <inFile> <outFile>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   inFile    Input file name (ASF internal format).\n"
	 "   gapFile   Data gaps file name.\n"
	 "   outFile   Output HDF5 file name.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s converts an SEASAT ASF internal file to an HDF5 file.\n",
	 name);
  printf("\n"
	 "%s %s\n"
	 "\n", TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  int nouts = 0;
  char **outs = NULL;
  char configFile[512], *gapFile = NULL;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */
  logflag = 0;
  
  // Parse command line args
  while (currArg < (argc-2)) {
    char *key=argv[currArg++];
    if (strmatch(key,"-log")) {
      sprintf(logFile, "%s", argv[currArg]);
      logflag = 1;
    }
    else if (strmatch(key, "-gap")) {
      gapFile = (char *) MALLOC(sizeof(char)*512);
      CHECK_ARG(1);
      strcpy(gapFile, GET_ARG(1));
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
  
  asfSplashScreen(argc, argv);

  char *inFile = STRDUP(argv[currArg]);
  char *outFile = STRDUP(argv[currArg+1]);  

  // Generating XML file
  meta_parameters *meta = meta_read(inFile);
  iso_meta *iso = meta2iso(meta);
  sprintf(configFile, "%s/iso_meta_ext.xml", get_asf_share_dir());
  asfPrintStatus("\nReading parameters from default configuration file:\n"
		 "%s\n", configFile);
  char *xmlFile = appendExt(outFile, ".iso.xml");
  iso_ext_meta_write(iso, xmlFile, outFile, configFile, gapFile);
  FREE(xmlFile);
  xmlFile = appendExt(inFile, ".xml");
  iso_meta_write(iso, gapFile, xmlFile);
  iso_meta_free(iso);
  meta_free(meta);

  // Generate HDF5 file
  char *inImg = appendExt(inFile, ".img");
  export_hdf(xmlFile, inImg, outFile, NULL, &nouts, &outs);
  FREE(xmlFile);

  // Generate KML file
  c2v_config *cfg = NULL;
  sprintf(configFile, "%s/convert2vector.config", get_asf_share_dir());
  asfPrintStatus("\nReading parameters from default configuration file:\n"
		 "%s\n", configFile);
  cfg = read_c2v_config(configFile);
  char *metaFile = appendExt(inFile, ".meta");
  char *kmlFile = appendExt(outFile, ".kml");
  strcpy(cfg->input_format, "META");
  strcpy(cfg->output_format, "KML");
  strcpy(cfg->input_file, metaFile);
  strcpy(cfg->output_file, kmlFile);
  convert2vector(cfg);
  FREE(cfg);

  // Clean up
  FREE(inFile);
  FREE(outFile);
  if (gapFile)
    FREE(gapFile);
  FREE(metaFile);
  FREE(kmlFile);
  FREE(inImg);

  return 0;
}

