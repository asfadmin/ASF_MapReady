/* Start off with documentaion stuff */

#define ASF_NAME_STRING \
"   meta2sprocket"

#define ASF_USAGE_STRING \
"<inAsfMetaFile> <outSprocketMetadataFile>"

#define ASF_DESCRIPTION_STRING \
"   Converts an ASF Tools internal format meta file into a Sprocket\n"\
"   style metadata file."

#define ASF_INPUT_STRING \
"   The input is a *.meta ASF Tools internal format file."

#define ASF_OUTPUT_STRING \
"   The output is a *.metadata Sprocket format file."

#define ASF_EXAMPLES_STRING \
"   To import CEOS format to the ASF tools internal format run:\n"\
"        example> meta2sprocket fileASF fileSprocket"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"    VERS:   DATE:  AUTHOR:     PURPOSE:\n"\
"    ---------------------------------------------------------------\n"\
"    0.1    03/05   P. Denny    Initial development"

/*****************************************************************************/

#define REQUIRED_ARGS 2

#include "asf.h"
#include "asf_reporting.h"
#include "asf_meta.h"


void usage(void)
{
  printf("\n"
         "USAGE:\n"
          ASF_NAME_STRING " " ASF_USAGE_STRING
          "\n\n");
  exit (EXIT_FAILURE);
}


/* help_page - go here when the -help option is specified */
void help_page()
{
  char happy_string[4066];
  char command[4096];

  /* What to print out for help */
  sprintf(happy_string,
          "\n\n"
          "Tool name:\n" ASF_NAME_STRING "\n\n"
          "Usage:\n" ASF_NAME_STRING " " ASF_USAGE_STRING "\n\n"
          "Description:\n" ASF_DESCRIPTION_STRING "\n\n"
          "Input:\n" ASF_INPUT_STRING "\n\n"
          "Output:\n"ASF_OUTPUT_STRING "\n\n"
          "Examples:\n" ASF_EXAMPLES_STRING "\n\n"
          "Version:\n" CONVERT_PACKAGE_VERSION_STRING "\n\n"
          "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n");

  /* If we can, use less */
  sprintf (command, "echo '%s' | less --prompt='Type q to quit help, h for "
     "help with help browser'", happy_string);
  if(system(command) == 0)
    exit(EXIT_SUCCESS);

  /* Hmmm, less didn't work cause we got here, try using more */
  sprintf(command,"echo '%s' | more",happy_string);
  if(system(command) == 0)
    exit(EXIT_SUCCESS);

  /* Okay, neither less or more work (obviously if we made it here),
   * just print the info straight to stdout and exit */
  printf(happy_string);
  exit(EXIT_SUCCESS);
}


/*****************************************************************************/
int main(int argc, char *argv[])
{
  char inName[256]="";
  char outName[256]="";

  /* Check every arg to see if the user asked for help */
  int ii=1;
  for (ii=1; ii<argc; ii++) {
    if ( (strcmp(argv[ii],"--help")==0) || (strcmp(argv[ii],"-help")==0) || (strcmp(argv[ii],"-h")==0) ) {
      help_page();
    }
  }

  /*Make sure the user specified the proper number of arguments*/
  if(argc != REQUIRED_ARGS+1) {
    usage();/*This exits with a failure*/
  }

  /*Report what was retrieved at the command line */
  asfSplashScreen(argc, argv);

  /* Fetch required arguments */
  strcpy(inName, argv[argc - 2]);
  strcpy(outName,argv[argc - 1]);

  /* Apend .metadata to sprocket file */
  strcat(outName, ".metadata");

  /* Do the actual convertion */
  meta_parameters *meta = meta_read(inName);
  meta_write_sprocket(outName, meta, NULL);

  /* done! */
  exit(EXIT_SUCCESS);
}
