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
/******************************************************************************
NAME: raster_calc

SYNOPSIS:  raster_calc [-log <file>] <out.ext> "exp" <inA.ext> [<inB.ext> [...]]

DESCRIPTION:
	Calculates an output image based on some mathematical function of the
	input image (or images).  That is, if the expression is "2*a", the
	output image's pixel values will be twice the input image's pixel
	values. Works with any type of input data-- byte, short, long, or float.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

ROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
     1.0     11/97   O. Lawlor   Initial Development.
     1.01     7/01   R. Gens     Added logfile switch
     1.1      2/02   P. Denny    Updated command line parsing
     1.2      2/04   P. Denny    Changed name from las_op to raster_calc
                                  Changed from the GPL to the BSD License
     1.5      6/04   P. Denny    Update to use meta1.1 instead of DDR
                                  Re-update commandline parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "expression.h"

#define MAXIMGS 20
#define VERSION 1.5

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <out.ext> \"exp\" <inA.ext> [<inB.ext> [...]]\n",
	name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <out.ext>        ASF tools format image with extension to be created.\n"
	"   <exp>            Expression.\n"
	"   <inA.ext>        First input image with extension.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   [<inB.ext>]      Optional second input image with extension.\n"
	"   [...]            Optional additional images with extension.\n"
	"   [-log <file>]    Option to have output written to a log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Creates an output ASF tools format image based upon the\n"
	"   mathematical expression you give it.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc,char **argv)
{
  int ii;
  char *expression;
  char *outFile,*inFiles[MAXIMGS];
  int input_count;
  extern int currArg;         /* in cla.h from asf.h; initialized to 1 */

  fLog=NULL;
  logflag=FALSE;

  if(argc<4) usage(argv[0]);
  while (currArg < (argc-4)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1); /*one string argument: log file */
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile,"a");
      logflag=TRUE;
    }
  }

  outFile = argv[currArg++];
  expression = argv[currArg++];
  input_count = argc - currArg;
  for (ii=0; ii<input_count; ii++)
    inFiles[ii] = argv[currArg+ii];

  asfSplashScreen(argc, argv);

  raster_calc(outFile, expression, input_count, inFiles);

  exit(EXIT_SUCCESS);
}
