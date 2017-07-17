/****************************************************************
NAME:  c2p

SYNOPSIS:    c2p inCPXfile outAPfile

DESCRIPTION:
    Convert complex images to amp & phase files

    inCPXfile is a complex image file. An extension of .cpx will be appended.
    outAPfile is the base name for the resulting amplitude and phase files.
    An extension of .amp & .phase will be appended.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     9/96   M. Shindle
                    & R. Fatland - Original Development
    1.05    5/00   P. Denny      - output a .ddr file
    1.25    6/03   P. Denny      - Forget about the DDR file, use meta
                                    Don't let input & output name be the same
                                    Use get/put_*_line routines instead of
                                     ASF_FREAD/ASF_FWRITE 
    1.5     5/07   K. Hogenson   - moved code to libasf_sar

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   c2p:  complex to polar (amp + phase) stream converter		    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "asf_insar.h"
#include "asf_sar.h"
#include "ifm.h"
#include <stdbool.h>

#define VERSION		1.5
#define BUF		256
#define SAME		0
/* #define CHUNK_OF_LINES -- defined in ../../include/asf.h */

void usage(char *name);

int main (int argc, char *argv[])
{
  char inDataName[255], inMetaName[255], *baseName;

/* cla parsing */
  handle_common_asf_args(&argc, &argv, "c2p");

/* After extracting the optional flags, should have 2 args left */
  if (argc != 3)
      usage(argv[0]);

/* Make sure input and output names are different */
  if (strcmp(argv[1],argv[2])==SAME) {
    printf("c2p: Input and output names cannot be the same. Exiting.\n");
    exit(EXIT_FAILURE);
  }

  // Assign filenames and check their existence
  baseName = (char *) MALLOC(sizeof(char)*255);
  baseName = get_basename(argv[1]);
  sprintf(inDataName, "%s.img", baseName);
  if (!fileExists(inDataName))
    asfPrintError("Data file (%s) does not exist.\n", inDataName);
  sprintf(inMetaName, "%s.meta", baseName);
  if (!fileExists(inMetaName))
    asfPrintError("Metadata file (%s) does not exist.\n", inMetaName);

/* Do it! */
  c2p_ext(inDataName, inMetaName, argv[2], FALSE, FALSE);
  
  if (logflag)
      FCLOSE(fLog);

  return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <in> <out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in>  The input file, assumed to be complex data.\n"
	"          will add .img extension to create <in>.img,\n"
    "          if necessary.\n"
	"   <out> The base file name for the output;\n"
	"          Writes out files named <out>_amp.img and\n"
    "          <out>_phase.img, and metadata for each.\n\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts a complex image file to polar image files (amp + phase)\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1); 
}
