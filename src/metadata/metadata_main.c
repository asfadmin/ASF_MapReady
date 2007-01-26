/********************************************************************
NAME:    metadata.c -- MAIN PROGRAM TO READ/TEST CEOS METADATA

SYNOPSIS:     metadata [ <record type> -all -save ] infile
 Where, the rectypes are:
   -dssr        Data Set Summary record
   -shr         Scene Header record
   -mpdr        Map Projection Data Record
   -ppdr        Platform Position Data record
   -atdr        Attitude Data record
   -radr        Radiometric Data record
   -rcdr        Radiometric Compensation Data record
   -dqsr        Data Quality Summary record
   -pdhr        Processed Data Histograms record
   -sdhr        Signal Data Histograms record
   -rasr        Range Spectra record
   -asf_facdr   ASF Facility Related Data record
   -esa_facdr   ESA Facility Related Data record
   -ifdr        Image File Descriptor record
   -lfdr        Leader File Descriptor record
   infile       Base name of the SAR image

 NOTE: NOT ALL OF THESE OPTIONS ARE INCLUDED IN THIS RELEASE. SEE USAGE.

DESCRIPTION:
	Reads infile.ext, where ext is one of L, D, ldr, trl, tlr, or dat
        to find the record type specified.  Once found, converts the data
        to a structure of values and prints the values out, either to the
        screen or to file as specified by the -f switch.

        This program handles both old ASF format image triplet (leader,
        data, trailer) and new ASF format pair (leader, data) files.
        New format files are "looked for" first.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           3/96   T. Logan (ASF)
  1.01          7/96   M. Shindle (ASF) - Corrected minor bug
  2.00	        8/96   T. Logan (ASF)   - Expanded to access RADARSAT data
  2.01	       12/96   T. Logan (ASF)   - Added check_cal() call
  2.02          7/97   D. Corbett       - Don't print mpdr if get_mpdr erred
  2.03          3/98   O. Lawlor (ASF)  - Moved prn_ceos types over to metadata,
                                          which is the only program that ever
					  uses them.
  2.04		6/99   M. Ayers (ASF)   - Added prn_fdr to print file descriptor
					  records.
  2.05		9/01   S. Watts  	- Added case statements to display
					  Signal Data Histogram Records.
  2.3           3/02   P. Denny         - Update command line arguments
  2.31         12/06   B. Dixon         - Do not require .D and .L files (both)
  3.0           1/07   R. Gens, et al   - CLA cleanup

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*********************************************************************/
/****************************************************************************
*								            *
*   Metadata retrieves ceos structures from SAR metadata		    *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "ceos.h"
#include "asf.h"
#include "get_ceos_names.h"
#include "metadisplay.h"
#include "cla.h"

#define VERSION 3.0

void usage(char *name)
{
   fprintf(stderr,"\n");
   fprintf(stderr,"USAGE:\n");
   fprintf(stderr,"  %s [ <option> ] infile\n",name);
   fprintf(stderr,"\n");
   fprintf(stderr,"OPTIONS:\n");
   fprintf(stderr,"  -save        Write the records to an output file\n");
   fprintf(stderr,"  -all         All records\n");
   fprintf(stderr,"  -dssr        Data Set Summary record\n");
   fprintf(stderr,"  -shr         Scene Header record\n");
   fprintf(stderr,"  -mpdr        Map Projection Data Record\n");
   fprintf(stderr,"  -ppdr        Platform Position Data record\n");
   fprintf(stderr,"  -atdr        Attitude Data record\n");
   fprintf(stderr,"  -radr        Radiometric Data record\n");
   fprintf(stderr,"  -rcdr        Radiometric Compensation Data record\n");
   fprintf(stderr,"  -dqsr        Data Quality Summary record\n");
   fprintf(stderr,"  -pdhr        Processed Data Histograms record\n");
   fprintf(stderr,"  -sdhr        Signal Data Histograms record\n");
   fprintf(stderr,"  -rasr        Range Spectra record\n");
   fprintf(stderr,"  -asf_facdr   ASF Facility Related Data record\n");
   fprintf(stderr,"  -esa_facdr   ESA Facility Related Data record\n");
   fprintf(stderr,"  -ifdr        Image File Descriptor record\n");
   fprintf(stderr,"  -lfdr        Leader File Descriptor record\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"  infile    The base name of the CEOS image\n\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"DESCRIPTION:\n");
   fprintf(stderr,"  Metadata retrieves ceos structures from CEOS metadata\n\n");
   fprintf(stderr,"Version %.2f,  ASF SAR TOOLS\n\n",VERSION);
   exit (1);
}

int main(int argc, char **argv)
{
  char *fileName;

  int dssr_flag = extract_flag_options(&argc, &argv, "-dssr", "--dssr", NULL);
  int shr_flag = extract_flag_options(&argc, &argv, "-shr", "--shr", NULL);
  int mpdr_flag = extract_flag_options(&argc, &argv, "-mpdr", "--mpdr", NULL);
  int ppdr_flag = extract_flag_options(&argc, &argv, "-ppdr", "--ppdr", NULL);
  int atdr_flag = extract_flag_options(&argc, &argv, "-atdr", "--atdr", NULL);
  int ampr_flag = extract_flag_options(&argc, &argv, "-ampr", "--ampr", NULL);
  int radr_flag = extract_flag_options(&argc, &argv, "-radr", "--radr", NULL);
  int rcdr_flag = extract_flag_options(&argc, &argv, "-rcdr", "--rcdr", NULL);
  int dqsr_flag = extract_flag_options(&argc, &argv, "-dqsr", "--dqsr", NULL);
  int pdhr_flag = extract_flag_options(&argc, &argv, "-pdhr", "--pdhr", NULL);
  int sdhr_flag = extract_flag_options(&argc, &argv, "-sdhr", "--sdhr", NULL);
  int rasr_flag = extract_flag_options(&argc, &argv, "-rasr", "--rasr", NULL);
  int ppr_flag = extract_flag_options(&argc, &argv, "-ppr", "--ppr", NULL);
  int ifdr_flag = extract_flag_options(&argc, &argv, "-ifdr", "--ifdr", NULL);
  int facdr_flag = extract_flag_options(&argc, &argv, "-facdr", "--facdr", NULL);
  int asf_facdr_flag = 
    extract_flag_options(&argc, &argv, "-asf_facdr", "--asf_facdr", NULL);
  int esa_facdr_flag = 
    extract_flag_options(&argc, &argv, "-esa_facdr", "--esa_facdr", NULL);
  int lfdr_flag = extract_flag_options(&argc, &argv, "-lfdr", "--lfdr", NULL);
  int all_flag = extract_flag_options(&argc, &argv, "-all", "--all", NULL);
  int save = extract_flag_options(&argc, &argv, "-save", "--save", NULL);

  if (argc == 1)
    usage(argv[0]);

  fileName = (char *) MALLOC(sizeof(char)*255);
  strcpy(fileName, argv[1]);

  if (dssr_flag || all_flag)
    output_record(fileName, ".dssr", 10, save);
  if (shr_flag || all_flag)
    output_record(fileName, ".shr", 18, save);
  if (mpdr_flag || all_flag)
    output_record(fileName, ".mpdr", 20, save);
  if (ppdr_flag || all_flag)
    output_record(fileName, ".ppdr", 30, save);
  if (atdr_flag || all_flag)
    output_record(fileName, ".atdr", 40, save);
  if (ampr_flag || all_flag)
    output_record(fileName, ".ampr", 44, save);
  if (radr_flag || all_flag)
    output_record(fileName, ".radr", 50, save);
  if (rcdr_flag || all_flag)
    output_record(fileName, ".rcdr", 51, save);
  if (dqsr_flag || all_flag)
    output_record(fileName, ".dqsr", 60, save);
  if (pdhr_flag || all_flag)
    output_record(fileName, ".pdhr", 70, save);
  if (sdhr_flag || all_flag)
    output_record(fileName, ".shdr", 71, save);
  if (rasr_flag || all_flag)
    output_record(fileName, ".rasr", 80, save);
  if (ppr_flag || all_flag)
    output_record(fileName, ".ppr", 120, save);
  if (ifdr_flag || all_flag)
    output_record(fileName, ".ifdr", 192, save);
  if (facdr_flag || all_flag)
    output_record(fileName, ".facdr", 200, save);
  if (asf_facdr_flag || all_flag)
    output_record(fileName, ".asf_facdr", 210, save);
  if (esa_facdr_flag || all_flag)
    output_record(fileName, ".esa_facdr", 220, save);
  if (lfdr_flag || all_flag)
    output_record(fileName, ".lfdr", 300, save);

  exit (0);
}

