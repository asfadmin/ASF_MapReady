/*******************************************************************************
NAME:				create_meta

PURPOSE:  Convert given CEOS file into a .meta file.

PROGRAM HISTORY:
PROGRAMMER		DATE		REASON
----------		----		------
O. Lawlor       12/98       Creating asf_meta.a library.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:	

ALGORITHM REFERENCES:  

*******************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu


NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include "asf_meta.h"

#define VERSION 1.0

int main(argc,argv)
int argc;
char *argv[];
{
	meta_parameters *sar;
	if (argc!=3)
	{
		printf("Usage: create_meta <in> <out>\n"
		"\n"
		"  in: a CEOS, AISP, or other metadata input file\n"
		" out: a .meta file, containing the essentials\n"
		"      distilled from above.\n"
		"\n"
		"   create_meta analyses the given input file, \n"
		"and attempts to extract all the metadata it needs.\n"
		"It will create a valid .meta file, or report an error.\n"
		"The .meta file it creates is an ASCII file containing\n"
		"all metadata required for geocoding, interferometric \n"
		"processing, and other ASF software.\n"
		"ASF-STEP, Version %.2f\n",VERSION);
		exit(1);
	}
	sar=meta_create(argv[1]);
	meta_write(sar,argv[2]);
	return 0;
} 
