/******************************************************************************
NAME:    get_rectypes.c

SYNOPSIS: get_rectypes sarfile.ext 

DESCRIPTION: Reads an ASF groundstation file and prints the
             names of all of the records found therein

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
VERSION		DATE 	AUTHOR		PURPOSE
-------		----	------		--------------------------------
  1.0		4/96	T. Logan	Initial Development
  1.1		6/01	P. Denny	Changed from function to program

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   get_rectypes -- reads an ASF groundstation file and prints the	    *
*                   names of all of the records found therein  	 	    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ceos.h"
#include "asf.h"
#include "asf_endian.h"

#define VERSION 1.1

int main(int argc, char **argv)
{
	FILE 	 *fp;
	int	 itype, length;
	struct HEADER	bufhdr;

	if (argc != 2)
	{
		printf( "\nUsage: ");
		printf( "%s <file>\n\n",argv[0]);
		printf( "	<file> File (with extension) to get records from.\n\n");
		printf( "get_rectypes reads an ASF groundstation file\n"
				"and prints the names of all of the records found\n"
				"therein.\n\n");
		printf("VERSION %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}
	
	fp = FOPEN(argv[1], "r");
	while (1)
	{
		if (fread(&bufhdr,sizeof(bufhdr),1,fp)!=1)
		{
			printf("**End of file detected.\n");
			fclose(fp);
			exit(0);
		}
		itype = bufhdr.rectyp[1];
		switch (itype) {
		  case (10): printf("Data Set Summary Record.\n"); break;
		  case (11): printf("Data Record.\n"); break;	
		  case (20): printf("Map Projection Data Record.\n"); break;
		  case (30): printf("Platform Position Data Record.\n"); break;
		  case (40): printf("Attitude Data Record.\n"); break;
		  case (50): printf("Radiometric Data Record.\n"); break;
		  case (51): printf("Radiometric Compensation Record.\n"); break;
	          case (60): printf("Data Quality Summary Record.\n"); break;
		  case (70): printf("Data Histograms Record.\n"); break;
		  case (80): printf("Range Spectra Record.\n"); break;
		  case (90): printf("Digital Elevation Model Descriptor Rec.\n"); break;
		  case (100): printf("Radar Parameter Data Update Record.\n"); break;
		  case (110): printf("Annotation Data Record.\n"); break;
		  case (120): printf("Detailed Processing Parameters Record.\n"); break;
		  case (130): printf("Calibration Data Record.\n"); break;
		  case (140): printf("Ground Control Points Record.\n"); break;
		  case (192): printf("File Descriptor Record.\n"); break; 
		  case (200): printf("Facility Related Data Record.\n"); break;
		  case (201): printf("GPS Metadata Record.\n"); break;
		  case (210): printf("Facility Related Data Record (RADARSAT).\n"); break;
		  case (300): printf("Leader File Descriptor Record.\n"); break;
		  default: printf("Not Valid Record Type: %d\n",itype);
		}

		length = bigInt32(bufhdr.recsiz) - 12;
		if((fseek(fp, length, 1)) != 0) 
		{
			printf("**Error scanning over data portion of record, exiting.\n");
			fclose(fp);
			exit(0);
		}
	}
	return 0; /* for whiny compilers */	
}
