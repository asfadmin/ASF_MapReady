/******************************************************************************
NAME: convert_geo_ddr.c

SYNOPSIS:convert_geo_ddr infile outfile

DESCRIPTION:
        Takes a geograhic .ddr file and converts corner coordinates, 
	projection distance, and projection units from units of seconds 
	to units of degrees.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    c_getddr()		 reads values from .ddr file
    c_putddr()		 updates existing .ddr file (if input is .img)

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	  6-19-01  Sherman Watts  Values from Geograhic .ddr file matches the
                                  the values from the .img file viewed by xid.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   convert_geo_ddr -- converts values in units of seconds to degrees.      *
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
*	Alaska SAR Facility			APD Web Site:		    *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "ddr.h"

#define VERSION 1.0

void usage(char *name);

main(int argc,char *argv[])
{
  FILE            *fpin, *fpout;  /* file pointer                   */
  static   char   infile[255],    /* name of input SAR image file*/
                  outfile[255];   /* name of output RAW file*/
  struct   DDR       ddr;         /* ddr structure*/
  char     projection_units[12] = "degrees";
  int      i;		            /*index for loops*/
  
   /*--------  Usage and Command Line Inputs -------------*/
  if (argc != 3)
    usage(argv[0]);
  
  strcpy(infile,argv[1]);
  strcpy(outfile,argv[2]);
  
  printf(" Input *.ddr is %s\n",infile);
  printf(" Output *.ddr is %s\n",outfile);
  
  c_getddr(infile, &ddr);
  
  /**********CALCULATIONS FOR CONVERSION**********/
  for (i = 0; i < 2; i++)
    {
      ddr.upright[i] = ddr.upright[i] / 3600;
      ddr.upleft[i] = ddr.upleft[i] / 3600;
      ddr.loright[i] = ddr.loright[i] / 3600;
      ddr.loleft[i] = ddr.loleft[i] / 3600;
    }	
  ddr.pdist_x = ddr.pdist_x / 3600;
  ddr.pdist_y = ddr.pdist_y / 3600;
  strcpy(ddr.proj_units, projection_units);
  
  
  /*********OPEN FILES, WRITE TO OUTPUT FILE, CLOSE FILES*********/
  
  fpin = FOPEN(infile,"rb");
  fpout = FOPEN(outfile,"wb");

  c_putddr(outfile, &ddr);	
  
  fclose(fpin);                 
  fclose(fpout);
  
  exit(0);
}


/********************************************************************
************VOID PROCEDURE TO GIVE PROGRAM USAGE*********************
********************************************************************/
void usage(char *name)
{
 printf("\nUsage: %s <infile> <outfile>\n"
        "\t    <infile>     Input file with extension (*.ddr)\n"
        "\t    <outfile>    Output file with extension (*.ddr)\n",name);
 printf("\nTakes a geograhic .ddr file and converts corner "
        "coordinates,\nprojection distance, and projection "
	"units from units of seconds\nto units of degrees.\n");
 printf("\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
 exit(1);
}
