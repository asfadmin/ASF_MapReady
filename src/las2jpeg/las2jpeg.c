/****************************************************************
NAME: las2jpeg

SYNOPSIS: las2jpeg [-m | -p palfile] <infile> <outfile>

		 -m         input file is an unwrapped mask file.
		 -p         apply palfile to input byte file.
		 <infile>   A 1- or 3- banded LAS byte image.
		 <outfile>  a JPEG/JFIF image file.

DESCRIPTION:
	Las2jpeg  converts a 1- or 3- banded LAS byte image into a
       	JPEG image using either built-in palette files or  a  user
       	specified  palette  file.   JPEG  images  are usually much
       	smaller, and can be read in many other software  packages.
	Calls the jpeg library to JPEG compress it and write
	it out.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0                  O. Lawlor - Original Development

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/****************************************************************************
*								            *
*   las2jpeg -- reads a LAS file into RGB order in memory, and		    *
*		then calls the jpeg library to JPEG compress it and write   *
*		it out. 						    *
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

#include "asf.h"
#include "ifm.h"
#include "ifm2ppm.h"
#include "ddr.h"

#define BUF          256
#define VERSION      1.0
#define INC          5

#define zMASK        0x01
#define zXID         0x02
#define zUSER        0x04
#define zWRITE       0x10

#define zALL         0x07


int main (int argc, char *argv[])
{
  char infile[BUF];
  char outfile[BUF];
  char *outpal=NULL;
  Uchar *out;
RGBDATA table[256];
  Uchar *band;
  FILE *fin, *fout;
  Uchar flags = 0;
  int wid,len;
  int cnt, i;
  extern char *optarg;
  extern int optind;
  struct DDR ddr;
  int c;
  int x,y;

  /* Start time keeping*/
  StartWatch();
   
  /* handle command line args*/
  while ((c=getopt(argc,argv,"dmo:p:")) != EOF)
    switch (c) {
       case 'm':
          mask_colortable(table);
	  flags |= zMASK;
          break;
       case 'p':
	  user_colortable(table,optarg);
	  flags |= zUSER;
	  break;
       case '?':
          fprintf(stderr,"Invalid option.\n");
          usage(argv[0]);
          break;
    }

  /* set constants & other variables  */
  if (argc - optind != 2)  usage(argv[0]);  
  if ( !(flags & zALL)) 
    grey_colortable(table);
  strcpy(infile,argv[optind]);
  strcpy(outfile,argv[optind+1]);
  c_getddr(argv[optind],&ddr);
  wid=ddr.ns;
  len=ddr.nl;
  if (ddr.nbands==3)
  	flags |=zXID;
  
  /* open files */
  fin = fopenImage(infile,"rb");
  fout = FOPEN(appendExt(outfile,".jpg"),"w");
  
  /*
   * malloc buffers, check and open files 
   */
  band = (Uchar *)MALLOC(wid * len * 3);
  out = (Uchar *)MALLOC(len * wid * 3);

  /*
   * process data
   */
  if (flags & zXID) {
    FREAD(band,sizeof(Uchar),3*wid*len,fin);
    for (cnt=0; cnt<3; cnt++) {
      for (y=0;y<len;y++)
         for (x=0;x<wid;x++)
 	    out[(y*wid+x)*3+cnt] = band[cnt*wid*len+y*wid+x];
    }
  } else { 
     FREAD(band,sizeof(Uchar),len*wid,fin);
     for (x=0; x < len*wid; x++)
     {
	i=x*3;
        out[i  ] = table[band[x]].red;
        out[i+1] = table[band[x]].green;
        out[i+2] = table[band[x]].blue;
     }
  } 
  
  if (flags & zWRITE) write_table(table,outpal);

  write_image(fout,out,wid,len);

  /* close output file & exit*/
  free(band);
  free(out);
  fclose(fin);
  fclose(fout);
  StopWatch();
  return(0);
}

void usage(char *name)
{
  printf("\nusage: %s [-m | -p palfile] <infile> <outfile>",name);
  printf("\n");
  printf(" -m         input file is an unwrapped mask file.\n");
  printf(" -p         apply palfile to input byte file.\n");
  printf(" <infile>   A 1- or 3- banded LAS byte image.\n");
  printf(" <outfile>  a JPEG/JFIF image file.\n");
  printf("\n"
	"las2jpeg converts the given LAS byte image\n"
	"into a JPEG file.  This is a common image file\n"
	"format, used on the world-wide web.\n\n");
  printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
}
