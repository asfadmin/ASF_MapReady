/****************************************************************
NAME:  idtable

SYNOPSIS: idtable table_file file1...

DESCRIPTION:
	Creates a table file which has an id number that correspond to the
	input files. Each input file is then copied with each nonzero value
	receiving the number located in the idtable.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     11/95        Original design

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   idtable -- Creates a table file which has an id number that corresponds *
*  	       to the input files.					    *
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
#include "ceos.h"
#include "worgen.h"

/* constants and type definitions */
#define VERSION  1.0
#define BUF	 256
#define BUFFER   1000000
typedef unsigned int Ulong;

/* local function declaration */
void print_usage(void);

int main (argc, argv) 
int	argc;
char	**argv;
{
  FILE *fTableFile, *fImgFile, *fIdImgFile;
  char trlfile[BUF], imgfile[BUF], idimgfile[BUF];
  char *imgbuffer;
  int num_images, number;
  int i;
  int /*img_id,*/ items;
  /*struct VFDRECV vfdr;*/  /* FDR in ASCII format */

  /* check args */
  if (argc < 3)
    print_usage();

  /* open table file */
  fTableFile=FOPEN(argv[1],"w");
  
  /* allocate memory for image buffer */
  imgbuffer = (char *)MALLOC(BUFFER * sizeof(char));
  
  /* 
   * loop through each image
   * write out id numbers to file
   * convert to new file
   */
   printf("%s: looping through each image\n",argv[0]);
   num_images = argc - 1;
   for (number = 1; number < num_images; number++) {
      /* generate file names */
      sprintf(trlfile,"%s",argv[number+1]);
      sprintf(imgfile,"%s.img",argv[number+1]);
      sprintf(idimgfile,"id%s.img",argv[number+1]);
      printf("\tWorking on image '%s'\n",imgfile);      

      /* add entry to table */ 
      fprintf(fTableFile,"%d\t%d\n",number,number);

      /* open image files */
      fImgFile = fopenImage(imgfile,"rb");
      fIdImgFile = fopenImage(idimgfile,"wb");
      if (fImgFile == NULL || fIdImgFile == NULL) {
	 fprintf(stderr,"%s: could not open file '%s' and/or '%s'\n",
		 argv[0],imgfile,idimgfile);
         exit(4);
      }

      /* 
       * read in data, convert it, 
       * and write it out to the new file.
       */
      do {
        items = fread(imgbuffer,sizeof(char),BUFFER,fImgFile);
        for (i=0; i<items; i++) {
	  if (imgbuffer[i] != 0)
	    imgbuffer[i] = number;
        }
        fwrite(imgbuffer,sizeof(char),items,fIdImgFile);
      } while (!feof(fImgFile));

      /* close these files and run again */
      fclose(fImgFile);
      fclose(fIdImgFile);
   }

   /* close and free files */
   fclose(fTableFile);
   free(imgbuffer);

   return 0;
}

/* Print the usage of the program */
void print_usage() {
  fprintf(stderr,"\nUsage: %s table_file file1...\n",
	  "idtable");
  fprintf(stderr,"\nCreates a table file which has an id number that"
	" corresponds\nto the input files.\n");
  fprintf(stderr,"Version %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit (1);
}
