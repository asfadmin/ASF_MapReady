/****************************************************************
NAME:  idimage

SYNOPSIS: idimage <table_file> <infile> <outfile>

DESCRIPTION:
	Replaces table numbers read from infile with id numbers corresponding
	to table values located in table_file. All output is written to
	outfile. infile & outfile should be base names. The proper extensions
	(.ddr & .img) will be added.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     11/95        M. Shindle Original design

HARDWARE/SOFTWARE LIMITATIONS:
   Change idtable so that it determines amount of memory needed at run time.
   I also assume just one banded images.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
   At most the table can handle 80 table values.
   If image has more than one band, output ddr will be incorrect.

****************************************************************/
/****************************************************************************
*								            *
*   idimage -- replaces table numbers from infile with id numbers           *
*	       corresponding to table value in table_file. 		    *
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
#include "las.h"
#include "asf_las.h"

/* constants and type definitions */
#define VERSION  1.0
#define BUF	 256
#define BUFFER   500000
#define uint unsigned int

/* local function declaration */
void print_usage(void);

int main (argc, argv) 
int	argc;
char	**argv;
{
  FILE *fTableFile, *fin, *fout;
  char inname[BUF], inimage[BUF]; 
  char outname[BUF], outimage[BUF];
  char *inbuffer;
  uint *outbuffer;
  int idtable[81];
  int i, number;
  int img_id, items;
  /*int one = 1;*/
  struct DDR ddr;

  /* check args */
  if (argc != 4)
    print_usage();
  
  /* obtain file names */
  strcpy(inname,argv[2]);
  strcpy(outname,argv[3]);
  sprintf(inimage,"%s.img",inname);
  sprintf(outimage,"%s.img",outname);

  /* open table file */
  fTableFile=FOPEN(argv[1],"r");

  /* 
   * assign id numbers to table
   * set the the 1st value to zero
   */
  idtable[0] = 0;
  printf("idimage:\n\tConstructed the following image id table:\n");
  printf("\t-----------------------------------------\n");
  while (fscanf(fTableFile,"%d %d",&number,&img_id) != EOF) {
   /* fscanf(fTableFile,"%d %d",&number,&img_id); */
    idtable[number] = img_id;
    printf("%d\t  %d\n",number,img_id);
  } 

  /* allocate memory for image buffer */
  inbuffer = (char *)MALLOC(BUFFER * sizeof(char));
  outbuffer = (uint *)MALLOC(BUFFER *sizeof(uint));

  /* 
   * open image files
   * convert to new file
   */
   printf("\n\tconverting image\n");
   /* open image files */
   fin = fopenImage(inimage,"rb");
   fout = fopenImage(outimage,"wb");
   if (fin == NULL || fout == NULL) {
     fprintf(stderr,"%s: could not open file '%s' and/or '%s'\n",
	     argv[0],inimage,outimage);
     exit(4);
   }
   
   /*
    * loop through file
    * read in data, convert it, 
    * and write it out to the new file.
    */
   do {
     items = fread(inbuffer,sizeof(char),BUFFER,fin);
     for (i=0; i<items; i++) {
       outbuffer[i] = idtable[(int)inbuffer[i]];
     }
     fwrite(outbuffer,sizeof(uint),items,fout);
   } while (!feof(fin));

   /* close all files */
   fclose(fin);
   fclose(fout);
   fclose(fTableFile);

   /* free memory */
   free(inbuffer);
   free(outbuffer);

   /* 
    * create new ddr
    * change data size
    * write out
    */
   printf("\tmodifying the ddr\n");
   c_getddr(inname,&ddr);

   ddr.dtype = ELONG;

   c_putddr(outname,&ddr);

   /* return and exit */
   printf("\texiting idimage\n");
   return 0;
}

/* Print the usage of the program */
void print_usage() {
  fprintf(stderr,"\nUsage: %s <table_file> <infile> <outfile>\n",
	  "idimage");
  fprintf(stderr,"    <infile> & <outfile> will have extensions appended.\n");
  fprintf(stderr,"\nReplaces table numbers from infile with id numbers\n"
	"corresponding to table value in table_file.\n");
  fprintf(stderr,"\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit (1);
}
