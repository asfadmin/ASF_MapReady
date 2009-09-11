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
