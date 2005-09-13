/******************************************************************************
NAME: trim_slc

SYNOPSIS: trim_slc <inSLCfile> <outCPXfile> <new top line> <new left sample>
	[<trim new height> <trim new width>]

DESCRIPTION:
	Trim_slc does two things: it converts a single-int-complex ASF CEOS
format image into a LAS 6.0 float-complex image, and allows you to trim that image.

	You specify the input and output filenames (WITH extention), and 
a top left coordinate.  Trim_slc will create a new image in outfile which
starts at these coordinates.  Optionally, you can also specify the new height 
and width of the output image (if not specified, trim will use the old size
minus the top left coordinates-- i.e. minus the part that is trimmed off).

	Trim_slc creates a valid LAS 6.0 DDR (data descriptor record) from
the CEOS metadata.

	This program should work with, but has not been tested with,
RADARSAT era datafiles and metadata.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   Orion Lawlor Needed to import and trim SIC files
    				for interferometry.
    1.1	    5/98   Orion Lawlor Caplib/bug fix.
    1.2     12/98  Orion Lawlor Modified language for dataset-correctness.
    1.3     4/00   Mark Ayers   Added .meta file creation to trim_slc
    1.4     6/00   D. Koster    Modified to handle files > 2GB
    1.5     7/01   R. Gens	Added a flag for Focus processor
    1.75   12/03   P. Denny     Update command line order / parsing
                                 Update to meta1.1 (remove DDR use).

HARDWARE/SOFTWARE LIMITATIONS: 

SEE ALSO:
	trim(1)

BUGS: none known

******************************************************************************/
/****************************************************************************
*								            *
*   Trim_slc: lets you trim/add any # of samples to a Single Int Complex    *
*	      image file.						    *
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
#include "ceos.h"
#include "las.h"
#include "asf_meta.h"

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#define NUM_ARGS 4

#define VERSION 1.75


int main(int argc, char *argv[])
{
   int era;
   int in_bytes_per_line,line_header;
   struct IOF_VFDR vfdr;
   struct VFDRECV asf_facdr;
   int pixelSize=8;
   int x,y,startX,startY,endX,endY,inMaxX,inMaxY,outMaxX,outMaxY;
   int lastReadY,firstReadX,numInX;
   FILE *in,*out;
   float *outbuffer;
   short *inbuffer;
   meta_parameters *meta;
   char infile[256],infileData[256],outfileData[256],outfileMeta[256];
   int widthFlag=FALSE, heightFlag=FALSE;
   int widthTemp, heightTemp;

/* Parse command line arguments */
   while (currArg < (argc-NUM_ARGS)) {
      char *key = argv[currArg++];
      if (strmatch(key,"-width")) {
         CHECK_ARG(1);
         widthTemp = atoi(GET_ARG(1));
         widthFlag = TRUE;
      }
      else if (strmatch(key,"-height")) {
         CHECK_ARG(1);
         heightTemp = atoi(GET_ARG(1));
         heightFlag = TRUE;
     }
      else {
         printf( "\n**Invalid option:  %s\n",argv[currArg-1]);
         usage(argv[0]);
      }
   }
   if ((argc-currArg) < NUM_ARGS) {
      printf("Insufficient arguments.\n");
      usage(argv[0]);
   }
   /* Name infile extention based on whether CEOS is pre or post RADARSAT */
   era = set_era(argv[currArg],infileData,0);
   era = set_era(infileData,infile,1);

   create_name(outfileData,argv[currArg+1],".cpx");
   create_name(outfileMeta,argv[currArg+1],".meta");
   startY = atoi(argv[currArg+2]);
   startX = atoi(argv[currArg+3]);

/* Get metadata */
   meta = meta_create(infileData);
   get_ifiledr(infile,&vfdr);
   get_asf_facdr(infile,&asf_facdr);

   line_header = (era==0) ? 12 : vfdr.predata;
   in_bytes_per_line = vfdr.reclen;

/* Calculate trimmed dimensions */
   inMaxX = asf_facdr.apixels;
   inMaxY = asf_facdr.alines;
   endY = (heightFlag) ? startY+heightTemp : inMaxY-startY;
   endX = (widthFlag) ? startX+widthTemp : inMaxX-startX;
   outMaxX = endX-startX;
   outMaxY = endY-startY;

/* Update metadata */
   meta->general->data_type = COMPLEX_REAL32;
   meta->general->line_count = outMaxY;
   meta->general->sample_count = outMaxX;
   meta->general->start_line = startY;
   meta->general->start_sample = startX;

/* Open the files */
   in=fopenImage(infileData,"rb");
   out=fopenImage(outfileData,"wb");

/* If everything's OK, then allocate an input and output buffer big enough for
 * one line of input and output data, respectively.*/
   outbuffer= (float *)MALLOC(2*sizeof(float)*outMaxX);
   inbuffer= (short *)MALLOC(2*sizeof(short)*inMaxX);

/* Let the user know what's goin' down */
   printf("trim_slc: input SLC: '%s'. output Float-Complex: '%s'.\n",infileData,outfileData);
   if (0!=strncmp(vfdr.formcode,"CI*4",4)) {
      printf("Fatal Error: This program only works with\n"
             "  Single Int Complex input data ('COMPLEX INTEGER*4').  \n"
             "  This data file is ('%s').\n",vfdr.formatid);
      exit(EXIT_FAILURE);
   }
   printf("        Input image size: Lines=%i, Samples=%i\n",inMaxY,inMaxX);
   printf("        Output image size: Lines=%i, Samples=%i\n",outMaxY,outMaxX);


/*If necessary, fill the top of the output with zeros, by loading up a buffer and writing.*/
   for (x=0;x<outMaxX*2;x++)
  	 outbuffer[x]=0.0;
   for (y=0;y<-startY && y<outMaxY;y++)
   {
      FWRITE(outbuffer,pixelSize,outMaxX,out);
      if (0==y%100) printf("        Writing output image line %i...\n",y);
   }

/*Do some calculations on where we should read.*/
   firstReadX=MAX(0,-startX);
   numInX=MIN(MIN(outMaxX,inMaxX-(firstReadX+startX)),outMaxX-firstReadX);
   lastReadY=MIN(outMaxY,inMaxY-startY);

   for (;y<lastReadY;y++) {
       const int SLCPixSize=4;
       int inputY = y+startY;
       int inputX = firstReadX+startX;

       FSEEK64(in,(inputY+1)*in_bytes_per_line+line_header+SLCPixSize*inputX,0);
       FREAD(inbuffer,SLCPixSize,numInX,in);
       for (x=0;x<numInX;x++) {
           outbuffer[2*(x+firstReadX)]=inbuffer[x*2];
  	   outbuffer[2*(x+firstReadX)+1]=inbuffer[x*2+1];
       }
       FWRITE(outbuffer,pixelSize,outMaxX,out);
       if (0==y%100) printf("    Writing output image line %i...\n",y);
   }
/*Reset buffer to zeros and fill remaining pixels.*/
   for (x=0;x<outMaxX*2;x++)
  	 outbuffer[x]=0.0;
   for (;y<outMaxY;y++)
   {
  	 FWRITE(outbuffer,pixelSize,outMaxX,out);
  	 if (0==y%100) printf("    Writing output image line %i...\n",y);
   }

/* now that it worked, write the metadata file */
   meta_write(meta,outfileMeta);

   printf("trim_slc completed sucessfully!\n");

/*Now free memory and close up the files, 'cause we're done.*/
   meta_free(meta);
   FREE(inbuffer);
   FREE(outbuffer);
   FCLOSE(in);
   FCLOSE(out);

   exit(EXIT_SUCCESS);
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-width <width>] [-height <height>]\n"
	"            <inSLC> <outCPX> <top_line> <left_samp>\n",name); 
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inSLC      Single-Look-Complex image files (.D and .L or .dat and .ldr)\n"
 	"   outCPX     Output file of <inSLC> in float complex format.\n"
	"   top_line   Upper coordinate (line)\n"
	"   left_samp  Left coordinate (sample)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -width    Trim/enlarge to new <width>\n"
	"   -height   Trim/enlarge to new <height>\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Allows you to trim/add any number of samples to a Single Int Complex image\n"
	"   file, while creating a .meta, and converting to float complex format.\n");
 printf("\n"
	"Version %4.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
