/****************************************************************************
NAME:      tpl_search

SYNOPSIS:  tpl_search type image outtpl 
  
	type	either a or d, specifies whether an ascending or a
                descending SAR image was used as input, and modifies
		the search orientation appropriately.
	image	input LAS 6.0 format SAR image (.img) with metadata (.ddr)
	outtpl	output ascii tiepoint location file

DESCRIPTION:
	Performs an adaptive search of the input SAR image to find
        ideal location for tie point correlation.  This is done
        by finding areas of high contrast in an image that should 
        correspond to mountain peaks or other discernible image 
        features. 
 
EXTERNAL ASSOCIATES:	greycorr.c 	program that uses outtpl file

FILE REFERENCES:	image.img	input SAR image (LAS 6.0 format)
			image.ddr	input image metadata (LAS 6.0)
			outtpl.tpl	output ascii tie-point file

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0	    4/95	Increase chance of proper image correlation
			by using points with high intensity changes
    2.0	    10/95	Converted to asf_tools standards. Ported from
                        YMP to Solaris.  Added calls to read input
			file's .ddr to get the image size.
    3.0      7/97       Produce tpl file suitable for 2-pass correlation
    3.1	    11/98	Added scaling based on image size to reduce number
			of tie point for 30m correlations

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
	Iteratively searches for an acceptable number of tie point locations
   based on cross track pixel to pixel intensity variations.  Starting with
   a high minimum pixel value and a large difference threshhold, the para-
   meters thresh and min are slowly lowered until enough pixels fit the
   criteria of being above min and more than thresh from the next range 
   pixel.  

ALGORITHM REFERENCES:

BUGS:

****************************************************************************/
/******************************************************************************
*                                                                             *
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
#include "ddr.h"
#include "asf_reporting.h"

#define  VERSION	3.1

int main(argc,argv) int argc; char **argv;
{
	char  infile[255],		/*  input SAR image file name     */
	      outfile[255];		/*  output tie point file name    */
	FILE  *fpin, *fpout;		/*  File pointers 		  */
	int   tpl[5000][2];		/*  List of valid tie points      */
        float *buf;		        /*  Input file buffer		  */
	double dbuf, dbuf2;             /*  double version of buf values  */
	int   min;			/*  the minimum pixel value to be */
					/*    accepted as a tie point     */
	int   offset;			/*  Offset into the input buffer  */
	int   nl,np;			/*  Number of lines and samples   */
	int   i, j;			/*  Loop counters		  */
	int   thresh;			/*  Differencing Methods          */
	int   npts = 0,			/*  Number of tie points found    */
              large = 0;		/*  # points > min value          */
	char  opt;			/*  Image pass option a or d      */
        int   line_skip = 8;		/*  # lines to skip between checks */
	int   samp_skip = 32;		/*  # samples to skip between checks */
        struct   DDR    ddr;         	/*  ddr structure                 */

if (argc != 4) 
  {
    asfPrintStatus("\n");
    asfPrintStatus("Usage:  %s a|d image outtpl \n",argv[0]);
    asfPrintStatus("    inputs:  image    LAS 6.0 format SAR image (.img)\n");
    asfPrintStatus("    outputs: outtpl   Tie point location file\n");
    asfPrintStatus("    type:    a | d    Specify ascending or "
		   "descending image\n\n");
    asfPrintStatus("    Version %.2f,  ASF Tools\n\n",VERSION);
    exit(1);
  }

StartWatch();

opt = argv[1][0];
strcat(strcpy(infile,argv[2]),".img");
strcat(strcpy(outfile,argv[3]),".tpl");

if (opt == 'A' || opt == 'a') opt = 'A';
else if (opt == 'D' || opt == 'd') opt = 'D'; 
else
 {
   asfPrintStatus("Image type is either a(scending) or d(escending)\n");
   asfPrintError("Invalid image type option : %c\n",opt);
 }

if (c_getddr(argv[2], &ddr)!=0)
  { asfPrintError("Unable to get ddr for file %s\n",infile); }
nl = ddr.nl; np = ddr.ns;

asfPrintStatus("Image %s is type %c\n",infile,opt);
asfPrintStatus("Image size is %i by %i\n",np,nl);
 
buf = (float *) MALLOC (np*nl);

fpin = fopenImage(infile,"rb");
for (i = 0; i < nl; ++i)
  getFloatLine(fpin, &ddr, i, &buf[i*np]);
FCLOSE(fpin);

npts = 0;

/****** Not sure that I like this addition (TL 11/98) : *******/
/* line_skip =(int) ((float)line_skip * ((float)nl / 1024.0)); */
/* samp_skip =(int) ((float)samp_skip * ((float)np / 1024.0)); */
 
asfPrintStatus("Skipping %i pixels after a good point, every %i lines\n",
	       samp_skip, line_skip);

for (min = 240; npts < 150 && min > 99; min -= 10)
  for (thresh = min-10; npts < 150 && thresh > 49; thresh -= 10) {
     for (i = 32,npts = 0,large = 0; i < (nl-32); i+=line_skip) {
        offset = i*np;
        for (j = 32; j < (np-64); j++) {
          dbuf = (double) buf[offset+j];
          if (dbuf > min) {
            large++;
	    if (opt == 'A') dbuf2 = (double) buf[offset+j-1];
	    else            dbuf2 = (double) buf[offset+j+1];
            if ((dbuf-dbuf2)>thresh) 
		{ tpl[npts][0] = i; tpl[npts][1] = j; npts++; j += samp_skip; }
          }   /* dbuf > min */
        }   /* loop on j */
      }  /* loop on i */
    if (large < 200) thresh = 0;
  }  /* loop on min and thresh */

/* Write the output file */
fpout = fopen(outfile,"w");
for (i=0; i<npts; i++)
  fprintf(fpout,"%i %i %i %i\n",tpl[i][0],tpl[i][1],tpl[i][0],tpl[i][1]);
fclose(fpout);

asfPrintStatus("Tpl_search: #Pts >%i & >%i from neighbor = %i\n",
	       min,thresh,npts);
StopWatch();
exit(0);
}

