/*********************************************************************
NAME:     resample.c -- resamples ASF SAR images to desired resolution

SYNOPSIS: resample infile outfile pixsiz

DESCRIPTION:
        Resamples the input file to a desired output pixel resolution
    (assumed to be in meters).  Images are downsized by a factor equal
    to the desired resolution divided by the true resolution of the
    image (as given in the image's metadata).  This is the size used
    for the kernel processing.  Resampling proceeds by selecting the
    pixel in the input image that closest represents the middle of the
    new pixel in the output image.  The output pixel is given the value 
    of the average of the kernel around the choosen input pixel.  This
    program combines the programs filter.c and subsample.c from the
    original EDC Terrain Correction software (LAS Modules).
        The image used as input is single-banded byte valued
    LAS 6.0 .img file.
        The output file produced by resample is a flat file (as per
    LAS 6.0 image standard) and a LAS ddr file is also created or updated
    to provide image metadata (size, etc).

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    c_getddr()		 reads values from ddr file
    c_putddr()		 updates existing ddr file (if input is .img)
    create_ddr()	 creates a new ddr file (if input is .dat)
    filter()		 finds a kernel average at a given point

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     3/17/94      combined LAS modules filter and subsample on YMP
    2.0     3/94         Ported to T3D
    2.1     3/94         Began T3D I/O optimizations
    2.2     10/2/94      T3D Optimizations, Clean Up, & Commenting
    3.0	    4/95	 Ported to Sun workstations.  Allowed input to be
			 an ASF SAR image or a 1-band byte LAS 6.0 image. 
    3.1	    10/4/95      Bug fix - trying to seek line -1 tested for.
    3.2	    10/30/95     Changed call to create_ddr and added timer calls.
    3.3     9/96         Changed to handle RADARSAT era data
    3.4     6/98         Fixed non-ANSI timer routine.      
    4.0     12/98        Paradoxically, removed ASF capability to make
                         cross-dataset capability easier to implement.
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
    Parse command line parameters
    Read input image metadata from ddr file
    Establish kernel processing parameters
    Open input and output files
    for each output line
       calculate file read position
       read next kernel from input file
       if (!LAS) strip ceos wrapper from input data
       for each output pixel
	 apply kernel to input data at appropriate position to get output value
       write output line to file
    if (!LAS) create a ddr for output file     
    else      copy input ddr to output ddr (with update)
    Close input and output files 

ALGORITHM REFERENCES:

BUGS:

AUTHOR:   T. Logan
*********************************************************************/
/****************************************************************************
*								            *
*   resample.c -- resamples ASF SAR images to desired resolution	    *
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
#include "ddr.h"
#include "worgen.h"

float filter(            /****************************************/
    float *inbuf,        /* input image buffer                   */
    int    nl,           /* number of lines for inbuf            */
    int    ns,           /* number of samples per line for inbuf */
    int    x,            /* sample in desired line               */
    int    nsk);
 
#define VERSION 4.0

int main(argc,argv)
    int      argc;
    char     **argv;
{
    FILE            *fpin, *fpout;  /* file pointer                   */
    static   char   infile[255],    /* name of input SAR image file   */
                    outfile[255];   /* name of output RAW file        */
    float   *inbuf,                 /* stripped input buffer          */
            *outbuf;                /* stripped output buffer         */
    struct   DDR       ddr;         /* ddr structure 		      */
    int      np, nl,                /* in number of pixels,lines      */
             onp, onl,              /* out number of pixels,lines     */
             nsk,                   /* kernel size in samples         */
	     half,		    /* half of the kernel size        */
	     n_lines,		    /* number of lines in this kernel */
	     s_line,		    /* start line for input file      */
             xi = 0,                /* inbuf int x sample #           */
             yi = 0,                /* inbuf int y line #             */
              j;                    /* loop counters                  */
    int i;
    float    pixsiz = 0.0,          /* output image pixel size        */
	     scalfact,              /* scale factor                   */
             base,                  /* base sample/line               */
             rate;                  /* # input pixels/output pixel    */

    StartWatch();

   /*--------  Process Command Line Inputs -------------*/
    if (argc != 4)
     {
      printf("\nUsage: %s <infile> <outfile> <pixsiz>\n"
      "     <infile>     LAS 6.0 input image file\n"
      "     <outfile>    LAS 6.0 output image file \n"
      "     <pixsiz>     Pixel size in meters of output image (m)\n"
      "\n"
      "Resamples, via a centered kernel, the given LAS image to\n"
      "the given new size.\n",argv[0]);
      printf("\nVersion %.2f, ASF SAR Tools\n\n",VERSION);
      exit(1);
     }

    strcpy(infile,argv[1]);
    strcpy(outfile,argv[2]);
    sscanf(argv[3],"%f",&pixsiz);

    printf("\n\n\nResample: Performing filtering and subsampling..\n\n");
    printf(" Input image is %s\n",infile);
    printf(" Output image is %s\n",outfile);
    printf(" Output pixel size is %f meters\n",pixsiz);
   
    c_getddr(infile, &ddr);
    /*
      if (ddr.dtype!=1)
      {
        printf("ERROR!  resample only works with byte images.\n"
               "You can convert the image to a byte image using remap's\n"
               "-byte and -map options, or use amp2img\n"
               "Exiting...\n");
        exit(1);
      }
    */
    nl = ddr.nl;
    np = ddr.ns;
    
    scalfact = 1.0/(pixsiz/ddr.pdist_x);
    nsk = (int) (pixsiz/ddr.pdist_x + 0.5);
    if (nsk%2 == 0) nsk++;              /* Must be odd sized kernel */
    
    onp = (np) * scalfact;
    onl = (nl) * scalfact;
    base = 1.0 / (2.0 * scalfact) - 0.5;
    rate = 1.0 / scalfact;
    half = (nsk-1)/2;
    n_lines = nsk;
 
    inbuf= (float*) MALLOC ((unsigned)nsk*np*sizeof(float));
    outbuf = (float*) MALLOC ((unsigned)onp*sizeof(float));
 
   /*----------  Open the Input & Output Files ---------------------*/
    fpin=fopenImage(infile,"rb");
    fpout=fopenImage(outfile,"wb");
 
   /*--------  Process inbuf to give outbuf ------------------------*/
    printf(" Scale Factor : %f\n",scalfact);
    printf(" Kernel Size  : %i\n",nsk);
    printf(" base-- %f rate-- %f\n\n",base,rate);
    printf("Writing raw byte image file %i samples by %i lines\n\n",onp,onl);
 
    for (i = 0; i < onl; i++)
      {
       /*--------- Read next set of lines for kernel ---------------*/
       yi = i * rate + base + 0.5;
       s_line = yi-half;
       if (s_line < 0) s_line = 0;
       if (nl < nsk+s_line) n_lines = nl-s_line;
      
       FSEEK(fpin,s_line*np,0);
       // FREAD(inbuf,np*n_lines,1,fpin);
       getFloatLine(fpin, &ddr, i, inbuf);
       /*--------- Produce the output line and write to disk -------*/ 
       for (j = 0; j < onp; j++)
         {
          xi = j * rate + base + 0.5;
          outbuf[j] = filter(inbuf,n_lines,np,xi,nsk);
         }
       
       // FWRITE(&outbuf[0],onp,1,fpout);
       putFloatLine(fpout, &ddr, i, outbuf);
       if (i%40==0) 
          printf(" Processing Output Line %i\n",i);
      }
        
   /*----------  Close Files and Exit  -----------------------------*/
	ddr.nl = onl;
	ddr.ns = onp;
	ddr.pdist_x = pixsiz;
	ddr.pdist_y = pixsiz;
	j = c_putddr(outfile,&ddr);

    FCLOSE(fpin);                 
    FCLOSE(fpout);
    StopWatch();
    return(0);
}
