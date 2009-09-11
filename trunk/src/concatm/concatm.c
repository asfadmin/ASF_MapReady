/*******************************************************************************
NAME:  concatm

SYNOPSIS:  concatm [-c][-m mask][-v ovrlp] [-o <outfile>] nl ns 
		   <file1> sl1 ss1 [<file2> sl2 ss2] [ ... ]

	-c		do color concatenation (first image-> red; 
			second -> green...)
	-m 		mask value for background pixels
	-v		processing option for overlapping pixels
	-o outfile	output file name
	nl		number of lines in output image
	ns   		number of samples in output image
	fileX		input file name #X
	slX		starting line in output image to place fileX
	ssX		starting samp in output image to place fileX	

DESCRIPTION:    Creates an output image by overlaying each input image into
                the output space at the specified locations 

PROGRAM HISTORY:
----------------
PROGRAMMER   VERSION  DATE       REASON
------------------------------------------------------------------------
B. Wilsey     1.0    Aug 1984	 Original development
D. Akkerman   1.1    Aug 1987    PR #4247 modified to allow for single band 
				  specification of a multi-band images
K. Zanter     1.2   Sept l987    NEWLAS (conversion to 'C')
B. Ailts      1.3   July 1988    LAS5.0 conversion -- added -AUTO subcommand
B. Ailts      1.4    Nov 1988    Changed name of cal_line_samp and Set_data_type
				  in order to work on the IBM RT
T. Mittan     1.5    Jan 1992	 Added update, and averaging options, also 
				  maskval can now be a range
T. Mittan     1.6    Mar 1992	 Modified averaging option to choose which 
				  direction the average will be performed.
D. Etrheim    1.7    Sep 1992    Check pixel size against an Epsilon value.
                                 Comparing floating point values directly can
                                  cause problems if an image is transfered from
                                  another machine.
D. Etrheim    1.8    Apr 1993    Correct problem in update mode of deleting the
				  first input image.  Free allocated space.
D. Bridges    1.9    Oct 1993    Modified to allow NL and NS to accept NULL when
				  in undate mode.
T. Logan      2.0    May 1995	 Removed TAE Dependencies for concat manual
O. Lawlor     2.1    Jun 1997	 Changed hostin list to char **, for
			          more-reliable prototypes. (now matches LAS
				  versions.)
S. Watts      2.2    Nov 2001    fixed bug found while autotesting.
P. Denny      2.35   Mar 2002    Updated commandline parsing & usage()

HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:        LAS		                

ALGORITHM DESCRIPTION:
Initialization of variables
Retrieve the user-specified parameters.  
The data type of the output image is determined by the maximum data type 
 of the input images.
For each of the input images
  The projection coordinates are retrieved from the DDR files
  The projection coordinates are re-calculated to compensate for specified
       windows
  The starting line and sample parameters are retrieved
Output image is opened to the specified dimensions (NL and NS parameters)
If not in update mode output image is padded with the first MASK value
For each input image
   The ouput image is opened for update
   The input image is overlaid into the output image according to the 
    appropriate starting line and sample values
The completion message is output.

ALGORITHM REFERENCES:     none
*******************************************************************************/

/******************************************************************************
*                                                                             *
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
#include "las.h"
#include "locinc.h"
#include "concat.h"

#define NIMGS 50
#define VERSION 2.35

/* The following variables must be declared global for image I/O routines */
struct FDESC **fdesc;		/* image file descriptors	          */
struct GDESC *gdesc;		/* group descriptor                       */

int image_starts_in[MAXIMG * MAXBND + 1];/* offsets into thebuf for input */
int image_starts_out[MAXBND+1];		 /* offsets into thebuf for output*/

int doConcat=1;		/* Copy data?          */
int colorConcat=0;	/* Create color image? */
int colorCur=0;		/* Current color band  */

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-c] [-m msk][-v ovr] <outfile> <outfile_nl> <outfile_ns>\n" 
	"                 <infile1> <infile1_sl> <infile1_ss>\n"
	"                 <infile2> <infile2_sl> <infile2_ss>\n"
	"                 [<infileN> <infileN_sl> <infileN_ss>] [...]\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   outfile     Output file name\n"
	"   outfile_nl  Number of line in output image\n"
	"   outfile_ns  Number of samples in the output image\n"
	"   infileN     Input file name #N\n"
	"   infileN_sl  Starting line in output image to place fileN\n"
	"   infileN_ss  Starting samp in output image to place fileN\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"	
	"   -c      do color concatenation (first image-> red; second -> green...)\n"
	"   -m msk  mask value for background pixels (0-255)\n"
	"   -v ovr  Overlap pixel option.  Can be REPLAC, LEAVE, or AVER\n"
	"              <default REPLAC>\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   concatm -- Creates an output image by overlaying each input\n" 
	"   image into the output space at the user specified locations.\n");
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n"
	"\n", VERSION);
 exit(1);
}                                     /*ASF*/

int main(int argc,char **argv)
{
 double line_inc;		 /* increment value in line direction	      */
 double samp_inc;		 /* increment value in sample direction	      */
 float  maskval;		  /* mask value				      */

 static int bands[MAXIMG * NIMGS][MAXBND+1];/* band specification	      */
 int	aver;			 /* average option for overlap condition      */
 int	*laver; 	  	 /* average option for overlap condition      */
 int    cflag;			 /* combination flag for C_UPDDR	      */
 int	image;			 /* curent image			      */
 int    image_count;		 /* total number of images specified	      */
 int 	in_count;		 /* number of IN entries specified 	      */
 int    index;			 /* index				      */
 int	i;			 /* counter				      */
 int	update;			 /* output image flag			      */
 int	leave;			 /* leave option for overlap condition	      */
 int    limage;			 /* current logical image		      */
 int    nbands[MAXIMG * NIMGS];	 /* number of bands in each image	      */
 int    nl;			 /* number of lines in output image           */
 /*int    nl1;*/		 /* number of lines in output image           */
 int    ns;			 /* number of samples per line in output image*/
 /*int    ns1;*/		 /* number of samples per line in output image*/
 int    numimg[NIMGS];		 /* number of images in each IN entry	      */
 int    out_dtype;		 /* data type of output image		      */
 int	replace;		 /* replace option for overlap condition      */
 int    sl[NIMGS];		 /* starting line locations		      */
 int    ss[NIMGS];		 /* starting sample locations		      */
 int	status;			 /* function return status code		      */
 int    totbnd;			 /* total number of output bands              */
 int    *twindow;		 /* temporary window specifications	      */
 int    window[NIMGS * MAXIMG][4]; /* window specification		      */
 int    out_window[NIMGS][4]; 	 /* window specification		      */ 

 char    *hostin[NIMGS];	 /* buffer storage for "hostin"		      */
 char    *hosti_ptr; 		 /* buffer storage for "hostin"		      */
 char    *hosto_ptr; 		 /* buffer storage for "hostin"		      */
 char    hostout[CMLEN];          /* buffer storage for "hostout" 	      */
 char    option[7];	         /* option values			      */
 char    taeout[CMLEN];           /* buffer storage for "taeout"   	      */
 struct DDR *ddr;                 /* pointer to the input DDR structure *//*ASF*/


 fdesc=(struct FDESC **)MALLOC(sizeof(struct FDESC *)*MAXIMG);
 for (i=0;i<NIMGS;i++)
	hostin[i]=(char *)MALLOC(sizeof(char)*CMLEN);


 /* Set default values
 ---------------------*/
 update = FALSE;                         		/*ASF*/
 maskval = 0.0;                          		/*ASF*/
 strcpy(option,"REPLAC");                		/*ASF*/
 in_count = 0;                           		/*ASF*/
 image = 0;                              		/*ASF*/
 ddr = (struct DDR *)MALLOC(sizeof(struct DDR));         /*ASF*/

 /* Parse command line arguments	
 ------------------------------*/
 if (argc > 9)
   currArg=1;	/* from cla.h which is in asf.h */
 else
   {printf("[concatm] Insufficient arguments.\n"); usage(argv[0]);}
 while (argv[currArg][0] == '-')
 {
    char *key=argv[currArg++];
    if (strmatch(key,"-c")) {
      colorConcat = 1;
    }
    else if (strmatch(key,"-m")) {
      CHECK_ARG(1);
      maskval = atof(GET_ARG(1));
      printf("[concatm] Mask value is %f\n",maskval);
    }
    else if(strmatch(key,"-v")) {
      CHECK_ARG(1);
      strcpy(option,GET_ARG(1));
      if ((strcmp(option,"REPLAC")!=0) && (strcmp(option,"LEAVE")!=0) && (strcmp(option,"AVER")!=0))
        {printf("[concatm] -v option requires REPLAC, LEAVE, or AVER.\n"); usage(argv[0]);}
      printf("[concatm] Overlap option is %s\n",option);
    }
    else {printf("[concatm] Invalid option: %s\n",argv[currArg-1]);usage(argv[0]);}
 }
 if ((argc-currArg) < 9) {printf("[concatm] Insufficient arguments.\n"); usage(argv[0]);}

 /* Nab output filename, nl, & ns */
 strcpy(hostout,argv[currArg]);
 strcpy(taeout, argv[currArg++]);
 nl = atoi(argv[currArg++]);
 ns = atoi(argv[currArg++]);
 printf("[concatm] Output image is %s\n",hostout);

 /* Get input names, nl's, & ns's */
 for (; currArg<argc; currArg++)
 {
    strcpy(hostin[in_count],argv[currArg++]);
    if (currArg>=argc) {printf("[concatm] Insufficient arguments.\n"); usage(argv[0]);}
    numimg[in_count] = 1;
    nbands[in_count] = 1;
    out_window[in_count][0] = atoi(argv[currArg++]);
    if (currArg>=argc) {printf("[concatm] Insufficient arguments.\n"); usage(argv[0]);}
    out_window[in_count][1] = atoi(argv[currArg]);
    if (currArg>=argc) {printf("[concatm] Insufficient arguments.\n"); usage(argv[0]);}
    window[in_count][0] = 1;
    sl[in_count] = out_window[in_count][0];
    window[in_count][1] = 1;
    ss[in_count] = out_window[in_count][1];
    status = c_getddr(hostin[in_count],ddr);            /*ASF*/
    if (status != E_SUCC)
       c_errmsg("Fatal error encountered","concatm-fatal",LAS_FATAL);
    window[in_count][2] = ddr->nl;                      /*ASF*/
    window[in_count][3] = ddr->ns;                      /*ASF*/
    printf("[concatm] Input image %i is %s\n",in_count,hostin[in_count]);
    in_count++;
 }                                     /*ASF*/

 if (!in_count)                       /*ASF*/
   {printf("[concatm] Not enough input files.\n"); usage(argv[0]);}

 /* Check for images outside of output image */
 for (i=0; i<in_count; i++)
  {
    if (sl[i]>nl || ss[i]>ns) 
      {
	printf("Starting location of image %i is out of output image\n",i);
	exit(1);
      }
    if (window[i][2]-sl[i]>nl) out_window[i][2] = nl;
    else out_window[i][2] = window[i][2];
    if (window[i][3]-ss[i]>ns) out_window[i][3] = ns;
    else out_window[i][3] = window[i][3];
  }

 if ((laver = (int *)calloc(MAXNS,(sizeof(int *)))) == NULL)
   {
   c_errmsg("Error dynamically alocating memory","concatm-warn",NON_FATAL);
   c_errmsg("Fatal error encountered","concat-warn",LAS_FATAL);
   }

 /* get user-specified parameters and TAE globals 
 ------------------------------------------------*/
 image_count = in_count;
 if (colorConcat)
	totbnd=3;
 else
	totbnd = 1;

 /* determine output data type 
 -----------------------------*/
 set_dtype(hostin,image_count,&out_dtype/*,maskval,&in_count*/);

 /* initialize output image by padding with mask value 
 -----------------------------------------------------*/
 if (!update) pad(hostout,&nl,&ns,&totbnd,&out_dtype,&maskval);

 /* overlay inset images 
 -----------------------*/
 index = 0;
 hosti_ptr = hostin[0];
 if (update)
  {
  hosto_ptr = hostin[0];
  index += numimg[0];
  hosti_ptr = hostin[index];
  limage = 1;
  /*nl1 = nl;*/
  /*ns1 = ns;*/
  }
 else 
  {
  limage = 0;
  hosto_ptr = hostout;
  }

 for (; limage < in_count; limage++)
    {
    if (strcmp(option,"REPLAC") == 0)
       {
       replace = TRUE;
       leave = FALSE;
       aver = FALSE;
       }
    else if (strcmp(option,"LEAVE") == 0)
       {
       replace = FALSE;
       leave = TRUE;
       aver = FALSE;
       }
    else
       {
       replace = FALSE;
       leave = FALSE;
       aver = TRUE;
       for (i = 0 ; i < MAXNL; i++) laver[i] = FALSE; 
       averopt(laver,out_window,&limage);
       }

    if ((window[index][NL] > 0) && (window[index][NS] > 0))
       overlay_img(hosti_ptr,numimg[limage],&window[index],&bands[index],
	   &nbands[index],&sl[limage],&ss[limage],&maskval,hosto_ptr,
	   &out_dtype,&totbnd,&nl,&ns,replace,leave,aver,&update,laver);

 colorCur++;
	if (colorCur>=totbnd)
		colorCur=0;

    index += numimg[limage];
    hosti_ptr =hostin[index];
    }

 /* Update the DDR fields.  Since the input images all start at different places
    in the output image.  Set a temporary window of each of the input images
    to show how they relate to the output space.
 ------------------------------------------------------------------------------*/
 twindow = (int *)calloc(image_count,4 * 4);
 if (twindow == NULL)
   c_errmsg("Nonfatal error encountered","concatm-warn",NON_FATAL);

 for (image = 0; image < image_count; image++)
   {
   *(twindow + image * 4) = window[image][0] - sl[image] + 1;
   *(twindow + (image * 4) + 1) = window[image][1] - ss[image] + 1;
   *(twindow + (image * 4) + 2) = nl;
   *(twindow + (image * 4) + 3) = ns;
   }
 line_inc = 1.0;
 samp_inc = 1.0;
 cflag = COMB;
 if (!update)
   status = c_upddr((const char **)hostin,hosto_ptr,&image_count,&twindow,bands,nbands,
		 &line_inc,&samp_inc,&totbnd,&cflag);
 if (status != E_SUCC)
   c_errmsg("Nonfatal error encountered","concatm-warn",NON_FATAL);

 free(twindow);
   
 /* inform user of successful completion 
 ---------------------------------------*/
 printf("\n[concatm] Concat Manual Successful Completion; Wrote image %s\n\n",hostout);
 for (i=0;i<NIMGS;i++)
	free(hostin[i]);
 return (0);
}


