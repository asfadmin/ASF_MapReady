/*******************************************************************************
NAME:	concat

PURPOSE:
	Create an output image by overlaying each input image into
	the output at the specified locations 

SYNOPSIS:
	concat [-m mask][-v ovrlp][-c][-n] <-o outfile> <file1 file2 ...>

DESCRIPTION:
	Create an output image by overlaying each input image into
	the output at the specified locations.  This Unix routine
	provides similar functionality to the LAS 6.0 routine
	concat-auto, but operates from the Unix command line.  The
	output is a large area geocoded SAR image with a valid DDR,
 	describing the geolocation of the image.

	inputs:	file1 file2 ...	LAS 6.0 .img files and there .ddr
				valid metadata files.  At least
				one input image must be specified.
				Inputs should not include extensions

	output:	outfile.img,	LAS 6.0 .img file and its
		outfile.ddr	.ddr file giving the geolocation.
				This input must be specified.
				No extension should be given, the
				.img will be appended.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
PROGRAMMER    VERSION    DATE	REASON
----------    -------	-----	------------------------------------
B. Wilsey	1.0	 8/84	Original development
D. Akkerman     1.1	 8/87	PR #4247 modified to allow for single band 
				  specification of a multi-band images
K. Zanter	1.2	 9/87	NEWLAS (conversion to 'C')
B. Ailts	1.3	 7/88	LAS5.0 conversion -- added -AUTO subcommand
B. Ailts		11/88	Changed name of cal_line_samp and Set_data_type
				  in order to work on the IBM RT
T. Mittan		 1/92	Added update, and averaging options, also 
				  maskval can now be a range
T. Mittan		 3/92	Modified averaging option to choose which 
				  direction the average will be performed.
D. Etrheim		 9/92	Check pixel size against an Epsilon value.
                                  Comparing floating point values directly can
                                  cause problems if an image is transfered from
                                  another machine.
D. Etrheim		 4/93	Correct problem in update mode of deleting the
				  first input image.  Free allocated space.
D. Bridges	2.6	10/93	Modified to allow NL and NS to accept NULL when
				  in undate mode.
T. Logan	2.7	 4/95	Removed TAE dependency (ASF); major changes:
				  change maskval to scalar
				  change option to scalar
				  remove min/max variables & functions
				  removal of timer calls and variables
				  removal of c_inlas & c_complt calls
				  removal of getpar routine
				  removal of TAE PARBLK refernces & variables
				  acceptance of command-line parameters
O. Lawlor	2.8	 6/97	Changed hostin list to char **, for
				  more-reliable prototypes. (now matches
				  LAS versions.)
O. Lawlor	2.9	 8/97	Added -c switch for color concatenation.
O. Lawlor	3.0	 3/98	Added -n switch to "fake" concatenation.
P. Denny        3.2      3/02   Updated commandline parsing & usage()


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None known

PROJECT:
	LAS		                

ALGORITHM DESCRIPTION:
Initialization of variables
Retrieve the user-specified parameters.  
The data type of the output image is determined by the maximum data type 
 of the input images.
If subcommand equals AUTO
  For each of the input images
      The projection coordinates are retrieved from the DDR files
      The projection coordinates are re-calculated to compensate for specified
       windows
      The starting line and sample locations within the output image are
       calculated for each input image
Else
  The starting line and sample parameters are retrieved
Output image is opened to the specified dimensions (NL and NS parameters)
If not in update mode output image is padded with the first MASK value
For each input image
   The ouput image is opened for update
   The input image is overlaid into the output image according to the 
    appropriate starting line and sample values
If $MINMAX equals YES second pass is made to determine min and max values of 
 each band
The associated min/max and history files are created for the output image 
The completion message is output.

ALGORITHM REFERENCES:
	none

BUGS:
	No known bugs

*****************************************************************************
*								            *
*   concat --  Creates a mosaic of all input geocoded SAR images	    *
*              to produce an output image of an equal area.		    *
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
#include "las.h"
#include "locinc.h"
#include "concat.h"

#define NIMGS 50
#define VERSION 3.2


/* The following variables must be declared global for image I/O routines */
struct FDESC **fdesc;		/* image file descriptors		*/
struct GDESC *gdesc;		/* group descriptor			*/
int	image_starts_in[MAXIMG * MAXBND + 1]; /* offsets into thebuf for input */
int	image_starts_out[MAXBND+1]; /* offsets into thebuf for output	*/
int	doConcat=1;		/* Actually copy data?			*/
int	colorConcat=0;		/* Create color image?			*/
int	colorCur=0;		/* Current color band.			*/

int main(int argc,char **argv)  /* ASF */
{
 struct DDR *ddr;		/* pointer to the input DDR structure	*/
 double	line_inc;		/* increment value in line direction	*/
 double	samp_inc;		/* increment value in sample direction	*/
 float	maskval;		/* mask values				*/
 static int bands[MAXIMG * NIMGS][MAXBND+1];/* band specification	*/
 int	aver;			/* average option for overlap condition	*/
 int	*laver;			/* average option for overlap condition	*/
 int	cflag;			/* combination flag for C_UPDDR		*/
 int	image;			/* curent image				*/
 int	image_count;		/* total number of images specified	*/
 int	in_count;		/* number of IN entries specified	*/
 int	index;			/* index				*/
 int	i;			/* counter				*/
 int	update;			/* output image flag			*/
 int	leave;			/* leave option for overlap condition	*/
 int	limage;			/* current logical image		*/
 int	nbands[MAXIMG * NIMGS];	/* number of bands in each image	*/
 int	nl;			/* number of lines in output image	*/
 int	ns;			/* number of samples per line in output image*/
 int	numimg[NIMGS];		/* number of images in each IN entry	*/
 int	out_dtype;		/* data type of output image		*/
 int	replace;		/* replace option for overlap condition	*/
 int	sl[NIMGS];		/* starting line locations		*/
 int	ss[NIMGS];		/* starting sample locations		*/
 int	status=0;		/* function return status code		*/
 int	totbnd;			/* total number of output bands		*/
 int	*twindow;		/* temporary window specifications	*/
 int	window[NIMGS * MAXIMG][4]; /* window specification		*/
 int	out_window[NIMGS][4];	/* window specification			*/

 char	*hostin[NIMGS],*hosto_ptr,*hosti_ptr; /* buffer storage for "hostin"*/
 char	hostout[CMLEN];		/* buffer storage for "hostout"		*/
 char	taeout[CMLEN];		/* buffer storage for "taeout"		*/
 char	option[7];		/* replace option for overlap condition */

 int	outsize_flag;		/* output size flag		    ASF */
 int	ulcoors_flag;		/* upper left coordinate flag	    ASF */
 double	outsize[2];		/* output image size		    ASF */
 double	ulcoors[2];		/* upper left coordinates	    ASF */
 float	projtol;		/* projection coordinate tolerance  ASF */
/************************************************************************/

 fdesc=(struct FDESC **)MALLOC(sizeof(struct FDESC *)*MAXIMG);
 for (i=0;i<NIMGS;i++)
	hostin[i]=(char *)MALLOC(sizeof(char)*CMLEN);

 /* Set default values 
 --------------------*/
 update = FALSE;		/*ASF*/
 maskval = 0.0;			/*ASF*/
 strcpy(option,"REPLAC");	/*ASF*/
 in_count = 0;			/*ASF*/
 image = 0;			/*ASF*/

 /* Parse command line arguments	
 ------------------------------*/
 if (argc > 3)
   currArg=1;	/* from cla.h which is in asf.h */
 else
   {printf("[concat] Insufficient arguments.\n"); usage(argv[0]);}
 while (argv[currArg][0] == '-')
 {
    char *key=argv[currArg++];
    if (strmatch(key,"-n")) {
      doConcat=0;
    }
    else if (strmatch(key,"-c")) {
      colorConcat = 1;
    }
    else if (strmatch(key,"-m")) {
      CHECK_ARG(1);
      maskval = atof(GET_ARG(1));
      printf("[concat] Mask value is %f\n",maskval);
    }
    else if(strmatch(key,"-v")) {
      CHECK_ARG(1);
      strcpy(option,GET_ARG(1));
      if ((strcmp(option,"REPLAC")!=0) && (strcmp(option,"LEAVE")!=0) && (strcmp(option,"AVER")!=0))
        {printf("[concat] -v option requires REPLAC, LEAVE, or AVER.\n"); usage(argv[0]);}
      printf("[concat] Overlap option is %s\n",option);
    }
    else {printf("[concat] Invalid option: %s\n",argv[currArg-1]);usage(argv[0]);}
 }
 if ((argc-currArg) < 3) {printf("[concat] Insufficient arguments.\n"); usage(argv[0]);}

 /* Get output file name */ 
 strcpy(hostout,argv[currArg]);
 strcpy(taeout,argv[currArg]);
 printf("[concat] Output image is %s\n",hostout);
 image = 1;
 currArg++;

 /* Get all input files */
 for (; currArg < argc; currArg++)	/*ASF*/
 {
    strcpy(hostin[in_count],argv[currArg]);
    printf("[concat] Input image %i is %s\n",in_count+1,hostin[in_count]);
    in_count++;
 }					/*ASF*/

 if (!in_count)		/*ASF*/
	usage(argv[0]);

 ddr = (struct DDR *)MALLOC(sizeof(struct DDR));	/*ASF*/
 laver = (int *)calloc(MAXNS,(sizeof(int *)));		/*ASF*/
 if (laver == NULL || ddr == NULL)			/*ASF*/
  {
   c_errmsg("Error dynamically alocating memory","concat-warn",NON_FATAL);
   c_errmsg("Fatal error encountered","concat-warn",LAS_FATAL);
  }

 /* get user-specified parameters and TAE globals 
 -----------------------------------------------*/
 image_count = in_count;				/*ASF*/
 if (colorConcat)
	totbnd=3;
 else
	totbnd = 1;					/*ASF*/
 projtol = 50.0;					/*ASF*/
 for (i = 0; i < in_count; i++)				/*ASF*/
  {
    numimg[i] = 1;					/*ASF*/
    nbands[i] = 1;					/*ASF*/
    window[i][0] = 1;	/* Starting Line */		/*ASF*/
    window[i][1] = 1;   /* Starting Sample */		/*ASF*/

    /*  Get the ddr information of input image
    ------------------------------------------*/
    status = c_getddr(hostin[i],ddr);		/*ASF*/ 
    if (status != E_SUCC)
    {
    	printf("Getting image %i, named '%s'.\n",i,hostin[i]);
         c_errmsg("Fatal error encountered","concat-fatal",LAS_FATAL);
    }
    window[i][2] = ddr->nl;				/*ASF*/
    window[i][3] = ddr->ns;				/*ASF*/

    /********************************
    bands[i][????] =  ??
    ********************************/
  }

 ulcoors_flag = FALSE;				/*ASF*/
 outsize_flag = FALSE;				/*ASF*/
 index = in_count;				/*ASF*/

 cal_lin_smp(hostin,&index,&in_count,numimg,window,projtol,&ulcoors_flag,
            &outsize_flag,ulcoors,outsize,sl,ss,&nl,&ns,out_window);

 /* determine output data type 
 ----------------------------*/
 set_dtype(hostin,image_count,&out_dtype/*,maskval,&in_count*/);

 /* initialize output image by padding with mask value 
 ----------------------------------------------------*/
 if (!update) pad(hostout,&nl,&ns,&totbnd,&out_dtype,&maskval);

 /* overlay inset images 
 ----------------------*/
 index = 0;
 hosti_ptr = hostin[0];
 if (update)
  {
  hosto_ptr = hostin[0];
  index += numimg[0];
  hosti_ptr = hostin[index];
  limage = 1;
  }
 else 
  {
  limage = 0;
  hosto_ptr = hostout;
  }

 for (; limage < in_count; limage++)
    {
    if (strcmp(option,"REPLAC") == 0)  /* ASF */
       {
       replace = TRUE;
       leave = FALSE;
       aver = FALSE;
       }
    else if (strcmp(option,"LEAVE") == 0)  /* ASF */
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
    hosti_ptr = hostin[index];
    }

 /* Update the DDR fields.  Since the input images all start at different places
   in the output image.  Set a temporary window of each of the input images
   to show how they relate to the output space.
 --------------------------------------------------------------------------------*/
 twindow = (int *)calloc(image_count,4 * sizeof(int));
 if (twindow == NULL)
   c_errmsg("Nonfatal error encountered","concat-warn",NON_FATAL);

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
   c_errmsg("non-fatal error encountered","concat-warn",NON_FATAL);

 free(twindow);
 /* inform user of successful completion 
 --------------------------------------*/
 printf("\n[concat] Successful completion; Wrote image %s\n",hostout);
	/* ASF */
 for (i=0;i<NIMGS;i++)
	free(hostin[i]);
 return 0;
}



void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	" %s [-m mask][-v ovrlp] [-c] [-n] <outfile> <file1> <file2> [...]\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"      outfile   Output LAS 6.0 format file, without extension\n"
 	"  file1 file2   Input file names, without extensions\n"
        "        [...]   Additional input file names\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"        -n   Set everything up, but do not copy data.\n"
	"        -c   output a color mosaic-- each image goes in a separate band.\n"
	"   -m mask   mask value for pixel fill value\n"
	"  -v ovrlp   pixel overlap option: REPLAC, LEAVE or AVER\n"
	"                    (default is REPLAC)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program creates an output image by overlaying each input\n"
	"   image into the output at the specified locations\n");
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n"
	"\n",VERSION);
   exit(1);
}					/*ASF*/
