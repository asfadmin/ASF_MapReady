/****************************************************************
NAME: amp2img

SYNOPSIS: amp2img [-look lxs] [-step lxs] [-log <log file>] [-quiet] infile outfile

DESCRIPTION:
	Convert a float-valued amplitude image into a viewable byte image.
	Amp2img allows specification of file size as well as a "look area" in
	which to generate a byte image. The new byte file will have the same
	width as the original and a factor of 5 less rows.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    This program is a compilation of two programs written by Shusun Li and
    Tom Logan. This program fulfills the original purpose of Tom's with the
    added flexibility of Shusun's.

    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     5/96         M. Shindle - original creation
    1.01    5/96         M. Shindle - minor cosmetic changes
    2.0     8/96         M. Shindle - works on float files
    3.0     9/96         M. Shindle - specify step line & samples
    3.1	    2/97	 T. Logan   - add creation of ddr and fix man page
    3.2     5/97         T. Logan   - add -i switch to read metadata
    3.3     5/97         O. Lawlor  - made it always use DDRs,
    				      and consistent CLA's with multilook.
    3.4     8/97	 O. Lawlor  - DDR bug fix.
    3.5	    8/97         T. Logan   - CLA bug fix
    3.6	    10/97	 T. Logan   - Actually uses the step sample parameter
				      now (bug fix) & simplified mldata routine
    3.7	    7/98         O. Lawlor  - Slightly smaller image, bug fix.
    3.8	    5/01	 P. Denny   - Refined automated multilook
					Fool proofed command line
    3.81    7/01	 R. Gens    - Added logfile and quiet switch
    3.95    4/02         P. Denny   - Redefined commandline parsing & usage()

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   amp2img -- Converts a float .amp file into a byte image.		    *
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
#include "asf_meta.h"
#include "worgen.h"
#include "amp2img.h"
#include "ifm.h"

#define VERSION 4.0

void usage(char *name);

float get_mean(FILE *,int,int,int);
float calc_hist(FILE *,int,int,float);
void mldata(FILE *, FILE *,int,int,int,int,int,int,float *,unsigned char *,
	    int,int,int,float);

int main(int argc, char *argv[])
{
   FILE		*fpin,*fpout;
   char		 fnin[256],fnout[256];
   char 	 tmpExt[5],tmpstr1[256],tmpstr2[256];
   int		 wid=DEFAULT_WIDTH;	      
   int		 len=DEFAULT_LENGTH;
   int		 StepLine=5;
   int		 StepSample=1;
   int		 LookLine=5;
   int		 LookSample=1;
   int		 usingDefaultLS=1;	/*Has user overridden look line & sample?*/
   int		 nitems, newbytes;
   float	 mean, mult, avg;
   float	*ibuf;
   unsigned char *obuf;
   int		 obuflen;
   meta_parameters *meta, *outmeta;

   logflag=0;
   currArg=1;
   /* parse command line */
   while (currArg < (argc-2)) {
	char *key = argv[currArg++];
	if (strmatch(key,"-look")) {
		CHECK_ARG(1)
		if (2!=sscanf(GET_ARG(1),"%dx%d",&LookLine,&LookSample)) {
			printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
			usage(argv[0]);
		}
       		usingDefaultLS=0;
	}
	else if (strmatch(key,"-step")) {
		CHECK_ARG(1)
		if (2!=sscanf(GET_ARG(1),"%dx%d",&StepLine,&StepSample)) {
			printf("**ERROR: '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
			usage(argv[0]);
		}
		usingDefaultLS=0;
	}
	else if (strmatch(key,"-log")) {
		CHECK_ARG(1);
		strcpy(logFile,GET_ARG(1));
		fLog = FOPEN(logFile, "a");
		logflag=1;
	}
	else if (strmatch(key,"-quiet")) {
		quietflag=1;
	}
	else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
   }
   if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

   /* Nab required args */
   strcpy(fnin, argv[currArg]);
   strcpy(fnout,argv[currArg+1]);

   /* track time */
   StartWatch();
   system("date");
   printf("Program: amp2img\n\n");
   if (logflag) {
     StartWatchLog(fLog);
     printLog("Program: amp2img\n\n");
   }

   /* Disallow same input & output names */
   strcpy(tmpstr1,fnout);   strcat(tmpstr1,".amp");
   strcpy(tmpstr2,fnout);   strcat(tmpstr2,".img");
   if (0==strcmp(fnin,tmpstr1) || 0==strcmp(fnin,tmpstr2)) {
	printf("**ERROR: The input & output filenames may not be the same. (Regardless of infile extention)");
	usage(argv[0]);
   }
   
   /* If infile does not have an extention then fail.  */
   if (NULL==findExt(fnin))
	strcpy(tmpExt,"");
   else
	strcpy(tmpExt,findExt(fnin));
   if (strncmp(tmpExt,".amp",5) && strncmp(tmpExt,".img",5))
     {printf("**ERROR: The infile name needs a .amp or .img extension.\n");usage(argv[0]);}
   
   /* open input file */
   fpin = fopenImage(fnin,"rb");
   meta = meta_read(fnin);

   if (usingDefaultLS)
   {
	/* We don't want to multilook any image twice.  */
	if ( meta->sar->line_increment == meta->ifm->nLooks )	
		StepLine=StepSample=LookLine=LookSample=1;
	meta_free(meta);
   }

   if ( (meta->general->data_type!=REAL32) && (meta->general->data_type!=REAL64) )
   {
   	sprintf(errbuf, "   ERROR: Input data must be floating point data.\n");
   	printErr(errbuf);
   }
   wid = meta->general->sample_count;
   len = meta->general->line_count;
   
   outmeta = meta_read(fnin);
   outmeta->general->data_type = BYTE;
   outmeta->general->sample_count = wid/StepSample;
   outmeta->general->line_count = len/StepLine;
   outmeta->sar->line_increment *= StepLine;
   outmeta->sar->sample_increment *= StepSample;
   outmeta->general->x_pixel_size *= StepSample;
   outmeta->general->y_pixel_size *= StepLine;
   meta_write(outmeta, fnout);
   
   /* determine mean value from input file */
   avg = get_mean(fpin,wid,len,TRUE);
   mean = calc_hist(fpin,wid,len,avg);
   mult = 128.0 / mean;
   
   /* open output file */
   fpout = fopenImage(fnout,"wb");
   
   /* allocate memory */
   obuflen= outmeta->general->sample_count;
   ibuf = (float *) MALLOC(sizeof(float) * wid * LookLine);
   obuf = (unsigned char *) MALLOC(sizeof(unsigned char) * obuflen);
   
   /* start conversion */
   if (!quietflag) {
     printf("   Skipping every %d col and %d row\n",StepSample,StepLine);
     printf("   Looking at every %d col and %d row\n",LookSample,LookLine);
   }
   
   /* set mldata params  */
   nitems = (LookLine-StepLine)*wid;
   newbytes = StepLine*wid;
   if (!quietflag) printf("   Size of nitems: %d\n   Size of newbytes: %d\n",nitems,newbytes);
/*   printf("Converting file to byte...\n");*/

   /* look over data for full set of LookLines write data to file fpout */
   mldata(fpin,fpout,nitems,newbytes,LookLine,LookSample,
	  StepLine,StepSample,ibuf,obuf,meta->general->line_count,
	  meta->general->sample_count,obuflen,mult);

   FCLOSE(fpin);
   FCLOSE(fpout);
   
   /* Print end time */
   StopWatch();
   if (logflag) 
     StopWatchLog(fLog);

   return 0;
}

void usage(char *name) {
 printf("\n"
	"USAGE:\n"
	"   %s [-look lxs] [-step lxs] [-log log_file] [-quiet] <infile> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   infile   is an image file (WITH extension-- .amp or .img)\n"
	"              accompanied by a .meta file.\n"
	"   outfile  is the image file to be created, .img and .meta\n"
	"              (Do not include an extension on your outfile)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -look lxs      change number of look lines and samples (default = 5x1).\n"
	"   -step lxs      change number of step lines and samples (default = 5x1).\n"
	"   -log log_file  allows the output to be written to a log file\n"
	"   -quiet         suppresses the output to the essential\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts a float .amp file into a byte image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
