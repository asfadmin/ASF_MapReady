/******************************************************************************
NAME: dspddr

SYNOPSIS:  dspddr [-b band] [-f <filename>] <SARfile>

DESCRIPTION:  

	Dspddr displays all the information in the DDR file. By default,
        dspddr will output to the screen all information unless overridden by
        the -f option. To display specific bands, denote each band by a -b
        followed by the band number. SARfile should be the DDR file without
        extensions.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
ROGRAMMER		DATE		REASON
-------------------------------------------------------------------
K. Zanter	      Feb. 1988		Original development
B. Ailts	      Jul. 1988         LAS 5.0 conversion
B. Ailts	      Dec. 1988		Removed mknam and cmtae -- setprt now
					returns full tae name
					Closed files on fatal errors
B. Ailts	      Mar. 1989		Now accepts alias names
B. Ailts	      Oct. 1989         Changed last accessed field to be
					last modified
P. Pease/GSFC         NOV. 1989         Changed call from clean to c_clean.
M. Shindle            June 1995         Removed TAE dependencies
O. Lawlor             March 26, 1998    Fixed uninitialized bflag bug.
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

	The program begins with the standard initialization and retrieval of 
 	user-specifed paramters.  
	The contents of the DDR file are retrieved.
	The fields associated with the image as a whole are written to the 
 	specified device(s).
	The fields associated with each specified band are written to the 
	specified device(s).

ALGORITHM REFERENCES:
	
	The format of the screens used in DSPDDR
	are nearly identical to those used in EDITDDR.
BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   dspddr -- Displays information in the DDR file.			    *
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

#include  "asf.h"
#include "las.h"

#define TERM 0
#define PFILE 1
#define VERSION 3.0

lasErr c_getddr_old(const char *hname,struct DDR *ddr);

void dstbnd (FILE *fp, int *print, char *name, int *band, struct BDDR *bddr); 
void dstimg (FILE *fp, int *print, char *name, struct DDR *ddr); 
void usage(char *name);

int main(int argc,char **argv)
{
	FILE *fp;			/* file pointer to print file		      */
	struct DDR ddr;			/* data in records 1 and 2 of DDR 	      */
	struct BDDR bddr;		/* data in the band records 		      */
	int	bands[MAXBND];		/* specified band numbers 		      */
	int	i;			/* loop index				      */
	int	print[2];		/* output destination			      */
	int	status;			/* function return status code		      */
	int	totbnd;			/* total number of bands to be processed      */
	char hname[CMLEN];		/* host name of input image		      */
	char prtfile[CMLEN];		/* print file name			      */
	int c;
	extern int optind;
	extern char *optarg;
	int bflag=0; /*O. Lawlor, 3/98.*/

	/* set default values */
	print[TERM] = 1;
	print[PFILE] = 0;

	/* process command line */
	while ((c=getopt(argc,argv,"b:f:")) != -1)
	   switch (c) {
	      case 'b':
		bands[bflag] = atol(optarg);
		bflag++;
		break;
	      case 'f':
		print[PFILE] = 1;
		print[TERM] = 0;
		strcpy(prtfile,optarg);
		break;
	      default:
		usage(argv[0]);
		break;	
	   }

	/* Get SARfile & place in hname.
	   Add DDR extension. */
	if (optind >= argc) 
	   usage(argv[0]);

	strcpy(hname,argv[optind]);

	/* get records 1 and 2 of DDR file */
	status = c_getddr_old(hname,&ddr);

	/* get band number specification. Either get all of the bands or only the
	   valid bands specified by user. */
	if ( !bflag ) {
	   totbnd = ddr.nbands;
	   for (i = 0; i < totbnd; i++)
	       bands[i] = i+1;
	} else {
	   totbnd = bflag;
	   for (i = 0; i < totbnd; i++)
	     if (bands[i] > ddr.nbands) {
		c_errmsg("Invalid band number","dspddr-bands",NON_FATAL);
		c_errmsg("Fatal error encountered","dspddr-fatal",LAS_FATAL);
	     }
	}  /*  bands != null value  */


	/* open the print file (if necessary)
	-------------------------------------*/
	if ( print[PFILE] )
	   fp = FOPEN(prtfile,"w");

	/* output data for records 1 and 2
	----------------------------------*/
	dstimg(fp,print,hname,&ddr);

	/* get and display band dependent records 
	-----------------------------------------*/
	for (i = 0; i < totbnd; i++) {
	    status = int_c_getbdr(hname,&bddr,&bands[i]);
	    if (status != E_SUCC)
	       if (print[PFILE])
	          fclose(fp);
	    dstbnd(fp,print,hname,&bands[i],&bddr);
	}

	/* close the print file (if necessary)
	--------------------------------------*/
	if (print[PFILE])
	  fclose(fp);

	/* inform user of successful completion
	---------------------------------------*/
	/*fprintf(stderr,"DSPDDR successfully completed!\n");*/
	return 0;
} 

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-b band] [-f <filename>] <SARfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENT:\n"
	"   <SARfile>   Name of ddr to display (extention is optional)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -b <band>      Specify which bands to display.\n"
	"                    (Each band must be proceeded by a -b.)\n"
	"   -f <filename>  Place information in <filename>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Displays information in the DDR file.\n");
 printf("\n"
	"Version: %.2f, ASF SAR Tools.\n"
	"\n",VERSION);
 exit(1);
}

