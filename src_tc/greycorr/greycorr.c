/****************************************************************************
NAME:   GREYCORR  --   Performs grey-level image-to-image correlation
 
SYNOPSIS:  greycorr reference search intplfile outtplfile
 
DESCRIPTION:   This program attempts to perform correlation of the two
    input images to produce a file of tie-points.  The correlations are
    attempted at the candidate points listed in the input tpl file.
    Processing proceeds by extracting a chip from the reference image
    and smaller chip from the search image for each candidate point.
    The correlation them calculates a degree of match for all possible
    placements of the search chip in the reference chip.  If the maximum
    resulting correlation is less than the minimum allowed, this candidate
    is discarded.  Otherwise a tie-point is calculated and written to the
    output tpl file in ascii format.  These tie-points consist of a point
    from the reference image and the point from the search image that
    produced the best correlation. 
	Note that attempts at correlation are offset by the x,y value in 
    in the input "offset" file.  Normally this value is (0,0) but in cases where
    the images are not expected to line up, the offset point value should
    be changed appropriately. 
 
EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    terrcorr             Calling Routine for Terrain Correction
    gcorr                Subroutine that performs correlation
 
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    reference.img        input byte reference image
    search.img           input byte search image
    intplfile.tpl        input ASCII tie point location file
    outtplfile.tpl       output ASCII tie point file
 
PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  6.0    7/94   T. Logan      ASF      Ported to YMP, removed TAE 
				       & LAS dependencies
  7.0    9/94   T. Logan      ASF      Ported to T3D, combined with
				       with mtpgrid.c routine
  8.0    10/94  T. Logan      ASF      Optimization 
  9.0    10/95  T. Logan      ASF      Reverse port to Solaris 
  9.1     7/97  T. Logan      ASF      Modified to use pixel size to control
				       the parameters for the search
  9.2	  8/97	O. Lawlor     ASF      Modified to accept any LAS image input
  9.5     5/99  O. Lawlor     ASF      Allows an "offset" file (e.g. from fftMatch)
                                       Allows mincorr on command line.
                                       Guts almost totally re-written.
 
HARDWARE/SOFTWARE LIMITATIONS: Only works correctly for even chip sizes
 
ALGORITHM DESCRIPTION:
   Read Input Files
   For each candidate point
       if point is in reference image
         if point is in search image
           correlate tie-point
       if correlation is acceptable
         write tie-point to file 
   Close files and exit
 
ALGORITHM REFERENCES:
   This routine was adapted from the LAS 6.0 version of GREYCORR.
   This part of the routine was completely re-written
 
BUGS:
****************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu


NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include <math.h>
#include "correlate.h"
#include "asf_meta.h"

#define VERSION     9.5

void usage(char *name)
{
  printf("\nUsage: %s [-n nFile] [-o offFile] [-s minStrength] [-v]\n"
  	"                  reference search intpl outtpl \n\n",name);
  printf("    options:\n"
  	 "            -n nFile       output 'number of points' file \n"
  	 "                              (# good,# bad,# out)\n"
  	 "            -o offFile     input 'image offset' file (x,y)\n"
  	 "            -s minStrength minimum correlation strength (default: 2.0)\n"
  	 "            -v             be verbose (print out correlations)\n");
  printf("    inputs: reference.im   reference LAS image\n");
  printf("            search.img     search LAS image\n");
  printf("            intpl.tpl      input tie point locations\n\n");
  printf("    output: outtpl.tpl     output tie point file\n");
  printf("    Version %.2f,  ASF STEP TOOLS\n",VERSION);
  exit(1);
}


int main (int argc, char **argv)
{
/************************************************************************/ 
float   *rf_buf;		/* reference image read buffer	       	*/ 
char   ref_name[200];            /* reference image name               	*/
float   *sf_buf;		/* search image read buffer	       	*/
char   sea_name[200];           /* search image name                   	*/
char   in[200];			/* input Tie Point Location file name   */
char   out[200];		/* output Tie Point Location file name 	*/
FILE   *fp_ref,*fp_sea;		/* file pointer, search and reference 	*/
FILE   *tplfp,*fp;		/* input tpl, output file pointer       */
FILE   *fp_num=NULL,*fp_off=NULL; /*Number of points & offset files */
int   ref_nl, ref_ns;           /* reference image size                	*/
int   sea_nl, sea_ns;           /* search image size                   	*/
int   sl, ss;		        /* Start Line, Start Sample	       	*/
int   i,j,loc;			/* loop counter                        	*/
int   pt;                       /* search coordinates and point number 	*/
int   fitmeth;                  /* Method of Fit to Use                	*/
int    num_good = 0;		/* succesful correlations count        	*/
int    num_bad = 0;		/* unsuccesful correlations count      	*/
int    num_out = 0;		/* out-of-bounds count  	      	*/
double ref_pt[2];		/* tie-point reference coordinates     	*/
double sea_pt[2];               /* tie-point search coordinates        	*/
float  strength;		/* Strength of correlation             	*/
float rx,ry,sx,sy;		/* ref & search x,y points              */
float *ref_chip;		/* reference chip buffer               	*/
float *sea_chip;		/* search chip buffer                  	*/
float mincorr=2.0;	        /* Min acceptable correlation strength 	*/
float CHIP_SIZE;                /* The size of the correlation chips    */
float est_err[2];		/* Estimated correlation errors        	*/
float best_fit[2];		/* Best fit offsets from correl. peak  	*/
int   npls[2];		        /* Size of search subimage             	*/
int   nplr[2];		        /* Size of reference subimage          	*/
float ioffrq[2];		/* Requested max horiz & vert offsets  	*/
float nomoff[2];		/* Nominal offsets                     	*/
float off_x,off_y;		/* Requested offset - from offset file  */  
struct  DDR ddr_ref,ddr_sea;	/* Data descriptor - image metadata     */
int argNo; 			/* Current command-line argument number */
int verbose=0; 			/* Flag: run in verbose mode? */
/************************************************************************/ 
StartWatch();
CHIP_SIZE = 32.0;

/* Get processing parameters from the user
  ---------------------------------------*/
argNo=1;
while (argc-argNo>4)
{
	if (argv[argNo][0]!='-') 
		usage(argv[0]);
	
	switch (argv[argNo++][1])
	{
	case 'o':/*Open image offset file*/
		fp_off=FOPEN(argv[argNo++],"r");break;
	case 'n':/*Open number of correlations file*/
		fp_num=FOPEN(argv[argNo++],"w");break;
	case 's':/*Minimum correlation strength*/
		sscanf(argv[argNo++],"%f",&mincorr);break;
	case 'z':/*Correlation chip size*/
		sscanf(argv[argNo++],"%f",&CHIP_SIZE);break;
	case 'v':/*Verbose mode*/
		verbose=1;break;
	default:
		usage(argv[0]);
	}
}

if (argc-argNo==4)
{
	strcat(strcpy(ref_name,argv[argNo]),".img");
	strcat(strcpy(sea_name,argv[argNo+1]),".img");
	strcat(strcpy(in,argv[argNo+2]),".tpl");
	strcat(strcpy(out,argv[argNo+3]),".tpl");
} else
	usage(argv[0]);


/* Allocate buffers, read reference and search images, establish parameters
  -------------------------------------------------------------------------*/

c_getddr(ref_name, &ddr_ref);
ref_nl = ddr_ref.nl; ref_ns = ddr_ref.ns;
c_getddr(sea_name, &ddr_sea);
sea_nl = ddr_sea.nl; sea_ns = ddr_sea.ns;

rf_buf = (float *) MALLOC (sizeof(float)*ref_ns);
sf_buf = (float *) MALLOC (sizeof(float)*sea_ns);
 
fp_ref = fopenImage(ref_name,"rb");
fp_sea = fopenImage(sea_name,"rb");

tplfp = FOPEN(in,"r");
fp = FOPEN(out,"w");

/*Determine image offset*/
if (fp_off)
	fscanf(fp_off,"%f%f",&(off_x),&(off_y));
else
	off_x = off_y = 0;
/*Quantize offset to integers*/
off_x=floor(off_x+0.5);off_y=floor(off_y+0.5);
 
/* Perform preprocessing of tpl data 
 -----------------------------------*/
fitmeth = PARAB;

	nplr[0] = nplr[1] = CHIP_SIZE;
	ioffrq[0] = ioffrq[1] = (int)( nplr[0]/2.0*0.9);

	npls[0] = npls[1] = (int)(2.0*nplr[0]);
	
for (i = 0; i < 2; i++) nomoff[i] = (float) (npls[i]-nplr[i])/2.0;

ref_chip=(float *)MALLOC((int)(sizeof(float)*nplr[0]*nplr[1]));
sea_chip=(float *)MALLOC((int)(sizeof(float)*npls[0]*npls[1]));

printf("Processing parameters:\n");
printf(" ref_nl = %i   ref_ns = %i \n",ref_nl,ref_ns); 
printf(" sea_nl = %i   sea_ns = %i \n",sea_nl,sea_ns); 
printf(" mincorr = %f fitmeth = %i\n",mincorr,fitmeth);
printf(" chip = %d; search = %d; maxOff = %f\n",nplr[0],npls[0],ioffrq[0]);
printf(" nomoff[0] = %f\n", nomoff[0]);
printf(" offset x = %f   offset y = %f\n",off_x,off_y);

/*************************************************************/
/* 		Process all tie point pairs		     */
/*************************************************************/
pt = 0;
printf("\nBeginning Correlation Process\n"); 
while (EOF!=fscanf(tplfp,"%f%f%f%f",&ry,&rx,&sy,&sx))
  {
    pt++;

    /* Extract reference window */
    sl = (int) (ry - nplr[0]/2.0 + 0.5);
    ss = (int) (rx - nplr[1]/2.0 + 0.5);
    if((sl>=0)&&((sl+nplr[0])<ref_nl)&&(ss>=0)&&((ss+nplr[1])<ref_ns)) {
      loc = 0;
      for (i = 0; i<nplr[0]; i++)
      {
       getFloatLine(fp_ref,&ddr_ref,sl+i,rf_buf);
       for (j = 0; j < nplr[1]; j++)
        ref_chip[loc++] = (float) rf_buf[ss+j];
      }

      /* Extract search window */
      sl = (int) (sy -off_y - npls[0]/2.0 + 0.5);
      ss = (int) (sx -off_x - npls[1]/2.0 + 0.5);
      if((sl>=0)&&((sl+npls[0])<sea_nl) && (ss>=0)&&((ss+npls[1])<sea_ns)) {
        loc = 0;
        for (i = 0; i<npls[0]; i++)
        {
         getFloatLine(fp_sea,&ddr_sea,sl+i,sf_buf);
         for (j = 0; j < npls[1]; j++)
          sea_chip[loc++] = (float) sf_buf[ss+j];
        }

        gcorr(sea_chip,ref_chip,npls,nplr, 
        	fitmeth, nomoff, &strength, best_fit, est_err,verbose);
        /*printf("x,y,s: %.2f %.2f %.2f\n",best_fit[0],best_fit[1],strength);*/
       	
        if ((fabs(best_fit[1])<ioffrq[0])&&
       		(fabs(best_fit[0])<ioffrq[1])&&
        	(strength>mincorr)) {
            ref_pt[0] = ry; ref_pt[1] = rx;
	    sea_pt[0] = sy + best_fit[0]-off_y; 
	    sea_pt[1] = sx + best_fit[1]-off_x;
            if (fprintf(fp,"%f %f %f %f\n",ref_pt[0],ref_pt[1],
	                                   sea_pt[0],sea_pt[1])<0)
                { printf("Unable to write tie point data\n");exit(1); }
	    num_good++;
            printf(".");
         } else { printf("x"); num_bad++;}
      } else { printf("o"); num_out++;}
    } else { printf("o"); num_out++;}
   if (!(pt%20)) printf("\n");
   fflush(stdout);
  }
fclose(tplfp);
printf("\n\nCorrelation was attempted at %i points\n",pt);
 
/* Close Down Processing and Exit 
 --------------------------------*/
fclose(fp);
printf("\n\nCorrelate finished, wrote %i tie-points to file %s\n",num_good,out); 
StopWatch();
free(rf_buf);
free(sf_buf);
if (fp_num)
	fprintf(fp_num,"%d %d %d\n",num_good,num_bad,num_out);

exit(num_good);
}
 
