/******************************************************************************
NAME:	 converts Unsigned Int*2 RAMP products into LAS byte images

SYNOPSIS: ui2byte <infile> <outfile> [layoutfile]

DESCRIPTION: 
	Calculates image histogram, maps input values falling
	from LOWER_PERCENTAGE to UPPER_PERCENTAGE to 0 to 255.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    4/28/98  T. Logan	  Create byte versions of RAMP imagery
    2.0     4/10/98  T. Logan     Scale data values from 5% to 95% linearly
    3.0	    2/00     T. Logan     Reads only from standard CEOS records

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   Ceosui2byte converts Unsigned Int*2 RAMP products into LAS byte images  *
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ceos.h"

#define VERSION 3.0

#define UPPER_PERCENTAGE  0.95
#define LOWER_PERCENTAGE  0.05

/* Prototypes */
int create_ddr(char*,int,int,double,int);
void usage(char*);

int main(int argc,char *argv[])
{
    unsigned char     *ibuf,*obuf;
    struct IOF_VFDR ifiledr;
    struct dataset_sum_rec dssr;
    FILE 	*fpi, *fpo;
    char	infile[256];
    char	outfile[256];
    char	layoutfile[256];
    int		layout_flag=0;
    int         np, nl;                /* in number of pixels,lines      */
    int		line, i;
    int 	ldr = 192;
    int 	hdr;
    int		line_length;
    int    	tmp;
    int		hist[65536];
    int         SampleCutOff, LowerValueCutOff, UpperValueCutOff, Spread;
    int         cnt, j, ocnt, offset;
    unsigned char	conversion[65536];
    float	Scale;
    float	tmpflt;
    float 	pixSize;
    extern char *optarg;    /* for getopt (to parse command line) */
    extern int optind;      /* for getopt */
    int c;                  /* for getopt */

    
    while ((c=getopt(argc,argv,"l:")) != EOF)
    {
	switch (c) {
	 case 'l':/*Use layout file*/
	    strcpy(layoutfile,optarg);
	    layout_flag=1;
	    break;
	default:
	    usage(argv[0]);
	}
    }
    if (optind >= argc) 
	usage(argv[0]);

    strcat(strcpy(infile, argv[optind]),".D");
    strcat(strcpy(outfile,argv[optind+1]),".img");

    if (layout_flag)
      {
	/* Read layout file and get image sizes */
	fpi=FOPEN(layoutfile,"r");
	fscanf(fpi,"%i %i %i %i",&hdr,&ldr,&np,&nl);
	fclose(fpi);
	printf("Layout File Read:: hdr %i ldr %i np %i nl %i\n",hdr,ldr,np,nl);
	line_length = ldr + 2*np;
      }
    else
      { 
	/* Read facility related data record & get image sizes */
	get_ifiledr(infile, &ifiledr);
	nl = ifiledr.linedata;
	np = ifiledr.datgroup;
	hdr = line_length = ifiledr.reclen;
	ldr = line_length - ifiledr.sardata; 
	get_dssr(infile,&dssr);
	pixSize = dssr.line_spacing;
      }

    ibuf = (unsigned char *) MALLOC (nl*line_length*sizeof(unsigned char));
    obuf = (unsigned char *) MALLOC (nl*np*sizeof(unsigned char));

    /* Open File, Skip header, Read File, & Close */
    printf("Reading input file %s\n",infile);
    
    fpi=FOPEN(infile,"rb");
    FSEEK(fpi,hdr,0);
    FREAD(ibuf,(unsigned)(nl*line_length),1,fpi);
    fclose(fpi);

    /* Zero out the histogram array */
    for (i=0; i<65536; i++) hist[i] = 0;

    /* Create the histogram of the data */
    printf("Creating Histogram of the Data\n");
    offset = 0;
    for (line = 0; line < nl; line++)
     {
       for (i=0; i<np; i++)
        {
 	 /* create UI*2 value */
	 tmp = ibuf[offset+ldr+2*i]*256 + ibuf[offset+ldr+2*i+1];
         if (tmp<0 || tmp>65535) printf("Discarding impossible value %i\n",tmp);
	 else hist[tmp]++;
	}
       offset+=line_length;
       if (line % 1024==0) printf("...scanning line %4i\n",line);
     }
	  
    /* Find Lower Value Cut Off */
    SampleCutOff = LOWER_PERCENTAGE * (nl*np);
    for (i=0,cnt=0; cnt<SampleCutOff; i++) cnt += hist[i];
    LowerValueCutOff = i;   
 
    /* Find Upper Value Cut Off */
    SampleCutOff = (1.0 - UPPER_PERCENTAGE) * (nl*np);
    for (i=65535,cnt=0; cnt<SampleCutOff; i--) cnt += hist[i];
    UpperValueCutOff = i;   

    Spread = UpperValueCutOff - LowerValueCutOff;
    Scale  = (float) 255.0 / Spread;

    /* Create the scaling LUT */
    for (i=0; i<65536; i++)
      {
	if (i<LowerValueCutOff)
	      conversion[i] =(unsigned char) 0;
        else if (i<UpperValueCutOff)
	   {
	      tmpflt = (float) (i-LowerValueCutOff) * Scale;
	      conversion[i]= (unsigned char) tmpflt;
	   }
        else  conversion[i] = (unsigned char) 255;
      }

    printf("\n\n");
    printf("Lower Value Cut Off: %i\n",LowerValueCutOff);
    printf("Upper Value Cut Off: %i\n",UpperValueCutOff);
    printf("Scaling Factor: %f\n",Scale);
    for (i=0; i<255; i++)
     {
       if (i % 8 == 0) { printf("\n%3i-%3i:",i,i+7); }
       for (j=0; (int)conversion[j] <= i; j++);
       printf("%6i ",j);
     }
    printf("\n\n");

    /* Perform Conversion of Data */
    ocnt = 0;
    offset = 0;
    for (line = 0; line < nl; line++)
     {
       for (i=0; i<np; i++)
        {
 	 /* create UI*2 value */
	 tmp = ibuf[ldr+offset+2*i]*256 + ibuf[ldr+offset+2*i+1];
   	 obuf[ocnt++] = conversion[tmp];
	}
       offset += line_length;
       if (line % 512==0) printf("...writing line %4i\n",line);
     }

    /* Write output data */

    fpo = FOPEN(outfile,"wb");
    FWRITE(obuf,(unsigned)(np*nl),1,fpo);
    fclose(fpo);

    create_ddr(outfile,(long)nl,(long)np,pixSize,1);

    exit(1);
}

void usage(char *name)
{
	printf( "\nUSAGE:\n"
		"   %s [-l <layout_file>] infile outfile\n",name);
	printf( "\nOPTIONS:\n"
		"   -l <layoutfile>  name of layout file containing:\n"
        	"                     file header bytes\n"
        	"                     record leader bytes\n"
        	"                     data samples per line\n"
        	"                     data lines in file\n");
	printf( "REQUIRED INPUT:\n"
		"   infile    base name (.L,.D)\n");
	printf( "REQUIRED OUTPUT:\n"
		"   outfile   base name (.img,.ddr)\n");
	printf( "\nDESCRIPTION\n"
		"   Converts CEOS I*2 images into LAS byte images\n");
	printf("\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
	exit(1);
}
