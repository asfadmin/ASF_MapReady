/******************************************************************************
NAME:	 converts Unsigned Int*2 RAMP products into LAS byte images

SYNOPSIS: ui2byte <infile> <outfile>
		infile         base name (.img,.ddr)
        	outfile        base name (.img,.ddr)

DESCRIPTION: 
	Reads LAS image into memory.  Calculates a full data histogram.
        Finds upper and lower data value cut offs for lower and upper 5% of
        data in file, and scales this data to byte values for the output
        file.

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

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   converts Unsigned Int*2 RAMP products into LAS byte images              *
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
#include "ddr.h"

#define VERSION 2.0

#define UPPER_PERCENTAGE  0.95
#define LOWER_PERCENTAGE  0.05

int main(int argc,char *argv[])
{
    struct   DDR ddr;
    unsigned char     *ibuf,*obuf;
    FILE 	*fpi, *fpo;
    char	*infile, *outfile;
    int         np, nl;                /* in number of pixels,lines      */
    int		line, i;
    int		line_length;
    int    	tmp;
    int		hist[65536];
    int         SampleCutOff, LowerValueCutOff, UpperValueCutOff, Spread;
    int         cnt, j, ocnt, offset;
    unsigned char	conversion[65536];
    int		min = 0;
    int 	max = 255;
    float	Scale;
    float	tmpflt;

     if (argc!=3 )
      {
	printf("\nUsage: %s infile outfile\n",
		argv[0]);
	printf("       infile	  base name (.img,.ddr)\n");
	printf("       outfile    base name (.img,.ddr)\n");
	printf("\nRead LAS INT*2 product, and scales by histogram \n"
	"to byte version.");
	printf("\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
	exit(1);
      }

    infile=argv[1];
    outfile=argv[2];
    
    c_getddr(infile,&ddr);
    ddr.dtype=1;
    c_putddr(outfile,&ddr); 
    nl = ddr.nl;
    np = ddr.ns;

    printf("Mapping from [%f%%,%f%%] to [%i,%i]\n",LOWER_PERCENTAGE*100.0,UPPER_PERCENTAGE*100.0,min,max);

    line_length = 2*np;

    ibuf = (unsigned char *) MALLOC (nl*line_length*sizeof(unsigned char));
    obuf = (unsigned char *) MALLOC (nl*np*sizeof(unsigned char));

    /* Open File, Skip header, Read File, & Close */
    printf("Reading input file %s\n",infile);
    
    fpi=fopenImage(infile,"rb");
    FREAD(ibuf,nl*line_length,1,fpi);
    FCLOSE(fpi);

    /* Zero out the histogram array */
    for (i=0; i<65536; i++) hist[i] = 0;

    /* Create the histogram of the data */
    printf("Creating Histogram of the Data\n");
    for (line = 0; line < nl; line++)
     {
       offset=line*line_length;
       for (i=0; i<np; i++)
        {
 	 /* create UI*2 value */
	 tmp = ibuf[offset+2*i]*256 + ibuf[offset+2*i+1];
         if (tmp<0 || tmp>65535) printf("Discarding impossible value %i\n",tmp);
	 else hist[tmp]++;
	}
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
    Scale  = (float) (max-min) / Spread;

    /* Create the scaling LUT */
    for (i=0; i<65536; i++)
      {
	if (i<LowerValueCutOff)
	      conversion[i] =(unsigned char) min;
        else if (i<UpperValueCutOff)
	   {
	      tmpflt = (float) (i-LowerValueCutOff) * Scale;
	      conversion[i]= (unsigned char) tmpflt+min;
	   }
        else  conversion[i] = (unsigned char) max;
      }

    printf("\n\n");
    printf("Lower Value Cut Off: %i\n",LowerValueCutOff);
    printf("Upper Value Cut Off: %i\n",UpperValueCutOff);
    printf("Scaling Factor: %f\n",Scale);
    for (i=min; i<max; i++)
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
	 tmp = ibuf[offset+2*i]*256 + ibuf[offset+2*i+1];
   	 obuf[ocnt++] = conversion[tmp];
	}
       offset += line_length;
       if (line % 512==0) printf("...writing line %4i\n",line);
     }

    /* Write output data */
    fpo=fopenImage(outfile,"wb");
    FWRITE(obuf,np*nl,1,fpo);
    FCLOSE(fpo);

    return 0;
}
