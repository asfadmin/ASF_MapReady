/******************************************************************************
NAME:  sr2gr - converts slant range images to ground range images

SYNOPSIS:   sr2gr infile inmeta outfile outmeta pixsiz

       infile     base name (.img,.ddr)
       inmeta     input metadata name
       outfile    base name (.img,.ddr)
       outmeta    output metadata name (.meta)
       pixsiz     output pixel spacing (m)

DESCRIPTION:

        This program converts slant range imagery into ground range
        imagery.  The algorithm calculates the ground range to the
        first pixel in the image and from the spacecraft ephemeris,
        earth ellipsoid, and slant range to first pixel given in the
        image's metadata.  It then uses the slant range spacing interval
        to determine appropriate ground range positions.  The remapping
        is performed using bi-linear interpolation.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   T. Logan	Original Implementation (Based on
				JPL provided routine sr2gr_vec)
    2.0	    3/98   T. Logan     Modify to work with AISP outputs
    3.0     4/98   T. Logan     Modified to work with RAMMS data
    3.1     8/98   O. Lawlor    Corrected azimuth pixel spacing equation.
    4.0     12/98  O. Lawlor    Modified for new metadata routines.


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   sr2gr - converts slant range images to ground range images		    *
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
#include "sr2gr.h"

#define VERSION 4.0

/*Create vector for multilooking.*/
void ml_vec(float oldSize, float newSize,float *ml)
{
	float  gr=0;
	int    i;
	
	for (i=0; i<MAX_IMG_SIZE; i++)
	{
		ml[i]=gr/oldSize;
		gr+=newSize;
	}
}

int main(int argc,char *argv[])
{
	struct   DDR  ddr,oddr;         /* ddr structure                  */
	int      np, nl,                /* in number of pixels,lines      */
	onp, onl,              /* out number of pixels,lines     */
	i,line;
	
	float    newSize,oldX,oldY;
	
	float    sr2gr[MAX_IMG_SIZE];
	float    ml2gr[MAX_IMG_SIZE];
	int		a_lower[MAX_IMG_SIZE];
	int		lower[MAX_IMG_SIZE], upper[MAX_IMG_SIZE];
	float	a_ufrac[MAX_IMG_SIZE], a_lfrac[MAX_IMG_SIZE];
	float	ufrac[MAX_IMG_SIZE], lfrac[MAX_IMG_SIZE];
	
	float     *ibuf1,*ibuf2,*obuf;
	
	char	*infile,*inmeta,*outfile,*outmeta;
	FILE 	*fpi, *fpo;
	meta_parameters *meta;
	
	if (argc != 6)
	{
		printf("\nUsage: %s infile inmeta outfile outmeta pixsiz \n",argv[0]);
		printf(
		"       infile	   LAS base name (.img,.ddr)\n"
		"       inmeta     input metadata file\n"
		"       outfile    LAS base name (.img,.ddr)\n"
		"       outmeta    output metadata file (.meta)\n"
		"       pixsiz     output pixel spacing, in meters\n"
		"\n"
		"Converts the given image to ground range, from slant range.\n");
		printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}
	infile=argv[1];
	inmeta=argv[2];
	outfile=argv[3];
	outmeta=argv[4];
	sscanf(argv[5],"%f",&newSize);
	
	c_getddr(infile, &ddr);
	oddr=ddr;
	nl = ddr.nl;
	np = ddr.ns;
	
	ibuf1 = (float *) MALLOC (np*sizeof(float));
	ibuf2 = (float *) MALLOC (np*sizeof(float));
	
	printf("Output Pixels: %f (range)  %f (azimuth)\n",newSize,newSize);
	
	meta=meta_init(inmeta);
	if (meta->geo->type!='S')
	{
		bail("Slant to ground range only works with slant range images!\n");
		exit(1);
	}
	
	oldX=meta->geo->xPix*ddr.sample_inc;
	oldY=meta->geo->yPix*ddr.line_inc;
	
	/*Update metadata and DDR for new pixel size*/
	meta->geo->timeShift+=(oddr.master_line*meta->geo->azPixTime);
	meta->geo->slantShift+=(oddr.master_sample*meta->geo->xPix);
	oddr.master_line=oddr.master_sample=1.0;
	
	meta->geo->azPixTime*=newSize/meta->geo->yPix;
	oddr.line_inc=oddr.sample_inc=1.0;
	
	/*Create ground/slant and azimuth conversion vectors*/
	sr2gr_vec(meta,oldX,newSize,sr2gr);
	ml_vec(oldY,newSize,ml2gr);
	meta->geo->type='G'; 
	meta->geo->xPix=newSize;
	meta->geo->yPix=newSize;
	
	onp = MAX_IMG_SIZE;
	onl = MAX_IMG_SIZE;
	for (i=MAX_IMG_SIZE-1; i>0; i--) if ((int)sr2gr[i] > np) onp = i;
	for (i=MAX_IMG_SIZE-1; i>0; i--) if ((int)ml2gr[i] > nl) onl = i;
	
	obuf = (float *) MALLOC (onp*sizeof(float));
	oddr.nl=onl;
	oddr.ns=onp;
	oddr.pdist_x=oddr.pdist_y=newSize;
	
	c_putddr(outfile,&oddr);
	meta_write(meta,outmeta);
	
	fpi=fopenImage(infile,"rb");
	fpo=fopenImage(outfile,"wb");
	
	printf(" Input image is %s\n",infile);
	printf(" Input  lines, samples: %i %i\n",nl,np);
	printf(" Output image is %s\n",outfile);
	printf(" Output lines, samples: %i %i\n\n\n",onl,onp);
	
	for (i=0; i<MAX_IMG_SIZE; i++)
	{
		lower[i] = (int) sr2gr[i];
		upper[i] = lower[i] + 1;
		ufrac[i] = sr2gr[i] - (float) lower[i];
		lfrac[i] = 1.0 - ufrac[i]; 
		
		a_lower[i] = (int) ml2gr[i];
		a_ufrac[i] = ml2gr[i] - (float) a_lower[i];
		a_lfrac[i] = 1.0 - a_ufrac[i]; 
	}
	
	for (line = 0; line < onl; line++)
	{
		if (a_lower[line]+1<ddr.nl)
		{
			getFloatLine(fpi,&ddr,a_lower[line],ibuf1);
			getFloatLine(fpi,&ddr,a_lower[line]+1,ibuf2);
		}
		for (i=0; i<onp; i++)
		{
			int val00,val01,val10,val11,tmp1,tmp2;
			val00 = ibuf1[lower[i]];
			val01 = ibuf1[upper[i]];
			val10 = ibuf2[lower[i]];
			val11 = ibuf2[upper[i]];
			
			tmp1 = val00*lfrac[i] + val01*ufrac[i];
			tmp2 = val10*lfrac[i] + val11*ufrac[i];
			
			obuf[i] = tmp1*a_lfrac[line] + tmp2*a_ufrac[line]; 
		}
		putFloatLine(fpo,&oddr,line,obuf);
		if (line % 200 == 0) printf("...writing line %i\n",line);
	}
	FCLOSE(fpi);
	FCLOSE(fpo);
	
	return 0;
}

