/******************************************************************************
NAME:  deskew -- remove squint induced skew in imagery

SYNOPSIS:  deskew <infile> <inmeta> <outfile> <outmeta>

DESCRIPTION:

	Deskew uses the squint angle of an image along with the
        look angle to determine the amount of parallelogram shift
        skew that has been introduced in an image due to the doppler
        centroid choosen during image processing.  It then remaps
        the image (using bi-linear interpolation) to remove this
        skew.
 

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0		   Tom Logan

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   deskew -- removes squint induced skew from groundrange images           *
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
#include "geolocate.h"
#include "ddr.h"

#define VERSION 1.0
#define RTD	(180.0/M_PI)
#define DTR 	(M_PI/180.0)

int main(int argc,char *argv[])
{
	double	yaw, look, fac;
	struct   DDR       ddr;         /* ddr structure                  */
	int      np, nl,                /* in number of pixels,lines      */
	i, line, out_line;
	/*char     tfile[256];*/
	FILE        *fpi, *fpo;
	unsigned char *ibuf;
	unsigned char *obuf;
	float	*skew;
	int *lower;
	char		*infile,*inmeta, *outfile,*outmeta;
	
	if (argc != 4)
	{ 
	  printf("\nUsage: %s <infile> <inmeta> <outfile> <outmeta>\n",argv[0]);
	  printf("       <infile> 	input file name (.img,.ddr)\n");
	  printf("       <inmeta>   input image metadata\n");
	  printf("       <outfile>  output file name (.img,.ddr)\n");
	  printf("       <outmeta>  output image metadata (.meta)\n");
	  printf("\n"
	         "Removes doppler skew from image.\n");
	  printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
	  exit(1);
	}
	infile=argv[1];
	inmeta=argv[2];
	outfile=argv[3];
	outmeta=argv[4];
	c_getddr(infile, &ddr); 
	nl = ddr.nl; np = ddr.ns;
	c_putddr(outfile,&ddr);
	
	{
		meta_parameters *meta=meta_init(inmeta);
		GEOLOCATE_REC *g;
		stateVector stVec=meta_get_stVec(meta,0.0);
		if (meta->geo->deskew!=0)
		{
			printf("The image %s/%s is already deskewed!\n",
				infile,inmeta);
			exit(30);
		}
		
		g=init_geolocate_meta(&stVec,meta);
		getLookYaw(g,
			meta_get_slant(meta,0,0),
			meta_get_dop(meta,0,0),
			&look,&yaw);
		free_geolocate(g);
		
		meta->geo->deskew=1;
		meta_write(meta,outmeta);
	}
	
	fac = sin(look)*sin(yaw);
	
	skew =  (float *) MALLOC (np * sizeof(float));
	lower =  (int *) MALLOC (np * sizeof(int));
	
	for (i=0; i<np; i++)
	{
		skew[i] = (float)i * fac;
		lower[i] = (int) skew[i];
	}
	printf("Maximum Skew = %f\n", skew[np-1]);
	
	ibuf =  (unsigned char *) MALLOC (np * sizeof(unsigned char));
	obuf =  (unsigned char *) MALLOC (nl*np * sizeof(unsigned char));
	
	fpi=fopenImage(infile,"rb");
	
	for (line = 0; line < nl; line++)  
	{
		FREAD(ibuf,np,1,fpi);
		for (i=0; i<np; i++)
		{
			out_line = line + lower[i];
			if (out_line >= 0 && out_line < nl) 
				obuf[out_line*np+i] = ibuf[i];
		}
		if (line%1000==0) printf("Reading Line %i\n",line);
	}
	FCLOSE(fpi);
	
	fpo=fopenImage(outfile,"wb");
	FWRITE(obuf,np,1,fpo);
	FCLOSE(fpo);
	return 0;
}
