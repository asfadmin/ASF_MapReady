/******************************************************************************
NAME: usgs2las-- convert an old-format USGS DEM to a LAS DEM

SYNOPSIS:  usgs2las <in USGS> <out LAS>

        	<in USGS>: a USGS DEM file with ASCII-formatted records.
        	<out LAS>: an output INT*2 las image name.  Must include
			   extension (e.g. .img)

DESCRIPTION:

        All the ASF SAR tools use LAS format images.
        This tool lets you ingest an old-format USGS DEM (INT*2 format,
        some-arc-second spacing) into LAS format.

        The old USGS DEM format is a set of ASCII-formatted
        1024-byte records (type A, B, and C).  This program should work
        with any sample spacing/ground area.

	This program reads in an old-format USGS DEM.
	The old format is a series of 1024-byte records, 
	which are of type A (projection & metadata), B (elevation),
	and C (errors).

	This program reads the type A and B records to create
	a LAS version of the input image, with appropriate 
	metadata.  The DEM is transposed (bottom left corner
	flips with upper right corner, other corners stationary)
	as it is read in.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1999   Orion Lawlor 

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:
	Format of USGS DEM taken from dem2tga.c (c) 1997 Jon Larimer

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   usgs2las-- convert an old-format USGS DEM to a LAS DEM 		    *
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
#include "ddr.h"

FILE *demFile=NULL;
int bytesRead;/*Number of bytes read from fixed-size fields so far*/
int recordNo;

void seek(int where)
{
	FSEEK(demFile,where,0);
	bytesRead=0;
}

void nextstr(char *dest)
{
	if (1!=fscanf(demFile,"%s",dest))
	{printf("Error-- couldn't read string from DEM file (record %d)!\n",recordNo);exit(1);}
}

int nextint(void) {
	int ret;
	char inStr[10];
	if (0>=fscanf(demFile," %6s",inStr))
	{printf("Error-- couldn't read integer from DEM file (record %d)!\n",recordNo);exit(1);}
	
	if (1!=sscanf(inStr,"%d",&ret))
	{
		char errStr[1000];
		fscanf(demFile,"%s",errStr);
		printf("Error-- couldn't read integer from DEM string '%s' (record %d)!\n",inStr,recordNo);
		exit(1);
	}
	return ret;
} 

double nextfloat(void) {
	char in[200];
	int i;
	double ret;
	nextstr(in);

	for (i=0;in[i];i++)
		if (in[i]=='D')
			in[i]='E';

	if (1!=sscanf(in,"%lg",&ret))
	{printf("Error-- couldn't read double from DEM string '%s'!\n",in);exit(1);}
	return ret;
}

/*Read fixed-size field of len bytes from input file into
given string, appending terminating NULL*/

void fixString(char *dest,int len)
{
	char ignored[1024];
	bytesRead+=len;
	if (bytesRead>=1024)
	{/*Pad out to integral multiple of 1024-byte blocks.
	Everything is a multiple of six except this, so we have to skip 4 bytes
	of spaces.*/
		FREAD(ignored,4,1,demFile);
		bytesRead=len;
	}
	FREAD(dest,len,1,demFile);
	dest[len]=0;
}

/*Read 6-byte integer from current file, and return*/
int fixInt(void)
{
	int ret;
	char inStr[10];
	fixString(inStr,6);
	if (1!=sscanf(inStr,"%d",&ret))
	{
		printf("Error-- couldn't read integer from DEM string '%s' (record %d)!\n",inStr,recordNo);
		exit(1);
	}
	return ret;
}


#define VERSION 1.0

int main(int argc, char **argv) {
	char *outFileName;
	FILE * lasOut;
	struct DDR ddr;/*Output ddr record*/
	/*int x*/
	int y,cur_x,cur_y,samplesRead;
	int len;
	int blocksPerProfile=1;/*This will be re-set once we know the image size*/
	int i,unitsCode,projCode;
	char name[200],ignored[1000];
	float *outBuf;
	float pixScale;/*Scaling factor to convert elevations to meters*/

/*Check command line arguments & open files*/
	if(argc != 3) {
		fprintf(stdout, "\nUSAGE: usgs2las <in USGS> <out LAS>\n");
		fprintf(stdout, "  <in USGS>   is the old-format USGS DEM you want to convert.\n");
		fprintf(stdout, "  <out LAS>   is a LAS output image.\n");
		fprintf(stdout, "\nusgs2las -- Converts an old-format USGS DEM"
			" to a LAS DEM\n");
		fprintf(stdout, "Version %.2f, ASF SAR TOOLS\n\n", VERSION);
		exit(1);
	}

	demFile=FOPEN(argv[1],"r");
	outFileName=argv[2];

/*Create the LAS Data Descriptor Record (DDR) for the DEM*/
	c_intddr(&ddr);
	ddr.nbands=1;/*1 band (height)*/
	ddr.dtype=2;/*INT*2 pixels*/
	/*Set DDR validity flags*/
	ddr.valid[DDPCV]=ddr.valid[DDZCV]=ddr.valid[DDPUV]=
		ddr.valid[DDPDV]=ddr.valid[DDCCV]=1;

/*Read DEM type A record-- contains metadata about the DEM*/
	/* get the name field (144 characters) */
	for(i=0; i < 144; i++) { name[i] = fgetc(demFile); }
	name[40] = '\0';/*Cut off the latter characters (which seem to be garbage)*/
	fprintf(stdout, "Quad name field: '%s'\n", name);
	/* don't need the next 19 items for anything */
	for(i=0; i < 19; i++) { (void)nextstr(ignored); }
	
	/*Figure out the projection system*/
	projCode=nextint();
	if (projCode==3)
	{/*Image is aligned with latitude & longitude grid*/
		printf("DEM is oriented with lat-lon grid\n");
		ddr.proj_code=0;/*GEOGRAPHIC (arc-seconds) projection*/
		strcpy(ddr.proj_units,"SEC");
	} else {
		printf("Sorry!  The projection code %d is unknown!  Exiting...\n",projCode);
		exit(1);
	}
	
	/*Figure out the units for the elevations, and convert to meters*/
	unitsCode=nextint();
	if (unitsCode==1) 
	{/*then elevations are in feet*/
		printf("Elevations in feet-- will convert to meters\n");
		pixScale=0.3048;/*Convert elevations in feet to elevations in meters*/
	} else if (unitsCode==2) 
	{/*Then elevations are in meters*/
		printf("Elevations in meters-- no conversion needed\n");
		pixScale=1.0;
	} else 
	{/*Unknown elevation units-- ask user for conversion factor*/
		printf("Elevations are in unknown units %d!!\n"
			"Please enter conversion factor to meters:",unitsCode);
		scanf("%f",&pixScale);
	}
	
	(void)nextstr(ignored);
	/*Get DEM corners*/
	printf("Ground coordinates of 4 corners of DEM: (in arc-seconds) \n");
	/*Order in file is lon, lat for bottom left, upper left, upper right, bottom left*/
	ddr.loleft[1]=nextfloat();
	ddr.loleft[0]=nextfloat();
	ddr.upleft[1]=nextfloat();
	ddr.upleft[0]=nextfloat();
	ddr.upright[1]=nextfloat();
	ddr.upright[0]=nextfloat();
	ddr.loright[1]=nextfloat();
	ddr.loright[0]=nextfloat();

	fprintf(stdout,"Minimum elevation: %f, maximum elevation: %f\n",
		nextfloat(),nextfloat());
	/* don't need the next 3 items */
	for(i=1; i<3; i++) nextstr(ignored);

	cur_y=nextint();
	ddr.ns = nextint();
	ddr.nl=1000;/*This will be re-set inside the loop*/
	
/* Read DEM type B records-- these contain the actual elevations of the DEM. */

	printf("Reading data...\n");
/* Read the DEM data, which is oriented down-up,left-right (instead of left-right,up-down) */
	samplesRead=0;
	recordNo=0;
	while (samplesRead<ddr.nl*ddr.ns) {
		seek(recordNo*1024*blocksPerProfile+1024);
		recordNo++;
		cur_y=fixInt();
		cur_x=fixInt();
		len=fixInt();
		printf("Reading line %d, sample %d\n",cur_y,cur_x);
		
		if (recordNo==1)
		{/*This is the first record-- do some initialization*/
		
		/*Each profile consists of a 144-byte header followed by "len"
		  6-byte elevations.  Round up to nearest multiple of 1024, and that's
		  the number of 1024-byte blocks in each profile*/
			blocksPerProfile=(len*6+144+1023)/1024;
		
		/*Write out DDR*/
			ddr.nl=len;
			
			/*Set DDR projection distances and corners*/
			printf("Nl=%d, ns=%d\n",ddr.nl,ddr.ns);
			ddr.pdist_x=(ddr.upright[1]-ddr.upleft[1])/(ddr.ns-1);
			ddr.pdist_y=(ddr.upleft[0]-ddr.loleft[0])/(ddr.nl-1);
			printf("Projection distances (arc-seconds): %f in x; %f in y\n",ddr.pdist_x,ddr.pdist_y);
			outBuf=(float *)MALLOC(sizeof(float)*ddr.ns*ddr.nl);
			c_putddr(outFileName,&ddr);
		}
		
		/*Skip 6 items at the start of each row*/
		fixInt();/* Skip some integer*/
		for (i=0;i<5;i++) 
			fixString(ignored,24); /*Skip random floating-point values*/

		if ((cur_y-1)+len>ddr.nl) len=ddr.nl-(cur_y-1);
	/*Copy over the elevations in this row*/
		for(y=0; y<len; y++) 
			outBuf[(ddr.nl-(cur_y+y))*ddr.ns+(cur_x-1)] = pixScale*fixInt() ; 
		
		samplesRead+=len;
	/*Read location items at start of *next* row*/
		/*printf(".");fflush(stdout);*/
	}

	printf("Writing data...\n");
	lasOut=fopenImage(outFileName,"wb");
/*Write out data the "right" way (left-right,up-down) out of our buffer*/
	for (y=0;y<ddr.nl;y++)
		putFloatLine(lasOut,&ddr,y,&(outBuf[y*ddr.ns]));

/*Close files-- we're done!*/
	fprintf(stdout, "Finished.\n");
	free(outBuf);
	fclose(lasOut);
	fclose(demFile);
	return 0;
}
  
  
