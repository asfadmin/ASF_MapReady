/******************************************************************************
NAME: stds2las

SYNOPSIS:  sdts2las <in SDTS> <out LAS>
	
        	SDTS: a USGS SDTS DEM file.  This must be the path to the
                	XXXXCOL0.DDF file, but XXXXIDEN.DDF, XXXXSPDM.DDF,
                	and XXXXRSDF.DDF files must exist as well.
        	LAS: an output INT*2 las image name.  Must include extension
                	(e.g. .img)

DESCRIPTION:

	All the ASF SAR tools use LAS format images.
        This tool lets you ingest a USGS SDTS DEM (INT*2 format,
        UTM projection, 30m or 10m resolution) into LAS format.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
	concat
	sdts_ingest

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	  5/13/99  Orion Lawlor

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:
			Based on show173 code from:
                        
			   U.S. Geological Survey
                          National Mapping Division
                     Spatial Data Transfer Standard (SDTS)
                                 FIPS 173
                          SDTS Browse Utility
                 program:         show173.c
                 author:          Bob Lazar
                 date:            November 27, 1992
                 language:        C

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   sdts2las-- convert a USGS SDTS DEM to a LAS DEM			    *
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



#include <stdio.h>
#include <string.h>
#include "stc123.h"
#include "asf.h"

int status;              /* status returned by FIPS library routines */

#define check(x) check_func(x,__LINE__)
void check_func(int returnCode,int lineNo)
{
	if(returnCode!=0) return;/*No error*/
	printf("Sorry, the last file I/O command (line %d) \n"
		"returned error %d!\n",status,returnCode);
	exit(1);
}

int ddr_nl,ddr_ns;/*Output DDR Info*/

/*Convert inName, a SDTS CEL0 series of elevation profiles, into
the LAS INT*2 file outFile*/
void convertBinary(char *inName,FILE *outFile,short backgroundFill)
{
	short *outLine=(short *)malloc(sizeof(short)*50000); /*Line of data to write out*/
	int lineLen=0;

	/*char file_name[100];*/     /* input ISO 8211 file name */
	FILE *fpin;
	long int_level;          /* interchange level returned by beg123file */    
	char ccs[4];             /* character set string returned by beg123file */
	char ice;                /* inline code extension from beg123file */
	unsigned char string[5000];       /* subfield data returned by rd123sfld */
	
	ddr_nl=ddr_ns=0;
	
	/*      Open input file         */
	if (! beg123file (inName,'R',&int_level,&ice,ccs,&fpin))
	{
		printf ("\nERROR OPENING FILE '%s'-- is this a SDTS file?\n",inName);
		exit(0);
	}
	
	/*      Read data descriptive record (DDR)      */
	check(rd123ddrec(fpin,(char *)string,&status)); 
	
	status = -1;
	/*       Loop to process each subfield             */
	do {
		char leadid;             /* leader ID returned by rd123sfld */
		long str_len;            /* byte length of subfield returned by rd123sfld */
		char tag[10];            /* field tag returned by rd123sfld and chk123sfld */
		char descr[100];        /* subfield description (mnemonic) returned by chk123sfld */
		char frmts[100];         /* subfield format returned by chk123sfld */
		
		/*      Read data record subfield    */
		check(rd123sfld(fpin,tag,&leadid,(char *)string,&str_len,&status));
		
		/*      Retrieve description of current subfield        */
		check(chk123sfld(fpin,tag,descr,frmts));
		
		/*     Determine if format is binary         */
		if (strstr (frmts, "B") != NULL)
		{/*Assume it's a 2-byte, big-endian binary number*/
			short outVal;
			outVal=(((short)string[0])<<8)|string[1];
			if (outVal<0)
				outVal=backgroundFill;/*Record a zero instead of negative data*/
			outLine[lineLen++]=outVal;
		}
		else
			printf("%-5s %-8s %-6s %6ld %s\n",
			       tag,descr,frmts,str_len,string);
		
		/*       Output record/end of file delimeters                */
		if (status == 3||status==4)   /* subfield is at end of record */
		{
			printf("Record was %d samples...\n",lineLen);
			fwrite(outLine,2,lineLen,outFile);
			ddr_ns=lineLen;
			lineLen=0;
			ddr_nl++;
		}
		if (status == 4)   /* subfield is at end of file */
		{
			printf ("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
			printf ("$$$$$ END OF CEL0 FILE $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
			printf ("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
		}
	} while (status != 4);   /* Break out of loop at end of file */
	
	/*       Close input ISO 8211 file        */
	status = end123file (&fpin);
	free(outLine);
}

/*Fetch the given subfield from the given SDTS file
(specified by the base name and 4-character fileID).
If multiple occurences exist, they are concatenated into dest.*/
void fetchField(char *baseName,char *fileID,char *subField,char *dest)
{
	int i;
	FILE *fpin;
	char fName[500];
	long int_level;          /* interchange level returned by beg123file */    
	char ccs[4];             /* character set string returned by beg123file */
	char ice;                /* inline code extension from beg123file */
	char string[5000];       /* subfield data returned by rd123sfld */
	
	strcpy(fName,baseName);
	for (i=0;i<4;i++)
		fName[strlen(fName)-8+i]=fileID[i];
	
	strcpy(dest,"");/*Make the destination empty*/
	
	/*      Open input file         */
	if (! beg123file (fName,'R',&int_level,&ice,ccs,&fpin))
	{
		printf ("\nERROR OPENING FILE '%s'-- is this a SDTS file?\n",fName);
		exit(0);
	}
	
	/*      Read data descriptive record (DDR)      */
	check(rd123ddrec(fpin,string,&status)); 
	
	status = -1;
	/*       Loop to process each subfield             */
	do {
		char leadid;             /* leader ID returned by rd123sfld */
		long str_len;            /* byte length of subfield returned by rd123sfld */
		char tag[10];            /* field tag returned by rd123sfld and chk123sfld */
		char descr[100];        /* subfield description (mnemonic) returned by chk123sfld */
		char frmts[100];         /* subfield format returned by chk123sfld */
		
		/*      Read data record subfield    */
		check(rd123sfld(fpin,tag,&leadid,(char *)string,&str_len,&status));
		
		/*      Retrieve description of current subfield        */
		check(chk123sfld(fpin,tag,descr,frmts));
		
		/*     Determine if this is our field         */
		if (strncmp (descr,subField,strlen(subField))==0)
		{
			if (strstr (frmts, "B") != NULL)/*Binary record-- parse and print it*/
			{
				char outString[100];
				int bin[8];/*Each binary byte*/
				int i;
				for (i=0;i<8;i++)
					bin[i]=0xff&(int)string[i];
				switch(str_len)
				{
					case 1:/*Byte data*/
						sprintf(outString,"%d",bin[0]);break;
					case 2:/*Short data*/
						sprintf(outString,"%d",(bin[0]<<8)+bin[1]);break;
					case 4:/*Int data*/
						sprintf(outString,"%d",(bin[0]<<24)+(bin[1]<<16)+(bin[2]<<8)+bin[3]);break;
					default:/*Unknown size*/
						sprintf(outString,"%x",(bin[0]<<24)+(bin[1]<<16)+(bin[2]<<8)+bin[3]);break;
				}
				strcat(dest,outString);/*Concatenate now-ASCII field onto our string*/
			} else /*Is an ASCII record--just return it*/
				strcat(dest,string);/*Concatenate ASCII fields onto our string*/
			strcat(dest," ");/*add a space to separate elements*/
		}
		/*
		Field Subfield Format Length Data
		----- -------- ------ ------ ----
			printf("%-5s %-8s %-6s %6ld %s\n",
		tag,  descr,   frmts,str_len,string);*/
		
	} while (status != 4);   /* Break out of loop at end of file */
	
	/*       Close input ISO 8211 file        */
	status = end123file (&fpin);
}

/*Convert the given string into doubles, and find their minimum and
maximum*/
void minMax4(char *stringOf4,double *min,double *max)
{
	double vals[4]={-1,-1,-1,-1};
	int i;
	sscanf(stringOf4,"%lf%lf%lf%lf",&vals[0],&vals[1],&vals[2],&vals[3]);
	*min=2000000000000000000.0;
	*max=-2000000000000000000.0;
	for (i=0;i<4;i++)
	{
		if (*min>vals[i]) *min=vals[i];
		if (*max<vals[i]) *max=vals[i];
	}
}


void
main(int argc,char *argv[])
{
	char las_file[100];      /* LAS output file name */
	FILE *las_fp;     /*Binary LAS output file*/

	char in_file[100];     /* input ISO 8211 file name */
	char cmd[5000];       /* command string */
	
	double minX,maxX,minY,maxY;/*Extents of image, in UTM projection coordinates*/
	double latCen,lonCen;/*Latitude and longitude, in decimal degrees, of center*/
	int zone;/*UTM zone code*/
	int projDist;/*DEM projection distance, in meters*/
	
	if (argc!=3) 
	{
		printf("\nUsage: sdts2las <in SDTS> <out LAS>\n"
			"\tSDTS:     a USGS SDTS DEM file.\n"
			"\t LAS:     an output INT*2 LAS image name.\n\n"
			"Converts a USGS SDTS DEM into LAS 6.0 format\n"
			"Version 1.0, ASF SAR TOOLS\n\n");
		exit(1);
	}
	
	strcpy(in_file,argv[1]);
	strcpy(las_file,argv[2]);
	
	las_fp=fopen(las_file,"wb");
	convertBinary(in_file,las_fp,0);
	fclose(las_fp);
	
	printf("Computing Metadata...\n");
	
	fetchField(in_file,"IDEN","TITL",cmd); printf("Title: '%s'\n",cmd);
	
	/*Fetch the corners of the image, and compute the extents*/
	fetchField(in_file,"SPDM","!X",cmd); printf("X: '%s'\n",cmd);minMax4(cmd,&minX,&maxX);
	fetchField(in_file,"SPDM","!Y",cmd); printf("Y: '%s'\n",cmd);minMax4(cmd,&minY,&maxY);
	
	/*Fetch the latitude and longitude of the image center*/
	fetchField(in_file,"IDEN","DAID",cmd); 
	sscanf(cmd,"LAT:: %lf LONG:: %lf",&latCen,&lonCen);
	printf("Loc: '%s'\nCenter is at Lat=%f, lon=%f\n",cmd,latCen,lonCen);
	
	/*Compute UTM zone and projection distance*/
	zone=(int)((lonCen + 180.0) / 6.0+0.9999999);
	
	/*Estimate pixel size from projection distances and number of samples*/
	projDist=(int)((maxX-minX)/ddr_ns+0.5);
	
/*Find top-left corner more accurately  (Can't easily get the units right, so I'm skipping this...)
	fetchField(in_file,"RSDF","X",cmd);printf("New left X:%s\n",cmd);
		sscanf(cmd,"%lf",&minX);minX/=1000.0;
	fetchField(in_file,"RSDF","Y",cmd);printf("New top  Y:%s\n",cmd);
		sscanf(cmd,"%lf",&maxY);maxY/=10.0;
*/
	
	/*Re-set bottom & right projection coordinates based on accurate
	  projection distance*/
	minY=maxY-projDist*ddr_nl;
	maxX=minX+projDist*ddr_ns;
	
	sprintf(cmd,"makeddr -p UTM %d %f %f %f %f %d %s %d %d short\n",
		zone,maxY,minX,minY,maxX,projDist,las_file,ddr_nl,ddr_ns);
	system(cmd);
	
}
