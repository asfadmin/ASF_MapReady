/******************************************************************************
NAME: lz2raw

SYNOPSIS: lz2raw <input superPrep signal> <output>

DESCRIPTION:

    lz2raw converts the given VEXCEL Level-0 signal data file into
an AISP-compatible raw format.  It extracts the necessary processing
parameters (slant range, prf, satellite mode) from the satellite
headers, which it copiously outputs during processing.  It extracts
state vectors from the VEXCEL .par file.

The currently supported satellites are:
        -ERS-1 and ERS-2
        -JERS
        -RADARSAT, all beams & modes (excluding SCANSAR)

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.8  Orion Lawlor, 8/3/98
    1.0  Orion Lawlor, 3/99
    1.1  Orion Lawlor, 3/99
    1.2  Dave Koster, 6/00    Modified to read files larger than 2GB

HARDWARE/SOFTWARE LIMITATIONS:

	Does not compensate for skipped lines.
	Output is very inefficient for SCANSAR images.  
	SCANSAR images cannot be quicklooked.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   lz2raw converts the given VEXCEL Level-0 signal data file into	    *
*	   an AISP-compatible raw format.				    *
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
#include "lzFetch.h"
#include "decoder.h"
#include "dateUtil.h"

#define VERSION 1.2

/*********************************
updateMeta:
	Updates the given satellite record with
the (more accurate) Earth radius, spacecraft height,
velocity, etc. using state vector in the ".par" file.
*/
/*Utility routine: Convert LZ-state vector-style date
to structure: DD-MMM-YYYY hh:mm:ss.ttt*/
void lzStateTime(const char *lzStr,ymd_date *date,hms_time *time)
{
	char month[100];/*3-letter, all-caps description of the month*/
	const char *monthNames[12]=/*List of months of year, in format above*/
		{"JAN","FEB","MAR","APR","MAY","JUN",
		 "JUL","AUG","SEP","OCT","NOV","DEC"};
	int monthNo;

	if (6!=sscanf(lzStr,"%d-%[^- ]-%d %d:%d:%lf",
		&date->day,month,&date->year,
		&time->hour,&time->min,&time->sec))
	{/*We couldn't read the date correctly*/
		printf("ERROR! createMeta_lz:lzStTime couldn't parse LZP\n"
			"state vector date string '%s'!\n",lzStr);
		printf("Month='%s'\n",month);
		exit(1);
	}
/*Try to figure out the month*/
	monthNo=0;
	while (monthNo<12 && (0!=strncmp(month,monthNames[monthNo],3)))
			monthNo++;
	if (monthNo==12)
		{printf("ERROR! createMeta_lz:lzStTime couldn't match month '%s'!\n",
			month);exit(1);}
	date->month=monthNo+1;
}

/*Internal Metadata creation routines (asf_ceos.a)*/
meta_parameters *raw_init(void);
void ceos_init(const char *in_fName,meta_parameters *sar);
void propagate_state(meta_parameters *meta,int nStVec);


void createMeta_lz(bin_state *s,char *inN,char *outN,int numLines)
{
	double clock_ang;
	char parN[256];
	meta_parameters *meta=raw_init();
	stateVector stVec;/*Source state vector*/
	ymd_date st_date,img_date;
	julian_date st_jdate,img_jdate;
	hms_time st_time,img_time;
	char *st_timeStr;/*Source state vector time, DD-MMM-YYYY hh:mm:ss.ttt*/
	char *img_timeStr;/*Image time string: YYYYMMDDhhmmssttt*/
	
/*Open the parameter file, and read a state vector.*/
	strcat(strcpy(parN,inN),".par");
	
	stVec.pos.x = lzDouble(parN,"prep_block.state_vector.x:",NULL);
	stVec.pos.y = lzDouble(parN,"prep_block.state_vector.y:",NULL);
	stVec.pos.z = lzDouble(parN,"prep_block.state_vector.z:",NULL);
	stVec.vel.x = lzDouble(parN,"prep_block.state_vector.xv:",NULL);
	stVec.vel.y = lzDouble(parN,"prep_block.state_vector.yv:",NULL);
	stVec.vel.z = lzDouble(parN,"prep_block.state_vector.zv:",NULL);
	st_timeStr=lzStr(parN,"prep_block.state_vector.Date:",NULL);
	parse_ymdTime(st_timeStr, &st_date, &st_time);

	date_ymd2jd(&st_date,&st_jdate);
	
/*	Convert from GEI to fixed-earth: (NOT needed-- already fixed?)
	gei2fixed(&stVec,utc2gha(st_jdate.year,st_jdate.jd,
		st_time.hour,st_time.min,st_time.sec));*/
	
/*Create a state vector structure to hold our state vector,
  and copy it over.*/
	meta->stVec=raw_init_state(1);
	meta->stVec->vecs[0].vec=stVec;

/*Find start of current scene*/
	img_timeStr=lzStr(parN,"prep_block.location[0].line_date:",NULL);
	date_dssr2date(img_timeStr,&img_date,&img_time);
	date_ymd2jd(&img_date,&img_jdate);

/*Set time of image start & state vector, and propagate state vector*/
	meta->stVec->year=img_date.year;
	meta->stVec->julDay=img_jdate.jd;
	meta->stVec->second=date_hms2sec(&img_time);
	
	meta->stVec->vecs[0].time=date2sec(&st_jdate,&st_time)-date2sec(&img_jdate,&img_time);
	propagate_state(meta,2+(int)((numLines/s->prf)/30.0));

/*Figure out satellite look direction*/ 
	clock_ang = lzDouble(parN,"prep_block.clock_angle:",NULL);
	if (clock_ang  == 90.0) 
		s->lookDir='R';
	else if (clock_ang == -90.0)
		s->lookDir='L';
	else {printf("ERROR! Clock angle in .par file is %f!\n",clock_ang); exit(1); }
	
/*Update s-> fields with new state vector*/
	addStateVector(s,&meta->stVec->vecs[0].vec);
	
/*Update fields for which we have decoded header info.*/
	updateMeta(s,meta);
	meta->ifm->orig_nLines=numLines;
	
/*Write out and free the metadata structure*/
	meta_write(meta,outN);
	meta_free(meta);
	
	free(parN);
}

/********************************
convertMetadata:
	Creates AISP .in and .fmt files,
as well as determining the number of lines in the l0 file,
by reading the granule (.gran) file.
*/
#include <ctype.h>
bin_state *convertMetadata_lz(char *inN,char *outN,int *nLines,readPulseFunc *readNextPulse)
{
	bin_state *s;
	int numLines;
	char lzN[256];
	int i;
	char *satName;
	
	strcat(strcpy(lzN,inN),".par");

/*Figure out what kind of SAR data we have, and initialize the appropriate decoder.*/
	satName=lzStr(lzN,"prep_block.satellite:",NULL);
	/*Trim white space from end of satellite name*/
	i=strlen(satName)-1;
	while (isspace(satName[i]))
		satName[i--]=0;
	
	/*Initialize the appropriate decoder routine*/
	if (0==strncmp(satName,"ERS",3))
		s=ERS_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"JERS",4))
		s=JRS_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"RSAT",4))
		s=RSAT_decoder_init(inN,outN,readNextPulse);
	else 
		{printf("Unrecognized satellite '%s'!\n",satName);exit(1);}
	
/*Read in essential parameters from granule file.*/
	numLines=lzInt(lzN,"prep_block.number_lines:",NULL);
	numLines-= 500;  /* This is a stupid hack */
	
/*Write out AISP input parameter files.*/
	createMeta_lz(s,inN,outN,numLines);
	writeAISPparams(s,outN,0.0,-99.0,-99.0);
	writeAISPformat(s,outN);

/*Clean up and leave.*/
	*nLines=numLines;
	free(lzN);
	return s;
}


/*****************************
main:
	Open input file.
	Read each echo pulse
	Write each echo pulse
*/
int main(int argc,char *argv[])
{
	int outLine,nTotal;
	char *inName,*outName;
	FILE *out;
	
	bin_state *s;
	readPulseFunc readNextPulse;
	iqType *iqBuf;
	
/*Parse CLA's.*/
	if (argc!=3)
		{printf("\nUSAGE: %s <input> <output>\n"
		"\t<input>    VEXCEL Level-0 signal data file."
		"\n\t<output>   AISP-comatible raw format.\n\n"
		"This program decodes the input VEXCEL\n"
		"Level-0 format signal data into AISP-compatible\n"
		"raw data.  Currently supports ERS, JERS, Radarsat \n"
	        "Strip mode (all beams), but not Radarsat ScanSAR mode.\n"
		"\nVersion %.2f, ASF SAR TOOLS\n\n",argv[0],VERSION);exit(1);}
	inName=argv[1];
	outName=appendExt(argv[2],".raw");

/*First, we read the metadata to determine where window position shifts happen,
as well as the number of lines in the image.*/
	s=convertMetadata_lz(inName,outName,&nTotal,&readNextPulse);
	iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);
	
/*Now we just loop over the output lines, writing as we go.*/
	out=FOPEN(outName,"wb");
	s->nLines=0;
	for (outLine=0;outLine<nTotal;outLine++)
	{
	/*Now read and write pulse of data.*/
		readNextPulse(s,iqBuf);
		FWRITE(iqBuf,s->nSamp,2,out);
	/*Write status information to screen.*/
		if (outLine%1000==0)
			printf("Converting line %d of %d\n",outLine,nTotal);
		s->nLines++;
	}
	
	return 0;
}
