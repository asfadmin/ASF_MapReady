/******************************************************************************
NAME: ceos2raw

SYNOPSIS: ceos2raw <input CEOS signal> <output> [ <beamNo> ]

DESCRIPTION:
        ceos2raw converts the given CEOS signal data file into
	an AISP-compatible raw format.  It extracts the necessary processing
	parameters (slant range, prf, satellite mode) from the satellite
	headers, which it copiously outputs during processing.  It extracts
	state vectors from the CEOS platform position data record.

	The currently supported CEOS formats are:
        	-ESA CCSD CEOS data
        	-VEXCEL RSA-compatible CEOS signal data

	ASF CCSD data can be read directly by AISP and quicklook.

	The currently supported satellites are:
        	-ERS-1 and ERS-2
        	-JERS
        	-RADARSAT, all beams & modes (including SCANSAR)

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.8  Orion Lawlor,  8/3/98
    1.0  O. Lawlor,     3/99
    1.1  Dave Koster,   6/00    Modified to read files larger than 2GB
    1.2  Patrick Denny, 5/01    Disallowed same in and out files
    1.3  Patrick Denny, 6/01    Remove .L and .D links after processing
			         added to ceosUtil.c
    1.35 P. Denny       4/02    Update commandline Parsing & usage()
                                 beamNo option was ignored! so I removed
                                 it from the usage

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
*   ceos2raw -- This program decodes the input data into AISP-compatible    *
*		raw data.  Currently supports ERS, JERS, Radarsat           *
*		Strip mode (all beams) including Radarsat ScanSAR mode.     *
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
#include "decoder.h"
#include "ceos.h"
#include "dateUtil.h"

#define VERSION 1.45

/*********************************
createMeta:
	Create outN.meta file using information
from CEOS in the inN.L file.
*/
/*Return the length (in lines) of the given CEOS image*/
int ceosLen(char *inN)
{
	struct IOF_VFDR viof;/*Imagery Options File (values)*/
	get_ifiledr(inN,&viof);
	return viof.numofrec;
}
/*Internal Metadata creation routines (asf_ceos.a)*/
meta_parameters *raw_init(void);
void ceos_init(const char *in_fName,meta_parameters *sar);
void propagate_state(meta_parameters *meta,int nStVec);

/*hack function for .ldr & .raw compatibility (in ceosUtil.c)*/
void linkFlag(short,char*);


void createMeta_ceos(bin_state *s,struct dataset_sum_rec *dssr,char *inN,char *outN)
{
	meta_parameters *meta=raw_init();
	
	/* Read look direction, time & state vectors from CEOS file
         ---------------------------------------------------------*/
	if (!extExists(inN,".L") && extExists(inN,".ldr"))
	{/*HACK: link .ldr file over to .L file-- keeps get_facdr happy*/
		linkFlag(0,inN);
		if (findExt(inN)&&(0!=strcmp(findExt(inN),".D")))
		{/*HACK: link .raw (or whatever) over to .D file-- keeps get_iof happy*/
			linkFlag(1,inN);
		}
	}
	ceos_init(inN,meta);
	s->lookDir=meta->geo->lookDir;
	
        /* Check for VEXCEL LZP Data-- has odd state vectors
         --------------------------------------------------*/
	if (0==strncmp(dssr->sys_id,"SKY",3))
	{
	/*Correct for wrong time of image start-- image start time
	(from DSSR) is *actually* in the center of the image.*/
		double imgLen;/*Half of length of image, in seconds*/
		int i;
		imgLen=ceosLen(inN)/2.0/s->prf;
		printf("VEXCEL Level-0 CEOS: Shifted by %f seconds...\n",imgLen);
		/*Correct the image start time*/
		meta->stVec->second-=imgLen;
		if (meta->stVec->second<0)
			{meta->stVec->julDay--;meta->stVec->second+=24*60*60;}
		/*Correct the time of the state vectors, which are *relative* to image start.*/
		for (i=0;i<meta->stVec->num;i++)
			meta->stVec->vecs[i].time+=imgLen;
	/*State vectors are too far apart or too far from image as read-
	   propagate them*/
		propagate_state(meta,3);
	}
	
	/* Update s-> fields with new state vector
         ----------------------------------------*/
	addStateVector(s,&meta->stVec->vecs[0].vec);

        /* Update fields for which we have decoded header info.
         -----------------------------------------------------*/
	updateMeta(s,meta);

        /* Write out and free the metadata structure
         ------------------------------------------*/
	meta_write(meta,outN);
	meta_free(meta);
}

/********************************
convertMetadata:
	Creates AISP .in and .fmt files,
as well as determining the number of lines in the l0 file,
by reading the granule (.gran) file.
*/

bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,readPulseFunc *readNextPulse)
{
	bin_state *s;
	struct dataset_sum_rec dssr;
	char *satName;
	
/*Figure out what kind of SAR data we have, and initialize the appropriate decoder.*/
	get_dssr(inN,&dssr);
	satName=dssr.mission_id;
	
	if (0==strncmp(satName,"E",1))
		s=ERS_ceos_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"J",1))
		s=JRS_ceos_decoder_init(inN,outN,readNextPulse);
	else if (0==strncmp(satName,"R",1))
		s=RSAT_ceos_decoder_init(inN,outN,readNextPulse);
	else 
		{printf("Unrecognized satellite '%s'!\n",satName);exit(1);}
	createMeta_ceos(s,&dssr,inN,outN);
	
/*Write out AISP input parameter files.*/
	writeAISPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
	writeAISPformat(s,outN);

/*Clean up and leave.*/
	*nLines=1000000;
	
	return s;
}


/*****************************
main:
	Open input file.
	Read each echo pulse
	Write each echo pulse
*/
#define SAME 0
int main(int argc,char *argv[])
{
	int outLine,nTotal;
	char *inName,*outName;
	FILE *out;
	
	bin_state *s;
	readPulseFunc readNextPulse;
	iqType *iqBuf;
	
/*Parse CLA's.*/
	StartWatch();

	if (argc!=3)
		usage(argv[0]);

	if (SAME==strcmp(argv[1], argv[2]))	    /* File names are the same */
	{
		printf(	"*************************************************************\n"
		       	"* Error: Input and output files must not be named the same. *\n"
			"*************************************************************\n");
		exit(1);
	}
	inName=argv[1];
	outName=appendExt(argv[2],".raw");
	
/* First, we read the metadata to determine where window position shifts happen,
   as well as the number of lines in the image.*/
	s=convertMetadata_ceos(inName,outName,&nTotal,&readNextPulse);
	iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*(s->nSamp));
	
/*Now we just loop over the output lines, writing as we go.*/
	out=FOPEN(outName,"wb");
	getNextCeosLine(s->binary);/*Skip CEOS header.*/
	s->nLines=0;
	for (outLine=0;outLine<nTotal;outLine++)
	{
	/*Now read and write pulse of data.*/
		readNextPulse(s,iqBuf);
		FWRITE(iqBuf,s->nSamp,2,out);
	/*Write status information to screen.*/
		if (outLine%1000==0)
			printf("Converting line %d\n",outLine);
		s->nLines++;
	}

	linkFlag(2,NULL);
	return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <input> <output>\n",name);
 printf("\n"
	"ARGUMENTS:\n"
	"   <input>    VEXCEL Level-0 CEOS signal data\n"
	"   <output>   AISP-compatible raw data.\n");
/*	"   -b <beamNo>  Number of beams to write out.\n"
 *	"                 Optional; only for SCANSAR data\n"
 *	"                 Default is beam 0 for the first beam.\n");
 */
 printf("\n"
	"DESCRIPTION:\n"
	"   This program decodes the input data into AISP-compatible\n"
	"   raw data.  Currently supports ERS, JERS, Radarsat \n"
	"   Strip mode (all beams) including Radarsat ScanSAR mode.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}

