/******************************************************************************
NAME: ceos2raw

SYNOPSIS: ceos2raw <input CEOS signal> <output>

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
    1.2  Patrick Denny, 5/01    Disallowed same name for in and out files
    1.3  Patrick Denny, 6/01    Remove .L and .D links after processing
			         added to ceosUtil.c
    1.35 P. Denny       4/02    Update commandline Parsing & usage()
                                 beamNo option was ignored! so I removed
                                 it from the usage
    1.5 P. Denny        2/03    Change to use new meta structures
                                 Fix commandline parsing routine

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
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/

#include "asf.h"
#include "decoder.h"
#include "ceos.h"
#include "dateUtil.h"

#define VERSION 1.5

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
/*Internal Metadata creation routines */
meta_parameters *raw_init(void);
void ceos_init(const char *in_fName,meta_parameters *sar);

/*hack function for .ldr & .raw compatibility (in ceosUtil.c)*/
void linkFlag(short,char*);


void createMeta_ceos(bin_state *s,struct dataset_sum_rec *dssr,char *inN,char *outN)
{
	meta_parameters *meta=raw_init();
	
	ceos_init(inN,meta);
	s->lookDir = meta->sar->look_direction;
	s->nLines = meta->general->line_count;

        /* Check for VEXCEL LZP Data-- has odd state vectors
         --------------------------------------------------*/
	if (0==strncmp(dssr->sys_id,"SKY",3))
	{
	/*Correct for wrong time of image start-- image start time
	 *(from DSSR) is *actually* in the center of the image.*/
		double imgLen;/*Half of length of image, in seconds*/
		int i;
		imgLen=ceosLen(inN)/2.0/s->prf;
		printf("VEXCEL Level-0 CEOS: Shifted by %f seconds...\n",imgLen);
		/*Correct the image start time*/
		meta->state_vectors->second -= imgLen;
		if (meta->state_vectors->second<0) {
			meta->state_vectors->julDay--;
			meta->state_vectors->second+=24*60*60;
		}
		/*Correct the time of the state vectors, which are *relative* to
		 *image start.*/
		for (i=0;i<meta->state_vectors->vector_count;i++)
			meta->state_vectors->vecs[i].time += imgLen;
		/* State vectors are too far apart or too far from image as read
		 * -- propagate them */
		propagate_state(meta, 3, (s->nLines / s->prf / 2.0) );
	}
	
	/* Update s-> fields with new state vector
         ----------------------------------------*/
	addStateVector(s,&meta->state_vectors->vecs[0].vec);

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
		{printf("Unrecognized satellite '%s'!\n",satName);exit(EXIT_FAILURE);}
	createMeta_ceos(s,&dssr,inN,outN);
	
/*Write out AISP input parameter files.*/
	writeAISPparams(s,outN,dssr.crt_dopcen[0],dssr.crt_dopcen[1],dssr.crt_dopcen[2]);
	writeAISPformat(s,outN);

	*nLines=s->nLines;
	
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
	meta_parameters *meta;
	readPulseFunc readNextPulse;
	iqType *iqBuf;
	
	StartWatch();

/* Parse command line args */
	logflag = quietflag = 0;
	while (currArg < (argc-2))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile, GET_ARG(1));
			logflag=1;
			fLog = FOPEN(logFile, "a");
		}
		else if (strmatch(key,"-quiet")) {
			quietflag=1;
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	if (SAME==strcmp(argv[currArg], argv[currArg+1])) { /* File names are the same */
		printf(	"* Error: Input and output files must not be named the same. *\n");
		exit(EXIT_FAILURE);
	}
	inName = argv[currArg++];
	outName = appendExt(argv[currArg],".raw");
	
/* Read look direction, time & state vectors from CEOS file */
	if (!extExists(inName,".L") && extExists(inName,".ldr"))
	{/*HACK: link .ldr file over to .L file-- keeps get_facdr happy*/
		linkFlag(0,inName);
		if (findExt(inName)&&(0!=strcmp(findExt(inName),".D")))
		{/*HACK: link .raw (or whatever) over to .D file-- keeps get_iof happy*/
			linkFlag(1,inName);
		}
	}

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
	
	printf("Converted %d lines.\n",outLine);

/* Make sure all the meta feilds are up to date */
	meta = meta_read(outName);
	updateMeta(s,meta);
	meta_write(meta, outName);
	meta_free(meta);

/* Cleanup */
	delete_bin_state(s);
	if (logflag) FCLOSE(fLog);
	FCLOSE(out);

	linkFlag(2,NULL);
	return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] [-quiet] <input> <output>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <input>    VEXCEL Level-0 CEOS signal data\n"
	"   <output>   AISP-compatible raw data.\n");
/*	("   -b <beamNo>  Number of beams to write out.\n"
 *	"                 Optional; only for SCANSAR data\n"
 *	"                 Default is beam 0 for the first beam.\n");
 */
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log     Allows the output to be written to a log file\n"
	"   -quiet   Suppresses the output to the essential\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program decodes the input data into AISP-compatible\n"
	"   raw data.  Currently supports ERS, JERS, Radarsat \n"
	"   Strip mode (all beams) including Radarsat ScanSAR mode.\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

