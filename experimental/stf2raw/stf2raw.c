/******************************************************************************
NAME: stf2raw

SYNOPSIS:
        stf2raw [-lat <lower> <upper>] [-log <file>] [-quiet] [-prc <path>]
	        <input> <output>

                <input>   VEXCEL Level-0 signal data
                <output>  AISP-compatible raw data

DESCRIPTION:
        Converts the given VEXCEL Level-0 signal data file into an AISP-
        compatible raw format. It extracts the necessary processing parameters 
        (slant range, prf, satellite mode) from the satellite headers, which it 
        copiously outputs during processing. It extracts state vectors from the 
        VEXCEL .par file.

        The currently supported satellites are:
                -ERS-1 and ERS-2
                -JERS
                -RADARSAT, all beams & modes (excluding SCANSAR)

        Also checks for and corrects missing and truncated lines in ERS data. 
        Note: this is ONLY FOR ERS DATA.

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
    2.2  Dave Koster,  6/00     Modified to read files larger than 2GB
    3.0  T. Logan,     2/01     Modified to check for and fill missing data
    3.01 R. Gens,      7/01     Added logfile and quiet switch
    3.02 S. Watts,     10/01    Write ACTUAL # lines to <out>.meta
    3.5  P. Denny      1/03     Incorporated Rudi's latitude constraints
                                 Use new meta structures
                                 Update commandline parsing routine
                                 Estimate doppler or grab it from par file
				 
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	Does not compensate for skipped lines for JERS or RSAT.
        Output is very inefficient for SCANSAR images.
        SCANSAR images cannot be quicklooked.


******************************************************************************/
/******************************************************************************
*                                                                             *
* stf2raw -- Decodes the input data into AISP-compatible raw data             *
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   www.asf.alaska.edu            *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "lzFetch.h"
#include "decoder.h"
#include "dateUtil.h"

#define VERSION 3.5

/* allocation routine for meta_state_vectors */
meta_state_vectors *meta_state_vectors_init(int num_of_vectors);

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
		printf("   ERROR! createMeta_lz:lzStTime couldn't parse LZP\n"
			"   state vector date string '%s'!\n",lzStr);
		printf("   Month='%s'\n",month);
		  sprintf(errbuf,"   ERROR: createMeta_lz:lzStTime couldn't parse LZP\n"
			"   state vector date string '%s'!\n   Month='%s'\n",lzStr,month);
		  printErr(errbuf);
	}
/*Try to figure out the month*/
	monthNo=0;
	while (monthNo<12 && (0!=strncmp(month,monthNames[monthNo],3)))
			monthNo++;
	if (monthNo==12)
	{
		printf("   ERROR: createMeta_lz:lzStTime couldn't match month '%s'!\n",month);
		sprintf(errbuf,"   ERROR: createMeta_lz:lzStTime couldn't match month '%s'!\n",month);
		printErr(errbuf);
	}
	date->month=monthNo+1;
}

/* Initializes base meta structure (asf_meta.a)*/
meta_parameters *raw_init(void);
/* In (local_directory)/propagate.c */
void propagate_state(meta_parameters *meta, int nStVec, double data_int);
/* In (local_directory)/fetch_prc_stvec.c */
int fetch_prc_stvec(char *prc_path, ymd_date *seekDate, hms_time *seekTime, 
	stateVector *retVec, ymd_date *retDate, hms_time *retTime, int orbit);

void createMeta_lz(bin_state *s, char *inN, char *outN, char *img_timeStr, int nVec,
                   float fd, float fdd, float fddd, int prcflag, char *prcPath)
{
	double clock_ang;
	char parN[256],buf[255];
	meta_parameters *meta=raw_init();
	stateVector stVec;/*Source state vector*/
	ymd_date st_date,img_date;
	julian_date st_jdate,img_jdate;
	hms_time st_time,img_time;
	char *st_timeStr;/*Source state vector time, DD-MMM-YYYY hh:mm:ss.ttt*/
	int num_vecs;

/*Open the parameter file*/
	strcat(strcpy(parN,inN),".par");

/*Find start of current scene*/
	if (img_timeStr == NULL)
	  img_timeStr=lzStr(parN,"prep_block.location[0].line_date:",NULL);
	date_dssr2date(img_timeStr,&img_date,&img_time);
	date_ymd2jd(&img_date,&img_jdate);

/*Read a state vector.*/
	if (prcflag == 0) {
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].x:", nVec);
	    stVec.pos.x = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].y:", nVec);
    	    stVec.pos.y = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].z:", nVec);
	    stVec.pos.z = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].xv:", nVec);
	    stVec.vel.x = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].yv:", nVec);
	    stVec.vel.y = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].zv:", nVec);
	    stVec.vel.z = lzDouble(parN,buf,NULL);
	    sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:", nVec);
	    st_timeStr=lzStr(parN,buf,NULL);
	    parse_ymdTime(st_timeStr, &st_date, &st_time);
	}
	else {
	    int orbit = lzInt(parN,"prep_block.OrbitNr:",NULL);
	    fetch_prc_stvec(prcPath,&img_date,&img_time,&stVec,&st_date,&st_time,orbit);
	}

	date_ymd2jd(&st_date,&st_jdate);
	
/*	Convert from GEI to fixed-earth: (NOT needed-- already fixed?)
	gei2fixed(&stVec,utc2gha(st_jdate.year,st_jdate.jd,
		st_time.hour,st_time.min,st_time.sec));*/
	
/*Create a state vector structure to hold our state vector, with *one* state_loc, and copy it over.*/
	meta->state_vectors = meta_state_vectors_init(1);
	meta->state_vectors->vecs[0].vec = stVec;

/*Set time of image start & state vector, and propagate state vector*/
	meta->state_vectors->year = img_date.year;
	meta->state_vectors->julDay = img_jdate.jd;
	meta->state_vectors->second = date_hms2sec(&img_time);
	meta->state_vectors->vecs[0].time = date2sec(&st_jdate,&st_time)-date2sec(&img_jdate,&img_time);
	num_vecs = 2 + (int)(s->nLines/s->prf)/30.0;
	propagate_state(meta, num_vecs+1, (s->nLines/s->prf)/num_vecs);

/*Figure out satellite look direction*/ 
	clock_ang = lzDouble(parN,"prep_block.clock_angle:",NULL);
	if (clock_ang  == 90.0) 
		s->lookDir='R';
	else if (clock_ang == -90.0)
		s->lookDir='L';
	else {
          printf("   ERROR! Clock angle in .par file is %f!\n",clock_ang); 
	  sprintf(errbuf,"   ERROR! Clock angle in .par file is %f!\n",clock_ang);
	  printErr(errbuf);
	}
	
/*Update s-> fields with new state vector*/
	addStateVector(s,&meta->state_vectors->vecs[0].vec);
	
/*Update fields for which we have decoded header info.*/
	updateMeta(s,meta,inN);

/*Put some finishing touches on the updated Meta */
	meta->sar->range_doppler_coefficients[0] = fd;
	meta->sar->range_doppler_coefficients[1] = fdd;
	meta->sar->range_doppler_coefficients[2] = fddd;
	meta->sar->azimuth_doppler_coefficients[0] = fd;
	meta->sar->azimuth_doppler_coefficients[1] = 0.0;
	meta->sar->azimuth_doppler_coefficients[2] = 0.0;
	
/*Write out AISP .in parameter file.*/
	writeAISPparams(s,outN,fd,fdd,fddd);		

/*Write out and free the metadata structure*/
	meta_write(meta,outN);
	meta_free(meta);
}

/********************************
convertMetadata:
	Creates AISP .in and .fmt files,
as well as determining the number of lines in the l0 file,
by reading the granule (.gran) file.
*/
#include <ctype.h>
bin_state *convertMetadata_lz(char *inName,char *outName,int *numLines,
                              readPulseFunc *readNextPulse,
                              int prcflag, char *prcPath)
{
	bin_state *s;
	char lzName[256];
	int i;
	char *satName;
	
	strcat(strcpy(lzName,inName),".par");

/*Figure out what kind of SAR data we have, and initialize the appropriate decoder.*/
	satName=lzStr(lzName,"prep_block.satellite:",NULL);
	/*Trim white space from end of satellite name*/
	i=strlen(satName)-1;
	while (isspace(satName[i]))
		satName[i--]=0;
	
	/*Initialize the appropriate decoder routine*/
	if (0==strncmp(satName,"ERS",3))
		s=ERS_decoder_init(inName,outName,readNextPulse);
	else if (0==strncmp(satName,"JERS",4))
		s=JRS_decoder_init(inName,outName,readNextPulse);
	else if (0==strncmp(satName,"RSAT",4))
		s=RSAT_decoder_init(inName,outName,readNextPulse);
	else 
		{
		  printf("   Unrecognized satellite '%s'!\n",satName);
		  sprintf(errbuf,"   Unrecognized satellite '%s'!\n",satName);
		  printErr(errbuf);
		}
	
/*Read in essential parameters from granule file.*/
       /** numLines from parfile is often wrong;
 	** will learn the actual number of lines later in main()
	**/
	*numLines = lzInt(lzName,"prep_block.number_lines:",NULL);
	s->nFrames = lzInt(lzName,"prep_block.number_frames:",NULL);

/*Write out AISP .fmt parameter file. This must be done before processing
	** because starting line, window shift, and agc scaling are
	** perpetually updated via the updateAGC_window function
	** during processing.*/
	writeAISPformat(s,outName);

	return s;
}

int openErrorLog(bin_state *s, char *inN)
{
	char errName[256];
	strcat(strcpy(errName,inN),".errlog");
	s->fperr = FOPEN(errName,"w");

	return 1;
}

/*Prototypes*/
void createSubset(char *inN, float lowerLat, float upperLat, long *imgStart,
                  long *imgEnd, char *imgTimeStr, int *nVec,
                  float *fd, float *fdd, float *fddd);
void estimateDoppler(char *inN, float *fd, float *fdd, float *fddd);

/* Global variables - needed to handle missing lines properly */
long imgStart, imgEnd;
int outLine;
/*****************************
main:
	Open input file.
	Read each echo pulse
	Write each echo pulse
*/
int main(int argc,char *argv[])
{
	int 		nTotal, nVec=1;
	float		percent=5.0;
	float		fd,fdd,fddd;
	float		lowerLat=-99.0,upperLat=99.0;
	char 		*inName,*outName,imgTimeStr[20]="";
	bin_state 	*s;
	readPulseFunc 	readNextPulse;
	iqType 		*iqBuf;
	int		prcflag=0;
	char		prcPath[256];
	extern int	currArg; /* from cla.h (in asf.h) (initialized to 1) */

	logflag = quietflag = 0;

	/* Parse command line args */
	while (currArg < (argc-2))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-lat")) {
			CHECK_ARG(2);
			lowerLat = atof(GET_ARG(2));
			upperLat = atof(GET_ARG(1));
			if(lowerLat>upperLat) {
				float tmp=upperLat;
				upperLat = lowerLat;
				lowerLat = tmp;
			}
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile, GET_ARG(1));
			logflag=1;
			fLog = FOPEN(logFile, "a");
		}
		else if (strmatch(key,"-quiet")) {
			quietflag=1;
		}
		else if (strmatch(key,"-prc")) {
			CHECK_ARG(1);
			strcpy(prcPath, GET_ARG(1));
			prcflag = 1;
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	inName = argv[currArg++];
	outName = appendExt(argv[currArg],".raw");
		
	/* Determine start and end line for latitude constraint
	 -----------------------------------------------------*/
	if (lowerLat!=-99.0 && upperLat!=99.0)
	  createSubset(inName, lowerLat, upperLat, &imgStart, &imgEnd,
                       imgTimeStr, &nVec, &fd, &fdd, &fddd);
	else
	  estimateDoppler(inName, &fd, &fdd, &fddd);

	/* Read the metadata to determine where window position shifts
	   happen, as well as the number of lines in the image.
         ------------------------------------------------------------*/
	s=convertMetadata_lz(inName,outName,&nTotal,&readNextPulse,prcflag,
	                     prcPath);
	iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);
	if (imgEnd == 0) imgEnd = nTotal;
	
	/* Now we just loop over the output lines, writing as we go.
	 ---------------------------------------------------------*/
	s->fpOut=FOPEN(outName,"wb");
	s->nLines=0;
	s->readStatus=1;

	openErrorLog(s,inName);

	for (outLine=0;outLine<nTotal;outLine++)
	{
		if (s->curFrame >= s->nFrames) { 
		  printf("   Reached end of file\n"); 
		  if (logflag) printLog("   Reached end of file\n"); 
		  break; 
		}

	        /* Now read the next pulse of data.
		 ---------------------------------*/
		readNextPulse(s,iqBuf);

		/* If the read status is good, write this data.
		 ---------------------------------------------*/ 
		if (s->readStatus == 1) {
		  /* write some extra lines at the end for the SAR processing */
		  if (((outLine >= imgStart) && (outLine <= imgEnd+4096)) ||  /* descending */
		    ((outLine >= imgEnd) && (outLine <= imgStart+4096)))      /* ascending */
		  {
			FWRITE(iqBuf,sizeof(iqType),s->nSamp*2,s->fpOut); 
			s->nLines++;
		  }
		}
		/* Write status information to screen. 
		 ------------------------------------*/
		if ((outLine*100/nTotal) == percent) {
			printf("   Completed %3.0f percent\n", percent);
			percent += 5.0;
		}
	}
	if (lowerLat!=-99.0 && upperLat!=99.0) {
	  s->nLines -= 4096; /* reduce the line number from extra padding */
	  createMeta_lz(s,inName,outName,imgTimeStr,nVec,fd,fdd,fddd,prcflag,
	                prcPath);
	  /* Set meta's center lat & long */
/******	  meta->general->center_latitude = lowerLat + (upperLat-lowerLat)/2;*/
/*****	  meta->general->center_longitude =  */
	}
	else
	  createMeta_lz(s,inName,outName,NULL,nVec,fd,fdd,fddd,prcflag,prcPath);

	/* Clean up memory & open files
	 -----------------------------*/
	FREE(iqBuf);
	FREE(outName);
	FCLOSE(s->fpOut); 
	FCLOSE(s->fperr);
	delete_bin_state(s);

	/* Report & finish
	 ----------------*/
	printf("   Completed 100 percent\n\n");
	printf("   Wrote %i lines of raw signal data.\n\n",s->nLines);
	if (logflag) {
	  sprintf(logbuf,"   Wrote %i lines of raw signal data.\n\n",s->nLines);
	  printLog(logbuf);
	}

	exit(EXIT_SUCCESS);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-lat <lower> <upper>] [-log <file>] [-quiet] [-prc <path>]\n"
	"                   <input> <output>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <input>   VEXCEL Level-0 signal data.\n"
	"   <output>  AISP-compatible raw data.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -lat <lower> <upper> Option for latitude constraints.\n"
	"   -log <file>          Copy output to a log file.\n"
	"   -quiet               Surpress output to the essential.\n"
	"   -prc <path>          Path to use percision state vectors.\n"
	"                         (assume prc format)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   ASF Level-Zero Swath Conversion Routine.\n"
	"   This program decodes the input data into AISP-compatible\n"
	"   raw data.  Currently supports ERS, JERS, Radarsat, Strip mode\n"
	"   (all beams), but not Radarsat ScanSAR mode.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
