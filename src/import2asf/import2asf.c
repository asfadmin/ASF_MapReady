/*************************************************************************
NAME:	import2asf
 
SYNOPSIS:  import2asf [-amplitude] [-sigma] [-gamma] [-beta] [-power] 
               [-log <file>] -CEOS | -STF | -ESRI | -ENVI infile outfile

DESCRIPTION:
		import2asf ingests the complete variety of CEOS data,
                STF data as well as the external ESRI and ENVI data
		into ASF internal format

DESCRIPTION:
    Ingests the complete variety of CEOS data into ASF tools internal format

EXTERNAL ASSOCIATES:

FILE REFERENCES:

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0    12/03   R. Gens     Combined sarin, calibrate, trim_slc and 
                                ceos2raw into one program
    1.1     4/04   P. Denny    Allowed for RSI naming scheme as well as
                                our typical CEOS naming scheme
    1.2     5/04   R. Gens     Added stf2raw and external ESRI and ENVI
                                formats; renamed the tool to import2asf

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*************************************************************************/
/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "calibrate.h"
#include "ceos.h"
#include "ceos_io.h"
#include "dateUtil.h"
#include "decoder.h"
#include "esri.h"
#include "lzFetch.h"
#include <ctype.h>
#include <string.h>

#define VERSION 1.2
#define MAX_tableRes 512
#define REQUIRED_ARGS 3

void createSubset(char *inN, float lowerLat, float upperLat, long *imgStart,
                  long *imgEnd, char *imgTimeStr, int *nVec,
                  float *fd, float *fdd, float *fddd);
void estimateDoppler(char *inN, float *fd, float *fdd, float *fddd);
int openErrorLog(bin_state *s, char *inN);
void createMeta_lz(bin_state *s, char *inN, char *outN, char *img_timeStr, int nVec,
                   float fd, float fdd, float fddd, int prcflag, char *prcPath);
bin_state *convertMetadata_lz(char *inName,char *outName,int *numLines,
                              readPulseFunc *readNextPulse,
                              int prcflag, char *prcPath);
void createMeta_ceos(bin_state *s, struct dataset_sum_rec *dssr, char *inN,
                     char *outN);
bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,
                                readPulseFunc *readNextPulse);
void usage(char *name);

int firstRecordLen(char *ceosName)
{
  FILE *f;
  struct HEADER h;

  f=FOPEN(ceosName,"rb");    /*Open file.*/
  FREAD(&h,1,12,f);          /*Read first CEOS header.*/
  FCLOSE(f);                 /*Close file*/
  return bigInt32(h.recsiz); /*put recsiz in proper endian format & return*/
}

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;

  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';

  return out;
}

/* Global variables - needed to handle missing lines properly */
long imgStart, imgEnd;
int outLine;

int main(int argc, char *argv[])
{
  struct IOF_VFDR image_fdr;
  meta_parameters *meta;
  cal_params *cal_param=NULL;
  bin_state *s;
  readPulseFunc readNextPulse;
  iqType *iqBuf;
  esri_header *esri=NULL;
  FILE *fpIn=NULL, *fpOut=NULL, *fp;;
  char in_type[25]="", out_type[25]="";
  char inDataName[256], inMetaName[256], outName[288], prcPath[255];
  char outBaseName[256], type[255]="", tmp[255], imgTimeStr[20]="";
  char line[255]="", key[25]="", value[25]="";
  int nl, ns=0, ii, kk, tableRes=MAX_tableRes, tablePix=0, headerBytes;
  int latConstraintFlag=FALSE, prcflag=FALSE, nTotal, nVec=1;
  long offset;
  short *short_buf=NULL, *cpx_buf=NULL;
  char *byte_buf=NULL;
  float *out_buf=NULL, percent=5.0;
  float fd, fdd, fddd, lowerLat=NAN, upperLat=NAN;
  double noise_table[MAX_tableRes];
  double incid_cos[MAX_tableRes], incid_sin[MAX_tableRes];
  extern int currArg; /* from cla.h in asf.h */

  logflag=FALSE;
  quietflag=FALSE;

  /* Parse command line args */
  while (currArg < (argc-REQUIRED_ARGS)) {
    char *key=argv[currArg++];
    /* check for required option */
    if (strmatch(key,"-CEOS")) {
      if (strcmp(type,"")==0) strcpy(type,key);
      else {
	sprintf(errbuf,"\n   ERROR: image type already defined as %s!\n\n", type);
	printErr(errbuf);
      }
    }
    else if (strmatch(key,"-STF")) {
      if (strcmp(type,"")==0) strcpy(type,key);
      else {
	sprintf(errbuf,"\n   ERROR: image type already defined as %s!\n\n", type);
	printErr(errbuf);
      }
    }
    else if (strmatch(key,"-ESRI")) {
      if (strcmp(type,"")==0) strcpy(type,key);
      else {
	sprintf(errbuf,"\n   ERROR: image type already defined as %s!\n\n", type);
	printErr(errbuf);
      }
    }
    else if (strmatch(key,"-ENVI")) {
      if (strcmp(type,"")==0) strcpy(type,key);
      else {
	sprintf(errbuf,"\n   ERROR: image type already defined as %s!\n\n", type);
	printErr(errbuf);
      }
    }
    /* check for other options */
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1); /*one string argument: log file */
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile,"a");
      logflag=TRUE;
    }
    else if (strmatch(key,"-quiet"))
      quietflag=TRUE;
    else if (strmatch(key,"-prc")) {
      CHECK_ARG(1);
      strcpy(prcPath, GET_ARG(1));
      prcflag=TRUE;
    }
    else if (strmatch(key,"-lat")) {
      CHECK_ARG(2);
      lowerLat = strtod(GET_ARG(2),NULL);
      upperLat = strtod(GET_ARG(1),NULL);
      if(lowerLat>upperLat) {
	float tmp=upperLat;
	upperLat = lowerLat;
	lowerLat = tmp;
      }
      if (   lowerLat<-90.0 || lowerLat>90.0
	     || upperLat<-90.0 || upperLat>90.0) {
	printf("Invalid latitude constraint (must be between -90 and 90).\n");
	printf("Exiting...\n");
	exit(EXIT_FAILURE);
      }
      latConstraintFlag=TRUE;
    }
    else if (strmatch(key,"-amplitude")) 
      sprintf(out_type,"amp"); /* create amplitude image */
    else if (strmatch(key,"-sigma")) 
      sprintf(out_type, "sigma");  /* create calibrated image (sigma dB values) */
    else if (strmatch(key,"-gamma")) 
      sprintf(out_type, "gamma");  /* create calibrated image (gamma dB values) */
    else if (strmatch(key,"-beta")) 
      sprintf(out_type, "beta");  /* create calibrated image (beta dB values) */
    else if (strmatch(key,"-power")) 
      sprintf(out_type, "power");  /* create power image */
    else {
      printf("\n** Invalid option:  %s\n\n",argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < REQUIRED_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  /* Read required arguments */
  strcpy(inDataName,argv[currArg]);
  strcpy(inMetaName,argv[currArg+1]);
  strcpy(outBaseName,argv[currArg+2]);

  /* Lets get started */
  system("date");
  printf("Program: import2asf\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: import2asf\n\n");
  }

  /* Check whether options are chosen correctly */
  if (strncmp(type, "-STF", 4)!=0) { 
    if (prcflag) {
      sprintf(tmp, "   WARNING: No precision state vectors used for this image type!\n");
      printf(tmp);
      if (logflag) printLog(tmp);
      prcflag=FALSE;
    }
    if (latConstraintFlag) {
      sprintf(tmp, "   WARNING: No latitude constraints for this image type!\n");
      printf(tmp);
      if (logflag) printLog(tmp);
      latConstraintFlag=FALSE;
    }
  }

  /* Ingest all sort of flavors of CEOS data */
  if (strncmp(type, "-CEOS", 5)==0) {

    sprintf(tmp,"   Data format: CEOS\n");
    printf(tmp);
    if (logflag) printLog(tmp);

    /* Create metadata */
    meta=meta_create(inMetaName);

    /* Check out input data type */
    if (meta->general->data_type==COMPLEX_BYTE) { /* raw data */

      /* Let the user know what format we are working on */
      if (strcmp(out_type, "")!=0)
	sprintf(tmp,
		"   Input data type: level zero raw data\n"
		"   Chosen output option not possible\n"
		"   Output data type: complex float raw data\n\n");
      else
	sprintf(tmp,
		"   Input data type: level zero raw data\n"
		"   Output data type: complex float raw data\n\n");
      printf(tmp);
      if (logflag) printLog(tmp);

      /* Handle output files */ 
      create_name(outName, outBaseName, "_raw.img");
      meta->general->data_type=COMPLEX_REAL32;
      s=convertMetadata_ceos(inMetaName, outName, &nl, &readNextPulse);
      iqBuf=(iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
      fpOut=FOPEN(outName, "wb");
      getNextCeosLine(s->binary, s, inMetaName, outName); /* Skip CEOS header. */
      s->nLines=0;
      for (ii=0; ii<nl; ii++) {
	readNextPulse(s, iqBuf, inDataName, outName);
	FWRITE(iqBuf, s->nSamp, 2, fpOut);
	s->nLines++;
      }
      return 0;
    }

    /* complex data */
    else if (meta->general->data_type==COMPLEX_INTEGER16) {

      /* Let the user know what format we are working on */
      if (strcmp(out_type, "")!=0) 
	sprintf(tmp,
		"   Input data type: single look complex\n"
		"   Chosen output option not possible\n"
		"   Output data type: single look dcomplex\n\n");
      else
	sprintf(tmp,
		"   Input data type: single look complex\n"
		"   Output data type: single look complex\n\n");
      printf(tmp);
      if (logflag) printLog(tmp);

      /* Handle output files */ 
      create_name(outName, outBaseName, "_cpx.img");
      meta->general->data_type=COMPLEX_REAL32;
      meta_write(meta,outName);

      /* Take care of image files and memory */
      fpIn  = fopenImage(inDataName,"rb");
      fpOut = fopenImage(outName,"wb");
      nl = meta->general->line_count;
      ns = meta->general->sample_count;
      cpx_buf = (short *) MALLOC(2*ns * sizeof(short));
      out_buf = (float *) MALLOC(2*ns * sizeof(float));
      
      /* Read single look complex data */
      get_ifiledr(inDataName,&image_fdr);
      /* file + line header */
      headerBytes = firstRecordLen(inDataName)
	+ (image_fdr.reclen - ns * image_fdr.bytgroup);
      for (ii=0; ii<nl; ii++) {
	offset = headerBytes+ii*image_fdr.reclen;
	FSEEK(fpIn, offset, 0);
	FREAD(cpx_buf, sizeof(short), 2*ns, fpIn);
	for (kk=0; kk<ns*2; kk++)  
	  out_buf[kk]=(float)cpx_buf[kk];
	FWRITE(out_buf, sizeof(float), 2*ns, fpOut);
	if ((ii*100/nl)>percent) {
          printf("   Completed %3.0f percent\n",percent);
          percent+=5.0;
	}
      }
      printf("   Completed 100 percent\n\n");
      FCLOSE(fpOut);
    }

    else { /* some kind of amplitude data */
      
      /* Let the user know what format we are working on */
      if (check_cal(inMetaName)==0 && strcmp(out_type, "power")!=0)
	sprintf(out_type, "amp");
      if (meta->projection!=NULL && meta->projection->type!=MAGIC_UNSET_CHAR) 
	/* This must be ScanSAR */
	if (meta->projection->type!=SCANSAR_PROJECTION) {
	  /* This is actually geocoded */
	  sprintf(tmp, 
		  "   Input data type: level two data\n"
		  "   Output data type: geocoded amplitude image\n\n");
	  create_name(outName, outBaseName, "_geo.img");
	}
      if (strcmp(out_type, "amp")==0) {
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: amplitude image\n\n");
	create_name(outName, outBaseName, "_amp.img");
      }
      else if (strcmp(out_type, "power")==0) {
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: power image\n\n");
	create_name(outName, outBaseName, "_power.img");
      }
      else if (strcmp(out_type, "sigma")==0) {
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: calibrated image (sigma dB values)\n\n");
	create_name(outName, outBaseName, "_sigma.img");
      }
      else if (strcmp(out_type, "gamma")==0) {
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: calibrated image (gamma dB values)\n\n");
	create_name(outName, outBaseName, "_gamma.img");
      }
      else if (strcmp(out_type, "beta")==0) { 
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: calibrated image (beta dB values)\n\n");
	create_name(outName, outBaseName, "_beta.img");
      }
      else { /* no output type so far: default is amplitude */
	sprintf(out_type, "amp");
	sprintf(tmp, 
		"   Input data type: level one data\n"
		"   Output data type: amplitude image\n\n");
	create_name(outName, outBaseName, "_amp.img");
      }
      printf(tmp);
      if (logflag) printLog(tmp);
      
      /* Open image files */
      fpIn=fopenImage(inDataName,"rb");
      fpOut=fopenImage(outName,"wb");
      
      /* Check number of lines and samples, size of the header */
      nl=meta->general->line_count;
      ns=meta->general->sample_count;
      get_ifiledr(inDataName,&image_fdr);
      headerBytes = firstRecordLen(inDataName)
	+ (image_fdr.reclen - ns * image_fdr.bytgroup);
      
      /* Allocate memory for 16 bit amplitude data */
      if (meta->general->data_type==INTEGER16) { /* 16 bit amplitude data */
	sprintf(in_type, "int16");
	short_buf = (short *) MALLOC(ns * sizeof(short));
	out_buf = (float *) MALLOC(ns * sizeof(float));
      }
      /* Allocate memory for 8 bit amplitude data */
      else if (meta->general->data_type==BYTE) { /* 8 bit amplitude data */
	sprintf(in_type, "byte");
	byte_buf = (char *) MALLOC(ns * sizeof(char));
	out_buf = (float *) MALLOC(ns * sizeof(float));
      }
      else 
	printErr("   Error: Unkown data format\n");
      
      /* Handle output files */ 
      meta->general->data_type=REAL32;
      meta_write(meta,outName);
      
      /* Read calibration parameters if required */
      if (strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0 
	  || strcmp(out_type, "beta")==0) {
	if (nl<1500) tableRes=128;
	else if (nl<3000) tableRes=256;
	tablePix=((ns+(tableRes-1))/tableRes);
	cal_param=create_cal_params(inMetaName);
	if (cal_param==NULL) sprintf(out_type, "amp");
	if (strcmp(out_type, "sigma")==0) 
	  cal_param->output_type=sigma_naught;
	else if (strcmp(out_type, "gamma")==0) 
	  cal_param->output_type=gamma_naught;
	else if (strcmp(out_type, "beta")==0) 
	  cal_param->output_type=beta_naught;
	else {
	  sprintf(tmp,
		  "\n"
		  "   Calibration parameters could not be extracted out of CEOS file\n"
		  "   Output data type: amplitude image\n\n");
	  printf(tmp);
	  if (logflag) printLog(tmp);
	}
      }
      
      /* Read 16 bit data and convert to calibrated amplitude data */
      if ((strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0 
	   || strcmp(out_type, "beta")==0) && (strcmp(in_type, "int16")==0)) {
	
	for (ii=0; ii<nl; ii++) {
	  offset = headerBytes+ii*image_fdr.reclen;
	  FSEEK(fpIn, offset, 0);
	  FREAD(short_buf, sizeof(short), ns, fpIn);
	  
	  /*Allocate noise table entries and/or update if needed.*/
	  if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	    for (kk=0;kk<tableRes;kk++)
	      noise_table[kk]=get_noise(cal_param,kk*tablePix,ii);
	  if (strcmp(out_type, "gamma")==0)
	    /*Allocate incidence table entries or update.*/
	    if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	      for (kk=0;kk<tableRes;kk++)
		incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
	  if (strcmp(out_type, "beta")==0)
	    /*Allocate incidence table entries or update.*/
	    if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	      for (kk=0;kk<tableRes;kk++)
		incid_sin[kk]=get_invSinIncAngle(cal_param,kk*tablePix,ii);
	  
	  for (kk=0; kk<ns; kk++) { 
	    if (short_buf[kk])
	      {  /*Interpolate noise table to find this pixel's noise.*/
		double index=(float)kk/tablePix;
		int    base=(int)index;
		double frac=index-base;
		double noise=noise_table[base]+frac*(noise_table[base+1]-noise_table[base]);
		double incid=0.0;
		if (cal_param->output_type==gamma_naught)
		  incid=incid_cos[base]+frac*(incid_cos[base+1]-incid_cos[base]);
		if (cal_param->output_type==beta_naught)
		  incid=incid_sin[base]+frac*(incid_sin[base+1]-incid_sin[base]);
		out_buf[kk]=get_cal_dn(cal_param,noise,incid,short_buf[kk]);
	      }
	    else 
	      out_buf[kk]=0;
	  }
	  
	  put_float_line(fpOut, meta, ii, out_buf);
	  
	  if ((ii*100/nl)>percent) {
	    printf("   Completed %3.0f percent\n",percent);
	    percent+=5.0;
	  }
	}
	printf("   Completed 100 percent\n\n");
	FCLOSE(fpOut);
      }
      
      /* Read 8 bit data and convert to calibrated amplitude data */
      else if ((strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0
		|| strcmp(out_type, "beta")==0) && (strcmp(in_type, "byte")==0)) {
	
	for (ii=0; ii<nl; ii++) {
	  offset = headerBytes+ii*image_fdr.reclen;
	  FSEEK(fpIn, offset, 0);
	  FREAD(byte_buf, sizeof(char), ns, fpIn);
	  
	  /*Allocate noise table entries and/or update if needed.*/
	  if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	    for (kk=0;kk<tableRes;kk++)
	      noise_table[kk]=get_noise(cal_param,kk*tablePix,ii);
	  if (strcmp(out_type, "gamma")==0)
	    /*Allocate incidence table entries or update.*/
	    if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	      for (kk=0;kk<tableRes;kk++)
		incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
	  if (strcmp(out_type, "beta")==0)
	    /*Allocate incidence table entries or update.*/
	    if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
	      for (kk=0;kk<tableRes;kk++)
		incid_sin[kk]=get_invSinIncAngle(cal_param,kk*tablePix,ii);
	  
	  for (kk=0; kk<ns; kk++) { 
	    if (byte_buf[kk]) {
	      /*Interpolate noise table to find this pixel's noise.*/
	      double index=(float)kk/tablePix;
	      int    base=(int)index;
	      double frac=index-base;
	      double noise=noise_table[base]+frac*(noise_table[base+1]-noise_table[base]);
	      double incid=0.0;
	      if (cal_param->output_type==gamma_naught)
		incid=incid_cos[base]+frac*(incid_cos[base+1]-incid_cos[base]);
	      if (cal_param->output_type==beta_naught)
		incid=incid_sin[base]+frac*(incid_sin[base+1]-incid_sin[base]);
	      out_buf[kk]=get_cal_dn(cal_param,noise,incid,byte_buf[kk]);
	    }
	    else 
	      out_buf[kk]=0;
	  }
	  
	  put_float_line(fpOut, meta, ii, out_buf);
	  
	  if ((ii*100/nl)>percent) {
	    printf("   Completed %3.0f percent\n",percent);
	    percent+=5.0;
	  }
	}
	printf("   Completed 100 percent\n\n");
	FCLOSE(fpOut);
      }
      
      /* Read 16 bit amplitude data */
      else if (strcmp(in_type, "int16")==0) {
	
	for (ii=0; ii<nl; ii++) {
	  offset = headerBytes+ii*image_fdr.reclen;
	  FSEEK(fpIn, offset, 0);
	  FREAD(short_buf, sizeof(short), ns, fpIn);
	  
	  for (kk=0; kk<ns; kk++) {
	    if (strcmp(out_type, "power")==0)
	      out_buf[kk]=(float)(short_buf[kk]*short_buf[kk]);
	    else
	      out_buf[kk]=(float)short_buf[kk];
	  }
	  
	  put_float_line(fpOut, meta, ii, out_buf);
	  
	  if ((ii*100/nl)>percent) {
	    printf("   Completed %3.0f percent\n",percent);
	    percent+=5.0;
	  }
	}
	printf("   Completed 100 percent\n\n");
	FCLOSE(fpOut);
      }
      
      /* Read 8 bit amplitde data */
      else {
	
	for (ii=0; ii<nl; ii++) {
	  offset = headerBytes+ii*image_fdr.reclen;
	  FSEEK(fpIn, offset, 0);
	  FREAD(byte_buf, 1, ns, fpIn);
	  
	  for (kk=0; kk<ns; kk++) {
	    if (strcmp(out_type, "power")==0) out_buf[kk]=(float)(byte_buf[kk]*byte_buf[kk]);
	    else out_buf[kk]=(float)byte_buf[kk];
	  }
	  
	  put_float_line(fpOut, meta, ii, out_buf);
	  
	  if ((ii*100/nl)>percent) {
	    printf("   Completed %3.0f percent\n",percent);
	    percent+=5.0;
	  }
	}
	printf("   Completed 100 percent\n\n");
	FCLOSE(fpOut);
      }
    }    
  }

  /* Ingest Vexcel Sky Telemetry Format (STF) data */
  else if (strncmp(type, "-STF", 4)==0) {

    sprintf(tmp,"   Data format: STF\n");
    printf(tmp);
    if (logflag) printLog(tmp);
    
    /* Handle output file name */
    create_name(outName, outBaseName, "_raw.img");

    if (latConstraintFlag) {
      /* Determine start and end line for latitude constraint */
      createSubset(inDataName, lowerLat, upperLat, &imgStart, &imgEnd,
		   imgTimeStr, &nVec, &fd, &fdd, &fddd);
    }
    else {
      /* Since createSubset isn't called we need to figure nVec
       * also we estimate the doppler */
      int done = FALSE;
      char *tmp_timeStr;
      char buf[256];
      char parName[256];
      hms_time tmp_time;
      ymd_date tmp_date;
      double vec_sec, loc_sec;
      double new_delta, old_delta=1000.0, delta=1000.0;
      
      /* Figure out what time the image started getting recorded */
      strcpy(parName,inMetaName);
      tmp_timeStr = lzStr(parName, "prep_block.location[0].line_date:", NULL);
      date_dssr2date(tmp_timeStr,&tmp_date,&tmp_time);
      loc_sec = date_hms2sec(&tmp_time);
      
      /* Find the index of the closest state vector to the start time */
      nVec=1;
      while (!done) {
	sprintf(buf, "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:", nVec);
	tmp_timeStr = lzStr(parName, buf, NULL);
	parse_ymdTime(tmp_timeStr,&tmp_date,&tmp_time);
	vec_sec = date_hms2sec(&tmp_time);
	new_delta = fabs(vec_sec - loc_sec);
	if ((new_delta>delta) && (old_delta>delta)) done=TRUE;
	old_delta = delta;
	delta = new_delta;
	nVec++;
      }
      FREE(tmp_timeStr);
      estimateDoppler(inDataName, &fd, &fdd, &fddd);
    }
    
    /* Read the metadata to determine where window position shifts
       happen, as well as the number of lines in the image.
       ------------------------------------------------------------*/
    s=convertMetadata_lz(inDataName,outName,&nTotal,&readNextPulse,prcflag,prcPath);
    iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);
    if (imgEnd == 0) imgEnd = nTotal;
    
    /* Now we just loop over the output lines, writing as we go.
       ---------------------------------------------------------*/
    s->fpOut=FOPEN(outName,"wb");
    s->nLines=0;
    s->readStatus=1;
    
    openErrorLog(s,inDataName);
    
    for (outLine=0;outLine<nTotal;outLine++)
      {
	if (s->curFrame >= s->nFrames) {
	  printf("   Reached end of file\n");
	  if (logflag) printLog("   Reached end of file\n");
	  break;
	}
	
	/* Now read the next pulse of data.
	   ---------------------------------*/
	readNextPulse(s, iqBuf, inDataName, outName);
	
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
    
    if (latConstraintFlag) {
      s->nLines -= 4096; /* reduce the line number from extra padding */
      createMeta_lz(s,inDataName,outName,imgTimeStr,nVec,fd,fdd,fddd,prcflag,prcPath);
    }
    else {
      createMeta_lz(s,inDataName,outName,NULL,nVec,fd,fdd,fddd,prcflag,prcPath);
    }
    
    /* Clean up memory & open files
       -----------------------------*/
    FREE(iqBuf);
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
  }

  /* Ingest ESRI format data */
  else if (strncmp(type, "-ESRI", 5)==0) {

    sprintf(tmp,"   Data format: ESRI\n");
    printf(tmp);
    if (logflag) printLog(tmp);

    /* Handle output file name */
    create_name(outName, outBaseName, "_amp.img");
 
    /* Allocate memory for ESRI header structure */
    esri = (esri_header *)MALLOC(sizeof(esri_header));
    
    /* Read .hdr and fill meta structures */ 
    fp = FOPEN(inMetaName, "r");
    while (NULL != fgets(line, 255, fp)) {
      sscanf(line, "%s %s", key, value);
      if (strncmp(key, "NROWS", 5)==0) esri->nrows = atoi(value);
      else if (strncmp(key, "NCOLS", 5)==0) esri->ncols = atoi(value);
      else if (strncmp(key, "NBITS", 5)==0) {
	esri->nbits = atoi(value);
	if (esri->nbits < 8) {
	  sprintf(errbuf, "\n   ERROR: metadata do not support data less than 8 bit\n\n");
	  printErr(errbuf);
	}
      } 
      else if (strncmp(key, "NBANDS", 6)==0) {
	esri->nbands = atoi(value);
	if (esri->nbands > 1) {
	  sprintf(errbuf, "\n   ERROR: metadata do not support multi-band data\n\n");
	  printErr(errbuf);
	}
      }
      else if (strncmp(key, "BYTEORDER", 9)==0) esri->byteorder = value[0];
      else if (strncmp(key, "LAYOUT", 6)==0) {
	sprintf(esri->layout, "%s", value);
	if (strncmp(uc(esri->layout), "BIL", 3)!=0) {
	  sprintf(errbuf, "\n   ERROR: metadata do not support data other than BIL format\n\n");
	  printErr(errbuf);
	}
      }
      else if (strncmp(key, "SKIPBYTES", 9)==0) {
	esri->skipbytes = atoi(value);
	if (esri->skipbytes > 0) {
	  sprintf(errbuf, "\n   ERROR: metadata only support generic binary data\n\n");
	  printErr(errbuf);
	}
      }
      else if (strncmp(key, "ULXMAP", 6)==0) esri->ulxmap = atof(value);
      else if (strncmp(key, "ULYMAP", 6)==0) esri->ulymap = atof(value);
      else if (strncmp(key, "XDIM", 4)==0) esri->xdim = atof(value);
      else if (strncmp(key, "YDIM", 4)==0) esri->ydim = atof(value);
      /* bandrowbytes, totalrowbytes, bandgapdata and nodata currently not used */
    }
    FCLOSE(fp);
    
    /* Fill metadata structure with valid data */
    meta = esri2meta(esri);
    
    /* Write metadata file */
    meta_write(meta, outName);

    /* Write data file - currently no header, so just copying generic binary */
    sprintf(tmp, "cp %s %s", inDataName, outName);
    system(tmp);

    /* Clean and report */
    meta_free(meta);
    sprintf(logbuf, "   Converted ESRI file (%s) to ASF internal file (%s)\n\n",
	    inDataName, outName);
    printf(logbuf);
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      printLog(logbuf);
      FCLOSE(fLog);
    }
  }
  /* Ingest ENVI format data */
  else if (strncmp(type, "-ENVI", 5)==0) {

	sprintf(tmp,"   Data format: ENVI\n");
	printf(tmp);
	if (logflag) printLog(tmp);

  }

  exit(EXIT_SUCCESS);
}


/* usage - enter here on command-line usage error*/
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -prc <path> ] [ -lat <lower> <upper> ]\n"
         "      [ -amplitude ] [ -sigma ] [ -gamma ] [ -beta ] [ -power ]\n"
 	 "      [ -log <file> ] [ - quiet ] -CEOS | -STF | -ESRI | -ENVI\n"
	 "      <inData> <inMeta> <outBase>\n", name);
  printf("\n"
	 "OPTIONAL ARGUMENTS:\n"
	 "   -prc <path> Path to use DLR precision state vectors.\n"
	 "   -lat <lower> <upper> Latitude constraints for STF data.\n"
	 "   -amplitude	 Creates an amplitude image.\n" 
	 "   -sigma	 Creates a calibrated image (sigma dB values).\n"
	 "   -gamma	 Creates a calibrated image (gamma dB values).\n"
	 "   -beta	 Creates a calibrated image (beta dB values).\n"
	 "   -power	 Creates a power image.\n"
 	 "   -log <file> Allows the output to be written to a log file.\n"
	 "   -quiet      Restricts the output to the essential\n");
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   -CEOS       Input data is in CEOS format.\n"
         "   -STF        Input data is in Sky Telemetry Format.\n"
         "   -ESRI       Input data is in generic ESRI raster format.\n"
         "   -ENVI       Input data is in ENVI format.\n"
         "   inData      Data file including the extension.\n"
         "   inMeta      Metadata file with the extension.\n"
	 "   outBase     Basename of your new ASF tools format data.\n");
  printf("\n"
	 "DESCRIPTION:\n"
 	 "   %s ingests the complete variety of CEOS data, STF data\n"
         "   as well as external ESRI and ENVI data into the internal ASF data.\n",name);
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n\n", VERSION);
  exit(EXIT_FAILURE);
}
