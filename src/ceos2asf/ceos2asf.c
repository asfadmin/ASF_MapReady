/*************************************************************************
 NAME:	ceos2asf
 
 SYNOPSIS:  ceos2asf [-amplitude] [-sigma] [-gamma] [-beta] [-power] 
                     [-log <file>] infile outfile

 DESCRIPTION:
		ceos2asf ingests the complete variety of CEOS data
		into ASF internal format
 
 EXTERNAL ASSOCIATES:

 FILE REFERENCES:
 
 PROGRAM HISTORY:
     VERS:   DATE:  AUTHOR:	PURPOSE:
     ---------------------------------------------------------------
     1.0    12/03   R. Gens	Combined sarin, calibrate, trim_slc and 
				ceos2raw into one program

 HARDWARE/SOFTWARE LIMITATIONS:

 ALGORITHM DESCRIPTION:
 
 ALGORITHM REFERENCES:
 
 BUGS:
 
****************************************************************************
								           *
   ceos2asf ingests the complete variety of CEOS data
                into ASF internal format
   
   Copyright (C) 2003  ASF Technical Services Office    	    	   *
									   *
   This program is free software; you can redistribute it and/or modify    *
   it under the terms of the GNU General Public License as published by    *
   the Free Software Foundation; either version 2 of the License, or       *
   (at your option) any later version.					   *
									   *
   This program is distributed in the hope that it will be useful,	   *
   but WITHOUT ANY WARRANTY; without even the implied warranty of    	   *
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	   *
   GNU General Public License for more details.  (See the file LICENSE     *
   included in the asf_tools/ directory).				   *
									   *
   You should have received a copy of the GNU General Public License       *
   along with this program; if not, write to the Free Software		   *
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
									   *
       ASF Advanced Product Development LAB Contacts:			   *
	APD E-mail:	apd@asf.alaska.edu 				   *
 									   *
	Alaska SAR Facility			APD Web Site:	           *	
	Geophysical Institute			www.asf.alaska.edu/apd	   *
      	University of Alaska Fairbanks					   *
	P.O. Box 757320							   *
	Fairbanks, AK 99775-7320					   *
								  	   *
****************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "calibrate.h"
#include "ceos_io.h"
#include "ceos.h"
#include "decoder.h"

#define VERSION 1.0
#define MAX_tableRes 512

void createMeta_ceos(bin_state *s,struct dataset_sum_rec *dssr,char *inN,char *outN);
bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,readPulseFunc *readNextPulse);
void linkFlag(short,char*);
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

int main(int argc, char *argv[])
{
	struct IOF_VFDR image_fdr;
        meta_parameters *meta;
        cal_params *cal_param=NULL;
        bin_state *s;
        readPulseFunc readNextPulse;
        iqType *iqBuf;
        FILE *fpIn=NULL, *fpOut=NULL;
	char in_type[25]="", out_type[25]="", infile[255], outfile[255], *inbase, *outbase, tmp[255];
	int nl, ns=0, ii, kk, tableRes=MAX_tableRes, tablePix=0, headerBytes;
	long offset;
	short *short_buf=NULL, *cpx_buf=NULL;
	char *byte_buf=NULL;
        float *out_buf=NULL, percent=5.0;
        double noise_table[MAX_tableRes], incid_cos[MAX_tableRes], incid_sin[MAX_tableRes];

	logflag=0;
	quietflag=0;
	currArg=1;	/* from cla.h which is in asf.h */

	/* Parse command line args */
	while (currArg < (argc-2))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1); /*one string argument: log file */
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile,"a");
			logflag=1;
		}
		else if (strmatch(key,"-quiet"))
			quietflag=1;
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
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	/* Read required arguments */
	inbase=argv[currArg];
	outbase=argv[currArg+1];
	create_name(infile, inbase, ".D");

	/* Lets get started */
	StartWatch();
	system("date");
	printf("Program: ceos2asf\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: ceos2asf\n\n");
	}

	/* Create metadata */
        meta=meta_create(inbase);

	/* Check out input data type */
	if (meta->general->data_type==COMPLEX_BYTE) { /* raw data */

	  /* Let the user know what format we are working on */
	  if (strcmp(out_type, "")!=0)
	    sprintf(tmp,"   Input data type: level zero raw data\n"
			"   Chosen output option not possible\n   Output data type: complex float raw data\n\n");
	  else
	    sprintf(tmp,"   Input data type: level zero raw data\n   Output data type: complex float raw data\n\n");
	  printf(tmp);
	  if (logflag) printLog(tmp);

	  /* Handle output files */ 
	  create_name(outfile, outbase, "_raw.img");
	  meta->general->data_type=COMPLEX_REAL32;
          s=convertMetadata_ceos(infile, outfile, &nl, &readNextPulse);
          iqBuf=(iqType *) MALLOC(sizeof(iqType)*2*(s->nSamp));
          fpOut=FOPEN(outfile, "wb");
          getNextCeosLine(s->binary, s, infile, outfile); /* Skip CEOS header. */
          s->nLines=0;
          for (ii=0; ii<nl; ii++) {
                readNextPulse(s, iqBuf, infile, outfile);
                FWRITE(iqBuf, s->nSamp, 2, fpOut);
                s->nLines++;
          }
	  linkFlag(2, NULL);
          return 0;
	}

	else if (meta->general->data_type==COMPLEX_INTEGER16) { /* complex data */

	  /* Let the user know what format we are working on */
	  if (strcmp(out_type, "")!=0) 
	    sprintf(tmp,"   Input data type: single look complex\n"
			"   Chosen output option not possible\n   Output data type: single look complex\n\n");
	  else
	    sprintf(tmp,"   Input data type: single look complex\n   Output data type: single look complex\n\n");
	  printf(tmp);
	  if (logflag) printLog(tmp);

	  /* Handle output files */ 
	  create_name(outfile, outbase, "_cpx.img");
	  meta->general->data_type=COMPLEX_REAL32;
          meta_write(meta,outfile);

	  /* Take care of image files and memory */
	  fpIn=fopenImage(infile,"rb");
          fpOut=fopenImage(outfile,"wb");
          nl=meta->general->line_count;
          ns=meta->general->sample_count;
          cpx_buf = (short *) MALLOC(2*ns * sizeof(short));
          out_buf = (float *) MALLOC(2*ns * sizeof(float));

	  /* Read single look complex data */
	  get_ifiledr(infile,&image_fdr);
	  headerBytes = firstRecordLen(infile)+(image_fdr.reclen-ns*image_fdr.bytgroup); /* file + line header */
	  for (ii=0; ii<nl; ii++) {
	    offset = headerBytes+ii*image_fdr.reclen;
	    FSEEK(fpIn, offset, 0);
	    FREAD(cpx_buf, sizeof(short), 2*ns, fpIn);
	    for (kk=0; kk<ns*2; kk++)  
	      out_buf[kk]=(float)cpx_buf[kk];
	    put_float_line(fpOut, meta, ii, out_buf);
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
	  if (check_cal(infile)==0 && strcmp(out_type, "power")!=0)
	    sprintf(out_type, "amp");
	  if (meta->projection!=NULL) {
	    if (meta->projection->type!='A' && meta->projection->type!=MAGIC_UNSET_CHAR) {
	      sprintf(out_type, "amp");
	      sprintf(tmp, "   Input data type: level two data\n   Output data type: geocoded amplitude image\n\n");
	      create_name(outfile, outbase, "_geo.img");
	    }
	    else {
	      sprintf(out_type, "amp");
	      sprintf(tmp, "   Input data type: level one data\n   Output data type: amplitude image\n\n");
	      create_name(outfile, outbase, "_amp.img");
	    }
	  }
	  else if (strcmp(out_type, "amp")==0) {
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: amplitude image\n\n");
	    create_name(outfile, outbase, "_amp.img");
	  }
	  else if (strcmp(out_type, "power")==0) {
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: power image\n\n");
	    create_name(outfile, outbase, "_power.img");
	  }
	  else if (strcmp(out_type, "sigma")==0) {
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: calibrated image (sigma dB values)\n\n");
	    create_name(outfile, outbase, "_sigma.img");
	  }
	  else if (strcmp(out_type, "gamma")==0) {
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: calibrated image (gamma dB values)\n\n");
	    create_name(outfile, outbase, "_gamma.img");
	  }
	  else if (strcmp(out_type, "beta")==0) { 
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: calibrated image (beta dB values)\n\n");
	    create_name(outfile, outbase, "_beta.img");
	  }
	  else { /* no output type so far: default is amplitude */
	    sprintf(out_type, "amp");
	    sprintf(tmp, "   Input data type: level one data\n   Output data type: amplitude image\n\n");
	    create_name(outfile, outbase, "_amp.img");
	  }
	  printf(tmp);
	  if (logflag) printLog(tmp);

	  /* Open image files */
	  fpIn=fopenImage(infile,"rb");
          fpOut=fopenImage(outfile,"wb");

	  /* Check number of lines and samples, size of the header */
          nl=meta->general->line_count;
          ns=meta->general->sample_count;
	  get_ifiledr(infile,&image_fdr);
	  headerBytes = firstRecordLen(infile)+(image_fdr.reclen-ns*image_fdr.bytgroup);
        
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
          meta_write(meta,outfile);

	  /* Read calibration parameters if required */
          if (strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0 || strcmp(out_type, "beta")==0) {
	    if (nl<1500) tableRes=128;
	    else if (nl<3000) tableRes=256;
	    tablePix=((ns+(tableRes-1))/tableRes);
	    cal_param=create_cal_params(infile);
            if (cal_param==NULL) sprintf(out_type, "amp");
	    if (strcmp(out_type, "sigma")==0) 
	      cal_param->output_type=sigma_naught;
	    else if (strcmp(out_type, "gamma")==0) 
	      cal_param->output_type=gamma_naught;
	    else if (strcmp(out_type, "beta")==0) 
	      cal_param->output_type=beta_naught;
	    else {
	      sprintf(tmp, "   \nCalibration parameters could not be extracted out of CEOS file\n"
			   "   Output data type: amplitude image\n\n");
	      printf(tmp);
	      if (logflag) printLog(tmp);
	    }
	  }

	  /* Read 16 bit data and convert to calibrated amplitude data */
          if ((strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0 || strcmp(out_type, "beta")==0) &&
		(strcmp(in_type, "int16")==0)) {
	    
	    for (ii=0; ii<nl; ii++) {
	      offset = headerBytes+ii*image_fdr.reclen;
	      FSEEK(fpIn, offset, 0);
	      FREAD(short_buf, sizeof(short), ns, fpIn);

	      if (strcmp(out_type, "sigma")==0)
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
          else if ((strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0 || strcmp(out_type, "beta")==0) &&
		(strcmp(in_type, "byte")==0)) {
	    
	    for (ii=0; ii<nl; ii++) {
	      offset = headerBytes+ii*image_fdr.reclen;
	      FSEEK(fpIn, offset, 0);
	      FREAD(byte_buf, sizeof(char), ns, fpIn);

	      if (strcmp(out_type, "sigma")==0)
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
		if (byte_buf[kk])
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
                if (strcmp(out_type, "power")==0) out_buf[kk]=(float)(short_buf[kk]*short_buf[kk]);
                else out_buf[kk]=(float)short_buf[kk];
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

	/* Clear up and exit */
	StopWatch();
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}

	exit(0);
}


/* usage - enter here on command-line usage error*/
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [ -amplitude ] [ -sigma ] [ -gamma ] [ -beta ] [ -power ]\n"
	"             [-log <file> ] <infile> <outfile>\n", name);
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -amplitude	Creates an amplitude image.\n" 
	"   -sigma	Creates a calibrated image (sigma dB values).\n"
	"   -gamma	Creates a calibrated image (gamma dB values).\n"
	"   -beta	Creates a calibrated image (beta dB values).\n"
	"   -power	Creates a power image.\n"
	"   -log <file>	Allows the output to be written to a log file.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s ingests the complete variety of CEOS data into the internal ASF data.\n",name);
 printf("\n"
	"Version %.2f, ASF SAR TOOLS\n\n", VERSION);
 exit(1);
}

