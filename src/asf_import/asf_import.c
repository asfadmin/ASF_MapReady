/*************************************************************************
<documentation>
<name>
    asf_import
</name>

<synopsis>
asf_import [-amplitude | -sigma | -gamma | -beta | -power] [-lat <lower> <upper>]
           <in_base_name> <out_base_name>
</synopsis>

<description>
    Ingests all varieties of CEOS and STF data formats as well as the external
    ESRI and ENVI data formats and outputs ASF internal format metadata
    and data files.
</description>

<input>
   The format of the input file must be specified as CEOS, STF, ESRI, or ENVI.
   The data file name must be provided seperately from the meta file name.
   The output file provided should only be a base name, by which the created
   files will be named, with appropriate extensions.
</input>

<output>
   Outputs data and metadata files with the user-provided base name and
   appropriate extensions.
</output>

<options>
   -amplitude     Create an amplitude image. This is the default behavior.
   -sigma         Create a calibrated image (sigma dB values).
   -gamma         Create a calibrated image (gamma dB values).
   -beta          Create a calibrated image (beta dB values).
   -power         Create a power image.
   -log           Output will be written to a specified log file.
   -quiet         Supresses all non-essential output.
   -lat           Specify lower and upper latitude contraints.
   -old           Output in old style ASF internal format.
   -prc           Replace the restituted state vectors from the original raw
                  data acquired by the ERS satellites with preceision state
                  vectors from DLR.
</options>

<examples>
   asf_import CEOS file1.img file1.meta file2
</examples>

<limitations>
   None.
</limitations>

<see_also>
   asf_convert, asf_export
</see_also>

<copyright>
*******************************************************************************
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
*******************************************************************************
</copyright>
</documentation>

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    0.1    12/03   R. Gens     Combined sarin, calibrate, trim_slc and
                                ceos2raw into one program
    0.2     4/04   P. Denny    Allowed for RSI naming scheme as well as
                                our typical CEOS naming scheme
    0.3     5/04   R. Gens     Added stf2raw and external ESRI and ENVI
                                formats; renamed the tool to import2asf
    0.4     5/04   J. Nicoll   Fixed sign issue when converting 8 bit
                                data to calibrated amplitude data
    0.41    5/04   P. Denny    Made format of input data a required
                                argument (CEOS,STF,ESRI,ENVI)
    0.5     5/04   P. Denny    Added hidden option to write out sprocket
                                style metadata.

*******************************************************************************/


#include "asf.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "calibrate.h"
#include "ceos.h"
#include "ceos_io.h"
#include "dateUtil.h"
#include "decoder.h"
#include "envi.h"
#include "esri.h"
#include "lzFetch.h"
#include <ctype.h>
#include <string.h>

#define VERSION 0.5
#define MAX_tableRes 512
#define REQUIRED_ARGS 4

/* PROTOTYPES */
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
bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,
                                readPulseFunc *readNextPulse);
/* in sprocket_layers.c */
void create_sprocket_layers(const char *asfName, const char *importName);


/* Helpful functions */
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

void print_progress(int current_line, int total_lines)
{
  current_line++;

  if ((current_line%256==0) || (current_line==total_lines)) {
    printf("\rWrote %5d of %5d lines.", current_line, total_lines);
    fflush(NULL);
    if (current_line == total_lines) {
      printf("\n");
      if (logflag) {
        sprintf(logbuf,"Wrote %5d of %5d lines.", current_line, total_lines);
        printLog(logbuf);
      }
    }
  }
}


/* Global variables - needed to handle missing lines properly */
long imgStart, imgEnd;
int outLine;
int oldFlag=FALSE;


/* Lets go! */
int main(int argc, char *argv[])
{
  struct IOF_VFDR image_fdr;
  meta_parameters *meta;
  cal_params *cal_param=NULL;
  bin_state *s;
  readPulseFunc readNextPulse;
  iqType *iqBuf;
  esri_header *esri=NULL;
  envi_header *envi=NULL;
  FILE *fpIn=NULL, *fpOut=NULL, *fp;
  char in_type[25]="", out_type[25]="";
  char inDataName[256], inMetaName[256], outName[288], prcPath[255];
  char outBaseName[256], sprocketName[256];
  char type[255]="", tmp[255], imgTimeStr[20]="";
  char *map_info_ptr=NULL, *proj_info_ptr, proj_info[255]="", map_info[255]="";
  double fTmp1, fTmp2;
  int projection_key;
  int nl, ns=0, ii, kk, tableRes=MAX_tableRes, tablePix=0, headerBytes;
  int latConstraintFlag=FALSE, prcflag=FALSE, nTotal, nVec=1;
  int sprocketFlag=FALSE;
  int sigmaFlag=FALSE, betaFlag=FALSE, gammaFlag=FALSE, powerFlag=FALSE;
  long offset;
  unsigned short *short_buf=NULL;
  short *cpx_buf=NULL;
  unsigned char *byte_buf=NULL;
  float *out_buf=NULL;
  complexFloat *out_cpx_buf;
  float fd, fdd, fddd, lowerLat=NAN, upperLat=NAN;
  double noise_table[MAX_tableRes];
  double incid_cos[MAX_tableRes], incid_sin[MAX_tableRes];
  extern int currArg; /* from cla.h in asf.h */

  logflag=FALSE;
  quietflag=FALSE;

  /* Parse command line args */
  while (currArg < (argc-REQUIRED_ARGS)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-sprocket")) {
      sprocketFlag=TRUE;
    }
    else if (strmatch(key,"-old")) {
      oldFlag=TRUE;
    }
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
    else if (strmatch(key,"-sigma")) {
      sprintf(out_type, "sigma");  /* create calibrated image (sigma dB values) */
      sigmaFlag=TRUE;
    }
    else if (strmatch(key,"-gamma")) {
      sprintf(out_type, "gamma");  /* create calibrated image (gamma dB values) */
      gammaFlag=TRUE;
    }
    else if (strmatch(key,"-beta")) {
      sprintf(out_type, "beta");  /* create calibrated image (beta dB values) */
      betaFlag=TRUE;
    }
    else if (strmatch(key,"-power")) {
      sprintf(out_type, "power");  /* create power image */
      powerFlag=TRUE;
    }
    else {
      printf("\n** Invalid option:  %s\n\n",argv[currArg-1]);
      usage();
    }
  }
  if ((argc-currArg) < REQUIRED_ARGS) {
    printf("Insufficient arguments.\n");
    usage();
  }

  /* Make sure the sprocket flag hasn't been declared with a calibration or
   * power flag */
  if (sprocketFlag && (sigmaFlag||gammaFlag||betaFlag||powerFlag)) {
    printf(" * Silly calibration engineer, you can't declare -sigma, -beta, -gamma,\n"
           " * or -power with the all powerful -sprocket option!  Try again. Exiting...\n");
    exit(EXIT_FAILURE);
  }

  /* Read required arguments */
  strcpy(type,argv[currArg++]);
  for (ii=0; ii<strlen(type); ii++) {
    type[ii] = (char)toupper(type[ii]);
  }
  strcpy(inDataName,argv[currArg++]);
  strcpy(inMetaName,argv[currArg++]);
  strcpy(outBaseName,argv[currArg++]);

  /* Lets get started */
  system("date");
  printf("Program: asf_import\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: asf_import\n\n");
  }

  /* Check whether options are chosen correctly */
  if (strncmp(type, "STF", 3)!=0) {
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
  if (strncmp(type, "CEOS", 4)==0) {

    sprintf(tmp,"   Data format: CEOS\n");
    printf(tmp);
    if (logflag) printLog(tmp);

    /* Create metadata */
    meta=meta_create(inMetaName);

    /* Check out input data type */
    if (meta->general->data_type==COMPLEX_BYTE) { /* raw data */
      int trash;

      if (sprocketFlag) {
        printf("Data is level 0, SProCKET can not use it. Exiting...\n");
        exit(EXIT_FAILURE);
      }

      /* Let the user know what format we are working on */
      if (strcmp(out_type, "")!=0)
        sprintf(tmp,
                "   Input data type: level zero raw data\n"
                "   Chosen output option not possible\n"
                "   Output data type: complex byte raw data\n\n");
      else
        sprintf(tmp,
               "   Input data type: level zero raw data\n"
                "   Output data type: complex byte raw data\n\n");
      printf(tmp);
      if (logflag) printLog(tmp);

      /* Handle output files */
      create_name(outName, outBaseName, "_raw.img");
      nl = meta->general->line_count;
      s = convertMetadata_ceos(inMetaName, outName, &trash, &readNextPulse);
      iqBuf = (iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
      fpOut = FOPEN(outName, "wb");
      getNextCeosLine(s->binary, s, inMetaName, outName); /* Skip CEOS header. */
      s->nLines = 0;
      printf("\n");
      for (ii=0; ii<nl; ii++) {
        readNextPulse(s, iqBuf, inDataName, outName);
        FWRITE(iqBuf, s->nSamp*2, 1, fpOut);
        print_progress(ii,nl);
       s->nLines++;
      }
      updateMeta(s,meta,NULL,0);
      if (oldFlag) {
        meta_new2old(meta);
        meta_write_old(meta, outName);
      }
      else meta_write(meta,outName);

      /* Write .raw file for backwards compatibility */
      {
        char img[256], raw[256];
        sprintf(img,"%s_raw.img",outBaseName);
        sprintf(raw,"%s_raw.raw",outBaseName);
        link(img,raw);
      }

      meta_free(meta);
      FCLOSE(fpOut);
      printf("Finished.\n\n");
    }

    /* complex (level 1) data */
    else if (meta->general->data_type==COMPLEX_INTEGER16) {

      /* Let the user know what format we are working on */
      if (strcmp(out_type, "")!=0)
        sprintf(tmp,
                "   Input data type: single look complex\n"
                "   Chosen output option not possible\n"
                "   Output data type: single look complex\n\n");
      else
        sprintf(tmp,
                "   Input data type: single look complex\n"
                "   Output data type: single look complex\n\n");
      printf(tmp);
      if (logflag) printLog(tmp);

      /* Handle output files */
      create_name(outName, outBaseName, "_cpx.img");
      meta->general->data_type=COMPLEX_REAL32;

      if (oldFlag) {
        char ddrName[256];
        struct DDR ddr;
        create_name(ddrName, outBaseName, "_cpx.ddr");
        meta2ddr(meta, &ddr);
        c_putddr(ddrName, &ddr);
      }
      else meta_write(meta,outName);

      /* Take care of image files and memory */
      fpIn  = fopenImage(inDataName,"rb");
      fpOut = fopenImage(outName,"wb");
      nl = meta->general->line_count;
      ns = meta->general->sample_count;
      cpx_buf = (short *) MALLOC(2*ns * sizeof(short));
      out_cpx_buf = (complexFloat *) MALLOC(ns * sizeof(complexFloat));

      /* Read single look complex data */
      get_ifiledr(inDataName,&image_fdr);
      /* file + line header */
      headerBytes = firstRecordLen(inDataName)
                    + (image_fdr.reclen - ns * image_fdr.bytgroup);
      for (ii=0; ii<nl; ii++) {
        offset = headerBytes+ii*image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);
        FREAD(cpx_buf, sizeof(short), 2*ns, fpIn);
        for (kk=0; kk<ns; kk++) {
          /* Put read in data in proper endian format */
          big16(cpx_buf[kk*2]);
          big16(cpx_buf[kk*2+1]);
          /* Now do our stuff */
          out_cpx_buf[kk].real=(float)cpx_buf[kk*2];
          out_cpx_buf[kk].imag=(float)cpx_buf[kk*2+1];
        }
        put_complexFloat_line(fpOut, meta, ii, out_cpx_buf);
        print_progress(ii,nl);
      }
      FCLOSE(fpOut);
      printf("Finished.\n\n");
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
        short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
        out_buf = (float *) MALLOC(ns * sizeof(float));
      }
      /* Allocate memory for 8 bit amplitude data */
      else if (meta->general->data_type==BYTE) { /* 8 bit amplitude data */
        sprintf(in_type, "byte");
        byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
        out_buf = (float *) MALLOC(ns * sizeof(float));
      }
      else
        printErr("   Error: Unkown data format\n");

      /* Handle output files */
      meta->general->data_type=REAL32;

      if(oldFlag) {
        char ddrName[256];
        struct DDR ddr;
        meta2ddr(meta, &ddr);
        c_putddr(ddrName, &ddr);
      }
      else meta_write(meta,outName);

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
          /* Can't use get_float_line() for CEOS data, so we have to use FSEEK,
           * FREAD, and then put the bytes in proper endian order manually  */
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
          for (kk=0; kk<ns; kk++) {
            big16(short_buf[kk]);
          }

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
            if (short_buf[kk]) {
            /*Interpolate noise table to find this pixel's noise.*/
                double index=(float)kk/tablePix;
                int    base=(int)index;
                double frac=index-base;
                double noise=noise_table[base]+frac*(noise_table[base+1]-noise_table[base]);
                double incid=1.0;
                if (cal_param->output_type==gamma_naught)
                  incid=incid_cos[base]+frac*(incid_cos[base+1]-incid_cos[base]);
                if (cal_param->output_type==beta_naught)
                 incid=incid_sin[base]+frac*(incid_sin[base+1]-incid_sin[base]);
                out_buf[kk]=get_cal_dn(cal_param,noise,incid,(int)short_buf[kk]);
            }
            else
              out_buf[kk]=0;
          }

          put_float_line(fpOut, meta, ii, out_buf);

          print_progress(ii,nl);
        }
        FCLOSE(fpOut);
        printf("Finished.\n\n");
      }

      /* Read 8 bit data and convert to calibrated amplitude data */
      else if ((strcmp(out_type, "sigma")==0 || strcmp(out_type, "gamma")==0
                || strcmp(out_type, "beta")==0) && (strcmp(in_type, "byte")==0)) {

        for (ii=0; ii<nl; ii++) {
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

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
              double incid=1.0;
              if (cal_param->output_type==gamma_naught)
                incid=incid_cos[base]+frac*(incid_cos[base+1]-incid_cos[base]);
              if (cal_param->output_type==beta_naught)
                incid=incid_sin[base]+frac*(incid_sin[base+1]-incid_sin[base]);
              out_buf[kk]=get_cal_dn(cal_param,noise,incid,(int)byte_buf[kk]);
            }
            else
              out_buf[kk]=0;
          }

          put_float_line(fpOut, meta, ii, out_buf);

          print_progress(ii,nl);
        }
        FCLOSE(fpOut);
        printf("Finished.\n\n");
      }

      /* Read 16 bit amplitude data */
      else if (strcmp(in_type, "int16")==0) {

        for (ii=0; ii<nl; ii++) {
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
          for (kk=0; kk<ns; kk++) {
            /* Put the data in proper endian order before we do anything */
            big16(short_buf[kk]);
            /* Now do our stuff */
            if (strcmp(out_type, "power")==0)
              out_buf[kk]=(float)(short_buf[kk]*short_buf[kk]);
            else
              out_buf[kk]=(float)short_buf[kk];
          }

          put_float_line(fpOut, meta, ii, out_buf);

          print_progress(ii,nl);
        }
        FCLOSE(fpOut);
        printf("Finished.\n\n");
      }

      /* Read 8 bit amplitde data */
      else {

        for (ii=0; ii<nl; ii++) {
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

          for (kk=0; kk<ns; kk++) {
            if (strcmp(out_type, "power")==0)
              out_buf[kk]=(float)(byte_buf[kk]*byte_buf[kk]);
            else
              out_buf[kk]=(float)byte_buf[kk];
          }

          put_float_line(fpOut, meta, ii, out_buf);

          print_progress(ii,nl);
        }
        FCLOSE(fpOut);
        printf("Finished.\n\n");
      }
    }
  }

  /* Ingest Vexcel Sky Telemetry Format (STF) data */
  else if (strncmp(type, "STF", 4)==0) {

    if (sprocketFlag) {
      printf("Data is level 0, sprocket can not use this. Exiting...\n");
      exit(EXIT_FAILURE);
    }

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
        print_progress(outLine,nTotal);
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
  }

  /* Ingest ESRI format data */
  else if (strncmp(type, "ESRI", 4)==0) {
    char line[255]="", key[25]="", value[25]="";

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
    meta_write(meta,outName);
if (sprocketFlag) {
  create_name(sprocketName, outName, ".metadata");
  meta_write_sprocket(sprocketName, meta, NULL);
}

    /* Write data file - currently no header, so just copying generic binary */
    fileCopy(inDataName, outName);

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
  else if (strncmp(type, "ENVI", 4)==0) {
    char line[255]="", key[25]="", value[25]="", bla[25];

    sprintf(tmp,"   Data format: ENVI\n");
    printf(tmp);
    if (logflag) printLog(tmp);

    /* Handle output file name */
    create_name(outName, outBaseName, "_amp.img");

    /* Allocate memory for ESRI header structure */
    envi = (envi_header *)MALLOC(sizeof(envi_header));

    /* Read .hdr and fill meta structures */
    fp = FOPEN(inMetaName, "r");
    while (NULL != fgets(line, 255, fp)) {
      sscanf(line, "%s = %s", key, value);
      if (strncmp(key, "samples", 6)==0) envi->samples = atoi(value);
      else if (strncmp(key, "lines", 5)==0) envi->lines = atoi(value);
      else if (strncmp(key, "bands", 5)==0) envi->bands = atoi(value);
      else if (strncmp(key, "header", 6)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "offset", 6)==0)
          envi->header_offset = atoi(value);
      }
      /*** ignore file type for the moment ***/
      else if (strncmp(key, "data", 4)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "type", 4)==0)
          envi->data_type = atoi(value);
      }
      else if (strncmp(key, "interleave", 10)==0)
        sprintf(envi->interleave, "%s", value);
      else if (strncmp(key, "sensor", 6)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "type", 4)==0)
          sprintf(envi->sensor_type, "%s", value);
      }
      else if (strncmp(key, "byte", 4)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "order", 5)==0)
          envi->byte_order = atoi(value);
      }
      else if (strncmp(key, "map", 3)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "info", 4)==0) {
          map_info_ptr = strstr(line, ",");
          sprintf(map_info, "%s", map_info_ptr);
        }
      }
      else if (strncmp(key, "projection", 10)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "info", 4)==0) {
          proj_info_ptr = strstr(line, ",");
          sprintf(proj_info, "%s", proj_info_ptr);
          sscanf(value, "{%i,", &projection_key);
        }
      }
      else if (strncmp(key, "wavelength", 10)==0) {
        sscanf(line, "%s %s = %s", bla, key, value);
        if (strncmp(key, "units", 5)==0)
          sprintf(envi->wavelength_units, "%s", value);
      }
      /*** ignore wavelength for the moment ***/
      /*** ignore data ignore for the moment ***/
      /*** ignore default stretch for the moment ***/
    }
    FCLOSE(fp);

    switch(projection_key)
      {
      case 3:
        sprintf(envi->projection, "UTM");
        sscanf(map_info, ", %i %i %lf %lf %lf %lf %i %s",
               &envi->ref_pixel_x, &envi->ref_pixel_y,
               &envi->pixel_easting, &envi->pixel_northing,
               &envi->proj_dist_x, &envi->proj_dist_y,
               &envi->projection_zone, envi->hemisphere);
        sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
               &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
               &envi->center_lon, bla);
        break;
      case 4:
        sprintf(envi->projection, "Lambert Conformal Conic");
        sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
               &envi->ref_pixel_x, &envi->ref_pixel_y,
               &envi->pixel_easting, &envi->pixel_northing,
               &envi->proj_dist_x, &envi->proj_dist_y,
               envi->hemisphere);
        sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s}",
               &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
               &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
               &envi->standard_parallel2, bla);
        break;
      case 9:
        sprintf(envi->projection, "Albers Conical Equal Area");
        sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
               &envi->ref_pixel_x, &envi->ref_pixel_y,
               &envi->pixel_easting, &envi->pixel_northing,
               &envi->proj_dist_x, &envi->proj_dist_y,
               envi->hemisphere);
        sscanf(proj_info, ", %lf, %lf, %lf, %lf, %lf %lf %lf %lf%s}",
               &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
               &envi->center_lon, &fTmp1, &fTmp2, &envi->standard_parallel1,
               &envi->standard_parallel2, bla);
        break;
      case 11:
        sprintf(envi->projection, "Lambert Azimuthal Equal Area");
        sscanf(map_info, ", %i, %i, %lf, %lf, %lf, %lf, %s}",
               &envi->ref_pixel_x, &envi->ref_pixel_y,
               &envi->pixel_easting, &envi->pixel_northing,
               &envi->proj_dist_x, &envi->proj_dist_y,
               envi->hemisphere);
        sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
               &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
               &envi->center_lon, bla);
        break;
      case 31:
        sprintf(envi->projection, "Polar Stereographic");
        sscanf(map_info, ", %d, %d, %lf, %lf, %lf, %lf, %s}",
               &envi->ref_pixel_x, &envi->ref_pixel_y,
               &envi->pixel_easting, &envi->pixel_northing,
               &envi->proj_dist_x, &envi->proj_dist_y,
               envi->hemisphere);
        sscanf(proj_info, ", %lf, %lf, %lf, %lf, %s}",
               &envi->semimajor_axis, &envi->semiminor_axis, &envi->center_lat,
               &envi->center_lon, bla);
        break;
      default:
        sprintf(errbuf, "\n   ERROR: unsupported map projection\n\n");
        printErr(errbuf);
        break;
      }

    /* Fill metadata structure with valid data */
    meta = envi2meta(envi);

    /* Write metadata file */
    meta_write(meta,outName);
if (sprocketFlag) {
  create_name(sprocketName, outName, ".metadata");
  meta_write_sprocket(sprocketName, meta, NULL);
}


    /* Write data file - currently no header, so just copying generic binary */
    fileCopy(inDataName, outName);

    /* Clean and report */
    meta_free(meta);
    sprintf(logbuf, "   Converted ENVI file (%s) to ASF internal file (%s)\n\n",
            inDataName, outName);
    printf(logbuf);
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      printLog(logbuf);
      FCLOSE(fLog);
    }
  }

  else {
        sprintf(tmp,"Unrecognized data format: '%s'\n\n",type);
        printf(tmp);
        if (logflag) printLog(tmp);
        usage();
  }

  if (sprocketFlag) {
    create_sprocket_layers(outName, inMetaName);
  }

  exit(EXIT_SUCCESS);
}


/* usage - enter here on command-line usage error*/
void usage()
{

 printf("\n"
	"USAGE:\n"
	"   asf_import [-amplitude | -sigma | -gamma | -beta | -power] [-lat <lower> <upper>]\n"
	"              <in_base_name> <out_base_name>\n");
 exit(EXIT_FAILURE);
}
