#include "asf_import.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "calibrate.h"
#include "decoder.h"

#define MAX_tableRes 512

/* Prototypes from the meta library */
void meta_new2old(meta_parameters *meta);
void meta_write_old(meta_parameters *meta, const char *file_name);

/* Prototype in ceos2raw.c */
bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,
                                readPulseFunc *readNextPulse);

/******************************************************************************
 * Import a wide variety for CEOS flavors (hopefully all) to our very own ASF
 * Tools format */
void import_ceos(char *inDataName, char *inMetaName, char *lutName,
                 char *outBaseName, flag_indices_t flags[])
{
  char outDataName[256], outMetaName[256];              /* Output file names */
  int nl=MAGIC_UNSET_INT, ns=MAGIC_UNSET_INT;     /* Number of lines/samples */
  int ii, kk;                                                /* loop indices */
  int headerBytes;                       /* Number of bytes in a CEOS header */
  long offset;
  meta_parameters *meta=NULL;                         /* Meta data structure */
  cal_params *cal_param=NULL;                  /* Calibration info structure */
  struct IOF_VFDR image_fdr;                  /* CEOS File Descriptor Record */
  data_type_t ceos_data_type;                     /* Data type for CEOS File */
  FILE *fpIn=NULL, *fpOut=NULL;              /* In/output data file pointers */


  /* Fill output names (don't add extention to data name because it differs
   * for raw, complex, and 'image' */
  strcpy(outDataName, outBaseName);
  strcpy(outMetaName, outBaseName);
  strcat(outMetaName, TOOLS_META_EXT);

  /* Create metadata */
  meta=meta_create(inMetaName);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;

  /************************* BEGIN RAW DATA SECTION **************************/
  if (meta->general->data_type==COMPLEX_BYTE) {
    int trash;
    bin_state *s;  /* Structure with info about the satellite & its raw data */
    readPulseFunc readNextPulse; /* Pointer to function that reads the next line of CEOS Data */
    iqType *iqBuf;           /* Buffer containing the complex i & q channels */

    /* Die if the sprocket flag is specified, since it doesn't do lvl 0 */
    if (flags[f_SPROCKET] != FLAG_NOT_SET) {
      print_error("Data is level 0, SProCKET can not use it.");
      exit(EXIT_FAILURE);
    }

    /* Let the user know what format we are working on */
    strcpy(logbuf,
           "   Input data type: level zero raw data\n"
           "   Output data type: complex byte raw data\n");
    if (flags[f_QUIET] == FLAG_NOT_SET)
      printf(logbuf);
    printLog(logbuf);

    /* Make sure that none of the level one flags are set */
    strcpy(logbuf,"");
    if (flags[f_AMP] != FLAG_NOT_SET)
      sprintf(logbuf, "%s amplitude", logbuf);
    if (flags[f_SIGMA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s sigma", logbuf);
    if (flags[f_BETA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s beta", logbuf);
    if (flags[f_GAMMA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s gamma", logbuf);
    if (flags[f_POWER] != FLAG_NOT_SET)
      sprintf(logbuf, "%s power", logbuf);
    sprintf(logbuf,
            "Warning:\n"
            "  The following flags will be ignored since this is a level zero data set:\n"
            "  %s\n\n", logbuf);
    if (flags[f_QUIET] == FLAG_NOT_SET)
      printf(logbuf);
    printLog(logbuf);

    /* Handle output files */
    strcat(outDataName,TOOLS_RAW_EXT);
    meta->general->image_data_type = RAW_IMAGE;
    s = convertMetadata_ceos(inMetaName, outMetaName, &trash, &readNextPulse);
    iqBuf = (iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
    fpOut = FOPEN(outDataName, "wb");
    getNextCeosLine(s->binary, s, inMetaName, outDataName); /* Skip CEOS header. */
    s->nLines = 0;
    if(flags[f_QUIET] == FLAG_NOT_SET) printf("\n");
    for (ii=0; ii<nl; ii++) {
      readNextPulse(s, iqBuf, inDataName, outDataName); /* I think outDataName is just a place holder... at least for now */
      FWRITE(iqBuf, s->nSamp*2, 1, fpOut);
      line_meter(ii,nl);
     s->nLines++;
    }
    updateMeta(s,meta,NULL,0);
  }
  /************************** END RAW DATA SECTION ***************************/

  /****************** BEGIN COMPLEX (level 1) DATA SECTION *******************/
  else if (meta->general->data_type==COMPLEX_INTEGER16) {
    short *cpx_buf=NULL;
    complexFloat *out_cpx_buf;

    /* Let the user know what format we are working on */
    strcpy(logbuf,
           "   Input data type: single look complex\n"
           "   Output data type: single look complex\n");
    if (flags[f_QUIET] == FLAG_NOT_SET)
      printf(logbuf);
    printLog(logbuf);

    /* Make sure that none of the level one flags are set */
    strcpy(logbuf,"");
    if (flags[f_AMP] != FLAG_NOT_SET)
      sprintf(logbuf, "%s amplitude", logbuf);
    if (flags[f_SIGMA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s sigma", logbuf);
    if (flags[f_BETA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s beta", logbuf);
    if (flags[f_GAMMA] != FLAG_NOT_SET)
      sprintf(logbuf, "%s gamma", logbuf);
    if (flags[f_POWER] != FLAG_NOT_SET)
      sprintf(logbuf, "%s power", logbuf);
    sprintf(logbuf,
            "Warning:\n"
            "  The following flags will be ignored since this is a complex data set:\n"
            "  %s\n", logbuf);
    if (flags[f_QUIET] == FLAG_NOT_SET)
      printf(logbuf);
    printLog(logbuf);

    /* Deal with metadata */
    meta->general->data_type=COMPLEX_REAL32;
    meta->general->image_data_type=COMPLEX_IMAGE;

    /* Take care of image files and memory */
    strcat(outDataName,TOOLS_COMPLEX_EXT);
    fpIn  = fopenImage(inDataName,"rb");
    fpOut = fopenImage(outDataName,"wb");
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
      line_meter(ii,nl);
    }
  }
  /******************* END COMPLEX (level 1) DATA SECTION ********************/

  /*********************** BEGIN DETECTED DATA SECTION ***********************/
  else {
    unsigned short *short_buf=NULL;
    unsigned char *byte_buf=NULL;
    float *out_buf=NULL;

    strcat(outDataName,TOOLS_IMAGE_EXT);

    if (check_cal(inMetaName)==0 &&
        ( (flags[f_SIGMA]!=FLAG_NOT_SET) || (flags[f_SIGMA]!=FLAG_NOT_SET) ||
          (flags[f_SIGMA]!=FLAG_NOT_SET) ) ) {
      print_error("Unable to find calibration parameters in the metadata.\n");
    }

    /* Let the user know what format we are working on */
    if (meta->projection!=NULL && meta->projection->type!=MAGIC_UNSET_CHAR) {
      /* This must be ScanSAR */
      if (meta->projection->type!=SCANSAR_PROJECTION) {
        /* This is actually geocoded */
        sprintf(logbuf,
                "   Input data type: level two data\n"
                "   Output data type: geocoded amplitude image\n\n");
      }
    }
    else if (flags[f_AMP] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: amplitude image\n\n");
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    }
    else if (flags[f_POWER] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: power image\n\n");
      meta->general->image_data_type = POWER_IMAGE;
    }
    else if (flags[f_SIGMA] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (sigma dB values)\n\n");
      meta->general->image_data_type = SIGMA_IMAGE;
    }
    else if (flags[f_GAMMA] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (gamma dB values)\n\n");
      meta->general->image_data_type = GAMMA_IMAGE;
    }
    else if (flags[f_BETA] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (beta dB values)\n\n");
      meta->general->image_data_type = BETA_IMAGE;
    }
    else if (flags[f_LUT] != FLAG_NOT_SET) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: user defined LUT image\n\n");
      meta->general->image_data_type = LUT_IMAGE;
    }
    else { /* No chosen output type: default to amplitude */
      flags[f_AMP] = FLAG_SET;
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: amplitude image\n\n");
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    }
    if(flags[f_QUIET] == FLAG_NOT_SET) printf(logbuf);
    printLog(logbuf);

    /* Open image files */
    fpIn=fopenImage(inDataName,"rb");
    fpOut=fopenImage(outDataName,"wb");

    /* Check size of the header */
    get_ifiledr(inDataName,&image_fdr);
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - ns * image_fdr.bytgroup);

    /* Allocate memory for 16 bit amplitude data */
    if (meta->general->data_type==INTEGER16) { /* 16 bit amplitude data */
      ceos_data_type = INTEGER16;
      short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
      out_buf = (float *) MALLOC(ns * sizeof(float));
    }
    /* Allocate memory for 8 bit amplitude data */
    else if (meta->general->data_type==BYTE) { /* 8 bit amplitude data */
      ceos_data_type = BYTE;
      byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
      out_buf = (float *) MALLOC(ns * sizeof(float));
    }
    else
      print_error("Unkown CEOS data format");

    meta->general->data_type=REAL32;



  /**** Take care of LUT images ****/
    if (flags[f_LUT]!=FLAG_NOT_SET) {
      FILE *fpLut;
      double incid_table[MAX_tableRes];                  /* Incidence angle */
      double scale_table[MAX_tableRes];                   /* Scaling factor */
      double incid[MAX_tableRes], old, new;
      char line[255];
      int nLut=0, n, tableRes=MAX_tableRes, tablePix=0, ll;

      /**** Read look up table ****/
      fpLut = FOPEN(lutName, "r");
      for (ii=0; ii<MAX_tableRes; ii++) {
	incid_table[ii] = 0.0;
	scale_table[ii] = 0.0;
      }
      while(fgets(line, 255, fpLut)) {
	sscanf(line, "%lf\t%lf", &incid_table[nLut], &scale_table[nLut]);
	nLut++;
      }

      /**** Read 16 bit data and apply look up table ****/
      if (ceos_data_type == INTEGER16) {

	if (nl<1500) tableRes=128;
	else if (nl<3000) tableRes=256;
	tablePix=((ns+(tableRes-1))/tableRes);

        for (ii=0; ii<nl; ii++) {
          /* Can't use get_float_line() for CEOS data, so we have to use FSEEK,
           * FREAD, and then put the bytes in proper endian order manually  */
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(short_buf, sizeof(unsigned short), ns, fpIn);

	  /* Allocate incidence table entries or update */
	  if (ii==0 || (ii%(nl/tableRes)==0 && meta->projection != NULL))
	    for (kk=0;kk<tableRes;kk++)
	      incid[kk] = meta_incid(meta, kk*tablePix, ii);

	  /* Calculate output values */
          for (kk=0; kk<ns; kk++) {
            if (short_buf[kk]) {
	      big16(short_buf[kk]);
	      old = 10000000;
	      for (ll=0; ll<nLut; ll++) {
		new = incid[kk]*R2D - incid_table[ll];
		if (fabs(new) < fabs(old)) {
		  old = new;
		  n++;
		}
		else break;
	      }
	      out_buf[kk] = short_buf[kk] * scale_table[n];
	    }
	    else
	      out_buf[kk] = 0;
	  }
          put_float_line(fpOut, meta, ii, out_buf);
          line_meter(ii,nl);
	}

      } /**** End processing of 16 bit input data ****/

      /**** Read 8 bit data and apply look up table ****/
      else if (ceos_data_type == BYTE) {

	if (nl<1500) tableRes=128;
	else if (nl<3000) tableRes=256;
	tablePix=((ns+(tableRes-1))/tableRes);

        for (ii=0; ii<nl; ii++) {
	  /* Read image line */
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

	  /* Allocate incidence table entries or update */
	  if (ii==0 || (ii%(nl/tableRes)==0 && meta->projection != NULL))
	    for (kk=0;kk<tableRes;kk++)
	      incid[kk] = meta_incid(meta, ii, kk*tablePix);

	  /* Calculate output values */
          for (kk=0; kk<ns; kk++) {
            if (byte_buf[kk]) {
              /* Interpolate incidence */
              double index=(float)kk/tablePix;
              int    base=(int)index;
              double frac=index-base;

	      old = 10000000;
	      n = 0;
	      for (ll=0; ll<nLut; ll++) {
		new = (incid[base]+frac*(incid[base+1]-incid[base]))*R2D
		       - incid_table[ll];
		if (fabs(new) < fabs(old)) {
		  old = new;
		  n++;
		}
		else break;
	      }
	      out_buf[kk] = byte_buf[kk] * scale_table[n];
	    }
	    else
	      out_buf[kk] = 0;
	  }
          put_float_line(fpOut, meta, ii, out_buf);
          line_meter(ii,nl);
	}

      } /**** End processing of byte input data ****/
    } /**** End LUT output section ****/



  /**** Create calibrated (sigma, gamma, or beta naught) dB image ****/
    else if ( (flags[f_SIGMA]!=FLAG_NOT_SET) || (flags[f_GAMMA]!=FLAG_NOT_SET)
	      || (flags[f_BETA]!=FLAG_NOT_SET) ) {
      int tableRes=MAX_tableRes, tablePix=0;        /* Calibration variables */
      double noise_table[MAX_tableRes];       /* Noise table for calibration */
      double incid_cos[MAX_tableRes];       /* Cosine of the incidence angle */
      double incid_sin[MAX_tableRes];         /* Sine of the incidence angle */

      /* Read calibration parameters if required */
      if (nl<1500) tableRes=128;
      else if (nl<3000) tableRes=256;
      tablePix=((ns+(tableRes-1))/tableRes);
      cal_param=create_cal_params(inMetaName);
      if (cal_param==NULL) /* Die if we can't get the calibration params */
        print_error("Unable to extract calibration parameters from CEOS file.");
      if (flags[f_SIGMA]!=FLAG_NOT_SET)
        cal_param->output_type=sigma_naught;
      else if (flags[f_GAMMA]!=FLAG_NOT_SET)
        cal_param->output_type=gamma_naught;
      else if (flags[f_BETA]!=FLAG_NOT_SET)
        cal_param->output_type=beta_naught;

    /**** Read 16 bit data and convert to calibrated amplitude data ****/
      if (ceos_data_type == INTEGER16) {

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
          if (flags[f_GAMMA]!=FLAG_NOT_SET)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
          if (flags[f_BETA]!=FLAG_NOT_SET)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_sin[kk]=get_invSinIncAngle(cal_param,kk*tablePix,ii);

          for (kk=0; kk<ns; kk++) {
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

          put_float_line(fpOut, meta, ii, out_buf);

          line_meter(ii,nl);
        }
      } /**** End processing of 16 bit input data ****/

    /**** Read 8 bit data and convert to calibrated amplitude data ****/
      else if (ceos_data_type == BYTE) {

        for (ii=0; ii<nl; ii++) {
          offset = headerBytes+ii*image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

          /*Allocate noise table entries and/or update if needed.*/
          if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
            for (kk=0;kk<tableRes;kk++)
              noise_table[kk]=get_noise(cal_param,kk*tablePix,ii);
          if (flags[f_GAMMA]!=FLAG_NOT_SET)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
          if (flags[f_BETA]!=FLAG_NOT_SET)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_sin[kk]=get_invSinIncAngle(cal_param,kk*tablePix,ii);

          for (kk=0; kk<ns; kk++) {
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

          put_float_line(fpOut, meta, ii, out_buf);

          line_meter(ii,nl);
        }
      } /**** End processing of byte input data ****/
    } /**** End calibrated output section ****/

  /**** Read 16 bit amplitude data ****/
    else if (ceos_data_type == INTEGER16) {

      for (ii=0; ii<nl; ii++) {
        offset = headerBytes+ii*image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);
        FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
        for (kk=0; kk<ns; kk++) {
          /* Put the data in proper endian order before we do anything */
          big16(short_buf[kk]);
          /* Now do our stuff */
          if (flags[f_POWER]!=FLAG_NOT_SET)
            out_buf[kk]=(float)(short_buf[kk]*short_buf[kk]);
          else
            out_buf[kk]=(float)short_buf[kk];
        }

        put_float_line(fpOut, meta, ii, out_buf);

        line_meter(ii,nl);
      }
    }

  /**** Read 8 bit amplitde data ****/
    else {

      for (ii=0; ii<nl; ii++) {
        offset = headerBytes+ii*image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);
        FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

        for (kk=0; kk<ns; kk++) {
          if (flags[f_POWER] != FLAG_NOT_SET)
            out_buf[kk]=(float)(byte_buf[kk]*byte_buf[kk]);
          else
            out_buf[kk]=(float)byte_buf[kk];
        }

        put_float_line(fpOut, meta, ii, out_buf);

        line_meter(ii,nl);
      }
    }
  }
  /************************ END DETECTED DATA SECTION ************************/

  /* Now that the data file is written, write the metadata */
  if(flags[f_OLD_META] != FLAG_NOT_SET) {
    char ddrName[256];
    struct DDR ddr;
		strcpy(ddrName, outBaseName);
		strcat(ddrName, ".ddr");
    meta2ddr(meta, &ddr);
    c_putddr(ddrName, &ddr);
    meta_new2old(meta);
    meta_write_old(meta, outMetaName);
  }
  else meta_write(meta,outMetaName);
  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);

  if(flags[f_QUIET] == FLAG_NOT_SET) printf("Finished.\n\n");
}
