#include "asf_import.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "calibrate.h"
#include "decoder.h"

#define MAX_tableRes 512

/* Prototypes from the meta library */
void meta_new2old(meta_parameters *meta);
void meta_write_old(meta_parameters *meta, const char *file_name);

static int isPP(meta_parameters *meta)
{
    return strstr(meta->general->processor, "PREC") != NULL;
}

/* Prototype in ceos2raw.c */
bin_state *convertMetadata_ceos(char *inN,char *outN,int *nLines,
                                readPulseFunc *readNextPulse);

/******************************************************************************
 * Import a wide variety for CEOS flavors (hopefully all) to our very own ASF
 * Tools format */
void import_ceos(char *inDataName, char *bandExt, int band, int nBands, 
		 char *inMetaName, char *lutName, char *outBaseName, 
		 radiometry_t radiometry, int db_flag)
{
  char outDataName[256], outMetaName[256];              /* Output file names */
  int nl=MAGIC_UNSET_INT, ns=MAGIC_UNSET_INT;     /* Number of lines/samples */
  long long ii, kk;                                          /* loop indices */
  int headerBytes;                       /* Number of bytes in a CEOS header */
  int leftFill;
  int rightFill;
  int tempFlag=FALSE;             /* Flag on whether or not to print warning */
  long long offset;
  meta_parameters *meta=NULL;                         /* Meta data structure */
  cal_params *cal_param=NULL;                  /* Calibration info structure */
  struct IOF_VFDR image_fdr;                  /* CEOS File Descriptor Record */
  data_type_t ceos_data_type;                     /* Data type for CEOS File */
  FILE *fpIn=NULL, *fpOut=NULL;              /* In/output data file pointers */
  int lut_flag=FALSE;

  /* Fill output names (don't add extention to data name because it differs
   * for raw, complex, and 'image' */
  sprintf(outDataName, "%s_%s", outBaseName, bandExt);
  sprintf(outMetaName, "%s_%s", outBaseName, bandExt);
  //strcpy(outDataName, outBaseName);
  //strcpy(outMetaName, outBaseName);
  strcat(outMetaName, TOOLS_META_EXT);

  /* Create metadata */
  meta=meta_create(inMetaName);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;

  /*PP Earth Radius Kludge*/
  if (isPP(meta))
  {
    double pp_er, pp_atpp;
    pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
    if (meta->sar) meta->sar->earth_radius_pp = pp_er;
  }

  /*Provided a LUT?*/
  if (lutName != NULL && strlen(lutName) > 0)
      lut_flag = TRUE;

  /************************* BEGIN RAW DATA SECTION **************************/
  if (meta->general->data_type==COMPLEX_BYTE) {
    int trash;
    bin_state *s;  /* Structure with info about the satellite & its raw data */
    readPulseFunc readNextPulse; /* Pointer to function that reads the next line 
				    of CEOS Data */
    iqType *iqBuf;           /* Buffer containing the complex i & q channels */

    /* Die if the sprocket flag is specified, since it doesn't do lvl 0 */
    //if (flags[f_SPROCKET] != FLAG_NOT_SET) {
    //  asfPrintError("Data is level 0, SProCKET can not use it.");
    //  exit(EXIT_FAILURE);
    //}

    /* Let the user know what format we are working on */
    asfPrintStatus("   Input data type: level zero raw data\n"
                   "   Output data type: complex byte raw data\n");
    if (nBands > 1)
      asfPrintStatus("   Input band: %s\n", bandExt);

    /* Make sure that none of the detected level one flags are set */
    strcpy(logbuf,"");
    switch (radiometry) {
        case r_AMP:
            sprintf(logbuf, "%s amplitude", logbuf);  tempFlag=TRUE; break;
        case r_SIGMA:
            sprintf(logbuf, "%s sigma", logbuf);      tempFlag=TRUE; break;
        case r_BETA:
            sprintf(logbuf, "%s beta", logbuf);       tempFlag=TRUE; break;
        case r_GAMMA:
            sprintf(logbuf, "%s gamma", logbuf);      tempFlag=TRUE; break;
        case r_POWER:
            sprintf(logbuf, "%s power", logbuf);      tempFlag=TRUE; break;
    }
    if (tempFlag) {
      asfPrintStatus(
        "Warning:\n"
        "  The following flags will be ignored since this is a level zero data set:\n"
        "  %s\n", logbuf);
    }

    /* Handle output files */
    strcat(outDataName,TOOLS_RAW_EXT);
    //    meta->general->data_type = COMPLEX_REAL32;  
    // FIXME: should we output floats or bytes?
    meta->general->image_data_type = RAW_IMAGE;
    s = convertMetadata_ceos(inMetaName, outMetaName, &trash, &readNextPulse);
    asfRequire (s->nBeams==1,"Unable to import level 0 ScanSAR data.\n");
    iqBuf = (iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
    fpOut = FOPEN(outDataName, "wb");
    getNextCeosLine(s->binary, s, inMetaName, outDataName); /* Skip CEOS header. */
    s->nLines = 0;
    for (ii=0; ii<nl; ii++) {
      readNextPulse(s, iqBuf, inDataName, outDataName);
      FWRITE(iqBuf, s->nSamp*2, 1, fpOut);
      asfLineMeter(ii,nl);
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
    asfPrintStatus("   Input data type: single look complex\n"
                   "   Output data type: single look complex\n");
    if (nBands > 1)
      asfPrintStatus("   Input band: %s\n", bandExt);
    if (strcmp(meta->general->bands, "") == 0)
      strcat(meta->general->bands, ",");
    strcat(meta->general->bands, bandExt);

    /* Make sure that none of the detected level one flags are set */
    strcpy(logbuf,"");
    switch (radiometry) {
        case r_AMP:
            sprintf(logbuf, "%s amplitude", logbuf);  tempFlag=TRUE; break;
        case r_SIGMA:
            sprintf(logbuf, "%s sigma", logbuf);      tempFlag=TRUE; break;
        case r_BETA:
            sprintf(logbuf, "%s beta", logbuf);       tempFlag=TRUE; break;
        case r_GAMMA:
            sprintf(logbuf, "%s gamma", logbuf);      tempFlag=TRUE; break;
        case r_POWER:
            sprintf(logbuf, "%s power", logbuf);      tempFlag=TRUE; break;
    }
    if (tempFlag) {
      asfPrintStatus(
        "Warning:\n"
        "  The following flags will be ignored since this is a complex data set:\n"
        "  %s\n", logbuf);
    }

    /* Deal with metadata */
    meta->general->data_type=COMPLEX_REAL32;
    meta->general->image_data_type=COMPLEX_IMAGE;

    /* Take care of image files and memory */
    strcat(outDataName,TOOLS_COMPLEX_EXT);
    fpIn  = fopenImage(inDataName,"rb");
    //if (band == 1)
    fpOut = fopenImage(outDataName,"wb");
    //else
    //  fpOut = fopenImage(outDataName,"ab");
    cpx_buf = (short *) MALLOC(2*ns * sizeof(short));
    out_cpx_buf = (complexFloat *) MALLOC(ns * nBands * sizeof(complexFloat));

    /* Read single look complex data */
    //get_ifiledr(inDataName,&image_fdr);
    get_ifiledr(inMetaName,&image_fdr);
    /* file + line header *
       Same change as for regular imagery. To be tested. */
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - (ns + leftFill + rightFill)
		     * image_fdr.bytgroup);
    /*
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - ns * image_fdr.bytgroup);
    */
    for (ii=0; ii<nl; ii++) {
      offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
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
      asfLineMeter(ii,nl);
    }
  }

  else if (meta->general->data_type==COMPLEX_REAL32) {
    float *cpx_buf=NULL;
    complexFloat *out_cpx_buf;

    /* Let the user know what format we are working on */
    asfPrintStatus("   Input data type: single look complex\n"
                   "   Output data type: single look complex\n");
    if (nBands > 1)
      asfPrintStatus("   Input band: %s\n", bandExt);
    if (strcmp(meta->general->bands, "") == 0)
      strcat(meta->general->bands, ",");
    strcat(meta->general->bands, bandExt);

    /* Make sure that none of the detected level one flags are set */
    strcpy(logbuf,"");
    switch (radiometry) {
        case r_AMP:
            sprintf(logbuf, "%s amplitude", logbuf);  tempFlag=TRUE; break;
        case r_SIGMA:
            sprintf(logbuf, "%s sigma", logbuf);      tempFlag=TRUE; break;
        case r_BETA:
            sprintf(logbuf, "%s beta", logbuf);       tempFlag=TRUE; break;
        case r_GAMMA:
            sprintf(logbuf, "%s gamma", logbuf);      tempFlag=TRUE; break;
        case r_POWER:
            sprintf(logbuf, "%s power", logbuf);      tempFlag=TRUE; break;
    }
    if (tempFlag) {
      asfPrintStatus(
        "Warning:\n"
        "  The following flags will be ignored since this is a complex data set:\n"
        "  %s\n", logbuf);
    }

    /* Deal with metadata */
    meta->general->data_type=COMPLEX_REAL32;
    meta->general->image_data_type=COMPLEX_IMAGE;

    /* Take care of image files and memory */
    strcat(outDataName,TOOLS_COMPLEX_EXT);
    fpIn  = fopenImage(inDataName,"rb");
    //if (band == 1)
    fpOut = fopenImage(outDataName,"wb");
    //else
    //  fpOut = fopenImage(outDataName,"ab");
    cpx_buf = (float *) MALLOC(2*ns * sizeof(float));
    out_cpx_buf = (complexFloat *) MALLOC(ns * nBands * sizeof(complexFloat));

    /* Read single look complex data */
    //get_ifiledr(inDataName,&image_fdr);
    get_ifiledr(inMetaName,&image_fdr);
    /* file + line header *
       Same change as for regular imagery. To be tested. */
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - (ns + leftFill + rightFill)
		     * image_fdr.bytgroup);
    /*
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - ns * image_fdr.bytgroup);
    */
    for (ii=0; ii<nl; ii++) {
      offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
      FSEEK64(fpIn, offset, SEEK_SET);
      FREAD(cpx_buf, sizeof(float), 2*ns, fpIn);
      for (kk=0; kk<ns; kk++) {
        /* Put read in data in proper endian format */
        big32(cpx_buf[kk*2]);
        big32(cpx_buf[kk*2+1]);
        /* Now do our stuff */
        out_cpx_buf[kk].real=cpx_buf[kk*2];
        out_cpx_buf[kk].imag=cpx_buf[kk*2+1];
      }
      put_complexFloat_line(fpOut, meta, ii, out_cpx_buf);
      asfLineMeter(ii,nl);
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
        ( (radiometry == r_SIGMA) || (radiometry == r_GAMMA) || 
          (radiometry == r_BETA) ) ) {
      asfPrintError("Unable to find calibration parameters in the metadata.\n");
    }

    /* Let the user know what format we are working on */
    if (meta->projection!=NULL && meta->projection->type!=MAGIC_UNSET_CHAR) {
      /* This must be ScanSAR */
      if (meta->projection->type != SCANSAR_PROJECTION &&
	  strcmp(meta->general->sensor, "RSAT") == 0) {
        /* This is actually geocoded.  We don't trust any
           already-geocoded products other than polar stereo in the
           northern hemisphere (because of the RGPS Ice tracking
           project, these have been tested a lot and actually work
           correctly). */
        if ( meta->projection->type != POLAR_STEREOGRAPHIC
             || meta->projection->hem == 'S' ) {
          asfPrintWarning("Import of map projected (Level 2) CEOS images other "
                        "than northern hemisphere polar stereo images is "
                        "prohibited, because these products are broken.  "
                        "Don't use them.\n");
        }
        sprintf(logbuf,
                "   Input data type: level two data\n"
                "   Output data type: geocoded amplitude image\n");
        meta->general->image_data_type = GEOCODED_IMAGE;
      }
      else if (strcmp(meta->general->sensor, "ALOS") == 0 &&
	       strcmp(meta->general->mode, "1B2R") == 0) {
	sprintf(logbuf,
		"   Input data type: level two data\n"
		"   Output data type: georeferenced amplitude image\n");
        meta->general->image_data_type = GEOREFERENCED_IMAGE;
      }
      else if (strcmp(meta->general->sensor, "ALOS") == 0 &&
	       strcmp(meta->general->mode, "1B2G") == 0) {
	sprintf(logbuf,
		"   Input data type: level two data\n"
		"   Output data type: geocoded amplitude image\n");
        meta->general->image_data_type = GEOCODED_IMAGE;
      }
      else {
        sprintf(logbuf,
                "   Input data type: level one data\n"
                "   Output data type: amplitude image\n");
        meta->general->image_data_type = AMPLITUDE_IMAGE;
      }
    }
    else if (radiometry == r_AMP) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: amplitude image\n");
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    }
    else if (radiometry == r_POWER) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: power image\n");
      meta->general->image_data_type = POWER_IMAGE;
    }
    else if (radiometry == r_SIGMA) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (sigma power scale values)\n");
      meta->general->image_data_type = SIGMA_IMAGE;
    }
    else if (radiometry == r_GAMMA) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (gamma power scale values)\n");
      meta->general->image_data_type = GAMMA_IMAGE;
    }
    else if (radiometry == r_BETA) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: calibrated image (beta power scale values)\n");
      meta->general->image_data_type = BETA_IMAGE;
    }
    else if (lut_flag) {
      sprintf(logbuf,
              "   Input data type: level one data\n"
              "   Output data type: user defined LUT image\n");
      meta->general->image_data_type = LUT_IMAGE;
    }
    else { /* No chosen output type: default to amplitude */
        radiometry = r_AMP;
        sprintf(logbuf,
                "   Input data type: level one data\n"
                "   Output data type: amplitude image\n");
        meta->general->image_data_type = AMPLITUDE_IMAGE;
    }
    asfPrintStatus(logbuf);
    if (nBands > 1)
      asfPrintStatus("   Input band: %s\n\n", bandExt);
    else
      asfPrintStatus("\n");
    if (strcmp(meta->general->bands, "") == 0)
      strcat(meta->general->bands, ",");
    strcat(meta->general->bands, bandExt);

    /* Open image files */
    fpIn=fopenImage(inDataName,"rb");
    //if (band == 1)
    fpOut=fopenImage(outDataName,"wb");
    //else
    //fpOut=fopenImage(outDataName,"ab");

    /* Check size of the header */
    //get_ifiledr(inDataName,&image_fdr);
    get_ifiledr(inMetaName,&image_fdr);
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
    headerBytes = firstRecordLen(inDataName)
                  + (image_fdr.reclen - (ns + leftFill + rightFill)
		     * image_fdr.bytgroup);

    /* Allocate memory for 16 bit amplitude data */
    if (meta->general->data_type==INTEGER16) { /* 16 bit amplitude data */
      ceos_data_type = INTEGER16;
      short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
      //out_buf = (float *) MALLOC(ns * nBands * sizeof(float));
      out_buf = (float *) MALLOC(ns * sizeof(float));
    }
    /* Allocate memory for 8 bit amplitude data */
    else if (meta->general->data_type==BYTE) { /* 8 bit amplitude data */
      ceos_data_type = BYTE;
      byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
      //out_buf = (float *) MALLOC(ns * nBands * sizeof(float));
      out_buf = (float *) MALLOC(ns * sizeof(float));
    }
    else
      asfPrintError("Unkown CEOS data format");

    meta->general->data_type=REAL32;



  /**** Take care of LUT images ****/
    if (lut_flag) {
      FILE *fpLut;
      double incid_table[MAX_tableRes];                  /* Incidence angle */
      double scale_table[MAX_tableRes];                   /* Scaling factor */
      double incid[MAX_tableRes], old, new;
      double UL_incid, UR_incid, LL_incid, LR_incid;
      double min_incid=100.0, max_incid=0.0;
      char line[255];
      int nLut=0, n, tableRes=MAX_tableRes, tablePix=0, ll, min, max;

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

      /* Calculate minimum and maximum incidence angle */
      UL_incid = meta_incid(meta, 0, 0);
      UR_incid = meta_incid(meta, nl, 0);
      LL_incid = meta_incid(meta, 0, ns);
      LR_incid = meta_incid(meta, nl, ns);
      if (UL_incid < min_incid) min_incid = UL_incid;
      if (UL_incid > max_incid) max_incid = UL_incid;
      if (UR_incid < min_incid) min_incid = UR_incid;
      if (UR_incid > max_incid) max_incid = UR_incid;
      if (LL_incid < min_incid) min_incid = LL_incid;
      if (LL_incid > max_incid) max_incid = LL_incid;
      if (LR_incid < min_incid) min_incid = LR_incid;
      if (LR_incid > max_incid) max_incid = LR_incid;
      min_incid *= R2D;
      max_incid *= R2D;

      /* Look up the index for the minimum in the LUT */
      n = 0;
      old = 100000000;
      for (ii=0; ii<nLut; ii++) {
        new = min_incid - incid_table[ii];
        if (fabs(new) < fabs(old)) {
          old = new;
          n++;
        }
        else break;
      }
      min = n;

      /* Look up the index for the maximum in the LUT */
      n = 0;
      old = 100000000;
      for (ii=0; ii<nLut; ii++) {
        new = max_incid - incid_table[ii];
        if (fabs(new) < fabs(old)) {
          old = new;
          n++;
        }
        else break;
      }
      max = n;

      /**** Read 16 bit data and apply look up table ****/
      if (ceos_data_type == INTEGER16) {

        if (nl<1500) tableRes=128;
        else if (nl<3000) tableRes=256;
        tablePix=((ns+(tableRes-1))/tableRes);

        for (ii=0; ii<nl; ii++) {
          /* Can't use get_float_line() for CEOS data, so we have to use SEEK,
           * FREAD, and then put the bytes in proper endian order manually  */
          offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(short_buf, sizeof(unsigned short), ns, fpIn);

          /* Allocate incidence table entries or update */
          if (ii==0 || (ii%(nl/tableRes)==0 && meta->projection != NULL))
            for (kk=0;kk<tableRes;kk++)
              incid[kk] = meta_incid(meta, kk*tablePix, ii);

          /* Calculate output values */
          for (kk=0; kk<ns; kk++) {
            if (short_buf[kk]) {
              double index=(double)kk/tablePix;
              int    base=(int)index;
              double frac=index-base;
              double interp = incid[base]+frac*(incid[base+1]-incid[base]);
              big16(short_buf[kk]);
              old = 10000000;

              for (ll=min; ll<max; ll++)
                if (interp < incid_table[ll]) break;

              out_buf[kk] = short_buf[kk] *
                (((scale_table[ll]-scale_table[ll-1]) /
                  (incid_table[ll]-incid_table[ll-1])) *
                  (interp-incid_table[ll-1]) + scale_table[ll-1]);
            }
            else
              out_buf[kk] = 0;
          }

          put_float_line(fpOut, meta, ii, out_buf);
          asfLineMeter(ii,nl);
        }

      } /**** End processing of 16 bit input data ****/

      /**** Read 8 bit data and apply look up table ****/
      else if (ceos_data_type == BYTE) {
        if (nl<1500) tableRes=128;
        else if (nl<3000) tableRes=256;
        tablePix=((ns+(tableRes-1))/tableRes);

        for (ii=0; ii<nl; ii++) {
          /* Read image line */
          offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

          /* Allocate incidence table entries or update */
          if (ii==0 || (ii%(nl/tableRes)==0 && meta->projection != NULL))
            for (kk=0;kk<tableRes;kk++)
              incid[kk] = meta_incid(meta, ii, kk*tablePix);

          /* Calculate output values */
          for (kk=0; kk<ns; kk++) {
            if (byte_buf[kk]) {
              /* Interpolate incidence angle */
              double index=(double)kk/tablePix;
              int    base=(int)index;
              double frac=index-base;
              double interp= (incid[base]+frac*(incid[base+1]-incid[base]))*R2D;

              old = 10000000;
              for (ll=min; ll<=max; ll++)
                if (interp < incid_table[ll]) break;

              out_buf[kk] = byte_buf[kk] *
                (((scale_table[ll]-scale_table[ll-1]) /
                  (incid_table[ll]-incid_table[ll-1])) *
                  (interp-incid_table[ll-1]) + scale_table[ll-1]);
            }
            else
              out_buf[kk] = 0;
          }
          put_float_line(fpOut, meta, ii, out_buf);
          asfLineMeter(ii,nl);
        }

      } /**** End processing of byte input data ****/
    } /**** End LUT output section ****/



  /**** Create calibrated (sigma, gamma, or beta naught) image ****/
    else if (radiometry==r_SIGMA || radiometry==r_GAMMA || radiometry==r_BETA)
    {
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
        asfPrintError("Unable to extract calibration parameters from CEOS file.");
      if (radiometry==r_SIGMA)
        cal_param->output_type=sigma_naught;
      else if (radiometry==r_GAMMA)
        cal_param->output_type=gamma_naught;
      else if (radiometry==r_BETA)
        cal_param->output_type=beta_naught;

    /**** Read 16 bit data and convert to calibrated amplitude data ****/
      if (ceos_data_type == INTEGER16) {

        for (ii=0; ii<nl; ii++) {
          /* Can't use get_float_line() for CEOS data, so we have to use FSEEK,
           * FREAD, and then put the bytes in proper endian order manually  */
          offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
          for (kk=0; kk<ns; kk++) {
            big16(short_buf[kk]);
          }

          /*Allocate noise table entries and/or update if needed.*/
          if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
            for (kk=0;kk<tableRes;kk++)
              noise_table[kk]=get_noise(cal_param,kk*tablePix,ii);
          if (radiometry==r_GAMMA)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
          if (radiometry==r_BETA)
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
	      if (db_flag) {
		out_buf[kk]=get_cal_dn_in_db(cal_param,noise,incid,(int)short_buf[kk]);
	      }
	      else {
		out_buf[kk]=get_cal_dn(cal_param,noise,incid,(int)short_buf[kk]);
	      }
	    }
            else
              out_buf[kk]= 0.0;
          }

          put_float_line(fpOut, meta, ii, out_buf);
          asfLineMeter(ii,nl);
        }
      } /**** End processing of 16 bit input data ****/

    /**** Read 8 bit data and convert to calibrated amplitude data ****/
      else if (ceos_data_type == BYTE) {

        for (ii=0; ii<nl; ii++) {
          offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
          FSEEK64(fpIn, offset, SEEK_SET);
          FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

          /*Allocate noise table entries and/or update if needed.*/
          if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
            for (kk=0;kk<tableRes;kk++)
              noise_table[kk]=get_noise(cal_param,kk*tablePix,ii);
          if (radiometry==r_GAMMA)
            /*Allocate incidence table entries or update.*/
            if (ii==0 || (ii%(nl/tableRes)==0 && cal_param->noise_type!=by_pixel))
              for (kk=0;kk<tableRes;kk++)
                incid_cos[kk]=get_invCosIncAngle(cal_param,kk*tablePix,ii);
          if (radiometry==r_BETA)
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
              if (db_flag) {
                out_buf[kk]=get_cal_dn_in_db(cal_param,noise,incid,(int)byte_buf[kk]);
              }
              else {
                out_buf[kk]=get_cal_dn(cal_param,noise,incid,(int)byte_buf[kk]);
              }
            }
            else
              out_buf[kk]= 0.0;
          }

          put_float_line(fpOut, meta, ii, out_buf);
          asfLineMeter(ii,nl);
        }
      } /**** End processing of byte input data ****/
    } /**** End calibrated output section ****/

  /**** Read 16 bit amplitude data ****/
    else if (ceos_data_type == INTEGER16) {

      for (ii=0; ii<nl; ii++) {
        offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);
        FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
        for (kk=0; kk<ns; kk++) {
          /* Put the data in proper endian order before we do anything */
          big16(short_buf[kk]);
          /* Now do our stuff */
          if (radiometry==r_POWER)
            out_buf[kk]=(float)(short_buf[kk]*short_buf[kk]);
          else
            out_buf[kk]=(float)short_buf[kk];
        }

        put_float_line(fpOut, meta, ii, out_buf);
        asfLineMeter(ii,nl);
      }
    }

  /**** Read 8 bit amplitde data ****/
    else {

      for (ii=0; ii<nl; ii++) {
        offset = (long long)headerBytes+ii*(long long)image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);
        FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);

        for (kk=0; kk<ns; kk++) {
          if (radiometry==r_POWER)
            out_buf[kk]=(float)(byte_buf[kk]*byte_buf[kk]);
          else
            out_buf[kk]=(float)byte_buf[kk];
        }

        put_float_line(fpOut, meta, ii, out_buf);
        asfLineMeter(ii,nl);
      }
    }

    if (out_buf)
      FREE(out_buf);
  }
  /************************ END DETECTED DATA SECTION ************************/

  /* Now that the data file is written, write the metadata */
  //if(flags[f_OLD_META] != FLAG_NOT_SET) {
    //char ddrName[256];
    //struct DDR ddr;
    //strcpy(ddrName, outBaseName);
    //strcat(ddrName, ".ddr");
    //meta2ddr(meta, &ddr);
    //c_putddr(ddrName, &ddr);
    //meta_new2old(meta);
    //meta_write_old(meta, outMetaName);
  //} else {
    meta_write(meta,outMetaName);
  //}

  if (isPP(meta))
  {
      asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
      asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                     meta->sar->earth_radius);
  }

  meta_free(meta);

  if (fpIn) /* CEOS L0 doesn't set fpIn, will still be NULL */
    FCLOSE(fpIn);
  FCLOSE(fpOut);
}
