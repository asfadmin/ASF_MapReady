#include "asf_import.h"
#include "dateUtil.h"
#include "decoder.h"
#include "lzFetch.h"
#include "get_stf_names.h"
#include "frame_calc.h"

/* Prototypes */
void createSubset(char *inN, float lowerLat, float upperLat, long *imgStart,
                  long *imgEnd, char *imgTimeStr, int *nVec,
                  float *fd, float *fdd, float *fddd);
void createMeta_lz(bin_state *s, char *inN, char *outN, char *img_timeStr,
                   int nVec, float fd, float fdd, float fddd, int prcFlag,
                   char *prcPath);
bin_state *convertMetadata_lz(char *inName,char *outName,int *numLines,
                              readPulseFunc *readNextPulse);
void estimateDoppler(char *inN, float *fd, float *fdd, float *fddd);


/******************************************************************************
 * Import Sky Telemetry Format data to ASF Tools format */
void import_stf(char *inBaseName, char *outBaseName, radiometry_t radiometry,
        char *inMetaNameOption, int lat_constrained, double lowerLat,
        double upperLat, char *prcPath)
{
  char outDataName[256]="", outMetaName[256]="", imgTimeStr[20]="";
  char *inDataName, *inMetaName;
  int nTotal;
  int  nVec=1;
  int outLine;                     /* Used to handle missing lines correctly */
  long imgStart=0, imgEnd=0;       /* Used to handle missing lines correctly */
  float fd, fdd, fddd;                               /* Doppler coefficients */
  FILE *fpOut=NULL;                           /* Data file to be written out */
  bin_state *s;    /* Structure with info about the satellite & its raw data */
  iqType *iqBuf;             /* Buffer containing the complex i & q channels */
  readPulseFunc readNextPulse; /* Pointer to function that reads the next line of CEOS Data */
  int tempFlag=FALSE;
  meta_parameters *meta;
  double lat, lon;
  int prc_flag = FALSE;

  if (inMetaNameOption == NULL) {
    require_stf_pair(inBaseName, &inDataName, &inMetaName);
  }
  else {
    /* Allow the base name to be different for data & meta */
    require_stf_data(inBaseName, &inDataName);
    require_stf_metadata(inMetaNameOption, &inMetaName);
  }

  /* Handle output file name */
  strcpy(outDataName,outBaseName);
  strcat(outDataName,TOOLS_RAW_EXT);
  strcpy(outMetaName,outBaseName);
  strcat(outMetaName,TOOLS_META_EXT);

  /* PRC? */
  if (prcPath != NULL && strlen(prcPath) > 0)
      prc_flag = TRUE;

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
      case r_SIGMA_DB:
          sprintf(logbuf, "%s sigma", logbuf);      tempFlag=TRUE; break;
      case r_BETA_DB:
          sprintf(logbuf, "%s beta", logbuf);       tempFlag=TRUE; break;
      case r_GAMMA_DB:
          sprintf(logbuf, "%s gamma", logbuf);      tempFlag=TRUE; break;
      case r_POWER:
          sprintf(logbuf, "%s power", logbuf);      tempFlag=TRUE; break;
  }

  if (lat_constrained) {
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
      sprintf(buf,
          "prep_block.sensor.ephemeris.sv_block.state_vector[%d].Date:",
          nVec);
      tmp_timeStr = lzStr(parName, buf, NULL);
      parse_ymdTime(tmp_timeStr,&tmp_date,&tmp_time);
      vec_sec = date_hms2sec(&tmp_time);
      new_delta = fabs(vec_sec - loc_sec);
      if ((new_delta > delta) && (old_delta > delta)) {
          done=TRUE;
      }
      old_delta = delta;
      delta = new_delta;
      nVec++;
    }
    FREE(tmp_timeStr);
    estimateDoppler(inDataName, &fd, &fdd, &fddd);
  }

  /* Read the metadata to determine where window position shifts happen, as
     well as the number of lines in the image. */
  s=convertMetadata_lz(inDataName,outMetaName,&nTotal,&readNextPulse);
  asfRequire (s->nBeams==1,"Unable to import level 0 ScanSAR data.\n");
  iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);
  if (imgEnd == 0) imgEnd = nTotal;

  /* Now we just loop over the output lines, writing as we go. */
  fpOut=FOPEN(outDataName,"wb");
  s->nLines=0;
  s->readStatus=1;

  for (outLine=0; outLine<nTotal; outLine++) {
      if (s->curFrame >= s->nFrames) {
        asfPrintStatus("\n\n   Reached end of file\n\n");
        break;
      }

      /* Now read the next pulse of data. */
     readNextPulse(s, iqBuf, inDataName, outDataName);

      /* If the read status is good, write this data. */
      if (s->readStatus == 1) {
        /* write some extra lines at the end for the SAR processing */
        if (((outLine >= imgStart) && (outLine <= imgEnd+4096)) ||  /* descending */
            ((outLine >= imgEnd) && (outLine <= imgStart+4096)))    /* ascending */
        {
            FWRITE(iqBuf,sizeof(iqType),s->nSamp*2,fpOut);
            s->nLines++;
        }
      }
      // Write status information to screen.
      asfLineMeter(outLine, nTotal);
  }
  asfLineMeter(nTotal, nTotal);

  if (lat_constrained) {
    s->nLines -= 4096; /* reduce the line number from extra padding */
    createMeta_lz(s,inDataName,outMetaName,imgTimeStr,nVec,fd,fdd,fddd,
                  prc_flag,prcPath);
  }
  else {
    createMeta_lz(s,inDataName,outMetaName,NULL,nVec,fd,fdd,fddd,
                  prc_flag,prcPath);
  }

  /* Clean up memory & open files */
  FREE(iqBuf);
  FCLOSE(fpOut);
  delete_bin_state(s);

  /* Determine center latitude and longitude */
  meta = meta_read(outMetaName);
  meta_get_latLon(meta, meta->general->line_count/2, meta->general->sample_count,
          0.0, &lat, &lon);
  meta->general->center_latitude = lat;
  meta->general->center_longitude = lon;
  meta->general->frame = asf_frame_calc(meta->general->sensor, lat, meta->general->orbit_direction);
  meta_write(meta, outMetaName);
  meta_free(meta);

  free_stf_names(inDataName, inMetaName);
}

// Read STF metadata for raw data
meta_parameters *meta_read_stf(const char *inFile)
{
  bin_state *s;
  readPulseFunc readNextPulse;
  char *inDataName, *inMetaName;
  int nTotal;
  float fd, fdd, fddd;
  meta_parameters *meta;

  const char *dir = get_asf_tmp_dir();
  char *suff = time_stamp_dir();
  char *outFile = MALLOC(sizeof(char)*(strlen(dir)+strlen(suff)+64));
  sprintf(outFile, "%s/temp_%s.meta", dir, suff);

  char *baseName = stripExt(inFile);
  require_stf_pair(baseName, &inDataName, &inMetaName);
  s=convertMetadata_lz(inDataName, outFile, &nTotal, &readNextPulse);
  s->nLines = nTotal;
  estimateDoppler(inDataName, &fd, &fdd, &fddd);
  createMeta_lz(s, inDataName, outFile, NULL, 3, fd, fdd, fddd, 0, NULL);
  delete_bin_state(s);
  meta = meta_read(outFile);
  meta_get_latLon(meta, meta->general->line_count/2, 
		  meta->general->sample_count, 0.0, 
		  &meta->general->center_latitude,
		  &meta->general->center_longitude);
  meta_get_corner_coords(meta);

  // clean up
  remove_file(outFile);

  char *fmt = appendExt(outFile, ".fmt");
  remove_file(fmt);
  free(fmt);

  char *in = appendExt(outFile, ".in");
  remove_file(in);
  free(in);

  free(outFile);
  free(baseName);
  free(inDataName);
  free(inMetaName);
  free(suff);

  return meta;
}
