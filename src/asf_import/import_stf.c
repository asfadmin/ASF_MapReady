#include "asf_import.h"
#include "dateUtil.h"
#include "decoder.h"
#include "lzFetch.h"
#include "asf_reporting.h"

/* Prototypes */
void createSubset(char *inN, float lowerLat, float upperLat, long *imgStart,
                  long *imgEnd, char *imgTimeStr, int *nVec,
                  float *fd, float *fdd, float *fddd);
void createMeta_lz(bin_state *s, char *inN, char *outN, char *img_timeStr,
                   int nVec, float fd, float fdd, float fddd, int prcFlag,
                   char *prcPath);
bin_state *convertMetadata_lz(char *inName,char *outName,int *numLines,
                              readPulseFunc *readNextPulse, int prcFlag,
                              char *prcPath);
void estimateDoppler(char *inN, float *fd, float *fdd, float *fddd);


/******************************************************************************
 * Import Sky Telemetry Format data to ASF Tools format */
void import_stf(char *inDataName, char *inMetaName, char *outBaseName,
                int flags[],
                double lowerLat, double upperLat, char *prcPath)/*this last line of parameters are extra from the rest of the import_*() functions */
{
  char outDataName[256]="", outMetaName[256]="";        /* Output file names */
  char imgTimeStr[20]="";
  int nTotal, nVec=1;
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

  if (flags[f_SPROCKET] != FLAG_NOT_SET) {
    asfPrintError("Data is level 0, sprocket can not use this.\n");
    exit(EXIT_FAILURE);
  }

  /* Handle output file name */
  strcpy(outDataName,outBaseName);
  strcat(outDataName,TOOLS_RAW_EXT);
  strcpy(outMetaName,outBaseName);
  strcat(outMetaName,TOOLS_META_EXT);

  /* Make sure that none of the detected level one flags are set */
  strcpy(logbuf,"");
  if (flags[f_AMP] != FLAG_NOT_SET)
    { sprintf(logbuf, "%s amplitude", logbuf);  tempFlag=TRUE; }
  if (flags[f_SIGMA] != FLAG_NOT_SET)
    { sprintf(logbuf, "%s sigma", logbuf);      tempFlag=TRUE; }
  if (flags[f_BETA] != FLAG_NOT_SET)
    { sprintf(logbuf, "%s beta", logbuf);       tempFlag=TRUE; }
  if (flags[f_GAMMA] != FLAG_NOT_SET)
    { sprintf(logbuf, "%s gamma", logbuf);      tempFlag=TRUE; }
  if (flags[f_POWER] != FLAG_NOT_SET)
    { sprintf(logbuf, "%s power", logbuf);      tempFlag=TRUE; }
  if (tempFlag) {
    asfPrintStatus(
      "Warning:\n"
      "  The following flags will be ignored since this is a level zero data set:\n"
      "  %s\n", logbuf);
  }

  if (flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
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

  /* Read the metadata to determine where window position shifts happen, as
     well as the number of lines in the image. */
  s=convertMetadata_lz(inDataName,outMetaName,&nTotal,&readNextPulse,
                       flags[f_PRC],prcPath);
  iqBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);
  if (imgEnd == 0) imgEnd = nTotal;

  /* Now we just loop over the output lines, writing as we go. */
  fpOut=FOPEN(outDataName,"wb");
  s->nLines=0;
  s->readStatus=1;

  for (outLine=0; outLine<nTotal; outLine++) {
      if (s->curFrame >= s->nFrames) {
        asfPrintStatus("   Reached end of file\n");
        break;
      }

      /* Now read the next pulse of data. */
     readNextPulse(s, iqBuf, inDataName, outDataName); /* I think outDataName is just a place holder... at least for now */

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
      /* Write status information to screen. 
      asfLineMeter(outLine,nTotal);*/
  }

  if (flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
    s->nLines -= 4096; /* reduce the line number from extra padding */
    createMeta_lz(s,inDataName,outMetaName,imgTimeStr,nVec,fd,fdd,fddd,
                  flags[f_PRC],prcPath);
  }
  else {
    createMeta_lz(s,inDataName,outMetaName,NULL,nVec,fd,fdd,fddd,
                  flags[f_PRC],prcPath);
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
  meta_write(meta, outMetaName);
}
