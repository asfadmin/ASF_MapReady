#include "asf_import.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "calibrate.h"
#include "decoder.h"
#include <ctype.h>

#define MAX_tableRes 512

// Prototypes
void import_ceos_raw(char *inDataName, char *inMetaName, char *outDataName, 
		     char *outMetaName, char *bandExt, int band, int nBands, 
		     radiometry_t radiometry, int import_single_band);
void import_ceos_complex_int(char *inDataName, char *inMetaName, 
			     char *outDataName, char *outMetaName, 
			     char *bandExt, int band, int nBands, 
			     radiometry_t radiometry, int import_single_band);
void import_ceos_complex_float(char *inDataName, char *inMetaName, 
			       char *outDataName, char *outMetaName, 
			       char *bandExt, int band, int nBands, 
			       radiometry_t radiometry, int import_single_band);
void import_ceos_detected(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, char *bandExt, int band, int nBands, 
			  radiometry_t radiometry, int import_single_band, 
			  char *lutName, int db_flag);
void import_ceos_byte_lut(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, meta_parameters *meta, int band, 
			  int import_single_band, int nBands, char *lutName);
void import_ceos_int_lut(char *inDataName, char *inMetaName, char *outDataName, 
			 char *outMetaName, meta_parameters *meta, int band, 
			 int import_single_band, int nBands, char *lutName);
void import_ceos_byte_cal(char *inDataName, char *inMetaName, char *outDataName,
			  char *outMetaName, int band, int import_single_band,
			  int nBands, meta_parameters *meta, int db_flag,
			  radiometry_t radiometry);
void import_ceos_int_cal(char *inDataName, char *inMetaName, char *outDataName,
			 char *outMetaName, int band, int import_single_band,
			 int nBands, meta_parameters *meta, int db_flag,
			 radiometry_t radiometry);
void import_ceos_byte_amp(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, meta_parameters *meta, int band, 
			  int import_single_band, int nBands,
                          radiometry_t radiometry);
void import_ceos_int_amp(char *inDataName, char *inMetaName, char *outDataName, 
			 char *outMetaName, meta_parameters *meta, int band, 
			 int import_single_band, int nBands,
                         radiometry_t radiometry);


/*
These next few functions are used to fix scaling errors in the data that
result from the PP using an incorrect swath velocity during some of
the calculations.

Here is a summary of the fixes as described by Orion Lawlor in an e-mail
on 3/14/06:

xpix_ypix prints out these values for an ARDoP image.
> > azimuth pixel size at scene center: 3.9648920 meters   (xpix_ypix)
> > ASF geolocate azimuth velocity: 6660.144 m/s   (xpix_ypix)
> > PP swath velocity: 6626.552 m/s = ...
The velocities should be about the same for the corresponding
L1 image.

The PP L1 pixel spacing is supposed to always be 12.5m.
But because the PP miscalulates the swath velocity, L1 images actually
have a pixel spacing of:
PP spacing * real velocity / PP velocity = real spacing
12.5 * 6660.144/6626.552 = 12.5633662876 m

An ARDOP image's pixel spacing is properly computed by xpix_ypix (and
not in the "yPix" field of the .meta file!) and multilooked, so the
ARDOP image pixel spacing is really:
3.9648920 m/pix * 5-pixel ARDOP multilook = 19.8244600 m/pix

The expected L1-to-multilooked-ARDOP image scale factor is just the
ratio of the two image's pixel spacings:
19.8244600 m/pix / 12.5633662876 m/pix = 1.5779576545
*/

void fix_ypix(const char *outBaseName, double correct_y_pixel_size)
{
  asfPrintStatus("Applying y pixel size correction to the metadata...\n");

  char outMetaName[256];
  sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

  meta_parameters *omd = meta_read(outMetaName);

  asfPrintStatus("Original y pixel size: %g\n",
		 omd->general->y_pixel_size);
  asfPrintStatus("            corrected: %g\n",
		 correct_y_pixel_size);

  omd->general->y_pixel_size = correct_y_pixel_size;
  meta_write(omd, outMetaName);
  meta_free(omd);
}

float get_default_azimuth_scale(const char *outBaseName)
{
  char outMetaName[256];
  sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

  meta_parameters *omd = meta_read(outMetaName);

  float pp_velocity, corrected_velocity;
  xpyp_getVelocities(omd, &pp_velocity, &corrected_velocity);

  asfPrintStatus("       PP Velocity: %g\n", pp_velocity);
  asfPrintStatus("Corrected Velocity: %g\n", corrected_velocity);

  float qua, az_pixsiz;
  xpyp_getPixSizes(omd, &qua, &az_pixsiz);

  asfPrintStatus("      y pixel size: %g (xpix_ypix)\n", az_pixsiz);
  asfPrintStatus("      y pixel size: %g (metadata)\n",
		 omd->general->y_pixel_size);

    float real_spacing =
      omd->general->y_pixel_size * corrected_velocity / pp_velocity;

    float scale = az_pixsiz / real_spacing;

    asfPrintStatus("             Scale: %g\n\n", scale);
    meta_free(omd);

    return scale;
}

float get_default_ypix(const char *outBaseName)
{
  char outMetaName[256];
  sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

  meta_parameters *omd = meta_read(outMetaName);

  float qua, az_pixsiz;
  xpyp_getPixSizes(omd, &qua, &az_pixsiz);
  meta_free(omd);

  return az_pixsiz;
}

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
void import_ceos(char *inBaseName, char *outBaseName, char *format_type, 
		 char *band_id, char *lutName, double *p_range_scale, 
		 double *p_azimuth_scale, double *p_correct_y_pixel_size, 
		 char *inMetaNameOption, radiometry_t radiometry, int db_flag)
{
  char outDataName[256], outMetaName[256];
  ceos_description *ceos;
  char **inBandName = NULL, **inMetaName = NULL;
  char unscaledBaseName[256]="", bandExt[10]="";
  int do_resample, do_metadata_fix, ii, nBands, trailer, import_single_band = 0;
  int band;
  double range_scale = -1, azimuth_scale = -1, correct_y_pixel_size = 0;

  if (inMetaNameOption == NULL)
    require_ceos_pair(inBaseName, &inBandName, &inMetaName, &nBands,
		      &trailer);
  else {
    /* Allow the base name to be different for data & meta */
    require_ceos_data(inBaseName, &inBandName, &nBands);
    require_ceos_metadata(inMetaNameOption, &inMetaName, &trailer);
  }

  // Determine some flags
  do_resample = p_range_scale != NULL && p_azimuth_scale != NULL;
  do_metadata_fix = p_correct_y_pixel_size != NULL;
  
  if (p_range_scale)
    range_scale = *p_range_scale;
  if (p_azimuth_scale)
        azimuth_scale = *p_azimuth_scale;
  if (p_correct_y_pixel_size)
    correct_y_pixel_size = *p_correct_y_pixel_size;
  
  strcpy(unscaledBaseName, outBaseName);
  if (do_resample) {
    strcat(unscaledBaseName, "_unscaled");
  }

  ceos = get_ceos_description(inBaseName, NOREPORT);

  for (ii=0; ii<nBands; ii++) {

    strcpy(outDataName, outBaseName);
    strcpy(outMetaName, outBaseName);
    strcat(outMetaName, TOOLS_META_EXT);

    // Determine the band extension (band ID)
    if (ceos->sensor == SAR || ceos->sensor == PALSAR) {
      char *polarization;
      polarization = get_polarization(inBandName[ii]);
      strcpy(bandExt, polarization);
      FREE(polarization);
    }
    else if (ceos->sensor == AVNIR) {
      int band_number;      
      band_number = get_alos_band_number(inBandName[ii]);
      sprintf(bandExt, "0%d", band_number);
    }
    else if (ceos->sensor == PRISM)
      sprintf(bandExt, "01");
    
    // p will point to the beginning of the actual file (past the path)
    char *p = strrchr(inBandName[ii], '/');
    if (!p)
      p = inBandName[ii];
    else
      ++p;
    
    // Check to see if the user forced a band extension
    // (band_id) upon us... override the above if so
    if (band_id && strlen(band_id)) {
      // Force ii index to point at correct filename,
      // dying if it can't be found.
      char file_prefix[256];
      int found=0;
      
      // Be forgiving on numeric-only band IDs and let
      // the user enter digits with or without leading
      // zeros, e.g. 01 or 1 ...they are equivalent
      int iBandNo = atoi(band_id);
      if (iBandNo > 0 && iBandNo <= MAX_BANDS) {
	// band_id appears to be a valid numeric band number
	int alpha = 0;
	char *s = band_id;
	while (*s != '\0') {
	  if (isalpha((int)*s)) alpha = 1;
	  s++;
	}
	if (!alpha)
	  sprintf(band_id, "%02d", iBandNo);
      }
      band = atoi(band_id);
      sprintf(file_prefix, "IMG-%s-", band_id);
      ii = 0;
      do {
	if (strncmp(inBandName[ii], file_prefix, strlen(file_prefix)) == 0)
	  found = 1;
	ii++;
	if (ii > nBands)
	  asfPrintError("Expected ALOS-type, CEOS-formatted, multi-band data "
			"and\n"
			"selected band (\"%s\") file was not found \"%s\"\n", 
			band_id, strcat(file_prefix, inBaseName));
      } while (!found);
      if (found) {
	ii--;
	import_single_band = 1;
	strcpy(bandExt, band_id);
      }
    }
    else 
      band = ii+1;
    asfPrintStatus("   File: %s\n", inBandName[ii]);
    if (import_single_band) 
      nBands = 1;
    
    // Ingest the different data types
    if (ceos->ceos_data_type == CEOS_RAW_DATA)
      import_ceos_raw(inBandName[ii], inMetaName[0], outDataName, outMetaName, 
		      bandExt, band, nBands, radiometry, import_single_band);
    
    else if (ceos->ceos_data_type == CEOS_SLC_DATA_INT)
      import_ceos_complex_int(inBandName[ii], inMetaName[0], outDataName, 
			      outMetaName, bandExt, band, nBands, radiometry, 
			      import_single_band);
    
    else if (ceos->ceos_data_type == CEOS_SLC_DATA_FLOAT)
      import_ceos_complex_float(inBandName[ii], inMetaName[0], outDataName, 
				outMetaName, bandExt, band, nBands, radiometry, 
				import_single_band);
    
    else if (ceos->ceos_data_type == CEOS_AMP_DATA)
      import_ceos_detected(inBandName[ii], inMetaName[0], outDataName, 
			   outMetaName, bandExt, band, nBands, radiometry, 
			   import_single_band, lutName, db_flag);
  }

  // Resample, if necessary
  if (do_resample) {
    if (range_scale < 0)
      range_scale = DEFAULT_RANGE_SCALE;
    
    if (azimuth_scale < 0)
      azimuth_scale = get_default_azimuth_scale(unscaledBaseName);
    
    asfPrintStatus("Resampling with scale factors: "
		   "%lf range, %lf azimuth.\n",
		   range_scale, azimuth_scale);
    
    resample_nometa(unscaledBaseName, outBaseName,
		    range_scale, azimuth_scale);
    
    asfPrintStatus("Done.\n");
  }

  // metadata pixel size fix, if necessary
  if (do_metadata_fix) {
    if (correct_y_pixel_size < 0)
      correct_y_pixel_size = get_default_ypix(outBaseName);
    
    fix_ypix(outBaseName, correct_y_pixel_size);
  }
  
  free_ceos_names(inBandName, inMetaName);
}

void import_ceos_raw(char *inDataName, char *inMetaName, char *outDataName, 
		     char *outMetaName, char *bandExt, int band, int nBands, 
		     radiometry_t radiometry, int import_single_band)
{  
  FILE *fpOut=NULL;
  long long ii;
  int nl, trash, tempFlag=FALSE;
  bin_state *s;  /* Structure with info about the satellite & its raw data */
  readPulseFunc readNextPulse; /* Pointer to function that reads the next line
				  of CEOS Data */
  iqType *iqBuf;           /* Buffer containing the complex i & q channels */
  meta_parameters *meta;
  
  /* Create metadata */
  meta = meta_create(inMetaName);
  nl = meta->general->line_count;

  /*PP Earth Radius Kludge*/
  if (isPP(meta))
    {
      double pp_er, pp_atpp;
      pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
      if (meta->sar) meta->sar->earth_radius_pp = pp_er;
    }

  /* Let the user know what format we are working on */
  asfPrintStatus("   Input data type: level zero raw data\n"
		 "   Output data type: complex byte raw data\n");
  if (nBands > 1)
    asfPrintStatus("   Input band: %s\n", bandExt);
  if (band > 1) {
    meta_parameters *metaTmp=NULL;
    metaTmp = meta_read(outMetaName);
    strcat(meta->general->bands, metaTmp->general->bands);
    meta_free(metaTmp);
  }
  if (strcmp(meta->general->bands, "") != 0)
    strcat(meta->general->bands, ",");
  strcat(meta->general->bands, bandExt);
  
  /* Make sure that none of the detected level one flags are set */
  strcpy(logbuf,"");
  switch (radiometry) 
    {
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
  
  /* Handle output files */
  strcat(outDataName,TOOLS_RAW_EXT);
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
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  meta_write(meta, outMetaName);
  
  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
		   meta->sar->earth_radius);
  }
  
  meta_free(meta);
  
  FCLOSE(fpOut);
}
 
void import_ceos_complex_int(char *inDataName, char *inMetaName, 
			     char *outDataName, char *outMetaName, 
			     char *bandExt, int band, int nBands, 
			     radiometry_t radiometry, int import_single_band) 
{
  FILE *fpIn=NULL, *fpOut=NULL;
  char bandStr[50];
  short *cpx_buf=NULL;
  complexFloat cpx;
  float *amp_buf=NULL, *phase_buf=NULL;
  int nl, ns, tempFlag=FALSE, leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  meta_parameters *meta;
  
  /* Create metadata */
  meta = meta_create(inMetaName);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  if (nBands == 1 && strlen(meta->sar->polarization) == 0)
    strcpy(meta->sar->polarization, bandExt);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");

  /*PP Earth Radius Kludge*/
  if (isPP(meta))
  {
    double pp_er, pp_atpp;
    pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
    if (meta->sar) meta->sar->earth_radius_pp = pp_er;
  }

  /* Let the user know what format we are working on */
  asfPrintStatus("   Input data type: single look complex\n"
		 "   Output data type: single look complex (2-band)\n");
  if (nBands > 1)
    asfPrintStatus("   Input band: %s\n", bandExt);
  if (band > 1) {
    meta_parameters *metaTmp = NULL;
    metaTmp = meta_read(outMetaName);
    strcat(meta->general->bands, metaTmp->general->bands);
    meta_free(metaTmp);
  }
  else 
    strcpy(meta->general->bands, "");
  if (strcmp(meta->general->bands, "") != 0)
    strcat(meta->general->bands, ",");
  if (strlen(bandExt) == 0)
    sprintf(bandStr, "AMP-%s,PHASE-%s", 
	    meta->sar->polarization, meta->sar->polarization);
  else
    sprintf(bandStr, "AMP-%s,PHASE-%s", bandExt, bandExt);

  strcat(meta->general->bands, bandStr);
  
  /* Make sure that none of the detected level one flags are set */
  strcpy(logbuf,"");
  switch (radiometry) 
    {
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
  
  /* Deal with metadata */
  meta->general->data_type = REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;
  meta->general->band_count = import_single_band ? 2 : band*2;
  
  /* Take care of image files and memory */
  strcat(outDataName, TOOLS_COMPLEX_EXT);
  fpIn  = fopenImage(inDataName,"rb");
  if (band == 1)
    fpOut = fopenImage(outDataName,"wb");
  else
    fpOut = fopenImage(outDataName,"ab");
  cpx_buf = (short *) MALLOC(2*ns * sizeof(short));
  amp_buf = (float *) MALLOC(ns * sizeof(float));
  phase_buf = (float *) MALLOC(ns * sizeof(float));
  
  /* Read single look complex data */
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
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    FREAD(cpx_buf, sizeof(short), 2*ns, fpIn);
    for (kk=0; kk<ns; kk++) {
      /* Put read in data in proper endian format */
      big16(cpx_buf[kk*2]);
      big16(cpx_buf[kk*2+1]);
      /* Now do our stuff */
      cpx.real = (float)cpx_buf[kk*2];
      cpx.imag = (float)cpx_buf[kk*2+1];
      amp_buf[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      phase_buf[kk] = atan2(cpx.imag, cpx.real);
    }
    put_band_float_line(fpOut, meta, 0, ii, amp_buf);
    put_band_float_line(fpOut, meta, 1, ii, phase_buf);
    asfLineMeter(ii, nl);
  }
  FREE(cpx_buf);
  FREE(amp_buf);
  FREE(phase_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 2 : ii*2;
  if (nBands == 2 && meta->sar)
    sprintf(meta->general->bands, "AMP-%s,PHASE-%s", 
	    meta->sar->polarization, meta->sar->polarization);
  meta_write(meta, outMetaName);

  if (isPP(meta))
  {
      asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
      asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                     meta->sar->earth_radius);
  }

  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}
 
void import_ceos_complex_float(char *inDataName, char *inMetaName, 
			       char *outDataName, char *outMetaName, 
			       char *bandExt, int band, int nBands, 
			       radiometry_t radiometry, int import_single_band) 
{
  FILE *fpIn=NULL, *fpOut=NULL;
  int nl, ns, tempFlag=FALSE, leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  float *cpx_buf=NULL;
  complexFloat cpx;
  float *amp_buf=NULL, *phase_buf=NULL;
  char bandStr[50];
  struct IOF_VFDR image_fdr;
  meta_parameters *meta;
  
  /* Create metadata */
  meta = meta_create(inMetaName);
  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  if (nBands == 1)
    strcpy(meta->sar->polarization, bandExt);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");

  /*PP Earth Radius Kludge*/
  if (isPP(meta))
  {
    double pp_er, pp_atpp;
    pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
    if (meta->sar) meta->sar->earth_radius_pp = pp_er;
  }

  /* Let the user know what format we are working on */
  asfPrintStatus("   Input data type: single look complex\n"
		 "   Output data type: single look complex (2 band)\n");
  if (nBands > 1)
    asfPrintStatus("   Input band: %s\n", bandExt);
  if (band > 1) {
    meta_parameters *metaTmp=NULL;
    metaTmp = meta_read(outMetaName);
    strcat(meta->general->bands, metaTmp->general->bands);
    meta_free(metaTmp);
  }
  if (strcmp(meta->general->bands, "") != 0)
    strcat(meta->general->bands, ",");
  sprintf(bandStr, "AMP-%s,PHASE-%s", bandExt, bandExt);
  strcat(meta->general->bands, bandStr);
  
  /* Make sure that none of the detected level one flags are set */
  strcpy(logbuf,"");
  switch (radiometry) 
    {
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
  
  /* Deal with metadata */
  meta->general->data_type = REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;
  meta->general->band_count = import_single_band ? 2 : band*2;

  /* Take care of image files and memory */
  strcat(outDataName, TOOLS_COMPLEX_EXT);
  fpIn  = fopenImage(inDataName, "rb");
  if (band == 1)
    fpOut = fopenImage(outDataName, "wb");
  else
    fpOut = fopenImage(outDataName, "ab");
  cpx_buf = (float *) MALLOC(2*ns * sizeof(float));
  amp_buf = (float *) MALLOC(ns * sizeof(float));
  phase_buf = (float *) MALLOC(ns * sizeof(float));
  
  /* Read single look complex data */
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
      cpx.real = cpx_buf[kk*2];
      cpx.imag = cpx_buf[kk*2+1];
      amp_buf[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      phase_buf[kk] = atan2(cpx.imag, cpx.real);
    }
    put_band_float_line(fpOut, meta, 0, ii, amp_buf);
    put_band_float_line(fpOut, meta, 1, ii, phase_buf);
    asfLineMeter(ii, nl);
  }
  FREE(cpx_buf);
  FREE(amp_buf);
  FREE(phase_buf);
  strcpy(meta->general->basename, inDataName);
  meta_write(meta, outMetaName);

  if (isPP(meta))
  {
      asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
      asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                     meta->sar->earth_radius);
  }

  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}
 
void import_ceos_detected(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, char *bandExt, int band, int nBands, 
			  radiometry_t radiometry, int import_single_band, 
			  char *lutName, int db_flag)
{
  int lut_flag=FALSE;
  data_type_t ceos_data_type;
  meta_parameters *meta;
  char cal_scale[16];
  
  if (db_flag)
    strcpy (cal_scale,"decibel");
  else
    strcpy (cal_scale, "power scale");
  	
  // Create metadata
  meta = meta_create(inMetaName);
  ceos_data_type = meta->general->data_type;

  // PP Earth Radius Kludge
  if (isPP(meta))
    {
      double pp_er, pp_atpp;
      pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
      if (meta->sar) meta->sar->earth_radius_pp = pp_er;
    }

 if (lutName != NULL && strlen(lutName) > 0)
    lut_flag = TRUE;

  strcat(outDataName,TOOLS_IMAGE_EXT);
  
  if (radiometry == r_SIGMA || radiometry == r_GAMMA ||
      radiometry == r_BETA) {
    if (meta->optical) {
      asfPrintWarning("Cannot apply SIGMA, GAMMA or BETA radiometry "
		      "to optical data.\n"
		      "Set radiometry to the default amplitude\n");
      radiometry = r_AMP;
    }
    else if (check_cal(inMetaName)==0)
      asfPrintError("Unable to find calibration parameters "
		    "in the metadata.\n");
  }
  
  /* FIXME! Temporary warning about unsupported ALOS radiometries */
  if (strcmp(meta->general->sensor, "ALOS") == 0) {
    if (radiometry == r_POWER)
      asfPrintError("Currently unsupported radiometry for ALOS!\n");
  }

  /* Let the user know what format we are working on */
  if (meta->projection!=NULL && meta->projection->type!=MAGIC_UNSET_CHAR) {
    /* This must be ScanSAR */
    if (meta->projection->type != SCANSAR_PROJECTION &&
	strncmp(meta->general->sensor, "RSAT", 4) == 0 &&
	strncmp(meta->general->processor, "ASF", 4) == 0) {
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
    else if (strncmp(meta->general->sensor, "RSAT", 4) == 0 &&
	     strncmp(meta->general->processor, "CDPF", 4) == 0) {
      // This is geocoded data from MDA
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
    else if (radiometry == r_SIGMA) {
      sprintf(logbuf,
	      "   Input data type: level one data\n"
	      "   Output data type: calibrated image (sigma %s values)\n",
	      cal_scale);
      meta->general->image_data_type = SIGMA_IMAGE;
    }
    else if (radiometry == r_GAMMA) {
      sprintf(logbuf,
	      "   Input data type: level one data\n"
	      "   Output data type: calibrated image (gamma %s values)\n",
	      cal_scale);
      meta->general->image_data_type = GAMMA_IMAGE;
    }
    else if (radiometry == r_BETA) {
      sprintf(logbuf,
	      "   Input data type: level one data\n"
	      "   Output data type: calibrated image (beta %s values)\n",
	      cal_scale);
      meta->general->image_data_type = BETA_IMAGE;
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
	    "   Output data type: calibrated image (sigma %s values)\n",
	    cal_scale);
    meta->general->image_data_type = SIGMA_IMAGE;
  }
  else if (radiometry == r_GAMMA) {
    sprintf(logbuf,
	    "   Input data type: level one data\n"
	    "   Output data type: calibrated image (gamma %s values)\n",
	    cal_scale);
    meta->general->image_data_type = GAMMA_IMAGE;
  }
  else if (radiometry == r_BETA) {
    sprintf(logbuf,
	    "   Input data type: level one data\n"
	    "   Output data type: calibrated image (beta %s values)\n",
	    cal_scale);
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
  if (band > 1) {
    meta_parameters *metaTmp=NULL;
    metaTmp = meta_read(outMetaName);
    strcpy(meta->general->bands, metaTmp->general->bands);
    meta_free(metaTmp);
  }
  if (strcmp(meta->general->bands, "") != 0)
    strcat(meta->general->bands, ",");
  strcat(meta->general->bands, bandExt);
  
  if (meta->optical)
    meta->general->data_type = BYTE;
  else
    meta->general->data_type = REAL32;
  
  // Ingest the various detected formats
  if (ceos_data_type == BYTE) {
    if (lut_flag)
      import_ceos_byte_lut(inDataName, inMetaName, outDataName, outMetaName, meta,
			   band, import_single_band, nBands, lutName);
    else if (radiometry==r_SIGMA || radiometry==r_GAMMA || radiometry==r_BETA)
      import_ceos_byte_cal(inDataName, inMetaName, outDataName, outMetaName, 
			   band, import_single_band, nBands, meta, db_flag,
			   radiometry);
    else
      import_ceos_byte_amp(inDataName, inMetaName, outDataName, outMetaName, meta,
			   band, import_single_band, nBands, radiometry);
  }
  else if (ceos_data_type == INTEGER16) { 
    if (lut_flag)
      import_ceos_int_lut(inDataName, inMetaName, outDataName, outMetaName, meta,
			  band, import_single_band, nBands, lutName);
    else if (radiometry==r_SIGMA || radiometry==r_GAMMA || radiometry==r_BETA)
      import_ceos_int_cal(inDataName, inMetaName, outDataName, outMetaName,
			  band, import_single_band, nBands, meta, db_flag,
			  radiometry);
    else
      import_ceos_int_amp(inDataName, inMetaName, outDataName, outMetaName, meta,
			  band, import_single_band, nBands, radiometry);
  }    
  else
    asfPrintError("Unkown CEOS data format");
}

void import_ceos_byte_lut(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, meta_parameters *meta, int band, 
			  int import_single_band, int nBands, char *lutName)
{
  FILE *fpIn=NULL, *fpOut=NULL, *fpLut;
  unsigned char *byte_buf=NULL;
  float *out_buf=NULL;
  int nl, ns, leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  double incid_table[MAX_tableRes];
  double scale_table[MAX_tableRes];
  double incid[MAX_tableRes], old, new;
  double UL_incid, UR_incid, LL_incid, LR_incid;
  double min_incid=100.0, max_incid=0.0;
  char line[255];
  int nLut=0, n, tableRes=MAX_tableRes, tablePix=0, ll, min, max;
  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  
  // Read look up table
  fpLut = FOPEN(lutName, "r");
  for (ii=0; ii<MAX_tableRes; ii++) {
    incid_table[ii] = 0.0;
    scale_table[ii] = 0.0;
  }
  while(fgets(line, 255, fpLut)) {
    sscanf(line, "%lf\t%lf", &incid_table[nLut], &scale_table[nLut]);
    nLut++;
  }
  
  // Calculate minimum and maximum incidence angle
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
    
  // Look up the index for the minimum in the LUT
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
  
  // Look up the index for the maximum in the LUT 
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

  // Allocate memory  
  byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
  out_buf = (float *) MALLOC(ns * sizeof(float));
  
  // Open image files
  fpIn = fopenImage(inDataName, "rb");
  if (band == 1)
    fpOut = fopenImage(outDataName, "wb");
  else
    fpOut = fopenImage(outDataName, "ab");

  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) + 
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  if (nl < 1500) tableRes = 128;
  else if (nl < 3000) tableRes = 256;
  tablePix =((ns+(tableRes-1)) / tableRes);

  // Do some work  
  for (ii=0; ii<nl; ii++) {
    /* Read image line */
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);
    
    /* Allocate incidence table entries or update */
    if (ii==0 || (ii%(nl/tableRes)==0 && meta->projection != NULL))
      for (kk=0; kk<tableRes; kk++)
	incid[kk] = meta_incid(meta, ii, kk*tablePix);
    
    /* Calculate output values */
    for (kk=0; kk<ns; kk++) {
      if (byte_buf[kk]) {
	/* Interpolate incidence angle */
	double index = (double)kk/tablePix;
	int    base = (int)index;
	double frac = index-base;
	double interp = (incid[base]+frac*(incid[base+1]-incid[base]))*R2D;
	
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
    asfLineMeter(ii, nl);
  }

  // Clean up
  FREE(out_buf);
  FREE(byte_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");
  meta_write(meta, outMetaName);
  
  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
		   meta->sar->earth_radius);
  }
  
  meta_free(meta);
  
  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void import_ceos_int_lut(char *inDataName, char *inMetaName, char *outDataName, 
			 char *outMetaName, meta_parameters *meta, int band, 
			 int import_single_band, int nBands, char *lutName)
{
  FILE *fpIn=NULL, *fpOut=NULL, *fpLut;
  unsigned short *short_buf=NULL;
  float *out_buf=NULL;
  int nl, ns, leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  double incid_table[MAX_tableRes];
  double scale_table[MAX_tableRes];
  double incid[MAX_tableRes], old, new;
  double UL_incid, UR_incid, LL_incid, LR_incid;
  double min_incid=100.0, max_incid=0.0;
  char line[255];
  int nLut=0, n, tableRes=MAX_tableRes, tablePix=0, ll, min, max;
  nl = meta->general->line_count;
  ns = meta->general->sample_count;

  // Read look up table
  fpLut = FOPEN(lutName, "r");
  for (ii=0; ii<MAX_tableRes; ii++) {
    incid_table[ii] = 0.0;
    scale_table[ii] = 0.0;
  }
  while(fgets(line, 255, fpLut)) {
    sscanf(line, "%lf\t%lf", &incid_table[nLut], &scale_table[nLut]);
    nLut++;
  }

  // Calculate minimum and maximum incidence angle
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

  // Look up the index for the minimum in the LUT
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
  
  // Look up the index for the maximum in the LUT 
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

  // Allocate memory
  short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
  out_buf = (float *) MALLOC(ns * sizeof(float));

  // Open image files
  fpIn=fopenImage(inDataName,"rb");
  if (band == 1)
    fpOut=fopenImage(outDataName,"wb");
  else
    fpOut=fopenImage(outDataName,"ab");

  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  if (nl<1500) tableRes=128;
  else if (nl<3000) tableRes=256;
  tablePix=((ns+(tableRes-1))/tableRes);

  // Do some work  
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
    asfLineMeter(ii, nl);
  } 
  FREE(out_buf);
  FREE(short_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");
  meta_write(meta, outMetaName);

  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                   meta->sar->earth_radius);
  }
  
  meta_free(meta);
  
  FCLOSE(fpIn);
  FCLOSE(fpOut);
}
         
void import_ceos_byte_cal(char *inDataName, char *inMetaName, char *outDataName,
                          char *outMetaName, int band, int import_single_band,
                          int nBands, meta_parameters *meta, int db_flag,
                          radiometry_t radiometry)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned char *byte_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  cal_params *cal_param=NULL;
  int tableRes=MAX_tableRes, tablePix=0;        /* Calibration variables */
  double noise_table[MAX_tableRes];       /* Noise table for calibration */
  double incid_cos[MAX_tableRes];       /* Cosine of the incidence angle */
  double incid_sin[MAX_tableRes];         /* Sine of the incidence angle */

  // Read calibration parameters if required
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

  // Allocate memory
  byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
  out_buf = (float *) MALLOC(ns * sizeof(float));

  // Open image files
  fpIn=fopenImage(inDataName,"rb");
  if (band == 1)
    fpOut=fopenImage(outDataName,"wb");
  else
    fpOut=fopenImage(outDataName,"ab");

  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  // Do some work
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
	  out_buf[kk]=get_cal_dn_in_db(cal_param,noise,incid,(int)byte_buf[kk]
				       );
	  }
	else {
	  out_buf[kk]=get_cal_dn(cal_param,noise,incid,(int)byte_buf[kk]);
	}
      }
      else
	out_buf[kk]= 0.0;
    }
    
    put_float_line(fpOut, meta, ii, out_buf);
    asfLineMeter(ii, nl);
  }

  // Clean up
  FREE(out_buf);
  FREE(byte_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");
  meta_write(meta, outMetaName);
  
  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                   meta->sar->earth_radius);
  }
  
  meta_free(meta);
  
  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void import_ceos_int_cal(char *inDataName, char *inMetaName, char *outDataName,
                          char *outMetaName, int band, int import_single_band,
                          int nBands, meta_parameters *meta, int db_flag,
                          radiometry_t radiometry)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned short *short_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  cal_params *cal_param=NULL;
  int tableRes=MAX_tableRes, tablePix=0;        /* Calibration variables */
  double noise_table[MAX_tableRes];       /* Noise table for calibration */
  double incid_cos[MAX_tableRes];       /* Cosine of the incidence angle */
  double incid_sin[MAX_tableRes];         /* Sine of the incidence angle */
  
  // Read calibration parameters if required 
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

  // Allocate memory
  short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
  out_buf = (float *) MALLOC(ns * sizeof(float));

  // Open image files 
  fpIn=fopenImage(inDataName,"rb");
  if (band == 1)
    fpOut=fopenImage(outDataName,"wb");
    else
    fpOut=fopenImage(outDataName,"ab");

  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  // Do some work
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
    asfLineMeter(ii, nl);
  }
  FREE(out_buf);
  FREE(short_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4)
    strcpy(meta->sar->polarization, "quad-pol");
  meta_write(meta, outMetaName);

  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                   meta->sar->earth_radius);
  }

  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void import_ceos_byte_amp(char *inDataName, char *inMetaName, char *outDataName, 
			  char *outMetaName, meta_parameters *meta, int band, 
			  int import_single_band, int nBands,
			  radiometry_t radiometry)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned char *byte_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;

  // Allocate memory
  byte_buf = (unsigned char *) MALLOC(ns * sizeof(unsigned char));
  out_buf = (float *) MALLOC(ns * sizeof(float));

  // Open image files
  fpIn = fopenImage(inDataName,"rb");
  if (band == 1)
    fpOut = fopenImage(outDataName,"wb");
  else
    fpOut = fopenImage(outDataName, "ab");

  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  // Do some work
  for (ii=0; ii<nl; ii++) {
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);
    
    for (kk=0; kk<ns; kk++) {
      if (radiometry == r_POWER) {
	out_buf[kk] = (float)(byte_buf[kk]*byte_buf[kk]);
      }
      else if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
	out_buf[kk] = (float)byte_buf[kk+image_fdr.predata];
      }
      else {
	out_buf[kk] = (float)byte_buf[kk];
      }
    }
    
    put_float_line(fpOut, meta, ii, out_buf);
    asfLineMeter(ii, nl);
  }
  
  // Clean up
  FREE(out_buf);
  FREE(byte_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2 && meta->sar)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4 && meta->sar)
    strcpy(meta->sar->polarization, "quad-pol");
  meta_write(meta, outMetaName);

  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                   meta->sar->earth_radius);
  }

  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void import_ceos_int_amp(char *inDataName, char *inMetaName, char *outDataName, 
			 char *outMetaName, meta_parameters *meta, int band, 
			 int import_single_band, int nBands,
			 radiometry_t radiometry)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned short *short_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;

  // Allocate memory
  short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
  out_buf = (float *) MALLOC(ns * sizeof(float));

  // Open image files
  fpIn = fopenImage(inDataName, "rb");
  if (band == 1)
    fpOut = fopenImage(outDataName, "wb");
  else
    fpOut = fopenImage(outDataName, "ab");
  // Calculate header size
  if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
    get_ALOS_optical_ifiledr(inMetaName, &image_fdr);
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
  }
  if (strncmp(meta->general->sensor, "ALOS", 4) == 0 && meta->optical) {
    leftFill = image_fdr.predata+image_fdr.lbrdrpxl + 1;
    rightFill = image_fdr.sufdata+image_fdr.rbrdrpxl + 1;
  }
  else {
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
    (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);

  // Do some work
  for (ii=0; ii<nl; ii++) {
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
    for (kk=0; kk<ns; kk++) {
      /* Put the data in proper endian order before we do anything */
      big16(short_buf[kk]);
      /* Now do our stuff */
      if (radiometry == r_POWER) {
	out_buf[kk] = (float)(short_buf[kk]*short_buf[kk]);
      }
      else if (strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) {
        out_buf[kk] = (float)byte_buf[kk+image_fdr.predata];
      }
      else {
	out_buf[kk] = (float)short_buf[kk];
      } 
    }
    put_float_line(fpOut, meta, ii, out_buf);
    asfLineMeter(ii,nl);
  }

  // Clean up
  FREE(out_buf);
  FREE(short_buf);
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  if (nBands == 1 && meta->sar && strlen(meta->general->bands) == 0)
    strcpy(meta->general->bands, meta->sar->polarization);
  else if (nBands == 2 && meta->sar)
    strcpy(meta->sar->polarization, "dual-pol");
  else if (nBands == 4 && meta->sar)
    strcpy(meta->sar->polarization, "quad-pol");
  if (meta->sar)
    if (strncmp(meta->sar->polarization, "???", 3) == 0)
      strcpy(meta->sar->polarization, meta->general->bands);
  meta_write(meta, outMetaName);

  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3lf\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3lf\n",
                   meta->sar->earth_radius);
  }

  meta_free(meta);

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}
