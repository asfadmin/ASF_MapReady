#include "asf_import.h"
#include "asf_endian.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "calibrate.h"
#include "decoder.h"
#include "dateUtil.h"
#include <ctype.h>
#include <assert.h>

#define MAX_tableRes 512
#define MAX_IMG_SIZE 100000
#define EXP2(x) (x*x)
#define EXP3(x) (x*x*x)
#define EXP4(x) (x*x*x*x)
#define EXP5(x) (x*x*x*x*x)

// Prototypes
void import_ceos_raw(char *inDataName, char *inMetaName, char *outDataName,
         char *outMetaName, char *bandExt, int band, int nBands,
         radiometry_t radiometry, int line, int sample, int width, int height,
         int import_single_band);
void import_ceos_data(char *inDataName, char *inMetaName, char *outDataName,
		      char *outMetaName, char *bandExt, int band, int nBands,
		      int nBandsOut, radiometry_t radiometry,
                      int line, int sample, int width, int height,
                      int import_single_band, int complex_flag,
                      int multilook_flag, int azimuth_look_count, 
		      int range_look_count, char *lutName, int amp0_flag,
                      int apply_ers2_gain_fix_flag);

void import_ceos_int_slant_range_amp(char *inDataName, char *inMetaName,
       char *outDataName, char *outMetaName, meta_parameters *meta, int band,
       int import_single_band, int nBands, radiometry_t radiometry);
void import_ceos_int_slant_range_cal(char *inDataName, char *inMetaName,
                     char *outDataName,
       char *outMetaName, meta_parameters *meta, int band,
       int import_single_band, int nBands,
       radiometry_t radiometry, int dbFlag);
int is_alos_palsar_1_5_plus(char *inDataName, char *inMetaName);

static void status_data_type(meta_parameters *meta, data_type_t data_type,
                 radiometry_t radiometry,
                 int complex_flag, int multilook_flag);
double quadratic_2_incidence_angle(long long l, long long s, float *q);

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

int outOfBounds(int line, int sample, int height, int width, int ns, int nl)
{
  if (line < 0) return TRUE;
  if (sample < 0) return TRUE;
  if (height > nl) return TRUE;
  if (width > ns) return TRUE;
  if ((nl - line) < height) return TRUE;
  if ((ns - sample) < width) return TRUE;
  return FALSE;
}

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
void import_ceos(char *inBaseName, char *outBaseName,
		 char *band_id, char *lutName_in, double *p_range_scale,
		 double *p_azimuth_scale, double *p_correct_y_pixel_size,
		 int line, int sample, int width, int height,
		 char *inMetaNameOption, radiometry_t radiometry, int db_flag,
		 int complex_flag, int multilook_flag, int azimuth_look_count,
		 int range_look_count, int amp0_flag, int apply_ers2_gain_fix)
{
  char outDataName[256], outMetaName[256];
  ceos_description *ceos;
  meta_parameters *meta=NULL;
  char **inBandName = NULL, **inMetaName = NULL;
  char unscaledBaseName[256]="", bandExt[10]="";
  int do_resample, do_metadata_fix, ii, nBands, trailer;
  int band, import_single_band = 0;
  double range_scale = -1, azimuth_scale = -1, correct_y_pixel_size = 0;

  if (inMetaNameOption == NULL) {
    require_ceos_pair(inBaseName, &inBandName, &inMetaName, &nBands,
                      &trailer);
  }
  else {
    /* Allow the base name to be different for data & meta */
    require_ceos_data(inBaseName, &inBandName, &nBands);
    require_ceos_metadata(inMetaNameOption, &inMetaName, &trailer);
  }

  // Determine some flags
  do_resample = p_range_scale != NULL && p_azimuth_scale != NULL;
  do_metadata_fix = p_correct_y_pixel_size != NULL;

  if (p_range_scale) {
    range_scale = *p_range_scale;
  }
  if (p_azimuth_scale) {
    azimuth_scale = *p_azimuth_scale;
  }
  if (p_correct_y_pixel_size) {
    correct_y_pixel_size = *p_correct_y_pixel_size;
  }

  strcpy(unscaledBaseName, outBaseName);
  if (do_resample) {
    strcat(unscaledBaseName, "_unscaled");
  }

  // Add amplitude band, if requested, and applicable
  int nBandsOut = nBands;
  if (amp0_flag) {
    // Taking this code out -- we will still add the extra band (called "AMP")
    // at the beginning even if the rest of the data is already being imported
    // as amplitude -- this is so Faraday Rotation correction, or anything that
    // applies calibration after import, doesn't have to mess around with
    // adding the extra band, it will always be done here.
    //if (radiometry == r_AMP) {
    //  asfPrintWarning("amp0 flag ignored -- amplitude data is "
    //                  "already being imported.\n");
    //  amp0_flag = FALSE;
    //}
    //else {
      nBandsOut = 1;
    //}
  }

  // can't do db when radiometry is AMPLITUDE
  if (radiometry == r_AMP && db_flag) {
    asfPrintWarning("Since the requested output radiometry is AMPLITUDE, "
                    "the DB flag will\nbe ignored.\n");
    db_flag=FALSE;
  }

  ceos = get_ceos_description(inBaseName, REPORT_LEVEL_NONE);
  if ((ceos->product == LEVEL_1A || ceos->product == LEVEL_1B1) &&
       ceos->sensor == PRISM)
  {
    asfPrintWarning(
      "FOUND: PRISM Level %s data.  This data exists as several strips\n"
      "of data in separate files.  Ingesting all 4 strips and combining\n"
      "them into a single band image is not yet supported ...but will\n"
      "be in a future build.  The import process will now continue, but\n"
      "only a single (first found) strip from the image will be imported.\n",
      ceos->product == LEVEL_1A ? "1A" : "1B1");
    import_single_band = TRUE;
    nBandsOut = 1;
  }

  for (ii=0; ii<nBandsOut; ii++) {

    if (do_resample) {
      strcpy(outDataName, unscaledBaseName);
      strcpy(outMetaName, unscaledBaseName);
      strcat(outMetaName, TOOLS_META_EXT);
    }
    else {
      strcpy(outDataName, outBaseName);
      strcpy(outMetaName, outBaseName);
      strcat(outMetaName, TOOLS_META_EXT);
    }

    radiometry_t rad = radiometry;
    int index = ii;

    // Here is where the handle the amp0 flag.  Trick the band loop
    // into turning the ii==0 case into an AMPLITUDE band, using the
    // data from band #1.  ii==1 will then become the normal ii==0
    // case.  "index" serves as the adjusted value of ii.  (ie the index
    // into the inBandName[] array.)
    if (amp0_flag && ii==0) {
      rad = r_AMP;
      strcpy(bandExt,"AMP");
      band = 1;
    }
    else {
      if (amp0_flag) {
        --index;
      }

      // Determine the band extension (band ID)
      if (ceos->satellite == RSAT) {
        strcpy(bandExt, "HH");
      }
      else if (ceos->satellite == ERS || ceos->satellite == JERS) {
        strcpy(bandExt, "VV");
      }
      else if (ceos->sensor == SAR || ceos->sensor == PALSAR) {
        char *polarization;
        polarization = get_polarization(inBandName[index]);
        strcpy(bandExt, polarization);
        FREE(polarization);
      }
      else if (ceos->sensor == AVNIR || ceos->sensor == PRISM) {
        int band_number;
        band_number = get_alos_band_number(inBandName[index]);
        if (band_number <= 9)
          sprintf(bandExt, "0%d", band_number);
        else
          sprintf(bandExt, "%d", band_number);
       }
      else {
        strcpy(bandExt, MAGIC_UNSET_STRING);
      }

      // p will point to the beginning of the actual file (past the path)
      char *p = strrchr(inBandName[index], '/');
      if (!p) {
        p = inBandName[index];
      }
      else {
        ++p;
      }

      // Check to see if the user forced a band extension
      // (band_id) upon us... override the above if so
      // Don't go through this if you are only ingesting an amplitude image
      if (band_id && strlen(band_id) && strcmp_case(band_id, "NONE") != 0) {
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
          if (!alpha) {
            sprintf(band_id, "%02d", iBandNo);
          }
        }
        band = atoi(band_id);
        sprintf(file_prefix, "IMG-%s-", band_id);
        ii = 0;
        do {
          char *band_basename = get_basename(inBandName[ii]);
          if (strncmp(band_basename, file_prefix, strlen(file_prefix)) == 0) {
            found = 1;
          }
          ii++;
          if (ii > nBands) {
            asfPrintError("Expected ALOS-type, CEOS-formatted, multi-band"
                          " data and\n"
                          "selected band (\"%s\") file was not found \"%s\"\n",
                          band_id, strcat(file_prefix, inBaseName));
          }
          free(band_basename);
        } while (!found);
        if (found) {
          ii--;
          import_single_band = 1;
          nBandsOut = 1;
          strcpy(bandExt, band_id);
        }
      }
      else {
        band = ii+1;
      }
      asfPrintStatus("\n   File: %s\n", inBandName[index]);
      if (import_single_band) {
        nBands = 1;
      }
    }

    // This is the little extra exit for importing only amplitude images
    if (band_id && strcmp_case(band_id, "NONE") == 0 && ii>0) {
      if (do_resample) {
        if (range_scale < 0) {
          range_scale = DEFAULT_RANGE_SCALE;
        }

        if (azimuth_scale < 0) {
          azimuth_scale = get_default_azimuth_scale(unscaledBaseName);
        }

        asfPrintStatus("Resampling with scale factors: "
                   "%f range, %f azimuth.\n",
                   range_scale, azimuth_scale);

        resample(unscaledBaseName, outBaseName, range_scale, azimuth_scale);

        asfPrintStatus("\n\nDone.\n\n");
      }

      return;
    }

    if (ceos->facility == CDPF                      &&
        ceos->ceos_data_type != CEOS_SLC_DATA_INT   &&
        ceos->product != SSG                        &&
        ceos->product != GEC)
    {
      meta = meta_create(inMetaName[0]);
      meta->general->radiometry = rad;
    }

    // Update the radiometry if db flag is set
    // (UNLESS we are doing the secret amplitude band at the beginning)
    if (db_flag && !(amp0_flag && ii==0)) {
      rad += 3;
    }

    // Set LUT to NULL if string is empty
    char *lutName = NULL;
    if (lutName_in && strlen(lutName_in) > 0) {
      lutName = STRDUP(lutName_in);
    }

    // Ingest the different data types
    if (ceos->ceos_data_type == CEOS_RAW_DATA) {
      import_ceos_raw(inBandName[index], inMetaName[0], outDataName,
                      outMetaName, bandExt, band, nBands, rad,
                      line, sample, width, height, import_single_band);
    }
    else {
      if (ceos->facility == CDPF &&
          ceos->ceos_data_type != CEOS_SLC_DATA_INT)
      {
        if (rad >= r_SIGMA && rad <= r_GAMMA_DB) {
          import_ceos_int_slant_range_cal(inBandName[index], inMetaName[0],
                                          outDataName, outMetaName, meta, band,
                                          import_single_band, nBands, rad, db_flag);
        }
        else if (ceos->product == SSG || ceos->product == GEC ||
         ceos->product == SCN) {
          import_ceos_data(inBandName[index], inMetaName[0], outDataName,
                           outMetaName, bandExt, band, nBands, nBandsOut, rad,
                           line, sample, width, height,
                           import_single_band, complex_flag, multilook_flag,
			   azimuth_look_count, range_look_count,
                           lutName, amp0_flag, apply_ers2_gain_fix);
        }
        else {
          import_ceos_int_slant_range_amp(inBandName[index], inMetaName[0],
                                          outDataName, outMetaName, meta, band,
                                          import_single_band, nBands, rad);
        }
      }
      else {
        import_ceos_data(inBandName[index], inMetaName[0], outDataName,
                         outMetaName, bandExt, band, nBands, nBandsOut, rad,
                         line, sample, width, height,
                         import_single_band, complex_flag, multilook_flag,
			 azimuth_look_count, range_look_count,
                         lutName, amp0_flag, apply_ers2_gain_fix);
      }
    }
    if (meta) {
      meta_free(meta);
    }
  }

  // Resample, if necessary
  if (do_resample) {
    if (range_scale < 0) {
      range_scale = DEFAULT_RANGE_SCALE;
    }

    if (azimuth_scale < 0) {
      azimuth_scale = get_default_azimuth_scale(unscaledBaseName);
    }

    asfPrintStatus("Resampling with scale factors: "
                   "%f range, %f azimuth.\n",
                   range_scale, azimuth_scale);

    resample(unscaledBaseName, outBaseName, range_scale, azimuth_scale);
    char *del_file = appendExt(unscaledBaseName, ".img");
    remove_file(del_file);
    FREE(del_file);
    del_file = appendExt(unscaledBaseName, ".meta");
    remove_file(del_file);
    FREE(del_file);

    asfPrintStatus("\n\nDone.\n\n");
  }

  // metadata pixel size fix, if necessary
  if (do_metadata_fix) {
    if (correct_y_pixel_size < 0) {
      correct_y_pixel_size = get_default_ypix(outBaseName);
    }

    fix_ypix(outBaseName, correct_y_pixel_size);
  }

  free_ceos_names(inBandName, inMetaName);
  free(ceos);
}

void import_ceos_raw(char *inDataName, char *inMetaName, char *outDataName,
         char *outMetaName, char *bandExt, int band, int nBands,
         radiometry_t radiometry, int line, int sample, int width, int height,
     int import_single_band)
{
  FILE *fpOut=NULL;
  long long ii;
  int nl, trash;
  bin_state *s;  /* Structure with info about the satellite & its raw data */
  readPulseFunc readNextPulse; /* Pointer to function that reads the next line
          of CEOS Data */
  iqType *iqBuf;           /* Buffer containing the complex i & q channels */
  meta_parameters *meta;

  /* Create metadata */
  meta = meta_read_raw(inMetaName);
  nl = meta->general->line_count;

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

  /* Handle output files */
  strcat(outDataName,TOOLS_RAW_EXT);
  // FIXME: should we output floats or bytes?
  s = convertMetadata_ceos(inMetaName, outMetaName, &trash, &readNextPulse);
  asfRequire (s->nBeams==1,"Unable to import level 0 ScanSAR data.\n");
  iqBuf = (iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
  fpOut = FOPEN(outDataName, "wb");
  getNextCeosLine(s->binary, s, inMetaName, outDataName); /* Skip CEOS header. */
  s->nLines = 0;
  for (ii=0; ii<nl; ii++) {
    readNextPulse(s, iqBuf, inDataName, outDataName);
    ASF_FWRITE(iqBuf, s->nSamp*2, 1, fpOut);
    asfLineMeter(ii,nl);
    s->nLines++;
  }
  strcpy(meta->general->basename, inDataName);
  meta->general->band_count = import_single_band ? 1 : meta->general->band_count;
  struct dataset_sum_rec dssr;
  get_dssr(inMetaName, &dssr);
  if (dssr.sensor_id && strlen(dssr.sensor_id) && 
  	strncmp_case(dssr.mission_id, "ALOS", 4) != 0 && nBands == 1) {
    // HACK ALERT!!!  There must be a better way to get the polarizations and band names...
    char *s = dssr.sensor_id + strlen(dssr.sensor_id) - 1;
    while (isspace((int)*s)) s--;
    s -= 1;
    strcpy(meta->general->bands, s);
  }
  else if (strcmp_case(meta->general->sensor_name, "SAR") != 0) {
    sprintf(meta->general->bands, "%s",
            (meta->general->band_count == 1) ? "01" :
            (meta->general->band_count == 2) ? "01,02" :
            (meta->general->band_count == 3) ? "01,02,03" :
            (meta->general->band_count == 4) ? "01,02,03,04" : MAGIC_UNSET_STRING);
  }
  if (meta->sar) {
      struct VFDRECV facdr;
      get_asf_facdr(inMetaName, &facdr);
      if (strncmp(s->satName, "ERS1", 4) == 0) {
          // FIXME: Defaulting to 0.0 and spitting out a warning is a temporary fix ...see Mantis [723]
          asfPrintWarning("Reading of the Facility Related Data Record for CEOS format Level 0 product\n"
                     "is not currently supported.\n"
                     "Platform yaw, pitch, and roll defaulting to 0.0...\n");
          meta->sar->yaw = 0.0;
          meta->sar->pitch = 0.0;
          meta->sar->roll = 0.0;
      }
      else {
          meta->sar->yaw = facdr.scyaw;
          meta->sar->pitch = facdr.scpitch;
          meta->sar->roll = facdr.scroll;
      }
      strcpy(meta->sar->polarization, meta->general->bands);
		  if (strncmp(s->satName, "ALOS", 4) == 0)
  			meta->sar->prf = dssr.prf / 1000.0;
  		else
      	meta->sar->prf = s->prf;
      meta->sar->slant_range_first_pixel = s->range_gate*speedOfLight/2.0;
      meta->sar->multilook = 0;
  }
  if (strncmp(s->satName, "ERS1", 4) != 0 &&
      !meta_is_valid_double(meta->general->center_latitude) &&
      !meta_is_valid_double(meta->general->center_longitude))
  {
      double lat, lon;
      meta_get_latLon(meta, meta->general->line_count/2, meta->general->sample_count,
                    0.0, &lat, &lon);
      meta->general->center_latitude = lat;
      meta->general->center_longitude = lon;
  }

  /* Overwrite out ARDOP input parameter files.  (NOTE: They were written in convertMeta_ceos() previously */
  /* (Now that certain variables such as the prf have been properly updated)                               */
  if (fabs(dssr.crt_dopcen[0]) > 15000)
      writeARDOPparams(s,outMetaName, 0, -99, -99);
  else if (fabs(dssr.crt_dopcen[1]) > 1)
      writeARDOPparams(s,outMetaName, dssr.crt_dopcen[0], -99*s->prf, -99*s->prf);
  else
      writeARDOPparams(s,outMetaName,dssr.crt_dopcen[0],dssr.crt_dopcen[1],
                       dssr.crt_dopcen[2]);
  writeARDOPformat(s,outMetaName);
  delete_bin_state(s);

  /* Write ASF metadata out */
  meta_write(meta, outMetaName);

  if (isPP(meta)) {
    asfPrintStatus("PP Earth Radius: %.3f\n", meta->sar->earth_radius_pp);
    asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3f\n",
       meta->sar->earth_radius);
  }
  meta_free(meta);

  FCLOSE(fpOut);
}

void import_ceos_int_slant_range_amp(char *inDataName, char *inMetaName,
                     char *outDataName,
       char *outMetaName, meta_parameters *meta, int band,
       int import_single_band, int nBands,
       radiometry_t radiometry)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned short *short_buf=NULL, *tmp_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes, flip=FALSE, ons;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  struct proc_parm_rec *ppr=NULL;
  double gr_inc, sr_inc, sr0, srL;
  int *upper, *lower;
  float *ufrac, *lfrac;

  // Check whether image needs to be flipped
  if (meta->general->orbit_direction == 'D' && !meta->projection &&
      (strncmp(meta->general->processor, "RSI", 3) == 0 ||
       strncmp(meta->general->processor, "CDPF", 4) == 0 ||
       strncmp(meta->general->processor, "KACST", 5) == 0))
    flip = TRUE;

  struct dataset_sum_rec dssr;
  get_dssr(inDataName, &dssr);
  if (strncmp(dssr.time_dir_pix,"DECREASE",8)==0) {
    flip = TRUE;
  }

  // Read data type from metadata
  data_type_t data_type = meta->general->data_type;

  // Set image data type
  if (data_type >= COMPLEX_BYTE)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else
    meta->general->image_data_type = AMPLITUDE_IMAGE;

  // Set image data type
  meta->general->radiometry = radiometry;

  // Give user status on input and output data type
  status_data_type(meta, data_type, radiometry, 0, 1);

  // Open image files
  fpIn = fopenImage(inDataName, "rb");
  if (band == 1)
    fpOut = fopenImage(outDataName, "wb");
  else
    fpOut = fopenImage(outDataName, "ab");
  // Calculate header size
  get_ifiledr(inMetaName,&image_fdr);
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

  ppr = (struct proc_parm_rec*) MALLOC(sizeof(struct proc_parm_rec));
  get_ppr(inDataName, ppr);

  gr_inc = meta->general->x_pixel_size;
  sr0 = ppr->srgr_coefset[0].srgr_coef[0];
  srL = ppr->srgr_coefset[0].srgr_coef[0] +
    ns*gr_inc*ppr->srgr_coefset[0].srgr_coef[1] +
    EXP2(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[2] +
    EXP3(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[3] +
    EXP4(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[4] +
    EXP5(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[5];
  sr_inc = SPD_LIGHT /
    ((4.0 * meta->sar->range_sampling_rate) *
     meta->general->sample_count / meta->sar->original_sample_count);
  ons = (int) ((srL - sr0)/sr_inc + 0.5);
  printf("slant range first: %.3f, slant range last: %.3f\n", sr0, srL);
  printf("slant range pixel size: %.3f, number of output samples: %d\n",
     sr_inc, ons);

  lower = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  upper = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  lfrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  ufrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  for (kk=0; kk<ons; kk++) {
    lower[kk] = (int) (kk*ns/ons);
    upper[kk] = lower[kk] + 1;
    ufrac[kk] = kk*ns/ons - (float) lower[kk];
    lfrac[kk] = 1.0 - ufrac[kk];
    if (lower[kk] >= ns)
      lower[kk] = ns - 1;
    if (upper[kk] >= ns)
      upper[kk] = ns - 1;
  }

  // Allocate memory
  short_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
  if (flip)
    tmp_buf = (unsigned short *) MALLOC(ns * sizeof(unsigned short));
  out_buf = (float *) MALLOC(ons * sizeof(float));

  // Update metadata
  meta->general->x_pixel_size = sr_inc;
  meta->general->sample_count = ons;
  meta->general->start_sample = 0;
  meta->sar->sample_increment = 1.0;
  meta->sar->image_type = 'S';

  for (ii=0; ii<nl; ii++) {
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    ASF_FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
    // Need to check for endianess for the entire line first, in case we
    // need to flip
    for (kk=0; kk<ns; kk++) {
      /* Put the data in proper endian order before we do anything */
      big16(short_buf[kk]);
      if (flip)
    tmp_buf[kk] = short_buf[kk];
    }
    // Now do our stuff
    for (kk=0; kk<ons; kk++) {
      if (flip)
    short_buf[kk] = tmp_buf[ns-kk-1];
      out_buf[kk] = (float)(short_buf[lower[kk]]*lfrac[kk] +
                short_buf[upper[kk]]*ufrac[kk]);
      if (radiometry == r_POWER)
    out_buf[kk] = (float)((short_buf[lower[kk]]*lfrac[kk] +
                   short_buf[upper[kk]]*ufrac[kk]) *
                  (short_buf[lower[kk]]*lfrac[kk] +
                   short_buf[upper[kk]]*ufrac[kk]));
    }
    put_float_line(fpOut, meta, ii, out_buf);
    asfLineMeter(ii,nl);
  }

  // Clean up
  FREE(out_buf);
  FREE(short_buf);
  if (tmp_buf)
    FREE(tmp_buf);
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

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

void import_ceos_int_slant_range_cal(char *inDataName, char *inMetaName,
                     char *outDataName,
       char *outMetaName, meta_parameters *meta, int band,
       int import_single_band, int nBands,
       radiometry_t radiometry, int dbFlag)
{
  FILE *fpIn=NULL, *fpOut=NULL;
  unsigned short *short_buf=NULL, *tmp_buf=NULL;
  float *out_buf=NULL;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int leftFill, rightFill, headerBytes, flip=FALSE, ons;
  int n, tab_inc, i_low, i_up;
  long long ii, kk, offset;
  struct IOF_VFDR image_fdr;
  struct proc_parm_rec *ppr=NULL;
  struct RSI_VRADDR *radr=NULL;
  double gr_inc, sr_inc, sr0, srL;
  double *rad_tab=NULL, a2, a3, scaledPower;
  int *upper=NULL, *lower=NULL;
  float *ufrac=NULL, *lfrac=NULL;

  // Check whether image needs to be flipped
  if (meta->general->orbit_direction == 'D' && !meta->projection &&
      (strncmp(meta->general->processor, "RSI", 3) == 0 ||
       strncmp(meta->general->processor, "CDPF", 4) == 0 ||
       strncmp(meta->general->processor, "KACST", 5) == 0))
    flip = TRUE;

  struct dataset_sum_rec dssr;
  get_dssr(inDataName, &dssr);
  if (strncmp(dssr.time_dir_pix,"DECREASE",8)==0) {
    flip = TRUE;
  }

  // Read data type from metadata
  data_type_t data_type = meta->general->data_type;

  // Set image data type
  if (data_type >= COMPLEX_BYTE)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else
    meta->general->image_data_type = AMPLITUDE_IMAGE;

  // Set image data type
  meta->general->radiometry = radiometry;

  // Give user status on input and output data type
  status_data_type(meta, data_type, radiometry, 0, 1);

  // Open image files
  fpIn = fopenImage(inDataName, "rb");
  if (band == 1)
    fpOut = fopenImage(outDataName, "wb");
  else
    fpOut = fopenImage(outDataName, "ab");
  // Calculate header size
  get_ifiledr(inMetaName,&image_fdr);
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

  ppr = (struct proc_parm_rec*) MALLOC(sizeof(struct proc_parm_rec));
  get_ppr(inDataName, ppr);

  gr_inc = meta->general->x_pixel_size;
  sr0 = ppr->srgr_coefset[0].srgr_coef[0];
  srL = ppr->srgr_coefset[0].srgr_coef[0] +
    ns*gr_inc*ppr->srgr_coefset[0].srgr_coef[1] +
    EXP2(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[2] +
    EXP3(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[3] +
    EXP4(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[4] +
    EXP5(ns*gr_inc)*ppr->srgr_coefset[0].srgr_coef[5];
  sr_inc = SPD_LIGHT /
    ((4.0 * meta->sar->range_sampling_rate) *
     meta->general->sample_count / meta->sar->original_sample_count);
  ons = (int) ((srL - sr0)/sr_inc + 0.5);

  lower = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  upper = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  lfrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  ufrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  for (kk=0; kk<ons; kk++) {
    lower[kk] = (int) (kk*ns/ons);
    upper[kk] = lower[kk] + 1;
    ufrac[kk] = kk*ns/ons - (float) lower[kk];
    lfrac[kk] = 1.0 - ufrac[kk];
    if (lower[kk] >= ns)
      lower[kk] = ns - 1;
    if (upper[kk] >= ns)
      upper[kk] = ns - 1;
  }

  // Read lookup up table from radiometric data record
  radr = (struct RSI_VRADDR *) MALLOC(sizeof(struct RSI_VRADDR));
  get_rsi_raddr(inDataName, radr);
  n = radr->n_samp;
  rad_tab = (double *) MALLOC(sizeof(double) * n);
  for (ii=0; ii<n; ii++)
    rad_tab[ii] = radr->lookup_tab[ii];
  tab_inc = radr->samp_inc;
  a3 = radr->offset;

  // Allocate memory
  short_buf = (unsigned short *) MALLOC(sizeof(unsigned short) * ns);
  tmp_buf = (unsigned short *) MALLOC(sizeof(unsigned short) *
                      (ns>ons ? ns : ons));
  out_buf = (float *) MALLOC(sizeof(float) * ons);

  // Update metadata
  meta->general->x_pixel_size = sr_inc;
  meta->general->sample_count = ons;
  meta->general->start_sample = 0;
  meta->sar->sample_increment = 1.0;
  meta->sar->image_type = 'S';

  for (ii=0; ii<nl; ii++) {
    offset = (long long)headerBytes + ii*(long long)image_fdr.reclen;
    FSEEK64(fpIn, offset, SEEK_SET);
    ASF_FREAD(short_buf, sizeof(unsigned short), ns, fpIn);
    // Need to check for endianess for the entire line first, in case we
    // need to flip
    for (kk=0; kk<ns; kk++) {
      /* Put the data in proper endian order before we do anything */
      big16(short_buf[kk]);
      if (flip)
    tmp_buf[kk] = short_buf[kk];
    }
    if (flip) {
      for (kk=0; kk<ns; kk++)
    short_buf[kk] = tmp_buf[ns-kk-1];
    }

    // Now do our stuff
    for (kk=0; kk<ons; kk++) {
      if (kk < (tab_inc*(n-1))) {
    i_low = kk/tab_inc;
    i_up = i_low + 1;
    assert(i_low>=0 && i_low<n);
    assert(i_up>=0 && i_up<n);
    a2 = rad_tab[i_low] +
      ((rad_tab[i_up] - rad_tab[i_low])*((kk/tab_inc) - i_low));
      }
      else
    a2 = rad_tab[n-1] +
      ((rad_tab[n-1] - rad_tab[n-2])*((kk/tab_inc) - n-1));
      tmp_buf[kk] = (float)(short_buf[lower[kk]]*lfrac[kk] +
                short_buf[upper[kk]]*ufrac[kk]);
      if (radiometry == r_BETA)
    scaledPower = ((float)tmp_buf[kk]*tmp_buf[kk]+a3)/a2;
      if (scaledPower < 0.0 || tmp_buf[kk] < 0.0)
    scaledPower = 0.0;
      if (dbFlag)
    out_buf[kk] = (float)(10.0*log10(scaledPower));
      else
    out_buf[kk] = (float) scaledPower;
    }
    put_float_line(fpOut, meta, ii, out_buf);
    asfLineMeter(ii,nl);
  }

  // Clean up
  FREE(lower);
  FREE(upper);
  FREE(lfrac);
  FREE(ufrac);
  FREE(rad_tab);
  FREE(out_buf);
  FREE(short_buf);
  if (tmp_buf)
    FREE(tmp_buf);
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

  FCLOSE(fpIn);
  FCLOSE(fpOut);
}

int is_alos_palsar_1_5_plus(char *inDataName, char *inMetaName)
{
  int ret=0;
  ceos_description *ceos = get_ceos_description(inDataName, REPORT_LEVEL_STATUS);
  if (ceos->sensor == PALSAR && ceos->product == SGI)
    ret=1;

  return ret;
}

// Print input and output data types
static void status_data_type(meta_parameters *meta, data_type_t data_type,
                 radiometry_t radiometry,
                 int complex_flag, int multilook_flag)
{
  char radioStr[50], geoStr[50]="";

  switch (radiometry)
    {
    case r_AMP:
      sprintf(radioStr, "amplitude");
      break;
    case r_POWER:
      sprintf(radioStr, "power");
      break;
    case r_SIGMA:
      sprintf(radioStr, "calibrated sigma (power scale)");
      break;
    case r_SIGMA_DB:
      sprintf(radioStr, "calibrated sigma (decibel)");
      break;
    case r_BETA:
      sprintf(radioStr, "calibrated beta (power scale)");
      break;
    case r_BETA_DB:
      sprintf(radioStr, "calibrated beta (decibel)");
      break;
    case r_GAMMA:
      sprintf(radioStr, "calibrated gamma (power scale)");
      break;
    case r_GAMMA_DB:
      sprintf(radioStr, "calibrated gamma (decibel)");
      break;
    }

  // Give status on input data type
  if (data_type >= COMPLEX_BYTE)
    asfPrintStatus("   Input data type: single look complex\n");
  else if (meta->projection != NULL &&
       meta->projection->type != UNKNOWN_PROJECTION) {
    // This must be ScanSAR
    if (meta->projection->type != SCANSAR_PROJECTION &&
    strncmp(meta->general->sensor, "RSAT", 4) == 0 &&
    strncmp(meta->general->processor, "ASF", 4) == 0) {
      // This is actually geocoded.  We don't trust any
      // already-geocoded products other than polar stereo in the
      // northern hemisphere (because of the RGPS Ice tracking
      // project, these have been tested a lot and actually work
      // correctly).
      if ( meta->projection->type != POLAR_STEREOGRAPHIC ||
       meta->projection->hem == 'S' ) {
    asfPrintWarning("Import of map projected (Level 2) CEOS images other "
                        "than northern hemisphere polar stereo images is "
                        "prohibited, because these products are broken.  "
                        "Don't use them.\n");
      }
      asfPrintStatus("   Input data type: level two data\n");
      sprintf(geoStr, "geocoded ");
    }
    else if ((strncmp(meta->general->processor, "CSTARS", 6) == 0 ||
          strncmp(meta->general->processor, "CDPF", 4) == 0) &&
         meta->projection->type == SCANSAR_PROJECTION)
    {
      asfPrintStatus("   Input data type: level one data\n");
      strcpy(geoStr, "");
    }
    else if ((strncmp(meta->general->processor, "CSTARS", 6) == 0 ||
          strncmp(meta->general->processor, "CDPF", 4) == 0) &&
         meta->projection->type != SCANSAR_PROJECTION)
    {
      asfPrintStatus("   Input data type: level two data\n");
      sprintf(geoStr, "geocoded ");
    }
    else if (strcmp(meta->general->sensor, "ALOS") == 0 &&
         strcmp(meta->general->mode, "1B2G") == 0) {
      asfPrintStatus("   Input data type: level two data\n");
      sprintf(geoStr, "geocoded ");
    }
  }
  else
    asfPrintStatus("   Input data type: level one data\n");
  if (strcmp(meta->general->sensor, "ALOS") == 0 &&
      strcmp(meta->general->mode, "1B2R") == 0)
    sprintf(geoStr, "georeferenced ");

  // Give status on output data type
  if (data_type >= COMPLEX_BYTE) {
    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB)
      asfPrintStatus("   Output data type: %s image\n", radioStr);
    else if (complex_flag)
      asfPrintStatus("   Output data type: single look complex\n");
    else if (multilook_flag)
      asfPrintStatus("   Output data type: multilooked amplitude\n");
    else
      asfPrintStatus("   Output data type: single look complex (2-band)\n");
  }
  else
    asfPrintStatus("   Output data type: %s%s image\n", geoStr, radioStr);
}

// Assign band names
void assign_band_names(meta_parameters *meta, char *outMetaName,
               char *bandExt, int band, int nBands, int nBandsOut,
               radiometry_t radiometry, int complex_flag)
{
  char bandStr[512], radiometryStr[20];

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

  if (radiometry == r_POWER)
    sprintf(radiometryStr, "POWER-");
  else if (radiometry == r_SIGMA)
    sprintf(radiometryStr, "SIGMA-");
  else if (radiometry == r_SIGMA_DB)
    sprintf(radiometryStr, "SIGMA_DB-");
  else if (radiometry == r_GAMMA)
    sprintf(radiometryStr, "GAMMA-");
  else if (radiometry == r_GAMMA_DB)
    sprintf(radiometryStr, "GAMMA_DB-");
  else if (radiometry == r_BETA)
    sprintf(radiometryStr, "BETA-");
  else if (radiometry == r_BETA_DB)
    sprintf(radiometryStr, "BETA_DB-");
  else if (complex_flag)
    sprintf(radiometryStr, "COMPLEX-");
  else
    strcpy(radiometryStr, "");

  if (band==1 && nBandsOut==1 && radiometry==r_AMP) {
    // This is the "-amp0" case
    sprintf(bandStr, "AMP");
  }
  else if (meta->general->image_data_type == COMPLEX_IMAGE)
  {
    if (!complex_flag) {
      if (strlen(bandExt) == 0)
	sprintf(bandStr, "%sAMP-%s,%sPHASE-%s", radiometryStr, 
		meta->sar->polarization, radiometryStr, 
		meta->sar->polarization);
      else
	sprintf(bandStr, "%sAMP-%s,%sPHASE-%s", radiometryStr, bandExt,
		radiometryStr, bandExt);
    }
    else {
      if (strlen(bandExt) == 0)
	sprintf(bandStr, "%s%s", radiometryStr, meta->sar->polarization);
      else
	sprintf(bandStr, "%s%s", radiometryStr, bandExt);
    }
  }
  else {
    if (strlen(bandExt) == 0)
      sprintf(bandStr, "%s%s", radiometryStr, meta->sar->polarization);
    else
      sprintf(bandStr, "%s%s", radiometryStr, bandExt);
  }
  strcat(meta->general->bands, bandStr);
}

static float get_ers2_gain_adj(meta_parameters *meta, radiometry_t radiometry)
{
    if (strcmp(meta->general->sensor,"ERS2") != 0)
      return 0.0;

    // First, compute the number of days since launch
    ymd_date ref_ymd;
    ref_ymd.year = 1995;
    ref_ymd.month = 4;
    ref_ymd.day = 21;
    hms_time ref_hms;
    ref_hms.hour = 0;
    ref_hms.min = 0;
    ref_hms.sec = 0.;

    ymd_date ac_ymd;
    hms_time ac_hms;
    parse_DMYdate(meta->general->acquisition_date, &ac_ymd, &ac_hms);

    // These gain adjustments are from Wade - the ERS2 satellite requires a
    // date-dependent gain adjustment, a constant db adjustment to be applied
    // to all pixels in the image uniformly.  We currently only do this for
    // calibrated images.
    //    Launch    - 1/ 1/2001  y = -0.0018x + 0.0023
    //    1/ 2/2001 - 2/26/2003  y = -0.002247x + 0.9296
    //    2/27/2003 - current    y = -0.0018x - 0.3323 + 3
    // where:
    //    x = days since launch
    //    y = gain adjustment (in db)

    double days_since_launch =
      date_difference(&ref_ymd,&ref_hms,&ac_ymd,&ac_hms)/24./60./60.;

    double db_adj;
    if (days_since_launch < 2082) { // less than 1/1/2001
      // y = -0.0018x + 0.0023
      db_adj = -0.0018*days_since_launch + 0.0023;
    }
    else if (days_since_launch < 2869) { // less than 2/27/2003
      // y = -0.002247x + 0.9296
      db_adj = -0.002247*days_since_launch + 0.9296;
    }
    else { // after 2/27/2003
      // y = -0.0018x - 0.3323 + 3
      db_adj = -0.0018*days_since_launch - 0.3323 + 3.0;
    }

    double adj;
    if (radiometry==r_SIGMA_DB ||
        radiometry==r_BETA_DB ||
        radiometry==r_GAMMA_DB)
    {
      // the easy case: already in db
      adj = -db_adj;
      asfPrintStatus("ERS2 Gain Adjustment: %.2f (constant adjustment)\n",
                     adj);
    }
    else if (radiometry==r_AMP)
    {
      // don't apply correction to amplitude currently!
      adj = 0;
    }
    else // r_SIGMA, r_BETA, r_GAMMA, r_POWER
    {
      // calculate scale factor
      adj = pow(10., -db_adj/10.);
      asfPrintStatus("ERS2 Gain Adjustment: %.2f (constant scale factor)\n",
                     adj);
    }

    return (float)adj;
}

static float apply_ers2_gain_fix(radiometry_t radiometry, float correction,
                                 float current_value)
{
    if (radiometry==r_SIGMA_DB ||
        radiometry==r_BETA_DB ||
        radiometry==r_GAMMA_DB)
    {
      // the easy case: already in db, just add the correction
      return current_value + correction;
    }
    else if (radiometry==r_AMP)
    {
      // don't apply correction to amplitude currently!
      return current_value;
    }
    else // r_SIGMA, r_BETA, r_GAMMA, r_POWER
    {
      // multiplicative correction
      return current_value * correction;
    }

    assert(FALSE); // not reached
    return 0;
}

// Import all flavors of detected data
void import_ceos_data(char *inDataName, char *inMetaName, char *outDataName,
                      char *outMetaName, char *bandExt, int band, int nBands,
                      int nBandsOut, radiometry_t radiometry,
                      int line, int sample, int width, int height,
                      int import_single_band, int complex_flag,
                      int multilook_flag, int azimuth_look_count, 
		      int range_look_count, char *lutName, int amp0_flag,
                      int apply_ers2_gain_fix_flag)
{
  FILE *fpIn=NULL;
  int nl, ns, alc, rlc, nAzimuthLooks, nRangeLooks, flip=FALSE;
  int leftFill, rightFill, headerBytes;
  int out = 0;
  int min, max;
  int projected = 0; // Set to true if data is geocoded
  long long ii, kk, ll, mm, nn, offset;
  float fValue;
  double *incid_table, *scale_table;
  struct IOF_VFDR image_fdr;
  meta_parameters *meta;
  data_type_t data_type;

  // input buffers
  unsigned char *byte_buf=NULL, *cpx_byte_buf=NULL, *tmp_byte_buf=NULL;
  unsigned short *short_buf=NULL, *tmp_short_buf=NULL;
  short *cpx_short_buf=NULL, *tmp_cpx_short_buf=NULL;
  int *int_buf=NULL, *cpx_int_buf=NULL, *tmp_int_buf=NULL;
  float *float_buf=NULL, *cpx_float_buf=NULL, *tmp_float_buf=NULL;
  double *double_buf=NULL, *cpx_double_buf=NULL, *tmp_double_buf=NULL;

  // output buffers
  unsigned char *amp_byte_buf=NULL;
  float *amp_float_buf=NULL;
  float *phase_float_buf=NULL;
  float *incid=NULL;
  complexFloat cpx, *cpxFloat_buf=NULL, *cpx_float_ml_buf=NULL;

  // Output file will stay open through multiple calls to this function.
  // We open it on first call, try to close it on the last call.
  // Attempt to detect if we are called again after file was closed
  // using the "output_file_closed" flag -- die if set.
  // To handle batch mode, we reset the "output_file_closed" flag if we
  // are going to open the file.
  static FILE *fpOut = NULL;
  static int output_file_closed = FALSE;
  if (band == 1 || import_single_band) {
      if (fpOut) {
          asfPrintError("Impossible: Output file should not be opened!\n");
      }
      output_file_closed = FALSE;
  }
  if (output_file_closed) {
    asfPrintError("Impossible: Output file has already been closed!\n");
  }

  // Create metadata
  meta = meta_create(inMetaName);
  strcpy(meta->general->basename, inDataName);

  // Read data type from metadata
  data_type = meta->general->data_type;

  // Set image data type
  if (data_type >= COMPLEX_BYTE) {
    meta->general->image_data_type = COMPLEX_IMAGE;
  }
  else if (lutName) {
    meta->general->image_data_type = LUT_IMAGE;
  }
  else {
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  }

  // Set flag for whether the data is projected or not
  projected = (meta->sar && meta->sar->image_type == 'P' && meta->projection) ? 1 : 0;

  // Set image data type
  meta->general->radiometry = radiometry;

  // Read calibration LUT from file
  if (lutName) {
    read_cal_lut(meta, lutName, &incid_table, &scale_table, &min, &max);
  }

  // Determine dB flag
  int db_flag = FALSE;
  if (radiometry >= r_SIGMA_DB && radiometry <= r_GAMMA_DB) {
    db_flag = TRUE;
  }

  // FIXME! Temporary error-out for unsupported ALOS radiometries
  // r_POWER should be supported for level 1.5R/G regardless,
  // and levels below that need some evaluation to determine
  // when to apply the power conversion
  if (strcmp(meta->general->sensor, "ALOS") == 0) {
      if (radiometry == r_POWER && !is_alos_palsar_1_5_plus(inDataName, inMetaName)) {
          asfPrintError("POWER is a currently unsupported radiometry for ALOS!\n");
      }
  }
  if (!import_single_band &&
      nBands < 4          &&
      strcmp_case(meta->general->sensor_name, "AVNIR") == 0)
  {
      asfPrintWarning("Only %d bands were found, but 4 bands are expected for ALOS AVNIR data.\n"
              "Band numbering in metadata file may not properly identify the list of\n"
              "available bands, i.e. if bands 01, 03, and 04 were found, the metadata\n"
              "will still use \"01,02,03\" for the band names.\n\nWere some files deleted?\n\n", nBands);
      meta->general->band_count = nBands;
  }

  // Take care of optical data settings
  if (meta->optical) {
    meta->general->data_type = ASF_BYTE;
    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
      asfPrintWarning("Cannot apply SIGMA, GAMMA or BETA radiometry "
                      "to optical data.\n"
                      "Set radiometry to the default amplitude\n");
      radiometry = r_AMP;
    }
  }
  else {
    meta->general->data_type = REAL32;
  }

  nl = meta->general->line_count;
  ns = meta->general->sample_count;
  if (azimuth_look_count > 0)
    alc = nAzimuthLooks = azimuth_look_count;
  else {
    if (meta->sar)
      alc = nAzimuthLooks = meta->sar->azimuth_look_count;
    else
      alc = nAzimuthLooks = 1;
  }
  if (range_look_count > 0)
    rlc = nRangeLooks = range_look_count;
  else {
    if (meta->sar)
      rlc = nRangeLooks = meta->sar->range_look_count;
    else
      rlc = nRangeLooks = 1;
  }

  // PP Earth Radius Kludge
  if (isPP(meta) && meta->sar)
  {
    double pp_er, pp_atpp;
    pp_get_corrected_vals(inMetaName, &pp_er, &pp_atpp);
    if (meta->sar) meta->sar->earth_radius_pp = pp_er;
  }

  // Initialize calibration if you need to
  if (radiometry >= r_SIGMA && radiometry <= r_GAMMA && meta->sar) 
    create_cal_params_ext(inMetaName, meta, FALSE);
  else if (radiometry >= r_SIGMA_DB && radiometry <= r_GAMMA_DB && meta->sar)
    create_cal_params_ext(inMetaName, meta, TRUE);    
  if (strcmp_case(meta->general->sensor, "ERS1") == 0 ||
      strcmp_case(meta->general->sensor, "ERS2") == 0 ||
      strcmp_case(meta->general->sensor, "JERS1") == 0 ||
      strcmp_case(meta->general->sensor, "RSAT-1") == 0)
    asfPrintWarning("The noise floor removal is not applied to the data!\n");

  // Making sure that multilook flag is set appropriately
  if (data_type >= COMPLEX_BYTE && !complex_flag)
    multilook_flag = TRUE;

  // Give user status on input and output data type
  status_data_type(meta, data_type, radiometry, complex_flag, multilook_flag);

  // Assign band names, set correct data type and band count
  if (meta->sar) {
    assign_band_names(meta, outMetaName, bandExt, band, nBands, nBandsOut,
                      radiometry, complex_flag);
    if (complex_flag) {
      meta->general->data_type = COMPLEX_REAL32;
      meta->general->band_count = import_single_band ? 1 : band;
    }
    else if (data_type >= COMPLEX_BYTE) {
      meta->general->data_type = REAL32;
      if (strcmp_case(bandExt, "AMP") == 0)
	meta->general->band_count = 1;
      else
	meta->general->band_count = import_single_band ? 2 : band*2;
    }
    else {
      meta->general->data_type = REAL32;
      meta->general->band_count = import_single_band ? 1 : band;
    }
  }
  else if (meta->optical) {
    sprintf(bandExt, "0%d", band);
    if (nBands > 1) {
      asfPrintStatus("   Input band: %s\n", bandExt);
    }
    if (band > 1) {
        if (strcmp_case(meta->general->sensor_name, "PRISM") != 0 &&
            (strcmp_case(meta->general->mode, "1A") != 0 ||
             strcmp_case(meta->general->mode, "1B1") != 0))
        {
            meta_parameters *metaTmp=NULL;
            metaTmp = meta_read(outMetaName);
            strcat(meta->general->bands, metaTmp->general->bands);
            meta_free(metaTmp);
        }
    }
    if (strcmp(meta->general->bands, "") != 0) {
      strcat(meta->general->bands, ",");
    }
    strcat(meta->general->bands, bandExt);
  }

  // Take care of polarized imagery
  if (meta->sar) {
    if (nBands == 1) {
      strcpy(meta->sar->polarization, bandExt);
    }
    else if (nBands == 2) {
      strcpy(meta->sar->polarization, "dual-pol");
      meta->general->image_data_type = POLARIMETRIC_IMAGE;
    }
    else if (nBands == 4) {
      strcpy(meta->sar->polarization, "quad-pol");
      meta->general->image_data_type = POLARIMETRIC_IMAGE;
    }
    else {
      asfPrintError("Impossible: nbands = %d\n", nBands);
    }
  }

  // Open files
  if (strcmp_case(meta->general->sensor_name, "PRISM") == 0 &&
      (strcmp_case(meta->general->mode, "1A") == 0 ||
       strcmp_case(meta->general->mode, "1B1") == 0))
  {
    char *append = (char *) MALLOC(sizeof(char)*15);
    sprintf(append, "_%s%s", bandExt, TOOLS_IMAGE_EXT);
    sprintf(outMetaName, "%s_%s%s", outDataName, bandExt, TOOLS_META_EXT);
    strcat(outDataName, append);
  }
  else {
    strcat(outDataName, TOOLS_IMAGE_EXT);
  }
  fpIn  = fopenImage(inDataName, "rb");
  if (!fpOut) {
    fpOut = fopenImage(outDataName, "wb");
  }

  // Allocate memory for input buffers
  switch (data_type) {
    case ASF_BYTE:
      byte_buf = (unsigned char *) CALLOC(ns, sizeof(unsigned char));
      tmp_byte_buf = (unsigned char *) CALLOC(ns, sizeof(unsigned char));
      asfPrintStatus("   Data type: BYTE\n");
      break;
    case INTEGER16:
      short_buf = (unsigned short *) CALLOC(ns, sizeof(unsigned short));
      tmp_short_buf = (unsigned short *) CALLOC(ns, sizeof(unsigned short));
      asfPrintStatus("   Data type: INTEGER16\n");
      break;
    case INTEGER32:
      int_buf = (int *) CALLOC(ns, sizeof(int));
      tmp_int_buf = (int *) CALLOC(ns, sizeof(int));
      asfPrintStatus("   Data type: INTEGER32\n");
      break;
    case REAL32:
      float_buf = (float *) CALLOC(ns, sizeof(float));
      tmp_float_buf = (float *) CALLOC(ns, sizeof(float));
      asfPrintStatus("   Data type: REAL32\n");
      break;
    case REAL64:
      double_buf = (double *) CALLOC(ns, sizeof(double));
      tmp_double_buf = (double *) CALLOC(ns, sizeof(double));
      asfPrintStatus("   Data type: REAL64\n");
      break;
    case COMPLEX_BYTE:
      cpx_byte_buf = (unsigned char *) CALLOC(2*ns*alc, sizeof(unsigned char));
      tmp_byte_buf =  (unsigned char *) CALLOC(2*ns*alc, sizeof(unsigned char));
      asfPrintStatus("   Data type: COMPLEX_BYTE\n");
      break;
    case COMPLEX_INTEGER16:
      cpx_short_buf = (short *) CALLOC(2*ns*alc, sizeof(short));
      tmp_cpx_short_buf = (short *) CALLOC(2*ns*alc, sizeof(short));
      asfPrintStatus("   Data type: COMPLEX_INTEGER16\n");
      break;
    case COMPLEX_INTEGER32:
      cpx_int_buf = (int *) CALLOC(2*ns*alc, sizeof(int));
      tmp_int_buf = (int *) CALLOC(2*ns*alc, sizeof(int));
      asfPrintStatus("   Data type: COMPLEX_INTEGER32\n");
      break;
    case COMPLEX_REAL32:
      cpx_float_buf = (float *) CALLOC(2*ns*alc, sizeof(float));
      tmp_float_buf = (float *) CALLOC(2*ns*alc, sizeof(float));
      asfPrintStatus("   Data type: COMPLEX_REAL32\n");
      break;
    case COMPLEX_REAL64:
      cpx_double_buf = (double *) CALLOC(2*ns*alc, sizeof(float));
      tmp_double_buf = (double *) CALLOC(2*ns*alc, sizeof(float));
      asfPrintStatus("   Data type: COMPLEX_REAL64\n");
      break;
  }

  // Allocate memory for output buffers
  switch (data_type) {
    case ASF_BYTE:
        amp_float_buf = (float *) CALLOC(ns*alc, sizeof(float));
        break;
    case INTEGER16:
    case INTEGER32:
    case REAL32:
    case REAL64:
        amp_float_buf = (float *) CALLOC(ns*alc, sizeof(float));
        break;
    case COMPLEX_BYTE:
    case COMPLEX_INTEGER16:
    case COMPLEX_INTEGER32:
    case COMPLEX_REAL32:
        if (complex_flag && !multilook_flag) {
	  cpxFloat_buf = (complexFloat *) CALLOC(ns*alc, sizeof(complexFloat));
        }
        else {
            cpx_float_ml_buf = (complexFloat *) CALLOC(2*ns*alc, sizeof(float));
            amp_float_buf = (float *) CALLOC(ns*alc, sizeof(float));
            phase_float_buf = (float *) CALLOC(ns*alc, sizeof(float));
        }
        break;
    case COMPLEX_REAL64:
        break;
  }

  // Figure out left fill and right fill
  if ((strcmp(meta->general->sensor, "ALOS") == 0 && meta->optical) ||
      strncmp_case(meta->general->processor, "CSTARS", 6) == 0) {
    get_ALOS_optical_ifiledr(inMetaName,&image_fdr);
    leftFill = image_fdr.predata + image_fdr.lbrdrpxl;
    rightFill = image_fdr.sufdata + image_fdr.rbrdrpxl;
  }
  else {
    get_ifiledr(inMetaName,&image_fdr);
    leftFill = image_fdr.lbrdrpxl;
    rightFill = image_fdr.rbrdrpxl;
  }
  headerBytes = firstRecordLen(inDataName) +
                (image_fdr.reclen - (ns + leftFill + rightFill) * image_fdr.bytgroup);
  meta->general->sample_count -= leftFill;

  // Set metadata for multilooking
  if (meta->sar) {
      if (data_type >= COMPLEX_BYTE) {
          meta->sar->multilook = 0;
      }
      else {
          meta->sar->multilook = 1;
      }

      // If multilook flag is true but data is already multilooked, turn off flag
      if (multilook_flag && meta->sar && meta->sar->multilook) {
          multilook_flag = FALSE;
      }

      if (multilook_flag) {
	meta->general->line_count = 
	  (int)((float)nl / (float)nAzimuthLooks);
	meta->general->sample_count =
	  (int)((float)ns / (float)nRangeLooks);
	meta->general->y_pixel_size *= nAzimuthLooks;
	meta->general->x_pixel_size *= nRangeLooks;
	meta->sar->azimuth_time_per_pixel *= nAzimuthLooks;
	meta->sar->range_time_per_pixel *= nRangeLooks;
	meta->sar->multilook = 1;
	if (strcmp_case(meta->general->sensor, "ALOS") == 0) {
	  meta->general->line_scaling = nAzimuthLooks;
	  meta->general->sample_scaling = nRangeLooks;
	}
      }
  }

  // Populate incidence angle array for calibration
  //
  // 1. If the image is not geocoded, then this array will be sample_count long
  //    and will contain one incidence angle for each pixel.
  // 2. If the image IS geocoded however, this array will contain 11 coefficients
  //    for a 2D quadratic fit
  if (meta->sar) {
    incid = incid_init(meta);
  }

  // Check whether image needs to be flipped
  if (meta->general->orbit_direction == 'D' &&
      (!meta->projection || meta->projection->type != SCANSAR_PROJECTION) &&
      (strncmp_case(meta->general->processor, "CDPF", 4) == 0 ||
       strncmp_case(meta->general->processor, "RSI", 3) == 0 ||
       strncmp_case(meta->general->processor, "KACST", 5) == 0))
  {
    flip = TRUE;
  }

  struct dataset_sum_rec dssr;
  get_dssr(inDataName, &dssr);
  if (strncmp(dssr.time_dir_pix,"DECREASE",8)==0) {
    flip = TRUE;
  }

  if (data_type >= COMPLEX_BYTE) {
    float gain_adj = 0.0;
    if (apply_ers2_gain_fix_flag)
        gain_adj = get_ers2_gain_adj(meta,radiometry);

    // Go through complex imagery in chunks
    for (ii = 0; ii < nl; ii += nAzimuthLooks) {
      alc = nAzimuthLooks;
      if (ii + alc > nl) {
        if (multilook_flag)
          break;
        alc = nl - ii;
      }

      for (ll = 0; ll < alc; ll++) {
        int line = ii+ll;
        asfLineMeter(line, nl);
        offset = (long long)headerBytes+ (ii+ll)*(long long)image_fdr.reclen;
        FSEEK64(fpIn, offset, SEEK_SET);

        // Read the data according to their data type
        switch (data_type) {
          case COMPLEX_BYTE:
            ASF_FREAD(cpx_byte_buf+2*ll*ns, sizeof(unsigned char), 2*ns, fpIn);
            break;
          case COMPLEX_INTEGER16:
            ASF_FREAD(cpx_short_buf+2*ll*ns, sizeof(short), 2*ns, fpIn);
            break;
          case COMPLEX_INTEGER32:
            ASF_FREAD(cpx_int_buf+2*ll*ns, sizeof(int), 2*ns, fpIn);
            break;
          case COMPLEX_REAL64:
            ASF_FREAD(cpx_double_buf+2*ll*ns, sizeof(double), 2*ns, fpIn);
            break;
          case COMPLEX_REAL32:
            ASF_FREAD(cpx_float_buf+2*ll*ns, sizeof(float), 2*ns, fpIn);
            break;
          case ASF_BYTE:
          case INTEGER16:
          case INTEGER32:
          case REAL32:
          case REAL64:
            break;
        }

        // Put read in data in proper endian format
        for (kk=0; kk<ns; kk++) {
          switch (data_type) {
            case COMPLEX_BYTE:
              break;
            case COMPLEX_INTEGER16:
              big16(cpx_short_buf[ll*ns*2 + kk*2]);
              big16(cpx_short_buf[ll*ns*2 + kk*2+1]);
              if (flip) {
                tmp_cpx_short_buf[ll*ns*2 + kk*2] = 
		  cpx_short_buf[ll*ns*2 + kk*2];
                tmp_cpx_short_buf[ll*ns*2 + kk*2+1] = 
		  cpx_short_buf[ll*ns*2 * kk*2+1];
              }
              break;
            case COMPLEX_INTEGER32:
              big32(cpx_int_buf[ll*ns*2 + kk*2]);
              big32(cpx_int_buf[ll*ns*2 + kk*2+1]);
              if (flip) {
                tmp_int_buf[ll*ns*2 + kk*2] = cpx_int_buf[ll*ns*2 + kk*2];
                tmp_int_buf[ll*ns*2 + kk*2+1] = cpx_int_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_REAL32:
              big32(cpx_float_buf[ll*ns*2 + kk*2]);
              big32(cpx_float_buf[ll*ns*2 + kk*2+1]);
              if (flip) {
                tmp_float_buf[ll*ns*2 + kk*2] = 
		  cpx_float_buf[ll*ns*2 + kk*2];
                tmp_float_buf[ll*ns*2 + kk*2+1] = 
		  cpx_float_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_REAL64:
              big64(cpx_double_buf[ll*ns*2 + kk*2]);
              big64(cpx_double_buf[ll*ns*2 + kk*2+1]);
              if (flip) {
                tmp_double_buf[ll*ns*2 + kk*2] = 
		  cpx_double_buf[ll*ns*2 + kk*2];
                tmp_double_buf[ll*ns*2 + kk*2+1] = 
		  cpx_double_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case ASF_BYTE:
            case INTEGER16:
            case INTEGER32:
            case REAL32:
            case REAL64:
              break;
          }
        }

        // Flip the line if necessary and assign output value
        for (kk=0; kk<ns; kk++) {
          cpx.real = cpx.imag = 0.0;
          switch (data_type)
          {
            case ASF_BYTE:
            case INTEGER16:
            case INTEGER32:
            case REAL32:
            case REAL64:
              // should not happen
              break;
            case COMPLEX_BYTE:
              if (flip) {
                cpx.real = (float) cpx_byte_buf[ll*ns*2 + (ns-kk-1)*2];
                cpx.imag = (float) cpx_byte_buf[ll*ns*2 + (ns-kk-1)*2+1];
              }
              else {
                cpx.real = (float) cpx_byte_buf[ll*ns*2 + kk*2];
                cpx.imag = (float) cpx_byte_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_INTEGER16:
              if (flip) {
                cpx.real = (float) cpx_short_buf[ll*ns*2 + (ns-kk-1)*2];
                cpx.imag = (float) cpx_short_buf[ll*ns*2 + (ns-kk-1)*2+1];
              }
              else {
                cpx.real = (float) cpx_short_buf[ll*ns*2 + kk*2];
                cpx.imag = (float) cpx_short_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_INTEGER32:
              if (flip) {
                cpx.real = (float) cpx_int_buf[ll*ns*2 + (ns-kk-1)*2];
                cpx.imag = (float) cpx_int_buf[ll*ns*2 + (ns-kk-1)*2+1];
              }
              else {
                cpx.real = (float) cpx_int_buf[ll*ns*2 + kk*2];
                cpx.imag = (float) cpx_int_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_REAL32:
              if (flip) {
                cpx.real = cpx_float_buf[ll*ns*2 + (ns-kk-1)*2];
                cpx.imag = cpx_float_buf[ll*ns*2 + (ns-kk-1)*2+1];
              }
              else {
                cpx.real = cpx_float_buf[ll*ns*2 + kk*2];
                cpx.imag = cpx_float_buf[ll*ns*2 + kk*2+1];
              }
              break;
            case COMPLEX_REAL64:
              if (flip) {
                cpx.real = (float) cpx_double_buf[ll*ns*2 + (ns-kk-1)*2];
                cpx.imag = (float) cpx_double_buf[ll*ns*2 + (ns-kk-1)*2+1];
              }
              else {
                cpx.real = (float) cpx_double_buf[ll*ns*2 + kk*2];
                cpx.imag = (float) cpx_double_buf[ll*ns*2 + kk*2+1];
              }
              break;
          }

          if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
            fValue = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
            if (multilook_flag) {
              cpx_float_ml_buf[ll*ns + kk].real = cpx.real;
              cpx_float_ml_buf[ll*ns + kk].imag = cpx.imag;
            }
            else {
                double incidence_angle;
                if (projected) {
		  incidence_angle = quadratic_2_incidence_angle(ll, kk, incid);
                }
                else {
		  incidence_angle = incid[kk];
                }
                amp_float_buf[ll*ns + kk] = 
		  get_cal_dn(meta, incidence_angle, kk, fValue, bandExt, 
			     db_flag);
                phase_float_buf[ll*ns + kk] =  atan2(cpx.imag, cpx.real);
            }
          }
          else if (complex_flag && !multilook_flag) {
            cpxFloat_buf[ll*ns + kk] = cpx;
          }
          else if (cpx.real != 0.0 || cpx.imag != 0.0) {
            if (multilook_flag) {
              cpx_float_ml_buf[ll*ns + kk].real = cpx.real;
              cpx_float_ml_buf[ll*ns + kk].imag = cpx.imag;
            }
            else {
              amp_float_buf[ll*ns + kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
              phase_float_buf[ll*ns + kk] =  atan2(cpx.imag, cpx.real);
            }
          }
          else {
            if (multilook_flag) {
              cpx_float_ml_buf[ll*ns + kk].real = 0.0;
              cpx_float_ml_buf[ll*ns + kk].imag = 0.0;
            }
            else {
              amp_float_buf[ll*ns + kk] = 0.0;
              phase_float_buf[ll*ns + kk] = 0.0;
            }
          }
        }
      }

      // Multilook if requested
      if (multilook_flag) {
	for (kk = 0; kk < ns; kk += nRangeLooks) {
	  int idx = kk/rlc;
	  rlc = nRangeLooks;
	  if (kk + rlc > ns) {
	    rlc = ns - kk;
	  }
	  float amp = 0.0;
	  cpx.real = cpx.imag = 0;
	  for (mm = 0; mm < alc; mm ++) {
	    for (nn = 0; nn < rlc; nn++) {
	      float re = cpx_float_ml_buf[mm*ns + kk + nn].real;
	      float im = cpx_float_ml_buf[mm*ns + kk + nn].imag;
	      cpx.real += re;
	      cpx.imag += im;
	      amp += re*re + im*im;
	    }
	  }
	  amp /= (float)(alc*rlc); // Average of the squares
	  cpx.real /= (float)(alc*rlc);
	  cpx.imag /= (float)(alc*rlc);
	  cpx_float_ml_buf[idx] = cpx;
	  if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
	    double incidence_angle;
	    if (projected)
	      incidence_angle = quadratic_2_incidence_angle(ll, kk+nn/2, incid);
	    else
	      incidence_angle = incid[kk];
	    amp_float_buf[idx] = 
	      get_cal_dn(meta, incidence_angle, kk+nn/2, sqrt(amp), bandExt, 
			 db_flag);
	  }
	  else 
	    amp_float_buf[idx] = sqrt(amp);
	  phase_float_buf[idx] = atan2(cpx.imag, cpx.real);
	}
      }
      
      int out_band = import_single_band ? 0 : (band - 1) * 2;

      if (apply_ers2_gain_fix_flag && strcmp(meta->general->sensor,"ERS2") == 0)
      {
        if (radiometry != r_AMP) {
          if (complex_flag) {
            // in the complex case, we need to convert back to polar, apply
            // the correction to the amplitude, then convert back to cartesian
            for (kk=0; kk<ns*alc; ++kk) {
              complexFloat cpx = cpxFloat_buf[kk];
              double amp = hypot(cpx.real, cpx.imag);
              double phase = atan2(cpx.imag, cpx.real);
              amp = apply_ers2_gain_fix(radiometry, gain_adj, amp);
              cpxFloat_buf[kk].real = amp*cos(phase);
              cpxFloat_buf[kk].imag = amp*sin(phase);
            }
          }
          else {
            int array_len = multilook_flag ? ns : ns*alc;
            for (kk=0; kk<array_len; ++kk)
              amp_float_buf[kk] =
                apply_ers2_gain_fix(radiometry, gain_adj, amp_float_buf[kk]);
          }
        }
      }

      if (multilook_flag) {
	if (complex_flag) {
	  put_band_complexFloat_line(fpOut, meta, out_band, out, 
				     cpx_float_ml_buf);
	  out++;
	}
	else {
	  put_band_float_line(fpOut, meta, out_band+0, out, amp_float_buf);
	  if (!(amp0_flag && out_band==0))
	    put_band_float_line(fpOut, meta, out_band+1, out, phase_float_buf);
	  out++;
	}
      }
      else {
        for (mm=0; mm<alc; mm++) {
          if (complex_flag) {
            put_band_complexFloat_line(fpOut, meta, out_band, ii+mm, 
				       cpxFloat_buf+mm*ns);
          }
          else {
            put_band_float_line(fpOut, meta, out_band+0, ii+mm,
                                amp_float_buf+mm*ns);
            if (!(amp0_flag && out_band==0)) {
              put_band_float_line(fpOut, meta, out_band+1, ii+mm,
                                  phase_float_buf+mm*ns);
            }
          }
        }
      }
    }
  }
  else {
    // Go through detected imagery line by line
    float gain_adj = 0.0;
    if (apply_ers2_gain_fix_flag)
        gain_adj = get_ers2_gain_adj(meta,radiometry);

    for (ii=0; ii<nl; ii++) {
      asfLineMeter(ii, nl);

      offset = (long long)headerBytes+ ii*(long long)image_fdr.reclen;
      FSEEK64(fpIn, offset, SEEK_SET);

      // Read the data according to their data type
      switch (data_type) {
        case REAL32:
            ASF_FREAD(float_buf, sizeof(float), ns, fpIn);
            break;
        case ASF_BYTE:
            ASF_FREAD(byte_buf, sizeof(unsigned char), ns, fpIn);
            break;
        case INTEGER16:
            ASF_FREAD(short_buf, sizeof(short), ns, fpIn);
            break;
        case INTEGER32:
            ASF_FREAD(int_buf, sizeof(int), ns, fpIn);
            break;
        case REAL64:
            ASF_FREAD(double_buf, sizeof(double), ns, fpIn);
            break;
        case COMPLEX_BYTE:
            ASF_FREAD(cpx_byte_buf, sizeof(unsigned char), 2*ns, fpIn);
            break;
        case COMPLEX_INTEGER16:
        case COMPLEX_INTEGER32:
        case COMPLEX_REAL32:
        case COMPLEX_REAL64:
            break;
      }

      // Put read in data in proper endian format
      for (kk=0; kk<ns; kk++) {
          switch (data_type) {
            case REAL32:
                big32(float_buf[kk]);
                if (flip) {
                    tmp_float_buf[kk] = float_buf[kk];
                }
                break;
            case ASF_BYTE:
                if (flip) {
                    tmp_byte_buf[kk] = byte_buf[kk];
                }
                break;
            case INTEGER16:
                big16(short_buf[kk]);
                if (flip) {
                    tmp_short_buf[kk] = short_buf[kk];
                }
                break;
            case INTEGER32:
                big32(int_buf[kk]);
                if (flip) {
                    tmp_int_buf[kk] = int_buf[kk];
                }
                break;
            case REAL64:
                big64(double_buf[kk]);
                if (flip) {
                    tmp_double_buf[kk] = double_buf[kk];
                }
                break;
            case COMPLEX_BYTE:
            case COMPLEX_INTEGER16:
            case COMPLEX_INTEGER32:
            case COMPLEX_REAL32:
            case COMPLEX_REAL64:
                break;
        }
      }

      // Flip the line if necessary and assign output value
      for (kk = 0; kk < ns; kk++) {
        if (lutName) {
            double incidence_angle;
            int x = ii, y = kk;
            if (projected) {
                incidence_angle = quadratic_2_incidence_angle(x, y, incid);
            }
            else {
                incidence_angle = incid[kk]; //meta_incid(meta, x, y);
            }
            for (mm = min; mm <= max; mm++) {
                if (incidence_angle < incid_table[mm]) {
                    break;
                }
                if (data_type == ASF_BYTE) {
                    if (flip) {
                        byte_buf[kk] = tmp_byte_buf[ns-kk-1];
                    }
                    amp_float_buf[kk] = (float) byte_buf[kk] *
                                        (((scale_table[mm]-scale_table[mm]) /
                                        (incid_table[mm]-incid_table[mm])) *
                                        (incidence_angle - incid_table[mm-1]) * scale_table[mm-1]);
                }
                else if (data_type == INTEGER16) {
                    if (flip) {
                        short_buf[kk] = tmp_short_buf[ns-kk-1];
                    }
                    amp_float_buf[kk] = (float) short_buf[kk] *
                                        (((scale_table[mm]-scale_table[mm]) /
                                        (incid_table[mm]-incid_table[mm])) *
                                        (incidence_angle - incid_table[mm-1]) * scale_table[mm-1]);
                }
                else {
                    asfPrintStatus("LUT not implemented for this data type!\n");
                }
            }
        }
        else {
            switch (data_type) {
                case ASF_BYTE:
                    if (flip) {
                        byte_buf[kk] = tmp_byte_buf[ns-kk-1];
                    }
                    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
                        double incidence_angle;
                        if (projected) {
                            incidence_angle = quadratic_2_incidence_angle(ii, kk, incid);
                        }
                        else {
                            incidence_angle = incid[kk];
                        }
                        amp_float_buf[kk] = 
			  get_cal_dn(meta, incidence_angle, kk, 
				     (float)byte_buf[kk], bandExt, db_flag);
                    }
                    else if (radiometry == r_POWER) {
                        amp_float_buf[kk] = (float) byte_buf[kk]*byte_buf[kk];
                    }
                    else if (strcmp(meta->general->sensor, "ALOS") == 0 &&
                             meta->optical)
                    {
                        if (kk+leftFill>=ns) {
                            amp_float_buf[kk] = 0.0;
                        } else {
                            amp_float_buf[kk] = (float) byte_buf[kk+leftFill];
                        }
                    }
                    else {
                        amp_float_buf[kk] = (float) byte_buf[kk];
                    }
                    break;
                case INTEGER16:
                    if (flip) {
                        short_buf[kk] = tmp_short_buf[ns-kk-1];
                    }
                    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
                        double incidence_angle;
                        if (projected) {
                            incidence_angle = quadratic_2_incidence_angle(ii, kk, incid);
                        }
                        else {
                            incidence_angle = incid[kk];
                        }
                        amp_float_buf[kk] = 
			  get_cal_dn(meta, incidence_angle, kk, 
				     (float)short_buf[kk], bandExt, db_flag);
                    }
                    else if (radiometry == r_POWER) {
                        amp_float_buf[kk] = (float) short_buf[kk]*short_buf[kk];
                    }
                    else {
                        amp_float_buf[kk] = (float) short_buf[kk];
                    }
                    break;
                case INTEGER32:
                    if (flip) {
                        int_buf[kk] = tmp_int_buf[ns-kk-1];
                    }
                    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
                        double incidence_angle;
                        if (projected) {
                            incidence_angle = quadratic_2_incidence_angle(ii, kk, incid);
                        }
                        else {
                            incidence_angle = incid[kk];
                        }
                        amp_float_buf[kk] = 
			  get_cal_dn(meta, incidence_angle, kk, 
				     (float)int_buf[kk], bandExt, db_flag);
                    }
                    else if (radiometry == r_POWER) {
                        amp_float_buf[ns+kk] = (float) int_buf[kk]*int_buf[kk];
                    }
                    else {
                        amp_float_buf[ns+kk] = (float) int_buf[kk];
                    }
                    break;
                case REAL32:
                    if (flip) {
                        float_buf[kk] = tmp_float_buf[ns-kk-1];
                    }
                    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
                        double incidence_angle;
                        if (projected) {
                            incidence_angle = quadratic_2_incidence_angle(ii, kk, incid);
                        }
                        else {
                            incidence_angle = incid[kk];
                        }
                        amp_float_buf[kk] = 
			  get_cal_dn(meta, incidence_angle, kk, float_buf[kk], 
				     bandExt, db_flag);
                    }
                    else if (radiometry == r_POWER) {
                        amp_float_buf[kk] = float_buf[kk]*float_buf[kk];
                    }
                    else {
                        amp_float_buf[kk] = float_buf[kk];
                    }
                    break;
                case REAL64:
                    if (flip) {
                        double_buf[kk] = tmp_double_buf[ns-kk-1];
                    }
                    if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
                        double incidence_angle;
                        if (projected) {
                            incidence_angle = quadratic_2_incidence_angle(ii, kk, incid);
                        }
                        else {
                            incidence_angle = incid[kk];
                        }
                        amp_float_buf[kk] = 
			  get_cal_dn(meta, incidence_angle, kk, 
				     (float)double_buf[kk], bandExt, db_flag);
                    }
                    else if (radiometry == r_POWER) {
                        amp_float_buf[kk] = (float) double_buf[kk]*double_buf[kk];
                    }
                    else {
                        amp_float_buf[kk] = (float) double_buf[kk];
                    }
                    break;
                case COMPLEX_BYTE:
                case COMPLEX_INTEGER16:
                case COMPLEX_INTEGER32:
                case COMPLEX_REAL32:
                case COMPLEX_REAL64:
                    break;
            }
        }
            if (strcmp(meta->general->sensor,"ERS2") == 0 && apply_ers2_gain_fix_flag)
            amp_float_buf[kk] =
                apply_ers2_gain_fix(radiometry, gain_adj, amp_float_buf[kk]);
      }
      if (import_single_band) {
          put_band_float_line(fpOut, meta, 0, ii, amp_float_buf);
      }
      else {
          // This only works if the bands start with number 1
          // Not necessarily the case
          put_band_float_line(fpOut, meta, band-1, ii, amp_float_buf);
      }
    }
  }

  if (import_single_band || band == nBandsOut) {
    FCLOSE(fpOut);
    fpOut = NULL;
    output_file_closed = TRUE;
  }

  // Set radiometry
  meta->general->radiometry = radiometry;

  // Ensure that the band count always matches the number of bands in
  // the bands string -- to do that, we just count the commas
  meta->general->band_count = 1 + count_char(meta->general->bands, ',');

  // Save the metadata
  meta_write(meta, outMetaName);

  /* for debugging
     if (isPP(meta))
     {
     asfPrintStatus("PP Earth Radius: %.3f\n", meta->sar->earth_radius_pp);
     asfPrintStatus("  (for comparison) Scene Center Earth Radius: %.3f\n",
     meta->sar->earth_radius);
     }
  */

  // Clean up
  if (incid)
    FREE(incid);
  if (byte_buf) {
    FREE(byte_buf);
    FREE(tmp_byte_buf);
  }
  if (short_buf) {
    FREE(short_buf);
    FREE(tmp_short_buf);
  }
  if (int_buf) {
    FREE(int_buf);
    FREE(tmp_int_buf);
  }
  if (float_buf) {
    FREE(float_buf);
    FREE(tmp_float_buf);
  }
  if (double_buf) {
    FREE(double_buf);
    FREE(tmp_double_buf);
  }
  if (cpx_byte_buf) {
    FREE(cpx_byte_buf);
    FREE(tmp_byte_buf);
  }
  if (cpx_short_buf) {
    FREE(cpx_short_buf);
    FREE(tmp_cpx_short_buf);
  }
  if (cpx_int_buf) {
    FREE(cpx_int_buf);
    FREE(tmp_int_buf);
  }
  if (cpx_float_buf) {
    FREE(cpx_float_buf);
    FREE(tmp_float_buf);
  }
  if (cpx_double_buf) {
    FREE(cpx_double_buf);
    FREE(tmp_double_buf);
  }
  if (amp_byte_buf) {
    FREE(amp_byte_buf);
  }
  if (cpxFloat_buf) {
    FREE(cpxFloat_buf);
  }
  if (amp_float_buf) {
    FREE(amp_float_buf);
  }
  if (phase_float_buf) {
    FREE(phase_float_buf);
  }
  if (cpx_float_ml_buf) {
    FREE(cpx_float_ml_buf);
  }

  meta_free(meta);

  FCLOSE(fpIn);
}

double quadratic_2_incidence_angle(long long x, long long y, float *q)
{
    // x = line number
    // y = sample (pixel) number
    // q = array of 2D quadratic polynomial coefficients
    float incidence_angle = q[0]       +
                            q[1]*x     + q[2]*y     +
                            q[3]*x*x   + q[4]*x*y   + q[5]*y*y     +
                            q[6]*x*x*y + q[7]*x*y*y + q[8]*x*x*y*y +
                            q[9]*x*x*x + q[10]*y*y*y;

    return incidence_angle;
}

// Read CEOS metadata without touching the data file
meta_parameters *meta_read_only(const char *in_fName)
{
  meta_parameters *meta = raw_init();
  report_level_t level = REPORT_LEVEL_NONE;
  ceos_description *ceos = get_ceos_description_ext(in_fName, level, FALSE);

  if (ceos->sensor == SAR || ceos->sensor == PALSAR)
    ceos_init_sar_ext(ceos, in_fName, meta, TRUE);
  else if (ceos->sensor == AVNIR || ceos->sensor == PRISM)
    ceos_init_optical(in_fName, meta);

  FREE(ceos);
  return meta;
}

// Read CEOS metadata for raw data
meta_parameters *meta_read_raw(const char *inFile_)
{
  char inFile[1024];
  if (strlen(inFile_)>1023) asfPrintError("Filename is too long: %s\n", inFile_);
  strcpy(inFile, inFile_);
 
  double re, ht, fs, prf, vel;
  int trash;
  meta_parameters *meta = raw_init();
  report_level_t level = REPORT_LEVEL_NONE;
  ceos_description *ceos = get_ceos_description(inFile, level);
  bin_state *s;
  iqType *iqBuf; 
  readPulseFunc readNextPulse;
  char *baseName, tmpDir[1024], outFile[1024];

  ceos_init_sar_ext(ceos, inFile, meta, FALSE);
  meta->general->image_data_type = RAW_IMAGE;
  meta->general->radiometry = r_AMP;

  // PP Earth Radius Kludge
  if (isPP(meta)) {
      double pp_er, pp_atpp;
      pp_get_corrected_vals(inFile, &pp_er, &pp_atpp);
      if (meta->sar) 
	meta->sar->earth_radius_pp = pp_er;
  }

  re = meta->sar->earth_radius;
  ht = meta->sar->satellite_height - meta->sar->earth_radius;

  if (ceos->sensor != PALSAR) {
    baseName = get_basename(inFile);
    strcpy(tmpDir, baseName);
    strcat(tmpDir, "-");
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
    sprintf(outFile, "%s/bogus.meta", tmpDir);
    s = convertMetadata_ceos(inFile, outFile, &trash, &readNextPulse);
    iqBuf = (iqType*)MALLOC(sizeof(iqType)*2*(s->nSamp));
    FILE *fpOut = FOPEN(outFile, "wb");
    getNextCeosLine(s->binary, s, inFile, outFile);
    s->nLines = 0;
    readNextPulse(s, iqBuf, inFile, outFile);
    FCLOSE(fpOut);
    s->nLines = meta->general->line_count;
    struct dataset_sum_rec dssr;
    get_dssr(inFile, &dssr);
    if (strncmp(s->satName, "JERS1", 5) == 0) {
      s->prf = dssr.prf;
      s->range_gate = dssr.rng_gate / 1000000.0;
    }
    if (strncmp(s->satName, "ERS1", 4) == 0) {
      double outputDelay=CONF_ERS1_rangePulseDelay;
      s->range_gate = s->dwp+9.0/s->prf-outputDelay;
    }
    if (meta->sar && strncmp(s->satName, "JERS1", 5) == 0 &&
	meta_is_valid_double(meta->sar->earth_radius) &&
	meta_is_valid_double(meta->sar->satellite_height)) {
      s->re = meta->sar->earth_radius;
      s->ht = meta->sar->satellite_height - meta->sar->earth_radius;
      s->vel = sqrt(9.821*s->re*(s->re/(s->ht+s->re)));
    }
    updateMeta(s, meta, NULL, 0);

    meta->sar->slant_range_first_pixel = s->range_gate * SPD_LIGHT / 2.0;
    meta->sar->range_sampling_rate = fs = s->fs;
    prf = s->prf;
    vel = s->vel;
    remove_dir(tmpDir);
  }
  else {
    prf = meta->sar->prf;
    fs = meta->sar->range_sampling_rate;
    vel = sqrt(9.821*re*re/(ht+re));
  }
  meta->general->x_pixel_size = 1.0 / fs * (SPD_LIGHT / 2.0);
  meta->general->y_pixel_size = 1.0 / prf * vel * (re / (re + ht));
  if (meta->general->center_latitude == 0.0 &&
      meta->general->center_longitude == 0.0)
    meta_get_latLon(meta, meta->general->line_count/2,
		    meta->general->sample_count/2, 0.0,
		    &meta->general->center_latitude,
		    &meta->general->center_longitude);
  meta_get_corner_coords(meta);

  return meta;
}
