#include "asf_sar.h"
#include "asf_raster.h"
#include "asf_meta.h"
#include "asf.h"
#include <assert.h>

int asf_calibrate(const char *inFile, const char *outFile, 
		  radiometry_t outRadiometry, int wh_scaleFlag)
{
  meta_parameters *metaIn = meta_read(inFile);
  meta_parameters *metaOut = meta_read(inFile);

  // Check for valid output radiometry
  if (outRadiometry == r_AMP || outRadiometry == r_POWER)
    asfPrintError("Invalid radiometry (%s) passed into calibration function!\n",
		  radiometry2str(outRadiometry));

  // Check whether output radiometry fits with Woods Hole scaling flag
  if (wh_scaleFlag && outRadiometry >= r_SIGMA && outRadiometry <= r_GAMMA)
    outRadiometry += 3;

  // This can only work if the image is in some SAR geometry
  if (metaIn->projection && metaIn->projection->type != SCANSAR_PROJECTION)
    asfPrintError("Can't apply calibration factors to map projected images\n");

  radiometry_t inRadiometry = metaIn->general->radiometry;
  asfPrintStatus("Calibrating %s image to %s image\n\n", 
		 radiometry2str(inRadiometry), radiometry2str(outRadiometry));
  // FIXME: This function should be able to remap between different
  //        radiometry projections.
  if (metaIn->general->radiometry != r_AMP)
    asfPrintError("Currently only AMPLITUDE as radiometry is supported!\n");

  metaOut->general->radiometry = outRadiometry;
  int dbFlag = FALSE;
  if (outRadiometry >= r_SIGMA && outRadiometry <= r_GAMMA)
    metaOut->general->no_data = 0.0001;
  if (outRadiometry >= r_SIGMA_DB && outRadiometry <= r_GAMMA_DB) {
    metaOut->general->no_data = -40.0;
    dbFlag = TRUE;
  }
  if (outRadiometry == r_SIGMA || outRadiometry == r_SIGMA_DB)
    metaOut->general->image_data_type = SIGMA_IMAGE;
  else if (outRadiometry == r_BETA || outRadiometry == r_BETA_DB)
    metaOut->general->image_data_type = BETA_IMAGE;
  else if (outRadiometry == r_GAMMA || outRadiometry == r_GAMMA_DB)
    metaOut->general->image_data_type = GAMMA_IMAGE;
  if (wh_scaleFlag)
    metaOut->general->data_type = BYTE;

  char *input = appendExt(inFile, ".img");
  char *output = appendExt(outFile, ".img");
  FILE *fpIn = FOPEN(input, "rb");
  FILE *fpOut = FOPEN(output, "wb");

  int dualpol = strncmp_case(metaIn->general->mode, "FBD", 3) == 0 ? 1 : 0;
  int band_count = metaIn->general->band_count;
  int sample_count = metaIn->general->sample_count;
  int line_count = metaIn->general->line_count;
  char **bands = 
    extract_band_names(metaIn->general->bands, band_count);

  float *bufIn = (float *) MALLOC(sizeof(float)*sample_count);
  float *bufOut = (float *) MALLOC(sizeof(float)*sample_count);
  float *bufIn2 = NULL, *bufOut2 = NULL, *bufOut3 = NULL;
  if (dualpol && wh_scaleFlag) {
    bufIn2 = (float *) MALLOC(sizeof(float)*sample_count);
    bufOut2 = (float *) MALLOC(sizeof(float)*sample_count);
    bufOut3 = (float *) MALLOC(sizeof(float)*sample_count);
    metaOut->general->band_count = 3;
    sprintf(metaOut->general->bands, "%s,%s,%s-%s", 
	    bands[0], bands[1], bands[0], bands[1]);
  }

  int ii, jj, kk;
  float cal_dn, cal_dn2;
  double incid;
  if (dualpol && wh_scaleFlag) {
    metaOut->general->image_data_type = RGB_STACK;
    for (ii=0; ii<line_count; ii++) {
      get_band_float_line(fpIn, metaIn, 0, ii, bufIn);
      get_band_float_line(fpIn, metaIn, 1, ii, bufIn2);
      for (jj=0; jj<sample_count; jj++) {
	// Taking the remapping of other radiometries out for the moment
	//if (inRadiometry >= r_SIGMA && inRadiometry <= r_BETA_DB)
	//bufIn[jj] = cal2amp(metaIn, incid, jj, bands[kk], bufIn[jj]);
	incid = meta_incid(metaIn, ii, jj);
	cal_dn = 
	  get_cal_dn(metaOut, incid, jj, bufIn[jj], bands[0], dbFlag);
	cal_dn2 = 
	  get_cal_dn(metaOut, incid, jj, bufIn2[jj], bands[1], dbFlag);
	if (FLOAT_EQUIVALENT(cal_dn, metaIn->general->no_data) ||
	    cal_dn == cal_dn2) {
	  bufOut[jj] = 0;
	  bufOut2[jj] = 0;
	  bufOut3[jj] = 0;
	}
	else {
	  bufOut[jj] = (cal_dn + 31) / 0.15 + 1.5;
	  bufOut2[jj] = (cal_dn2 + 31) / 0.15 + 1.5;
	  bufOut3[jj] = bufOut[jj] - bufOut2[jj];
	}
      }
      put_band_float_line(fpOut, metaOut, 0, ii, bufOut);
      put_band_float_line(fpOut, metaOut, 1, ii, bufOut2);
      put_band_float_line(fpOut, metaOut, 2, ii, bufOut3);
      asfLineMeter(ii, line_count);
    }
  }
  else {
    for (kk=0; kk<band_count; kk++) {
      for (ii=0; ii<line_count; ii++) {
	get_band_float_line(fpIn, metaIn, kk, ii, bufIn);
	for (jj=0; jj<sample_count; jj++) {
	  // Taking the remapping of other radiometries out for the moment
	  //if (inRadiometry >= r_SIGMA && inRadiometry <= r_BETA_DB)
	  //bufIn[jj] = cal2amp(metaIn, incid, jj, bands[kk], bufIn[jj]);
	  incid = meta_incid(metaIn, ii, jj);
	  cal_dn = 
	    get_cal_dn(metaOut, incid, jj, bufIn[jj], bands[kk], dbFlag);
	  if (wh_scaleFlag) {
	    if (FLOAT_EQUIVALENT(cal_dn, metaIn->general->no_data))
	      bufOut[jj] = 0;
	    else
	      bufOut[jj] = (cal_dn + 31) / 0.15 + 1.5;
	  }
	  else
	    bufOut[jj] = cal_dn;
	}
	put_band_float_line(fpOut, metaOut, kk, ii, bufOut);
	asfLineMeter(ii, line_count);
      }
      FREE(bands[kk]);
    }
  }
  meta_write(metaOut, outFile);
  meta_free(metaIn);
  meta_free(metaOut);
  FREE(bufIn);
  FREE(bufOut);
  if (dualpol) {
    FREE(bufIn2);
    FREE(bufOut2);
    FREE(bufOut3);
  }
  FREE(bands);

  return FALSE;
}
