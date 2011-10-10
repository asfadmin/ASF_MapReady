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

  char *input = appendExt(inFile, ".img");
  char *output = appendExt(outFile, ".img");
  FILE *fpIn = FOPEN(input, "rb");
  FILE *fpOut = FOPEN(output, "wb");

  float *bufIn = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
  float *bufOut = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
  char **bands = 
    extract_band_names(metaIn->general->bands, metaIn->general->band_count);

  int ii, jj, kk;
  double *incid = 
    (double *) MALLOC(sizeof(double)*metaIn->general->sample_count);
  for (ii=0; ii<metaIn->general->sample_count; ii++)
    incid[ii] = meta_incid(metaIn, 0, ii);
  for (kk=0; kk<metaIn->general->band_count; kk++) {
    for (ii=0; ii<metaIn->general->line_count; ii++) {
      get_band_float_line(fpIn, metaIn, kk, ii, bufIn);
      for (jj=0; jj<metaIn->general->sample_count; jj++) {
	// Taking the remapping of other radiometries out for the moment
	//if (inRadiometry >= r_SIGMA && inRadiometry <= r_BETA_DB)
	//bufIn[jj] = cal2amp(metaIn, incid, jj, bands[kk], bufIn[jj]);
	bufOut[jj] = 
	  get_cal_dn(metaOut, incid[jj], jj, bufIn[jj], bands[kk], dbFlag);
      }
      put_band_float_line(fpOut, metaOut, kk, ii, bufOut);
      asfLineMeter(ii, metaIn->general->line_count);
    }
    FREE(bands[kk]);
  }
  meta_write(metaOut, outFile);
  meta_free(metaIn);
  meta_free(metaOut);
  FREE(bufIn);
  FREE(bufOut);
  FREE(bands);

  return FALSE;
}
