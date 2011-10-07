#include <asf_raster.h>
#include <asf_terrcorr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "vector.h"

static void geodetic_to_ecef(double lat, double lon, double h, Vector *v)
{
  const double a = 6378144.0;    // GEM-06 Ellipsoid.
  const double e = 8.1827385e-2; // GEM-06 Eccentricity
  const double e2 = e*e;

  lat *= D2R;
  lon *= D2R;

  double sin_lat = sin(lat);
  double cos_lat = cos(lat);

  double f = sqrt(1 - e2*sin_lat*sin_lat);
  double af = a/f;

  v->x = (af + h)*cos_lat*cos(lon);
  v->y = (af + h)*cos_lat*sin(lon);
  v->z = (a*(1-e2)/f + h)*sin_lat;
}

static Vector get_satpos(meta_parameters *meta, int line)
{
  int ns = meta->general->sample_count;
  double t = meta_get_time(meta, line, ns/2);

  // find the state vector closest to the specified time
  int ii,closest_ii=0;
  double closest_diff=9999999;
  for (ii=0; ii<meta->state_vectors->vector_count; ++ii) {
    double diff = fabs(meta->state_vectors->vecs[ii].time - t);
    if (diff < closest_diff) {
      closest_ii = ii;
      closest_diff = diff;
    }
  }

  stateVector closest_vec = meta->state_vectors->vecs[closest_ii].vec;
  double closest_time = meta->state_vectors->vecs[closest_ii].time;

  stateVector stVec = propagate(closest_vec, closest_time, t);

  Vector satpos;
  satpos.x = stVec.pos.x;
  satpos.y = stVec.pos.y;
  satpos.z = stVec.pos.z;
  return satpos;
}

static void calculate_vectors_for_line(meta_parameters *meta_dem, meta_parameters *meta_img, int line, FILE *dem_fp, Vector **vectorLine, Vector *nextVectors)
{
  int jj;
  double lat, lon;
  int ns = meta_img->general->sample_count;
  float demLine[ns];

  get_float_line(dem_fp, meta_dem, line, demLine);

  for(jj = 0; jj < ns; ++jj) {
    Vector *v = MALLOC(sizeof(Vector));
    meta_get_latLon(meta_img, line, jj, 0, &lat, &lon);
    geodetic_to_ecef(lat, lon, demLine[jj], v);
    vectorLine[jj] = v;

    meta_get_latLon(meta_img, line+1, jj, 0, &lat, &lon);
    geodetic_to_ecef(lat, lon, demLine[jj], &nextVectors[jj]);
  }
}

static void push_next_vector_line(Vector ***localVectors, Vector *nextVectors, meta_parameters *meta_dem, meta_parameters *meta_img, FILE *dem_fp, int line)
{
  int ns = meta_img->general->sample_count;
  Vector **vectorsLine = MALLOC(sizeof(Vector*)*ns);
  calculate_vectors_for_line(meta_dem, meta_img, line, dem_fp, vectorsLine, nextVectors);

  int i;
  for(i = 0; i < ns; i++)
    if(localVectors[0] != NULL)
      vector_free(localVectors[0][i]);
  FREE(localVectors[0]);

  localVectors[0] = localVectors[1];
  localVectors[1] = localVectors[2];
  localVectors[2] = vectorsLine;
}

static Vector * calculate_normal(Vector ***localVectors, int sample)
{
  Vector *v1, *v2, *normal;

  v1 = vector_copy(localVectors[0][sample]);
  vector_subtract(v1, localVectors[2][sample]);

  v2 = vector_copy(localVectors[1][sample-1]);
  vector_subtract(v2, localVectors[1][sample+1]);

  normal = vector_cross(v2, v1);
  vector_multiply(normal, 1./vector_magnitude(normal));

  vector_free(v1);
  vector_free(v2);
  
  return normal;
}

static float
calculate_correction(meta_parameters *meta_in, int line, int samp,
                     Vector *satpos, Vector *n, Vector *p, Vector *p_next)
{
  // R: vector from ground point (p) to satellite (satpos)
  Vector *R = vector_copy(satpos);
  vector_subtract(R, p);

  Vector *x = vector_copy(p);
  vector_subtract(x, p_next);

  // Rx: R cross x -- image plane normal
  Vector *Rx = vector_cross(R,x);
  vector_multiply(Rx, 1./vector_magnitude(Rx));

  // cos(phi) is the correction factor we need
  double cosphi = vector_dot(Rx,n);
  if (cosphi < 0) cosphi = -cosphi;

  vector_free(x);
  vector_free(R);
  vector_free(Rx);

  // need to remove old correction factor (sin of the incidence angle)
  double incid = meta_incid(meta_in, line, samp);
  return cosphi / sin(incid);
}

int rtc(char *input_file, char *dem_file, int maskFlag, char *mask_file,
        char *output_file)
{
  asfPrintStatus("Input file: %s\n", input_file);
  asfPrintStatus("DEM: %s\n", dem_file);
  asfPrintStatus("Output file: %s\n", output_file);
  asfPrintStatus("Layover/shadow mask: %s\n\n",
                 maskFlag ? mask_file : "none");

  char *inputImg = appendExt(input_file, ".img");
  char *inputMeta = appendExt(input_file, ".meta");
  char *demImg = appendExt(dem_file, ".img");
  char *demMeta = appendExt(dem_file, ".meta");
  char *outputImg = appendExt(output_file, ".img");
  char *outputMeta = appendExt(output_file, ".meta");
  char *maskImg = maskFlag ? appendExt(mask_file, ".img") : NULL;
  char *maskMeta = maskFlag ? appendExt(mask_file, ".meta") : NULL;
  char *corrImg = "correction2.img";
  char *corrMeta = "correction2.meta";

  if (!fileExists(inputImg))
    asfPrintError("Not found: %s\n", inputImg);
  if (!fileExists(inputMeta))
    asfPrintError("Not found: %s\n", inputMeta);
  if (!fileExists(demImg))
    asfPrintError("Not found: %s\n", demImg);
  if (!fileExists(demMeta))
    asfPrintError("Not found: %s\n", demMeta);
  if (maskFlag) {
    if (!fileExists(maskImg))
      asfPrintError("Not found: %s\n", maskImg);
    if (!fileExists(maskMeta))
      asfPrintError("Not found: %s\n", maskMeta);
  }

  asfPrintStatus("Reading metadata...\n");
  meta_parameters *meta_in = meta_read(inputMeta);
  if (!meta_in) asfPrintError("Failed to read metadata: %s\n", inputMeta);
  meta_parameters *meta_out = meta_read(inputMeta);
  meta_parameters *meta_dem = meta_read(demMeta);
  if (!meta_dem) asfPrintError("Failed to read metadata: %s\n", demMeta);
  meta_parameters *meta_corr = NULL;
  if (corrImg) meta_corr = meta_read(demMeta);

  // Check the input radiometry - only accept amplitude
  if (meta_in->general->radiometry != r_AMP)
    asfPrintError("Radiometric correction requires amplitude images!\n");
  char **bands = 
    extract_band_names(meta_in->general->bands, meta_in->general->band_count);

  int ns = meta_in->general->sample_count;
  int nl = meta_in->general->line_count;
  int nb = meta_in->general->band_count;
  int dns = meta_dem->general->sample_count;
  int dnl = meta_dem->general->line_count;
  assert(ns == dns);
  assert(nl == dnl);

  Vector ***localVectors = MALLOC(sizeof(Vector**)*3);
  memset(localVectors, 0, sizeof(Vector**)*3);
  Vector nextVectors[ns];

  FILE *fpIn = FOPEN(inputImg, "rb");
  FILE *fpOut = FOPEN(outputImg, "wb");
  FILE *fpCorr = NULL;
  if (corrImg) fpCorr = FOPEN(corrImg, "wb");
  FILE *dem_fp = FOPEN(demImg, "rb");

  float *corr = MALLOC(sizeof(float)*ns);
  float *bufIn = MALLOC(sizeof(float)*ns);
  float *bufOut = MALLOC(sizeof(float)*ns);

  int ii, jj, kk;
  for(ii = 1; ii < 3; ++ii) {
    localVectors[ii] = MALLOC(sizeof(Vector**)*ns);
    calculate_vectors_for_line(meta_in, meta_dem, ii - 1, dem_fp, localVectors[ii], nextVectors);
  }

  double incid, sigma;
  // We aren't applying the correction to the edges of the image
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, 0, bufIn);
    for (jj=0; jj<ns; ++jj)
      bufOut[jj] = 
	get_rad_cal_dn(meta_in, 0, jj, bands[kk], bufIn[jj], corr[jj]);
    put_band_float_line(fpOut, meta_out, kk, 0, bufOut);
  }

  for(ii = 1; ii < nl - 1; ++ii) {
    push_next_vector_line(localVectors, nextVectors, meta_dem, meta_in, dem_fp, ii + 1);
    corr[0] = corr[ns-1] = 1;
    Vector satpos = get_satpos(meta_in, ii);
    for(jj = 1; jj < ns - 1; ++jj) {
      Vector * normal = calculate_normal(localVectors, jj);
      corr[jj] = calculate_correction(meta_in, ii, jj, &satpos, normal, localVectors[1][jj], &nextVectors[jj]);
      vector_free(normal);
    }

    if (corrImg)
        put_float_line(fpCorr, meta_corr, ii, corr);

    for (kk=0; kk<nb; ++kk) {
      get_band_float_line(fpIn, meta_in, kk, ii, bufIn);
      for (jj=0; jj<ns; ++jj)
	bufOut[jj] = 
	  get_rad_cal_dn(meta_in, ii, jj, bands[kk], bufIn[jj], corr[jj]);
      put_band_float_line(fpOut, meta_out, kk, ii, bufOut);
    }

    asfLineMeter(ii, nl);
  }

    // We aren't applying the correction to the edges of the image
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, nl-1, bufIn);
    for (jj=0; jj<ns; ++jj)
      bufOut[jj] = 
	get_rad_cal_dn(meta_in, nl-1, jj, bands[kk], bufIn[jj], corr[jj]);
    put_band_float_line(fpOut, meta_out, kk, nl-1, bufIn);
  }

  for(ii = 0; ii < 3; ++ii) {
    for(jj = 0; jj < ns; ++jj) {
      vector_free(localVectors[ii][jj]);
    }
    FREE(localVectors[ii]);
  }
  FREE(localVectors);

  FCLOSE(fpOut);
  FCLOSE(fpIn);
  if (fpCorr) FCLOSE(fpCorr);

  // update output metadata
  for (ii=0; ii<meta_out->general->band_count; ii++) {
    FREE(bands[ii]);
  }
  FREE(bands);
  meta_write(meta_out, outputMeta);

  if (corrImg) {
    meta_write(meta_corr, corrMeta);
    meta_free(meta_corr);
  }

  meta_free(meta_out);
  meta_free(meta_in);
  meta_free(meta_dem);

  FREE(bufIn);
  FREE(bufOut);
  FREE(corr);

  FREE(inputImg);
  FREE(inputMeta);
  FREE(demImg);
  FREE(demMeta);
  FREE(outputImg);
  FREE(outputMeta);
  FREE(maskImg);
  FREE(maskMeta);

  return TRUE;
}
