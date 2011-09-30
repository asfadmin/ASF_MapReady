#include <asf_terrcorr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "vector.h"

static void push_next_height_line(float **localHeights, meta_parameters *meta_dem, FILE *dem_fp, int ns, int line)
{
  float *demLine = MALLOC(sizeof(float)*ns);
  get_float_line(dem_fp, meta_dem, line, demLine);
  FREE(localHeights[0]);
  localHeights[0] = localHeights[1];
  localHeights[1] = localHeights[2];
  localHeights[2] = demLine;
}

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

static Vector get_position(float **localHeights, meta_parameters *meta_in, int line, int samp)
{
  double lat, lon;
  Vector v;
  meta_get_latLon(meta_in, line, samp, 0, &lat, &lon);
  geodetic_to_ecef(lat, lon, localHeights[1][samp], &v);
  return v;
}

static Vector * calculate_normal(meta_parameters *meta, float **localHeights, int line, int sample)
{
  double lat, lon, h;
  Vector v1, v2, v;
  Vector *normal;

  h = localHeights[0][sample];
  meta_get_latLon(meta, line-1, sample, 0, &lat, &lon); 
  geodetic_to_ecef(lat, lon, h, &v1);

  h = localHeights[2][sample];
  meta_get_latLon(meta, line+1, sample, 0, &lat, &lon); 
  geodetic_to_ecef(lat, lon, h, &v);
  vector_subtract(&v1, &v);

  h = localHeights[1][sample-1];
  meta_get_latLon(meta, line, sample-1, 0, &lat, &lon); 
  geodetic_to_ecef(lat, lon, h, &v2);

  h = localHeights[1][sample+1];
  meta_get_latLon(meta, line, sample+1, 0, &lat, &lon); 
  geodetic_to_ecef(lat, lon, h, &v);
  vector_subtract(&v2, &v);

  normal = vector_cross(&v2, &v1);
  vector_multiply(normal, 1./vector_magnitude(normal));
  
  return normal;
}

static float
calculate_correction(meta_parameters *meta_in, int line, int samp, float h,
                     Vector *satpos, Vector *n, Vector *p)
{
  // R: vector from ground point (p) to satellite (satpos)
  Vector *R = vector_copy(satpos);
  vector_subtract(R, p);

  // x: vector from p to p_next (along the ground)
  //    so we use the next point in the image, but forced to the same height
  double lat, lon;
  meta_get_latLon(meta_in, line+1, samp, 0, &lat, &lon);
  Vector p_next;
  geodetic_to_ecef(lat, lon, h, &p_next);

  Vector *x = vector_copy(p);
  vector_subtract(x, &p_next);

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
  char *corrImg = "correction.img";
  char *corrMeta = "correction.meta";

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

  int ns = meta_in->general->sample_count;
  int nl = meta_in->general->line_count;
  int nb = meta_in->general->band_count;
  int dns = meta_dem->general->sample_count;
  int dnl = meta_dem->general->line_count;
  assert(ns == dns);
  assert(nl == dnl);

  float **localHeights = MALLOC(sizeof(float*)*3);
  memset(localHeights, 0, sizeof(float*)*3);

  FILE *fpIn = FOPEN(inputImg, "rb");
  FILE *fpOut = FOPEN(outputImg, "wb");
  FILE *fpCorr = NULL;
  if (corrImg) fpCorr = FOPEN(corrImg, "wb");
  FILE *dem_fp = FOPEN(demImg, "rb");

  float *corr = MALLOC(sizeof(float)*ns);
  float *bufIn = MALLOC(sizeof(float)*ns);

  int ii, jj, kk;
  for(ii = 1; ii < 3; ++ii) {
    float *demLine = MALLOC(sizeof(float)*dns);
    get_float_line(dem_fp, meta_dem, ii - 1, demLine);
    localHeights[ii] = demLine;
  }

  // We aren't applying the correction to the edges of the image
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, 0, bufIn);
    put_band_float_line(fpOut, meta_out, kk, 0, bufIn);
  }

  for(ii = 1; ii < nl - 1; ++ii) {
    push_next_height_line(localHeights, meta_dem, dem_fp, dns, ii+1);
    corr[0] = corr[ns-1] = 1;
    Vector satpos = get_satpos(meta_in, ii);
    for(jj = 1; jj < ns - 1; ++jj) {
      Vector * normal = calculate_normal(meta_in, localHeights, ii, jj);
      Vector position = get_position(localHeights, meta_in, ii, jj);
      corr[jj] = calculate_correction(meta_in, ii, jj, localHeights[1][jj], &satpos, normal, &position);
      vector_free(normal);
    }

    if (corrImg)
        put_float_line(fpCorr, meta_corr, ii, corr);

    for (kk=0; kk<nb; ++kk) {
      get_band_float_line(fpIn, meta_in, kk, ii, bufIn);
      for (jj=0; jj<ns; ++jj)
        bufIn[jj] *= corr[jj];
      put_band_float_line(fpOut, meta_out, kk, ii, bufIn);
    }

    asfLineMeter(ii, nl);
  }

    // We aren't applying the correction to the edges of the image
  for(kk = 0; kk < nb; ++kk) {
    get_band_float_line(fpIn, meta_in, kk, nl-1, bufIn);
    put_band_float_line(fpOut, meta_out, kk, nl-1, bufIn);
  }

  for(ii = 0; ii < 3; ++ii) {
    FREE(localHeights[ii]);
  }
  FREE(localHeights);

  FCLOSE(fpOut);
  FCLOSE(fpIn);
  if (fpCorr) FCLOSE(fpCorr);

  meta_write(meta_out, outputMeta);

  if (corrImg) {
    meta_write(meta_corr, corrMeta);
    meta_free(meta_corr);
  }

  meta_free(meta_out);
  meta_free(meta_in);
  meta_free(meta_dem);

  FREE(bufIn);
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

