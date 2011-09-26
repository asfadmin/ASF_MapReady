#include <asf_terrcorr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "vector.h"

// kludge, for testing
int skip_it(int kk) {
  return kk < 1500*3482 || kk > 2100*3482;
}

static float *
read_dem(meta_parameters *meta_dem, char *demImg)
{
  int ns = meta_dem->general->sample_count;
  int nl = meta_dem->general->line_count;
  float *demData = MALLOC(sizeof(float)*ns*nl);

  FILE *fp = FOPEN(demImg, "rb");

  int ii;
  for (ii=0; ii<nl; ++ii) {
    get_float_line(fp, meta_dem, ii, demData + ii*ns);
    asfLineMeter(ii,nl);
  }

  FCLOSE(fp);
  return demData;
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

static int num_tot = 0;
static int num_in = 0;

static float
bilinear_interp(float *demData, int nl, int ns, double l, double s)
{

  ++num_tot;

  if (l<0 || l>=nl || s<0 || s>=ns) {
    return 0;
  }

  ++num_in;

  int ix = (int)s;
  int iy = (int)l;

  float p00 = demData[ix   + ns*(iy  )];
  float p10 = demData[ix+1 + ns*(iy  )];
  float p01 = demData[ix   + ns*(iy+1)];
  float p11 = demData[ix+1 + ns*(iy+1)];

  float a00 = p00;
  float a10 = p10 - p00;
  float a01 = p10 - a00;
  float a11 = p00 - p01 - p10 + p11;

  return a00 + a10*(s-ix) + a01*(l-iy) + a11*(s-ix)*(l-iy);
}

static float height_at(float *heights, int nl, int ns, int line, int samp)
{
  assert(line>=-1);
  assert(samp>=-1);
  assert(line<=nl);
  assert(samp<=ns);
  int kk = (line+1)*(ns+2) + samp+1;
  assert(kk < (nl+2)*(ns+2));
  return heights[kk];
}

static void llh_at(float *lats, float *lons, float *heights,
                   int nl, int ns, int line, int samp,
                   double *lat, double *lon, double *h)
{
  assert(line>=-1);
  assert(samp>=-1);
  assert(line<=nl);
  assert(samp<=ns);
  int kk = (line+1)*(ns+2) + samp+1;
  assert(kk < (nl+2)*(ns+2));
  *lat = (double)(lats[kk]);
  *lon = (double)(lons[kk]);
  *h = (double)(heights[kk]);
}

static void
calculate_normals(meta_parameters *meta_in, meta_parameters *meta_dem,
                  float *demData, Vector **normals_ret, Vector **pos_ret,
                  float **heights_ret)
{
  int nl = meta_in->general->line_count;
  int ns = meta_in->general->sample_count;
  int dnl = meta_dem->general->line_count;
  int dns = meta_dem->general->sample_count;

  int map_nl = nl+2;
  int map_ns = ns+2;
  int size = map_nl*map_ns;
  int ii, jj, kk;

  meta_parameters *meta_tmp = meta_copy(meta_in);
  meta_tmp->general->sample_count = map_ns;
  meta_tmp->general->line_count = map_nl;

  Vector *normals = MALLOC(sizeof(Vector)*nl*ns);
  Vector *pos = MALLOC(sizeof(Vector)*nl*ns);

  float *lats = MALLOC(sizeof(float)*size);
  float *lons = MALLOC(sizeof(float)*size);
  float *heights = MALLOC(sizeof(float)*size);
  float *tmpbuf = MALLOC(sizeof(float)*map_ns);

  asfPrintStatus("Calculating lat/lons ...\n");
  FILE *fpTmp = FOPEN("lats.img", "wb");

  kk=0;
  for (ii=-1; ii<=nl; ++ii) {
    for (jj=-1; jj<=ns; ++jj) {
      assert(kk<size);
      if (skip_it(kk)) {
        lats[kk] = lons[kk] = 0;
      } else {
        double lat, lon;
        meta_get_latLon(meta_in, ii, jj, 0, &lat, &lon);
        lats[kk] = (float)lat;
        lons[kk] = (float)lon;
      }
      tmpbuf[jj+1] = lats[kk];
      ++kk;
    }
    asfLineMeter(ii,nl+1);
    put_float_line(fpTmp, meta_tmp, ii+1, tmpbuf);
  }
  FCLOSE(fpTmp);
  meta_write(meta_tmp, "lats.meta");

  asfPrintStatus("Calculating heights ...\n");

  fpTmp = FOPEN("heights.img", "wb");

  kk=0;
  for (ii=-1; ii<=nl; ++ii) {
    for (jj=-1; jj<=ns; ++jj) {
      assert(kk<size);
      tmpbuf[jj+1] = 0;
      if (skip_it(kk)) {
        heights[kk] = 0;
      } else {
        double lat, lon, line, samp, dummy;
        llh_at(lats, lons, heights, nl, ns, ii, jj, &lat, &lon, &dummy);
        meta_get_lineSamp(meta_dem, lat, lon, 0, &line, &samp);
        heights[kk] = bilinear_interp(demData, dnl, dns, line, samp);
      }
      tmpbuf[jj+1] = heights[kk];
      ++kk;
    }
    asfLineMeter(ii,nl+1);
    put_float_line(fpTmp, meta_tmp, ii+1, tmpbuf);
  }
  FCLOSE(fpTmp);
  meta_write(meta_tmp, "heights.meta");

  asfPrintStatus("Calculating terrain normals...\n");
  fpTmp = FOPEN("angles.img", "wb");
  for (jj=0; jj<ns+2; ++jj)
    tmpbuf[jj] = 0;
  put_float_line(fpTmp, meta_tmp, 0, tmpbuf);
  put_float_line(fpTmp, meta_tmp, nl+1, tmpbuf);

  kk=0;
  for (ii=0; ii<nl; ++ii) {
    tmpbuf[0] = tmpbuf[ns] = 0;
    for (jj=0; jj<ns; ++jj) {
      assert(kk<nl*ns);
      if (skip_it(kk)) {
        normals[kk].x = normals[kk].y = 0; normals[kk].z = 1;
        tmpbuf[jj+1] = 0;
      } else {
        Vector v1, v2, v;
        double lat, lon, h;

        llh_at(lats, lons, heights, nl, ns, ii-1, jj, &lat, &lon, &h);
        geodetic_to_ecef(lat, lon, h, &v1);
        llh_at(lats, lons, heights, nl, ns, ii+1, jj, &lat, &lon, &h);
        geodetic_to_ecef(lat, lon, h, &v);
        vector_subtract(&v1, &v);

        llh_at(lats, lons, heights, nl, ns, ii, jj-1, &lat, &lon, &h);
        geodetic_to_ecef(lat, lon, h, &v2);
        llh_at(lats, lons, heights, nl, ns, ii, jj+1, &lat, &lon, &h);
        geodetic_to_ecef(lat, lon, h, &v);
        vector_subtract(&v2, &v);

        Vector *n = vector_cross(&v2, &v1);
        vector_multiply(n, 1./vector_magnitude(n));

        llh_at(lats, lons, heights, nl, ns, ii, jj, &lat, &lon, &h);
        geodetic_to_ecef(lat, lon, h, &pos[kk]);

        geodetic_to_ecef(lat, lon, h, &v1);
        geodetic_to_ecef(lat, lon, h+100, &v);
        vector_subtract(&v1,&v);
        vector_multiply(&v1, 1./vector_magnitude(&v1));

        normals[kk] = *n;
        double ang = acos(vector_dot(n,&v1));
        tmpbuf[jj+1] = ang*R2D;

        vector_free(n);
      }
      ++kk;
    }
    asfLineMeter(ii, nl);
    put_float_line(fpTmp, meta_tmp, ii+1, tmpbuf);
  }
  FCLOSE(fpTmp);
  meta_write(meta_tmp, "angles.meta");

  FREE(lats);
  FREE(lons);
  FREE(tmpbuf);
  meta_free(meta_tmp);

  *normals_ret = normals;
  *pos_ret = pos;
  *heights_ret = heights;
}

static Vector get_satpos(meta_parameters *meta, int line)
{
  int ns = meta->general->sample_count;
  double lat, lon, h;
  meta_get_latLon(meta, line, ns/2, 0, &lat, &lon);
  h = meta_get_sat_height(meta,line,ns/2);

  Vector satpos;
  geodetic_to_ecef(lat, lon, h, &satpos);
  return satpos;
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
  if (corrImg) meta_corr = meta_read(inputMeta);

  asfPrintStatus("Reading DEM...\n");
  float *demData = read_dem(meta_dem, demImg);

  int ns = meta_in->general->sample_count;
  int nl = meta_in->general->line_count;
  int nb = meta_in->general->band_count;
  int ii, jj, kk;

  FILE *fpIn = FOPEN(inputImg, "rb");
  FILE *fpOut = FOPEN(outputImg, "wb");
  FILE *fpCorr = NULL;
  if (corrImg) fpCorr = FOPEN(corrImg, "wb");

  float *corr = MALLOC(sizeof(float)*ns);
  float *bufIn = MALLOC(sizeof(float)*ns);

  asfPrintStatus("Interpolating DEM ...\n");
  Vector *normals, *pos;
  float *heights;
  calculate_normals(meta_in, meta_dem, demData, &normals, &pos, &heights);
  FREE(demData);

  asfPrintStatus("Performing radiometric correction...\n");

  for (ii=0; ii<nl; ++ii) {
    Vector sat_pos = get_satpos(meta_in, ii);
    for (jj=0; jj<ns; ++jj) {
      kk = ii*ns + jj;
      if (skip_it(kk)) {
        corr[jj] = 1;
      } else {
        float h = height_at(heights, nl, ns, ii, jj);
        Vector *n = normals + kk;
        Vector *p = pos + kk;
        corr[jj] = calculate_correction(meta_in, ii, jj, h, &sat_pos, n, p);
        if (ii==6617) {
          printf("%d: %f\n", jj, corr[jj]);
        }
      }
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
  FREE(normals);
  FREE(pos);

  FREE(inputImg);
  FREE(inputMeta);
  FREE(demImg);
  FREE(demMeta);
  FREE(outputImg);
  FREE(outputMeta);
  FREE(maskImg);
  FREE(maskMeta);

  printf("Coverage: %.2f\n", 100.*num_in/(float)num_tot);
  return TRUE;
}


