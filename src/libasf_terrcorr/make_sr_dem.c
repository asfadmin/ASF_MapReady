#include <asf_terrcorr.h>
#include <stdio.h>
#include <assert.h>
#include <gsl/gsl_spline.h>
#include <asf_raster.h>
#include "float_image.h"

static float *
read_dem(meta_parameters *meta_dem, const char *demImg)
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

static void
sar_to_dem(meta_parameters *meta_sar, meta_parameters *meta_dem,
           double line_sar, double samp_sar,
           double *line_dem, double *samp_dem)
{
  double lat, lon;
  int ret;

  ret = meta_get_latLon(meta_sar, line_sar, samp_sar, 0, &lat, &lon);
  if (ret != 0) {
    asfPrintError("meta_get_latLon error: line = %f, samp = %f\n",
                  line_sar, samp_sar);
  }
  ret = meta_get_lineSamp(meta_dem, lat, lon, 0, line_dem, samp_dem);
  if (ret != 0) {
    asfPrintError("meta_get_lineSamp error: lat = %f, lon = %f\n",
                  lat, lon);
  }
}

static float get_from_float_image(FloatImage *fi, double samp, double line,
               float_image_sample_method_t method)
{
    if (samp > 0 && samp < fi->size_x-1 && line > 0 && line < fi->size_y-1)
        return float_image_sample(fi, samp, line, method);
    else
        return 0;
}

static double map(meta_parameters *meta_sar, meta_parameters *meta_dem,
                  FloatImage *fiDem, float_image_sample_method_t method,
                  double line, double samp)
{
    double dem_line, dem_samp;
    sar_to_dem(meta_sar, meta_dem, line, samp, &dem_line, &dem_samp);
    return (double)get_from_float_image(fiDem, dem_samp, dem_line, method);
}
                
static float get_sr_height_at(meta_parameters *meta_sar, meta_parameters *meta_dem,
                              FloatImage *fiDem, double line, double samp)
{
    //const float_image_sample_method_t method = FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC;
    const float_image_sample_method_t method = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
    return map(meta_sar, meta_dem, fiDem, method, line, samp);
}

static void map_to_sr(meta_parameters *meta_sar, const char *demImg, const char *demMeta, const char *srDemImg, int pad)
{
  asfPrintStatus("Reading DEM...\n");
  meta_parameters *meta_dem = meta_read(demMeta);
  float *demData = NULL;
  FloatImage *fi_dem = NULL;

  if (0)
    demData = read_dem(meta_dem, demImg);
  else
    fi_dem = float_image_new_from_metadata(meta_dem, demImg);

  if (demData)
    asfPrintStatus("Old method: reading entire DEM.\n");
  if (fi_dem)
    asfPrintStatus("New method: float image\n");
  if (demData && fi_dem)
    asfPrintError("Impossible.\n");

  // add the padding if requested
  meta_parameters *meta_out = meta_copy(meta_sar);
  meta_out->general->line_count += pad*2;
  meta_out->general->sample_count += pad*2;
  meta_out->general->start_line -= pad;
  meta_out->general->start_sample -= pad;

  // fixing up the output metadata.  Note that we must keep the SAR section
  // intact since that specifies our geometry which is the whole point of
  // this exercise.
  strcpy(meta_out->general->basename, meta_dem->general->basename);
  strcpy(meta_out->general->sensor, MAGIC_UNSET_STRING);
  strcpy(meta_out->general->processor, MAGIC_UNSET_STRING);
  strcpy(meta_out->general->mode, MAGIC_UNSET_STRING);
  strcpy(meta_out->general->sensor_name, MAGIC_UNSET_STRING);
  meta_out->general->image_data_type = DEM;
  meta_out->general->radiometry = MAGIC_UNSET_INT;
  strcpy(meta_out->general->acquisition_date, meta_dem->general->acquisition_date);
  meta_out->general->orbit = MAGIC_UNSET_INT;
  meta_out->general->orbit_direction = MAGIC_UNSET_CHAR;
  meta_out->general->frame = MAGIC_UNSET_INT;
  meta_out->general->band_count = 1;
  strcpy(meta_out->general->bands, "DEM");

  int nl = meta_out->general->line_count;
  int ns = meta_out->general->sample_count;

  float *buf = MALLOC(sizeof(float)*ns);
  FILE *fpOut = FOPEN(srDemImg, "wb");

  asfPrintStatus("Creating slant range DEM...\n");

  int ii, jj;
  for (ii=0; ii<nl; ii++) {
    for (jj=0; jj<ns; jj++) {
        buf[jj] = get_sr_height_at(meta_sar, meta_dem, fi_dem, ii, jj);
    }

    put_float_line(fpOut, meta_out, ii, buf);
    asfPrintStatus("Completed %.1f%%  \r", 100.*ii/(double)nl);
  }
  asfPrintStatus("Completed 100%%   \n");
  
  FCLOSE(fpOut);
  meta_write(meta_out, srDemImg);

  meta_free(meta_out);
  meta_free(meta_dem);

  FREE(buf);
  FREE(demData);
  if (fi_dem)
    float_image_free(fi_dem);
}

static double bilinear_interp(double *xi, double *yi, int N, double x)
{
  if (x < xi[0]) return yi[0];
  if (x > xi[N-1]) return yi[N-1];

  // find 2 nearest points
  int left = 0;
  int right = N-1;
  int mid = N/2;  
  while (right - left > 1) {
    if (x < xi[mid]) {
      right = mid;
    } else { //if (x > xi[left]) {
      left = mid;
    }
    mid = (right + left) / 2;
    if (mid < left) { asfPrintError("interp error1 %d %d %d\n", left, mid, right); }
    if (mid > right) { asfPrintError("interp error2 %d %d %d\n", left, mid, right); }
  }

  double len = xi[right] - xi[left];
  double dx = x - xi[left];

  return yi[right] * dx/len + yi[left] * (1-dx)/len;
}

static void apply_height_shifts(const char *srDem, const char *outImg)
{
  asfPrintStatus("Applying height shifts...\n");

  meta_parameters *meta_dem = meta_read(srDem);
  meta_parameters *meta_out = meta_copy(meta_dem);

  int nl = meta_out->general->line_count;
  int ns = meta_out->general->sample_count;

  const int PAD = 0;//400;
  int ons = meta_out->general->sample_count + PAD;
  meta_out->general->sample_count = ons;
  meta_out->general->start_sample = PAD;

  float *buf = MALLOC(sizeof(float)*ns);
  float *incid = MALLOC(sizeof(float)*ns);
  float *incid_out = MALLOC(sizeof(float)*ons);

  double *samps = MALLOC(sizeof(double)*ns);
  double *heights = MALLOC(sizeof(double)*ns);

  float *out_line = MALLOC(sizeof(float)*ons);

  FILE *fpOut = FOPEN(outImg, "wb");
  FILE *fpIn = FOPEN(srDem, "rb");

    memset(out_line, 0, sizeof(float)*ons);
    put_float_line(fpOut, meta_out, nl-1, out_line);

  int ii, jj;
  for (jj=0; jj<ns; ++jj)
    incid[jj] = meta_incid(meta_dem, nl/2, jj);
  for (jj=0; jj<PAD; ++jj)
    incid_out[jj] = meta_incid(meta_dem, nl/2, jj-PAD);
  for (jj=0; jj<ns; ++jj)
    incid_out[jj+PAD] = incid[jj];

  for (ii=0; ii<nl; ++ii) {
  //for (ii=6100; ii<6800; ++ii) {
    get_float_line(fpIn, meta_dem, ii, buf);
    for (jj=0; jj<ns; ++jj) {
      double h = (double)buf[jj];
      double samp = (double)jj - h*cos((double)incid[jj])/meta_dem->general->x_pixel_size;
      samps[jj] = samp;
      heights[jj] = h;
    }
    
    double samp_min = samps[0], samp_max=samps[0];
    for (jj=1; jj<ns; ++jj) {
      if (samps[jj] < samp_min) samp_min = samps[jj];
      if (samps[jj] > samp_max) samp_max = samps[jj];
    }

    int jj_min = (int)samp_min + 1;
    if (jj_min < -PAD+1) jj_min = -PAD;
    int jj_max = (int)samp_max + 1;

    jj_min += PAD;
    jj_max += PAD;

    // Ensure strictly increasing in preparation for splines
    double *xi = MALLOC(sizeof(double)*ons);
    double *yi = MALLOC(sizeof(double)*ons);
    int n = 0;
    for (jj=0; jj<ns; ++jj) {
      //printf("jj=%d n=%d %f\n", jj, n, samps[jj]);
      if (jj>0 && samps[jj] < xi[n-1]) {
        //printf("Backup!\n");
        // how far back must we clobber?
        while (n>=0 && samps[jj] < xi[n-1]) {
          //printf("%d %f (%f)\n", back, xi[n], samps[jj]);
          --n;
        }
      }
      xi[n] = samps[jj];
      yi[n] = heights[jj];
      ++n;
    }
   
    //if (ii==1105)
    //  for (jj=0; jj<n; ++jj)
    //    printf("%d %f %f\n", jj, xi[jj], yi[jj]);
    for (jj=1; jj<n; ++jj)
      if (xi[jj-1] > xi[jj]) asfPrintError("Aargh: xi[%d]=%f, xi[%d]=%f\n", jj-1, xi[jj-1], jj, xi[jj]);

    memset(out_line, 0, sizeof(float)*ons);
    if (n>3) {
      
      // now create splines to generate output
      gsl_interp_accel *acc = gsl_interp_accel_alloc ();
      gsl_spline *spline = gsl_spline_alloc (gsl_interp_cspline, n);
      gsl_spline_init (spline, xi, yi, n);
      int use_splines = TRUE;

      for (jj=jj_min; jj<jj_max; ++jj) {
        if (jj<0 || jj>=ons) continue; // { asfPrintError("Oops jj=%d (%d,%d)\n", jj, 0, ons); }
        double h_interp;
        if (use_splines)
          h_interp = gsl_spline_eval_check (spline, (double)(jj-PAD), acc);
        else
          h_interp = bilinear_interp(xi, yi, n, (double)(jj-PAD));
        out_line[jj] = (float)h_interp;
      }

      gsl_interp_accel_free(acc);
      gsl_spline_free(spline);
    }
    //if (ii==1105)
    //  for (jj=0; jj<ons; ++jj)
    //    printf("%d %f\n", jj, out_line[jj]);

    // now apply the really weird vertical shift
    //for (jj=0; jj<ons; ++jj)
    //  if (out_line[jj] > 0)
    //    out_line[jj] -= (ons-jj)*tan(PI/2-incid_out[jj])*meta_out->general->x_pixel_size;

    //if (ii==1105)
    //  for (jj=0; jj<ons; ++jj)
    //    printf("%d %f\n", jj, out_line[jj]);

    // wow we did it!
    put_float_line(fpOut, meta_out, ii, out_line);
  }

  meta_write(meta_out, outImg);
  meta_free(meta_out);
  meta_free(meta_dem);
  FREE(buf);
  FCLOSE(fpOut);
  FCLOSE(fpIn);

  FREE(incid);
  FREE(incid_out);
  FREE(samps);
  FREE(heights);
  FREE(out_line);
}
                  
int make_sr_dem(meta_parameters *meta_sar, const char *demBase, const char *output_name)
{
  char *demImg = appendExt(demBase, ".img");
  char *demMeta = appendExt(demBase, ".meta");
  int ret = make_gr_dem_ext(meta_sar, demImg, demMeta, 0, .1, output_name, 0);
  FREE(demImg);
  FREE(demMeta);
  return ret;
}

// This is the external facing function from this file.
// Given the geometry from a sar image (meta_sar), and a DEM (in the files
// demImg and demMeta), we write out an image/meta pair with the specified
// output_name, which is a subset of the given DEM that covers the given
// sar geometry.  This function uses interpolations for speed, so there is
// a tolerance parameter that specifies how accurate the line/sample mapping
// from sar to dem geometry needs to be.

// Input:
//   meta_parameters *meta_sar--  SAR geometry to subset the DEM
//   const char *demImg       --  DEM data filename
//   const char *demMeta      --  DEM metadata filename
//   int pad                  --  number of lines to add at the top/bottom/left/right
//   double tolerance         --  how accurate the approximation mapping needs to be,
//                                in units of pixels
//   const char *output_name  --  output filename (basename)
//   int test_mode            --  adds checks for the accuracy of the mapping, and
//                                does some unit testing
// Output:
//   no output parameters, the output is the output_name files (.img and .meta)
// Return Value:
//   return TRUE on success, FALSE on fail
//
int make_sr_dem_ext(meta_parameters *meta_sar, const char *demImg, const char *demMeta,
                    int pad, double tolerance, const char *output_name, int test_mode)
{
  char *outImg = appendExt(output_name, ".img");
  meta_parameters *meta_dem = meta_read(demMeta);

  // do not do DEM smoothing if the DEM pixel size is better or close to the
  // SAR image's pixel size.  Highly oversampled DEMs benefit from smoothing,
  // even if we do bicubic oversampling
  int do_averaging = TRUE;
  if (meta_dem->general->y_pixel_size - 10 < meta_sar->general->y_pixel_size)
    do_averaging = FALSE;

  asfPrintStatus("Averaging: %s (DEM %f, SAR: %f)\n", do_averaging ? "YES" : "NO",
                 meta_dem->general->y_pixel_size,
                 meta_sar->general->y_pixel_size);

  // Will have some intermediate files here
  // 1. result of initial mapping to slant range ("_sr")
  // 2. result of smoothing ("_smooth")
  // 3. final result (output_name)
  // We may skip step #2.
  char *output_step1 = appendStr(output_name, "_sr");
  char *outImgStep1 = appendExt(output_step1, ".img");
  char *output_step2, *outImgStep2;
  if (do_averaging) {
    output_step2 = appendStr(output_name, "_smooth");
    outImgStep2 = appendExt(output_step2, ".img");
  }
  else {
    output_step2 = STRDUP(output_step1);
    outImgStep2 = STRDUP(outImgStep1);
  }

  // step 1 -- map DEM from projected to slant range (no height corrections)
  int already_have_slant_range = TRUE;
  if (already_have_slant_range) {
    copyImgAndMeta(demImg, outImgStep1);
  } else {
    if (0)
      map_to_sr(meta_sar, demImg, demMeta, outImgStep1, pad);
    else 
      make_gr_dem_ext(meta_sar, demImg, demMeta, pad, .1, outImgStep1, 0);
  }

  // step 2 -- apply 3x3 filter
  if (do_averaging) {
    asfPrintStatus("Smoothing with 3x3 kernel ...\n");
    smooth(outImgStep1, outImgStep2, 3, EDGE_TRUNCATE);
  }
  
  // step 3 -- apply height shifts
  apply_height_shifts(outImgStep2, outImg);

  FREE(outImg);
  FREE(outImgStep1);
  FREE(outImgStep2);
  FREE(output_step1);
  FREE(output_step2);

  // success
  return 0;
}

