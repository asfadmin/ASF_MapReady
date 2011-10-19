#include <asf_terrcorr.h>
#include <stdio.h>
#include <assert.h>
#include <gsl/gsl_spline.h>
#include <asf_raster.h>

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
  
static double
bilinear_interp_fn(double y, double x,
                   double p00, double p10, double p01, double p11)
{
  double x1 = 1-x;
  double y1 = 1-y;

  return p00*x1*y1 + p10*x*y1 + p01*x1*y + p11*x*y;
}

static float
interp_demData(float *demData, int nl, int ns, double l, double s)
{
  if (l<0 || l>=nl-1 || s<0 || s>=ns-1) {
    return 0;
  }

  int ix = (int)s;
  int iy = (int)l;

  int bilinear = l<3 || l>=nl-3 || s<3 || s>=ns-3;
  //int bilinear = 1;
  if (bilinear) {

    float p00 = demData[ix   + ns*(iy  )];
    float p10 = demData[ix+1 + ns*(iy  )];
    float p01 = demData[ix   + ns*(iy+1)];
    float p11 = demData[ix+1 + ns*(iy+1)];

    return (float)bilinear_interp_fn(s-ix, l-iy, p00, p10, p01, p11);
  }
  else {

    double ret, x[4], y[4], xi[4], yi[4];
    int ii;

    for (ii=0; ii<4; ++ii) {
      y[0] = demData[ix-1 + ns*(iy+ii-1)];
      y[1] = demData[ix   + ns*(iy+ii-1)];
      y[2] = demData[ix+1 + ns*(iy+ii-1)];
      y[3] = demData[ix+2 + ns*(iy+ii-1)];

      x[0] = ix - 1;
      x[1] = ix;
      x[2] = ix + 1;
      x[3] = ix + 2;

      gsl_interp_accel *acc = gsl_interp_accel_alloc ();
      gsl_spline *spline = gsl_spline_alloc (gsl_interp_cspline, 4);    
      gsl_spline_init (spline, x, y, 4);
      yi[ii] = gsl_spline_eval(spline, s, acc);
      gsl_spline_free (spline);
      gsl_interp_accel_free (acc);

      xi[ii] = iy + ii - 1;
    }

    gsl_interp_accel *acc = gsl_interp_accel_alloc ();
    gsl_spline *spline = gsl_spline_alloc (gsl_interp_cspline, 4);    
    gsl_spline_init (spline, xi, yi, 4);
    ret = gsl_spline_eval(spline, l, acc);
    gsl_spline_free (spline);
    gsl_interp_accel_free (acc);
 
    return (float)ret;
  }
  
  asfPrintError("Impossible.");
}

static void check(int num, double a, double b)
{
  if (a==b || fabs(a-b)<0.0000001)
    return;
  
  asfPrintError("Failed test %d -- %f %f\n", num, a, b);
}

static void test_interp()
{
  check(1, bilinear_interp_fn(.5,.5,1,1,1,1), 1);
  check(2, bilinear_interp_fn(.1,.1,1,1,1,1), 1);
  check(3, bilinear_interp_fn(.5,.5,1,1,2,2), 1.5);
  check(4, bilinear_interp_fn(.5,.5,1,2,1,2), 1.5);
  check(5, bilinear_interp_fn(.5,.5,1,2,1,2), 1.5);
  check(6, bilinear_interp_fn(0,0,1,2,3,4), 1);
  check(7, bilinear_interp_fn(1,1,1,2,3,4), 4);
  check(8, bilinear_interp_fn(0,1,1,2,3,4), 2);
  check(9, bilinear_interp_fn(1,0,1,2,3,4), 3);  
}

static void xy_interp(int line, int samp,
                      int line_lo, int line_hi, int samp_lo, int samp_hi,
                      double *lines, double *samps,
                      double *interp_line, double *interp_samp)
{
  assert(samp>=samp_lo && samp<=samp_hi);
  assert(line>=line_lo && line<=line_hi);

  double l = (double)(line - line_lo) / (double)(line_hi - line_lo);
  double s = (double)(samp - samp_lo) / (double)(samp_hi - samp_lo);

  *interp_line = bilinear_interp_fn(l, s, lines[0], lines[1], lines[2], lines[3]);
  *interp_samp = bilinear_interp_fn(l, s, samps[0], samps[1], samps[2], samps[3]);
}

static double
calc_err(meta_parameters *meta_sar, meta_parameters *meta_dem,
         int line_lo, int line_hi, int samp_lo, int samp_hi,
         double *lines, double *samps)
{
  int l = (line_lo + line_hi) * .5;
  int s = (samp_lo + samp_hi) * .5;

  double interp_line, interp_samp, real_line, real_samp;
  xy_interp(l, s, line_lo, line_hi, samp_lo, samp_hi, lines, samps,
            &interp_line, &interp_samp);

  sar_to_dem(meta_sar, meta_dem, l, s, &real_line, &real_samp);

  asfPrintStatus("  Interp: (%f,%f)\n", interp_line, interp_samp);
  asfPrintStatus("  Actual: (%f,%f)\n", real_line, real_samp);
  
  return hypot(interp_line - real_line, interp_samp - real_samp);
}

static void
get_interp_params(meta_parameters *meta_sar, meta_parameters *meta_dem,
                  int line_lo, int line_hi, int samp_lo, int samp_hi,
                  double *lines, double *samps)
{
  sar_to_dem(meta_sar, meta_dem, line_lo, samp_lo, &lines[0], &samps[0]);
  sar_to_dem(meta_sar, meta_dem, line_lo, samp_hi, &lines[1], &samps[1]);
  sar_to_dem(meta_sar, meta_dem, line_hi, samp_lo, &lines[2], &samps[2]);
  sar_to_dem(meta_sar, meta_dem, line_hi, samp_hi, &lines[3], &samps[3]);
}

static int
find_grid_size(meta_parameters *meta_sar, meta_parameters *meta_dem,
               int initial_size, double tolerance)
{
  // pick a square centered on the center of the image
  // we will start with the biggest square and compare the error at the center
  // point between using (1) bilinear interp and (2) the real mapping
  // if the error is larger than the tolerance, shrink square and try again
  int line = meta_sar->general->line_count / 2;
  int samp = meta_sar->general->sample_count / 2;
  int sz = initial_size;
  double lines[4], samps[4];

  while (1) {
    int sz2 = sz/2;
    int line_lo = line - sz2;
    int line_hi = line_lo + sz;
    int samp_lo = samp - sz2;
    int samp_hi = samp_lo + sz;
    get_interp_params(meta_sar, meta_dem, line_lo, line_hi, samp_lo, samp_hi,
                      lines, samps);
    double err = calc_err(meta_sar, meta_dem, line_lo, line_hi, samp_lo, samp_hi,
                          lines, samps);
    if (err < tolerance) {
      asfPrintStatus("Using square size %d (err %f < %f)\n", sz, err, tolerance);
      break;
    }
    asfPrintStatus("Square size %d, no good (err %f > %f)\n", sz, err, tolerance);
    sz = sz2;
    if (sz <= 2) {
      asfPrintStatus("Proceeding with grid size 1.\n");
      sz = 1;
      break;
    }
  }
  return sz;
}

int make_gr_dem(meta_parameters *meta_sar, const char *demBase, const char *output_name)
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
int make_gr_dem_ext(meta_parameters *meta_sar, const char *demImg, const char *demMeta,
                    int pad, double tolerance, const char *output_name, int test_mode)
{
  if (test_mode)
    test_interp();
  
  asfPrintStatus("Reading DEM...\n");
  meta_parameters *meta_dem = meta_read(demMeta);
  float *demData = read_dem(meta_dem, demImg);
  int dnl = meta_dem->general->line_count;
  int dns = meta_dem->general->sample_count;

  char *outImg = appendExt(output_name, ".img");
  char *output_name_tmp, *outImgTmp;

  // do not do DEM smoothing if the DEM pixel size is better or close to the
  // SAR image's pixel size.
  int do_averaging = TRUE;
  if (meta_dem->general->y_pixel_size - 10 < meta_sar->general->y_pixel_size)
    do_averaging = FALSE;
  asfPrintStatus("Averaging: %s (DEM %f, SAR: %f)\n", do_averaging ? "YES" : "NO",
                 meta_dem->general->y_pixel_size,
                 meta_sar->general->y_pixel_size);
  if (do_averaging) {
    output_name_tmp = appendStr(output_name, "_unsmoothed");
    outImgTmp = appendExt(output_name_tmp, ".img");
  }
  else {
    output_name_tmp = STRDUP(output_name);
    outImgTmp = STRDUP(outImg);
  }

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

  // finding the right grid size
  int size = find_grid_size(meta_sar, meta_dem, 512, .1*tolerance);

  asfPrintStatus("Creating ground range image...\n");

  float *buf = MALLOC(sizeof(float)*ns*size);
  FILE *fpOut = FOPEN(outImgTmp, "wb");

  // these are for tracking the quality of the bilinear interp
  // not used if test_mode is false
  int num_out_of_tol = 0;
  int num_checked = 0;
  int num_bad = 0;
  double max_err = 0;
  double avg_err = 0;

  int ii, jj;
  for (ii=0; ii<nl; ii += size) {
    int line_lo = ii;
    int line_hi = ii + size;

    for (jj=0; jj<ns; jj += size) {
      double lines[4], samps[4];
      
      int samp_lo = jj;
      int samp_hi = jj + size;

      get_interp_params(meta_sar, meta_dem, line_lo, line_hi, samp_lo, samp_hi,
                        lines, samps);

      int iii, jjj;
      for (iii=0; iii<size; ++iii) {
        for (jjj=0; jjj<size && jj+jjj<ns; ++jjj) {
          int index = iii*ns + jj + jjj;
          assert(index < ns*size);

          double line_out, samp_out;
          xy_interp(ii+iii, jj+jjj, line_lo, line_hi, samp_lo, samp_hi, lines, samps,
                    &line_out, &samp_out);

          // random checking of the quality of our interpolations
          if (test_mode && iii%11==0 && jjj%13==0) {
            double real_line, real_samp; 
            sar_to_dem(meta_sar, meta_dem, ii+iii, jj+jjj, &real_line, &real_samp);

            double err = hypot(real_line - line_out, real_samp - samp_out);

            avg_err += err;
            if (err > max_err)
              max_err = err;

            if (err > tolerance) {
              asfPrintStatus("Out of tolerance at %d,%d: (%f,%f) vs (%f,%f) -> %f\n",
                             ii+iii, jj+jjj, line_out, samp_out, real_line, real_samp,
                             err);
              ++num_out_of_tol;
            }
            if (err > .5) {
              asfPrintStatus("Error is larger than 1 pixel!\n");
              ++num_bad;
            }
            ++num_checked;
          }
          buf[index] = interp_demData(demData, dnl, dns, line_out, samp_out);
        }
      }
    }

    put_float_lines(fpOut, meta_out, ii, size, buf);
    asfPrintStatus("Completed %.1f%%  \r", 100.*ii/(double)nl);
  }
  asfPrintStatus("Completed 100%%   \n");

  if (test_mode) {
    asfPrintStatus("Tolerance was %f\n", tolerance);
    asfPrintStatus("%d/%d checked pixels had error exceeding tolerance. (%.1f%%)\n",
                   num_out_of_tol, num_checked, 100.*num_out_of_tol/(double)num_checked);
    asfPrintStatus("%d/%d checked pixels had error larger than half a pixel. (%.1f%%)\n",
                   num_bad, num_checked, 100.*num_bad/(double)num_checked);
    asfPrintStatus("Maximum error: %f pixels\n", max_err);
    avg_err /= (double)num_checked;
    asfPrintStatus("Average error: %f pixels\n", avg_err);
  }

  FCLOSE(fpOut);
  meta_write(meta_out, outImgTmp);

  meta_free(meta_out);
  meta_free(meta_dem);

  FREE(buf);
  FREE(demData);

  // now apply 3x3 filter
  if (do_averaging) {
    asfPrintStatus("Smoothing with 3x3 kernel ...\n");
    smooth(outImgTmp, outImg, 3, EDGE_TRUNCATE);
  }

  FREE(outImg);
  FREE(outImgTmp);
  FREE(output_name_tmp);

  return FALSE;
}
