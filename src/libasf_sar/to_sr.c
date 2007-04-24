#include "asf_sar.h"
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>

#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_statistics_double.h>

static void copyImgAndMeta(const char *src, const char *dst)
{
    char * src_meta_file = appendExt(src, ".meta");
    char * src_img_file = appendExt(src, ".img");

    char * dst_meta_file = appendExt(dst, ".meta");
    char * dst_img_file = appendExt(dst, ".img");

    fileCopy(src_meta_file, dst_meta_file);
    fileCopy(src_img_file, dst_img_file);

    free(src_meta_file);
    free(src_img_file);

    free(dst_meta_file);
    free(dst_img_file);
}

static double min2(double a, double b)
{
    return a<b ? a : b;
}

static double min4(double a, double b, double c, double d)
{
    return min2(min2(a,b), min2(c,d));
}

static double max2(double a, double b)
{
    return a>b ? a : b;
}

static double max4(double a, double b, double c, double d)
{
    return max2(max2(a,b), max2(c,d));
}

static void ts2ls(meta_parameters *meta, double time, double slant,
                  double *pLine, double *pSamp)
{
  double lat, lon, line, samp;
  meta_timeSlantDop2latLon(meta, time, slant, 0, 0, &lat, &lon);
  meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);
  *pLine = line;
  *pSamp = samp;
}

static int
proj_to_sr(const char *infile, const char *outfile, double pixel_size)
{
    int ii, jj, kk;
    const float_image_sample_method_t sampling_method =
        FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;

    // overall algorithm:
    // 1. find extents in time/slant space
    // 2. for each pixel in output, resample in input space

    meta_parameters *inMeta = meta_read(infile);
    int nl = inMeta->general->line_count;
    int ns = inMeta->general->sample_count;

    if (!inMeta->projection && !inMeta->transform)
        asfPrintError("Expected a projection/transform block!\n");

    //asfPrintStatus("Converting %s to slant range...\n", infile);

    // first, find extents in time/slant space
    // do this by projecting image corners to time/slant
    int tl_x=0,    tl_y=0;
    int tr_x=ns-1, tr_y=0;
    int bl_x=0,    bl_y=nl-1;
    int br_x=ns-1, br_y=nl-1;

    // we have to find the "real" corners of the image
    // do this using the first band of the input image as a reference
    if (inMeta->general->band_count == 1)
        asfPrintStatus("Tiling the input image...\n");
    else
        asfPrintStatus("Tiling the reference band of the input image...\n");

    FloatImage *in = float_image_new_from_metadata(inMeta, infile);

    // find top left pixel -- TOP-most non-no-data pixel in the image
    for (ii=0; ii<nl; ++ii)
        for (jj=0; jj<ns; ++jj) {
            double val = float_image_get_pixel(in, jj, ii);
            if (val != inMeta->general->no_data && val != 0.0) {
                tl_x = jj; tl_y = ii;
                goto found_tl;
            }
        }

    asfPrintError("Couldn't find top-left pixel! Entire image no data?\n");

  found_tl:

    // find top right pixel -- RIGHT-most non-no-data pixel in the image
    for (jj=ns-1; jj>=0; --jj)
        for (ii=0; ii<nl; ++ii) {
            double val = float_image_get_pixel(in, jj, ii);
            if (val != inMeta->general->no_data && val != 0.0) {
                tr_x = jj; tr_y = ii;
                goto found_tr;
            }
        }

    asfPrintError("Couldn't find top-right pixel! Entire image no data?\n");

  found_tr:

    // find bottom left pixel -- LEFT-most non-no-data pixel in the image
    for (jj=0; jj<ns; ++jj)
        for (ii=nl-1; ii>=0; --ii) {
            double val = float_image_get_pixel(in, jj, ii);
            if (val != inMeta->general->no_data && val != 0.0) {
                bl_x = jj; bl_y = ii;
                goto found_bl;
            }
        }

    asfPrintError("Couldn't find bottom-left pixel! Entire image no data?\n");

  found_bl:

    // find bottom right pixel -- BOTTOM-most non-no-data pixel in the image
    for (ii=nl-1; ii>=0; --ii)
        for (jj=ns-1; jj>=0; --jj) {
            double val = float_image_get_pixel(in, jj, ii);
            if (val != inMeta->general->no_data && val != 0.0) {
                br_x = jj; br_y = ii;
                goto found_br;
            }
        }

    asfPrintError("Couldn't find bottom-right pixel! Entire image no data?\n");

  found_br:

    asfPrintStatus("Determining image extents in time/slant coordinates.\n");
    //asfPrintStatus("Corners are at: TL (%d,%d)\n", tl_y, tl_x);
    //asfPrintStatus(" (line,sample)  TR (%d,%d)\n", tr_y, tr_x);
    //asfPrintStatus("                BL (%d,%d)\n", bl_y, bl_x);
    //asfPrintStatus("                BR (%d,%d)\n", br_y, br_x);

    double tl_time, tl_slant;
    double tr_time, tr_slant;
    double bl_time, bl_slant;
    double br_time, br_slant;

    meta_get_timeSlantDop(inMeta, tl_y, tl_x, &tl_time, &tl_slant, NULL);
    meta_get_timeSlantDop(inMeta, tr_y, tr_x, &tr_time, &tr_slant, NULL);
    meta_get_timeSlantDop(inMeta, bl_y, bl_x, &bl_time, &bl_slant, NULL);
    meta_get_timeSlantDop(inMeta, br_y, br_x, &br_time, &br_slant, NULL);

    //asfPrintStatus("Corners are at: TL (%f,%f)\n", tl_time, tl_slant);
    //asfPrintStatus(" (time,slant)   TR (%f,%f)\n", tr_time, tr_slant);
    //asfPrintStatus("                BL (%f,%f)\n", bl_time, bl_slant);
    //asfPrintStatus("                BR (%f,%f)\n", br_time, br_slant);

    double slant_start = min4(tl_slant, tr_slant, bl_slant, br_slant);
    double slant_end = max4(tl_slant, tr_slant, bl_slant, br_slant);

    double time_min = min4(tl_time, tr_time, bl_time, br_time);
    double time_max = max4(tl_time, tr_time, bl_time, br_time);

    double slant_incr;
    double time_start, time_end, time_incr;
    int onl, ons;

    if (pixel_size > 0) {
        slant_incr = pixel_size;
        ons = (slant_end - slant_start) / slant_incr;

        if (inMeta->sar) {
            // in this case, the original data has a SAR block, we will use the
            // same azimuth time per pixel.
            time_incr = inMeta->sar->azimuth_time_per_pixel;
            if (time_incr > 0) {
                time_start = time_min;
                time_end = time_max;
            }
            else {
                time_start = time_max;
                time_end = time_min;
            }
            onl = (time_end - time_start) / time_incr;
        }
        else {
            // here, no sar block in the original data, just make a square
            // image with increasing time
            onl = ons;
            time_incr = (time_max - time_min) / (double)onl;
            time_start = time_min;
            time_end = time_max;
        }
    }
    else {
        // not provided a slant range pixel size, we'll figure something out

        if (inMeta->sar) {
            // use the same azimuth time per pixel.
            time_incr = inMeta->sar->azimuth_time_per_pixel;
            if (time_incr > 0) {
                time_start = time_min;
                time_end = time_max;
            }
            else {
                time_start = time_max;
                time_end = time_min;
            }
            onl = (time_end - time_start) / time_incr;
        }
        else {
            // no info... determine azimuth time per pixel by keeping
            // the height the same as in the original image
            onl = nl;
            time_incr = (time_max - time_min) / (double)onl;
            time_start = time_min;
            time_end = time_max;
        }

        // make it square, to get the slant range pixel size
        ons = onl;
        pixel_size = slant_incr = (slant_end - slant_start) / (double)ons;
    }

    asfPrintStatus("  Slant range values: %f -> %f\n", slant_start, slant_end);
    asfPrintStatus("  Slant range pixel size: %f\n", pixel_size);
    asfPrintStatus("  Time values: %f -> %f\n", time_start, time_end);
    asfPrintStatus("  Output Image will be %5d x %5d LxS\n", onl, ons);
    asfPrintStatus("      (Input Image was %5d x %5d LxS)\n", nl, ns);

    // generate a grid over the image, to generate our splines
    // this grid size seems to work pretty well...
    int n = 120;

    asfPrintStatus("Creating %dx%d mapping grid...\n", n, n);

    double time_grid_incr = ((double)(onl))/((double)(n)) * time_incr;
    double slant_grid_incr = ((double)(ons))/((double)(n)) * slant_incr;

    // allocating memory for the splines, and the arrays to generate them
    gsl_interp_accel **samp_accels = MALLOC(sizeof(gsl_interp_accel *) * n);
    gsl_spline **samp_splines = MALLOC(sizeof(gsl_spline *) * n);

    gsl_interp_accel **line_accels = MALLOC(sizeof(gsl_interp_accel *) * n);
    gsl_spline **line_splines = MALLOC(sizeof(gsl_spline *) * n);

    double *slant_in = MALLOC(sizeof(double)*n);
    double *line_out = MALLOC(sizeof(double)*n);
    double *samp_out = MALLOC(sizeof(double)*n);

    // an alias -- use the same array (to save memory -- these are not used
    // at the same time), but create an alias for it, so it is not so confusing
    double *time_in = slant_in;

    // set up the vertical splines
    for (jj=0; jj<n; ++jj) {
        double slant = slant_start + jj*slant_grid_incr;

        for (ii=0; ii<n; ++ii) {
            time_in[ii] = time_start + ii*time_grid_incr;
            ts2ls(inMeta, time_in[ii], slant, &line_out[ii], &samp_out[ii]);
        }

        samp_accels[jj] = gsl_interp_accel_alloc();
        samp_splines[jj] = gsl_spline_alloc(gsl_interp_cspline, n);
        gsl_spline_init(samp_splines[jj], time_in, samp_out, n);

        line_accels[jj] = gsl_interp_accel_alloc();
        line_splines[jj] = gsl_spline_alloc(gsl_interp_cspline, n);
        gsl_spline_init(line_splines[jj], time_in, line_out, n);
    }

    // now, we're on to the resampling stage.. loop through output pixels
    asfPrintStatus("Generating slant range image...\n");

    double no_data_value = meta_is_valid_double(inMeta->general->no_data) ?
        inMeta->general->no_data : 0;

    // keep track of error sizes
    double max_error = 0;
    double avg_error = 0;
    int count = 0;

    // these stride values allow us to track when we're in between grid points
    int ii_n = onl/n;
    int jj_n = ons/n;
    int ii_n2 = ii_n/2;
    int jj_n2 = jj_n/2;

    // set up output metadata
    meta_parameters *outMeta = meta_read(infile);

    if (outMeta->transform) {
        FREE(outMeta->transform);
        outMeta->transform = NULL;
    }

    if (outMeta->projection) {
        FREE(outMeta->projection);
        outMeta->projection = NULL;
    }

    outMeta->general->line_count = onl;
    outMeta->general->sample_count = ons;

    if (!outMeta->sar)
        outMeta->sar = meta_sar_init();

    outMeta->sar->image_type = 'S';

    outMeta->sar->azimuth_time_per_pixel = time_incr;
    outMeta->sar->time_shift = time_start;

    outMeta->general->y_pixel_size = inMeta->sar->azimuth_time_per_pixel / 
                                     time_incr * inMeta->general->y_pixel_size;

    outMeta->sar->slant_range_first_pixel = slant_start;
    outMeta->general->x_pixel_size = slant_incr;

    outMeta->sar->line_increment = outMeta->sar->sample_increment = 1;
    outMeta->general->start_sample = outMeta->general->start_line = 0;

    outMeta->general->no_data = no_data_value;

    char **band_name = extract_band_names(inMeta->general->bands,
                                          inMeta->general->band_count);

    // now generate output image
    char *img_file = appendExt(outfile, ".img");
    float *out = MALLOC(sizeof(float) * ons);

    for (kk=0; kk<inMeta->general->band_count; ++kk) {
        if (inMeta->general->band_count != 1)
            asfPrintStatus("Working on band: %s\n", band_name[kk]);

        // for the 2nd and higher bands, free the band from the previous iteration,
        // and read in the next band from the input image
        if (kk>0) {
            float_image_free(in);
            asfPrintStatus("Loading input...\n");
            in = float_image_band_new_from_metadata(inMeta, kk, infile);
        }

        FILE *ofp = FOPEN(img_file, kk==0 ? "wb" : "ab");
        asfPrintStatus("Generating output...\n");
        for (ii=0; ii<onl; ++ii) {
            asfLineMeter(ii,onl);
            double time = time_start + ii * time_incr;

            // set up horizontal splines for this row
            gsl_interp_accel *samp_accel = gsl_interp_accel_alloc();
            gsl_spline *samp_spline = gsl_spline_alloc(gsl_interp_cspline, n);

            gsl_interp_accel *line_accel = gsl_interp_accel_alloc();
            gsl_spline *line_spline = gsl_spline_alloc(gsl_interp_cspline, n);

            for (jj=0; jj<n; ++jj) {
                slant_in[jj] = slant_start + jj * slant_grid_incr;
                samp_out[jj] = gsl_spline_eval(samp_splines[jj], time,
                                               samp_accels[jj]);
                line_out[jj] = gsl_spline_eval(line_splines[jj], time,
                                               line_accels[jj]);
            }

            gsl_spline_init(samp_spline, slant_in, samp_out, n);
            gsl_spline_init(line_spline, slant_in, line_out, n);

            // use the splines to produce output pixels
            for (jj=0; jj<ons; ++jj) {
                double slant = slant_start + jj * slant_incr;
                double samp = gsl_spline_eval(samp_spline, slant, samp_accel);
                double line = gsl_spline_eval(line_spline, slant, line_accel);

                // check the spline every so often (halfway between grid points)
                // only do this on band #1 (the reference band)
                if (kk==0 && ii%ii_n2==0 && ii%ii_n!=0 && jj%jj_n2==0 && jj%jj_n!=0) {
                    double lat, lon, samp_real, line_real;
                    meta_timeSlantDop2latLon(inMeta, time, slant, 0,0, &lat, &lon);
                    meta_get_lineSamp(inMeta, lat, lon, 0, &line_real, &samp_real);
                    
                    double err = (line-line_real) * (line-line_real) +
                                 (samp-samp_real) * (samp-samp_real);

                    //printf("(%d,%d) -- Actual: (%f,%f) Splined: (%f,%f)\n",
                    //       ii, jj, line_real, samp_real, line, samp);

                    if (err > max_error) max_error = err;
                    avg_error += err;
                    ++count;
                }

                // now interpolate within the original image
                // if we are outside, use "no_data" from metadata
                double val = no_data_value;
                if (line > 0 && line < nl-1 && samp > 0 && samp < ns-1)
                    val = float_image_sample(in, samp, line, sampling_method);

                out[jj] = (float)val;
            }

            gsl_interp_accel_free(samp_accel);
            gsl_spline_free(samp_spline);

            gsl_interp_accel_free(line_accel);
            gsl_spline_free(line_spline);

            put_float_line(ofp, outMeta, ii, out);
        }

        fclose(ofp);
    }

    // free the last band of the input
    float_image_free(in);

    FREE(slant_in);
    FREE(line_out);
    FREE(samp_out);

    FREE(samp_accels);
    FREE(samp_splines);

    FREE(line_accels);
    FREE(line_splines);

    FREE(out);

    for (kk=0; kk<inMeta->general->band_count; ++kk)
        FREE(band_name[kk]);
    FREE(band_name);

    // see how bad our errors were
    avg_error /= (double)count;
    asfPrintStatus("Model max error: %f, avg: %f\n",
                   max_error, avg_error);

    double thresh = 0.1;
    if (max_error > 100*thresh)
        asfPrintError("Maximum error exceeded threshold: %f > %f\n",
                      max_error, 100*thresh);
    else if (avg_error > 10*thresh)
        asfPrintError("Average error exceeded threshold: %f > %f\n",
                      avg_error, 10*thresh);
    if (max_error > 10*thresh)
        asfPrintWarning("Maximum error exceeds threshold: %f > %f\n",
                        max_error, 10*thresh);
    if (avg_error > thresh)
        asfPrintWarning("Average error exceeds threshold: %f > %f\n",
                        avg_error, thresh);

    char *meta_file = appendExt(outfile, ".meta");
    asfPrintStatus("Writing %s\n", meta_file);
    meta_write(outMeta, meta_file);
    free(meta_file);
    free(img_file);

    meta_free(outMeta);
    meta_free(inMeta);

    return 0; //success
}

int to_sr_pixsiz(const char *infile, const char *outfile, double pixel_size)
{
   meta_parameters *inMeta = meta_read(infile);
   int ret;

   if (inMeta->sar && inMeta->sar->image_type == 'G') {
       // ground range image
       ret = gr2sr_pixsiz(infile, outfile, pixel_size);
   }
   else if (inMeta->sar && inMeta->sar->image_type == 'S') {
       // already a slant range image, just copy it
       copyImgAndMeta(infile, outfile);
       ret = 0; // success
   }
   else if ((inMeta->sar && inMeta->sar->image_type == 'P') ||
            (inMeta->projection)) {
       // projected image
       ret = proj_to_sr(infile, outfile, pixel_size);
   }
   else if (inMeta->sar && inMeta->transform &&
            inMeta->sar->image_type == 'R') {
       // georeferenced, ALOS most likely
       ret = proj_to_sr(infile, outfile, pixel_size);
   }
   else {
       asfPrintError("Couldn't figure out what kind of image this is.\n");
       ret = 1;
   }

   meta_free(inMeta);
   return ret;
}

int to_sr(const char *infile, const char *outfile)
{
    char *img_in = MALLOC(sizeof(char)*(strlen(infile)+10));
    char *img_out = MALLOC(sizeof(char)*(strlen(outfile)+10));
    create_name(img_in, infile, ".img");
    create_name(img_out, outfile, ".img");
    int ret = to_sr_pixsiz(img_in, img_out, -1);
    FREE(img_in);
    FREE(img_out);
    return ret;
}
