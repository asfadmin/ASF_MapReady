#include "asf_sar.h"
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>

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

int proj_to_sr(const char *infile, const char *outfile, double pixel_size)
{
    int ii, jj, ret;
    const float_image_sample_method_t sampling_method =
        FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;

    // overall algorithm:
    // 1. find extents in time/slant space
    // 2. for each pixel in output, resample in input space

    meta_parameters *inMeta = meta_read(infile);
    int nl = inMeta->general->line_count;
    int ns = inMeta->general->sample_count;

    if (!inMeta->projection && !inMeta->transform)
        asfPrintError("Expected a projection block!\n");

    if (pixel_size < 0) {
        if (inMeta->sar) {
            pixel_size =
                SPD_LIGHT / ((2.0 * inMeta->sar->range_sampling_rate) *
                             inMeta->general->sample_count /
                             inMeta->sar->original_sample_count);
        }
        else {
            // no sar block... how can we get a slant range pixel size?
            // does it even make sense???
            pixel_size = inMeta->general->x_pixel_size;
        }
    }

    asfPrintStatus("Converting %s to slant range...\n", infile);
    asfPrintStatus("  Slant range pixel size: %f\n", pixel_size);

    // first, find extents in time/slant space
    // do this by projecting image corners to time/slant
    int tl_x, tl_y;
    int tr_x, tr_y;
    int bl_x, bl_y;
    int br_x, br_y;
    
    // we have to find the "real" corners of the image
    FloatImage *in = float_image_new_from_metadata(inMeta, infile);

    // find top left pixel -- TOP-most non-no-data pixel in the image
    for (ii=0; ii<nl; ++ii)
        for (jj=0; jj<ns; ++jj)
            if (float_image_get_pixel(in,jj,ii) != inMeta->general->no_data) {
                tl_x = jj; tl_y = ii;
                goto found_tl;
            }

    asfPrintError("Couldn't find top-left pixel! Entire image no data?\n");

  found_tl:

    // find top right pixel -- RIGHT-most non-no-data pixel in the image
    for (jj=ns-1; jj>=0; --jj)
        for (ii=0; ii<nl; ++ii)
            if (float_image_get_pixel(in,jj,ii) != inMeta->general->no_data) {
                tr_x = jj; tr_y = ii;
                goto found_tr;
            }

    asfPrintError("Couldn't find top-right pixel! Entire image no data?\n");

  found_tr:

    // find bottom left pixel -- LEFT-most non-no-data pixel in the image
    for (jj=0; jj<ns; ++jj)
        for (ii=nl-1; ii>=0; --ii)
            if (float_image_get_pixel(in,jj,ii) != inMeta->general->no_data) {
                bl_x = jj; bl_y = ii;
                goto found_bl;
            }

    asfPrintError("Couldn't find bottom-left pixel! Entire image no data?\n");

  found_bl:

    // find bottom right pixel -- BOTTOM-most non-no-data pixel in the image
    for (ii=nl-1; ii>=0; --ii)
        for (jj=ns-1; jj>=0; --jj)
            if (float_image_get_pixel(in,jj,ii) != inMeta->general->no_data) {
                br_x = jj; br_y = ii;
                goto found_br;
            }

    asfPrintError("Couldn't find bottom-right pixel! Entire image no data?\n");

  found_br:

    asfPrintStatus("Corners are at: TL (%d,%d)\n", tl_y, tl_x);
    asfPrintStatus(" (line,sample)  TR (%d,%d)\n", tr_y, tr_x);
    asfPrintStatus("                BL (%d,%d)\n", bl_y, bl_x);
    asfPrintStatus("                BR (%d,%d)\n", br_y, br_x);

    double tl_time, tl_slant;
    double tr_time, tr_slant;
    double bl_time, bl_slant;
    double br_time, br_slant;

    meta_get_timeSlantDop(inMeta, tl_y, tl_x, &tl_time, &tl_slant, NULL);
    meta_get_timeSlantDop(inMeta, tr_y, tr_x, &tr_time, &tr_slant, NULL);
    meta_get_timeSlantDop(inMeta, bl_y, bl_x, &bl_time, &bl_slant, NULL);
    meta_get_timeSlantDop(inMeta, br_y, br_x, &br_time, &br_slant, NULL);

    asfPrintStatus("Corners are at: TL (%f,%f)\n", tl_time, tl_slant);
    asfPrintStatus(" (time,slant)   TR (%f,%f)\n", tr_time, tr_slant);
    asfPrintStatus("                BL (%f,%f)\n", bl_time, bl_slant);
    asfPrintStatus("                BR (%f,%f)\n", br_time, br_slant);

    double slant_start = min4(tl_slant, tr_slant, bl_slant, br_slant);
    double slant_end = max4(tl_slant, tr_slant, bl_slant, br_slant);

    double time_min = min4(tl_time, tr_time, bl_time, br_time);
    double time_max = max4(tl_time, tr_time, bl_time, br_time);

    double slant_incr = pixel_size;
    int ons = (slant_end - slant_start) / slant_incr;

    double time_start, time_end, time_incr;
    int onl;

    if (inMeta->sar) {
        // in this case, we original data has a SAR block, we will use the
        // same azimuth time per pixel.
        time_incr = inMeta->sar->azimuth_time_per_pixel;
        if (time_incr > 0) {
            onl = (time_max - time_min) / time_incr;
            time_start = time_min;
            time_end = time_max;
        }
        else {
            onl = (time_min - time_max) / time_incr;
            time_start = time_max;
            time_end = time_min;
        }
    }
    else {
        // here, no sar block in the original data, just make a square image
        // with increasing time
        onl = ons;
        time_incr = (time_max - time_min) / (double)onl;
        time_start = time_min;
        time_end = time_max;
    }

    asfPrintStatus("  Slant range values: %f -> %f\n", slant_start, slant_end);
    asfPrintStatus("  Time values: %f -> %f\n", time_start, time_end);
    asfPrintStatus("  Output Image will be %dx%d LxS\n", onl, ons);

    FloatImage *out = float_image_new(ons, onl);

    // now, we're on to the resampling stage.. loop through output pixels
    asfPrintStatus("Generating output image...\n");

    for (ii=0; ii<onl; ++ii) {
        asfLineMeter(ii,onl);
        double time = time_start + ii * time_incr;
        for (jj=0; jj<ons; ++jj) {
            double lat, lon, line, samp;
            double slant = slant_start + jj * slant_incr;

            //printf("t,s: %f,%f\n", time, slant);
            meta_timeSlantDop2latLon(inMeta, time, slant, 0, 0, &lat, &lon);
            //printf("lat,lon: %f,%f\n", lat, lon);
            meta_get_lineSamp(inMeta, lat, lon, 0, &line, &samp);
            //printf("line,samp: %f,%f\n", line, samp);

            double val = inMeta->general->no_data;
            if (line > 0 && line < nl-1 && samp > 0 && samp < ns-1)
                val = float_image_sample(in, samp, line, sampling_method);

            //printf("val: %f\n", val);
            float_image_set_pixel(out, jj, ii, val);
        }
    }

    char *img_file = appendExt(outfile, ".img");
    asfPrintStatus("Writing %s...\n", img_file);
    ret = float_image_store(out, img_file, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
    free(img_file);

    if (!ret)
        asfPrintError("Error storing output image!\n");

    float_image_free(in);
    float_image_free(out);

    // set up output metadata
    meta_parameters *outMeta = meta_read(infile);

    outMeta->general->line_count = onl;
    outMeta->general->sample_count = ons;

    if (!outMeta->sar)
        outMeta->sar = meta_sar_init();

    outMeta->sar->image_type = 'S';
    outMeta->sar->azimuth_time_per_pixel = time_incr;
    outMeta->sar->slant_range_first_pixel = slant_start;
    outMeta->sar->line_increment = outMeta->sar->sample_increment = 1;
    outMeta->general->start_sample = outMeta->general->start_line = 0;
    
    char *meta_file = appendExt(outfile, ".meta");
    asfPrintStatus("Writing %s...\n", meta_file);
    meta_write(outMeta, meta_file);
    free(meta_file);

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
    return to_sr_pixsiz(infile, outfile, -1);
}
