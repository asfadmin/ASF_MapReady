#include <stdio.h>
#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "float_image.h"

static const float_image_byte_order_t fibo_be =
    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;

void usage()
{
    printf("Usage:\n"
           "    combine <outfile> <infile1> <infile2> ... \n\n"
           "At least 2 input files are required.\n"
           "All input files must be geocoded to the same projection, with\n"
           "the same projection parameters, and the same pixel size.\n");
    exit(1);
}

static void print_proj_info(meta_parameters *meta)
{
    project_parameters_t pp = meta->projection->param;

    switch (meta->projection->type)
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
        asfPrintStatus(" Projection: UTM\n   Zone: %d\n\n", pp.utm.zone);
        break;

    case POLAR_STEREOGRAPHIC:
        asfPrintStatus(
            " Projection: Polar Stereographic\n"
            "   Standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Hemisphere: %c\n\n", 
            pp.ps.slat, pp.ps.slon, pp.ps.is_north_pole ? 'N' : 'S');
        break;

    case ALBERS_EQUAL_AREA:
        asfPrintStatus(
            " Projection: Albers Equal Area Conic\n"
            "   First standard parallel: %.4f\n"
            "   Second standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Latitude of origin: %.4f\n\n",
            pp.albers.std_parallel1, pp.albers.std_parallel2,
            pp.albers.center_meridian, pp.albers.orig_latitude);
        break;

    case LAMBERT_CONFORMAL_CONIC:
        asfPrintStatus(
            " Projection: Lambert Conformal Conic\n"
            "   First standard parallel: %.4f\n"
            "   Second standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Latitude of origin: %.4f\n\n",
            pp.lamcc.plat1, pp.lamcc.plat2, pp.lamcc.lon0, pp.lamcc.lat0);
        break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        asfPrintStatus(
            " Projection: Lambert Azimuthal Equal Area\n"
            "   Latitude of origin: %.4f\n"
            "   Central meridian: %.4f\n\n",
            pp.lamaz.center_lat, pp.lamaz.center_lon);
        break;

    default:
        asfPrintError("Projection type not supported!\n");
        break;
    }
}

static int proj_parms_match(meta_parameters *m1, meta_parameters *m2)
{
    // these cases actualy should have already been handled
    if (!m1->projection || !m2->projection)
        return FALSE;

    if (m1->projection->type != m2->projection->type)
        return FALSE;

    project_parameters_t pp1 = m1->projection->param;
    project_parameters_t pp2 = m2->projection->param;

    switch (m1->projection->type)
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
        return pp1.utm.zone == pp2.utm.zone;

    case POLAR_STEREOGRAPHIC:
        return
            pp1.ps.slat == pp2.ps.slat &&
            pp1.ps.slon == pp2.ps.slon &&
            pp1.ps.is_north_pole == pp2.ps.is_north_pole;
        break;

    case ALBERS_EQUAL_AREA:
        return
            pp1.albers.std_parallel1 == pp2.albers.std_parallel1 &&
            pp1.albers.std_parallel2 == pp2.albers.std_parallel2 &&
            pp1.albers.center_meridian == pp2.albers.center_meridian &&
            pp1.albers.orig_latitude == pp2.albers.orig_latitude;
        break;

    case LAMBERT_CONFORMAL_CONIC:
        return
            pp1.lamcc.plat1 == pp2.lamcc.plat1 &&
            pp1.lamcc.plat2 == pp2.lamcc.plat2 &&
            pp1.lamcc.lon0 == pp2.lamcc.lon0 &&
            pp1.lamcc.lat0 == pp2.lamcc.lat0;
        break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        return
            pp1.lamaz.center_lat == pp2.lamaz.center_lat &&
            pp1.lamaz.center_lon == pp2.lamaz.center_lon;
        break;

    default:
        return FALSE;
    }
}

static void get_corners(meta_parameters *meta,
                        double *x0, double *y0,
                        double *xL, double *yL)
{
    *x0 = meta->projection->startX;
    *y0 = meta->projection->startY;

    *xL = meta->projection->startX + 
            meta->projection->perX * meta->general->sample_count;

    *yL = meta->projection->startY + 
            meta->projection->perY * meta->general->line_count;
}

static void update_corners(double px, double py,
                           double *x0, double *y0,
                           double *xL, double *yL,
                           double this_x0, double this_y0,   
                           double this_xL, double this_yL)
{
    if (px > 0) {
        // if perX is positive: startX will be the SMALLEST x value
        //                      endX will be the LARGEST x value
        if (this_x0 < *x0) *x0 = this_x0;
        if (this_xL > *xL) *xL = this_xL;
    } else {
        // if perX is negative: startX will be the LARGEST x value
        //                      endX will be the SMALLEST x value
        if (this_x0 > *x0) *x0 = this_x0;
        if (this_xL < *xL) *xL = this_xL;
    }

    if (py > 0) {
        // if perY is positive: startY will be the SMALLEST y value
        //                      endY will be the LARGEST y value
        if (this_y0 < *y0) *y0 = this_y0;
        if (this_yL > *yL) *yL = this_yL;
    } else {
        // if perY is negative: startY will be the LARGEST y value
        //                      endY will be the SMALLEST y value
        if (this_y0 > *y0) *y0 = this_y0;
        if (this_yL < *yL) *yL = this_yL;
    }
}

static void determine_extents(char **infiles, int n_inputs,
                              int *size_x, int *size_y,
                              double *start_x, double *start_y,
                              double *per_x, double *per_y)
{
    // the first input file is the "reference" -- all other metadata
    // must match the first (at least as far as projection, etc)
    meta_parameters *meta0 = meta_read(infiles[0]);

    if (!meta0) {
        asfPrintError("Couldn't read metadata for %s!\n", infiles[0]);
    }

    if (!meta0->projection) {
        asfPrintError("%s is not geocoded!\n", infiles[0]);
    }

    asfPrintStatus("Reference image is: %s\nGeocoding:\n", infiles[0]);
    print_proj_info(meta0);

    // these values must be matched by all images
    double px, py;
    px = *per_x = meta0->projection->perX;
    py = *per_y = meta0->projection->perY;

    // these don't have to be matched, we will update as we go along
    double x0, y0, xL, yL;
    get_corners(meta0, &x0, &y0, &xL, &yL);

    projection_type_t proj_type = meta0->projection->type;

    int i, n_ok = 1, n_bad = 0;
    for (i=1; i<n_inputs; ++i) {
        char *file = infiles[i];
        //asfPrintStatus("  Processing metadata for %s...\n", file);
        meta_parameters *meta = meta_read(file);

        char *why="";
        if (!meta)
            why = "Couldn't read metadata";
        else if (!meta->projection)
            why = "Image is not geocoded";
        else if (meta->projection->perX != px)
            why = "X pixel size doesn't match reference image";
        else if (meta->projection->perY != py)
            why = "Y pixel size doesn't match reference image";
        else if (meta->projection->type != proj_type)
            why = "Image is in a different projection";
        else if (!proj_parms_match(meta0, meta))
            why = "Projection parameters differ";

        if (strlen(why) > 0) {
            ++n_bad;
            asfPrintStatus("Image '%s': NOT OK (%s)\n", file, why);
            infiles[i] = NULL; // mark for future ignore-ation
        } else {
            ++n_ok;
            asfPrintStatus("Image '%s': ok (%dx%d LxS)\n", file,
                meta->general->line_count, meta->general->sample_count);

            double this_x0, this_y0, this_xL, this_yL;
            get_corners(meta, &this_x0, &this_y0, &this_xL, &this_yL);
            update_corners(px, py, &x0, &y0, &xL, &yL,
                           this_x0, this_y0, this_xL, this_yL);
        }

        meta_free(meta);
    }

    if (n_ok < 2) {
        asfPrintError("Not enough images to combine.\n");
    }

    *start_x = x0;
    *start_y = y0;

    // calculate number of lines/samples from corner to corner
    *size_x = (int) ((xL-x0)/px + .5);
    *size_y = (int) ((yL-y0)/py + .5);

    meta_free(meta0);
}

static void add_pixels(FloatImage *out, char *file,
                       int size_x, int size_y,
                       double start_x, double start_y,
                       double per_x, double per_y)
{
    meta_parameters *meta = meta_read(file);

    if (!meta) {
        asfPrintError("Couldn't read metadata for: %s!\n", file);
    }

    // figure out where in the giant image these pixels will go
    int start_line, start_sample;

    // this should work even if per_x / per_y are negative...
    start_sample = (int) ((meta->projection->startX - start_x) / per_x + .5);
    start_line = (int) ((meta->projection->startY - start_y) / per_y + .5);

    int ns = meta->general->sample_count;
    int nl = meta->general->line_count;

    asfPrintStatus("  Location in combined is S:%d-%d, L:%d-%d\n",
        start_sample, start_sample + ns,
        start_line, start_line + nl);

    if (start_sample + ns > out->size_x || start_line + nl > out->size_y) {
        asfPrintError("Image extents were not calculated correctly!\n");
    }

    FILE *img = fopenImage(file, "rb");
    if (!img) {
        asfPrintError("Couldn't open image file: %s!\n", file);
    }

    float *line = MALLOC(sizeof(float)*ns);

    int y;
    for (y=0; y<nl; ++y) {
        get_float_line(img, meta, y, line);

        int x;
        for (x=0; x<ns; ++x) {
            float v = line[x];

            // don't write out "no data" values
            if (v != meta->general->no_data)
                float_image_set_pixel(out, x + start_sample, y + start_line, v);
        }

        asfLineMeter(y, nl);
    }

    fclose(img);
    free(line);
    meta_free(meta);
}

int main(int argc, char *argv[])
{
    if (argc<3) usage();

    char *outfile = argv[1];
    char **infiles = &argv[2];
    int n_inputs = argc - 2;

    int ret, i, size_x, size_y;
    double start_x, start_y;
    double per_x, per_y;

    asfSplashScreen(argc, argv);

    asfPrintStatus("Combining %d files to produce: %s\n", n_inputs, outfile);

    asfPrintStatus("Input files:\n");
    for (i = 0; i < n_inputs; ++i)
        asfPrintStatus("   %d: %s%s\n", i+1, infiles[i], i==0 ? " (reference)" : "");

    // determine image parameters
    determine_extents(infiles, n_inputs, &size_x, &size_y, &start_x, &start_y,
        &per_x, &per_y);

    asfPrintStatus("\nCombined image size: %dx%d LxS\n", size_y, size_x);
    asfPrintStatus("  Start X,Y: %f,%f\n", start_x, start_y);
    asfPrintStatus("    Per X,Y: %.2f,%.2f\n", per_x, per_y);

    // float_image will handle caching of the large output image
    FloatImage *out = float_image_new(size_x, size_y);

    // loop over the input images, last to first, so that the files listed
    // first have their pixels overwrite files listed later on the command line
    int n = argc-1;
    do {
        char *p = argv[n];
        if (p && strlen(p)>0) {
            asfPrintStatus("\nProcessing %s... \n", p);

            // add this image's pixels
            add_pixels(out, p, size_x, size_y, start_x, start_y, per_x, per_y);
        }
    } while (--n>1);

    asfPrintStatus("Combined all images, saving result.\n");

    char *outfile_full = appendExt(outfile, ".img");
    ret = float_image_store(out, outfile_full, fibo_be);
    if (ret!=0) asfPrintError("Error storing output image!\n");
    float_image_free(out);
    free(outfile_full);

    // now the metadata -- use infile1's metadata as the template
    asfPrintStatus("Writing metadata.\n");

    meta_parameters *meta_out = meta_read(argv[2]);

    meta_out->projection->startX = start_x;
    meta_out->projection->startY = start_y;
    meta_out->general->line_count = size_y;
    meta_out->general->sample_count = size_x;

    meta_write(meta_out, outfile);
    meta_free(meta_out);

    asfPrintStatus("Done.\n");
    return 0;
}
