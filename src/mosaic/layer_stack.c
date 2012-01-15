#include <stdio.h>
#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

#include "asf_contact.h"
#include "asf_license.h"
#include "asf_version.h"

#define ASF_NAME_STRING "layer_stack"

void help()
{
    printf(
"Tool name:\n"
"    %s\n\n"
"Usage:\n"
"    %s [-background <value>] [-extent <file>] [-output <file>] <infile list>\n\n"
"Description:\n"
"    This program stacks the input files together, producing an image stack\n"
"    that is the intersect of all listed input images.\n\n"
"Input:\n"
"    The tool is looking for a text file with all input file names, one file\n"
"    name per line.\n\n"
"    All input files must be geocoded to the same projection, with the same\n"
"    projection parameters, and the same pixel size.\n\n"
"Options:\n"
"    -background <value> (-b)\n"
"        Specifies a value to use for background pixels.  If not given, 0 is\n"
"        used.\n"
"    -extent <file>\n"
"        Specifies the file the determines the extent of the image stack.\n"
"    -output <file>\n"
"        The output file is created as a multiband image. The band names are\n"
"        the file names, so that some meaning file names can be used when the\n"
"        ASF internal file is exported.\n"
"    -help\n"
"        Print this help information and exit.\n"
"    -license\n"
"        Print copyright and license for this software and exit.\n"
"    -version\n"
"        Print version and copyright and exit.\n\n"
"See also:\n"
"    asf_mosaic, asf_geocode\n\n"
"Contact:\n"
"%s\n",
ASF_NAME_STRING, ASF_NAME_STRING, ASF_CONTACT_STRING);
    print_version(ASF_NAME_STRING);
    exit(1);
}

void usage()
{
    printf("Usage:\n"
    "    %s [-background <value>] [-extent <file>] [-output <file>] "
"<infile list> \n\n",
	   ASF_NAME_STRING);
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
        if (this_x0 > *x0) *x0 = this_x0;
        if (this_xL < *xL) *xL = this_xL;
    } else {
        // if perX is negative: startX will be the LARGEST x value
        //                      endX will be the SMALLEST x value
        if (this_x0 < *x0) *x0 = this_x0;
        if (this_xL > *xL) *xL = this_xL;
    }

    if (py > 0) {
        // if perY is positive: startY will be the SMALLEST y value
        //                      endY will be the LARGEST y value
        if (this_y0 > *y0) *y0 = this_y0;
        if (this_yL < *yL) *yL = this_yL;
    } else {
        // if perY is negative: startY will be the LARGEST y value
        //                      endY will be the SMALLEST y value
        if (this_y0 < *y0) *y0 = this_y0;
        if (this_yL > *yL) *yL = this_yL;
    }
}

static void determine_extents(char **infiles, int n_inputs,
                              int *size_x, int *size_y, int *n_bands,
                              double *start_x, double *start_y,
                              double *per_x, double *per_y, int extent)
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

    int nBands;
    nBands = *n_bands = meta0->general->band_count;

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
	else if (meta->general->band_count != nBands)
	  why = "Number of bands differ";

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
	    if (!extent)
	      update_corners(px, py, &x0, &y0, &xL, &yL,
			     this_x0, this_y0, this_xL, this_yL);
        }

        meta_free(meta);
    }

    if (n_ok < 2) {
        asfPrintError("Not enough images to mosaic.\n");
    }

    *start_x = x0;
    *start_y = y0;

    // calculate number of lines/samples from corner to corner
    *size_x = (int) ((xL-x0)/px + .5);
    *size_y = (int) ((yL-y0)/py + .5);

    meta_free(meta0);
}

int compare_big_doubles(const double *a, const double *b)
{
  double temp = *a - *b;
  if (temp > 0)
    return 1;
  else if (temp < 0)
    return -1;
  else
    return 0;
}

int compare_small_doubles(const double *a, const double *b)
{
  double temp = *a - *b;
  if (temp > 0)
    return -1;
  else if (temp < 0)
    return 1;
  else
    return 0;
}

static void add_to_stack(char *out, int band, char *file,
                         int size_x, int size_y,
                         double start_x, double start_y,
                         double per_x, double per_y, int multiband)
{
    meta_parameters *metaIn = meta_read(file);
    meta_parameters *metaOut = meta_read(out);
    char *base = (char *) MALLOC(sizeof(char)*512);
    int start_line, start_sample;

    // this should work even if per_x / per_y are negative...
    start_sample = (int) ((start_x - metaIn->projection->startX) / per_x + .5);
    start_line = (int) ((start_y - metaIn->projection->startY) / per_y + .5);

    int ns = metaIn->general->sample_count;
    int nl = metaIn->general->line_count;

    asfPrintStatus("  Location in stacked image is S:%d-%d, L:%d-%d\n",
        start_sample, start_sample + size_x,
        start_line, start_line + size_y);

    if (start_sample + size_x > ns || start_line + size_y > nl) {
        asfPrintError("Image extents were not calculated correctly!\n");
    }

    FILE *fpIn = FOPEN(file, "rb");
    FILE *fpOut;
    char *metaFile = appendExt(out, ".meta");
    if (band > 0 || !multiband) {
      fpOut = FOPEN(out, "ab");
      sprintf(base, ",%s", get_basename(file));
      strcat(metaOut->general->bands, base);
    }
    else {
      fpOut = FOPEN(out, "wb");
      sprintf(base, "%s", get_basename(file));
      strcpy(metaOut->general->bands, base);
    }
    if (multiband)
      metaOut->general->band_count = band + 1;
    else
      metaOut->general->band_count = 1;
    meta_write(metaOut, metaFile);
    float *line = MALLOC(sizeof(float)*size_x);

    int y;
    for (y=start_line; y<start_line+size_y; ++y) {
      get_partial_float_line(fpIn, metaIn, y, start_sample, size_x, line);
      if (multiband) 
	put_band_float_line(fpOut, metaOut, band, y-start_line, line); 
      else
	put_float_line(fpOut, metaOut, y-start_line, line); 
      asfLineMeter(y, start_line+size_y);
    }

    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(line);
    FREE(metaFile);
    FREE(base);
    meta_free(metaIn);
    meta_free(metaOut);
}

void update_location_block(meta_parameters *meta)
{
  if (!meta->projection)
    asfPrintWarning("Image does not have a projection block.\n"
		    "Not updating location block!\n");
  else
    asfPrintWarning("Location block is updated based on the projection "
		    "information.\nThis might no reflect the actual corner "
		    "coordinates as it could\ninclude background values.\n");
  
  double startX = meta->projection->startX;
  double startY = meta->projection->startY;
  double endX = startX + meta->general->sample_count*meta->projection->perX;
  double endY = startY + meta->general->line_count*meta->projection->perY;
  double lat, lon, height;
  proj_to_latlon(meta->projection, startX, startY, 0.0, &lat, &lon, &height);
  meta->location->lat_start_near_range = lat*R2D;
  meta->location->lon_start_near_range = lon*R2D;
  proj_to_latlon(meta->projection, startX, endY, 0.0, &lat, &lon, &height);
  meta->location->lat_end_near_range = lat*R2D;
  meta->location->lon_end_near_range = lon*R2D;
  proj_to_latlon(meta->projection, endX, startY, 0.0, &lat, &lon, &height);
  meta->location->lat_start_far_range = lat*R2D;
  meta->location->lon_start_far_range = lon*R2D;
  proj_to_latlon(meta->projection, endX, endY, 0.0, &lat, &lon, &height);
  meta->location->lat_end_far_range = lat*R2D;
  meta->location->lon_end_far_range = lon*R2D;
}

int main(int argc, char *argv[])
{
  int extentFlag = FALSE, multiband = FALSE;
  char *extent = (char *) MALLOC(sizeof(char)*1024);
  char *outfile = (char *) MALLOC(sizeof(char)*1024);
  
    handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
    if (argc>1 && (strcmp(argv[1], "-help")==0 || strcmp(argv[1],"--help")==0))
        help();
    if (argc<3) usage();

    double background_val=0;
    extract_double_options(&argc, &argv, &background_val, "-background",
                           "--background", "-b", NULL);
    if (extract_string_options(&argc, &argv, extent, "-extent", "--extent", 
			       NULL))
      extentFlag = TRUE;
    if (extract_string_options(&argc, &argv, outfile, "-output", "--output",
			       NULL))
      multiband = TRUE;
    
    char *infile = argv[1];

    int i, size_x, size_y, n_inputs=0, start=0, n_bands;
    double start_x, start_y;
    double per_x, per_y;

    asfSplashScreen(argc, argv);
    char *line = (char *) MALLOC(sizeof(char)*512);
    FILE *fpList = FOPEN(infile, "r");
    while (fgets(line, 512, fpList))
      if (strlen(line) > 0)
        n_inputs++;
    FCLOSE(fpList);
    if (extentFlag) {
      n_inputs++;
      start = 1;
    }
    char **infiles = (char **) MALLOC(sizeof(char *)*n_inputs);
    if (extentFlag) {
      infiles[0] = (char *) MALLOC(sizeof(char)*512);
      strcpy(infiles[0], extent);
    }
    fpList = FOPEN(infile, "r");
    for (i=start; i<n_inputs; i++) {
      fgets(line, 512, fpList);
      chomp(line);
      infiles[i] = (char *) MALLOC(sizeof(char)*512);
      strcpy(infiles[i], line);
    }
    FCLOSE(fpList);
    FREE(line);

    if (multiband)
      asfPrintStatus("Stacking %d files to produce: %s\n", n_inputs, outfile);
    else
      asfPrintStatus("Putting %d files in image stack\n", n_inputs);

    asfPrintStatus("Input files:\n");
    for (i=start; i<n_inputs; ++i)
      asfPrintStatus("   %d: %s%s\n", i+1, infiles[i], i==0 ? " (reference)" : "");

    determine_extents(infiles, n_inputs, &size_x, &size_y, &n_bands,
		      &start_x, &start_y, &per_x, &per_y, extentFlag);

    asfPrintStatus("\nStacked image size: %dx%d LxS\n", size_y, size_x);
    asfPrintStatus("  Start X,Y: %f,%f\n", start_x, start_y);
    asfPrintStatus("    Per X,Y: %.2f,%.2f\n", per_x, per_y);

    meta_parameters *meta_out = meta_read(infiles[0]);
    if (multiband)
      meta_out->general->image_data_type = IMAGE_LAYER_STACK;
    meta_out->projection->startX = start_x;
    meta_out->projection->startY = start_y;
    meta_out->general->line_count = size_y;
    meta_out->general->sample_count = size_x;
    meta_out->general->no_data = background_val;
    update_location_block(meta_out);

    //char *outfile_full = appendExt(outfile, ".img");
    char *outfile_full = (char *) MALLOC(sizeof(char)*1024);

    if (multiband) {
      sprintf(outfile_full, "%s.img", stripExt(outfile));
      meta_write(meta_out, outfile_full);
    }
    for (i=start; i<n_inputs; i++) {
      asfPrintStatus("\nProcessing band %d (%s) ... \n", i, infiles[i]);
      if (!multiband) {
	sprintf(outfile_full, "%s_stack.img", stripExt(infiles[i]));
	meta_write(meta_out, outfile_full);
      }
      add_to_stack(outfile_full, i, infiles[i], size_x, size_y, 
                   start_x, start_y, per_x, per_y, multiband);
    }
    meta_free(meta_out);

    FREE(outfile_full);
    for (i=0; i<n_inputs; i++)
      FREE(infiles[i]);
    FREE(infiles);
    FREE(extent);
    FREE(outfile);

    asfPrintStatus("Done.\n");
    return 0;
}
