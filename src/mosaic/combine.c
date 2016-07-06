#include <stdio.h>
#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "banded_float_image.h"
#include "dateUtil.h"

#include "asf_contact.h"
#include "asf_license.h"
#include "asf_version.h"

#define ASF_NAME_STRING "combine"

static const float_image_byte_order_t fibo_be =
    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;

void help()
{
    printf(
"Tool name:\n"
"    %s\n\n"
"Usage:\n"
"    %s [-preference <value>] [-overlap <value>] [-background <value>]\n"
"      [-list <file>] <outfile> <infile1> <infile2> ... \n\n"
"Description:\n"
"    This program mosaics the input files together, producing an output image\n"
"    that is the union of all listed input images.  Where the input images\n"
"    overlap, the pixel in the image listed earlier on the command-line is\n"
"    chosen.\n\n"
"Input:\n"
"    At least 2 input files are required.  You may list as many input files\n"
"    as desired, however extremely large output files will take some time to\n"
"    process.\n\n"
"    All input files must be geocoded to the same projection, with the same\n"
"    projection parameters, and the same pixel size.\n\n"
"Output:\n"
"    The output file, listed first, is created by the program, and, depending\n"
"    on the locations of the input files relative to each other, can range\n"
"    in size from equal to the largest input image, to much larger than the\n"
"    total size of all input images.\n\n"
"Options:\n"
"    -preference <value>\n"
"        Specifies a preference for the order the images are combined.\n"
"        Valid entries are: north, south, east, west, old, new.\n"
"        The value \"north\", for example,  means the image with the center\n"
"        farthest to the north will be on top of the image stack.\n"
"    -overlap <value>\n"
"        Specifies the overlap preference.\n"
"        Valid entries are: average, minimum, maximum.\n"
"        All value within the image stack are assessed on a pixel basis.\n"
"        Requires the use of the -list option.\n"
"    -background <value> (-b)\n"
"        Specifies a value to use for background pixels.  If not given, 0 is\n"
"        used.\n"
"    -list <file>\n"
"        Use a list file instead of adding images on the command line.\n"
"    -help\n"
"        Print this help information and exit.\n"
"    -license\n"
"        Print copyright and license for this software and exit.\n"
"    -version\n"
"        Print version and copyright and exit.\n\n"
"Examples:\n"
"    %s out in1 in2 in3 in4 in5 in6\n\n"
"Limitations:\n"
"    Theoretically, any size output image will work.  The output image will\n"
"    be cached on disk if it is too large to fit in memory, however in this\n"
"    situation the process will be quite slow.\n\n"
"    All input images MUST be in the same projection, with the same projection\n"
"    parameters, and the same pixel size.\n\n"
"See also:\n"
"    asf_mosaic, asf_geocode\n\n"
"Contact:\n"
"%s\n",
ASF_NAME_STRING, ASF_NAME_STRING, ASF_NAME_STRING,
ASF_CONTACT_STRING);
    print_version(ASF_NAME_STRING);
    exit(1);
}

void usage()
{
    printf("Usage:\n"
           "    %s [-preference <value>] [-overlap <value> ][-background <value>]\n"
           "      [-list <file>] <outfile> <infile1> <infile2> ... \n\n"
           "At least 2 input files are required.\n", ASF_NAME_STRING);
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
	
    case LAT_LONG_PSEUDO_PROJECTION:
        asfPrintStatus(" Projection: Geographic\n");
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

    case LAT_LONG_PSEUDO_PROJECTION:
      return TRUE;
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
                              int *size_x, int *size_y, int *n_bands,
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

int compare_big_doubles(const void *a, const void *b)
{
  double A = *(double*)a;
  double B = *(double*)b;
  double temp = A - B;
  if (temp > 0)
    return 1;
  else if (temp < 0)
    return -1;
  else
    return 0;
}

int compare_small_doubles(const void *a, const void *b)
{
  double A = *(double*)a;
  double B = *(double*)b;
  double temp = A - B;
  if (temp > 0)
    return -1;
  else if (temp < 0)
    return 1;
  else
    return 0;
}

static void sort_input_preference(char **infiles, int n_inputs, 
				  char *preference)
{
  int ii, kk;
  double lat[n_inputs], lon[n_inputs], sec[n_inputs], sorter[n_inputs];
  ymd_date date;
  hms_time time;
  julian_date jd;
  char **tmpfiles = (char **) MALLOC(sizeof(char *)*n_inputs);

  asfPrintStatus("Sort input images for preference: %s\n\n", preference);

  // Read in metadata
  for (ii=0; ii<n_inputs; ii++) {
    tmpfiles[ii] = (char *) MALLOC(sizeof(char)*50);
    meta_parameters *meta = meta_read(infiles[ii]);
    lat[ii] = meta->general->center_latitude;
    lon[ii] = meta->general->center_longitude;
    parse_DMYdate(meta->general->acquisition_date, &date, &time);
    date_ymd2jd(&date, &jd);
    sec[ii] = date2sec(&jd, &time);
    if (strcmp_case(preference, "north") == 0 ||
	strcmp_case(preference, "south") == 0)
      sorter[ii] = lat[ii];
    else if (strcmp_case(preference, "west") == 0 ||
	     strcmp_case(preference, "east") == 0)
      sorter[ii] = lon[ii];
    else if (strcmp_case(preference, "old") == 0 ||
	     strcmp_case(preference, "new") == 0)
      sorter[ii] = sec[ii];
    meta_free(meta);
  }
  
  // Get on with the sorting business
  if (strcmp_case(preference, "south") == 0 ||
      strcmp_case(preference, "west") == 0 ||
      strcmp_case(preference, "old") == 0)
    qsort(sorter, n_inputs, sizeof(double), compare_big_doubles);
  else
    qsort(sorter, n_inputs, sizeof(double), compare_small_doubles);

  // Apply the order to input file list
  for (ii=0; ii<n_inputs; ii++) {
    for (kk=0; kk<n_inputs; kk++) {
      if ((strcmp_case(preference, "north") == 0 ||
	   strcmp_case(preference, "south") == 0) &&
	  sorter[ii] == lat[kk])
	sprintf(tmpfiles[ii], "%s", infiles[kk]);
      else if ((strcmp_case(preference, "east") == 0 ||
		strcmp_case(preference, "west") == 0) &&
	       sorter[ii] == lon[kk])
	sprintf(tmpfiles[ii], "%s", infiles[kk]);
      else if ((strcmp_case(preference, "old") == 0 ||
		strcmp_case(preference, "new") == 0) &&
	       sorter[ii] == sec[kk])
	sprintf(tmpfiles[ii], "%s", infiles[kk]);
    }
  }
  for (ii=0; ii<n_inputs; ii++) {
    sprintf(infiles[ii], "%s", tmpfiles[ii]);
    //printf("Sorted file[%d]: %s\n", ii+1, infiles[ii]);
  }

  // Clean up
  for (ii=0; ii<n_inputs; ii++)
    FREE(tmpfiles[ii]);
  FREE(tmpfiles);
}

static void add_pixels(BandedFloatImage *out, char *file,
                       int image, char *overlap, 
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
    int nb = meta->general->band_count;
    char **bands = extract_band_names(meta->general->bands, nb);

    asfPrintStatus("  Location in combined is S:%d-%d, L:%d-%d\n",
        start_sample, start_sample + ns,
        start_line, start_line + nl);

    if (start_sample + ns > out->images[0]->size_x || 
    	start_line + nl > out->images[0]->size_y) {
        asfPrintError("Image extents were not calculated correctly!\n");
    }

    FILE *img = fopenImage(file, "rb");
    if (!img) {
        asfPrintError("Couldn't open image file: %s!\n", file);
    }

    float *line = MALLOC(sizeof(float)*ns);
    float current;

    int z;
    for (z=0; z<nb; ++z) {

      asfPrintStatus("  Band: %s\n", bands[z]);

      int y;
      for (y=0; y<nl; ++y) {
        get_band_float_line(img, meta, z, y, line);
	
        int x;
        for (x=0; x<ns; ++x) {
	        float v = line[x];

          if (strlen(overlap) > 0 && 
            !FLOAT_EQUIVALENT(v, meta->general->no_data)) {
            current = banded_float_image_get_pixel(out, z, x+start_sample, 
              y+start_line);
            if (strcmp_case(overlap, "minimum") == 0 && current < v &&
              !FLOAT_EQUIVALENT(current, meta->general->no_data)) {
              v = current;
              banded_float_image_set_pixel(out, nb, 
                x+start_sample, y+start_line, (float)image);
            }
            else if (strcmp_case(overlap, "maximum") == 0 && current > v &&
              !FLOAT_EQUIVALENT(current, meta->general->no_data)) {
              v = current;
              banded_float_image_set_pixel(out, nb, 
                x+start_sample, y+start_line, (float)image);
            }
            else if (strcmp_case(overlap, "average") == 0) {
              float image_count = banded_float_image_get_pixel(out, nb, 
                x+start_sample, y+start_line);
              if (image_count > 0)
                v = (image_count*current + v)/(image_count+1);
              banded_float_image_set_pixel(out, nb,
                x+start_sample, y+start_line, image_count+1);
            }
          }
          	  
          // don't write out "no data" values
          if (!FLOAT_EQUIVALENT(v, meta->general->no_data))
            banded_float_image_set_pixel(out, z, 
              x+start_sample, y+start_line, v);
        }
	
        asfLineMeter(y, nl);
      }
    }

    fclose(img);
    free(line);
    meta_free(meta);
}

void update_location_block(meta_parameters *meta)
{
  if (!meta->projection)
    asfPrintWarning("Image does not have a projection block.\n"
		    "Not updating location block!\n");
  else
    asfPrintWarning("Location block is updated based on the projection "
		    "information.\nThis might not reflect the actual corner "
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

void read_input_files(char *listFile, char ***infiles, int *n_inputs)
{
  char line[1024], **inFiles;
  int ii, nFiles = 0;
  FILE *fpIn = FOPEN(listFile, "r");
  while (fgets(line, 1024, fpIn) != NULL)
    nFiles++;
  FCLOSE(fpIn);
  fpIn = FOPEN(listFile, "r");
  inFiles = (char **) MALLOC(sizeof(char *)*nFiles);
  for (ii=0; ii<nFiles; ii++) {
    inFiles[ii] = (char *) MALLOC(sizeof(char)*1024);
    fgets(inFiles[ii], 1024, fpIn);
    chomp(inFiles[ii]);
  }
  FCLOSE(fpIn);
  *infiles = inFiles;
  *n_inputs = nFiles;
}

int main(int argc, char *argv[])
{
    handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
    if (argc>1 && (strcmp(argv[1], "-help")==0 || strcmp(argv[1],"--help")==0))
        help();
    if (argc<3) usage();

    double background_val=0;
    extract_double_options(&argc, &argv, &background_val, "-background",
                           "--background", "-b", NULL);
    char *preference = (char *) MALLOC(sizeof(char)*50);
    strcpy(preference, "");
    extract_string_options(&argc, &argv, preference, "-preference",
			   "--preference", "-p", NULL);
    if (strlen(preference) > 0 &&
	    strcmp_case(preference, "north") != 0 &&
	    strcmp_case(preference, "south") != 0 &&
	    strcmp_case(preference, "east") != 0 &&
	    strcmp_case(preference, "west") != 0 &&
	    strcmp_case(preference, "old") != 0 &&
	    strcmp_case(preference, "new") != 0)
      asfPrintError("Can't handle this preference (%s)!\n", preference);
    
    char *list = (char *) MALLOC(sizeof(char)*512);
    strcpy(list, "");
    extract_string_options(&argc, &argv, list, "-list", "--list", "-l", NULL);

    char *overlap = (char *) MALLOC(sizeof(char)*50);
    strcpy(overlap, "");
    extract_string_options(&argc, &argv, overlap, "-overlap",
			   "--overlap", "-o", NULL);
    if (strlen(overlap) > 0 &&
      strcmp_case(overlap, "average") != 0 &&
      strcmp_case(overlap, "minimum") != 0 &&
      strcmp_case(overlap, "maximum") != 0)
      asfPrintError("Can't handle this overlap option (%s)!\n", overlap);
    if (strlen(overlap) > 0 && strlen(list) == 0)
      asfPrintError("The -overlap option requires the use of the -list option!"
        "\n");
    
    char *outfile = argv[1];
    char **infiles;
    int ii, n_inputs;
    if (strlen(list) > 0)
      read_input_files(list, &infiles, &n_inputs);
    else {
      infiles = &argv[2];
      n_inputs = argc - 2;
    }

    int ret, i, size_x, size_y, n_bands;
    double start_x, start_y;
    double per_x, per_y;

    asfSplashScreen(argc, argv);

    asfPrintStatus("Combining %d files to produce: %s\n", n_inputs, outfile);

    // Sort the input files, in case we have a different preference
    if (strlen(preference) > 0)
      sort_input_preference(infiles, n_inputs, preference);

    asfPrintStatus("Input files:\n");
    for (i = 0; i < n_inputs; ++i)
        asfPrintStatus("   %d: %s%s\n", i+1, infiles[i], i==0 ? " (reference)" : "");

    // determine image parameters
    determine_extents(infiles, n_inputs, &size_x, &size_y, &n_bands,
		      &start_x, &start_y, &per_x, &per_y);

    // float_image will handle cacheing of the large output image
    asfPrintStatus("\nAllocating space for output image ...\n");
    BandedFloatImage *out;
    if (strlen(overlap) > 0) {
      if (background_val != 0)
        out = banded_float_image_new_with_value(n_bands+1, size_x, size_y, 
                  (float)background_val);
      else
        out = banded_float_image_new(n_bands+1, size_x, size_y);
      int ii, kk;
      for (ii=0; ii<size_x; ii++)
        for (kk=0; kk<size_y; kk++)
          banded_float_image_set_pixel(out, n_bands, ii, kk, 0.0);
    }
    else {
      if (background_val != 0)
        out = banded_float_image_new_with_value(n_bands, size_x, size_y, 
                  (float)background_val);
      else
        out = banded_float_image_new(n_bands, size_x, size_y);
    }
    asfPrintStatus("\nCombined image size: %dx%d LxS\n", size_y, size_x);
    asfPrintStatus("  Start X,Y: %f,%f\n", start_x, start_y);
    asfPrintStatus("    Per X,Y: %.2f,%.2f\n", per_x, per_y);

    // loop over the input images, last to first, so that the files listed
    // first have their pixels overwrite files listed later on the command line
    if (strlen(list)) {
      for (ii=0; ii<n_inputs; ii++) {
        asfPrintStatus("\nProcessing %s... \n", infiles[ii]);
        add_pixels(out, infiles[ii], ii, overlap, size_x, size_y, start_x, 
          start_y, per_x, per_y);
      }
    }
    else {
      int n = argc-1;
      do {
        char *p = argv[n];
        if (p && strlen(p)>0) {
            asfPrintStatus("\nProcessing %s... \n", p);

            // add this image's pixels
            add_pixels(out, p, -99, overlap, size_x, size_y, start_x, start_y, 
            per_x, per_y);
        }
      } while (--n>1);
    }

    asfPrintStatus("Combined all images, saving result.\n");

    // first the metadata -- use infile1's metadata as the template
    asfPrintStatus("Writing metadata.\n");

    meta_parameters *meta_out;
    if (strlen(list))
      meta_out = meta_read(infiles[0]);
    else
      meta_out = meta_read(argv[2]);

    meta_out->projection->startX = start_x;
    meta_out->projection->startY = start_y;
    meta_out->general->line_count = size_y;
    meta_out->general->sample_count = size_x;

    // Update location block
    update_location_block(meta_out);

    meta_get_latLon(meta_out, meta_out->general->line_count/2, 
      meta_out->general->sample_count/2, 0,
 		  &meta_out->general->center_latitude, 
 		  &meta_out->general->center_longitude);

    meta_write(meta_out, outfile);

    char *outfile_full = appendExt(outfile, ".img");
    ret = banded_float_image_store(out, outfile_full, fibo_be);
    if (ret!=0) asfPrintError("Error storing output image!\n");
    banded_float_image_free(out);
    free(outfile_full);

    meta_free(meta_out);
    if (strlen(list)) {
      for (ii=0; ii<n_inputs; ii++)
        FREE(infiles[ii]);
      FREE(infiles);
    }

    asfPrintStatus("Done.\n");
    return 0;
}
