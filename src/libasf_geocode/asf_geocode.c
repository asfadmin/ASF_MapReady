// Standard libraries.
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Libraries from packages outside ASF.
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_statistics_double.h>

// Libraries developed at ASF.
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <float_image.h>
#include <uint8_image.h>
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>
#include <asf_glib.h>
#include <asf_nan.h>

// Headers defined by this library.
#include "asf_geocode.h"

const float_image_byte_order_t fibo_be = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;

typedef int project_t(project_parameters_t *pps, double lat, double lon,
		  double height, double *x, double *y, double *z, datum_type_t dtm);
typedef int project_arr_t(project_parameters_t *pps, double *lat, double *lon,
		  double *height, double **projected_x, double **projected_y,
		  double **projected_z, long length, datum_type_t dtm);
typedef int unproject_t(project_parameters_t *pps, double x, double y, double z,
		  double *lat, double *lon, double *height, datum_type_t dtm);
typedef int unproject_arr_t(project_parameters_t *pps, double *x, double *y,
      double *z, double **lat, double **lon,
      double **height, long length, datum_type_t dtm);

static void print_proj_info(projection_type_t projection_type,
                            project_parameters_t *pp)
{
    switch (projection_type)
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
        asfPrintStatus(" Projection: UTM\n   Zone: %d\n\n", pp->utm.zone);
        break;

    case POLAR_STEREOGRAPHIC:
        asfPrintStatus(
            " Projection: Polar Stereographic\n"
            "   Standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Hemisphere: %c\n\n", 
            pp->ps.slat, pp->ps.slon, pp->ps.is_north_pole ? 'N' : 'S');
        break;

    case ALBERS_EQUAL_AREA:
        asfPrintStatus(
            " Projection: Albers Equal Area Conic\n"
            "   First standard parallel: %.4f\n"
            "   Second standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Latitude of origin: %.4f\n\n",
            pp->albers.std_parallel1, pp->albers.std_parallel2,
            pp->albers.center_meridian, pp->albers.orig_latitude);
        break;

    case LAMBERT_CONFORMAL_CONIC:
        asfPrintStatus(
            " Projection: Lambert Conformal Conic\n"
            "   First standard parallel: %.4f\n"
            "   Second standard parallel: %.4f\n"
            "   Central meridian: %.4f\n"
            "   Latitude of origin: %.4f\n\n",
            pp->lamcc.plat1, pp->lamcc.plat2, pp->lamcc.lon0, pp->lamcc.lat0);
        break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
        asfPrintStatus(
            " Projection: Lambert Azimuthal Equal Area\n"
            "   Latitude of origin: %.4f\n"
            "   Central meridian: %.4f\n\n",
            pp->lamaz.center_lat, pp->lamaz.center_lon);
        break;

    default:
        asfPrintError("Projection type not supported!\n");
        break;
    }
}

// Blurb about what the user can do if projection errors are too
// large.  Called when we detect this condition.
static void print_large_error_blurb(int force_flag)
{
  if (!force_flag) {
    asfPrintWarning(
      "Large projection errors occurred!\n\nLarge projection errors can result\n"
      "if your projection parameters do not accurately represent the scene you\n"
      "are geocoding.  You can either re-run geocode using the '--force' option,\n"
      "or adjust your projection parameters to better reflect the scene.\n");
  }
}

static int is_alos_prism(meta_parameters *meta) {
    return strcmp(meta->general->sensor, "ALOS") == 0 &&
        strcmp(meta->general->sensor_name, "PRISM") == 0;
}

static int is_alos_avnir(meta_parameters *meta) {
    return strcmp(meta->general->sensor, "ALOS") == 0 &&
        strcmp(meta->general->sensor_name, "AVNIR") == 0;
}

// Since our normal approach is to pass the datum from the input image
// on through to the (re)projected output image, reprojecting a pixel
// from a lat long pseudoprojected image requires us to do almost
// nothing.  Pseudoprojected images have units of degrees, so all we
// have to do is convert as appropriate.  But we still need these
// functions to use when we need to use a function pointer to perform
// a generic operation.
static int
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
			 double height, double *x, double *y, double *z,
             datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *x = lon * R2D;
  *y = lat * R2D;
  if (z) *z = height;

  return TRUE;
}
static int
project_lat_long_pseudo_inv (project_parameters_t *pps, double x, double y,
			     double z, double *lat, double *lon,
			     double *height, datum_type_t datum)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps; datum = datum;

  *lat = y * D2R;
  *lon = x * D2R;
  if (height) *height = z;

  return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
//
// This comment block describes the general procedure for geocoding
// images stored in a SAR geometry.  For map projecting or
// reprojecting other things (notably DEMs) a seperate code thread
// using a different procedure is used.
//
// We want to find natural cubic splines to form approximating
// functions X and Y st
//
//      X(x, y) = input image x pixel coordinate of projection coordinates x, y
//      Y(x, y) = input image y pixel coordinate of projection coordinates x, y
//
// The basic steps are:
//
//   1.  Find the extent of the input image in projection coordinate
//       space, i.e. the minimum and maximum x and y projection
//       coordinates of all the pixels in the input image.
//
//   2.  Find the pairs of input image pixel coordinates corresponding
//       to the points of an evenly distributed grid in the output
//       projection space.
//
//   3.  Construct interpolating cubic splines for each column of points
//       in the grid.
//
//   4.  For each row in the output image, construct an interpolating
//       spline over the values which result from evaluating the column
//       splines at that y position.
//
//   5.  Verify that the splines aren't introducing too much error away
//       from the control points by examing the errors in the spline
//       approximation compared to the results of the analytical
//       transformation of a denser grid.
//
// The steps don't necessarily occer in exactly this order in the code
// though.
//
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////

// This is the form of the input data we want to fit splines to.
struct data_to_fit {
  size_t grid_size;		// Size of grid of points, in points on a side.
  size_t n;			// Number of transformed points (grid_size^2).
  double *x_proj;		// Projection x coordinates.
  double *y_proj;		// Projection y coordinates.
  // Input image pixel coordinates put 0, 0 at the top left.
  double *x_pix;		// Input image pixel x coordinate.
  double *y_pix;		// Input image pixel y coordinate.

  // These values are like the above ones, and should form a grid
  // covering the same area, but are considerably more sparse.
  size_t sparse_grid_size;
  size_t sparse_n;
  double *sparse_x_proj;
  double *sparse_y_proj;
  double *sparse_x_pix;
  double *sparse_y_pix;
};

///////////////////////////////////////////////////////////////////////////////
//
// Hold your nose...
//
// These used to be function scoped statics in reverse_map_x and
// reverse_map_y routines, but that broke horribly when this program
// got converted into a library function because they never got reset
// between calls, so now these are globals so that we can reset them
// all to their initial state when we get to the end of the function.
// The little '_rmx' and '_rmy' suffixes help remind us that these
// globals go with the reverse_map_x and reverse_map_y routines,
// repsectively.

  // True iff this is our first time through this routine.
  static gboolean first_time_through_rmx = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel_rmx;
  static gsl_spline **y_spline_rmx;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel_rmx;
  static gsl_spline *crnt_rmx;
  // Value of y for which current interpolator works.
  static double last_y_rmx;

  // True iff this is our first time through this routine.
  static gboolean first_time_through_rmy = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel_rmy;
  static gsl_spline **y_spline_rmy;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel_rmy;
  static gsl_spline *crnt_rmy;
  // Value of y for which current interpolator works.
  static double last_y_rmy;

///////////////////////////////////////////////////////////////////////////////

// Reverse map from projection coordinates x, y to input pixel
// coordinate X.  This function looks at the data to fit on the first
// time through, in order to set up vertical splines, after which this
// argument is mostly ignored.  But not entirely, so you can't free or
// change the dtf.  Mapping is efficient only if the y coordinates are
// usually identical between calls, since when y changes a new spline
// between splines has to be created.
static double
reverse_map_x (struct data_to_fit *dtf, double x, double y)
{
  // Convenience aliases.
  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *xpixs = dtf->sparse_x_pix;

  if ( G_UNLIKELY (first_time_through_rmx || y != last_y_rmx) ) {
    if ( !first_time_through_rmx ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel_rmx);
      gsl_spline_free (crnt_rmx);
    } else {
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel_rmx = g_new (gsl_interp_accel *, sgs);
      y_spline_rmx = g_new (gsl_spline *, sgs);
      size_t ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
	gsl_vector *cypv = gsl_vector_alloc (sgs);
	gsl_vector *cxpixv = gsl_vector_alloc (sgs);
	size_t jj;
	for ( jj = 0 ; jj < sgs ; jj++ ) {
	  gsl_vector_set (cypv, jj, yprojs[jj * sgs + ii]);
	  gsl_vector_set (cxpixv, jj, xpixs[jj * sgs + ii]);
	}
	y_accel_rmx[ii] = gsl_interp_accel_alloc ();
	y_spline_rmx[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline_rmx[ii], cypv->data, cxpixv->data, sgs);
	gsl_vector_free (cxpixv);
	gsl_vector_free (cypv);
      }
      first_time_through_rmx = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel_rmx = gsl_interp_accel_alloc ();
    crnt_rmx = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline_rmx[ii], y, y_accel_rmx[ii]);
    }
    gsl_spline_init (crnt_rmx, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y_rmx = y;
  }

  return gsl_spline_eval (crnt_rmx, x, crnt_accel_rmx);
}

// This routine is analagous to reverse_map_x, including the same
// caveats and confusing behavior.
static double
reverse_map_y (struct data_to_fit *dtf, double x, double y)
{
  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *ypixs = dtf->sparse_y_pix;

  if ( G_UNLIKELY (first_time_through_rmy || y != last_y_rmy) ) {
    if ( !first_time_through_rmy ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel_rmy);
      gsl_spline_free (crnt_rmy);
    } else {
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel_rmy = g_new (gsl_interp_accel *, sgs);
      y_spline_rmy = g_new (gsl_spline *, sgs);
      size_t ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
	// Current y projection value.
	gsl_vector *cypv = gsl_vector_alloc (sgs);
	// Current y pixel value.
	gsl_vector *cypixv = gsl_vector_alloc (sgs);
	size_t jj;
	for ( jj = 0 ; jj < sgs ; jj++ ) {
	  gsl_vector_set (cypv, jj, yprojs[jj * sgs + ii]);
	  gsl_vector_set (cypixv, jj, ypixs[jj * sgs + ii]);
	}
	y_accel_rmy[ii] = gsl_interp_accel_alloc ();
	y_spline_rmy[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline_rmy[ii], cypv->data, cypixv->data, sgs);
	gsl_vector_free (cypixv);
	gsl_vector_free (cypv);
      }
      first_time_through_rmy = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel_rmy = gsl_interp_accel_alloc ();
    crnt_rmy = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline_rmy[ii], y, y_accel_rmy[ii]);
    }
    gsl_spline_init (crnt_rmy, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y_rmy = y;
  }

  return gsl_spline_eval (crnt_rmy, x, crnt_accel_rmy);
}

static void determine_projection_fns(int projection_type, project_t **project,
                                     project_arr_t **project_arr, unproject_t **unproject,
                                     unproject_arr_t **unproject_arr)
{
  switch ( projection_type ) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      if (project) *project = project_utm;
      if (project_arr) *project_arr = project_utm_arr;
      if (unproject) *unproject = project_utm_inv;
      if (unproject_arr) *unproject_arr = project_utm_arr_inv;
      break;
    case POLAR_STEREOGRAPHIC:
      if (project) *project = project_ps;
      if (project_arr) *project_arr = project_ps_arr;
      if (unproject) *unproject = project_ps_inv;
      *unproject_arr = project_ps_arr_inv;
      break;
    case ALBERS_EQUAL_AREA:
      if (project) *project = project_albers;
      if (project_arr) *project_arr = project_albers_arr;
      if (unproject) *unproject = project_albers_inv;
      if (unproject_arr) *unproject_arr = project_albers_arr_inv;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      if (project) *project = project_lamcc;
      if (project_arr) *project_arr = project_lamcc_arr;
      if (unproject) *unproject = project_lamcc_inv;
      if (unproject_arr) *unproject_arr = project_lamcc_arr_inv;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      if (project) *project = project_lamaz;
      if (project_arr) *project_arr = project_lamaz_arr;
      if (unproject) *unproject = project_lamaz_inv;
      if (unproject_arr) *unproject_arr = project_lamaz_arr_inv;
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      if (project) *project = project_lat_long_pseudo;
      if (project_arr) *project_arr = NULL; // shouldn't need this
      if (unproject) *unproject = project_lat_long_pseudo_inv;
      if (unproject_arr) *unproject_arr = NULL; // or this
      break;
    default:
      g_assert_not_reached ();
      if (project) *project = NULL;
      if (project_arr) *project_arr = NULL;
      if (unproject) *unproject = NULL;
      if (unproject_arr) *unproject_arr = NULL;
      break;
  }
}

// USGS seamless DEMs use < -900 to indicate "invalid" ...
// How can we generalize this?
static int usgs_invalid(float x) { return x < -900; }

// Similar to float_image_sample, except knows about invalid data
// in DEMs -- returns invalid data if any of the pixels used for
// interpolation is invalid.
static float dem_sample(FloatImage *dem, float x, float y,
                        float_image_sample_method_t sample_method)
{
    switch (sample_method) {
        case FLOAT_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR:
            return float_image_sample(dem, x, y, sample_method);

        case FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR:
        {
            // b = "below", a = "above"
            int xb = floor(x), yb = floor(y), xa = ceil(x), ya = ceil(y);

            float ul = float_image_get_pixel(dem, xb, yb);
            if (usgs_invalid(ul)) return ul;

            float ur = float_image_get_pixel(dem, xa, yb);
            if (usgs_invalid(ur)) return ur;

            float ll = float_image_get_pixel(dem, xb, ya);
            if (usgs_invalid(ll)) return ll;

            float lr = float_image_get_pixel(dem, xa, ya);
            if (usgs_invalid(lr)) return lr;

            float ux = ul + (ur-ul) * (x-xb);
            float lx = ll + (lr-ll) * (x-xb);
            return ux + (lx-ux) * (y-yb);
        }

        case FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC:
        {
            int i, j;
            int xi = floor(x), yi = floor(y);

            int minx = xi-1;
            int miny = yi-1;
            int maxx = xi+2;
            int maxy = yi+2;

            if (minx<0) minx=0;
            if (miny<0) miny=0;
            if (maxx>=(int)dem->size_x) maxx = dem->size_x-1;
            if (maxy>=(int)dem->size_y) maxy = dem->size_y-1;

            for (i=minx; i<=maxx; ++i)
                for (j=miny; j<=maxy; ++j) {
                    float v = float_image_get_pixel(dem, i, j);
                    if (usgs_invalid(v)) return v;
                }

            return float_image_sample(dem, x, y, sample_method);
        }

        default:
            asfPrintError("Invalid sampling method in dem_sample!\n");
    }
    return 0; // not reached
}

int asf_geocode_utm(resample_method_t resample_method, double average_height,
                    datum_type_t datum, double pixel_size,
                    char *band_id, char *in_base_name, char *out_base_name,
                    float background_val)
{
  project_parameters_t pp;
  projection_type_t projection_type = UNIVERSAL_TRANSVERSE_MERCATOR;

  // force calculation of these values
  pp.utm.zone = MAGIC_UNSET_INT;
  pp.utm.lon0 = MAGIC_UNSET_DOUBLE;
  pp.utm.lat0 = MAGIC_UNSET_DOUBLE;

  return asf_geocode(&pp, projection_type, FALSE, resample_method,
		     average_height, datum, pixel_size, band_id,
                     in_base_name, out_base_name, background_val);
}

int asf_geocode_from_proj_file(const char *projection_file,
		 int force_flag, resample_method_t resample_method,
		 double average_height, datum_type_t datum, double pixel_size,
		 char *band_id, char *in_base_name, char *out_base_name,
                 float background_val)
{
  project_parameters_t pp;
  projection_type_t projection_type;

  parse_proj_args_file(projection_file, &pp, &projection_type);

  return asf_geocode(&pp, projection_type, force_flag, resample_method,
		     average_height, datum, pixel_size, band_id,
                     in_base_name, out_base_name, background_val);
}

int asf_geocode(project_parameters_t *pp, projection_type_t projection_type,
                int force_flag, resample_method_t resample_method,
                double average_height, datum_type_t datum, double pixel_size,
                char *band_id, char *in_base_name, char *out_base_name, float background_val)
{
  char input_meta_data[1024];
  char *bands; // A string containing list of available band_id's
  int ret;
  int multiband = 1; // boolean - true means geocode all available bands, false means one band
  int band_count;
  int band_num = 0;

  sprintf(input_meta_data, "%s.meta", in_base_name);
  meta_parameters *imd = meta_read(input_meta_data);
  bands = STRDUP(imd->general->bands);
  band_count = imd->general->band_count;
  if (band_count > MAX_BANDS) {
    asfPrintWarning("Data contains too many bands (%d)\n"
        "asf_geocode cannot geocode more than %d bands\n",
    band_count, MAX_BANDS);
  }
  asfRequire(band_count > 0,
             "Invalid band_count in metadata\n");


  // If the band_id is NULL, is empty, or is equal to 'all'
  // then geocode all available bands, else geocode the selected
  // band
  //if (band_id) band_num = get_band_number(bands, band_count, band_id);
  if (band_id == NULL ||
      (band_id && (strlen(band_id) == 0)) ||
      (band_id && strcmp(uc(band_id), "ALL") == 0)
     ) {
    // Geocode all available bands
    multiband = 1;
    ret = asf_geocode_ext(pp, projection_type, force_flag, resample_method,
                          average_height, datum, pixel_size,
                          multiband, band_num, in_base_name,
                          out_base_name, background_val);
    if (ret != 0)
    {
        char **band_names = extract_band_names(imd->general->bands,
                                               imd->general->band_count);
        asfPrintError("Failed to geocode band %s\n", band_names[band_num]);
    }
  }
  else {
    // Geocode a single selected band

    multiband = 0;
    band_num = get_band_number(bands, band_count, band_id);
    asfRequire(band_num >= 0 && band_num < MAX_BANDS,
               "Selected band number out of range\n");
    ret = asf_geocode_ext(pp, projection_type, force_flag, resample_method,
                          average_height, datum, pixel_size,
                          multiband, band_num, in_base_name,
                          out_base_name, background_val);
    asfRequire(ret == 0,
               "Failed to geocode band number 01\n");
  }

  meta_free(imd);
  FREE(bands);

  return ret;
}

int asf_geocode_ext(project_parameters_t *pp, projection_type_t projection_type,
                    int force_flag, resample_method_t resample_method,
                    double average_height, datum_type_t datum, double pixel_size,
                    int multiband, int band_num, char *in_base_name,
                    char *out_base_name, float background_val)
{
    char *in_base_names[2];
    in_base_names[0] = MALLOC(sizeof(char)*(strlen(in_base_name)+1));
    strcpy(in_base_names[0], in_base_name);
    in_base_names[1] = NULL;

    double lat_min = -999, lon_min = -999;
    double lat_max = 999, lon_max = 999;

    return asf_mosaic(pp, projection_type, force_flag, resample_method,
        average_height, datum, pixel_size, multiband, band_num,
        in_base_names, out_base_name, background_val, lat_min, lat_max,
        lon_min, lon_max);
}

int asf_mosaic(project_parameters_t *pp, projection_type_t projection_type,
               int force_flag, resample_method_t resample_method,
               double average_height, datum_type_t datum, double pixel_size,
               int multiband, int band_num, char **in_base_names,
               char *out_base_name, float background_val, double lat_min,
               double lat_max, double lon_min, double lon_max)
{
  int i,ret;
  int process_as_byte=TRUE;

  // ref_input is which of the input images is the "reference" -- i.e.,
  // which will be used as the template for the output metadata.  This
  // is decided based on which input has the most bands, since the
  // output will have that many bands, too.  n_bands is how many bands
  // are in that reference input file.
  int ref_input=0;
  int n_bands=1;

  void (*report_func) (const char *format, ...);
  report_func = force_flag ? asfPrintWarning : asfPrintError;

  // Assign output filename
  GString *output_image = g_string_new (out_base_name);
  GString *output_meta_data = g_string_new (output_image->str);
  g_string_append (output_meta_data, ".meta");
  g_string_append (output_image, ".img");

  // Determine number of input images
  int n_input_images = 0;
  {
    char **p = in_base_names;
    while (*p++) ++n_input_images;
  }
  if (n_input_images == 0) {
      asfPrintError("Geocode: no input images specified!\n");
  } else if (n_input_images == 1) {
      asfPrintStatus("Geocoding %s\n", in_base_names[0]);
  } else {
      asfPrintStatus("Mosaicking %d images:\n", n_input_images);
      for (i=0; i<n_input_images; ++i)
          asfPrintStatus("  #%d: %s\n", i+1, in_base_names[i]);
      asfPrintStatus("\n");
  }

  // keep track of the input metadata's spheroids.  For projected data,
  // if they all match, then we'll go ahead and use it in the output.
  spheroid_type_t *input_spheroid = NULL;

  //------------------------------------------------------------------------
  // Now working on determining the output file's extents

  // Assign our transformation function pointers to point to the
  // appropriate functions.
  project_t *project;
  project_arr_t *project_arr;
  unproject_t *unproject;
  unproject_arr_t *unproject_arr;

  determine_projection_fns(projection_type, &project, &project_arr,
    &unproject, &unproject_arr);

  // these will hold the extents of the output image in projection
  // coordinates
  double min_x = DBL_MAX;
  double max_x = -DBL_MAX;
  double min_y = DBL_MAX;
  double max_y = -DBL_MAX;

  asfPrintStatus ("Determining input image extents in projection coordinate "
    "space... \n");

  // Loop over the input images to determine output extent
  for (i=0; i<n_input_images; ++i)
  {
    char *in_base_name = STRDUP(in_base_names[i]);
    if (n_input_images > 1)
        asfPrintStatus("Working on input image #%d: %s\n", i+1, in_base_name);

    // strip off known extensions
    char *ext = findExt(in_base_name);
    if (ext) *ext = '\0';

    GString *input_meta_data = g_string_new (in_base_name);
    g_string_append (input_meta_data, ".meta");

    // Input metadata.
    meta_parameters *imd = meta_read (input_meta_data->str);

    if (imd->optical) {
        if (strcmp(imd->general->mode, "1A") == 0 ||
            strcmp(imd->general->mode, "1B1") == 0) {
        asfPrintError("Geocoding %s level %s data is not supported.\n",
                        imd->general->sensor_name, imd->general->mode);
        }
    }

    // the number of bands in the output will be equal to the largest
    // number of bands in all of the inputs.  Any inputs that "run out"
    // of bands will just be ignored when generating that particular
    // band in the output image.
    if (imd->general->band_count > n_bands) {
      n_bands = imd->general->band_count;
      ref_input = i;
    }

    // If we have an already projected image as input, we will need to
    // be able to unproject its coordinates back to lat long before we
    // can reproject them into the desired projection, so here we select
    // a function that can do that.

    // Flag true iff input image not map projected or pseudoprojected.
    gboolean input_projected = FALSE;
    // Convenience alias (valid iff input_projected).
    meta_projection *ipb = imd->projection;
    project_parameters_t *ipp = (ipb) ? &imd->projection->param : NULL;
    project_t *project_input;
    unproject_t *unproject_input;

    if ( ((imd->sar && imd->sar->image_type == 'P') || imd->general->image_data_type == DEM)
        && imd->projection && imd->projection->type != SCANSAR_PROJECTION )
    {
        input_projected = TRUE;
        determine_projection_fns(imd->projection->type, &project_input, NULL,
          &unproject_input, NULL);
    }

    // first time through, set up the defaults for the projection -- e.g, any
    // scene-related defaults are applied.
    if (i==0) {
      apply_defaults (projection_type, pp, imd, &average_height, &pixel_size);
      check_parameters (projection_type, datum, pp, imd, force_flag);
    }

    // Don't allow height correction to be applied to ALOS Prism or Avnir
    if (average_height != 0.0 && (is_alos_prism(imd) || is_alos_avnir(imd))) {
        asfPrintWarning("Specified height correction of %gm will be ignored for "
                        "ALOS Prism/Avnir data.\n", average_height);
        average_height = 0.0;
    }

    // Old Scansar data needs a 400m height adjustment.
    if (imd->sar && imd->sar->image_type == 'P' &&
        imd->projection && imd->projection->type == SCANSAR_PROJECTION) {
        asfPrintStatus("Will apply 400m height correction for SCANSAR data.\n");
        average_height -= 400;
    }

    project_set_avg_height (average_height);

    // Input image dimensions in pixels in x and y directions.
    size_t ii_size_x = imd->general->sample_count;
    size_t ii_size_y = imd->general->line_count;

    // The latitude and longitude of the center of the image.
    double lat_0, lon_0;
    ret = meta_get_latLon (imd, ii_size_y / 2.0, ii_size_x / 2.0, average_height,
                           &lat_0, &lon_0);

    // If the user's selected datum is NAD27 -- test it out to make sure we
    // have the capability of using it.  If we don't we'll fall back to WGS84.
    // This is because the NAD27 transformations depend on grid shift files
    // that are not available for all parts of the world (specifically, outside
    // of North America).
    if (datum == NAD27_DATUM) {
        int ok = test_nad27(lat_0, lon_0);
        double lat, lon;

        if (ok) {
            meta_get_latLon (imd, 0, 0, average_height, &lat, &lon);
            ok = test_nad27(lat, lon);

            meta_get_latLon (imd, ii_size_y, 0, average_height, &lat, &lon);
            ok &= test_nad27(lat, lon);

            meta_get_latLon (imd, 0, ii_size_x, average_height, &lat, &lon);
            ok &= test_nad27(lat, lon);

            meta_get_latLon (imd, ii_size_y, ii_size_x, average_height,
                            &lat, &lon);
            ok &= test_nad27(lat, lon);
        }

        if (!ok) {
            asfPrintWarning("The scene is in a region of the world that "
                            "doesn't have NAD27 grid shift\ndata available.  "
                            "Using WGS84 instead.\n");
            datum = WGS84_DATUM;
        }
    }

    if (input_projected) {
      if (i == 0) {
        input_spheroid = MALLOC(sizeof(spheroid_type_t));
        *input_spheroid = imd->projection->spheroid;
      } else {
        if (input_spheroid && *input_spheroid != imd->projection->spheroid) {
          free(input_spheroid);
          input_spheroid = NULL;
        }
      }
    }

    // First we march around the entire outside of the image and compute
    // projection coordinates for every pixel, keeping track of the
    // minimum and maximum projection coordinates in each dimension.
    // This lets us determine the exact extent of the projected image in
    // projection coordinates.

    { // Scoping block.
        // Number of pixels in the edge of the image.
        size_t edge_point_count = 2 * ii_size_x + 2 * ii_size_y - 4;
        double *lats = g_new (double, edge_point_count);
        double *lons = g_new (double, edge_point_count);
        size_t current_edge_point = 0;
        size_t ii = 0, jj = 0;
        for ( ; ii < ii_size_x - 1 ; ii++ ) {
            if ( input_projected ) {
                double xpc = ipb->startX + ipb->perX * ii;
                double ypc = ipb->startY + ipb->perY * jj;
                ret = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]), NULL,
                    imd->projection->datum);
                g_assert (ret);
            }
            else {
                ret = meta_get_latLon (imd, (double)jj, (double)ii, average_height,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]));
                //g_assert (ret == 0);
                asfRequire(ret == 0, "Unable to determine latitude and longitude "
                    "from current edge point\n");
                lats[current_edge_point] *= D2R;
                lons[current_edge_point] *= D2R;
            }
            current_edge_point++;

            asfPercentMeter((float)current_edge_point / (float)edge_point_count);
        }
        for ( ; jj < ii_size_y - 1 ; jj++ ) {
            if ( input_projected ) {
                double xpc = ipb->startX + ipb->perX * ii;
                double ypc = ipb->startY + ipb->perY * jj;
                ret = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]), NULL,
                    imd->projection->datum);
                g_assert (ret);
            }
            else {
                ret = meta_get_latLon (imd, (double)jj, (double)ii, average_height,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]));
                g_assert (ret == 0);
                lats[current_edge_point] *= D2R;
                lons[current_edge_point] *= D2R;
            }
            current_edge_point++;

            asfPercentMeter((float)current_edge_point / (float)edge_point_count);
        }
        for ( ; ii > 0 ; ii-- ) {
            if ( input_projected ) {
                double xpc = ipb->startX + ipb->perX * ii;
                double ypc = ipb->startY + ipb->perY * jj;
                ret = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]), NULL,
                    imd->projection->datum);
                g_assert (ret);
            }
            else {
                ret = meta_get_latLon (imd, (double)jj, (double)ii, average_height,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]));
                g_assert (ret == 0);
                lats[current_edge_point] *= D2R;
                lons[current_edge_point] *= D2R;
            }
            current_edge_point++;

            asfPercentMeter((float)current_edge_point / (float)edge_point_count);
        }
        for ( ; jj > 0 ; jj-- ) {
            if ( input_projected ) {
                double xpc = ipb->startX + ipb->perX * ii;
                double ypc = ipb->startY + ipb->perY * jj;
                ret = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]), NULL,
                    imd->projection->datum);
                g_assert (ret);
            }
            else {
                ret = meta_get_latLon (imd, (double)jj, (double)ii, average_height,
                    &(lats[current_edge_point]),
                    &(lons[current_edge_point]));
                g_assert (ret == 0);
                lats[current_edge_point] *= D2R;
                lons[current_edge_point] *= D2R;
            }
            current_edge_point++;

            asfPercentMeter((float)current_edge_point / (float)edge_point_count);
        }
        g_assert (current_edge_point == edge_point_count);
        // Pointers to arrays of projected coordinates to be filled in.
        // The projection function will allocate this memory itself.
        double *x = NULL, *y = NULL;
        x = y = NULL;
        // Project all the edge pixels.
        ret = project_arr (pp, lats, lons, NULL, &x, &y, NULL,
            edge_point_count, datum);
        g_assert (ret == TRUE);

        // Find the extents of the image in projection coordinates.
        for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
            if ( x[ii] < min_x ) { min_x = x[ii]; }
            if ( x[ii] > max_x ) { max_x = x[ii]; }
            if ( y[ii] < min_y ) { min_y = y[ii]; }
            if ( y[ii] > max_y ) { max_y = y[ii]; }
        }

        free (y);
        free (x);
        g_free (lons);
        g_free (lats);
    }

    // If the input pixel size is -1, that means the user didn't specify one.
    // So, we will use the FIRST input file's pixel size.  The second (and
    // later) times through this loop, the pixel_size will already have been
    // set to the first file's pixel size, and so we'll never go into this
    // if() block when i>0.
    if (pixel_size < 0)
    {
        g_assert(i==0);
        if (n_input_images > 1)
            asfPrintStatus("No pixel size specified.\nUsing azimuth pixel size"
                "from the first image's metadata: %f\n", imd->general->y_pixel_size);
        else
            asfPrintStatus("No pixel size specified, using azimuth pixel size from "
                "metadata: %f\n", imd->general->y_pixel_size);
        pixel_size = imd->general->y_pixel_size;
    }

    // If all input metadata is byte, we will store everything as bytes,
    // in order to save memory.  (But the math will use floating point.)
    if (imd->general->data_type != BYTE)
        process_as_byte = FALSE;

    // done with this image's metadata
    meta_free(imd);
    g_string_free(input_meta_data, TRUE);
    free(in_base_name);
  }

  asfPrintStatus("Done.\n\nProcessing this data as: %s\n",
    process_as_byte ? "BYTE" : "REAL32");

  if (n_bands > 1)
    asfPrintStatus("Number of bands in the output: %d\n", n_bands);
    
  // now apply the input lat/lon "bounding box" restriction to the
  // calculated extents.  Do this by converting the 4 corner points of
  // the lat/lon box.
  {
    double x, y, z;
    // this time, we update the min if we're GREATER, max if we're SMALLER
    if (lat_min >= -90 && lon_min >= -180) {
      project (pp, lat_min, lon_min, average_height, &x, &y, &z, datum);
      if (x > min_x) min_x = x;    if (x < max_x) max_x = x;
      if (y > min_y) min_y = y;    if (y < max_y) max_y = y;
    }
    if (lat_min >= -90 && lon_max <= 180) {
      project (pp, lat_min, lon_max, average_height, &x, &y, &z, datum);
      if (x > min_x) min_x = x;    if (x < max_x) max_x = x;
      if (y > min_y) min_y = y;    if (y < max_y) max_y = y;
    }
    if (lat_max <= 90 && lon_min >= -180) {
      project (pp, lat_max, lon_min, average_height, &x, &y, &z, datum);
      if (x > min_x) min_x = x;    if (x < max_x) max_x = x;
      if (y > min_y) min_y = y;    if (y < max_y) max_y = y;
    }
    if (lat_max <= 90 && lon_max <= 180) {
      project (pp, lat_max, lon_max, average_height, &x, &y, &z, datum);
      if (x > min_x) min_x = x;    if (x < max_x) max_x = x;
      if (y > min_y) min_y = y;    if (y < max_y) max_y = y;
    }
  }

  // Projection coordinates per pixel in output image.  There is a
  // significant assumption being made here: we assume that the
  // projection coordinates (which are in meters) come at least
  // reasonably close to the ground distances.  This way, when we set
  // the projection coordinates per pixel in the output image equal to
  // the pixel size of the input image, we should be resampling at
  // close to one-to-one (which is where resampling works and we don't
  // have to worry about pixel averaging or anything).
  double pc_per_x = pixel_size;
  double pc_per_y = pixel_size;

  // Maximum pixel indicies in output image.
  size_t oix_max = ceil ((max_x - min_x) / pc_per_x);
  size_t oiy_max = ceil ((max_y - min_y) / pc_per_y);

  // Lat/Lon of the center of the output image
  double output_lat_0, output_lon_0, z;
  double center_x = min_x + (max_x - min_x)/2, center_y = min_y + (max_y-min_y)/2;
  unproject(pp, center_x, center_y, average_height, &output_lat_0,
    &output_lon_0, &z, datum);

  asfPrintStatus("Output Image Information:\n");
  asfPrintStatus("Size: %dx%d LxS\n", oiy_max+1, oix_max+1);
  asfPrintStatus("Center: X,Y: %.1f,%.1f   Lat,Lon: %.3f,%.3f\n",
    center_x, center_y, output_lat_0*R2D, output_lon_0*R2D);
  asfPrintStatus("Pixel size: %.1f m\n", pixel_size);
  if (average_height != 0.0)
      asfPrintStatus("Height correction: %fm.\n", average_height);
  print_proj_info(projection_type, pp);

  // Translate the command line notion of the resampling method into
  // the lingo known by the float_image and uint8_image classes.
  // The compiler is reassured with a default.
  float_image_sample_method_t float_image_sample_method
    = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
  uint8_image_sample_method_t uint8_image_sample_method
    = UINT8_IMAGE_SAMPLE_METHOD_BILINEAR;
  
  switch ( resample_method ) {
    case RESAMPLE_NEAREST_NEIGHBOR:
      float_image_sample_method =
        FLOAT_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR;
      uint8_image_sample_method =
        UINT8_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR;
      break;
    case RESAMPLE_BILINEAR:
      float_image_sample_method =
        FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
      uint8_image_sample_method =
        UINT8_IMAGE_SAMPLE_METHOD_BILINEAR;
      break;
    case RESAMPLE_BICUBIC:
      float_image_sample_method =
        FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC;
      uint8_image_sample_method =
        UINT8_IMAGE_SAMPLE_METHOD_BICUBIC;
      break;
    default:
      g_assert_not_reached ();
  }

  // Generate the output filename
  GString *output_file = g_string_new (output_image->str);
  g_string_append (output_file, ".img");

  //-------------------------------------------------------------------------
  // Now working on generating the output metadata file

  // Each band of the input image will map using the same mapping function
  // (i.e., splines).  Each input image will map using (presumably)
  // different mapping functions.  It is expensive to calculate the
  // splines, so we should loop over the bands in the inner loop, and
  // the images in the outer.  This isn't too great, however, for the
  // output -- ideally, we would generate the bands in the inner loop,
  // to avoid having to store an array of float_images (for each band).
  // We're stuck with the array, though.  We can use the almost
  // obsoleted banded_float_image class.

  // However, if we just have one output image, we can skip the
  // banded_float_image, and just write the final outputs directly,
  // just as in the old geocode (in fact, it *is* the old geocode).

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from input image with the most bands,
  // and add the geocoding parameters.
  meta_parameters *omd = meta_read (in_base_names[ref_input]);
  g_assert(omd->general->band_count == n_bands);

  double y_pixel_size = omd->general->y_pixel_size;

  // Check metadata ptrs (omd->general is theoretically guaranteed good)
  if (omd->projection) {
    if (omd->projection->perY > 0) {
      g_assert (0);		/* Shouldn't happen.  */
      pc_per_y = (int) (omd->projection->perY / y_pixel_size + 0.5) * pixel_size;
    }
    else {
      pc_per_y = (int) (-omd->projection->perY / y_pixel_size + 0.5) * pixel_size;
    }
  }
  if (omd->projection == NULL) {
    omd->projection = meta_projection_init();
  }
  asfRequire(omd->general != NULL && omd->projection != NULL,
             "Could not initialize output metadata structures.\n");

  // Update no data value
  if (!meta_is_valid_double(background_val)) background_val = 0;
  if (process_as_byte && background_val < 0) background_val = 0;
  if (process_as_byte && background_val > 255) background_val = 255;
  omd->general->no_data = background_val;

  omd->general->x_pixel_size = pixel_size;
  omd->general->y_pixel_size = pixel_size;
  omd->general->line_count = oiy_max + 1;
  omd->general->sample_count = oix_max + 1;
  if (omd->sar) {
      omd->sar->image_type = 'P';
      // It doesn't make sense to update these, we're just keeping them
      // for 'historical' info ...
      //double x_scale = pixel_size / omd->general->x_pixel_size;
      //double y_scale = pixel_size / y_pixel_size;
      //omd->sar->range_time_per_pixel *= x_scale;
      //omd->sar->azimuth_time_per_pixel *= y_scale;
      //omd->sar->range_doppler_coefficients[1] *= x_scale;
      //omd->sar->range_doppler_coefficients[2] *= x_scale * x_scale;
      //omd->sar->azimuth_doppler_coefficients[1] *= y_scale;
      //omd->sar->azimuth_doppler_coefficients[2] *= y_scale * y_scale;
  }
  omd->projection->type = projection_type;
  omd->projection->startX = min_x;
  omd->projection->startY = max_y;
  omd->projection->perX = pc_per_x;
  omd->projection->perY = -pc_per_y;
  strcpy (omd->projection->units, "meters");
  if ( output_lat_0 > 0.0 ) {
    omd->projection->hem = 'N';
  }
  else {
    omd->projection->hem = 'S';
  }

  /* output the correct earth radii based on the datum that was used
  to do the projection */
  omd->projection->datum = datum;
  if (datum == WGS84_DATUM) {
    const double wgs84_semiminor_axis
        = WGS84_SEMIMAJOR * (1 - (1 / WGS84_INV_FLATTENING));
    omd->projection->re_major = WGS84_SEMIMAJOR;
    omd->projection->re_minor = wgs84_semiminor_axis;
  } else if (datum == NAD27_DATUM) {
    // NAD27 datum is based on clark 1866 ellipsoid
    const double nad27_semiminor_axis
        = CLARKE1866_SEMIMAJOR * (1 - (1 / CLARKE1866_INV_FLATTENING));
    omd->projection->re_major = CLARKE1866_SEMIMAJOR;
    omd->projection->re_minor = nad27_semiminor_axis;
  } else if (datum == NAD83_DATUM) {
    // NAD83 datum is based on GRS80 ellipsoid
    const double nad83_semiminor_axis
        = GRS1980_SEMIMAJOR * (1 - (1 / GRS1980_INV_FLATTENING));
    omd->projection->re_major = GRS1980_SEMIMAJOR;
    omd->projection->re_minor = nad83_semiminor_axis;
  } else if (datum == ITRF97_DATUM) {
    // ITRF97 datum is based on WGS84 (btw)
    const double itrf97_semiminor_axis
        = INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SEMIMAJOR *
        (1 - (1 / INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_INV_FLATTENING));
    omd->projection->re_major = INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SEMIMAJOR;
    omd->projection->re_minor = itrf97_semiminor_axis;
  } else {
    asfPrintError("Unsupported datum! %d\n", datum_toString(datum));
  }

  // NOTE: If we ever allow the user to provide a spheroid
  // selection on the command line (asf_convert, asf_geocode)
  // or via the GUI (asf_convert_gui) then this will need
  // to change, but for now, associate a spheroid with
  // the datum based on standard use.

  // Use the input projection's spheroid if they all had the same
  // one.
  if (input_spheroid)
    omd->projection->spheroid = *input_spheroid;
  else
    omd->projection->spheroid = datum_spheroid(datum);
  omd->projection->param = *pp;
  free(input_spheroid);
  input_spheroid = NULL;

  // Adjust bands and band_count for the case where the user
  // selected to geocode only a single band out of a multi-band
  // file
  char **band_name =
    extract_band_names(omd->general->bands, omd->general->band_count);

  if (!multiband) {
    if (band_name)
      sprintf(omd->general->bands, "%s", band_name[band_num]);
    else
      strcpy(omd->general->bands, MAGIC_UNSET_STRING);

    omd->general->band_count = 1;
  }

  meta_write (omd, output_meta_data->str);

  //-----------------------------------------------------------------------------
  // Now working on generating the output images

  double *projX = MALLOC(sizeof(double)*(oix_max+1));
  double *projY = MALLOC(sizeof(double)*(oix_max+1));

  // When mosaicing -- use banded_float_image to store the output, write
  //                   it out after processing all inputs
  // When geocoding -- use float arrays to store the output, and write it
  //                   out line-by-line

  // one and only one of these will be non-NULL.  The flag output_by_line
  // indicates which one
  BandedFloatImage *output_bfi = NULL;
  float *output_line = NULL;
  int output_by_line = n_input_images == 1;

  if (n_input_images > 1) {
    output_bfi = banded_float_image_new(n_bands, oix_max+1, oiy_max+1);
  } else {
    output_line = MALLOC(sizeof(float)*(oix_max+1));
  }

  // loop over the input images
  for(i=0; i<n_input_images; ++i) {

    // Figure out this input image's info
    char *in_base_name = STRDUP(in_base_names[i]);
    if (n_input_images > 1)
        asfPrintStatus("\nWorking on input image #%d: %s\n", i+1, in_base_name);

    // strip off known extensions
    char *ext = findExt(in_base_name);
    if (ext) *ext = '\0';

    GString *input_image = g_string_new (in_base_name);
    GString *input_meta_data = g_string_new (input_image->str);
    g_string_append (input_meta_data, ".meta");
    g_string_append (input_image, ".img");

    // Input metadata.
    meta_parameters *imd = meta_read (input_meta_data->str);

    // Issue a warning when the chosen pixel size is smaller than the
    // input pixel size.
    if ( GSL_MIN(imd->general->x_pixel_size,
	        imd->general->y_pixel_size) > pixel_size ) {
        asfPrintWarning
        ("Requested pixel size %lf is smaller then the input image resolution "
        "(%le meters).\n", pixel_size,
        GSL_MIN (imd->general->x_pixel_size, imd->general->y_pixel_size));
    }

    // The pixel size requested by the user better not oversample by
    // the factor of 2.  Specifying --force will skip this check
    if (!force_flag && GSL_MIN(imd->general->x_pixel_size,
	        imd->general->y_pixel_size) > (2*pixel_size) ) {
        report_func
        ("Requested pixel size %lf is smaller then the minimum implied by half \n"
        "the input image resolution (%le meters), this is not supported.\n",
        pixel_size, GSL_MIN (imd->general->x_pixel_size,
			        imd->general->y_pixel_size));
    }

    // Input image dimensions in pixels in x and y directions.
    size_t ii_size_x = imd->general->sample_count;
    size_t ii_size_y = imd->general->line_count;

    // The latitude and longitude of the center of the image.
    double lat_0, lon_0;
    ret = meta_get_latLon (imd, ii_size_y / 2.0, ii_size_x / 2.0, average_height,
                           &lat_0, &lon_0);

    // Flag true iff input image not map projected or pseudoprojected.
    gboolean input_projected = FALSE;
    // Convenience alias (valid iff input_projected).
    meta_projection *ipb = imd->projection;
    project_parameters_t *ipp = (ipb) ? &imd->projection->param : NULL;
    project_t *project_input;
    unproject_t *unproject_input;

    if ( ((imd->sar && imd->sar->image_type == 'P') || imd->general->image_data_type == DEM)
        && imd->projection && imd->projection->type != SCANSAR_PROJECTION )
    {
        input_projected = TRUE;
        determine_projection_fns(imd->projection->type, &project_input, NULL,
          &unproject_input, NULL);
    }

    // Generate some mappings between output image projection
    // coordinates and input image pixel coordinates, using proj.  We
    // compute transformations for points on a grid_size * grid_size
    // grid and a sparse_grid_size * sparse_grid_size grid.
    asfPrintStatus ("Performing analytical projection of a spatially "
		    "distributed\nsubset of input image pixels...\n");
    fflush (stdout);
    double x_range_size = max_x - min_x, y_range_size = max_y - min_y;
    // This grid size seems to work pretty well in general for our
    // products (good accuracy everywhere, decent speed).
    size_t grid_size = 131;
    // However, there isn't much point in using as many grid points as
    // we have pixels, so for small tiles, we set this to about 10
    // percent of larger image dimension in pixels.
    if ( ii_size_x / grid_size < 10 && ii_size_y / grid_size < 10 ) {
        grid_size = GSL_MAX (ii_size_x, ii_size_y) / 10;
        if ( grid_size % 2 != 1 )
            grid_size++;
    }
    g_assert (grid_size % 2 == 1);
    size_t mapping_count = pow ((double) grid_size, 2.0);
    struct data_to_fit dtf;
    dtf.grid_size = grid_size;
    dtf.n = mapping_count;
    dtf.x_proj = g_new0 (double, mapping_count);
    dtf.y_proj = g_new0 (double, mapping_count);
    dtf.x_pix = g_new0 (double, mapping_count);
    dtf.y_pix = g_new0 (double, mapping_count);
    // Determine the density and stride for the sparse grid.
    const size_t sparse_grid_sample_stride = 2;
    const size_t sparse_grid_size = grid_size / 2 + 1;
    size_t sparse_mapping_count = pow ((double) sparse_grid_size, 2.0);
    dtf.sparse_grid_size = sparse_grid_size;
    dtf.sparse_n = sparse_mapping_count;
    dtf.sparse_x_proj = g_new0 (double, sparse_mapping_count);
    dtf.sparse_y_proj = g_new0 (double, sparse_mapping_count);
    dtf.sparse_x_pix = g_new0 (double, sparse_mapping_count);
    dtf.sparse_y_pix = g_new0 (double, sparse_mapping_count);
    // Spacing between grid points, in output projection coordinates.
    double x_spacing = x_range_size / (grid_size - 1);
    double y_spacing = y_range_size / (grid_size - 1);
    // Index into the flattened list of mappings we want to produce.
    size_t current_mapping = 0;
    size_t current_sparse_mapping = 0;
    size_t ii;

    for ( ii = 0 ; ii < grid_size ; ii++ ) {
        size_t jj;
        for ( jj = 0 ; jj < grid_size ; jj++ ) {
            g_assert (sizeof (long int) >= sizeof (size_t));
            // Projection coordinates for the current grid point.
            double cxproj = min_x + x_spacing * jj;
            double cyproj = min_y + y_spacing * ii;

            // Corresponding latitude and longitude.
            double lat, lon;
            ret = unproject (pp, cxproj, cyproj, ASF_PROJ_NO_HEIGHT,
                &lat, &lon, NULL, datum);
            if ( !ret ) {
                // Details of the error should have already been printed.
                asfPrintError ("Projection Error!\n");
            }

            lat *= R2D;
            lon *= R2D;

            // here we have some kludgery to handle crossing the meridian
            if (fabs(lon-lon_0) > 300) {
                if (lon_0 < 0 && lon > 0) lon -= 360;
                if (lon_0 > 0 && lon < 0) lon += 360;
            }

            // Corresponding pixel indicies in input image.
            double x_pix, y_pix;
            if ( input_projected ) {
                // Input projection coordinates of the current pixel.
                double ipcx, ipcy;
                ret = project_input (ipp, D2R*lat, D2R*lon, ASF_PROJ_NO_HEIGHT,
                    &ipcx, &ipcy, NULL, imd->projection->datum);
                if ( ret == 0 ) {
                    g_assert_not_reached ();
                }
                g_assert (ret);
                // Find the input image pixel indicies corresponding to input
                // projection coordinates.
                x_pix = (ipcx - ipb->startX) / ipb->perX;
                y_pix = (ipcy - ipb->startY) / ipb->perY;
            }
            else {
                ret = meta_get_lineSamp (imd, lat, lon, average_height,
                    &y_pix, &x_pix);
                //g_assert (ret == 0);
                if (ret != 0) {
                    asfPrintError("Failed to determine line and sample from latitude "
                        "and longitude\nLat: %f, Lon: %f\n", lat, lon);
                }
            }
            dtf.x_proj[current_mapping] = cxproj;
            dtf.y_proj[current_mapping] = cyproj;
            dtf.x_pix[current_mapping] = x_pix;
            dtf.y_pix[current_mapping] = y_pix;

            if ( ii % sparse_grid_sample_stride == 0 &&
                 jj % sparse_grid_sample_stride == 0 ) 
            {
                dtf.sparse_x_proj[current_sparse_mapping] = cxproj;
                dtf.sparse_y_proj[current_sparse_mapping] = cyproj;
                dtf.sparse_x_pix[current_sparse_mapping] = x_pix;
                dtf.sparse_y_pix[current_sparse_mapping] = y_pix;
                current_sparse_mapping++;
            }
            current_mapping++;

            asfPercentMeter((float)current_mapping / (float)(grid_size*grid_size));
        }
    }

    // Here are some convenience macros for the spline model.
#define X_PIXEL(x, y) reverse_map_x (&dtf, x, y)
#define Y_PIXEL(x, y) reverse_map_y (&dtf, x, y)

    // We want to choke if our worst point in the model is off by this
    // many pixels or more.
    double max_allowable_error = 1.25;

    // Check the health of the our spline model by comparing the input
    // image pixel coordinates predicted by the model for each point
    // with the known values.
    {
        // This is a small image which will show a visual of the
        // distribution of errors in the output grid.
        FloatImage *error_map = float_image_new (grid_size, grid_size);

        gsl_vector *model_x_errors = gsl_vector_alloc (dtf.n);
        gsl_vector *model_y_errors = gsl_vector_alloc (dtf.n);
        gsl_vector *model_errors = gsl_vector_alloc (dtf.n);
        for ( ii = 0 ; ii < dtf.n ; ii++ ) {
            // x pixel index in input image as predicted by model.
            double xpfm = X_PIXEL (dtf.x_proj[ii], dtf.y_proj[ii]);
            double ypfm = Y_PIXEL (dtf.x_proj[ii], dtf.y_proj[ii]);
            double x_error = xpfm - dtf.x_pix[ii];
            double y_error = ypfm - dtf.y_pix[ii];
            double error_distance = sqrt (pow (x_error, 2) + pow (y_error, 2));
            float_image_set_pixel (error_map, ii % grid_size, ii / grid_size,
			            error_distance);
            gsl_vector_set (model_x_errors, ii, x_error);
            gsl_vector_set (model_y_errors, ii, y_error);
            gsl_vector_set (model_errors, ii, error_distance);
        }
        // Uncomment the following 2 lines to get an image showing the
        // distribution of errors in the approximating grid.
        //float_image_export_as_jpeg (error_map, "error_map.jpg", grid_size, 0);
        float_image_free (error_map);
        double mean_error
            = gsl_stats_mean (model_errors->data, model_errors->stride,
			        model_errors->size);
        double error_standard_deviation
            = gsl_stats_sd_m (model_errors->data, model_errors->stride,
			        model_errors->size, mean_error);

        double max_x_error = gsl_vector_max (model_x_errors);
        double min_x_error = gsl_vector_min (model_x_errors);
        double largest_x_error;
        if ( fabs (max_x_error) > fabs (min_x_error) ) {
            largest_x_error = max_x_error;
        }
        else {
            largest_x_error = min_x_error;
        }

        double max_y_error = gsl_vector_max (model_y_errors);
        double min_y_error = gsl_vector_min (model_y_errors);
        double largest_y_error;
        if ( fabs (max_y_error) > fabs (min_y_error) ) {
            largest_y_error = max_y_error;
        }
        else {
            largest_y_error = min_y_error;
        }

        double largest_error = gsl_vector_max (model_errors);
        if ( largest_error > max_allowable_error ) {
            print_large_error_blurb(force_flag);
	        report_func("Largest error was larger than maximum allowed! "
		                "%f > %f\n", largest_error, max_allowable_error);
        }
        asfPrintStatus ("For the differences between spline model values and "
		        "projected values\nfor the analytically projected "
		        "control points:\n");
        asfPrintStatus ("Mean: %g\n", mean_error);
        asfPrintStatus ("Standard deviation: %g\n", error_standard_deviation);
        asfPrintStatus ("Maximum (Worst observed error in pixel index distance): "
		        "%g\n", largest_error);
        asfPrintStatus ("Maximum x error (worst observed error in x pixel index): "
		        "%g\n", largest_x_error);
        asfPrintStatus ("Maximum y error (worst observed error in y pixel index): "
		        "%g\n", largest_y_error);
        gsl_vector_free (model_errors);
        gsl_vector_free (model_y_errors);
        gsl_vector_free (model_x_errors);
    }

    // If we don't have a projected image, we are basing things on
    // meta_get_lineSamp, which is awesome.  Check correctness of reverse
    // mappings of some corners, as an extra paranoid check.  We insist
    // on the model being within this many pixels for reverse
    // transformations of the projection coordinates of the corners of
    // the output image back to the pixel indicies in the input image.
    if ( !input_projected ) {

        // The maximum corner error we are willing to tolerate.
        double max_corner_error;

        // The so called scansar projection has problems that prevent us
        // from getting as good a match as we would like (see below about
        // asymmetry or meta_get_latLon and meta_get_lineSamp).
        // FIXME: Fix the broken scansar crap *HARD*.
        if ( imd->sar && imd->sar->image_type == 'P' ) {
            g_assert (imd->projection->type == SCANSAR_PROJECTION);
            max_corner_error = 3 * max_allowable_error;
        }
        else {
            max_corner_error = max_allowable_error;
        }

        // Upper left corner.
        double ul_lat, ul_lon;
        ret = meta_get_latLon (imd, 0.0, 0.0, average_height, &ul_lat, &ul_lon);
        g_assert (ret == 0);

        // Test the symmetry of meta_get_latLon/meta_get_lineSamp.  I
        // believe it is pretty good for everything but scansar projected
        // input data, where it is off by 1.5% or so and therefore throws
        // this error check just a bit outside of a pixel.  But if the
        // problem is somewhere else I want to know.
        if ( imd->sar &&  imd->sar->image_type != 'P' ) {
            int ret1, ret2;
            const double stpx = 1.0, stpy = 1.0;   // Symmetry test pixel indicies.
            double st_lat, st_lon;   // Symmetry test lat and long values.
            ret1 = meta_get_latLon (imd, stpy, stpx, average_height,
                                    &st_lat, &st_lon);
            double strx, stry;       // Symmetry test result values.
            ret2 = meta_get_lineSamp (imd, st_lat, st_lon, average_height,
                                        &stry, &strx);

            // We will insist that the results are symmetric to within this
            // fraction after transforming out and back.
            printf ("Symmetry testing latLong vs. lineSamp... ");
            const double sym_th = 0.1;   // Symmetry threshold.
            if (ret1 || ret2) {
                asfPrintError("Symmetry test failed! %s %s.\n",
                                ret1 ? "meta_get_latLon returned error" : "",
                                ret2 ? "meta_get_lineSamp returned error" : "");
            }
            if (!(fabs (strx - stpx) < sym_th && fabs (stry - stpy) < sym_th)) {
                asfPrintWarning("\nFailed symmetry test: x- |%.5f-%.5f| = %.5f\n"
                        "                      y- |%.5f-%.5f| = %.5f  (tol=%.2f)\n",
                        strx,stpx,fabs(strx-stpx),stry,stpy,fabs(stry-stpy),sym_th);

                // Abort if the error is "horrible" (more than a pixel)
                if (fabs (strx-stpx) > 10*sym_th || fabs (stry-stpy) > 10*sym_th) {
                    asfPrintError("Aborting... symmetry testing error too large.\n");
                }
            } else {
                asfPrintStatus ("good to within %lf pixels.\n", sym_th);
            }
        }

        double ul_x, ul_y;
        project (pp, D2R * ul_lat, D2R * ul_lon, ASF_PROJ_NO_HEIGHT,
	        &ul_x, &ul_y, NULL, datum);
        double ul_x_pix_approx = X_PIXEL (ul_x, ul_y);
        if (fabs (ul_x_pix_approx) > max_corner_error ) {
            print_large_error_blurb(force_flag);
            report_func("Upper left x corner error was too large!  %f > %f\n",
		        fabs (ul_x_pix_approx), max_corner_error );
        }
        else {
            asfPrintStatus ("Upper left x corner error: %f\n",
		            fabs (ul_x_pix_approx));
        }

        double ul_y_pix_approx = Y_PIXEL (ul_x, ul_y);
        if (fabs (ul_y_pix_approx) > max_corner_error ) {
            print_large_error_blurb(force_flag);
            report_func ("Upper left y corner error was too large! %f > %f\n",
		        fabs (ul_y_pix_approx), max_corner_error );
        }
        else {
            asfPrintStatus ("Upper left y corner error: %f\n",
		            fabs (ul_y_pix_approx));
        }

        // Lower right corner.
        double lr_lat, lr_lon;
        ret = meta_get_latLon (imd, (float) (ii_size_y - 1),
                            (float) (ii_size_x - 1),
                            average_height, &lr_lat, &lr_lon);
        g_assert (ret == 0);

        double lr_x, lr_y;
        project (pp, D2R * lr_lat, D2R * lr_lon, ASF_PROJ_NO_HEIGHT,
	        &lr_x, &lr_y, NULL, datum);
        double lr_x_pix_approx = X_PIXEL (lr_x, lr_y);
        double lr_x_corner_error = fabs (lr_x_pix_approx - (ii_size_x - 1));
        if ( lr_x_corner_error > max_corner_error ) {
            print_large_error_blurb(force_flag);
            report_func ("Lower right x corner error was too large! %f > %f\n",
		        lr_x_corner_error, max_corner_error);
        }
        else {
            asfPrintStatus ("Lower right x corner error: %f\n", lr_x_corner_error);
        }
        double lr_y_pix_approx = Y_PIXEL (lr_x, lr_y);
        double lr_y_corner_error = fabs (lr_y_pix_approx - (ii_size_y - 1));
        if ( lr_y_corner_error > max_corner_error ) {
            print_large_error_blurb(force_flag);
            report_func ("Lower right Y corner error was too large! %f > %f\n",
		            lr_y_corner_error, max_corner_error);
        }
        else {
            asfPrintStatus ("Lower right y corner error: %f\n", lr_y_corner_error);
        }
    }

    // Now the mapping function is calculated and we can apply that to
    // all the bands in the file (or to the single band selected with
    // the -band option)

    // Now the band loop (the inner loop, see the discussion above)
    // Note that we use this file's metadata band count -- if this is
    // less than the number of bands in the output image, that band
    // will mosaic with fewer bands.
    int kk;
    for (kk=0; kk<imd->general->band_count; kk++) {
      if (multiband || kk == band_num) {
        if (n_bands > 1)
          asfPrintStatus("Geocoding band: %s\n", band_name[kk]);

        // For optical data -- we'll do processing as BYTE, to save memory
        // So, only one of the {Float/UInt8}Image will be non-null
        FloatImage *iim = NULL;
        UInt8Image *iim_b = NULL;

        asfPrintStatus("Creating tiles for the input image ...\n");

        // open up the input image
        if (process_as_byte)
          iim_b = uint8_image_band_new_from_metadata(imd, kk, input_image->str);
        else
          iim = float_image_band_new_from_metadata(imd, kk, input_image->str);

        asfPrintStatus("Resampling input image into output image "
          "coordinate space...\n");

        FILE *outFp = NULL; // only used in the line-by-line output case
        if (output_by_line) {
          // open for append, if multiband && this isn't the first band
          outFp = FOPEN(output_file->str, multiband && kk>0 ? "a" : "w");
        }

        if (output_by_line)
          g_assert(outFp && output_line && !output_bfi);
        else
          g_assert(!outFp && !output_line && output_bfi);

        // Set the pixels of the output image.
        size_t oix, oiy;		// Output image pixel indicies.
        for (oiy = 0 ; oiy <= oiy_max ; oiy++) {

          asfLineMeter(oiy, oiy_max + 1 );

          int oix_first_valid = -1;
          int oix_last_valid = -1;

          for ( oix = 0 ; oix <= oix_max ; oix++ ) {
            // Projection coordinates for the center of this pixel.
            double oix_pc = ((double) oix/oix_max) * (max_x-min_x) + min_x;
            // We want projection coordinates to increase as we move from
            // the bottom of the image to the top, so that north ends up up.
            double oiy_pc = (1.0-(double)oiy/oiy_max) * (max_y-min_y) + min_y;

            projX[oix] = oix_pc;
            projY[oix] = oiy_pc;

            // Determine pixel of interest in input image.  The fractional
            // part is desired, we will use some sampling method to
            // interpolate between pixel values.
            double input_x_pixel = X_PIXEL (oix_pc, oiy_pc);
            double input_y_pixel = Y_PIXEL (oix_pc, oiy_pc);

            g_assert (ii_size_x <= SSIZE_MAX);
            g_assert (ii_size_y <= SSIZE_MAX);

            float value;

            // If we are outside the extent of the input image, set to the
            // fill value.  We do this only on the first image -- subsequent
            // images will only overwrite with real data.
            if (   input_x_pixel < 0
                || input_x_pixel > (ssize_t) ii_size_x - 1.0
                || input_y_pixel < 0
                || input_y_pixel > (ssize_t) ii_size_y - 1.0 )
            {
              if (i == 0) { // first image
                if (output_by_line)
                  output_line[oix] = background_val;
                else
                  banded_float_image_set_pixel(output_bfi, kk, oix, oiy,
                    background_val);
              }
            }
            // Otherwise, set to the value from the appropriate position in
            // the input image.
            else
            {
              if (process_as_byte) {
                value = 
                  uint8_image_sample(iim_b, input_x_pixel, input_y_pixel,
                    uint8_image_sample_method);
              }
              else if ( imd->general->image_data_type == DEM ) {
                value =
                  dem_sample(iim, input_x_pixel, input_y_pixel,
                    float_image_sample_method);
              }
              else {
                value =
                  float_image_sample(iim, input_x_pixel, input_y_pixel,
                    float_image_sample_method);
              }

              oix_last_valid = oix;
              if (oix_first_valid == -1) oix_first_valid = oix;

              if (output_by_line)
                output_line[oix] = value;
              else
                banded_float_image_set_pixel(output_bfi, kk, oix, oiy, value);
            }

          } // end of for-each-sample-in-line set output values

          // If we are reprojecting a DEM, need to account for the height
          // difference between the vertical datum (NGVD27) and our WGS84
          // ellipsoid. Since geoid heights closely match vertical datum
          // heights, this will work for SAR imagery
          if ( imd->general->image_data_type == DEM ) {

            // At present, don't handle byte DEMs.  Don't think such a thing
            // is even possible, really.
            g_assert(iim && !iim_b);

            double *lat, *lon;
            lat = lon = NULL; // => libproj will allocate for us

            // Need to get each pixel's location in lat/lon in order to get
            // the geoid height.  We saved each pixel's projection coordinates,
            // above, so we just to need to convert those, then use the
            // lat/lon values to get the required geoid height correction,
            // add it to the height at the pixel.

            // Doing it like this (instead of pixel-by-pixel) allows us to
            // use the array version of libproj, which is *much* faster.

            unproject_arr(pp, projX, projY, NULL, &lat, &lon, NULL,
              oix_max + 1, datum);

            // the outer if guards against the case where no valid pixels
            // were on this line (i.e., both are -1)
            if (oix_first_valid > 0 && oix_last_valid > 0) {
              if (output_by_line) {
                for (oix = oix_first_valid; (int)oix <= oix_last_valid; ++oix) {
                  output_line[oix] +=
                    get_geoid_height(lat[oix]*R2D, lon[oix]*R2D);
                }
              } else {
                for (oix = oix_first_valid; (int)oix <= oix_last_valid; ++oix) {
                  float value = banded_float_image_get_pixel(output_bfi,
                    kk, oiy, oix);
                  banded_float_image_set_pixel(output_bfi, kk, oix, oiy,
                    value + get_geoid_height(lat[oix]*R2D, lon[oix]*R2D));
                }
              }
            }

            free(lat);
            free(lon);
          }

          // write the line, if we're doing line-by-line output
          if (output_by_line)
            put_float_line(outFp, omd, oiy, output_line);

        } // End of for-each-line set output values

        // done writing this band
        if (output_by_line)
          fclose(outFp);

        // free up the input image
        g_assert(!iim_b || !iim);
        if (process_as_byte)
          uint8_image_free (iim_b);
        else
          float_image_free (iim);

        if (imd->general->band_count == 1)
          asfPrintStatus("Done resampling image.\n");
        else
          asfPrintStatus("Done resampling band.\n");

        if (y_pixel_size < 0 && omd->projection == NULL) {
          g_assert (0); 		/* Shouldn't be here.  */
        }

      } // End of 'if multiband or single band and current band is requested band'
    } // End of 'for each band' in the file, 'map-project the data into the file'

    /////////////////////////////////////////////////////////////////////////
    //
    // Clear out all the persistent spline memory goop used by the
    // reverse_map_x and reverse_map_y routines (see comments near the
    // declarations of these variables).
    //

    size_t sgs = dtf.sparse_grid_size; // Convenience alias.

    first_time_through_rmx = TRUE;
    first_time_through_rmy = TRUE;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      gsl_interp_accel_free (y_accel_rmx[ii]);
      gsl_interp_accel_free (y_accel_rmy[ii]);
      gsl_spline_free (y_spline_rmx[ii]);
      gsl_spline_free (y_spline_rmy[ii]);
    }
    g_free (y_accel_rmx);
    g_free (y_accel_rmy);
    g_free (y_spline_rmx);
    g_free (y_spline_rmy);
    gsl_interp_accel_free (crnt_accel_rmx);
    gsl_interp_accel_free (crnt_accel_rmy);
    gsl_spline_free (crnt_rmx);
    gsl_spline_free (crnt_rmy);

    /////////////////////////////////////////////////////////////////////////
    // Done with the data being modeled.
    g_free (dtf.sparse_y_pix);
    g_free (dtf.sparse_x_pix);
    g_free (dtf.sparse_y_proj);
    g_free (dtf.sparse_x_proj);
    g_free (dtf.y_pix);
    g_free (dtf.x_pix);
    g_free (dtf.y_proj);
    g_free (dtf.x_proj);

    // Done with the file name arguments.
    g_string_free (input_meta_data, TRUE);
    g_string_free (input_image, TRUE);
    free(in_base_name);
    meta_free(imd);
  }
  if (band_name) {
    int kk;
    for (kk=0; kk < omd->general->band_count; ++kk)
      FREE(band_name[kk]);
    FREE(band_name);
  }

  free(projX);
  free(projY);

  if (output_by_line)
    free(output_line);
  else {
    // write the output image
    int kk;
    asfPrintStatus("\nGenerating final output.\n");
    for (kk=0; kk<omd->general->band_count; ++kk) {
      FloatImage *fi = banded_float_image_get_band(output_bfi, kk);
      float_image_band_store(fi, output_image->str, omd, kk>0);
    }
    banded_float_image_free(output_bfi);
  }

  meta_free (omd);
  g_string_free (output_file, TRUE);
  g_string_free (output_meta_data, TRUE);
  g_string_free (output_image, TRUE);

  asfPrintStatus("Geocoding complete.\n\n");
  return 0;
}
