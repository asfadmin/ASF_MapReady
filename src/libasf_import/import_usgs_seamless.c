// Import a Unites States Geological Survey (USGS) digital elevation
// model (a pseudoprojected GeoTIFF flavor) into our own ASF Tools
// format.

#include <assert.h>

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <float_image.h>
#include <spheroids.h>

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"

#include "tiff_to_float_image.h"
#include "write_meta_and_img.h"

// Import a USGS seamless server digital elevation model (a
// pseudoprojected GeoTIFF flavor) into our own ASF Tools format.
void
import_usgs_seamless (const char *inFileName, const char *outBaseName, ...)
{
  // Let the user know what format we are working on.
  asfPrintStatus
    ("   Input data type: GeoTIFF (USGS seamless pseudoprojected DEM "
     "flavor)\n");
  asfPrintStatus
    ("   Output data type: float-valued pseudoprojected DEM in ASF format\n");

  // Open the input tiff file.
  TIFF *input_tiff = XTIFFOpen (inFileName, "r");
  asfRequire (input_tiff != NULL, "Error opening input TIFF file.\n");

  // Open the structure that contains the geotiff keys.
  GTIF *input_gtif = GTIFNew (input_tiff);
  asfRequire (input_gtif != NULL,
	      "Error reading GeoTIFF keys from input TIFF file.\n");

  // Counts holding the size of the returns from gt_methods.get method
  // calls.  The geo_keyp.h header has the interface specification for
  // this method.  gt_methods.get doesn't seem to be documented as
  // part of the public GeoTIFF API, however, it works, unlike the
  // TIFFGetField call, which seg faults.  Maybe I'm missing some
  // setup call that I need, but: a. I can't find anything in the
  // incomplete API documentation telling me what that might be, and
  // b. I'm using a sequence of calls analogous to that use in
  // export_as_geotiff, which works, and c. suspiciously, the listgeo
  // program that comes with libgeotiff also uses this gt_methods.get
  // approach and doesn't use TIFFGetField at all so far as I can
  // tell.
  int count;

  // Get the tie point which defines the mapping between raster
  // coordinate space and geographic coordinate space.  Although
  // geotiff theoretically supports multiple tie points, we don't
  // (rationale: ArcView currently doesn't either, and multiple tie
  // points don't make sense with the pixel scale option, which we
  // need).
  double *tie_point;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_TIEPOINTS, &count,
			       &tie_point);
  assert (count == 6);
  //  TIFFGetField (input_tiff, TIFFTAG_GEOTIEPOINTS, 6, tie_point);

  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_PIXELSCALE, &count,
			       &pixel_scale);
  assert (count == 3);
  //  TIFFGetField (input_tiff, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

  // Ensure that the model type is as expected for this type of product.
  short model_type;
  int read_count
    = GTIFKeyGet (input_gtif, GTModelTypeGeoKey, &model_type, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
  asfRequire (model_type == ModelTypeGeographic, "Input GeoTIFF key "
	      "GTModelTypeGeoKey does not have the value "
	      "'ModelTypeGeographic' expected for this input file type.\n");

  // It seem the USGS people have (possibly accidently through ESRI)
  // chosen to set GTRasterTypeGeoKey to RasterPixelIsArea.  This is
  // probably reasonable since the elevation is interferometry-derived
  // (rather than coming from, say, laser altimitry, which would
  // definately suggest RasterPixelIsPoint).  However, this isn't how
  // people usually think of elevation postings, so I don't trust it
  // not to get changed at some point, and it doesn't matter much, so
  // I ignore this tag.

  // So far as I can tell, the GeoTIFF spec doesn't give us a way to
  // determine the maximum citation length, so we just use a really
  // long one and hope it is long enough.  There is a field in the
  // undocumented key structure itself called count that might be what
  // we want, but I don't know if it always gets set correctly.
  size_t max_citation_length = 10000;
  char *citation = MALLOC ((max_citation_length + 1) * sizeof (char));
  GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, max_citation_length);
  // Ensure the citation is at least eventually terminated somewhere.
  citation[max_citation_length] = '\0';

  // Essentially, get the earth model on which the lat/long are defined.
  short geographic_type;
  read_count
    = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &geographic_type, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");

  datum_type_t datum;
  switch ( geographic_type ) {
  case GCS_WGS_84:
    datum = WGS84_DATUM;
    break;
  case GCS_NAD27:
    datum = NAD27_DATUM;
    break;
  case GCS_NAD83:
    datum = NAD83_DATUM;
    break;
  default:
    asfPrintError ("Unsupported GeographicTypeGeoKey value in GeoTIFF file");
    break;
  }

  spheroid_type_t spheroid = datum_spheroid (datum);

  // Get the angular units in which other metadata parameters are specified.
  short angular_units;
  read_count
    = GTIFKeyGet (input_gtif, GeogAngularUnitsGeoKey, &angular_units, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
  asfRequire (angular_units == Angular_Degree, "Input GeoTIFF key "
	      "GeogAngularUnitsGeoKey does not have the value 'Angular_Degree "
	      "expected for this input file type.\n");

  asfPrintStatus("\nConverting GeoTIFF to float image...\n");
  FloatImage *image = tiff_to_float_image (input_tiff);

  //asfPrintStatus("Storing pre-bad data scan jpeg image...\n");
  //float_image_export_as_jpeg
  //  (image, "pre_bad_data_remap.jpeg",
  //   image->size_x > image->size_y ? image->size_x : image->size_y, NAN);

  // DEMs of this flavor tend to be full of bad data values that make
  // the statistics hopeless, saturate output, etc.  For now we deal
  // with this by mapping these values to a less negative magic number
  // of our own that still lets things work somewhat (assuming the bad
  // data values are rare at least).
  asfPrintStatus("Checking for bad data values...\n");
  const float bad_data_ceiling = -10e10;
  const float new_bad_data_magic_number = -999.0;
  size_t ii, jj;
  for ( ii = 0 ; ii < image->size_y ; ii++ ) {
    for ( jj = 0 ; jj < image->size_x ; jj++ ) {
      if ( float_image_get_pixel (image, jj, ii) < bad_data_ceiling ) {
        float_image_set_pixel (image, jj, ii, new_bad_data_magic_number);
      }
    }
  }

  // Get the raster width and height of the image.
  uint32 width = image->size_x, height = image->size_y;

  // Create a new metadata file for the image.
  meta_parameters *meta_out = raw_init ();
  meta_out->optical = NULL;
  meta_out->thermal = NULL;
  meta_out->projection = meta_projection_init ();
  meta_out->stats = meta_stats_init ();
  meta_out->state_vectors = NULL;
  meta_out->location = meta_location_init ();
  // Don't set any of the deprecated structure elements.
  meta_out->stVec = NULL;
  meta_out->geo = NULL;
  meta_out->ifm = NULL;
  meta_out->info = NULL;

  // Fill in metadata as best we can.

  meta_general *mg = meta_out->general;	// Convenience alias.

  char *sensor_string = "USGS Seamless data (e.g., NED, SRTM)";
  assert (strlen (sensor_string) < FIELD_STRING_MAX);
  strcpy (mg->sensor, sensor_string);

  char *mode_string = "N/A";
  assert (strlen (mode_string) < MODE_FIELD_STRING_MAX);
  strcpy (mg->mode, mode_string);

  char *processor  = "Unknown";
  assert (strlen (processor) < FIELD_STRING_MAX);
  strcpy (mg->processor, processor);
  // FIXME: add check as described above.

  mg->data_type = REAL32;

  mg->image_data_type = DEM;

  char *system = "big_ieee";
  assert (strlen (system) < FIELD_STRING_MAX);
  strcpy (mg->system, system);

  strcpy(mg->basename, inFileName);

  mg->band_count = 1;

  mg->line_count = height;
  mg->sample_count = width;

  mg->start_line = 0;
  mg->start_sample = 0;

  mg->x_pixel_size = pixel_scale[0];
  mg->y_pixel_size = pixel_scale[1];

  // For now we are going to insist that the degrees per pixel in the
  // X and Y directions are identical(ish).  I believe asf_geocode at
  // least would work for non-square pixel dimensions, with the
  // caveats that output pixels would still be square, and would have
  // default size derived solely from the input pixel size (arc length
  // between pixels) in the latitude direction.
  assert (fabs (mg->x_pixel_size - mg->y_pixel_size) < 0.0001);
  // Better to use gsl_fcmp, but it probably adds a library
  // dependency, which is extra bad now that asf_import is itself a
  // library.
  //  assert (gsl_fcmp (mg->x_pixel_size, mg->y_pixel_size, 0.0001));

  // Image raster coordinates of tie point.
  double raster_tp_x = tie_point[0];
  double raster_tp_y = tie_point[1];

  // Coordinates of tie point in pseudoprojection space.
  double tp_lon = tie_point[3];
  double tp_lat = tie_point[4];

  mg->center_latitude
    = (height / 2.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;
  mg->center_longitude
    = (width / 2.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;

  asfPrintStatus("Gathering image statistics...\n");
  float min, max, mean, standard_deviation;
  float_image_statistics (image, &min, &max, &mean, &standard_deviation,
			  FLOAT_IMAGE_DEFAULT_MASK);
  spheroid_axes_lengths (spheroid, &mg->re_major, &mg->re_minor);

  meta_projection *mp = meta_out->projection; // Convenience alias.

  // No necessity to set projection in sar block. Existence of projection
  // block will do just fine.
  // meta_out->sar->image_type = 'P';
  mp->type = LAT_LONG_PSEUDO_PROJECTION;

  mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
  mp->startY = (0.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;

  mp->perX = mg->x_pixel_size;
  mp->perY = -mg->y_pixel_size;

  strcpy (mp->units, "degrees");

  mp->hem = mg->center_latitude > 0.0 ? 'N' : 'S';

  mp->spheroid = spheroid;

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  mp->datum = datum;

  mp->height = mean;

  // Note that the pseudoprojection "projection" doesn't need a
  // special project_parameters structure, since all the details about
  // it are reflected in the above.

  meta_stats *ms = meta_out->stats; // Convenience alias.
  ms->mean = mean;
  // The root mean square error and standard deviation are very close
  // by definition when the number of samples is large, there seems to
  // be some confusion about the definitions of one relative to the
  // other, and I don't think its worth agonizing about the best thing
  // to do.
  ms->rmse = standard_deviation;
  ms->std_deviation = standard_deviation;
  ms->mask = FLOAT_IMAGE_DEFAULT_MASK;

  meta_location *ml = meta_out->location; // Convenience alias.
  ml->lat_start_near_range = mp->startX;
  ml->lon_start_near_range = mp->startY;
  ml->lat_start_far_range = mp->startX + mp->perX * width;
  ml->lon_start_far_range = mp->startY;
  ml->lat_end_near_range = mp->startX;
  ml->lon_end_near_range = mp->startY + mp->perY * height;
  ml->lat_end_far_range = mp->startX + mp->perX * width;
  ml->lon_end_far_range = mp->startY + mp->perY * height;

  asfPrintStatus("Writing ASF .img and .meta output files...\n");
  int return_code = write_meta_and_img (outBaseName, meta_out, image);
  asfRequire (return_code == 0,
	      "Failed to write new '.meta' and '.img' files.");
  asfPrintStatus("\n");

#ifdef DEBUG_IMPORT_USGS_SEAMLESS_JPEG_OUTPUT
  {
    // Take a look at a jpeg version of the image.  These DEMs seem to
    // use a huge negative value for no data points, so we mask those
    // out in order to get an unsaturated output image.  To get a
    // range that consists of more than one point (since I'm not sure
    // what you have to add to -FLT_MAX to get a different value), the
    // upper limit of the mask range is a very negative number.
    // Hopefully values like that don't occur much in DEMs.

    GString *out_data_file_jpeg = g_string_append (g_string_new (outBaseName),
						   "_debug.jpeg");
    g_print ("Making JPEG of output image named %s for debugging...\n",
	     out_data_file_jpeg->str);
    float_image_export_as_jpeg_with_mask_interval
      (image, out_data_file_jpeg->str, width > height ? width : height,
       -FLT_MAX, -1e16);
    g_string_free (out_data_file_jpeg, TRUE);
  }
#endif

  // We're now done with the data and metadata.
  meta_free (meta_out);
  float_image_free (image);

  // We must be done with the citation string too :)
  FREE (citation);
}
