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
#include "asf_import.h"
#include "asf_reporting.h"

// Given a potentially non-normalized longitude argument, return
// longitude normalized into the range [-90, 90].  Note that
// normalizing some longitudes implies that the associated latitude
// must also be changed in order to get the same geographic point,
// e.g. normalized_longitude (100) == 10, but at a latitude 180
// degrees from the original latitude.
static double
normalized_longitude (double longitude)
{
  if ( fabs (longitude) > 90 )
    longitude = R2D * asin (sin (D2R * longitude));

  return longitude;
}

// Import a shuttle radar topography mission (SRTM) digital elevation
// model (a GeoTIFF flavor) into our own ASF Tools format.
void
import_srtm_seamless (char *inFileName, char *outBaseName, int flag[])
{
  // Let the user know what format we are working on.
  asfPrintStatus
    ("   Input data type: GeoTIFF (SRTM pseudoprojected DEM flavor)\n");
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
  // (rational: ArcView currently doesn't either, and multiple tie
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
  asfRequire (model_type == ModelTypeGeographic,
	      "Input GeoTIFF key ModelTypeGeoKey does not have the value "
	      "'ModelTypeGeographic' expected for this input file type.\n"); 

  // It seem the SRTM people have chosen to set GTRasterTypeGeoKey to
  // RasterPixelIsArea.  This is probably reasonable since the
  // elevation is interferometry-derived (rather than coming from,
  // say, laser altimitry, which would definately suggest
  // RasterPixelIsPoint).  However, this isn't how people usually
  // think of elevation postings, so I don't trust it not to get
  // changed at some point, and it doesn't matter much, so I ignore
  // this tag.

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

  // Essentially, get the earth model on whith the lat/long are defined.
  short geographic_type;
  read_count 
    = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &geographic_type, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
  asfRequire (geographic_type == GCS_WGS_84, "Input GeoTIFF key "
	      "GeographicTypeGeoKey does not have the value 'GCS_WGS_84 "
	      "expected for this input file type.\n");
  
  // Get the angular units in which other metadata parameters are specified.
  short angular_units;
  read_count
    = GTIFKeyGet (input_gtif, GeogAngularUnitsGeoKey, &angular_units, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
  asfRequire (angular_units == Angular_Degree, "Input GeoTIFF key "
	      "GeogAngularUnitsGeoKey does not have the value 'Angular_Degree "
	      "expected for this input file type.\n");

  // Get the raster width and height of the image.
  uint32 width, height;
  TIFFGetField(input_tiff, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(input_tiff, TIFFTAG_IMAGELENGTH, &height);

  // Pull the actual image data out of the TIFF and store it as a
  // float_image.

  FloatImage *image = float_image_new (width, height);

  // Allocate a buffer for a line of pixels.
  tdata_t buf = _TIFFmalloc (TIFFScanlineSize (input_tiff));

  // Allocate another buffer that we are sure consists of floats (who
  // knows, maybe the input file is in doubles.  FIXME: insert a bits
  // per sample test or something to ensure that this is not the case.
  float *float_buf = malloc (width * sizeof (float));

  uint32 current_row;
  for ( current_row = 0 ; current_row < height ; current_row++ ) {
    // FIXME: last argument is irrelevant unless PlanarConfiguration
    // is equal to 2, but need to add a check to ensure this not the case.
    TIFFReadScanline (input_tiff, buf, current_row, 0);
    uint32 current_column;
    for ( current_column = 0 ; current_column < width ; current_column++ ) {
      // FIXME: make sure this buf cruf from libTIFF really points floats.
      float_image_set_pixel (image, current_column, current_row, 
			     ((float *) buf)[current_column]);
    }
    asfLineMeter (current_row, height);
  }

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

  char *sensor_string = "Shuttle Radar Topography Mission";
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

  mg->line_count = height;
  mg->sample_count = width;

  mg->x_pixel_size = pixel_scale[0];
  mg->y_pixel_size = pixel_scale[1];

  // Image raster coordinates of tie point.
  double raster_tp_x = tie_point[0];
  double raster_tp_y = tie_point[1];

  // Coordinates of tie point in pseudoprojection space.
  double tp_lat = tie_point[3];
  double tp_lon = tie_point[4];

  mg->center_latitude 
    = (width / 2.0 - raster_tp_x) * mg->x_pixel_size + tp_lat;
  mg->center_longitude
    = (height / 2.0 - raster_tp_y) * mg->y_pixel_size + tp_lon;

  float min, max, mean, standard_deviation;
  float_image_statistics (image, &min, &max, &mean, &standard_deviation,
			  FLOAT_IMAGE_DEFAULT_MASK);
  mg->average_height = mean;

  // We confirmed above that the GEOID model used is WGS84.
  mg->re_major = WGS84_SEMIMAJOR;
  mg->re_minor
    = WGS84_SEMIMAJOR - (1.0 / WGS84_INV_FLATTENING) * WGS84_SEMIMAJOR;

  meta_projection *mp = meta_out->projection; // Convenience alias.

  mp->type = LAT_LONG_PSEUDO_PROJECTION;

  mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lat;
  mp->startY = (0.0 - raster_tp_y) * mg->y_pixel_size + tp_lon;

  mp->perX = mg->x_pixel_size;
  mp->perY = mg->y_pixel_size;
  
  strcpy (mp->units, "degrees");

  mp->hem = normalized_longitude (mg->center_longitude) > 0.0 ? 'N' : 'S';

  // We confirmed above that the product is on the WGS84_SPHEROID.
  mp->spheroid = WGS84_SPHEROID;

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  // We confirmed above that the product is using the WGS84 system.
  mp->datum = WGS84_DATUM;

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

  // Determine the name of the output metadata file to write.
  GString *out_meta_file = g_string_new (outBaseName);
  g_string_append (out_meta_file, ".meta");

  // Determine the name of the output data file to write.
  GString *out_data_file = g_string_new (outBaseName);
  g_string_append (out_data_file, ".img");

  // Write the metadata to a file.
  meta_write (meta_out, out_meta_file->str);

  // Done with the metadata and the file name we wanted to write it in.
  g_string_free (out_meta_file, TRUE);
  meta_free (meta_out);

  // Write the data file itself.
  int return_code = float_image_store (image, out_data_file->str,
				       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

#ifdef DEBUG_IMPORT_SRTM_SEAMLESS_JPEG_OUTPUT
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
    float_image_export_as_jpeg_with_mask_interval 
      (image, out_data_file_jpeg->str, width > height ? width : height,
       -FLT_MAX, -1e16);
    g_string_free (out_data_file_jpeg, TRUE);
  }
#endif

  // Done with the data file's name and its FloatImage incarnation.
  g_string_free (out_data_file, TRUE);
  float_image_free (image);
}
