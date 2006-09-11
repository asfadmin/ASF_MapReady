// Import a UTM projected GeoTIFF originally created by the ASF tools
// back into our own ASF Tools format.

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
#include "projected_image_import.h"
#include "tiff_to_float_image.h"
#include "write_meta_and_img.h"

// Import a UTM projected GeoTIFF originally created by the ASF tools
// back into our own ASF Tools format.
void
import_asf_utm_geotiff (const char *inFileName, const char *outBaseName)
{
  // Let the user know what format we are working on.
  asfPrintStatus
    ("   Input data type: GeoTIFF (ASF UTM projected flavor)\n");
  asfPrintStatus
    ("   Output data type: float-valued pseudoprojected image in ASF "
     "format\n");

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

  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_PIXELSCALE, &count,
			       &pixel_scale);
  assert (count == 3);

  // Ensure that the model type is as expected for this type of product.
  short model_type;
  int read_count
    = GTIFKeyGet (input_gtif, GTModelTypeGeoKey, &model_type, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");  
  asfRequire (model_type == ModelTypeProjected, "Input GeoTIFF key "
	      "GTModelTypeGeoKey does not have the value "
	      "'ModelTypeProjected' expected for this input file type.\n");

  // We choose to ignore the GTRasterTypeGeoKey.  It could be set to
  // point or area without it making much difference, so we don't
  // worry about it.

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
 
  // Verify that the linear units are as expected for this projection type.
  short unit_type;
  read_count 
    = GTIFKeyGet (input_gtif, GeogLinearUnitsGeoKey, &unit_type, 0, 1);
  asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
  asfRequire (unit_type == Linear_Meter, "Input GeoTIFF key "
	      "GeogLinearUnitsGeoKey does not have the value 'Linear_Meter'"
	      "expected for this input file type.\n");

  short cs_type;		// Coordinate system type.
  read_count
    = GTIFKeyGet (input_gtif, ProjectedCSTypeGeoKey, &cs_type, 0, 1);
  int zone_number;
  char hemisphere_code;
  utm_zone_and_hemisphere_from_projected_coordinate_system_type_geotiff_geokey
    (cs_type, &hemisphere_code, &zone_number);

  FloatImage *image = tiff_to_float_image (input_tiff);

  // Get the raster width and height of the image.
  uint32 width = image->size_x, height = image->size_y;

  // Create a new metadata file for the image.
  meta_parameters *meta_out = raw_init ();
  meta_out->optical = NULL;
  meta_out->thermal = NULL;
  meta_out->projection = meta_projection_init ();
  // For an arbitrary image, the statistics are probably going to be
  // wrong for one reason or another, and there is no way to
  // anticipate all the possibilities (zero fill, other value fill,
  // huge magic numbers, NaNs in image, etc.), so we don't bother.
  meta_out->stats = NULL;
  meta_out->state_vectors = NULL;
  meta_out->location = meta_location_init ();
  // Don't set any of the deprecated structure elements.
  meta_out->stVec = NULL;
  meta_out->geo = NULL;
  meta_out->ifm = NULL;
  meta_out->info = NULL;

  // Fill in metadata as best we can.

  meta_general *mg = meta_out->general;	// Convenience alias.

  char *sensor_string = "Unknown";
  assert (strlen (sensor_string) < FIELD_STRING_MAX);
  strcpy (mg->sensor, sensor_string);

  char *mode_string = "NA";
  assert (strlen (mode_string) < MODE_FIELD_STRING_MAX);
  strcpy (mg->mode, mode_string);

  char *processor  = "Unknown";
  assert (strlen (processor) < FIELD_STRING_MAX);
  strcpy (mg->processor, processor);

  mg->data_type = REAL32;

  mg->image_data_type = IMAGE;

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

  // Coordinates of tie point in projection space.
  double tp_map_x = tie_point[3];
  double tp_map_y = tie_point[4];

  // Setting these is a hassle.  We should do it when it becomes clear
  // that someone cares, but probably not before, lest we end up with
  // a bunch of untested crud.
  mg->center_latitude = MAGIC_UNSET_DOUBLE;
  mg->center_longitude = MAGIC_UNSET_DOUBLE;

  // We confirmed above that the GEOID model used is WGS84.
  spheroid_axes_lengths (WGS84_SPHEROID, &mg->re_major, &mg->re_minor);  

  meta_projection *mp = meta_out->projection; // Convenience alias.

  mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
  
  mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_map_x;
  mp->startY = (0.0 - raster_tp_y) * mg->y_pixel_size + tp_map_y;

  mp->perX = mg->x_pixel_size;
  mp->perY = mg->y_pixel_size;

  strcpy (mp->units, "meters");

  mp->hem = hemisphere_code;

  mp->spheroid = WGS84_SPHEROID;

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  mp->datum = WGS84_DATUM;

  // No way to tell.
  mp->height = MAGIC_UNSET_DOUBLE;
  
  mp->param.utm.zone = zone_number;
  mp->param.utm.false_easting = 500000.0;
  if ( hemisphere_code == 'N' ) {
    mp->param.utm.false_northing = 0.0;
  }
  else {
    mp->param.utm.false_northing = 10000000.0;
  }
  mp->param.utm.lat0 = 0.0;
  mp->param.utm.lon0 = utm_zone_to_central_meridian (zone_number);
  /* By definition, the ast_meta.h header says scale_factor is this. */
  mp->param.utm.scale_factor = 0.9996;

  // Same comment applies to these as to the center_latitude and
  // center_longitude fields of the general block.
  meta_location *ml = meta_out->location; // Convenience alias.
  ml->lat_start_near_range = MAGIC_UNSET_DOUBLE;
  ml->lon_start_near_range = MAGIC_UNSET_DOUBLE;
  ml->lat_start_far_range = MAGIC_UNSET_DOUBLE;
  ml->lon_start_far_range = MAGIC_UNSET_DOUBLE;
  ml->lat_end_near_range = MAGIC_UNSET_DOUBLE;
  ml->lon_end_near_range = MAGIC_UNSET_DOUBLE;
  ml->lat_end_far_range = MAGIC_UNSET_DOUBLE;
  ml->lon_end_far_range = MAGIC_UNSET_DOUBLE;  

  int return_code = write_meta_and_img (outBaseName, meta_out, image);
  asfRequire (return_code == 0, 
	      "Failed to write new '.meta' and '.img' files.");

#ifdef DEBUG_IMPORT_ASF_UTM_GEOTIFF_JPEG_OUTPUT
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
