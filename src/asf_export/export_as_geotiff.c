#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <proj_api.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_reporting.h>
#include <asf_export.h>
#include <float_image.h>

#define ASF_NAME_STRING "asf_export"

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

/* Set geotiff keys common to all the user defined projections we deal
   with.  */
static void
set_common_keys (GTIF *ogtif)
{
  GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
              user_defined_value_code);
  GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
              user_defined_value_code);
  GTIFKeySet (ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
  GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
  GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1,
              Angular_Degree);
}

/* Set the false easting and false northing geotif parameters.  */
static void
set_false_easting_and_northing (GTIF *ogtif, double false_easting,
                                double false_northing)
{
  GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, false_easting);
  GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, false_northing);
}

/* Return a pointer to a static string giving a text name for
   ellipsoid.  This pointer is only good until the next call to
   ellipsoid_name, so the string should be copied if it needs to be
   save for later.  The string is gauranteed to be less than 100
   characters in length, not including the trailing null byte.  */
static char *
ellipsoid_name (asf_export_ellipsoid_t ellipsoid)
{
  const size_t max_name_length = 100;
  static char *ret = NULL;      /* Text representation to return.  */
  if ( ret == NULL ) {
    ret = (char *) malloc ((max_name_length + 1) * sizeof (char));
  }

  int ret_length;               /* For lengths of strings written to ret.  */
  switch ( ellipsoid ) {
  case CLARKE1866:
    ret_length = snprintf (ret, max_name_length + 1, "CLARKE1866");
    break;
  case GEM10C:
    ret_length = snprintf (ret, max_name_length + 1, "GEM10C");
    break;
  case WGS84:
    ret_length = snprintf (ret, max_name_length + 1, "WGS84");
    break;
  case USER_DEFINED:
    ret_length = snprintf (ret, max_name_length + 1, "user defined");
    break;
  default:
    /* Shouldn't be here.  */
    asfPrintError("Unable to cope with given ellipsoid.\n");
  }
  asfRequire (ret_length >= 0 && ret_length <= max_name_length,
              "bad ellipsoid name length");

  return ret;
}

void
export_as_geotiff (const char *metadata_file_name,
                   const char *image_data_file_name,
                   const char *output_file_name,
                   long max_size,
                   scale_t sample_mapping)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Scale factor needed to satisfy max_size argument.  */
  size_t scale_factor;
  unsigned short sample_size = 4;
  unsigned short sample_format;
  int jj;
  TIFF *otif;
  GTIF *ogtif;
  ssize_t ii;
  int return_code;

  /* Major and minor ellipse axis lengths.  This shows up in two
     different places in our metadata, we want the projected one if
     its available, otherwise the one from general.  */
  double re_major, re_minor;
  /* Nail down which ellipsoid we are on exactly.  The ASF metadata
     doesn't specify this though, so we take a look at the major and
     minor axis values and try to find a matching ellipsoid.  */
  asf_export_ellipsoid_t ellipsoid;
  const double clarke1866_major_axis = 6378206.4;
  const double clarke1866_minor_axis = 6356583.8;
  const double gem10c_major_axis = 6378144;
  const double gem10c_minor_axis = 6356759;
  const double wgs66_major_axis = 6378145.0;
  const double wgs66_minor_axis = 6356759.769356;
  const double wgs84_major_axis = 6378137;
  const double wgs84_flattening = 1.0 / 298.257223563;
  const double wgs84_minor_axis = wgs84_major_axis * (1 - wgs84_flattening);

  /* Insist that the minor axis match what we are expecting to within
     this tolerance.  */
  double axis_tolerance = 0.2;

  asfRequire (md->general->data_type == REAL32,
              "Can only ingest ASF format floating point data.");
  asfRequire (sizeof (unsigned short) == 2,
              "Unsigned short integer data type size is different than "
              "expected.\n");
  asfRequire (sizeof (unsigned int) == 4,
              "Unsigned integer data type size is different than expected.\n");

  /* Get the image data.  */
  asfPrintStatus("Loading image...\n");
  asfRequire (md->general->data_type == REAL32,
              "Input data type must be in 32-bit floating point format.\n");
  const off_t start_of_file = 0;
  FloatImage *iim
    = float_image_new_from_file (md->general->sample_count,
                                 md->general->line_count,
                                 image_data_file_name, start_of_file,
                                 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  /* We want to scale the image st the long dimension is less than or
     equal to the prescribed maximum, if any.  */
  if ( (max_size > iim->size_x && max_size > iim->size_y)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    scale_factor = 1;
  }
  else {
    scale_factor = ceil ((double) GSL_MAX (iim->size_x, iim->size_y)
                         / max_size);
    /* The scaling code we intend to use needs odd scale factors.  */
    if ( scale_factor % 2 != 1 ) {
      scale_factor++;
    }
  }

  /* Generate the scaled version of the image, if needed.  */
  FloatImage *si;
  if ( scale_factor != 1 ) {
    asfPrintStatus ("Scaling...\n");
    si = float_image_new_from_model_scaled (iim, scale_factor);
  }
  else {
    si = iim;
  }

  /* Open output tiff file and GeoKey file descriptor.  */
  otif = XTIFFOpen (output_file_name, "w");
  asfRequire(otif != NULL, "Error opening output tiff file.\n");
  ogtif = GTIFNew (otif);
  asfRequire (ogtif != NULL, "Error opening output GeoKey file descriptor.\n");

  /* Lower and upper extents of the range of float values which are to
     be mapped linearly into the output space.  Needed for if output
     will be bytes.  */
  float omin, omax;

  /* Scale float image down to bytes, if required.  This is currently
     done in a very memory intensive way and could stand to be
     rewritten to use float_image.  */
  float mean, standard_deviation;
  const int default_sampling_stride = 30;
  const int minimum_samples_in_direction = 10;
  int sampling_stride = GSL_MIN (default_sampling_stride,
                                 GSL_MIN (si->size_x, si->size_y)
                                 / minimum_samples_in_direction);
  float min_sample = -1.0, max_sample = -1.0;
  gsl_histogram *my_hist = NULL;
  if (sample_mapping != NONE) {
    /* We might someday want to mask out certain valus for some type of
       images, so they don't corrupt the statistics used for mapping
       floats to JSAMPLEs.  Enabling this will require changes to the
       statistics routines and the code that does the mapping from
       floats to JSAMPLEs.  */
    /*
     * double mask;
     * if ( md->general->image_data_type == SIGMA_IMAGE
     *      || md->general->image_data_type == GAMMA_IMAGE
     *      || md->general->image_data_type == BETA_IMAGE
     *      || strcmp(md->general->mode, "SNA") == 0.0
     *      || strcmp(md->general->mode, "SNB") == 0.0
     *      || strcmp(md->general->mode, "SWA") == 0.0
     *      || strcmp(md->general->mode, "SWB") == 0.0 )
     *   mask = 0.0;
     * else
     *   mask = NAN;
     */

    /* We need a version of the data in byte form, so we have to map
       floats into bytes.  */

    /* Make sure the unsigned char is the size we expect.  */
    asfRequire (sizeof(unsigned char) == 1,
                "Size of the unsigned char data type on this machine is "
                "different than expected.\n");

    /* Gather some statistics to help with the mapping.  Note that if
       min_sample and max_sample will actually get used for anything
       they will be set to some better values than this.  */
    asfPrintStatus("Gathering image statistics...\n");
    get_statistics (si, sample_mapping, sampling_stride, &mean,
                    &standard_deviation, &min_sample, &max_sample, &omin,
                    &omax, &my_hist);

    /* Its a byte image, so the sample_size is one.  */
    sample_size = 1;
    sample_format = SAMPLEFORMAT_UINT;
  }
  else {
    /* Its a floating point image.  */
    g_assert (sizeof (float) == 4);
    sample_size = 4;
    sample_format = SAMPLEFORMAT_IEEEFP;
  }

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, si->size_x);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, si->size_y);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP, 1);
  TIFFSetField(otif, TIFFTAG_XRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_YRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, sample_format);
  // Setting this tag screws up byte images
  // TIFFSetField(otif, TIFFTAG_DATATYPE, sample_format);

  /* Set the GeoTIFF extension image tags.  */

  /* Calling the observations from which SAR image pixels are derived
     may seem a bit weird.  However, nobody seems to know exactly how
     observation pixels relate to the geolocation information
     associated with the image anyway, and using PixelIsPoint will let
     scaling work right when we average together blocks of pixels with
     the first block at pixel 0,0.  */
  GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsPoint);

  if ( md->sar->image_type == 'P' ) {
    re_major = md->projection->re_major;
    re_minor = md->projection->re_minor;
  }
  else {
    re_major = md->general->re_major;
    re_minor = md->general->re_minor;
  }

  if ( FLOAT_COMPARE_TOLERANCE (re_major, clarke1866_major_axis,
                                axis_tolerance)
       && FLOAT_COMPARE_TOLERANCE (re_minor, clarke1866_minor_axis,
                                   axis_tolerance) ) {
    ellipsoid = CLARKE1866;
  }
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, wgs84_major_axis,
                                     axis_tolerance)
            && FLOAT_COMPARE_TOLERANCE (re_minor, wgs84_minor_axis,
                                        axis_tolerance) ) {
    ellipsoid = WGS84;
  }
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, gem10c_major_axis,
                                     axis_tolerance)
            && FLOAT_COMPARE_TOLERANCE (re_minor, gem10c_minor_axis,
                                        axis_tolerance) ) {
    ellipsoid = GEM10C;
  }
  /* FIXME: I have some ellipsoid that looks slightly like this thing,
     so in order to give us a datum to use for geotiffs we pretend its
     this.  The geolocation is bad because of range migration
     anyway.  */
  else if ( FLOAT_COMPARE_TOLERANCE (re_major, wgs66_major_axis, 2)
            && FLOAT_COMPARE_TOLERANCE (re_minor, wgs66_minor_axis, 5) ) {
    ellipsoid = WGS66;
  }
  else {
    /* FIXME: we badly need to get the ellipsoid/datum mess sorted
       out.  This problem goes deeper than asf_export, however.  */
    asfPrintWarning ("couldn't conclude which ellipsoid is being used from "
                     "ellipsoid axis dimensions in metadata, using user "
                     "defined ellipsoid\n");
    ellipsoid = USER_DEFINED;
  }

  /* If we have a map projected image, write the projection
     information into the GeoTIFF.  */
  /* FIXME: this is a terrible hack to deal with scansar crap.  */
  if ( md->sar->image_type == 'P'
       && md->projection->type != SCANSAR_PROJECTION) {
    /* Tie point for image corner.  To avoid problems with for example
       ArcView, and to escape the fact that the meaning of multiple
       tie points with constant scale factors in GeoTIFF is horribly
       ill-defined, we use only a single tie point.  */
    double tie_point[6];
    double pixel_scale[3];
    short projection_code;
    int max_citation_length = 500;
    char *citation;
    int citation_length;

    if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
                                  clarke1866_major_axis, axis_tolerance)
         && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
                                     clarke1866_minor_axis,
                                     axis_tolerance) ) {
      ellipsoid = CLARKE1866;
    }
    else if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
                                       wgs84_major_axis, axis_tolerance)
              && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
                                          wgs84_minor_axis, axis_tolerance) ) {
      ellipsoid = WGS84;
    }
    else if ( FLOAT_COMPARE_TOLERANCE (md->projection->re_major,
                                       gem10c_major_axis, axis_tolerance)
              && FLOAT_COMPARE_TOLERANCE (md->projection->re_minor,
                                          gem10c_minor_axis,
                                          axis_tolerance) ) {
      ellipsoid = GEM10C;
    }
    else {
      /* FIXME: we badly need to get the ellipsoid/datum mess sorted
         out.  This problem goes deeper than asf_export, however.  */
      asfPrintWarning ("couldn't conclude which ellipsoid is being used from "
                       "being used from ellipsoid axis dimensions in "
                       "metadata, assuming WGS84 ellipsoid\n");
      ellipsoid = WGS84;
    }

    /* We will tie down the top left corner of the image (which has
       TIFF raster coordinates 0, 0, 0).  */
    tie_point[0] = 0.0;
    tie_point[1] = 0.0;
    tie_point[2] = 0.0;
    /* FIXME: we should be getting the actual corner of the image
       here, not the center of the corner pixel, and I'm not sure that
       startX and startY are what we want (verify and fix if
       needed.  */
    tie_point[3] = md->projection->startX;
    tie_point[4] = md->projection->startY;
    tie_point[5] = 0.0;
    /* Some applications (e.g., ArcView) won't handle GeoTIFF images
       with more than one tie point pair.  Therefore, only the upper
       left corner is being written to the GeoTIFF file.  In order to
       write all computed tie points to the GeoTIFF, change the 6 to
       size in the line below.  */
    TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_point);

    /* Set the scale of the pixels, in projection coordinates.  */
    pixel_scale[0] = md->projection->perX * scale_factor;
    pixel_scale[1] = md->projection->perY * scale_factor;
    pixel_scale[2] = 0;
    TIFFSetField (otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1,
                ModelTypeProjected);

    /* Write the appropriate geotiff keys for the projection type.  */
    switch ( md->projection->type ) {

    case UNIVERSAL_TRANSVERSE_MERCATOR:
      {
        /* For now we only handle UTM data that is referenced to the
           WGS84 ellipsoid.  */
        asfRequire(ellipsoid == WGS84,
                   "UTM data must be relative to the WGS84 ellipsoid.\n");

        /* This weird paranoid assertion is because I remember once when
           we couln't figure out how to set some datum code right, we
           set it to -1.  */
        asfRequire(md->projection->param.utm.zone != -1,"Unknown UTM zone.\n");

        /* Here we use some funky arithmetic to get the correct
           geotiff coordinate system type key from our zone code.
           Here are a few assertions to try to ensure that the
           convention used for the libgeotiff constants is as
           expected.  Also note that we have already verified that we
           are on a WGS84 ellipsoid.  */
        asfRequire(PCS_WGS84_UTM_zone_60N - PCS_WGS84_UTM_zone_1N == 59,
                   "Unable to create geotiff tags to accepted convention.\n");
        asfRequire(PCS_WGS84_UTM_zone_60S - PCS_WGS84_UTM_zone_1S == 59,
                   "Unable to create geotiff tags to accepted convention.\n");

        if ( md->projection->hem == 'N' ) {
          const int northern_utm_zone_base = PCS_WGS84_UTM_zone_1N - 1;
          projection_code = northern_utm_zone_base;
        }
        else if ( md->projection->hem == 'S' ) {
          const int southern_utm_zone_base = PCS_WGS84_UTM_zone_1S - 1;
          projection_code = southern_utm_zone_base;
        }
        else {               /* Shouldn't be here.  */
          asfPrintError("You are not in the northern or southern hemisphere;\n"
                        "you are now in the twighlight zone");
        }
        projection_code += md->projection->param.utm.zone;

        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    projection_code);
        GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length
          = snprintf (citation, max_citation_length + 1,
                      "UTM zone %d %c projected GeoTIFF on WGS84 ellipsoid "
                      "datum written by Alaska Satellite Facility tools.",
                      md->projection->param.utm.zone,
                      md->projection->hem);
        asfRequire((citation_length >= 0)
                   && (citation_length <= max_citation_length),
                   "geotiff citation too long" );
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
        break;
      }
    case POLAR_STEREOGRAPHIC:
      {
        set_common_keys (ogtif);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_PolarStereographic);
        GTIFKeySet (ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.slon);
        GTIFKeySet (ogtif, ProjOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.slat);
        set_false_easting_and_northing (ogtif, 0.0, 0.0);

        ///////////////////////////////////////////////////////////////////////
        //
        // Here we employ a slightly weird strategy: we always use a
        // WGS84 datum, no matter what the ASF metadata for the
        // product we are exporting says.  This is ok because the
        // error introduced is very small compared to other error
        // sources, and the geotiff viewers handle WGS84 datums much
        // better than user defined ones.
        //
        ///////////////////////////////////////////////////////////////////////

        /* Maximum length of coordinate system description strings used
           with the proj library. */
        const size_t max_coordinate_system_description_length = 1000;
        /* The coordinate system as described by the ASF metadata. */
        char *tmp = malloc (max_coordinate_system_description_length + 1);
        int write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=stere +a=%lf +b=%lf +lat_0=%lf +lon_0=%lf "
                      "+lat_ts=%lf", re_major, re_minor,
                      (md->projection->param.ps.slat > 0.0 ? 90.0 : -90.0),
                      md->projection->param.ps.slon,
                      md->projection->param.ps.slat);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ input_coordinate_system = pj_init_plus (tmp);
        asfRequire (input_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        /* The coordinate system to be used for the output geotiff.  */
        write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=stere +datum=WGS84 +lat_0=%lf +lon_0=%lf "
                      "+lat_ts=%lf",
                      (md->projection->param.ps.slat > 0.0 ? 90.0 : -90.0),
                      md->projection->param.ps.slon,
                      md->projection->param.ps.slat);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ geotiff_coordinate_system = pj_init_plus (tmp);
        asfRequire (geotiff_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        double tmp1 = md->projection->startX;
        double tmp2 = md->projection->startY;
        double tmp3 = 0;
        return_code = pj_transform (input_coordinate_system,
                                    geotiff_coordinate_system, 1, 1, &tmp1,
                                    &tmp2, &tmp3);
        asfRequire (return_code == 0, "pj_transform signalled an error");
        /* The maximum allowable projection error.  If changing the
           datum from the one in the metadata to the WGS84 datum moves
           the projection corner point by this amount or more in
           projection coordinates, an exception is triggered.  This
           value was chosen based on a seat-of-the-pants feel which
           accounts for the various error sources: range migration, etc.
           If the geolocation process is anywhere near this accurate, we
           are doing really good. */
        const double max_allowable_projection_error = 30.0;
        asfRequire (sqrt (pow (fabs (tmp1 - md->projection->startX), 2)
                          + pow (fabs (tmp2 - md->projection->startY), 2))
                    < max_allowable_projection_error,
                    "using the WGS84 datum to represent data with a \n"
                    "datum resulted in too much error");
        free (tmp);

        GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);

        ///////////////////////////////////////////////////////////////////////

        /* Fill in the details of the geographic coordinate system used.
           At the moment, we always use WGS84 (see above), so this code
           is out.  */
        // switch ( ellipsoid ) {
        // case CLARKE1866:
        // GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
        // GCSE_Clarke1866);
        //   break;
        // case GEM10C:
        //   GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
        //               GCSE_GEM10C);
        //   break;
        // case WGS84:
        //   GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
        //               GCS_WGS_84);
        //   break;
        // case USER_DEFINED:
        //   GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
        //               user_defined_value_code);
        //   GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
        //               user_defined_value_code);
        //   /* The angular units are degrees and the meridian is
        //      Greenwitch, so we don't need to define them
        //      explicitly.  The GeogCitation key will be filled in
        //      later.  */
        //   GTIFKeySet (ogtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1,
        //               user_defined_value_code);
        //   GTIFKeySet (ogtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1,
        //               re_major);
        //   GTIFKeySet (ogtif, GeogSemiMinorAxisGeoKey, TYPE_DOUBLE, 1,
        //               re_minor);
        //   citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        //   citation_length
        //     = snprintf (citation, max_citation_length + 1,
        //                 "Geographic coordinate system using reference "
        //                 "ellipsoid with semimajor axis of %f meters and "
        //                 "semiminor axis of %f meters",
        //                 re_major, re_minor);
        //   GTIFKeySet (ogtif, GeogCitationGeoKey, TYPE_ASCII, 1, citation);
        //   free (citation);
        //   break;
        // default:  /* Shouldn't be here.  */
        //   asfPrintError("Unable to cope with given ellipsoid.\n");
        //   break;
        // }

        /* Set the citation key.  */
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length
          = snprintf (citation, max_citation_length + 1,
                      "Polar stereographic projected GeoTIFF using %s "
                      "ellipsoid datum written by Alaska Satellite Facility "
                      "tools.", ellipsoid_name (ellipsoid));
        asfRequire (citation_length >= 0
                    && citation_length <= max_citation_length,
                    "bad citation length");
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
        break;
      }

    case LAMBERT_CONFORMAL_CONIC:
      {
        set_common_keys (ogtif);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_LambertConfConic);
                    //              CT_LambertConfConic_2SP);
        GTIFKeySet (ogtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.lat0);
        GTIFKeySet (ogtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.lon0);
        GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.plat1);
        GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.plat2);
        //      GTIFKeySet (ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        //      GTIFKeySet (ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        set_false_easting_and_northing (ogtif, 0.0, 0.0);

        ///////////////////////////////////////////////////////////////////////
        //
        // Here we employ a slightly weird strategy: we always use a
        // WGS84 datum, no matter what the ASF metadata for the
        // product we are exporting says.  This is ok because the
        // error introduced is very small compared to other error
        // sources, and the geotiff viewers handle WGS84 datums much
        // better than user defined ones.
        //
        ///////////////////////////////////////////////////////////////////////

        /* Maximum length of coordinate system description strings used
           with the proj library. */
        const size_t max_coordinate_system_description_length = 1000;
        /* The coordinate system as described by the ASF metadata. */
        char *tmp = malloc (max_coordinate_system_description_length + 1);
        int write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=lcc +a=%lf +b=%lf +lat_0=%lf +lon_0=%lf "
                      "+lat_1=%lf +lat_2=%lf", re_major, re_minor,
                      md->projection->param.lamcc.lat0,
                      md->projection->param.lamcc.lon0,
                      md->projection->param.lamcc.plat1,
                      md->projection->param.lamcc.plat2);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ input_coordinate_system = pj_init_plus (tmp);
        asfRequire (input_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        /* The coordinate system to be used for the output geotiff.  */
        write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=lcc +datum=WGS84 +lat_0=%lf +lon_0=%lf "
                      "+lat_1=%lf +lat_2=%lf",
                      md->projection->param.lamcc.lat0,
                      md->projection->param.lamcc.lon0,
                      md->projection->param.lamcc.plat1,
                      md->projection->param.lamcc.plat2);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ geotiff_coordinate_system = pj_init_plus (tmp);
        asfRequire (geotiff_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        double tmp1 = md->projection->startX;
        double tmp2 = md->projection->startY;
        double tmp3 = 0;
        return_code = pj_transform (input_coordinate_system,
                                    geotiff_coordinate_system, 1, 1, &tmp1,
                                    &tmp2, &tmp3);
        asfRequire (return_code == 0, "pj_transform signalled an error");
        /* The maximum allowable projection error.  If changing the
           datum from the one in the metadata to the WGS84 datum moves
           the projection corner point by this amount or more in
           projection coordinates, an exception is triggered.  This
           value was chosen based on a seat-of-the-pants feel which
           accounts for the various error sources: range migration,
           etc.  If the geolocation process is anywhere near this
           accurate, we are doing really good. */
        const double max_allowable_projection_error = 30.0;
        asfRequire (sqrt (pow (fabs (tmp1 - md->projection->startX), 2)
                          + pow (fabs (tmp2 - md->projection->startY), 2))
                    < max_allowable_projection_error,
                    "using the WGS84 datum to represent data with a \n"
                    "datum resulted in too much error");
        free (tmp);

        GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);

        ///////////////////////////////////////////////////////////////////////

        /* Set the citation key.  */
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length
          = snprintf (citation, max_citation_length + 1,
                      "Lambers conformal conic projected GeoTIFF using %s "
                      "ellipsoid datum written by Alaska Satellite Facility "
                      "tools.", ellipsoid_name (ellipsoid));
        asfRequire (citation_length >= 0
                    && citation_length <= max_citation_length,
                    "bad citation length");
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
        break;
      }

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      {
        set_common_keys (ogtif);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_LambertAzimEqualArea);
        GTIFKeySet (ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.center_lat);
        GTIFKeySet (ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.center_lon);
        set_false_easting_and_northing (ogtif, 0.0, 0.0);

        ///////////////////////////////////////////////////////////////////////
        //
        // Here we employ a slightly weird strategy: we always use a
        // WGS84 datum, no matter what the ASF metadata for the
        // product we are exporting says.  This is ok because the
        // error introduced is very small compared to other error
        // sources, and the geotiff viewers handle WGS84 datums much
        // better than user defined ones.
        //
        ///////////////////////////////////////////////////////////////////////

        /* Maximum length of coordinate system description strings used
           with the proj library. */
        const size_t max_coordinate_system_description_length = 1000;
        /* The coordinate system as described by the ASF metadata. */
        char *tmp = malloc (max_coordinate_system_description_length + 1);
        int write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=laea +a=%lf +b=%lf +lat_0=%lf +lon_0=%lf",
                      re_major, re_minor,
                      md->projection->param.lamaz.center_lat,
                      md->projection->param.lamaz.center_lon);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ input_coordinate_system = pj_init_plus (tmp);
        asfRequire (input_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        /* The coordinate system to be used for the output geotiff.  */
        write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=laea +datum=WGS84 +lat_0=%lf +lon_0=%lf",
                      md->projection->param.lamaz.center_lat,
                      md->projection->param.lamaz.center_lon);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ geotiff_coordinate_system = pj_init_plus (tmp);
        asfRequire (geotiff_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        double tmp1 = md->projection->startX;
        double tmp2 = md->projection->startY;
        double tmp3 = 0;
        return_code = pj_transform (input_coordinate_system,
                                    geotiff_coordinate_system, 1, 1, &tmp1,
                                    &tmp2, &tmp3);
        asfRequire (return_code == 0, "pj_transform signalled an error");
        /* The maximum allowable projection error.  If changing the
           datum from the one in the metadata to the WGS84 datum moves
           the projection corner point by this amount or more in
           projection coordinates, an exception is triggered.  This
           value was chosen based on a seat-of-the-pants feel which
           accounts for the various error sources: range migration,
           etc.  If the geolocation process is anywhere near this
           accurate, we are doing really good. */
        const double max_allowable_projection_error = 30.0;
        asfRequire (sqrt (pow (fabs (tmp1 - md->projection->startX), 2)
                          + pow (fabs (tmp2 - md->projection->startY), 2))
                    < max_allowable_projection_error,
                    "using the WGS84 datum to represent data with a \n"
                    "datum resulted in too much error");
        free (tmp);

        GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);

        ///////////////////////////////////////////////////////////////////////

        /* Set the citation key.  */
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length
          = snprintf (citation, max_citation_length + 1,
                      "Lambert azimuthal equal area projected GeoTIFF using "
                      "%s ellipsoid datum written by Alaska Satellite "
                      "Facility tools.", ellipsoid_name (ellipsoid));
        asfRequire (citation_length >= 0
                    && citation_length <= max_citation_length,
                    "bad citation length");
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
        break;
      }

    case ALBERS_EQUAL_AREA:
      {
        set_common_keys (ogtif);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_AlbersEqualArea);
        GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.std_parallel1);
        GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.std_parallel2);
        GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.orig_latitude);
        GTIFKeySet (ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.center_meridian);
        set_false_easting_and_northing (ogtif, 0.0, 0.0);

        ///////////////////////////////////////////////////////////////////////
        //
        // Here we employ a slightly weird strategy: we always use a
        // WGS84 datum, no matter what the ASF metadata for the
        // product we are exporting says.  This is ok because the
        // error introduced is very small compared to other error
        // sources, and the geotiff viewers handle WGS84 datums much
        // better than user defined ones.
        //
        ///////////////////////////////////////////////////////////////////////

        /* Maximum length of coordinate system description strings used
           with the proj library. */
        const size_t max_coordinate_system_description_length = 1000;
        /* The coordinate system as described by the ASF metadata. */
        char *tmp = malloc (max_coordinate_system_description_length + 1);
        int write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=aea +a=%lf +b=%lf +lat_1=%lf +lat_2=%lf "
                      "+lat_0=%lf +lon_0=%lf", re_major, re_minor,
                      md->projection->param.albers.std_parallel1,
                      md->projection->param.albers.std_parallel2,
                      md->projection->param.albers.orig_latitude,
                      md->projection->param.albers.center_meridian);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ input_coordinate_system = pj_init_plus (tmp);
        asfRequire (input_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        /* The coordinate system to be used for the output geotiff.  */
        write_count
          = snprintf (tmp, max_coordinate_system_description_length + 1,
                      "+proj=aea +datum=WGS84 +lat_1=%lf +lat_2=%lf "
                      "+lat_0=%lf +lon_0=%lf",
                      md->projection->param.albers.std_parallel1,
                      md->projection->param.albers.std_parallel2,
                      md->projection->param.albers.orig_latitude,
                      md->projection->param.albers.center_meridian);
        asfRequire (write_count < max_coordinate_system_description_length + 1,
                    "problem forming projection description for proj library");
        projPJ geotiff_coordinate_system = pj_init_plus (tmp);
        asfRequire (geotiff_coordinate_system != NULL,
                    "problem initializing projection description for proj");
        double tmp1 = md->projection->startX;
        double tmp2 = md->projection->startY;
        double tmp3 = 0;
        return_code = pj_transform (input_coordinate_system,
                                    geotiff_coordinate_system, 1, 1, &tmp1,
                                    &tmp2, &tmp3);
        asfRequire (return_code == 0, "pj_transform signalled an error");
        /* The maximum allowable projection error.  If changing the
           datum from the one in the metadata to the WGS84 datum moves
           the projection corner point by this amount or more in
           projection coordinates, an exception is triggered.  This
           value was chosen based on a seat-of-the-pants feel which
           accounts for the various error sources: range migration,
           etc.  If the geolocation process is anywhere near this
           accurate, we are doing really good. */
        const double max_allowable_projection_error = 30.0;
        asfRequire (sqrt (pow (fabs (tmp1 - md->projection->startX), 2)
                          + pow (fabs (tmp2 - md->projection->startY), 2))
                    < max_allowable_projection_error,
                    "using the WGS84 datum to represent data with a \n"
                    "datum resulted in too much error");
        free (tmp);

        GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);

        ///////////////////////////////////////////////////////////////////////

        /* Set the citation key.  */
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length
          = snprintf (citation, max_citation_length + 1,
                      "Albers equal-area conic projected GeoTIFF using %s "
                      "ellipsoid datum written by Alaska Satellite Facility "
                      "tools.", ellipsoid_name (ellipsoid));
        asfRequire (citation_length >= 0
                    && citation_length <= max_citation_length,
                    "bad citation length");
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
        break;
      }
    default:
      /* Shouldn't be here.  */
      asfPrintError ("Unable to cope with input map projection.\n");
    }
  }

  /* FIXME: this is a terrible hack to deal with scansar crap.  */
  else if ( md->sar->image_type == 'G'
            || (md->sar->image_type == 'P'
                && md->projection->type == SCANSAR_PROJECTION) ) {
    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeGeographic);

    /*    if ( ellipsoid == WGS84 ) {*/
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_WGS84);
      /*    }
       *    else {
       *      ** User defined geographic coordinate system.  **
       *      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1,
       *                  user_defined_value_code);
       *      switch ( ellipsoid ) {
       *      case CLARKE1866:
       *        GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *                    DatumE_Clarke1866);
       *        break;
       *      case GEM10C:
       *        GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *                    DatumE_GEM10C);
       *        break;
       *      case WGS66:
       *        ** Set to a newrby available ellipsoid.  We have far worse
       *           problems than the ellipsoid being a bit wrong.  **
       *        GTIFKeySet (ogtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,
       *                    DatumE_WGS84);
       *        break;
       *      case WGS84:
       *        ** Shouldn't be here (this should have been handled using the
       *           non-user defined GeographicTypeGeoKey).  **
       *        asfRequire(FALSE);
       *        break;
       *      default:
       *        asfRequire(FALSE);         ** Shouldn't be here.  **
       *      }
       *    }
       */
    GTIFKeySet (ogtif, GeogPrimeMeridianGeoKey, TYPE_SHORT, 1, PM_Greenwich);
    GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
    {
      /* Tie points for image corners.  There is space for four tie
         points, each consisting of three raster coordinates, followed
         by three geospatial coordinates.  */
      /* FIXME: I suspect this code of being wrong, I think it should
	 just be a big flat array of 4 * 6 values.  Not sure where the
	 idea of a multidimensional array got started, couldn't find
	 any examples of it and the makegeo program that comes with
	 GeoTIFF uses a flat array.  I'm not changing anything now
	 since this branch is probably so broken it doesn't really
	 matter, and GeoTIFF definition of images tied down using this
	 three tie point style is horribly vauge and unsupported.  */
      double tie_points[4][6];

      /* Get the lat/longs of three image corners.  */
      double c1_lat, c1_long, c2_lat, c2_long, c3_lat, c3_long;
      meta_get_latLon (md, 0, 0, 0, &c1_lat, &c1_long);
      meta_get_latLon (md, 0, iim->size_x, 0, &c2_lat, &c2_long);
      meta_get_latLon (md, iim->size_y, 0, 0, &c3_lat, &c3_long);

      /* Put three tie points in the image, as described in 2.6.2 of the
         geotiff spec..  */
      tie_points[0][0] = 0.0;
      tie_points[0][1] = 0.0;
      tie_points[0][2] = 0.0;
      tie_points[0][3] = c1_lat;
      tie_points[0][4] = c1_long;
      tie_points[0][5] = 0.0;
      tie_points[1][0] = 0.0;
      tie_points[1][1] = si->size_x;
      tie_points[1][2] = 0.0;
      tie_points[1][4] = c2_lat;
      tie_points[1][5] = c2_long;
      tie_points[1][6] = 0.0;
      tie_points[2][0] = si->size_y;
      tie_points[2][1] = 0.0;
      tie_points[2][2] = 0.0;
      tie_points[2][4] = c3_lat;
      tie_points[2][5] = c3_long;
      tie_points[2][6] = 0.0;

      /* Write the eighteen values that make up the three tie
         points.  */
      TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 18, tie_points);
    }
  }

  else if ( md->sar->image_type == 'S' ) {
    /* Slant range image conversion not implemented yet.  */
    asfPrintError("Slant range image conversion not implemented yet.\n");
  }

  else {
    /* Shouldn't be here (unrecognized image type). */
    asfPrintError("Unrecognized image type.\n");
  }

  asfPrintStatus("Writing Output File...\n");

  // Stuff for histogram equalization
  gsl_histogram_pdf *my_hist_pdf = NULL;
  if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
    my_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
    gsl_histogram_pdf_init (my_hist_pdf, my_hist);
  }

  /* Write the actual image data.  */
  float *line_buffer = g_new (float, si->size_x);
  unsigned char *byte_line_buffer = g_new (unsigned char, si->size_x);
  for ( ii = 0 ; ii < si->size_y ; ii++ ) {
    if ( sample_mapping == NONE ) {
      float_image_get_row (si, ii, line_buffer);
      if ( TIFFWriteScanline (otif, line_buffer, ii, 0) < 0 ) {
        asfPrintError("Error writing to output geotiff file %s",
                      output_file_name);
      }
    }
    else {
      for ( jj = 0 ; jj < si->size_x ; jj++ ) {
        float paf = float_image_get_pixel (si, jj, ii);     // Pixel as float.
        byte_line_buffer[jj] = pixel_float2byte(paf, sample_mapping, omin,
                                                omax, my_hist, my_hist_pdf);
      }
      if ( TIFFWriteScanline (otif, byte_line_buffer, ii, 0) < 0 ) {
        asfPrintError("Error writing to output geotiff file %s",
                      output_file_name);
      }
    }
    asfLineMeter(ii, si->size_y);
  }
  g_free (byte_line_buffer);
  g_free (line_buffer);

  return_code = GTIFWriteKeys (ogtif);
  asfRequire (return_code, "Error writing geotiff keys.\n");

  GTIFFree (ogtif);
  XTIFFClose (otif);

  // If the scale factor wasn't one, the scaled version of the image
  // will be different from the original and so will need to be freed
  // seperately.
  if ( si != iim) {
    float_image_free (si);
  }

  float_image_free (iim);
  meta_free (md);
}

