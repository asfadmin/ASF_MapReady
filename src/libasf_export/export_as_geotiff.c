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
   save for later.  The string is gauranteed   TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, sample_format);
to be less than 100
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
                   scale_t sample_mapping)
{
  char **in_base_names;

  in_base_names = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  in_base_names[0] = (char *) MALLOC(512*sizeof(char));
  in_base_names[0] = stripExt(image_data_file_name);

  return 
    export_rgb_as_geotiff(in_base_names, output_file_name, sample_mapping, 0);
}

void
tags_for_geotiff (const char *metadata_file_name,
		  const char *image_data_file_name,
		  const char *output_file_name,
		  scale_t sample_mapping,
		  int rgb,
		  TIFF **out_tif, GTIF **out_gtif)
{
  TIFF *otif;
  GTIF *ogtif;

  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);

  /* Scale factor needed to satisfy max_size argument.  */
  unsigned short sample_size = 4;
  unsigned short sample_format;
  int return_code;

  /* Major and minor ellipse axis lengths.  This shows up in two
     different places in our metadata, we want the projected one if
     its available, otherwise the one from general.  */
  double re_major, re_minor;
  /* Nail down which ellipsoid we are on exactly.  Old ASF metadata
     versions don't specify this though, so we may want to take a look
     at the major and minor axis values and try to find a matching
     ellipsoid.  */
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

  asfRequire (sizeof (unsigned short) == 2,
              "Unsigned short integer data type size is different than "
              "expected.\n");
  asfRequire (sizeof (unsigned int) == 4,
              "Unsigned integer data type size is different than expected.\n");

  if (sample_mapping != NONE) {
    
    asfRequire (sizeof(unsigned char) == 1,
		"Size of the unsigned char data type on this machine is "
		"different than expected.\n");
    sample_size = 1;
    sample_format = SAMPLEFORMAT_UINT;
  }
  else {
    g_assert (sizeof (float) == 4);
    sample_size = 4;
    sample_format = SAMPLEFORMAT_IEEEFP;
  }

  /* Open output tiff file and GeoKey file descriptor.  */
  otif = XTIFFOpen (output_file_name, "w");
  asfRequire(otif != NULL, "Error opening output tiff file.\n");
  ogtif = GTIFNew (otif);
  asfRequire (ogtif != NULL, "Error opening output GeoKey file descriptor.\n");

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, md->general->sample_count);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, md->general->line_count);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  if (rgb) {
    TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 3);
  }
  else {
    TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
    TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  }
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
     "points" may seem a bit weird.  However, nobody seems to know
     exactly how observation pixels relate to the geolocation
     information associated with the image anyway, and using
     PixelIsPoint will let scaling work right when we average together
     blocks of pixels with the first block at pixel 0,0.  */
  GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsPoint);

  if ( md->projection ) {
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
  if ( md->projection != NULL && md->projection->type != SCANSAR_PROJECTION) {
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

      // Since we're assuming wgs84, might as well use those values
      // in the projection parameters
      re_major = wgs84_major_axis;
      re_minor = wgs84_minor_axis;
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
       write all computed tie points to the GeoTIFF, change the 6 in
       the line below.  */
    TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_point);

    /* Set the scale of the pixels, in projection coordinates.  */
    if (md->projection->perX < 0.0)
        asfPrintWarning("Unexpected non-positive perX in the "
                        "projection block.\n");

    pixel_scale[0] = fabs(md->projection->perX);

    if (md->projection->perY > 0.0)
        asfPrintWarning("Unexpected non-negative perY in the "
                        "projection block.\n");

    pixel_scale[1] = fabs(md->projection->perY);
    pixel_scale[2] = 0;
    TIFFSetField (otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);

    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1,
                ModelTypeProjected);

    /* Write the appropriate geotiff keys for the projection type.  */
    switch ( md->projection->type ) {

    case UNIVERSAL_TRANSVERSE_MERCATOR:
      {
	/* If we're using a WGS84 ellipsoid, life is easy, we can just
	   use a single GeoTIFF tag to express everything about the
	   coordinates system.  */
	if ( ellipsoid == WGS84 ) {

	  /* Here we use some funky arithmetic to get the correct
	     geotiff coordinate system type key from our zone code.
	     Here are a few assertions to try to ensure that the
	     convention used for the libgeotiff constants is as
	     expected.  Also note that we have already verified that
	     we are on a WGS84 ellipsoid.  */
	  asfRequire(PCS_WGS84_UTM_zone_60N - PCS_WGS84_UTM_zone_1N == 59,
		     "Unable to create geotiff tags to accepted "
		     "convention.\n");
	  asfRequire(PCS_WGS84_UTM_zone_60S - PCS_WGS84_UTM_zone_1S == 59,
		     "Unable to create geotiff tags to accepted "
		     "convention.\n");

	  if ( md->projection->hem == 'N' ) {
	    const int northern_utm_zone_base = PCS_WGS84_UTM_zone_1N - 1;
	    projection_code = northern_utm_zone_base;
	  }
	  else if ( md->projection->hem == 'S' ) {
	    const int southern_utm_zone_base = PCS_WGS84_UTM_zone_1S - 1;
	    projection_code = southern_utm_zone_base;
	  }
	  else {               /* Shouldn't be here.  */
	    asfPrintError("You are not in the northern or southern "
			  "hemisphere;\nyou are now in the twilight zone");
	  }
	  projection_code += md->projection->param.utm.zone;
	  
	  GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
		      projection_code);
	  GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1,
		      Linear_Meter);
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

	/* Not so lucky as to be using WGS84.  Try to set up other datum.  */
	else {
	  /* For now we only handle UTM data that is referenced to the
	     WGS84 ellipsoid.  */
	  asfRequire(ellipsoid == WGS84,
		     "UTM data must be relative to the WGS84 ellipsoid.\n");
	}
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
  else if ( md->sar ) {
    if ( md->sar->image_type == 'G'
	 || (md->sar->image_type == 'P'
	     && md->projection->type == SCANSAR_PROJECTION) ) {
      GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, 
		  ModelTypeGeographic);
      
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
      GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, 
		  Angular_Degree);
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
	meta_get_latLon (md, 0, md->general->sample_count, 0, &c2_lat, &c2_long);
	meta_get_latLon (md, md->general->line_count, 0, 0, &c3_lat, &c3_long);
	
	/* Put three tie points in the image, as described in 2.6.2 of the
	   geotiff spec..  */
	tie_points[0][0] = 0.0;
	tie_points[0][1] = 0.0;
	tie_points[0][2] = 0.0;
	tie_points[0][3] = c1_lat;
	tie_points[0][4] = c1_long;
	tie_points[0][5] = 0.0;
	tie_points[1][0] = 0.0;
	tie_points[1][1] = md->general->sample_count;
	tie_points[1][2] = 0.0;
	tie_points[1][4] = c2_lat;
	tie_points[1][5] = c2_long;
	tie_points[1][6] = 0.0;
	tie_points[2][0] = md->general->line_count;
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
  }

  meta_free (md);

  *out_tif = otif;
  *out_gtif = ogtif;
}

void
export_rgb_as_geotiff (char **in_base_names,
		       const char *output_file_name,
		       scale_t sample_mapping,
		       int rgb)
{
  TIFF *otif=NULL;
  GTIF *ogtif=NULL;
  int jj;
  ssize_t ii;
  char metadata_file_name[512], image_data_file_name[512];
  int return_code;

  sprintf(metadata_file_name, "%s.meta", in_base_names[0]);
  sprintf(image_data_file_name, "%s.img", in_base_names[0]);
  meta_parameters *md = meta_read (metadata_file_name);

  // Work out all those tags
  tags_for_geotiff (metadata_file_name, image_data_file_name,
		    output_file_name, sample_mapping, rgb, &otif, &ogtif);

  if (rgb) {
    double red_min, red_max, red_mean, red_stdDev;
    double green_min, green_max, green_mean, green_stdDev;
    double blue_min, blue_max, blue_mean, blue_stdDev;

    // Red channel statistics
    gsl_histogram *red_hist = NULL;
    gsl_histogram_pdf *red_hist_pdf = NULL;
    if (sample_mapping != NONE) { // byte image
      asfRequire (sizeof(unsigned char) == 1,
		  "Size of the unsigned char data type on this machine is "
		  "different than expected.\n");
      asfPrintStatus("Gathering red channel statistics ...\n");
      calc_stats_from_file(in_base_names[0], 0.0, &red_min, &red_max, 
			   &red_mean, &red_stdDev, red_hist);
      if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	red_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	gsl_histogram_pdf_init (red_hist_pdf, red_hist);
      }
    }
    
    // Green channel statistics
    gsl_histogram *green_hist = NULL;
    gsl_histogram_pdf *green_hist_pdf = NULL;
    if (sample_mapping != NONE) { // byte image
      asfRequire (sizeof(unsigned char) == 1,
		  "Size of the unsigned char data type on this machine is "
		  "different than expected.\n");
      asfPrintStatus("Gathering green channel statistics ...\n");
      calc_stats_from_file(in_base_names[1], 0.0, &green_min, &green_max, 
			   &green_mean, &green_stdDev, green_hist);
      if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	green_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	gsl_histogram_pdf_init (green_hist_pdf, green_hist);
      }
    }
    
    // Blue channel statistics
    gsl_histogram *blue_hist = NULL;
    gsl_histogram_pdf *blue_hist_pdf = NULL;
    if (sample_mapping != NONE) { // byte image
      asfRequire (sizeof(unsigned char) == 1,
		  "Size of the unsigned char data type on this machine is "
		  "different than expected.\n");
      asfPrintStatus("Gathering blue channel statistics ...\n");
      calc_stats_from_file(in_base_names[2], 0.0, &blue_min, &blue_max, 
			   &blue_mean, &blue_stdDev, blue_hist);
      if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	blue_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	gsl_histogram_pdf_init (blue_hist_pdf, blue_hist);
      }
    }

    // Write the actual image data
    FILE *fp_red, *fp_green, *fp_blue;
    float *red_float_line, *green_float_line, *blue_float_line, *rgb_float_line;
    unsigned char *rgb_byte_line;

    fp_red = FOPEN(image_data_file_name, "rb");
    sprintf(image_data_file_name, "%s.img", in_base_names[1]);
    fp_green = FOPEN(image_data_file_name, "rb");
    sprintf(image_data_file_name, "%s.img", in_base_names[2]);
    fp_blue = FOPEN(image_data_file_name, "rb");

    int sample_count = md->general->sample_count;
    red_float_line = (float *) MALLOC(sizeof(float) * sample_count);
    green_float_line = (float *) MALLOC(sizeof(float) * sample_count);
    blue_float_line = (float *) MALLOC(sizeof(float) * sample_count);
    rgb_float_line = (float *) MALLOC(sizeof(float) * sample_count * 3);
    rgb_byte_line = (unsigned char *) 
      MALLOC(sizeof(unsigned char) * sample_count * 3);

    for (ii=0; ii<md->general->line_count; ii++) {
      if (sample_mapping == NONE) {
	get_float_line(fp_red, md, ii, red_float_line);
	get_float_line(fp_green, md, ii, green_float_line);
	get_float_line(fp_blue, md, ii, blue_float_line);
	for (jj=0; jj<sample_count; jj++) {
	  rgb_float_line[jj*3] = red_float_line[jj];
	  rgb_float_line[(jj*3)+1] = green_float_line[jj];
	  rgb_float_line[(jj*3)+2] = blue_float_line[jj];
	}
	if ( TIFFWriteScanline (otif, rgb_float_line, ii, 0) < 0 ) {
	  asfPrintError("Error writing to output geotiff file %s",
			output_file_name);
	}
      }
      else {
	get_float_line(fp_red, md, ii, red_float_line);
	get_float_line(fp_green, md, ii, green_float_line);
	get_float_line(fp_blue, md, ii, blue_float_line);
	for (jj=0; jj<sample_count; jj++) {
	  rgb_byte_line[jj*3] = 
	    pixel_float2byte(red_float_line[jj], sample_mapping, 
			     red_min, red_max, red_hist, red_hist_pdf, 
			     md->general->no_data);
	  rgb_byte_line[(jj*3)+1] = 
	    pixel_float2byte(green_float_line[jj], sample_mapping, 
			     green_min, green_max, green_hist, green_hist_pdf, 
			     md->general->no_data);
	  rgb_byte_line[(jj*3)+2] = 
	    pixel_float2byte(blue_float_line[jj], sample_mapping, 
			     blue_min, blue_max, blue_hist, blue_hist_pdf, 
			     md->general->no_data);
	}
	if ( TIFFWriteScanline (otif, rgb_byte_line, ii, 0) < 0 ) {
	  asfPrintError("Error writing to output geotiff file %s",
			output_file_name);
	}
      }
      asfLineMeter(ii, md->general->line_count);
    }
    FREE(red_float_line);
    FREE(green_float_line);
    FREE(blue_float_line);
    FREE(rgb_float_line);
    FREE(rgb_byte_line);
  }
  else {
    double min, max, mean, stdDev;

    // image statistics
    gsl_histogram *hist = NULL;
    gsl_histogram_pdf *hist_pdf = NULL;
    if (sample_mapping != NONE) { // byte image
      asfRequire (sizeof(unsigned char) == 1,
		  "Size of the unsigned char data type on this machine is "
		  "different than expected.\n");
      if (rgb)
	asfPrintStatus("Gathering red channel statistics ...\n");
      else
	asfPrintStatus("Gathering statistics ...\n");
      calc_stats_from_file(in_base_names[0], 0.0, &min, &max, 
			   &mean, &stdDev, hist);
      if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	gsl_histogram_pdf_init (hist_pdf, hist);
      }
    }
    
    FILE *fp;
    float *float_line;
    unsigned char *byte_line;

    fp = FOPEN(image_data_file_name, "rb");
    int sample_count = md->general->sample_count;
    float_line = (float *) MALLOC(sizeof(float) * sample_count);
    for (ii=0; ii<md->general->line_count; ii++ ) {
      if (sample_mapping == NONE) {
	get_float_line(fp, md, ii, float_line);
	if ( TIFFWriteScanline (otif, float_line, ii, 0) < 0 ) {
	  asfPrintError("Error writing to output geotiff file %s",
			output_file_name);
	}
      }
      else {
	for (jj=0; jj<sample_count; jj++) {
	  byte_line[jj] = 
	    pixel_float2byte(float_line[jj], sample_mapping, 
			     min, max, hist, hist_pdf, md->general->no_data);
	}
	if ( TIFFWriteScanline (otif, byte_line, ii, 0) < 0 ) {
	  asfPrintError("Error writing to output geotiff file %s",
			output_file_name);
	}
      }
      asfLineMeter(ii, md->general->line_count);
    }
  }

  return_code = GTIFWriteKeys (ogtif);
  asfRequire (return_code, "Error writing geotiff keys.\n");

  GTIFFree (ogtif);
  XTIFFClose (otif);

  meta_free (md);
}
