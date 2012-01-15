// standard libraries
#include <math.h>

// libraries developed by ASF
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>

// Prototypes
int asf_calc_utm_zone(double lon);
void check_parameters(projection_type_t projection_type, datum_type_t datum,
		      project_parameters_t *pp, meta_parameters *meta,
		      int force_flag);

// Checking routine for projection parameter input.
void check_parameters(projection_type_t projection_type, datum_type_t datum,
		      project_parameters_t *pp, meta_parameters *meta,
		      int force_flag)
{
  double lon, min_lat, max_lat;
  int zone, min_zone=60, max_zone=1;

  void (*report_func) (const char *format, ...);

  if (force_flag) {
    report_func = asfPrintWarning;
  }
  else {
    report_func = asfPrintError;
  }

  asfRequire(!(datum == HUGHES_DATUM && projection_type != POLAR_STEREOGRAPHIC),
               "Hughes ellipsoid is only supported for polar stereographic projections.\n");

  switch (projection_type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:

      // Outside range tests:
      //
      // Valid UTM projections:
      //
      //   WGS84 + zone 1 thru 60 + N or S hemisphere
      //   NAD83 + zone 2 thru 23 + N hemisphere
      //   NAD27 + zone 2 thru 22 + N hemisphere
      //
      if (!meta_is_valid_int(pp->utm.zone)) {
        report_func("Invalid zone number found (%d).\n", pp->utm.zone);
      }
      if (!meta_is_valid_double(pp->utm.lat0)) {
        report_func("Invalid Latitude of Origin found (%.4f).\n", pp->utm.lat0);
      }
      switch(datum) {
        case NAD27_DATUM:
        {
          // FIXME: Might want to consider keeping even the lowest latitude in the
          // image inside the northern hemisphere
          double lower_limiting_latitude = meta->general->center_latitude;
          if (pp->utm.zone < 2 || pp->utm.zone > 22)
            report_func("Zone '%i' outside the supported range (2 to 22) for NAD27...\n%s"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                pp->utm.zone, !force_flag ?
                  "\nUse the -force option (Ignore projection errors) or adjust the\n"
                  "selected projection parameters to something more appropriate:\n\n" : "");
          if (lower_limiting_latitude < 0 || lower_limiting_latitude > 90)
            report_func("Image location (latitude %.4f) outside the supported range "
                "(0 deg to +90 deg) for NAD27...\n%s"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                lower_limiting_latitude, !force_flag ?
                  "\nUse the -force option (Ignore projection errors) or adjust the\n"
                  "selected projection parameters to something more appropriate:\n\n" : "");
        }
          break;
        case NAD83_DATUM:
        {
          // FIXME: Might want to consider keeping even the lowest latitude in the
          // image inside the northern hemisphere
          double lower_limiting_latitude = meta->general->center_latitude;
          if (pp->utm.zone < 2 || pp->utm.zone > 23)
            report_func("Zone '%i' outside the supported range (2 to 23) for NAD83...\n%s"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                pp->utm.zone, !force_flag ?
                  "\nUse the -force option (Ignore projection errors) or adjust the\n"
                  "selected projection parameters to something more appropriate:\n\n" : "");
          if (lower_limiting_latitude < 0 || lower_limiting_latitude > 90)
            report_func("Image location (latitude %.4f) outside the supported range "
                "(0 deg to +90 deg) for NAD83...\n%s"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                lower_limiting_latitude, !force_flag ?
                  "\nUse the -force option (Ignore projection errors) or adjust the\n"
                  "selected projection parameters to something more appropriate:\n\n" : "");
        }
          break;
        case WGS84_DATUM:
        {
          // FIXME: Technically, we should use the lowest attitude in the image to compare to -90
          // and the highest latitude in the image to compare to +90...
          double limiting_latitude = meta->general->center_latitude;
          if (pp->utm.zone < 1 || pp->utm.zone > 60)
            report_func("Zone '%i' outside the valid range of (1 to 60)\n", pp->utm.zone);
          if (limiting_latitude < -90 || limiting_latitude > 90)
            report_func("Image location (latitude %.4f) outside the valid range "
                "of (-90 deg to +90 deg)\n", limiting_latitude);
        }
          break;
        default:
          report_func("Unrecognized or unsupported datum found in projection parameters.\n");
          break;
      }
      if (!meta_is_valid_double(pp->utm.lon0) || pp->utm.lon0 < -180 || pp->utm.lon0 > 180)
        report_func("Central meridian (%.4f) undefined or outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->utm.lon0);
      if (!meta_is_valid_double(pp->utm.scale_factor) || !FLOAT_EQUIVALENT(pp->utm.scale_factor, 0.9996))
        report_func("Scale factor (%.4f) undefined or different from default value (0.9996)\n",
		      pp->utm.scale_factor);
      if (!meta_is_valid_double(meta->general->center_latitude) ||
          meta->general->center_latitude < -90.0 ||
          meta->general->center_latitude > 90.0)
        report_func("Central latitude (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", meta->general->center_latitude);
      if (!meta_is_valid_double(pp->utm.false_easting) ||
          (meta->general->center_latitude >= 0.0 &&
          !FLOAT_EQUIVALENT(pp->utm.false_easting, 500000))
         )
        report_func("False easting (%.1f) undefined or different from default value (500000)\n",
                      pp->utm.false_easting);
      if (!meta_is_valid_double(pp->utm.false_northing) ||
          (meta->general->center_latitude >= 0.0 &&
          !FLOAT_EQUIVALENT(pp->utm.false_northing, 0))
         )
        report_func("False northing (%.1f) undefined or different from default value (0)\n",
                      pp->utm.false_northing);
      if (!meta_is_valid_double(pp->utm.false_easting) ||
          (meta->general->center_latitude < 0.0 &&
          !FLOAT_EQUIVALENT(pp->utm.false_easting, 500000))
         )
        report_func("False easting (%.1f) undefined or different from default value (500000)\n",
                      pp->utm.false_easting);
      if (!meta_is_valid_double(pp->utm.false_northing) ||
          (meta->general->center_latitude < 0.0 &&
          !FLOAT_EQUIVALENT(pp->utm.false_northing, 10000000))
         )
        report_func("False northing (%.1f) undefined or different from default value (10000000)\n",
                      pp->utm.false_northing);

      //// Zone test - The zone must contain some dirt (or water) in the image.
      // FIXME: meta_get_latLon() really should handle the LAT_LONG_PSEUDO_PROJECTION
      // case rather than kludging it in here...

      // Top left zone
      double lat;
      if (meta->location)
          lon = meta->location->lon_start_near_range;
      else
          meta_get_latLon(meta, 0, 0, 0.0, &lat, &lon);
      zone = asf_calc_utm_zone(lon);
      if (zone < min_zone) min_zone = zone;
      if (zone > max_zone) max_zone = zone;

      // Top right zone
      if (meta->location)
          lon = meta->location->lon_start_far_range;
      else
          meta_get_latLon(meta, 0, meta->general->sample_count - 1, 0.0,
              &lat, &lon);
      zone = asf_calc_utm_zone(lon);
      if (zone < min_zone) min_zone = zone;
      if (zone > max_zone) max_zone = zone;

      // Bottom left zone
      if (meta->location)
          lon = meta->location->lon_end_near_range;
      else
          meta_get_latLon(meta, meta->general->line_count - 1, 0, 0.0,
              &lat, &lon);
      zone = asf_calc_utm_zone(lon);
      if (zone < min_zone) min_zone = zone;
      if (zone > max_zone) max_zone = zone;

      // Bottom right zone
      if (meta->location)
          lon = meta->location->lon_end_far_range;
      else
          meta_get_latLon(meta, meta->general->line_count - 1,
      	      meta->general->sample_count - 1, 0.0, &lat, &lon);
      zone = asf_calc_utm_zone(lon);
      if (zone < min_zone) min_zone = zone;
      if (zone > max_zone) max_zone = zone;

      // Handle special cases, where the center of the image is in
      // zone 1, or 60, and the edges are one zone over, accross the
      // 1/60 boundary.  These are the cases (left, center, right):
      //   (60,1,1), (60,1,2), (59,60,1), (60,60,1)
      if ((pp->utm.zone == 1 &&
           ((min_zone == 1 && max_zone == 60) ||
            (min_zone == 2 && max_zone == 60)))
          ||
          (pp->utm.zone == 60 &&
           ((min_zone == 1 && max_zone == 59) ||
            (min_zone == 1 && max_zone == 60))))
      {
        // this is okay, and happens sometimes with scansar images
        // no action necessary
        ;

      } else if (pp->utm.zone < min_zone || pp->utm.zone > max_zone + 1) {
        report_func("Zone '%i' outside the range of corresponding image "
		      "coordinates (%i to %i)\n", pp->utm.zone, min_zone,
		      max_zone);
      }
      break;

    case POLAR_STEREOGRAPHIC:
      // Outside range tests
      if (!meta_is_valid_double(pp->ps.slat) || pp->ps.slat < -90 || pp->ps.slat > 90)
        report_func("Latitude of origin (%.4f) undefined or outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->ps.slat);
      if (!meta_is_valid_double(pp->ps.slon) || pp->ps.slon < -180 || pp->ps.slon > 180)
        report_func("Central meridian (%.4f) undefined or outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->ps.slon);

      // Distortion test - only areas with a latitude above 60 degrees North or
      // below -60 degrees South are permitted
      if (!meta_is_valid_double(meta->general->center_latitude)) {
        report_func("Invalid center latitude (%.4f) found.\n",
                   meta->general->center_latitude);
      }
      if (!meta_is_valid_int(pp->ps.is_north_pole) ||
           (pp->ps.is_north_pole != 0 && pp->ps.is_north_pole != 1))
        report_func("Invalid north pole flag (%s) found.\n",
                    pp->ps.is_north_pole == 0 ? "SOUTH" :
                        pp->ps.is_north_pole == 1 ? "NORTH" : "UNKNOWN");
      if (meta->general->center_latitude < 60.0 && pp->ps.is_north_pole) {
	if (force_flag)
	  report_func("Geocoding of areas below 60 degrees latitude in the "
                      "polar stereographic map projection is not advisable.\n");
	else
	  report_func("Geocoding of areas below 60 degrees latitude in the "
                      "polar stereographic map projection is not supported "
		      "by this tool.\n");
      }
      if (meta->general->center_latitude > -60.0 && !pp->ps.is_north_pole) {
	if (force_flag)
	  report_func("Geocoding of areas above -60 degrees latitude in the "
                      "polar stereographic map projection is not advisable.\n");
	else
	  report_func("Geocoding of areas above -60 degrees latitude in the "
                      "polar stereographic map projection is not supported "
		      "by this tool.\n");
      }

      break;

    case ALBERS_EQUAL_AREA:

      // Outside range tests
      if (!meta_is_valid_double(pp->albers.std_parallel1) ||
           pp->albers.std_parallel1 < -90 ||
           pp->albers.std_parallel1 > 90)
        report_func("First standard parallel (%.4f) undefined or outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->albers.std_parallel1);
      if (!meta_is_valid_double(pp->albers.std_parallel2) ||
           pp->albers.std_parallel2 < -90 ||
           pp->albers.std_parallel2 > 90)
        report_func("Second standard parallel (%.4f) undefined or outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->albers.std_parallel2);
      if (!meta_is_valid_double(pp->albers.center_meridian) ||
           pp->albers.center_meridian < -180 ||
           pp->albers.center_meridian > 180)
        report_func("Central meridian (%.4f) undefined or outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->albers.center_meridian);
      if (!meta_is_valid_double(pp->albers.orig_latitude) ||
           pp->albers.orig_latitude < -90 ||
           pp->albers.orig_latitude > 90)
        report_func("Latitude of origin (%.4f) undefined or outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->albers.orig_latitude);

      // Distortion test - only areas with a latitude not more than 30 degrees
      // outside the latitude range defined by first and second parallel are
      // permitted.
      if (pp->albers.std_parallel1 < pp->albers.std_parallel2) {
	min_lat = pp->albers.std_parallel1 - 30.0;
	max_lat = pp->albers.std_parallel2 + 30.0;
      }
      else {
	min_lat = pp->albers.std_parallel2 - 30.0;
	max_lat = pp->albers.std_parallel1 + 30.0;
      }
      if (!meta_is_valid_double(meta->general->center_latitude))
        report_func("Invalid center latitude found (%.4f).\n",
                    meta->general->center_latitude);
      if (meta->general->center_latitude > max_lat ||
	  meta->general->center_latitude < min_lat) {
	if (force_flag)
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Albers Equal Area projection with "
		      "the standard parallels (first: %.1f and second: %.1f ) is "
		      "not advisable.\n", min_lat, max_lat,
		      pp->albers.std_parallel1, pp->albers.std_parallel2);
	else
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Albers Equal Area projection with "
		      "the standard parallels (first: %.1f and second: %.1f ) is "
		      "not supported by this tool.\n", min_lat, max_lat,
		      pp->albers.std_parallel1, pp->albers.std_parallel2);
      }
      break;

    case LAMBERT_CONFORMAL_CONIC:
      // Outside range tests
      if (!meta_is_valid_double(pp->lamcc.plat1) ||
           pp->lamcc.plat1 < -90 || pp->lamcc.plat1 > 90)
        report_func("First standard parallel (%.4f) undefined or outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->lamcc.plat1);
      if (!meta_is_valid_double(pp->lamcc.plat2) ||
           pp->lamcc.plat2 < -90 || pp->lamcc.plat2 > 90)
	report_func("Second standard parallel '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->lamcc.plat2);
      if (!meta_is_valid_double(pp->lamcc.lon0) ||
           pp->lamcc.lon0 < -180 || pp->lamcc.lon0 > 180)
	report_func("Central meridian '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->lamcc.lon0);
      if (!meta_is_valid_double(pp->lamcc.lat0) ||
           pp->lamcc.lat0 < -90 || pp->lamcc.lat0 > 90)
	report_func("Latitude of origin '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->lamcc.lat0);

      // Distortion test - only areas with a latitude not more than 30 degrees
      // outside the latitude range defined by first and second parallel are
      // permitted.
      if (pp->lamcc.plat1 < pp->lamcc.plat2) {
	min_lat = pp->lamcc.plat1 - 30.0;
	max_lat = pp->lamcc.plat2 + 30.0;
      }
      else {
	min_lat = pp->lamcc.plat2 - 30.0;
	max_lat = pp->lamcc.plat1 + 30.0;
      }
      if (!meta_is_valid_double(meta->general->center_latitude))
        report_func("Invalid center latitude found (%.4f).\n",
                    meta->general->center_latitude);
      if (meta->general->center_latitude > max_lat ||
	  meta->general->center_latitude < min_lat) {
	if (force_flag)
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Lambert Conformal Conic "
		      "projection with the standard parallels (first: %.1f and "
		      "second: %.1f ) is not advisable.\n", min_lat,
		      max_lat, pp->lamcc.plat1, pp->lamcc.plat2);
	else
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Lambert Conformal Conic "
		      "projection with the standard parallels (first: %.1f and "
		      "second: %.1f ) is not supported by this tool.\n", min_lat,
		      max_lat, pp->lamcc.plat1, pp->lamcc.plat2);
      }

      break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:

      // Outside range tests
      if (!meta_is_valid_double(pp->lamaz.center_lon) ||
           pp->lamaz.center_lon < -180 || pp->lamaz.center_lon > 180)
	report_func("Central meridian '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->lamaz.center_lon);
      if (!meta_is_valid_double(pp->lamaz.center_lat) ||
           pp->lamaz.center_lat < -90 || pp->lamaz.center_lat > 90)
	report_func("Latitude of origin '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->lamaz.center_lat);

      break;

    case EQUI_RECTANGULAR:

      // Outside range tests
      if (!meta_is_valid_double(pp->eqr.central_meridian) ||
           pp->eqr.central_meridian < -180 || pp->eqr.central_meridian > 180)
	report_func("Central meridian '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->eqr.central_meridian);
      if (!meta_is_valid_double(pp->eqr.orig_latitude) ||
           pp->eqr.orig_latitude < -90 || pp->eqr.orig_latitude > 90)
	report_func("Latitude of origin '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->eqr.orig_latitude);

      break;

    case EQUIDISTANT:

      // Outside range tests
      if (!meta_is_valid_double(pp->eqr.central_meridian) ||
           pp->eqc.central_meridian < -180 || pp->eqc.central_meridian > 180)
	report_func("Central meridian '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->eqc.central_meridian);
      if (!meta_is_valid_double(pp->eqr.orig_latitude) ||
           pp->eqc.orig_latitude < -90 || pp->eqc.orig_latitude > 90)
	report_func("Latitude of origin '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->eqc.orig_latitude);

      break;

    case MERCATOR:

      // Outside range tests
      if (!meta_is_valid_double(pp->mer.central_meridian) ||
           pp->mer.central_meridian < -180 || pp->mer.central_meridian > 180)
	report_func("Central meridian '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->mer.central_meridian);
      if (!meta_is_valid_double(pp->mer.orig_latitude) ||
           pp->mer.orig_latitude < -90 || pp->mer.orig_latitude > 90)
	report_func("Latitude of origin '%.4f' outside the defined range "
		      "(-90 deg to 90 deg)\n", pp->mer.orig_latitude);

      // Distortion test - only areas with a latitude not more than 30 degrees
      // outside the latitude range defined by first and second parallel are
      // permitted.
      min_lat = pp->mer.standard_parallel - 30.0;
      max_lat = pp->mer.standard_parallel + 30.0;
      if (!meta_is_valid_double(meta->general->center_latitude))
        report_func("Invalid center latitude found (%.4f).\n",
                    meta->general->center_latitude);
      if (meta->general->center_latitude > max_lat ||
	  meta->general->center_latitude < min_lat) {
	if (force_flag)
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Mercator projection with the "
		      "standard parallel (%.1f) is not advisable.\n", min_lat,
		      max_lat, pp->mer.standard_parallel);
	else
	  report_func("Geocoding of areas with latitudes outside the defined range "
		      "(%.1f deg %.1f deg) in the Mercator projection with the "
		      "standard parallel (%.1f) is not supported by this tool.\n", 
		      min_lat, max_lat, pp->mer.standard_parallel);
      }
      break;

    case SINUSOIDAL:

      // Outside range tests
      if (!meta_is_valid_double(pp->sin.longitude_center) ||
           pp->sin.longitude_center < -180 || pp->sin.longitude_center > 180)
	report_func("Longitude center '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->sin.longitude_center);

      break;

  case LAT_LONG_PSEUDO_PROJECTION:
    break;

    default:
      asfPrintError("Chosen projection type not supported!\n");
      break;
    }
}

