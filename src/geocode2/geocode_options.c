#include "geocode_options.h"
#include "parse_options.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_reporting.h"
#include "proj_api.h"

#include <stdlib.h>
#include <string.h>

project_parameters_t * get_geocode_options(int *argc, char **argv[],
					   projection_type_t * proj_type)
{
    project_parameters_t * pps;

    pps = parse_options(argc, argv, proj_type);

    if (pps)
    {
	apply_defaults(*proj_type, pps);
	sanity_check(*proj_type, pps);
    }

    return pps;
}

static int calc_utm_zone(double lon)
{
    return((int)(((lon + 180.0) / 6.0) + 1.0));
}

static void verify_valid_latitude(double lat)
{
    if (ISNAN(lat))
	return;

    if (lat > 90 || lat < -90)
    {
	asfPrintWarning("Invalid Latitude: %f\n", lat);
    }
}

static void verify_valid_longitude(double lon)
{
    if (ISNAN(lon))
	return;

    if (lon > 360 || lon < -360)
    {
	asfPrintWarning("Invalid Longitude: %f\n", lon);
    }
}

void sanity_check(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:

	    if (pps->utm.zone != MAGIC_UNSET_INT)
	    {
		if ((abs(pps->utm.zone) < 1) || (abs(pps->utm.zone) > 60))
		{
		    asfPrintError("Illegal zone number: %d\n", pps->utm.zone);
		}
	    }

	    verify_valid_latitude(pps->utm.lat0);
	    verify_valid_longitude(pps->utm.lon0);

	    break;

	case POLAR_STEREOGRAPHIC:

	    verify_valid_latitude(pps->ps.slat);
	    verify_valid_longitude(pps->ps.slon);

	    break;

	case ALBERS_EQUAL_AREA:

	    verify_valid_latitude(pps->albers.std_parallel1);
	    verify_valid_latitude(pps->albers.std_parallel2);
	    verify_valid_latitude(pps->albers.orig_latitude);
	    verify_valid_longitude(pps->albers.center_meridian);

	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:

	    verify_valid_latitude(pps->lamaz.center_lat);
	    verify_valid_longitude(pps->lamaz.center_lon);

	    break;

	case LAMBERT_CONFORMAL_CONIC:

	    verify_valid_latitude(pps->lamcc.plat1);
	    verify_valid_latitude(pps->lamcc.plat2);
	    verify_valid_latitude(pps->lamcc.lat0);
	    verify_valid_longitude(pps->lamcc.lon0);

	    break;

	default:
	    asfPrintError("sanity_check: illegal projection type!");
    }
}

void apply_defaults(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    /* set the zone based on the specified longitude */
	    if (!ISNAN(pps->utm.lon0))
	    {
		if (pps->utm.zone == MAGIC_UNSET_INT)
		    pps->utm.zone = calc_utm_zone(pps->utm.lon0);
	    }

	    /* false easting & false northing are fixed for utm */
	    if (!ISNAN(pps->utm.lat0))
	    {
		pps->utm.false_northing = pps->utm.lat0 > 0 ? 0 : 10000000;
		pps->utm.false_easting = 500000;
	    }
	    else
	    {
		pps->utm.false_easting = MAGIC_UNSET_DOUBLE;
		pps->utm.false_northing = MAGIC_UNSET_DOUBLE;
	    }

	    break;

	case POLAR_STEREOGRAPHIC:
	    if (ISNAN(pps->ps.false_easting))
		pps->ps.false_easting = 0;
	    if (ISNAN(pps->ps.false_northing))
		pps->ps.false_northing = 0;

	    break;

	case ALBERS_EQUAL_AREA:
	    if (ISNAN(pps->albers.false_easting))
		pps->albers.false_easting = 0;
	    if (ISNAN(pps->albers.false_northing))
		pps->albers.false_northing = 0;

	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    if (ISNAN(pps->lamaz.false_easting))
		pps->lamaz.false_easting = 0;
	    if (ISNAN(pps->lamaz.false_northing))
		pps->lamaz.false_northing = 0;

	    break;

	case LAMBERT_CONFORMAL_CONIC:
	    if (ISNAN(pps->lamcc.false_easting))
		pps->lamcc.false_easting = 0;
	    if (ISNAN(pps->lamcc.false_northing))
		pps->lamcc.false_northing = 0;

	    break;

	default:
	    asfPrintError("apply_defaults: illegal projection type!");
    }
}

void to_radians(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    if (!ISNAN(pps->utm.lon0))
		pps->utm.lon0 *= DEG_TO_RAD;
	    if (!ISNAN(pps->utm.lat0))
		pps->utm.lat0 *= DEG_TO_RAD;

	    break;

	case POLAR_STEREOGRAPHIC:
	    if (!ISNAN(pps->ps.slon))
		pps->ps.slon *= DEG_TO_RAD;
	    if (!ISNAN(pps->ps.slat))
		pps->ps.slat *= DEG_TO_RAD;

	    break;

	case ALBERS_EQUAL_AREA:
	    if (!ISNAN(pps->albers.center_meridian))
		pps->albers.center_meridian *= DEG_TO_RAD;
	    if (!ISNAN(pps->albers.orig_latitude))
		pps->albers.orig_latitude *= DEG_TO_RAD;
	    if (!ISNAN(pps->albers.std_parallel1))
		pps->albers.std_parallel1 *= DEG_TO_RAD;
	    if (!ISNAN(pps->albers.std_parallel2))
		pps->albers.std_parallel2 *= DEG_TO_RAD;

	    break;

	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    if (!ISNAN(pps->lamaz.center_lat))
		pps->lamaz.center_lat *= DEG_TO_RAD;
	    if (!ISNAN(pps->lamaz.center_lon))
		pps->lamaz.center_lon *= DEG_TO_RAD;

	    break;

	case LAMBERT_CONFORMAL_CONIC:
	    if (!ISNAN(pps->lamcc.plat1))
		pps->lamcc.plat1 *= DEG_TO_RAD;
	    if (!ISNAN(pps->lamcc.plat2))
		pps->lamcc.plat2 *= DEG_TO_RAD;
	    if (!ISNAN(pps->lamcc.lat0))
		pps->lamcc.lat0 *= DEG_TO_RAD;
	    if (!ISNAN(pps->lamcc.lon0))
		pps->lamcc.lon0 *= DEG_TO_RAD;

	    break;

	default:
	    asfPrintError("to_radians: illegal projection type!");
    }
}
