// Implementation of interface described in projected_image_import.h.

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include "asf.h"
#include "projected_image_import.h"

double
utm_zone_to_central_meridian (int zone)
{
  return -177.0 + (zone - 1) * 6.0;
}

void
utm_zone_and_hemisphere_from_projected_coordinate_system_type_geotiff_geokey
  (short cs_type, char *hemisphere, int *zone)
{
  /* Here we use some funky arithmetic to get the correct geotiff
     coordinate system type key from our zone code.  Here are a few
     assertions to try to ensure that the convention used for the
     libgeotiff constants is as expected.  Also note that we have
     already verified that we are on a WGS84 ellipsoid.  */
  asfRequire (PCS_WGS84_UTM_zone_60N - PCS_WGS84_UTM_zone_1N == 59,
	      "Geotiff UTM projected coordinate system type constants don't "
	      "obbey expected convention.\n");
  asfRequire (PCS_WGS84_UTM_zone_60S - PCS_WGS84_UTM_zone_1S == 59,
	      "Geotiff UTM projected coordinate system type constants don't "
	      "obbey expected convention.\n");

  if ( cs_type - PCS_WGS84_UTM_zone_1N <= 59 ) {
    *hemisphere = 'N';
    *zone = cs_type - PCS_WGS84_UTM_zone_1N + 1;
  }
  else {
    int code_diff = cs_type - PCS_WGS84_UTM_zone_1S;
    if ( code_diff >= 0 && code_diff <= 59 ) {
      *hemisphere = 'S';
      *zone = code_diff + 1;
    }
    else {
      asfPrintError ("Invalid ProjectedCSTypeGeoKey for GeoTIFF flavor.");
    }
  }
}
