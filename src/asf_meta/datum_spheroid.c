#include <assert.h>

#include <asf_meta.h>

/* Return the spheroid generally associated with a given datum.
   Unfortunately, in the larger world, a given datum isn't really
   always necessarily associated with a particular spheroid.  For the
   purposes of the data on the insdie of the ASF tools world, however,
   we consider the correspondence to be as described in this function.
   So be sure to test on import if this correspondence is really
   true!  

   This function fails if given a datum it hasn't been taught about
   yet.  */
spheroid_type_t
datum_spheroid (datum_type_t datum)
{
  switch ( datum ) {

  case NAD27_DATUM:
    return CLARKE1866_SPHEROID;

  case NAD83_DATUM:
    return GRS1980_SPHEROID;

  case WGS84_DATUM:
    return WGS84_SPHEROID;

  default:
    assert (0);
    return WGS84_SPHEROID;
  }
}

/* Returns a string identifying the given datum */
const char *datum_toString(datum_type_t datum)
{
  switch (datum) {
      /* Earth Gravity Model 1996 (spheroid: WGS84) */
      case EGM96_DATUM:   
          return "EGM96";

      /* European Datum 1950 (International 1924) */
      case ED50_DATUM:    
          return "ED50";

      /* European Terrestrial Reference Frame 1989 (WGS84) */
      case ETRF89_DATUM:  
          return "ETRF89";

      /* European Terrestrial Reference System 1989 (GRS 1980) */
      case ETRS89_DATUM:  
          return "ETRS89";

      /* International Terrestrial Reference Frame (GRS 1980) */
      case ITRF97_DATUM:    
          return "ITRF97";

      /* North American Datum 1927 (Clarke 1866) */
      case NAD27_DATUM:   
          return "NAD27";

      /* North American Datum 1983 (GRS 1980) */
      case NAD83_DATUM:   
          return "NAD83";

      /* World Geodetic System 1972 (WGS72) */
      case WGS72_DATUM:   
          return "WGS72";

      /* World Geodetic System 1984 (WGS84) */
      case WGS84_DATUM:    
          return "WGS84";

      case UNKNOWN_DATUM:
          return "UNKNOWN";

      default:
          return MAGIC_UNSET_STRING;
  }
}

/* Returns a string identifying the given spheroid */
const char *spheroid_toString(spheroid_type_t spheroid)
{
  switch (spheroid) {
      case BESSEL_SPHEROID:
          return "BESSEL";

      case CLARKE1866_SPHEROID:
          return "CLARKE1866";

      case CLARKE1880_SPHEROID:
          return "CLARKE1880";

      case GEM6_SPHEROID:
          return "GEM6";

      case GEM10C_SPHEROID:
          return "GEM10C";

      case GRS1980_SPHEROID:
          return "GRS1980";

      case INTERNATIONAL1924_SPHEROID:
          return "INTERNATIONAL1924";

      case INTERNATIONAL1967_SPHEROID:
          return "INTERNATIONAL1967";

      case WGS72_SPHEROID:
          return "WGS72";

      case WGS84_SPHEROID:
          return "WGS84";

      default:
          return MAGIC_UNSET_STRING;
  }
}
