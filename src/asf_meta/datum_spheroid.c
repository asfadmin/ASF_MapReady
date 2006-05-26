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
    break;
  case NAD83_DATUM:
    return GRS1980_SPHEROID;
    break;
  case WGS84_DATUM:
    return WGS84_SPHEROID;
    break;
  default:
    assert (0);
    break;
  }
}
