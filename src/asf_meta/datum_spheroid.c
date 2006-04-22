#include <asf_meta.h>

/* Return the spheroid associated with a given datum.  This function
   fails if given a datum it hasn't been taught about yet.  */
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
  default:
    assert (0);
    break;
  }
}
