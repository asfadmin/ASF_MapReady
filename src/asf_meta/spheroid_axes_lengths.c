#include <assert.h>

#include <asf_meta.h>
#include <spheroids.h>

// Fill in major and minor with the axes lenghts of spheroid.
void
spheroid_axes_lengths (spheroid_type_t spheroid, double *major, double *minor)
{
  switch ( spheroid ) {
  case BESSEL_SPHEROID:
    *major = BESSEL_SEMIMAJOR;
    *minor = *major - (1.0 / BESSEL_INV_FLATTENING) * *major;
    break;
  case CLARKE1866_SPHEROID:
    *major = CLARKE1866_SEMIMAJOR;
    *minor = *major - (1.0 / CLARKE1866_INV_FLATTENING) * *major;
    break;
  case CLARKE1880_SPHEROID:
    *major = CLARKE1880_SEMIMAJOR;
    *minor = *major - (1.0 / CLARKE1880_INV_FLATTENING) * *major;
    break;
  case GEM6_SPHEROID:
    *major = GEM6_SEMIMAJOR;
    *minor = *major - (1.0 / GEM6_INV_FLATTENING) * *major;
    break;
  case GEM10C_SPHEROID:
    *major = GEM10C_SEMIMAJOR;
    *minor = *major - (1.0 / GEM10C_INV_FLATTENING) * *major;
    break;
  case GRS1980_SPHEROID:
    *major = GRS1980_SEMIMAJOR;
    *minor = *major - (1.0 / GRS1980_INV_FLATTENING) * *major;
    break;
  case INTERNATIONAL1924_SPHEROID:
    *major = INTERNATIONAL1924_SEMIMAJOR;
    *minor = *major - (1.0 / INTERNATIONAL1924_INV_FLATTENING) * *major;
    break;
  case INTERNATIONAL1967_SPHEROID:
    *major = INTERNATIONAL1967_SEMIMAJOR;
    *minor = *major - (1.0 / INTERNATIONAL1967_INV_FLATTENING) * *major;
    break;
  case WGS72_SPHEROID:
    *major = WGS72_SEMIMAJOR;
    *minor = *major - (1.0 / WGS72_INV_FLATTENING) * *major;
    break;
  case WGS84_SPHEROID:
    *major = WGS84_SEMIMAJOR;
    *minor = *major - (1.0 / WGS84_INV_FLATTENING) * *major;
    break;
  default:
    assert (0);
    break;
  }
}
