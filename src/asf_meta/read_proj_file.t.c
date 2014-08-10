#include "CUnit/Basic.h"
#include "asf_meta.h"

static int cmptol(double d1, double d2, double tol)
{
  return FLOAT_COMPARE_TOLERANCE(d1,d2,tol);
}

void test_read_proj_file()
{
  project_parameters_t pps;
  projection_type_t proj_type;
  datum_type_t datum;
  spheroid_type_t spheroid;

  read_proj_file("utm.proj", &pps, &proj_type, &datum, &spheroid);
  CU_ASSERT(proj_type == UNIVERSAL_TRANSVERSE_MERCATOR);
  //CU_ASSERT(datum == WGS84_DATUM);
  //CU_ASSERT(spheroid == WGS84_SPHEROID);

  read_proj_file("albers_equal_area_conic_alaska", &pps, &proj_type, &datum, &spheroid);
  CU_ASSERT(proj_type == ALBERS_EQUAL_AREA);
  CU_ASSERT(datum == NAD83_DATUM);
  CU_ASSERT(spheroid == GRS1980_SPHEROID);
  CU_ASSERT(cmptol(pps.albers.std_parallel1,55.0,.0001));
  CU_ASSERT(cmptol(pps.albers.std_parallel2,65.0,.0001));
  CU_ASSERT(cmptol(pps.albers.center_meridian,-154,.0001));
  CU_ASSERT(cmptol(pps.albers.orig_latitude,50,.0001));
}

