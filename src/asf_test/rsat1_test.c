#include <asf.h>
#include <asf_meta.h>
#include "CUnit/Automated.h"
#include "CUnit/Basic.h"

void test_rsat1_scansar_geotiff_alaska_albers(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_albers.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_albers_equal_area_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_eqc(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_eqc.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_equidistant.specs"));
}

void test_rsat1_scansar_geotiff_alaska_eqr(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_eqr.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_equirectangular_world.specs"));
}

void test_rsat1_scansar_geotiff_alaska_lamaz(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_lamaz.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_lambert_azimuthal_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_lamcc(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_lamcc.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_lambert_conformal_conic_north_america.specs"));
}

void test_rsat1_scansar_geotiff_alaska_mer(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_mer.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_mercator_world.specs"));
}

void test_rsat1_scansar_geotiff_alaska_ps(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_ps_alaska.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_ps_north_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_ps_ssmi(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_ps_ssmi.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_ps_north_ssmi.specs"));
}

void test_rsat1_scansar_geotiff_alaska_sin(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_sin.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_sinusoidal_modis.specs"));
}

void test_rsat1_scansar_geotiff_alaska_utm(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1_scansar/geotiff_alaska/R142020282G3S001_utm.tif",
			      "rsat1_scansar/geotiff_alaska/geotiff_universal_transverse_mercator_zone3.specs"));
}

int add_rsat1_scansar_geotiff_alaska_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("RSAT1 ScanSAR GeoTIFF Alaska tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
    
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Albers", 
			   test_rsat1_scansar_geotiff_alaska_albers)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Equidistant", 
			   test_rsat1_scansar_geotiff_alaska_eqc)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Equirectangular", 
			   test_rsat1_scansar_geotiff_alaska_eqr)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Lambert Azimuthal", 
			   test_rsat1_scansar_geotiff_alaska_lamaz)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Lambert Conic", 
			   test_rsat1_scansar_geotiff_alaska_lamcc)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Mercator", 
			   test_rsat1_scansar_geotiff_alaska_mer)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF PS Alaska", 
			   test_rsat1_scansar_geotiff_alaska_ps)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF PS SSMI", 
			   test_rsat1_scansar_geotiff_alaska_ps_ssmi)) ||
      //(NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Sinusoidal", 
      //		   test_rsat1_scansar_geotiff_alaska_sin)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF UTM", 
			   test_rsat1_scansar_geotiff_alaska_utm))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
