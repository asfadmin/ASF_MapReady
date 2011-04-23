#include "asf_test.h"

// GeoTIFF map projection test suite
void test_rsat1_scansar_geotiff_alaska_albers(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_albers.tif",
			      "rsat1/map_projections/geotiff_albers_equal_area_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_eqc(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_eqc.tif",
			      "rsat1/map_projections/geotiff_equidistant.specs"));
}

void test_rsat1_scansar_geotiff_alaska_eqr(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_eqr.tif",
			      "rsat1/map_projections/geotiff_equirectangular_world.specs"));
}

void test_rsat1_scansar_geotiff_alaska_lamaz(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_lamaz.tif",
			      "rsat1/map_projections/geotiff_lambert_azimuthal_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_lamcc(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_lamcc.tif",
			      "rsat1/map_projections/geotiff_lambert_conformal_conic_north_america.specs"));
}

void test_rsat1_scansar_geotiff_alaska_mer(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_mer.tif",
			      "rsat1/map_projections/geotiff_mercator_world.specs"));
}

void test_rsat1_scansar_geotiff_alaska_ps(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_ps_alaska.tif",
			      "rsat1/map_projections/geotiff_ps_north_alaska.specs"));
}

void test_rsat1_scansar_geotiff_alaska_ps_ssmi(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_ps_ssmi.tif",
			      "rsat1/map_projections/geotiff_ps_north_ssmi.specs"));
}

void test_rsat1_scansar_geotiff_alaska_sin(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_sin.tif",
			      "rsat1/map_projections/geotiff_sinusoidal_modis.specs"));
}

void test_rsat1_scansar_geotiff_alaska_utm(void)
{
  CU_ASSERT_TRUE(geotiff_test("rsat1/map_projections/R142020282G3S001_utm.tif",
			      "rsat1/map_projections/geotiff_universal_transverse_mercator_zone3.specs"));
}

int add_rsat1_map_projections_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("RSAT1 map projection tests", NULL, NULL);
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
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF Sinusoidal", 
			   test_rsat1_scansar_geotiff_alaska_sin)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 ScanSAR GeoTIFF UTM", 
			   test_rsat1_scansar_geotiff_alaska_utm))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}

// End-to_end GeoTIFF test suite
void test_end2end_rsat1_geotiff_ps(void)
{
  CU_ASSERT_FALSE(asf_convert(FALSE, "end2end/geotiff/geotiff_ps_test.cfg"));
  CU_ASSERT_TRUE(geotiff_test("end2end/geotiff/R163006179P1S001_ps.tif",
			      "end2end/geotiff/geotiff_ps_north_alaska.specs"));
  cu_diffimage("end2end/geotiff/R163006179P1S001.tif",
	       "end2end/geotiff/R163006179P1S001_ps.tif");
}

void test_end2end_rsat1_geotiff_utm(void)
{
  CU_ASSERT_FALSE(asf_convert(FALSE, "end2end/geotiff/geotiff_utm_test.cfg"));
  CU_ASSERT_TRUE(geotiff_test("end2end/geotiff/R163749163U1S001_utm.tif",
			      "end2end/geotiff/geotiff_universal_transverse_mercator.specs"));
  cu_diffimage("end2end/geotiff/R163749163U1S001.tif",
	       "end2end/geotiff/R163749163U1S001_utm.tif");
}

int add_rsat1_geotiff_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("RSAT1 GeoTIFF tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
    
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "RSAT1 GeoTIFF PS", 
			   test_end2end_rsat1_geotiff_ps)) ||
      (NULL == CU_add_test(pSuite, "RSAT1 GeoTIFF UTM", 
			   test_end2end_rsat1_geotiff_utm))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}

// Overlay test suite
void test_rsat1_overlay(void)
{
  c2v_config *cfg = read_c2v_config("rsat1/overlay/rsat1_overlay.cfg");
  CU_ASSERT_TRUE(convert2vector(cfg));
  FREE(cfg);
  cu_difftext("rsat1/overlay/test_overlay.kml", 
	      "rsat1/overlay/R163749163U1S001.kml",
	      "rsat1/overlay/test_except.lst");
}

int add_rsat1_overlay_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("RSAT1 overlay tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "KML overlay generation",
			   test_rsat1_overlay))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
