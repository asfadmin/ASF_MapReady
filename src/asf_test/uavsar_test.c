#include <asf.h>
#include <asf_meta.h>
#include "CUnit/Automated.h"
#include "CUnit/Basic.h"

void test_uavsar_polsar_mlc_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_mlc.meta", 
			   "uavsar/metadata/uavsar_polsar_slant_range_meta.specs"));
}

void test_uavsar_polsar_stokes_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_dat.meta", 
			   "uavsar/metadata/uavsar_polsar_slant_range_meta.specs"));
}

void test_uavsar_polsar_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_grd.meta", 
			   "uavsar/metadata/uavsar_polsar_ground_range_meta.specs"));
}

void test_uavsar_polsar_dem_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_hgt.meta", 
			   "uavsar/metadata/uavsar_polsar_ground_range_meta.specs"));
}

void test_uavsar_insar_amp_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_amp.meta", 
			   "uavsar/metadata/uavsar_insar_slant_range_meta.specs"));
}

void test_uavsar_insar_int_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_int.meta", 
			   "uavsar/metadata/uavsar_insar_slant_range_meta.specs"));
}

void test_uavsar_insar_unw_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_unw.meta", 
			   "uavsar/metadata/uavsar_insar_slant_range_meta.specs"));
}

void test_uavsar_insar_cor_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_cor.meta", 
			   "uavsar/metadata/uavsar_insar_slant_range_meta.specs"));
}

void test_uavsar_insar_amp_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_amp_grd.meta", 
			   "uavsar/metadata/uavsar_insar_ground_range_meta.specs"));
}

void test_uavsar_insar_int_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_int_grd.meta", 
			   "uavsar/metadata/uavsar_insar_ground_range_meta.specs"));
}

void test_uavsar_insar_unw_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_unw_grd.meta", 
			   "uavsar/metadata/uavsar_insar_ground_range_meta.specs"));
}

void test_uavsar_insar_cor_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_cor_grd.meta", 
			   "uavsar/metadata/uavsar_insar_ground_range_meta.specs"));
}

void test_uavsar_insar_hgt_grd_metadata(void)
{
  CU_ASSERT_TRUE(meta_test("uavsar/metadata/yellowstone_hgt_grd.meta", 
			   "uavsar/metadata/uavsar_insar_ground_range_meta.specs"));
}

int add_uavsar_metadata_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("UAVSAR metadata tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "UAVSAR PolSAR multilook", 
			   test_uavsar_polsar_mlc_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR PolSAR Stokes", 
			   test_uavsar_polsar_stokes_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR PolSAR ground range", 
			   test_uavsar_polsar_grd_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR PolSAR digital elevation model", 
			   test_uavsar_polsar_dem_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR amplitudes", 
			   test_uavsar_insar_amp_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR interferogram", 
			   test_uavsar_insar_int_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR unwrapped phase", 
			   test_uavsar_insar_unw_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR correlation", 
			   test_uavsar_insar_cor_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR ground range amplitudes", 
			   test_uavsar_insar_amp_grd_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR ground range interferogram", 
			   test_uavsar_insar_int_grd_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR ground range unwrapped phase",
			   test_uavsar_insar_unw_grd_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR ground range correlation", 
			   test_uavsar_insar_cor_grd_metadata)) ||
      (NULL == CU_add_test(pSuite, "UAVSAR InSAR ground range DEM", 
			   test_uavsar_insar_hgt_grd_metadata))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
