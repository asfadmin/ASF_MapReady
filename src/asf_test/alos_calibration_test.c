#include "asf_test.h"

void test_get_cal_dn_alos_sigma_db_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_db_regular.specs"));
}

void test_get_cal_dn_alos_sigma_db_noiselevel(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_db_noiselevel.specs"));
}

void test_get_cal_dn_alos_sigma_db_nodata(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_db_nodata.specs"));
}

void test_get_cal_dn_alos_sigma_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_regular.specs"));
}

void test_get_cal_dn_alos_sigma_noiselevel(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_noiselevel.specs"));
}

void test_get_cal_dn_alos_sigma_nodata(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_sigma_nodata.specs"));
}

void test_get_cal_dn_alos_gamma_db_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_gamma_db_regular.specs"));
}

void test_get_cal_dn_alos_gamma_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_gamma_regular.specs"));
}

void test_get_cal_dn_alos_beta_db_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_beta_db_regular.specs"));
}

void test_get_cal_dn_alos_beta_regular(void)
{
  CU_ASSERT_TRUE(lib_test("get_cal_dn", "libraries/get_cal_dn",
			  "get_cal_dn_alos_beta_regular.specs"));
}

int add_alos_calibration_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("ALOS calibration library functions tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "ALOS calibrated value sigma dB regular", 
			   test_get_cal_dn_alos_sigma_db_regular)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value sigma dB noise level", 
			   test_get_cal_dn_alos_sigma_db_noiselevel)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value sigma dB nodata", 
			   test_get_cal_dn_alos_sigma_db_nodata)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value sigma regular", 
			   test_get_cal_dn_alos_sigma_regular)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value sigma noise level", 
			   test_get_cal_dn_alos_sigma_noiselevel)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value sigma nodata", 
			   test_get_cal_dn_alos_sigma_nodata)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value gamma dB regular", 
			   test_get_cal_dn_alos_gamma_db_regular)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value gamma regular", 
			   test_get_cal_dn_alos_gamma_regular)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value beta dB regular", 
			   test_get_cal_dn_alos_beta_db_regular)) ||
      (NULL == CU_add_test(pSuite, "ALOS calibrated value beta regular", 
			   test_get_cal_dn_alos_beta_regular))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
