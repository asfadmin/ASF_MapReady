#include "asf_test.h"

void test_alos_palsar_browse_binary(void)
{
  CU_ASSERT_FALSE(brs2jpg("alos/browse/EALPSRP112490880.BRS",
			  "alos/browse/workreport",
			  "alos/browse/alos_browse_test.jpg"));
  cu_diffimage("alos/browse/alos_browse_test.jpg", 
	       "alos/browse/ALPSRP112490880.jpg");
}

void test_alos_palsar_browse_metadata(void)
{
  // Extract dimensions from JPEG
  jpeg_info_t jpg;
  get_jpeg_info_hdr_from_file("alos/browse/alos_browse_test.jpg", &jpg, NULL);

  // Dimensions are supposed to be multiple of 100
  CU_ASSERT(jpg.width % 100 == 0);
  CU_ASSERT(jpg.height % 100 == 0);
}

int add_alos_browse_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("ALOS Palsar browse image tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "ALOS Palsar browse generation",
			   test_alos_palsar_browse_binary)) ||
      (NULL == CU_add_test(pSuite, "ALOS Palsar browse metadata", 
			   test_alos_palsar_browse_metadata))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
