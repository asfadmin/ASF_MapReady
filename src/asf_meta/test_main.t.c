#include "CUnit/Basic.h"

void test_xml();
void test_meta_get_latLon();
void test_meta_get_lineSamp();

int main()
{
   CU_pSuite pSuite = NULL;

   /* initialize the CUnit test registry */
   if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

   /* add a suite to the registry */
   pSuite = CU_add_suite("asf_meta suite", NULL, NULL);
   if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* add the tests to the suite */
   if ((NULL == CU_add_test(pSuite, "xml", test_xml)) ||
       (NULL == CU_add_test(pSuite, "meta_get_latLon", test_meta_get_latLon)) ||
       (NULL == CU_add_test(pSuite, "meta_get_lineSamp", test_meta_get_lineSamp)))
   {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* Run all tests using the CUnit Basic interface */
   CU_basic_set_mode(CU_BRM_VERBOSE);
   CU_basic_run_tests();
   int nfail = CU_get_number_of_failures();
   CU_cleanup_registry();
   return nfail>0;
}
