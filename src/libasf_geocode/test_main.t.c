#include "CUnit/Basic.h"

void test_geoid(void);
void test_geoid_adjust(void);

int main(void)
{
   CU_pSuite pSuite = NULL;

   /* initialize the CUnit test registry */
   if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

   /* add a suite to the registry */
   pSuite = CU_add_suite("libasf_geocode suite", NULL, NULL);
   if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* add the tests to the suite */
   if ((NULL == CU_add_test(pSuite, "geoid", test_geoid)) ||
       //(NULL == CU_add_test(pSuite, "strUtil", test_strUtil)) ||
       (NULL == CU_add_test(pSuite, "geoid_adjust", test_geoid_adjust)))
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
