#include "CUnit/Basic.h"

void test_vector();
void test_strUtil();
void test_complex();
void test_solve1d();

int main()
{
   CU_pSuite pSuite = NULL;

   /* initialize the CUnit test registry */
   if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

   /* add a suite to the registry */
   pSuite = CU_add_suite("asf suite", NULL, NULL);
   if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* add the tests to the suite */
   if ((NULL == CU_add_test(pSuite, "vector", test_vector)) ||
       (NULL == CU_add_test(pSuite, "strUtil", test_strUtil)) ||
       (NULL == CU_add_test(pSuite, "solve1d", test_solve1d)) ||
       (NULL == CU_add_test(pSuite, "complex", test_complex)))
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
