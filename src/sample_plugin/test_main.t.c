#include "CUnit/Basic.h"

void test_sample_plugin1();
void test_sample_plugin2();

int main()
{
   CU_pSuite pSuite = NULL;

   /* initialize the CUnit test registry */
   if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

   /* add a suite to the registry */
   pSuite = CU_add_suite("sample_plugin suite", NULL, NULL);
   if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* add the tests to the suite */
   if ((NULL == CU_add_test(pSuite, "test_sample_plugin1", test_sample_plugin1)) ||
       (NULL == CU_add_test(pSuite, "test_sample_plugin2", test_sample_plugin2)))
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
