#include "asf_test.h"

void test_alos_palsar_leader2text(void)
{
  CU_ASSERT_TRUE(output_all_metadata("alos/leader/LED-ALPSRP112490880-H1.5_UA",
				      "alos/leader/leader_test.txt"));
}

void test_alos_palsar_leader_compare(void)
{
  CU_ASSERT_FALSE(asfSystem("diff alos/leader/ALPSRP112490880.LED.txt alos/leader/leader_test.txt"));
}

int add_alos_leader_tests(void)
{
  CU_pSuite pSuite = NULL;
  
  // Add suite to the registry
  pSuite = CU_add_suite("ALOS Palsar leader file tests", NULL, NULL);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  
  // Add tests to the suite
  if ((NULL == CU_add_test(pSuite, "Generation text version of leader file",
			   test_alos_palsar_leader2text)) ||
      (NULL == CU_add_test(pSuite, 
			   "Comparison of leader test and leader reference", 
			   test_alos_palsar_leader_compare))) {
    CU_cleanup_registry();
    return CU_get_error();
  }
  return TRUE;
}
