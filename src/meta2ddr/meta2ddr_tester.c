/****************************************************
 Unit tests for the meta2ddr & ddr2meta.

 These use the 'check' unit testing framework for C.
 This is more of a single system test at this point,
 but hopefully new code will add new tests.
 
 TODO: When meta library can handle it, test for
  * empty fields when reading the new meta (ie empty
  * satellite_binary_time)
****************************************************/

/* System headers.  */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <check.h>		/* For unit check functions.  */

/* Over-safe micron for test floating point comparisons.  */
#define UNIT_TESTS_MICRON 0.00001
#define UNIT_TESTS_FLOAT_COMPARE(a, b) (abs(a - b) < UNIT_TESTS_MICRON ? 1 : 0)

/**********************************************
 * Test making las -> meta.                  */
START_TEST(test_ddr2meta)
{
  char cmd[256];
  printf("\n");

  sprintf(cmd,"ddr2meta %s/old_style %s/ddr2meta_out",
          _TEST_DATA_DIR,_TEST_DATA_DIR);
  printf("%s\n",cmd);  
  fail_unless( !system(cmd), "test_ddr2meta: meta2ddr returned non-zero exit code.");

  sprintf(cmd,"diff %s/ddr2meta_out.meta %s/ddr2meta_out.meta > %s/diff_ddr2meta.txt",
          _TEST_DATA_DIR,_TEST_DATA_DIR,_TEST_DATA_DIR);
  fail_unless( !system(cmd), "test_ddr2meta: diff between out and good .meta file.");
}
END_TEST

/**********************************************
 * Test making meta -> las.                  */
START_TEST(test_meta2ddr)
{
  char cmd[256];
  printf("\n");

  sprintf(cmd,"meta2ddr %s/new_style %s/meta2ddr_out",
          _TEST_DATA_DIR,_TEST_DATA_DIR);
  printf("%s\n",cmd);  
  fail_unless( !system(cmd), "test_meta2ddr: meta2ddr returned non-zero exit code.");

  sprintf(cmd,"diff %s/meta2ddr_out.meta %s/meta2ddr_good.meta > %s/diff_meta2ddr_meta.txt",
          _TEST_DATA_DIR,_TEST_DATA_DIR,_TEST_DATA_DIR);
  fail_unless( !system(cmd), "test_meta2ddr: diff between out and good .meta file");

  sprintf(cmd,"dspddr %s/meta2ddr_out.ddr > %s/meta2ddr_out.txt",_TEST_DATA_DIR,_TEST_DATA_DIR);
  system(cmd);
  sprintf(cmd,"dspddr %s/meta2ddr_good.ddr > %s/meta2ddr_good.txt",_TEST_DATA_DIR,_TEST_DATA_DIR);
  system(cmd);
  sprintf(cmd,"diff %s/meta2ddr_out.txt %s/meta2ddr_good.txt > %s/diff_meta2ddr_ddr.txt",
          _TEST_DATA_DIR,_TEST_DATA_DIR,_TEST_DATA_DIR);
  printf("%s\n",cmd);  
  fail_unless( !system(cmd), "test_meta2ddr: diff between out and good .ddr file");
}
END_TEST

/**********************************************
 * Machinery for running the 'check' tests.  */
Suite *meta2ddr_suite(void)
{
  Suite *suite = suite_create("meta2ddr\n");
  TCase *tc_core = tcase_create("Core");
  
  suite_add_tcase(suite, tc_core);
 
  tcase_add_test(tc_core, test_ddr2meta);
  tcase_add_test(tc_core, test_meta2ddr);

  return suite;
}

/**********************************************/
int main(void)
{
  int failure_count;
  Suite *suite = meta2ddr_suite();
  SRunner *suite_runner = srunner_create(suite);
 
  srunner_run_all(suite_runner, CK_VERBOSE);

  failure_count = srunner_ntests_failed(suite_runner);

  srunner_free(suite_runner);
  suite_free(suite);

  return ( failure_count == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
