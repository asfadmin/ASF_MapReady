/* Unit tests for the asf_meta library.  These use the 'check' unit
   testing framework for C.  Not complete by any means, but hopefully
   new code will use them at least.  */

/* Note that these tests have wired in the values from the test meta
   file.  */

#include <asf_meta.h>
#include <check.h>		/* For unit check functions.  */

/* Over-safe micron for test floating point comparisons.  */
#define UNIT_TESTS_MICRON 0.00001
#define UNIT_TESTS_FLOAT_COMPARE(a, b) (abs(a - b) < UNIT_TESTS_MICRON ? 1 : 0)

/* Test part of meta_read that parses new files.  */
START_TEST(test_meta_read_new_format)
{
  meta_parameters *meta = meta_read("test_file_new_style.meta");
  
  /* Check a random fields to make sure things are working.  */
  fail_unless(meta->general->orbit == 123, 
	      "orbit field from general block not read correctly");
  
  /* Check a not-so-random field: things from projection params block
     are currently partly holdover from deprecated code and use a
     union.  */
  fail_unless(meta->projection->type == 'A' 
	      && meta->projection->param.atct.alpha1 == 0.6,
	      "alpha1 field from param->atct block not read correctly");
  
  /* Another not-so-random field check: state vector blocks currently
     use wierd field names in the data file and map strangely into a
     dynamicly allocated internal structure, lots of possibility for
     error.  */
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->state_vectors
				             ->vecs[1].vec.pos.y, 
				       22333),
	      "Y position element of second state vector not read correctly");

  meta_free(meta);
}
END_TEST

/* Test routine which writes new-style meta files.  */
START_TEST(test_meta_read_write_new_format)
{
  /* Get something to write.  */
  meta_parameters *meta_in = meta_read("test_file_new_style.meta");
  meta_parameters *meta_reread;	/* For re-read structure.  */
  
  meta_write(meta, "test_output_file.meta");
  meta_read(meta_reread, "test_output_file.meta");
  
  /* Test a few random or not-so-random fields for equality.  */
  fail_unless(!strcmp(meta->general->sensor, meta_reread->general->sensor),
	      "sensor elements from original and written-then-reread metadata structures don't match");
  fail_unless(meta->projection->type == meta_reread->projection->type,
	      "projection->type fields from original and written-then-reread metadata structures not equal");
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,
				       meta_reread->projection
				                    ->param.atct.rlocal));
}
ENT_TEST

/* Machinery for running the 'check' tests.  */
Suite *asf_meta_suite(void)
{
  Suite *s = suite_create("asf_meta");
  TCase *tc_core = tcase_create("Core");
  
  suite_add_tcase(s, tc_core);
  
  tcase_add_test(tc_core, test_meta_read_new_format);

  return s;
}

int main(void)
{
  int nf;
  Suite *s = asf_meta_suite();
  SRunner *sr = srunner_create(s);
 
  srunner_run_all(sr, CK_NORMAL);

  nf = srunner_ntests_failed(sr);

  srunner_free(sr);
  suite_free(s);

  return ( nf == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
