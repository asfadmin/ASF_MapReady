/* Unit tests for the asf_meta library.  These use the 'check' unit
   testing framework for C.  Not complete by any means, but hopefully
   new code will use them at least.  */

/* Note that these tests have wired in the values from the test meta
   file.  */

/* System headers.  */
#include <stdlib.h>
#include <unistd.h>

/* ASF headers.  */
#include <asf.h>
#include <asf_meta.h>

#include <check.h>		/* For unit check functions.  */

/* Over-safe micron for test floating point comparisons.  */
#define UNIT_TESTS_MICRON 0.00001
#define UNIT_TESTS_FLOAT_COMPARE(a, b) (abs(a - b) < UNIT_TESTS_MICRON ? 1 : 0)

/* Test the part of meta_read that parses new files.  */
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
  meta_parameters *meta = meta_read("test_file_new_style.meta");
  meta_parameters *meta_reread;	/* For re-read structure.  */
  
  meta_write(meta, "test_output_file.meta");
  meta_reread = meta_read("test_output_file.meta");
  
  /* Test a few random or not-so-random fields for equality.  */
  fail_unless(!strcmp(meta->general->sensor, meta_reread->general->sensor),
	      "sensor elements from original and written-then-reread metadata structures don't match");
  fail_unless(meta->projection->type == meta_reread->projection->type,
	      "projection->type fields from original and written-then-reread metadata structures not equal");
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,
				       meta_reread->projection
				                    ->param.atct.rlocal),
	      "projection->param.atct.rlocal fields from original and written-then-reread metadata structures not equal");

  if ( unlink("test_output_file.meta") == -1 ) {
    perror("failed to delete intra-test temporary file test_output_file.meta");
    fail("");
  }
}
END_TEST

/* Test part of ceos_init and aisp_init from meta_create.  */
START_TEST(test_meta_create_new_format)
{
  char err_msg[256];
  meta_parameters *meta;
  
  meta = meta_create("./CEOS/E12259029000X008");
  
  /* Check random fields to make sure things are working.  */
  sprintf(err_msg,"meta->general->orbit = %d; not read correctly, should be 22590",meta->general->orbit);
   fail_unless(meta->general->orbit == 22590,err_msg);
  sprintf(err_msg,"meta->general->orbit_direction = '%c'; not read correctly, should be 'D'",meta->general->orbit_direction);
   fail_unless(meta->general->orbit_direction == 'D',err_msg);
  sprintf(err_msg,"meta->sar->prf = %lf; not read correctly, should be 1679.9023438",meta->sar->prf);
   fail_unless(meta->sar->prf == 1679.9023438,err_msg);

  /* Check a not-so-random field: things from projection params block
     are currently partly holdover from deprecated code and use a
     union.  */
/*  fail_unless((meta->projection->type=='A') && (meta->projection->param.atct.alpha1==0.6),
	      "alpha1 field from param->atct block not read correctly");
  */
  /* Another not-so-random field check: state vector blocks currently
     use wierd field names in the data file and map strangely into a
     dynamicly allocated internal structure, lots of possibility for
     error.  */
/*  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->state_vectors->vecs[1].vec.pos.y, 22333),
	      "Y position element of second state vector not read correctly");
*/
  meta_write(meta, "test_output_of_meta_create.meta");
  meta_free(meta);
}
END_TEST

/* Machinery for running the 'check' tests.  */
Suite *asf_meta_suite(void)
{
  Suite *s = suite_create("asf_meta");
  TCase *tc_core = tcase_create("Core");
  
  suite_add_tcase(s, tc_core);
  
  tcase_add_test(tc_core, test_meta_read_new_format);
  tcase_add_test(tc_core, test_meta_read_write_new_format);
  tcase_add_test(tc_core, test_meta_create_new_format);

  return s;
}

int main(void)
{
  int nf;
  Suite *s = asf_meta_suite();
  SRunner *sr = srunner_create(s);
 
  srunner_run_all(sr, CK_VERBOSE);

  nf = srunner_ntests_failed(sr);

  srunner_free(sr);
  suite_free(s);

  return ( nf == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
