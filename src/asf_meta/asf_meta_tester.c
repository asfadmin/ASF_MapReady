/********************************************************************
   Unit tests for the asf_meta library.  These use the 'check' unit
   testing framework for C.  Not complete by any means, but hopefully
   new code will use them at least.

* Note: these tests have wired in the values from the test meta file.
********************************************************************/

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

const char *_TEST_DATA_DIR = "test_input";

/* Test the part of meta_read that parses new files.  */
START_TEST(test_meta_read_new_format)
{
  char in_file[512];
  meta_parameters *meta;

  sprintf(in_file,"%s/test_file_new_style.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);

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
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->state_vectors->vecs[1].vec.pos.y,22333),
	      "Y position element of second state vector not read correctly");

  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,6000000.0),
	      "meta->projection->param.ps.slon is messed up");

  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,6000000.0),
	      "meta->geo->proj->param.ps.slon is messed up");

  meta_free(meta);
}
END_TEST

/********************************************************
 * Test all possible projections                       */
START_TEST(test_atct) /* along track cross track */
{
  char error_string[256];
  char in_file[512];
  meta_parameters *meta;
  sprintf(in_file,"%s/new_style_atct.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);
  meta_write(meta,"test_output/out_atct.meta");
/* look at a couple fields real quick */
  sprintf(error_string,"meta->sar->dopRangeQuad is %-16.11g, should be 0.0001246",meta->sar->range_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->range_doppler_coefficients[2],0.0001246),error_string);
  sprintf(error_string,"meta->sar->dopAzQuad is %-16.11g, should be 2e-07",meta->sar->azimuth_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->azimuth_doppler_coefficients[2],2e-07),error_string);

/* look at the good stuff */
  sprintf(error_string,"meta->projection->param.atct.rlocal is %-16.11g, should be 6000000.0",meta->projection->param.atct.rlocal);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,6000000.0),error_string);
  sprintf(error_string,"meta->geo->proj->param.atct.rlocal is %-16.11g, should be 6000000.0",meta->geo->proj->param.atct.rlocal);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.atct.rlocal,6000000.0),error_string);

  sprintf(error_string,"meta->projection->param.atct.alpha1 is %-16.11g, should be 0.6",meta->projection->param.atct.alpha1);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.alpha1,0.6),error_string);
  sprintf(error_string,"meta->geo->proj->param.atct.alpha1 is %-16.11g, should be 0.6",meta->geo->proj->param.atct.alpha1);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.atct.alpha1,0.6),error_string);

  sprintf(error_string,"meta->projection->param.atct.alpha2 is %-16.11g, should be -0.44",meta->projection->param.atct.alpha2);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.alpha2,-0.44),error_string);
  sprintf(error_string,"meta->geo->proj->param.atct.alpha2 is %-16.11g, should be -0.44",meta->geo->proj->param.atct.alpha2);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.atct.alpha2,-0.44),error_string);

  sprintf(error_string,"meta->projection->param.atct.alpha3 is %-16.11g, should be 0.6",meta->projection->param.atct.alpha3);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.alpha3,0.6),error_string);
  sprintf(error_string,"meta->geo->proj->param.atct.alpha3 is %-16.11g, should be 0.6",meta->geo->proj->param.atct.alpha3);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.atct.alpha3,0.6),error_string);

  meta_free(meta);
}
END_TEST

START_TEST(test_lamcc) /* Lambert conformal conic */
{
  char error_string[256];
  char in_file[512];
  meta_parameters *meta;
  sprintf(in_file,"%s/new_style_lamcc.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);
  meta_write(meta,"test_output/out_lamcc.meta");
/* look at a couple fields real quick */
  sprintf(error_string,"meta->sar->dopRangeQuad is %-16.11g, should be 0.0001246",meta->sar->range_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->range_doppler_coefficients[2],0.0001246),error_string);
  sprintf(error_string,"meta->sar->dopAzQuad is %-16.11g, should be 2e-07",meta->sar->azimuth_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->azimuth_doppler_coefficients[2],0.0000002),error_string);

/* look at the good stuff */
  sprintf(error_string,"meta->projection->param.lamcc.plat1 is %-16.11g, should be 73.123",meta->projection->param.lamcc.plat1);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.lamcc.plat1,73.123),error_string);
  sprintf(error_string,"meta->geo->proj->param.lamcc.plat1 is %-16.11g, should be 73.123",meta->geo->proj->param.lamcc.plat1);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.lamcc.plat1,73.123),error_string);

  sprintf(error_string,"meta->projection->param.lamcc.plat2 is %-16.11g, should be 71.123",meta->projection->param.lamcc.plat2);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.lamcc.plat2,71.123),error_string);
  sprintf(error_string,"meta->geo->proj->param.lamcc.plat2 is %-16.11g, should be 71.123",meta->geo->proj->param.lamcc.plat2);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.lamcc.plat2,71.123),error_string);

  sprintf(error_string,"meta->projection->param.lamcc.lat0 is %-16.11g, should be 72.123",meta->projection->param.lamcc.lat0);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.lamcc.lat0,72.123),error_string);
  sprintf(error_string,"meta->geo->proj->param.lamcc.lat0 is %-16.11g, should be 72.123",meta->geo->proj->param.lamcc.lat0);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.lamcc.lat0,72.123),error_string);
  
  sprintf(error_string,"meta->projection->param.lamcc.lon0 is %lf; should be -158.123",meta->projection->param.lamcc.lon0);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.lamcc.lon0,-158.123),error_string);
  sprintf(error_string,"meta->geo->proj->param.lamcc.lon0 is %lf; should be -158.123",meta->geo->proj->param.lamcc.lon0);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.lamcc.lon0,-158.123),error_string);
  meta_free(meta);
}
END_TEST

START_TEST(test_ps) /* Polar Stereo */
{
  char error_string[256];
  char in_file[512];
  meta_parameters *meta;
  sprintf(in_file,"%s/new_style_ps.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);
  meta_write(meta,"test_output/out_ps.meta");
/* look at a couple fields real quick */
  sprintf(error_string,"meta->sar->dopRangeQuad is %-16.11g, should be 0.0001246",meta->sar->range_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->range_doppler_coefficients[2],0.0001246),error_string);
  sprintf(error_string,"meta->sar->dopAzQuad is %-16.11g, should be 2e-07",meta->sar->azimuth_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->azimuth_doppler_coefficients[2],2e-07),error_string);

/* look at the good stuff */
  sprintf(error_string,"meta->projection->param.ps.slat is %-16.11g, should be 72.374152778",meta->projection->param.ps.slat);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.ps.slat,72.374152778),error_string);
  sprintf(error_string,"meta->geo->proj->param.ps.slat is %-16.11g, should be 72.374152778",meta->geo->proj->param.ps.slat);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.ps.slat,72.374152778),error_string);

  sprintf(error_string,"meta->projection->param.ps.slon is %-16.11g, should be -158.3591",meta->projection->param.ps.slon);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.ps.slon,-158.3591),error_string);
  sprintf(error_string,"meta->geo->proj->param.ps.slon is %-16.11g, should be -158.3591",meta->geo->proj->param.ps.slon);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.ps.slon,-158.3591),error_string);
  meta_free(meta);
}
END_TEST

START_TEST(test_utm) /* universal transverse mercator */
{
  char error_string[256];
  char in_file[512];
  meta_parameters *meta;
  sprintf(in_file,"%s/new_style_utm.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);
  meta_write(meta,"test_output/out_utm.meta");
/* look at a couple fields real quick */
  sprintf(error_string,"meta->sar->dopRangeQuad is %-16.11g, should be 0.0001246",meta->sar->range_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->range_doppler_coefficients[2],0.0001246),error_string);
  sprintf(error_string,"meta->sar->dopAzQuad is %-16.11g, should be 2e-07",meta->sar->azimuth_doppler_coefficients[2]);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->sar->azimuth_doppler_coefficients[2],2e-07),error_string);
/* look at the good stuff */
  sprintf(error_string,"meta->projection->param.utm.zone is %d, should be 6",meta->projection->param.utm.zone);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.utm.zone,6),error_string);
  sprintf(error_string,"meta->geo->proj->param.utm.zone is %d, should be 6",meta->geo->proj->param.utm.zone);
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->geo->proj->param.utm.zone,6),error_string);
  meta_free(meta);
}
END_TEST

/* Test the part of meta_read that parses old files.  */
START_TEST(test_meta_read_old_format)
{
  char in_file[512];
  meta_parameters *meta;
  sprintf(in_file,"%s/test_file_old_style.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);

  /* Check a random fields to make sure things are working.  */
  fail_unless(meta->sar->azimuth_time_per_pixel == 0.015099817313, "azPixTime field from geo block not read correctly");
  fail_unless(meta->general->line_count == 8262, "nl from ddr read incorrectly");
  
  /* Check a not-so-random field: things from projection params block
     are currently partly holdover from deprecated code and use a
     union.  */
  fail_unless(meta->projection->type == 'P' 
	      && meta->projection->param.ps.slon == -158.3591,
	      "ps_lon field from param->ps block not read correctly");
  
  /* Another not-so-random field check: state vector blocks currently
     use wierd field names in the data file and map strangely into a
     dynamicly allocated internal structure, lots of possibility for
     error.  */
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->state_vectors->vecs[1].vec.pos.y,-359150.94171),
	      "Y position element of second state vector not read correctly");

  meta_free(meta);
}
END_TEST

/* Test routine which writes new-style meta files.  */
START_TEST(test_meta_read_write_new_format)
{
  /* Get something to write.  */
  char in_file[512];
  meta_parameters *meta;
  meta_parameters *meta_reread;	/* For re-read structure.  */

  sprintf(in_file,"%s/test_file_new_style.meta",_TEST_DATA_DIR);
  meta = meta_read(in_file);
  meta_write(meta, "test_output/test_output_file.meta");
  meta_reread = meta_read("test_output/test_output_file.meta");
  
  /* Test a few random or not-so-random fields for equality.  */
  fail_unless(!strcmp(meta->general->sensor, meta_reread->general->sensor),
	      "sensor elements from original and written-then-reread metadata structures don't match");
  fail_unless(meta->projection->type == meta_reread->projection->type,
	      "projection->type fields from original and written-then-reread metadata structures not equal");
  fail_unless(UNIT_TESTS_FLOAT_COMPARE(meta->projection->param.atct.rlocal,
				       meta_reread->projection
				                    ->param.atct.rlocal),
	      "projection->param.atct.rlocal fields from original and written-then-reread metadata structures not equal");

  if ( unlink("test_output/test_output_file.meta") == -1 ) {
    perror("failed to delete intra-test temporary file test_output_file.meta");
    fail("");
  }
}
END_TEST

/* Test part of ceos_init and ardop_init from meta_create.  */
START_TEST(test_meta_create_new_format)
{
printf("\nNOT PERFORMING TEST_META_CREATE_NEW_FORMAT()!!\n");
/*
  char err_msg[256];
  meta_parameters *meta;

  meta = meta_create("./CEOS/E12259029000X008");
  
** Check random fields to make sure things are working.  **
  sprintf(err_msg,"meta->general->orbit = %d; not read correctly, should be 22590",meta->general->orbit);
   fail_unless(meta->general->orbit == 22590,err_msg);
  sprintf(err_msg,"meta->general->orbit_direction = '%c'; not read correctly, should be 'D'",meta->general->orbit_direction);
   fail_unless(meta->general->orbit_direction == 'D',err_msg);
  sprintf(err_msg,"meta->sar->prf = %lf; not read correctly, should be 1679.9023438",meta->sar->prf);
   fail_unless(meta->sar->prf == 1679.9023438,err_msg);
  meta_write(meta, "test_output/CEOS_E12259029000X008.meta");
  meta_free(meta);

** Write out a bunch of stuff to manully check **
  meta = meta_create("./CEOS/E20291729000X010");
  meta_write(meta,"test_output/CEOS_E20291729000X010.meta");
  meta_free(meta);
  meta = meta_create("./CEOS/R129968351P4S515");
  meta_write(meta,"test_output/CEOS_R129968351P4S515.meta");
  meta_free(meta);
  meta = meta_create("./CEOS/R129968351U4S115");
  meta_write(meta,"test_output/CEOS_R129968351U4S115.meta");
  meta_free(meta);
*/
}
END_TEST

/* Machinery for running the 'check' tests.  */
Suite *asf_meta_suite(void)
{
  Suite *s = suite_create("asf_meta\n");
  TCase *tc_core = tcase_create("Core");
  
  suite_add_tcase(s, tc_core);
  
  system("rm ./test_output/*");
  
  tcase_add_test(tc_core, test_meta_read_new_format);
  tcase_add_test(tc_core, test_atct);
  tcase_add_test(tc_core, test_lamcc);
  tcase_add_test(tc_core, test_ps);
  tcase_add_test(tc_core, test_utm);
  tcase_add_test(tc_core, test_meta_read_old_format);
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
