/* Unit tests for the asf library.  These use the 'check' unit testing
   framework for C.  Not complete by any means, but hopefully new code
   will use them at least.  */

/* Note that these tests may contain hard wired values that correspond
   to the test data.  */

/* System headers.  */
#include <unistd.h>

/* ASF headers.  */
#include <asf.h>

#include <check.h>		/* For unit check functions.  */

/* A random micron for floating point comparisons.  */
#define UNIT_TESTS_MICRON 0.000000001
#define UNIT_TESTS_FLOAT_COMPARE(a, b) (abs((a) - (b)) \
                                        < UNIT_TESTS_MICRON ? 1 : 0)

/* Names of test files used, relative to source directory from which
   tests are assumed to be run.  */
#define INT_STAR_2_META_FILE "test_data/test_int_star_2_file.meta"
#define INT_STAR_2_IMG_FILE "test_data/test_int_star_2_file.img"
#define BYTE_META_FILE "test_data/test_byte_file.meta"
#define BYTE_IMG_FILE "test_data/test_byte_file.img"
#define REAL_STAR_8_META_FILE "test_data/test_real_star_8_file.meta"
#define REAL_STAR_8_IMG_FILE "test_data/test_real_star_8_file.img"

/* Line number to read when testing get_float_line.  */
#define TEST_LINE_NUMBER 2
/* First int (or byte, short, long, etc.) sample value to use in test line.  */
#define FIRST_INT_SAMPLE_TEST_VALUE 3
/* First float (or double) sample value to use in test line.
   Subsequent samples increase by 1(.0) per sample.  */
#define FIRST_FLOAT_SAMPLE_TEST_VALUE 3.3423262

/* Function to set up environment for the tests to run in.  */
void setup(void)
{
  /* Set up the two byte integer test data file.  Note meta file must
     already exist.  */
  {
    meta_parameters *meta = meta_read(INT_STAR_2_META_FILE);
    FILE *file = FOPEN(INT_STAR_2_IMG_FILE, "w");
    /* Buffer for data to be written.  */
    int16_t *data = MALLOC(meta->general->sample_count 
			   * (TEST_LINE_NUMBER + 1) * sizeof(int16_t));
    int idx1, idx2;		/* Index variables.  */

    /* Write lines of zeros for the lines we aren't reading.  */
    for ( idx1 = 0 ; idx1 < TEST_LINE_NUMBER * meta->general->sample_count ; 
	  idx1++ )
	data[idx1] = 0;

    /* Write the line of test data.  */
    for ( idx2 = 0 ; idx2 < meta->general->sample_count ; idx2++ )
      data[idx1 + idx2] = FIRST_INT_SAMPLE_TEST_VALUE + idx2;

    /* Write test data to file.  */
    FWRITE(data, sizeof(int16_t), 
	   meta->general->sample_count * (TEST_LINE_NUMBER + 1), file);

    meta_free(meta);
    FCLOSE(file);
  }

  /* Set up the byte test data file.  Note meta file must already
     exist.  */
  {
    meta_parameters *meta = meta_read(BYTE_META_FILE);
    FILE *file = FOPEN(BYTE_IMG_FILE, "w");
    uint8_t *data = MALLOC(meta->general->sample_count 
			   * (TEST_LINE_NUMBER + 1) * sizeof(uint8_t));
    int idx1, idx2;		/* Index variables.  */

    /* Write lines of zeros for the lines we aren't reading.  */
    for ( idx1 = 0 ; idx1 < TEST_LINE_NUMBER * meta->general->sample_count ; 
	  idx1++ )
	data[idx1] = 0;

    /* Write the line of test data.  */
    for ( idx2 = 0 ; idx2 < meta->general->sample_count ; idx2++ )
      data[idx1 + idx2] = FIRST_INT_SAMPLE_TEST_VALUE + idx2;

    /* Write test data to file.  */
    FWRITE(data, sizeof(uint8_t), 
	   meta->general->sample_count * (TEST_LINE_NUMBER + 1), file);

    meta_free(meta);
    FCLOSE(file);
  }

  /* Set up the eight byte real test data file.  Note meta file must
     already exist.  */
  {
    meta_parameters *meta = meta_read(REAL_STAR_8_META_FILE);
    FILE *file = FOPEN(REAL_STAR_8_IMG_FILE, "w");
    double *data = MALLOC(meta->general->sample_count * (TEST_LINE_NUMBER + 1) 
			  * sizeof(double));
    int idx1, idx2;		/* Index variables.  */

    /* Write lines of zeros for the lines we aren't reading.  */
    for ( idx1 = 0 ; idx1 < TEST_LINE_NUMBER * meta->general->sample_count ; 
	  idx1++ )
	data[idx1] = 0;

    /* Write the line of test data.  */
    for ( idx2 = 0 ; idx2 < meta->general->sample_count ; idx2++ )
      data[idx1 + idx2] = FIRST_FLOAT_SAMPLE_TEST_VALUE + idx2;

    /* Write test data to file.  */
    FWRITE(data, sizeof(double), 
	   meta->general->sample_count * (TEST_LINE_NUMBER + 1), file);

    meta_free(meta);
    FCLOSE(file);
  }
}

/* Tear-down function for the test's environment.  */
void teardown(void)
{
  unlink(INT_STAR_2_IMG_FILE);
  unlink(BYTE_IMG_FILE);
  unlink(REAL_STAR_8_IMG_FILE);
}

/* Test the part of meta_read that parses new files.  */
START_TEST(test_get_float_line_from_int_star_2)
{
  meta_parameters *meta = meta_read(INT_STAR_2_META_FILE);
  FILE *image_file_p = FOPEN(INT_STAR_2_IMG_FILE, "r");
  /* Destination for data read from test file.  */
  float *dest = MALLOC(meta->general->sample_count * sizeof(float));
  int idx;			/* Index for samples.  */
  
  get_float_line(image_file_p, meta, TEST_LINE_NUMBER, dest);
  
  /* Check line data against its expected values.  */
  for ( idx = 0 ; idx < meta->general->sample_count ; idx++ ) {
    fail_unless(UNIT_TESTS_FLOAT_COMPARE(dest[idx], 
					 FIRST_INT_SAMPLE_TEST_VALUE + idx),
		"unexpected floating point value in fetched line data");
  }

  meta_free(meta);
  FCLOSE(image_file_p);
}
END_TEST


START_TEST(test_get_float_line_from_byte)
{
  meta_parameters *meta = meta_read(BYTE_META_FILE);
  FILE *image_file_p = FOPEN(BYTE_IMG_FILE, "r");
  /* Destination for data read from test file.  */
  float *dest = MALLOC(meta->general->sample_count * sizeof(float));
  int idx;			/* Index for samples.  */
  
  get_float_line(image_file_p, meta, TEST_LINE_NUMBER, dest);
  
  /* Check line data against its expected values.  */
  for ( idx = 0 ; idx < meta->general->sample_count ; idx++ )
    fail_unless(UNIT_TESTS_FLOAT_COMPARE(dest[idx], 
					 FIRST_INT_SAMPLE_TEST_VALUE + idx),
		"unexpected floating point value in fetched line data");

  meta_free(meta);
  FCLOSE(image_file_p);
}
END_TEST

START_TEST(test_get_float_line_from_real_star_8)
{
  meta_parameters *meta = meta_read(REAL_STAR_8_META_FILE);
  FILE *image_file_p = FOPEN(REAL_STAR_8_IMG_FILE, "r");
  /* Destination for data read from test file.  */
  float *dest = MALLOC(meta->general->sample_count * sizeof(float));
  int idx;			/* Index for samples.  */
  
  get_float_line(image_file_p, meta, TEST_LINE_NUMBER, dest);
  
  /* Check line data against its expected values.  */
  for ( idx = 0 ; idx < meta->general->sample_count ; idx++ )
    fail_unless(UNIT_TESTS_FLOAT_COMPARE(dest[idx], 
					 FIRST_FLOAT_SAMPLE_TEST_VALUE + idx),
		"unexpected floating point value in fetched line data");

  meta_free(meta);
  FCLOSE(image_file_p);
}
END_TEST

/* Machinery for running the 'check' tests.  */
Suite *asf_suite(void)
{
  Suite *asf = suite_create("asf");
  TCase *core = tcase_create("core");
  
  suite_add_tcase(asf, core);

  tcase_add_checked_fixture(core, setup, teardown);
  
  tcase_add_test(core, test_get_float_line_from_int_star_2);
  tcase_add_test(core, test_get_float_line_from_byte);
  tcase_add_test(core, test_get_float_line_from_real_star_8);

  return asf;
}

int main(void)
{
  int failure_count;
  Suite *asf = asf_suite();
  SRunner *suite_runner = srunner_create(asf);
 
  srunner_run_all(suite_runner, CK_VERBOSE);

  failure_count = srunner_ntests_failed(suite_runner);

  srunner_free(suite_runner);
  suite_free(asf);

  return ( failure_count == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
