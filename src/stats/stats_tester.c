/****************************************************
 Unit tests for stats.

 These use the 'check' unit testing framework for C.
****************************************************/

/* System headers.  */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <check.h>
#include "stats.h"
#include "asf_nan.h"

/* Over-safe micron for test floating point comparisons.  */
#define UNIT_TESTS_MICRON 0.0000000001
#define UNIT_TESTS_FLOAT_COMPARE(a, b) (abs(a - b) < UNIT_TESTS_MICRON ? 1 : 0)

int hist[256] = {
	1093,3086,5248,7330,9416,11181,13027,14861,
	16518,18001,19340,20545,21794,22563,23356,24128,
	24499,25264,25368,25315,25611,25481,25234,25239,
	25030,24396,23858,23503,22635,21878,21224,20565,
	19762,19284,18329,17087,16779,15886,15061,14451,
	13689,12750,12020,11269,10768,9969,9493,8962,
	8248,7652,7309,6684,6333,5789,5424,5007,
	4793,4265,4038,3685,3422,3217,3042,2681,
	2519,2326,2222,1966,1847,1721,1612,1498,
	1438,1270,1183,1172,1021,929,900,791,
	688,684,664,575,553,515,462,454,
	444,385,398,376,344,330,293,286,
	262,228,227,180,198,193,189,175,
	161,147,148,126,97,109,106,100,
	102,82,93,86,66,78,52,63,
	58,54,46,49,58,60,41,41,
	40,34,31,43,24,24,35,25,
	25,23,26,20,15,22,19,20,
	17,13,11,14,8,16,15,12,
	13,4,11,7,5,7,5,9,
	7,8,5,3,9,1,1,4,
	4,3,4,5,2,4,7,4,
	3,2,1,4,4,1,4,0,
	2,0,1,2,0,2,0,2,
	2,2,2,1,0,1,0,2,
	2,2,0,0,4,1,0,0,
	2,0,1,0,1,0,1,0,
	1,0,0,0,0,0,1,0,
	0,1,0,1,0,1,0,0,
	0,1,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,
	0,0,0,0,1,0,0,1
};

/**********************************************
 * Test stat file reading routine.           */
START_TEST(test_stat_read)
{
	char err_msg[256];
	stat_parameters stats;
	
	stat_read (&stats,"R13267916000X006.stat");

/* Check all the fields */
	sprintf(err_msg,
		"test_stat_read: stats.min = %lf, should be 1.0\n",stats.min);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.min, 1.0), err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.max = %lf, should be 27915.396484\n",
		stats.max);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.max, 27915.396484), err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.mean = %lf, should be 3149.6018862\n",
		stats.mean);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.mean, 3149.6018862),err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.rmse = %lf, should be 1.1414064459\n",
		stats.rmse);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.rmse, 1.1414064459),err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.std_deviation = %lf, should be 1.1414070421\n",
		stats.std_deviation);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.std_deviation, 1.1414070421),
		err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.mask = %lf, should be NaN\n",stats.mask);
	fail_unless( stats.mask != stats.mask, err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.slope = %lf, should be 0.0091350712219\n",
		stats.slope);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.slope, 0.0091350712219),
		err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.offset = %lf, should be -0.0091350712219\n",
		stats.offset);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.offset, -0.0091350712219),
		err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.upper_right_line = %d, should be 40\n",
		stats.upper_right_line);
	fail_unless( stats.upper_right_line == 40,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.upper_right_samp = %d, should be 600\n",
		stats.upper_right_samp);
	fail_unless( stats.upper_right_samp == 600,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.lower_left_line = %d, should be 2440\n",
		stats.lower_left_line);
	fail_unless( stats.lower_left_line == 2440,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.lower_left_samp = %d, should be 1000\n",
		stats.lower_left_samp);
	fail_unless( stats.lower_left_samp == 1000,err_msg);

/* Check several spots in the histogram to make sure its cool */
	sprintf(err_msg,
		"test_stat_read: stats.histogram[0] = %d, should be 1093\n",
		stats.histogram[0]);
	fail_unless( stats.histogram[0]   == 1093, err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[9] = %d, should be 18001\n",
		stats.histogram[9]);
	fail_unless( stats.histogram[9]   == 18001,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[18] = %d, should be 25368\n",
		stats.histogram[18]);
	fail_unless( stats.histogram[18]  == 25368,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[27] = %d, should be 23503\n",
		stats.histogram[27]);
	fail_unless( stats.histogram[27]  == 23503,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[36] = %d, should be 16779\n",
		stats.histogram[36]);
	fail_unless( stats.histogram[36]  == 16779,err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[45] = %d, should be 9969\n",
		stats.histogram[45]);
	fail_unless( stats.histogram[45]  == 9969, err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[54] = %d, should be 5424\n",
		stats.histogram[54]);
	fail_unless( stats.histogram[54]  == 5424, err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[63] = %d, should be 2681\n",
		stats.histogram[63]);
	fail_unless( stats.histogram[63]  == 2681, err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[116] = %d, should be 66\n",
		stats.histogram[116]);
	fail_unless( stats.histogram[116] == 66,   err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[209] = %d, should be 0\n",
		stats.histogram[209]);
	fail_unless( stats.histogram[209] == 0,    err_msg);
	sprintf(err_msg,
		"test_stat_read: stats.histogram[255] = %d, should be 1\n",
		stats.histogram[255]);
	fail_unless( stats.histogram[255] == 1,    err_msg);
}
END_TEST

/**********************************************************************
 * Test stat file writing routine.
 * Fills the stats structure, writes it out, then runs the read test */
START_TEST(test_stat_write)
{
	int ii;
	char err_msg[256];
	stat_parameters stats;

	stats.min = 1;
	stats.max = 27915.396484;
	stats.mean = 3149.6018862;
	stats.rmse = 1.1414064459;
	stats.std_deviation = 1.1414070421;
	stats.mask = NAN;
	stats.slope = 0.0091350712219;
	stats.offset = -0.0091350712219;
	stats.upper_right_line = 40;
	stats.upper_right_samp = 600;
	stats.lower_left_line = 2440;
	stats.lower_left_samp = 1000;
	for (ii=0; ii<256; ii++) stats.histogram[ii] = hist[ii];

	stat_write (&stats,"temp.stat");
	stat_read (&stats,"temp.stat");
	remove("temp.stat");

/* Check all the fields */
	sprintf(err_msg,
		"test_stat_write: stats.min = %lf, should be 1.0\n",stats.min);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.min, 1.0), err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.max = %lf, should be 27915.396484\n",
		stats.max);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.max, 27915.396484), err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.mean = %lf, should be 3149.6018862\n",
		stats.mean);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.mean, 3149.6018862),err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.rmse = %lf, should be 1.1414064459\n",
		stats.rmse);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.rmse, 1.1414064459),err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.std_deviation = %lf, should be 1.1414070421\n",
		stats.std_deviation);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.std_deviation, 1.1414070421),
		err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.mask = %lf, should be NaN\n",stats.mask);
	fail_unless( stats.mask != stats.mask, err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.slope = %lf, should be 0.0091350712219\n",
		stats.slope);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.slope, 0.0091350712219),
		err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.offset = %lf, should be -0.0091350712219\n",
		stats.offset);
	fail_unless( UNIT_TESTS_FLOAT_COMPARE(stats.offset, -0.0091350712219),
		err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.upper_right_line = %d, should be 40\n",
		stats.upper_right_line);
	fail_unless( stats.upper_right_line == 40,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.upper_right_samp = %d, should be 600\n",
		stats.upper_right_samp);
	fail_unless( stats.upper_right_samp == 600,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.lower_left_line = %d, should be 2440\n",
		stats.lower_left_line);
	fail_unless( stats.lower_left_line == 2440,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.lower_left_samp = %d, should be 1000\n",
		stats.lower_left_samp);
	fail_unless( stats.lower_left_samp == 1000,err_msg);

/* Check several spots in the histogram to make sure its cool */
	sprintf(err_msg,
		"test_stat_write: stats.histogram[0] = %d, should be 1093\n",
		stats.histogram[0]);
	fail_unless( stats.histogram[0]   == 1093, err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[9] = %d, should be 18001\n",
		stats.histogram[9]);
	fail_unless( stats.histogram[9]   == 18001,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[18] = %d, should be 25368\n",
		stats.histogram[18]);
	fail_unless( stats.histogram[18]  == 25368,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[27] = %d, should be 23503\n",
		stats.histogram[27]);
	fail_unless( stats.histogram[27]  == 23503,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[36] = %d, should be 16779\n",
		stats.histogram[36]);
	fail_unless( stats.histogram[36]  == 16779,err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[45] = %d, should be 9969\n",
		stats.histogram[45]);
	fail_unless( stats.histogram[45]  == 9969, err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[54] = %d, should be 5424\n",
		stats.histogram[54]);
	fail_unless( stats.histogram[54]  == 5424, err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[63] = %d, should be 2681\n",
		stats.histogram[63]);
	fail_unless( stats.histogram[63]  == 2681, err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[116] = %d, should be 66\n",
		stats.histogram[116]);
	fail_unless( stats.histogram[116] == 66,   err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[209] = %d, should be 0\n",
		stats.histogram[209]);
	fail_unless( stats.histogram[209] == 0,    err_msg);
	sprintf(err_msg,
		"test_stat_write: stats.histogram[255] = %d, should be 1\n",
		stats.histogram[255]);
	fail_unless( stats.histogram[255] == 1,    err_msg);
}
END_TEST

/**********************************************
 * Machinery for running the 'check' tests.  */
Suite *stats_suite(void)
{
  Suite *suite = suite_create("stats\n");
  TCase *tc_core = tcase_create("Core");
  
  suite_add_tcase(suite, tc_core);

  tcase_add_test(tc_core, test_stat_read);
  tcase_add_test(tc_core, test_stat_write);

  return suite;
}

/**********************************************/
int main(void)
{
  int failure_count;
  Suite *suite = stats_suite();
  SRunner *suite_runner = srunner_create(suite);
 
  srunner_run_all(suite_runner, CK_VERBOSE);

  failure_count = srunner_ntests_failed(suite_runner);

  srunner_free(suite_runner);
  suite_free(suite);

  return ( failure_count == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
