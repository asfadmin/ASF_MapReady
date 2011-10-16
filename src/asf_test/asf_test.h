#ifndef ASF_TEST_H
#define ASF_TEST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_vector.h"
#include "asf_convert.h"
#include "asf_raster.h"
#include "asf_export.h"
#include "asf_jpeg.h"
#include "metadisplay.h"
#include "CUnit/Automated.h"
#include "CUnit/Basic.h"

typedef struct
{
  char *meta_file;
  float incid;
  int sample;
  float inDn;
  char *bandExt;
  int dbFlag;
  double value;                 
} t_get_cal_dn;

typedef struct
{
  char *name;                   // name of library function
  t_get_cal_dn *get_cal_dn;     // details for get_cal_dn function
} t_library;

typedef struct
{
  char *suite;                  // name of test suite
  char *type;                   // test type: metadata, library, binary
  int short_config;             // configuration flag: 0 - short, 1 - long
  int test_count;               // number of test - for internal use only
  char *status;                 // suite test status: new, pass, fail
} t_general;

typedef struct
{
  char *test;                   // name of test
  char *file;                   // file to be tested
  char *specs;                  // test specifications
  t_library *lib;               // library function details
  char *status;                 // test status: new, pass, fail, skip
} t_test;

typedef struct
{
  char comment[255];            // first line for comments
  t_general *general;           // general setup
  t_test **test;                // test parameters
} test_config;

// config file functions
int init_test_config(char *configFile);
test_config *init_fill_test_config(char *configFile);
void free_test_config(test_config *cfg);
test_config *read_test_config(char *configFile);
int write_test_config(char *configFile, test_config *cfg);

// prototypes
int add_uavsar_metadata_tests(void);
int add_uavsar_geotiff_tests(void);
int add_rsat1_map_projections_tests(void);
int add_rsat1_geotiff_tests(void);
int add_rsat1_overlay_tests(void);
int add_alos_browse_tests(void);
int add_alos_leader_tests(void);
int add_alos_calibration_tests(void);

void cu_difftext(char *testFile, char *referenceFile, char *exceptFile);
void cu_diffimage(char *testFile, char *referenceFile);

#endif
