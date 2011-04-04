#ifndef ASF_TEST_H
#define ASF_TEST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  char *suite;                  // name of test suite
  char *type;                   // test type: metadata, binary
  char *interface;              // interface: automated, basic, manual
  int short_config;             // configuration flag: 0 - short, 1 - long
  int test_count;               // number of test - for internal use only
  char *status;                 // suite test status: new, pass, fail
} t_general;

typedef struct
{
  char *test;                   // name of test
  char *file;                   // file to be tested
  char *specs;                  // test specifications
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

#endif
