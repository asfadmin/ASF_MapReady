#include "asf_license.h"
#include "asf_version.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#define ASF_COPYRIGHT_STRING \
"Copyright (c) %d, University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

static void print_copyright()
{
    time_t t;
    struct tm *ts; 
    t = time(NULL);
    ts = localtime(&t);
    int year = ts->tm_year+1900;
    printf("\n"ASF_COPYRIGHT_STRING"\n", year);
}

// Print our copyright and license notice & exit
static void print_license(int license_id)
{
  print_copyright();
  switch (license_id) {
    case ASF_BSD_ID:
      printf(ASF_BSD_LICENSE_STRING"\n");
      break;
    default:
      printf("License not found.\n");
      break;
  }
  exit(EXIT_SUCCESS);
}

static void print_version(const char *program_name)
{
    printf("%s, version %s\n", program_name, 
                   CONVERT_PACKAGE_VERSION_STRING);
    print_copyright();
    exit (EXIT_SUCCESS);
}

void handle_license_and_version_args(int argc, char *argv[],
                                     const char *program_name)
{
    int i;
    for (i = 0; i < argc; ++i) {
        if (strcmp(argv[i], "-license") == 0 ||
            strcmp(argv[i], "--license") == 0) {
            print_license(ASF_BSD_ID);
        }
        if (strcmp(argv[i], "-version") == 0 ||
            strcmp(argv[i], "--version") == 0) {
            print_version(program_name);
        }
    }
}
