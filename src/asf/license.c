#include "asf_license.h"
#include "asf_version.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#define ASF_COPYRIGHT_STRING \
"Copyright (c) %d, University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

void print_copyright()
{
    time_t t;
    struct tm *ts;
    t = time(NULL);
    ts = localtime(&t);
    int year = ts->tm_year+1900;
    printf("\n"ASF_COPYRIGHT_STRING"\n", year);
}

// Print our copyright and license notice & exit
void print_license(int license_id)
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

const char *version_string(const char *program_name)
{
  static char out_buf[1024];

  if (strlen(SVN_REV)>0) {
    sprintf(out_buf, "%s (part of %s %s)",
            SVN_REV, TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);
  } else {
    sprintf(out_buf, "%s %s",
            TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);
  }

  return out_buf;
}

void print_version(const char *program_name)
{
  if (strlen(SVN_REV)>0) {
    printf("%s, revision %s (part of %s %s)\n",
           program_name, SVN_REV, TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);
  } else {
    printf("%s, part of %s %s (unknown build)\n", program_name,
           TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);
  }

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
