#include "asf_license.h"
#include "asf_version.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

void print_copyright()
{
    time_t t;
    struct tm *ts;
    t = time(NULL);
    ts = localtime(&t);
    int year = ts->tm_year+1900;
    printf(ASF_COPYRIGHT_STRING"\n");
}

// Print our copyright and license notice & exit
void print_license(int license_id)
{
  print_copyright();
  switch (license_id) {
    case ASF_LICENSE_ID:
      printf(ASF_LICENSE_STRING);
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

  sprintf(out_buf, "%s, part of %s %s", program_name,
          TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);

  return out_buf;
}

void print_version(const char *program_name)
{
  printf("%s, part of %s %s\n", program_name,
         TOOL_SUITE_NAME, TOOL_SUITE_VERSION_STRING);

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
            print_license(ASF_LICENSE_ID);
        }
        if (strcmp(argv[i], "-version") == 0 ||
            strcmp(argv[i], "--version") == 0) {
            print_version(program_name);
        }
    }
}
