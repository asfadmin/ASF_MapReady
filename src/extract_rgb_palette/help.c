#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "cla.h"
#include "asf_version.h"
#include "asf_contact.h"
#include "asf_license.h"
#include "extract_rgb_palette_help.h"

int checkForOption(char* key, int argc, char* argv[])
{
    int ii = 1;
    while(ii < argc)
    {
        if(strmatch(key, argv[ii]))
            return(ii);
        ++ii;
    }
    return(0);
}

void check_for_help(int argc,char *argv[])
{
  if (checkForOption("--help", argc, argv) ||
      checkForOption("-h", argc, argv)     ||
      checkForOption("-help", argc, argv)) {
    print_help();
    exit(1);
  }
}

void usage()
{
    printf("\nUsage:\n   %s\n\n", TOOL_USAGE);
}

void print_help()
{
    printf("\nTool name:\n   %s\n", TOOL_NAME);
    usage();
    printf("Description:\n%s\n\n", TOOL_DESCRIPTION);
    if(strlen(TOOL_INPUT)) printf("Input:\n%s\n\n", TOOL_INPUT);
    if(strlen(TOOL_OUTPUT)) printf("Output:\n%s\n\n", TOOL_OUTPUT);
    if(strlen(TOOL_OPTIONS)) printf("Options:\n%s\n\n", TOOL_OPTIONS);
    if(strlen(TOOL_EXAMPLES)) printf("Examples:\n%s\n\n", TOOL_EXAMPLES);
    if(strlen(TOOL_LIMITATIONS)) printf("Limitations:\n%s\n\n", TOOL_LIMITATIONS);
    if(strlen(TOOL_SEE_ALSO)) printf("See Also:\n%s\n\n", TOOL_SEE_ALSO);
    printf("Contact:\n%s\n", ASF_CONTACT_STRING);
    print_version(TOOL_NAME);
    print_copyright();
}

