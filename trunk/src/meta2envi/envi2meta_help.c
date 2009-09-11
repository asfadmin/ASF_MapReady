#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "cla.h"
#include "asf_version.h"
#include "asf_contact.h"
#include "asf_license.h"
#include "envi2meta_help.h"

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
    fprintf(stderr,"\nUsage:\n   %s\n\n", TOOL_USAGE);
}

void print_help()
{
    fprintf(stderr,"\nTool name:\n   %s\n", TOOL_NAME);
    usage();
    fprintf(stderr,"Description:\n%s\n\n", TOOL_DESCRIPTION);
    if(strlen(TOOL_INPUT)) fprintf(stderr,"Input:\n%s\n\n", TOOL_INPUT);
    if(strlen(TOOL_OUTPUT)) fprintf(stderr,"Output:\n%s\n\n", TOOL_OUTPUT);
    if(strlen(TOOL_OPTIONS)) fprintf(stderr,"Options:\n%s\n\n", TOOL_OPTIONS);
    if(strlen(TOOL_EXAMPLES)) fprintf(stderr,"Examples:\n%s\n\n", TOOL_EXAMPLES);
    if(strlen(TOOL_LIMITATIONS)) fprintf(stderr,"Limitations:\n%s\n\n", TOOL_LIMITATIONS);
    if(strlen(TOOL_SEE_ALSO)) fprintf(stderr,"See Also:\n%s\n\n", TOOL_SEE_ALSO);
    fprintf(stderr,"Contact:\n%s\n", ASF_CONTACT_STRING);
    print_version(TOOL_NAME);
    print_copyright();
}

