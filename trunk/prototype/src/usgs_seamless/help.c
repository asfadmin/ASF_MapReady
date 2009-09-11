#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <string>
#include "asf_version.h"
#include "asf_contact.h"
#include "asf_license.h"
#include "akdem_grab_help.h"

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
  exit(EXIT_FAILURE);
}
 
void print_version(const char *program_name)
{
    if (strlen(SVN_REV)>0) {
      printf("\n%s, version %s (part of %s %s)\n",
             program_name, SVN_REV, TOOL_SUITE_NAME, MAPREADY_VERSION_STRING);
    } else {
      printf("\n%s, part of %s %s (unknown build)\n", program_name,
             TOOL_SUITE_NAME, MAPREADY_VERSION_STRING);
    }
 
    print_copyright();
    exit (EXIT_FAILURE);
}
 
int strmatch(const char *key, const char *match)
{
  int ii;
  for (ii=0;match[ii];ii++)
    if (key[ii]!=match[ii])
      return 0;
  return 1;
}

static int _checkForOption(char* key, int argc, char* argv[])
{
  int ii=0;
  while(ii < argc)
  {
    if(strmatch(key, argv[ii]))
      return(ii);
    ++ii;
   }
   return(0);
}

void handle_license_and_version_args(int argc, char *argv[],
                                     const char *program_name)
{
  if(_checkForOption("--license", argc, argv) ||
     _checkForOption("-l", argc, argv)        ||
     _checkForOption("-license", argc, argv))
  {
    print_license(ASF_BSD_ID);
  }
  if(_checkForOption("--version", argc, argv) ||
     _checkForOption("-v", argc, argv)        ||
     _checkForOption("-version", argc, argv))
  {
    print_version(program_name);
  }
}

void check_for_help(int argc,char *argv[])
{
  if (_checkForOption("--help", argc, argv) ||
      _checkForOption("-h", argc, argv)     ||
      _checkForOption("-help", argc, argv)) {
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
	fprintf(stderr,"Contact:\n%s", ASF_CONTACT_STRING);
        print_version(TOOL_NAME);
        print_copyright();
}

