#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "cla.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "asf_version.h"
#include "asf_contact.h"
#include "asf_license.h"
#include "convert2vector_help.h"

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

int getDoubleOption(char *key, int argc, char* argv[], double *val, double def)
{
    int idx = checkForOption(key, argc, argv);
    if (idx && (idx+1)<argc) {
        char *endp;
        double d;
        long l;
        d = strtod(argv[idx+1], &endp);
        if (argv[idx+1] != endp && *endp == '\0') {
            *val = d;
        }
        else {
            l = strtol(argv[idx+1], &endp, 0);
            if (argv[idx+1] != endp && *endp == '\0') {
                *val = (double)l;
            }
            else {
                *val = meta_is_valid_double(def) ? def : MAGIC_UNSET_DOUBLE;
                idx = 0;
            }
        }
    }
    else {
        *val = meta_is_valid_double(def) ? def : MAGIC_UNSET_DOUBLE;
        idx = 0;
    }
    return idx;
}

int getIntegerOption(char *key, int argc, char* argv[], int *val, int def)
{
    int idx = checkForOption(key, argc, argv);
    if (idx && (idx+1)<argc) {
        char *endp;
        double d;
        long l;
        d = strtod(argv[idx+1], &endp);
        if (argv[idx+1] != endp && *endp == '\0') {
            if (floor(d) == d) {
                *val = (int)d;
            }
            else {
                *val = meta_is_valid_int(def) ? def : MAGIC_UNSET_INT;
                idx = 0;
            }
        }
        else {
            l = strtol(argv[idx+1], &endp, 0);
            if (argv[idx+1] != endp && *endp == '\0') {
                *val = l;
            }
            else {
                *val = meta_is_valid_int(def) ? def : MAGIC_UNSET_INT;
                idx = 0;
            }
        }
    }
    else {
        *val = meta_is_valid_int(def) ? def : MAGIC_UNSET_INT;
        idx = 0;
    }
    return idx;
}

int getStringOption(char *key, int argc, char *argv[], char *val, char *def)
{
    int idx = checkForOption(key, argc, argv);
    if (idx && (idx+1)<argc) {
        if (argv[idx+1] && strlen(argv[idx+1])) {
            strcpy(val, argv[idx+1]);
        }
        else {
            strcpy(val, (def && strlen(def)) ? def : MAGIC_UNSET_STRING);
            idx = 0;
        }
    }
    else {
        strcpy(val, (def && strlen(def)) ? def : MAGIC_UNSET_STRING);
        idx = 0;
    }
    return idx;
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

void usage(char *msg)
{
    if (msg && strlen(msg)) {
        fprintf(stderr, "\n%s: %s\n", TOOL_NAME, msg);
    }
    fprintf(stderr,"\nUsage:\n   %s\n\n", TOOL_USAGE);
    fprintf(stderr,"Try `%s -help' for more information.\n\n", TOOL_NAME);
}

void print_help()
{
    fprintf(stderr,"\nTool name:\n   %s\n", TOOL_NAME);
    usage(NULL);
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

