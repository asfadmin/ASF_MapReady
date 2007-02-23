#include "cla.h"
#include "asf.h"
#include "asf_nan.h"
#include "assert.h"
#include <stdarg.h>

int currArg=1;   /* Points to current command line arg */

/*strmatch is like strcmp, but it permits partial matching (e.g. "-rot" matches "-rotate"),
   and returns a one if the strings match, 0 otherwise.*/
int strmatch(const char *key, const char *match)
{
	int ii;
	for (ii=0;/*key[ii]&&*/match[ii];ii++)
		if (key[ii]!=match[ii])
			return 0;
	return 1;
}

static int print_warn = 1;

// this is just to help with the debugging of these
/*
static void print_args(int argc, char *argv[])
{
    int i;
    for (i=0; i<argc; ++i)
        printf("%d: %s\n", i, argv[i]);
}
*/

static void no_arg(char * option)
{
    if (print_warn)
        asfPrintWarning("No argument specified for option: %s\n", option);
}

static void bad_arg(char * option, char * arg)
{
    if (print_warn)
        asfPrintWarning("Bad argument specified for option %s: %s\n",
                        option, arg);
}

static void double_arg(const char * option)
{
    if (print_warn)
        asfPrintWarning("Option occurs twice: %s\n", option);
}

static char* gnuify(const char *s)
{
    // returns the gnu-style version of the command line option
    // e.g., returns "--scale" given "-scale".
    static char gnuified[64];

    if (s && s[0]=='-' && strlen(s)>2 && s[1]!='-') {
        assert(strlen(s)<63);
        sprintf(gnuified, "-%s", s);
        return gnuified;
    } else
        return NULL;
}

void remove_args(int start, int end, int *argc, char **argv[])
{
    int i, j, nargs;

    nargs = end - start + 1;
    i = start;
    j = start + nargs;
    
    while (j < *argc)
    {
        char * tmp = (*argv)[i];
        (*argv)[i] = (*argv)[j];
        (*argv)[j] = tmp;
        
        ++i;
        ++j;
    }
    
    *argc -= nargs;
}

//
// Functions for dealing with parsing flags
//

static void detect_flag_option(int argc, char *argv[], char *arg, int *found)
{
    int i;
    
    for (i = 0; i < argc; ++i)
    {
        if (strcmp(argv[i], arg) == 0)
        {
            *found = TRUE;
            return;
        }
    }
}

static void extract_flag_option(int *argc, char **argv[],
                                char *arg, int *found)
{
    int i;
    
    if (!arg) return;

    for (i = 0; i < *argc; ++i)
    {
        if (strcmp((*argv)[i], arg) == 0)
        {
            *found = TRUE;
            remove_args(i, i, argc, argv);
            --i; // we must check this arg again
        }
    }
}

/*
  Example:
    quietflag = detect_flag_options(argc, argv, "-quiet", "--quiet", NULL);

  Returns TRUE if one of the given arguments does occur on the command line
*/

int detect_flag_options(int argc, char **argv, ...)
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;
    
    va_start(ap, argv);
    do
    {
        arg = va_arg(ap, char *);        
        if (arg) {
            detect_flag_option(argc, argv, arg, &found);
            if (found)
                break;
        }
    }
    while (arg);
    va_end(ap);

    return found;
}

/*
  Example:
    quietflag = extract_flag_options(&argc, &argv, "-quiet", "--quiet", NULL);

  This one differs from detect_flag_options in that the detected argument is
  removed from the list.  Duplicate flags are ok.
*/
int extract_flag_options(int *argc, char ***argv, ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;
    
    va_start(ap, argv);
    do
    {
        arg = va_arg(ap, char *);
        extract_flag_option(argc, argv, arg, &found);
        extract_flag_option(argc, argv, gnuify(arg), &found);
    }
    while (arg);
    va_end(ap);

    return found;
}

//
// Functions for dealing with parsing doubles
//

int parse_double(const char * str, double * val)
{
    char *p;
    *val = strtod(str, &p);
    return !(*str == '\0' || *p != '\0');
}

int parse_double_option(int *i, int argc, char *argv[], int *specified,
                        double *value)
{
    double val;
    int ok;

    ++(*i);
    if (*i == argc)
    {
        no_arg(argv[*i-1]);
        ok = FALSE;
    }
    else if (*specified)
    {
        double_arg(argv[*i]);
        ok = FALSE;
    }
    else
    {
        if (parse_double(argv[*i], &val))
        {
            *value = val;
            *specified = TRUE;
            ok = TRUE;
        }
        else
        {
            bad_arg(argv[*i-1], argv[*i]);
            ok = FALSE;
        }
    }
    
    return ok;
}

static void extract_double_option(int *argc, char **argv[], double *val,
                                  char *arg, int *found)
{
    int i;
 
    if (!arg)
        return;

    for (i=0; i<*argc; ++i)
        if (strcmp((*argv)[i], arg) == 0)
            if (parse_double_option(&i, *argc, *argv, found, val))
                remove_args(i-1, i, argc, argv);
}

/*
  Example:
    double x;
    if (extract_double_options(&argc, &argv, &x, "-scale", "-s", NULL)) {
       ... user specified the "-s" option...
    } else {
       ... user did not specify the option
    }

  Notes:
  1) The detected argument is removed from the list.
  2) On the command line, we expect to see a space between the option and
     the double value:
        asf_something -s 12.5 ...
     In this case the function returns TRUE with *val = 12.5
  3) Gnu-style versions are allowed, e.g. "--scale", single character
     options excepted.

  Returns FALSE if the option is not on the command line.
*/
int extract_double_options(int *argc, char **argv[], double *val, ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;
    
    *val = MAGIC_UNSET_DOUBLE;

    va_start(ap, val);
    do
    {
        arg = va_arg(ap, char *);
        extract_double_option(argc, argv, val, arg, &found);
        extract_double_option(argc, argv, val, gnuify(arg), &found);
    }
    while (arg);
    return found;
}

//
// Functions for dealing with parsing integers
//

int parse_int(const char * str, int * val)
{
  char *p;
  *val = (int) strtol(str, &p, 10);

  if (*str == '\0' || *p != '\0')
  {
    return FALSE;
  }

  return TRUE;
}

int parse_int_option(int *i, int argc, char *argv[], int *specified,
		     int *value)
{
  int val;
  int ok;

  ++(*i);
  if (*i == argc)
  {
    no_arg(argv[*i-1]);
    ok = FALSE;
  }
  else if (*specified)
  {
    double_arg(argv[*i]);
    ok = FALSE;
  }
  else
  {
    *specified = TRUE;
    if (parse_int(argv[*i], &val))
    {
      *value = val;
      ok = TRUE;
    }
    else
    {
      bad_arg(argv[*i-1], argv[*i]);
      ok = FALSE;
    }
  }

  return ok;
}

static void extract_int_option(int *argc, char **argv[], int *val,
			       char *arg, int *found)
{
    int i;
 
    if (!arg)
        return;

    for (i=0; i<*argc; ++i)
        if (strcmp((*argv)[i], arg) == 0)
            if (parse_int_option(&i, *argc, *argv, found, val))
                remove_args(i-1, i, argc, argv);
}

int extract_int_options(int *argc, char **argv[], int *val, ... )
{
    va_list ap;
    char * arg = NULL;
    int found = FALSE;
    
    *val = MAGIC_UNSET_INT;

    va_start(ap, val);
    do
    {
        arg = va_arg(ap, char *);
        extract_int_option(argc, argv, val, arg, &found);
        extract_int_option(argc, argv, val, gnuify(arg), &found);
    }
    while (arg);
    return found;
}

//
// Functions for dealing with parsing strings
//

int parse_string_option(int *i, int argc, char *argv[], int *specified,
			char *value)
{
  int ok;

  ++(*i);
  if (*i == argc)
  {
    no_arg(argv[*i-1]);
    ok = FALSE;
  }
  else if (*specified)
  {
    double_arg(argv[*i]);
    ok = FALSE;
  }
  else
  {
    *specified = TRUE;
    strcpy(value, argv[*i]);
    ok = TRUE;
  }

  return ok;
}

static void handle_string_option(int *argc, char **argv[], char *val,
                                 char *arg, int *found, int remove_arg)
{
  int i;

  for (i = 0; i < *argc; ++i)
  {
    if (strcmp((*argv)[i], arg) == 0)
    {
      if (parse_string_option(&i, *argc, *argv, found, val))
      {
        if (remove_arg)
        {
          remove_args(i-1, i, argc, argv);
        }
      }
    }
  }
}

static void detect_string_option(int argc, char *argv[], char *val,
                                 char *arg, int *found)
{
  handle_string_option(&argc, &argv, val, arg, found, FALSE);
}

static void extract_string_option(int *argc, char **argv[], char *val,
                                  char *arg, int *found)
{
  handle_string_option(argc, argv, val, arg, found, TRUE);
}

/* assumes the space has been allocated by caller */
int extract_string_options(int *argc, char **argv[], char *val, ... )
{
  va_list ap;
  char * arg = NULL;
  int found = FALSE;

  *val = '\0';

  va_start(ap, val);
  do
  {
    arg = va_arg(ap, char *);

    if (arg)
      extract_string_option(argc, argv, val, arg, &found);
  }
  while (arg);
 // FIXME: Needs a va_end()... in several functions here (parse_options.c)
  return found;
}

/* assumes the space has been allocated by caller */
int detect_string_options(int argc, char *argv[], char *val, ... )
{
  va_list ap;
  char * arg = NULL;
  int found = FALSE;

  *val = '\0';

  va_start(ap, val);
  do
  {
    arg = va_arg(ap, char *);

    if (arg)
      detect_string_option(argc, argv, val, arg, &found);
  }
  while (arg);

  return found;
}

//
// Function for dealing with parsing logs
//

void parse_log_options(int *argc, char **argv[])
{
  if (extract_string_options(argc, argv, logFile, "--log", "-log", NULL))
    logflag = 1;
  else
    logflag = 0;

  if (extract_flag_options(argc, argv, "--quiet", "-quiet", NULL))
    quietflag = 1;
  else
    quietflag = 0;
}
