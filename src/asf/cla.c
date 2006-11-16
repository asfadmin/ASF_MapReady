#include "cla.h"
#include "asf.h"
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

// this is just to help with the debugging of these
static void print_args(int argc, char *argv[])
{
    int i;
    for (i=0; i<argc; ++i)
        printf("%d: %s\n", i, argv[i]);
}

static void remove_args(int start, int end, int *argc, char **argv[])
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
        
        if (arg)
            extract_flag_option(argc, argv, arg, &found);
    }
    while (arg);
    va_end(ap);

    return found;
}
