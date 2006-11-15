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

/*
  Example:
    quietflag = detect_flag_options(argc, argv, "-quiet", "--quiet", NULL);
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
