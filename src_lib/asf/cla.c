#include "cla.h"


int currArg;		/* Points to current command line arg */

/*strmatch is like strcmp, but it permits partial matching (e.g. "-rot" matches "-rotate"),
   and returns a one if the strings match, 0 otherwise.*/
int strmatch(const char *a, const char *b)
{
	int ii;
	for (ii=0;/*a[ii]&&*/b[ii];ii++)
		if (a[ii]!=b[ii])
			return 0;
	return 1;
}
