#include "cla.h"


int currArg;		/* Points to current command line arg */

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
