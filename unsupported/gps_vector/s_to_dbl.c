/*******************************************************************************

FUNCTION: Change the ascii data into float data.

HISTORY: Shusun Li.
	 Mike Shindle
*******************************************************************************/

#include "asf.h"



double s_to_dbl(s, n)
char	*s;
int	n;
{
  char strn[256];
  double tmp;

  strncpy(strn, s, n);
  strn[n] = '\0';
  tmp = atof(strn);

  return (tmp);
}
