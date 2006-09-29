#include <asf.h>

char *uc (const char *string)
{
  static char out[1024];
  int ii;

  for (ii=0; ii<strlen(string); ii++)
    out[ii]=toupper(string[ii]);
  out[ii]='\0';

  return out;
}
