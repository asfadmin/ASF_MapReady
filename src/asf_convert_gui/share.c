#include "asf.h"
#include <stdio.h>

char *
find_in_share(const char * filename)
{
   char * ret = (char *) malloc(sizeof(char) * 
                     (strlen(get_asf_share_dir()) + strlen(filename) + 5));
   sprintf(ret, "%s%c%s", get_asf_share_dir(), DIR_SEPARATOR, filename);
   return ret;
}

char *
find_in_bin(const char * filename)
{
   char * ret = (char *) malloc(sizeof(char) *
                    (strlen(get_asf_bin_dir()) + strlen(filename) + 5));
   sprintf(ret, "%s%c%s", get_asf_bin_dir(), DIR_SEPARATOR, filename);
   return ret;
}

