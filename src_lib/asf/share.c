#include "asf.h"
#include "config.h"

char * strdup(const char *);

static char * s_share_dir = 0;

const char * 
get_asf_share_dir()
{
  if (!s_share_dir) {
#ifdef win32
    s_share_dir = strdup("???");
#else
    s_share_dir = strdup(ASF_SHARE_DIR);
#endif

    /* remove trailing path separator, if one is present */
    if (s_share_dir[strlen(s_share_dir) - 1] == DIR_SEPARATOR) {
      s_share_dir[strlen(s_share_dir) - 1] = '\0';
    }
  }
  
  return s_share_dir;
}

FILE * 
fopen_share_file(const char * filename, const char * mode)
{
  char * full_name;
  const char * share_dir;
  FILE * fp;

  share_dir = get_asf_share_dir();
  full_name = (char *) malloc (sizeof(char) *
                                (strlen(filename) + strlen(share_dir) + 10));

  sprintf(full_name, "%s%c%s", share_dir, DIR_SEPARATOR, filename);

  fp = FOPEN(full_name, mode);

  free(full_name);
  return fp;
}

