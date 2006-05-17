#include "asf.h"

char * strdup(const char *);

static char * s_tmp_dir = 0;

void
set_asf_tmp_dir(const char *tmp_dir)
{
    s_tmp_dir = strdup(tmp_dir);

    /* remove trailing path separator, if one is present */
    if (s_tmp_dir[strlen(s_tmp_dir) - 1] == DIR_SEPARATOR) {
      s_tmp_dir[strlen(s_tmp_dir) - 1] = '\0';
    }

}

const char *
get_asf_tmp_dir()
{
  if (!s_tmp_dir) {

    // default to current directory
    s_tmp_dir = strdup(".");

  }

  return s_tmp_dir;
}

static char * full_tmp_name (const char *filename)
{
  char * full_name;
  const char * tmp_dir;

  tmp_dir = get_asf_tmp_dir();
  full_name = (char *) malloc (sizeof(char) *
                                (strlen(filename) + strlen(tmp_dir) + 10));

  sprintf(full_name, "%s%c%s", tmp_dir, DIR_SEPARATOR, filename);
}

FILE * 
fopen_tmp_file(const char * filename, const char * mode)
{
  FILE * fp;
  char * full_name;

  full_name = full_tmp_name (filename);
  fp = FOPEN(full_name, mode);

  free(full_name);
  return fp;
}

int
unlink_tmp_file(const char *filename)
{
  char * full_name;
  int ret;

  full_name = full_tmp_name (filename);
  ret = unlink (full_name);
  free(full_name);

  return ret;
}
