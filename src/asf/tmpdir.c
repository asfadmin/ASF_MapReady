#include "asf.h"

char * strdup(const char *);

/* static var for holding the temporary directory        */
/* use the get/set methods instead of accessing directly */
static char * s_tmp_dir = 0;

/* Setting the temporary directory */
void
set_asf_tmp_dir(const char *tmp_dir)
{
    s_tmp_dir = strdup(tmp_dir);

    /* remove trailing path separator, if one is present */
    if (s_tmp_dir[strlen(s_tmp_dir) - 1] == DIR_SEPARATOR) {
      s_tmp_dir[strlen(s_tmp_dir) - 1] = '\0';
    }

}

/* Getting the temporary directory.                    */
/* You may want to use fopen_tmp_file instead, though. */
const char *
get_asf_tmp_dir()
{
  if (!s_tmp_dir) {

    // default to current directory
    s_tmp_dir = strdup(".");

  }

  return s_tmp_dir;
}

// internal function, prepending tmp dir to a filename
// caller must free returned memory
static char * full_tmp_name (const char *filename)
{
  char * full_name;
  const char * tmp_dir;

  tmp_dir = get_asf_tmp_dir();
  full_name = (char *) malloc (sizeof(char) *
                                (strlen(filename) + strlen(tmp_dir) + 10));

  sprintf(full_name, "%s%c%s", tmp_dir, DIR_SEPARATOR, filename);
  return full_name;
}


/* return a file pointer to a file opened in the temporary dir */
/* pass in just the name of the file, with no path info        */
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

/* remove a file from the temporary dir */
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
