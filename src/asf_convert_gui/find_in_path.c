#include "find_in_path.h"

#ifdef win32
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

/* used g_find_program_in_path from glib as a starting */
/* point to implement this                             */
char *
find_in_path(char * file)
{
  char *path, *buf, *name, *p;
  int len, pathlen;

  path = (char *)g_getenv("PATH");

  len = strlen(file) + 1;
  pathlen = strlen(path);

  /* work area */
  buf = (char *)malloc(pathlen + len + 2); 

  /* put separator + filename at the end of the buffer */
  name = buf + pathlen + 1;
  *name = DIR_SEPARATOR;
  memcpy(name + 1, file, len);

  /* now try each path item, prepended to the filename in the work area */
  p = path;
  do
  {
    char * start;
    char * q = strchr(p + 1, PATH_SEPARATOR);

    /* if separator not found, point to the end */
    if ( !q ) 
      q = path + pathlen;

    start = name - (q - p);

    /* copy path portion to the work area */
    memcpy( start, p, q - p );

    if (g_file_test( start, G_FILE_TEST_EXISTS ))
    {
      char * ret;
      ret = strdup(start);
      free(buf);
      return ret; 
    }

    p = q;
  } 
  while (*p++ != '\0');

  /* not found! */ 
  free(buf);
  return NULL;
}
