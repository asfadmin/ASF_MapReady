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
gchar *
find_in_path(gchar * file)
{
  gchar *path, *buf, *name, *p;
  int len, pathlen;

  /* first see if file is in current directory */
//  if (g_file_test(file, G_FILE_TEST_EXISTS))
//  {
//    return g_strdup(file);
//  }

  path = (gchar *)g_getenv("PATH");

  len = strlen(file) + 1;
  pathlen = strlen(path);

  /* work area */
  buf = (gchar *) g_malloc( sizeof(gchar) * (pathlen + len + 2) ); 

  /* put separator + filename at the end of the buffer */
  name = buf + pathlen + 1;
  *name = DIR_SEPARATOR;
  memcpy(name + 1, file, len);

  /* now try each path item, prepended to the filename in the work area */
  p = path;
  do
  {
    gchar * start;
    gchar * q = strchr(p + 1, PATH_SEPARATOR);

    /* if separator not found, point to the end */
    if ( !q ) 
      q = path + pathlen;

    start = name - (q - p);

    /* copy path portion to the work area */
    memcpy( start, p, q - p );

    if (g_file_test( start, G_FILE_TEST_EXISTS ))
    {
      gchar * ret = g_strdup(start);
      g_free(buf);
      return ret; 
    }

    p = q;
  } 
  while (*p++ != '\0');

  /* not found! */ 
  g_free(buf);
  return NULL;
}

gchar *
find_dir_in_path(gchar * dir)
{
  gchar *path, *buf, *name, *p;
  int len, pathlen;

  /* first see if file is in current directory */
  if (g_file_test(dir, G_FILE_TEST_IS_DIR))
  {
    return g_strdup(dir);
  }

  path = (gchar *)g_getenv("PATH");

  len = strlen(dir) + 1;
  pathlen = strlen(path);

  /* work area */
  buf = (gchar *) g_malloc( sizeof(gchar) * (pathlen + len + 2) ); 

  /* put separator + filename at the end of the buffer */
  name = buf + pathlen + 1;
  *name = DIR_SEPARATOR;
  memcpy(name + 1, dir, len);

  /* now try each path item, prepended to the filename in the work area */
  p = path;
  do
  {
    gchar * start;
    gchar * q = strchr(p + 1, PATH_SEPARATOR);

    /* if separator not found, point to the end */
    if ( !q ) 
      q = path + pathlen;

    start = name - (q - p);

    /* copy path portion to the work area */
    memcpy( start, p, q - p );

    if (g_file_test( start, G_FILE_TEST_IS_DIR ))
    {
      gchar * ret = g_strdup(start);
      g_free(buf);
      return ret; 
    }

    p = q;
  } 
  while (*p++ != '\0');

  /* not found! */ 
  g_free(buf);
  return NULL;
}
