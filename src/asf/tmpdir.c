#include "asf.h"
#include "config.h"
#include <errno.h>

/* static var for holding the temporary directory        */
/* use the get/set methods instead of accessing directly */
static char * s_tmp_dir = 0;

/* Setting the temporary directory */
void
set_asf_tmp_dir(const char *tmp_dir)
{
    s_tmp_dir = STRDUP(tmp_dir);

    /* remove trailing path separator, if one is present */
    if (s_tmp_dir[strlen(s_tmp_dir) - 1] == DIR_SEPARATOR) {
      s_tmp_dir[strlen(s_tmp_dir) - 1] = '\0';
    }

}

/* Getting the temporary directory.                    */
/* You may want to use fopen_tmp_file instead, though. */
const char *get_asf_tmp_dir()
{
  if (!s_tmp_dir) {

#if defined(win32)

    // on windows, pull the share dir from the registry

    char str_value[REG_VALUE_SIZE];
    get_string_from_registry(s_asf_tmp_dir_key, str_value);
    s_tmp_dir = STRDUP(str_value);

#else

    // on UNIX, assume ASF_TMP_DIR has been set by the configure
    // script -- in config.h

    s_tmp_dir = STRDUP(ASF_TMP_DIR);

#endif

    // remove trailing path separator, if one is present

    if (s_tmp_dir[strlen(s_tmp_dir) - 1] == DIR_SEPARATOR) {
      s_tmp_dir[strlen(s_tmp_dir) - 1] = '\0';
    }
  }

  return s_tmp_dir;
}

const char *get_tmp_log_file(char *tool)
{
  char *tmpDir = get_asf_tmp_dir();
  char *tmpLog = (char *) MALLOC(sizeof(char)*(strlen(tmpDir)+strlen(tool)+20));
  sprintf(tmpLog, "%s%c%s%i.log", tmpDir, DIR_SEPARATOR, tool, (int)getpid());
  
  return tmpLog;
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
  int ret=0;

  full_name = full_tmp_name (filename);
  if (fileExists(full_name)) {
    ret = unlink (full_name);
    if (ret < 0) 
      asfPrintWarning("unlink_tmp_file '%s': %s\n", full_name, strerror(errno));
  }
  free(full_name);

  return ret;
}
