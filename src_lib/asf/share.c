#include "asf.h"
#include "config.h"

char * strdup(const char *);

static char * s_share_dir = 0;

#if defined(win32)
#include <windows.h>
static const char * s_asf_application_key = "Software\\ASF\\";
#endif

const char * 
get_asf_share_dir()
{
  if (!s_share_dir) {

#if defined(win32)

      /* on windows, pull the share dir from the registry */

    HKEY Hkey;
    char str_value[512];
    unsigned long read_size;

    RegCreateKeyEx(HKEY_LOCAL_MACHINE, s_asf_application_key, 0, 0,
		   REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, 0,
		   &HKey, 0);

    read_size = sizeof(str_value);
    RegQueryValueEx(HKey, "ShareDir", 0, 0, (BYTE*)str_value, &read_size);

    RegCloseKey(HKey);

    s_share_dir = strdup(str_value);

#else

      /* on UNIX, assume ASF_SHARE_DIR has been set by the configure */
      /* script -- in config.h                                       */

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

