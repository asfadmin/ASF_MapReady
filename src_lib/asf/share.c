
char * strdup(const char *);

static char * s_share_dir = 0;
static char * s_bin_dir = 0;

#if defined(win32)

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <windows.h>

static const char * s_asf_application_key = "Software\\ASF_Tools\\";

static const char * s_asf_share_dir_key = "Share_Dir";
static const char * s_asf_install_dir_key = "Install_Dir";

#else

#include "asf.h"
#include "config.h"

#endif

#if defined(win32)
LONG 
get_string_from_registry(const char * key, char * str_value)
{
    HKEY Hkey;
    unsigned long read_size;
    LONG rv;

    RegOpenKeyEx(HKEY_LOCAL_MACHINE, s_asf_application_key, 0,
                 KEY_QUERY_VALUE, &Hkey);
    
    read_size = sizeof(str_value);
    rv = RegQueryValueEx(Hkey, s_asf_share_dir_key, 0, 0, (BYTE*)str_value, 
                         &read_size);

    if (rv == ERROR_PATH_NOT_FOUND || rv == ERROR_FILE_NOT_FOUND) {
      RegOpenKeyEx(HKEY_CURRENT_USER, s_asf_application_key, 0,
                   KEY_QUERY_VALUE, &Hkey);
      rv = RegQueryValueEx(Hkey, s_asf_share_dir_key, 0, 0, (BYTE*)str_value, 
                           &read_size);
    }

    if (rv != ERROR_SUCCESS) {
      LPVOID ErrBuf;
      DWORD dw = GetLastError();
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM,
                    0, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &ErrBuf, 0, 0);
      printf("error %d: %s\n", dw, ErrBuf);
      LocalFree(ErrBuf);
      strcpy(str_value, "");
    }
      
    RegCloseKey(Hkey);

    return rv;
}

#endif

const char *
get_asf_bin_dir()
{
  if (!s_bin_dir) {

#if defined(win32)

      /* on windows, pull the install dir from the registry */

    char str_value[512];
    get_string_from_registry(s_asf_install_dir_key, str_value);
    s_bin_dir = strdup(str_value);

#else

      /* on UNIX, assume ASF_INSTALL_DIR has been set by the configure */
      /* script -- in config.h                                         */

    s_bin_dir = strdup(ASF_BIN_DIR);

#endif

      /* remove trailing path separator, if one is present */
    
    if (s_bin_dir[strlen(s_bin_dir) - 1] == DIR_SEPARATOR) {
      s_bin_dir[strlen(s_bin_dir) - 1] = '\0';
    }
  }

  return s_bin_dir;
}

const char * 
get_asf_share_dir()
{
  if (!s_share_dir) {

#if defined(win32)

      /* on windows, pull the share dir from the registry */

    char str_value[512];
    get_string_from_registry(s_asf_share_dir_key, str_value);
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

