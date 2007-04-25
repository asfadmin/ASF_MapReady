
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

static const char * s_asf_application_key = "SOFTWARE\\ASF_Tools\\";

static const char * s_asf_share_dir_key = "Install_Dir";
static const char * s_asf_install_dir_key = "Install_Dir";

#else

#include "asf.h"
#include "config.h"

#endif

#if defined(win32)

#define REG_VALUE_SIZE 512

LONG 
get_string_from_registry_ex(const char *folder, const char * key, char * str_value)
{
    HKEY Hkey;
    unsigned long read_size;
    LONG rv;

    rv = RegOpenKeyEx(HKEY_LOCAL_MACHINE, folder, 0, KEY_QUERY_VALUE, &Hkey);
    
    read_size = 512;
    rv = RegQueryValueEx(Hkey, key, 0, 0, (BYTE*)str_value, &read_size);

    if (rv == ERROR_PATH_NOT_FOUND || rv == ERROR_FILE_NOT_FOUND ||
        rv == ERROR_INVALID_HANDLE) {
      RegCloseKey(Hkey);
      rv = RegOpenKeyEx(HKEY_CURRENT_USER, folder, 0, KEY_QUERY_VALUE, &Hkey);
      rv = RegQueryValueEx(Hkey, key, 0, 0, (BYTE*)str_value, &read_size);
    }

    if (rv != ERROR_SUCCESS) {
      LPVOID ErrBuf;
      DWORD dw = GetLastError();
      FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM,
                    0, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &ErrBuf, 0, 0);
      LocalFree(ErrBuf);
      strcpy(str_value, "");
    }
    RegCloseKey(Hkey);

    return rv;
}

static char *form_asf_application_key()
{
    // form the registry key entry:
    //   \SOFTWARE\ASF_Tools\<version>
    char *ver = STRDUP(CONVERT_PACKAGE_VERSION_STRING);

    // strip out all but "major.minor"
    char *p = strchr(ver, '.');
    if (p) p = strchr(p+1, '.');
    if (p) *p = '\0';

    char *app_key =
        MALLOC(sizeof(char)*(strlen(s_asf_application_key)+10));
    sprintf(app_key, "%s%s\\", s_asf_application_key, ver);

    return app_key;
}

LONG 
get_string_from_registry(const char * key, char * str_value)
{
    HKEY Hkey;
    unsigned long read_size;
    LONG rv;
    char *app_key;

    app_key = form_asf_application_key();

    rv = RegOpenKeyEx(HKEY_LOCAL_MACHINE, app_key, 0,
                      KEY_QUERY_VALUE, &Hkey);
    
    read_size = REG_VALUE_SIZE;
    rv = RegQueryValueEx(Hkey, s_asf_share_dir_key, 0, 0, (BYTE*)str_value, 
                         &read_size);

    if (rv == ERROR_PATH_NOT_FOUND || rv == ERROR_FILE_NOT_FOUND ||
        rv == ERROR_INVALID_HANDLE) {
      RegCloseKey(Hkey);
      rv = RegOpenKeyEx(HKEY_CURRENT_USER, app_key, 0, KEY_QUERY_VALUE, &Hkey);
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
      LocalFree(ErrBuf);
      strcpy(str_value, "");
    }
    RegCloseKey(Hkey);

    free(app_key);
    return rv;
}

void
print_all_reg_vals()
{
    int ValEnumIndex=0;
    LONG Err;
    char ValName[512];
    DWORD size;
    unsigned char * Buff = NULL;
    DWORD BuffSize;
    DWORD VarType;
    HKEY Hkey;
    char *app_key;

    app_key = form_asf_application_key();

    size = 512;
    BuffSize = 512;

    RegOpenKeyEx(HKEY_CURRENT_USER, app_key, 0, KEY_QUERY_VALUE, &Hkey);

    do {
        Err = RegEnumValue(Hkey, ValEnumIndex, ValName, &size, NULL, &VarType,
            Buff, &BuffSize);
        if (Err == ERROR_NO_MORE_ITEMS) break;
        if (Err == ERROR_SUCCESS) {
            printf("%s : %s\n", ValName, Buff);
        } else {
            LPVOID ErrBuf;
            DWORD dw = GetLastError();
            FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_FROM_SYSTEM,
                0, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR) &ErrBuf, 0, 0);
            printf("ouch, got an error! %d: %s\n", dw, ErrBuf);
            break;
        }
        ++ValEnumIndex;
    } while (1);

    free(app_key);
    sleep(10);
    RegCloseKey(Hkey);
}

#else

#include <stdio.h>
#include <stdlib.h>

static int check_for_version_file_in_share_dir(const char *dir)
{
  const char *version_file = "convert_version.txt";

  int len = strlen(dir) + strlen(version_file) + 3;
  if (len < 256) len = 256;

  char *file = MALLOC(len * sizeof(char));
  sprintf(file, "%s/%s", dir, version_file);

  int found = 0;
  char *ver = CONVERT_PACKAGE_VERSION_STRING;

  FILE *f = fopen(file, "r");
  if (f) {
    fgets(file, len, f);
    if (strncmp(file, ver, strlen(ver)) == 0)
      found = 1;
    fclose(f);
  }

  FREE(file);
  return found;
}

static int check_for_known_file_in_dir(const char *dir, const char *known_file)
{
    char * file = (char *)MALLOC(strlen(dir) + strlen(known_file) + 3);
    sprintf(file, "%s/%s", dir, known_file);

    int found = 0;

    FILE *f = fopen(file, "rt");
    if (f) {
      found = 1;
      fclose(f);
    }

    FREE(file);
    return found;
}

static int check_for_known_file_in_share_dir(const char *dir)
{
    return check_for_version_file_in_share_dir(dir);
}

static int check_for_known_file_in_bin_dir(const char *dir)
{
    const char * known_file = "asf_import";
    return check_for_known_file_in_dir(dir, known_file);
}

#endif

const char *
get_asf_bin_dir()
{
  if (!s_bin_dir) {

#if defined(win32)

      /* on windows, pull the install dir from the registry */

    char str_value[REG_VALUE_SIZE];
    char str_value_fixed[REG_VALUE_SIZE];
    int i,j;
 
    get_string_from_registry(s_asf_install_dir_key, str_value);
    for (i = j = 0; i <= strlen(str_value); ++i, ++j) {
      if (i == 0) {
        char drive = str_value[0];
        char colon = str_value[1];
        char backslash = str_value[2];
        if (colon == ':' && backslash == '\\') {
          sprintf(str_value_fixed, "/cygdrive/%c/", tolower(drive));
          i += 3; j += strlen(str_value_fixed);
        }
      }
      switch (str_value[i]) {
        //case ' ':
        //  str_value_fixed[j] = '\\';
        //  str_value_fixed[j+1] = ' ';
        //  ++j;
        //  break;
        //case '\\':
        //  str_value_fixed[j] = '/';
        //  break;
        default:
          str_value_fixed[j] = str_value[i];
          break;
      }
    }
          
    s_bin_dir = strdup(str_value_fixed);
    //printf("bin_dir : %s\n", s_bin_dir);
#else

      /* on UNIX, assume ASF_INSTALL_DIR has been set by the configure */
      /* script -- in config.h                                         */

    s_bin_dir = strdup(ASF_BIN_DIR);

    /* See if this works - check for a known file which              */
    /* is in the asf binaries directory.                             */
    if (!check_for_known_file_in_bin_dir(s_bin_dir))
    {
        const char path_sep = ':';
	char *path, *buf, *p;
	int found = 0;

	//printf("Known file not in config.h's bin dir: %s\n", s_bin_dir);
	//printf("Searching the path...\n");

	path = getenv("PATH");	
	buf = MALLOC(strlen(path));

	p = path;
	do {
	    char * q = strchr(p + 1, path_sep);
	    if (!q) q = path + strlen(path); /* last item in path */

	    int i;
	    for (i = 0; i < q - p; ++i)
	        buf[i] = p[i];
	    buf[i] = '\0';

	    //printf("Checking %s ...\n", buf);

	    if (check_for_known_file_in_bin_dir(buf))
	    {
	        free(s_bin_dir);
		s_bin_dir = strdup(buf);
		found = 1;
		//printf("  Found in %s!\n", buf);
		break;
	    }

	    //printf("  Not found in %s.\n", buf);
	    p = q;
	}
	while (*p++ != '\0');

	if (!found)
	    printf("Falling back to default bin dir: %s\n", s_bin_dir);

	FREE(buf);
    }

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
  //print_all_reg_vals();

  if (!s_share_dir) {

#if defined(win32)

      /* on windows, pull the share dir from the registry */

    char str_value[REG_VALUE_SIZE];
    get_string_from_registry(s_asf_share_dir_key, str_value);
    s_share_dir = strdup(str_value);

#else

      /* on UNIX, assume ASF_SHARE_DIR has been set by the configure */
      /* script -- in config.h                                       */

    s_share_dir = strdup(ASF_SHARE_DIR);

    /* See if this works - check for a known file which              */
    /* is in the asf share directory.                                */
    if (!check_for_known_file_in_share_dir(s_share_dir))
    {
        const char path_sep = ':';
	char *path, *buf, *share, *p;
	int found = 0;

	// printf("Known file not in config.h's share dir: %s\n", s_share_dir);
	// printf("Searching the path...\n");

        /* kludgery! Must find the location of the share dir the hard  */
        /* way, which is to search the directories in the user's path, */
        /* go '../share/asf_tools' relative to that, and check for the */
        /* known file.                                                 */

        /* first: it might not be share/asf_tools, get what it really is */
        share = strstr(ASF_SHARE_DIR, "share");
        if (!share) {
	    /* this is bad... */
	    printf("Bad configure? 'share' not found in default share dir.\n");
	    printf("Falling back to default share dir: %s\n", s_share_dir);
	    return s_share_dir;
	}
        --share;
	
	path = getenv("PATH");	
	buf = MALLOC(strlen(path) + strlen(share) + 10);

	p = path;
	do {
	    char * q = strchr(p + 1, path_sep);
	    if (!q) q = path + strlen(path); /* last item in path */

	    int i;
	    for (i = 0; i < q - p; ++i)
	        buf[i] = p[i];
	    buf[i] = '\0';

        // If path item ends with a separator, pop that off
        if (buf[strlen(buf) - 1] == '/')
            buf[strlen(buf) - 1] = '\0';

	    // printf("Checking %s ...\n", buf);
	    /* only try this one if it ends with 'bin' */
	    if (strcmp(buf + strlen(buf) - 3, "bin") == 0) {
	        *(buf + strlen(buf) - 4) = '\0';
		strcat(buf, share);

		if (check_for_known_file_in_share_dir(buf))
		{
		    free(s_share_dir);
		    s_share_dir = strdup(buf);
		    found = 1;
		    // printf("  Found in %s!\n", buf);
		    break;
		}

		// printf("  Not found in %s.\n", buf);
	    }
	    p = q;
	}
	while (*p++ != '\0');

	if (!found)
	    printf("Falling back to default share dir: %s\n", s_share_dir);

	FREE(buf);
    }

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

