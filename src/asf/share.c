static char * s_share_dir = 0;
static char * s_bin_dir = 0;
static char * s_argv0 = 0;

#if defined(win32)

static char * s_bin_dir_win = 0;

#include "asf.h"
#include <windows.h>

static const char * s_asf_application_key = "SOFTWARE\\ASF_Tools\\"TOOL_SUITE_NAME"\\";
static const char * s_version_string = TOOL_SUITE_VERSION_STRING;

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
    //   \SOFTWARE\ASF_Tools\<app>\<version>
    char *ver = STRDUP(s_version_string);

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
            printf("ouch, got an error! %ld: %s\n", dw, (char*)ErrBuf);
            break;
        }
        ++ValEnumIndex;
    } while (1);

    free(app_key);
    //sleep(10);
    RegCloseKey(Hkey);
}

#else

#include <stdio.h>
#include <stdlib.h>

static int check_for_version_file_in_share_dir(const char *dir)
{
  const char *version_file = TOOL_SUITE_SHARE_DIR "_version.txt";

  int len = strlen(dir) + strlen(version_file) + 3;
  if (len < 256) len = 256;

  char *file = MALLOC(len * sizeof(char));
  sprintf(file, "%s/%s", dir, lc(version_file));

  int found = 0;
  char *ver = TOOL_SUITE_VERSION_STRING;

  FILE *f = fopen(file, "r");
  if (f) {
    if (fgets(file, len, f)) {
      if (strncmp(file, ver, strlen(ver)) == 0)
	found = 1;
    }
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
          
    s_bin_dir = STRDUP(str_value_fixed);
    //printf("bin_dir : %s\n", s_bin_dir);
#else

      /* on UNIX, assume ASF_INSTALL_DIR has been set by the configure */
      /* script -- in config.h                                         */

    s_bin_dir = STRDUP(ASF_BIN_DIR);

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
		s_bin_dir = STRDUP(buf);
		found = 1;
		//printf("  Found in %s!\n", buf);
		break;
	    }

	    //printf("  Not found in %s.\n", buf);
	    p = q;
	}
	while (*p++ != '\0');

	if (!found)
	    printf("Using default bin dir: %s\n", s_bin_dir);

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

// returns a windows-friendly version of the binary dir (no "/cygdrive/...")
// for compatibility with CreateProcess()
const char *
get_asf_bin_dir_win()
{
#if defined(win32)
  if (!s_bin_dir_win) {

    /* on windows, pull the install dir from the registry */
    char str_value[REG_VALUE_SIZE];
    get_string_from_registry(s_asf_install_dir_key, str_value);          
    s_bin_dir_win = STRDUP(str_value);

    /* remove trailing path separator, if one is present */    
    if (s_bin_dir_win[strlen(s_bin_dir_win) - 1] == DIR_SEPARATOR)
      s_bin_dir_win[strlen(s_bin_dir_win) - 1] = '\0';
  }

  return s_bin_dir_win;
#else
  /* normal version will work just fine on non-Win */
  return get_asf_bin_dir();
#endif
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
    s_share_dir = STRDUP(str_value);

#else

      /* on UNIX, assume ASF_SHARE_DIR has been set by the configure */
      /* script -- in config.h                                       */

    s_share_dir = STRDUP(ASF_SHARE_DIR);

    /* See if this works - check for a known file which              */
    /* is in the asf share directory.                                */
    if (!check_for_known_file_in_share_dir(s_share_dir))
    {
        const char path_sep = ':';
        const char *share_p;
	char *path, *buf, *share, *p;
	int found = 0;

	//printf("Known file not in config.h's share dir: %s\n", s_share_dir);
	//printf("Searching the path...\n");

        /* kludgery! Must find the location of the share dir the hard  */
        /* way, which is to search the directories in the user's path, */
        /* go '../share/asf_tools' relative to that, and check for the */
        /* known file.                                                 */

        /* first: it might not be share/asf_tools, get what it really is */
        share_p = strstr(ASF_SHARE_DIR, "share");
        if (!share_p) {
	    /* this is bad... */
	    printf("Bad configure? 'share' not found in default share dir.\n");
	    printf("Using default share dir: %s\n", s_share_dir);
	    return s_share_dir;
	}
        --share_p;
	
        /* Add on the tool name, e.g. share/asf_tools/mapready */
        share = MALLOC(sizeof(char)*(strlen(share_p) + 
                                     strlen(TOOL_SUITE_SHARE_DIR) + 10));
        sprintf(share, "%s/%s", share_p, TOOL_SUITE_SHARE_DIR);

        /* now build the full buffer */
	path = getenv("PATH");
	buf = MALLOC(sizeof(char)*(strlen(path) + strlen(share) + 10));

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

	    //printf("Checking %s ...\n", buf);
	    /* only try this one if it ends with 'bin' */
	    if (strlen(buf) > 3 && strcmp(buf + strlen(buf) - 3, "bin") == 0) {
                *(buf + strlen(buf) - 4) = '\0';
                strcat(buf, share);

		if (check_for_known_file_in_share_dir(buf))
		{
                    free(s_share_dir);
                    s_share_dir = STRDUP(buf);
                    found = 1;
                    //printf("  Found in %s!\n", buf);
                    break;
		}

		//printf("  Not found in %s.\n", buf);
	    }
	    p = q;
	}
	while (*p++ != '\0');

	if (!found)
	    printf("Using default share dir: %s\n", s_share_dir);

	FREE(buf);
	FREE(share);
    }

#endif

      /* remove trailing path separator, if one is present */

    if (s_share_dir[strlen(s_share_dir) - 1] == DIR_SEPARATOR) {
      s_share_dir[strlen(s_share_dir) - 1] = '\0';
    }

    //printf("Share dir: %s\n", s_share_dir);
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
  full_name = (char *) MALLOC (sizeof(char) *
                                (strlen(filename) + strlen(share_dir) + 10));

  sprintf(full_name, "%s%c%s", share_dir, DIR_SEPARATOR, filename);

  fp = fopen(full_name, mode);
  if (!fp)
    asfPrintWarning("Could not open share file: %s\n", filename);

  free(full_name);
  return fp;
}

int share_file_exists(const char *filename)
{
  char * full_name;
  const char * share_dir;

  share_dir = get_asf_share_dir();
  full_name = (char *) MALLOC (sizeof(char) *
                                (strlen(filename) + strlen(share_dir) + 10));

  sprintf(full_name, "%s%c%s", share_dir, DIR_SEPARATOR, filename);

  int exists = fileExists(full_name);
  free(full_name);

  return exists;
}

const char *get_asf_share_dir_with_argv0(const char *argv0)
{
#ifdef win32
    // "realpath" not available on Windows
    char *argv0_real_path = STRDUP(argv0);
#else
    // handle use of "./mapready" etc
    char *argv0_real_path = realpath(argv0, NULL);
    if (!argv0_real_path) {
        // probably user did not specify a path when running MapReady
        // so, can use the normal method for finding the share dir
        return get_asf_share_dir();
    }
#endif

    // strip off the executable, leaving just the path info
    char *argv0_path = get_dirname(argv0_real_path);

    if (!s_argv0)
        s_argv0 = STRDUP(argv0_path);

#ifndef win32
    // If the user specified a path on the command line, we want to
    // try that directory first, when searching for the share dir.
    // If this doesn't work, in get_asf_share_dir(), called below, we will
    // use the normal path-searching method to find the share dir.  If this
    // does work, then the call below will just return what we've already
    // found.
    if (!s_share_dir) {
        // obtain what needs to be pasted on to the location of the bin dir
        // to get to the share dir (i.e. "share/asf_tools")
        char *share = strstr(ASF_SHARE_DIR, "share");
        if (share) {
            if (argv0_path && strlen(argv0_path) > 0) {
                // a copy for us to change "whatever/bin" to "whatever/share/asf_tools"
                char *buf = MALLOC(sizeof(char)*(strlen(argv0_path) + strlen(share) + strlen(TOOL_SUITE_SHARE_DIR) + 5));
                strcpy(buf, argv0_path);

                // only try this if the binary location ends with 'bin'
                if (strcmp(buf + strlen(buf) - 4, "bin/") == 0) {
                    // strip off "bin" - add "share/asf_tools"
                    *(buf + strlen(buf) - 4) = '\0';
                    strcat(buf, share);
                    strcat(buf, "/");
                    strcat(buf, TOOL_SUITE_SHARE_DIR);
                    if (check_for_known_file_in_share_dir(buf)) {
                        // this is the one!
                        s_share_dir = STRDUP(buf);
                    }
                }
                FREE(buf);
            }
        }
    }
#endif

    FREE(argv0_path);
    FREE(argv0_real_path);
    return get_asf_share_dir();
}

const char *get_argv0()
{
    return s_argv0;
}
