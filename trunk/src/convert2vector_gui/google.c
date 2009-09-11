#ifdef win32

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "c2v.h"

#ifdef win32
static char * escapify(const char * s)
{
    int i,j;
    char * ret = MALLOC(2*strlen(s)*sizeof(char));
    for (i = 0, j = 0; i <= strlen(s); ++i)
    {
        switch(s[i])
        {
            case '\\':
                ret[j] = ret[j+1] = s[i];
                ++j;
                break;
            default:
                ret[j] = s[i];
                break;
        }
        ++j;
    }

    return ret;
}
#else
static char *find_in_path(char * file)
{
    char *path, *buf, *name, *p;
    int len, pathlen;

    /* first see if file is in current directory */
    if (fileExists(file))
        return STRDUP(file);

    path = (gchar *)g_getenv("PATH");

    len = strlen(file) + 1;
    pathlen = strlen(path);

    /* work area */
    buf = MALLOC( sizeof(char) * (pathlen + len + 2) ); 

    /* put separator + filename at the end of the buffer */
    name = buf + pathlen + 1;
    *name = '/';
    memcpy(name + 1, file, len);

    /* now try each path item, prepended to the filename in the work area */
    p = path;
    do
    {
        char * start;
        char * q = strchr(p + 1, ':');

        /* if separator not found, point to the end */
        if ( !q ) 
            q = path + pathlen;

        start = name - (q - p);

        /* copy path portion to the work area */
        memcpy( start, p, q - p );

        if (fileExists(start))
        {
            char * ret = STRDUP(start);
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
#endif

void open_in_google_earth(const char *kml_file)
{
    char *ge;

#ifdef win32
    char path[1024];
    char *dirname = get_dirname(kml_file);
    FindExecutable((LPCTSTR)kml_file, (LPCTSTR)dirname, (LPTSTR)path);
    ge = escapify(path);
    printf("Path to google earth: %s\n", ge);
    free(dirname);

    char cmd[1024];
    assert(strlen(ge)+strlen(kml_file)<1000);
    sprintf(cmd, "\"%s\" \"%s\"", ge, kml_file);

    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    memset(&si, 0, sizeof(si));
    memset(&pi, 0, sizeof(pi));
    si.cb = sizeof(si);

    if( !CreateProcess( NULL,   // No module name (use command line)
            cmd,                // Command line
            NULL,               // Process handle not inheritable
            NULL,               // Thread handle not inheritable
            FALSE,              // Set handle inheritance to FALSE
            0,                  // No creation flags
            NULL,               // Use parent's environment block
            NULL,               // Use parent's starting directory 
            &si,                // Pointer to STARTUPINFO structure
            &pi )               // Pointer to PROCESS_INFORMATION structure
            ) 
    {
        message_box("Error running Google Earth:\n"
                    "CreateProcess failed (%ld)\n", GetLastError());
        return;
    }
#else
    ge = find_in_path("googleearth");
    if (!ge)
    {
       message_box("Couldn't find googleearth! Is it installed?");
       return;
    }

    int pid = fork();
    if (pid == 0) {
        asfSystem("\"%s\" \"%s\"", ge, kml_file);
        exit(EXIT_SUCCESS);
    }
#endif
}

void open_in_excel(const char *csv_file)
{
    char *excel;

#ifdef win32

    char path[1024];
    char *dirname = get_dirname(csv_file);
    FindExecutable((LPCTSTR)csv_file, (LPCTSTR)dirname, (LPTSTR)path);
    excel = escapify(path);
    printf("Path to csv application: %s\n", excel);
    free(dirname);

    char cmd[1024];
    assert(strlen(excel)+strlen(csv_file)<1000);
    sprintf(cmd, "\"%s\" \"%s\"", excel, csv_file);

    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    memset(&si, 0, sizeof(si));
    memset(&pi, 0, sizeof(pi));
    si.cb = sizeof(si);

    if( !CreateProcess( NULL,   // No module name (use command line)
            cmd,                // Command line
            NULL,               // Process handle not inheritable
            NULL,               // Thread handle not inheritable
            FALSE,              // Set handle inheritance to FALSE
            0,                  // No creation flags
            NULL,               // Use parent's environment block
            NULL,               // Use parent's starting directory 
            &si,                // Pointer to STARTUPINFO structure
            &pi )               // Pointer to PROCESS_INFORMATION structure
            ) 
    {
        message_box("Error running external application:\n"
                    "CreateProcess failed (%ld)\n", GetLastError());
        return;
    }
#else
    // hmm, what to do on linux?
    excel = find_in_path("gnumeric");
    if (!excel)
    {
       message_box("Couldn't find gnumeric! Is it installed?");
       return;
    }

    int pid = fork();
    if (pid == 0) {
        asfSystem("\"%s\" \"%s\"", excel, csv_file);
        exit(EXIT_SUCCESS);
    }
#endif
}
