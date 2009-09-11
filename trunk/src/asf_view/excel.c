#ifdef win32
#include <windows.h>
#include <assert.h>
#include <stdio.h>

extern const char *detect_csv_assoc(void);

void open_excel(const char *csv_file)
{
    const char *csv_app = detect_csv_assoc();
    if (csv_app && strlen(csv_app) > 0) {
        printf("Opening '%s' with external application  '%s'\n",
                       csv_file, csv_app);

        char cmd[1024];
        assert(strlen(csv_app)+strlen(csv_file)<1000);
        sprintf(cmd, "\"%s\" \"%s\"", csv_app, csv_file);

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
                printf( "CreateProcess failed (%ld)\n", GetLastError() );
                return;
        }
    }
}
#endif
