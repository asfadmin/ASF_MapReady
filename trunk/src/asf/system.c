#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef win32
#include <windows.h>
#endif

int
asfSystem(const char *format, ...)
{
  va_list ap;
  char cmd[4096];

  va_start(ap, format);
  vsnprintf(cmd, 4095, format, ap);

  //printf("Running system command: %s\n", cmd);

#ifdef win32

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
      DWORD dw = GetLastError();
      //printf( "CreateProcess failed (%ld)\n", dw );

      LPVOID lpMsgBuf;
      FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),
        (LPTSTR)&lpMsgBuf,
        0,
        NULL);

      printf("CreateProcess() failed with error %ld: %s\n",
        dw, (char*)lpMsgBuf);
      printf("Failed command: %s\n", cmd);

      return -1;
  }
  return 0;

#else

  int ret = system(cmd);

  if (ret != 0) {
    if (errno != 0)
      printf("Error running command %d: %s\n", errno, strerror(errno));
    else
      printf("Error running command.\n");
  }
  return ret;

#endif
}
