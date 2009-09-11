#include "asf_fork.h"

#if defined(win32)

#define BYTE __byte
#include "asf_meta.h"
#undef BYTE

#include <windows.h>

typedef DWORD WINAPI winForkFunc(void *);

#else

#include "asf_meta.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#endif

int asfFork(forkFunc child_fn, void *child_params,
            forkFunc parent_fn, void *parent_params)
{

#ifdef win32

  winForkFunc *f = (winForkFunc *)child_fn;

  DWORD id;
  HANDLE h = CreateThread(
      NULL,                        // default security attributes
      0,                           // default stack size
      f,                           // thread function name
      child_params,                // argument to thread function
      0,                           // default creation flags
      &id);                        // returns the thread id

  DWORD dwWaitResult;

  do {
    parent_fn(parent_params);
    dwWaitResult = WaitForSingleObject(h, 50);
  }
  while (dwWaitResult == WAIT_TIMEOUT);

#else

  int pid = fork();
  if (pid == 0)
  {
    // this is the child
    int ret = child_fn(child_params);

    exit(ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
  }
  else
  {
    // this is the parent
    while (waitpid(-1, NULL, WNOHANG) == 0)
    {
      parent_fn(parent_params);
    }
  }

#endif

  return 0;
}
