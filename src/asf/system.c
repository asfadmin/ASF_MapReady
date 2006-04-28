#include <stdarg.h>
#include <errno.h>

#ifdef win32
#include <process.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

int
asfSystem(const char *format, ...)
{
  va_list ap;
  char cmd[4096];
  int ret;

  va_start(ap, format);
  vsprintf(cmd, format, ap);

  //printf("Running system commamd: %s\n", cmd);

#ifdef win32

#ifndef	HAVE_GNU_LD
#define	__environ	environ
#endif

    int pid, save, status;
    struct sigaction sa, intr, quit;

    sa.sa_handler = SIG_IGN;
    sa.sa_flags = 0;
    sigemptyset (&sa.sa_mask);

    if (sigaction (SIGINT, &sa, &intr) < 0)
    {
        return -1;
    }

    if (sigaction (SIGQUIT, &sa, &quit) < 0)
    {
        save = errno;
        (void) sigaction (SIGINT, &intr, (struct sigaction *) NULL);
        errno = save;
        return -1;
    }

    pid = fork();
    if (pid == 0)
    {
        char shell_path[512];
        const char *new_argv[4];

        new_argv[0] = "sh.exe";
        new_argv[1] = "-c";
        new_argv[2] = cmd;
        new_argv[3] = 0;

        (void) sigaction (SIGINT, &intr, (struct sigaction *) NULL);
        (void) sigaction (SIGQUIT, &quit, (struct sigaction *) NULL);

        sprintf(shell_path, "%s/sh", get_asf_bin_dir());
        execve(shell_path, (char * const *) new_argv, __environ);

        exit(127);
    }
    else if (pid < 0)
    {
        /* fork failed */
        return -1;
    }
    else
    {
        int n;
        do 
        {
            n = waitpid(pid, &status, 0);
        }
        while (n == -1 && errno == EINTR);

        if (n != pid)
        {
            status = -1;
        }
    }

    save = errno;
    if (sigaction (SIGINT, &intr, (struct sigaction *) NULL) |
        sigaction (SIGQUIT, &quit, (struct sigaction *) NULL))
    {
        if (errno == ENOSYS)
            errno = save;
        else
            return -1;
    }

    return status;

#else
  ret = system(cmd);
#endif

  if (ret != 0) {
    printf("Error running command %d: %s\n", errno, strerror(errno));
  }
  return ret;
}
