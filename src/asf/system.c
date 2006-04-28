#include <stdarg.h>
#include <errno.h>

int
asfSystem(const char *format, ...)
{
  va_list ap;
  char cmd[4096];

  va_start(ap, format);
  vsprintf(cmd, format, ap);

  asfPrintStatus("Running system commamd: %s\n", cmd);

  int ret = system(cmd);
  
  if (ret != 0) {
    asfPrintError("Error running command %d: %s\n", errno, strerror(errno));
  }
  return ret;
}
