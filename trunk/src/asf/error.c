/************************************
Error function
************************************/

#include "caplib.h"
#include "error.h"
#include "log.h"

char errbuf[255];

void printErr(char *msg)
{
  fprintf(stderr, "%s", msg);
  if (fLog!=NULL) {
    fprintf(fLog, "%s", msg);
    FCLOSE(fLog);
  }
  exit(0);
}
