/* SccsId[]= @(#)misc.c	2.41 3/24/98 */
static char sccsid_misc[]= "@(#)PPmisc.c:2.41";


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "externs.h"
#include "error.h"

/******************************************************************/
void info_handler(int error_code, 
                  char *file,
                  char *fmt, ...)
{
  va_list args;
  char string[500];
  FILE *fp;

  va_start(args, fmt);
  vsprintf(string, fmt, args);
  va_end(args);

  if (file) {
    if ((fp=fopen(error_file, "w")) == NULL) {
      fprintf(stderr, "error: open error for %s\n", error_file);
      fflush(stderr);
      exit(ierr_2);
    }
    else {
      fprintf(fp, "%s\n", file);
      fclose(fp);
    }
  }
		
  if (!error_code) {
    fprintf(stderr, "info: %s\n", string);
    fflush(stderr);
    return;
  }
  else {
    fprintf(stderr, "error: %s\n", string);
    fflush(stderr);
    exit(error_code);
  }
}


/******************************************************************/
void Fill_str( char *string, int len, char *fmt, ... )
{
  va_list args;
  int l;

  va_start(args,fmt);
  vsprintf(string,fmt,args);
  l = strlen(string);
  if ( l < len ) memset(&string[l],' ',len-l);
  string[len-1] = '\0';
  va_end(args);
}

/******************************************************************/
void unix_command(char *fmt, ...)
{
  char string[500];
  int rc;
  va_list args;

  va_start(args, fmt);
  vsprintf(string, fmt, args);
  va_end(args);

  rc = system(string);

  rc = rc/256;

  if (rc != 0)
    info_handler(ierr_22, "", "unix command failed");

  return;
}
