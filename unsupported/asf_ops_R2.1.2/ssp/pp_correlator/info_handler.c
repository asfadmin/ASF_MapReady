/* SccsId[]= @(#)info_handler.c	2.41 3/24/98 */
static char sccsid_info_handler[]= "@(#)PPinfo_handler.c:2.41";

#include "pp.h"
#include <stdarg.h>

void info_handler(int error_code, 
                  char *file,
                  char *fmt, ...)
{
  /* if error code > 0, print error message in *fmt, and exit */
  /* if *file != NULL, also write string file to error_file defined globally*/
  /* if error code == 0, print message and return */

  va_list args;
  char string[500];
  FILE *fp;

  va_start(args, fmt);
  vsprintf(string, fmt, args);
  va_end(args);
  

  /* write to error_file if file pointer is not NULL */
  if (file) {    
    if ((fp=fopen(error_file, "w")) == NULL) {
      fprintf(stderr, "error: open error for %s\n", error_file);
      fflush(stderr);
    } else {
      fprintf(fp, "%s\n", file);
      fclose(fp);
    }
  }

  if (!error_code) {
    fprintf(stderr, "info: %s\n", string);
    fflush(stderr);
    return;
  } else {
    fprintf(stderr, "error: %s\n", string);
    fflush(stderr);
    MPI_Abort(MPI_COMM_WORLD,error_code);
    exit(error_code);
  }
}



