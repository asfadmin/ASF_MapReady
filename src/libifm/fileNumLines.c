/****************************************************************
FUNCTION NAME: fileNumLines

SYNTAX: int fileNumLines(c)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    fname       char *          Pointer to file name

DESCRIPTION:
   Return the number of lines in a given file. The assumption is that we are
   checking an ascii file.

RETURN VALUE:
   Return the number of lines.

SPECIAL CONSIDERATIONS:
   fname must point to an ASCII file.

PROGRAM HISTORY:
   1.0 - Mike Shindle - Original devlopment. Based on code in K&R 2nd
			Edition.
****************************************************************/
#include "asf.h"
#include "ifm.h"

int fileNumLines(char *fname)
{
  int c, nl;
  FILE *fp;

  if (!fileExist(fname)) {
    fprintf(stderr,"fileNumLines(): requested file %s does not exist\n", fname);
    Exit("fileNumLines():  fatal error");
  }

  /* open file */
  fp = FOPEN(fname,"r");
  
  nl = 0;
  while ((c=fgetc(fp)) != EOF)
    if (c == '\n')
      ++nl;
  
  if (nl == 0) 
    Alert("fileNumLines() found file '%s' with 0 lines",fname);

  fclose(fp);
  return nl;
}
