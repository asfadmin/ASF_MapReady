/****************************************************************
FUNCTION NAME:  fileExist

SYNTAX:
        int fileExist(char *f)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
      f          char *          filename to check

DESCRIPTION:
     Checks to see if a file exists and returns either TRUE or FALSE.

RETURN VALUE:
     TRUE - if file exists
     FALSE - if file does not exist

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   VER.    AUTHOR:      CHANGES:
   -------------------------------------------------------------------
   1.0     Rob Fatland   Original Devlopment
   1.1     M. Shindle    Redesigned into present file form

****************************************************************/
#include "asf.h"

#include <unistd.h>
#include <fcntl.h>

int fileExist(char *f)
{
  int fd;
  int stat;

  fd = open(f, 0);
  
  if (fd < 3)
   stat = FALSE;
  else
   stat = TRUE;
 
  close(fd);
  return stat;
}

