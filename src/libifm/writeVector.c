/****************************************************************
FUNCTION NAME:  writeVector

SYNTAX:  void writeVector(v,fnm,type,n);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		void *		source vector
    fnm		char *		destination filename
    type	data_t		data type
    n		int		number of elements

DESCRIPTION:
    Saves a vector to the specified file name. If it cannot open the file for
    whatever reason, it will not save the data.

    The file is saved as a binary file.

RETURN VALUE:
    None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Rob Fatland & Mike Shindle - original development
****************************************************************/
#include "asf.h"

#include <unistd.h>
#include <fcntl.h>
#include "ifm.h"

void writeVector (void *v, char *fnm, data_t type, int n)
{
  int fd, i, Size;

  fd = creat(fnm, 0664);
  if (fd < 3) {
    Alert("writeVector(): cannot create or open file '%s'",fnm);
    return;
  }
  Size = n*checkDataType(type);
  i = write (fd, (char *)(v), Size);
  if (i != Size) {
    Alert("writeVector():  bad write to file '%s'",fnm);
  }
  close(fd);
  return;
}
