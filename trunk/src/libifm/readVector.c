/****************************************************************
FUNCTION NAME: readVector

SYNTAX:  void readVector(v,fnm,type,n);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		void *		pointer to memory for incoming data
    fnm		char *		source filename
    type	data_t		data type
    n		int		number of elements

DESCRIPTION:
	read an n element vector from a binary file to the data structure
	pointed to by v. 

RETURN VALUE:
 	A filled data structure v.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
	1.0 - R. Fatland & M. Shindle - Original Code
****************************************************************/

#include "ifm.h"
#include <unistd.h>
#include <fcntl.h>

void readVector(void *v, char *fnm, data_t type, int n)
{
  int fd, i, Size;

  fd = open(fnm, 0);
  if (fd < 3) { 
    Exit("readVector():  error opening '%s'", fnm);
  }

  Size = n*checkDataType(type);
  i = read (fd, (char *)(v), Size);
  if (i != Size) 
    Exit ("readVector(): bad read from file '%s'", fnm);
  close(fd);
  return;
}
