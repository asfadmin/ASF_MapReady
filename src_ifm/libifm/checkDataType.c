/****************************************************************
FUNCTION NAME:  checkDataType

SYNTAX:  int checkDataType(type);


PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    type        data_t          Type of variable to return size of

DESCRIPTION:
	Returns the size in bytes of the specified data type. 

RETURN VALUE:
	The size in bytes of the user specified data type. If data type is
	not known, return -1.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
    1.0 - Rob Fatland & Mike Shindle - Original Devlopment
****************************************************************/
#include "ifm.h"

int checkDataType(data_t type)
{
  if (type == STRING            ) return sizeof(char);
  if (type == INT               ) return sizeof(int);
  if (type == INTEGER           ) return sizeof(int);
  if (type == SHORT_INT         ) return sizeof(short int);
  if (type == FLOAT             ) return sizeof(float);
  if (type == DOUBLE            ) return sizeof(double);
  if (type == CHAR              ) return sizeof(char);
  if (type == UCHAR             ) return sizeof(Uchar);
  if (type == COMPLEX           ) return sizeof(Complex);
  if (type == FLOAT_COMPLEX     ) return sizeof(FComplex);
  if (type == DOUBLE_COMPLEX    ) return sizeof(DComplex);

  return(-1);
}
