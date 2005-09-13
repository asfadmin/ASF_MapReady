/* Functions to convert meta->general->data_type field from complex to polar
 * and visa versa. If data is already in the correct format, leave it. */

#include "asf_meta.h"


int meta_polar2complex(int data_type)
{
  int ret;
  if (data_type < COMPLEX_BYTE) {
   switch (data_type) {
     case BYTE:      ret = COMPLEX_BYTE;      break;
     case INTEGER16: ret = COMPLEX_INTEGER16; break;
     case INTEGER32: ret = COMPLEX_INTEGER32; break;
     case REAL32:    ret = COMPLEX_REAL32;    break;
     case REAL64:    ret = COMPLEX_REAL64;    break;
    }
  }
  else
    ret = data_type;
  return ret;
}

int meta_complex2polar(int data_type)
{
  int ret;
  if (data_type > REAL64) {
    switch (data_type) {
      case COMPLEX_BYTE:      ret = BYTE;      break;
      case COMPLEX_INTEGER16: ret = INTEGER16; break;
      case COMPLEX_INTEGER32: ret = INTEGER32; break;
      case COMPLEX_REAL32:    ret = REAL32;    break;
      case COMPLEX_REAL64:    ret = REAL64;    break;
    }
  }
  else
    ret = data_type;
  return ret;
}




