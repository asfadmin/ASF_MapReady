/* Functions to convert meta->general->data_type field from complex to polar
 * and visa versa. If data is already in the correct format, leave it. */

#include "asf_meta.h"


int meta_polar2complex(int data_type)
{
  if (data_type < COMPLEX_BYTE) {
   switch (data_type) {
     case BYTE:      return COMPLEX_BYTE;      break;
     case INTEGER16: return COMPLEX_INTEGER16; break;
     case INTEGER32: return COMPLEX_INTEGER32; break;
     case REAL32:    return COMPLEX_REAL32;    break;
     case REAL64:    return COMPLEX_REAL64;    break;
     default:        return -1;
    }
  }
  else
    return data_type;
}

int meta_complex2polar(int data_type)
{
  if (data_type > REAL64) {
    switch (data_type) {
      case COMPLEX_BYTE:      return BYTE;      break;
      case COMPLEX_INTEGER16: return INTEGER16; break;
      case COMPLEX_INTEGER32: return INTEGER32; break;
      case COMPLEX_REAL32:    return REAL32;    break;
      case COMPLEX_REAL64:    return REAL64;    break;
      default:        return -1;
    }
  }
  else
    return data_type;
}




