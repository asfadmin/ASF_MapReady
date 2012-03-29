#ifndef ASF_JPEG_H
#define ASF_JPEG_H

#include "asf.h"
#include "asf_tiff.h" // for "uint32"

#ifdef HAVE_STDLIB_H
#undef HAVE_STDLIB_H
#endif

#ifdef cygwin
#ifdef HAVE_BOOLEAN
#undef HAVE_BOOLEAN
#endif
#endif

#ifdef win32
#define INT32 __int32
typedef int boolean;
#define HAVE_BOOLEAN
#endif

#include <jpeglib.h>

#ifdef win32
#undef INT32
#endif

typedef struct {
  uint32 byte_width;
  uint32 width;
  uint32 height;
  data_type_t data_type; // ASF data type
  int num_bands; // Number of color elements
} jpeg_info_t;

#ifndef MIN_DIMENSION
#define MIN_DIMENSION (16)
#endif

#endif
