#ifndef ASF_JPEG_H
#define ASF_JPEG_H

#ifdef win32
#define BYTE __byte
#endif

#include "asf.h"
#include "asf_tiff.h" // for "boolean"

#ifdef HAVE_STDLIB_H
#undef HAVE_STDLIB_H
#endif

#ifdef cygwin
#ifdef HAVE_BOOLEAN
#undef HAVE_BOOLEAN
#endif
#endif

#include <jpeglib.h>

typedef struct {
  uint32 byte_width;
  uint32 width;
  uint32 height;
  data_type_t data_type; // ASF data type
  int num_bands; // Number of color elements
} jpeg_info_t;

#ifdef win32
#undef BYTE
#endif

#ifndef MIN_DIMENSION
#define MIN_DIMENSION (16)
#endif

#endif
