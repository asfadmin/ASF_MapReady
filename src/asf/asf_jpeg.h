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

#ifdef win32
#undef BYTE
#endif

#endif
