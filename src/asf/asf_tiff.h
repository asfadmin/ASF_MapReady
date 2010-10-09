#ifndef ASF_TIFF_H
#define ASF_TIFF_H

#ifdef win32
#define BYTE __byte
#define POINT __point
#endif

#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>

#ifdef win32
#undef BYTE
#undef POINT
#endif

#ifndef MIN_DIMENSION
#define MIN_DIMENSION (16)
#endif

#endif
