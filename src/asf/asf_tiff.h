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
#include <asf.h>

#define TIFFTAG_ASF_INSAR_METADATA  42112
#define TIFFTAG_ASF_DEM_METADATA  42112
#define N(a)  (sizeof (a) / sizeof (a[0]))

#ifdef win32
#undef BYTE
#undef POINT
#endif

#ifndef MIN_DIMENSION
#define MIN_DIMENSION (16)
#endif

#endif

#ifdef  MAX_RGB
#undef  MAX_RGB
#endif
#define MAX_RGB             255
void _XTIFFInitialize(void);
