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
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>

#ifdef win32
#undef BYTE
#undef POINT
#endif

#endif
