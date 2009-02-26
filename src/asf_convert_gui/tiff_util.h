#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <geotiff_support.h>

int read_tiff(const char *filename, int *nlines, int *nsamps,
              unsigned char **data);
