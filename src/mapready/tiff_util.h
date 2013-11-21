#include "asf_tiff.h"
#include <geotiff_support.h>

int read_tiff(const char *filename, int *nlines, int *nsamps,
              unsigned char **data);
void set_tiff_warning_handler();

