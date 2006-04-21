#ifndef FORM_GEOTIFF_NAME_H
#define FORM_GEOTIFF_NAME_H

#include <glib.h>

// Given a file name without an extension (though it might include a
// path), look for a file with an extension appropriate for a TIFF
// file.  Possible extensions are searched for in this order: '.tif',
// '.tiff', '.TIF', '.TIFF'.  If no file with an appropriate extension
// is found, return NULL, otherwise return the file name in new
// GString instance.
GString *
find_geotiff_name (const char *inBaseName);

#endif // FORM_GEOTIFF_NAME_H
