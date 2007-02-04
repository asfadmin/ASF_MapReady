#ifndef FIND_ARCGIS_GEOTIFF_AUX_NAME_H
#define FIND_ARCGIS_GEOTIFF_AUX_NAME_H

#include <glib.h>

// Given a file name without an extension (though it might include a
// path), look for a file with an extension appropriate for an ArcGIS GeoTIFF
// metadata (.aux) file.  Possible extensions are searched for in this order:
// '.tif.aux', '.tiff.aux', '.aux', '.TIF.AUX', '.TIFF.AUX', '.AUX.  If no
// file with an appropriate extension is found, return NULL, otherwise return
// the file name in new GString instance.

GString *
find_arcgis_geotiff_aux_name (const char *inBaseName);

#endif // FIND_ARCGIS_GEOTIFF_AUX_NAME
