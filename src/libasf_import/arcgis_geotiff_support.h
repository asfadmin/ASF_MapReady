#ifndef _ARCGIS_GEOTIFF_SUPPORT_H_
#define _ARCGIS_GEOTIFF_SUPPORT_H

#define DHFA_UNKNOWN_PROJECTION (-1)

short getArcgisProjType(const char *auxFile);
int isArcgisGeotiff(const char *inFile);
void readArcgisAuxProjectionParameters(const char *inFile, meta_projection *mp);

#endif // _ARCGIS_GEOTIFF_SUPPORT_H_
