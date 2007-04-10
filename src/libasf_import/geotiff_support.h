#ifndef _IMPORT_ARCGIS_GEOTIFF_H_
#define _IMPORT_ARCGIS_GEOTIFF_

int PCS_2_UTM (short pcs, char *hem, datum_type_t *datum, unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);

#endif // _IMPORT_ARCGIS_GEOTIFF_H_
