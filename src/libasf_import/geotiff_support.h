#ifndef _IMPORT_ARCGIS_GEOTIFF_H_
#define _IMPORT_ARCGIS_GEOTIFF_

#define UNKNOWN_SAMPLE_FORMAT -1
int PCS_2_UTM (short pcs, char *hem, datum_type_t *datum, unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);
int get_tiff_data_config(TIFF *tif,
                         short *sample_format, short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, int *num_bands,
                         int *is_scanline_format);

#endif // _IMPORT_ARCGIS_GEOTIFF_H_
