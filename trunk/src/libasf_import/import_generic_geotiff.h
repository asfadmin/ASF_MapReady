#ifndef _IMPORT_GENERIC_GEOTIFF_H_
#define _IMPORT_GENERIC_GEOTIFF_H_

int geotiff_band_image_write(TIFF *tif, meta_parameters *omd,
                             const char *outBaseName, int num_bands,
                             int *ignore, short bits_per_sample,
                             short sample_format, short planar_config);

#ifndef _GEOTIFF_SUPPORT_H_
meta_parameters * read_generic_geotiff_metadata(const char *inFileName,
                             int *ignore, ...);
#endif

#endif // _IMPORT_ARCGIS_GEOTIFF_H_
