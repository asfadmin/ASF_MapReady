#ifndef _GEOTIFF_SUPPORT_H
#define _GEOTIFF_SUPPORT_H

#ifndef UNKNOWN_SAMPLE_FORMAT
#define UNKNOWN_SAMPLE_FORMAT -1
#endif

int PCS_2_UTM (short pcs, char *hem, datum_type_t *datum, unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);
int get_tiff_data_config(TIFF *tif,
                         short *sample_format, short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, int *num_bands,
                         int *is_scanline_format);
int get_bands_from_citation(int *num_bands, char **band_str, int *empty, char *citation);
int tiff_image_band_statistics (TIFF *tif, meta_parameters *omd,
                                meta_stats *stats,
                                int num_bands, int band_no,
                                short bits_per_sample, short sample_format,
                                short planar_config,
                                int use_mask_value, double mask_value);

#endif
