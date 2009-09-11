#ifndef _GEOTIFF_SUPPORT_H
#define _GEOTIFF_SUPPORT_H

#ifndef UNKNOWN_SAMPLE_FORMAT
#define UNKNOWN_SAMPLE_FORMAT -1
#endif
typedef enum {
  UNKNOWN_TIFF_TYPE = 0,
  SCANLINE_TIFF,
  STRIP_TIFF,
  TILED_TIFF
} tiff_format_t;
typedef struct {
  tiff_format_t format;
  uint32 scanlineSize;
  uint32 numStrips;
  uint32 rowsPerStrip;
  uint32 tileWidth;
  uint32 tileLength;
  int imageCount;
  int volume_tiff;
} tiff_type_t;
typedef struct {
  short sample_format;
  short bits_per_sample;
  short planar_config;
  short samples_per_pixel;
} tiff_data_config_t;

#include "asf_meta.h"

int PCS_2_UTM (short  pcs, char *hem, datum_type_t *datum,  unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);
int get_tiff_data_config(TIFF *tif,
                         short *sample_format, short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, short *num_bands,
                         int *is_scanline_format, int *is_palette_color_tiff, report_level_t report_level);
int get_bands_from_citation(int *num_bands, char **band_str, int *empty, char *citation, int num_expected);
int tiff_image_band_statistics (TIFF *tif, meta_parameters *omd,
                                meta_stats *stats, int is_dem,
                                int num_bands, int band_no,
                                short bits_per_sample, short sample_format,
                                short planar_config,
                                int use_mask_value, double mask_value);
void get_tiff_type(TIFF *tif, tiff_type_t *tiffInfo);
void ReadScanline_from_TIFF_Strip(TIFF *tif, tdata_t buf, unsigned long row, int band);
void ReadScanline_from_TIFF_TileRow(TIFF *tif, tdata_t buf, unsigned long row, int band);
meta_parameters * read_generic_geotiff_metadata(const char *inFileName,
                             int *ignore, ...);
int isGeotiff(const char *file);

#endif
