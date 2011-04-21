#ifndef _GEOTIFF_SUPPORT_H
#define _GEOTIFF_SUPPORT_H

#ifndef UNKNOWN_SAMPLE_FORMAT
#define UNKNOWN_SAMPLE_FORMAT -1
#endif

#include "libasf_proj.h"

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

int PCS_2_UTM(short pcs, char *hem, datum_type_t *datum, unsigned long *zone);
int get_tiff_data_config(TIFF *tif, short *sample_format, 
			 short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, short *num_bands,
                         int *is_scanline_format, int *is_palette_color_tiff, 
			 report_level_t report_level);
int get_bands_from_citation(int *num_bands, char **band_str, int *empty, 
			    char *citation, int num_expected);
void get_tiff_type(TIFF *tif, tiff_type_t *tiffInfo);
void ReadScanline_from_TIFF_Strip(TIFF *tif, tdata_t buf, unsigned long row, 
				  int band);
void ReadScanline_from_TIFF_TileRow(TIFF *tif, tdata_t buf, unsigned long row, 
				    int band);
int isGeotiff(const char *file);

#endif
