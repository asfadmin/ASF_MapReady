#ifndef _ASF_IMPORT_H_
#define _ASF_IMPORT_H_

#ifndef __ASF_META_H__
#include "asf_meta.h"
#endif

#define TOOLS_META_EXT    ".meta"
#define TOOLS_IMAGE_EXT   ".img"
#define TOOLS_RAW_EXT     ".img"
#define TOOLS_COMPLEX_EXT ".img"

#define DEFAULT_RANGE_SCALE 1.00343642612
#define DEFAULT_AZIMUTH_SCALE 1.003333505

/* This is the external interface to asf_import */
int asf_import(radiometry_t radiometry, // r_AMP,R_SIGMA,r_BETA,r_GAMMA,r_POWER
               int db_flag,   // TRUE if the output should be in decibels
                              // only ok for radiometry=SIGMA,GAMMA,BETA
         int complex_flag, // TRUE if ingested SLC should in I/Q
         int multilook_flag, // TRUE is SLC should be multilooked while
                             // being ingested.
               char *format_type, // eg, "STF", "CEOS" - etc
               char *band_id, // eg, "" (default for all bands), "VH", "03" - etc
               char *image_data_type, // "geocoded_image", "dem", or "mask"
               char *lutName, // NULL for no lookup table
                              // otherwise, this is the lookup table filename
               char *prcPath, // NULL for not using precision orbit data
                              // otherwise, this is the precision state vector
                              // path
               double lowerLat, // -99 means not constrained
               double upperLat, // -99 means not constrained
	       int line, // start line subset - default set to 0
	       int sample, // start sample subset - default set to 0
	       int width, // -99 means no subsetting
	       int height, // -99 means no subsetting
               double *p_range_scale, // NULL for no scaling
               double *p_azimuth_scale, // NULL for no scaling
               double *p_correct_y_pixel_size, // NULL for no fixing
               char *inMetaNameOption, // NULL for normal metadata naming
                                       // otherwise, this is the meta file name
               char *inBaseName, // input file
               char *outBaseName // output file
               );

/*********************************************************************/
/* The rest of these are private implementation functions for import */

/* Prototypes from utilities.c */
int firstRecordLen(char *ceosName);

/* Prototypes from sprocket_layers.c */
void create_sprocket_layers(const char *asfName, const char *importName);

/* import_*() function prototypes */
void import_ceos(char *inBaseName, char *outBaseName, char *format_type,
                 char *band_id, char *lutName, double *p_range_scale,
                 double *p_azimuth_scale, double *p_correct_y_pixel_size,
		 int line, int sample, int width, int height,
                 char *inMetaNameOption, radiometry_t radiometry, int db_flag,
     int complex_flag, int multilook_flag);
void import_stf(char *inBaseName, char *outBaseName, radiometry_t radiometry,
    char *inMetaNameOption, int lat_constrained, double lowerLat,
    double upperLat, char *prcPath);

void
import_generic_geotiff (const char *inFileName, const char *outBaseName, ...);

void import_bil(char *inBaseName, char *outBaseName);
void import_gridfloat(char *inBaseName, char *outBaseName);

void import_airsar(const char *inFileName, const char *outBaseName);
meta_parameters *import_airsar_meta(const char *inBaseName);

void import_gamma_isp(const char *inBaseName, const char *outBaseName);

#endif
