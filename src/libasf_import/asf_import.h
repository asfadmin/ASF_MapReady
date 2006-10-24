#ifndef _ASF_IMPORT_H_
#define _ASF_IMPORT_H_

#define TOOLS_META_EXT    ".meta"
#define TOOLS_IMAGE_EXT   ".img"
#define TOOLS_RAW_EXT     ".raw"
#define TOOLS_COMPLEX_EXT ".cpx"

typedef enum {
    r_AMP=1,
    r_SIGMA,
    r_BETA,
    r_GAMMA,
    r_POWER,
} radiometry_t;

#define DEFAULT_RANGE_SCALE 1.00343642612
#define DEFAULT_AZIMUTH_SCALE 1.003333505

/* This is the external interface to asf_import */
int asf_import(radiometry_t radiometry, // r_AMP,R_SIGMA,r_BETA,r_GAMMA,r_POWER
               int db_flag,   // TRUE if the output should be in decibels
                              // only ok for radiometry=SIGMA,GAMMA,BETA
               char *format_type, // eg, "STF", "CEOS" - etc
               char *lutName, // NULL for no lookup table
                              // otherwise, this is the lookup table filename
	       char *prcPath, // NULL for not using precision orbit data
                              // otherwise, this is the precision state vector
                              // path
               double lowerLat, // -99 means not constrained
               double upperLat, // -99 means not constrained
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
void import_ceos(char *inDataName, char *inMetaName, char *lutName,
                 char *outBaseName, radiometry_t radiometry, int db_flag);

void import_envi(char *inDataName,char *inMetaName,char *outBaseName);
void import_esri(char *inDataName,char *inMetaName,char *outBaseName);

void import_stf(char *inDataName, char *inMetaName, char *outBaseName,
                radiometry_t radiometry, int lat_constrained,
                double lowerLat, double upperLat, char *prcPath);/*this last line of parameters are extra from the rest of the import_*() functions */
void
import_usgs_seamless (const char *inFileName, const char *outBaseName, ...);
void
import_asf_utm_geotiff (const char *inFileName, const char *outBaseName, ...);

#endif
