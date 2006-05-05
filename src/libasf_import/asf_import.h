#ifndef _ASF_IMPORT_H_
#define _ASF_IMPORT_H_

#define FLAG_SET 1
#define FLAG_NOT_SET -1

#define TOOLS_META_EXT    ".meta"
#define TOOLS_IMAGE_EXT   ".img"
#define TOOLS_RAW_EXT     ".raw"
#define TOOLS_COMPLEX_EXT ".cpx"

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
    f_AMP=1,
    f_SIGMA,
    f_BETA,
    f_GAMMA,
    f_POWER,
    f_DB,
    f_SPROCKET,
    f_LUT,
    f_LAT_CONSTRAINT,
    f_PRC,
    f_FORMAT,
    f_OLD_META,
    f_METADATA_FILE,
    f_LOG,
    f_QUIET,
    f_RANGE_SCALE,
    f_AZIMUTH_SCALE,
    f_FIX_META_YPIX,
    NUM_FLAGS
} flag_indices_t;

#define DEFAULT_RANGE_SCALE 1.00343642612
#define DEFAULT_AZIMUTH_SCALE 1.003333505

/* Prototypes from utilities.c */
int firstRecordLen(char *ceosName);
int checkForOption(char* key, int argc, char* argv[]);
int checkForOptionWithArg(char *key, int argc, char* argv[]);
double getDoubleOptionArgWithDefault(char *arg, double def);
void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName);

/* Prototypes from sprocket_layers.c */
void create_sprocket_layers(const char *asfName, const char *importName);

/* Import function itself.  */
int asf_import(int flags[NUM_FLAGS], char *format_type, char *lutName, 
	       char *prcPath, double lowerLat, double upperLat, 
	       double range_scale, double azimuth_scale, 
	       double correct_y_pixel_size,
	       char *inBaseName, char *outBaseName);

/* import_*() function prototypes */
void import_ceos(char *inDataName,char *inMetaName,char *lutName,
                 char *outBaseName,int flags[]);
void import_envi(char *inDataName,char *inMetaName,char *outBaseName,int flags[]);
void import_esri(char *inDataName,char *inMetaName,char *outBaseName,int flags[]);
void import_stf (char *inDataName,char *inMetaName,char *outBaseName,int flags[],
                 double lowerLat, double upperLat, char *prcPath);/*this last line of parameters are extra from the rest of the import_*() functions */
void
import_usgs_seamless (const char *inFileName, const char *outBaseName,
		      int flag[]);
void
import_asf_utm_geotiff (const char *inFileName, const char *outBaseName,
			int flag[]);

#endif
