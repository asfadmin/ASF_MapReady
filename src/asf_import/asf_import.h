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
    f_SPROCKET,
    f_LAT_CONSTRAINT,
    f_PRC,
    f_FORMAT,
    f_OLD_META,
    f_LOG,
    f_QUIET,
    NUM_FLAGS
} flag_indices_t;

/* Prototypes from utilities.c */
void usage(void);
void help_page();
int firstRecordLen(char *ceosName);
char *uc(char *string);
void print_splash_screen(int argc, char* argv[]);
void print_progress(int current_line, int total_lines);
int checkForOption(char* key, int argc, char* argv[]);
void print_error(char *msg);
void check_return(int ret, char *msg);
void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName);

/* Prototypes from sprocket_layers.c */
void create_sprocket_layers(const char *asfName, const char *importName);

/* import_*() function prototypes */
void import_ceos(char *inDataName,char *inMetaName,char *outBaseName,flag_indices_t flags[]);
void import_envi(char *inDataName,char *inMetaName,char *outBaseName,flag_indices_t flags[]);
void import_esri(char *inDataName,char *inMetaName,char *outBaseName,flag_indices_t flags[]);
void import_stf (char *inDataName,char *inMetaName,char *outBaseName,flag_indices_t flags[],
                 double lowerLat, double upperLat, char *prcPath);/*this last line of parameters are extra from the rest of the import_*() functions */

#endif
