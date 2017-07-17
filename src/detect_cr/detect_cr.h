#ifndef _DETECT_CR_H_
#define _DETECT_CR_H_

#define FLAG_SET 1
#define FLAG_NOT_SET -1
#include "ifm.h"
#include <stdbool.h>

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
  f_CHIPS=1,
  f_CHIP_SIZE,
  f_TEXT,
  f_PROFILE,
  f_GEOCODE,
  f_LOG,
  f_QUIET,
  NUM_FLAGS
} flag_indices_t;

/* Prototypes from utilities.c */
void usage(char *name);
void help_page();
int firstRecordLen(char *ceosName);
void print_splash_screen(int argc, char* argv[]);
void print_progress(int current_line, int total_lines);
int checkForOption(char* key, int argc, char* argv[]);
void print_error(char *msg);
void check_return(int ret, char *msg);
void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName);

/* Prototypes */
void topOffPeak(float *peaks, int i, int j, int maxI, float *di, float *dj);
bool findPeak(int x, int y, float elev, char *szImg, float *peakX, float *peakY, 
	      char *chip, char *text, char *projFile);
bool outOfBounds(int x, int y, int srcSize);

#endif
