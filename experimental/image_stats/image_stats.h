#ifndef _IMAGE_STATS_H_
#define _IMAGE_STATS_H_

#include "asf.h"
#include "asf_meta.h"

#define BUFSIZE 1024
#ifndef PI
#define PI 3.14159265358979323846
#endif
#define RES_X 16
#define RES_Y 16
#define MAX_PTS 300

#define SQR(X) ((X)*(X))

#define FLAG_SET 1
#define FLAG_NOT_SET -1

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, FLOAT_MICRON))

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
  f_LOOK=1,
  f_INCIDENCE,
  f_RANGE,
  f_MIN,
  f_MAX,
  f_TOLERANCE,
  f_BINS,
  f_INTERVAL,
  f_START_LINE,
  f_START_SAMPLE,
  f_HEIGHT,
  f_WIDTH,
  f_MASK,
  f_COMPARE,
  f_LOG,
  f_QUIET,
  NUM_FLAGS
} flag_indices_t;

typedef struct {
  long count; /* number of pixels in plot bin */
  double mean; /* mean value of plot bin */
  double stdDev; /* standard deviation of plot bin */
} plot_t;

/* Prototypes out of utilities.c */
int firstRecordLen(char *ceosName);
char *uc(char *string);
void print_splash_screen(int argc, char* argv[]);
void print_progress(int current_line, int total_lines);
int checkForOption(char* key, int argc, char* argv[]);
void print_error(char *msg);
void check_return(int ret, char *msg);
void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName);

/* Prototypes out of meta_util.c */
double get_satellite_height(double time, stateVector stVec);
double get_earth_radius(double time, stateVector stVec, double re, double rp);
double get_slant_range(meta_parameters *meta, double er, double ht, int sample);
double get_look_angle(double er, double ht, double sr);
double get_incidence_angle(double er, double ht, double sr);

/* Prototypes out of plot.c */
void calculate_plot(char *gridFile, char *inFile, char *compFile, char *maskFile, 
		    char *outFile, meta_parameters *meta, float xConstant);

#endif
