#ifndef _DETECT_CR_H_
#define _DETECT_CR_H_

#define FLAG_SET 1
#define FLAG_NOT_SET -1
#include "ifm.h"
#include <fftw3.h>

/* Evaluate to true if floats are within tolerance of each other.  */
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)

/* Default tolerance used for many floating point comparisons in this
   program.  */
#define ASF_EXPORT_FLOAT_MICRON 0.000000001

/* Compare floats using the default tolerance for this program.  */
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
  f_CHIP=1,
  f_TEXT,
  f_LOG,
  f_QUIET,
  NUM_FLAGS
} flag_indices_t;

/* Prototypes from utilities.c */
void usage(void);
void help_page();
int firstRecordLen(char *ceosName);
void print_splash_screen(int argc, char* argv[]);
void print_progress(int current_line, int total_lines);
int checkForOption(char* key, int argc, char* argv[]);
void print_error(char *msg);
void check_return(int ret, char *msg);
void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName);

// Prototypes from fftw.c
fcpx *forward_fft(complexFloat *image, int line_count, int sample_count);
complexFloat *inverse_fft(fftwf_complex *fft_cpx, int line_count, int sample_count);
fcpx *oversample(fcpx *in, int srcSize, int oversampling_factor);
void complex2polar(complexFloat *in, int line_count, int sample_count,
		   float *amplitude, float *phase);

/* Prototypes */
void topOffPeak(float *peaks, int i, int j, int maxI, float *di, float *dj);
bool findPeak(float *s, int size, float *peakX, float *peakY);
bool outOfBounds(int x, int y, int srcSize);
void baseband(fcpx *s, int window_size);
void transpose(float *s);
bool find_mainlobe(float *t, float *profile, int size, int peak, float clutter_power, 
		   int *mainlobe_min, int *mainlobe_max);
bool calc_resolution(float *profile, int mainlobe_min, int mainlobe_max, int max,
		     float pixel_spacing, float clutter_power, float *resolution);
float calc_clutter_power(float *s, int srcSize, int peak_sample, int peak_line,
			 float azimuth_resolution, float range_resolution);
bool find_sidelobe(float *profile, int size, int sign, int peak_line, int boundary,
		   int *sidelobe);
bool calc_pslr(float *profile, int size, int peak, int sidelobe_min, int sidelobe_max,
	       float *pslr);
int modr(int number, int base);
void integrate(float *s, int srcSize, int sample, int line, float azimuth_length, 
	       float range_length, float *sum, float *maximum);

#endif
