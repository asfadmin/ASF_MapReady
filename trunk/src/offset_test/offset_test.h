#ifndef _OFFSET_TEST_H_
#define _OFFSET_TEST_H_

#define borderX 80	/* Distances from edge of image to start correlating.*/
#define borderY 80
#define maxDisp 1.8	/* Forward and reverse correlations which differ 
			   by more than this will be deleted.*/
#define maxDxDy 1.0     /* Maximum allowed difference before point is 
			   reported as moved */
#define minSNR 0.3
#define VERSION 1.0

#define FLAG_SET 1
#define FLAG_NOT_SET -1

typedef enum {
  f_LOG=1,
  f_QUIET,
  NUM_FLAGS
} flag_indices_t;

/* Prototypes out of utilities.c */
int firstRecordLen(char *ceosName);
void print_splash_screen(int argc, char* argv[]);
void print_progress(int current_line, int total_lines);
int checkForOption(char* key, int argc, char* argv[]);
void print_error(char *msg);
void check_return(int ret, char *msg);

#endif
