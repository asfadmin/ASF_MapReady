#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/times.h>
#include <time.h>

/* Global Prototypes */
extern void string_all_trim (char *str);
extern int ERROR (char *caller, char *str, int status);
extern void file_check (int fd, char *name);
extern void FILE_check (FILE * fp, char *name, char *caller);
extern void read_metadata_int (FILE * metadata, char *item, int *value);
extern void read_metadata_double (FILE * metadata, char *item, double *value);
extern double slant2look (double R, double Re, double H);
extern double look2incidence (double look, double Re, double H);
extern void read_integer (FILE * in, unsigned int *buffer, int start, int end);
extern double stdev (double sum, double sum2, double n);
extern void read_float (FILE * in, float *buffer, int start, int end);
extern void read_mask (FILE * in, unsigned char *buffer, int start, int end, int ns);
extern double look2slant (double look, double Re, double H);
extern double sigma02gama0 (double inc, double sigma0);
extern void write_integer (int out, unsigned int *buffer, int number);

   /* Constants */
#define EXIT (-1)
#define OK (0)
#define COMMENT_METADATA (10)

#define SIGMA0_UNDEFINED (-100.0)
#define SIGMA0_UNDEFINED_WRITE (-130.0)
