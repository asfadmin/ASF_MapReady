/*Standard ASF Utility include file.*/
#ifndef __ASF_H
#define __ASF_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caplib.h"
#include "asf_meta.h"    /* For get_(data)_line(s) and put_(data)_line(s) */
#include "error.h"
#include "log.h"
#include "cla.h"
#include "asf_complex.h" /* For get_complexFloat_line(s) and
                          * put_complexFloat_line(s) */

#ifndef PI
# define PI 3.14159265358979323846
#endif
#ifndef M_PI
# define M_PI PI
#endif 
#define D2R (PI/180.0)
#define R2D (180.0/PI)

#ifndef FALSE
# define FALSE (0)
#endif
#ifndef TRUE
# define TRUE (!FALSE)
#endif

/* Print an error with printf-style formatting codes and args, then die.  */
void bail(const char *message, ...)/* ; is coming, don't worry.  */
/* The GNU C compiler can give us some special help if compiling with
   -Wformat or a warning option that implies it.  */
#ifdef __GNUC__
     /* Function attribute format says function is variadic a la
        printf, with argument 1 its format spec and argument 2 its
        first optional argument corresponding to the spec.  Function
        attribute noreturn says function doesn't return.  */
     __attribute__ ((format (printf, 1, 2), noreturn))
#endif 
; /* <-- Semicolon for bail prototype.  */

/*****************************************
 * Deprecated program timing routines.  There are better ways (like
 * looking at yor watch even) to time programs than using these
 * routines.  Call StartWatch(Log) at the beginning, and then call
 * StopWatch(Log) at the end to automaticly print out some information
 * about how much CPU time the program took to standard output or to a
 * log file.  If the program takes more than 72 minutes, the reported
 * time may be totally wrong on some systems, due to clock wrap
 * around.  So its probably best not to use these in new code
 * (consider them deprecated).  */
void StartWatch(void);
void StopWatch(void);
void StartWatchLog(FILE *fLog);
void StopWatchLog(FILE *fLog);

/*****************************************
 * FileUtil: 
 * A collection of file name and I/O utilities. Implemented * in
 * asf.a/fileUtil.c */

char *findExt(char *name);
/* The maximum allowable length in characters (not including trailing
   null character) of result strings from the appendExt routine.  */
#define MAX_APPENDEXT_RESULT_STRING_LENGTH 255
/* Deprecated: use create_name and preallocated memory instead.  This
   function is badly misnamed.  What it actuall does: First, if newExt
   is NULL (not an empty string, but a NULL pointer, return a new copy
   of name.  Otherwise, return in new memory a string consisting of a
   copy of name with the rightmost dot extension, if present, replaced
   with newExt.  If no dot extension exists originally, the new
   extension is appended.  */
char *appendExt(const char *name,const char *newExt);
/* Ruturn true iff file name exists and is readable.  */
int fileExists(const char *name);
/* Return true iff fileExists(appendExt(name, newExt)) would return
   true, but without the memory leak fileExists(appendExt(name,
   newExt)) would produce. */
int extExists(const char *name,const char *newExt);
/* Creates name out by clipping off the rightmost dot extension, if
   any, of in, and then appending newExt.  out must point to existing
   memory large enough to store the result.  */
void create_name(char *out,const char *in,const char *newExt);

FILE *fopenImage(const char *name,const char *accessType);

/* Size of line chunk to read or write.  */
#define CHUNK_OF_LINES 256

/*****************************************
 * ioLine: Grab any data type and fill a buffer of _type_ data.
 * Assumes that the datae file contains data in big endian order and
 * returns data in host byte order. Implemented in asf.a/ioLine.c */
int get_float_line(FILE *file, meta_parameters *meta, int line_number,
		float *dest);
int get_float_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_get, float *dest);
int get_double_line(FILE *file, meta_parameters *meta, int line_number,
		double *dest);
int get_double_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_get, double *dest);
int get_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
		complexFloat *dest);
int get_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_get, complexFloat *dest);
int put_float_line(FILE *file, meta_parameters *meta, int line_number,
		const float *source);
int put_float_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_put, const float *source);
int put_double_line(FILE *file, meta_parameters *meta, int line_number,
		const double *source);
int put_double_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_put, const double *source);
int put_complexFloat_line(FILE *file, meta_parameters *meta, int line_number,
		const complexFloat *source);
int put_complexFloat_lines(FILE *file, meta_parameters *meta, int line_number,
		int num_lines_to_put, const complexFloat *source);

#endif
