/*Standard ASF Utility include file.*/
#ifndef __ASF_H
#define __ASF_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "caplib.h"
#include "asf_meta.h"    /* For get_(data)_line(s) and put_(data)_line(s) */
#include "error.h"
#include "log.h"
#include "cla.h"
#include "asf_complex.h" /* For get_complexFloat_line(s) and
                          * put_complexFloat_line(s) */

#include "asf_version.h"

/* Logging should eventually be moved from the asf.a library to this library */
#include "log.h"

#ifndef PI
# define PI 3.141592653589793238462643383279502884197169399375105820974944592307
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

// In windows (cygwin) apparently the directory separator can be either
// back slash or forward slash
#if defined(win32)
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_STR "/\\"
#define IS_DIR_SEPARATOR(character) ((character == '\\') || (character == '/'))
#else
#define DIR_SEPARATOR '/'
#define DIR_SEPARATOR_STR "/"
#define IS_DIR_SEPARATOR(character) (character == '/')
#endif

#define EXTENSION_SEPARATOR '.'
#define ALOS_EXTENSION_SEPARATOR '-'

/* Speed of light */
#define SPD_LIGHT 2.997924562e8

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

/******************************************************************************
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
void StartWatchLog(FILE *log_fp);
void StopWatchLog(FILE *log_fp);
char* date_stamp(void);
char* date_time_stamp(void);
char* time_stamp_dir(void);

/******************************************************************************
 * strUtil:
 * A collection of string manipulation utilities. */

/* Return a static pointer to a variation of the given string in upper case.
   Do not allocate or free memory for this string, only point at it.
   This function is case insensitive. */
char *uc (const char *string);

/* Case-insensitive string comparison */
int strcmp_case(const char *s1, const char *s2);

/* Allocate and return a new string which is s1+s2, neither of which are null*/
char *appendStr(const char *s1, const char *s2);

/******************************************************************************
 * FileUtil:
 * A collection of file name and I/O utilities. Implemented * in
 * asf.a/fileUtil.c */

/* Return a pointer into string name pointing to the dot ('.')
   character in the trailing dot extension, or a NULL pointer if name
   doesn't include any dots.  */
char *findExt(const char *name);
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
/* 5/06: No longer a buffer overrun risk - we can preallocate the proper
   amount of memory!  Only suggested change would now be a name
   change to "changeExt" as that is much more indicative of its function. */
char *appendExt(const char *name,const char *newExt);
/* Return true iff file name exists and is readable.  */
int fileExists(const char *name);
/* Return true iff fileExists(appendExt(name, newExt)) would return
   true, but without the memory leak fileExists(appendExt(name,
   newExt)) would produce. */
int extExists(const char *name,const char *newExt);
/* Add a string to the end of the filename's basename.  For example, if
   if filename is "file.ext" and suffix is "_2" then "file_2.ext" is
   returned.  The return value must be freed by the caller. */
char * appendToBasename(const char *filename, const char *suffix);

void append_ext_if_needed(char *file_name, const char *newExt, 
                          const char *alsoAllowedExt);

void append_band_ext(char *inFile, char *outFile, char *bandExt);

/* Remove the extension from the name, if any.
   Returns a newly allocated string. */
char *stripExt(const char *in);

/* Creates name out by clipping off the rightmost dot extension, if
   any, of in, and then appending newExt.  out must point to existing
   memory large enough to store the result.  */
void create_name(char *out,const char *in,const char *newExt);

/* Takes a string and fills one pre-allocated array with any path prior to
   the file name, and fills another pre-allocated array with the file name */
void split_dir_and_file(const char *inString, char *dirName, char *fileName);

#define PREPENDED_EXTENSION -1
#define APPENDED_EXTENSION   1
/* Fills 'extension' with the file's extension and returns TRUE if there is
   indeed an extension. Otherwise it returns FALSE and fills 'extension' with
   an empty string. If 'side' is PREPENDED_EXTENSION, then it looks for the
   extension at the front of the file name. If 'side' is APPENDED_EXTENSION, it
   looks for the extension at the end of the file name. Otherwise it returns
   FALSE and fills 'extension' with an empty string. The extension separator
   is a '.' It assumes 'fileName' is only the file name (no path included) */
int split_base_and_ext(char *fileName, int side, char separator,
		       char *baseName, char *extension);

/* first tries to open the given image name, then appends ".img" and tries again
   It returns a pointer to the opened file.*/
FILE *fopenImage(const char *name,const char *accessType);

/* Copy the file specified by "src" to the file specified by "dst". */
void fileCopy(const char *src, const char *dst);

/* Create & remove directories, these return 0 on success */
int create_dir(const char *dir);
int create_clean_dir(const char *dir);
int remove_dir(const char *dir);

/******************************************************************************
 * ioLine: Grab any data type and fill a buffer of _type_ data.
 * Assumes that the datae file contains data in big endian order and
 * returns data in host byte order. Implemented in asf.a/ioLine.c */

/* Size of line chunk to read or write.  */
#define CHUNK_OF_LINES 32

int get_byte_line(FILE *file, meta_parameters *meta, int line_number,
                  unsigned char *dest);
int get_byte_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_get, unsigned char *dest);
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

/***************************************************************************
 * Get the location of the ASF Share Directory */
const char * get_asf_share_dir(void);
const char * get_asf_bin_dir(void);
FILE * fopen_share_file(const char * filename, const char * mode);

/***************************************************************************
 * Wrapper for system() that is more portable, plus uses varargs */
int asfSystem(const char *format, ...);

/***************************************************************************
 * Get the location of the temporary directory (this is where log files,
 * etc should be put).  The application is in charge of setting this!     */
void set_asf_tmp_dir(const char *tmp_dir);
const char * get_asf_tmp_dir(void);
FILE * fopen_tmp_file(const char * filename, const char * mode);
int unlink_tmp_file(const char *filename);

void fileRename(const char *src, const char *dst);
void renameImgAndMeta(const char *src, const char *dst);
char *get_basename(const char *filename);
char *get_dirname(const char *in);
char *get_filename(const char *in);

/* Prototypes from progress_meters.c *****************************************/
void asfLineMeter(int currentLine, int totalLines);
void asfPercentMeter(double inPercent);

/* Prototypes from print_alerts.c ********************************************/
/* Do not print to the terminal, only report to the log file */
void asfPrintToLogOnly(const char *format, ...);
/* Basically a printf that pays attention to quiet & log flags */
void asfPrintStatus(const char *format, ...);
/* Basically a printf that pays attention to log flag but NOT the quiet flag */
void asfForcePrintStatus(const char *format, ...);
/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...);
/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...);

/* Prototype from splash_screen.c ********************************************/
/* Print the commandline captured, date, and PID to screen & logfile */
void asfSplashScreen(int argc, char **argv);

/* Prototypes from diagnostics.c *********************************************/
/* Given a condition, and an optional printf()-style message string (possibly
   with additional variadic arguments), this macro will print an assert style
   message and the optional message to the terminal & log file iff the
   condition is false. The "" is in case there is no format string for VA_ARGS */
#define asfRequire(COND,...) \
        (require_function (__FILE__, __LINE__, (COND), (#COND),"" __VA_ARGS__))
/* This routine is onlyused by the require macro, never use it elsewhere!  */
void require_function (const char *file, int line, int condition,
                       const char *condSrt, const char *message, ...)
#ifdef __GNUC__
 __attribute__ ((format (printf, 5, 6)))
#endif
; /* <--- semicolon for require_function */

/* Given a printf()-style message string and additional variadic
   arguments for the message string, in that order, this macro will
   print message to terminal, and log message prefixed with source file
   and line information to the log file. */
#define asfDie(...) (die_function (__FILE__, __LINE__, __VA_ARGS__))
/* This routine is used by the die macro.  It should never be used
   anywhere else.  */
void die_function (const char *file, int line, const char *message, ...)
#ifdef __GNUC__
 __attribute__ ((format (printf, 3, 4), noreturn))
#endif
; /* <--- semicolon for die_function */

#endif
