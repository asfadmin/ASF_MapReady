/*Standard ASF Utility include file.*/
#ifndef __ASF_H
#define __ASF_H

#ifdef win32
// guard against conflicts in jpeg & system includes on mingw
#define HAVE_BOOLEAN
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "caplib.h"
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
#ifndef D2R
#define D2R (PI/180.0)
#endif
#ifndef R2D
#define R2D (180.0/PI)
#endif
#define DEGREES2ARCMINUTES 60.0
#define ARCMINUTES2DEGREES (1.0/DEGREES2ARCMINUTES)
#define DEGREES2ARCSECONDS 3600.0
#define ARCSECONDS2DEGREES (1.0/DEGREES2ARCSECONDS)

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
#define CAN_EXTENSION_SEPARATOR '_'
#define PLUS_SEPARATOR '+'

/* Speed of light */
#define SPD_LIGHT 2.997924562e8

// Maximum number of bands that are supported by the ingest.
#define MAX_BANDS 50

/* These are the dummy values that meta variables get set to at initialization */
#define MAGIC_UNSET_CHAR ('?')
#define MAGIC_UNSET_STRING ("???")
#define MAGIC_UNSET_SHORT (-9999)
#define MAGIC_UNSET_INT (-999999999)
/* For MAGIC_UNSET_DOUBLE to work, you must include asf_nan.h */
#define MAGIC_UNSET_DOUBLE (NAN)

typedef enum {
  REPORT_LEVEL_NONE=1,
  REPORT_LEVEL_LOG,
  REPORT_LEVEL_STATUS,
  REPORT_LEVEL_WARNING,
  REPORT_LEVEL_ERROR
} report_level_t;

extern report_level_t g_report_level;

typedef enum {
  ASF_BYTE=1,
  INTEGER16,
  INTEGER32,
  REAL32,
  REAL64,
  COMPLEX_BYTE,
  COMPLEX_INTEGER16,
  COMPLEX_INTEGER32,
  COMPLEX_REAL32,
  COMPLEX_REAL64
} data_type_t;

typedef enum {
  FILE_EXISTS=1,
  FILE_TYPE,
  VALID_VALUE,
  VALUE_RANGE,
  METADATA
} check_input_t;

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

// Prototypes from check.c
void check_return(int ret, char *msg);
int check_status(char *status);


/******************************************************************************
 * strUtil:
 * A collection of string manipulation utilities. */

/* Return a static pointer to a variation of the given string in upper case.
   Do not allocate or free memory for this string, only point at it.
   This function is case insensitive. */
char *uc (const char *string);
char *lc (const char *string);

/* Case-insensitive string comparison */
int strcmp_case(const char *s1, const char *s2);
int strncmp_case(const char *s1, const char *s2, size_t n);

/* Allocate and return a new string which is s1+s2, neither of which are null*/
char *appendStr(const char *s1, const char *s2);

/* Wrapper for strncpy, guarantees result is null-terminated. */
/* Important difference from strncpy:                         */
/*                 ONLY COPIES len-1 CHARACTERS!              */
char *strncpy_safe(char *dst, const char *src, size_t len);

// Trim leading and trailing white spaces from a string
char *trim_spaces(const char *s);

// Removes carriage returns from strings
void chomp(char *str);

/* Return TRUE if "str" ends with (case-insensitive) "tail" */
int endsWith(const char *str, const char *tail);

/* Wrapper for strtok_r() -- some systems do not have it */
char *STRTOK_R(char *str1, const char *str2, char **lasts);

// return the number of occurences of char 'c' in string 's'
int count_char(const char *s, char c);

// a version of strstr that ignores case
char *strstr_case(const char *str, const char *key);

// replaces occurences in "str" of "searchStr" with "replaceStr"
char *asf_strReplace(const char *str, const char *searchStr,
		     const char *replaceStr);

// quoted-element csv string parsing routines
char *quoted_string_parse(char *p, char *s, int max_len, int line_num,
                          char sep);
void split_into_array(const char *str, char sep, int *nelem, char ***parr);
void free_char_array(char ***parr, int nelem);
void split2(const char *str_in, char sep, char **s1_out, char **s2_out);
const char *get_str(char *line, int column_number);
char get_char(char *line, int column_num);
int get_int(char *line, int column_number);
int get_long(char *line, int column_number);
double get_double(char *line, int column_number);
char get_req_char(char *line, int column_num, int *ok);
int get_req_int(char *line, int column_number, int *ok);
long get_req_long(char *line, int column_number, int *ok);
double get_req_double(char *line, int column_number, int *ok);
int find_str(char *line, char *str);
int find_2nd_str(char *line, char *str);
int find_nth_str(char *line, char *str, int occurence);

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
/* Return number of bytes in the named file */
long long fileSize(const char *name);
/* Return the number of regular files found in the given path */
int numFiles(const char *path);
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
void copyImgAndMeta(const char *src, const char *dst);

/* Create & remove directories, these return 0 on success */
int is_dir(const char *dir);
int create_dir(const char *dir);
int create_clean_dir(const char *dir);
int remove_dir(const char *dir);
/* return newly allocated string, with just the path of the given
   file.  returns empty if no dir info given.  */
char * getPath(const char *in);

/* returns ".exe" on Windows, "" everywhere else */
const char *bin_postfix(void);

/* wrapper for unlink */
int remove_file(const char *file);

/* delete image and metadata files given a basename */
void removeImgAndMeta(const char *base);

void catFile(char *file);

/***************************************************************************
 * Get the location of the ASF Share Directory, (and some other stuff) */
const char * get_asf_share_dir(void);
const char * get_asf_share_dir_with_argv0(const char *);
const char * get_asf_bin_dir(void);
const char * get_asf_bin_dir_win(void);
FILE * fopen_share_file(const char * filename, const char * mode);
const char *get_argv0(void);
int share_file_exists(const char *filename);

/***************************************************************************
 * Wrapper for system() that is more portable, plus uses varargs */
int asfSystem(const char *format, ...);

/***************************************************************************
 * This version of asfSystem() does not block, Windows only -- Linux
 * will use fork() instead                                                */
#ifdef win32
int asfSystem_NoWait(const char *format, ...);
#endif

/***************************************************************************
 * Get the location of the temporary directory (this is where log files,
 * etc should be put).  The application is in charge of setting this!     */
void set_asf_tmp_dir(const char *tmp_dir);
const char *get_asf_tmp_dir(void);
const char *get_tmp_log_file(char *tool);
FILE * fopen_tmp_file(const char * filename, const char * mode);
int unlink_tmp_file(const char *filename);

void fileRename(const char *src, const char *dst);
void renameImgAndMeta(const char *src, const char *dst);
char *get_basename(const char *filename);
char *get_dirname(const char *in);
char *get_filename(const char *in);

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
/* Report to user & logfile, then die -- unless user has set g_report_level
   to NOREPORT, in which case we silently continue without saying anything */
void asfPrintErrorMaybe(const char *format, ...);
// Report at the different levels
void asfReport(report_level_t level, const char *format, ...);
void asfLineMeter(int currentLine, int totalLines);
void asfPercentMeter(double inPercent);
void asfRunWatchDog(double delay);
void asfStopWatchDog(void);

/* Prototype from splash_screen.c ********************************************/
/* Print the commandline captured, date, and PID to screen & logfile */
void asfSplashScreen(int argc, char **argv);

// solve1d.c
typedef double solve1d_fn(void *params, double x);
int solve1d(solve1d_fn *f, void *params, int min_x, int max_x, double acc,
            double *root);

// httpUtil.c
unsigned char *download_url(const char *url, int verbose, int *length);
int download_url_to_file(const char *url, const char *filename);

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

// Can now specify unused arguments to functions with:
//  int function(int x, int *y)
// like this:
//  int function(int UNUSED(x), int* UNUSED(x))
#ifdef UNUSED
#elif defined(__GNUC__) 
# define UNUSED(x) UNUSED_ ## x __attribute__((unused)) 
#elif defined(__LCLINT__) 
# define UNUSED(x) /*@unused@*/ x 
#else 
# define UNUSED(x) x 
#endif

#endif
