/*Standard ASF Utility include file.*/
#ifndef __ASF_H
#define __ASF_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "caplib.h"
#include "asf_meta.h"
#include "error.h"
#include "log.h"
#include "cla.h"

#ifndef PI
# define PI 3.14159265358979323846
#endif
#ifndef M_PI
# define M_PI PI
#endif 
#define D2R (PI/180.0)
#define R2D (180.0/PI)

/* Create NAN (Not a Number) since it is not in a standard library.
 * Conforms to ieee NaN.  Under most architectures will be interpreted
 * as a signalling NaN (SNaN). SNaN should signal trapping operations,
 * but to date (July 2002) I am unaware of any operations that trap for
 * NaN whatsoever. Therefore this NaN should behave like a quiet NaN (QNaN)
 * and will propagate through equations until it's used in a condional
 * statement (which should return False) */
#if defined (lil_endian)
# define __nan_bytes { 0xff, 0xff, 0xbf, 0x7f }
#else /*defined (big_endian)*/
# define __nan_bytes { 0x7f, 0xbf, 0xff, 0xff }
#endif
static union { unsigned char __c[4]; float __d; } __nan_union = { __nan_bytes };
#define NAN __nan_union.__d


/* Print an error with printf-style formatting codes and args, then die.  */
void bail(const char *message, ...)/* ; is coming, don't worry.  */
/* The GNU C compiler can give us some special help if compiling with
   -Wformat or a warning option that implies it.  */
#ifdef __GNUC__
     /* Function attribute format says function is variadic al la
        printf, with argument 1 its format spec and argument 2 its
        first optional argument corresponding to the spec.  Function
        attribute noreturn says function doesn't return.  */
     __attribute__ ((format (printf, 1, 2), noreturn))
#endif 
; /* <-- Semicolon for err_die prototype.  */
void StartWatch(void);
void StartWatchLog(FILE *fLog);
void StopWatch(void);
void StopWatchLog(FILE *fLog);

/*****************************************
FileUtil:
	A collection of file I/O utilities.
Actually implemented in asf.a/fileUtil.c

extExists returns whether the given
file basename and extension exist and
are readable.

fileExists returns whether the given file
name exists and is readable.

findExt returns a pointer to the beginning
(the period) of the given name's first
extension, or NULL if none exists.

appendExt allocates its return string
on the heap, so it must be free'd, or
memory will leak.  It can take a NULL extension,
whereupon it just allocates a copy of the given
string, and returns it.

fopenImage first tries to open the given image 
name, then appends ".img" and tries again.
It returns a pointer to the opened file.
*/

int extExists(const char *name,const char *newExt);
int fileExists(const char *name);
char *findExt(char *name);
char *appendExt(const char *name,const char *newExt);
FILE *fopenImage(const char *name,const char *accessType);
void get_float_line(FILE *file, meta_parameters *meta, int line_number, 
		    float *dest);
void create_name(char *out,const char *in,const char *newExt);

#endif
