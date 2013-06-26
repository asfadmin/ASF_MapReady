#ifndef __ASF_NAN_H
#define __ASF_NAN_H

#include "asf.h"

/* Create NAN (Not a Number) since it is not in a standard library.
 * Conforms to ieee NaN.  Under most architectures will be interpreted
 * as a signalling NaN (SNaN). SNaN should signal trapping operations,
 * but to date (July 2002) I am unaware of any operations that trap for
 * NaN whatsoever. Therefore this NaN should behave like a quiet NaN (QNaN)
 * and will propagate through equations until it's used in a condional
 * statement (which should return False) */
#ifndef NAN
# if defined (ASF_LIL_ENDIAN)
#  define __nan_bytes { 0xff, 0xff, 0xbf, 0x7f }
# else
#  define __nan_bytes { 0x7f, 0xbf, 0xff, 0xff }
# endif /* defined (ASF_LIL_ENDIAN) */
  static union { unsigned char __c[4]; float __d; } __nan_union = { __nan_bytes };
# define NAN __nan_union.__d
#endif /* NAN */

/* Specify necessary headers for the isnan() function/macro */
#if defined (linux) || defined(darwin) || defined (win32)
# include <math.h>
#else
# include <ieeefp.h>
# include <math.h>
#endif /* defined (linux) || defined(darwin) || defined (win32) */

#define ISNAN(X) (isnan(X))

#endif /* __ASF_NAN_H*/
