/* ASF stuff required for glib support */

#ifndef ASF_GLIB_H
#define ASF_GLIB_H

/* work-around for a bug in the windows version of glib */
#ifdef win32

#ifdef g_assert
#undef g_assert
#define g_assert(expr) assert(expr)
#endif

#ifdef g_assert_not_reached
#undef g_assert_not_reached
#define g_assert_not_reached() assert(FALSE)
#endif

#endif

#endif
