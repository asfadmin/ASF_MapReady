/*
 * Warning, this file was automatically created by the TIFF configure script
 * VERSION:	 v3.4beta037
 * Double warning: This file was hacked by Patick Denny Oct 2002
 *                 Made to permanent file & depends on rules in 
 *                 asf_tools/make_support/system_rules
 */
#ifndef _PORT_
#define _PORT_ 1
#ifdef __cplusplus
extern "C" {
#endif
#if defined (ieee_big)
# define HOST_FILLORDER FILLORDER_MSB2LSB
#else /*ieee_lil*/
# define HOST_FILLORDER FILLORDER_LSB2MSB
#endif
#if defined (big_endian)
# define HOST_BIGENDIAN	1
#else  /*lil_endian*/
# define HOST_BIGENDIAN	0
#endif
#define HAVE_MMAP 1
#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
typedef double dblparam_t;
#ifdef __GNUC__
# ifdef __STRICT_ANSI__
#  define INLINE __inline__
# else
#  define INLINE inline
# endif
#else
# define INLINE
#endif

#define GLOBALDATA(TYPE,NAME)	extern TYPE NAME
#ifdef __cplusplus
}
#endif
#endif
