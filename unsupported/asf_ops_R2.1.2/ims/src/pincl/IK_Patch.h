/*
 * Name: IK_Patch.h
 *
 * Description: This file contains code which is used to patch bugs in
 * system libraries and any other required non-IMS fixes.
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:04:07  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:43:07  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:21:06  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:50:06  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.2  1994/06/23  16:14:21  winter
 * Added conditional prototype for strdup().
 *
 * Revision 4.0.1.1  1994/06/20  19:34:38  winter
 * Added prototype for ims_free().
 *
 * Revision 4.0  1994/06/20  16:40:04  ims
 * Initial checkin of stub code.
 *
 * */

/*****************************************************************************/

#ifndef __IK_PATCH_H__
#define __IK_PATCH_H__

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <sys/types.h>

/* Third-party library headers */

/* IMS headers */

/*****************************************************************************/

/* #define directives */

/*****************************************************************************/

/* Data type definitions */

/*****************************************************************************/

/* Function prototypes */

extern int ims_free(void *ptr);
extern void *ims_realloc(void *ptr, size_t size);

#ifndef HAVE_STRDUP
extern char *strdup(const char *s);
#endif   /* !HAVE_STRDUP */

/*****************************************************************************/

#endif  /* __IK_PATCH_H__ */
