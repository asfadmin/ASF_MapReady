/*
 * Name: IK_Session.h
 *
 * Description: This file contains function prototypes and other
 * information needed to use the code in IK_Session.c.
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
 * Revision 5.0  1995/11/06  13:56:47  ims
 * COPIED FROM 4.5.1.2 FOR GUI_PROMO_5_0_951106.
 *
 * Revision 4.5.1.2  1995/10/12  19:11:52  winter
 * Added prototypes for IK_GetSessionStartTime() and IK_GetSessionID().
 *
 * Revision 4.5.1.1  1995/10/12  18:00:32  winter
 * Placeholder.
 *
 * Revision 4.5  1995/10/12  17:58:29  winter
 * Initial checkin.
 *
 * Revision 4.5  1995/10/12  17:58:29  winter
 * Initial checkin.
 *
 * */

/*****************************************************************************/

#ifndef __IK_SESSION_H__
#define __IK_SESSION_H__

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

/* Data types */

/*****************************************************************************/

/* External variable declarations */

/*****************************************************************************/

/* Function prototypes */

extern int IK_InitSessionData(void);
extern time_t IK_GetSessionStartTime(void);
extern char *IK_GetSessionID(void);

/*****************************************************************************/

#endif  /* __IK_SESSION_H__ */
