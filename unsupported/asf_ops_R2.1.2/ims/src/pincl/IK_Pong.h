/*
 * Name: IK_Pong.h
 *
 * Description: This file contains data definitions and function
 * prototypes for the interface to PONG message type.
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
 * Revision 5.0  1995/11/06  13:04:08  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:43:09  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.3.1.1  1994/09/02  12:57:46  winter
 * Added first functional PONG message identification and handling code.
 *
 * Revision 4.3  1994/09/01  15:02:55  winter
 * Initial checkin.
 *
 * */

/*****************************************************************************/

#ifndef __IK_PONG_H__
#define __IK_PONG_H__

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <stdio.h>   /* Needed for FILE in "odldef.h" */

/* Third-party library headers */
#include "odldef.h"

/* IMS headers */

/*****************************************************************************/

/* #define directives */

/* This string is the name assigned to the PONG ODL group. */
#define PONG_GROUP_NAME "PONG"

/* These strings are the names assigned to the parameters within the
   PONG ODL group. */
#define PONG_TX_CLIENT_PARAM_NAME "TX_CLIENT"
#define PONG_TX_SERVER_PARAM_NAME "TX_SERVER"

/*****************************************************************************/

/* Function prototypes */

extern IK_BOOLEAN IK_IsAPongMessage(AGGREGATE agg);
extern int IK_HandlePongMessage(AGGREGATE agg_pong, int i_sockfd);

/*****************************************************************************/

#endif  /* __IK_PONG_H__ */
