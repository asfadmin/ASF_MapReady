/*
 * Name: IK_Pong.c
 *
 * Description: This file contains the code which implements the PONG
 * message type. The PONG message is used to query the status of a
 * remote IK layer.
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
 * Revision 4.5  1995/07/27  18:43:08  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.3.1.1  1994/09/02  12:57:23  winter
 * Added first functional PONG message identification and handling code.
 *
 * Revision 4.3  1994/09/01  15:02:32  winter
 * Initial checkin.
 *
 * */

/*****************************************************************************/

/* Define the RCS identifier string. */
static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <assert.h>
#include <stdio.h>   /* Needed for FILE in "odldef.h" */
#include <string.h>
#include <sys/time.h>

/* Third-party library headers */
#include "odldef.h"
#include "odlinter.h"

/* IMS headers */
#include "IK_Network.h"
#include "IK_Pong.h"
#include "IK_Syslog.h"

/*****************************************************************************/

/* External variable declarations */

extern IK_BOOLEAN IK_Is_Server;

/*****************************************************************************/

/* Local #define directives */

/* This constant is just used as a mnemonic for the (usually) NULL
   second argument to the gettimeofday() function. */
#define USE_DEFAULT_TIME_ZONE (NULL)

/*****************************************************************************/

/* Data type definitions */

/*****************************************************************************/

/* Local variable definitions */

/*****************************************************************************/

/* Local function prototypes */

/*****************************************************************************/

/*
 * Name: IK_IsAPongMessage()
 *
 * Description: This function determines if the ODL tree passed as its
 * argument is a valid PONG message.
 *
 * Parameters:
 * AGGREGATE agg - ODL aggregate for tree to examine
 *
 * Return Values:
 * IK_TRUE - If the tree is a valid PONG message tree
 * IK_FALSE - if not
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions:
 * None
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Thursday 1 September 1994 (Eric Winter) - Started initial version.
 *
 * */

IK_BOOLEAN IK_IsAPongMessage(AGGREGATE agg)
{
  /*-------------------------------------------------------------------------*/

  /* Validate input. */
  assert(agg != NULL);

  /*-------------------------------------------------------------------------*/

  /* Look for the PONG group within the tree. If found, this should be
     a PONG message tree. */
  if (FindGroup(agg, PONG_GROUP_NAME) != NULL) {
    return(IK_TRUE);
  } else {
    return(IK_FALSE);
  }

}

/*****************************************************************************/

/*
 * Name: IK_HandlePongMessage()
 *
 * Description: This function processes a PONG message by adding
 * timestamps to it and transmitting it back through the specified
 * socket.
 *
 * Parameters:
 * AGGREGATE agg_pong - The PONG aggregate to transmit
 * int i_sockfd - Descriptor for socket to write PONG message to
 *
 * Return Values:
 * int 0 - If all goes well
 * int -1 - If an error occurs
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions:
 * None
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Thursday 1 September (Eric Winter) - Started initial version.
 *
 * */

int IK_HandlePongMessage(AGGREGATE agg_pong, int i_sockfd)
{

  /* agg_pongGroup is the ODL aggregate for the PONG group within the
     PONG message. */
  AGGREGATE agg_pongGroup;

  /* tv_txServer is the time that this server bounces the PONG message
     back out the socket. */
  struct timeval tv_txServer;

  /* param_txServer is the ODL parameter for the server transmit
     time. */
  PARAMETER param_txServer;

  /* valdat_txServer is the ODL value data structure for the server
     transmit time. */
  VALUE_DATA valdat_txServer;

  /* val_txServer is the ODL value structure for the server transmit
     time. */
  VALUE val_txServer;

  /* pc_buf[] is a text buffer used for assembling and converting ODL
     values. */
  char pc_buf[IK_MAXBUFLEN];

  /*-------------------------------------------------------------------------*/

  /* Validate input. */
  assert(agg_pong != NULL);
  assert(i_sockfd >= 0);

  /*-------------------------------------------------------------------------*/

  /* Find the PONG group within the PONG message. */
  agg_pongGroup = FindGroup(agg_pong, PONG_GROUP_NAME);
  if (agg_pongGroup == NULL) {
    IK_Syslog(LOG_ERR, "IK_HandlePongMessage(): Can't find PONG ODL group");
    return(-1);
  }

  /* If this PONG message is received by a server... */
  if (IK_Is_Server == IK_TRUE) {

    /* ... find the TX_SERVER parameter, if it exists. If not, create
       it. */
    param_txServer = FindParameter(agg_pongGroup, PONG_TX_SERVER_PARAM_NAME);
    if (param_txServer == NULL) {

      /* The server transmit time parameter does not exist, so create
         it. */
      param_txServer = NewParameter(agg_pongGroup, KP_ATTRIBUTE,
				    PONG_TX_SERVER_PARAM_NAME);
      if (param_txServer == NULL) {
	IK_Syslog(LOG_ERR, "IK_HandlePongMessage(): Error adding server "
		      "transmit time parameter to PONG message");
	return(-1);
      }

      /* Indicate that the pong timestamp is a 2-part value (seconds
	 and microseconds). */
      param_txServer->value_kind = KV_SEQUENCE;
      param_txServer->rows = 1;
      param_txServer->columns = 2;

    }

    /* Get the current time to use as the server transmission time. */
    if (gettimeofday(&tv_txServer, USE_DEFAULT_TIME_ZONE) == -1) {
      IK_vSyslog(LOG_ERR, "IK_HandlePongMessage(): Error in call to "
		 "gettimeofday(): errno = %d (%s)", errno, strerror(errno));
      return(-1);
    }

    /* Add the timestamp values to the server transmit time
       parameter. */

    /* Seconds */
    (void) sprintf(pc_buf, "%ld", tv_txServer.tv_sec);
    valdat_txServer = ODLConvertInteger(pc_buf, strlen(pc_buf));
    val_txServer = NewValue(param_txServer, &valdat_txServer);
    if (val_txServer == NULL) {
      IK_Syslog(LOG_ERR, "IK_HandlePongMessage(): Could not add server "
		"transmit time seconds value");
      return(-1);
    }

    /* Microseconds */
    (void) sprintf(pc_buf, "%ld", tv_txServer.tv_usec);
    valdat_txServer = ODLConvertInteger(pc_buf, strlen(pc_buf));
    val_txServer = NewValue(param_txServer, &valdat_txServer);
    if (val_txServer == NULL) {
      IK_Syslog(LOG_ERR, "IK_HandlePongMessage(): Could not add server "
		"transmit time microseconds value");
      return(-1);
    }

    /* Send the message back out through the specified socket. */
    if (IK_TxODL(i_sockfd, &agg_pong) == -1) {
      IK_Syslog(LOG_ERR, "IK_HandlePongMessage(): Could not transmit pong "
		"message");
      return(-1);
    }

  } else {

    /* We are a client process, so do nothing for now. */

  }

  /* Return normally. */
  return(0);

}
