/*
 * Name: IK_Clnt.c
 *
 * Description: This file contains all the client-specific functions.
 *
 * Notes:
 *
 *-----------------------------------------------------------------------------
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
 * Revision 5.0  1995/11/06  13:03:26  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5  1995/07/27  18:41:48  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.2  1995/02/13  22:32:38  ims
 * COPIED FROM 4.3.1.3 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.3  1994/09/19  13:23:22  ryan
 * Added several close() statements so as to not leave open
 * but unconnected sockets lying around.
 *
 * Revision 4.3.1.2  1994/09/15  15:15:18  winter
 * Lots of code cleanup changes.
 *
 * Revision 4.3.1.1  1994/09/02  18:29:40  winter
 * Placeholder revision to start the branch.
 *
 * Revision 4.3  1994/08/26  10:49:07  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.3  1994/08/26  10:49:07  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0  1994/06/08  14:21:31  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.2.1.4  1994/05/23  18:07:50  ryan
 * moved IK_Ims.h to be the first include file.
 *
 * Revision 3.2.1.3  1994/05/11  11:40:11  winter
 * Fixed type ("grep ") on line 263.
 *
 * Revision 3.2.1.2  1994/05/11  10:54:58  winter
 * Changed the (struct sockadd *) cast to a (struct sockaddr *) cast.
 *
 * Revision 3.2.1.1  1994/05/11  10:49:17  winter
 * Placeholder revision to start the branch.
 * The intent of this branch (at the start, at least) is to fix a bug
 * discovered by Tze Hong and Brett Kerbel during the port to the Sun. On or
 * about line 165 (?), there is a cast of (struct sockadd) which should be
 * changed to (struct sockaddr).
 *
 * Revision 3.2  1994/05/03  20:10:55  ims
 * Promotion from BUILD_94MAY03.
 *
 * Revision 3.2  1994/05/03  20:10:55  ims
 * Promotion from BUILD_94MAY03.
 *
 * Revision 3.1.1.2  1994/04/18  14:05:34  winter
 * More cleanup during analysis by Eric Winter.
 *
 * Revision 3.1.1.1  1994/04/15  19:45:57  winter
 * Lots of cleanup changes and many, many, many new comments.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.2.2  1994/03/28  16:32:23  ryan
 * fixed up network code and added includes for SunOS port.
 *
 * Revision 3.0.2.1  1994/03/23  14:25:35  hu
 * add -- #include "IK_Ims.h"
 *
 * Revision 3.0.1.5  1994/02/14  21:28:37  ryan
 * a few small changes
 *
 * Revision 3.0.1.4  1994/02/14  15:52:05  hu
 * This has the Code Clean Up changes
 *
 * Revision 3.0.1.2  1994/01/28  19:03:05  ryan
 * removed superfluous fd_set variable which caused problems
 * under IRIX 5.1.
 *
 * Revision 3.0.1.1  1994/01/21  20:48:35  ryan
 * phase 1 cleanup
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.12  1993/08/31  21:26:05  sylvain
 * Re-wrote IK_Connect and IK_GetImage with one entry and one exit.
 * Explicitly call alarm () before and immediately after all slow system
 * 	calls to try and make it as atomic as possible.
 * I also check for errno == EAGAIN for POSIX compatibility.
 * Call IK_setsockopt() to set all of the socket options.
 * Use errno as a sanity check now.  So errno must be cleared upon entry to all
 * 	functions.
 * If NODELECT is declared then I use a loop count to determine if a connection
 * 	has goon away.
 *
 * Revision 1.8  1993/02/24  03:13:22  sylvain
 * added a signal call to catch SIGPIPE with IK_PipeHandler
 *
 * Revision 1.7  1993/01/22  20:03:28  sylvain
 * just cleaned IK_GetImage() up a bit
 *
 * Revision 1.6  1993/01/15  02:08:42  honce
 * doubles size of errbuf.  the sprintf at line 154 could
 * overrun 80 characters.
 *
 * Revision 1.5  1993/01/14  19:04:24  sylvain
 * added better error/syslog messages, and moved the DebugLevels into the
 * correct range
 *
 * Revision 1.4  1992/11/18  21:15:29  sylvain
 * made sure that allarm alarm calls were accompanied by an alarm call
 * to turn the alarm off.
 *
 * Revision 1.3  1992/10/29  17:38:30  sylvain
 * added IK_ImsErrno values, and debuged IK_GetImage finally.
 *
 * Revision 1.2  1992/10/21  16:09:16  sylvain
 * fixed typo
 *
 * Revision 1.1  1992/09/11  18:29:39  sylvain
 * ifdef'd out the DebugLevel decalaration if we're not debugging
 *
 * Revision 1.0  1992/08/31  08:51:53  sylvain
 * converted from SCCS to RCS
 *
 * Revision 0.5  92/08/25  10:08:36  sylvain
 * initial writting and port to UNIX and VMS.
 * */

/*****************************************************************************/

/* Create the RCS identifier string. */
static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* NOTE: sys/types.h must be #included before netinet/in.h, since the
   latter requires the u_long typedef. */
#include <sys/types.h>

/* Standard headers */
#include <assert.h>
#include <netdb.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

/* IMS headers */
#include "IK_Network.h"
#include "IK_Syslog.h"

/*****************************************************************************/

/* Local #define directives */

/* This constant is used to indicate that the default protocol should
   be used when creating a socket. */
#define USE_DEFAULT_SOCKET_PROTOCOL (0)

/*****************************************************************************/

/* External variable declarations */

/* IK_Signal_Brk is defined in IK_Comn.c.*/
extern IK_BOOLEAN IK_Signal_Brk;

/*****************************************************************************/

/* External function prototypes */

extern int IK_AddMonTS(IK_BOOLEAN f_transmitting, AGGREGATE *pagg_root,
 int i_mesgSize);

/*****************************************************************************/

/*
 * Name: IK_Connect()
 *
 * Description: This function makes a socket stream connection to the
 * DAAC server. This function makes a connection to the host
 * `host_addr` and returns the socket descriptor to it.
 *
 * Parameters:
 * char *pch_serverAddress - the hostname/address of the server
 * int i_serverPortNum - the port number to connect to on pch_serverAddress
 *
 * Return Values:
 * type: int
 * Returns: upon success, 0. On failure, -1 and errno, IK_ImsErrno.
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions
 *
 * Precondition 1: The system cannot create another socket (socket
 * failed; possibly out of buffers).
 * Postcondition 1: LOG a message to IK_Syslog and SET errno to
 * ENOBUFS, IK_ImsErrno to IK_ENOBUFS. Return (-1) as the value of the
 * function.
 *
 * Precondition 2: The pch_serverAddress is invalid.
 * Postcondition 2: LOG a ("invalid host: %s\n", pch_serverAddress) via
 * IK_Syslog, SET errno to EINVAL, IK_ImsErrno to IK_EINVAL and RETURN
 * (-1) as the value of the function.
 *
 * Precondition 3: The pch_serverAddress is valid but the server is not
 * available.
 * Postcondition 3: LOG "DAAC down" to syslog via IK_Syslog.  SET
 * errno to EHOSTDOWN, IK_ImsErrno to IK_EHOSTDOWN and RETURN (-1) as
 * the value of the function.
 *
 * Precodition 4: The conection was refused by the server.
 * Postcondition 4: SET errno to ECONNREFUSED, IK_ImsErrno to
 * IK_ECONNREFUSED, and RETURN (-1) as the value of the function.
 *
 * Precodition 5: The pch_serverAddress is valid and the server is available.
 * Postcondition 5: The socket descriptor of the connection is
 * returned as the value of the function.
 *
 * Revision History:
 *
 * Tuesday 6 September 1994 (Eric Winter) - Overhauled. Corrected call
 * to gethostbyname() - it was using improper arguments when a numeric
 * IP address was specified.
 * */

int IK_Connect(char *pch_serverAddress, int i_serverPortNum)
{

  /* i_sockfd is the socket descriptor. Why is this variable declared
     static? */
  static int i_sockfd;

  /* phostent_server points to information about the host address. */
  struct hostent *phostent_server = NULL;

  /* sockaddr_server contains the address of the server. */
  struct sockaddr_in sockaddr_server;

  /* i_returnCode is the return value for this function. */
  int i_returnCode = 0;

  /* s_errorMesg[] is used to create error messages. */
  char s_errorMesg[IK_MAXBUFLEN * 2];

  /*-------------------------------------------------------------------------*/

  /* Validate input. */
  assert(pch_serverAddress != NULL);
  assert(i_serverPortNum >= 0);

  /*-------------------------------------------------------------------------*/

  /* Clear the server address structure. */
  (void) memset(&sockaddr_server, 0, sizeof(sockaddr_server));

  /* The host was specified by a FQDN or dotted-decimal string. */
  phostent_server = gethostbyname(pch_serverAddress);
  if (phostent_server == NULL) {
    IK_vSyslog(LOG_ERR, "IK_Connect(): Invalid hostname: %s",
	       pch_serverAddress);
    errno = EINVAL;
    IK_ImsErrno = IK_EINVAL;
    i_returnCode = -1;
  } else {

    /* Set the socket address type to the server address type
       (always AF_INET). */
    sockaddr_server.sin_family = phostent_server->h_addrtype;

    /* Copy the IP address (a 32-bit number in network byte order) of
       the server to the IP address of the socket to connect to. Note
       that the address (actually, the first element of the array)
       pointed to by phostent_server->h_addr (an alias for
       h_addr_list[0]) is not a char *; it is actually a in_addr
       *. Therefore, the h_length member is always 4 (for now). */
    (void) memcpy(&sockaddr_server.sin_addr, phostent_server->h_addr,
		  phostent_server->h_length);

    /* Set the port number to connect to at the server end, in network
       byte order. */
    sockaddr_server.sin_port = htons(i_serverPortNum);

    /* Log a status message. */
    (void) sprintf(s_errorMesg, "IK_Connect(): requesting a connection to: "
		   "%s\n", pch_serverAddress);
    IK_DPRINTF(9, LOG_DEBUG, s_errorMesg);

    /* Create the socket as an Internet stream socket, using the
       default protocol for this socket type. */
    i_sockfd = socket(AF_INET, SOCK_STREAM, USE_DEFAULT_SOCKET_PROTOCOL);
    if (i_sockfd == -1) {
      IK_Syslog(LOG_ERR, "IK_Connect():unable to create socket");
      errno = ENOBUFS;
      IK_ImsErrno = IK_ENOMEM;
      i_returnCode = -1;
    } else {

      /* Bind and connect the socket to the host. */
      (void) alarm(IK_TIMEOUT_LENGTH);
      i_returnCode = connect(i_sockfd, (struct sockaddr *) &sockaddr_server,
			     sizeof(sockaddr_server));
      (void) alarm(0);
      if (i_returnCode == -1) {
	(void) sprintf(s_errorMesg, "IK_Connect(): Unable to connect to "
		       "server: errno = %d - %s", errno, strerror(errno));
	close(i_sockfd);
	errno = ECONNREFUSED;
	IK_ImsErrno = IK_ECONNREFUSED;
	IK_Syslog(LOG_ERR, s_errorMesg);
      } else if (IK_Signal_Brk) {

	/* We broke for a signal. Unable to connect. */
	close(i_sockfd);
	IK_Signal_Brk = IK_FALSE;
	errno = EHOSTDOWN;
	IK_ImsErrno = IK_EHOSTDOWN;
	i_returnCode = -1;

      } else {

	/* We connected! */
	i_returnCode = IK_setsockopt(i_sockfd);

      }

    }

  }

  /* If the socket was created and bound properly, return the socket
     descriptor. Otherwise, return -1. */
  return(i_returnCode < 0 ? -1 : i_sockfd);

}

/*****************************************************************************/

/*
 * Name: IK_GetImage()
 *
 * Description: This function reads an HDF image off the net. This
 * function is called when the network manager gets an ODL image
 * header, which indicates that an image is soon to follow.
 *
 * Parameters:
 * int socketno - the port to get the image from 
 * char* buffer - the buffer to put the image in 
 * unsigned short size - the size of the image 
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
 *     Precondition 1 :
 *     	   The connection timed out.
 *
 *     Postcondition 1 :
 *     	   LOG a "Connection to DAAC timed out" message via IK_Syslog, 
 *     	   and SET errno to ETIMEDOUT, IK_ImsErrno to IK_ETIMEDOUT.  
 *         And RETURN (-1) as the value of the function.
 *
 *     Precondition 2 :
 *     	   The connection failed.
 *
 *     Postcondition 2 :
 *     	   LOG a "Connection to DAAC failed" message via IK_Syslog, 
 *     	   and SET errno to EHOSTUNREACH, IK_ImsErrno to IK_EHOSTUNREACH.  
 *         And RETURN (-1) as the value of the function.
 *
 *     Precondition 3 : 
 *     	   The DAAC 'goes away' in the middle of transmitting an 
 *     	   image.
 * 
 *     Postcondition 3 :
 *     	   SET errno to EHOSTDOWN, IK_ImsErrno to IK_EHOSTDOWN, 
 *         and RETURN (-1) as the value of the function.
 *
 *     Precondition 4 :
 *     	   The buffer is filled with part of an image.
 *
 *     Postcondition 4 :
 *     	   read size bytes off the net, RETURN the size 
 *         of the image read.
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Tuesday 6 September 1994 (Eric Winter) - Overhauled.
 *
 * */

int IK_GetImage(int socketno, char *buffer, unsigned short size)
{

  /* num. of bytes written */
  int len = 0;

  /* no errors yet */
  int i_returnCode = 0;

  /* used to get around SOURCE QUENCE of server */
  int loop_cnt = 0;

  /* number of descriptors ready to read from */
  int ndesc;

  /* total number of bytes received */
  unsigned short total_count = 0;

  /* buffer for creating error to be logged to IK_Syslog */
  char s_errorMesg[IK_MAXBUFLEN];

  /* a pointer into buffer */
  char *ptr = buffer;

  /* the size of the image to send */
  unsigned short image_sz;

  struct timeval timeout;

  /*-------------------------------------------------------------------------*/

  if (errno != 0) {
    IK_vSyslog(LOG_ERR, "IK_GetImage(): errno was set upon entry, but never "
	       "cleared, errno = %d (%s)", errno, strerror(errno));
    IK_ImsErrno = IK_EINVAL;
    i_returnCode = -1;
  } else {

    /* There are no pending errors, so proceed normally. */
    IK_DFUN_CALL(sprintf(s_errorMesg, "IK_GetImage(): trying to read %u "
			 "bytes of the image", size));
    IK_DPRINTF(9, LOG_DEBUG, s_errorMesg);

    /* Read the image data from the socket. */
    for (image_sz = total_count = len = 0;
	 !i_returnCode && (total_count < size);
	 total_count += (int) (len > 0) ? len : 0)
      {
	image_sz = size - total_count;
	errno = 0;
	(void) alarm(IK_TIMEOUT_LENGTH);
	len = read(socketno, ptr, image_sz);
	(void) alarm(0);
	if (IK_Signal_Brk) { /* was SIGALRM called ? */
	  IK_Syslog (LOG_ERR, "IK_GetImage(): Connection while reading the "
		     "image, timed out");
	  IK_Signal_Brk = IK_FALSE; /* reset for next time */
	  i_returnCode = -1;
	} else if (errno == EINTR) { /* another signal interupt */
	  IK_vSyslog(LOG_ERR, "IK_GetImage(): Connection failed due to "
		     "signal interupt, errno = %d (%s)", errno,
		     strerror(errno));
	  i_returnCode = -1;
	} else {
	  if (errno == EWOULDBLOCK || errno == EAGAIN || len == 0) {
	    if (++loop_cnt >= IK_MAX_EWOULDBLOCK) {
	      /* maybe a source quench ? */
	      errno = EHOSTUNREACH;
	      IK_ImsErrno = IK_EHOSTUNREACH;
	      i_returnCode = -1;
	    }
	  } else if (len < 0 || errno != 0) {
	    IK_vSyslog(LOG_ERR, "IK_GetImage(): Connection to archive went "
		       "away, errno = %d (%s)", errno, strerror(errno));
	    errno = EHOSTUNREACH;
	    IK_ImsErrno = IK_EHOSTUNREACH;
	    i_returnCode = -1;
	  } else {
	    loop_cnt = 0;
	    IK_DFUN_CALL(sprintf (s_errorMesg, "IK_GetImage(): successfully "
				  "read %d bytes from the net", len));
	    IK_DPRINTF(9, LOG_DEBUG, s_errorMesg);
	    IK_ImsErrno = IK_EPROTO;
	    ptr += len > 0 ? len : 0;
	  }
	}
      }
    if (!i_returnCode)
      (void) IK_AddMonTS(0, NULL, size);
  }
  return(i_returnCode == -1 ? i_returnCode : total_count);
}
