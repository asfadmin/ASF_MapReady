/*
 * Name: IK_Serv.c
 *
 * Description: This file contains the source to all of the IMS server
 * routines.
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
 * Revision 5.0  1995/12/07  14:18:17  ims
 * Promoted on 7 December 1995.
 *
 * Revision 4.5.1.2  1995/07/27  19:08:43  ims
 * COPIED FROM 4.4.1.2 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.2  1995/02/13  22:37:38  ims
 * COPIED FROM 4.3.1.2 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.2  1994/12/01  20:18:53  ryan
 * Reshuffled header files in order to compensate for Irix5.2 dependency.
 *
 * Revision 4.3.1.1  1994/09/02  12:55:55  winter
 * Finished first round of code cleanup.
 *
 * Revision 4.3  1994/08/26  10:50:13  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0  1994/06/08  15:20:20  ims
 * PROMOTION FROM REV4
 *
 * Revision 3.1.1.2  1994/05/23  18:27:47  ryan
 * moved IK_Ims.h to be the first include file.
 *
 * Revision 3.1.1.1  1994/05/23  18:27:16  ryan
 * new branch
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.3  1994/02/07  21:13:20  hu
 * This is the place holder revision that starts the branch.
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.11.1.1  1993/09/22  19:17:21  sylvain
 * Fixed a bug that was passing accept the pointer to the address structure
 * instead of the address structure itself.
 *
 * Revision 1.11  1993/08/31  21:26:49  sylvain
 * Added single entry/single exit to IK_InitConnection, IK_Accept and IK_PutImage.
 * Call IK_setsockopt() to set all of the socket options.
 * VMS specific select is only compiled now if compiling on vms and
 * 	NOSELECT is defined.
 * Added lingering to VMS IK_Accept().
 * Properaly cast arguments to select() for VMS IK_Accept().
 * Added select call to unix IK_Accept if NOSELECT is not defiend.
 * Explicitly call alarm () before and immediately after all slow system
 * 	calls to try and make it as atomic as possible.
 * I also check for errno == EAGAIN for POSIX compatibility.
 *
 * Revision 1.10  1993/02/26  17:08:07  sylvain
 * fixed a mismatching of braces in IK_Accept (unix version)
 *
 * Revision 1.9  1993/02/24  03:12:56  sylvain
 * added a signal call to catch SIGPIPE with IK_PipeHandler
 *
 * Revision 1.8  1993/02/12  18:53:17  sylvain
 * added a vms specific IK_Accept the uses qio to wait for connections.
 * This was implemented because IK_Accept was chewing up too
 * much of the cpu when it was just checking
 * for a connection.  Now it effectively sleeps until a connection
 * comes in.
 *
 * Revision 1.7  1993/01/15  17:45:58  sylvain
 * in IK_Accept, added a ntohl() call prior to getting the hostname of the
 * system that connected to our server
 *
 * Revision 1.6  1993/01/14  19:05:54  sylvain
 * added better error/syslog messages, and moved the DebugLevels into the
 * correct range
 *
 * Revision 1.5  1992/11/18  21:15:29  sylvain
 * made sure that allarm alarm calls were accompanied by an alarm call
 * to turn the alarm off.
 *
 * Revision 1.4  1992/11/16  14:11:04  sylvain
 * moved the signal handler call inside the IK_InitConnection
 *
 * Revision 1.3  1992/10/30  22:16:26  sylvain
 * cleaned up IK_PutImage a little bit.
 *
 * Revision 1.2  1992/10/29  17:38:30  sylvain
 * added IK_ImsErrno values, and debuged IK_PutImage finally.
 *
 * Revision 1.1  1992/09/11  18:28:50  sylvain
 * ifdef'd out the DebugLevel decalaration if we're not debuggin
 *
 * Revision 1.0  1992/08/31  08:52:55  sylvain
 * convert from sccs to rcs
 *
 * Revision 0.5  92/08/25  10:13:42  sylvain
 * initial code ported to UNIX and VMS.
 * 
*/

/*****************************************************************************/

/* Define the RCS identifier string. */
static const char *rcsid =
"$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

#include <sys/types.h>
#include <sys/time.h>

/* This header must be #included before the others to ensure the
   u_long type is available. */
#include <unistd.h>

/* Standard headers */
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <sys/socket.h>

/* Third-party library headers */

/* IMS headers */
#include "IK_Network.h"
#include "IK_Syslog.h"

/*****************************************************************************/

/* External variable declarations */

extern IK_BOOLEAN IK_Signal_Brk;
extern IK_BOOLEAN IK_Is_Server;

/*****************************************************************************/

/* Local variable definitions */

/* This constant is used to give a symbolic name to the default
   protocol option when creating a socket using the socket() system
   call. */
static const int i_useDefaultSocketProtocol = 0;

/*****************************************************************************/

/*
 * Name: IK_InitConnection()
 *
 * Description: This function gets ready to accept new client
 * connections on the specified TCP port. The requisite host
 * information is gathered, the socket is created and bound, and then
 * the socket is set to listen for connection attempts.
 *
 * Parameters:
 * int port - TCP port number to connect to
 *
 * Return Values:
 * int socketdesc - Descriptor of socket created
 * int -1 - If an error occurs
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * IK_ImsErrno
 * errno
 *
 * Pre- and Post-Conditions:
 *
 * Precondition 1: Unable to obtain host information.
 * Postcondition 1: Log error message, set IK_ImsErrno to IK_EPROTO,
 * errno to EPROTO, and return -1.
 *
 * Precondition 2: The system can not create another socket (the
 * socket() system call failed).
 * Postcondition 2: Log error message, set errno to ENOBUFS,
 * IK_ImsErrno to IK_ENOMEM. Return -1.
 *
 * Precondition 3: The address is already in use (the bind() system
 * call failed).
 * Postcondition 3: Log error message and set IK_ImsErrno to
 * IK_EPROTO. Shutdown the socket. Return -1 as the value of the
 * function.
 *
 * Precondition 4: The socket was created, the binding was
 * successfully made and the listening state was established.
 * Postcondition 4: The socket descriptor of the connection is
 * returned as the value of the function.
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Wednesday 31 August 1994 (Eric Winter) - Overhauled. Updated
 * precondition and postcondition analysis, and added lots of
 * comments.
 *
 * */

int IK_InitConnection(int port)
{

  /* sockdesc holds the socket descriptor that the port and address
     for this process will be bound to. N.B. WHY IS THIS VARIABLE
     DECLARED STATIC? It doesn't hurt anything, but is there some
     performance issue we should be aware of? */
  static int sockdesc;

  /* clnt_addr holds information about the clients that connect to the
     socket. */
  struct sockaddr_in clnt_addr;

  /* hp points to a hostent structure that contains information on the
     host running this process. */
  struct hostent *hp;

  /* thishost[] is the name of the host that this process is executing
     on.  */
  char thishost[MAXHOSTNAMELEN + 1];

  /* return_code is the execution status of this function. Initialize
     it to 0, since no errors have occurred yet. */
  int return_code = 0;

  /*-------------------------------------------------------------------------*/

  /* Clear the address structure used to hold the client address. */
  (void) memset(&clnt_addr, 0, sizeof(clnt_addr));

  /* Get the name of the host running this process. */
  (void) gethostname(thishost, (int) sizeof(thishost));

  /* Get the hostent data structure for this host. Note that
     gethostbyname() returns a pointer to a static data area, so the
     resulting hostent data structure must be copied if it is to be
     saved. */
  hp = gethostbyname(thishost);

  /* Verify that the hostent structure was obtained. */
  if (hp == NULL) {

    /* An error arose while trying to get the hostent data structure
       for this host. */
    IK_Syslog(LOG_ERR, "IK_InitConnection(): Couldn't get host information "
	      "for this host");
    IK_vSyslog(LOG_ERR, "\tUnable to resolve %s to an IP address - check the "
	       "name server", thishost);
    IK_ImsErrno = IK_EPROTO;
    errno = EPROTO;
    return_code = -1;

  } else {

    /* Fill in an address structure describing the clients which can
       connect to this socket. */

    /* The address type is (currently) always AF_INET, signifying an
       Internet TCP/IP address. */
    clnt_addr.sin_family = hp->h_addrtype;

    /* Indicate that this process will accept connection requests from
       any adress, and convert that specification to network byte
       order. */
    clnt_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    /* Add the port number to the client address, in network byte
       order. */
    clnt_addr.sin_port = htons(port);

    /* Create the socket to bind to the port. The new socket is
       specified to use Internet protocols (AF_INET), is a stream-type
       socket (SOCK_STREAM) as opposed to a datagram-oriented socket,
       and uses the default protocol for this socket type. */
    return_code = sockdesc = socket(AF_INET, SOCK_STREAM,
				    i_useDefaultSocketProtocol);

    /* Verify that the socket was created correctly. */
    if (return_code < 0) {
      IK_Syslog(LOG_ERR, "IK_InitConnection(): unable to create a socket");
      errno = ENOBUFS;
      IK_ImsErrno = IK_ENOMEM;
    } else {

      /* Bind the socket to this process and port. */
      return_code = bind(sockdesc, &clnt_addr, sizeof(clnt_addr));

      /* Verify that the binding took place coorectly. */
      if (return_code < 0) {
	IK_vSyslog(LOG_ERR, "IK_InitConnection(): tried to bind to an "
		   "address that was already being used, errno = %d - %s",
		   errno, strerror(errno));
	(void) IK_Shutdown(sockdesc);
	IK_ImsErrno = IK_EPROTO;
      } else {

	/* The socket is now properly bound, so begin listening for
           connection attempts on the socket. */
	(void) listen(sockdesc, IK_MAXQLEN);

      }

    }

  }

  /* Return the execution status of this function (-1 if an error
     occurred, or the socket descriptor if the socket was created,
     bound, and listening. */
  return(return_code < 0 ? -1 : sockdesc);

}

/*****************************************************************************/

/*
 * Name: IK_Accept()
 *
 * Description: This function accepts requests for new client
 * connections.
 *
 * Parameters:
 * int socketno - Descriptor for socket to accept connection on
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
 * dd mm yy (Eric Winter) - Started initial version.
 *
 * */

/*  this accepts connection requests for a new connection
 *
 *-------------------------UNIX VERSION---------------------------------
 *     Precondition 1 :
 *     	   No connections are waiting yet.
 *
 *     Postcondition 1 :
 *     	   set internal timer and wait for a connetion. If the timer 
 *     goes off before a connection is made, return -1, and set IK_ImsErrno
 *     and errno to {IK_}ETIMEDOUT, else return the socket for the new 
 *     connection.
 *
 *     Precondition 2 :
 *     	   A connection is waiting to be made.
 *
 *     Postcondition 2:
 *     	   Make the connections with the client and RETURN the 
 *     	   socket descriptor of the new connection.
 * 
 *     Precondition 3 : VMS VERSION - a qio error has occured.
 *
 *     postcondition 3 : log a messsage to syslog and set IK_ImsErrno, and 
 *     errno to IK_EPROTO, and return -1
 *
 * Returns: upon success, 0. On failure, -1 and errno, IK_ImsErrno. 
 *
 *--------------------------VMS VERSION---------------------------------------
 * VMS version contributed by   Eric Dorfman.
 * 
 * VMS Version of accept(), but one which sets a timer before attempting the
 * accept() call.  
 * 
 * There are four possible exit conditions:
 *
 *    1.  If the accept() succeeds before the specified timeout period
 *        elapses, returns whatever accept() would return.
 *
 *    2.  If the timeout period expires before a connection is accepted,
 *        returns -1 and sets IK_ImsErrno to IK_ETIMEDOUT.
 *
 *    3.  If an error occurs in the select() call, returns -1 and sets
 *        IK_ImsErrno to IK_EPROTO, the values of socket errno 
 *        will be written to syslog.
 *
 *    4.  If an error occurs elsewhere, returns -1 and sets IK_ImsErrno to 
 *        IK_EPROTO and logs a message to syslog.
 *
 * NOTES: 
 *    This grabs ahold of event flags 1 and 2, hardcoded below, and should
 *    be rewritten to allocate two free flags instead.  Shouldn't be a
 *    problem, though.
 */

int IK_Accept(int socketno)
{

  /* newsock is the descriptor for the socket created for the new
     connection. */
  int newsock;

  /* hp points to the hostent structure for the connecting client. */
  struct hostent *hp;

  /* peer contains the address structure for the client. */
  struct sockaddr_in peer;

  /* peer_sz is the size of the client address structure (peer). */
  int peer_sz;

  /* return_code is the execution status of this function (no errors
     yet). */
  int return_code = 0;

  /* ntoh_addr is the client address is host byte order. */
#ifdef ultrix
  int ntoh_addr;
#else
  unsigned long ntoh_addr;
#endif

  /* fdvar is the file descriptor set used to tell select() which
     socket to wait for. */
  fd_set fdvar;

  /* timeout holds the timeout parameters for the select() call. */
  struct timeval timeout;

  /* ndesc is returned by select() as the number sockets ready to be
     read or written. */
  int ndesc;

  /*-------------------------------------------------------------------------*/

  /* Calculate the size of the client address structure. */
  peer_sz = (int) sizeof(struct sockaddr_in);

#ifndef NOSELECT

  /* Set the timeout structure for the select() call. The default
     timeout value is 5 minutes. */
  timeout.tv_sec = IK_TIMEOUT_LENGTH;
  timeout.tv_usec = 0;

  /* Clear the file descriptor array for selct(), then set the flag so
     that only the desired descriptor is monitored. */
  FD_ZERO(&fdvar);
  FD_SET(socketno, &fdvar);

  /* Monitor the socket until input is ready, or the timeout alarm
     goes off. */
  ndesc = select(socketno + 1, &fdvar, (fd_set *) 0, (fd_set *) 0, &timeout);

  /* If select() returned an error.... */
  if (ndesc < 0) {

    /* ... process it. */
    if (errno == ENOMSG) {
      errno = EWOULDBLOCK;
    } else {
      IK_vSyslog(LOG_ERR, "IK_Accept(): select() failed, errno = %d - %s",
		 errno, strerror(errno));
    }
    return_code = -1;

  } else if (ndesc == 0) {

    /* The select() call returned 0, indicating that the timer
       expired. */
    errno = EWOULDBLOCK;
    IK_ImsErrno = IK_EWOULDBLOCK;
    return_code = -1;

  } else {

    /* The select() call indicated that the socket was ready for
       reading. */

#endif   /* !NOSELECT */

    /* Create a new socket by accepting the connection request. */
    newsock = accept(socketno, (struct sockaddr *) &peer, &peer_sz);

    /* Verify that the new socket was created properly. */
    if (newsock >= 0) {

      /* The new socket has been created, so convert the client
         address to host byte order. */
      ntoh_addr = ntohl(peer.sin_addr.s_addr);

      /* Look up the client host information using its address. */
      hp = gethostbyaddr(&ntoh_addr, sizeof(ntoh_addr), AF_INET);
      if (hp == NULL) {
	IK_vSyslog(LOG_ERR, "IK_Accept(): accepted a connection from [%s]",
		   inet_ntoa(peer.sin_addr));
      } else {
	IK_vSyslog(LOG_INFO, "IK_Accept(): accepted a connection from %s [%s]",
		   hp->h_name, inet_ntoa(peer.sin_addr));
      }

      /* Since we accepted a connection, we must be a server. */
      IK_Is_Server = IK_TRUE;

      /* Set the standard socket options for the new socket. */
      return_code = IK_setsockopt(socketno);

    } else {

      /* The accept() call failed. */
      IK_vSyslog(LOG_ERR, "IK_Accept(): unable to accept a new connection, "
		 "errno = %d - %s", errno, strerror(errno));
      return_code = -1;
      IK_ImsErrno = IK_EPROTO;

    }
#ifndef NOSELECT
  }
#endif /* !NOSELECT */

  /* If no errors occurred, return the descriptor for the
     newly-created socket. Return -1 if an error occurred. */
  return return_code == 0 ? newsock : -1;

}

/*****************************************************************************/

/*
 * Name: IK_PutImage()
 *
 * Description: This function puts an HDF image on the network.
 *
 * Parameters:
 * int socketno - the descriptor to write to
 * char *buffer - the image to put on the net
 * unsigned short image_chunk_sz - the size of the chunk to write
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
 *     	   LOG a "Connection to client has timed out" message via 
 *     	   IK_Syslog and SET errno to ETIMEDOUT, IK_ImsErrno to 
 *         IK_ETIMEDOUT. RETURN (-1) as the value of the function.
 *
 *     Precondition 2 :
 *     	   The connection has failed.
 * 
 *     Postcondition 2 :
 *     	   LOG a "Connection to DAAC failed" message via IK_Syslog and 
 *     	   SET errno to EHOSTUNREACH, IK_ImsErrno to IK_EHOSTUNREACH. 
 *         And RETURN (-1) as the value of the function.
 *
 *     Precondition 3 :
 *         the image buffer is written.
 *
 *     Postcondition 3 :
 *     	   Write the image to the port and RETURN the number of bytes 
 *     	   actually written. 
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Thursday 1 September 1994 (Eric Winter) - Overhauled.
 *
 * */

int IK_PutImage(int socketno, char *buffer, unsigned short image_chunk_sz)
{

  /* no error yet */
  int return_code = 0;

  /* num. of bytes written */
  int len;

  /* total number of bytes written to the net */
  int total_sent;

  /* a buffer to create error msgs in */
  char errbuf[IK_MAXBUFLEN];

  /* a ptr into the buffer to send */
  char *ptr;

  /* the size of the msg to send */
  unsigned short chunk_sz;

  /* useed to prevent SOURCE QUENCH */
  int loop_cnt = 0;

  /*-------------------------------------------------------------------------*/

  /* Verify that there are no unprocessed errors. */
  if (errno != 0) {
    IK_vSyslog (LOG_ERR, "IK_PutImage(): errno was set upon entry, but never "
		"cleared, errno = %d - %s", errno, strerror(errno));
    IK_ImsErrno = IK_EINVAL;
    return_code = -1;
  } else {
    IK_DFUN_CALL(sprintf(errbuf, "IK_PutImage(): writing %u bytes of the "
			 "image to the net", image_chunk_sz));
    IK_DPRINTF(9, LOG_DEBUG, errbuf);

    /* Send the image chunk. */
    for (ptr = buffer, total_sent = 0; 
	 return_code == 0 && total_sent < image_chunk_sz;
	 total_sent += (int) (len > 0) ? len : 0, ptr += (len > 0) ? len : 0) {

      /* Clear the error code for this pass (BAD FORM!). */
      errno = 0;

      /* Calculate the number of image bytes remaining to send. */
      chunk_sz = image_chunk_sz - (unsigned short) total_sent;

      /* Set the transmission timeout alarm. */
      (void) alarm(IK_TIMEOUT_LENGTH);

      /* Write the remainder of the image to the socket. */
      len = socket_write(socketno, ptr, chunk_sz);

      /* Deactivate the transmission timeout alarm. */
      (void) alarm(0);

      /* Check to see if a signal occurred. If so, process the signal
         based on its type. */
      if (IK_Signal_Brk) {

	/* A SIGALRM or SIGPIPE arose. */
	IK_vSyslog(LOG_ERR, "IK_PutImage(): Connection failed due to signal "
		   "interupt, errno = %d - %s", errno, strerror(errno));
	errno = EHOSTUNREACH;
	IK_ImsErrno = IK_EHOSTUNREACH;
	IK_Signal_Brk = IK_FALSE; /* reset for next time */
	return_code = -1;	

      } else if (errno == EINTR) {

	/* A interrupt signal was generated. */
	IK_vSyslog(LOG_ERR, "IK_PutImage(): Connection failed due to signal "
		   "interupt, errno = %d - %s", errno, strerror(errno));
	IK_ImsErrno = IK_EPROTO;
	return_code = -1;

      } else {

	/* Some other signal was raised. */
	if (errno == EWOULDBLOCK || errno == EAGAIN) {

	  /* Occurs if NOSELECT defined and no input is
             available. Increment the transmission attempt counter,
             and break out if the retry threshhold is exceeded. */
	  if (++loop_cnt >= IK_MAX_EWOULDBLOCK) {
	    errno = EHOSTUNREACH;
	    IK_ImsErrno = EHOSTUNREACH;
	    return_code = -1;
	  }

	}

	/* Check to see if an error occurred during the
           transmission. */
	if (len < 0) {
	  IK_vSyslog(LOG_ERR, "IK_PutImage(): unable to write the image to "
		     "the net, errno = %d - %s", errno, strerror(errno));
	  IK_ImsErrno = IK_EPROTO;
	  return_code = -1;
	} else {

	  /* The image was correctly transmitted. */
	  loop_cnt = 0;
	  IK_DFUN_CALL(sprintf(errbuf, "IK_PutImage(): sent %d bytes of the "
			       "image", len));
	  IK_DPRINTF(9, LOG_DEBUG, errbuf);

	}

      }

    }

  }

  /* If no errors occurred, return the total number of bytes
     transmitted. Otherwise, return the execution status. */
  return(return_code == 0 ? total_sent : return_code);

}
