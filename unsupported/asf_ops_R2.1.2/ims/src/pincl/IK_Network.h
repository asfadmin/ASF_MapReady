/*
 * Name: IK_Network.h
 *
 * Description: This file contains definitions and includes for the
 * IMS network software.
 *
 * Notes:
 *
 *---------------------------------------------------------------------------
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
 * Revision 5.0.1.1  1995/11/06  13:47:43  ims
 * COPIED FROM 4.5.1.2 FOR GUI_PROMO_5_0_951106.
 *
 * Revision 4.5.1.2  1995/09/15  13:13:11  winter
 * THIS REVISION IS A MERGER OF 4.5.1.1 AND 4.5.2.2 AND SHOULD BE USED AS THE
 * BASIS FOR ALL FURTHER DEVELOPMENT.
 *
 * Revision 4.5.1.1  1995/07/27  19:02:51  ims
 * COPIED FROM 4.4.1.4 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.4  1995/05/25  20:54:14  ims
 * Replaced the line:
 * #include <sys/syslog.h>
 * by
 * #ifdef HPUX
 * #include <syslog.h>
 * #else
 * #include <sys/syslog.h>
 * #endif
 *
 * Revision 4.4.1.3  1995/02/13  22:36:52  ims
 * COPIED FROM 4.3.1.3 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.3  1995/01/17  13:55:34  ryan
 * changed sys/syslog.h to syslog.h.  Including sys/syslog.h directly
 * was incorrect.
 *
 * Revision 4.3.1.2  1994/08/26  12:02:27  ims
 * COPIED FROM REVISION 4.0.1.2.
 *
 * Revision 4.0.1.2  1994/07/08  11:59:35  winter
 * Fixed type, #efine->#define, and an unterminated comment.
 *
 * Revision 4.0.1.1  1994/06/08  17:14:12  ims
 * COPIED FROM REVISION 3.2.1.1.
 *
 * Revision 3.2.1.1  1994/05/04  19:29:53  winter
 * COPIED FROM REVISION 3.1.1.2.1.1.
 *
 * Revision 3.1.1.2.1.1  1994/04/18  13:53:19  winter
 * Extensive code cleanup during analysis by Eric Winter.
 *
 * Revision 3.1.1.2  1994/04/07  12:24:17  ryan
 * added logic to __sgi and TIME_FUNC definitions to avoid
 * complaints from the preprocessor.
 * [replicated from 3.0.1.4]
 *
 * Revision 3.1.1.1  1994/04/07  12:21:32  ryan
 * new branch
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.3  1994/03/17  19:39:07  ryan
 * cleaned up declarations for socket_{read,write,close,ioctl}
 *
 * Revision 3.0.1.2  1994/03/10  16:14:00  ryan
 * cleaned up macro definitions
 *
 * Revision 3.0.1.1  1994/02/02  20:45:23  ryan
 * starting a new branch
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.14  1993/06/25  19:53:01  sylvain
 * added 2 more definitions for the message passing code :
 *
 * 	IK_MAX_EWOULDBLOCK - the max number of successive retries before
 * 		declaring a session dead.
 *
 * 	RD_WAIT_DELAY - the delay time for select to wait for an incoming
 * 		connection.
 *
 * Revision 1.13  1993/02/24  03:22:07  sylvain
 * added prototpe for IK_PipeHandler
 
 * Revision 1.12  1993/02/01  21:21:07  honce
 * IK_NETBLOCK_SZ was defined as 64*1024, changed ((64*1024) - 1) because
 * the network code uses (unsigned short) which is too small for the old
 * definition.
 *
 * Revision 1.11  1993/01/06  21:05:17  sylvain
 * added an ifdef for DAAC_SERVER to take out the
 * GCMD stuff when a daac compiles this file
 *
 * Revision 1.10  1992/12/21  16:57:40  sylvain
 * added proto for IK_TxMD
 *
 * Revision 1.9  1992/12/16  20:09:55  sylvain
 * added prototype for getpid() which is used in IK_Comn.c to generate a unique
 * debugger file name
 *
 * Revision 1.8  1992/12/15  20:58:51  sylvain
 * a daac version
 *
 * Revision 1.6.1.2  1992/11/10  23:56:45  sylvain
 * fixed ultrix include problem
 *
 * Revision 1.6.1.1  1992/11/02  13:57:04  sylvain
 * fixed prototypes for IK_GetImage and IK_PutImage
 *
 * Revision 1.6  1992/10/29  17:48:10  sylvain
 * fixed prototypes for IK_GetImage and IK_PutImage
 *
 * Revision 1.5  1992/10/23  15:04:21  sylvain
 * fixed socket_{read*write} prototypes to correspond with vms.
 *
 * Revision 1.4  1992/10/21  18:41:00  sylvain
 * remote include for wait.h (it's non-ANSI)
 *
 * Revision 1.3  1992/10/16  18:14:47  sylvain
 * added prototype for IK_TxMD.
 *
 * Revision 1.2  1992/10/02  19:04:33  sylvain
 * delete include line that was put in at the last revision
 *
 * Revision 1.1  1992/10/02  17:31:49  sylvain
 * added the header file for the alarm(2) prototype
 *
 * Revision 1.0  1992/08/31  08:52:21  sylvain
 * convert from sccs to rcs
 *
 * Revision 0.5  92/08/25  10:12:33  sylvain
 * initial code ported to UNIX and vms.
 *
*/

/*****************************************************************************/

#ifndef __IK_NETWORK_H__
#define __IK_NETWORK_H__

/*****************************************************************************/


/* When compiling ANSI C code on the IRIX boxes (OS 4.0.1 at least),
   the compiler defines the symbol "__sgi". However, in order to get
   the prototypes for the socket code "sgi" needs to be defined. */

#if defined(__sgi) && !defined(sgi)
#define sgi   /* include prototypes from socket code */
#endif

/*****************************************************************************/

/* #include directives for VMS and UNIX */

/* Standard headers */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>

/* Third-party library headers */
#include "odldef.h"

/* IMS headers */
#include "IK_Ims.h"
#include "IK_Errno.h"

#ifndef DAAC_SERVER
#include "IK_Gcmd.h"
#endif

/*---------------------------------------------------------------------------*/

/* #define directives for VMS OR UNIX */

#ifdef vms   /* vms defined by VMS compiler */

#define MAXHOSTNAMELEN (64)

#define LOG_ERR    (3)    /* error conditions */
#define LOG_NOTICE (5)    /* normal but significant condition */
#define LOG_INFO   (6)    /* informational */
#define LOG_DEBUG  (7)    /* debug-level messages */
#define EPROTO     (71)   /* protocol error */
#define SIG_ERR    (-1)

#include <stat.h>
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]param.h"
#include "multinet_root:[multinet.include.sys]ioctl.h"
#include "multinet_root:[multinet.include.sys]fcntl.h"
#include "multinet_root:[multinet.include.sys]time.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"

#else   /* !vms */

#ifdef sgi

int gethostname(char*, int);

#endif

#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

#if !defined(EPROTO)
#define EPROTO EPROTOTYPE
#endif

#if !defined(AIX) && !defined (ultrix)
#include <sys/fcntl.h>
#endif

#if defined(AIX)
#include <sys/select.h>
#endif

#include <sys/stat.h>

#ifdef HPUX
#include <syslog.h>
#else
#include <sys/syslog.h>
#endif

#include <sys/ioctl.h>

#ifndef TIME_FUNC
#define TIME_FUNC gmtime
#endif   /* !TIME_FUNC */

#ifndef socket_read
#define socket_read read
#endif   /* !socket_read */

#ifndef socket_write
#define socket_write write
#endif   /* !socket_write */

#ifndef socket_close
#define socket_close shutdown
#endif   /* !socket_close */

#ifndef socket_ioctl
#define socket_ioctl ioctl
#endif   /* !socket_ioctl */

/* Prototypes that are in <unistd.h> but that include collides with
   too many ansi functions */
unsigned int alarm(unsigned int);
pid_t getpid(void);
int close(int);

#endif   /* !vms */

/*---------------------------------------------------------------------------*/

#ifndef RD_WAIT_DELAY
#define RD_WAIT_DELAY (1)   /* how long should select wait for a msg */
#endif

#ifndef LINGER_TIME
#define LINGER_TIME (10)   /* Time to wait for write to complete (sec) */
#endif

#ifndef IK_NETBLK_SZ
#define IK_NETBLK_SZ ((64 * 1024) - 1)   /* size bytes to read/write */
#endif

#ifndef IK_PORTNUM
#define IK_PORTNUM (3500)   /* default port number */
#endif

#ifndef IK_MAXQLEN
#define IK_MAXQLEN (5)   /* the length of the  listen queue */
#endif

#ifndef IK_TIMEOUT_LENGTH
#define IK_TIMEOUT_LENGTH (60 * 5)   /*  period to wait until a timeout */
#endif

#ifndef IK_MAX_EWOULDBLOCK
#define IK_MAX_EWOULDBLOCK (1000)
#endif

#ifndef IK_MIN_BLOCK_SZ
#define IK_MIN_BLOCK_SZ (100)   /* the minimum net chunk to try to read */
#endif

/*****************************************************************************/

/* Function prototypes */

/* Socket code prototypes */
extern int IK_InitConnection(int);
extern int IK_Accept(int);
extern int IK_Connect(char *, int);
extern int IK_RxODL(int, AGGREGATE *);
extern int IK_TxODL(int, AGGREGATE *);

#ifndef DAAC_SERVER
extern int IK_TxMD(AGGREGATE, IK_Gcmd ***);
#endif

extern int IK_PutImage(int, char *, unsigned short);
extern int IK_GetImage(int, char *, unsigned short);
extern int IK_Shutdown(int);
extern int IK_Close(int);
extern int IK_setsockopt(int);
extern int WriteBufferToSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen);
extern int ReadBufferFromSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen);

/* This is the timeout error handler. */
void IK_AlarmHandler(int);
void IK_PipeHandler(int);

/*****************************************************************************/

#endif   /* __IK_NETWORK_H__ */
