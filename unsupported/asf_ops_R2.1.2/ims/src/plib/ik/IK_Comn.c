/*
 * Name: IK_Comn.c
 *
 * Description: This file contains the common functions that are used
 * by both the server and the client.
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
 * Revision 5.0  1995/11/06  13:03:28  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.6  1995/10/24  12:21:25  winter
 * Added code for server to clear STATS_REPORT tree after processing.
 *
 * Revision 4.5.1.5  1995/10/23  14:31:52  winter
 * Moved in code from IK_Nmfn.c which handles STATS_REPORT messages.
 *
 * Revision 4.5.1.4  1995/09/25  18:44:52  winter
 * Added Linux port code change by Leonid.
 *
 * Revision 4.5.1.3  1995/09/15  12:59:58  winter
 * THIS REVISION IS A MERGER OF 4.5.1.2 AND 4.5.2.2, AND SHOULD BE USED AS THE
 * BASIS FOR ALL FUTURE WORK. IF YOU HAVE ANY QUESTIONS, CONTACT ERIC.
 *
 * Revision 4.5.2.2  1995/09/14  20:17:45  lrosen
 * Made ReadBufferFromSocket globally available by removing "static".
 *
 * Revision 4.5.2.1  95/09/05  10:36:37  lrosen
 * 1. IK_RxODL did not handle Pong messages if it was called from a client.
 * With the new statistics report "ponger" that the client starts, IK_RxODL
 * needed to return the pong message to its calling routine if it is not a
 * server (ie. the stats ponger).
 * 
 * 2. For servers, IK_RxODL did not handle messages following a PONG.
 * The code would append any tree that followed the PONG tree to the PONG
 * tree.  It then identified this as another PONG message and sent it
 * back to the Ponger.  The fix done here is to clear the receive tree of
 * the PONG after handling it.  The QUIT that follows a PONG is now
 * correctly read and passed back to the calling program.
 * 
 * 3. IK_Stats needs access to function WriteBufferToSocket to
 * communicate with the ponger program.  For this reason,
 * WriteBufferToSocket is now made globally available by removing
 * "static" prefix and putting function declaration into IK_Network.c.
 * 
 * 4. Also, had to put in return code initialization fix: IK_RxODL
 * i_returnCode was not initialized to 0 so that this function always
 * returned whatever garbage it happened to be.  It is now initialized to
 * 0 on each entry.
 * 
 * Revision 4.5  95/07/27  18:41:51  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 * 
 * Revision 4.4.1.13  1995/03/28  13:29:47  winter
 * DOOH! I added back code to create the RxODLTmpFile. The code to do this had
 * been accidentally delete during an earlier revision on this branch. The
 * code was added at the end of IK_RxODL().
 *
 * Revision 4.4.1.12  1995/02/23  13:33:56  ryan
 * Added a few helpful comments.  Removed extraneous reference
 * to IK_revname.
 *
 * Revision 4.4.1.11  1995/02/22  12:47:23  ryan
 * Added select() call to IK_RxODL.  This change allows the calling
 * environment to poll the open socket connection.  This is also in
 * keeping with the expected behavior of the function.
 *
 * Revision 4.4.1.10  1995/02/13  22:34:03  ims
 * COPIED FROM 4.3.1.10 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.10  1994/11/28  14:32:14  ryan
 * Removed function WaitForSocket().  Function is no longer
 * necessary.
 *
 * Revision 4.3.1.9  1994/09/02  12:54:01  winter
 * Finished code cleanup and analysis.
 *
 * Revision 4.3.1.8  1994/09/01  17:59:37  winter
 * Rewrote IK_RxODL() to use new PONG functions, and added some assert()
 * macros ro check input. THIS REVISION REQUIRES IK_Pong.[ch]!
 *
 * Revision 4.3.1.7  1994/08/26  11:39:35  ims
 * COPIED FROM REVISION 4.0.1.10.
 *
 * Revision 4.0.1.10  1994/08/15  13:26:49  winter
 * Maintenance checkin; not done with this file yet.
 *
 * Revision 4.0.1.9  1994/07/25  15:53:51  winter
 * Changed two case () clauses to if () clauses, due to conflicting definitions
 * of EAGAIN and EWOULDBLOCK.
 *
 * Revision 4.0.1.8  1994/07/18  14:27:38  winter
 * Checkpoint chekin.
 *
 * Revision 4.0.1.7  1994/07/14  19:36:48  winter
 * Continued rewrite of IK_TxODL().
 *
 * Revision 4.0.1.6  1994/07/13  20:09:24  winter
 * Gutted IK_TxODL() and started using WriteFileToSocket(). Much easier to
 * read now - and it seems to work as expected.
 *
 * Revision 4.0.1.5  1994/07/13  16:19:33  winter
 * Added use of functions WriteODLTreeSizeToSocket() and WriteBufferToSocket()
 * to make IK_TxODL() _much_ more readable.
 *
 * Revision 4.0.1.4  1994/07/13  10:54:18  winter
 * Overhauled IK_RxODL() (mostly), but not done yet. This checkin is just to
 * save intermediate work.
 *
 * Revision 4.0.1.3  1994/07/08  20:00:53  winter
 * Code cleanup.
 *
 * Revision 4.0.1.2  1994/06/08  14:25:18  ims
 * COPIED FROM REVISION 3.2.1.6.
 *
 * Revision 3.2.1.6  1994/06/04  13:08:52  winter
 * Removed Stats calls; they have been moved to IK_Nmfn.c.
 *
 * Revision 3.2.1.5  1994/05/31  12:31:52  winter
 * Added code to PONG message handler to use high-resolution timestamps based
 * on the gettimeofday() function. Also added calls to IK_Stats_Update() just
 * before messages are transmitted and just after they are received. Also
 * did a lot of code cleanup and addition of comments.
 *
 * Revision 3.2.1.4  1994/05/18  14:35:55  ryan
 * modified IK_AddVersion.
 * 	1. moved VERSION group to on level deeper inside ODL message.
 * 	   this now matches the dictionary.
 * 	2. if the environment variable IMS_STAFF is set, then a parameter
 * 	   IMS_STAFF will be added to the VERSION group and the value
 * 	   will be copied.
 *
 * Revision 3.2.1.3  1994/05/09  15:45:43  winter
 * Removed uncommented note at the top of the file. DUMB!
 *
 * Revision 3.2.1.2  1994/05/09  13:37:37  winter
 * MERGED 3.2.1.1 AND 3.1.1.8.
 *
 * Revision 3.2.1.1  1994/05/04  17:43:30  ims
 * COPIED FROM REVISION 3.1.1.4.1.2.
 *
 * Revision 3.1.1.4.1.2  1994/05/04  17:38:28  winter
 * MERGER OF 3.1.1.4.1.1 AND 3.1.1.7.
 *
 * Revision 3.1.1.4.1.1  1994/04/18  14:07:15  winter
 * Extensive code cleanup during analysis by Eric Winter.
 *
 * Revision 3.1.1.4  1994/04/15  14:15:24  ryan
 * added PONG bouncing code to be called by server.  cleaned up
 * some of the other code.
 *
 * Revision 3.1.1.3  1994/04/07  18:26:34  ryan
 * added in "pong" message handling.  right now, servers and clients
 * will just *ignore* pong messages and will simply listen for another
 * message.
 *
 * Revision 3.1.1.2  1994/04/07  15:14:26  ryan
 * added handling for PROTOCOL_VERSION and CLIENT_VERSION to
 * all outgoing error messages.
 * also added hack for handling case where neither HAVE_SELECT nor
 * NOSELECT are defined.  default to assume that select() *does* exist.
 *
 * Revision 3.1.1.1  1994/04/06  19:55:52  ryan
 * new branch
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 *
 * Revision 1.29.1.4  1993/10/14  16:21:00  barth
 * By Greg Sylvain's direction:  added ifdef NOSELECT to drop looping when
 *      non-blocking calls are being used.
 *
 * Revision 1.29.1.3  1993/09/09  19:55:24  sylvain
 * remove re-declaration of variables that were used when doing the
 *      select in IK_RxODL.
 *
 * Revision 1.29.1.2  1993/09/08  11:43:43  sylvain
 * remove unneeded varaible sz_remaining when reading and writing the message
 *      size.  Replace this with the actual calculate to be done in the
 *      socket_{read|write} statement itself.
 *
 * Revision 1.29.1.1  1993/09/03  15:25:33  sylvain
 * fixed the bug with reading and writing very long (megabyte)
 *       messages.  (IK_{Rx|Tx}ODL)
 * The bug only manifested intself when the net was being heavily utilized.
 * In the loop that writes (and reads) the message size (sizeof (long)),
 *       the loop made sure that there were 4 bytes (sizeof (long))
 *       transmitted, but the pointer to the message size was
 *       never incremented when the when only part of the size was
 *       written.  So on the net loop, the same byte of the long
 *       was sent.
 *
 * Revision 1.29  1993/08/31  21:27:04  sylvain
 * Added the generaic function IK_setsockopt() that is called by both the
 *      server and the client to set the socket options and signal
 *      handlers.
 * Added a select call to IK_RxODL if compiled without NOSELECT.
 * All slow system calls are now immediatedly preceeded and postceeded by
 *      alarm() calls to try and make it as atomic as posible.
 * Removed the reading and writting of the bell character from the transmitting
 *      and receiving of messages.
 * Use errno as a sanity check now.  So errno must be cleared upon entry to all
 *      functions.
 * Check for errno == EAGAIN for POSIX complatibility.
 * If NODELECT is declared then I use a loop count to determine if a connection
 *      has goon away.
 * IK_Shutdown no longer sets errno (IK_ImsErrno) if there was a problem
 *      shuting down the socket.
 * Added the funcion IK_Close() to close the actual socket and file desciptor.
 *
 * Revision 1.27  1993/03/04  17:57:02  honce
 * IK_PipeHandler() reset signal(SIGPIPE, IK_AlarmHandler) instead of
 * signal(SIGPIPE, IK_PipeHandler) as it should have.
 *
 * Revision 1.26  1993/03/03  00:44:12  honce
 * changed calls to IK_LOGPROTOERR() to really use the last two arguments.
 *
 * Revision 1.25  1993/02/24  03:12:20  sylvain
 * added IK_PipeHandler for handling SIGPIPE.  it sets errno to EHOSTUNREACH
 *
 * Revision 1.24  1993/02/12  18:14:22  sylvain
 * figured out when the last case for a failure of a socket write would
 * occur on vms.  Now I log an appropriate message to syslog.
 *
 * Revision 1.23  1993/01/15  02:12:40  honce
 * doubled size of errbuf.  Some of the sprintf's overran old
 * size.
 *
 * Revision 1.22  1993/01/14  19:04:24  sylvain
 * added better error/syslog messages, and moved the DebugLevels into the
 * correct range
 *
 * Revision 1.21  1993/01/13  17:10:59  sylvain
 * fixed a bug that prevent the detection of the other end of the connection
 * being dropped
 *
 * Revision 1.20  1992/12/16  21:41:05  sylvain
 * made the debugger pathname configurable at runtime, via IK_SysConfig
 *
 * Revision 1.19  1992/11/18  21:15:29  sylvain
 * made sure that allarm alarm calls were accompanied by an alarm call
 * to turn the alarm off.
 *
 * Revision 1.18  1992/11/13  15:26:55  sylvain
 * IK_TxODL and IK_RxODL wont return if we can't open the debugger file
 *
 * Revision 1.17  1992/11/12  22:35:17  sylvain
 * IK_RxODL and IK_TxODL now create a sperate file when NDEBUG IS NOT defined
 * on compilation.  this is the debugger file to look at the actuall messages
 * being sent/recieved
 *
 * Revision 1.16  1992/11/11  22:24:03  sylvain
 * I only check for a pointer to be passed to the IK_?xODL now, I was getting
 * too many clashes by looking for a "root" tree.
 *
 * Revision 1.15  1992/11/11  19:49:23  sylvain
 * return blocking if we were unable to read ANYTHING from the socket
 * (len == 0, EOF)
 *
 * Revision 1.14  1992/11/05  19:34:18  sylvain
 * fixed the fix for the IBM
 *
 * Revision 1.13  1992/11/05  16:15:57  sylvain
 * fixed the ODL expression that verified the
 * arguemnt passed in to IK_TxODL and IK_RxODL as a NULL tree, to work on IBM
 *
 * Revision 1.12  1992/10/29  17:38:30  sylvain
 * added IK_ImsErrno values, updated comments, did some perfermance
 * enhancements for IK_RxODL and IK_TxODL.
 *
 * Revision 1.11  1992/10/23  14:49:49  sylvain
 * ported the previous pathces to vms and fixed IK_AddMonTS() so that it now
 *     will replace the time stamp  with a new one if one exists, or just
 *     create one.
 *
 * Revision 1.10  1992/10/21  16:21:30  honce
 * Added ;'s to end of IK_DPRINTF().
 *
 * Revision 1.9  1992/10/21  15:07:47  sylvain
 * IK_RxODL only open the tmpfile when it actually receives the head of
 * a message.
 *
 * Revision 1.8  1992/10/16  16:26:38  sylvain
 * more bug fixes, added a continue to the loop that got the msg size and
 * fixed a bug that caused the msg size to grow when an error was encountered
 *
 * Revision 1.7  1992/10/14  19:30:43  sylvain
 * put an extra loop around the socket_write call to make sure everything was
 * written, and replaced the fputs in the IK_RxODL with a fwrite call.
 *
 * Revision 1.6  1992/10/13  21:11:15  sylvain
 * cleared errno value for fread in IK_TxODL
 *
 * Revision 1.5  1992/10/09  13:02:48  sylvain
 * move a WriteLabel that output thet label to stderr, within a NDEBUG ifdef
 * block.
 *
 * Revision 1.4  1992/10/07  12:37:13  sylvain
 * fixed a bug in the last version,  IK_RxODL never received any mesages
 *
 * Revision 1.3  1992/10/06  11:51:08  sylvain
 * prepenned the transmitions of a bel character to get around garbadge that w
 * was being read on multiple transmissions on the same socket.
 *
 * Revision 1.2  1992/09/03  21:05:17  sylvain
 * deletes tmp files on VMS now.
 *
 * Revision 1.1  1992/09/02  17:09:20  sylvain
 * add function prototype for IK_AddMonTS
 *
 * Revision 1.0  1992/08/31  08:52:07  sylvain
 * convert from SCCS to RCS
 *
 * Revision 0.5  92/08/25  10:10:02  sylvain
 * initial code ported to UNIX and vms.
 *
*/

/*****************************************************************************/

/* Define the RCS identifier string. */
static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* This header must be #included _before_ sys/stat.h, for the
   definition of the dev_t type. */
#include <sys/types.h>

/* Standard headers */
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#if defined(__linux__) || defined(LINUX386)
#include <linux/limits.h>
#endif

/* Third-party library headers */
#include "odldef.h"
#include "odlinter.h"

/* IMS headers */
#include "IK_Errno.h"
#include "IK_Network.h"
#include "IK_Pong.h"
#include "IK_Syslog.h"

/*****************************************************************************/

/* Local #define directives */

/* HP/UX supports filesystems with short filenames (15 characters) */
#ifdef HPUX
#undef FILENAME_MAX
#define FILENAME_MAX (255)
#endif

/* By default, assume that we *DO* have a select() call.  -pmr */
#if !defined(NOSELECT) && !defined(HAVE_SELECT)
#  define HAVE_SELECT
#endif

/*
 * Note: For some bizarre reason, the functions select, setsockopt,
 * and ioctl do not always have prototypes available from system
 * header files.  In POSIX-compliant systems, these will be found in
 * <unistd.h>.  You have been warned.
 */


/* Various macros that are used everywhere. They are just here to try
   and conserve some space. */

#define IK_LOGPROTOERR(fp, errstr, a1, a2) { \
          IK_vSyslog(LOG_ERR, errstr, a1, a2); \
          errno = EPROTO; \
          IK_ImsErrno = IK_EPROTO; \
          if (fp != (FILE*) NULL) { \
            IK_CLOSEFILES(fp); \
          } \
          i_returnCode = -1; \
        }

/*---------------------------------------------------------------------------*/

/* This constant is the countdown time for the alarm() signal call
   when used to wait for a timeout on a write() to a socket. */
#define SOCKET_WRITE_ALARM_TIME (IK_TIMEOUT_LENGTH)

/* This constant is the countdown time for the alarm() signal call
   when used to wait for a timeout on a read() from a socket. */
#define SOCKET_READ_ALARM_TIME (IK_TIMEOUT_LENGTH)

/* This constant is used to specify that when shutting down a socket,
   all further read and write attempts on the socket will be
   disallowed. */
#define SOCKET_SHUTDOWN_READ_WRITE (2)

/*---------------------------------------------------------------------------*/

/* This constant is the name to use for the VERSION ODL group. */
#define VERSION_GROUP_NAME "VERSION"

/* These constants define the names of the parameters within the
   VERSION group. */
#define CLIENT_VERSION_PARAM_NAME "CLIENT_VERSION"
#define PROTOCOL_VERSION_PARAM_NAME "PROTOCOL_VERSION"
#define IMS_STAFF_PARAM_NAME "IMS_STAFF"

/* This constant defines the maximum length of a protocol versions
   string. */
#define MAX_PROTOCOL_VERSION_LENGTH (16)

/* This constant holds the name of the IMS_STAFF environment
   variable. */
#define IMS_STAFF_ENVVAR_NAME "IMS_STAFF"

/*---------------------------------------------------------------------------*/

/* IK_STATS_LOG_ENV_VAR is the name of the environment variable which
   should be set at the DAAC server end to specify the complete path
   to the file in which to place the incoming statistics reports. If
   this environment variable is not set, a default directory and file
   will be used (specified by the macro IK_DEFAULT_STATS_LOG). */
#define IK_STATS_LOG_ENV_VAR "IMS_STATS_LOG"

/* IK_DEFAULT_STATS_LOG specifies the default path for the statistics
   report log. This value is used to determine the directory and file
   in which to place the statistics log only if the environment
   variable specified by IK_STATS_LOG_ENV_VAR is not specified. */
#define IK_DEFAULT_STATS_LOG "./statslog.txt"

/* This is the name of a STATS_REPORT ODL group. */
#define STATS_REPORT_GROUP_NAME "STATS_REPORT"

/* These are the names of the parameters in a stats report group. */
#define STATS_REPORT_LENGTH_PARAM_NAME "STATS_REPORT_LENGTH"
#define STATS_REPORT_STRING_PARAM_NAME "STATS_REPORT_STRING"

/*---------------------------------------------------------------------------*/

#ifdef DEBUG

/* This is the prefix to use for the name of the transmit archive file
   (used for debugging). */
#define DEBUG_TX_ARCHIVE_PREFIX "TxODLTmpFile"

/* This is the prefix to use for the name of the receive archive file
   (used for debugging). */
#define DEBUG_RX_ARCHIVE_PREFIX "RxODLTmpFile"

#endif /* DEBUG */

/*****************************************************************************/

/* Global variable definitions */

/* This flag is set to IK_TRUE when an alarm signal (SIGALRM) is
   generated during a read or write on the socket, i.e. a connection
   timeout. */
IK_BOOLEAN IK_Signal_Brk = IK_FALSE;

/* Are we on the server side? */
IK_BOOLEAN IK_Is_Server = IK_FALSE;

/*****************************************************************************/

/* External function prototypes */

/* Function to add timestamp group, from IK_ODLetc.c. */
extern int IK_AddMonTS(IK_BOOLEAN tx, AGGREGATE *pagg_root, int size);

/*****************************************************************************/

/* Local function prototypes */

static int IK_AddVersion(AGGREGATE root);

static int WriteFileToSocket(int i_sockfd, const char *pc_filename,
			     long *pl_fileSize);
int WriteBufferToSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen);

static int ReadFileFromSocket(int i_sockfd, const char *pc_filename,
			      long *pl_fileSize);
int ReadBufferFromSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen);
static IK_Boolean IK_IsAStatsReportMessage(AGGREGATE agg);
static int IK_IngestStatsReport(AGGREGATE agg_root);
 
/*****************************************************************************/

/*
 * Name: IK_RxODL()
 *
 * Description: Read an ODL tree from the specified socket and place
 * its contents into the ODL tree pointed to by pagg_root. This
 * function is called when the calling process is ready to process
 * another ODL tree. It reads the ODL label from the socket, then
 * converts the ODL label to a label file on disk, and then reads in
 * and returns that tree.
 *
 * Parameters:
 * int i_sockfd - Socket descriptor to read from
 * AGGREGATE *pagg_root - Pointer to root AGGREGATE for tree to fill in
 * with data read from the socket
 *
 * Return Values:
 * Upon success, the size of the message read in.
 * On failure, -1 and set errno, IK_ImsErrno.
 *
 * Warnings:
 *
 * Global Variables Used:
 * errno
 * IK_ImsErrno
 *
 * Pre- and Post-Conditions
 * Not available 
 *
 * Notes:
 *
 * 1) the format of a message that will be received is :
 * <msg length> <msg>
 *
 * IK_RxODL() will return immediately, if there is nothing to be read.
 * The calling function should re-call IK_RxODL() to check for input
 * at a later time.
 *
 * Revision History:
 *
 * 26 May 1994 (Eric Winter) - Modifed PONG handler code to use
 * higher-resolution timestamps.
 *
 * Tuesday 12 July 1994 (Eric Winter) - Added NULL check for root
 * argument. Removed vms support. Changed !NDEBUG cases to
 * DEBUG. Changed for() loops to more easily understood while() loops.
 *
 * Thursday 1 September 1994 (Eric Winter) - Rewrote to use
 * IK_IsAPongMessage() and IK_HandlePongMessage(), as well as assert()
 * for input validation.
 *
 * Tuesday 28 March 1995 (Eric Winter) - DOOH! Added back code to
 * create RxODLTmpFile. This code had been accidentally deleted during
 * an earlier revision.
 *
 * Monday 23 October 1995 (Eric Winter) - Added code to process
 * STATS_REPORT messages. It weas moved here from IK_Nmfn.c.
 * */

int IK_RxODL(int i_sockfd, AGGREGATE *pagg_root)
{

    /* This flag causes the function to loop around in case of a pong
       message. */
    IK_BOOLEAN f_listening;

    /* i_returnCode contains the execution status of this function. */
    int i_returnCode;

    /* l_NBOODLfileSize is the ODL file size in Network Byte Order
       (NBO). */
    long l_NBOODLfileSize;

    /* l_ODLfileSize is the size of the ODL label file, which is the
       same as the size of the message to receive. */
    long l_ODLfileSize;

    /* pc_ODLlabelFilename is the path to the temporary file that
       holds the ODL label for the ODL tree to read from the
       socket. */
    char pc_ODLlabelFilename[MAXPATHLEN];

    /* pf_ODLlabelFile points to the FILE for the temporary ODL label
       file which holds the incoming ODL tree. */
    FILE *pf_ODLlabelFile;

#ifdef HAVE_SELECT

    /* fdset_waitingSocket is the structure that tells select() what
       socket to wait for. */
  fd_set fdset_waitingSocket;

    /* tv_timeout is used to hold the seconds and microseconds for the
       timeout period for the select() call. */
    struct timeval tv_timeout;

    /* i_numPendingSockets is the return value from select(), i.e. the
       number of descriptors ready for input. */
    int i_numPendingSockets;

#endif /* HAVE_SELECT */

    /*------------------------------------------------------------------------*/

    /* Validate input. */
    assert(i_sockfd >= 0);
    assert(pagg_root != NULL);
    assert(*pagg_root != NULL);

    /*------------------------------------------------------------------------*/

    /* Initialize the return code to 0 */
    i_returnCode = 0;

    /* Verify that there are no unprocessed errors. */
    if (errno != 0) {
	IK_vSyslog(LOG_ERR, "IK_RxODL(): errno was set upon entry, but never "
		   "cleared; errno = %d (%s)", errno, strerror(errno));
	IK_ImsErrno = IK_EINVAL;
	return(-1);
    }

    /*------------------------------------------------------------------------*/

#ifdef HAVE_SELECT

    /* Initialize the timeout length for the call to select(). Note
       that the value RD_WAIT_DELAY is defined in IK_Network.h as 1
       second, as of this writing (27 March 1995). */
    tv_timeout.tv_sec = RD_WAIT_DELAY;
    tv_timeout.tv_usec = 0;

    /* Specify the socket we want to wait on. */
    FD_ZERO(&fdset_waitingSocket);
    FD_SET(i_sockfd, &fdset_waitingSocket);

    /* Wait for input on the socket. */
    i_numPendingSockets = select(i_sockfd + 1, &fdset_waitingSocket,
				 (fd_set *) NULL, (fd_set *) NULL, &tv_timeout);

    /* Does the socket have pending data? */
    if (i_numPendingSockets == 0) {

	/* The socket has no pending data, so the timeout occured. */
	errno = EWOULDBLOCK;
	IK_ImsErrno = IK_EWOULDBLOCK;
	i_returnCode = -1;
	return(i_returnCode);

    } else if (i_numPendingSockets < 0) {

	/* An error occurred during the select() call. */
	IK_vSyslog(LOG_ERR, "IK_RxODL(): select() failed, errno = %d - %s",
		   errno, strerror(errno));
	IK_ImsErrno = IK_EPROTO;
	i_returnCode = -1;
	return(i_returnCode);

    }

    /* Check to see if there is input waiting on our specified
       socket. */
    if (FD_ISSET(i_sockfd, &fdset_waitingSocket) <= 0) {
	IK_vSyslog (LOG_ERR, "IK_RxODL(): select() failed, errno = %d - %s",
		    errno, strerror(errno));
	IK_ImsErrno = IK_EPROTO;
	i_returnCode = -1;
	return(i_returnCode);
    }

    /* If we got this far, select() is telling us that there is data
       waiting to be read in.*/
 
#endif /* HAVE_SELECT */
  
    /*------------------------------------------------------------------------*/

    /* Initialize the listening flag to IK_TRUE, since we have not yet
       received a message (pong or non-pong). */
    f_listening = IK_TRUE;

    /* Sit in a loop and wait for a message. As long as f_listening is
       IK_TRUE, we will listen at the socket. When a non-pong message
       has been received, set f_listening to IK_FALSE and break out of
       the listening loop. We do it this way because, if a "pong"
       message comes in, we simply want to handle it and then wait for
       another message. */

    do {  /* while listening */

	/* Read the message size from the socket. */
	if (ReadBufferFromSocket(i_sockfd, &l_NBOODLfileSize,
				 sizeof(l_NBOODLfileSize)) == -1) {
	    IK_Syslog(LOG_ERR, "IK_RxODL(): Error reading message size from "
		      "socket");
	    i_returnCode = -1;
	    break;
	}

	/* Convert the message size from NBO to HBO. */
	l_ODLfileSize = ntohl(l_NBOODLfileSize);

	/* Create a temporary filename for the file to hold the ODL
	   label for the ODL tree to be received. */
	(void) tmpnam(pc_ODLlabelFilename);
	if (pc_ODLlabelFilename[0] == '\0') {
	    IK_vSyslog(LOG_ERR, "IK_RxODL(): Unable to generate a unique "
		       "temporary filename; errno = %d (%s)", errno ,
		       strerror(errno));
	    IK_ImsErrno = IK_ENOMEM;
	    i_returnCode = -1;
	    break;
	}

	/* DANGER DANGER : In SunOS 4.1.1, errno is always set after
	   this call, but tmpfile() is able to create the temporary
	   file ????? */
	errno = 0;

	/* Read the message from the socket into the ODL label
           file. */
	if (ReadFileFromSocket(i_sockfd, pc_ODLlabelFilename, &l_ODLfileSize)
	    == -1) {
	    IK_Syslog(LOG_ERR, "IK_RxODL(): Error reading ODL message into "
		      "file");
	    i_returnCode = -1;
	    break;
	}

	/* Open the label file for reading. */
	pf_ODLlabelFile = fopen(pc_ODLlabelFilename, "r");
	if (pf_ODLlabelFile == NULL) {
	    IK_vSyslog(LOG_ERR, "IK_RxODL(): Can't open ODL label file %s for "
		       "reading; errno = %d (%s)", pc_ODLlabelFilename, errno,
		       strerror(errno));
	    i_returnCode = -1;
	    break;
	}

	/* Convert the ODL label file to an in-memory ODL tree. */
	if (ReadLabel(pf_ODLlabelFile, *pagg_root) == 0) {
	    IK_vSyslog(LOG_ERR, "IK_RxODL(): An ODL tree was read off the net, "
		       "but it was un-parsable; errno = %d (%s)",
		       errno, strerror(errno));
	    (void) RemoveAggregate(*pagg_root);
	    *pagg_root = NULL;
	    i_returnCode = -1;
	    break;
	}

	/* Close the ODL label file. */
	if (fclose(pf_ODLlabelFile) == EOF) {
	    IK_vSyslog(LOG_ERR, "IK_RxODL(): Error closing ODL label file %s; "
		       "errno = %d (%s)", pc_ODLlabelFilename, errno,
		       strerror(errno));
	    (void) RemoveAggregate(*pagg_root);
	    *pagg_root = NULL;
	    i_returnCode = -1;
	    break;
	}

	/* Add the incoming time stamp to the root aggregate. Note
	   that this is the first thing added to an incoming tree. */
	if (IK_AddMonTS(IK_FALSE, pagg_root, (int) l_ODLfileSize) < 0) {
	    IK_Syslog(LOG_ERR, "IK_RxODL(): The time stamps could not be "
		      "added to the ODL Tree");
	    (void) RemoveAggregate(*pagg_root);
	    *pagg_root = NULL;
	    IK_ImsErrno = IK_EPROTO;
	    i_returnCode = -1;
	    break;
	}

	/*--------------------------------------------------------------------*/

	/* If the newly-arrived ODL tree is for a PONG or STATS_REPORT
	   message, process it and then loop back to wait for another
	   message. */
	if (IK_IsAPongMessage(*pagg_root) == IK_TRUE) {

	    /* This is a PONG message.  If server, process it and
               bounce it back out the socket.  If client, then exit
               listening loop and return the pong message to the
               calling program (probably the statistics Ponger). */

	    if (IK_Is_Server == IK_TRUE) {
		if (IK_HandlePongMessage(*pagg_root, i_sockfd) == -1) {
		    IK_Syslog(LOG_ERR, "IK_RxODL(): Error handling PONG "
			      "message");
		    (void) RemoveAggregate(*pagg_root);
		    *pagg_root = NULL;
		    IK_ImsErrno = IK_EPROTO;
		    i_returnCode = -1;
		    break;
		}

		/* Server has handled PONG.  Clear the received
                   tree. */
		(void) RemoveAggregate(*pagg_root);
		*pagg_root = NewAggregate(NULL, KA_GROUP, "ROOT", NULL);
		if (*pagg_root == NULL){
		    IK_Syslog(LOG_ERR, "IK_RxODL:Failed New Aggregate call\n");
		    *pagg_root = NULL;
		    i_returnCode = -1;
		    break;
		}

	    } else {          /* Client.  Exit listening loop. */
		f_listening = IK_FALSE;
	    }

	} else if (IK_IsAStatsReportMessage(*pagg_root) == IK_TRUE) {

	    /* A STATS_REPORT message was received. If this is a
               server process, ingest the pong message. Otherwise,
               this is a client process, so exit the listening
               loop. */
	    if (IK_Is_Server == IK_TRUE) {
		if (IK_IngestStatsReport(*pagg_root) == -1) {
		    IK_Syslog(LOG_ERR, "IK_RxODL(): Error ingesting stats "
			      "report message");
		    (void) RemoveAggregate(*pagg_root);
		    *pagg_root = NULL;
		    IK_ImsErrno = IK_EPROTO;
		    i_returnCode = -1;
		    break;
		}

		/* Server has handled a STATS_REPORT. Clear the
                   received tree. */
		(void) RemoveAggregate(*pagg_root);
		*pagg_root = NewAggregate(NULL, KA_GROUP, "ROOT", NULL);
		if (*pagg_root == NULL){
		    IK_Syslog(LOG_ERR, "IK_RxODL(): Unable to create new "
			      "aggregate after processing STATS_REPORT");
		    *pagg_root = NULL;
		    i_returnCode = -1;
		    break;
		}

	    } else {
		f_listening = IK_FALSE;
	    }

	} else {

	    /* No, this wasn't a PONG or STATS_REPORT message, so
	       break out of the listening loop. */
	    f_listening = IK_FALSE;

	}

	/* Delete the temporary file. */
	if (remove(pc_ODLlabelFilename) == -1) {
	    IK_vSyslog(LOG_DEBUG, "IK_RxODL(): Error trying to delete the "
		       "temporary file %s", pc_ODLlabelFilename);
	    return(-1);
	}

    } while (f_listening == IK_TRUE);

    /*------------------------------------------------------------------------*/

#ifdef DEBUG

    {
	/* pc_receiveDebugFilename[] contains the name of the file to
	   use for archiving all incoming ODL messages. */
	char pc_receiveDebugFilename[MAXPATHLEN];

	/* pf_receiveDebugFile is a pointer to the FILE structure for
	   the receive archive file. */
	FILE *pf_receiveDebugFile;

	/* Create the path to the receive archive debug file. */
	(void) sprintf(pc_receiveDebugFilename, "%s%s.%d",
		       IK_SysConfig("GaeaTmp"), DEBUG_RX_ARCHIVE_PREFIX,
		       getpid());

	/* Open the receive archive file. */
	pf_receiveDebugFile= fopen(pc_receiveDebugFilename, "a");
	if (pf_receiveDebugFile == NULL) {
	    IK_vSyslog(LOG_ERR, "IK_RxODL(): Unable to open the debugger "
		       "temporary filename (%s) for the ODL label that was "
		       "received; errno %d (%s)", pc_receiveDebugFilename,
		       errno, strerror(errno));
	} else {

	    /* Save a copy of the message to look at later. */
	    (void) fprintf(pf_receiveDebugFile,
			   "\n\n****** The next message received was *****"
			   "\n\n");
	    WriteLabel(pf_receiveDebugFile, *pagg_root);
	    (void) fclose(pf_receiveDebugFile);

	}

    }

#endif /* DEBUG */

    /*------------------------------------------------------------------------*/

    /* Return the size of the message read from the socket, or the
       error code. */
    return(i_returnCode == 0 ? l_ODLfileSize : i_returnCode);

}

/*****************************************************************************/

/*
 * Name: IK_TxODL()
 *
 * Description: This function transmits an ODL tree to a DAAC. This
 * function is called when an ODL tree is ready to be transmitted to a
 * DAAC at the other end of the socket. First, the ODL tree is
 * converted to an ODL label by writing the tree to a file. Then the
 * size of the ODL tree is transmitted. Finally, the label file is
 * sent via the socket to the DAAC.
 *
 * Parameters:
 * int i_sockfd - Socket descriptor for the socket through which the
 * tree should be sent
 * AGGREGATE *pagg_root - Pointer to root AGGREGATE for ODL tree to be
 * transmitted
 *
 * Return Values:
 * Upon success, the size of the message sent. On failure, -1 and set
 * errno, IK_ImsErrno.
 *
 * Warnings:
 *
 * Global Variables Used:
 * errno
 * IK_ImsErrno
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *
 * The format of a message sent is : <msg length> <msgv>
 *
 * Revision History:
 *
 * 940407 PMR added call to IK_AddVersion
 *
 * Thursday 14 July 1994 (Eric Winter) - Finished overhauling function
 * to remove VMS support and extra debugging code. Also started using
 * WriteBufferToSocket() and WriteFileToSocket(), to make this code
 * _much_ more readable.
 *
 * */

int IK_TxODL(int i_sockfd, AGGREGATE *pagg_root)
{

  /* pc_ODLlabelFilename is the path to the temporary file that holds
     the ODL label file for the ODL tree to transmit to the socket. */
  char pc_ODLlabelFilename[MAXPATHLEN];

  /* pf_ODLlabelFile is the FILE pointer for the temporary file used
     to hold the ODL label. */
  FILE *pf_ODLlabelFile;

  /* stat_ODLfile holds the results of the stat() call for the ODL
     temporary file. The file size is extracted from this
     structure. */
  struct stat stat_ODLfile;

  /* l_ODLfileSize is the size of the ODL label file, which is the
     same as the size of the message to send. */
  long l_ODLfileSize;

  /* l_NBOODLfileSize is the ODL file size in Network Byte Order
     (NBO). */
  long l_NBOODLfileSize;

  /*-------------------------------------------------------------------------*/

  /* Validate all input. */
  assert(i_sockfd >= 0);
  assert(pagg_root != NULL);
  assert(*pagg_root != NULL);

  /* Verify that no outstanding system call errors have occurred. */
  if (errno != 0) {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): errno was set before entering but never "
	       "cleared, errno = %d (%s)", errno, strerror(errno));
    IK_ImsErrno = IK_EINVAL;
    return(-1);
  }

  /* At this point, we are assured that we have a nonnegative socket
     descriptor, and a non-NULL ODL tree pointer and root
     node. However, errors can still arise if the descriptor does not
     correspond to an active socket, or the ODL tree is corrupt. */

  /*-------------------------------------------------------------------------*/

  /* Add the MONITOR group (contains the outgoing timestamp) to the
     tree to be transmitted. The first argument (IK_TRUE) is the
     transmit flag. The third argument (size) is 0 since it is an
     obsolete argument. */
  if (IK_AddMonTS(IK_TRUE, pagg_root, 0) == -1) {
    IK_Syslog(LOG_ERR, "IK_TxODL(): The time stamps could not be added to "
	      "the outgoing tree");
    IK_ImsErrno = IK_EPROTO;
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Add CLIENT_VERSION and PROTOCOL_VERSION. */
  if (IK_AddVersion(*pagg_root) == -1) {
    IK_Syslog(LOG_ERR, "IK_TxODL(): The client and protocol version numbers "
	      "could not be added to the Tx tree");
    IK_ImsErrno = IK_EPROTO;
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Create a temporary filename for the file to hold the ODL label
     for the ODL tree to be transmitted. */
  (void) tmpnam(pc_ODLlabelFilename);
  if (pc_ODLlabelFilename[0] == '\0') {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): Unable to generate a unique temporary "
	       "filename; errno = %d (%s)", errno , strerror(errno));
    IK_ImsErrno = IK_ENOMEM;
    return(-1);
  }

  /* DANGER DANGER : In SunOS 4.1.1, errno is always set after this
     call, but tmpfile() is able to create the temporary file ????? */
  errno = 0;

  /* Open the temporary ODL label file for writing. */
  pf_ODLlabelFile = fopen(pc_ODLlabelFilename, "w");
  if (pf_ODLlabelFile == NULL) {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): Unable to open temporary file %s for ODL "
	       "label to be transmitted; errno = %d (%s)",
	       pc_ODLlabelFilename, errno, strerror(errno));
    return(-1);
  }

  /* Write the ODL tree to a the file as an ODL label. */
  WriteLabel(pf_ODLlabelFile, *pagg_root);

  /* Close the label file. */
  if (fclose(pf_ODLlabelFile) == EOF) {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): Error closing temporary file %s; "
	       "errno = %d (%s)", errno, strerror(errno), pc_ODLlabelFilename);
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Stat the file to get the size. */
  if (stat(pc_ODLlabelFilename, &stat_ODLfile) < 0) {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): Unable to stat the ODL label file "
	       "to get the size; errno = %d (%s)", errno, strerror(errno));
    return(-1);
  }

  /* Extract the file size from the file stats structure. */
  l_ODLfileSize = (long) stat_ODLfile.st_size;

  /* Convert the file size to network byte order (NBO). */
  l_NBOODLfileSize = htonl(l_ODLfileSize);

  /*-------------------------------------------------------------------------*/

  /* Transmit the size of the ODL tree to the destination DAAC via the
     specified socket. */
  if (WriteBufferToSocket(i_sockfd, &l_NBOODLfileSize,
			  sizeof(l_NBOODLfileSize)) == -1) {
    IK_Syslog(LOG_ERR, "IK_TxODL(): Error writing ODL tree size to socket");
    return(-1);
  }

  /* Transmit the contents of the file through the socket. */
  if (WriteFileToSocket(i_sockfd, pc_ODLlabelFilename, &l_ODLfileSize) == -1) {
    IK_vSyslog(LOG_ERR, "IK_TxODL(): Error writing ODL file %s to socket",
	       pf_ODLlabelFile);
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Delete the temporary file. */
  if (remove(pc_ODLlabelFilename) == -1) {
    IK_vSyslog(LOG_DEBUG, "IK_TxODL(): Error trying to delete the temporary "
	       "file %s", pc_ODLlabelFilename);
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

#ifdef DEBUG

  {

    /* pc_transmitDebugFilename[] contains the name of the file to use
       for archiving all outoing ODL messages. */
    char pc_transmitDebugFilename[MAXPATHLEN];

    /* pf_transmitDebugFile is a pointer to the FILE structure for the
       transmit archive file. */
    FILE *pf_transmitDebugFile;

    /* Create the path to the transmit archive debug file. */
    (void) sprintf(pc_transmitDebugFilename, "%s%s.%d",
		   IK_SysConfig("GaeaTmp"), DEBUG_TX_ARCHIVE_PREFIX, getpid());

    /* Open the transmit archive file. */
    pf_transmitDebugFile= fopen(pc_transmitDebugFilename, "a");
    if (pf_transmitDebugFile == NULL) {
      IK_vSyslog(LOG_ERR, "IK_TxODL(): Unable to open the debugger temporary "
		 "filename (%s) for the ODL label that was to be sent; "
		 "errno %d (%s)", pc_transmitDebugFilename, errno,
		 strerror(errno));
    } else {

      /* Save a copy of the message to look at later. */
      (void) fprintf(pf_transmitDebugFile,
		     "\n\n****** The next message sent was *****\n\n");
      WriteLabel(pf_transmitDebugFile, *pagg_root);
      (void) fclose(pf_transmitDebugFile);

    }

  }

#endif /* DEBUG */

  /*-------------------------------------------------------------------------*/

  /* Return the number of bytes written. */
  return(l_ODLfileSize);

}

/*****************************************************************************/

/*
 * Name: IK_setsockopt()
 *
 * Description: This function sets the socket options for the given
 * socket. The following options are activated: non-blocking I/O (if
 * select() is not available), socket keepalive messages, socket
 * lingering on close, and catching of alarm and broken pipe signals.
 *
 * Parameters:
 * int i_sockfd - Descriptor for socket for which options are to be set
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
 * Thursday 1 September (Eric Winter) - Overhauled.
 *
 * */

int IK_setsockopt(int i_sockfd)
{

  /* If this flag is non-zero, turn on non-blocking I/O for the
     socket. If zero, do not turn on non-blocking I/O. */
  int f_nonBlocking;

  /* This is the function execution status variable. */
  int i_returnCode = 0;

  /* If this flag is non-zero, turn on the "keep alive" option for the
     socket. */
  int f_keepAlive = 1;

  /* This structure is used to specify how long to continue to try to
     transmit unsent data on the socket after shutdown. */
  struct linger linger;

  /*-------------------------------------------------------------------------*/

  /* If the select() system call is available, do not use non-blocking
     I/O. If not available, use non-blocking I/O. */
#ifdef HAVE_SELECT
  f_nonBlocking = 0;
#else /* !HAVE_SELECT */
  f_nonBlocking = 1;
#endif

  /* Activate/deactivate non-blocking I/O, as needed. */
  i_returnCode = ioctl(i_sockfd, FIONBIO, &f_nonBlocking);
  if (i_returnCode < 0) {
    IK_Syslog(LOG_NOTICE, "IK_setsockopt(): unable to get a non-blocking "
	      "socket");
  } else {

    /* Set the keepalive option. This causes periodic transmissions to
       be made to the other end of the socket to ensure the connection
       stays open. If the other end does not respond, the connection
       is declared dead. */
    i_returnCode = setsockopt(i_sockfd, SOL_SOCKET, SO_KEEPALIVE,
			      &f_keepAlive, sizeof(f_keepAlive));
    if (i_returnCode < 0) {
      IK_vSyslog(LOG_ERR, "IK_setsockopt(): unable to set the keepalive "
		 "option for the new socket, errno = %d (%s)",
		 errno, strerror(errno));
    } else {

      /* Set the lingering options - turn on lingering for 10
         seconds. Note that in current implementations, the l_linger
         value is ignored. The effect is to try to send any unsent
         data when the socket is closed if l_linger is non-zero. */
      linger.l_onoff = 1;
      linger.l_linger = LINGER_TIME;

      /* Turn on socket lingering. */
      i_returnCode = setsockopt(i_sockfd, SOL_SOCKET, SO_LINGER,
				&linger, sizeof(linger));
      if (i_returnCode < 0) {
	IK_vSyslog(LOG_ERR, "IK_setsockopt(): unable to set the lingering "
		   "option for the new socket, errno = %d (%s)",
		   errno, strerror(errno));
      } else {

	/* Catch all SIGALRM signals (used for timeouts). */
	if (signal(SIGALRM, IK_AlarmHandler) == SIG_ERR) {
	  IK_vSyslog(LOG_ERR, "IK_setsockopt(): unable to install the signal "
		     "handler to catch the alarm signal, errno = %d (%s)",
		     errno, strerror(errno));
	  i_returnCode = -1;
	} else {

	  /* Catch all SIGPIPE signals (used for timeouts). */
	  if (signal(SIGPIPE, IK_PipeHandler) == SIG_ERR) {
	    IK_vSyslog(LOG_ERR, "IK_setsockopt(): unable to install the "
		       "signal handler to catch the SIGPIPE signal, "
		       "errno = %d (%s)", errno, strerror(errno));
	    i_returnCode = -1;
	  }
	}
      }
    }
  }

  /* Return the execution status. */
  return(i_returnCode);

}

/*****************************************************************************/

/*
 * Name: IK_Shutdown()
 *
 * Description: This function performs a shutdown of the connection
 * associated with the socket descriptor i_sockfd.
 *
 * Parameters:
 * int i_sockfd - Descriptor for the socket to be shut down
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
 *         A valid socket descriptor is passed in.
 *
 *     Postcondition 1 :
 *         Shut the connection down, telling the other end of the
 *         connection that we will no longer read anymore data (how = 0).
 *         RETURN 0 as the value of the function.
 *
 *     Precondition 2 :
 *         The socket was is no longer connected.
 *
 *     Postcondition 2 :
 *         SET errno to ENOTCONN, IK_ImsErrno to IK_ENOTCONN and LOG
 *         a "socket not connected" message to IK_Syslog. RETURN (-1)
 *         as the value of the function.
 *
 *     Precondition 3 :
 *         The socket descriptor is no longer valid.
 *
 *     Postcondition 3 :
 *         SET errno to ENOTSOCK, IK_ImsErrno to IK_ENOTSOCK, and LOG
 *         an "invalid socket descriptor" mesage via IK_Syslog. RETURN (-1)
 *         as the value of the function.
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Thursday 1 September 1994 (Eric Winter) - Overhauled.
 *
 * */

int IK_Shutdown(int i_sockfd)
{

  /* Disallow reading and writing on the socket. */
  if (shutdown(i_sockfd, SOCKET_SHUTDOWN_READ_WRITE) < 0) {
    IK_vSyslog(LOG_ERR, "IK_Shutdown(): error shutting down the socket, "
	       "errno = %d (%s)", errno, strerror(errno));
    errno = 0;
  }

  /* Close the socket. */
  (void) IK_Close(i_sockfd);

  /* Return normally. */
  return(0);

}

/*****************************************************************************/

/*
 * Name: IK_Close()
 *
 * Description: This function closes the specified socket.
 *
 * Parameters:
 * i_sockfd - Descriptor for socket to close
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
 * (1) - it is assumed that IK_Shutdown has already been called for this
 *     socket.
 *
 * Revision History:
 *
 * Thursday 1 September 1994 (Eric Winter) - Overhauled.
 *
 * */

int IK_Close(int i_sockfd)
{

  /* We don't care about an error closing a socket, we're done with it
     anyway. */
   (void) close(i_sockfd);

   /* Return normally. */
   return(0);

}

/*****************************************************************************/

/*
 * Name: IK_AlarmHandler()
 *
 * Description: This function is the error handler called when a
 * timeout occurs. This function handles the SIGALRM signal that is
 * generated on a socket timeout.
 *
 * Parameters:
 * int i_sigNum - Number of the signal which triggered the call
 *
 * Return Values:
 * None
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * errno
 * IK_ImsErrno
 * IK_Signal_Brk
 *
 * Pre- and Post-Conditions:
 *      Precondition 1:
 *              a socket has timed out.
 *
 *      Postcondition 1 :
 *              LOG an error to IK_Syslog and set SIGNAL_BREAK so that
 *              the parent can handle to signal
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Friday 2 September 1994 (Eric Winter) - Overhauled.
 *
 * */

void IK_AlarmHandler(int i_sigNum)
{

  /* Reset the signal handler for the SIGALRM signal. */
  (void) signal(SIGALRM, IK_AlarmHandler);

  /* If a SIGALRM lead to this call, set error variables to indicate
     that a socket timeout occurred. */
  if (i_sigNum == SIGALRM) {
    errno = ETIMEDOUT;
    IK_ImsErrno = IK_ETIMEDOUT;
    IK_Signal_Brk = IK_TRUE;
  }

}

/*****************************************************************************/

/*
 * Name: IK_PipeHandler()
 *
 * Description: This function is the error handler called when a
 * timeout occurs. This handles the SIGPIPE signal that is generated
 * on a socket by trying to write to a socket that has been closed on
 * the other end.
 *
 * Parameters:
 * int i_sigNum - Number for signal which caused this call
 *
 * Return Values:
 * None
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions:
 *      Precondition 1:
 *              a socket has been broken.
 *
 *      Postcondition 1 :
 *              set IK_Signal_Brk so that the parent can handle to signal
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Friday 2 September 1994 (Eric Winter) - Overhauled.
 *
 * */

void IK_PipeHandler(int i_sigNum)
{

  /* Reset the signal handler for the SIGPIPE signal. */
  (void) signal(SIGPIPE, IK_PipeHandler);

  /* If a SIGPIPE lead to this call, set error variables to indicate
     that a socket connection was broken. */
  if (i_sigNum == SIGPIPE) {
    errno = EHOSTUNREACH;
    IK_ImsErrno = IK_EHOSTUNREACH;
    IK_Signal_Brk = IK_TRUE;
  }

}

/*****************************************************************************/

/*
 * Name: IK_AddVersion()
 *
 * Description: This function adds a VERSION group to an outgoing ODL
 * message.
 *
 * Parameters:
 * AGGREGATE agg_root - pointer to outgoing message
 *
 * Return Values:
 * -1 if any errors occurred
 * 0 if no errors occurred
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 *   these proprocessor are used:
 *     REVNAME
 *          name of the build. (defined in Gaea_config.h)
 *     PROTOCOL_VERSION
 *          protocol number. (defined in IK_Ims.h)
 *
 * Pre- and Post-Conditions
 * None
 *
 * Notes:
 * None
 * 
 * Revision History:
 *
 * 940518 PMR  Added handling for IMS_STAFF parameter
 *
 * Thursday 14 July 1994 (Eric Winter) - Made this function static.
 *
 * Friday 2 September 1994 (Eric Winter) - Overhauled.
 * */

static int IK_AddVersion(AGGREGATE agg_root)
{

  /* agg_group refers to the first group in the ODL message. */
  AGGREGATE agg_firstGroup;

  /* agg_versionGroup points to the VERSION ODL group. */
  AGGREGATE agg_versionGroup;

  /* param_clientVersion is the CLIENT_VERSION parameter for the
     VERSION group. */
  PARAMETER param_clientVersion;

  /* value_clientVersion is the value of the CLIENT_VERSION
     parameter. */
  VALUE	value_clientVersion;

  /* valdat_clientVersion is the VALUE_DATA structure for the first
     value of the CLIENT_VERSION parameter. */
  VALUE_DATA valdat_clientVersion;

  /* param_protocolVersion is the PROTOCOL_VERSION parameter for the
     VERSION group. */
  PARAMETER param_protocolVersion;

  /* value_protocolVersion is the value of the PROTOCOL_VERSION
     parameter. */
  VALUE	value_protocolVersion;

  /* valdat_protocolVersion is the VALUE_DATA structure for the first
     value of the PROTOCOL_VERSION parameter. */
  VALUE_DATA valdat_protocolVersion;

  /* s_protocolVersion is the ASCII representation of the protocol
     version number. */
  char s_protocolVersion[MAX_PROTOCOL_VERSION_LENGTH];

  /* s_IMSstaffEnvVar is a pointer to the value of the IMS_STAFF
     environment variable. */
  char *s_IMSstaffEnvVar;

  /* param_IMSstaff is the IMS_STAFF parameter for the VERSION
     group. */
  PARAMETER param_IMSstaff;

  /* value_IMSstaff is the value of the IMS_STAFF parameter. */
  VALUE value_IMSstaff;

  /* valdat_IMSstaff is the VALUE_DATA structure for the first value
     of the IMS_STAFF parameter. */
  VALUE_DATA valdat_IMSstaff;

  /*-------------------------------------------------------------------------*/

  /* Validate input. */
  assert(agg_root != NULL);

  /*-------------------------------------------------------------------------*/

  /* Find the first group in the message. */
  agg_firstGroup = NextAggregate(agg_root);
  if (agg_firstGroup == NULL) {
    IK_Syslog(LOG_ERR, "IK_AddVersion(): couldn't find any groups in "
	      "message");
    return(-1);
  }

  /* Look for the VERSION group. If it does not exist, create it. */
  agg_versionGroup = FindGroup(agg_firstGroup, VERSION_GROUP_NAME);
  if (agg_versionGroup == NULL) {
    agg_versionGroup = NewAggregate(agg_firstGroup, KA_GROUP,
				    VERSION_GROUP_NAME, NULL);
    if (agg_versionGroup == NULL) {
      IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create VERSION group");
      return(-1);
    }
  }

  /*-------------------------------------------------------------------------*/

  /* Find the CLIENT_VERSION parameter. If it does not exist, create
     it. */
  param_clientVersion = FindParameter(agg_versionGroup,
				      CLIENT_VERSION_PARAM_NAME);
  if (param_clientVersion == NULL) {

    /* The parameter does not exist, so create it. */
    param_clientVersion = NewParameter(agg_versionGroup, KP_ATTRIBUTE,
				       CLIENT_VERSION_PARAM_NAME);
    if (param_clientVersion == NULL) {
      IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create CLIENT_VERSION "
		"parameter");
      return(-1);
    }

  } else {

    /* The parameter exists, so clean out the old value. Assume the
       parameter has only one value. */
    value_clientVersion = FirstValue(param_clientVersion);
    if (value_clientVersion != NULL) {
      (void) RemoveValue(value_clientVersion);
    }

  }

  /* Create an ODL value for the client version parameter, based on
     the revision name of this program. */
  valdat_clientVersion = ODLConvertString(REVNAME, strlen(REVNAME));
  value_clientVersion = NewValue(param_clientVersion, &valdat_clientVersion);
  if (value_clientVersion == NULL) {
    IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create CLIENT_VERSION value");
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Find the PROTOCOL_VERSION parameter. If it does not exist, create
     it. */
  param_protocolVersion = FindParameter(agg_versionGroup,
					PROTOCOL_VERSION_PARAM_NAME);
  if (param_protocolVersion == NULL) {

    /* The parameter does not exist, so create it. */
    param_protocolVersion = NewParameter(agg_versionGroup, KP_ATTRIBUTE,
					 PROTOCOL_VERSION_PARAM_NAME);
    if (param_protocolVersion == NULL) {
      IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create PROTOCOL_VERSION "
		"parameter");
      return(-1);
    }

  } else {

    /* The parameter exists, so remove its existing value. Note that
       only the first value is removed. */
    value_protocolVersion = FirstValue(param_protocolVersion);
    if (value_protocolVersion != NULL) {
      (void) RemoveValue(value_protocolVersion);
    }
  }

  /* Create an ODL value for the protocol version number. */
  (void) sprintf(s_protocolVersion, "%g", PROTOCOL_VERSION);
  valdat_protocolVersion = ODLConvertReal(s_protocolVersion,
					  strlen(s_protocolVersion));
  value_protocolVersion = NewValue(param_protocolVersion,
				   &valdat_protocolVersion);
  if (value_protocolVersion == NULL) {
    IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create PROTOCOL_VERSION value");
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* If the IMS_STAFF environment variable has been set, then look for
     the IMS_STAFF parameter. If it does not exist, create it, and
     give it the value of the environment variable. */
  s_IMSstaffEnvVar = getenv(IMS_STAFF_ENVVAR_NAME);
  if (s_IMSstaffEnvVar != NULL) {

    /* The environment variable has been set, so find the IMS staff
       parameter. If it does not exist, create it. */
    param_IMSstaff = FindParameter(agg_versionGroup, IMS_STAFF_PARAM_NAME);
    if (param_IMSstaff == NULL) {

      /* The parameter does not exist, so create it. */
      param_IMSstaff = NewParameter(agg_versionGroup, KP_ATTRIBUTE,
				    IMS_STAFF_PARAM_NAME);
      if (param_IMSstaff == NULL) {
	IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create IMS_STAFF "
		  "parameter");
	return(-1);
      }

    } else {

      /* The parameter exists, so remove the old value. Note that only
         the first (presumably only) value is removed. */
      value_IMSstaff = FirstValue(param_IMSstaff);
      if (value_IMSstaff != NULL) {
	RemoveValue(value_IMSstaff);
      }

    }

    /* Create an ODL value for the environment variable. */
    valdat_IMSstaff = ODLConvertString(s_IMSstaffEnvVar,
				       strlen(s_IMSstaffEnvVar));
    value_IMSstaff = NewValue(param_IMSstaff, &valdat_IMSstaff);
    if (value_IMSstaff == NULL) {
      IK_Syslog(LOG_ERR, "IK_AddVersion(): Can't create IMS_STAFF value");
      return(-1);
    }

  }

  /* Return normally. */
  return(0);

}

/*****************************************************************************/

/*
 * Name: WriteBufferToSocket()
 *
 * Description: This function writes to socket i_sockfd the contents
 * of the buffer pv_buf, of length sz_bufLen.
 *
 * Parameters:
 * int i_sockfd - Socket descriptor to write to
 * void *pv_buf - Pointer to beginning of buffer to write to the socket
 * size_t sz_bufLen - Length of buffer (same as number of bytes to transmit)
 *
 * Return Values:
 * int 0 - If all goes well (the entire buffer is written to the socket)
 * int -1 - If an error occurs
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * errno
 * IK_Signal_Brk
 * IK_ImsErrno
 *
 * Pre- and Post-Conditions:
 * None
 *
 * Notes: This function was written in order to simplify the calling
 * sequence in IK_TxODL(), and make the code easier to read and debug.
 *
 * Revision History:
 *
 * Wednesday 13 July 1994 (Eric Winter) - Started initial version.
 *
 * */

int WriteBufferToSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen)
{

  /* sz_totalBytesWritten is the cumulative number of bytes written to
     the socket. */
  size_t sz_totalBytesWritten;

  /* pv_curLoc points to the current location in the write
     buffer. This pointer contains the position from which the next
     packet of data to be written is read from. */
  void *pv_curLoc;

  /* u_numBytesLeft is the number of bytes remaining to be written to
     the socket. This value is also used as the number of bytes to
     write on each call to write(), i.e. each call to write tries to
     write all remaining bytes in the buffer. */
  unsigned u_numBytesLeft;

  /* i_numBytesWritten is the number of bytes written on a single call
     to write(). */
  int i_numBytesWritten;

  /* i_returnCode is the return value for this function (0 for
     success, -1 for failure. */
  int i_returnCode;

  /*-------------------------------------------------------------------------*/

  /* Validate all input. */

  /* Verify a valid socket descriptor was passed. */
  assert(i_sockfd >= 0);

  /* Verify that a valid buffer pointer was supplied. */
  assert(pv_buf != NULL);

  /* sz_bufLen must be a positive number. */
  assert(sz_bufLen > 0);

  /*-------------------------------------------------------------------------*/

  /* Initialize the return code to indicate success. */
  i_returnCode = 0;

  /*-------------------------------------------------------------------------*/

  /* Initialize the number of bytes written. */
  sz_totalBytesWritten = 0;

  /* Initialize the pointer to the current location within the
     buffer. Start at the beginning of the specified buffer. */
  pv_curLoc = pv_buf;

  /* Call write() on the socket repeatedly until the entire buffer
     contents are written, or an error occurs. */
  while (sz_totalBytesWritten < sz_bufLen) {

    /* Calculate the number of bytes left to write. */
    u_numBytesLeft = (unsigned) (sz_bufLen - sz_totalBytesWritten);

    /* Update the pointer into the transmission buffer. */
    pv_curLoc = (unsigned char *) pv_buf + sz_totalBytesWritten;

    /* Set the transmission timeout alarm. Ignore the amount of time
       remaining on any previous alarm. */
    (void) alarm(SOCKET_WRITE_ALARM_TIME);

    /* Try to write the remaining contents of the buffer. */
    i_numBytesWritten = write(i_sockfd, pv_curLoc, u_numBytesLeft);

    /* Clear the transmission timeout alarm, ignoring any time
       remaining on the alarm. */
    (void) alarm(0);

    /* If a signal was received during the transmission, process the
       signal. Otherwise, process the return value from write(). */
    if (IK_Signal_Brk) {

      /* A SIGALRM was generated by the transmission timeout alarm. */
      IK_vSyslog(LOG_ERR, "WriteBufferToSocket(): Call to write() "
		 "interrupted by SIGALRM; errno = %d (%s)",
		 errno, strerror(errno));

      /* Reset the signal flag. */
      IK_Signal_Brk = IK_FALSE;

      /* Set the return value to indicate an error. */
      i_returnCode = -1;

    } else if (errno != 0) {

      /* An error occurred during the write() call. If the error was
	 due to a temporary inability to write the data, then try
	 again. Otherwise, process the error code. IMPLEMENTATION
	 NOTE: This code was written with an if() for EWOULDBLOCK and
	 EAGAIN since these symbols are often defined to have the same
	 value, e.g. as in IRIX 5. This causes a "duplicate definition
	 in case statement" error with the IRIX 5 compiler. */
      if (errno == EWOULDBLOCK || errno == EAGAIN) {
	/* No problem (yet). */
      } else {
	switch (errno) {
	case EINTR:
	  IK_vSyslog(LOG_ERR, "WriteBufferToSocket(): Call to write() "
		     "interrupted by SIGINT; errno = %d (%s)",
		     errno, strerror(errno));
	  i_returnCode = -1;
	  break;
	case EHOSTUNREACH:
	  IK_vSyslog(LOG_ERR, "WriteBufferToSocket(): Connection has been "
		     "broken, errno = %d (%s)", errno, strerror(errno));
	  IK_ImsErrno = IK_EHOSTUNREACH;
	  i_returnCode = -1;
	  break;
	default:
	  IK_vSyslog(LOG_ERR, "WriteBufferToSocket(): Unknown error writing "
		     "to socket; errno = %d (%s)", errno, strerror(errno));
	  IK_ImsErrno = IK_EPROTO;
	  i_returnCode = -1;
	}
      }

    }

    /* If an error has occurred on this pass through the loop, break
       out of the loop. */
    if (i_returnCode != 0) {
      break;
    }

    /* Note that at this point, we are guaranteed to have dealt with
       any possible error from the write() call. Therefore, we can be
       sure that the value of i_numBytesWritten is
       nonnegative. However, just in case, trap for this "impossible"
       condition. */
    assert(i_numBytesWritten >= 0);

    /* Update the total of bytes written. */
    sz_totalBytesWritten += i_numBytesWritten;

  }

  /* At this point, the entire contents of the buffer have been
     written to the socket, or an error has occurred. */

  /*-------------------------------------------------------------------------*/

  /* Return the execution status. */
  return(i_returnCode);

}

/*****************************************************************************/

/*
 * Name: WriteFileToSocket()
 *
 * Description: This function writes the contents of the file
 * specified by the pc_filename argument to the socket specified by
 * the i_sockfd argument.
 *
 * Parameters:
 * int i_sockfd - Descriptor for socket to write file to
 * const char *pc_filename - String containing complete path to file
 * to be written to the socket
 * long *pl_fileSize - On successful return, contains the size of the file
 * written to the socket; on failure, left unchanged
 *
 * Return Values:
 * int 0 - If all goes well (the file contents are written to the socket);
 * set *pl_fileSize to the size of the file written to the socket
 * int -1 - If an error occurs; leave *pl_fileSize unchanged
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
 * Wednesday 13 July 1994 (Eric Winter) - Started initial version.
 *
 * */

static int WriteFileToSocket(int i_sockfd, const char *pc_filename,
 long *pl_fileSize)
{

  /* i_returnCode is the return code for this function. */
  int i_returnCode;

  /* ss_fileStats holds the data returned by the stat() system call
     for the target file. The file size is extracted from this
     structure and used as the number of bytes to write to the
     socket. */
  struct stat ss_fileStats;

  /* l_fileSize is the size of the file in bytes. This is the number
     of bytes that will be transmitted over the network. */
  long l_fileSize;

  /* pf is a pointer to the FILE structure for the file to be written
     to the socket. */
  FILE *pf;

  /* l_totalBytesWritten is the total number of bytes from the file
     that have been written to the socket. */
  long l_totalBytesWritten;

  /* l_bytesRemainingInFile is the number of bytes left in the file
     that still must be read and transmitted. */
  long l_bytesRemainingInFile;

  /* sz_bytesInBuffer is the number of bytes of data currently in the
     transmission buffer. All of these bytes must be written to the
     socket. */
  int sz_bytesInBuffer;

  /* This buffer contains the current data to be written to the
     socket. This buffer is filled repeatedly and written to the
     socket until the entire file has been written to the socket. */
  static char pc_transmitBuf[IK_NETBLK_SZ];

  /* sz_bytesToRead is the number of bytes to read from the file on the
     current reading pass. */
  int sz_bytesToRead;

  /*-------------------------------------------------------------------------*/

  /* Validate all inputs. */

  /* Verify that a valid socket descriptor has been supplied. */
  assert(i_sockfd > 0);

  /* Verify that a non-NULL filename has been supplied. */
  assert(pc_filename != NULL);

  /* Verify that a valid pointer for the file size has been
     supplied. */
  assert(pl_fileSize != NULL);

  /*-------------------------------------------------------------------------*/

  /* Initialize the execution status to 0 (success). */
  i_returnCode = 0;

  /*-------------------------------------------------------------------------*/

  /* Stat the file to get the size. */
  if (stat(pc_filename, &ss_fileStats) == -1) {
    IK_vSyslog(LOG_ERR, "WriteFileToSocket(): Error in stat() for file %s; ",
	       "errno = %d (%s)", pc_filename, errno, strerror(errno));
    return(-1);
  }

  /* Get the file size. */
  l_fileSize = (long) ss_fileStats.st_size;

  /*-------------------------------------------------------------------------*/

  /* Open the file for reading. */
  pf = fopen(pc_filename, "r");
  if (pf == NULL) {
    IK_vSyslog(LOG_ERR, "WriteFileToSocket(): Can't open file %s for reading; "
	       "errno = %d (%s)", pc_filename, errno, strerror(errno));
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Read in chunks of the file to fill the transmission buffer, then
     write the contents of the transmission buffer to the
     socket. Repeat this action until the entire contents of the file
     have been read into the buffer and written to the socket. */

  /* Clear the count of total bytes written so far. */
  l_totalBytesWritten = 0;

  /* Start chunking, keep getting chunks until there are no more. */
  while (l_totalBytesWritten < l_fileSize) {

    /* Calculate the number of bytes remaining in the file to be read
       and transmitted. */
    l_bytesRemainingInFile = l_fileSize - l_totalBytesWritten;

    /* Calculate the number of bytes to read from the file. In
       general, read until the buffer is full, or until there is no
       more data to read. */
    if (l_bytesRemainingInFile > sizeof(pc_transmitBuf)) {
      sz_bytesToRead = sizeof(pc_transmitBuf);
    } else {
      sz_bytesToRead = (size_t) l_bytesRemainingInFile;
    }

    /* Read the next buffer full of data from the file. */
    sz_bytesInBuffer = fread(pc_transmitBuf, sizeof(*pc_transmitBuf),
			     sz_bytesToRead, pf);
    if (sz_bytesInBuffer != sz_bytesToRead) {
      IK_vSyslog(LOG_ERR, "WriteFileToSocket(): Incomplete read of data from "
		 "file %s; errno = %d (%s)",
		 pc_filename, errno, strerror(errno));
      i_returnCode = -1;
      break;
    }

    /* Write the buffer to the socket. */
    if (WriteBufferToSocket(i_sockfd, pc_transmitBuf, sz_bytesInBuffer)
	== -1) {
      IK_vSyslog(LOG_ERR, "WriteFileToSocket(): Error writing buffer to "
		 "socket; errno = %d (%s)", errno, strerror(errno));
      i_returnCode = -1;
      break;
    }

    /* Increment the number of bytes written to the socket so far. */
    l_totalBytesWritten += (long) sz_bytesInBuffer;

  }

  /* If an error occurred during the read/write loop, abort this
     function (but close the file first). */
  if (i_returnCode != 0) {
    (void) fclose(pf);
    return(i_returnCode);
  }

  /* At this point we are guaranteed that the entire contents of the
     file have been written to the socket. */

  /*-------------------------------------------------------------------------*/

  /* Close the file. */
  if (fclose(pf) == EOF) {
    IK_vSyslog(LOG_ERR, "WriteFileToSocket(): Error closing file %s; "
	       "errno = %d (%s)", pc_filename, errno, strerror(errno));
    i_returnCode = -1;
    return(i_returnCode);
  }

  /*-------------------------------------------------------------------------*/

  /* Copy the file size to *pl_fileSize. */
  *pl_fileSize = l_fileSize;

  /* Return the execution status. */
  return(i_returnCode);

}

/*****************************************************************************/

/*
 * Name: ReadBufferFromSocket()
 *
 * Description: This function reads the specified number of bytes from
 * the specified socket, and places them into the specified buffer. If
 * all of the desired bytes are not read, an error is returned.
 *
 * Parameters:
 * int i_sockfd - Socket descriptor to read from
 * void *pv_buf - Pointer to beginning of buffer to fill with data read from
 * the socket
 * size_t sz_bufLen - Length of buffer (same as number of bytes to receive)
 *
 * Return Values:
 * int 0 - If all goes well (the desired number of bytes are read)
 * int -1 - If an error occurs
 *
 * Warnings:
 * None
 *
 * Global Variables Used:
 * errno
 * IK_ImsErrno
 * IK_Signal_Brk
 *
 * Pre- and Post-Conditions:
 * None
 *
 * Notes:
 * None
 *
 * Revision History:
 *
 * Friday 15 July 1994 (Eric Winter) - Started initial version.
 *
 * */

int ReadBufferFromSocket(int i_sockfd, void *pv_buf, size_t sz_bufLen)
{

  /* sz_totalBytesRead is the total number of bytes read from the
     socket. */
  size_t sz_totalBytesRead;

  /* pv_curLoc points to the current location in the read buffer. This
     pointer contains the position to which the next packet of data to
     be read from the socket is written to. */
  void *pv_curLoc;

  /* u_numBytesLeft is the number of bytes remaining to be read from
     the socket. This value is also used as the number of bytes to
     read on each call to read(), i.e. each call to read() tries to
     read all remaining bytes into the buffer. */
  unsigned u_numBytesLeft;

  /* i_numBytesRead is the number of bytes read on a single call to
     read(). */
  int i_numBytesRead;

  /* i_readsWithNoData is the number of calls to read() which have
     been executed and completed successfully, but read no data. When
     this number gets too big, the connection is assumed to be
     broken. */
  int i_readsWithNoData;

  /* i_returnCode is the return value for this function (0 for
     success, -1 for failure. */
  int i_returnCode;

  /*-------------------------------------------------------------------------*/

  /* Validate all input. */

  /* Verify a valid socket descriptor was passed. */
  assert(i_sockfd >= 0);

  /* Verify that a valid buffer pointer was supplied. */
  assert(pv_buf != NULL);

  /* sz_bufLen must be a positive number. */
  assert(sz_bufLen > 0);

  /*-------------------------------------------------------------------------*/

  /* Initialize the return code to indicate success. */
  i_returnCode = 0;

  /*-------------------------------------------------------------------------*/

  /* Initialize the number of bytes read. */
  sz_totalBytesRead = 0;

  /* Initialize the pointer to the current location within the
     buffer. Start at the beginning of the specified buffer. */
  pv_curLoc = pv_buf;

  /* Initialize the number of read attempts which read no data. */
  i_readsWithNoData = 0;

  /* Call read() on the socket repeatedly until the entire buffer
     contents are read, or an error occurs. */
  while (sz_totalBytesRead < sz_bufLen) {

    /* Calculate the number of bytes left to read. */
    u_numBytesLeft = (unsigned) (sz_bufLen - sz_totalBytesRead);

    /* Update the pointer into the receive buffer. */
    pv_curLoc = (unsigned char *) pv_buf + sz_totalBytesRead;

    /* Set the read() timeout alarm. Ignore the amount of time
       remaining on any previous alarm. */
    (void) alarm(SOCKET_READ_ALARM_TIME);

    /* Try to read the remaining contents of the buffer. */
    i_numBytesRead = read(i_sockfd, pv_curLoc, u_numBytesLeft);

    /* Clear the read() timeout alarm, ignoring any time remaining on
       the alarm. */
    (void) alarm(0);

    /* If a signal was received during the read, process the
       signal. Otherwise, process the return value from read(). */
    if (IK_Signal_Brk) {

      /* A SIGALRM was generated by the read timeout alarm. */
      IK_vSyslog(LOG_ERR, "ReadBufferFromSocket(): Call to read() "
		 "interrupted by SIGALRM; errno = %d (%s)",
		 errno, strerror(errno));

      /* Reset the signal flag. */
      IK_Signal_Brk = IK_FALSE;

      /* Set the return value to indicate an error. */
      i_returnCode = -1;

    } else if (errno != 0) {

      /* An error occurred during the read() call. If the error was
         due to a temporary inability to read the data, then try
         again. Otherwise, process the error code. IMPLEMENTATION
         NOTE: This code was written with an if() for EWOULDBLOCK and
         EAGAIN since these symbols are often defined to have the same
         value, e.g. as in IRIX 5. This causes a "duplicate definition
         in case statement" error with the IRIX 5 compiler. */
      if (errno == EWOULDBLOCK || errno == EAGAIN) {
	/* No problem (yet). */
      } else {
	switch (errno) {
	case EINTR:
	  IK_vSyslog(LOG_ERR, "ReadBufferFromSocket(): Call to read() "
		     "interrupted by SIGINT; errno = %d (%s)",
		     errno, strerror(errno));
	  i_returnCode = -1;
	  break;
	case EHOSTUNREACH:
	  IK_vSyslog(LOG_ERR, "ReadBufferFromSocket(): Connection has been "
		     "broken, errno = %d (%s)", errno, strerror(errno));
	  IK_ImsErrno = IK_EHOSTUNREACH;
	  i_returnCode = -1;
	  break;
	default:
	  IK_vSyslog(LOG_ERR, "ReadBufferFromSocket(): Unknown error reading "
		     "from socket; errno = %d (%s)", errno, strerror(errno));
	  IK_ImsErrno = IK_EPROTO;
	  i_returnCode = -1;
	}
      }

    }

    /* Check that data was actually read from the socket. If not,
       increment the count of unsuccessful read() calls. If the read()
       count threshhold is exceeded, assume the connection is
       broken. The current value of IK_MAX_EWOULDBLOCK is 1000. */
    if (i_returnCode == 0 && i_numBytesRead == 0) {

      /* Increment the number of read attempts which read no data. */
      i_readsWithNoData++;

      /* If the read count threshhold is exceeded, declare the
         connection dead and break out of the loop. */
      if (i_readsWithNoData > IK_MAX_EWOULDBLOCK) {
	IK_vSyslog(LOG_ERR, "ReadBufferFromSocket(): Declaring connection "
		   "dead. Tried to read %d successive times with no success",
		   IK_MAX_EWOULDBLOCK);
	errno = EHOSTUNREACH;
	IK_ImsErrno = IK_EHOSTUNREACH;
	i_returnCode = -1;
      }

    }

    /* If an error has occurred on this pass through the loop, break
       out of the loop. */
    if (i_returnCode != 0) {
      break;
    }

    /* Note that at this point, we are guaranteed to have dealt with
       any possible error from the read() call. Therefore, we can be
       sure that the value of i_numBytesRead is nonnegative. However,
       just in case, trap for this "impossible" condition. */
    assert(i_numBytesRead >= 0);

    /* Update the total of bytes read. */
    sz_totalBytesRead += i_numBytesRead;

  }

  /* At this point, the entire contents of the buffer have been
     read from the socket, or an error has occurred. */

  /*-------------------------------------------------------------------------*/

  /* Return the execution status. */
  return(i_returnCode);

}

/*****************************************************************************/

/*
 * Name: ReadFileFromSocket()
 *
 * Description: This function reads from a socket until the specified
 * number of bytes have been read, and writes all bytes to the
 * specified file.
 *
 * Parameters:
 * int i_sockfd - Descriptor for the socket to read from
 * const char *pc_filename - Complete path to file to store socket data into
 * long *pl_fileSize - Pointer to size of file to read from socket
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
 * Friday 15 July 1994 (Eric Winter) - Started initial version.
 *
 * */

static int ReadFileFromSocket(int i_sockfd, const char *pc_filename,
 long *pl_fileSize)
{

  /* i_returnCode is the return code for this function. */
  int i_returnCode;

  /* pf is a pointer to the FILE structure for the file to be written
     to the socket. */
  FILE *pf;

  /* l_fileSize is a local copy of the number of bytes to read from
     the socket for the specified file. */
  long l_fileSize;

  /* l_totalBytesRead is the total number of bytes from the socket
     that have been written to the file. At the end of the function,
     if all has gone well, this value should be the same as
     l_fileSize. */
  long l_totalBytesRead;

  /* l_bytesRemainingInSocket is the number of bytes left in the
     socket that still must be written to the file. */
  long l_bytesRemainingInSocket;

  /* sz_bytesInBuffer is the number of bytes of data currently in the
     receive buffer. All of these bytes must be written to the
     file. */
  int sz_bytesInBuffer;

  /* This buffer contains the current data to be written to the
     file. This buffer is filled repeatedly from the socket and
     written to the file until the entire file has been received and
     written to disk. */
  static char pc_receiveBuf[IK_NETBLK_SZ];

  /* sz_bytesToRead is the number of bytes to read from the socket on
     the current reading pass. */
  size_t sz_bytesToRead;

  /*-------------------------------------------------------------------------*/

  /* Validate all inputs. */

  /* Verify that a valid socket descriptor has been supplied. */
  assert(i_sockfd > 0);

  /* Verify that a non-NULL filename has been supplied. */
  assert(pc_filename != NULL);

  /* Verify that a valid pointer for the file size has been
     supplied. */
  assert(pl_fileSize != NULL);

  /*-------------------------------------------------------------------------*/

  /* Initialize the execution status to 0 (success). */
  i_returnCode = 0;

  /*-------------------------------------------------------------------------*/

  /* Open the file for writing. */
  pf = fopen(pc_filename, "w");
  if (pf == NULL) {
    IK_vSyslog(LOG_ERR, "ReadFileFromSocket(): Can't open file %s for "
	       "writing; errno = %d (%s)",
	       pc_filename, errno, strerror(errno));
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Read in chunks of the file from the socket to fill the receive
     buffer, then write the contents of the receive buffer to the
     file. Repeat this action until the entire contents of the file
     have been read into the buffer and written to the file. */

  /* Copy the file size to a local variable. */
  l_fileSize = *pl_fileSize;

  /* Clear the count of total bytes read so far. */
  l_totalBytesRead = 0;

  /* Read from the socket until all bytes are read, or until an error
     occurs. */
  while (l_totalBytesRead < l_fileSize) {

    /* Calculate the number of bytes remaining in the socket to be
       read and written to disk. */
    l_bytesRemainingInSocket = l_fileSize - l_totalBytesRead;

    /* Calculate the number of bytes to read from the socket on this
       pass. In general, read until the buffer is full, or until there
       is no more data to read. */
    if (l_bytesRemainingInSocket > (long) sizeof(pc_receiveBuf)) {
      sz_bytesToRead = sizeof(pc_receiveBuf);
    } else {
      sz_bytesToRead = (size_t) l_bytesRemainingInSocket;
    }

    /* Read the next buffer full of data from the socket. */
    if (ReadBufferFromSocket(i_sockfd, pc_receiveBuf, sz_bytesToRead) == -1) {
      IK_Syslog(LOG_ERR, "ReadFileFromSocket(): Error reading buffer from "
		"socket");
      i_returnCode = -1;
      break;
    }

    /* Set the number of bytes in the buffer to the number of bytes
       read. */
    sz_bytesInBuffer = sz_bytesToRead;

    /* Write the buffer to the file. */
    if (fwrite(pc_receiveBuf, sizeof(*pc_receiveBuf), sz_bytesInBuffer, pf)
	!= sz_bytesInBuffer) {
      IK_vSyslog(LOG_ERR, "ReadFileFromSocket(): Error writing buffer to "
		 "file; errno = %d (%s)", errno, strerror(errno));
      i_returnCode = -1;
      break;
    }

    /* Increment the number of bytes read from the socket so far. */
    l_totalBytesRead += (long) sz_bytesInBuffer;

  }

  /* If an error occurred during the read/write loop, abort this
     function (but close the file first). */
  if (i_returnCode != 0) {
    (void) fclose(pf);
    return(i_returnCode);
  }

  /* At this point we are guaranteed that the entire contents of the
     file have been read from the socket and written to disk. */

  /*-------------------------------------------------------------------------*/

  /* Close the file. */
  if (fclose(pf) == EOF) {
    IK_vSyslog(LOG_ERR, "ReadFileFromSocket(): Error closing file %s; "
	       "errno = %d (%s)", pc_filename, errno, strerror(errno));
    return(-1);
  }

  /*-------------------------------------------------------------------------*/

  /* Return the execution status. */
  return(i_returnCode);

}

/*****************************************************************************/

/*
 * Name: IK_IsAStatsReportMessage()
 *
 * Description: This function determines if the ODL tree passed as its
 * argument is a valid stats report message.
 *
 * Parameters:
 * AGGREGATE agg - ODL aggregate for tree to examine
 *
 * Return Values:
 * IK_TRUE - If the tree is a valid stats report message tree
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
 * Monday 23 October 1995 (Eric Winter) - Started initial version.
 *
 * */

static IK_BOOLEAN
IK_IsAStatsReportMessage(AGGREGATE agg)
{

    /* Validate input. */
    assert(agg != NULL);

    /*------------------------------------------------------------------------*/

    /* Look for the STATS_REPORT group within the tree. If found, this
       should be a STATS_REPORT message tree. */
    if (FindGroup(agg, STATS_REPORT_GROUP_NAME) != NULL) {
	return(IK_TRUE);
    } else {
	return(IK_FALSE);
    }

}

/*****************************************************************************/

/*
 * Name: IK_IngestStatsReport()
 *
 * Description: This function ingests a statistics report ODL
 * tree. This means that the contents of the report string are
 * appended to the statistics report log.
 *
 * Arguments:
 * AGGREGATE agg_root - Pointer to stats report tree to process
 *
 * Return Values:
 * int 0 - If everything goes well
 * int -1 - If an error occurs
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions:
 *
 * Revision History:
 *
 * 20 April 1994 (Eric Winter) - Added code to use a macro to specify
 * the statistics log filename, and an environment variable to specify
 * the directory it goes in.
 *
 * Monday 23 October 1995 (Eric Winter) - Moved function here from
 * IK_Nmfn.c, where it never should have been in the first place.
 * */

static int
IK_IngestStatsReport(AGGREGATE agg_root)
{

    /* agg_reportGrp is the ODL aggregate for the incoming statistics
       report. */
    AGGREGATE agg_reportGrp = NULL;

    /* param_reportLen and val_reportLen are used to hold the report
       length parameter and value, respectively. */
    PARAMETER param_reportLen = NULL;
    VALUE val_reportLen = NULL;

    /* i_reportLen is the length in characters of the statistics
       report string, as stored in the report length parameter. */
    int i_reportLen = 0;

    /* param_reportString and val_reportString are used to hold the
       report string parameter and value, respectively. */
    PARAMETER param_reportString = NULL;
    VALUE val_reportString = NULL;

    /* pc_reportString is the actual statistics report string. */
    char *pc_reportString = NULL;

    /* pFILE_reportLog points to the FILE for the statistics log
       file. */
    FILE *pFILE_reportLog = NULL;

    /* pc_statsLogPath points to the path (directory and filename) for
       the statistics log. */
    char *pc_statsLogPath = NULL;

    /*------------------------------------------------------------------------*/

    /* Validate input. */
    assert(agg_root != NULL);

    /*------------------------------------------------------------------------*/

    /* Get the ODL aggregate for the statistics report. */
    agg_reportGrp = FindGroup(agg_root, STATS_REPORT_GROUP_NAME);
    if (agg_reportGrp == NULL) {
	return(-1);
    }

    /*------------------------------------------------------------------------*/

    /* Get the ODL parameter for the statistics report length. */
    param_reportLen = FindParameter(agg_reportGrp,
				    STATS_REPORT_LENGTH_PARAM_NAME);
    if (param_reportLen == NULL) {
	return(-1);
    }

    /* Get the ODL value for the statistics report length. */
    val_reportLen = FirstValue(param_reportLen);
    if (val_reportLen == NULL) {
	return(-1);
    }

    /* Retrieve the length of the statistics report string. */
    i_reportLen = val_reportLen->item.value.integer.number;

    /*------------------------------------------------------------------------*/

    /* Get the ODL parameter for the statistics report string. */
    param_reportString = FindParameter(agg_reportGrp,
				       STATS_REPORT_STRING_PARAM_NAME);
    if (param_reportString == NULL) {
	return(-1);
    }

    /* Get the ODL value for the statistics report string. */
    val_reportString = FirstValue(param_reportString);
    if (val_reportString == NULL) {
	return(-1);
    }

    /* Retrieve the statistics report string. */
    pc_reportString = val_reportString->item.value.string;
    if (pc_reportString == NULL) {
	return(-1);
    }

    /*------------------------------------------------------------------------*/

    /* Determine the directory and file in which to place the
       statistics log file. The environment variable specified by the
       macro IK_STATS_LOG_ENV_VAR can be set to specify the directory
       and file in which to put the statistics log on a DAAC-by-DAAC
       basis. If this environment variable is not set, the default
       directory and file, specified by the macro
       IK_DEFAULT_STATS_LOG, is used instead. */
    pc_statsLogPath = getenv(IK_STATS_LOG_ENV_VAR);
    if (pc_statsLogPath == NULL) {

	/* The statistics log environment variable was not set, so use
	   the default value. */
	pc_statsLogPath = IK_DEFAULT_STATS_LOG;

    }

    /*------------------------------------------------------------------------*/

    /* Open the statistics report file for appending. N.B. This code
       must be modified later to account for concurrent server
       processes attempting to update the statistics log file
       simultaneously! */
    pFILE_reportLog = fopen(pc_statsLogPath, "a");
    if (pFILE_reportLog == NULL) {

	/* Couldn't open the report file, so return an error code. */
	return(-1);

    }

    /* Append the report string to the log file. */
    if (fprintf(pFILE_reportLog, "%s\n", pc_reportString)
	< (strlen(pc_reportString) + 1)) {

	/* There was an error during printing to the log file. */
	return(-1);

    }

    /* Close the statistics report file. */
    if (fclose(pFILE_reportLog) == EOF) {

	/* There was an error while closing the statistics report
           file. */
	return(-1);

    }

    /*------------------------------------------------------------------------*/

    /* All is well, so return no error code. */
    return(0);

}
