/*
 * @(#)socketXport.c	2.4 96/01/29 11:53:59
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <syslog.h>
#include <errno.h>
#include "odl.h"
#include "socketXport.h"
#include "inet.h"
#include "memUtils.h"

static char sccsid_socketxport_c[] =
	"@(#)socketXport.c	2.4 96/01/29 11:53:59";

/** GLOBAL variables within file */

int  GLOBAL_sockfd;
int  GLOBAL_portid = -1;
char *GLOBAL_CP_inet;

/* end GLOBAL....................*/

#define RECONNECT_INTERVAL	10	/* seconds */
#define RECONNECT_RETRIES	10
#define SLEEPTIME		300	/* seconds */

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

/*
 * NAME:
 * int InitClientInternal
 *
 * DESCRIPTION:
 * a private routine that performs tcp/ip initialization
 * for a client
 *
 * NOTES:
 *
 *
 */

int InitClientInternal(int portid, char *inetPtr)
{
    struct sockaddr_in serv_addr;
    int retval;

    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family      = AF_INET;
    /*  serv_addr.sin_addr.s_addr = inet_addr(SERV_HOST_ADDR); */
    serv_addr.sin_addr.s_addr = inet_addr(inetPtr);
    serv_addr.sin_port        = htons(portid);

    if ((GLOBAL_sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	return(-1);

    retval = connect(GLOBAL_sockfd, (struct sockaddr *) &serv_addr, 
	sizeof(serv_addr));
    if (retval < 0)
	return(-1);

    return(0);
}

/*
 * NAME:
 * InitClient
 *
 * DESCRIPTION:
 * a public routine that performs client initialization for
 * tcp/ip
 *
 * InitClient sets the following global variables:
 *     GLOBAL_portid		Port number to use for connection
 *     GLOBAL_CP_inet		CP's IP address as seen by application
 *
 * If the initial connection attempt to the CP fails, the routine will re-
 * attempt to establish the connection forever.
 */

int InitClient(int argc, char **argv)
{
    void attempt_connection_forever();
    int i, j, retval;
    int retryNo;
    int connected = FALSE;
    int parse_error = FALSE;

    GLOBAL_portid = -1;
    GLOBAL_CP_inet = (char *) NULL;

    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i], "-asfp") == 0) {

	    /* Check that another argument exists on the command line */
	    if ((i + 1) == argc) {
		printfLLog(LOG_ERR,
		    "Missing argument to -asfp flag\n");
		parse_error = TRUE;
		break;
	    }

	    /* Check that the next argument isn't another flag */
	    if (*argv[++i] == '-') {
		printfLLog(LOG_ERR,
		    "Argument to -asfp flag cannot start with a \"-\"\n");
		parse_error = TRUE;
		break;
	    }

	    /*
	    GLOBAL_portid = atoi(argv[i]);
	    */

	    if ((j = sscanf(argv[i], "%d", &GLOBAL_portid)) != 1) {
		printfLLog(LOG_ERR,
		    "Invalid port ID %s:  port ID must be an integer\n",
		    argv[i]);
		parse_error = TRUE;
		break;
	    }

	    if (GLOBAL_portid < 1024) {
		printfLLog(LOG_ERR,
		    "Invalid port ID %d:  port ID must be an integer < 1024\n",
		    GLOBAL_portid);
		parse_error = TRUE;
		break;
	    }

	} else if (strcmp(argv[i], "-asf_CP_inet") == 0) {

	    /* Check that another argument exists on the command line */
	    if ((i + 1) == argc) {
		printfLLog(LOG_ERR,
		    "Missing argument to -asf_CP_inet flag\n");
		parse_error = TRUE;
		break;
	    }

	    /* Check that the next argument isn't another flag */
	    if (*argv[++i] == '-') {
		printfLLog(LOG_ERR,
		    "Argument to -asf_CP_inet flag cannot start \
with a \"-\"\n");
		parse_error = TRUE;
		break;
	    }

	    GLOBAL_CP_inet = doMalloc(strlen(argv[i]) + 1);
	    strcpy(GLOBAL_CP_inet, argv[i]);
	}
    }

    if (parse_error == TRUE) {
	return -1;
    }

    if (GLOBAL_portid == -1) {
	printfLLog(LOG_ERR, "Port ID must be specified with -asfp flag\n");
	return -1;
    }

    if (GLOBAL_CP_inet == (char *) NULL) {
	printfLLog(LOG_ERR, "CP IP address must be specified with \
-asf_CP_inet flag\n");
	return -1;
    }

    if ((InitClientInternal(GLOBAL_portid, GLOBAL_CP_inet)) < 0) {
	attempt_connection_forever();
    }
    printfLLog(LOG_INFO, "Client CONNECTED to CP\n");
    return 0;
}


/*
 * NAME:
 * ReadMsgFromServer
 *
 * DESCRIPTION:
 * the routine used by the client to read a message from 
 * the CP
 *
 *
 * If the application detects that the connection to the CP has been lost,
 * this routine uses the same procedure as the InitClient() routine to
 * attempt to re-establish the connection.  The routine will attempt
 * to re-establish the connection every RECONNECT_INTERVAL seconds,
 * for a maximum of RECONNECT_RETRIES tries.  After reaching that number,
 * the routine prints an appropriate message, goes to sleep for SLEEPTIME
 * seconds, and then starts the whole process over again in an infinite
 * loop.
 *
 * NOTES:
 *
 *
 */

ODL ReadMsgFromServer(int *errStatusPtr)
{
    void attempt_connection_forever();

    char *msgPtr;
    ODL msgODL;
    int len;
    int retryNo;

    /*
    LABEL:
    */
    msgPtr = readMsg(GLOBAL_sockfd, &len);
    if (msgPtr == NULL) {
	if ((InitClientInternal(GLOBAL_portid, GLOBAL_CP_inet)) < 0) {
	    printfLLog(LOG_ERR,
		"Lost connection to CP -- attempting to reconnect\n");
	    attempt_connection_forever();
	}
	printfLLog(LOG_INFO, "Client RE-CONNECTED to CP\n");
	/* goto LABEL; */
	*errStatusPtr = -1;
	return(NULL);
    }
    msgODL = StrToODL(msgPtr, len);
    if (msgODL == NULL) {
	doFree(msgPtr);
	*errStatusPtr = -1;
	return(NULL);
    }
    doFree(msgPtr);
    *errStatusPtr = 0;
    return(msgODL); 
}

/*
 * NAME:
 * writeTextToServer
 *
 * DESCRIPTION:
 * a lower level routine used to read data from a socket
 *
 * NOTES:
 *
 *
 */

int writeTextToServer(char *bufPtr, int len)
{
    char *msgPtr;
    msgHeaderType *msgHeaderPtr;
    int tlen, retval;

    /* set up the message to send, attach header */
    msgPtr = (char *) doMalloc(len + sizeof(msgHeaderType));
    msgHeaderPtr = (msgHeaderType *) msgPtr;

    /* capture the length in tlen for actual tranmit */
    msgHeaderPtr->msgTotalLength = tlen = len + sizeof(msgHeaderType);
    msgHeaderPtr->msgBodyLength  = len;
    bcopy(bufPtr, &msgPtr[sizeof(msgHeaderType)],
	msgHeaderPtr->msgBodyLength);

    /* do the write */
    msgHeaderPtr->msgTotalLength = htonl(msgHeaderPtr->msgTotalLength);
    msgHeaderPtr->msgBodyLength  = htonl(msgHeaderPtr->msgBodyLength);
    retval = writen(GLOBAL_sockfd, msgPtr, tlen);

    /* free the buffer */
    free(msgPtr);
    return(retval - sizeof(msgHeaderType));
}

/*
 * NAME:
 * WriteMsgToServer
 *
 * DESCRIPTION:
 * the routine used by the client to write a message to 
 * the CP
 *
 * NOTES:
 *
 *
 */

int WriteMsgToServer(ODL ODLmsg)
{
    char *msgPtr;
    int  retval;

    msgPtr  =  (char *) ODLToStr(ODLmsg, NULL);
    if(msgPtr == NULL)
	return(-1);

    retval = writeMsg(GLOBAL_sockfd, msgPtr, strlen(msgPtr));
    free(msgPtr);

    if(retval <= 0)
	return(-1);

    return(1);
}

/*
 * NAME:
 * readMsg
 *
 * DESCRIPTION:
 * a lower level routine used to read data from a socket
 *
 * NOTES:
 *
 *
 */

char *readMsg(int fd, int *retvalPtr)
{
    int len, size, retval;
    char *bufPtr, *retPtr;
    msgHeaderType msgHeader;

    retval = recv(fd, (char *) &msgHeader, sizeof(msgHeaderType), MSG_PEEK); 
    if(retval <= 0) {
	*retvalPtr = retval;
	return(NULL);
    }

    /* convert the sizes (if necessary -- sgi defines these as null macros) */
    msgHeader.msgTotalLength = ntohl(msgHeader.msgTotalLength);
    msgHeader.msgBodyLength  = ntohl(msgHeader.msgBodyLength);

    bufPtr = (char *)doMalloc(msgHeader.msgTotalLength);
    retval = readn(fd, bufPtr, msgHeader.msgTotalLength,0);
    if(retval <= 0) {
	free(bufPtr);
	*retvalPtr = retval;
	return(NULL);
    }

    retPtr = (char *)doMalloc(msgHeader.msgBodyLength);
    bcopy(&bufPtr[sizeof(msgHeaderType)], retPtr, msgHeader.msgBodyLength);

    /* free the bufPtr here, let programer free 
    message body when he's done */
    free(bufPtr);
    *retvalPtr = retval - sizeof(msgHeaderType);
    return(retPtr);
}


/*
 * NAME:
 * readn
 *
 * DESCRIPTION:
 * the lowest level routine used to read data from a socket
 * insuring that all data is removed from the socket
 *
 * NOTES:
 *
 *
 */

int readn(fd, ptr, nbytes, flags)
register int fd;
register char *ptr;
register int nbytes;
int flags;
{
    int nleft, nread;

    nleft = nbytes;
    while (nleft > 0) {
	nread = recv(fd, ptr, nleft, flags);
	if (nread < 0)
	    return(nread);
	else if (nread == 0)
	    break;
	nleft -= nread;
	ptr += nread;
    }
    return(nbytes -nleft);
}

/*
 * NAME:
 * writeMsg
 *
 * DESCRIPTION:
 * a lower level routine used to read data from a socket
 *
 * NOTES:
 *
 *
 */

int writeMsg(int fd, char *bufPtr, int len)
{
    char *msgPtr;
    msgHeaderType *msgHeaderPtr;
    int tlen, retval;

    /* set up the message to send, attach header */
    msgPtr = (char *) doMalloc(len + sizeof(msgHeaderType));
    msgHeaderPtr = (msgHeaderType *)msgPtr;

    /* capture the length in tlen for actual tranmit */
    msgHeaderPtr->msgTotalLength = tlen = len + sizeof(msgHeaderType);
    msgHeaderPtr->msgBodyLength  = len;
    bcopy(bufPtr, &msgPtr[sizeof(msgHeaderType)],
	msgHeaderPtr->msgBodyLength);

    /* do the write */
    msgHeaderPtr->msgTotalLength = htonl(msgHeaderPtr->msgTotalLength);
    msgHeaderPtr->msgBodyLength  = htonl(msgHeaderPtr->msgBodyLength);
    retval = writen(fd, msgPtr, tlen);

    /* free the buffer */
    free(msgPtr);
    return(retval - sizeof(msgHeaderType));
}

/*
 * NAME:
 * writen
 *
 * DESCRIPTION:
 * the lowest level routine used to write data to a socket
 * insuring that all data is placed on the socket
 *
 * NOTES:
 *
 *
 */

int writen(fd, ptr, nbytes)
register int fd;
register char *ptr;
register int nbytes;
{
    int nleft, nwritten;

    nleft = nbytes;
    while (nleft > 0) {
        /*nwritten = send(fd, ptr, nleft, 0); */
        nwritten = write(fd, ptr, nleft);
	if (nwritten <= 0)
	    return(nwritten);
	nleft -= nwritten;
	ptr += nwritten;
    }
    return(nbytes - nleft);
}


/*
 * NAME:
 *  CloseXport 
 *
 * DESCRIPTION:
 *  close the tcp/ip socket  
 *
 * NOTES:
 *
 */

int CloseXport()
{
    close(GLOBAL_sockfd);
    return(0);
}

/*
 * NAME:
 * attempt_connection_forever
 *
 * DESCRIPTION:
 *
 * If the initial connection attempt to the CP fails, the routine will re-
 * attempt to establish the connection every RECONNECT_INTERVAL seconds,
 * for a maximum of RECONNECT_RETRIES tries.  After reaching that number,
 * the routine prints an appropriate message, goes to sleep for SLEEPTIME
 * seconds, and then starts the whole process over again in an infinite
 * loop.
 *
 * NOTES:
 * Logically, SLEEPTIME should be much greater than RECONNECT_INTERVAL.
 *
 */

void attempt_connection_forever()
{
    int retryNo;
    int connected = FALSE;

    for (;;) {
	for (retryNo = 1; retryNo <= RECONNECT_RETRIES; retryNo++) {
	    close(GLOBAL_sockfd);
	    if (InitClientInternal(GLOBAL_portid, GLOBAL_CP_inet) >= 0) {
		connected = TRUE;
		break;
	    } else {
		printfLLog(LOG_ERR,
		    "Connection attempt to CP failed (%d/%d)\n",
		    retryNo, RECONNECT_RETRIES);
		if (retryNo == RECONNECT_RETRIES) break;
		sleep(RECONNECT_INTERVAL);
	    }
	}
	if (connected == TRUE) {
	    break;
	} else {
	    printfLLog(LOG_ERR,
		"Failed to connect after %d seconds -- will retry \
in %d seconds\n", (RECONNECT_RETRIES-1)*RECONNECT_INTERVAL, SLEEPTIME);
	    sleep(SLEEPTIME);
	}
    }
}
