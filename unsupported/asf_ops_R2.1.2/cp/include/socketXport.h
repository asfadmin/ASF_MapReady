#ifndef _SOCKETXPORT_H
#define _SOCKETXPORT_H
/*--------------------------------
 * sccs header
 * @(#)socketXport.h	2.5 95/06/28 17:55:46
 *------------------------------*/

/*------------------------------------------
 * Name:
 *  socketXport.h
 *----------------------------------------*/

static char sccsid_socketxport_h[] =
	"@(#)socketXport.h	2.5 95/06/28 17:55:46";

typedef struct msgHeaderTypeTag{
                       int  msgTotalLength;
                       int  msgBodyLength;
                    }msgHeaderType;

/* may not be used */
typedef struct messageTypeTag{
                       msgHeaderType msgHeader;
                       char          *msgBody;
                     }messageType;

int InitClientInternal(int portid, char *inetPtr);
int InitClient(int argc, char **argv);
ODL ReadMsgFromServer(int *errStatusPtr);
int WriteMsgToServer(ODL ODLmsg);

char *readMsg(int fd, int *errorPtr);
int readn(register fd, char *ptr,register nbytes, int flags);
int WriteTextToServer(char *bufPtr, int len);
int writeMsg(int fd, char *bufPtr, int len);
int writen(register fd, char *ptr, register nbytes);
int CloseXport();

#endif				/* !_SOCKETXPORT_H */
