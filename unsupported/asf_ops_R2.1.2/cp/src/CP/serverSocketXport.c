#define PRINT_OUTGOING /* */

static char sccsid_serverSocketXport_c[] = "@(#)serverSocketXport.c	4.17 96/12/10 12:18:46";

#include <stdio.h>
#include <sys/types.h>
#include <malloc.h> /* free */
#include <syslog.h> 

#include "memUtils.h" /* why do we use free() *and* doFree() ??? */
#include "odl.h"
#include "socketXport.h"
#include "serverSocketXport.h"

/*----------------------------------------------------------
 * NAME:
 *  ReadMsgFromClient
 *
 * DESCRIPTION:
 *  read message from client on a specific socket
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

ODL ReadMsgFromClient(int sockfd, int *errStatusPtr)
{
 char *msgPtr;
 ODL  msgODL, valODL;
 int  len;

  msgPtr =  readMsg(sockfd, &len);   
  if (msgPtr == NULL) {             /* len < 0 is error; len=0 means closed */
    *errStatusPtr = ((len < 0) ? -1 : 0);  
    return(NULL);
  } 

  msgODL = StrToODL(msgPtr, len);
  if (msgODL == NULL) {
     free(msgPtr);
     *errStatusPtr = -2;
     return(NULL);
  }

  valODL = ODLcopy(Value(((ODL*)Val(msgODL))[0]));  /* return only the inside */
  if (valODL == NULL) {
    ODLFree(msgODL);
    free(msgPtr);
    *errStatusPtr = -3;
    return(NULL);
  }

  ODLFree(msgODL);                  /* off the outermost object */


  free(msgPtr);
  *errStatusPtr = 0;
  return(valODL);

} /* end ReadMsgFromClient...........*/


/*----------------------------------------------------------
 * NAME:
 *  WriteMsgToClient
 *
 * DESCRIPTION:
 *  write a message to a client over a specific socket
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int WriteMsgToClient(int sockfd, ODL ODLmsg)
{
 char *msgPtr, *str;
 int  retval;

  msgPtr  =  (char *)ODLToStr(ODLmsg, NULL);
  if (msgPtr == NULL)
   return(-1);

#ifdef PRINT_OUTGOING
  printf("CP sending out %s\n", str = ODLToStr(ODLmsg, NULL)); fflush(stdout);
  doFree(str);
#endif
  retval = writeMsg(sockfd, msgPtr, strlen(msgPtr));
  ODLFree(msgPtr);

  if (retval <= 0)
   return(-1);

  return(1);

} /* WriteMsgToClient...............*/

