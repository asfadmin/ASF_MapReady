/* #define EXTRA_DEBUG /* turn on extra LOG_DEBUG syslog messages */

/* #define PRINTLOG /* */
/* #define CB_DEBUG /* */ 

#define XWP_DEST_STR NULL /* left over from old code */

static char sccsid_cprtns_xwp_c[] = "@(#)cprtns_xwp.c	1.8 96/08/14 11:42:20";

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <signal.h>
#include <syslog.h> 

#include "asf.h"    
#include "odl.h"    /* contains tuan's value_data for agg */
#include "asfcommon.h"
#include "cpdefines.h"
#include "cplogs.h"
#include "cprtns_xwp.h"
#include "cpworkProc.h"
#include "cpconfig.h"
#include "cpreadThread.h"
#include "serverSocketXport.h"
#include "logUtils.h"
#include "memUtils.h"
#include "listcore.h"
#include "listpid.h"
#include "que_sys.h"
#include "inet.h"

/*----------------------------------------------------------
 * NAME:
 * buildDirectXwpElem
 *
 * DESCRIPTION:
 *  build an ersatz Xwp element for the handleXwp
 *  routine to display
 *
 * NOTES:
 *  Element will be freed in handleXwp, -- but note
 *  how it is freed
 *
 *---------------------------------------------------------*/
xwpQueElemType *buildDirectXwpElem(char *reqStrPtr, int actionFlag,
                int  replPos, char *srcPtr, dialogCBdata *cbData, int  xEntity)
{
 xwpQueElemType *direct_xwpQueElemPtr;

#ifdef CB_DEBUG
if (cbData != NULL) printCBdata("buildDirectXwpElem", cbData); fflush(stdout);
#endif
  /* set up the element to add */
  direct_xwpQueElemPtr = (xwpQueElemType *)doMalloc(sizeof(xwpQueElemType));
  if (direct_xwpQueElemPtr == NULL)
    return(NULL);

  if (reqStrPtr == NULL)
    return(NULL);
  direct_xwpQueElemPtr->reqStrPtr = (char *)doMalloc(strlen(reqStrPtr) + 1);
  if (direct_xwpQueElemPtr->reqStrPtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }

  direct_xwpQueElemPtr->actionFlag = actionFlag;
  direct_xwpQueElemPtr->replPos = replPos;
  if (srcPtr == NULL)
    return(NULL);
  direct_xwpQueElemPtr->srcPtr = (char *)doMalloc(strlen(srcPtr) + 1);

  if (direct_xwpQueElemPtr->srcPtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }
  if (cbData != NULL)
    memcpy(&direct_xwpQueElemPtr->cbData, cbData, sizeof(dialogCBdata)); 

  strcpy(direct_xwpQueElemPtr->reqStrPtr, reqStrPtr);
  strcpy(direct_xwpQueElemPtr->srcPtr,    srcPtr);

  /* strcpy(direct_xwpQueElemPtr->destPtr,   destPtr); */
/*  strcpy(direct_xwpQueElemPtr->cbData->platform,   cbData->platform); */

  direct_xwpQueElemPtr->xEntity         = xEntity;
  return(direct_xwpQueElemPtr);

} /* end buildDirectXwpElem.......................*/



/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessage_direct(char *namePtr, int logDest,
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to put up
 *  X entities
 *
 * NOTES:
 *  Note freeing of xwpQueElemPtr at end of handleXwpQ.
 *
 *---------------------------------------------------------*/
int ASFlogMessage_direct(char *namePtr, int logDest, char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
     va_start(args, format);
     vsprintf(messagePtr,  format, args);

     direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr, 
                           ADD_ITEM, 0, namePtr, XWP_DEST_STR, logDest);
#ifdef PRINTLOG
printf("ASFlogMessage_direct -- name %s dest %d msgPtr %s\n",
             namePtr, logDest, messagePtr);
#endif
    doFree(messagePtr);
    if (direct_xwpQueElemPtr == NULL)
      return(-1);
    handleXwpq(direct_xwpQueElemPtr);
    va_end(args);
    return(0);
  }

 return(-1);
} /* end ASFlogMessage_direct....................*/

/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessage_direct_CB(char *namePtr, int logDest,
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to put up
 *  X entities
 *
 * NOTES:
 *  Note freeing of xwpQueElemPtr at end of handleXwpQ.
 *
 *---------------------------------------------------------*/
int ASFlogMessage_direct_CB(char *namePtr, int logDest, dialogCBdata *cbData,
                            char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
     va_start(args, format);
     vsprintf(messagePtr,  format, args);

     direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr,
                           ADD_ITEM, 0, namePtr, cbData, logDest);
#ifdef PRINTLOG
printf("ASFlogMessage_direct_CB -- name %s dest %d msgPtr %s\n",
             namePtr, logDest, messagePtr);
#endif
    doFree(messagePtr);
    if (direct_xwpQueElemPtr == NULL)
      return(-1);
    handleXwpq(direct_xwpQueElemPtr);
    va_end(args);
    return(0);
  }

 return(-1);
} /* end ASFlogMessage_direct_CB....................*/




/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageSel_direct 
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to
 *  select an item in a list box at position replPos
 *  using the work proc queueing mechanism
 *
 * NOTES:
 *  - freeing of xwpQueElemPtr at end of handleXwpQ.
 *  - Xm does not seem to allow items that have not been added
 *    to be replaced
 *
 *---------------------------------------------------------*/
int ASFlogMessageSel_direct(char *namePtr, int logDest, int replPos,
              char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

  if (replPos < 0) {
#ifdef EXTRA_DEBUG
   printfLLog(LOG_DEBUG, LIST_POS_ERROR, namePtr, replPos);
#endif
   return(-1);
  }

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
   va_start(args, format);
   vsprintf(messagePtr,  format, args);

   direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr, 
           SELECT_ITEM, replPos, namePtr, XWP_DEST_STR, logDest);
   doFree(messagePtr);
   if (direct_xwpQueElemPtr == NULL)
    return(-1);
#ifdef PRINTLOG
dumpMstrq();
printf("ASFlogMessageSel_direct -- name %s dest %d replpos %d messagePtr %s\n", 
      namePtr, logDest, replPos, messagePtr);  
#endif
   handleXwpq(direct_xwpQueElemPtr);
   va_end(args);
 }  

 return(0);

} /* end ASFlogMessageSel_direct....................*/

/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageRepl_direct 
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to
 *  replace an item in a list box at position replPos
 *  using the work proc queueing mechanism
 *
 * NOTES:
 *  - freeing of xwpQueElemPtr at end of handleXwpQ.
 *  - Xm does not seem to allow items that have not been added
 *    to be replaced
 *
 *---------------------------------------------------------*/
int ASFlogMessageRepl_direct(char *namePtr, int logDest, int replPos,
              char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

  if (replPos < 0) {
#ifdef EXTRA_DEBUG
   printfLLog(LOG_DEBUG, LIST_POS_ERROR, namePtr, replPos);
#endif
   return(-1);
  }

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
   va_start(args, format);
   vsprintf(messagePtr,  format, args);

   direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr, 
           REPLACE_ITEM, replPos, namePtr, XWP_DEST_STR, logDest);
   doFree(messagePtr);
   if (direct_xwpQueElemPtr == NULL)
    return(-1);
#ifdef PRINTLOG
dumpMstrq();
printf("ASFlogMessageRepl_direct -- name %s dest %d replpos %d messagePtr %s\n", 
      namePtr, logDest, replPos, messagePtr);  
#endif
   handleXwpq(direct_xwpQueElemPtr);
   va_end(args);
 }  

 return(0);

} /* end ASFlogMessageRepl_direct....................*/


/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageDel_direct 
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to
 *  delete an item in the list box at replPos using
 *  the work proc queueing mechanism
 *
 * NOTES:
 *  - freeing of xwpQueElemPtr at end of handleXwpQ.
 *
 *---------------------------------------------------------*/
int ASFlogMessageDel_direct(char *namePtr, int logDest, int replPos,
              char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

  if (replPos < 0) {
#ifdef EXTRA_DEBUG
   printfLLog(LOG_DEBUG, LIST_POS_ERROR, namePtr, replPos);
#endif
   return(-1);
  }

  /* mark item for deletion */
  replPos = -1*replPos;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
   va_start(args, format);
   vsprintf(messagePtr,  format, args);

   direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr, 
                          DELETE_ITEM, replPos, namePtr, XWP_DEST_STR, logDest);
   doFree(messagePtr);
   if (direct_xwpQueElemPtr == NULL)
    return(-1);
#ifdef PRINTLOG
printf("nancy ASFlogMessageDel_direct -- name %s dest %d replpos %d\n", 
      namePtr, logDest, -1*replPos);  
#endif
   handleXwpq(direct_xwpQueElemPtr);
   va_end(args);
 }  
 return(0);

} /* end ASFlogMessageDel_direct....................*/


/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageInsert_direct
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to put up
 *  X entities
 *
 * NOTES:
 *  - Note freeing of xwpQueElemPtr at end of handleXwpQ.
 *  - Current this insert only happens in X thread so there is
 *    no corresponding queue routine
 *
 *---------------------------------------------------------*/
int ASFlogMessageInsert_direct(char *namePtr, int logDest, int pos,
              char *format, ...)
{
 va_list args;
 char    *messagePtr;
 xwpQueElemType *direct_xwpQueElemPtr;

#ifdef PRINTLOG
printf("ASFlogMessageInsert_direct -- name %s dest %d pos %d format %s\n", 
      namePtr, logDest, pos, format);  
#endif
  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
    va_start(args, format);
    vsprintf(messagePtr,  format, args);

    direct_xwpQueElemPtr = buildDirectXwpElem(messagePtr, 
                           INSERT_ITEM, pos, namePtr, XWP_DEST_STR, logDest);
    doFree(messagePtr);
    if (direct_xwpQueElemPtr == NULL)
      return(-1);
    handleXwpq(direct_xwpQueElemPtr);
    va_end(args);
    return(0);
  }

 return(-1);
} /* end ASFlogMessageInsert_direct....................*/


/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageDelAll_direct 
 *
 * DESCRIPTION:
 *  if in main X thread, directly use X calls to
 *  delete all items in the list box
 *  the work proc queueing mechanism
 *
 * NOTES:
 *  - freeing of xwpQueElemPtr at end of handleXwpQ.
 *
 *---------------------------------------------------------*/
int ASFlogMessageDelAll_direct(char *namePtr, int logDest) 
{
 xwpQueElemType *direct_xwpQueElemPtr;

 direct_xwpQueElemPtr = buildDirectXwpElem("DELETE_ALL", 
                        DELETE_ALL, NOT_SET_ID, namePtr, XWP_DEST_STR, logDest);
 if (direct_xwpQueElemPtr == NULL)
  return(-1);

#ifdef PRINTLOG
printf("ASFlogMessageDelall_direct -- name %s dest %d \n", namePtr, logDest); 
#endif
 handleXwpq(direct_xwpQueElemPtr);
   
 return(0);

} /* end ASFlogMessageDelAll_direct....................*/


/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessage 
 *
 * DESCRIPTION:
 *  add an item to a list box using the work proc queueing
 *  mechanism
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int ASFlogMessage(char *namePtr, int logDest, char *format, ...)
{
 va_list args;
 char    *messagePtr;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
    va_start(args, format);
    vsprintf(messagePtr,  format, args);

#ifdef PRINTLOG
printf("ASFlogMessage-- name %s dest %d msgPtr %s\n",
             namePtr, logDest, messagePtr);
#endif
    AddXwpReqToQue_XWPQUE(messagePtr, ADD_ITEM, 0, namePtr, XWP_DEST_STR, 
         logDest); 
    doFree(messagePtr);
    va_end(args);
    return(0);
  }

 return(-1);
} /* end ASFlogMessage....................*/

/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessage_CB
 *
 * DESCRIPTION:
 *  add an item to a list box using the work proc queueing
 *  mechanism
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int ASFlogMessage_CB(char *namePtr, int logDest, dialogCBdata *cbData,
                     char *format, ...)
{
 va_list args;
 char    *messagePtr;
 int retval;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
    va_start(args, format);
    vsprintf(messagePtr,  format, args);
#ifdef CB_DEBUG
printCBdata("ASFlog_CB IN ", cbData);
#endif

    retval = AddXwpReqToQue_XWPQUE(messagePtr, ADD_ITEM, 0, namePtr, cbData,
         logDest);
#ifdef CB_DEBUG
/*    printf("AddXwpReqToQue_XWPQUE returned %d\n", retval); */
dumpXwpq();
#endif

    doFree(messagePtr);
    va_end(args);
#ifdef CB_DEBUG
printCBdata("ASFlog_CB OUT", cbData);
#endif
    return(0);
  }

 return(-1);
} /* end ASFlogMessage_CB....................*/


/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageRepl 
 *
 * DESCRIPTION:
 *  replace an item in a list box at position replPos
 *  using the work proc queueing mechanism
 *
 * NOTES:
 *  Xm does not seem to allow items that have not been added
 *  to be replaced
 *
 *---------------------------------------------------------*/
int ASFlogMessageRepl(char *namePtr,int logDest, int replPos, char *format, ...)
{
 va_list args;
 char    *messagePtr;

  if (replPos < 0) {
#ifdef EXTRA_DEBUG
   printfLLog(LOG_DEBUG, LIST_POS_ERROR, namePtr, replPos);
#endif
   return(-1);
  }

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
    va_start(args, format);
    vsprintf(messagePtr,  format, args);
#ifdef PRINTLOG
printf("ASFlogMessageRepl -- name %s dest %d replpos %d\n", 
      namePtr, logDest, replPos);  
#endif

    AddXwpReqToQue_XWPQUE(messagePtr, 
         REPLACE_ITEM, replPos, namePtr, XWP_DEST_STR, logDest);
    doFree(messagePtr);
   va_end(args);
 }  

  return(0);

} /* end ASFlogMessageRepl....................*/

/*----------------------------------------------------------
 * NAME:
 *  ASFlogMessageDel 
 *
 * DESCRIPTION:
 *  delete an item in the list box at replPos using
 *  the work proc queueing mechanism
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int ASFlogMessageDel(char *namePtr, int logDest, int replPos, char *format, ...)
{
 va_list args;
 char    *messagePtr;

  if (replPos < 0) {
#ifdef EXTRA_DEBUG
   printfLLog(LOG_DEBUG, LIST_POS_ERROR, namePtr, replPos);
#endif
   return(-1);
  }

  /* mark item for deletion */
  replPos = -1*replPos;

  if ((messagePtr = doMalloc(MAX_DISPLAY_LEN)) != NULL) {
    va_start(args, format);
    vsprintf(messagePtr,  format, args);

    AddXwpReqToQue_XWPQUE(messagePtr, 
                         DELETE_ITEM, replPos, namePtr, XWP_DEST_STR, logDest);
   doFree(messagePtr);
   va_end(args);
 }  
 return(0);

} /* end ASFlogMessageDel....................*/

