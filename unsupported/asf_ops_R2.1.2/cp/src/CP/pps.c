#define USE_ONE_ALGORITHM /* one or two job request algorithms  */
                          /* we use one right now because       */
                          /* the second one isn't quite working */

#define EXPECT_ACK /* expect pps to return SUBSYSTEM_ACK before job */

/* #define FULL_PPS_DEBUG /* debug the automatic requesting of jobs */

/* #define PPS_DEBUG /* debug the automatic requesting of jobs */
/* #define PPS_SELECT /* debug the selection of which pps job to request */
/* #define PPS_RANK /* debug the ranking used in selection */

static char sccsid_pps_c[] = "@(#)pps.c	1.31 97/08/29 16:07:59";

#include <stdio.h>
#include <errno.h>
#include <stdlib.h> /* for qsort */
#include <syslog.h>
#include <sys/time.h>
#include "asfcommon.h"
#include "asf.h"  /* GetNewMsg */
#include "odl.h"
#include "logUtils.h"

#include "cpdefines.h"
#include "listcore.h"
#include "listmstrq.h"
#include "cpworkProc.h"
#include "cprtns.h"
#include "cplogs.h"
#include "que_sys.h" 
#include "cpconfig.h" 
#include "cpbuildMsg.h" 
#include "pps.h" 
#include "UxXt.h"  /* for UxAppContext */

char *GLOBAL_ppsLastModeStr, *GLOBAL_ppsLastTypeStr;
int GLOBAL_ppsLastMode=-1, GLOBAL_ppsLastType=-1;
int GLOBAL_ppsOut = False;
int GLOBAL_ppsAvail[P_REQ_LAST] = {1, 1, 1, 1};

int ppsLastRequested = -1;

extern int totalMQentries;
extern char *getModeStr();
extern char *getTypeStr();
extern char *timestamp();


void initPPS(subsysConfigType *subsysConfigPtr) 
{ 
  int i;

  for (i = 0; i < P_REQ_LAST; i++)
    GLOBAL_ppsAvail[i] = 1;

  ASFlogMessage(ASF_CP, WP_MAX_JOBS_TEXT_ID, "%d", 
             subsysConfigPtr->PPSqueueSize);

}


int getPPSlastRequested()
{
  return(ppsLastRequested);
}

char *getPPSlastMode()
{
  if (ppsLastRequested == P_SS_SCAN || ppsLastRequested == P_SS_FRAME)
    return(PPS_MSG_MODE_SS);
  else if (ppsLastRequested == P_CTS_SCAN || ppsLastRequested == P_CTS_FRAME)
    return(PPS_MSG_MODE_CTS);

  return(NULL);
} /* getPPSlastMode */


char *getPPSlastType()
{
  if (ppsLastRequested == P_SS_SCAN || ppsLastRequested == P_CTS_SCAN)
    return(PPS_MSG_TYPE_SCAN);
  else if (ppsLastRequested == P_SS_FRAME || ppsLastRequested == P_CTS_FRAME)
    return(PPS_MSG_TYPE_FRAME);
  return(NULL);
} /* getPPSlastType */




int selectPPSmsgType(int *mode, int *type)
{
  static int initialFill = True, lastChecked;
  int retval=-1; /* default to good return status */
  int i, rank[P_REQ_LAST] = {3, 2, 1, 0};
#ifdef PPS_RANK 
  int ii,jj;
#endif

  initialFill = (GLOBAL_ppsLastMode == -1 || GLOBAL_ppsLastType == -1);
#ifdef PPS_SELECT
printf("#####enter select: (global) mode %d type %d, this mode %d type %d fill %d, last %d\n",
 GLOBAL_ppsLastMode, GLOBAL_ppsLastType, *mode, *type, initialFill, ppsLastRequested);
#endif

#ifdef USE_ONE_ALGORITHM
  initialFill = True;  /* force to use first algorithm until the second works */
#endif

  if (initialFill) {
    for (i = 0; i < P_REQ_LAST; i++) {
      if (++ppsLastRequested >= P_REQ_LAST)
        ppsLastRequested = P_REQ_FIRST+1;

#ifdef PPS_SELECT
printf("i %d, select calling GetAvail_PPS with %d\n", i, ppsLastRequested);
#endif
      if (GetAvail_PPS(ppsLastRequested)) { /* needed to sleep when no jobs */
/*      if (1) { */
        switch (ppsLastRequested) {
          case P_SS_SCAN:
            *mode = MODE_SCANSAR_ID;
            *type = REQ_TYPE_SCAN_ID;
#ifdef PPS_SELECT
printf("select returning SS, SCAN, lastRequested %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_CTS_SCAN:
            *mode = MODE_CONTINUOUS_ID;
            *type = REQ_TYPE_SCAN_ID;
#ifdef PPS_SELECT
printf("select returning CTS, SCAN, lastRequested %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_SS_FRAME:
            *mode = MODE_SCANSAR_ID;
            *type = REQ_TYPE_FRAME_ID;
#ifdef PPS_SELECT
printf("select returning SS, FRAME, lastRequested %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_CTS_FRAME:
            *mode = MODE_CONTINUOUS_ID;
            *type = REQ_TYPE_FRAME_ID;
#ifdef PPS_SELECT
printf("select returning CTS, FRAME, lastRequested %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          default:
            *mode = *type = NOT_SET_ID; 
            ppsLastRequested = P_REQ_FIRST;
  /*           return(-1); don't return, try the lower numbered ones! */
            break;
        }
      }
               /* if got to here, found no match - try next one */
      if (ppsLastRequested == P_REQ_LAST)
        ppsLastRequested = P_REQ_FIRST;

    }

    return(retval);
  } /* initial Fill */

  else { /* these are refills, not initial population of queue */

    GetJobRank_MSTRQLIST(rank);

#ifdef PPS_RANK
printf("GetJobRank_MSTRQLIST returned this rank array:\n");
for (ii=0; ii < P_REQ_LAST; ii++)
  for (jj=0; jj < P_REQ_LAST; jj++)
    printf("job rank[%d] is %d\n", ii, jj); 
#endif

    lastChecked = ppsLastRequested;
    for (i=0; i < P_REQ_LAST; i++) {
#ifdef PPS_SELECT
      printf("loop i %d ppsLast %d lastChecked %d rank %d %d %d %d\n ", 
        i, ppsLastRequested, lastChecked, rank[0], rank[1], rank[2], rank[3]);
#endif

      if (lastChecked == rank[i] && !GetAvail_PPS(rank[i])) {
#ifdef PPS_SELECT
printf("\tcontinuing at the top.... lastChecked == rank && not avail\n");
#endif
        continue;  /* don't try one that just failed last time */
      }

      if (rank[i] == ppsLastRequested && lastChecked != ppsLastRequested) {
#ifdef PPS_SELECT
printf("\tcontinuing at the top.... rank == ppsLast && last != ppsLast\n");
#endif
        if (lastChecked == P_REQ_LAST) 
          lastChecked = P_REQ_FIRST;
        else
          lastChecked++;
        continue; /* begin next iteration of for... */
      }
#ifdef PPS_SELECT
printf("ready to process: rank[%d] %d, lastChecked %d rank[last] %d\n", i, rank[i], lastChecked, rank[i]);
#endif
        switch (rank[i]) {
          case P_SS_SCAN:
            *mode = MODE_SCANSAR_ID;
            *type = REQ_TYPE_SCAN_ID;
            ppsLastRequested = rank[i]; 
#ifdef PPS_SELECT
printf("selectPPSmsg selection: %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_CTS_SCAN:
            *mode = MODE_CONTINUOUS_ID;
            *type = REQ_TYPE_SCAN_ID;
            ppsLastRequested = rank[i]; 
#ifdef PPS_SELECT
printf("selectPPSmsg selection: %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_SS_FRAME:
            *mode = MODE_SCANSAR_ID;
            *type = REQ_TYPE_FRAME_ID;
            ppsLastRequested = rank[i]; 
#ifdef PPS_SELECT
printf("selectPPSmsg selection: %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          case P_CTS_FRAME:
            *mode = MODE_CONTINUOUS_ID;
            *type = REQ_TYPE_FRAME_ID;
            ppsLastRequested = rank[i]; 
#ifdef PPS_SELECT
printf("selectPPSmsg selection: %d\n", ppsLastRequested);
#endif
            return(1);
            break;
          default:
            *mode = *type = NOT_SET_ID;
#ifdef PPS_SELECT
printf("selectPPSmsg selection: %d\n", ppsLastRequested);
#endif
            ppsLastRequested = P_REQ_FIRST;
            break;
        }
      if (ppsLastRequested == P_REQ_LAST) {
#ifdef PPS_SELECT
printf("\tsetting ppsLastRequested from last %d to first %d\n", P_REQ_LAST, P_REQ_FIRST);
#endif
        ppsLastRequested = P_REQ_FIRST;
      }
    }

    return(retval);  /* didn't find one */
  
  } /* end of non-initial fill processing */


#ifdef PPS_SELECT
printf("leaving selectPPSmsgType returning retval %d mode %s type %s\n", 
        retval, mode, type);
#endif
  return(retval);

} /* selectPPSmsgType */

int requestPPSmsg(char *mode, char *reqType)
{
 ODL writeOutODL;
 int ok=0, sendStatus=0, socket,subsysState;
 char *namePtr, *errmsg;
 sysQueElemType *sysQueElemPtr;

  namePtr = getPPSname();
#ifdef PPS_DEBUG
printf("requestPPSmsg: %s %s\n", mode, reqType);
#endif

  subsysState = GetProcessState_PIDLIST(namePtr); /* see if process is running*/
  if (subsysState != SUBSYS_RUNNING_ID &&
      subsysState != SUBSYS_READY_ID &&
                                      /* subsysState != SUBSYS_HALTING_ID && */
      subsysState != SUBSYS_HEALTHING_ID) {
   printfLLog(LOG_ERR, CANNOT_GET_PID_TO, "send job request message \n");
    return(-1);
  }

  socket = GetSocketGivenName_PIDLIST(namePtr);
  if (socket < 0) {
   printfLLog(LOG_ERR, CANNOT_GET_PID_TO, "send job request message \n");
   return (-1);
  }
  printfLLog(LOG_INFO, PPS_REQUESTING_JOB,  mode, reqType, namePtr);
  sysQueElemPtr = GetFirstNonQCelem_SYSQUE(namePtr, DO_COPY);
  if (sysQueElemPtr != NULL) {
   FreeSysQueElement_SYSQUE(namePtr, sysQueElemPtr);
  }

  writeOutODL = (ODL) GetNewMsg(SPS_JOB_REQUEST);

  if (ODLSetVal(writeOutODL, errmsg = JOB_REQ_HDR_DEST, namePtr))
    if (ODLSetVal(writeOutODL, errmsg = JOB_REQ_TYPE, reqType))
      if (ODLSetVal(writeOutODL, errmsg = JOB_REQ_MODE, mode))
        if (ODLSetVal(writeOutODL, errmsg = JOB_REQ_HDR_SOURCE, ASF_CP))
          ok = 1;  /* if got to here all is ok;
                      otherwise errmsg contains your error string */
  if (!ok) {
   printfLLog(LOG_ERR, CANNOT_SET, errmsg);
   ODLFree(writeOutODL);
   return (-1);
  }

#ifdef EXPECT_ACK
  sendStatus = doSendMsg(namePtr, socket, writeOutODL, EXPECT_ACK_MSG);
  if (sendStatus < 0) {

   if (errno) /* if errno set, use that info in the error message */
    ASFlogMessage(ASF_CP, WP_ERROR_BOX,CANNOT_SEND_MSG_ERRNO,
                             namePtr, strerror(errno) );
   else
    ASFlogMessage(ASF_CP, WP_ERROR_BOX,CANNOT_SEND_MSG_SOCKET, namePtr);

    resetSubsysByName(namePtr);
  }
  ODLFree(writeOutODL);
  return(sendStatus);
#else
  sendStatus = doSendMsg(namePtr, socket, writeOutODL, EXPECT_REQUEST_MSG);
  if (sendStatus < 0) {
   if (errno) /* if errno set, use that info in the error message */
    ASFlogMessage(ASF_CP, WP_ERROR_BOX,CANNOT_SEND_MSG_ERRNO,
                             namePtr, strerror(errno) );
   else
    ASFlogMessage(ASF_CP, WP_ERROR_BOX,CANNOT_SEND_MSG_SOCKET, namePtr);

    resetSubsysByName(namePtr);
  }
  ODLFree(writeOutODL);
  if (sendStatus == 0)
    ChangeProcessState_PIDLIST(namePtr, SUBSYS_RUNNING_ID, NOT_SET_ID);

  return(sendStatus);
#endif

  return(-1);

} /* end requestPPSmsg..................................*/


void doPPS()
{
 static int mode=MODE_SCANSAR_ID, type=REQ_TYPE_SCAN_ID;
 baseListElemType *nPtr;
 XtIntervalId intervalId;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return;
#ifdef FULL_PPS_DEBUG
printf("doPPS - locked.  total jobs %d max jobs %d GetMsgOut() %d\n", totalMQentries, getPPSqueueSize(), GetMsgOut_PPS()); 
#endif

  if (GetMsgOut_PPS() != True) { /* if not already waiting for a pps message */
    if ( totalMQentries < getPPSqueueSize()) { /* is room for more? */
#ifdef PPS_DEBUG
printf("doPPS: calling select for mode %d type %d\n", mode, type);
#endif
        if (selectPPSmsgType(&mode, &type) != -1) {
          if ((requestPPSmsg(getModeStr(mode),getTypeStr(type)))==0)
            SetMsgOut_PPS();
        }
        else { /* couldn't find one! */
          SetMsgOut_PPS(); 
          printfLLog(LOG_DEBUG, PPS_NO_JOBS_OF_ANY_TYPE);
          intervalId = XtAppAddTimeOut(UxAppContext, PPS_RETRY_TIMEOUT_VALUE,
                    (XtTimerCallbackProc)ClearMsgOut_PPS, (XtPointer) TRUE); 
        }
    }
  }
#ifdef FULL_PPS_DEBUG
  else
    printf("doPPS: waiting for pps to send msg\n");
#endif
  unLockNameList_CORE(nPtr);

} /* doPPS */

/*----------------------------------------------------------
 * NAME:
 *  CreateLockList_PPS 
 *
 * DESCRIPTION:
 *  create the thread safe pps list and sema
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int CreateLockList_PPS()
{
 int retval;

  retval =  createOneNameListEntry_CORE(PPS_LIST);
  return(retval);

} /* CreateLockList_PPS...................*/

/*----------------------------------------------------------
 * NAME:
 *  GetLastModeStr_PPS
 *
 * DESCRIPTION:
 *  set the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
char * GetLastModeStr_PPS()
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return(NULL);

  unLockNameList_CORE(nPtr);
  return(GLOBAL_ppsLastModeStr);

} /* GetLastModeStr_PPS.................*/
/*----------------------------------------------------------
 * NAME:
 *  GetLastTypeStr_PPS
 *
 * DESCRIPTION:
 *  set the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
char * GetLastTypeStr_PPS()
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return(NULL);

  unLockNameList_CORE(nPtr);
  return(GLOBAL_ppsLastTypeStr);

} /* GetLastTypeStr_PPS.................*/


/*----------------------------------------------------------
 * NAME:
 *  SetMsgOut_PPS
 *
 * DESCRIPTION:
 *  set the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int SetMsgOut_PPS()
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return(-1);

  GLOBAL_ppsOut = True;
#ifdef PPS_DEBUG
printf("SetMsgOut_PPS: \n");
#endif

  unLockNameList_CORE(nPtr);
  return(-1);

} /* SetMsgOut_PPS.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetMsgOut_PPS
 *
 * DESCRIPTION:
 *  get the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int GetMsgOut_PPS()
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return(-1);

  unLockNameList_CORE(nPtr);
#ifdef FULL_PPS_DEBUG
printf("GetMsgOut_PPS: returning %d\n", GLOBAL_ppsOut);
#endif
  return(GLOBAL_ppsOut);

} /* GetMsgOut_PPS.................*/


/*----------------------------------------------------------
 * NAME:
 *  ClearMsgOut_PPS
 *
 * DESCRIPTION:
 *  clear the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void ClearMsgOut_PPS(int resetAll)
{
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return;

  GLOBAL_ppsOut = False;
#ifdef PPS_DEBUG
printf("ClearMsgOut_PPS: reset all: %d, %s\n", resetAll, timestamp());
#endif

  unLockNameList_CORE(nPtr);

  if (resetAll)
    for (i=0; i < P_REQ_LAST; i++) {
      SetAvail_PPS(i);
    }

  return;

} /* ClearMsgOut_PPS.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetAvail_PPS
 *
 * DESCRIPTION:
 *  clear the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int GetAvail_PPS(int which)
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return(0);

  unLockNameList_CORE(nPtr);

#ifdef PPS_DEBUG
printf("GetAvail_PPS[%d] returning %d\n", which, GLOBAL_ppsAvail[which]); 
#endif

  return(GLOBAL_ppsAvail[which]);

} /* GetAvail_PPS.................*/


/*----------------------------------------------------------
 * NAME:
 *  SetAvail_PPS
 *
 * DESCRIPTION:
 *  clear the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void SetAvail_PPS(int which)
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return;

  GLOBAL_ppsAvail[which] = 1;

#ifdef PPS_DEBUG
printf("SetAvail_PPS[%d] \n", which);
#endif
  unLockNameList_CORE(nPtr);
  return;

} /* SetAvail_PPS.................*/

/*----------------------------------------------------------
 * NAME:
 *  ClearAvail_PPS
 *
 * DESCRIPTION:
 *  clear the pps flag
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void ClearAvail_PPS(int which)
{
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PPS_LIST);
  if (nPtr == NULL)
     return;

  GLOBAL_ppsAvail[which] = 0;

  unLockNameList_CORE(nPtr);
  return;

} /* ClearAvail_PPS.................*/


/*----------------------------------------------------------
 * NAME:
 *  showPPSinfo
 *
 * DESCRIPTION:
 *  debug routine that shows all the variables related to the
 *  selection of new pps jobs
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

void showPPSinfo()
{
  int i, rank[P_REQ_LAST];

printf("\n\nPPS state****************************************\n");
printf("ppsLastRequested %d\n", ppsLastRequested);
printf("GLOBAL_ppsOut %d\n", GLOBAL_ppsOut);
printf("GLOBAL_ppsLastMode %d ", GLOBAL_ppsLastMode);
printf("GLOBAL_ppsLastType %d\n", GLOBAL_ppsLastType);
printf("GLOBAL_ppsAvail "); for (i=0; i < P_REQ_LAST; i++) printf("%d ", GLOBAL_ppsAvail[i]); printf("\n");

    GetJobRank_MSTRQLIST(rank);
printf("GetJobRank_MSTRQLIST returned rank:"); for (i=0; i < P_REQ_LAST; i++) printf("%d ", rank[i]); printf("\n");

printf("end PPS state****************************************\n");

} /* showPPSinfo */

