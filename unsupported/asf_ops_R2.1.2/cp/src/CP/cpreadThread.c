#define POP_JOB_PROFILE /* uncomment if don't want profile errors popped up */

/* #define READ_LOCK_DEBUG /* */

/* #define JOB_ORDER /* for testing out-of-order status,ack msgs */

#define EXTRA_DEBUG /* turn on extra LOG_DEBUG syslog messages */

#define CHANGE_EXE /* change exe name vs add useless pid to list */

#define EXIT_ZERO_RECEIVES /* if set, exit the thread when receive zero */
                           /* sized message.  */

#define CHECK_FILE_EXISTENCE /* jasper stub needs this commented out; 
                                /* define for operational */

#define CONNECT_INCOMING_READY /* enable this to connect to unexpected READY */

#define PRINT_INCOMING /* enable this to print incoming messages */

/* #define PRINT_DEBUG /* */

static char sccsid_cpreadThread_c[] = "@(#)cpreadThread.c	4.170 97/11/04 11:22:38";


#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <bstring.h> /* bzero */
#include <sys/types.h>
#include <unistd.h> /* for getpid */

#include <syslog.h>
#include <task.h>
#include <X11/Intrinsic.h>

#include "asf.h"    /* for GetAckMsg() proto */
#include "asfcommon.h"
#include "memUtils.h" /* for doMalloc() */
#include "logUtils.h" /* for printfLLog() */
#include "cpdefines.h"
#include "cpreadThread.h"
#include "serverSocketXport.h"
#include "listcore.h" 
#include "listmstrq.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "cplogs.h"
#include "que_xwp.h"   /* for dialogCBdata */
#include "cpconfig.h" 
#include "utils.h" 
#include "pps.h" 

/* global variables ***********************/

/* end global variables .................*/

extern char *determineProcessor(ODL msg, char *errmsg);
extern int domainCheck();
extern void ClearMsgOut_PPS();

typedef struct ODLmsgToCBTag{
                           char *ODLmsgTypePtr;
                           int  id;
                          }ODLmsgToCBType;

ODLmsgToCBType ODLmsgToCB[] = 
    {SUBSYSTEM_READY,            SUBSYSTEM_READY_ID,
     SUBSYSTEM_ACK,              SUBSYSTEM_ACK_ID,
     SUBSYSTEM_STATUS,           SUBSYSTEM_STATUS_ID,
     SUBSYSTEM_COMPLETED,        SUBSYSTEM_COMPLETED_ID,
     FRAME_REQUEST,              FRAME_REQUEST_ID,
     SPS_JOB_REQUEST,                SPS_JOB_REQUEST_ID,
     SCAN_REQUEST,               SCAN_REQUEST_ID,
     SPS_DECODE_REQUEST,         SPS_DECODE_REQUEST_ID,
     SPS_FRAME_REQUEST,          SPS_FRAME_REQUEST_ID,
     SPS_SCAN_REQUEST,           SPS_SCAN_REQUEST_ID,
     STATUS_MESSAGE,             STATUS_MESSAGE_ID,
     CLEANUP_REQUEST,            CLEANUP_REQUEST_ID,
     NULL,                       0};


/*----------------------------------------------------------
 * NAME:
 *  checkAndFindMsgID 
 *
 * DESCRIPTION:
 *  Check if we can parse the message, then find the 
 *  corresponding ID so that we can process it.
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int checkAndFindMsgID(ODL readInODL, int sockfd, int status, char *errmsg,
                        baseListElemType *nPtr)
{
 int i = 0,  msgidODLint;
 char *msgTypePtr;

  if (status < 0) {
    if (status == -1) {
      if (errno)
        ASFlogMessage( ASF_CP, WP_ERROR_BOX, THREAD_READ_ERR, sockfd, strerror(errno));
        printfLLog( LOG_DEBUG, THREAD_READ_ERR, sockfd, strerror(errno));
#ifdef EXIT_ZERO_RECEIVES
      close(sockfd);
#ifdef READ_LOCK_DEBUG
printf("checkAndFindMsgID unlocking READ_LIST\n"); fflush(stdout);
#endif
      unLockNameList_CORE(nPtr);
      exit(-1); 
#else
      return(-1);
#endif
    }
    else if (status == -2) {
     strcpy(errmsg,"cannot parse\n"); /* caller will do LOG_DEBUG with this */
     return(-1);
    }
    else if (status == -3) {
     strcpy(errmsg,"cannot allocate memory\n"); /* caller will do LOG_DEBUG */
     return(-1);
    }

  } 

  if ((ODLGetVal(readInODL, HDR_MSG_TYPE, &msgidODLint)) == 0) {
    strcpy(errmsg,"cannot determine message type\n");
    return(-1);
  }

  printfLLog(LOG_DEBUG, THREAD_RCVD_MSG, (char*) msgidODLint, sockfd);
  msgTypePtr = (char*) msgidODLint;

  while (ODLmsgToCB[i].ODLmsgTypePtr != NULL) {
    if (strcmp(ODLmsgToCB[i].ODLmsgTypePtr, msgTypePtr) == 0) 
      return(ODLmsgToCB[i].id);
    i++;
  }
  sprintf(errmsg, UNRECOG_MSG_TYPE, msgTypePtr);
  return(-1);
 
} /* end checkAndFindMsgID........................*/
  
/*----------------------------------------------------------
 * NAME:
 *  getSubTypeID
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getSubTypeID(ODL readInODL)
{
  char *sub_typePtr;

  if (!ODLGetVal(readInODL, BODY_SUB_TYPE , &sub_typePtr)) 
    return(-1);

  if (strcmp(sub_typePtr, "HEARTBEAT") == 0)
    return(HEARTBEAT_ID);
  else if (strcmp(sub_typePtr, "CLEANUP") == 0)
    return(CLEANUP_ID);
  else if (strcmp(sub_typePtr, "CATALOG") == 0)
    return(CATALOG_ID);
  else if (strcmp(sub_typePtr, "ASYNC") == 0)
    return(ASYNC_ID);

  return(-1);

} /* end getSubTypeID......................................*/

/*----------------------------------------------------------
 * NAME:
 *  getJobId
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getJobId(ODL readInODL)
{
  int jobId;

  if (!ODLGetVal(readInODL, BODY_JOB_ID , &jobId))
    return(-1);

  return(jobId);

} /* end getJobId......................................*/



/*----------------------------------------------------------
 * NAME:
 *  getSubTypeStr
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
char * getSubTypeStr(ODL readInODL)
{
  char *sub_typePtr;

  if (!ODLGetVal(readInODL, BODY_SUB_TYPE , &sub_typePtr))
    return(NULL);

  return(sub_typePtr);

} /* end getSubTypeStr......................................*/


/*----------------------------------------------------------
 * NAME:
 *  checkAndGetRequestStatus 
 *
 * DESCRIPTION:
 *  check the status of the request we just received
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int checkAndGetRequestStatus(char *namePtr, ODL readInODL, char *errmsg, int *ret_jobId) 
{
 int pos, jobid, req_status = SUBSYS_RETURN_ERROR_ID, ok=0, zz;
 int retval, cat, subType=-1, isMetaProduct=0, isRetrievedProduct=0, catStep;
 char *errMsgPtr = "", *p, *req_str;
 subsysFilesType *sf;
 mstrqListElemType *mqel=NULL;
 dialogCBdata *cbData;

 *ret_jobId = -1;
                                                       
  cbData = doMalloc(sizeof(dialogCBdata)); 
  if (cbData == NULL)  {
    printfLLog(LOG_ERR, MALLOC_ERROR, "checking message status");
    return(SUBSYS_RETURN_MALLOC_ID); /* need to add case for this! */
  }

  cbData->action = SUBSYS_RETURN_OK_ID;
  cbData->pos = cbData->rev = 0;
  cbData->buttonWid = NULL;
  strcpy(cbData->namePtr, namePtr);
  strcpy(cbData->platform, "");

  subType = getSubTypeID(readInODL);
  cat = GetSubsystemCategoryGivenName(namePtr);
#ifdef PRINT_DEBUG
printf("checkAndGetRequestStatus cat %d subtype %d\n", cat, subType ); 
#endif
  if (ODLGetVal(readInODL, strcpy(errmsg, BODY_JOB_ID), &jobid))  {
    ok = 1;
    cbData->pos = *ret_jobId = jobid;
  }

  if (ok && ODLGetVal(readInODL, strcpy(errmsg, BODY_STATUS), &req_str)) {
    ok = 1;
    if (strcmp(req_str, SUBSYS_RETURN_OK_TEXT) == 0)
      req_status = SUBSYS_RETURN_OK_ID;
    else if (strcmp(req_str, SUBSYS_RETURN_ERROR_TEXT) == 0)
      req_status = SUBSYS_RETURN_ERROR_ID;
    else if (strcmp(req_str, SUBSYS_RETURN_WRONG_TAPE_TEXT) == 0) {
      cbData->action = SUBSYS_WRONG_TAPE_ID;
      req_status = SUBSYS_RETURN_WRONG_TAPE_ID;
      errMsgPtr = WRONG_TAPE_MSG;
    }
    else {
      req_status = SUBSYS_RETURN_UNRECOGNIZED_ID;
      ok = 0;
      cbData->action    = SUBSYS_ERROR_ID;
      errmsg = "Unrecognized status";
    }
  }

  if (ok) {
    switch (req_status) {

      case SUBSYS_RETURN_WRONG_TAPE_ID:         
        ok = 0;
        break;

      case SUBSYS_RETURN_OK_ID:                 /* valid return status */
        ok = 1;
        catStep = GetCatalogStep_MSTRQLIST(jobid);
        isMetaProduct = (cat == IMS_CATEGORY_ID && 
                        (catStep == IMS_TYPE_STORE_SCAN_METADATA));
        isRetrievedProduct = (cat == IMS_CATEGORY_ID && 
                             (catStep == IMS_TYPE_GET_SCAN_RESULT ||
                              catStep == IMS_TYPE_GET_CAL_PARAMS ));
        if ( (cat == RDS_CATEGORY_ID || isMetaProduct || isRetrievedProduct ||
              cat == SSP2_CATEGORY_ID || cat == ASP_CATEGORY_ID) 
           && subType != CLEANUP_ID && subType != HEARTBEAT_ID ) {
          sf = GetSubsysFilesGivenJobId_MSTRQLIST(jobid);
#ifdef PRINT_DEBUG
showSf_sf("checkAndGetRequestStatus, got at start", sf);
printf("checkAndGetRequestStatus: subsys files jobid %d numFiles %d numProducts %d\n", sf->jobId,sf->numFiles, sf->numProducts); fflush(stdout);
#endif

          for (zz = 0; ok && zz < sf->numFiles; zz++) {
                              /* strip off the hostname before comparison */
            p = (p = strchr(sf->fileName[zz], ':')) ? p+1 : sf->fileName[zz];

            printfLLog(LOG_INFO, CHECKING_FILE_EXISTENCE, p);
#ifdef CHECK_FILE_EXISTENCE
            if (!nonzeroFileExists(p)) {
              ok = 0;
              errMsgPtr = SUBSYS_FILES_INCOMPLETE;
              printfLLog(LOG_ERR, PRODUCT_NOT_EXIST, namePtr, p);
 
            }
            else {
#endif
              strcpy(sf->productName[sf->numProducts], sf->fileName[zz]);
              (sf->numProducts)++;

              SetSubsysFilesGivenJobId_MSTRQLIST(jobid, sf);
#ifdef CHECK_FILE_EXISTENCE
           }
#endif
          }
#ifdef PRINT_DEBUG
showSf_sf("checkAndGetRequestStatus, leaving with", sf);
#endif
        doFree(sf);
        }
        break;

      case SUBSYS_RETURN_ERROR_ID:    /* bad status value, */
                                    /* copy the comment for display */
        ok = 0;
        cbData->action    = SUBSYS_ERROR_ID;
        if (ODLGetVal(readInODL, strcpy(errmsg,BODY_COMMENT), &errMsgPtr))
          strcpy(errmsg, errMsgPtr);
        else
          printfLLog(LOG_ERR, "Couldn't get COMMENT string from message");

        if (cat == PPS_CATEGORY_ID && jobid == 0 ) { /* error getting msg */
                strcpy(errmsg,  errMsgPtr);
          return(SUBSYS_RETURN_ERROR_ID);
        }
        break;

      case SUBSYS_RETURN_UNRECOGNIZED_ID:       
      default: 
        ok = 0; 
        errmsg = "Unknown return value";
        break;
    } /* switch */
  } /* if ok && jobId ok  && status ok */

  if (!ok) {  /* msg.body has an error, or files did not exist  */

           /* set rev and platform for cleanup message */
    if ( cat == PPS_CATEGORY_ID && jobid == PPS_JOB_REQUEST_ACK_JOB_ID) {
          cbData->rev = 0;
          strcpy(cbData->platform, "");
    }
    else {     /* error for an existing job */
      pos = GetDispPos_MSTRQLIST(jobid);
      if (pos != -1)
        mqel = GetReqGivenListPos_MSTRQLIST(pos);
      if (mqel != NULL) {
        cbData->rev = ODLGetInt(mqel->mstrqODLreq, BODY_REV, &retval);
        if (retval != -1) {
          strcpy(cbData->platform, ODLGetStr(mqel->mstrqODLreq, BODY_PLATFORM));
        }
      }
    }
       
/* if wrong tape, ask operator what to do */
    if (req_status == SUBSYS_RETURN_WRONG_TAPE_ID) {
          cbData->action = SUBSYS_WRONG_TAPE_ID;
          sprintf(errmsg, "Job %d %s", jobid, errMsgPtr); 
          ASFlogMessage_CB(ASF_CP, WP_QUESTION_BOX, cbData, 
                     SUBSYS_STATUS_ERROR, namePtr, errmsg);     
          return(SUBSYS_RETURN_WRONG_TAPE_ID);
    }
    else if (req_status == SUBSYS_RETURN_UNRECOGNIZED_ID) {
          cbData->action = SUBSYS_ERROR_ID;
          if (subType != HEARTBEAT_ID) { /* heartbeat is not assoc w/ a job */

            if (cat == IMS_CATEGORY_ID) { /* cp-ims gets special processing */
              ChangeStatus__MSTRQLIST(jobid, namePtr, Q_M_HANDLING_ERROR_ID);
              SetSendAck_MSTRQLIST(jobid); /* send ack before cp-ims timer */
            }                              /* expires and resends the msg */

            sprintf(errmsg, INVALID_STATUS_MSG, req_str, jobid);
            ASFlogMessage_CB(ASF_CP,WP_QUESTION_BOX, cbData, 
                          SUBSYS_STATUS_ERROR, namePtr, errmsg);     
          }
          else {
            ASFlogMessage_CB(ASF_CP, WP_INFO_BOX, cbData,
              SUBSYS_STATUS_ERROR_CANCEL, namePtr, getSubTypeStr(readInODL),
                errMsgPtr);

          }
          return(SUBSYS_RETURN_UNRECOGNIZED_ID);
    }
    else {
/* for other errors, send cleanup message to every subsys BUT ssp1, ims, pps */
/* also don't send a cleanup message if this is a response to a cleanup msg! */
                  /* have to use the question box here instead of error box */
                  /* because the callbacks are set up for the question box  */
                  /* (the error box has no callbacks to its OK button)      */
                  /* so if i had time, i'd add callbacks to the error box   */
          if (subType == HEARTBEAT_ID) {
            ASFlogMessage_CB(ASF_CP, WP_INFO_BOX, cbData, 
              SUBSYS_STATUS_ERROR_CANCEL, namePtr, getSubTypeStr(readInODL), 
                errMsgPtr);     
          }

          else if (subType == CLEANUP_ID) {
              /* cleanup CB could already have started/completed job deletion */
              /* if job still is on the subsystem queue and is not deleted, 
                 then we can pop up the error box */

           if (GetStatus__MSTRQLIST(namePtr, jobid) != Q_M_ENTRY_DELETED_ID)
              ASFlogMessage_CB(ASF_CP, WP_QUESTION_BOX, cbData, 
                SUBSYS_DONE_ERROR_CANCEL, namePtr, jobid, errMsgPtr);     
           else
             printfLLog(LOG_ERR, CLEANUP_JOB_DELETED, jobid);
          }
          else if (cat == PPS_CATEGORY_ID ) {
            cbData->action = SUBSYS_PPS_ERROR_ID;
            ASFlogMessage_CB(ASF_CP, WP_QUESTION_BOX, cbData, 
              PPS_DONE_ERROR_CANCEL, namePtr, jobid, errMsgPtr);     
          }
          else if (cat == IMS_CATEGORY_ID ) {

            if (isMetaProduct || isRetrievedProduct)
              cbData->action = SUBSYS_ERROR_ID;

            ChangeStatus__MSTRQLIST(jobid, namePtr, Q_M_HANDLING_ERROR_ID);
            SetSendAck_MSTRQLIST(jobid);

            ASFlogMessage_CB(ASF_CP, WP_QUESTION_BOX, cbData, 
              SUBSYS_DONE_ERROR_CANCEL, namePtr, jobid, errMsgPtr);     
          }
          else {
            ASFlogMessage_CB(ASF_CP, WP_CLEANUP_BOX, cbData,
                     SUBSYS_STATUS_ERROR, namePtr, errMsgPtr);    
          }
    }
    sprintf(errmsg, SUBSYS_STATUS_ERROR, namePtr, errMsgPtr);
  } /* end of not-ok processing */

  if (ok)
    doFree(cbData);


  return(ok ? jobid : SUBSYS_RETURN_ERROR_ID);

} /* end checkAndGetRequestStatus.........................*/


/*----------------------------------------------------------
 * NAME:
 *  doRead 
 *
 *
 * DESCRIPTION:
 *  read odl message off the connected socket, check then 
 *  process it.
 *
 * NOTES:
 *  NOTE ntohl is defined as a null macro
 *  on sgi machines
 *
 *
 *---------------------------------------------------------*/
void doRead(void *arg)
{
  int sockfd, jobId, msgSrcIn, msgID, retval, stateVal, status, cat, fileVer;
  int req_status, ok = 0, chk_status, catStep, subType,tstatus;
  static int successiveFailures=0;
  ODL readInODL=NULL;
  char msgSource[MAX_SUBSYS_NAME_LEN], *odlStrPtr, *charPtr;
  char errmsg[MAX_DISPLAY_LEN], *msgTypePtr;
  struct timeval msgTime;
 baseListElemType *nPtr;
#ifdef EXTRA_DEBUG
  int msgCount = 0;
#endif

  sockfd = (int)arg;                           /* do thread initializations */
  bzero(msgSource, sizeof(msgSource) );
  printfLLog(LOG_DEBUG, READ_STARTED,  arg);

  for (;; ) {

    bzero(errmsg, sizeof(errmsg) );
    readInODL = ReadMsgFromClient(sockfd, &status);
    if (readInODL == NULL)  {
      if (status == -1 )  {
        printfLLog(LOG_DEBUG, "%s thread read error: socket %d pid %d",
             GetNameGivenSocket_PIDLIST(sockfd), sockfd, getpid());
        if (++successiveFailures <= MAX_SOCKET_ERROR)
          continue;  /* go start over at the top of for loop */
        else {     /* more than 10 failures: exit this thread */
          printfLLog(LOG_ERR, MAX_SOCKET_ERROR_TEXT);
          printfLLog(LOG_DEBUG, CLOSING_SOCKET_EXIT, sockfd,
                   GetNameGivenSocket_PIDLIST(sockfd), getpid());
          RemoveSocket_PIDLIST(sockfd);
          close(sockfd);
          exit(0);       /* exit this read thread before we fill up syslog */
        }
      }
      else { /* 0 return means subsys shut down, so close connection */
        printfLLog(LOG_DEBUG, CLOSING_SOCKET_EXIT, sockfd,
                   GetNameGivenSocket_PIDLIST(sockfd), getpid());
        RemoveSocket_PIDLIST(sockfd);
        close(sockfd);
        exit(0);          /* exit this read thread */
      }
    }

    successiveFailures = 0;
    charPtr = ODLToStr(readInODL, NULL);
                 /* msgCount is total count of messages rcvd on this socket */
                 /* don't know why we need this... */
#ifdef EXTRA_DEBUG
    printfLLog(LOG_DEBUG, SOCKET_GOT_MSG, sockfd, 
                         GetNameGivenSocket_PIDLIST(sockfd)); 
#endif

#ifdef PRINT_INCOMING
if (charPtr) {
/*  printf("[%d] CP msg received 0x%x\n", getpid(), charPtr); fflush(stdout); */
  printf("CP msg received %s\n", charPtr); fflush(stdout);
}
#endif

    if (charPtr) {                            /* retrieve msg source */
      if (!ODLGetVal(readInODL, strcpy(errmsg,SOURCE), &msgSrcIn)) {
        printfLLog(LOG_ERR, "Error getting source for message"); 
        ODLFree(readInODL);
        continue;  /* go start over at the top of for loop */
      }
                                                /* verify msg source */
      strcpy(msgSource, (char *) msgSrcIn);
      if (GetProcessState_PIDLIST(msgSource) == -1) {
        ASFlogMessage(ASF_CP, WP_ERROR_BOX, RCVD_UNKNOWN_SOURCE, msgSource);
        ODLFree(readInODL);
        continue;  /* go start over at the top of for loop */
      }
    }

                                    /* here we have a valid message source */

    ODLFree(charPtr);

#ifdef READ_LOCK_DEBUG
printf("%d doRead locking READ_LIST\n", getpid()); fflush(stdout);
#endif

  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL) {
#ifdef READ_LOCK_DEBUG
printf("doRead failed locking READ_LIST... continuing\n"); fflush(stdout);
#endif
     continue;
}

#ifdef READ_LOCK_DEBUG
printf("doRead success locking READ_LIST\n"); fflush(stdout);
#endif

#ifdef PRINT_DEBUG
printf("CP to checkAndFindMsgID, msg is from %s\n", msgSource);
#endif
    msgID = checkAndFindMsgID(readInODL, sockfd, status, errmsg, nPtr);

#ifdef PRINT_DEBUG
printf("CP after checkAndFindMsgID to switch, msgid %d\n", msgID);
#endif
    ok = 0;

    switch(msgID) {
      case SUBSYSTEM_READY_ID: 
                                                  /* check process state */
          stateVal = GetProcessState_PIDLIST(msgSource);
          printfLLog(LOG_DEBUG, RCVD_SUBSYS_READY, msgSource, stateVal);

          if (stateVal == SUBSYS_DORMANT_ID || 
              stateVal == SUBSYS_NOT_RUNNING_ID) {

                     /* using the proc name,place the socket number and the
                      * pid associated with the read thread in the pid list
                      * so that we may manipulate it later */
            printfLLog(LOG_INFO, CONNECT_ATTEMPT, msgSource);
            ASFlogMessage(msgSource, QUE_STATUS_LABEL_ID, SYS_STARTED_TEXT);
            if (AddSocketTo_PIDLIST(msgSource,sockfd,getpid())) /* 0=ok */
              sprintf(errmsg, CANNOT_SET_SOCKET,  msgSource, sockfd);


                /****** NOTE:: for the time being, if process is not spawned
                    by cp, add to process list anyway on receipt of message */
            else if (stateVal != SUBSYS_RUNNING_ID) {
#ifdef CHANGE_EXE
              ChangeExeName_PIDLIST(msgSource, "orphan.subsys", getpid());  
              ASFlogMessage(msgSource, QUE_HEARTBEAT_LABEL_ID,
                SUBSYSTEM_HEARTBEAT_LABEL_TEXT);

#else
              AddPidToList_PIDLIST(SUBSYS_CLASS_ID, msgSource, 
                "orphan.subsys", "orphan.logfile", 0, PID_NOT_SET_ID);  
#endif
          }

            ASFlogMessage(ASF_CP, WP_INFO_BOX, CONNECT_ESTABLISHED, msgSource); 


#ifndef CONNECT_INCOMING_READY   /* do continue if we DON'T want to connect */
            ODLFree(readInODL); 
#ifdef READ_LOCK_DEBUG
printf("doRead unlocking READ_LIST\n"); fflush(stdout);
#endif
            unLockNameList_CORE(nPtr);
            continue;  /* go start over at the top of for loop */
#endif
          }
          else if (stateVal == SUBSYS_DIDNT_START_ID) {
            SendStopMsg(msgSource, sockfd, NO_TIMEOUT);
            close(sockfd);
#ifdef READ_LOCK_DEBUG
printf("doRead unlocking READ_LIST\n"); fflush(stdout);
#endif
            unLockNameList_CORE(nPtr);
            exit(0);
          }
          else if (stateVal == SUBSYS_STARTED_ID ) {     /* a new process */
#ifdef EXTRA_DEBUG
            printfLLog(LOG_DEBUG, HANDLING_NEW_PROC, msgSource);
#endif
                     /* using the proc name,place the socket number and the
                      * pid associated with the read thread in the pid list
                      * so that we may manipulate it later */
            if (AddSocketTo_PIDLIST(msgSource,sockfd,getpid()))  /* 0 = ok */
              sprintf(errmsg, CANNOT_SET_SOCKET,  msgSource, sockfd);
            ASFlogMessage(msgSource, QUE_STATUS_LABEL_ID, SYS_STARTED_TEXT);
          }
          else if (stateVal == SUBSYS_HALTING_ID) {
            ASFlogMessage(msgSource, QUE_STATUS_LABEL_ID, SYS_STARTED_TEXT);
            ChangeStatusToInterrupted_MSTRQLIST(msgSource,SUBSYS_HALTING_ID);
            printfLLog(LOG_INFO,SUBSYS_COMPLETED_HALT_TEXT, msgSource );
          }
          else { /* received ready from a subsystem not in our list */
            ASFlogMessage(ASF_CP, WP_ERROR_BOX, RCVD_UNEXPECTED_READY,
                 msgSource);
            ODLFree(readInODL);
#ifdef READ_LOCK_DEBUG
printf("doRead unlocking READ_LIST\n"); fflush(stdout);
#endif
            unLockNameList_CORE(nPtr);
            continue;  /* go start over at the top of for loop */
          }

                             /* mark first ims as ready in the pid list */
/**** start up ims as waiting too.
          if ((GetSubsystemCategoryGivenName(msgSource)) == IMS_CATEGORY_ID 
               && stateVal == SUBSYS_STARTED_ID)
            stateVal = SUBSYS_READY_ID;
          else     /* mark pps and the other subsystems as waiting */

            stateVal = SUBSYS_WAITING_ID;
          if (ChangeProcessState_PIDLIST(msgSource, stateVal, NOT_SET_ID) == -1)
            sprintf(errmsg, CANNOT_CHANGE_TO_RUN, msgSource);
          else 
            ok = 1;

          printfLLog(LOG_INFO, READY_FOR_REQS_TEXT,msgSource);
        break;



      case SUBSYSTEM_ACK_ID: 
        if ((ODLGetVal(readInODL, BODY_JOB_ID, &jobId)) == 0)
          sprintf(errmsg, "Cannot get job id for REQUEST_ACK for %s\n",
                                    msgSource);
        else {

#ifdef PRINT_DEBUG
printf("CP ack case removing timeout got jobid %d\n", jobId);
#endif
          RemoveTimeOut_PIDLIST(msgSource, MSG_SENT_INTERVAL_ID);
#ifdef PRINT_DEBUG
printf("CP ack after removed timeout jobid %d\n", jobId);
#endif

          if (jobId != PPS_JOB_REQUEST_ACK_JOB_ID) { /* pps special jobid=0 */
#ifdef JOB_ORDER
            status = GetStatus__MSTRQLIST(msgSource, jobId);
printf("---------rcvd ack from %s job %d, status was %d\n", msgSource, jobId, status);
#endif
#ifdef PRINT_DEBUG
printf("CP ack after changed mq status\n");
#endif
            if (ChangeStatus__MSTRQLIST(jobId,msgSource,Q_S_RCVD_ACK_ID) == -1)
                printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "ack");
            if (ODLGetVal(readInODL, TIME, &msgTime) == 0)
              printf("Cannot get msg time from %s msg\n", msgSource);
            else {
              setStartTime_MSTRQLIST(jobId, P_LAST, msgTime);
            }
          }
          ok = 1;
        }
        break;

      case SUBSYSTEM_COMPLETED_ID:
                                                     /* subsys message only */
        ok = 1;
        chk_status = checkAndGetRequestStatus(msgSource, readInODL, 
                                              errmsg, &jobId);
#ifdef PRINT_DEBUG
printf("checkAndGetRequestStatus %s returned %d jobid %d errmsg .%s.\n", 
        msgSource, chk_status, jobId, errmsg); fflush(stdout);
#endif
        stateVal = GetProcessState_PIDLIST(msgSource);
        printfLLog(LOG_DEBUG, RCVD_SUBSYS_COMPLETE,jobId,msgSource,stateVal);

        if (stateVal == SUBSYS_RUNNING_ID) /* otherwise is meaningless */
            ChangeProcessState_PIDLIST(msgSource, SUBSYS_READY_ID, NOT_SET_ID);

        cat = GetSubsystemCategoryGivenName(msgSource);
        if (chk_status == SUBSYS_RETURN_UNRECOGNIZED_ID)  {
          SetQCtype_MSTRQLIST(jobId, QC_TYPE_NONE); /* can't go to qc! */
          ChangeProcessState_PIDLIST(msgSource,SUBSYS_WAITING_ID,NOT_SET_ID);
        }
        else if (chk_status == SUBSYS_RETURN_WRONG_TAPE_ID) {
            SetQCtype_MSTRQLIST(jobId, QC_TYPE_NONE); /* can't go to qc! */

            if (ChangeStatus__MSTRQLIST(jobId, msgSource, 
                            Q_S_CHECKING_TAPE_ID) == -1)
              printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "repeat");
        }
        else if (chk_status != SUBSYS_RETURN_ERROR_ID && 
              chk_status != PPS_RETURN_NO_MSGS) {
            if (ChangeStatus__MSTRQLIST(jobId, msgSource, Q_S_DONE_ID) == -1)
              printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "done");
            if (ODLGetVal(readInODL, TIME, &msgTime) == 0)
              printf("Cannot get msg time from %s msg\n", msgSource);
            else {
              setEndTime_MSTRQLIST(jobId, P_LAST, msgTime);
            }
        }
        else if (chk_status == PPS_RETURN_NO_MSGS && cat == PPS_CATEGORY_ID) {
                sprintf(errmsg,  PPS_NO_JOBS_OF_TYPE, getPPSlastMode(),
                                                      getPPSlastType());
                ClearAvail_PPS(getPPSlastRequested());
                printfLLog(LOG_INFO, errmsg);
                ClearMsgOut_PPS(FALSE);  
        }
        else { /* something was wrong with the status, make subsys idle */
 
          printfLLog(LOG_DEBUG, "Processing job error: status %d", chk_status);
          printfLLog(LOG_ERR, "Processing job error: %s", errmsg);
          if (cat == PPS_CATEGORY_ID && jobId == 0) {
            ClearMsgOut_PPS(FALSE);
            ASFlogMessage(ASF_CP, WP_ERROR_BOX, PPS_JOB_GEN_ERROR, 
                     msgSource, errmsg);     
          }
          else {
#ifdef EXTRA_DEBUG
            printfLLog(LOG_DEBUG, "Error with job %d\n", jobId);
#else
   ;
#endif
          }
          ChangeProcessState_PIDLIST(msgSource,SUBSYS_WAITING_ID,NOT_SET_ID);

        }

        break;


      case SUBSYSTEM_STATUS_ID:
        stateVal = GetProcessState_PIDLIST(msgSource);

        subType = getSubTypeID(readInODL);
#ifdef PRINT_DEBUG
printf("doRead: processing SUBSYSTEM_STATUS, state %d subtype %d, chk_status %d\n", stateVal, subType, chk_status);
#endif


        ok = 0;
        chk_status = checkAndGetRequestStatus(msgSource, readInODL, 
             errmsg, &jobId);
#ifdef PRINT_DEBUG
printf("checkAndGetRequestStatus %s returned %d jobid %d errmsg .%s.\n", 
        msgSource, chk_status, jobId, errmsg); fflush(stdout);
#endif
        printfLLog(LOG_DEBUG, RCVD_SUBSYS_STATUS, msgSource, jobId, stateVal);
        if (chk_status < SUBSYS_RETURN_OK_ID ) {  
          ok = 1;  /* don't want second box popped up */

          if (subType == CLEANUP_ID) {
            RemoveTimeOut_PIDLIST(msgSource, CLEANUP_INTERVAL_ID);
            ChangeStatus__MSTRQLIST(jobId, msgSource,
                                    Q_M_PLACED_ON_HOLD_ERROR_ID);
          }
          else if (subType == HEARTBEAT_ID)
            RemoveTimeOut_PIDLIST(msgSource, HEALTH_INTERVAL_ID);
          if (GetSubsystemCategoryGivenName(msgSource) == IMS_CATEGORY_ID) {

 
            ChangeProcessState_PIDLIST(msgSource, SUBSYS_WAITING_ID,NOT_SET_ID);
          }

          break;   /* what the heck is this doing here??? */
                   /* i now see: if cp-ims returns an error, we don't */
                   /* want to process the completion stuff below */
        }
/* if got to here, the message return status was not a problem */

#ifdef JOB_ORDER
            status = GetStatus__MSTRQLIST(msgSource, jobId);
printf("---------rcvd status from %s job %d, status was %d\n", msgSource, jobId, status);
#endif

        switch(subType)  {
            case ASYNC_ID:
              break;            

            case HEARTBEAT_ID:
                odlStrPtr = ODLGetStr(readInODL, strcpy(errmsg, BODY_COMMENT));
                if (odlStrPtr != NULL) {
                  ok = 1;
                  RemoveTimeOut_PIDLIST(msgSource, HEALTH_INTERVAL_ID);
                  ASFlogMessage(msgSource, QUE_HEARTBEAT_LABEL_ID,
                      SUBSYSTEM_REPORTS_STATUS_TEXT, odlStrPtr);
                }

              break;

            case CLEANUP_ID:
                tstatus = GetStatus__MSTRQLIST(msgSource, jobId);
                if (tstatus != Q_M_ENTRY_DELETED_ID)
                   ChangeStatus__MSTRQLIST(jobId, msgSource,
                                    Q_M_PLACED_ON_HOLD_ERROR_ID);
                odlStrPtr = ODLGetStr(readInODL, strcpy(errmsg, BODY_COMMENT));
                if (odlStrPtr != NULL) {
/* "IDLE" returned from subsys puts the subsys in READY state, even
   if it was WAITING before the heartbeat.  anything but "IDLE"
   returned means the subsys is RUNNING
*/
                  ok = 1;
                  RemoveTimeOut_PIDLIST(msgSource, CLEANUP_INTERVAL_ID);
                }

              break;

            case CATALOG_ID:
               /* turn off "running" light since this is a catalog completion */
              ChangeProcessState_PIDLIST(msgSource, SUBSYS_READY_ID,NOT_SET_ID);

              status = GetStatus__MSTRQLIST(msgSource, jobId);
              if (ChangeStatus__MSTRQLIST(jobId, msgSource, Q_S_DONE_ID) == -1)
                  printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "catalog done");

              if (ODLGetVal(readInODL, TIME, &msgTime) == 0)
                printf("Cannot get msg time from %s msg\n", msgSource);
              else {
                setEndTime_MSTRQLIST(jobId, P_LAST, msgTime);
/* ignore the FILE_VERSION keyword unless this is a file version retrieval */
/* we can't count on the CP-IMS returning the proper file version */
                catStep = GetCatalogStep_MSTRQLIST(jobId) ;
                if (catStep == IMS_TYPE_GET_VERSION) {
                  fileVer = ODLGetInt(readInODL, BODY_FILE_VERSION, &retval);
                  if (retval != -1)
                     SetFileVersion_MSTRQLIST(jobId, fileVer);
                }
                else if (catStep == IMS_TYPE_STORE_SCAN_RESULT) {
/* here we need to add the .M file to list of files to be deleted */
                     ;
                }
                ok = 1;
              }
              break;
          };  /* switch */
          break;

      case FRAME_REQUEST_ID:
      case SCAN_REQUEST_ID:
        {
          int dc=0;
          char *dp, reason[MAX_PPS_COMMENT_LENGTH]="", 
                    dispMsg[2*MAX_PPS_COMMENT_LENGTH];

          if (strcmp(msgSource, getPPSname()) == 0) {  /* reset pps timer */
            ClearMsgOut_PPS(FALSE);
            SetAvail_PPS(getPPSlastRequested());
          }
          printfLLog(LOG_DEBUG,CP_RECEIVED_REQ_TEXT, ASF_CP, msgSource);  
          if (!JobExists_MSTRQLIST(jobId = getJobId(readInODL))) {
            printfLLog(LOG_DEBUG, "Performing domain check");
            if ((dc = domainCheck(readInODL, errmsg)) &&
                ((dp = determineProcessor(readInODL, errmsg)) != NULL)  ) {
              if (AddMstrqToList_MSTRQLIST(getPPSname(), MODIFIED, dp,
                        readInODL) !=-1 )
                ok = 1;
            }
            else { /* add to pps queue to send error back! */
              AddMstrqToList_MSTRQLIST(getPPSname(), MODIFIED, "UNDEFINED", 
                        readInODL);
              SetCatalogStep_MSTRQLIST(jobId, IMS_TYPE_DONE);
              if (!dc) /* domain check failed */
                sprintf(reason, DOMAIN_CHECK_FAILED, errmsg);
              else if (dp == NULL)   /* job profile test failed */
                sprintf(reason, errmsg);
                /*sprintf(reason, JOB_PROFILE_FAILED); */
              SetFailureReason_MSTRQLIST(jobId, reason);
              sprintf(dispMsg, RETURNING_JOB, jobId, getPPSname(), reason);
#ifdef POP_JOB_PROFILE
              ASFlogMessage(ASF_CP, WP_ERROR_BOX, dispMsg);
#endif
              ok = 1;
            }
          }
          else            /* pop up error if duplicate job exists */
              ASFlogMessage(ASF_CP, WP_ERROR_BOX, DUP_JOB_EXISTS, jobId);

          if (!ok) {
            printfLLog(LOG_ERR, ERROR_ADDING_MQ_ID, jobId);
            ODLFree(readInODL);
#ifdef READ_LOCK_DEBUG
printf("doRead unlocking READ_LIST\n"); fflush(stdout);
#endif
            unLockNameList_CORE(nPtr);
            continue; /* go back to top of do-forever loop... */
          }
        }
        break;

      default:
       ClearMsgOut_PPS(FALSE);  /* so we can try another message */
       break;

    } /* end msgid switch */
#ifdef PRINT_DEBUG
printf("CP end of switch ok is %d\n", ok);
#endif

    if (!ok) {
      ASFlogMessage(ASF_CP, WP_ERROR_BOX, DEB_ERROR_READING_MSG, errmsg);  
    }


#ifdef READ_LOCK_DEBUG
printf("doRead unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    ODLFree(readInODL);
  } /* end for ever loop */

} /* end doRead............................ */
