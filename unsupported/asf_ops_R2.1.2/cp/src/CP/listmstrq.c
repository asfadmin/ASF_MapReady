
#define MAX_LINE_LENGTH 255 /* random line length in saved file */
#define ALT_TAGS /* uncomment to allow multiple saved-file versions */

/* #define REPROMPT_MOUNT   uncomment to reprompt at top of q for mounts */

/* #define FILES */ 

/* #define CHANGE_STATUS  */
/* #define MSTR  uncomment to enable printfs of debug statements */
/* #define READ_WRITE  uncomment for printfs of read/write saved files */
/* #define PPS_SELECT  for pps job selection debugging */

/*----------------------------------------------------------
 * SCCS Header
 * File:listmstrq.c   Rev:4.2.0.0   Date:95/02/10
 *
 * Tag range 40,000-49,999 
 *---------------------------------------------------------*/

static char sccsid_listmstrq_c[] = "@(#)listmstrq.c	4.119 97/09/29 14:01:25";

#include <stdio.h>
#include <syslog.h>
#include <sys/time.h>

#include <sys/types.h> /* chmod() */
#include <sys/stat.h>  /* chmod() */

#include "asfcommon.h"
#include "odl.h"
#include "logUtils.h" /* printfLLog */
#include "memUtils.h" /* doMalloc */

#include "cpdefines.h"
#include "listcore.h"
#include "listmstrq.h"
#include "cpworkProc.h"
#include "cprtns.h"
#include "cpconfig.h"
#include "cprtns_xwp.h"
#include "cplogs.h"
#include "que_sys.h" 
#include "utils.h" /* readIntStrFromFile, bsort */
#include "pps.h" /* P_xxx */
#include "validate.h" /* SPS_mediaTypes */

int totalMQentries=0;

static char mountStr[600]; /* for returning build mount request label */

extern char *getCurrentStateFileName();
extern void adjustQueueID(mstrqListElemType *mqel,char* namePtr);
extern char GLOBAL_lastRDSMediaPrompted[MAX_RDS][512];
extern char GLOBAL_lastASPMediaPrompted[512];

extern char * readLineStrFromFile(FILE *fp, int len, int *bytes_read);
 
#define ALT_CP_VERSION_TAG "CP 3.5" /* alternate cp version tag */
#define CP_VERSION_TAG "CP 3.5" /* this must be manually changed whenever */
                                /* a major revision is done.  (i.e. if the */
                                /* first or second field is incremented */
                                /* all X.Y.* values are equiv to X.Y */
#define MAX_CP_VERSION_TAG_LEN 80 /* why not give 'em a line length */

/*----------------------------------------------------------
 * NAME:
 *  CreateMstrqList_MSTRQLIST 
 *
 * DESCRIPTION:
 *  create the thread safe master queue list and sema
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int CreateMstrqList_MSTRQLIST()
{
 int retval;

  retval =  createOneNameListEntry_CORE(MSTRQ_LIST);
  return(retval);

} /* CreateMstrqList_MSTRQLIST...................*/

/*----------------------------------------------------------
 * NAME:
 *  BuildMountRequestLabel
 *
 * DESCRIPTION:
 *  builds the format string that will be used when prompting
 *  the user to mount a tape
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * BuildMountRequestLabel(mstrqListElemType *mqel)
{
  char *errmsg, *odlMediaStr;
  int cat,rdsNum;
 
  strcpy(mountStr, "");
/* we know it is a tape, so the label is stored in ID */
  if ( ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MEDIA_ID, &odlMediaStr))
      sprintf(mountStr, PLEASE_MOUNT, odlMediaStr, mqel->jobId);

  cat = GetSubsystemCategoryGivenName(mqel->namePtr);  
  if (cat == RDS_CATEGORY_ID){
    rdsNum = getRDSnumGivenName(mqel ->namePtr);
    strcpy(GLOBAL_lastRDSMediaPrompted[rdsNum] , odlMediaStr);
  }
  else if (cat == ASP_CATEGORY_ID)
    strcpy(GLOBAL_lastASPMediaPrompted , odlMediaStr);
  return(mountStr);
}

/*----------------------------------------------------------
 * NAME:
 *  setMqJobDisplay
 *
 * DESCRIPTION:
 *  builds the format strings that will be used in the master
 *  queue display window job lists.  sets all these values
 *  in the master queue entry passed to this routine.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char *setMqJobDisplay(mstrqListElemType *mqel)
{
 char *odlProductTypeStr="", *odlModeStr="", odlFrameIdStr[10],
      *odlMediaStr="", *req_type_str="", *odlMediaLoc="", *p;
 char *platStr="", platrevseq[15], *errmsg;
 char reqStr[10];
 int err = 0, ok = 0;
 int odlRev, odlSeq, odlFrameId, odlSubFrameId;
 char descBuf[10];

#ifdef MSTR
printf("setMqJobDisplay: name %s\n", mqel->namePtr);
#endif

  if (!ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_PLATFORM, &platStr)) {
    return(errmsg);
  }

    if ( ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MEDIA_ID, &odlMediaStr))
      if ( ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MEDIA_LOC, &odlMediaLoc))
        if (ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MODE, &odlModeStr))
          ok = 1;       /* if ok is still set after all this, continue */

  if (ok) {
    odlRev = ODLGetInt(mqel->mstrqODLreq, errmsg=BODY_REV, &err);
    if (err == -1)
      ok = 0;
  }
  
  if (ok) {
    odlSeq = ODLGetInt(mqel->mstrqODLreq, errmsg=BODY_SEQ, &err);
    if (err == -1)
      ok = 0;
  }

  if (ok) {
    sprintf(platrevseq, "%s/%05d/%02d", platStr, odlRev, odlSeq);
    ok = ODLGetVal(mqel->mstrqODLreq, errmsg=HDR_MSG_TYPE, &req_type_str);
  }

  if (strcmp(req_type_str, "FRAME_REQUEST") == 0) {
    if (ok) {
       if (getModeId(odlModeStr) == MODE_SCANSAR_ID)
         strcpy(reqStr, "DECODE");
       else /* use 'frame' for all other modes */
         strcpy(reqStr, "FRAME");
     }

     if (ok) {
       odlFrameId = ODLGetInt(mqel->mstrqODLreq, errmsg=BODY_FRAME_ID, &err);
       if (err == -1)
         ok = 0;
     }
     if (ok) {                  /* got frame id successfully */
       odlProductTypeStr = ODLGetString(mqel->mstrqODLreq,
            errmsg=BODY_PRODUCT_TYPE, &err);
       if (err == -1)
           ok = 0;
       else {
           if (strcmp(odlProductTypeStr, "COMPLEX") == 0) {
             odlSubFrameId=ODLGetInt(mqel->mstrqODLreq, errmsg=BODY_SUBFRAME_ID,
                                   &err);
           if (err == -1)
             ok = 0;
           else
             sprintf(odlFrameIdStr, "%d/%d", odlFrameId, odlSubFrameId);
           }
       else /* if not asp/complex just display the frame id alone */
         sprintf(odlFrameIdStr, "%d", odlFrameId);

      }
    }
  }
  else if (strcmp(req_type_str, "SCAN_REQUEST") == 0) {
    strcpy(descBuf, "SCAN");
    odlProductTypeStr = (char *) doMalloc(sizeof("SCAN") + 1);
    strcpy(odlProductTypeStr, "SCAN");
    strcpy(odlFrameIdStr, "-----");
    strcpy(reqStr, "SCAN");
  }

  strcpy(descBuf, odlProductTypeStr);

                       /* create the master queue window format */

  strcpy(mqel->mqFmt[MAIN_JOB_ID_LIST], MAIN_JOB_ID_FMT);
  strcpy(mqel->mqFmt[MAIN_PLAT_REV_SEQ_LIST], MAIN_PLAT_REV_SEQ_FMT);
  strcpy(mqel->mqFmt[MAIN_FRAME_LIST], MAIN_FRAME_FMT);
  strcpy(mqel->mqFmt[MAIN_MEDIA_LIST], MAIN_MEDIA_FMT);
  strcpy(mqel->mqFmt[MAIN_MODE_LIST], MAIN_MODE_FMT);
  strcpy(mqel->mqFmt[MAIN_REQ_LIST], MAIN_REQ_FMT);
  strcpy(mqel->mqFmt[MAIN_TYPE_LIST], MAIN_TYPE_FMT);
  strcpy(mqel->mqFmt[MAIN_STATUS_LIST], MAIN_STATUS_FMT);

  sprintf(mqel->mqLabel[MAIN_JOB_ID_LIST], "%d", mqel->jobId);
  strcpy(mqel->mqLabel[MAIN_PLAT_REV_SEQ_LIST], platrevseq);
  strcpy(mqel->mqLabel[MAIN_FRAME_LIST], odlFrameIdStr);
  if (mqel->dataSource == DATA_FROM_TAPE)
    strcpy(mqel->mqLabel[MAIN_MEDIA_LIST], odlMediaStr);
  else
    strcpy(mqel->mqLabel[MAIN_MEDIA_LIST], SPS_mediaTypes[SPS_mediaType_DISK]);
  strcpy(mqel->mqLabel[MAIN_MODE_LIST], odlModeStr);
  strcpy(mqel->mqLabel[MAIN_REQ_LIST], reqStr);
  strcpy(mqel->mqLabel[MAIN_TYPE_LIST], odlProductTypeStr);
  strcpy(mqel->mqLabel[MAIN_STATUS_LIST], 
             p = GetQstatusAsText_MSTRQLIST(mqel->status, mqel->namePtr));
#ifdef DEBUG
printLabels(mqel);
#endif
  doFree(p);
  doFree(odlProductTypeStr);

  return(NULL);
} /* end setMqJobDisplay...............*/




/*----------------------------------------------------------
 * NAME:
 *  AddElemToList_MSTRQLIST
 *
 * DESCRIPTION:
 *  add an element to the master queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int AddElemToList_MSTRQLIST(mstrqListElemType *mqel)
{
 int retval;
 baseListElemType *nPtr;

#ifdef MSTR
printf("AddElemToList_MSTRQLIST, totalMQentries %d \n", totalMQentries);
#endif


  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  printfLLog(LOG_DEBUG, MSTRQ_ADDING, mqel->jobId, totalMQentries);

  /* add the element to the mstrq list */

  retval = InsertNewValueAt_CORE(nPtr, mqel, totalMQentries++);

  ASFlogMessage(ASF_CP, WP_NUM_JOBS_LABEL_ID, "");

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end AddElemToList_MSTRQLIST....................*/

/*----------------------------------------------------------
 * NAME:
 *  AddMstrqToList_MSTRQLIST
 *
 * DESCRIPTION:
 *  create and add an element to the master queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int AddMstrqToList_MSTRQLIST(char *namePtr, int modFlag, char *processor,
                             ODL mstrqODLreq)
{
 mstrqListElemType *mqel;
 int i, retval, pos;
 baseListElemType *nPtr;
 char *insertStr, *dataSource, *errmsg;
 
  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

                                          /* set up the element to add */
  mqel = (mstrqListElemType *)doMalloc(sizeof(mstrqListElemType));
  if (mqel == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
#ifdef MSTR
printf("AddMstrqToList_MSTRQLIST, name %s \n", namePtr);
#endif

                    /* gen up a request number -- use the number of requests */
  mqel->jobId = ODLGetInt(mstrqODLreq, BODY_JOB_ID, &retval);

                                           /* capture the subsystem name */
  mqel->namePtr = (char *)doMalloc(strlen(namePtr) + 1);
  if (mqel->namePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  strcpy(mqel->namePtr, namePtr);


  mqel->imageFileNamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->imageFileNamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->leaderFileNamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->leaderFileNamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->avgFileNamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->avgFileNamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->pmfFileNamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->pmfFileNamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->scanResultsFileNamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->scanResultsFileNamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->SSP2namePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->SSP2namePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  mqel->RDSnamePtr = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->RDSnamePtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  mqel->inputDir = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->inputDir == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  mqel->outputDir = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->outputDir == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  mqel->processor = doMalloc(strlen(processor) + 1);
  if (mqel->processor == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  strcpy(mqel->processor, processor);



  strcpy(mqel->imageFileNamePtr, UNDEFINED); 
  strcpy(mqel->leaderFileNamePtr, UNDEFINED); 
  strcpy(mqel->avgFileNamePtr, UNDEFINED); 
  strcpy(mqel->pmfFileNamePtr, UNDEFINED); 
  strcpy(mqel->scanResultsFileNamePtr, UNDEFINED); 
  strcpy(mqel->SSP2namePtr, "UNDEFINED"); 
  strcpy(mqel->RDSnamePtr, "UNDEFINED"); 
  strcpy(mqel->inputDir, "UNDEFINED"); 
  strcpy(mqel->outputDir, "UNDEFINED"); 
  strcpy(mqel->failureReason, "NONE");

  mqel->retryCount = 0;
  mqel->qcType     = QC_TYPE_NONE;
  mqel->numCalProductsStored = 0;
  mqel->numCalProducts = 0;
  mqel->fileVersion = 0;
  mqel->sendAck = 0;
  mqel->catalogStep = IMS_TYPE_NONE;
  mqel->catalogStepDone = FALSE;
  mqel->modifiedFlag = modFlag;
  mqel->dataReadyFlag = FALSE;

  mqel->lastStartTime.tv_sec = mqel->lastStartTime.tv_usec = 0L;
  mqel->lastEndTime.tv_sec = mqel->lastEndTime.tv_usec = 0L;
  mqel->scanStartTime.tv_sec = mqel->scanStartTime.tv_usec = 0L;
  mqel->scanEndTime.tv_sec = mqel->scanEndTime.tv_usec = 0L;
  mqel->decodeStartTime.tv_sec = mqel->decodeStartTime.tv_usec = 0L;
  mqel->decodeEndTime.tv_sec = mqel->decodeEndTime.tv_usec = 0L;
  mqel->frameStartTime.tv_sec = mqel->frameStartTime.tv_usec = 0L;
  mqel->frameEndTime.tv_sec = mqel->frameEndTime.tv_usec = 0L;
  mqel->qcStartTime.tv_sec = mqel->qcStartTime.tv_usec = 0L;
  mqel->qcEndTime.tv_sec = mqel->qcEndTime.tv_usec = 0L;
  mqel->catalogStartTime.tv_sec = mqel->catalogStartTime.tv_usec = 0L;
  mqel->catalogEndTime.tv_sec = mqel->catalogEndTime.tv_usec = 0L;

  mqel->subsysFiles.numFiles = 0;
  mqel->subsysFiles.numProducts = 0;
  mqel->subsysFiles.numDirs = 0;
  mqel->subsysFiles.jobId = 0;
  strcpy(mqel->subsysFiles.namePtr , "");
  for (i=0; i < MAX_SUBSYSTEM_CREATED_FILES; i++)
    strcpy(mqel->subsysFiles.fileName[i] , "");
  for (i=0; i < MAX_JOB_CREATED_FILES; i++)
    strcpy(mqel->subsysFiles.productName[i] , "");
  for (i=0; i < MAX_JOB_CREATED_DIRS; i++)
    strcpy(mqel->subsysFiles.dirName[i] , "");

#ifdef FILES
showSf_sf("AddMq", &mqel->subsysFiles);
#endif

  mqel->status       = Q_M_INCOMING_ID;

  mqel->mstrqODLreq  = (ODL) ODLcopy(mstrqODLreq);
  if (mqel->mstrqODLreq == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
  }


                                          /* get data source (disk or tape) */
  ODLGetVal(mstrqODLreq, BODY_MEDIA_TYPE, &dataSource);
  if (strcmp(dataSource, SPS_mediaTypes[SPS_mediaType_DISK]) == 0)   {
    mqel->dataSource = DATA_FROM_DISK;
    mqel->dataReadyFlag = TRUE;
  }
  else  /* was "DISK" */
    mqel->dataSource = DATA_FROM_TAPE;

             /* build the strings that will be used in the scrolling windows */
  if (errmsg = setMqJobDisplay(mqel) ) {
    ASFlogMessage(ASF_CP, WP_ERROR_BOX, CP_DISPLAY_SETUP_ERR, errmsg); 
    unLockNameList_CORE(nPtr);
    return(-1);
  }

                     /* insert either at the top or the bottom of the queue */
  pos = nPtr->nsize;
  ODLGetVal(mstrqODLreq, BODY_INSERT_TOP, &insertStr);
  if (strcmp(insertStr, "NO"))  /* if not NO, insert at top position */
    pos = 0;

  printfLLog(LOG_DEBUG, MSTRQ_ADDING, mqel->jobId, pos);

  retval = InsertNewValueAt_CORE(nPtr, mqel, pos); 
  totalMQentries++;

  ASFlogMessage(ASF_CP, WP_NUM_JOBS_LABEL_ID, ""); 

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end AddMstrqToList_MSTRQLIST....................*/



/*----------------------------------------------------------
 * NAME:
 *  FindMstrqInList_MSTRQLIST 
 *
 * DESCRIPTION:
 *  find a particular element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
mstrqListElemType *FindMstrqInList_MSTRQLIST(baseListElemType *nPtr,
                                   ODL mstrqODLreq, int *pos)
{
 mstrqListElemType *mqel;
 int i;
  
  /* find the mstrq in the list */
  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];

    if (mqel == NULL)
      return(NULL);

    if (mstrqODLreq == mqel->mstrqODLreq) {
      *pos = i;
      return(mqel);
    }
  } /* end for */

  return(NULL);
} /* end FindMstrqInList_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetFirstModifiedElem__MSTRQLIST 
 *
 * DESCRIPTION:
 *  get a copy of the first modified element in the master 
 *  queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
mstrqListElemType *GetFirstModifiedElem__MSTRQLIST(int *pos)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel, *retElemPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  *pos = -1;
  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(NULL);
   }  

   if (mqel->modifiedFlag == MODIFIED) {      /* found one */
    *pos = i;
    retElemPtr = doMalloc(sizeof(mstrqListElemType) );   /* create base ptr */
    if (retElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }  
                                                   /* get the request number */
    retElemPtr->jobId   = mqel->jobId;

                                      /* copy the subsystem name ptr */
    retElemPtr->namePtr = doMalloc(strlen(mqel->namePtr)+1);
    if (retElemPtr->namePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }  
    strcpy(retElemPtr->namePtr, mqel->namePtr); 


                                 /* copy the image File Name desc label ptr */
    retElemPtr->imageFileNamePtr = doMalloc(strlen(mqel->imageFileNamePtr)+1);
    if (retElemPtr->imageFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }  

                                 /* copy the leader File Name desc label ptr */
    retElemPtr->leaderFileNamePtr = doMalloc(strlen(mqel->leaderFileNamePtr)+1);
    if (retElemPtr->leaderFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy the avg File Name desc label ptr */
    retElemPtr->avgFileNamePtr = doMalloc(strlen(mqel->avgFileNamePtr)+1);
    if (retElemPtr->avgFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy the pmf File Name desc label ptr */
    retElemPtr->pmfFileNamePtr = doMalloc(strlen(mqel->pmfFileNamePtr)+1);
    if (retElemPtr->pmfFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                          /* copy the scan results File Name desc label ptr */
    retElemPtr->scanResultsFileNamePtr = doMalloc(strlen(mqel->scanResultsFileNamePtr)+1);
    if (retElemPtr->scanResultsFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy the ssp2 name desc label ptr */
    retElemPtr->SSP2namePtr = doMalloc(strlen(mqel->SSP2namePtr)+1);
    if (retElemPtr->SSP2namePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy the RDS name desc label ptr */
    retElemPtr->RDSnamePtr = doMalloc(strlen(mqel->RDSnamePtr)+1);
    if (retElemPtr->RDSnamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy inputDir label ptr */
    retElemPtr->inputDir = doMalloc(strlen(mqel->inputDir)+1);
    if (retElemPtr->inputDir == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy outputDir label ptr */
    retElemPtr->outputDir = doMalloc(strlen(mqel->outputDir)+1);
    if (retElemPtr->outputDir == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
                                 /* copy processor label ptr */
    retElemPtr->processor = doMalloc(strlen(mqel->processor)+1);
    if (retElemPtr->processor == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }

               
    strcpy(retElemPtr->imageFileNamePtr, mqel->imageFileNamePtr); 
    strcpy(retElemPtr->leaderFileNamePtr, mqel->leaderFileNamePtr); 
    strcpy(retElemPtr->avgFileNamePtr, mqel->avgFileNamePtr); 
    strcpy(retElemPtr->pmfFileNamePtr, mqel->pmfFileNamePtr); 
    strcpy(retElemPtr->scanResultsFileNamePtr, mqel->scanResultsFileNamePtr); 
    strcpy(retElemPtr->SSP2namePtr, mqel->SSP2namePtr); 
    strcpy(retElemPtr->RDSnamePtr, mqel->RDSnamePtr); 
    strcpy(retElemPtr->inputDir, mqel->inputDir); 
    strcpy(retElemPtr->outputDir, mqel->outputDir); 
    strcpy(retElemPtr->failureReason, mqel->failureReason); 
    strcpy(retElemPtr->processor, mqel->processor); 

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    strcpy(retElemPtr->mqFmt[i], mqel->mqFmt[i]);
    strcpy(retElemPtr->mqLabel[i], mqel->mqLabel[i]);
  }

    retElemPtr->qcType     = mqel->qcType;
    retElemPtr->retryCount     = mqel->retryCount;
    retElemPtr->numCalProductsStored     = mqel->numCalProductsStored;
    retElemPtr->numCalProducts     = mqel->numCalProducts;
    retElemPtr->fileVersion     = mqel->fileVersion;
    retElemPtr->sendAck     = mqel->sendAck;
    retElemPtr->catalogStep     = mqel->catalogStep;
    retElemPtr->catalogStepDone = mqel->catalogStepDone;
    retElemPtr->modifiedFlag = mqel->modifiedFlag;
    retElemPtr->dataSource = mqel->dataSource;
    retElemPtr->dataReadyFlag    = mqel->dataReadyFlag;
    retElemPtr->status       = mqel->status;
    retElemPtr->mstrqODLreq  = (ODL)ODLcopy(mqel->mstrqODLreq);
    if (retElemPtr->mstrqODLreq == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }  
    mqel->modifiedFlag = UNMODIFIED;
    unLockNameList_CORE(nPtr);
    return(retElemPtr);
   }  
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetFirstModifiedElem__MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  FreeMstrqListElem_MSTRQLIST
 *
 * DESCRIPTION:
 *  do all the calls necessary to free memory associated with
 *  an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void FreeMstrqListElem_MSTRQLIST(mstrqListElemType *mqel)
{
#ifdef MSTR
printf("FreeMstrqListElem_MSTRQLIST: jobid %d\n", mqel->jobId);
#endif
  doFree(mqel->namePtr);
  ODLFree(mqel->mstrqODLreq);
  mqel->mstrqODLreq = NULL;
  doFree(mqel->imageFileNamePtr);
  doFree(mqel->leaderFileNamePtr);
  doFree(mqel->avgFileNamePtr);
  doFree(mqel->pmfFileNamePtr);
  doFree(mqel->scanResultsFileNamePtr);
  doFree(mqel->SSP2namePtr);
  doFree(mqel->RDSnamePtr);
  doFree(mqel->inputDir);
  doFree(mqel->outputDir);
  doFree(mqel->processor);
  doFree(mqel);

}  /* FreeMstrqListElem_MSTRQLIST ..*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveODLreqFromList_MSTRQLIST 
 *
 * DESCRIPTION:
 *  remove and free an element from the master queue list
 *
 * NOTES:
 *  for evaluation
 *
 *---------------------------------------------------------*/
int RemoveODLreqFromList_MSTRQLIST(int elemIndex)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 
  elemIndex--;  /* decrement because value passed is 1..n while
                   mq array indices are 0..n-1 */

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  if (elemIndex >= nPtr->nsize) {
    printfLLog(LOG_DEBUG, MSTRQ_BAD_INDEX, elemIndex);
    unLockNameList_CORE(nPtr);
    return(-1);
  }
  mqel = (mstrqListElemType *)nPtr->arr[elemIndex];
  if (mqel == NULL) {
    printfLLog(LOG_DEBUG, MSTRQ_BAD_INDEX, elemIndex);
    unLockNameList_CORE(nPtr);
    return(-1);
  }
#ifdef MSTR
printf("RemoveODLreqFromList_MSTRQLIST: %s jobid %d list pos %d\n", mqel->namePtr, mqel->jobId, elemIndex);
#endif
  ODLFree(mqel->mstrqODLreq);
  mqel->mstrqODLreq = NULL;

  unLockNameList_CORE(nPtr);
  return(0);

} /* end RemoveODLreqFromList_MSTRQLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveMstrqFromList_MSTRQLIST 
 *
 * DESCRIPTION:
 *  remove and free and element from the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
mstrqListElemType *RemoveMstrqFromList_MSTRQLIST(int elemIndex)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 
  elemIndex--;  /* decrement because value passed is 1..n while
                   mq array indices are 0..n-1 */

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(NULL);
  }

  if (elemIndex >= nPtr->nsize) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }
  mqel = (mstrqListElemType *)nPtr->arr[elemIndex];
  if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }
#ifdef MSTR
printf("RemoveMstrqFromList_MSTRQLIST: jobid %d list pos %d\n", mqel->jobId, elemIndex);
#endif
  mqel = (mstrqListElemType *)RemoveNameListElemAt_CORE(nPtr, 
                        MSTRQ_LIST, elemIndex);


  totalMQentries--;  /* decrement this counter used for restoring mq elements */


  ASFlogMessage(ASF_CP, WP_NUM_JOBS_LABEL_ID, "");  /* update display */

  unLockNameList_CORE(nPtr);
  return(mqel);
} /* end RemoveMstrqFromList_MSTRQLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveAll_MSTRQLIST 
 *
 * DESCRIPTION:
 *  remove and free all elements from the master queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int RemoveAll_MSTRQLIST()
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }

   FreeMstrqListElem_MSTRQLIST(mqel);
  } /* end for loop */
  
  RemoveNameListAll_CORE(nPtr, MSTRQ_LIST);

  unLockNameList_CORE(nPtr);
  return(0);

} /* RemoveAll_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  ChangeSubsys__MSTRQLIST 
 *
 *
 * DESCRIPTION:
 *  change the destination subsystem of an element in the master queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *
 *---------------------------------------------------------*/
int ChangeSubsys__MSTRQLIST(int jobId, char *namePtr)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId) {     

#ifdef MSTR
printf("CP: change mstrq %d destination from %s to %s\n",
      mqel->jobId,  mqel->namePtr, namePtr);
#endif
      mqel->namePtr = namePtr;
      mqel->modifiedFlag = MODIFIED; 

      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* ChangeSubsys__MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  ChangeStatus__MSTRQLIST 
 *
 * DESCRIPTION:
 *  change the status of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ChangeStatus__MSTRQLIST(int jobId, char *namePtr, int status)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i, retval;
 char *stateFileName=NULL;
 FILE *fp;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId) {     
      if (strcmp(namePtr, mqel->namePtr) != 0){ /* time to change subsystems? */
#ifdef CHANGE_STATUS
printf("CS_M: job %d changing subsystem from %s to %s\n", jobId, mqel->namePtr, namePtr);
#endif
        printfLLog(LOG_DEBUG, JOB_QUEUE_CHANGE, jobId, mqel->namePtr, namePtr);
        doFree(mqel->namePtr);
        mqel->namePtr = doMalloc(strlen(namePtr) + 1);
        if (mqel->namePtr == NULL) {
          unLockNameList_CORE(nPtr);
          return(-1);
        } 
        strcpy(mqel->namePtr, namePtr);
      } /* end if strcmp */

#ifdef CHANGE_STATUS
printf("CS_M: pid %d change %s mstrq %d status from %d to %d\n", 
      getpid(), mqel->namePtr, mqel->jobId,  mqel->status, status); fflush(stdout);
#else
#ifdef MSTR
printf("CS_M: change %s mstrq %d status from %d to %d\n", 
      mqel->namePtr, mqel->jobId,  mqel->status, status); 
#endif
#endif /* CHANGE_STATUS */
/*    printfLLog(LOG_DEBUG, JOB_STATUS_CHANGE, jobId, mqel->status, status); */
      mqel->status = status;
      mqel->modifiedFlag = MODIFIED; 

      /* selective write */
      switch(status) {

    case Q_M_SCAN_READY_FOR_QC_ID:
    case Q_M_IMAGE_PRE_QC_ID:
    case Q_S_IMAGE_QC_READY_ID:
    case Q_M_SCAN_COMPLETED_ID:
    case Q_M_CYCLE_COMPLETED_ID:
    case Q_M_ENTRY_DELETED_ID:
    case Q_M_NEXT_STEP_ID:
    case Q_S_DONE_ID:
    case Q_M_IMAGE_QC_DONE_ACCEPT_ID:
    case Q_M_IMAGE_QC_DONE_REJECT_ID:
    case Q_M_INCOMING_ID:
    case Q_M_PLACED_ON_HOLD_DATA_ID:
    case Q_M_PLACED_ON_HOLD_ERROR_ID:
    case Q_M_CLEANING_UP_ID:
    case Q_M_DONE_ERROR_ID:
      writeStateFile();
      break;
    }
      /* ############### */

      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */
  unLockNameList_CORE(nPtr);
  return(-1);
} /* ChangeStatus__MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  ChangeStatusRestrict__MSTRQLIST 
 *
 * DESCRIPTION:
 *  change the status of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ChangeStatusRestrict__MSTRQLIST(int jobId, char *namePtr, int status, 
                                    int currStatus)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId && mqel->status != currStatus) {     
      if (strcmp(namePtr, mqel->namePtr) != 0){ /* time to change subsystems? */
#ifdef MSTR
printf("CSR_M: changing subsystem from %s to %s\n", mqel->namePtr, namePtr);
#endif
        doFree(mqel->namePtr);
        mqel->namePtr = doMalloc(strlen(namePtr) + 1);
        if (mqel->namePtr == NULL) {
          unLockNameList_CORE(nPtr);
          return(-1);
        } 
        strcpy(mqel->namePtr, namePtr);
      } /* end if strcmp */

#ifdef CHANGE_STATUS
printf("CSR_M: pid %d change %s mstrq %d status from %d to %d\n", 
      getpid(), mqel->namePtr, mqel->jobId,  mqel->status, status); fflush(stdout);
#else
#ifdef MSTR
printf("CSR_M: change %s mstrq %d status from %d to %d\n", 
      mqel->namePtr, mqel->jobId,  mqel->status, status); 
#endif
#endif /* CHANGE_STATUS */
      mqel->status = status;
      mqel->modifiedFlag = MODIFIED; 
      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */
  unLockNameList_CORE(nPtr);
  return(-1);
} /* ChangeStatusRestrict__MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  JobExists_MSTRQLIST
 *
 * DESCRIPTION:
 *  returns true if the job specified by jobId exists 
 *  returns false if the job does not exist
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int JobExists_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR
printf("CP: JobExists_MSTRQLIST %d \n", jobId);
#endif
      return(1);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(0);

} /* JobExists_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  IncrementCatalogStep_MSTRQLIST
 *
 * DESCRIPTION:
 *  increment the catalog step for an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int IncrementCatalogStep_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      mqel->catalogStep++;
      mqel->catalogStepDone = FALSE;
#ifdef MSTR
printf("CP: IncrementCatalogStep_MSTRQLIST %d from %d to %d\n",
      mqel->jobId,  mqel->catalogStep-1, mqel->catalogStep);
#endif

      return(0);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* IncrementCatalogStep_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  SetCatalogStep_MSTRQLIST 
 *
 * DESCRIPTION:
 *  change the catalogStep of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetCatalogStep_MSTRQLIST(int jobId, int catalogStep)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId) {     
#ifdef MSTR
printf("CP: set catalog step job %d catalogStep from %d to %d\n", 
      mqel->jobId,  mqel->catalogStep, catalogStep); 
#endif
      mqel->catalogStep = catalogStep;
      mqel->catalogStepDone = FALSE;
      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* SetCatalogStep_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetCatalogStep_MSTRQLIST
 *
 * DESCRIPTION:
 *  change the catalogStep of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetCatalogStep_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
#ifdef MSTR
printf("CP: get catalog step job %d catalogStep %d\n",
      mqel->jobId,  mqel->catalogStep);
#endif
      unLockNameList_CORE(nPtr);
      return(mqel->catalogStep);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetCatalogStep_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  IncrementNumCalProductsStored_MSTRQLIST
 *
 * DESCRIPTION:
 *  change the numCalProducts of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int IncrementNumCalProductsStored_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->numCalProductsStored++;
#ifdef MSTR
printf("CP: increment num pvs products stored job %d numCalProducts to %d\n",
      mqel->jobId,  mqel->numCalProductsStored);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* IncrementNumCalProductsStored_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetNumCalProductsStored_MSTRQLIST
 *
 * DESCRIPTION:
 *  return the numCalProducts of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetNumCalProductsStored_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
#ifdef MSTR
printf("CP: get num pvs products stored job %d numCalProducts %d\n",
      mqel->jobId,  mqel->numCalProductsStored);
#endif
      unLockNameList_CORE(nPtr);
      return(mqel->numCalProductsStored);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetNumCalProductsStored_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  SetNumCalProducts_MSTRQLIST 
 *
 * DESCRIPTION:
 *  change the numCalProducts of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetNumCalProducts_MSTRQLIST(int jobId, int numCalProducts)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId) {     
#ifdef MSTR
printf("CP: set num pvs products job %d numCalProducts from %d to %d\n", 
      mqel->jobId,  mqel->numCalProducts, numCalProducts); 
#endif
      mqel->numCalProducts = numCalProducts;
      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* SetNumCalProducts_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetNumCalProducts_MSTRQLIST
 *
 * DESCRIPTION:
 *  return the numCalProducts of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetNumCalProducts_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
#ifdef MSTR
printf("CP: get num pvs products job %d numCalProducts %d\n",
      mqel->jobId,  mqel->numCalProducts);
#endif
      unLockNameList_CORE(nPtr);
      return(mqel->numCalProducts);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetNumCalProducts_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  SetFileVersion_MSTRQLIST 
 *
 * DESCRIPTION:
 *  change the fileVersion of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetFileVersion_MSTRQLIST(int jobId, int fileVersion)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    } 

    if (mqel->jobId == jobId) {     
#ifdef MSTR
printf("CP: set file version job %d fileVersion from %d to %d\n", 
      mqel->jobId,  mqel->fileVersion, fileVersion); 
#endif
      mqel->fileVersion = fileVersion;
      unLockNameList_CORE(nPtr);
      return(0);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* SetFileVersion_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetFileVersion_MSTRQLIST
 *
 * DESCRIPTION:
 *  return the fileVersion of an element in the master queue list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetFileVersion_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;
#ifdef MSTR
printf("CP: enter get file version job %d \n", jobId);
#endif

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL) {
     return(-1);
  }

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
#ifdef MSTR
printf("CP: get file version job %d fileVersion %d\n",
      mqel->jobId,  mqel->fileVersion);
#endif
      unLockNameList_CORE(nPtr);
      return(mqel->fileVersion);
    } /* end if jobId */

  } /* end for loop */

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetFileVersion_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetReqGivenListPos_MSTRQLIST 
 *
 * DESCRIPTION:
 *   return the master queue element from a specified
 *   position in the master queue
 *
 * NOTES:
 *  VISIBLE
 *
 *
 *---------------------------------------------------------*/
mstrqListElemType * GetReqGivenListPos_MSTRQLIST(int listPos)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  if (listPos > nPtr->nsize || listPos == 0) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

   mqel = (mstrqListElemType *)nPtr->arr[listPos-1];
   if (mqel == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
    }
    unLockNameList_CORE(nPtr);
    return(mqel);

} /* GetReqGivenListPos_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetStatus__MSTRQLIST 
 *
 * DESCRIPTION:
 *  get the status of an element in the master queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *
 *---------------------------------------------------------*/
int GetStatus__MSTRQLIST(char *namePtr, int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      if (strcmp(namePtr, mqel->namePtr) != 0) {    /* does not apply */
        unLockNameList_CORE(nPtr);
        return(-1);
      }
      unLockNameList_CORE(nPtr);
      return(mqel->status);
     }
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* GetStatus__MSTRQLIST.................*/



/*----------------------------------------------------------
 * NAME:
 *  ChangeStatusToInterrupted_MSTRQLIST 
 *
 * DESCRIPTION:
 *  do an 'atomic' change of status for the request currently
 *  being processed on a subsystem for an element in the 
 *  master queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *
 *---------------------------------------------------------*/
int ChangeStatusToInterrupted_MSTRQLIST(char *namePtr, int mode)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i, jobId;
 int status_ID;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  if (mode == SUBSYS_STOPPING_ID)
    status_ID = Q_M_INTERRUPTED_ID;
  else
    status_ID = Q_M_PROCESSING_RESET_ID;

  switch (GetSubsystemCategoryGivenName(namePtr)) {
    case PPS_CATEGORY_ID:
      for (i = 0; i < nPtr->nsize; i++) {
        mqel = (mstrqListElemType *)nPtr->arr[i];
        if ((strcmp(namePtr, mqel->namePtr) == 0) && 
              mqel->status != Q_M_ENTRY_DELETED_ID &&
              mqel->status != Q_M_INCOMING_ID   &&
              mqel->status != Q_S_PLACED_ON_SYSQ_ID &&
              mqel->status != Q_M_NEXT_STEP_ID   ) {
#ifdef CHANGE_STATUS
printf("ChangeStatusToInterrupted_MSTRQLIST %s mode %d status was %d\n", namePtr, mode, mqel->status);
#endif
            mqel->status = status_ID; 
            mqel->modifiedFlag = MODIFIED;
            jobId = mqel->jobId;
            unLockNameList_CORE(nPtr);
            return(jobId);
        } 
      } 
      unLockNameList_CORE(nPtr);
      return(-1);
      break;
    case RDS_CATEGORY_ID:
    case SSP2_CATEGORY_ID:
    case ASP_CATEGORY_ID:
    case IMS_CATEGORY_ID:
      for (i = 0; i < nPtr->nsize; i++) {
        mqel = (mstrqListElemType *)nPtr->arr[i];
          if (strcmp(namePtr, mqel->namePtr) == 0) {
            if (mqel->status != Q_M_IMAGE_READY_FOR_QC_ID && 
                mqel->status != Q_M_IMAGE_PRE_QC_ID   && 
                mqel->status != Q_M_SCAN_READY_FOR_QC_ID   && 
                mqel->status != Q_M_SCAN_COMPLETED_ID   && 
                mqel->status != Q_S_IMAGE_QC_ON_HOLD_ID   && 
                mqel->status != Q_M_IMAGE_QC_ON_HOLD_ID   &&

                mqel->status != Q_S_IMAGE_QC_ACCEPT_ID   &&
                mqel->status != Q_M_IMAGE_QC_DONE_ACCEPT_ID   &&
                mqel->status != Q_S_IMAGE_QC_REJECT_ID   &&
                mqel->status != Q_M_IMAGE_QC_DONE_REJECT_ID   &&

                mqel->status != Q_S_DONE_ID &&
                mqel->status != Q_M_DONE_ERROR_ID   && 
                mqel->status != Q_M_ENTRY_DELETED_ID &&

                mqel->status != Q_M_PLACED_ON_HOLD_DATA_ID   && 
                mqel->status != Q_M_PLACED_ON_HOLD_ERROR_ID &&
                mqel->status != Q_S_PLACED_ON_SYSQ_ID ) {
#ifdef CHANGE_STATUS
printf("ChangeStatusToInterrupted_MSTRQLIST %s mode %d status was %d\n", namePtr, mode, mqel->status);
#endif
              mqel->status = status_ID; 
              mqel->modifiedFlag = MODIFIED;
              jobId = mqel->jobId;
              unLockNameList_CORE(nPtr);
              return(jobId);
            } /* end if status */
          } /* end if strcmp */
      } /* end for */ 
      break;
   default:
    unLockNameList_CORE(nPtr);
    return(-1);
    break;
  } /* end switch */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* ChangeStatusToInterrupted_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  ResetFirstInterruptedStatus_MSTRQLIST 
 *
 * DESCRIPTION:
 *  do an 'atomic' change of status for the last request
 *  that was processed on a subsystem to restart the queue
 *  for an element in the master queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *
 *---------------------------------------------------------*/
int ResetFirstInterruptedStatus_MSTRQLIST(char *namePtr)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i, jobId;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   /*
    * given the name, find the first process with
    * status equal to "processing" and set to interrupted
    */
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (strcmp(namePtr, mqel->namePtr) == 0) {
      if (mqel->status == Q_M_INTERRUPTED_ID) {
#ifdef MSTR
printf("ResetFirstInterruptedStatus_MSTRQLIST %s job %d old status %d\n", namePtr, mqel->jobId, mqel->status);
#endif
        mqel->status = Q_S_GOT_ITEM_OFF_ID;
        mqel->modifiedFlag = MODIFIED;
        jobId = mqel->jobId;
        unLockNameList_CORE(nPtr);
        return(jobId);
      }
    }
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* ResetFirstInterruptedStatus_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetFirstElemWithStatus_MSTRQLIST 
 *
 * DESCRIPTION:
 *  get the status of an element in the master queue list
 *
 * NOTES:
 *  VISIBLE
 *  - may not be used
 *
 *---------------------------------------------------------*/
int GetFirstElemWithStatus_MSTRQLIST(char *namePtr, int status)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (strcmp(namePtr, mqel->namePtr) == 0) {
    if (mqel->status == status) {
#ifdef MSTR
printf("GetFirstElemWithStatus_MSTRQLIST returning job %d for status %d\n", mqel->jobId, status);
#endif
      unLockNameList_CORE(nPtr);
      return(mqel->jobId);
    }
   }
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* GetFirstElemWithStatus_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetDispPos_MSTRQLIST 
 *
 * DESCRIPTION:
 *  get the current display position in the master queue
 *  for the master queue element with the specified job id
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int GetDispPos_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    printfLLog(LOG_DEBUG, NOT_IN_MSTRQ);
    unLockNameList_CORE(nPtr);
    return(-1);
   } /* end null pointer */

   if (mqel->jobId == jobId) {
    unLockNameList_CORE(nPtr);
    return(i+1);
   }
  } /* end for */

  printfLLog(LOG_DEBUG, NOT_IN_MSTRQ);
  unLockNameList_CORE(nPtr);
  return(-1);

} /* GetDispPos_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  MoveElemPrivate_MSTRQLIST 
 *
 * DESCRIPTION:
 *  a private routine that moves an element in the master 
 *  queue list to a new position
 *
 * NOTES:
 *  NOT-VISIBLE:: this routine is locked
 *                before it is entered
 *
 *
 *---------------------------------------------------------*/
int MoveElemPrivate_MSTRQLIST(baseListElemType *nPtr,int elemTo,int elemFrom )
{
 mstrqListElemType *mqel;
 int j, retval;

  for (j = 0; j < nPtr->nsize; j ++) {
    mqel = (mstrqListElemType *)nPtr->arr[j];
/*    printf("unmoved  num = %d\n", mqel->jobId); */
  }
  retval = MoveToPos_CORE(nPtr, elemTo, elemFrom);
 
  for (j = 0; j < nPtr->nsize; j ++) {
    mqel = (mstrqListElemType *)nPtr->arr[j];
/*     printf("moved  num = %d\n", mqel->jobId); */
  }

  return(retval);

} /* end MoveElemPrivate_MSTRQLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  MoveElem_MSTRQLIST 
 *
 * DESCRIPTION:
 *  the public routine that moves an element in the master 
 *  queue list
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int MoveElem_MSTRQLIST(int elemTo, int elemFrom )
{
 baseListElemType *nPtr;
 int retval;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  retval = MoveElemPrivate_MSTRQLIST(nPtr,elemTo, elemFrom);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveElem_MSTRQLIST */



/*----------------------------------------------------------
 * NAME:
 *  MoveToHead_MSTRQLIST 
 *
 * DESCRIPTION:
 *  a public routine that moves an element to the head of
 *  the master queue list 
 *
 * NOTES:
 *  lock is performed in MoveElem_ 
 *
 *---------------------------------------------------------*/
int MoveToHead_MSTRQLIST(int elemToMove )
{
 int retval;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  retval = MoveElemPrivate_MSTRQLIST(nPtr,0, elemToMove);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveToHead_MSTRQLIST..............*/


/*----------------------------------------------------------
 * NAME:
 *  MoveToTail_MSTRQLIST 
 *
 * DESCRIPTION:
 *  a public routine that moves an element to the tail of the
 *  master queue list 
 *
 * NOTES:
 *  lock is performed in MoveElem_ 
 *
 *
 *---------------------------------------------------------*/
int MoveToTail_MSTRQLIST(int elemToMove )
{
 int retval;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);
  
  retval = MoveElemPrivate_MSTRQLIST(nPtr, nPtr->nsize-1, elemToMove);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveToTail_MSTRQLIST..............*/


/*----------------------------------------------------------
 * NAME:
 *  GetCopyGivenJobId__MSTRQLIST 
 *
 * DESCRIPTION:
 *  get a copy of master queue element with the specified job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
mstrqListElemType *GetCopyGivenJobId__MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel, *retElemPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }  

    if (mqel->jobId == jobId) {
                                /* found one create the base ptr */
      retElemPtr = doMalloc(sizeof(mstrqListElemType) );
      if (retElemPtr == NULL) {
        unLockNameList_CORE(nPtr);
        return(NULL);
      }  

                                /* get the request number */
    retElemPtr->jobId   = mqel->jobId;

                                /* copy the subsystem name ptr */
    retElemPtr->namePtr = doMalloc(strlen(mqel->namePtr)+1);
    if (retElemPtr->namePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }  
    strcpy(retElemPtr->namePtr, mqel->namePtr); 

                                /* copy the image File Name desc label ptr */
    retElemPtr->imageFileNamePtr = doMalloc(strlen(mqel->imageFileNamePtr)+1);
    if (retElemPtr->imageFileNamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }  

                                /* copy the leader File Name desc label ptr */
    retElemPtr->leaderFileNamePtr = doMalloc(strlen(mqel->leaderFileNamePtr)+1);
    if (retElemPtr->leaderFileNamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the avg File Name desc label ptr */
    retElemPtr->avgFileNamePtr = doMalloc(strlen(mqel->avgFileNamePtr)+1);
    if (retElemPtr->avgFileNamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the pmf File Name desc label ptr */
    retElemPtr->pmfFileNamePtr = doMalloc(strlen(mqel->pmfFileNamePtr)+1);
    if (retElemPtr->pmfFileNamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                          /* copy the scan results File Name desc label ptr */
    retElemPtr->scanResultsFileNamePtr = 
                     doMalloc(strlen(mqel->scanResultsFileNamePtr)+1);
    if (retElemPtr->scanResultsFileNamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the SSP2namePtr label ptr */
    retElemPtr->SSP2namePtr = doMalloc(strlen(mqel->SSP2namePtr)+1);
    if (retElemPtr->SSP2namePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                   
                                /* copy the RDSnamePtr label ptr */
    retElemPtr->RDSnamePtr = doMalloc(strlen(mqel->RDSnamePtr)+1);
    if (retElemPtr->RDSnamePtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the inputDir label ptr */
    retElemPtr->inputDir = doMalloc(strlen(mqel->inputDir)+1);
    if (retElemPtr->inputDir == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the outputDir label ptr */
    retElemPtr->outputDir = doMalloc(strlen(mqel->outputDir)+1);
    if (retElemPtr->outputDir == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                                /* copy the processor label ptr */
    retElemPtr->processor = doMalloc(strlen(mqel->processor)+1);
    if (retElemPtr->processor == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
                   

    strcpy(retElemPtr->imageFileNamePtr, mqel->imageFileNamePtr); 
    strcpy(retElemPtr->leaderFileNamePtr, mqel->leaderFileNamePtr); 
    strcpy(retElemPtr->avgFileNamePtr, mqel->avgFileNamePtr); 
    strcpy(retElemPtr->pmfFileNamePtr, mqel->pmfFileNamePtr); 
    strcpy(retElemPtr->scanResultsFileNamePtr, mqel->scanResultsFileNamePtr); 
    strcpy(retElemPtr->SSP2namePtr, mqel->SSP2namePtr); 
    strcpy(retElemPtr->RDSnamePtr, mqel->RDSnamePtr); 
    strcpy(retElemPtr->inputDir, mqel->inputDir); 
    strcpy(retElemPtr->outputDir, mqel->outputDir); 
    strcpy(retElemPtr->failureReason, mqel->failureReason); 
    strcpy(retElemPtr->processor, mqel->processor); 


  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    strcpy(retElemPtr->mqFmt[i], mqel->mqFmt[i]);
    strcpy(retElemPtr->mqLabel[i], mqel->mqLabel[i]);
  }

    retElemPtr->qcType     = mqel->qcType;
    retElemPtr->retryCount     = mqel->retryCount;
    retElemPtr->numCalProductsStored     = mqel->numCalProductsStored;
    retElemPtr->numCalProducts     = mqel->numCalProducts;
    retElemPtr->fileVersion     = mqel->fileVersion;
    retElemPtr->sendAck     = mqel->sendAck;
    retElemPtr->catalogStep     = mqel->catalogStep;
    retElemPtr->catalogStepDone = mqel->catalogStepDone;
    retElemPtr->dataReadyFlag = mqel->dataReadyFlag;
    retElemPtr->dataSource = mqel->dataSource;
    retElemPtr->modifiedFlag = mqel->modifiedFlag;
    retElemPtr->status       = mqel->status;
    retElemPtr->mstrqODLreq  = (ODL)ODLcopy(mqel->mstrqODLreq);
    if (retElemPtr->mstrqODLreq == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }  
    mqel->modifiedFlag = UNMODIFIED;
    unLockNameList_CORE(nPtr);
    return(retElemPtr);
   }  
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetCopyGivenJobId__MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetProcessor_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetProcessor_MSTRQLIST(int jobId, char *processor)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->processor = (char *)doMalloc(strlen(processor) + 1);
      if (mqel->processor == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
printf("SetProcessor_MSTRQLIST: job %d to %s\n", jobId, processor);
      strcpy(mqel->processor, processor);
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetProcessor_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetProcessor_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetProcessor_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR 
printf("GetProcessor_MSTRQLIST returning %s\n", mqel->processor);
#endif 
      return(mqel->processor);
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetProcessor_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetInputDir_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetInputDir_MSTRQLIST(int jobId, char *inputDir)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->inputDir = (char *)doMalloc(strlen(inputDir) + 1);
      if (mqel->inputDir == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
#ifdef MSTR
printf("SetInputDir_MSTRQLIST: job %d to %s\n", jobId, inputDir);
#endif
      strcpy(mqel->inputDir, inputDir);
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetInputDir_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetInputDir_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetInputDir_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR
printf("GetInputDir_MSTRQLIST returning %s\n", mqel->inputDir);
#endif
      return(mqel->inputDir);
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetInputDir_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetOutputDir_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetOutputDir_MSTRQLIST(int jobId, char *outputDir)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->outputDir = (char *)doMalloc(strlen(outputDir) + 1);
      if (mqel->outputDir == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
#ifdef MSTR
printf("SetOutputDir_MSTRQLIST: job %d to %s\n", jobId, outputDir);
#endif

      strcpy(mqel->outputDir, outputDir);
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetOutputDir_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetOutputDir_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetOutputDir_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR
printf("GetOutputDir_MSTRQLIST returning %s\n", mqel->outputDir);
#endif
      return(mqel->outputDir);
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetOutputDir_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetSSPname_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetSSPname_MSTRQLIST(int jobId, char *SSP2namePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->SSP2namePtr = (char *)doMalloc(strlen(SSP2namePtr) + 1);
      if (mqel->SSP2namePtr == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
      strcpy(mqel->SSP2namePtr, SSP2namePtr);
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetSSPname_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetSSPname_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the SSP2 name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetSSPname_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR
printf("GetSSPname_MSTRQLIST returning %s\n", mqel->SSP2namePtr);
#endif
      return(mqel->SSP2namePtr);
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetSSPname_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetRDSname_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the RDS name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetRDSname_MSTRQLIST(int jobId, char *RDSnamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->RDSnamePtr = (char *)doMalloc(strlen(RDSnamePtr) + 1);
      if (mqel->RDSnamePtr == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
      strcpy(mqel->RDSnamePtr, RDSnamePtr);
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetRDSname_MSTRQLIST...........*/
/*----------------------------------------------------------
 * NAME:
 *  GetRDSname_MSTRQLIST
 *
 * DESCRIPTION:
 *   get the RDS name string in the master queue element
 *   specified by the given job id 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetRDSname_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
printf("GetRDSname_MSTRQLIST returning %s\n", mqel->RDSnamePtr);
      return(mqel->RDSnamePtr);
    } 
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetRDSname_MSTRQLIST...........*/




/*----------------------------------------------------------
 * NAME:
 *  GetProductType_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the ProductType_MSTRQLIST name string in the master queue element
 *   specified by the given job id and name string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetProductType_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 char *productType;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      productType = ODLGetStr(mqel->mstrqODLreq, BODY_PRODUCT_TYPE);
      if (productType == NULL) {
        printfLLog(LOG_ERR, CANNOT_GET, "product type");
        return(NULL);
      }
      else {
#ifdef MSTR
printf("GetProductType_MSTRQLIST returning %s\n", productType);
#endif
        return(productType); /* productType must be freed by caller */
      }
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetProductType_MSTRQLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  GetCompensationType_MSTRQLIST
 *
 * DESCRIPTION:
 *   return the "YES" or "NO" value of the compensation flag 
 *   for the master queue element specified by the given job id 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetCompensationType_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 char *compensation;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      compensation = ODLGetStr(mqel->mstrqODLreq, BODY_COMPENSATION_FLAG);
      if (compensation == NULL) {
        printfLLog(LOG_ERR, CANNOT_GET, "compensation type");
        return(NULL);
      }
      else {
#ifdef MSTR
printf("GetCompensationType_MSTRQLIST returning %s\n", compensation);
#endif
        return(compensation); /* productType must be freed by caller */
      }
    }
  }
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetCompensationType_MSTRQLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  getStartTime_MSTRQLIST
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getStartTime_MSTRQLIST(int jobId, ppsStatusType pType, struct timeval *retTime)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;
 struct timeval *t;
 char buf[TIME_STRING_SIZE];

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(0);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      if (pType == P_LAST)
        t = &mqel->lastStartTime ;
      else  if (pType == P_SCAN)
        t = &mqel->scanStartTime ;
      else  if (pType == P_DECODE)
        t = &mqel->decodeStartTime ;
      else  if (pType == P_FRAME)
        t = &mqel->frameStartTime ;
      else  if (pType == P_QC)
        t = &mqel->qcStartTime ;
      else  if (pType == P_CATALOG)
        t = &mqel->catalogStartTime ;

      retTime->tv_sec = t->tv_sec;
      retTime->tv_usec = t->tv_usec;
     timeval_to_string(buf, t);
#ifdef MSTR
printf("getStartTime_MSTRQLIST pType %d set %s\n", pType , buf);
#endif

      return(1);
    }
  }
  unLockNameList_CORE(nPtr);
  return(0);

} /* end getStartTime_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  getEndTime_MSTRQLIST
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int getEndTime_MSTRQLIST(int jobId, ppsStatusType pType, struct timeval *retTime)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;
 struct timeval *t;
 char buf[TIME_STRING_SIZE];

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(0);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      if (pType == P_LAST)
        t = &mqel->lastEndTime ;
      else  if (pType == P_SCAN)
        t = &mqel->scanEndTime ;
      else  if (pType == P_DECODE)
        t = &mqel->decodeEndTime ;
      else  if (pType == P_FRAME)
        t = &mqel->frameEndTime ;
      else  if (pType == P_QC)
        t = &mqel->qcEndTime ;
      else  if (pType == P_CATALOG)
        t = &mqel->catalogEndTime ;

      retTime->tv_sec = t->tv_sec;
      retTime->tv_usec = t->tv_usec;
     timeval_to_string(buf, t);
#ifdef MSTR
printf("getEndTime_MSTRQLIST pType %d set %s\n", pType , buf);
#endif
      return(1);
    }
  }
  unLockNameList_CORE(nPtr);
  return(0);

} /* end getEndTime_MSTRQLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  setEndTime_MSTRQLIST
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int setEndTime_MSTRQLIST(int jobId, ppsStatusType pType, struct timeval endTime)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;
 struct timeval *t;
#ifdef MSTR
 char buf[TIME_STRING_SIZE];
#endif

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(0);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      if (pType == P_LAST) 
        t = &mqel->lastEndTime ;
      else  if (pType == P_SCAN) 
        t = &mqel->scanEndTime ;
      else  if (pType == P_DECODE) 
        t = &mqel->decodeEndTime ;
      else  if (pType == P_FRAME) 
        t = &mqel->frameEndTime ;
      else  if (pType == P_QC) 
        t = &mqel->qcEndTime ;
      else  if (pType == P_CATALOG) 
        t = &mqel->catalogEndTime ;
      *t = endTime;
#ifdef MSTR
     timeval_to_string(buf, t);
printf("setEndTime_MSTRQLIST pType %d set %s\n", pType , buf);
#endif
      return(1);
    }
  }
  unLockNameList_CORE(nPtr);
  return(0);

} /* end setEndTime_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  setStartTime_MSTRQLIST
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int setStartTime_MSTRQLIST(int jobId, ppsStatusType pType, struct timeval ackTime)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;
 struct timeval *t;
#ifdef MSTR
 char buf[TIME_STRING_SIZE];
#endif

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(0);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      if (pType == P_LAST)
        t = &mqel->lastStartTime ;
      else  if (pType == P_SCAN)
        t = &mqel->scanStartTime ;
      else  if (pType == P_DECODE)
        t = &mqel->decodeStartTime ;
      else  if (pType == P_FRAME)
        t = &mqel->frameStartTime ;
      else  if (pType == P_QC)
        t = &mqel->qcStartTime ;
      else  if (pType == P_CATALOG)
        t = &mqel->catalogStartTime ;
      *t = ackTime; 
#ifdef MSTR
     timeval_to_string(buf, t);
printf("setStartTime_MSTRQLIST pType %d set %s\n", pType , buf);
#endif 
      return(1);
    }
  }
  unLockNameList_CORE(nPtr);
  return(0);

} /* end setStartTime_MSTRQLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  GetSendAck_MSTRQLIST 
 *
 * DESCRIPTION:
 *   return sendAck flag status
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetSendAck_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }  

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      return(mqel->sendAck);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetSendAck_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetSendAck_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set sendAck flag status
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetSendAck_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }  

    if (mqel->jobId == jobId) {
      mqel->sendAck = TRUE;
      unLockNameList_CORE(nPtr);
      return(0);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetSendAck_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  ClearSendAck_MSTRQLIST
 *
 * DESCRIPTION:
 *   clear sendAck flag status
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearSendAck_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->sendAck = FALSE;
      unLockNameList_CORE(nPtr);
      return(0);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ClearSendAck_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetDataReady_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set the data ready flag in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetDataReady_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }  

    if (mqel->jobId == jobId) {
      mqel->dataReadyFlag = TRUE;
      unLockNameList_CORE(nPtr);
      return(0);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetDataReady_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  ClearDataReady_MSTRQLIST
 *
 * DESCRIPTION:
 *   clear the data ready flag in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearDataReady_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      mqel->dataReadyFlag = FALSE;
      unLockNameList_CORE(nPtr);
      return(0);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ClearDataReady_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetRequestLabel_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the request label string in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetRequestLabel_MSTRQLIST(int jobId, int labelNo, char *string)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }

   if (mqel->jobId == jobId) {
    strcpy(mqel->mqLabel[labelNo], string);
    unLockNameList_CORE(nPtr);
    return(0);
   } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

}  /* SetRequestLabel_MSTRQLIST */

/*----------------------------------------------------------
 * NAME:
 *  GetRetry_MSTRQLIST
 *
 * DESCRIPTION:
 *  returns the retry count for an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int GetRetry_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
printfLLog(LOG_DEBUG, "GetRetry_MSTRQLIST: job %d count %d\n", jobId, mqel->retryCount);
      return(mqel->retryCount);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* GetRetry_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  ClearRetry_MSTRQLIST
 *
 * DESCRIPTION:
 *  increment the retry count for an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int ClearRetry_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      mqel->retryCount=0;
printfLLog(LOG_DEBUG, "ClearRetry_MSTRQLIST: job %d new count %d\n", jobId, mqel->retryCount);
      return(0);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* ClearRetry_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  IncrementRetry_MSTRQLIST
 *
 * DESCRIPTION:
 *  increment the retry count for an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int IncrementRetry_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      mqel->retryCount++;
printfLLog(LOG_DEBUG, "IncrementRetry_MSTRQLIST: job %d new count %d\n", jobId, mqel->retryCount);
      return(0);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* IncrementRetry_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  DecrementRetry_MSTRQLIST
 *
 * DESCRIPTION:
 *  increment the retry count for an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int DecrementRetry_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      mqel->retryCount--;
printfLLog(LOG_DEBUG, "DecrementRetry_MSTRQLIST: job %d new count %d\n", jobId, mqel->retryCount);
      return(0);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* DecrementRetry_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  SetQCtype_MSTRQLIST
 *
 * DESCRIPTION:
 *  set the qc type (scan/image) of an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int SetQCtype_MSTRQLIST(int jobId, int qcType)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      mqel->qcType = qcType;
#ifdef MSTR
printf("*******************SetQCtype_MSTRQLIST for job %d setting %d\n", jobId, mqel->qcType);
#endif
      return(0);
    }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* SetQCtype_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  GetQCtype_MSTRQLIST
 *
 * DESCRIPTION:
 *  get the qc type (scan/image) of an element in the master queue list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int GetQCtype_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef MSTR
printf("*******************GetQCtype_MSTRQLIST for job %d returning %d\n", jobId, mqel->qcType);
#endif
      return(mqel->qcType);
     }
  } 

  unLockNameList_CORE(nPtr);
  return(-1);

} /* GetQCtype_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  SetScanResultsFileName_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set the scan results file name in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetScanResultsFileName_MSTRQLIST(int jobId, char *scanResultsFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }  

   if (mqel->jobId == jobId) {
    if (mqel->scanResultsFileNamePtr != NULL)
          doFree(mqel->scanResultsFileNamePtr);

    mqel->scanResultsFileNamePtr = doMalloc(strlen(scanResultsFileNamePtr) + 1);
    if (mqel->scanResultsFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
    }
    strcpy(mqel->scanResultsFileNamePtr, scanResultsFileNamePtr);
    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */  
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetScanResultsFileName_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetScanResultsFileName_MSTRQLIST 
 *
 * DESCRIPTION:
 *   get the scan result file name in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetScanResultsFileName_MSTRQLIST(int jobId, char **scanResultsFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }  

   if (mqel->jobId == jobId) {
    *scanResultsFileNamePtr = mqel->scanResultsFileNamePtr;
    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */  
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetScanResultsFileName_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetQCFileNames_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set the leader and image file names in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetQCFileNames_MSTRQLIST(int jobId, char *leaderFileNamePtr, 
          char *imageFileNamePtr, char *avgFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

#ifdef MSTR
printf("SetQCFileNames_MSTRQLIST: job %d leader %s image %s avg %s\n", jobId,
leaderFileNamePtr, imageFileNamePtr, avgFileNamePtr);
#endif
  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }  

   if (mqel->jobId == jobId) {
    if (mqel->avgFileNamePtr != NULL)
          doFree(mqel->avgFileNamePtr);
    if (mqel->leaderFileNamePtr != NULL)
          doFree(mqel->leaderFileNamePtr);

    if (mqel->imageFileNamePtr != NULL)
          doFree(mqel->imageFileNamePtr);

    mqel->leaderFileNamePtr = doMalloc(strlen(leaderFileNamePtr) + 1);
    if (mqel->leaderFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
    }
    mqel->imageFileNamePtr = doMalloc(strlen(imageFileNamePtr) + 1);
    if (mqel->imageFileNamePtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
    }
    strcpy(mqel->leaderFileNamePtr, leaderFileNamePtr);
    strcpy(mqel->imageFileNamePtr, imageFileNamePtr);

/* this part is kind of optional - if the avgfile passed is NULL just exit */
    if (avgFileNamePtr != NULL ) {
      mqel->avgFileNamePtr = doMalloc(strlen(avgFileNamePtr) + 1);
      if (mqel->avgFileNamePtr == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
    }
      strcpy(mqel->avgFileNamePtr, avgFileNamePtr);
    }

    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */  
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetQCFileNames_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetQCFileNames_MSTRQLIST
 *
 * DESCRIPTION:
 *   get the leader and image file names in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetQCFileNames_MSTRQLIST(int jobId, char **leaderFileNamePtr,
      char **imageFileNamePtr, char **avgFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }

   if (mqel->jobId == jobId) {
    *leaderFileNamePtr = mqel->leaderFileNamePtr;
    *imageFileNamePtr  = mqel->imageFileNamePtr;
    *avgFileNamePtr  = mqel->avgFileNamePtr;
#ifdef MSTR
printf("GetQCFileNames_MSTRQLIST: job %d leader %s image %s avg %s\n", jobId,
mqel->leaderFileNamePtr, mqel->imageFileNamePtr, mqel->avgFileNamePtr);
#endif
    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetQCFileNames_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  SetFailureReason_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set the failure reason in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetFailureReason_MSTRQLIST(int jobId, char *reason)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

#ifdef MSTR
printf("SetFailureReason_MSTRQLIST: job %d failure reason %s\n", jobId, reason);
#endif
  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }  

    if (mqel->jobId == jobId) {
      printfLLog(LOG_INFO, PPS_FAILURE_LOG, mqel->jobId, reason);
      strcpy(mqel->failureReason, reason);
      unLockNameList_CORE(nPtr);
      return(0);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetFailureReason_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetFailureReason_MSTRQLIST
 *
 * DESCRIPTION:
 *   set the failure reason in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetFailureReason_MSTRQLIST(int jobId)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

#ifdef MSTR
printf("GetFailureReason_MSTRQLIST: job %d \n", jobId);
#endif
  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

    if (mqel->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      return(mqel->failureReason);
    } 
  } 
  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetFailureReason_MSTRQLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  SetPMFfileName_MSTRQLIST 
 *
 * DESCRIPTION:
 *   set the pmf file names in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetPMFfileName_MSTRQLIST(int jobId, char *pmfFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

#ifdef MSTR
printf("SetPMFfileName_MSTRQLIST: job %d pmf %s\n", jobId, pmfFileNamePtr);
#endif
  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }  

   if (mqel->jobId == jobId) {
    if (mqel->pmfFileNamePtr != NULL)
          doFree(mqel->pmfFileNamePtr);

/* this part is kind of optional - if the pmffile passed is NULL just exit */
    if (pmfFileNamePtr != NULL ) {
      mqel->pmfFileNamePtr = doMalloc(strlen(pmfFileNamePtr) + 1);
      if (mqel->pmfFileNamePtr == NULL) {
       unLockNameList_CORE(nPtr);
       return(-1);
      }
      strcpy(mqel->pmfFileNamePtr, pmfFileNamePtr);
    }

    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */  
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetPMFfileName_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetPMFfileName_MSTRQLIST
 *
 * DESCRIPTION:
 *   get the pmf file names in the master queue element
 *   specified by the given job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetPMFfileName_MSTRQLIST(int jobId, char **pmfFileNamePtr)
{
 baseListElemType *nPtr;
 mstrqListElemType *mqel;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   mqel = (mstrqListElemType *)nPtr->arr[i];
   if (mqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }

   if (mqel->jobId == jobId) {
    *pmfFileNamePtr = mqel->pmfFileNamePtr;
    unLockNameList_CORE(nPtr);
    return(0);
   } /* end if */
  } /* end for */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetPMFfileName_MSTRQLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  ReadElem_MSTRQLIST 
 *
 * DESCRIPTION:
 *  reads an element from the master queue list stored in a disk file
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
mstrqListElemType *ReadElem_MSTRQLIST(FILE *fp, int *err, int *insert, int *hold)
{
 mstrqListElemType *mqel;
 int i, cat, len, trueSize;
 int elemSize, namePtrSize, odlSize, avgFileNamePtrSize, pmfFileNamePtrSize,
     imageFileNamePtrSize, leaderFileNamePtrSize, scanResultsFileNamePtrSize,
     SSP2namePtrSize, inputDirSize, 
     failureReasonSize , processorNameSize;
 char c, *p, *odlPartPtr, str[120];
 char scanStartTime[TIME_STRING_SIZE];
 char scanEndTime[TIME_STRING_SIZE];
 char  decodeStartTime[TIME_STRING_SIZE];
 char  decodeEndTime[TIME_STRING_SIZE];
 char   frameStartTime[TIME_STRING_SIZE];
 char   frameEndTime[TIME_STRING_SIZE];
 char      qcStartTime[TIME_STRING_SIZE];
 char      qcEndTime[TIME_STRING_SIZE];
 char catalogStartTime[TIME_STRING_SIZE];
 char catalogEndTime[TIME_STRING_SIZE];

#ifdef READ_WRITE
printf("ReadElem ...\n");
#endif

  *insert = *hold = FALSE;


  mqel =  (mstrqListElemType *) doMalloc(sizeof(mstrqListElemType));

  if ((elemSize = readIntStrFromFile(fp, str, 10)) == 0)
    return(NULL);
#ifndef ALT_TAGS /* older version includes string length */
  if ((namePtrSize = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((imageFileNamePtrSize = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((leaderFileNamePtrSize = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((scanResultsFileNamePtrSize=readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((avgFileNamePtrSize=readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((pmfFileNamePtrSize=readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((SSP2namePtrSize=readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((inputDirSize=readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((failureReasonSize =readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((processorNameSize =readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
#endif

  if ((odlSize = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->jobId  = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->qcType  = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->retryCount  = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->numCalProductsStored = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->numCalProducts = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->fileVersion = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->catalogStep = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->modifiedFlag = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->dataSource = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);
  if ((mqel->status = readIntStrFromFile(fp, str, 10)) == 0 && !str)
    return(NULL);

#ifndef ALT_TAGS /* older version includes string length */
  mqel->namePtr = doMalloc(namePtrSize+2);
  mqel->imageFileNamePtr = doMalloc(imageFileNamePtrSize+2);
  mqel->leaderFileNamePtr = doMalloc(leaderFileNamePtrSize+2);
  mqel->scanResultsFileNamePtr = doMalloc(scanResultsFileNamePtrSize+2);
  mqel->avgFileNamePtr = doMalloc(avgFileNamePtrSize+2);
  mqel->pmfFileNamePtr = doMalloc(pmfFileNamePtrSize+2);
  mqel->SSP2namePtr = doMalloc(SSP2namePtrSize+2);
  mqel->inputDir = doMalloc(inputDirSize+2);
  mqel->processor = doMalloc(processorNameSize+2);
#endif

  mqel->namePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->imageFileNamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->leaderFileNamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->scanResultsFileNamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len);
  if (len <= 0)
    return(NULL);
  mqel->avgFileNamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->pmfFileNamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->SSP2namePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->RDSnamePtr = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  mqel->inputDir = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);
  strcpy(mqel->failureReason , readLineStrFromFile(fp, MAX_LINE_LENGTH, &len)); 
  if (len <= 0)
    return(NULL);
  mqel->processor = readLineStrFromFile(fp, MAX_LINE_LENGTH, &len); 
  if (len <= 0)
    return(NULL);



#ifdef READ_WRITE
printf("elemSize %d\n", elemSize);
printf("job %d\n", mqel->jobId);
printf("subsystem %s\n", mqel->namePtr);
printf("status %s\n", GetQstatusAsText_MSTRQLIST(mqel->status, mqel->namePtr));
printf("imagefilename %s\n", mqel->imageFileNamePtr);
printf("leaderfilename %s\n", mqel->leaderFileNamePtr);
printf("scan result filename %s\n", mqel->scanResultsFileNamePtr);
printf("avg filename %s\n", mqel->avgFileNamePtr);
printf("pmf filename %s\n", mqel->pmfFileNamePtr);
printf("inputDir %s\n", mqel->inputDir);
printf("SSP2namePtr %s\n", mqel->SSP2namePtr);
printf("RDSnamePtr %s\n", mqel->RDSnamePtr);
printf("numCalProductsStored %d\n", mqel->numCalProductsStored);
printf("numCalProducts %d\n", mqel->numCalProducts);
printf("fileVersion %d\n", mqel->fileVersion);
printf("catalogStep %s\n", GetCatStepAsText_MSTRQLIST(mqel->catalogStep));
printf("qcType %s\n", GetQCTypeAsText_MSTRQLIST(mqel->qcType));
printf("retryCount %d\n", mqel->retryCount);
printf("modified %d\n", mqel->modifiedFlag);
printf("source %s\n", GetDataSourceAsText_MSTRQLIST(mqel->dataSource) );
printf("failureReason %s\n", mqel->failureReason);
printf("processor %s\n", mqel->processor);
#endif
  
  for (i = 0; i < MAX_MAIN_LIST_ITEMS; i++) {
    fgets(mqel->mqFmt[i], MAX_LEN_Q_FMT,  fp); 
    len = strlen(mqel->mqFmt[i]);
    mqel->mqFmt[i][len-1] = '\0';  /* remove \n" */
    fgets(mqel->mqLabel[i], MAX_LEN_Q_STATUS_TEXT,  fp); 
    len = strlen(mqel->mqLabel[i]);
    mqel->mqLabel[i][len-1] = '\0';  /* remove \n" */
  }
  sprintf(mqel->mqLabel[MAIN_STATUS_LIST], MAIN_STATUS_FMT,
     p = GetQstatusAsText_MSTRQLIST(mqel->status,mqel->namePtr));
  doFree(p);


#ifdef READ_WRITE
  for (i = 0; i < MAX_MAIN_LIST_ITEMS; i++) {
printf("mqFmt[%d] %s\n", i, mqel->mqFmt[i]);
printf("mqLabel[%d] %s\n", i, mqel->mqLabel[i]);
  }
#endif

/* now read the statistical start/end times from the file */

 fgets(scanStartTime, TIME_STRING_SIZE+2, fp);  /* 1 for \0 and 1 for \n */
 fgets(scanEndTime, TIME_STRING_SIZE+2, fp);
 fgets(decodeStartTime, TIME_STRING_SIZE+2, fp);
 fgets(decodeEndTime, TIME_STRING_SIZE+2, fp);
 fgets(frameStartTime, TIME_STRING_SIZE+2, fp);
 fgets(frameEndTime, TIME_STRING_SIZE+2, fp);
 fgets(qcStartTime, TIME_STRING_SIZE+2, fp);
 fgets(qcEndTime, TIME_STRING_SIZE+2, fp);
 fgets(catalogStartTime, TIME_STRING_SIZE+2, fp);
 fgets(catalogEndTime, TIME_STRING_SIZE+2, fp);

#ifdef PRINT_DEBUG
printf("   scan start time %s ", scanStartTime);
printf("   scan   end time %s ", scanEndTime);
printf(" decode start time %s ", decodeStartTime);
printf(" decode   end time %s ", decodeEndTime);
printf("  frame start time %s ", frameStartTime);
printf("  frame   end time %s ", frameEndTime);
printf("     qc start time %s ", qcStartTime);
printf("     qc   end time %s ", qcEndTime);
printf("catalog start time %s ", catalogStartTime);
printf("catalog   end time %s ", catalogEndTime);
#endif

  if (*err = string_to_timeval(scanStartTime, &mqel->scanStartTime) ) 
    return(NULL);
  if (*err = string_to_timeval(scanEndTime, &mqel->scanEndTime) )
    return(NULL);
  if (*err = string_to_timeval( decodeStartTime, &mqel-> decodeStartTime) )
    return(NULL);
  if (*err = string_to_timeval( decodeEndTime, &mqel-> decodeEndTime) )
    return(NULL);
  if (*err = string_to_timeval(  frameStartTime, &mqel->  frameStartTime) )
    return(NULL);
  if (*err = string_to_timeval(  frameEndTime, &mqel->  frameEndTime) )
    return(NULL);
  if (*err = string_to_timeval(     qcStartTime, &mqel->     qcStartTime) )
    return(NULL);
  if (*err = string_to_timeval(     qcEndTime, &mqel->     qcEndTime) )
    return(NULL);
  if (*err = string_to_timeval(catalogStartTime, &mqel->catalogStartTime) )
    return(NULL);
  if (*err = string_to_timeval(catalogEndTime, &mqel->catalogEndTime) )
    return(NULL);

#ifdef ALT_TAGS /************* new tags ***********************************/

/* get the number of products and number of directories that were created */
/* during the previous run(s) of the CP.  the name of these products and */
/* their directories are needed so the CP can delete the files when this */
/* particular job completes. */

  if ((mqel->subsysFiles.numProducts = readIntStrFromFile(fp, str, 10)) == 0 
       && !str)
    return(NULL);
  if ((mqel->subsysFiles.numDirs = readIntStrFromFile(fp, str, 10)) == 0 
       && !str)
    return(NULL);

#ifdef READ_WRITE
printf("numProducts %d numDirs %d\n", mqel->subsysFiles.numProducts,
        mqel->subsysFiles.numDirs);
#endif

/* now, for each #products read, extract one product name from the file */
  for (i=0; i < mqel->subsysFiles.numProducts; i++)  {
    fgets(str, MAX_LINE_LENGTH, fp);
    str[strlen(str)-1] = '\0';  /* remove \n */
    strcpy(mqel->subsysFiles.productName[i], str);
  }
/* then, for each #dirs read, extract one dir name from the file */
  for (i=0; i < mqel->subsysFiles.numDirs; i++)  {
    fgets(str, MAX_LINE_LENGTH, fp);
    str[strlen(str)-1] = '\0';  /* remove \n */
    strcpy(mqel->subsysFiles.dirName[i], str);
  }
  mqel->subsysFiles.numFiles = 0;          /* only zero numFiles */

#ifdef READ_WRITE
  for (i=0; i < mqel->subsysFiles.numProducts; i++)
    printf("mqel->subsysFiles.numProducts[%d] %s\n", i, 
        mqel->subsysFiles.productName[i]);
  for (i=0; i < mqel->subsysFiles.numDirs; i++)
    printf("mqel->subsysFiles.numDirs[%d] %s\n", i, 
        mqel->subsysFiles.dirName[i]);
#endif
#endif /************* new tags ***********************************/

/* now read the ODL message itself */

  odlPartPtr  = doMalloc(odlSize+200); /* what the heck */
  for (i=0, p = odlPartPtr, trueSize=0; !(*err) && i < odlSize; i++) {
    if ((c = fgetc(fp)) == EOF)
      *err = 1;
    else {
      *p++ = c;
      trueSize++;
    }
  }
  *p = '\0';
#ifdef READ_WRITE
printf("true size read %d\n", trueSize);
#endif
  if (*err) {
   printf("EOF error\n");
    return(NULL);
  }

 if (GetSubsystemCategoryGivenName(mqel->namePtr) == RDS_CATEGORY_ID)
      adjustQueueID(mqel,(char*)NULL);

/* at this point we've finished reading all data from the file that is */
/* needed for this particular job. now it's time to place the job */
/* into a state consistent with the CP's restart (i.e. a job can't be */
/* processing because no subsystem is running) */

/* if the request was not finished processing, we need to back things
   up to the last stable state.  this means that any states in between
   "placed on queue' and "completion" have to be moved back to
   the "placed on queue" state */

  mqel->dataReadyFlag = TRUE;  /* assume data ready for ssp.  will set to  */
                               /* false below for rds/asp as necessary     */

  switch(mqel->status) {
                          /* here are all the jobs that need to be looked at */
                          /* right away by the master/subsystem handlers. */

    case Q_M_SCAN_READY_FOR_QC_ID:  
    case Q_M_IMAGE_READY_FOR_QC_ID:
    case Q_M_IMAGE_PRE_QC_ID:  
    case Q_S_IMAGE_QC_READY_ID:  
    case Q_M_SCAN_COMPLETED_ID:  
    case Q_M_CYCLE_COMPLETED_ID:  
    case Q_M_ENTRY_DELETED_ID: 
    case Q_M_NEXT_STEP_ID: 
    case Q_S_DONE_ID: 
    case Q_S_PLACED_ON_SYSQ_ID:
    case Q_M_PLACED_ON_SYSQ_ID:


      mqel->modifiedFlag = TRUE;  /* need job looked at next time */
      break;
                          /* here are all the jobs that need to be set to a */
                          /* different state than the one actually saved  */
                          /* these also need to be looked at right away */

    case Q_M_IMAGE_QC_DONE_ACCEPT_ID:  
      mqel->status = Q_S_IMAGE_QC_ACCEPT_ID;
      mqel->modifiedFlag = TRUE;  
      break;
    case Q_M_IMAGE_QC_DONE_REJECT_ID: 
      mqel->status = Q_S_IMAGE_QC_REJECT_ID;
      mqel->modifiedFlag = TRUE;  
      break;
    case Q_M_INCOMING_ID: 
      mqel->status = Q_M_NEXT_STEP_ID; 
      mqel->modifiedFlag = TRUE; 
      break;

                          /* here are all the jobs that were partially */
                          /* started by the CP or the subsystem; these */
                          /* jobs must be put back to the "on queue" state */
    case Q_S_CHECKING_TAPE_ID: 
    case Q_M_CHECKING_TAPE_ID: 
    case Q_M_REPEAT_LAST_STEP_ID: 
    case Q_M_INPUT_SOURCE_READY_ID: 
    case Q_M_RESTART_HERE_ID: 
    case Q_M_PROCESSING_RESET_ID: 
    case Q_M_PROCESSING_ID:
    case Q_M_SENT_ID:
    case Q_S_RCVD_ACK_ID:
    case Q_M_RCVD_ACK_ID:
    case Q_S_GOT_ITEM_OFF_ID:
    case Q_M_GOT_ITEM_OFF_ID:
    case Q_M_INPUT_SOURCE_NOT_READY_ID:
    case Q_S_READY_TO_SEND_ID:
    case Q_M_READY_TO_SEND_ID:
      mqel->status = Q_M_PLACED_ON_SYSQ_ID;
      mqel->modifiedFlag = TRUE;  /* need job looked at next time */
      cat = GetSubsystemCategoryGivenName(mqel->namePtr);
      if (cat == RDS_CATEGORY_ID || cat == ASP_CATEGORY_ID)
        mqel->dataReadyFlag = FALSE;      /* rds/asp data will be not ready */
      break;

   
                          /* here are all the jobs in a hold state */
    case Q_M_PLACED_ON_HOLD_DATA_ID: 
    case Q_M_PLACED_ON_HOLD_ERROR_ID: 
    case Q_M_IMAGE_QC_ON_HOLD_ID:
    case Q_S_IMAGE_QC_ON_HOLD_ID:
       mqel->modifiedFlag = TRUE;  /* need job looked at next time */
       *hold = TRUE;
       break;

    case Q_M_CLEANING_UP_ID: 
    case Q_M_DONE_ERROR_ID:   /* user was prompted by error or cleanup box */
                              /* so just put the job back on hold/error state */
       mqel->status = Q_M_PLACED_ON_HOLD_ERROR_ID;
       mqel->modifiedFlag = TRUE;  /* need job looked at next time */
       *hold = TRUE;
       break;

/*
    case Q_M_FINAL_ID:    job was really done -- let's just ignore it */
/*
       return(NULL); 
       break;
*/
    default:
      *insert = TRUE;  /* need to manually insert entry into subsys display  */
       break;
 
  }

  mqel->mstrqODLreq  = StrToODL(odlPartPtr, strlen(odlPartPtr));



  mqel->outputDir = doMalloc(strlen(UNDEFINED) + 1);
  if (mqel->outputDir == NULL) {
   return(NULL);
  }
  strcpy(mqel->outputDir, UNDEFINED);

#ifdef READ_WRITE
printf("ReadElem -- insert %d returning this mqel with status %d:\n", *insert,
 mqel->status);
  /*printf( "%d\n%d\n%d\n%d\n%d\n%d\n%s\n%s\n%s\n%d\n%d\n%d\n%d\n%s", */
  printf( "%d\n%d\n%d\n%d\n%d\n%s\n%d\n%d\n%d\n%d\n", 
             elemSize, odlSize, 
             mqel->jobId, mqel->qcType, mqel->retryCount, mqel->namePtr,
             mqel->catalogStep, mqel->modifiedFlag, mqel->dataSource,
             mqel->status);
#endif

/* printf("ReadElem -- odl part %s\n", odlPartPtr); */

  return(mqel);

} /* ReadElem_MSTRQLIST.................*/

/*----------------------------------------------------------
 * NAME:
 *  ReadIn_MSTRQLIST 
 *
 * DESCRIPTION:
 *  reads the entire contents of a master queue from a disk file
 *
 * NOTES:
 *  VISIBLE
 *  - may not be used
 *
 *---------------------------------------------------------*/
int ReadIn_MSTRQLIST(char *filename)
{
 mstrqListElemType *mqel;
 int err=0, i, retval=0, insert=0, holdFlag=0;
 int primaryTag=True; /* state file restored is primary or secondary choice */
 FILE *fp;
 char str[50];

  if ((fp = fopen(filename, "r")) == NULL) 
    return(-1);

  if (totalMQentries != 0) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, "Master Queue must be empty");
    return(-1);
  }

  fgets(str, MAX_CP_VERSION_TAG_LEN, fp); 
  if (strncmp(str, CP_VERSION_TAG, 6) != 0) { /* incompatible file */
    primaryTag = False;
    if (strncmp(str, ALT_CP_VERSION_TAG, 6) != 0) { /* incompatible file */
    printfLLog(LOG_ERR, "CP version tag (%s) incompatible in file %s", str,
filename);
    return(-2);
    }
  }
#ifdef READ_WRITE
printf("Restoring from version %s\n", str);
#endif
  printfLLog(LOG_INFO, "Restoring CP state from file %s\n", filename); 

  for (; !err ;) {
    mqel = ReadElem_MSTRQLIST(fp, &err, &insert, &holdFlag);
    if (err) {
      return(-2);
    }
    if (mqel) {
      if (mqel->status == Q_M_FINAL_ID) /* ignore job -- it was done anyway */
        continue;  /* return to top of loop and process next one */
      retval = AddElemToList_MSTRQLIST(mqel); 
      if (retval == -1)
        printfLLog(LOG_ERR, ERROR_ADDING_MQ_ID, mqel->jobId);
      ClearDataReady_MSTRQLIST(mqel->jobId);

      for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
        ASFlogMessageInsert_direct(ASF_CP, i,
          GetDispPos_MSTRQLIST(mqel->jobId), mqel->mqFmt[i],
             mqel->mqLabel[i]);
      }
      if (mqel->status == Q_S_IMAGE_QC_ON_HOLD_ID ||
          mqel->status == Q_M_IMAGE_QC_ON_HOLD_ID ||
          mqel->status == Q_M_IMAGE_PRE_QC_ID ||
          mqel->status == Q_M_SCAN_READY_FOR_QC_ID ||
          mqel->status == Q_M_IMAGE_READY_FOR_QC_ID )

        SetQCflag_SYSQUE(mqel->namePtr, mqel->jobId);

      if (mqel->status != Q_M_NEXT_STEP_ID) {
         retval = AddSysReqToQue_SYSQUE(mqel->namePtr, NOT_SET, mqel->jobId,
         TRUE, holdFlag, mqel->mstrqODLreq, mqel->dataReadyFlag);
         if (retval == -1) {
           printfLLog(LOG_ERR, ERROR_ADDING_SQ_ID, mqel->jobId, mqel->namePtr);
           continue;
         }
         for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
           ASFlogMessageInsert_direct(mqel->namePtr, i,
              GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId),
              mqel->mqFmt[i], mqel->mqLabel[i]);
         }
      }


#ifdef READ_WRITE
      printMstrqEl(mqel); 
#endif
    }
    else
      err = 1;
  }
/* nothing else left to read */
writeStateFile(); /* added 07/01/97 by Pearl PR# 2664 */

    return(0);  /* 0 for success */

} /* ReadIn_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  WriteOut_MSTRQLIST 
 *
 * DESCRIPTION:
 *  writes the entire contents of the master queue to a disk file
 *
 * NOTES:
 *  VISIBLE
 *  - may not be used
 *
 *---------------------------------------------------------*/
int WriteOut_MSTRQLIST(FILE *fp)
{
  mstrqListElemType *mqel;
  baseListElemType *nPtr;
  int i, j;
  int elemSize, lblSize, odlSize, timeSize=0, numIntsSaved, ni;
#ifdef ALT_TAGS
  int prodSize, dirSize;
#else /* old version still saves string lengths */
  int namePtrSize, imageFileNamePtrSize, leaderFileNamePtrSize,  
      avgFileNamePtrSize, scanResultsFileNamePtrSize, pmfFileNamePtrSize,
      SSP2namePtrSize, inputDirSize, failureReasonSize , processorNameSize;
#endif
  char *filePtr, *odlPartPtr, filePtrFmt[1024]; /* arbitrary 1024 */
  char scanStartTime[TIME_STRING_SIZE];
  char scanEndTime[TIME_STRING_SIZE];
  char  decodeStartTime[TIME_STRING_SIZE];
  char  decodeEndTime[TIME_STRING_SIZE];
  char   frameStartTime[TIME_STRING_SIZE];
  char   frameEndTime[TIME_STRING_SIZE];
  char      qcStartTime[TIME_STRING_SIZE];
  char      qcEndTime[TIME_STRING_SIZE];
  char catalogStartTime[TIME_STRING_SIZE];
  char catalogEndTime[TIME_STRING_SIZE];
  int retval = -2;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  fprintf(fp, "%s\n", CP_VERSION_TAG);
  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    odlPartPtr = ODLToStr(mqel->mstrqODLreq, NULL);
    if (odlPartPtr != NULL) {
      retval = 0;

      timeSize = timeval_to_string(scanStartTime, &mqel->scanStartTime);
      timeSize += timeval_to_string(scanEndTime, &mqel->scanEndTime);

      timeSize += timeval_to_string(frameStartTime, &mqel->frameStartTime);
      timeSize += timeval_to_string(frameEndTime, &mqel->frameEndTime);

      timeSize += timeval_to_string(decodeStartTime, &mqel->decodeStartTime);
      timeSize += timeval_to_string(decodeEndTime, &mqel->decodeEndTime);

      timeSize += timeval_to_string(qcStartTime, &mqel->  qcStartTime);
      timeSize += timeval_to_string(qcEndTime, &mqel->  qcEndTime);

      timeSize += timeval_to_string(catalogStartTime, &mqel->catalogStartTime);
      timeSize += timeval_to_string(catalogEndTime, &mqel->catalogEndTime);

      for (j=0, lblSize=0; j < MAX_MAIN_LIST_ITEMS; j++) {
        lblSize += strlen(mqel->mqFmt[j]);
        lblSize += strlen(mqel->mqLabel[j]);
      }

#ifdef ALT_TAGS
      for (j=0, prodSize=0; j < mqel->subsysFiles.numProducts; j++) 
        prodSize += strlen(mqel->subsysFiles.productName[j]);
      for (j=0, dirSize=0; j < mqel->subsysFiles.numDirs; j++) 
        dirSize += strlen(mqel->subsysFiles.dirName[j]);
#endif
      
      elemSize = 25 +  /* 25 \n will be in control string */
            MAX_PPS_TIME_ITEMS +    /* 10 time items will have \n */
            2*MAX_MAIN_LIST_ITEMS + /* also 2*MAX_MAIN_LIST_ITEMS \n */
            sizeof(elemSize)                                +
            timeSize +
            lblSize +
#ifdef ALT_TAGS
            sizeof(mqel->subsysFiles.numProducts) +
            sizeof(mqel->subsysFiles.numDirs) +
            prodSize +
            dirSize +
#else  /* older version: new one doesn't save string lengths              */
            sizeof(namePtrSize)                             + 
            sizeof(imageFileNamePtrSize)                   +
            sizeof(leaderFileNamePtrSize)                   +
            sizeof(scanResultsFileNamePtrSize)                   +
            sizeof(avgFileNamePtrSize)                   +
            sizeof(pmfFileNamePtrSize)                   +
            sizeof(SSP2namePtrSize)                   +
            sizeof(inputDirSize)                   +
            sizeof(failureReasonSize)                   +
            sizeof(processorNameSize)                   +
#endif
            sizeof(odlSize)                                 +
            sizeof(mqel->jobId)            +
            sizeof(mqel->qcType)            +
            sizeof(mqel->retryCount)            +
            strlen(mqel->namePtr)               +
            strlen(mqel->imageFileNamePtr)                   +
            strlen(mqel->leaderFileNamePtr)                   +
            strlen(mqel->scanResultsFileNamePtr)                   +
            strlen(mqel->avgFileNamePtr)                   +
            strlen(mqel->pmfFileNamePtr)                   +
            strlen(mqel->SSP2namePtr)                   +
            strlen(mqel->RDSnamePtr)                   +
            strlen(mqel->inputDir)                   +
            strlen(mqel->failureReason)                +
            strlen(mqel->processor)                +
            sizeof(mqel->numCalProductsStored)              +
            sizeof(mqel->numCalProducts)              +
            sizeof(mqel->fileVersion)              +
            sizeof(mqel->catalogStep)              +
            sizeof(mqel->modifiedFlag)          +
            sizeof(mqel->dataSource)          +
            sizeof(mqel->status)                +
            strlen(odlPartPtr)        ;
                          
#ifndef ALT_TAGS /* older version: new one doesn't save string lengths */
      namePtrSize            = strlen(mqel->namePtr);
      imageFileNamePtrSize  = strlen(mqel->imageFileNamePtr);
      leaderFileNamePtrSize  = strlen(mqel->leaderFileNamePtr);
      scanResultsFileNamePtrSize  = strlen(mqel->scanResultsFileNamePtr);
      avgFileNamePtrSize  = strlen(mqel->avgFileNamePtr);
      pmfFileNamePtrSize  = strlen(mqel->pmfFileNamePtr);
      SSP2namePtrSize  = strlen(mqel->SSP2namePtr);
      inputDirSize  = strlen(mqel->inputDir);
      failureReasonSize  = strlen(mqel->failureReason);
      processorNameSize  = strlen(mqel->processor);
#endif
      odlSize                = strlen(odlPartPtr);

      filePtr = doMalloc(elemSize+100); /* what's the 100 for? */

#ifdef ALT_TAGS
      numIntsSaved = 12;
#else
      numIntsSaved = 22;
#endif
      strcpy(filePtrFmt, "");
      for (ni=0; ni < numIntsSaved-1; ni++)
        sprintf(filePtrFmt, "%s%%04d\n", filePtrFmt); /* add one %04d\n */
      sprintf(filePtrFmt, "%s%%04d", filePtrFmt); /* last one has no \n */

      sprintf(filePtr,  filePtrFmt,
            elemSize,
#ifndef ALT_TAGS
            namePtrSize,
            imageFileNamePtrSize,
            leaderFileNamePtrSize,
            scanResultsFileNamePtrSize,
            avgFileNamePtrSize,
            pmfFileNamePtrSize,
            SSP2namePtrSize,
            inputDirSize,
            failureReasonSize,
            processorNameSize,
#endif
            odlSize,
            mqel->jobId,
            mqel->qcType,
            mqel->retryCount,
            mqel->numCalProductsStored,
            mqel->numCalProducts,
            mqel->fileVersion,
            mqel->catalogStep,
            mqel->modifiedFlag,
            mqel->dataSource,
            mqel->status);

      sprintf(filePtr, "%s\n%s", filePtr, mqel->namePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->imageFileNamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->leaderFileNamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->scanResultsFileNamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->avgFileNamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->pmfFileNamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->SSP2namePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->RDSnamePtr); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->inputDir); 
      sprintf(filePtr, "%s\n%s", filePtr, mqel->failureReason); 
      sprintf(filePtr, "%s\n%s\n", filePtr, mqel->processor); 
 
      for (j=0; j < MAX_MAIN_LIST_ITEMS; j++) {
        strcat(filePtr, mqel->mqFmt[j]);
        strcat(filePtr, "\n");
        strcat(filePtr, mqel->mqLabel[j]);
        strcat(filePtr, "\n");
      }
      sprintf(filePtr, "%s%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n", filePtr, 
                         scanStartTime,
                         scanEndTime,
                         decodeStartTime,
                         decodeEndTime,
                         frameStartTime,
                         frameEndTime,
                         qcStartTime,
                         qcEndTime,
                         catalogStartTime,
                         catalogEndTime);
#ifdef ALT_TAGS
/* add product info */
      sprintf(filePtr,"%s%04d\n%04d\n", filePtr,
              mqel->subsysFiles.numProducts, mqel->subsysFiles.numDirs);

      for (j=0; j < mqel->subsysFiles.numProducts; j++) 
        sprintf(filePtr, "%s%s\n", filePtr, mqel->subsysFiles.productName[j]); 
      for (j=0; j < mqel->subsysFiles.numDirs; j++) 
        sprintf(filePtr, "%s%s\n", filePtr, mqel->subsysFiles.dirName[j]); 
#endif

      strcat(filePtr, odlPartPtr); 

      fprintf(fp, "%s", filePtr);
      ODLFree(odlPartPtr);
      doFree(filePtr);
    } /* end if odlPart */
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(retval);

} /* WriteOut_MSTRQLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetQstatusAsText_MSTRQLIST
 *
 * DESCRIPTION:
 *  an access function that maps the master queue status to a
 *  text string and returns a pointer to that string
 *
 * NOTES:
 *  VISIBLE -- for evaluation
 *
 *---------------------------------------------------------*/
char *GetQstatusAsText_MSTRQLIST(int status, char *namePtr)
{
 char *ptr;

    ptr = doMalloc(MAX_LEN_Q_STATUS_TEXT);
    if (ptr == NULL) {
      return(NULL);
    }

    switch(status) {
      case Q_RECEIVED_ID :
        strcpy(ptr, NULL);
        break;
      case Q_M_PLACED_ON_SYSQ_ID:
        sprintf(ptr, Q_M_PLACED_ON_SYSQ_TEXT, namePtr);
        break;
      case Q_M_PROCESSING_ID    :
        sprintf(ptr, Q_M_PROCESSING_TEXT, namePtr);
        break;
      case Q_S_DONE_ID                    :
        sprintf(ptr, Q_S_DONE_TEXT, namePtr);
        break;
      case Q_M_INCOMING_ID :
        strcpy(ptr, Q_M_INCOMING_TEXT);
        break;
      case Q_M_CLEANING_UP_ID :
        sprintf(ptr, Q_M_CLEANING_UP_TEXT, namePtr);
        break;
      case Q_M_NEXT_STEP_ID :
        strcpy(ptr, Q_M_NEXT_STEP_TEXT);
        break;
      case Q_M_SENT_ID      :
        sprintf(ptr, Q_M_SENT_TEXT, namePtr);
        break;
      case Q_S_RCVD_ACK_ID  :
        sprintf(ptr, Q_S_RCVD_ACK_TEXT, namePtr);
        break;
      case Q_M_RCVD_ACK_ID  :
        sprintf(ptr, Q_M_RCVD_ACK_TEXT, namePtr);
        break;
      case Q_M_CYCLE_COMPLETED_ID  :
        sprintf(ptr, Q_M_CYCLE_COMPLETED_TEXT);
        break;
      case Q_S_GOT_ITEM_OFF_ID     :
        sprintf(ptr, Q_S_GOT_ITEM_OFF_TEXT, namePtr);
        break;
      case Q_M_IMAGE_READY_FOR_QC_ID:
        sprintf(ptr, Q_M_IMAGE_READY_FOR_QC_TEXT);
        break;
      case Q_S_IMAGE_QC_READY_ID:
        sprintf(ptr, Q_S_IMAGE_QC_READY_TEXT);
        break;
      case Q_M_IMAGE_PRE_QC_ID:
        /* sprintf(ptr, Q_M_IMAGE_PRE_QC_TEXT); */
        sprintf(ptr, Q_S_IMAGE_QC_READY_TEXT);
        break;
      case Q_M_SCAN_READY_FOR_QC_ID:
        sprintf(ptr, Q_M_SCAN_READY_FOR_QC_TEXT);
        break;
      case Q_S_IMAGE_QC_ON_HOLD_ID  :
        sprintf(ptr, Q_S_IMAGE_QC_ON_HOLD_TEXT, namePtr);
        break;
      case Q_M_IMAGE_QC_ON_HOLD_ID  :
        sprintf(ptr, Q_M_IMAGE_QC_ON_HOLD_TEXT, namePtr);
        break;
      case Q_S_IMAGE_QC_ACCEPT_ID   :
        sprintf(ptr, Q_S_IMAGE_QC_ACCEPT_TEXT);
        break;
      case Q_S_IMAGE_QC_REJECT_ID   :
        sprintf(ptr, Q_S_IMAGE_QC_REJECT_TEXT);
        break;
      case Q_M_IMAGE_QC_DONE_REJECT_ID:
        sprintf(ptr, Q_M_IMAGE_QC_DONE_REJECT_TEXT);
        break;
      case Q_M_IMAGE_QC_DONE_ACCEPT_ID:
        sprintf(ptr, Q_M_IMAGE_QC_DONE_ACCEPT_TEXT);
        break;
      case Q_M_INTERRUPTED_ID         :
        /* sprintf(ptr, Q_M_INTERRUPTED_TEXT, namePtr); */
        sprintf(ptr, Q_M_PLACED_ON_SYSQ_TEXT , namePtr); /* WAITING FOR XXX */
        break;
      case Q_M_RESTART_HERE_ID        :
        sprintf(ptr, Q_M_RESTART_HERE_TEXT, namePtr);
        break;
      case Q_M_ENTRY_DELETED_ID       :
        sprintf(ptr, Q_M_ENTRY_DELETED_TEXT, namePtr);
        break;
      case Q_M_SCAN_COMPLETED_ID      :
        sprintf(ptr, Q_M_SCAN_COMPLETED_TEXT);
        break;
      case Q_M_HANDLING_ERROR_ID       :
        sprintf(ptr, Q_M_HANDLING_ERROR_TXT, namePtr);
        break;
      case Q_M_FINAL_ID               :
        sprintf(ptr, Q_M_FINAL_TEXT);
        break;
      case Q_M_PROCESSING_RESET_ID    :
/*         sprintf(ptr, Q_M_PROCESSING_RESET_TEXT, namePtr);  */
        sprintf(ptr, Q_M_PLACED_ON_SYSQ_TEXT , namePtr); /* WAITING FOR XXX */
        break;
      case Q_M_DONE_ERROR_ID          :
        sprintf(ptr, Q_M_DONE_ERROR_TEXT, namePtr);
        break;
      case Q_M_INPUT_SOURCE_READY_ID  :
        sprintf(ptr, Q_M_INPUT_SOURCE_READY_TEXT, namePtr);
        break;
      case Q_M_INPUT_SOURCE_NOT_READY_ID :
        sprintf(ptr, Q_M_INPUT_SOURCE_NOT_READY_TEXT, namePtr);
        break;
      case Q_M_PLACED_ON_HOLD_ERROR_ID    :
        sprintf(ptr, Q_M_PLACED_ON_HOLD_ERROR_TEXT, namePtr);
        break;
      case Q_M_PLACED_ON_HOLD_DATA_ID :
        sprintf(ptr, Q_M_PLACED_ON_HOLD_DATA_TEXT, namePtr);
        break;
      case Q_M_REPEAT_LAST_STEP_ID    :
        sprintf(ptr, Q_M_REPEAT_LAST_STEP_TEXT, namePtr);
        break;
      case Q_S_READY_TO_SEND_ID    :
        sprintf(ptr, Q_S_READY_TO_SEND_TEXT, namePtr);
        break;
      case Q_M_READY_TO_SEND_ID    :
        sprintf(ptr, Q_M_READY_TO_SEND_TEXT, namePtr);
        break;
      case Q_M_GOT_ITEM_OFF_ID    :
        sprintf(ptr, Q_M_GOT_ITEM_OFF_TEXT, namePtr);
        break;
      case Q_S_PLACED_ON_SYSQ_ID    :
        sprintf(ptr, Q_S_PLACED_ON_SYSQ_TEXT, namePtr);
        break;
      case Q_M_CHECKING_TAPE_ID    :
      case Q_S_CHECKING_TAPE_ID    :
        sprintf(ptr, Q_M_CHECKING_TAPE_TEXT, namePtr);
        break;
      default:
        strcpy(ptr, "UNDEFINED");
        break;
      }
      return(ptr);
} /* GetQstatusAsText_MSTRQLIST........*/

/*----------------------------------------------------------
 * NAME:
 *  GetJobTotals_MSTRQLIST
 *
 * DESCRIPTION:
 *  an access function that maps the master queue status to a
 *  text string and returns a pointer to that string
 *
 * NOTES:
 *  VISIBLE -- for evaluation
 *
 *---------------------------------------------------------*/
int GetJobTotals_MSTRQLIST(int *ss_scan,  int *cts_scan, 
                           int *ss_frame, int *cts_frame)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i, biggest, ok = 1;
 char *errmsg, *odlModeStr, *req_type_str;

  *ss_scan = *cts_scan = *ss_frame = *cts_frame = 0;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    ok = ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MODE, &odlModeStr);
    if (ok)
      ok = ODLGetVal(mqel->mstrqODLreq, errmsg=HDR_MSG_TYPE, &req_type_str);

    if (!ok) {
      printf("Can't read mq odl %s\n", errmsg);
      unLockNameList_CORE(nPtr);
      return(-1);
    }

#ifdef MSTR
printf("GetJobTotals_MSTRQLIST mode %s type %s\n", odlModeStr, req_type_str);
#endif
    if (getModeId(odlModeStr) == MODE_CONTINUOUS_ID) {
      if (strcmp(req_type_str, "FRAME_REQUEST") == 0)
        (*cts_frame)++;
      else if (strcmp(req_type_str, "SCAN_REQUEST") == 0)
        (*cts_scan)++;
    }
    else if (getModeId(odlModeStr) == MODE_SCANSAR_ID) {       /* is scansar */
      if (strcmp(req_type_str, "FRAME_REQUEST") == 0)
        (*ss_frame)++;
      else if (strcmp(req_type_str, "SCAN_REQUEST") == 0)
        (*ss_scan)++;
    }

  } 

  biggest = Max(*ss_scan, *cts_scan); /* this isn't really what we want! */
  biggest = Max(biggest, *ss_frame);
  biggest = Max(biggest, *cts_frame);
#ifdef MSTR
printf("leaving GetJobTotals_MSTRQLIST: ");
printf("biggest %d ss_scan %d cts_scan %d ss_frame %d cts_frame %d\n",
biggest, *ss_scan , *cts_scan , *ss_frame , *cts_frame );

#endif

  unLockNameList_CORE(nPtr);
  return(biggest);

} /* GetJobTotals_MSTRQLIST */
/*----------------------------------------------------------
 * NAME:
 *  GetJobRank_MSTRQLIST
 *
 * DESCRIPTION:
 *  an access function that maps the master queue status to a
 *  text string and returns a pointer to that string
 *
 * NOTES:
 *  VISIBLE -- for evaluation
 *
 *---------------------------------------------------------*/
int GetJobRank_MSTRQLIST(int *rank)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i, j, ok = 1;
 char *errmsg, *odlModeStr, *req_type_str;
 int rankNum, ss_scan=0, cts_scan=0, ss_frame=0, cts_frame=0;
 int sorted_array[P_REQ_LAST] = {0, 0, 0, 0};
 int rankByIndex[P_REQ_LAST] = {0, 0, 0, 0};
 int tied[P_REQ_LAST] = {0, 0, 0, 0};

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    ok = ODLGetVal(mqel->mstrqODLreq, errmsg=BODY_MODE, &odlModeStr);
    if (ok)
      ok = ODLGetVal(mqel->mstrqODLreq, errmsg=HDR_MSG_TYPE, &req_type_str);

    if (!ok) {
      printf("Can't read mq odl %s\n", errmsg);
      unLockNameList_CORE(nPtr);
      return(-1);
    }

#ifdef MSTR
printf("GetJobRank_MSTRQLIST mode %s type %s\n", odlModeStr, req_type_str);
#endif
    if (getModeId(odlModeStr) == MODE_CONTINUOUS_ID) {
      if (strcmp(req_type_str, "FRAME_REQUEST") == 0)
        cts_frame++;
      else if (strcmp(req_type_str, "SCAN_REQUEST") == 0)
        cts_scan++;
    }
    else if (getModeId(odlModeStr) == MODE_SCANSAR_ID) {       /* is scansar */
      if (strcmp(req_type_str, "FRAME_REQUEST") == 0)
        ss_frame++;
      else if (strcmp(req_type_str, "SCAN_REQUEST") == 0)
        ss_scan++;
    }

  } 

  sorted_array[P_SS_SCAN] = ss_scan;
  sorted_array[P_SS_FRAME] = ss_frame;
  sorted_array[P_CTS_SCAN] = cts_scan;
  sorted_array[P_CTS_FRAME] = cts_frame;
  /* qsort_int(sorted_array, 0, P_REQ_LAST); */
#ifdef PPS_SELECT
printf("GetJobRank_MSTRQLIST before sort, #jobs of each type: ");
for (i=0; i < P_REQ_LAST; i++) printf("%d ", sorted_array[i]); printf("\n");
#endif
  bsort(sorted_array, P_REQ_LAST);

#ifdef PPS_SELECT
printf("GetJobRank_MSTRQLIST after  sort, #jobs of each type: ");
for (i=0; i < P_REQ_LAST; i++) printf("%d ", sorted_array[i]); printf("\n");
#endif


/* if a 2-way tie, set tied[i] to 2 for the tied elements */
/* if a 3-way tie, set tied[i] to 3 for the tied elements */

  for (i=0; i < P_REQ_LAST; i++)  
    for (j=0; j < P_REQ_LAST; j++)  
      if (sorted_array[i] == sorted_array[j]) {
        tied[i]++;
        tied[j]++;
      }

printf("tied: ");
  for (i=0; i < P_REQ_LAST; i++)  
    printf("%d ", tied[i]);
printf("\n");
  for (i=0; i < P_REQ_LAST; i++)  
    rankByIndex[i] = rank[i] = -1;


  for (rankNum = 0, i=0; i < P_REQ_LAST; i++) {
    if (rankByIndex[P_SS_SCAN] == -1 && sorted_array[i] == ss_scan) {
      rankByIndex[P_SS_SCAN] = rankNum++;
    }
    else if (rankByIndex[P_SS_FRAME] == -1 && sorted_array[i] == ss_frame) {
      rankByIndex[P_SS_FRAME] = rankNum++;
    }
    else if (rankByIndex[P_CTS_SCAN] == -1 && sorted_array[i] == cts_scan) {
      rankByIndex[P_CTS_SCAN] = rankNum++;
    }
    else if (rankByIndex[P_CTS_FRAME] == -1 && sorted_array[i] == cts_frame) {
      rankByIndex[P_CTS_FRAME] = rankNum++;
    }

  }
#ifdef PPS_SELECT
printf("after rankByIndex, setting rank\n");
for (i=0; i < P_REQ_LAST; i++) printf("%d ", rankByIndex[i]); printf("\n");
#endif

  for (i=0; i < P_REQ_LAST; i++) 
    for (j=0; j < P_REQ_LAST; j++) {
      if (i == rankByIndex[j])
        rank[i] = j;
       
    }

#ifdef PPS_SELECT
printf("leaving GetJobRank_MSTRQLIST: ");
for (i=0; i < P_REQ_LAST; i++) printf("%d ", rank[i]); printf("\n");

#endif

  unLockNameList_CORE(nPtr);
  return(0);

} /* GetJobRank_MSTRQLIST */

void printRank(int *rank)
{
  int i;
printf("Rank is ");
for (i=0; i < P_REQ_LAST; i++) printf("%d ", rank[i]); printf("\n");

}


/*----------------------------------------------------------
 * NAME:
 *  SetSubsysFilesGivenJobId_MSTRQLIST
 *
 * DESCRIPTION:
 *  given the jobId, set the list of files created by
 *  the subsystem corresponding to this jobId
 *
 * NOTES:
 *   VISIBLE
 *
 *
 *---------------------------------------------------------*/
int SetSubsysFilesGivenJobId_MSTRQLIST(int jobId, subsysFilesType *subsysFiles)
{
  mstrqListElemType *mqel;
  baseListElemType *nPtr;
  int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

  if (mqel->jobId == jobId) {
#ifdef FILES
printf("into SetSubsysFilesGivenJobId_MSTRQLIST, found jobId %d\n", jobId);
showSf_sf("SetSubsysFilesGivenJobId_MSTRQLIST before set, subsys (source value)", subsysFiles);
#endif

  memcpy(&mqel->subsysFiles, subsysFiles, sizeof( subsysFilesType));

#ifdef FILES
printf("SetSubsysFilesGivenJobId_MSTRQLIST job %d at 0x%x, moved from 0x%x, size 0x%x\n", subsysFiles->jobId, &mqel->subsysFiles, subsysFiles, sizeof( subsysFilesType));
#endif

#ifdef FILES
printf("leaving SetSubsysFilesGivenJobId_MSTRQLIST\n");
showSf_sf("SetSubsysFilesGivenJobId_MSTRQLIST after set, subsys (source value)", subsysFiles);
showSf_sf("SetSubsysFilesGivenJobId_MSTRQLIST after set, jobId (dest value)", &mqel->subsysFiles);
#endif
  unLockNameList_CORE(nPtr);
  return(0);
    }
  }
  return(-1);

} /* end SetSubsysFilesGivenJobId_MSTRQLIST...........*/
/*----------------------------------------------------------
 * NAME:
 *  GetSubsysFilesGivenJobId_MSTRQLIST
 *
 * DESCRIPTION:
 *  given the jobId, return the list of files created by
 *  the subsystem corresponding to this jobId
 *
 * NOTES:
 *   VISIBLE
 *
 *
 *---------------------------------------------------------*/
subsysFilesType *GetSubsysFilesGivenJobId_MSTRQLIST(int jobId)
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 subsysFilesType *subsysFiles;
 int i;

  nPtr = lockNameList_CORE(MSTRQ_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    mqel = (mstrqListElemType *)nPtr->arr[i];
    if (mqel == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    }

  if (mqel->jobId == jobId) {
#ifdef FILES
printf("into GetSubsysFilesGivenJobId_MSTRQLIST, found jobId %d\n", jobId);
showSf_sf("GetSubsysFilesGivenJobId_MSTRQLIST before get, jobId (source value)", &mqel->subsysFiles);
#endif

    subsysFiles = (subsysFilesType *)doMalloc(sizeof(subsysFilesType));
    memcpy(subsysFiles, &mqel->subsysFiles, sizeof(subsysFilesType));
#ifdef FILES
printf("GetSubsysFilesGivenJobId_MSTRQLIST job %d at 0x%x, moved from 0x%x, size 0x%x\n", subsysFiles->jobId, subsysFiles, &mqel->subsysFiles, sizeof( subsysFilesType));
#endif

#ifdef FILES
printf("leaving GetSubsysFilesGivenJobId_MSTRQLIST\n");
showSf_sf("GetSubsysFilesGivenJobId_MSTRQLIST after get, subsys (dest value)", subsysFiles);
showSf_sf("GetSubsysFilesGivenJobId_MSTRQLIST after get, jobId (source value)", &mqel->subsysFiles);
#endif

    unLockNameList_CORE(nPtr);
    return(subsysFiles);
    }
  }
    unLockNameList_CORE(nPtr);
    return(NULL);


} /* end GetSubsysFilesGivenJobId_MSTRQLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetCatStepAsText_MSTRQLIST
 *
 * DESCRIPTION:
 *  an access function that maps the master queue catalog step to a
 *  text string and returns a pointer to that string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetCatStepAsText_MSTRQLIST(int catStep)
{
 char *ptr;

    switch(catStep) {
      case IMS_TYPE_NONE :
        ptr = UNDEFINED;
        break;
      case IMS_TYPE_GET_VERSION:
        ptr = IMS_TYPE_GET_VERSION_TEXT;
        break;
      case IMS_TYPE_GET_SCAN_RESULT    :
        ptr = IMS_TYPE_GET_SCAN_RESULT_TEXT;
        break;
      case IMS_TYPE_GET_CAL_PARAMS                    :
        ptr = IMS_TYPE_GET_CAL_PARAMS_TEXT;
        break;
      case IMS_TYPE_STORE_CAL_PRODUCTS :
        ptr = IMS_TYPE_STORE_CAL_PRODUCTS_TEXT;
        break;
      case IMS_TYPE_STORE_PRODUCTS :
        ptr = IMS_TYPE_STORE_PRODUCTS_TEXT;
        break;
      case IMS_TYPE_STORE_SCAN_RESULT      :
        ptr = IMS_TYPE_STORE_SCAN_RESULT_TEXT;
        break;
      case IMS_TYPE_STORE_SCAN_METADATA:
        ptr = IMS_TYPE_STORE_SCAN_META_TEXT;
        break;
      case IMS_TYPE_CP_WORKING  :
        ptr = IMS_TYPE_CP_WORKING_TEXT;
        break;
      case IMS_TYPE_DONE  :
        ptr = IMS_TYPE_DONE_TEXT;
        break;
      default:
        ptr = UNDEFINED;
        break;
      }
      return(ptr);
} /* GetCatStepAsText_MSTRQLIST........*/


char *GetQCTypeAsText_MSTRQLIST(int qcType)
{
 char *ptr;

    switch(qcType) {
      case  QC_TYPE_NONE :
        ptr = "QC_TYPE_NONE";
        break;
      case QC_TYPE_IMAGE:
        ptr = "QC_TYPE_IMAGE";
        break;
      case QC_TYPE_SCAN    :
        ptr = "QC_TYPE_SCAN";
        break;
      case QC_TYPE_ACCEPT                    :
        ptr = "QC_TYPE_ACCEPT";
        break;
      case QC_TYPE_SCAN_REJECT :
        ptr = "QC_TYPE_SCAN_REJECT";
        break;
      case QC_TYPE_DONE :
        ptr = "QC_TYPE_DONE";
        break;
      default:
        ptr = UNDEFINED;
        break;
      }
      return(ptr);
} /* GetQCTypeAsText_MSTRQLIST........*/

char *GetDataSourceAsText_MSTRQLIST(int qcType)
{
 char *ptr;
    switch(qcType) {
      case  DATA_FROM_DISK :
        ptr = "DATA_FROM_DISK";
        break;
      case  DATA_FROM_TAPE :
        ptr = "DATA_FROM_TAPE";
        break;
      default:
        ptr = UNDEFINED;
        break;
      }
      return(ptr);
} /* GetDataSourceAsText_MSTRQLIST........*/
