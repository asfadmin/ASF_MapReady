/* #define EXTRA_DEBUG /* turn on extra LOG_DEBUG syslog messages */

/* #define SYSQ  /* enable this to printf sysq debug information */

/*----------------------------------------------------------
 * SCCS Header
 * File:que_sys.c   Rev:4.2.0.0   Date:95/02/10
 *
 * Tag range 10,000-19,999
 *---------------------------------------------------------*/

static char sccsid_que_sys_c[] = "@(#)que_sys.c	4.36 96/12/12 10:03:29";

#include <stdio.h>
#include <syslog.h>
#include <bstring.h> /* bzero */

#include "asfcommon.h"
#include "odl.h"    
#include "logUtils.h"
#include "memUtils.h"

#include "cpdefines.h"
#include "listcore.h"
#include "que_sys.h"
#include "cplogs.h"
#include "listmstrq.h"

/*----------------------------------------------------------
 * NAME:
 *  CreateSysReqQue_SYSQUE 
 *
 * DESCRIPTION:
 *  create a thread safe subsystem queue and sema for subsystem 
 *  denoted by "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int  CreateSysReqQue_SYSQUE(char *namePtr)
{
 int retval;

#ifdef SYSQ
printf("CreateSysReqQue_SYSQUE\n");
#endif
  retval =  createOneNameListEntry_CORE(namePtr);
  return(retval);

} /* CreateSysReqQue_SYSQUE...................*/



/*----------------------------------------------------------
 * NAME:
 *  AddSysReqToQue_SYSQUE
 *
 * DESCRIPTION:
 *  Add a new element to the subsystem's queue denoted by
 *  "namePtr"
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int AddSysReqToQue_SYSQUE(char *namePtr, char *reqStrPtr, int jobId, 
                          int restoring, int holdFlag, 
                          ODL sysODLreq, int dataReady)
{
 sysQueElemType *sysQueElemPtr, *tmp, *ptr;
 int retval, pos, i, status;
 baseListElemType *nPtr;
 char *insertStr;
 struct timeval thisTval, newTval;
 time_t thisTime, newTime;

#ifdef SYSQ
printf("AddSysReqToQue_SYSQUE %s jobId %d\n", namePtr, jobId);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
     return(-1);
                                       /* set up the element to add */
  sysQueElemPtr = (sysQueElemType *)doMalloc(sizeof(sysQueElemType));
  if (sysQueElemPtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }
  sysQueElemPtr->reqStrPtr = (char *)doMalloc(strlen(reqStrPtr) + 1);
  if (sysQueElemPtr->reqStrPtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(-1);
  }

  if (nPtr->nsize >0)
     tmp = (sysQueElemType *)nPtr->arr[nPtr->nsize-1];

  strcpy(sysQueElemPtr->reqStrPtr, reqStrPtr);
  sysQueElemPtr->jobId     = jobId;
  sysQueElemPtr->holdFlag    = holdFlag;
  sysQueElemPtr->qcFlag         = FALSE;
  sysQueElemPtr->dataReadyFlag  = dataReady;
  sysQueElemPtr->repeatFlag     = FALSE;
  sysQueElemPtr->checkMediaId     = MEDIA_CHECK_YES;
  sysQueElemPtr->sysODLreq   = (ODL) ODLcopy(sysODLreq);
  if (sysQueElemPtr->sysODLreq == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
  }



  pos = nPtr->nsize;  /* default insert position is at bottom of queue */
#ifdef SYSQ
  ODLGetVal(sysQueElemPtr->sysODLreq , BODY_INSERT_TOP, &insertStr);
printf("\n######################################################\nsqadd %s job %d... default pos %d restoring %d insert top %s\n", 
namePtr, jobId, pos, restoring, insertStr); fflush(stdout);
#endif

  if (!restoring) { /* don't want to re-top insert top jobs on restores */
    ODLGetVal(sysQueElemPtr->sysODLreq , BODY_INSERT_TOP, &insertStr);
                                            /* if NO --> add to end of queue */
    if (strcmp(insertStr, "NO") != 0) {  /* if YES -> find insert position */
      
      for(i = 0 ; i < nPtr->nsize; i++) {
        pos = i+1;
       ptr = (sysQueElemType *)nPtr->arr[i];
       status = GetStatus__MSTRQLIST(namePtr, ptr->jobId);
#ifdef SYSQ
printf("\tsqadd... %s checking job %d status %d\n", namePtr, ptr->jobId, status);
#endif
       if(status == Q_S_PLACED_ON_SYSQ_ID || status == Q_S_GOT_ITEM_OFF_ID) {
#ifdef SYSQ
printf("\tsqadd... %s comparing job %d to job %d\n", 
       namePtr, ptr->jobId , sysQueElemPtr->jobId);
#endif

#ifdef SYSQ
printf("\tsqadd... %s should compare timestamp for pos %d, job %d new job %d\n", namePtr, i, ptr->jobId, sysQueElemPtr->jobId);
printf("\tsqadd... %s assigning to pos %d\n", namePtr, i);
#endif
             /* compare timestamps of insert top jobs: if a job arrived */
             /* later than another, it should be inserted above that job */
           ODLGetVal(sysQueElemPtr->sysODLreq, HDR_TIME, &newTval);
           ODLGetVal(ptr->sysODLreq, HDR_TIME, &thisTval);
           newTime = timeval_to_time(newTval);
           thisTime = timeval_to_time(thisTval);
           if (thisTime <= newTime) { /* time later --> goes on top */
             pos = i;
             break; /* jump out of for */
           }

       }
      } /* for */
    }
  }
  else /* when restoring, set repeatFlag to TRUE */
    sysQueElemPtr->repeatFlag = TRUE;

#ifdef SYSQ
printf("\tsqadd... %s inserting job %d at pos %d\n", namePtr, jobId, pos);   fflush(stdout);
#endif
  retval = InsertNewValueAt_CORE(nPtr, sysQueElemPtr, pos);

  printfLLog(LOG_DEBUG, Q_ADDING, namePtr, sysQueElemPtr->jobId,  pos);

  unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("leaving AddSysReqToQue_SYSQUE, added to pos %d\n", pos);
dumpSysq(namePtr);
#endif
  return(retval);

} /* end AddSysReqToQue_SYSQUE....................*/


/*----------------------------------------------------------
 * NAME:
 *  FindSysReqInQue_SYSQUE 
 *
 * DESCRIPTION:
 *  find a job matching the specified ODL in the subsystems queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *FindSysReqInQue_SYSQUE(baseListElemType *nPtr, ODL sysODLreq, 
                                       int *pos)
{
 sysQueElemType *sysQueElemPtr;
 int i;
  
#ifdef SYSQ
printf("FindSysReqInQue_SYSQUE \n");
#endif
  /* find the sysODLreq in the list */
  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];

    if (sysQueElemPtr == NULL)
         return(NULL);

    if (sysODLreq == sysQueElemPtr->sysODLreq) {
      *pos = i;
      return(sysQueElemPtr);
    }
  } /* end for */

  return(NULL);
} /* end FindSysReqInQue_SYSQUE.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetElemGivenJobId_SYSQUE
 *
 * DESCRIPTION:
 *  find a job matching the specified ODL in the subsystems queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetElemGivenJobId_SYSQUE(char *namePtr, int jobId, int mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
printf("GetElemGivenJobId_SYSQUE %s jobId %d  mode %d...", namePtr,jobId,mode);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

for (i = 0; i < nPtr->nsize; i++) {
 sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
 if (sysQueElemPtr == NULL) {
  unLockNameList_CORE(nPtr);
  return(NULL);
 }
 if (sysQueElemPtr->jobId == jobId ) {
  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetElemGivenJobId_SYSQUE removing... job %d name %s mode %d\n", jobId, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, i, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, i);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, i, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     return(retSysQueElemPtr);
     break;
   case DO_COPY:
     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("returning %d\n", retSysQueElemPtr->jobId);
#endif
     return(retSysQueElemPtr);
     break;
   default:
     /* force the pointer to NULL */
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     unLockNameList_CORE(nPtr);
     return(NULL);
     break;
  } /* end switch */
 } /* end if */
 } /* end for */

 unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("\n");
#endif
 return(NULL);

} /* end GetElemGivenJobId_SYSQUE.................*/



/*----------------------------------------------------------
 * NAME:
 *  FreeSysQueElement_SYSQUE 
 *
 * DESCRIPTION:
 *  do all the "frees" necessary to release the memory for
 *  a sysqueue element
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void FreeSysQueElement_SYSQUE(char *namePtr, sysQueElemType *sysQueElemPtr)
{
#ifdef SYSQ
printf("FreeSysQueElement_SYSQUE %s job %d\n", namePtr, sysQueElemPtr->jobId); 
#endif
  doFree(sysQueElemPtr->reqStrPtr);
  if (sysQueElemPtr->sysODLreq)
    ODLFree(sysQueElemPtr->sysODLreq);
  sysQueElemPtr->sysODLreq = NULL;
  doFree(sysQueElemPtr); 

}  /* FreeSysQueElement_SYSQUE........*/


/*----------------------------------------------------------
 * NAME:
 *  FreeSysQueElemGivenPos_SYSQUE
 *
 * DESCRIPTION:
 *  do all the "frees" necessary to release the memory for
 *  a sysqueue element
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void FreeSysQueElemGivenPos_SYSQUE(char *namePtr, int sqPos)
{
  sysQueElemType *sqel;
#ifdef SYSQ
printf("FreeSysQueElemGivenPos_SYSQUE %s pos %d\n", namePtr, sqPos);
#endif
  sqel = GetReqGivenListPos_SYSQUE(namePtr, sqPos);
  if (sqel != NULL) {
    doFree(sqel->reqStrPtr);
    ODLFree(sqel->sysODLreq);
    sqel->sysODLreq = NULL;
    doFree(sqel);
  }
  else
    printfLLog(LOG_ERR, "Error freeing %s element from position %d\n", namePtr, sqPos);

}  /* FreeSysQueElemGivenPos_SYSQUE........*/



/*----------------------------------------------------------
 * NAME:
 *  CopyElemAtPrivate_SYSQUE
 *
 * DESCRIPTION:
 *  copy a sysque element
 *
 * NOTES:
 *  this is a private routine -- the queue is not locked
 *---------------------------------------------------------*/
sysQueElemType *CopyElemAtPrivate_SYSQUE(sysQueElemType *sysQueElemPtr)
{
 sysQueElemType *retSysQueElemPtr;

   if (sysQueElemPtr == NULL)
    return(NULL);

   retSysQueElemPtr = (sysQueElemType *)doMalloc(sizeof(sysQueElemType)); 
   if (retSysQueElemPtr == NULL)
      return(NULL);
  
   /* make sure memory is clear */
   bzero(retSysQueElemPtr, sizeof(sysQueElemType));  

   retSysQueElemPtr->reqStrPtr = doMalloc(strlen(sysQueElemPtr->reqStrPtr) + 1);

   if (retSysQueElemPtr->reqStrPtr == NULL)
      return(NULL);
  
   strcpy(retSysQueElemPtr->reqStrPtr, sysQueElemPtr->reqStrPtr);
   retSysQueElemPtr->jobId  = sysQueElemPtr->jobId;
   retSysQueElemPtr->holdFlag = sysQueElemPtr->holdFlag;
   retSysQueElemPtr->qcFlag      = sysQueElemPtr->qcFlag;
   retSysQueElemPtr->dataReadyFlag     = sysQueElemPtr->dataReadyFlag;
   retSysQueElemPtr->repeatFlag  = sysQueElemPtr->repeatFlag;
   retSysQueElemPtr->checkMediaId  = sysQueElemPtr->checkMediaId;
   retSysQueElemPtr->sysODLreq   = (ODL) ODLcopy(sysQueElemPtr->sysODLreq);
   if (retSysQueElemPtr->sysODLreq == NULL) {
     return(NULL);
   }

   return(retSysQueElemPtr);

} /* end CopyElemAtPrivate_SYSQUE......................................*/

/*----------------------------------------------------------
 * NAME:
 *  GetHeadOfSysQue_SYSQUE 
 *
 * DESCRIPTION:
 *  remove the head element of the subsystem queue denoted 
 *  by "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetHeadOfSysQue_SYSQUE(char *namePtr, int mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetHeadOfSysQue_SYSQUE %s mode %d\n", namePtr, mode);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

  switch(mode) {
   case DO_REMOVE:
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr,0);
     if (retvalPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     break;

   case DO_COPY:
     sysQueElemPtr =  nPtr->arr[0];

     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
  
     break;
   default:
     /* force the pointer to NULL */
     retSysQueElemPtr = NULL;
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     break;
  }
  unLockNameList_CORE(nPtr);
  return(retSysQueElemPtr);

} /* GetHeadOfSysQue_SYSQUE.............*/


/*----------------------------------------------------------
 * NAME:
 *  GetSysQueElemAt_SYSQUE 
 *
 * DESCRIPTION:
 *  remove the head element of the subsystem queue denoted 
 *  by "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetSysQueElemAt_SYSQUE(char *namePtr, int  elemNo, int  mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetSysQueElemAt_SYSQUE %s elemNo %d mode %d\n", namePtr, elemNo, mode);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

  if (elemNo >= nPtr->nsize) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetSysQueElemAt_SYSQUE removing... elemNo %d name %s mode %d\n", elemNo, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, elemNo, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, elemNo);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, elemNo, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     break;

   case DO_COPY:
     sysQueElemPtr =  nPtr->arr[elemNo];

     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
  
     break;
   default:
     /* force the pointer to NULL */
     retSysQueElemPtr = NULL;
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     break;
  }
  unLockNameList_CORE(nPtr);
  return(retSysQueElemPtr);

} /* GetSysQueElemAt_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  GetNextNeedingMountElem_SYSQUE 
 *
 * DESCRIPTION:
 *  return first element in the sys q with dataReadyFlag value of FALSE
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetNextNeedingMountElem_SYSQUE(char *namePtr, int  mode, 
     int *pos)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetNextNeedingMountElem_SYSQUE %s mode %d incoming pos %d\n", 
   namePtr, mode, *pos);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

for (; *pos < nPtr->nsize; *pos++) {
 sysQueElemPtr = (sysQueElemType *)nPtr->arr[*pos];
 if (sysQueElemPtr == NULL) {
  unLockNameList_CORE(nPtr);
  return(NULL);
 }
 if (sysQueElemPtr->dataReadyFlag == FALSE) {
  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetNextNeedingMountElem_SYSQUE removing... pos %d name %s mode %d\n", *pos, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, *pos, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, *pos);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, *pos, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     *pos++;
#ifdef SYSQ
printf("GetNextNeedingMountElem_SYSQUE %s mode %d returning pos %d\n", 
   namePtr, mode, *pos);
#endif
     return(retSysQueElemPtr);
     break;
   case DO_COPY:
     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     unLockNameList_CORE(nPtr);
     *pos++;
#ifdef SYSQ
printf("GetNextNeedingMountElem_SYSQUE %s mode %d returning pos %d\n", 
   namePtr, mode, *pos);
#endif
     return(retSysQueElemPtr);
     break;
   default:
     /* force the pointer to NULL */
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     unLockNameList_CORE(nPtr);
     return(NULL);
     break;
  } /* end switch */
 } /* end if */
 } /* end for */

*pos = 0; /* start at beginning of the list next time */

#ifdef SYSQ
printf("GetNextNeedingMountElem_SYSQUE %s mode %d AT END returning pos %d\n", 
   namePtr, mode, *pos);
#endif

 unLockNameList_CORE(nPtr);
 return(NULL);

} /* GetNextNeedingMountElem_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  JobExists_SYSQUE
 *
 * DESCRIPTION:
 *  returns 1 if job exists in the subsystem queue; 0 if does not
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int JobExists_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(0);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(0);
    }
    if (sysQueElemPtr->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("leaving JobExists_SYSQUE %s job %d \n", namePtr, jobId);
#endif
      return(1);
    }
  }

  unLockNameList_CORE(nPtr);
  return(0);

} /* end JobExists_SYSQUE */


/*----------------------------------------------------------
 * NAME:
 *  SetHoldFlag_SYSQUE
 *
 * DESCRIPTION:
 *  set the hold flag for the subsystem queue element with 
 *  the specified job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetHoldFlag_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;


  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->holdFlag = TRUE;
#ifdef SYSQ
printf("SetHoldFlag_SYSQUE: %s job %d\n", namePtr, jobId);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end SetHoldFlag_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  ClearHoldFlag_SYSQUE
 *
 * DESCRIPTION:
 *  clear the hold flag for the subsystem queue element with 
 *  clear specified job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearHoldFlag_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;



  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->holdFlag = FALSE;
#ifdef SYSQ
printf("ClearHoldFlag_SYSQUE: %s job %d\n", namePtr, jobId);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ClearHoldFlag_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetDataReady_SYSQUE 
 *
 * DESCRIPTION:
 *  set the data ready flag for the subsystem queue element with 
 *  clear specified job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetDataReady_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->dataReadyFlag = TRUE;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end SetDataReady_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  ClearDataReady_SYSQUE 
 *
 * DESCRIPTION:
 *  clear the data ready flag for the subsystem queue element with 
 *  clear specified job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearDataReady_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->dataReadyFlag = FALSE;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ClearDataReady_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetFirstIdWithMedia_SYSQUE
 *
 * DESCRIPTION:
 *  return job id of first job with specified checkMediaId value
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetFirstIdWithMedia_SYSQUE(char *namePtr, int checkMediaVal)
{
 int  i;
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

#ifdef SYSQ
printf("GetFirstIdWithMedia_SYSQUE -- %s checkMediaVal %d\n", namePtr, checkMediaVal);
#endif

  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (sysQueElemPtr->checkMediaId == checkMediaVal) {
#ifdef SYSQ
printf("GetFirstIdWithMedia_SYSQUE--returning job %d\n",sysQueElemPtr->jobId);
#endif
     unLockNameList_CORE(nPtr);
     return(sysQueElemPtr->jobId);
   } 
  } 

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetFirstIdWithMedia_SYSQUE.............*/


/*----------------------------------------------------------
 * NAME:
 *  GetFirstIdWithStatus_SYSQUE
 *
 * DESCRIPTION:
 *  return job id of first job with specified status value
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetFirstIdWithStatus_SYSQUE(char *namePtr, int status)
{
 int  i;
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

#ifdef SYSQ
printf("GetFirstIdWithStatus_SYSQUE -- %s status %d\n", namePtr, status);
#endif

  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (GetStatus__MSTRQLIST(namePtr, sysQueElemPtr->jobId)  == status) {
#ifdef SYSQ
printf("GetFirstIdWithStatus_SYSQUE--returning job %d\n",sysQueElemPtr->jobId);
#endif
     unLockNameList_CORE(nPtr);
     return(sysQueElemPtr->jobId);
   }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetFirstIdWithStatus_SYSQUE.............*/



/*----------------------------------------------------------
 * NAME:
 *  GetFirstNonHoldElem_SYSQUE 
 *
 * DESCRIPTION:
 *  get first non-qc and non hold element in the subsystem queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetFirstNonHoldElem_SYSQUE(char *namePtr, int  mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
/* printf("GetFirstNonHoldElem_SYSQUE %s mode %d...", namePtr, mode);  */
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

for (i = 0; i < nPtr->nsize; i++) {
 sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
 if (sysQueElemPtr == NULL) {
  unLockNameList_CORE(nPtr);
  return(NULL);
 }
/*******
printf("GetFirstNonHoldElem_SYSQUE %s job %d checking qc %d hold %d\n",namePtr,
 sysQueElemPtr->jobId, sysQueElemPtr->qcFlag, sysQueElemPtr->holdFlag);
*****/

 if (sysQueElemPtr->qcFlag == FALSE && sysQueElemPtr->holdFlag == FALSE) {
  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetFirstNonHoldElem_SYSQUE removing... pos %d name %s mode %d\n", i, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, i, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, i);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, i, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     return(retSysQueElemPtr);
     break;
   case DO_COPY:
     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
/*****
printf("hooray - found a match with job %d\n", retSysQueElemPtr->jobId); 
     unLockNameList_CORE(nPtr);
*****/
#ifdef SYSQ
/* printf("returning %d\n", retSysQueElemPtr->jobId); */
#endif
     return(retSysQueElemPtr);
     break;
   default:
     /* force the pointer to NULL */
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     unLockNameList_CORE(nPtr);
     return(NULL);
     break;
  } /* end switch */
 } /* end if */
 } /* end for */
 unLockNameList_CORE(nPtr);
#ifdef SYSQ
/*printf("\n"); */
#endif
 return(NULL);

} /* GetFirstNonHoldElem_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  GetFirstActiveElem_SYSQUE 
 *
 * DESCRIPTION:
 *  get the first element of the subsystem queue denoted
 *  by "namePtr" with the QC flag set to false, the hold flag
 *  set to false, and the data ready flag set to true.  
 *  either remove the element from the list or
 *  return a copy of the element, based on mode parameter
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetFirstActiveElem_SYSQUE(char *namePtr, int  mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
printf("GetFirstActiveElem_SYSQUE %s mode %d...", namePtr, mode); 
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

for (i = 0; i < nPtr->nsize; i++) {
 sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
 if (sysQueElemPtr == NULL) {
  unLockNameList_CORE(nPtr);
  return(NULL);
 }
 if (sysQueElemPtr->qcFlag == FALSE && 
     sysQueElemPtr->holdFlag == FALSE &&
     sysQueElemPtr->dataReadyFlag == TRUE) {
  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetFirstActiveElem_SYSQUE removing... pos %d name %s mode %d\n", i, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, i, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, i);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, i, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     return(retSysQueElemPtr);
     break;
   case DO_COPY:
     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("returning %d\n", retSysQueElemPtr->jobId);
#endif
     return(retSysQueElemPtr);
     break;
   default:
     /* force the pointer to NULL */
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     unLockNameList_CORE(nPtr);
     return(NULL);
     break;
  } /* end switch */
 } /* end if */
 } /* end for */
 unLockNameList_CORE(nPtr);
#ifdef SYSQ
printf("\n");
#endif
 return(NULL);

} /* GetFirstActiveElem_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  GetFirstNonQCelem_SYSQUE 
 *
 * DESCRIPTION:
 *  get the first element of the subsystem queue denoted
 *  by "namePtr" with the QC flag set to false.
 *  either remove the element from the list or
 *  return a copy of the element, based on mode parameter
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *GetFirstNonQCelem_SYSQUE(char *namePtr, int  mode)
{
 void *retvalPtr;
 sysQueElemType *sysQueElemPtr, *retSysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

/*nancy: this routine (i think) needs to be replaces with GetFirstReady..*/

/***
  GetFirstReadyElem_SYSQUE(namePtr, mode);
  return;

****/

#ifdef SYSQ
printf("GetFirstNonQCelem_SYSQUE %s mode %d\n", namePtr, mode);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

for (i = 0; i < nPtr->nsize; i++) {
 sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
 if (sysQueElemPtr == NULL) {
  unLockNameList_CORE(nPtr);
  return(NULL);
 }
 if (sysQueElemPtr->qcFlag == FALSE) {
  switch(mode) {
   case DO_REMOVE:
#ifdef EXTRA_DEBUG
printf("GetFirstNonQCelem_SYSQUE removing... pos %d name %s mode %d\n", i, namePtr, mode);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, i, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, i);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, i, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     retSysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     return(retSysQueElemPtr);
     break;
   case DO_COPY:
     retSysQueElemPtr = CopyElemAtPrivate_SYSQUE(sysQueElemPtr);
     if (retSysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
     unLockNameList_CORE(nPtr);
     return(retSysQueElemPtr);
     break;
   default:
     /* force the pointer to NULL */
     printfLLog(LOG_DEBUG, SYSQ_INVALID_MODE);
     unLockNameList_CORE(nPtr);
     return(NULL);
     break;
  } /* end switch */
 } /* end if */
 } /* end for */
 unLockNameList_CORE(nPtr);
 return(NULL);

} /* GetFirstNonQCelem_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  GetQCstatus_SYSQUE 
 *
 * DESCRIPTION:
 *  returns TRUE if there are any QC elements in the queue
 *  returns FALSE otherwise
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetQCstatus_SYSQUE(char *namePtr)
{
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
/* printf("GetQCstatus_SYSQUE %s \n", namePtr); */
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(FALSE);

 for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(FALSE);
   }
   if (sysQueElemPtr->qcFlag == TRUE) 
     return(TRUE);

  }
  unLockNameList_CORE(nPtr);
  return(FALSE);

} /* GetQCstatus_SYSQUE.............*/

/*----------------------------------------------------------
 * NAME:
 *  GetHoldStatus_SYSQUE
 *
 * DESCRIPTION:
 *  returns TRUE if there are any hold elements in the queue
 *  returns FALSE otherwise
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetHoldStatus_SYSQUE(char *namePtr)
{
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
/*printf("GetHoldStatus_SYSQUE %s \n", namePtr); */
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(FALSE);

 for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(FALSE);
   }
   if (sysQueElemPtr->holdFlag == TRUE)
     return(TRUE);

  }
  unLockNameList_CORE(nPtr);
  return(FALSE);

} /* GetHoldStatus_SYSQUE.............*/


/*----------------------------------------------------------
 * NAME:
 *  GetErrorStatus_SYSQUE
 *
 * DESCRIPTION:
 *  returns TRUE if there are any error elements in the queue
 *  returns FALSE otherwise
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetErrorStatus_SYSQUE(char *namePtr)
{
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;
 int    i;

#ifdef SYSQ
printf("GetErrorStatus_SYSQUE %s \n", namePtr);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(FALSE);

 for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(FALSE);
   }
   if (sysQueElemPtr->holdFlag == TRUE)
     return(TRUE);

  }
  unLockNameList_CORE(nPtr);
  return(FALSE);

} /* GetErrorStatus_SYSQUE.............*/



/*----------------------------------------------------------
 * NAME:
 *  GetRepeatFlagGivenJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  get the repeat flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i, retval;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      retval = sysQueElemPtr->repeatFlag;
#ifdef SYSQ
printf("GetRepeatFlagGivenJobId_SYSQUE %s job %d returning %d\n", namePtr, jobId, retval);
#endif

      unLockNameList_CORE(nPtr);
      return(retval);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end GetRepeatFlagGivenJobId_SYSQUE...........*/



/*----------------------------------------------------------
 * NAME:
 *  SetRepeatFlagGivenJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  set the repeat flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->repeatFlag = TRUE;
#ifdef SYSQ
printf("SetRepeatFlagGivenJobId_SYSQUE %s job %d\n", namePtr, jobId);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end SetRepeatFlagGivenJobId_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  ClearRepeatFlagGivenJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  clear the repeat flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->repeatFlag = FALSE;
#ifdef SYSQ
printf("ClearRepeatFlagGivenJobId_SYSQUE %s job %d\n", namePtr, jobId);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end ClearRepeatFlagGivenJobId_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetMediaFlagGivenJobId_SYSQUE
 *
 * DESCRIPTION:
 *  get the 'check media id' flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetMediaFlagGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      return(sysQueElemPtr->checkMediaId );
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end GetMediaFlagGivenJobId_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetMediaFlagGivenJobId_SYSQUE
 *
 * DESCRIPTION:
 *  set the 'check media id' flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetMediaFlagGivenJobId_SYSQUE(char *namePtr, int jobId, int checkMediaVal)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->checkMediaId = checkMediaVal;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end SetMediaFlagGivenJobId_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  ClearMediaFlagGivenJobId_SYSQUE
 *
 * DESCRIPTION:
 *  clear the 'check media id' flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearMediaFlagGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->checkMediaId = MEDIA_CHECK_NO;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end ClearMediaFlagGivenJobId_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  SetQCflag_SYSQUE 
 *
 * DESCRIPTION:
 *  set the QC flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetQCflag_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->qcFlag = TRUE;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end SetQCflag_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  ClearQCflag_SYSQUE 
 *
 * DESCRIPTION:
 *  clear the QC flag for the subsystem queue element
 *  specified by job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearQCflag_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      sysQueElemPtr->qcFlag = FALSE;
      unLockNameList_CORE(nPtr);
      return(0);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end ClearQCflag_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetTypeStringGivenSysReq_SYSQUE 
 *
 * DESCRIPTION:
 *  get the structure element "type string" for the ith
 *  element of subsystem "namePtr" queue
 *
 * NOTES:
 *  may not be needed
 *
 *
 *---------------------------------------------------------*/
char *GetTypeStringGivenSysReq_SYSQUE(char *namePtr, int i)
{
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

  sysQueElemPtr =  nPtr->arr[i];
  if (sysQueElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  } 

  unLockNameList_CORE(nPtr);
  return(sysQueElemPtr->reqStrPtr);
} /* end GetTypeStringGivenSysReq_SYSQUE...........*/



/*----------------------------------------------------------
 * NAME:
 *  GetIndexGivenJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  get the actual position in the sysque for a given job id
 *  
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetIndexGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetIndexGivenJobId_SYSQUE %s jobId %d\n", namePtr, jobId);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      unLockNameList_CORE(nPtr);
      return(i);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end GetIndexGivenJobId_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetListPosGivenJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  get the list position in the sysque for a given job id
 *  
 *
 * NOTES:
 *  list pos starts at 1 so return queue idx + 1
 *---------------------------------------------------------*/
int GetListPosGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

#ifdef SYSQ
/*printf("GetListPosGivenJobId_SYSQUE %s jobId %d...\n", namePtr, jobId); */
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    sysQueElemPtr =  nPtr->arr[i];
    if (sysQueElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (sysQueElemPtr->jobId == jobId) {
      unLockNameList_CORE(nPtr);
#ifdef SYSQ
/* printf("GetListPosGivenJobId_SYSQUE returning pos %d\n", i+1); */
#endif
      return(i+1);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);
} /* end GetListPosGivenJobId_SYSQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetJobIdGivenListPos_SYSQUE 
 *
 * DESCRIPTION:
 *  get the job id in the sysque for a given list position 
 *
 * NOTES:
 *  list positions start at 1 
 *---------------------------------------------------------*/
int GetJobIdGivenListPos_SYSQUE(char *namePtr, int listPos)
{
 sysQueElemType *sysQueElemPtr;
 int jobId;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetJobIdGivenListPos_SYSQUE %s listpos %d\n", namePtr, listPos);
#endif

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(-1);

  if (listPos > nPtr->nsize || listPos == 0) {
    unLockNameList_CORE(nPtr);
    return(-1);
  }
  sysQueElemPtr =  nPtr->arr[listPos - 1];
  if (sysQueElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
  }

  jobId = sysQueElemPtr->jobId;

  unLockNameList_CORE(nPtr);
  return(jobId);
} /* end GetJobIdGivenListPos_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetReqGivenListPos_SYSQUE
 *
 * DESCRIPTION:
 *  get the subsystem queue element for a given list position
 *
 * NOTES:
 *  list positions start at 1
 *---------------------------------------------------------*/
sysQueElemType *GetReqGivenListPos_SYSQUE(char *namePtr, int listPos)
{
 sysQueElemType *sqel;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetReqGivenListPos_SYSQUE %s listpos %d\n", namePtr, listPos);
#endif

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

  if (listPos > nPtr->nsize || listPos == 0) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }
  sqel =  nPtr->arr[listPos - 1];
  if (sqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

  unLockNameList_CORE(nPtr);
  return(sqel);
} /* end GetReqGivenListPos_SYSQUE...........*/



/*----------------------------------------------------------
 * NAME:
 *  GetSysReqGivenString_SYSQUE 
 *
 * DESCRIPTION:
 *  get the system request corresponding to "stringPtr" for  
 *  subsystem "namePtr" queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
ODL GetSysReqGivenString_SYSQUE(char *namePtr, char *stringPtr)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetSysReqGivenString_SYSQUE %s stringPtr %d\n", namePtr, stringPtr);
#endif
  nPtr = lockNameList_CORE(namePtr);

  /* find the sysODLreq in the list */
  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];

   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(stringPtr, sysQueElemPtr->reqStrPtr) == 0) {
     unLockNameList_CORE(nPtr);
     return(sysQueElemPtr->sysODLreq);
   }
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetSysReqGivenString_SYSQUE...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetSysReqGivenJobId_SYSQUE
 *
 * DESCRIPTION:
 *  get the system request corresponding to "stringPtr" for
 *  subsystem "namePtr" queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
ODL GetSysReqGivenJobId_SYSQUE(char *namePtr, int jobId)
{
 sysQueElemType *sysQueElemPtr;
 int i;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("GetSysReqGivenJobId_SYSQUE %s jobId %d\n", namePtr, jobId);
#endif
  nPtr = lockNameList_CORE(namePtr);

  /* find the sysODLreq in the list */
  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];

   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (sysQueElemPtr->jobId == jobId) {
     unLockNameList_CORE(nPtr);
     return(sysQueElemPtr->sysODLreq);
   }
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetSysReqGivenJobId_SYSQUE...........*/



/*----------------------------------------------------------
 * NAME:
 *  RemoveElemWithJobId_SYSQUE 
 *
 * DESCRIPTION:
 *  remove the element of the subsystem queue denoted 
 *  by "namePtr" and job id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
sysQueElemType *RemoveElemWithJobId_SYSQUE(char *namePtr, int  jobId)
{
 void *retvalPtr;
 int  i;
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
      return(NULL);

#ifdef SYSQ
printf("RemoveElemWithJobId_SYSQUE -- %s jobId %d\n", namePtr, jobId);
#endif

  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (sysQueElemPtr->jobId == jobId) { 
#ifdef EXTRA_DEBUG
printf("RemoveElemWithJobId_SYSQUE removing... pos %d name %s jobId %d\n", i, namePtr, jobId);
     printfLLog(LOG_DEBUG, SYSQ_REMOVING, i, namePtr);
#endif
     retvalPtr = RemoveNameListElemAt_CORE(nPtr, namePtr, i);
     if (retvalPtr == NULL) {
      printfLLog(LOG_ERR, SYSQ_NO_REMOVE, i, namePtr);
      unLockNameList_CORE(nPtr);
      return(NULL);
     }
/*     FreeSysQueElement_SYSQUE(namePtr, sysQueElemPtr); */
     sysQueElemPtr = (sysQueElemType *)retvalPtr;
     unLockNameList_CORE(nPtr);
     return(sysQueElemPtr);
   } /* end if jobId */
  } /* end for */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* RemoveElemWithJobId_SYSQUE.............*/


/*----------------------------------------------------------
 * NAME:
 *  MoveElemPrivate_SYSQUE 
 *
 * DESCRIPTION:
 *  a private routine to move an element from 
 *  "elemFrom" to "elemTo"
 *
 * NOTES:
 *  NOT-VISIBLE:: this routine is locked
 *                before it is entered
 *
 *---------------------------------------------------------*/
int  MoveElemPrivate_SYSQUE(baseListElemType *nPtr, int elemTo, int elemFrom )
{
 sysQueElemType *sysQueElemPtr;
 int j, retval;

#ifdef SYSQ
printf("MoveElemPrivate_SYSQUE from %d to %d\n", elemFrom, elemTo);
#endif
  for (j = 0; j < nPtr->nsize; j ++) {
    sysQueElemPtr = (sysQueElemType *)nPtr->arr[j];
    printf("unmoved  num = %d\n", sysQueElemPtr->jobId);
  }
  retval = MoveToPos_CORE(nPtr, elemTo, elemFrom);
 
  for (j = 0; j < nPtr->nsize; j ++) {
    sysQueElemPtr = (sysQueElemType *)nPtr->arr[j];
    printf("moved  num = %d\n", sysQueElemPtr->jobId);
  }

  return(retval);

} /* end MoveElemPrivate_SYSQUE...............*/


/*----------------------------------------------------------
 * NAME:
 *  MoveElem_SYSQUE 
 *
 * DESCRIPTION:
 *  the public routine to move an element from 
 *  "elemFrom" to "elemTo"
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int  MoveElem_SYSQUE(char *namePtr, int  elemTo, int  elemFrom )
{
 baseListElemType *nPtr;
 int retval;

#ifdef SYSQ
printf("MoveElem_SYSQUE from %d to %d \n", elemFrom, elemTo);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
     return(-1);

  retval = MoveElemPrivate_SYSQUE(nPtr,elemTo, elemFrom);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveElem_SYSQUE */

/*----------------------------------------------------------
 * NAME:
 *  MoveToHead_SYSQUE 
 *
 * DESCRIPTION:
 *  move element "elemToMove" to head of the subsystem queue
 *  denoted by "namePtr"
 *
 * NOTES:
 *  lock is performed in MoveElem_ 
 *
 *---------------------------------------------------------*/
int  MoveToHead_SYSQUE(char *namePtr, int elemToMove )
{
 int retval;
 baseListElemType *nPtr;

#ifdef SYSQ
printf("MoveToHead_SYSQUE to %d \n", elemToMove);
#endif
  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
     return(-1);

  retval = MoveElemPrivate_SYSQUE(nPtr,0, elemToMove);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveToHead_SYSQUE..............*/


/*----------------------------------------------------------
 * NAME:
 *  MoveToTail_SYSQUE 
 *
 * DESCRIPTION:
 *  move element "elemToMove" to tail of the subsystem queue
 *  denoted by "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int  MoveToTail_SYSQUE(char *namePtr, int elemToMove )
{
 int retval;
 baseListElemType *nPtr;
#ifdef SYSQ
printf("MoveToTail_SYSQUE to %d \n", elemToMove);
#endif

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
     return(-1);
  
  retval = MoveElemPrivate_SYSQUE(nPtr, nPtr->nsize-1, elemToMove);

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end MoveToTail_SYSQUE..............*/
