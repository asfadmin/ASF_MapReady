#define DISALLOW_DELETED /* don't allow 'cancel job' when job being deleted*/



#define REALLY_DELETE /* removes elements RIGHT NOW! */
#define STOP_BEFORE_EXIT /* enable this to require user to stop all 
                               subsystems manually before exiting the cp */
static char sccsid_cpmenu_c[] = "@(#)cpmenu.c	1.64 97/04/02 12:12:51";


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <unistd.h>

#include <string.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/FileSB.h>
#include "UxXt.h"
#include "asfcommon.h"
#include "odl.h"
#include "memUtils.h"
#include "logUtils.h"
#include "cpdefines.h"
#include "cplogs.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"
#include "que_sys.h"

#include "cpconfig.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "cpmenu.h"
#include "cpmainrtns.h"
#include "cpworkProc.h"
#include "validate.h"
#include "xutils.h"

extern Widget CPfileBox;
extern Widget CPmainJobIdList;
extern Widget rdsMountDialog;
extern Widget aspMountDialog;
extern void clearQueueID(ODL sysODLreq,char *namePtr);
extern void adjustQueueID(mstrqListElemType *mqel,char *namePtr);
int GLOBAL_saveRestoreFlag;

extern int totalMQentries;

/* ######################## TEST CODE ############################### */
#ifdef NEW_MOVE /* this code is not used yet: it's in place for move job mods */
void sensitizeEligibleMenuItems(Widget mainWid)
{
 int i, posCount, *posList, jobId, status, sqPos;
 Widget listWid;
 char *namePtr, *destSubsys = NULL;
 mstrqListElemType *mqel;


  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr == NULL)   /* is the CP window, not a subsystem */
      namePtr = ASF_CP;

printf("sensitizeEligibleMenuItems name %s\n", namePtr);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);
  if (!XmListGetSelectedPos(listWid, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "moving");
    return;
  }

printf("sensitizeEligibleMenuItems listWid %s, pos %d\n", XtName(listWid), posList[0]);

  if (strcmp(namePtr , ASF_CP) == NULL) { /* get mqel for CP window */
printf("sensitizeEligibleMenuItems for CP\n");
    mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
    if (mqel == NULL) {
     printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
     return;
    }
    jobId = mqel->jobId;
printf("sensitizeEligibleMenuItems jobid %d\n", jobId);
  }
  else { /* subsystem */
printf("sensitizeEligibleMenuItems for subsys %s\n", namePtr);
    jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
printf("sensitizeEligibleMenuItems jobid %d\n", jobId);
    if (jobId == -1) {
     printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
     return;
    }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Moving",  posList[0], jobId);
  status = GetStatus__MSTRQLIST(namePtr, jobId);

  mqel = GetCopyGivenJobId__MSTRQLIST(jobId);
  }

printf("found mqel for job %d\n", mqel->jobId);


} /* sensitiveElegibleMenuItems */
#endif
/* ######################## END TEST CODE ############################### */


/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuExitCB
 *
 * DESCRIPTION:
 *  handle the closing of the subsystem window from the window manager
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleSubsysMenuExitCB(Widget w,XtPointer client_data,XtPointer call_data)
{
 Widget mainWid = (Widget) client_data;
 char *namePtr;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr == NULL)
    return;
  else
    handleMenuStop(mainWid);

} /* end handleSubsysMenuExitCB......................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMenuPrintCB
 *
 * DESCRIPTION:
 *  print a screen dump using the print command specified
 *  in the configuration file
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuPrintCB(Widget w, XtPointer client_data, XtPointer cb_arg)
{
  char cmd[256];

   sprintf(cmd, getPrintCommand(), XtWindow(w) );
   system(cmd);

} /* handleMenuPrintCB */

/*----------------------------------------------------------
 * NAME:
 *  eligibleToMove
 *
 * DESCRIPTION:
 *  check whether a job is eligible to be moved from
 *  one subsystem queue to another.  returns NULL if not eligible
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static char *eligibleToMove(mstrqListElemType *mqel)
{
  int cat, msgTypeId, modeId, isJ1 = 0, state;
  char *msgTypeStr, *modeStr, *platformStr, *destSubsys = NULL;

                  /* only allow move of jobs that are 'waiting' on the queue */
  state = GetStatus__MSTRQLIST(mqel->namePtr, mqel->jobId) ;
  if (state != Q_S_PLACED_ON_SYSQ_ID && 
      state != Q_M_INTERRUPTED_ID &&
      state != Q_M_PROCESSING_RESET_ID &&
      state != Q_M_PLACED_ON_HOLD_ERROR_ID &&
      state != Q_M_PLACED_ON_HOLD_DATA_ID) {
    printfLLog(LOG_ERR, JOB_MOVE_BAD_STATE, mqel->jobId, 
                         GetProcessStateAsText(state));
    return(NULL);
  }

  cat = GetSubsystemCategoryGivenName(mqel->namePtr);
  if ((ODLGetVal(mqel->mstrqODLreq, HDR_MSG_TYPE, &msgTypeStr)) == 0) 
    return(NULL);
  if ((ODLGetVal(mqel->mstrqODLreq, BODY_MODE, &modeStr)) == 0) 
    return(NULL);
  if ((ODLGetVal(mqel->mstrqODLreq, BODY_PLATFORM, &platformStr)) == 0) 
    return(NULL);
  isJ1 = strcmp(platformStr, SPS_platforms[SPS_platform_J1]) == 0;


  destSubsys = NULL;
  msgTypeId = getTypeId(msgTypeStr);
  modeId = getModeId(modeStr);
  if (msgTypeId == REQ_TYPE_SCAN_ID) { /* only allow RDS continuous scans */
    if (cat == RDS_CATEGORY_ID) {
        destSubsys = getNextRDSname(getRDSnumGivenName(mqel->namePtr));
        if ((destSubsys == NULL) && (modeId == MODE_CONTINUOUS_ID)) {
            destSubsys = getASPname();
        }
    }
                    /* deny ASP J1 scans when "SINGLE" routing specified */
    else if (cat == ASP_CATEGORY_ID && !(isJ1 && special_J1_routing()) )
      destSubsys = getRDSname(0);
  }
  else if (cat == RDS_CATEGORY_ID) {
      destSubsys = getNextRDSname(getRDSnumGivenName(mqel->namePtr));
  }
  else if (cat == SSP2_CATEGORY_ID) {
    destSubsys = getNextSSPname(getSSP2numGivenName(mqel->namePtr));
  }

  if (destSubsys == NULL) {
    printfLLog(LOG_ERR, JOB_MOVE_BAD_DEST, msgTypeStr, mqel->jobId, modeStr);
    return(NULL);
  }

  return(destSubsys);

} /* eligibleToMove */


/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuMoveJob
 *
 * DESCRIPTION:
 *  move an item from one subsystem queue to another
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleSubsysMenuMoveJob(Widget mainWid)
{
 int i, posCount, *posList, jobId, status, sqPos,cat;
 Widget listWid;
 char *namePtr, *destSubsys = NULL;
 mstrqListElemType *mqel;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);

  if (!XmListGetSelectedPos(listWid, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, 
                                          "moving between subsystem queues");
    return;
  }

  jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
  if (jobId == -1) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Moving",  posList[0], jobId);


  mqel = GetCopyGivenJobId__MSTRQLIST(jobId);

  if ((destSubsys = eligibleToMove(mqel)) == NULL) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, JOB_MOVE_INVALID, mqel->jobId);
    return;
  }

/* if got to here, we've got a valid entry to move */

/* remove the entry from the original subsystem queue and subsys display */
  sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, jobId);
  FreeSysQueElemGivenPos_SYSQUE(mqel->namePtr, sqPos);
  RemoveElemWithJobId_SYSQUE(mqel->namePtr, jobId);
  removeLineForJobId(mqel->namePtr, jobId);

  cat = GetSubsystemCategoryGivenName(mqel->namePtr);
  if (cat == RDS_CATEGORY_ID) {
      printf(" *** in  new code\n");
      clearQueueID( mqel->mstrqODLreq,mqel-> namePtr);
      adjustQueueID(mqel,destSubsys);
  }

/* add new job to the destination subsystem queue */
  AddSysReqToQue_SYSQUE(destSubsys, NOT_SET, jobId,
         FALSE, FALSE, mqel->mstrqODLreq, mqel->dataReadyFlag);
/* add the display items to the destination subsystem queue */
  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) 
    ASFlogMessageInsert_direct(destSubsys, i,
       GetListPosGivenJobId_SYSQUE(destSubsys,jobId),
              mqel->mqFmt[i], mqel->mqLabel[i]);

/* change the destination subsystem in the master queue to the new dest */
  ChangeStatus__MSTRQLIST(jobId, destSubsys, Q_S_PLACED_ON_SYSQ_ID);

/* update the queue status label in the destination subsys queue */
  updateStatusLabel(destSubsys,
      GetListPosGivenJobId_SYSQUE(destSubsys, mqel->jobId),
      Q_S_PLACED_ON_SYSQ_ID, destSubsys);

/* update the queue status label in the main window */
  updateStatusLabel(ASF_CP, GetDispPos_MSTRQLIST(jobId),
             Q_S_PLACED_ON_SYSQ_ID, destSubsys);

} /* end handleSubsysMenuMoveJob......................................*/

/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuRemoveHold
 *
 * DESCRIPTION:
 *  remove the hold status of an item in the subsystem queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleSubsysMenuRemoveHold(Widget mainWid)
{
 int     posCount, *posList;
 Widget listWid;
 char   *namePtr;
 int jobId, status;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);

  if (!XmListGetSelectedPos(listWid, &posList, &posCount)) {
    /*printfLLog(LOG_ERR, LIST_SEL_ERROR); */
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "hold removal");
    return;
  }

  jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
  if (jobId == -1) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Removing Hold", posList[0], jobId);

  status = GetStatus__MSTRQLIST(namePtr, jobId);
  if (status != Q_M_INPUT_SOURCE_NOT_READY_ID &&
      status != Q_M_PLACED_ON_HOLD_DATA_ID &&
      status != Q_M_PLACED_ON_HOLD_ERROR_ID) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, CANNOT_REMOVE_HOLD);
   return;
  }

  if (ChangeStatus__MSTRQLIST(jobId, namePtr, Q_M_PLACED_ON_SYSQ_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);
  else {
    ClearHoldFlag_SYSQUE(namePtr, jobId);
    SetRepeatFlagGivenJobId_SYSQUE( namePtr, jobId);
  }

} /* end handleSubsysMenuRemoveHold......................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMenuPause
 *
 * DESCRIPTION:
 *  handle the "Pause" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuPause(Widget mainWid)
{
 char *namePtr;
 dialogCBdata *cbData;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr ==  NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_PROCNAME_TO, "Pause\n");
   return;
  }
  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_PAUSING_ID;
  cbData->buttonWid = NULL;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_PAUSE_TEXT, cbData->namePtr);


} /* end handleMenuPause......................................*/

/*----------------------------------------------------------
 * NAME:
 *  handleMenuReset
 *
 * DESCRIPTION:
 *  handle the "reset" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuReset(Widget mainWid)
{
 char *namePtr;
 dialogCBdata *cbData;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr ==  NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_PROCNAME_TO, "reset\n");
   return;
  }
  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_HALTING_ID;
  cbData->buttonWid = NULL;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_HALT_TEXT, cbData->namePtr);


} /* end handleMenuReset......................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMenuStop
 *
 * DESCRIPTION:
 *  handle the "exit" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuStop(Widget mainWid)
{
 char *namePtr;
 dialogCBdata *cbData;
#ifdef NO_STOP_DURING_PROCESSING
 int retval;
#endif

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr ==  NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_PROCNAME_TO, "stop processing\n");
   return;
  }

#ifdef NO_STOP_DURING_PROCESSING
  retval = GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_M_PROCESSING_ID);
  if (retval == -1) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, JOB_IN_PROCESS_STOP, namePtr);
    return;
  }
#endif


  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_STOPPING_ID;
  cbData->buttonWid = NULL;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_STOP_TEXT, cbData->namePtr);


} /* end handleMenuStop......................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuReady
 *
 * DESCRIPTION:
 *  handle the "ready" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleSubsysMenuReady(Widget mainWid)
{
 char *namePtr;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr ==  NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_PROCNAME_TO, "start processing\n");
    return;
  }

           /* only do something if current state is 'waiting' */
  if (GetProcessState_PIDLIST(namePtr) == SUBSYS_WAITING_ID) {
    ChangeProcessState_PIDLIST(namePtr, SUBSYS_READY_ID, NOT_SET_ID);
    ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_READY_TEXT);
  }

} /* end handleSubsysMenuReady......................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleSubsysPreQCspawn()
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int handleSubsysPreQCspawn(char *namePtr, int jobId)
{
 qcReqType qcReq;
 int status;
 char   *imageFileNamePtr, *leaderFileNamePtr, *avgFileNamePtr;

  printfLLog(LOG_INFO, AUTO_IMAGE_AVG, namePtr, jobId);

  if ((status = GetStatus__MSTRQLIST(namePtr, jobId)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "mstrq status for jobId", jobId);
    return (0);
  }

  printfLLog(LOG_DEBUG, JOB_STATUS, jobId, status);
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&
     status != Q_M_IMAGE_PRE_QC_ID &&
     status != Q_M_SCAN_READY_FOR_QC_ID &&
     status != Q_S_IMAGE_QC_ON_HOLD_ID &&
     status != Q_M_IMAGE_QC_ON_HOLD_ID) {
    printfLLog(LOG_ERR, REQUEST_NOT_READY, "for QC");
    return (0);
  }

  if (GetQCFileNames_MSTRQLIST(jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &avgFileNamePtr) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
    return (0);
  }

  printfLLog(LOG_DEBUG, QC_LEADER_IMAGE, leaderFileNamePtr, imageFileNamePtr);
  strcpy(qcReq.namePtr , namePtr);
  qcReq.jobId   = jobId;
  qcReq.qcType   = QC_TYPE_IMAGE;

  strcpy(qcReq.imageFileBuf, imageFileNamePtr);
  strcpy(qcReq.leaderFileBuf,leaderFileNamePtr);
  strcpy(qcReq.avgFileBuf,avgFileNamePtr);
  /*strcpy(qcReq.avgFileBuf,leaderFileNamePtr);
  qcReq.avgFileBuf[strlen(qcReq.avgFileBuf)-1] = 'A'; */

  return(spawnPreQC_CB(&qcReq));

} /* end handleSubsysPreQCspawn.............................*/

/*----------------------------------------------------------
 * NAME:
 *  handleSubsysAutoScanQCspawn()
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int handleSubsysAutoScanQCspawn(char *namePtr, int jobId)
{
 qcReqType qcReq;
 int     status;
 char   *scanResultsFileNamePtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return(0);

  printfLLog(LOG_INFO, AUTO_SCAN_QC, namePtr, jobId);

  if ((status = GetStatus__MSTRQLIST(namePtr, jobId)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "mstrq status for jobId", jobId);
    unLockNameList_CORE(nPtr);
    return (0);
  }

  printfLLog(LOG_DEBUG, JOB_STATUS, jobId, status);
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&
     status != Q_M_SCAN_READY_FOR_QC_ID &&
     status != Q_S_IMAGE_QC_ON_HOLD_ID &&
     status != Q_M_IMAGE_QC_ON_HOLD_ID) {
    printfLLog(LOG_ERR, REQUEST_NOT_READY, "for QC");
    unLockNameList_CORE(nPtr);
    return (0);
  }

  if (GetScanResultsFileName_MSTRQLIST(jobId,&scanResultsFileNamePtr)==-1){
    printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
    unLockNameList_CORE(nPtr);
    return (0);
  }

  printfLLog(LOG_DEBUG, QC_SCAN_RESULTS, scanResultsFileNamePtr);
  strcpy(qcReq.namePtr , namePtr);
  qcReq.jobId   = jobId;
  qcReq.qcType   = QC_TYPE_SCAN;
  strcpy(qcReq.scanResultsFileBuf, scanResultsFileNamePtr);

  unLockNameList_CORE(nPtr);
  return(spawnScanQC_CB(&qcReq));

} /* end handleSubsysAutoScanQCspawn.............................*/

/*----------------------------------------------------------
 * NAME:
 *  handleSubsysAutoQCspawn()
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int handleSubsysAutoQCspawn(char *namePtr, int jobId)
{
 qcReqType qcReq;
 int     status;
 char   *imageFileNamePtr, *leaderFileNamePtr, *avgFileNamePtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return(0);

  printfLLog(LOG_INFO, AUTO_QC, namePtr, jobId);

  if ((status = GetStatus__MSTRQLIST(namePtr, jobId)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "mstrq status for jobId", jobId);
    unLockNameList_CORE(nPtr);
    return (0);
  }

  printfLLog(LOG_DEBUG, JOB_STATUS, jobId, status);
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&
     status != Q_M_IMAGE_PRE_QC_ID &&
     status != Q_M_SCAN_READY_FOR_QC_ID &&
     status != Q_S_IMAGE_QC_ON_HOLD_ID &&
     status != Q_M_IMAGE_QC_ON_HOLD_ID) {
    printfLLog(LOG_ERR, REQUEST_NOT_READY, "for QC");
    unLockNameList_CORE(nPtr);
    return (0);
  }

  if (GetQCFileNames_MSTRQLIST(jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &avgFileNamePtr) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
    unLockNameList_CORE(nPtr);
    return (0);
  }

  printfLLog(LOG_DEBUG, QC_LEADER_IMAGE, leaderFileNamePtr, imageFileNamePtr);
  strcpy(qcReq.namePtr , namePtr);
  qcReq.jobId   = jobId;
  qcReq.qcType   = QC_TYPE_IMAGE;

  strcpy(qcReq.imageFileBuf, imageFileNamePtr);
  strcpy(qcReq.leaderFileBuf,leaderFileNamePtr);
  strcpy(qcReq.avgFileBuf,avgFileNamePtr);

  unLockNameList_CORE(nPtr);
  return(spawnQC_CB(&qcReq));

} /* end handleSubsysAutoQCspawn.............................*/



/*----------------------------------------------------------
 * NAME:
 *  handleMainMenuPerformQC()
 *
 * DESCRIPTION:
 *  handle the "Perform QC" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void handleMainMenuPerformQC()
{
 qcReqType qcReq;
 int     posCount, *posList, status, mqPos, sqPos;
 char *imageFileNamePtr, *leaderFileNamePtr, *scanResultsFileNamePtr;
 char *avgFileNamePtr;
 mstrqListElemType *mqel;

  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "Q/C");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }

  if ((status = GetStatus__MSTRQLIST(mqel->namePtr, mqel->jobId)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "mstrq status for jobId", mqel->jobId);
    return;
  }

  printfLLog(LOG_DEBUG, JOB_STATUS, mqel->jobId, status);
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&
     status != Q_M_IMAGE_PRE_QC_ID &&
      status != Q_M_SCAN_READY_FOR_QC_ID &&
      status != Q_S_IMAGE_QC_ON_HOLD_ID &&
      status != Q_M_IMAGE_QC_ON_HOLD_ID) {
    printfLLog(LOG_ERR, REQUEST_NOT_READY, "for QC");
    return;
  }

  strcpy(qcReq.namePtr , mqel->namePtr);
  qcReq.jobId   = mqel->jobId;
  qcReq.qcType   = mqel->qcType;

  mqPos = GetDispPos_MSTRQLIST(mqel->jobId);
  sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);

  if (qcReq.qcType == QC_TYPE_SCAN) {
    if (GetScanResultsFileName_MSTRQLIST(mqel->jobId,
                                         &scanResultsFileNamePtr) == -1) {
      printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
      return ;
    }
    printfLLog(LOG_DEBUG, QC_SCAN_RESULTS, scanResultsFileNamePtr);
    strcpy(qcReq.scanResultsFileBuf, scanResultsFileNamePtr);

    printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, PERFORMING_SCAN_QC, 
               posList[0], mqel->jobId);
    if (spawnScanQC_CB(&qcReq) == 1) {
      updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, Q_M_PERFORMING_QC_TEXT);
      updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos, 
                   Q_M_PERFORMING_QC_TEXT);
    }
  ClearHoldFlag_SYSQUE( mqel->namePtr, mqel->jobId);
  }
  else if (qcReq.qcType == QC_TYPE_IMAGE) {  /* image qc file */
  if (GetQCFileNames_MSTRQLIST(mqel->jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &avgFileNamePtr) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
    return;
    }
    printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, PERFORMING_QC,
               posList[0], mqel->jobId);
    printfLLog(LOG_DEBUG, QC_LEADER_IMAGE, leaderFileNamePtr, imageFileNamePtr);
    strcpy(qcReq.imageFileBuf, imageFileNamePtr);
    strcpy(qcReq.leaderFileBuf,leaderFileNamePtr);
    strcpy(qcReq.avgFileBuf,avgFileNamePtr);
    if (spawnQC_CB(&qcReq) == 1) {
      updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, Q_M_PERFORMING_QC_TEXT);
      updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos,
                   Q_M_PERFORMING_QC_TEXT);
    }
  ClearHoldFlag_SYSQUE( mqel->namePtr, mqel->jobId);
  }
  else /* cannot perform qc on this job */
    printfLLog(LOG_DEBUG, CANNOT_PERFORM_QC, mqel->namePtr, mqel->jobId);

  return;
} /* end handleMainMenuPerformQC.............................*/

/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuQCspawn()
 *
 * DESCRIPTION:
 *  handle the "Perform QC" menu selection from a subsystem queue window
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void handleSubsysMenuQCspawn(Widget mainWid)
{
 qcReqType qcReq;
 int     posCount, *posList, jobId, status, mqPos, sqPos;
 Widget listWid;
 char *namePtr, *imageFileNamePtr, *leaderFileNamePtr, *scanResultsFileNamePtr;
 char *avgFileNamePtr;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);

  if (!XmListGetSelectedPos(listWid, &posList, &posCount)) {
    /*printfLLog(LOG_DEBUG, LIST_SEL_ERROR); */
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "Q/C");
    return;
  }
  jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
  if (jobId == -1) {
    printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
    return;
  }

  if ((status = GetStatus__MSTRQLIST(namePtr, jobId)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "mstrq status for jobId", jobId);
    return;
  }


  printfLLog(LOG_DEBUG, JOB_STATUS, jobId, status);
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&
     status != Q_M_IMAGE_PRE_QC_ID &&
      status != Q_M_SCAN_READY_FOR_QC_ID &&
      status != Q_S_IMAGE_QC_ON_HOLD_ID &&
      status != Q_M_IMAGE_QC_ON_HOLD_ID) {
    printfLLog(LOG_ERR, REQUEST_NOT_READY, "for QC");
    return;
  }

  strcpy(qcReq.namePtr , namePtr);
  qcReq.jobId   = jobId;
  qcReq.qcType   = GetQCtype_MSTRQLIST(jobId);

  mqPos = GetDispPos_MSTRQLIST(jobId);
  sqPos = GetListPosGivenJobId_SYSQUE(namePtr, jobId);

  if (qcReq.qcType == QC_TYPE_SCAN) {
    if (GetScanResultsFileName_MSTRQLIST(jobId,&scanResultsFileNamePtr)==-1){
      printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
      return ;
    }
    printfLLog(LOG_DEBUG, QC_SCAN_RESULTS, scanResultsFileNamePtr);
    strcpy(qcReq.scanResultsFileBuf, scanResultsFileNamePtr);

    printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, PERFORMING_SCAN_QC,
               posList[0], jobId);
    if (spawnScanQC_CB(&qcReq) == 1) {
      updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, Q_M_PERFORMING_QC_TEXT);
      updatePosLabel(namePtr, QUE_STATUS_LIST, sqPos, Q_M_PERFORMING_QC_TEXT);
    }
  ClearHoldFlag_SYSQUE( namePtr, jobId);
  }
  else if (qcReq.qcType == QC_TYPE_IMAGE) {  /* image qc file */
  if (GetQCFileNames_MSTRQLIST(jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &avgFileNamePtr) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
    return;
    }
    printfLLog(LOG_DEBUG, QC_LEADER_IMAGE, leaderFileNamePtr, imageFileNamePtr);
    strcpy(qcReq.imageFileBuf, imageFileNamePtr);
    strcpy(qcReq.leaderFileBuf,leaderFileNamePtr);
    strcpy(qcReq.avgFileBuf,avgFileNamePtr);
    printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, PERFORMING_QC, posList[0], jobId);
    if (spawnQC_CB(&qcReq) == 1) {
      updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, Q_M_PERFORMING_QC_TEXT);
      updatePosLabel(namePtr, QUE_STATUS_LIST, sqPos, Q_M_PERFORMING_QC_TEXT);
    }
  ClearHoldFlag_SYSQUE( namePtr, jobId);
  }
  else /* cannot perform qc on this job */
    printfLLog(LOG_DEBUG, CANNOT_PERFORM_QC, namePtr, jobId);

  return;
} /* end handleSubsysMenuQCspawn.............................*/


/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuDetailedInfo()
 *
 * DESCRIPTION:
 *  handle the "show detailed info" menu selection by popping up
 *  a box with detailed information on the selected queue list item
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleSubsysMenuDetailedInfo(Widget mainWid, XtPointer client_data, 
                                  XtPointer cb_data)
{
 int     posCount, *posList;
 Widget listWid;
 char   *namePtr;
 int jobId;
 mstrqListElemType *mqel;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);

  if (!XmListGetSelectedPos(listWid, &posList, &posCount) ) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, 
                                                "detailed information");
    return;
  }

  jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
  if (jobId == -1) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
   return;
  }
  mqel = GetCopyGivenJobId__MSTRQLIST(jobId);
  showJobInfo(mqel->jobId, mqel->namePtr, mqel->mstrqODLreq);

  FreeMstrqListElem_MSTRQLIST(mqel);

} /* end handleSubsysMenuDetailedInfo.............................*/

/*----------------------------------------------------------
 * NAME:
 *  handleSubsysMenuDelete()
 *
 * DESCRIPTION:
 *  handle the "Remove Selected Job" subsystem menu selection
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleSubsysMenuDelete(Widget mainWid)
{
 int     posCount, *posList, sqPos;
 Widget listWid;
 char   *namePtr;
 char *productType, *compensated;
 int cat, jobId, status;
 sysQueElemType *sqel;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  listWid = GetJobIdListGivenName_PIDLIST(namePtr);

  if (!XmListGetSelectedPos(listWid, &posList, &posCount)) {
   /* printfLLog(LOG_ERR, LIST_SEL_ERROR); */
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "deletion");
    return;

  }
  jobId = GetJobIdGivenListPos_SYSQUE(namePtr, posList[0]);
  if (jobId == -1) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "jobId");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Deleting", posList[0], jobId);

  status = GetStatus__MSTRQLIST(namePtr, jobId);
  if (status == Q_M_PROCESSING_ID) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, JOB_IN_PROCESS_REMOVE);
   return;
  }
#ifdef DISALLOW_DELETED
  else if (status == Q_M_ENTRY_DELETED_ID) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, "Job already being deleted");
    return;
  }
#endif

  cat = GetSubsystemCategoryGivenName(namePtr);
  if (status == Q_M_PLACED_ON_SYSQ_ID || 
           status == Q_S_PLACED_ON_SYSQ_ID ||
           status == Q_M_GOT_ITEM_OFF_ID ||
           status == Q_M_INPUT_SOURCE_NOT_READY_ID ||
           status == Q_M_PLACED_ON_HOLD_DATA_ID) {
     if (cat == RDS_CATEGORY_ID)
       popdownMsgDialog(rdsMountDialog);
     else if (cat == ASP_CATEGORY_ID)
       popdownMsgDialog(aspMountDialog);

  }

  if (cat == SSP2_CATEGORY_ID) {
    productType = GetProductType_MSTRQLIST(jobId);
    compensated = GetCompensationType_MSTRQLIST(jobId);
 
    if (productType == NULL)
      printfLLog(LOG_ERR, CANNOT_GET, "product type");
    else if (compensated == NULL)
      printfLLog(LOG_ERR, CANNOT_GET, "compensation type");
    else {
      decrementSSP2Time(getSSP2numGivenName(namePtr),
                              getExpectedTime(productType), compensated);
    }
  } else if (cat == RDS_CATEGORY_ID) {
     sqel = GetReqGivenListPos_SYSQUE(namePtr,posList[0]);
     clearQueueID(sqel->sysODLreq,namePtr);
  }


  if (ChangeStatus__MSTRQLIST(jobId, namePtr, Q_M_ENTRY_DELETED_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);

    sqPos = GetListPosGivenJobId_SYSQUE(namePtr, jobId);
    if (sqPos == -1) {
      printfLLog(LOG_ERR, "Job %d is not on %s queue\n",
             jobId, namePtr);
    }
    else {
      FreeSysQueElemGivenPos_SYSQUE(namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(namePtr, jobId) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s queue",
          namePtr);
      removeLineForJobId(namePtr, jobId);
    }


} /* end handleSubsysMenuDelete.............................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMainMenuDelete
 *
 * DESCRIPTION:
 *  handle the "Delete Entry" main menu selection
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMainMenuDelete()
{
 int posCount, *posList;
 int status, cat, sqPos, state;
 mstrqListElemType *mqel;
 char *productType;

  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "deletion");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Deleting", posList[0], mqel->jobId);

/* allow any kind of job  but a processing job to be deleted */

  cat = GetSubsystemCategoryGivenName(mqel->namePtr);
  status = GetStatus__MSTRQLIST(mqel->namePtr, mqel->jobId);
  if (status == Q_M_PROCESSING_ID) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, JOB_IN_PROCESS_REMOVE);
    return;
  }


#ifdef DISALLOW_DELETED
  else if (status == Q_M_ENTRY_DELETED_ID) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, "Job already being deleted");
    return;
  }
#endif

  else if (status == Q_M_PLACED_ON_SYSQ_ID ||
           status == Q_S_PLACED_ON_SYSQ_ID ||
           status == Q_M_GOT_ITEM_OFF_ID ||
           status == Q_M_INPUT_SOURCE_NOT_READY_ID ||
           status == Q_M_PLACED_ON_HOLD_DATA_ID) {
     if (cat == RDS_CATEGORY_ID)
       popdownMsgDialog(rdsMountDialog);
     else if (cat == ASP_CATEGORY_ID)
       popdownMsgDialog(aspMountDialog);

  }

/* if it's a PPS job, we need to completely remove it from the CP queue */
/* and not attempt to send the final job status message.   */
/* also only allow PPS HOLD/ERROR jobs to be completely deleted. */
/* if a PPS job is not in HOLD/ERROR state then we proceed to the normal */
/* processing of sending the final SPS_JOB_STATUS message to PPS */

#ifdef COMPLETELY_REMOVE_PPS

  if (status == Q_M_PLACED_ON_HOLD_ERROR_ID && 
      GetSubsystemCategoryGivenName(mqel->namePtr) == PPS_CATEGORY_ID) {
    for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
      ASFlogMessageDel_direct(ASF_CP, i, GetDispPos_MSTRQLIST(mqel->jobId),
       mqel->mqFmt[i], mqel->mqLabel[i]);
    }
    RemoveODLreqFromList_MSTRQLIST(posList[0]);  /* free the odl part */
    if (RemoveMstrqFromList_MSTRQLIST(posList[0]) == NULL)
      printfLLog(LOG_ERR, "Error removing job from master queue ");
    else {
      printfLLog(LOG_INFO, "Job %d removed from master queue", mqel->jobId);
    }

           /* remove entry from subsystem queue and display, if there is one */
    if (status != Q_M_FINAL_ID ) {
      sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
      FreeSysQueElemGivenPos_SYSQUE(mqel->namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(mqel->namePtr, mqel->jobId) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s subsystem queue",
          mqel->namePtr);
      removeLineForJobId(mqel->namePtr, jobId);
    }
  }
  else { /* all non-PPS jobs get deleted in a way such that the
            final PPS job status message *IS* sent to the PPS */
#endif
  if (cat == SSP2_CATEGORY_ID) {
    productType = GetProductType_MSTRQLIST(mqel->jobId);
    if (productType == NULL)
      printfLLog(LOG_ERR, CANNOT_GET, "product type");
    else {
      decrementSSP2Time(getSSP2numGivenName(mqel->namePtr),
                              getExpectedTime(productType));
    }
  } else if (cat == RDS_CATEGORY_ID) {
     clearQueueID(mqel->mstrqODLreq,mqel->namePtr);
  }

    if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                              Q_M_ENTRY_DELETED_ID) == -1)
      printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
    state = GetProcessState_PIDLIST(mqel->namePtr);
/*    if (state != SUBSYS_DORMANT_ID && state != SUBSYS_NOT_RUNNING_ID) */


    sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
    if (sqPos == -1) {
      printfLLog(LOG_ERR, "Job %d is not on %s queue\n", 
             mqel->jobId, mqel->namePtr);
    }
    else {
      FreeSysQueElemGivenPos_SYSQUE(mqel->namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(mqel->namePtr, mqel->jobId) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s queue", 
          mqel->namePtr);
      removeLineForJobId(mqel->namePtr, mqel->jobId);
    }
#ifdef COMPLETELY_REMOVE_PPS
  }
#endif

} /* end handleMainMenuDelete............................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMenuStartAllExtCB
 *
 * DESCRIPTION:
 *  start all the subsystem process when the user selects
 *  the start all menu item.  also pop up the control subsystems
 *  window for controlling these subsystems.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleMenuStartAllExtCB()
{
 int i, retval, cat;
 Widget buttonWid = NULL;
 char *namePtr;

  i = 0;
  while ((namePtr = (char *)getSubsysNameNo(i++)) != NULL) {
    cat = GetSubsystemCategoryGivenName(namePtr);
    if (cat == PPS_CATEGORY_ID || cat == IMS_CATEGORY_ID)
      retval = doStartSubsystem(namePtr, buttonWid);
  }

} /* end handleMenuStartAllExtCB.............*/
/*----------------------------------------------------------
 * NAME:
 *  handleMenuReadyAllExtCB
 *
 * DESCRIPTION:
 *  start all the subsystem process when the user selects
 *  the start all menu item.  also pop up the control subsystems
 *  window for controlling these subsystems.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleMenuReadyAllExtCB()
{
 int i, cat;
 char *namePtr;

  i = 0;
  while ((namePtr = (char *)getSubsysNameNo(i++)) != NULL) {
    cat = GetSubsystemCategoryGivenName(namePtr);
    if (cat == PPS_CATEGORY_ID || cat == IMS_CATEGORY_ID)
      if (GetProcessState_PIDLIST(namePtr) == SUBSYS_WAITING_ID) {
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_READY_ID, NOT_SET_ID);
        ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_READY_TEXT);
      }
  }
} /* end handleMenuReadyAllExtCB.............*/
/*----------------------------------------------------------
 * NAME:
 *  handleMenuStartAllCB
 *
 * DESCRIPTION:
 *  start all the subsystem process when the user selects
 *  the start all menu item.  also pop up the control subsystems
 *  window for controlling these subsystems.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleMenuStartAllCB()
{
 int i, retval, cat;
 Widget buttonWid = NULL;
 char *namePtr;

  i = 0;
  while ((namePtr = (char *)getSubsysNameNo(i++)) != NULL) {
    cat = GetSubsystemCategoryGivenName(namePtr);
    if (cat != PPS_CATEGORY_ID && cat != IMS_CATEGORY_ID)
      retval = doStartSubsystem(namePtr, buttonWid);
  }

} /* end handleMenuStartAllCB.............*/
/*----------------------------------------------------------
 * NAME:
 *  handleMenuReadyAllCB
 *
 * DESCRIPTION:
 *  start all the subsystem process when the user selects
 *  the start all menu item.  also pop up the control subsystems
 *  window for controlling these subsystems.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleMenuReadyAllCB()
{
 int i, cat;
 char *namePtr;

  i = 0;
  while ((namePtr = (char *)getSubsysNameNo(i++)) != NULL) {
    cat = GetSubsystemCategoryGivenName(namePtr);
    if (cat != PPS_CATEGORY_ID && cat != IMS_CATEGORY_ID)
      if (GetProcessState_PIDLIST(namePtr) == SUBSYS_WAITING_ID) {
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_READY_ID, NOT_SET_ID);
        ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_READY_TEXT);
      }
  }
} /* end handleMenuReadyAllCB.............*/


/*----------------------------------------------------------
 * NAME:
 *  handleMenuFileAsCB
 *
 * DESCRIPTION:
 *  put up the file selection box when the user selects the
 *  save as or restore as menu item
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuFileAsCB(char *ptr)
{
 int  x, y;
 XmString xmstring, title_xmstring, dir_xmstring;
 char *title, *dir=NULL;

  GLOBAL_saveRestoreFlag = -1;

  queryPointer(&x,&y);

  xmstring = XmStringCreateSimple("*");          /* refresh listing of all */
  XmFileSelectionDoSearch(CPfileBox, xmstring);  /* files in this directory */
  XmStringFree(xmstring);

  if (strcmp(ptr, "Save As") == 0) {
    title = "Save Queue As...";
    GLOBAL_saveRestoreFlag = SAVE;
    dir = getSaveDir();
  }
  else if (strcmp(ptr, "Restore") == 0) {
    if (totalMQentries != 0) {
      ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, MQ_NOT_EMPTY);
      return;
    }
    title = "Restore Queue As...";
    GLOBAL_saveRestoreFlag = RESTORE;
    dir = getRestoreDir();
  }

  title_xmstring = XmStringCreateSimple(title);
  if (title_xmstring == NULL) {
/*
      XmStringFree(dir_xmstring);
*/
      printfLLog(LOG_ERR, X_MOTIF_ERROR, "XmStringCreateSimple");
      return;
  }

  dir_xmstring = XmStringCreateSimple(dir); /* the user had changed dirs */
  if (dir_xmstring == NULL) {
      printfLLog(LOG_ERR, X_MOTIF_ERROR, "XmStringCreateSimple");
      return;
  }

  XtVaSetValues(CPfileBox, XmNx, x, XmNy, y,
                XmNdialogTitle, title_xmstring,
                XmNdirectory, dir_xmstring, 
                NULL);

  UxPopupInterface(CPfileBox, no_grab);

  XmStringFree(title_xmstring);
  XmStringFree(dir_xmstring);

  return;

} /* handleMenuFileAsCB */


/*----------------------------------------------------------
 * NAME:
 *  handleMenuExitCB
 *
 * DESCRIPTION:
 *  prepare the parameters for the OkEnterCB callback
 *  and put up the confirmation question box when the
 *  user selects the "Exit" menu item
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMenuExitCB(Widget w, XtPointer dPtr, XtPointer cPtr)
{
 int i=0;
 pidListElemType *plEl;
 dialogCBdata *cbData;

/* first check to see if any subsystems are still running.  if so,
   ask the user to first stop the subsystems before exiting the cp */

#ifdef STOP_BEFORE_EXIT
  while ((plEl = GetCopyOfElemIn_PIDLIST(i++) ) != NULL)   {
    if (plEl->classID == SUBSYS_CLASS_ID && plEl->state != SUBSYS_DORMANT_ID
        && plEl->state != SUBSYS_STARTED_ID) {
      ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, NOT_ALL_STOPPED);  
      printfLLog(LOG_DEBUG, NOT_READY_FOR_EXIT, plEl->procNamePtr, plEl->state);
      return;
    }
  }
#endif

                       /* set up the callback data for user response*/

  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = EXIT_ALL_ID;
  cbData->buttonWid = NULL;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , "ALL");
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_EXIT_TEXT, cbData->namePtr);

  return;

} /* end handleMenuExitCB................ */


/*----------------------------------------------------------
 * NAME:
 *  doSaveRestore
 *
 * DESCRIPTION:
 *  callback attached to save/restore file menu OK button
 *  perfrom the actual queue save and restore
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void doSaveRestore(Widget wid, XtPointer clientData, XtPointer callData)
{
 int fd, retval;
 struct stat fs;
 char *filePtr, *fileNamePtr;
 XmFileSelectionBoxCallbackStruct *cbs;
 int charCnt;
 FILE *fp;

  charCnt = -1;           /* init the character count */

  cbs  = (XmFileSelectionBoxCallbackStruct *) callData;
  XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &fileNamePtr);
  if (fileNamePtr == NULL) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                 fileNamePtr);
    return;
  }

  switch(GLOBAL_saveRestoreFlag) {
    case SAVE:
      if ((fp = fopen(fileNamePtr, "w")) == NULL) {
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                    fileNamePtr);
       return;
      }
      retval = WriteOut_MSTRQLIST(fp);
      if (retval == -1)
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                    fileNamePtr);
      if (retval == -2)
        ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, ALL_JOBS_COMPLETED);
      fclose(fp);
      break;

    case RESTORE:
      fd = open(fileNamePtr, 0);
      if (fd == -1) {
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                   fileNamePtr);
        return;
      }

      retval = fstat(fd, &fs);

      if ((fs.st_mode & S_IFMT) == S_IFDIR) {
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                   fileNamePtr);
        return;
      }

      filePtr = (char *)doMalloc(fs.st_size);

      if (read(fd, filePtr, fs.st_size) != fs.st_size) {
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CP_CANT_ACCESS_FILE_TEXT,
                  fileNamePtr);
        doFree(filePtr);
        return;
      }
      close(fd);
      retval =  ReadIn_MSTRQLIST(fileNamePtr);
      if (retval)
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, RESTORE_FILE_BAD,
          fileNamePtr);
      break;

    default:
      break;
  } /* end switch */

  return;

} /* end doSaveRestore.....................................*/


/*----------------------------------------------------------
 * NAME:
 *  handleMainMenuDetailedInfo()
 *
 * DESCRIPTION:
 *  handle the "show detailed info" menu selection by popping up
 *  a box with detailed information on the selected queue list item
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void handleMainMenuDetailedInfo()
{
 int     posCount, *posList;
 mstrqListElemType *mqel;

  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, 
                                          "detailed information");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }
  /* showJobInfo(mqel); */
  showJobInfo(mqel->jobId, mqel->namePtr, mqel->mstrqODLreq);

} /* handleMainMenuDetailedInfo */

/*----------------------------------------------------------
 * NAME:
 *  handleMainMenuRemoveHold
 *
 * DESCRIPTION:
 *  remove the hold status of an item in the master queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void handleMainMenuRemoveHold()
{
 int posCount, *posList;
 int status;
 mstrqListElemType *mqel;


  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "hold removal");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Removing Hold", posList[0], mqel->jobId);

  status = GetStatus__MSTRQLIST(mqel->namePtr, mqel->jobId);
  if (status == Q_M_IMAGE_QC_ON_HOLD_ID || status == Q_S_IMAGE_QC_ON_HOLD_ID) {
    ClearQCflag_SYSQUE(mqel->namePtr, mqel->jobId);
  }
  else if (status != Q_M_INPUT_SOURCE_NOT_READY_ID &&
      status != Q_M_PLACED_ON_HOLD_DATA_ID &&
      status != Q_M_PLACED_ON_HOLD_ERROR_ID) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX,
                        "Cannot remove selected job from hold state");
   return;
  }

  if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                             Q_M_PLACED_ON_SYSQ_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
  else {
    ClearHoldFlag_SYSQUE( mqel->namePtr, mqel->jobId);
    SetRepeatFlagGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
  }

} /* end handleMainMenuRemoveHold............................*/

/*----------------------------------------------------------
 * NAME:
 *  handleMainMenuMoveJob
 *
 * DESCRIPTION:
 *  remove the hold status of an item in the master queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

void handleMainMenuMoveJob()
{
 int i, posCount, *posList, sqPos,cat;
 char *destSubsys;
 mstrqListElemType *mqel;

  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "moving");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }

  printfLLog(LOG_DEBUG, LIST_POS_JOB_ID, "Moving Job", posList[0], mqel->jobId);

  if ((destSubsys = eligibleToMove(mqel)) == NULL) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, JOB_MOVE_INVALID, mqel->jobId);
    return;
  }

/* if got to here, we've got a valid entry to move */

/* remove the entry from the original subsystem queue and subsys display */
  sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
  FreeSysQueElemGivenPos_SYSQUE(mqel->namePtr, sqPos);
  RemoveElemWithJobId_SYSQUE(mqel->namePtr, mqel->jobId);
  removeLineForJobId(mqel->namePtr, mqel->jobId);
  cat = GetSubsystemCategoryGivenName(mqel->namePtr);
  if (cat == RDS_CATEGORY_ID) {
      printf(" *** in  new code\n");
      clearQueueID( mqel->mstrqODLreq,mqel->namePtr);
      adjustQueueID(mqel,destSubsys);
  }



/* add new job to the destination subsystem queue */
  AddSysReqToQue_SYSQUE(destSubsys, NOT_SET, mqel->jobId,
         FALSE, FALSE, mqel->mstrqODLreq, mqel->dataReadyFlag);
/* add the display items to the destination subsystem queue */
  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++)
    ASFlogMessageInsert_direct(destSubsys, i,
       GetListPosGivenJobId_SYSQUE(destSubsys,mqel->jobId),
              mqel->mqFmt[i], mqel->mqLabel[i]);

/* change the destination subsystem in the master queue to the new dest */
  ChangeStatus__MSTRQLIST(mqel->jobId, destSubsys, Q_S_PLACED_ON_SYSQ_ID);

/* update the queue status label in the destination subsys queue */
  updateStatusLabel(destSubsys, 
      GetListPosGivenJobId_SYSQUE(destSubsys, mqel->jobId), 
      Q_S_PLACED_ON_SYSQ_ID, destSubsys);

/* update the queue status label in the main window */
  updateStatusLabel(ASF_CP, GetDispPos_MSTRQLIST(mqel->jobId),
             Q_S_PLACED_ON_SYSQ_ID, destSubsys);

} /* end handleMainMenuMoveJob............................*/

void performMenuItem(void *fn(), char *namePtr, Widget wid)
{
  baseListElemType *nPtr;
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return;
  
  if (wid == NULL && namePtr == NULL) /* wid and name NULL -> no args */
    (*fn)();
  else if (wid == NULL && namePtr != NULL) /* only name -> pass name */
    (*fn)(namePtr);
  else  if (wid != NULL && namePtr == NULL) /* only wid -> pass wid */
    (*fn)(wid);
  else  if (wid != NULL && namePtr != NULL) /* name and wid -> pass both */
    (*fn)(namePtr, wid);
  
  unLockNameList_CORE(nPtr);

} /* performMenuItem */
