static char sccsid_debug_c[] = "@(#)debug.c	1.40 97/02/20 12:00:57";

/* #define BUILD_DEBUG /* */
/* #define LABELS /* */
/* #define PIDS  /* nancy: turn this on to debug pidlist handling */
/* #define MSGQ  /* nancy: turn this on to debug msg queue handling */
/* #define MSTR /* nancy: turn this on to debug master queue elements */
/* #define SYSQ /* nancy: turn this on to debug sysque elements */
/* #define XWP /* nancy: turn this on to printf handleXwp calls */
/* #define SF /* nancy: turn on to debug subsysFiles stuff */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/List.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <task.h>

#include "CPmainQ.h"
#include "CPsubsysQ.h"
#include "CPerrorBox.h"
#include "CPinfoBox.h"
#include "odl.h"    /* contains tuan's value_data for agg */
#include "asfcommon.h"
#include "logUtils.h"

#include "cpdefines.h"
#include "cpconfig.h"
#include "serverSocketXport.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"
#include "que_sys.h"
#include "que_xwp.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "cpworkProc.h"
#include "inet.h"
#include "cplogs.h"
#include "cpbuildMsg.h"
#include "cpmenu.h"


void printXwpqEl(xwpQueElemType  *xp)
{
  char z[10];
  strncpy(z, xp->reqStrPtr, 10);
  z[6] = '\0';
#ifdef XWP
printf("printXwpqEl\n"); fflush(stdout);
  printf("Xwp element: reqStr %s src %s action %d pos %d xentity%d \n", 
   z, xp->srcPtr, xp->actionFlag, xp->replPos, xp->xEntity);
printf("sizeof xwp %d\n", sizeof(xp));
/*****
  if (xp.cbData != NULL)
     printf("Xwp cbData: action %d name %s widget 0x%x, pos %d rev %d platform %s\n", xp.cbData->action, xp.cbData->namePtr, xp.cbData->buttonWid, xp.cbData->pos, xp.cbData->rev, xp.cbData->platform);
  else
    printf("Xwp cbData is NULL! \n");
fflush(stdout);
*****/
#endif
}

void printSysqEl(char *namePtr, sysQueElemType  *sqel)
{
  char z[10];
  strncpy(z, sqel->reqStrPtr, 10);
  z[6] = '\0';

#ifdef SYSQ  
printf("\tjob %3d status %2d repeat %d mediaCheck %d hold %d dataReady %d qc %d\n", sqel->jobId, 
GetStatus__MSTRQLIST(namePtr, sqel->jobId),
sqel->repeatFlag, sqel->checkMediaId, 
sqel->holdFlag, sqel->dataReadyFlag, sqel->qcFlag );
#endif  
}

int dumpXwpq()
{
 xwpQueElemType *xqel;
 baseListElemType *nPtr;
 int i;

#ifdef XWP
printf("dumpXwpq\n");
#endif

  nPtr = lockNameList_CORE(XWPREQ_QUE);
  if (nPtr == NULL)
     return(-1);
#ifdef XWP
printf("\nX Workproc Queue (%d entries):\n", nPtr->nsize); fflush(stdout);
#endif

  for (i = 0; i < nPtr->nsize; i++) {
   xqel = (xwpQueElemType *)nPtr->arr[i];
   if (xqel == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }

   printXwpqEl(xqel);
  } /* end for loop */

/*printf("\n"); */
  unLockNameList_CORE(nPtr);
  return(0);

}
void printMstrqEl(mstrqListElemType  *mqel, char *str)
{

#ifdef MSTR
printf("%s:\n", str); 
printf("mstrq el name %s jobId %d status %d modified %d dataReady %d\n",
mqel->namePtr, mqel->jobId, mqel->status, mqel->modifiedFlag, mqel->dataReadyFlag);
#endif
  return;
}

void printLabels(mstrqListElemType *mqel)
{
#ifdef LABELS
  int i;
dumpMstrq();
  for (i = 0; i < MAX_MAIN_LIST_ITEMS; i++)
      printf("jobid %d mqLabel[%d] is %s\n", mqel->jobId, i, mqel->mqLabel[i]);
#endif
  return;
}



char *getWidgetName(int entity)
{
  static char ptr[255];
    switch(entity) {
      case MAIN_JOB_ID_LIST :
        strcpy(ptr, "MAIN_JOB_ID_LIST");
        break;
      case MAIN_PLAT_REV_SEQ_LIST :
        strcpy(ptr, "MAIN_PLAT_REV_SEQ_LIST");
        break;
      case MAIN_FRAME_LIST :
        strcpy(ptr, "MAIN_FRAME_LIST");
        break;
      case MAIN_MEDIA_LIST :
        strcpy(ptr, "MAIN_MEDIA_LIST");
        break;
      case MAIN_MODE_LIST :
        strcpy(ptr, "MAIN_MODE_LIST");
        break;
      case MAIN_REQ_LIST                  :
        strcpy(ptr, "MAIN_REQ_LIST  ");
        break;
      case MAIN_TYPE_LIST :
        strcpy(ptr, "MAIN_TYPE_LIST");
        break;
      case MAIN_STATUS_LIST :
        strcpy(ptr, "MAIN_STATUS_LIST");
        break;
      case WP_ERROR_BOX                   :
        strcpy(ptr, "WP_ERROR_BOX                  ");
        break;
      case WP_CLEANUP_BOX :
        strcpy(ptr, "WP_CLEANUP_BOX");
        break; 
      case WP_QUESTION_BOX :
        strcpy(ptr, "WP_QUESTION_BOX");
        break;
      case WP_INFO_BOX                    :
        strcpy(ptr, "WP_INFO_BOX                   ");
        break;

      case WP_MAX_JOBS_TEXT_ID :
        strcpy(ptr, "WP_MAX_JOBS_TEXT_ID");
        break;
      case WP_NUM_JOBS_LABEL_ID :
        strcpy(ptr, "WP_NUM_JOBS_LABEL_ID");
        break;

      case QUE_STATUS_LABEL_ID :
        strcpy(ptr, "QUE_STATUS_LABEL_ID");
        break;
      case QUE_HEARTBEAT_LABEL_ID           :
        strcpy(ptr, "QUE_HEARTBEAT_LABEL_ID          ");
        break;
      case QUE_JOB_ID_LIST :
        strcpy(ptr, "QUE_JOB_ID_LIST");
        break;
      case QUE_PLAT_REV_SEQ_LIST :
        strcpy(ptr, "QUE_PLAT_REV_SEQ_LIST");
        break;
      case QUE_FRAME_LIST                :
        strcpy(ptr, "QUE_FRAME_LIST               ");
        break;
      case QUE_MEDIA_LIST :
        strcpy(ptr, "QUE_MEDIA_LIST");
        break;
      case QUE_MODE_LIST :
        strcpy(ptr, "QUE_MODE_LIST");
        break;
      case QUE_REQ_LIST :
        strcpy(ptr, "QUE_REQ_LIST");
        break;
      case QUE_TYPE_LIST :
        strcpy(ptr, "QUE_TYPE_LIST");
        break;
      case QUE_STATUS_LIST :
        strcpy(ptr, "QUE_STATUS_LIST");
        break;
      default:
        strcpy(ptr, "UNDEFINED");
        break;
   }
   return(ptr);
}

int dumpMstrq()
{
 mstrqListElemType *mqel;
 baseListElemType *nPtr;
 int i;


#ifdef MSTR
extern int totalMQentries;
printf("\nMaster Queue (%d entries):\n", totalMQentries);
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

   printMstrqEl(mqel, "");
  } /* end for loop */

/*printf("\n"); */
  unLockNameList_CORE(nPtr);
  return(0);

}

printPidEl(pidListElemType *pidListElemPtr)
{
#ifdef PIDS
 printf("pidListElemPtr:%10s at 0x%x", pidListElemPtr->procNamePtr, pidListElemPtr->procNamePtr);
 printf("class %d ", pidListElemPtr->classID);
 printf("pid %4d ", pidListElemPtr->ppid);
 printf("state %d\n", pidListElemPtr->state);
#endif

}

void printListWidgets(pidListElemType *pidListElemPtr)
{
#ifdef PIDS
 printf("pidListElemPtr->jobIdList 0x%x %s\n", 
        pidListElemPtr->jobIdList, XtName(pidListElemPtr->jobIdList));
 printf("pidListElemPtr->platRevSeqList 0x%x %s\n", 
        pidListElemPtr->platRevSeqList, XtName(pidListElemPtr->platRevSeqList));
    printf("pidListElemPtr->frameList 0x%x %s\n", 
        pidListElemPtr->frameList, XtName(pidListElemPtr->frameList));
    printf("pidListElemPtr->mediaList 0x%x %s\n", 
        pidListElemPtr->mediaList, XtName(pidListElemPtr->mediaList));
    printf("pidListElemPtr->modeList 0x%x %s\n", 
        pidListElemPtr->modeList, XtName(pidListElemPtr->modeList));
    printf("pidListElemPtr->requestList 0x%x %s\n", 
        pidListElemPtr->requestList, XtName(pidListElemPtr->requestList));
    printf("pidListElemPtr->typeList 0x%x %s\n", 
        pidListElemPtr->typeList, XtName(pidListElemPtr->typeList));
    printf("pidListElemPtr->statusList 0x%x %s\n", 
        pidListElemPtr->statusList, XtName(pidListElemPtr->statusList));
#endif

}

void printListInfo(char *namePtr, Widget w, int shouldMatch)
{
  Dimension width=0,h=0;
  Dimension parent_w=0, parent_h=0;
  XmString *strlist, *jobList;
  XmStringTable str_table;
  char *text;
  int i, count, q_size = -1;
  Widget jobListW = GetJobIdListGivenName_PIDLIST(namePtr);

  assert(XmIsList(jobListW));
  assert(XmIsList(w));

  XtVaGetValues(w, XmNitemCount, &count, XmNitems, &strlist,
                   XmNheight, &h, XmNwidth, &width, NULL);


  if (strcmp(namePtr, ASF_CP) != 0) {  /* dump sysq info for subsystems */
    q_size = dumpSysq(namePtr);
    if (shouldMatch && count != q_size)
      printf("EMERGENCY***** %s Lines displayed %d does not match queue size %d ***\n", namePtr, count, q_size);
  }

  if (count > 0)
    printf("%6s list has %d items;", namePtr, count);

  str_table = (XmStringTable) XtMalloc (50 * sizeof (XmString *));
  XtVaGetValues(jobListW, XmNitems, &jobList, NULL);
  for (i=0; i < count; i++) {
    if (jobList[i] == NULL) printf("REAL emergency, joblist[%d] NULL\n",i);
    if (!XmStringGetLtoR (jobList[i], XmFONTLIST_DEFAULT_TAG, &text))
      continue;
    printf("\n\t{%s,", text);
    XtFree(text);
    if (strlist[i] == NULL) printf("REAL emergency, strlist[%d] NULL\n",i);
    if (!XmStringGetLtoR (strlist[i], XmFONTLIST_DEFAULT_TAG, &text))
      continue;
    printf("%s}", text);
    XtFree(text);
  }
  if (count > 0)
    printf("\n");

/*  printf("%6s-%20s list has %d items; ", namePtr, XtName(w), count); */

/****
  printf("   %s list has %d items; height %d width %d\n", 
          XtName(w), count, h, width);
/*
  XtVaGetValues(XtParent(w), XmNheight, &parent_h, XmNwidth, &parent_w, NULL);
  printf("   parent %s has %d items; height %d width %d\n",
              XtName(XtParent(w)), count, parent_h, parent_w);
*/
}

int dumpSysq(char *namePtr)
{
 sysQueElemType *sysQueElemPtr;
 baseListElemType *nPtr;
 int i, q_size = 0;

  nPtr = lockNameList_CORE(namePtr);
  if (nPtr == NULL)
     return(-1);

#ifdef SYSQ
if (nPtr->nsize > 0)
printf("     %s Sysq is:\n", namePtr);
#endif

  for (i = 0; i < nPtr->nsize; i++) {
   sysQueElemPtr = (sysQueElemType *)nPtr->arr[i];
   if (sysQueElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
   }
   printSysqEl(namePtr, sysQueElemPtr);
  } /* end for loop */

#ifdef SYSQ
if (nPtr->nsize > 0)
/* printf("\n"); */
/*printf("....end of %s SysQ\n", namePtr); */
#endif
  if (nPtr->nsize > 0)
    q_size = nPtr->nsize;
  unLockNameList_CORE(nPtr);
  return(q_size);

}

int dumpSysq_w(Widget w)
{
  char *namePtr;
  
  namePtr = GetNameGivenMainWid_PIDLIST(w);
printf("dumpSysq_w name %s for %s\n", namePtr, XtName(w));
  if (namePtr == NULL)
    return(0);

 dumpSysq(namePtr);
 return(1);
}


void printGlobalState()
{
#ifdef BUILD_DEBUG
extern menuStateType GLOBAL_state;

printf("job %d ", GLOBAL_state.jobId);
printf("sat_time %u ", GLOBAL_state.sat_time);
if (GLOBAL_state.sv_satBuf != NULL) printf("sv platform %s ", GLOBAL_state.sv_satBuf);
if (GLOBAL_state.sv_typeBuf != NULL) printf("sv type %s ", GLOBAL_state.sv_typeBuf);
if (GLOBAL_state.sv_coordSysBuf != NULL) printf("sv csys %s ", GLOBAL_state.sv_coordSysBuf);
printf("sv rev %d ", GLOBAL_state.sv_rev);
printf("file version %d ", GLOBAL_state.fileVersion);
/* printf("subframe %d ", GLOBAL_state.subframe_id);  */
if (GLOBAL_state.mode_buf != NULL) printf("mode %s ", GLOBAL_state.mode_buf);
if (GLOBAL_state.mediaType != NULL) printf("media type %s ", GLOBAL_state.mediaType);
if (GLOBAL_state.productT_buf != NULL) printf("product type %s \n", GLOBAL_state.productT_buf);
if (GLOBAL_state.frame_mode_buf != NULL) printf("frame mode %s ", GLOBAL_state.frame_mode_buf);
if (GLOBAL_state.projection_buf != NULL) printf("projection %s\n", GLOBAL_state.projection_buf);
if (GLOBAL_state.terrain_correction_buf != NULL) printf("terrain corr %s", GLOBAL_state.terrain_correction_buf);
printf("pixel spacing %lf ", GLOBAL_state.pixelSpacing);
printf("avg terrain height %lf \n", GLOBAL_state.avgTerrainHt);
if (GLOBAL_state.skew_buf != NULL) printf("skew %s ", GLOBAL_state.skew_buf);
if (GLOBAL_state.site_nameBuf != NULL)  printf("site name %s ", GLOBAL_state.site_nameBuf);

printf("rev %d ", GLOBAL_state.rev);
printf("seq %d ", GLOBAL_state.seq);
if (GLOBAL_state.activityBuf != NULL) printf("activity %s ", GLOBAL_state.activityBuf);

if (GLOBAL_state.output_format_buf != NULL) printf("output_fmt_buf %s ", GLOBAL_state.output_format_buf);
printf("gain %d ", GLOBAL_state.processingGain);

printf("\n");
#endif
}



extern char GLOBAL_lastRDSMediaPrompted[MAX_RDS][512], GLOBAL_lastRDSMediaMounted[MAX_RDS][512];
extern char GLOBAL_lastASPMediaPrompted[512], GLOBAL_lastASPMediaMounted[512];
void printMediaState(char *str)
{
/**** 
printf("%s\n", str);
printf("last RDS prompted %s\n", GLOBAL_lastRDSMediaPrompted);
printf("last RDS mounted %s\n", GLOBAL_lastRDSMediaMounted);
printf("last ASP prompted %s\n", GLOBAL_lastASPMediaPrompted);
printf("last ASP mounted %s\n", GLOBAL_lastASPMediaMounted);
/*****/

}

void doPurify(char *str)
{
  if (purify_is_running()) {
    if (strcmp(str, "ALL") == 0)
      purify_all_leaks();
    else if (strcmp(str, "NEW") == 0)
      purify_new_leaks();
  }

}
void showSf_sf(char *str, subsysFilesType *sf)
{

#ifdef SF
  int i;
printf("\tshowSf_sf: %s at 0x%x\n", str, sf);
    if (sf == NULL) {
      printf("\t\t nothing to show!\n");
      return;
    }

    printf("\t\tsf jobid %d numProducts %d numFiles %d numDirs %d\n",
           sf->jobId, sf->numProducts, sf->numFiles, sf->numDirs);

    if (sf->numFiles != 0) {
      printf("\t\tFiles\n");
      for (i = 0; i < sf->numFiles; i++)
        printf("\t\t%s\n", sf->fileName[i]);
    }

    if (sf->numProducts != 0) {
      printf("\t\tProducts at 0x%x\n", sf->productName[0]);
      for (i = 0; i < sf->numProducts; i++)
        printf("\t\t%s\n", sf->productName[i]);

    }
    if (sf->numDirs != 0) {
      printf("\t\tDirs at 0x%x\n", sf->dirName[0]);
      for (i = 0; i < sf->numDirs; i++)
        printf("\t\t%s\n", sf->dirName[i]);

    }
#endif

} /* showSf */


void printCBdata(char *str, dialogCBdata *ptr)
{
/*  printf("%s cbData: ", str); */
  if (ptr == NULL) {
    printf("NULL\n");
    return;
  }
  printf("%s cbData at 0x%x, points to 0x%x\n", str, &ptr, ptr); 

  printf("action %d pos %d rev %d ", ptr->action, ptr->pos, ptr->rev);
  printf("name %s platform %s ", ptr->namePtr, ptr->platform);
  if (ptr->buttonWid != NULL)
    printf("wid %s\n", XtName(ptr->buttonWid));
  else
    printf("wid NULL\n");
}


void changeJobState(int status)
{
 int posCount, *posList;
 mstrqListElemType *mqel;

  printf("new state %d\n", status);
  if (!XmListGetSelectedPos(CPmainJobIdList, &posList, &posCount)) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, LIST_SEL_ERROR, "moving");
    return;
  }

  mqel = GetReqGivenListPos_MSTRQLIST(posList[0]);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }

  printf("Changing job %d state to %d\n", mqel->jobId, status);
  ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, status);


}

/*----------------------------------------------------------
 * NAME:
 *  GetProcessStateAsText
 *
 * DESCRIPTION:
 *  returns the process state as a string
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * GetProcessStateAsText(int state)
{
 char *ptr;
    switch(state) {
      case  SUBSYS_DORMANT_ID :
        ptr = "SUBSYS_DORMANT_ID";
        break;
      case  SUBSYS_NOT_RUNNING_ID :
        ptr = "SUBSYS_NOT_RUNNING_ID";
        break;
      case  SUBSYS_STARTED_ID :
        ptr = "SUBSYS_STARTED_ID";
        break;
      case  SUBSYS_WAITING_ID :
        ptr = "SUBSYS_WAITING_ID";
        break;
      case  SUBSYS_READY_ID :
        ptr = "SUBSYS_READY_ID";
        break;
      case  SUBSYS_RUNNING_ID :
        ptr = "SUBSYS_RUNNING_ID";
        break;
      case  SUBSYS_HALTING_ID :
        ptr = "SUBSYS_HALTING_ID";
        break;
      case  SUBSYS_PAUSING_ID :
        ptr = "SUBSYS_PAUSING_ID";
        break;
      case  SUBSYS_HEALTHING_ID :
        ptr = "SUBSYS_HEALTHING_ID";
        break;
      case  SUBSYS_STOPPING_ID :
        ptr = "SUBSYS_STOPPING_ID";
        break;
      case  EXIT_ALL_ID :
        ptr = "EXIT_ALL_ID";
        break;
      case  SUBSYS_DIDNT_START_ID :
        ptr = "SUBSYS_DIDNT_START_ID";
        break;
      case  SUBSYS_ERROR_ID :
        ptr = "SUBSYS_ERROR_ID";
        break;
      case  SUBSYS_WRONG_TAPE_ID :
        ptr = "SUBSYS_WRONG_TAPE_ID";
        break;
      case  SUBSYS_PPS_ERROR_ID :
        ptr = "SUBSYS_PPS_ERROR_ID";
        break;
      case  SUBSYS_HUNG_ID :
        ptr = "SUBSYS_HUNG_ID";
        break;
      default:
        ptr = UNDEFINED;
        break;
      }
      return(ptr);

} /* GetProcessStateAsText........*/

