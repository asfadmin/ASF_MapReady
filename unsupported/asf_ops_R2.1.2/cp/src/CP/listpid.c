/* #define COPY_PIDS /* */
/* #define SOCKETS /* */
/* #define PIDS /* */ 
/* #define NAMES /* GetNameGiven_PIDLIST */
/* #define TIMEOUTS /* */

/*----------------------------------------------------------
 * SCCS Header
 * File:listpid.c   Rev:4.2.0.0   Date:95/02/10
 *
 * Tag range 30,000 39,999
 *---------------------------------------------------------*/

static char sccsid_listpid_c[] = "@(#)listpid.c	4.45 97/11/04 11:26:04";

#include <stdio.h>
#include <sys/types.h>
#include <syslog.h>
#include <signal.h>
#include <bstring.h> /* bzero */
#include <unistd.h>  /* close */

#include <X11/Intrinsic.h>

#include <Xm/Xm.h>  /* had to add these two for WM_DELETE_WINDOW */
#include <Xm/Protocols.h>

#include "UxXt.h"
#include "CPsubsysQ.h"  /* create_CPsubsysQ */

#include "logUtils.h"
#include "memUtils.h"
#include "asfcommon.h"
#include "pthread_wrapper.h"

#include "cpdefines.h"
#include "listcore.h"
#include "listpid.h"
#include "cplogs.h"
#include "cpmenu.h"

/* begin global variables */
extern Widget CPmainScrollBar;
extern Widget CPmainJobIdList;
extern Widget CPmainPlatRevSeqList;
extern Widget CPmainFrameList;
extern Widget CPmainMediaList;
extern Widget CPmainModeList;
extern Widget CPmainRequestList;
extern Widget CPmainTypeList;
extern Widget CPmainStatusList;
extern Widget UxTopLevel;

extern Widget getQueWidget();  /* CPsubsysQ.c */
extern void setupScrollBars(char *namePtr);

#ifdef TIMEOUTS
extern char *timestamp();
#endif

/* end global variables */

/* internal prototype */
static pidListElemType *FindPidOrTidInList_PIDLIST(baseListElemType *nPtr,
                                            int ppid, int *pos);

/* ..............*/

/*----------------------------------------------------------
 * NAME:
 *  CreatePidList_PIDLIST 
 *
 * DESCRIPTION:
 *  create a thread safe pid list and sema
 *
 * NOTES:
 *  Visible
 *
 *---------------------------------------------------------*/
int CreatePidList_PIDLIST()
{
 int retval;

 retval = createOneNameListEntry_CORE(PID_LIST);
 return(retval);

} /* CreatePidList_PIDLIST...................*/


/*----------------------------------------------------------
 * NAME:
 *   AddPidOrThreadToList_PIDLIST 
 *
 * DESCRIPTION:
 *  a lower level routine that adds an element to the pid list
 *  whether the element is a pid or a thread
 *
 * NOTES:
 * -this a private routine so the list is already 
 *  locked
 * -the subsystem queue windows are created here
 *---------------------------------------------------------*/
static int AddPidOrThreadToList_PIDLIST(baseListElemType *nPtr,
       int  classID, char *procNamePtr, char *exeNamePtr, char *logFileNamePtr,
       int   listPos, int   ppid, tid_t ttid, int   socket)
{
 pidListElemType *pidListElemPtr;
 static int externPos = 1;
 int i, retval;
 Atom WM_DELETE_WINDOW;
 Widget shell;
 Arg args[10];

#ifdef PIDS
printf("AddPidOrThreadToList_PIDLIST procname %s pos %d exename %s ppid %d ttid %d socket %d\n", 
        procNamePtr, listPos, exeNamePtr, ppid, ttid, socket);
#endif

                                             /* set up the element to add */
  pidListElemPtr = (pidListElemType *)doMalloc(sizeof(pidListElemType));
  if (pidListElemPtr == NULL)
     return(-1);

  /* clean the memory */
  bzero(pidListElemPtr, sizeof(pidListElemType) );

  pidListElemPtr->classID = classID;

  pidListElemPtr->procNamePtr = (char *)doMalloc(strlen(procNamePtr) + 1);
  if (pidListElemPtr->procNamePtr == NULL)
     return(-1);
  strcpy(pidListElemPtr->procNamePtr, procNamePtr);

  pidListElemPtr->exeNamePtr = (char *)doMalloc(strlen(exeNamePtr) + 1);
  if (pidListElemPtr->exeNamePtr == NULL)
     return(-1);
  strcpy(pidListElemPtr->exeNamePtr, exeNamePtr);

  pidListElemPtr->logFileNamePtr = (char *)doMalloc(strlen(logFileNamePtr) + 1);
  if (pidListElemPtr->logFileNamePtr == NULL)
     return(-1);
  strcpy(pidListElemPtr->logFileNamePtr, logFileNamePtr);

  /* NOTE:: for real process, ppid is pid, but ttid is 
            is -1, for real thread, ppid is threads pid but
            ttid is threads id */
  pidListElemPtr->logfp       = NULL;
  pidListElemPtr->ppid        = ppid;
  pidListElemPtr->ttid        = ttid;
  pidListElemPtr->socket      = socket;
  pidListElemPtr->listPos     = listPos;
  pidListElemPtr->termination_reason      = NOT_SET_ID;

  /* check to see if this is the cp itself */
  if (pidListElemPtr->classID == CP_CLASS_ID) {
    pidListElemPtr->jobIdList = CPmainJobIdList;
    pidListElemPtr->platRevSeqList = CPmainPlatRevSeqList;
    pidListElemPtr->frameList = CPmainFrameList;
    pidListElemPtr->mediaList = CPmainMediaList;
    pidListElemPtr->modeList = CPmainModeList;
    pidListElemPtr->requestList = CPmainRequestList;
    pidListElemPtr->typeList = CPmainTypeList;
    pidListElemPtr->statusList = CPmainStatusList;
    XtVaGetValues(CPmainStatusList, XmNverticalScrollBar, 
        &pidListElemPtr->scrollBar , NULL);
#ifdef PIDS
printf("\nThese are the CP class widgets\n");  
printListWidgets(pidListElemPtr); printf("\n\n\n");
#endif
  }

  /* check to see if this is a subsystem */
  if (pidListElemPtr->classID == SUBSYS_CLASS_ID ||
      pidListElemPtr->classID == PPS_CLASS_ID ||
      pidListElemPtr->classID == IMS_CLASS_ID)  {
    pidListElemPtr->mainWid = (Widget)create_CPsubsysQ(UxTopLevel, procNamePtr);

    pidListElemPtr->infoLabel = 
          (Widget)getQueWidget(pidListElemPtr->mainWid, "InfoLabel"); 

    pidListElemPtr->jobIdList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "JobIdList");
    pidListElemPtr->platRevSeqList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "PlatRevSeqList");
    pidListElemPtr->frameList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "FrameList");
    pidListElemPtr->mediaList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "MediaList");
    pidListElemPtr->modeList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "ModeList");
    pidListElemPtr->requestList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "RequestList");
    pidListElemPtr->typeList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "TypeList");
    pidListElemPtr->statusList = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "StatusList");
    pidListElemPtr->scrollBar = 
      (Widget)getQueWidget(pidListElemPtr->mainWid, "ScrollBar");

    pidListElemPtr->queBottomInfoLabel_0 = 
          (Widget)getQueWidget(pidListElemPtr->mainWid, "QueBottomInfoLabel_0"); 
#ifdef PIDS
printf("\n\nThese are the %s subsystem widgets\n", procNamePtr);  
printListWidgets(pidListElemPtr); printf("\n\n\n");
#endif

  shell = XtParent(pidListElemPtr->mainWid);
  shell = pidListElemPtr->mainWid;
  i = 0;
  XtSetArg(args[i], XmNdeleteResponse, XmDO_NOTHING); i++;
  XtSetValues(shell, args, i);

  WM_DELETE_WINDOW = XmInternAtom(XtDisplay(pidListElemPtr->mainWid),
                                  "WM_DELETE_WINDOW", FALSE);
  XmAddWMProtocolCallback(shell,
                          WM_DELETE_WINDOW,
                          (XtCallbackProc) handleSubsysMenuExitCB,
                          pidListElemPtr->mainWid);



  }
  /* we know all the above now, for processes connected through 
   * read threads an association is made on reception of the 
   * subsystem ready messasge when we do the AddSocket...
   */
  pidListElemPtr->assocThread = 0;

  /* 
   * NOTE: subsystems and rutil are added to the list but are not
   *       assumed to be running until the ready is received
   */
  switch(pidListElemPtr->classID) {
   case PPS_CLASS_ID: 
   case IMS_CLASS_ID: 
    i = 0;
/*     pidListElemPtr->state = SUBSYS_RUNNING_ID; */
    pidListElemPtr->state = SUBSYS_DORMANT_ID;
    pidListElemPtr->externPos     = externPos++;
    break;

   case SUBSYS_CLASS_ID:
    i = 0;
    pidListElemPtr->state = SUBSYS_DORMANT_ID;
    break;

   case RUTIL_CLASS_ID: 
    pidListElemPtr->state = SUBSYS_STARTED_ID;
    break;

   case CP_CLASS_ID: 
   case QC_CLASS_ID: 
   case THREAD_CLASS_ID:
   case LOG_BROWSER_CLASS_ID: 
    pidListElemPtr->state = SUBSYS_RUNNING_ID;
    break;

   default:
    pidListElemPtr->state = SUBSYS_NOT_RUNNING_ID;
    break;
  } /* end switch */

  /* set the state modified flag */
  pidListElemPtr->stateModFlag = MODIFIED;
  pidListElemPtr->startID      = 0;
  pidListElemPtr->stopID       = 0;
  pidListElemPtr->haltID       = 0;
  pidListElemPtr->healthID     = 0;
  pidListElemPtr->msg_sentID   = 0;
 
  /* add the element to the pid list */
  retval = AddElemToNameList_CORE(nPtr, PID_LIST, pidListElemPtr);

  if (pidListElemPtr->classID == SUBSYS_CLASS_ID ||
      pidListElemPtr->classID == IMS_CLASS_ID ||
      pidListElemPtr->classID == PPS_CLASS_ID ||
      pidListElemPtr->classID == CP_CLASS_ID) 
    setupScrollBars(procNamePtr);

  return(retval);

} /* end AddPidOrThreadToList_PIDLIST....................*/


/*----------------------------------------------------------
 * NAME:
 *  AddPidToList_PIDLIST
 *
 * DESCRIPTION:
 *  add a pid to the pid list 
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int AddPidToList_PIDLIST(int classID, char *procNamePtr, char *exeNamePtr, 
         char *logFileNamePtr, int  listPos, int  ppid)
{
 int retval;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  retval = AddPidOrThreadToList_PIDLIST(nPtr, classID,
              procNamePtr, /* asf name */
              exeNamePtr, logFileNamePtr, listPos,
              ppid,        /* pid */
              -1,          /* thread flag */
              -1);          /* socket */
  unLockNameList_CORE(nPtr);
  return(retval);

} /* end AddPidToList_PIDLIST....................*/


/*----------------------------------------------------------
 * NAME:
 *  FindPidOrTidInList_PIDLIST 
 *
 * DESCRIPTION:
 *  find pid "ppid" in the pid list and return its pos 
 *
 * NOTES:
 *  private routine so list is not locked
 *
 *---------------------------------------------------------*/
static pidListElemType *FindPidOrTidInList_PIDLIST(baseListElemType *nPtr,
                                            int ppid, int *pos)
{
 pidListElemType *pidListElemPtr;
 int i;
  
  /* find the pid in the list */
  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL)
      return(NULL);

    if (ppid == pidListElemPtr->ppid) {
      *pos = i;
      return(pidListElemPtr);
    }

  } /* end for */

  return(NULL);
} /* end FindPidOrTidInList_PIDLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  IsPidNoForProcess_PIDLIST 
 *
 * DESCRIPTION:
 *  determine if pid number is for a "real" process or a 
 *  thread.
 *
 * NOTES:
 *  visible
 *
 *---------------------------------------------------------*/
int IsPidNoForProcess_PIDLIST(int ppid)
{
 int i;
 baseListElemType *nPtr;
 pidListElemType *pidListElemPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *) nPtr->arr[i]; 
    if (ppid == pidListElemPtr->ppid && pidListElemPtr->ttid == -1) {
       unLockNameList_CORE(nPtr);
       return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);
} /* IsPidNoForProcess_PIDLIST........*/


/*----------------------------------------------------------
 * NAME:
 *  ChangeProcessState_PIDLIST 
 *
 * DESCRIPTION:
 *  an access routine that changes the state of process
 *  "procNamePtr"
 *
 * NOTES:
 *  visible
 *
 *---------------------------------------------------------*/
int ChangeProcessState_PIDLIST(char *procNamePtr, int state,
                               int termination_reason)
{
 int i;
 pidListElemType *pidListElemPtr;
 baseListElemType *nPtr;

#ifdef PIDS
printf("ChangeProcessState_PIDLIST %s - state %d\n", procNamePtr, state);
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *) nPtr->arr[i]; 
    if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, procNamePtr) == 0) {
#ifdef TIMEOUTS
{  

printf("%d ChangeProcessState_PIDLIST - %s state %d stopid %ld, %s\n", getpid(), procNamePtr, state, pidListElemPtr->stopID, timestamp());
}
#endif
      pidListElemPtr->state        = state;
      pidListElemPtr->stateModFlag = MODIFIED;
      if (termination_reason != RETAIN_TERM_REASON)
        pidListElemPtr->termination_reason = termination_reason;

      unLockNameList_CORE(nPtr);
      return(0);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);
} /* ChangeProcessState_PIDLIST........*/



/*----------------------------------------------------------
 * NAME:
 *  GetProcessState_PIDLIST 
 *
 * DESCRIPTION:
 *  an access routine that returns the state of process
 *  "procNamePtr"
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int GetProcessState_PIDLIST(char *procNamePtr)
{
 int i;
 pidListElemType *pidListElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *) nPtr->arr[i]; 
    if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, procNamePtr) == 0) {
      unLockNameList_CORE(nPtr);
#ifdef PIDS
/* printf("GetProcessState_PIDLIST - name %s returning %d\n", procNamePtr, pidListElemPtr->state);  */
#endif
      return(pidListElemPtr->state);
    }
  }
  unLockNameList_CORE(nPtr);
  return(-1);
} /* GetProcessState_PIDLIST........*/

/*----------------------------------------------------------
 * NAME:
 *  GetNumInstances_PIDLIST
 *
 * DESCRIPTION:
 *  an access routine that returns the number of instances of
 *  the process "procNamePtr"
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int GetNumInstances_PIDLIST(char *procNamePtr)
{
 int i, count = 0;
 pidListElemType *pidListElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *) nPtr->arr[i];
    if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(count);
    }
    if (strcmp(pidListElemPtr->procNamePtr, procNamePtr) == 0) 
      count++;
  }
  unLockNameList_CORE(nPtr);
  return(count);
} /* GetNumInstances_PIDLIST........*/



/*----------------------------------------------------------
 * NAME:
 *  FreePidListElem_PIDLIST 
 *
 * DESCRIPTION:
 *  make all calls necessary to free memory associated
 *  with a pid list element
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void FreePidListElem_PIDLIST(pidListElemType *pidListElemPtr)
{
  doFree(pidListElemPtr->procNamePtr);
  doFree(pidListElemPtr->exeNamePtr);
  doFree(pidListElemPtr->logFileNamePtr);
  doFree(pidListElemPtr);

} /* FreePidListElem_PIDLIST........*/


/*----------------------------------------------------------
 * NAME:
 *  RemovePidFromListPrivate_PIDLIST 
 *
 * DESCRIPTION:
 *  the private routine that removes the specified element 
 *  from the pid list and frees the associated memory
 *
 * NOTES:
 *  this routine is private and assumnes the list is locked
 *
 *---------------------------------------------------------*/
static int RemovePidFromListPrivate_PIDLIST(baseListElemType *nPtr, 
        pidListElemType  *pidListElemPtr, int pos)
{
 int retval = 0;


  /* kill the associated read thread */
  if (pidListElemPtr->assocThread > 0) {
    retval = kill(pidListElemPtr->assocThread, SIGKILL);
    if (retval != 0)
      printfLLog(LOG_DEBUG, THREAD_NOT_EXIST, pthread_self(), 
              pidListElemPtr->procNamePtr, pidListElemPtr->ppid);
  }

  /* try and close the socket */
  if (pidListElemPtr->socket >= 0) {
    printfLLog(LOG_DEBUG, CLOSING_SOCKET, pidListElemPtr->procNamePtr,
                   pidListElemPtr->socket);
    close(pidListElemPtr->socket);
  }

  FreePidListElem_PIDLIST(pidListElemPtr);
  if (RemoveNameListElemAt_CORE(nPtr, PID_LIST, pos) == NULL)
    retval = -1;

  return(retval);

} /* end RemovePidFromListPrivate_PIDLIST..*/


/*----------------------------------------------------------
 * NAME:
 *  RemovePidFromListGivenPid_PIDLIST 
 *
 * DESCRIPTION:
 *  the public routine that removes the specified element 
 *  from the pid list and frees the associated memory
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int RemovePidFromListGivenPid_PIDLIST(int ppid)
{
 pidListElemType *pidListElemPtr;
 int pos, retval;
 baseListElemType *nPtr;

#ifdef PIDS
printf("RemovePidFromListGivenPid_PIDLIST - pid %d\n", ppid);
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  retval = 0;

  pidListElemPtr = FindPidOrTidInList_PIDLIST(nPtr, ppid, &pos); 
  if (pidListElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
  } 
  retval = RemovePidFromListPrivate_PIDLIST(nPtr, pidListElemPtr, pos);

  unLockNameList_CORE(nPtr);

  return(retval);

} /* end RemovePidFromListGivenPid_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  RemovePidFromListGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  remove the pid from the pid list with the name "namePtr"
 *  and free the associated memory
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int RemovePidFromListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, retval;
 baseListElemType *nPtr;

#ifdef PIDS
printf("RemovePidFromListGivenName_PIDLIST - name %s\n", namePtr);
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  retval = 0;

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
      retval =  RemovePidFromListPrivate_PIDLIST(nPtr, pidListElemPtr, i);
      unLockNameList_CORE(nPtr);
      return(retval);
     
    } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end RemovePidFromListGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  RemoveAll_PIDLIST 
 *
 * DESCRIPTION:
 *  remove all elements from the pid list and free the 
 *  associated memory
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int RemoveAll_PIDLIST()
{
 pidListElemType *pidListElemPtr;
 baseListElemType *nPtr;
 int i;

#ifdef PIDS
printf("RemoveAll_PIDLIST\n");
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL)
       return(-1);

    FreePidListElem_PIDLIST(pidListElemPtr);
  } /* end for loop */

  
  RemoveNameListAll_CORE(nPtr, PID_LIST);

  return(0);

} /* RemoveAll_PIDLIST.................*/


/*----------------------------------------------------------
 * NAME:
 *  GetNameGivenListPos_PIDLIST 
 *
 * DESCRIPTION:
 *  given the position in the Subsystem Control window,
 *  return the name.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
/*char *GetNameGivenListPos_PIDLIST(char *categoryNamePtr, int listPos) */
char *GetNameGivenListPos_PIDLIST(int listPos)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    } 
    if (pidListElemPtr->listPos == listPos) {
      unLockNameList_CORE(nPtr);
#ifdef NAMES
printf("GetNameGivenListPos_PIDLIST - listpos %d returning %s\n", pidListElemPtr->listPos, pidListElemPtr->procNamePtr); 
#endif

      return(pidListElemPtr->procNamePtr);
    } 

  } 

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetNameGivenListPos_PIDLIST...............*/

/*----------------------------------------------------------
 * NAME:
 *  GetListPosGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the name given the position in the Subsystem Control window,
 *  if no match, return -1
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetListPosGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

#ifdef PIDS
/* printf("GetListPosGivenName_PIDLIST - name %s\n", namePtr); */
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) { /* got a match */
      unLockNameList_CORE(nPtr);
#ifdef PIDS
/* printf("GetListPosGivenName_PIDLIST - name %s returning %d\n", namePtr, pidListElemPtr->listPos); */
#endif

      return(pidListElemPtr->listPos);
    }
  }

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetListPosGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  GetNameGivenMainWid_PIDLIST 
 *
 * DESCRIPTION:
 *  given the main widget for the SYSQueue window, return
 *  the name of the associated subsystem
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetNameGivenMainWid_PIDLIST(Widget mainWid)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(NULL);
    } 
    if (pidListElemPtr->mainWid == mainWid) {
      unLockNameList_CORE(nPtr);
#ifdef NAMES
printf("GetNameGivenMainWid_PIDLIST wid 0x%xs returning %s\n", mainWid, pidListElemPtr->procNamePtr);
#endif

      return(pidListElemPtr->procNamePtr);
    } /* end mainWid compare */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetNameGivenMainWid_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetNameGivenPid_PIDLIST 
 *
 *
 * DESCRIPTION:
 *  given the pid, return the process name
 *
 * NOTES:
 *   VISIBLE
 *
 *
 *---------------------------------------------------------*/
char *GetNameGivenPid_PIDLIST(int ppid)
{
 pidListElemType *pidListElemPtr;
 int pos;
 baseListElemType *nPtr;

#ifdef NAMES
printf("GetNameGivenPid_PIDLIST - pid %d... ", ppid); fflush(stdout);
#endif

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL) {
#ifdef NAMES
printf("couldnt lock pid list!\n");
#endif
     return(NULL);
  }

  pidListElemPtr = FindPidOrTidInList_PIDLIST(nPtr, ppid,  &pos);
  if (pidListElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
#ifdef NAMES
printf("couldnt find pid/tid %d in list!\n", ppid); fflush(stdout);
#endif
    return(NULL);
  } 
  unLockNameList_CORE(nPtr);
#ifdef NAMES
printf("returning %s\n", pidListElemPtr->procNamePtr); fflush(stdout);
#endif
  return(pidListElemPtr->procNamePtr);
 
} /* end GetNameGivenPid_PIDLIST...........*/

/*----------------------------------------------------------
 * NAME:
 *  GetPidGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name, return the pid
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int GetPidGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, ppid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
      ppid = pidListElemPtr->ppid;
      unLockNameList_CORE(nPtr);
#ifdef PIDS
printf("GetPidGivenName_PIDLIST %s returning %d\n", namePtr, ppid);
#endif
      return(ppid);
      
    } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetPidGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  SetPidGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name, place the process's pid in the
 *  pid list.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetPidGivenName_PIDLIST(char *namePtr, int ppid)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }
    if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
      pidListElemPtr->ppid = ppid;
      unLockNameList_CORE(nPtr);
      return(ppid);
     
    } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetPidGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  SetlogfpGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name, place the process's log file pointer
 *  in the pid list.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetlogfpGivenName_PIDLIST(char *namePtr, FILE *logfp)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    pidListElemPtr->logfp = logfp;
    unLockNameList_CORE(nPtr);
    return(0);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetlogfpGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  ClearlogfpGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  clear the log file pointer in the pid list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int ClearlogfpGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    pidListElemPtr->logfp = NULL;
    unLockNameList_CORE(nPtr);
    return(0);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ClearlogfpGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetMainWidGivenName_PIDLIST 
 *
 *
 * DESCRIPTION:
 *  return the main window widget for process with name
 *  "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetMainWidGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget mainWid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    mainWid = pidListElemPtr->mainWid;
    unLockNameList_CORE(nPtr);
    return(mainWid);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetMainWidGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  GetInfoLabelGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  return the subsys's label info widget 
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetInfoLabelGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->infoLabel;
    unLockNameList_CORE(nPtr);
    return(wid);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetInfoLabelGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetJobIdListGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  return the subsys's job id list widget 
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetJobIdListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->jobIdList;
    unLockNameList_CORE(nPtr);
    return(wid);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetJobIdListGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetPlatRevSeqListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's plat/rev/seq list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetPlatRevSeqListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->platRevSeqList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetPlatRevSeqListGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetFrameListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's frame list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetFrameListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->frameList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetFrameListGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetMediaListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's media list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetMediaListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->mediaList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetMediaListGivenName_PIDLIST...............*/

/*----------------------------------------------------------
 * NAME:
 *  GetModeListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's mode list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetModeListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->modeList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetModeListGivenName_PIDLIST...............*/





/*----------------------------------------------------------
 * NAME:
 *  GetRequestListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's request list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetRequestListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->requestList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetRequestListGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  GetTypeListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's type list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetTypeListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->typeList;
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetTypeListGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetStatusListGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's status list widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetStatusListGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->statusList;
#ifdef PIDS
printListWidgets(pidListElemPtr);
printf("GetStatusListGivenName_PIDLIST: %s returning 0x%x %s\n", namePtr, wid, XtName(wid));
#endif
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetStatusListGivenName_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  GetScrollBarGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the subsys's scroll bar widget
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
Widget GetScrollBarGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    wid = pidListElemPtr->scrollBar;
printListWidgets(pidListElemPtr);
printf("GetScrollBarGivenName_PIDLIST: %s returning 0x%x %s\n", namePtr, wid, XtName(wid));
    unLockNameList_CORE(nPtr);
    return(wid);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetScrollBarGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetBottomInfoLabelGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  return the subsys's info label widget 
 *  for process with name "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
Widget GetBottomInfoLabelGivenName_PIDLIST(char *namePtr, int labelID)
{
 pidListElemType *pidListElemPtr;
 int i;
 Widget wid;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
     switch(labelID) {
        case QUE_HEARTBEAT_LABEL_ID: 
           wid = pidListElemPtr->queBottomInfoLabel_0;
           break;
           break;
        default:
           wid = NULL;
           break;
     }
     unLockNameList_CORE(nPtr);
     return(wid);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetBottomInfoLabelGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  AddSocketTo_PIDLIST
 *
 * DESCRIPTION:
 *  add the socket and the associated thread id  to the 
 *  pid list for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int AddSocketTo_PIDLIST(char *namePtr, int socket, tid_t assocThread)
{
 int i;
 baseListElemType *nPtr;
 pidListElemType  *pidListElemPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  /* find the process/thread in the list */
  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr =  (pidListElemType *)nPtr->arr[i];

    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (strcmp(namePtr, pidListElemPtr->procNamePtr) == 0) {
     /* found name, add the socket */
     pidListElemPtr->socket      = socket;
     pidListElemPtr->assocThread = assocThread;
#ifdef SOCKETS
printf("AddSocketTo_PIDLIST: adding pid %d, socket %d to %s\n", assocThread, socket, namePtr);
#endif
     unLockNameList_CORE(nPtr);
     return(0);
    }
  } /* end for i */

  /* cant find process */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end AddSocketTo_PIDLIST............*/

/*----------------------------------------------------------
 * NAME:
 *  ChangeExeName_PIDLIST
 *
 * DESCRIPTION:
 *  change the exe name in the pid list for process with name "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int ChangeExeName_PIDLIST(char *namePtr, char *exeNamePtr, int pid)
{
 int i;
 baseListElemType *nPtr;
 pidListElemType  *pidListElemPtr;

#ifdef PIDS
printf("ChangeExeName_PIDLIST: entering, %s to %s, pid %d\n", namePtr, exeNamePtr, pid);
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  /* find the process/thread in the list */
  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr =  (pidListElemType *)nPtr->arr[i];

    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
    }

    if (strcmp(namePtr, pidListElemPtr->procNamePtr) == 0) {
     /* found name, change the exename */
     if (pidListElemPtr->exeNamePtr != NULL)
       doFree(pidListElemPtr->exeNamePtr);
     pidListElemPtr->exeNamePtr = (char *)doMalloc(strlen(exeNamePtr) + 1);
     if (pidListElemPtr->exeNamePtr == NULL)
        return(-1);
     pidListElemPtr->ppid = pid; 
#ifdef PIDS
printf("ChangeExeName_PIDLIST: doing it...%s to %s\n", namePtr, exeNamePtr);
#endif
     strcpy(pidListElemPtr->exeNamePtr, exeNamePtr);

     unLockNameList_CORE(nPtr);
     return(0);
    }
  } /* end for i */

  /* cant find process */
  unLockNameList_CORE(nPtr);
  return(-1);

} /* end ChangeExeName_PIDLIST............*/


/*----------------------------------------------------------
 * NAME:
 *  GetAssocThreadGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetAssocThreadGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, assocThread;
 baseListElemType *nPtr;

#ifdef PIDS
printf("GetAssocThreadGivenName_PIDLIST: %s\n", namePtr);
#endif
  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    assocThread = pidListElemPtr->assocThread;
    unLockNameList_CORE(nPtr);
#ifdef PIDS
printf("GetAssocThreadGivenName_PIDLIST: %s returning %d\n", namePtr, assocThread);
#endif
    return(assocThread);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetAssocThreadGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  GetSocketGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *   VISIBLE
 *  -- for evaluation
 *
 *---------------------------------------------------------*/
int GetSocketGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, socket;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
#ifdef SOCKETS
/* printf("GetSocketGivenName_PIDLIST: %s, comparing with %s\n", namePtr, pidListElemPtr->procNamePtr); fflush(stdout); */
#endif
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    socket = pidListElemPtr->socket;
    unLockNameList_CORE(nPtr);
#ifdef SOCKETS
printf("GetSocketGivenName_PIDLIST: %s, returning socket %d\n", namePtr, socket); 
#endif

    return(socket);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetSocketGivenName_PIDLIST...............*/

/*----------------------------------------------------------
 * NAME:
 *  GetNameGivenSocket_PIDLIST
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetNameGivenSocket_PIDLIST(int socket)
{
 pidListElemType *pidListElemPtr;
 int i;
 char *namePtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
#ifdef SOCKETS
/* printf("GetNameGivenSocket_PIDLIST: socket %d, comparing with %d\n", socket, pidListElemPtr->socket); fflush(stdout); */
#endif

   if (pidListElemPtr->socket == socket) {
    namePtr = doMalloc(strlen(pidListElemPtr->procNamePtr)+1);
    strcpy(namePtr, pidListElemPtr->procNamePtr);
    unLockNameList_CORE(nPtr);
#ifdef SOCKETS
printf("GetNameGivenSocket_PIDLIST: %d returning name %s\n", socket, namePtr ); 
#endif
    return(namePtr);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetNameGivenSocket_PIDLIST...............*/

/*---------------------------------------------------------*/
char *RemoveSocket_PIDLIST(int socket)
{
 pidListElemType *pidListElemPtr;
 int i;
 char *namePtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }

   if (pidListElemPtr->socket == socket) {
      pidListElemPtr->socket      = -1;
      unLockNameList_CORE(nPtr);
      return(NULL);

   }
  } /* end for i */

  return(NULL);

} /* end RemoveSocket_PIDLIST...............*/



/*----------------------------------------------------------
 * NAME:
 *  GetExeNameGivenName_PIDLIST
 *
 * DESCRIPTION:
 *  return the executable name for a pidlist entry
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char *GetExeNameGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, socket;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(NULL);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    socket = pidListElemPtr->socket;
    unLockNameList_CORE(nPtr);
    return(pidListElemPtr->exeNamePtr);

   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(NULL);

} /* end GetExeNameGivenName_PIDLIST...............*/




/*----------------------------------------------------------
 * NAME:
 *  GetHeadOf_PIDLIST 
 *
 * DESCRIPTION:
 *  remove the head of the pid list from  the list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
pidListElemType *GetHeadOf_PIDLIST()
{
 void *retvalPtr;
 pidListElemType *pidListElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
      return(NULL);

  retvalPtr = RemoveNameListElemAt_CORE(nPtr, PID_LIST,0);
  if (retvalPtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(NULL);
  }
  pidListElemPtr = (pidListElemType *)retvalPtr;
  unLockNameList_CORE(nPtr);

  return(pidListElemPtr);

} /* GetHeadOf_PIDLIST.............*/


/*----------------------------------------------------------
 * NAME:
 *  doGetCopy_private_PDILIST
 *
 * DESCRIPTION:
 *  a private routine that gets a copy of an element from 
 *  the pidlist
 *
 * NOTES:
 *  private use only -- does not lock list
 *
 *---------------------------------------------------------*/
static pidListElemType *doGetCopy_private_PDILIST(pidListElemType *pidListElemPtr)
{
 pidListElemType *retpidListElemPtr;

  retpidListElemPtr = (pidListElemType *)doMalloc(sizeof(pidListElemType));
  if (retpidListElemPtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }

  /* clear the memory */
  bzero(retpidListElemPtr, sizeof(pidListElemType) );

  retpidListElemPtr->classID =  pidListElemPtr->classID;

  retpidListElemPtr->procNamePtr = 
           (char *)doMalloc(strlen(pidListElemPtr->procNamePtr)+1); 
  if (retpidListElemPtr->procNamePtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }
  strcpy(retpidListElemPtr->procNamePtr,
         pidListElemPtr->procNamePtr);

  retpidListElemPtr->exeNamePtr = 
           (char *)doMalloc(strlen(pidListElemPtr->exeNamePtr)+1); 
  if (retpidListElemPtr->exeNamePtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }
  strcpy(retpidListElemPtr->exeNamePtr,
         pidListElemPtr->exeNamePtr);

  retpidListElemPtr->logFileNamePtr = 
           (char *)doMalloc(strlen(pidListElemPtr->logFileNamePtr)+1); 
  if (retpidListElemPtr->logFileNamePtr == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(NULL);
  }
  strcpy(retpidListElemPtr->logFileNamePtr,
         pidListElemPtr->logFileNamePtr);

  retpidListElemPtr->logfp               =  pidListElemPtr->logfp;
  retpidListElemPtr->listPos             =  pidListElemPtr->listPos;
  retpidListElemPtr->externPos             =  pidListElemPtr->externPos;
  retpidListElemPtr->ppid                =  pidListElemPtr->ppid;
  retpidListElemPtr->ttid                =  pidListElemPtr->ttid;
  retpidListElemPtr->state               =  pidListElemPtr->state;
  retpidListElemPtr->stateModFlag        =  pidListElemPtr->stateModFlag;
  retpidListElemPtr->termination_reason  =  pidListElemPtr->termination_reason;
  retpidListElemPtr->socket              =  pidListElemPtr->socket;
  retpidListElemPtr->assocThread         =  pidListElemPtr->assocThread;
  retpidListElemPtr->mainWid             =  pidListElemPtr->mainWid;
  retpidListElemPtr->infoLabel           =  pidListElemPtr->infoLabel;
  retpidListElemPtr->jobIdList             =  pidListElemPtr->jobIdList;
  retpidListElemPtr->platRevSeqList             =  pidListElemPtr->platRevSeqList;
  retpidListElemPtr->frameList             =  pidListElemPtr->frameList;
  retpidListElemPtr->mediaList             =  pidListElemPtr->mediaList;
  retpidListElemPtr->modeList             =  pidListElemPtr->modeList;
  retpidListElemPtr->requestList             =  pidListElemPtr->requestList;
  retpidListElemPtr->typeList             =  pidListElemPtr->typeList;
  retpidListElemPtr->statusList             =  pidListElemPtr->statusList;
  retpidListElemPtr->scrollBar             =  pidListElemPtr->scrollBar;

  return(retpidListElemPtr);
} /* end doGetCopy_private_PDILIST */


/*----------------------------------------------------------
 * NAME:
 *  GetCopyOfElemIn_PIDLIST 
 *
 * DESCRIPTION:
 *  a public routine that gets a copy of the element at 'i'
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
pidListElemType *GetCopyOfElemIn_PIDLIST(int i)
{
 pidListElemType *pidListElemPtr;
 pidListElemType *retpidListElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
      return(NULL);

  if (i >= nPtr->nsize) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

  if (i < 0) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

  pidListElemPtr = nPtr->arr[i];
  if (pidListElemPtr == NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_PID_EL);
    unLockNameList_CORE(nPtr);
    return(NULL);
  }

  retpidListElemPtr = doGetCopy_private_PDILIST(pidListElemPtr);
#ifdef COPY_PIDS
printf("GetCopyOfElemIn_PIDLIST i %d name %s\n", i, pidListElemPtr->procNamePtr);
#endif
 
  unLockNameList_CORE(nPtr);
  return(retpidListElemPtr);

} /* end GetCopyOfElemIn_PIDLIST................*/

/*----------------------------------------------------------
 * NAME:
 *  GetElemGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *  remove or copy --depending on mode flag-- the named process 
 *  from the pid list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
pidListElemType *GetElemGivenName_PIDLIST(char *namePtr, int mode)
{
 void *retvalPtr;
 int  i;
 pidListElemType *pidListElemPtr, *retpidListElemPtr = NULL;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
      return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
     printfLLog(LOG_ERR, CANNOT_GET_PID_EL);
     unLockNameList_CORE(nPtr);
     return(NULL);
    }

    if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
     if (mode == DO_REMOVE) { 
      retvalPtr = RemoveNameListElemAt_CORE(nPtr, PID_LIST,i);
      retpidListElemPtr = (pidListElemType *)retvalPtr;
     } /* end do remove */

     if (mode == DO_COPY) {
      retpidListElemPtr = doGetCopy_private_PDILIST(pidListElemPtr);
     } /* end do copy */

     if (mode == DO_GET_REAL) {
      retpidListElemPtr = pidListElemPtr;
     } /* end do copy */
    } /* end if state */

  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(retpidListElemPtr);

} /* GetElemGivenName_PIDLIST.........*/



/*----------------------------------------------------------
 * NAME:
 *  GetFirstProcessWithStateOf_PIDLIST 
 *
 * DESCRIPTION:
 *  remove or copy --depending on mode-- the named process with
 *  the specified state from the pid list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
pidListElemType *GetFirstProcessWithStateOf_PIDLIST(int state, int mode)
{
 void *retvalPtr;
 int  i;
 pidListElemType *pidListElemPtr, *retpidListElemPtr = NULL;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
      return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
     printfLLog(LOG_ERR, CANNOT_GET_PID_EL);
     unLockNameList_CORE(nPtr);
     return(NULL);
    }

    if (pidListElemPtr->state == state) {
     if (mode == DO_REMOVE) { 
      retvalPtr = RemoveNameListElemAt_CORE(nPtr, PID_LIST,i);
      retpidListElemPtr = (pidListElemType *)retvalPtr;
     } /* end do remove */

     if (mode == DO_COPY) {
      retpidListElemPtr = doGetCopy_private_PDILIST(pidListElemPtr);
     } /* end do copy */

    } /* end if state */

  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(retpidListElemPtr);

} /* GetFirstProcessWithStateOf_PIDLIST.........*/


/*----------------------------------------------------------
 * NAME:
 *  GetFirstProcessWithModifedState_PIDLIST 
 *
 * DESCRIPTION:
 *  remove or copy --depending on mode-- the first process with
 *  a modifed state from the pid list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
pidListElemType *GetFirstProcessWithModifedState_PIDLIST(int mode)
{
 void *retvalPtr;
 int  i;
 pidListElemType *pidListElemPtr, *retpidListElemPtr = NULL;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
      return(NULL);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      printfLLog(LOG_ERR, CANNOT_GET_PID_EL);
      unLockNameList_CORE(nPtr);
      return(NULL);
    }
    if (pidListElemPtr->stateModFlag == MODIFIED) {
      if (mode == DO_REMOVE) { 
        retvalPtr = RemoveNameListElemAt_CORE(nPtr, PID_LIST,i);
        retpidListElemPtr = (pidListElemType *)retvalPtr;
      } /* end do remove */

      if (mode == DO_COPY) 
        retpidListElemPtr = doGetCopy_private_PDILIST(pidListElemPtr);

      pidListElemPtr->stateModFlag = UNMODIFIED;
      unLockNameList_CORE(nPtr);
      return(retpidListElemPtr);
    } /* end if state modified */

  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(retpidListElemPtr);

} /* GetFirstProcessWithModifedState_PIDLIST.........*/


/*----------------------------------------------------------
 * NAME:
 *  GetClassGivenName_PIDLIST 
 *
 * DESCRIPTION:
 *
 * NOTES:
 *   VISIBLE
 *  -- for evaluation
 *
 *---------------------------------------------------------*/
int GetClassGivenName_PIDLIST(char *namePtr)
{
 pidListElemType *pidListElemPtr;
 int i, classID;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    classID = pidListElemPtr->classID;
    unLockNameList_CORE(nPtr);
    return(classID);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetClassGivenName_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveTimeOut_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name, remove the start timeout 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int RemoveTimeOut_PIDLIST(char *namePtr, int  intervalTypeID)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
    pidListElemPtr = (pidListElemType *)nPtr->arr[i];
    if (pidListElemPtr == NULL) {
      unLockNameList_CORE(nPtr);
      return(-1);
  }
  if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    switch(intervalTypeID) {
      case START_INTERVAL_ID:
         if (pidListElemPtr->startID != 0) {
            XtRemoveTimeOut(pidListElemPtr->startID);
            pidListElemPtr->startID = 0;
         }
         break;

      case STOP_INTERVAL_ID:
         if (pidListElemPtr->stopID != 0) {
           XtRemoveTimeOut(pidListElemPtr->stopID);
           pidListElemPtr->stopID = 0;
          }
          break;

      case HALT_INTERVAL_ID:
          if (pidListElemPtr->haltID != 0) {
           XtRemoveTimeOut(pidListElemPtr->haltID);
           pidListElemPtr->haltID = 0;
           }
           break;

      case HEALTH_INTERVAL_ID:
           if (pidListElemPtr->healthID != 0) {
            XtRemoveTimeOut(pidListElemPtr->healthID);
            pidListElemPtr->healthID = 0;
           }
           break;

      case MSG_SENT_INTERVAL_ID:
           if (pidListElemPtr->msg_sentID != 0) {
            XtRemoveTimeOut(pidListElemPtr->msg_sentID);
            pidListElemPtr->msg_sentID = 0;
           }
           break;

      case CLEANUP_INTERVAL_ID:
           if (pidListElemPtr->cleanupID != 0) {
            XtRemoveTimeOut(pidListElemPtr->cleanupID);
            pidListElemPtr->cleanupID = 0;
           }
           break;

      default:
           unLockNameList_CORE(nPtr);
           return(-1);
           break;
    } /* end switch */

#ifdef TIMEOUTS
{  

printf("\tRemoveTimeOut: %s type %d stopid %ld, %s\n", namePtr, intervalTypeID, pidListElemPtr->stopID, timestamp());
}

#endif
    unLockNameList_CORE(nPtr);
    return(0);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end RemoveTimeOut_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 * GetTimeOutID_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name and timeout type, return the timeout value
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
XtIntervalId GetTimeOutID_PIDLIST(char *namePtr, int intervalTypeID)
{
 pidListElemType *pidListElemPtr;
 int i, timeoutVal;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    switch(intervalTypeID) {
     case START_INTERVAL_ID:
      timeoutVal = pidListElemPtr->startID ;
      break;

     case STOP_INTERVAL_ID:
      timeoutVal = pidListElemPtr->stopID  ;
      break;

     case HALT_INTERVAL_ID:
      timeoutVal = pidListElemPtr->haltID  ;
      break;

     case HEALTH_INTERVAL_ID:
      timeoutVal = pidListElemPtr->healthID ;
      break;

     case MSG_SENT_INTERVAL_ID:
      timeoutVal = pidListElemPtr->msg_sentID ;
      break;

     case CLEANUP_INTERVAL_ID:
      timeoutVal = pidListElemPtr->cleanupID ;
      break;

     default:
      timeoutVal = 0;
      unLockNameList_CORE(nPtr);
      return(-1);
      break;
    } /* end switch */
    unLockNameList_CORE(nPtr);
#ifdef TIMEOUTS
printf("\tGetTimeOut: %s type %d returning %d stopid %ld, %s\n", namePtr, intervalTypeID, timeoutVal, pidListElemPtr->stopID, timestamp());

#endif
    return(timeoutVal);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end GetTimeOutID_PIDLIST...............*/

/*----------------------------------------------------------
 * NAME:
 * SetTimeOutID_PIDLIST 
 *
 * DESCRIPTION:
 *  given the process name, remove the start timeout 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int SetTimeOutID_PIDLIST(char *namePtr, int intervalTypeID,
                         XtIntervalId intervalID)
{
 pidListElemType *pidListElemPtr;
 int i;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   pidListElemPtr = (pidListElemType *)nPtr->arr[i];
   if (pidListElemPtr == NULL) {
     unLockNameList_CORE(nPtr);
     return(-1);
   }
   if (strcmp(pidListElemPtr->procNamePtr, namePtr) == 0) {
    switch(intervalTypeID) {
     case START_INTERVAL_ID:
      pidListElemPtr->startID = intervalID;
      break;

     case STOP_INTERVAL_ID:
      pidListElemPtr->stopID  = intervalID;
      break;

     case HALT_INTERVAL_ID:
      pidListElemPtr->haltID  = intervalID;
      break;

     case HEALTH_INTERVAL_ID:
      pidListElemPtr->healthID = intervalID;
      break;

     case MSG_SENT_INTERVAL_ID:
      pidListElemPtr->msg_sentID = intervalID;
      break;

     case CLEANUP_INTERVAL_ID:
      pidListElemPtr->cleanupID = intervalID;
      break;

     default:
      unLockNameList_CORE(nPtr);
      return(-1);
      break;
    } /* end switch */
#ifdef TIMEOUTS
printf("\tSetTimeOut: %s type %d interval %ld, %s\n", namePtr, intervalTypeID, intervalID, timestamp());


#endif
    unLockNameList_CORE(nPtr);
    return(0);
     
   } /* end strcmp */
  } /* end for i */

  unLockNameList_CORE(nPtr);
  return(-1);

} /* end SetTimeOutID_PIDLIST...............*/


/*----------------------------------------------------------
 * NAME:
 *  SetQCreqGivenPid_PIDLIST 
 *
 * DESCRIPTION:
 *  given the pid, return the process name
 *
 * NOTES:
 *   VISIBLE
 *
 *
 *---------------------------------------------------------*/
int SetQCreqGivenPid_PIDLIST(int ppid, qcReqType *qcReqPtr)
{
 pidListElemType *pidListElemPtr;
 int pos;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

  pidListElemPtr = FindPidOrTidInList_PIDLIST(nPtr, ppid,  &pos);
  if (pidListElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(-1);
  } 
  memmove(&pidListElemPtr->qcReq, qcReqPtr, sizeof( qcReqType));
  unLockNameList_CORE(nPtr);
  return(0);
 
} /* end SetQCreqGivenPid_PIDLIST...........*/


/*----------------------------------------------------------
 * NAME:
 *  GetQCreqGivenPid_PIDLIST 
 *
 *
 * DESCRIPTION:
 *  given the pid, return the process name
 *
 * NOTES:
 *   VISIBLE
 *
 *
 *---------------------------------------------------------*/
qcReqType *GetQCreqGivenPid_PIDLIST(int ppid)
{
 pidListElemType *pidListElemPtr;
 int pos;
 baseListElemType *nPtr;
 qcReqType *qcReqPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(NULL);

  pidListElemPtr = FindPidOrTidInList_PIDLIST(nPtr, ppid,  &pos);
  if (pidListElemPtr == NULL) {
    unLockNameList_CORE(nPtr);
    return(NULL);
  } 
  qcReqPtr = (qcReqType *)doMalloc(sizeof(qcReqType));
  memmove(qcReqPtr, &pidListElemPtr->qcReq, sizeof(qcReqType));
  unLockNameList_CORE(nPtr);
  return(qcReqPtr);
 
} /* end GetQCreqGivenPid_PIDLIST...........*/



/*----------------------------------------------------------
 * NAME:
 *  InsertWidgets_PIDLIST
 *
 * DESCRIPTION:
 *
 * NOTES:
 *  - for evaluation 
 *  - lock is performed in MoveElem_ 
 *
 *---------------------------------------------------------*/
int  InsertWidgets_PIDLIST(Widget mainWid)
{
 int retval;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(PID_LIST);
  if (nPtr == NULL)
     return(-1);

    /* pidListElemPtr = (pidListElemType *)nPtr->arr[i]; */

  unLockNameList_CORE(nPtr);
  return(retval);

} /* end InsertWidgets_PIDLIST..............*/
