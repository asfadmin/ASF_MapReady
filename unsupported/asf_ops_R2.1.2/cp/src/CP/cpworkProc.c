/* #define READ_LOCK_DEBUG /* */

/* #define STATE_FILE_DEBUG /* */

/*#define PRINT_ALL_DEFERRED  print every time a job is deferred in sysq */
/*#define LATER_JOB  debug later job deferral decisions */

#define JOB_DONE /* debug subsys job completion processing */
#define EXTRA_DEBUG /* turn on extra LOG_DEBUG syslog messages */

#define REALLY_DELETE_PRODUCTS /* really perform the rm on the products */


/* #define CB_DEBUG /* uncomment for CBdata debugging - question box*/
/* #define DEBUG /* uncomment for color state box debugging */
/* #define DEL_DEBUG /* uncomment for debugging of product deletion */


/* #define LABELS /* turn on for debugging labels */
/* #define LISTS  /* nancy: turn this on to debug scrolled queues */
/* #define MSGQ  /* nancy: turn this on to debug msg queue handling */
/* #define STATE_DEBUG /* uncomment for next-state debugging */
/* #define MSTR /* nancy: turn this on to debug master queue elements */
/* #define SYSQ /* nancy: turn this on to debug sysque elements */
/* #define FULL_SYSQ /* nancy: turn this on to debug sysque elements */
/* #define XWP /* nancy: turn this on to printf handleXwp calls */
/* #define PROC /* debug process state change */
/* #define SIG_DEBUG /* signal queue debugging */

static char sccsid_cpworkProc_c[] = "@(#)cpworkProc.c	4.171 97/04/14 12:22:21";

#define LOW_POSITION "Error updating display: %s position %d with %s"
#define HIGH_POSITION \
    "Error updating display: %s position %d; list only has %d elements"
#define LOW_POSITION_DEL "Error updating display: %s position %d "
#define HIGH_POSITION_DEL HIGH_POSITION

#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h> /* realpath -- but with our CFLAGS, it doesn't show up */
extern char *realpath(const char *, char *);
#include <sys/param.h> /* MAXPATHLEN */
#include <limits.h> /* PATH_MAX */
#include <netinet/in.h>
#include <sys/time.h>  /* gettimeofday */
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>  /* close */
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
#include <Xm/ScrolledW.h>
#include <Xm/TextF.h>
#include <task.h>

#include "CPmainQ.h"
#include "CPsubsysQ.h"
#include "CPerrorBox.h"
#include "CPinfoBox.h"
#include "odl.h"    /* contains tuan's value_data for agg */
#include "asfcommon.h"
#include "logUtils.h" /* printfLLog */
#include "memUtils.h" /* doMalloc */
#include "cpdefines.h"
#include "cpconfig.h"
#include "serverSocketXport.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"
#include "que_sys.h"
#include "que_xwp.h"
#include "CPquestionBox.h" /* need this after que_xwp.h ? */
#include "inet.h"
#include "cplogs.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "cpmainrtns.h"
#include "utils.h"
#include "xutils.h"
#include "cpmenu.h"
#include "cpbuildMsg.h"
#include "validate.h"
#include "pps.h"

/* begin global variables ***********************/
extern RDSqueueIDtype RDSqueue[MAX_TAPE_ID];

Widget display_msg();

int assignRDS(char *mediaIDPtr,char *mediaTypePtr);
void clearQueueID(ODL sysODLreq, char *namePTr);
void adjustQueueID(mstrqListElemType *mqel,char* namePtr);

char *getWidgetName(); /* debug routine */
extern int GLOBAL_MAX_TAPE_ID;
extern int numRDSqueue[MAX_RDS];
extern int GLOBAL_exitingCP;
extern int GLOBAL_signalPid[MAX_CHILD_PROCESSES];
extern int GLOBAL_signalStatus[MAX_CHILD_PROCESSES];
extern int GLOBAL_signalPidCount;
extern int totalMQentries;
extern Widget		UxTopLevel;
extern XtAppContext	UxAppContext;

extern Widget *CPstateRC[];
extern int getRDSnumGivenName(char* namePtr);
extern int setRDSnum_MSTRQLIST(int jobId,char* namePtr);
extern XtCallbackProc TapeMountedCB(Widget w, XtPointer dPtr, XtPointer cPtr);
extern XtCallbackProc TapeNotMountedCB(Widget w,XtPointer dPtr,XtPointer cPtr);

extern char *GLOBAL_ppsLastModeStr, *GLOBAL_ppsLastTypeStr;
extern int GLOBAL_ppsLastMode, GLOBAL_ppsLastType;

extern char GLOBAL_lastRDSMediaPrompted[MAX_RDS][512];
extern char GLOBAL_lastRDSMediaMounted[MAX_RDS][512];
extern char GLOBAL_lastASPMediaPrompted[512];
extern char GLOBAL_lastASPMediaMounted[512];

extern void popupTextBox(int jobId, char *subsysName);

int GLOBAL_pos = 0;

Widget rdsMountDialog, aspMountDialog;
static char GLOBAL_savedBaseName[256];
static char GLOBAL_currentStateFile[256];
static char GLOBAL_lastSavedStateFile[256];

/* end global variables .................*/

char *getCurrentStateFileName()
{
  return(GLOBAL_currentStateFile);
}

int assignRDS(char *mediaIDPtr, char *mediaTypePtr)
{
  int first,less,i,avail,num;
  char *mediaType;
  int match[MAX_RDS], found;
           

  avail = -1;
  if (GLOBAL_MAX_TAPE_ID > 0) {
     for (i = 0; i < MAX_TAPE_ID; i++) {
      if (RDSqueue[i].queueIndx == -1) {
          if (avail == -1) 
              avail = i;
          continue;
      }
      if (strcmp (RDSqueue[i].IDstr,mediaIDPtr) == 0) {
          RDSqueue[i].useCount ++;
          printf("found old record!!!! %s\n",mediaIDPtr);
          return (RDSqueue[i].queueIndx);
      }
     }
  } else {
    avail = 0;
  }

  if (avail == -1){
     printf("Queue full,....\n");
     return(0);
  }
  /* found no old record, get new entry */
  num = getNumRDSs();
  for (i = 0; i < num; i++) {
      match[i] = 0;
      mediaType = getRDSmediaType(i);
      printf("mediaType= %s, i=%d",mediaType,i);
      if (strcmp (mediaType, "BOTH" ) == 0) {
         match[i] = 1;
      } else {
        if (strcmp (mediaTypePtr, mediaType ) == 0) {
           match[i] = 1;
        }
        else
           match[i] = 0;
      }
  }       

  strcpy(RDSqueue[avail].IDstr,mediaIDPtr);
  found = -1;
  first = 0;
  for (i = 0; i < num; i++) {
      if (match[i] == 1)  {
         if (first == 0) {
             first ++;
             less = numRDSqueue[i];
         }
         printf ("match1=%d\n",match[i]);
         if (numRDSqueue[i] <= less) {
            printf("i=%d, less = %d, numRDSqueue=%d", i,less,  numRDSqueue[i]);
          less = numRDSqueue[i];
          found = i;
         }
      }
  } 
  if (found == -1)  {
   printf("ERROR: no RDS is available for this job\n");
   return(-1);
  }
  printf(" assign new entry: %d\n",found);
  RDSqueue[avail].queueIndx = found;
  RDSqueue[avail].useCount = 1;
  GLOBAL_MAX_TAPE_ID ++;
  numRDSqueue[found] ++;

  return (found);
}


void clearQueueID(ODL ODLmsg, char *namePtr)
{
  char *mediaIDPtr, RDSnum;
  int i;
  
  RDSnum = getRDSnumGivenName(namePtr);
  mediaIDPtr = ODLGetStr(ODLmsg, BODY_MEDIA_ID);
  if (mediaIDPtr == NULL) {
      printf(" Media queue record messed up\n");
      return;
  }
  printf(" clear queue: %s\n",mediaIDPtr);
  for (i = 0; i < MAX_TAPE_ID; i++) {
      if (strcmp (RDSqueue[i].IDstr,mediaIDPtr) == 0) {
          if (RDSqueue[i].queueIndx == RDSnum) {
             if (RDSqueue[i].useCount == 1)  {
                printf("*** deleting entry\n");
                RDSqueue[i].useCount = 0;
                RDSqueue[i].queueIndx = -1;
                strcpy(RDSqueue[i].IDstr,"");
                GLOBAL_MAX_TAPE_ID --; 
                numRDSqueue[RDSnum] --;
             } else if (RDSqueue[i].useCount > 1)  {
                printf("*** decrementing by 1 : %d\n",RDSqueue[i].useCount);
                RDSqueue[i].useCount --;
             }
          }
      }
    }
}

void adjustQueueID(mstrqListElemType *mqel,char* namePtr)
{
  int i,avail,RDSnum;

  printf(" **** adjusting queue ID\n");
  avail = -1;
  if (namePtr == NULL)
     RDSnum = getRDSnumGivenName(mqel ->namePtr);
  else
     RDSnum = getRDSnumGivenName(namePtr);
  if (GLOBAL_MAX_TAPE_ID > 0) {
     for (i = 0; i < MAX_TAPE_ID; i++) {
      if (RDSqueue[i].queueIndx == -1) {
          if (avail == -1)
              avail = i;
          continue;
      }
      if (strcmp (RDSqueue[i].IDstr,mqel->mqLabel[MAIN_MEDIA_LIST]) == 0) {
          if (RDSqueue[i].queueIndx == RDSnum) {
             RDSqueue[i].useCount ++;
             printf("found old record!!!! %s\n",mqel->mqLabel[MAIN_MEDIA_LIST]);
             printf(" Total = %d\n", RDSqueue[i].useCount);
             return;
          }
      }
     } /* end for */
  } else {
    avail = 0;
  }

  if (avail == -1){
     printf("Queue full in adjust,....\n");
     return;
  }
  strcpy(RDSqueue[avail].IDstr,mqel->mqLabel[MAIN_MEDIA_LIST]);
  printf(" assign new entry in adjust: %d\n",RDSnum);
  RDSqueue[avail].queueIndx = RDSnum;
  RDSqueue[avail].useCount = 1;
  GLOBAL_MAX_TAPE_ID ++;
  numRDSqueue[RDSnum] ++;

  return;
}

int writeStateFile() /* return 0 for success */
{
  FILE *fp;
  char *stateFileName;
  int retval;

  stateFileName = getCurrentStateFileName();
  if ((fp = fopen(stateFileName, "w")) != NULL) {
        retval = chmod(stateFileName, S_IRUSR |  S_IWUSR | S_IRGRP | S_IWGRP);
        retval = WriteOut_MSTRQLIST(fp);
        fclose(fp);
        return(retval);
  }
  return(0);

}

/*----------------------------------------------------------
 * NAME:
 *  getModeStr
 *
 * DESCRIPTION:
 *   return "MODE" string given integer mode id
 *
 * NOTES:
 *--------------------------------------------------------*/
char *getModeStr(int mode)
{
/*  return ((mode == MODE_SCANSAR_ID) ? PPS_MSG_MODE_SS : PPS_MSG_MODE_CTS);*/

  if (mode == MODE_SCANSAR_ID)
    return(PPS_MSG_MODE_SS);
  else if (mode == MODE_CONTINUOUS_ID)
    return(PPS_MSG_MODE_CTS);
  else
    return (NULL);

} /* getModeStr */

/*----------------------------------------------------------
 * NAME:
 *  getTypeStr
 *
 * DESCRIPTION:
 *   return "MSG_TYPE" string given integer message type id
 *
 * NOTES:
 *--------------------------------------------------------*/
char *getTypeStr(int type)
{
  /*return ((type == REQ_TYPE_SCAN_ID) ? "SCAN" : "FRAME"); */
  return ((type == REQ_TYPE_SCAN_ID) ? PPS_MSG_TYPE_SCAN : PPS_MSG_TYPE_FRAME);
  if (type == REQ_TYPE_SCAN_ID)
    return(PPS_MSG_TYPE_SCAN);
  else if (type == REQ_TYPE_FRAME_ID)
    return(PPS_MSG_TYPE_FRAME);
  else
    return(NULL);

} /* getTypeStr */


/*----------------------------------------------------------
 * NAME:
 *  getModeId
 *
 * DESCRIPTION:
 *   return integer mode id given "MODE" string
 *
 * NOTES:
 *--------------------------------------------------------*/
int getModeId(char *str)
{
  /* return ((strncmp(str, "ST", 2)) ? MODE_SCANSAR_ID: MODE_CONTINUOUS_ID ); */

  if (strncmp(str, SPS_modeStr_ST, 2) == 0)
    return(MODE_CONTINUOUS_ID);
  else if (strncmp(str, SPS_modeStr_SW, 2) == 0 || 
           strncmp(str, SPS_modeStr_SN, 2) == 0 )
    return(MODE_SCANSAR_ID);
  else if (strncmp(str, SPS_modeStr_EH, 2) == 0)
    return(MODE_EXT_HIGH_ID);
  else if ( strncmp(str, SPS_modeStr_WD, 2) == 0 )
    return(MODE_WIDE_ID);
  else if ( strncmp(str, SPS_modeStr_FN, 2) == 0 )
    return(MODE_FINE_ID);
  else if ( strncmp(str, SPS_modeStr_EL, 3) == 0)
    return(MODE_EXT_LOW_ID);
  else
    return(NOT_SET_ID);

} /* getModeId */

/*----------------------------------------------------------
 * NAME:
 *  getTypeId
 *
 * DESCRIPTION:
 *   return integer message type id given "MSG_TYPE" string
 *
 * NOTES:
 *--------------------------------------------------------*/
int getTypeId(char *str)
{
  int retval = NOT_SET_ID;

  if (strcmp(str, "SCAN_REQUEST") == 0)
    retval = REQ_TYPE_SCAN_ID;
  else if (strcmp(str, "FRAME_REQUEST") == 0)
    retval = REQ_TYPE_FRAME_ID;

  return(retval);
} /* getTypeId */


/*----------------------------------------------------------
 * NAME:
 *  initStateFile
 *
 * DESCRIPTION:
 *  save the last state file saved by the CP into a file
 *  that can be used by the "Restore" function to restore that
 *  that last state.  we must do this because as soon as the
 *  CP starts running, it will overwrite the contents of the
 *  state file from the previous run.
 *
 * NOTES:
 *--------------------------------------------------------*/

void initStateFile()
{
 FILE *fp;
 char base[1024], state[1024], last[1024];
 char *p, fullPathName[MAXPATHLEN], linkBuf[MAXPATHLEN];
 int bytes_read;

  strcpy(GLOBAL_savedBaseName, getSavedStateFile(CURRENT_STATE_BASENAME));
  strcpy(GLOBAL_currentStateFile, getSavedStateFile(CURRENT_STATE_FILE));
  strcpy(GLOBAL_lastSavedStateFile, getSavedStateFile(LAST_STATE_FILE));


#ifdef STATE_FILE_DEBUG
printf("\n\tbasename \t%s\n\tlastsaved \t%s\n\tsavedname \t%s\n\n",
GLOBAL_savedBaseName, GLOBAL_lastSavedStateFile, GLOBAL_currentStateFile);
#endif


  bytes_read = readlink(GLOBAL_savedBaseName, linkBuf, sizeof(linkBuf) );

  if (bytes_read != -1) { /* if saved file did exist, point last.saved to it */
    linkBuf[bytes_read] = '\0'; /* return string is not null terminated */
#ifdef STATE_FILE_DEBUG
    printf("readlink of %s is %s, size %d\n", 
             GLOBAL_savedBaseName, linkBuf, bytes_read);
#endif
    p = realpath(linkBuf, fullPathName);
/*  fullPathName[bytes_read]='\0';  /* returned string is not null terminated */
#ifdef STATE_FILE_DEBUG
    printf("real path of %s is %s, realpath returned %s\n", linkBuf, fullPathName, p);
#endif

/* remove old last-link */
#ifdef STATE_FILE_DEBUG
    printf("unlinking last %s \n", GLOBAL_lastSavedStateFile);
#endif
    if (unlink(GLOBAL_lastSavedStateFile))
        perror("unlink");

/* set last-link to the last saved file name */
#ifdef STATE_FILE_DEBUG
    printf("symlinking %s to %s\n",fullPathName,GLOBAL_lastSavedStateFile);
#endif
    if (symlink(fullPathName, GLOBAL_lastSavedStateFile) )
      printf("symlink error\n");
    }

/* remove old 'current' state file link */
#ifdef STATE_FILE_DEBUG
    printf("unlinking old base %s \n", GLOBAL_savedBaseName);
#endif
    if (unlink(GLOBAL_savedBaseName))
      perror("unlink");

/* create the current state file so we have something to link onto */
  if ((fp = fopen(GLOBAL_currentStateFile, "w")) == NULL) {
    printfLLog(LOG_ERR, "Can't open saved state file %s\n", 
               GLOBAL_currentStateFile);
    return;
  }

/* create link to 'current' state file */
#ifdef STATE_FILE_DEBUG
  printf("symlinking %s to %s\n", GLOBAL_currentStateFile, GLOBAL_savedBaseName);
#endif
  if (symlink(GLOBAL_currentStateFile, GLOBAL_savedBaseName) )
    perror("symlink");

#ifdef STATE_FILE_DEBUG
printf("\n\tbasename \t%s\n\tlastsaved \t%s\n\tsavedname \t%s\n\n",
GLOBAL_savedBaseName, GLOBAL_lastSavedStateFile, GLOBAL_currentStateFile);
#endif

} /* initStateFile */


/*----------------------------------------------------------
 * NAME:
 *  handleXwpLabel 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and write them into one the label widget specified
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpLabel(Widget wid, char *messagePtr) 
{
  XmString xmstring;

  if (wid == NULL)
    return;
  xmstring = XmStringCreateSimple(messagePtr);
  XtVaSetValues(wid, XmNlabelString, xmstring, NULL);
  XmStringFree(xmstring);

} /* end handleXwpLabel...................*/

/*----------------------------------------------------------
 * NAME:
 *  handleXwpTextField
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc
 *  and write them into the text field widget specified
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpTextField(Widget wid, char *value)
{
  XmString xmstring;

  if (wid == NULL)
    return;
  assert(XmIsTextField(wid));
  XmTextFieldSetString(wid, value);
  XmProcessTraversal (wid, XmTRAVERSE_NEXT_TAB_GROUP);
 

} /* end handleXwpTextField...................*/



/*----------------------------------------------------------
 * NAME:
 *  handleXwpInfoBox 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and create and write them into the CP info box
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpInfoBox(char *messagePtr) 
{
 int x, y;
 Widget infoBoxWidget;
 XmString xmstring;

  infoBoxWidget   = create_CPinfoBox(UxTopLevel); 
  xmstring =  XmStringCreateLtoR(messagePtr, XmFONTLIST_DEFAULT_TAG);

  queryPointer(&x, &y);
  XtVaSetValues(infoBoxWidget, XmNmessageString, xmstring, 
                 XmNx, x, XmNy, y, NULL);
  XtVaSetValues(XtParent(infoBoxWidget), 
                 XmNmwmFunctions, MWM_FUNC_MOVE + MWM_FUNC_RESIZE,
                 XmNmwmDecorations, MWM_DECOR_ALL + MWM_DECOR_MINIMIZE,
                 NULL);
  UxPopupInterface(infoBoxWidget, no_grab );

  XmStringFree(xmstring);

} /* end handleXwpInfoBox ..........*/


/*----------------------------------------------------------
 * NAME:
 *  handleXwpErrorBox
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc
 *  and create and write them into the CP error box
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpErrorBox(char *messagePtr)
{
 int x, y;
 Widget errorBoxWidget;
 XmString xmstring;

  errorBoxWidget   = create_CPerrorBox(UxTopLevel);
  xmstring =  XmStringCreateLtoR(messagePtr, XmFONTLIST_DEFAULT_TAG);

  queryPointer(&x, &y);
  XtVaSetValues(errorBoxWidget, XmNmessageString, xmstring,
                XmNx, x, XmNy, y, NULL);
  XtVaSetValues(XtParent(errorBoxWidget), 
                 XmNmwmFunctions, MWM_FUNC_MOVE + MWM_FUNC_RESIZE,
                 XmNmwmDecorations, MWM_DECOR_ALL + MWM_DECOR_MINIMIZE,
                 NULL);
  UxPopupInterface(errorBoxWidget, no_grab );
  soundBell();

  XmStringFree(xmstring);

} /* end handleXwpErrorBox ..........*/

/*----------------------------------------------------------
 * NAME:
 *  handleXwpCleanupBox 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and create and write them into the CP error box 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpCleanupBox(xwpQueElemType *xwpEl)
{

 popupCleanupBox(xwpEl);

} /* end handleXwpCleanupBox ..........*/

/*----------------------------------------------------------
 * NAME:
 *  handleXwpTextBox
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc
 *  and create and write them into the CP error box
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpTextBox(char *jobIdStr, char *source)
{
 popupTextBox(atoi(jobIdStr), source);

} /* end handleXwpTextBox ..........*/




/*----------------------------------------------------------
 * NAME:
 *  handleXwpQuestionBox
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and create and write them into the CP question box
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleXwpQuestionBox(xwpQueElemType *xwpEl) 
{
 int x, y;
 Widget questionBoxWidget;
 XmString xmstring;
 char *messagePtr = xwpEl->reqStrPtr;

#ifdef CB_DEBUG
printf("handleXwpQuestionBox\n"); fflush(stdout);
printCBdata("handleXwpQuestionBox", &xwpEl->cbData);
#endif

  questionBoxWidget   = create_CPquestionBox(UxTopLevel, &xwpEl->cbData);
#ifdef CB_DEBUG
printCBdata("after create_CPquestionBox", &xwpEl->cbData);
#endif
  xmstring =  XmStringCreateLtoR(messagePtr, XmFONTLIST_DEFAULT_TAG);

  queryPointer(&x, &y);
  XtVaSetValues(questionBoxWidget, XmNmessageString, xmstring,
                XmNx, x, XmNy, y, NULL);
  XtVaSetValues(XtParent(questionBoxWidget), 
                 XmNmwmFunctions, MWM_FUNC_MOVE + MWM_FUNC_RESIZE,
                 XmNmwmDecorations, MWM_DECOR_ALL + MWM_DECOR_MINIMIZE,
                 NULL);

  UxPopupInterface(questionBoxWidget, no_grab );

  XmStringFree(xmstring);



} /* end handleXwpQuestionBox..........*/


/*----------------------------------------------------------
 * NAME:
 *  handleMsgOrQueListBox 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and place them in the SYSQueue message list box, or SYSQueue
 *  queue entry list box for the correct subsystem
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleMsgOrQueListBox(Widget wid, xwpQueElemType *xwpQueElemPtr)
{
 XmString xmstring, xmstringList[1];
 Dimension w_w, p_w;
#ifdef MSGQ
 int shouldMatch=0;
#endif

  if (wid == NULL)
      return;
   assert(XmIsList(wid));

#ifdef MSGQ
/*  printf("   handleMsgOrQueListBox: queue %s\n", XtName(wid)); */
if (xwpQueElemPtr->actionFlag == REPLACE_ITEM ||
    xwpQueElemPtr->actionFlag == SELECT_ITEM  )
    shouldMatch = 1;

/***
printf("   ------------------ into handleMsgOrQueListBox %s, %d\n",
        xwpQueElemPtr->srcPtr, xwpQueElemPtr->actionFlag);
printListInfo(xwpQueElemPtr->srcPtr, wid, shouldMatch); 
***/
#endif

  xmstring = XmStringCreateSimple(xwpQueElemPtr->reqStrPtr);
  if (xmstring == NULL) {
    printfLLog(LOG_ERR, X_MOTIF_ERROR, "XmStringCreateSimple");
    return;
  }

  switch(xwpQueElemPtr->actionFlag) {
   case ADD_ITEM:
#ifdef MSGQ
    printf("add %s item: %s\n", XtName(wid), xwpQueElemPtr->reqStrPtr); 
#endif

XtVaGetValues(wid, XmNwidth, &w_w, NULL);
XtVaGetValues(XtParent(wid), XmNwidth, &p_w, NULL);
printf("adding... wid width %d parent width %d string %s wid %d\n", w_w, p_w, xwpQueElemPtr->reqStrPtr, strlen(xwpQueElemPtr->reqStrPtr));
    XmListAddItemUnselected(wid, xmstring, 0);
    break;

   case REPLACE_ITEM: 
     xmstringList[0] = xmstring;
#ifdef MSGQ 
printf("  replace %s item at %ld: %s\n", XtName(wid), xwpQueElemPtr->replPos, xwpQueElemPtr->reqStrPtr);
#endif 
     XmListReplaceItemsPosUnselected(wid, xmstringList, 1, xwpQueElemPtr->replPos); 
/*     XmListReplaceItemsPos(wid, xmstringList, 1, xwpQueElemPtr->replPos); */
     break;


   case DELETE_ITEM:
     /* make the position positive again */
     xwpQueElemPtr->replPos =  -1*xwpQueElemPtr->replPos;
#ifdef MSGQ
printf("  delete %s item at %ld: %s\n", XtName(wid), xwpQueElemPtr->replPos, xwpQueElemPtr->reqStrPtr);
#endif
     XmListDeleteItemsPos(wid, 1, xwpQueElemPtr->replPos);
     break;

   case DELETE_ALL:
#ifdef MSGQ
printf("  delete all : wid 0x%x\n", wid);
#endif
     XmListDeleteAllItems(wid);
     break;

   case INSERT_ITEM:
#ifdef MSGQ
printf("  insert %s item at %ld: %s\n", XtName(wid), xwpQueElemPtr->replPos, xwpQueElemPtr->reqStrPtr);
#endif
/****
XtVaGetValues(wid, XmNwidth, &w_w, NULL);
XtVaGetValues(XtParent(wid), XmNwidth, &p_w, NULL);
printf("inserting... wid %s width %d parent width %d string %s wid %d\n", 
XtName(wid), w_w, p_w, xwpQueElemPtr->reqStrPtr, strlen(xwpQueElemPtr->reqStrPtr));
*****/
     XmListAddItemUnselected(wid, xmstring, xwpQueElemPtr->replPos);
     break;

   case SELECT_ITEM:
#ifdef MSGQ
printf("  select %s item at %ld: %s\n", XtName(wid), xwpQueElemPtr->replPos, xwpQueElemPtr->
reqStrPtr);
#endif
     XmListSelectPos(wid, xwpQueElemPtr->replPos, 0);
/*     makePosVisible(wid,  xwpQueElemPtr->replPos); */
     break;


   default:
#ifdef MSGQ
printf("default (%d) in handleMsgOrQueListBox\n", xwpQueElemPtr->actionFlag);
#endif
     break;
  } /* end switch */

/****
printf("   ------------------ leaving handleMsgOrQueListBox %s, %d\n",
        xwpQueElemPtr->srcPtr, xwpQueElemPtr->actionFlag);
printListInfo(xwpQueElemPtr->srcPtr, wid, 1); 

***/
  XmStringFree(xmstring);

} /* end handleMsgOrQueListBox............................*/


/*----------------------------------------------------------
 * NAME:
 *  handleXwpq 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and distribute them to the correct X display window, eg. 
 *  main queue message window, subsystem queue info window, etc. 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int handleXwpq(xwpQueElemType    *direct_xwpQueElemPtr)
{
 xwpQueElemType    *xwpQueElemPtr;
 Widget            wid;
/* XEvent event; */
  baseListElemType *nPtr;
  char str[20]; /* size in digits of max # of jobs in system */

#ifdef READ_LOCK_DEBUG
printf("handleXwpq locking READ_LIST\n"); fflush(stdout);
#endif

  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
     return(0);

 
  if (direct_xwpQueElemPtr != NULL)    /* check if queue is empty */
     xwpQueElemPtr = direct_xwpQueElemPtr;
  else if ((xwpQueElemPtr = GetHeadOfXwpQue_XWPQUE()) == NULL) {
#ifdef READ_LOCK_DEBUG
printf("handleXwpq unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return(0);
  }
  
#ifdef XWP
  printf("handleXwpq: widget %s \n", getWidgetName(xwpQueElemPtr->xEntity));
dumpXwpq();
#endif
  switch(xwpQueElemPtr->xEntity) {

    case MAIN_JOB_ID_LIST:
    case  QUE_JOB_ID_LIST:
      wid = GetJobIdListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr); 
      break;
    case MAIN_PLAT_REV_SEQ_LIST:
    case  QUE_PLAT_REV_SEQ_LIST:
      wid = GetPlatRevSeqListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_FRAME_LIST:
    case  QUE_FRAME_LIST:
      wid = GetFrameListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_MEDIA_LIST               :
    case  QUE_MEDIA_LIST               :
      wid = GetMediaListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_MODE_LIST:
    case  QUE_MODE_LIST:
      wid = GetModeListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_REQ_LIST:
    case  QUE_REQ_LIST:
      wid = GetRequestListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_TYPE_LIST:
    case  QUE_TYPE_LIST:
      wid = GetTypeListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
      break;
    case MAIN_STATUS_LIST:
    case  QUE_STATUS_LIST:
      wid = GetStatusListGivenName_PIDLIST(xwpQueElemPtr->srcPtr);
      handleMsgOrQueListBox(wid, xwpQueElemPtr);
/*      XtCallActionProc(wid, "ListEndData", &event, NULL,0); */
      break;


    case QUE_STATUS_LABEL_ID:
      handleXwpLabel(GetInfoLabelGivenName_PIDLIST(xwpQueElemPtr->srcPtr),
                    xwpQueElemPtr->reqStrPtr);
      break;

    case WP_INFO_BOX:
      handleXwpInfoBox(xwpQueElemPtr->reqStrPtr);
#ifdef READ_LOCK_DEBUG
printf("handleXwpq unlocking READ_LIST\n"); fflush(stdout);
#endif
      unLockNameList_CORE(nPtr);
      return(0);

    case WP_ERROR_BOX:
      printfLLog(LOG_ERR, xwpQueElemPtr->reqStrPtr);
      handleXwpErrorBox(xwpQueElemPtr->reqStrPtr);
#ifdef READ_LOCK_DEBUG
printf("handleXwpq unlocking READ_LIST\n"); fflush(stdout);
#endif
      unLockNameList_CORE(nPtr);
      return(0);
 
    case WP_QUESTION_BOX:
      handleXwpQuestionBox(xwpQueElemPtr);
      break;

    case WP_CLEANUP_BOX:
      handleXwpCleanupBox(xwpQueElemPtr);
      break;

    case WP_TEXT_BOX:
      handleXwpTextBox(xwpQueElemPtr->reqStrPtr, xwpQueElemPtr->srcPtr);
      break;

    case WP_MAX_JOBS_TEXT_ID:
      sprintf(str, "%d", getPPSqueueSize());
      handleXwpTextField(maxQueueSize_tf, str);
      break; 
    case WP_NUM_JOBS_LABEL_ID:
      sprintf(str, "%d", totalMQentries);
      handleXwpLabel(currentQueueSizeValue_label, str);
      break; 

    case QUE_HEARTBEAT_LABEL_ID:
      wid = GetBottomInfoLabelGivenName_PIDLIST(xwpQueElemPtr->srcPtr, 
                                              xwpQueElemPtr->xEntity);
      handleXwpLabel(wid, xwpQueElemPtr->reqStrPtr);
      break;

   default:
     printfLLog(LOG_ERR, "Cannot get %s: %d", 
            "x window entity", xwpQueElemPtr->xEntity);
#ifdef READ_LOCK_DEBUG
printf("handleXwpq unlocking READ_LIST\n"); fflush(stdout);
#endif
     unLockNameList_CORE(nPtr);
     return(-1);
     break;

  }
  FreeXwpQueElement_XWPQUE(xwpQueElemPtr);

#ifdef READ_LOCK_DEBUG
printf("handleXwpq unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);
  return(0);

} /* end handleXwpq.........................*/


/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotStat
 *
 * DESCRIPTION:
 *  notify the user that a subsystem did not acknowledge a request
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotStat(caddr_t namePtr, XtIntervalId reqSentID)
{
 sysQueElemType *sqel; 
 int jobId; 
                                              /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, CLEANUP_INTERVAL_ID, 0);

  jobId = GetFirstIdWithStatus_SYSQUE(namePtr, Q_M_SENT_ID);

  if (jobId == -1) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX,
                 SUBSYSTEM_DID_NOT_STAT_LAST_TEXT, namePtr);

    printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_STAT_LAST_TEXT, namePtr);
  }else{
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX,
      SUBSYSTEM_DID_NOT_STAT_TEXT, namePtr, jobId); 

     printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_STAT_TEXT, namePtr, jobId );
  }
  ChangeStatus__MSTRQLIST(jobId, namePtr, Q_S_PLACED_ON_SYSQ_ID);
  resetSubsysByName(namePtr); 
/* since reset the subsystem, update the label manually */
/* update the queue status label in the destination subsys queue */
  updateStatusLabel(namePtr, GetListPosGivenJobId_SYSQUE(namePtr, jobId),
      Q_S_PLACED_ON_SYSQ_ID, namePtr);

} /* end NotifyUserThatProcDidNotStat...................*/

/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotAck
 *
 * DESCRIPTION:
 *  notify the user that a subsystem did not acknowledge a request
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotAck(caddr_t namePtr, XtIntervalId reqSentID)
{
 sysQueElemType *sqel; 
 int jobId; 
                                              /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, MSG_SENT_INTERVAL_ID, 0);

  jobId = GetFirstIdWithStatus_SYSQUE(namePtr, Q_M_SENT_ID);

  if (jobId == -1) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX,
                 SUBSYSTEM_DID_NOT_ACK_LAST_TEXT, namePtr);

    printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_ACK_LAST_TEXT,
              namePtr);
  }else{
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX,
      SUBSYSTEM_DID_NOT_ACK_TEXT, namePtr, jobId); 

     printfLLog(LOG_ERR, SUBSYSTEM_DID_NOT_ACK_TEXT, namePtr, jobId );
  }
  ChangeStatus__MSTRQLIST(jobId, namePtr, Q_S_PLACED_ON_SYSQ_ID);

  resetSubsysByName(namePtr);  /* don't send anything else to subsys */

              /* since we reset the subsystem, must update the labels */
              /* manually because the job will not be looked at anymore */

          /* update the queue status label in the destination subsys queue */
  updateStatusLabel(namePtr, GetListPosGivenJobId_SYSQUE(namePtr, jobId),
      Q_S_PLACED_ON_SYSQ_ID, namePtr);
          /* update the queue status label in the main window */
  updateStatusLabel(ASF_CP, GetDispPos_MSTRQLIST(jobId),
             Q_S_PLACED_ON_SYSQ_ID, namePtr);


} /* end NotifyUserThatProcDidNotAck...................*/

/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotResp
 *
 * DESCRIPTION:
 *  notify the user that a subsystem did not acknowledge a request
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotResp(caddr_t namePtr,XtIntervalId reqSentID)
{
                                              /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, MSG_SENT_INTERVAL_ID, 0);

  ASFlogMessage_direct(ASF_CP,WP_ERROR_BOX,"%s did not send message", namePtr);


} /* end NotifyUserThatProcDidNotResp...................*/



/*----------------------------------------------------------
 * NAME:
 *  doSendMsg
 *
 * DESCRIPTION:
 *  send a message to the specified subsystem
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int doSendMsg(char *namePtr, int socket, ODL writeOutODL, int expectedMsg)
{
 int retval;
 XtIntervalId requestSentID;
 
  retval = WriteMsgToClient(socket, writeOutODL); 
  if (retval < 0) {
   if (errno) /* if errno set, use that info in the error message */
     printfLLog(LOG_ERR, CANNOT_SEND_MSG_ERRNO, namePtr, strerror(errno));
   else
     printfLLog(LOG_ERR, CANNOT_SEND_MSG_SOCKET, namePtr);

   return(-1);
  }
#ifdef DEBUG
printf("doSendMsg: expect %d name %s socket %d\n", expectedMsg, namePtr, socket);
#endif
  if (expectedMsg == EXPECT_ACK_MSG) {
    requestSentID = XtAppAddTimeOut(UxAppContext, getRequestSentTimeInterval(), 
      (XtTimerCallbackProc)NotifyUserThatProcDidNotAck, namePtr);

    SetTimeOutID_PIDLIST(namePtr, MSG_SENT_INTERVAL_ID, requestSentID);
  }
  else if (expectedMsg == EXPECT_STATUS_MSG) {  /* expect status message */
    requestSentID = XtAppAddTimeOut(UxAppContext, getRequestSentTimeInterval(), 
      (XtTimerCallbackProc)NotifyUserThatProcDidNotStat, namePtr);

    SetTimeOutID_PIDLIST(namePtr, CLEANUP_INTERVAL_ID, requestSentID);
  }
  else if (expectedMsg == EXPECT_REQUEST_MSG) { /* expect pps request msg */
    requestSentID = XtAppAddTimeOut(UxAppContext, getRequestSentTimeInterval(),
      (XtTimerCallbackProc)NotifyUserThatProcDidNotResp, namePtr);

    SetTimeOutID_PIDLIST(namePtr, MSG_SENT_INTERVAL_ID, requestSentID);
  }

  return(0);
 
} /* end doSendMsg..................................*/


/*----------------------------------------------------------
 * NAME:
 *  selectLine 
 *
 * DESCRIPTION:
 *  select a "row" of entries in the main queue or a subsystem queue window
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void selectLine(Widget w, int listPos)
{
 int i;
#ifdef LABELS
 Dimension width, h;
#endif
 char *namePtr;
 mstrqListElemType *mqel;

  mqel = GetReqGivenListPos_MSTRQLIST(listPos);
  namePtr = GetNameGivenMainWid_PIDLIST(w);
#ifdef LABELS
printf("selectLine: name %s pos %d \n", namePtr, listPos);
#endif
  if (namePtr == NULL)
    namePtr = ASF_CP;

  for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
    ASFlogMessageSel_direct(namePtr, i , listPos, ""); 
  }
} /* selectLine */


/*----------------------------------------------------------
 * NAME:
 *  updatePosLabel
 *
 * DESCRIPTION:
 *  update one entry from a "row" of entries in the 
 *  main queue or a subsystem queue window.  also, select all
 *  entries in this same row.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void updatePosLabel(char *namePtr, int whichWidget , int listPos, char *value)
{
  Widget list_w;
  Boolean selected = False;
  int numItems=0;
  XmString *strlist;
#ifdef LABELS
  char *text;
#endif

#ifdef LABELS
printf("updatePosLabel: %s column %d pos %d value %s\n",
namePtr, whichWidget, listPos, value);
if (strcmp(namePtr, "CP-IMS") == NULL ) {
  printListInfo(namePtr, GetStatusListGivenName_PIDLIST(namePtr), 1);
}
#endif

  list_w = GetJobIdListGivenName_PIDLIST(namePtr);

  XtVaGetValues(list_w, XmNitemCount, &numItems, XmNitems, &strlist, NULL);

  if (listPos <= 0) {
    printfLLog(LOG_DEBUG, LOW_POSITION, namePtr, listPos, value);
    return;
  }
  else if (listPos > numItems) {
    printfLLog(LOG_DEBUG, "%s: listPos %d numItems %d string %s\n", namePtr, listPos, numItems, value);
    printfLLog(LOG_DEBUG, HIGH_POSITION, namePtr, listPos, numItems);
    return;
  }

#ifdef LABELS
printf("\tlistPos %d ok; numItems %d ", listPos, numItems);
  if (!XmStringGetLtoR (strlist[listPos-1], XmFONTLIST_DEFAULT_TAG, &text))
      return;
printf("strlist[%d] is %s\n", listPos-1, text);
  XtFree(text);
#endif

 
    selected = XmListPosSelected(list_w, listPos);

    ASFlogMessageRepl_direct(namePtr, whichWidget, listPos, value);
    if (selected)
      ASFlogMessageSel_direct(namePtr, whichWidget, listPos, value);

/****** do we want to select the line when update status ????   ****/

#ifdef SELECT_LINE
  selectLine(list_w, listPos); 
#endif

} /* updatePosLabel */

updateStatusLabel(char *queue, int listPos,  int status, char *namePtr)
{
  Widget list_w;
  Boolean selected = False;
  int numItems=0, queue_w;
  XmString *strlist;
  char *p;
#ifdef LABELS
  printf("updateStatusLabel: %s pos %d status %d name %s\n", queue, listPos, status, namePtr);
#endif

                      /* figure out which queue widget to update */
  if (is_CP(queue) )
    queue_w = MAIN_STATUS_LIST;
  else /* is a subsystem queue */
    queue_w = QUE_STATUS_LIST;

  p = GetQstatusAsText_MSTRQLIST(status, namePtr);
  if (p == NULL) {
    printfLLog(LOG_DEBUG,
          "Cannot get status string for %s state %d", namePtr, status);
  }

  updatePosLabel(queue, queue_w, listPos, p);
  doFree(p);

  return(0);

} /* updateStatusLabel */



/*----------------------------------------------------------
 * NAME:
 *  removeLabel
 *
 * DESCRIPTION:
 *  remove one entry from a "row" of entries in the 
 *  main queue or a subsystem queue window.  
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void removeLabel(char *namePtr, int whichWidget , int listPos, char *value)
{
  ASFlogMessageDel_direct(namePtr, whichWidget, listPos, value);
} /* removeLabel */

/*----------------------------------------------------------
 * NAME:
 *  removeLineForJobId
 *
 * DESCRIPTION:
 *  remove a "row" of entries in the main queue or a subsystem queue window
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void removeLineForJobId(char *namePtr, int jobId)
{
  Widget list_w;
  int numItems, listPos;
  XmString *strlist, job_xmstr;
  char jobStr[10];

  sprintf(jobStr, "%d", jobId);
  job_xmstr = XmStringCreateSimple(jobStr);

  list_w = GetJobIdListGivenName_PIDLIST(namePtr);

  listPos = XmListItemPos(list_w, job_xmstr);
  XmStringFree(job_xmstr);

#ifdef LABELS
  printf("removeLineForJobId: name %s job %d pos %d\n",namePtr,jobId,listPos);
if (strcmp(namePtr, "CP-IMS") == NULL ) {
  printListInfo(namePtr, GetStatusListGivenName_PIDLIST(namePtr), 1);
}
#endif

  XtVaGetValues(list_w, XmNitemCount, &numItems, XmNitems, &strlist, NULL);

  if (listPos <= 0) {
    printfLLog(LOG_DEBUG, LOW_POSITION_DEL, namePtr, listPos);
    return;
  }
  else if (listPos > numItems) {
    printfLLog(LOG_DEBUG, HIGH_POSITION_DEL, namePtr, listPos, numItems);
    return;
  }

  removeLabel(namePtr, QUE_JOB_ID_LIST, listPos, "");
  removeLabel(namePtr, QUE_PLAT_REV_SEQ_LIST, listPos, "");
  removeLabel(namePtr, QUE_FRAME_LIST, listPos, "");
  removeLabel(namePtr, QUE_MEDIA_LIST, listPos, "");
  removeLabel(namePtr, QUE_MODE_LIST, listPos, "");
  removeLabel(namePtr, QUE_REQ_LIST, listPos, "");
  removeLabel(namePtr, QUE_TYPE_LIST, listPos, "");
  removeLabel(namePtr, QUE_STATUS_LIST, listPos, "");

} /* removeLineForJobId */



/*----------------------------------------------------------
 * NAME:
 *  deleteProducts
 *
 * DESCRIPTION:
 *  remove a "row" of entries in the main queue or a subsystem queue window
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static void deleteProducts(int jobId)
{
  char cmd[MAXPATHLEN], *p;
  subsysFilesType *sf;
  int i;


/* return; */

    sf = GetSubsysFilesGivenJobId_MSTRQLIST(jobId);

    if (sf == NULL) {
      printfLLog(LOG_DEBUG, "No products to delete for job %d\n", jobId);
      return;
    }

#ifdef DEL_DEBUG
    printf("\t\tdeleteProducts: sf jobid %d numProducts %d numFiles %d\n",
           jobId, sf->numProducts, sf->numFiles);
    showSf_sf("deleteProducts", sf);
#endif


  if (retainProducts()) {
    printfLLog(LOG_INFO, "Retaining products generated by job %d\n", jobId);
    doFree(sf);
    return;
  }
  else
    printfLLog(LOG_INFO, "Removing products generated by job %d\n", jobId);

    for (i = 0; i < sf->numProducts; i++) {
      printfLLog(LOG_INFO, "Removing product %s\n", sf->productName[i]);
      p = (p = strchr(sf->productName[i], ':')) ? p+1 : sf->productName[i];

      sprintf(cmd, "/sbin/rm -f %s", p);
#ifdef REALLY_DELETE_PRODUCTS
      system(cmd); 
#else
#endif
    }
    for (i = 0; i < sf->numDirs; i++) {
      printfLLog(LOG_INFO, "Removing directory %s\n", sf->dirName[i]);
      p = (p = strchr(sf->dirName[i], ':')) ? p+1 : sf->dirName[i];

      sprintf(cmd, "/usr/bin/rmdir %s", p);
#ifdef REALLY_DELETE_PRODUCTS
      system(cmd);
#else
#endif
    }


  doFree(sf);


} /* deleteProducts */

/*----------------------------------------------------------
 * NAME:
 *  updateJobStateDisplay
 *
 * DESCRIPTION:
 *  update the color state table entries for job-related states
 *  (i.e. qc and hold)
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void updateJobStateDisplay(Widget stateTable, char *namePtr)
{
  int stateListPos;

  stateListPos = GetListPosGivenName_PIDLIST(namePtr);
  if (GetQCstatus_SYSQUE(namePtr))
    updateStateColor(stateListPos-1, DISP_COL_QC, stateTable);
  else  /* if no job is in qc mode, reset the qc entry */
    resetStateColor(stateListPos-1, DISP_COL_QC, stateTable);
  if (GetHoldStatus_SYSQUE(namePtr))
    updateStateColor(stateListPos-1, DISP_COL_HOLD, stateTable);
  else  /* if no job is in hold mode, reset the hold entry */
    resetStateColor(stateListPos-1, DISP_COL_HOLD, stateTable);

} /* updateJobStateDisplay */

/*----------------------------------------------------------
 * NAME:
 *  findNonSubsysJob
 *
 * DESCRIPTION:
 *  return job id of topmost job that can be processed without
 *  the subsystem being active
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int findNonSubsysJob(char *namePtr)
{
  int prioJob = -1;

/* first check if there is a priority job that can be processed even
   if the subsystem is not running at the moment */
  prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_DONE_ID);
  if (prioJob == -1)
    prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_IMAGE_QC_ACCEPT_ID);
  if (prioJob == -1)
    prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_IMAGE_QC_READY_ID);
  if (prioJob == -1)
    prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_IMAGE_QC_REJECT_ID);
  if (prioJob == -1)
    prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_IMAGE_QC_ON_HOLD_ID);
/*
  if (prioJob != -1)
    printf("findNonSubsysJob: returning job %d\n", prioJob);
*/
  return(prioJob);

} /* findNonSubsysJob */

/*----------------------------------------------------------
 * NAME:
 *  findPriorityJob
 *
 * DESCRIPTION:
 *  return job id of topmost job that can be processed without
 *  the subsystem being active
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int findPriorityJob(char *namePtr, int currJobId)
{
  char *deferMsg = DEFER_JOB_QC;
  int prioJob = -1;
 static int lastDeferPrioId=0, lastDeferPrioStatus=0;
#ifdef PRINT_ALL_DEFERRED
 static int lastDeferRunId=0, lastDeferRunStatus=0;
 static int  entryNum=0;
#endif


  prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_S_DONE_ID);
  if (prioJob != -1)
    deferMsg = DEFER_JOB_DONE;


  if (prioJob == -1) 
    prioJob = findNonSubsysJob(namePtr);

/* need to do qc completions even if tape mount is up */
  if (prioJob == -1) {  
    prioJob=GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_M_GOT_ITEM_OFF_ID);
    if (prioJob != -1)
      deferMsg = DEFER_JOB_TAPE_PROMPT;
  }

   if (prioJob != -1) {  /* do the priority job no matter what is on top */
                                 /* debug stuff */
#ifdef PRINT_ALL_DEFERRED
if (prioJob == currJobId)
  printf("%4d Found top job (%d) IS prio job\n", entryNum, prioJob);
else
  printf("%4d Found priority job %d instead of job %d; prio status %d\n",
    entryNum, prioJob, currJobId, GetStatus__MSTRQLIST(namePtr, currJobId));

        printfLLog(LOG_DEBUG, deferMsg, currJobId, prioJob);
#else
      if (!(lastDeferPrioId == currJobId &&
            lastDeferPrioStatus == GetStatus__MSTRQLIST(namePtr, currJobId))
          && prioJob != currJobId)
        printfLLog(LOG_DEBUG, deferMsg, currJobId, prioJob);
#endif
      lastDeferPrioId = currJobId;
      lastDeferPrioStatus = GetStatus__MSTRQLIST(namePtr, currJobId);

      return(prioJob);
  }

  return(-1);

} /* findPriorityJob */

/*----------------------------------------------------------
 * NAME:
 *  findLaterJob
 *
 * DESCRIPTION:
 *  return job id of topmost job *later* in the queue whose
 *  processing cannot be interrupted by the current job
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static int findLaterJob(char *namePtr, int topStatus, int currJobId)
{
  int status, laterJob=-1;
  char *deferMsg = DEFER_JOB_QC;
 static int lastDeferRunId=0, lastDeferRunStatus=0;
#ifdef PRINT_ALL_DEFERRED
 static int lastDeferPrioId=0, lastDeferPrioStatus=0;
 static int entryNum=0;
#endif

  if (topStatus == Q_S_PLACED_ON_SYSQ_ID) {
             /* check for all later "CHECKING TAPE" related status values */
      deferMsg = DEFER_JOB_TAPE;
                          /* CHECKING_TAPE status occurs while the wrong tape */
                          /* dialog is being displayed, until Ok or Cancel */
      laterJob = GetFirstElemWithStatus_MSTRQLIST(namePtr,
                     Q_M_CHECKING_TAPE_ID);
#ifdef LATER_JOB
printf("after Q_M_CHECKING_TAPE_ID... laterJob is %d\n", laterJob);
#endif
      if (laterJob == -1)
        laterJob = GetFirstElemWithStatus_MSTRQLIST(namePtr,
                     Q_S_CHECKING_TAPE_ID);
#ifdef LATER_JOB
printf("after Q_S_CHECKING_TAPE_ID... laterJob is %d\n", laterJob);
#endif

                          /* MEDIA_CHECK_PENDING occurs while the wrong tape */
      if (laterJob == -1) /* dialog is being displayed, until Ok or Cancel */
        laterJob = GetFirstIdWithMedia_SYSQUE(namePtr, MEDIA_CHECK_PENDING);

#ifdef LATER_JOB
printf("after MEDIA_CHECK_PENDING... laterJob is %d\n", laterJob);
#endif
                          /* MEDIA_CHECK_NO is only set by Ok callback */
                          /* so if it is set, this job has priority over */
      if (laterJob == -1) /* any insert top jobs that may have just arrived */
        laterJob = GetFirstIdWithMedia_SYSQUE(namePtr, MEDIA_CHECK_NO);
                        /* but if the job is on hold because of an error */
                        /* after bypassing the media check, don't process it */
        if ((status = GetStatus__MSTRQLIST(namePtr, laterJob)) == 
          Q_M_PLACED_ON_HOLD_ERROR_ID || status == Q_M_PLACED_ON_HOLD_DATA_ID)
          laterJob = -1;
#ifdef LATER_JOB
printf("after MEDIA_CHECK_NO... laterJob is %d\n", laterJob);
#endif

             /* check for any later job currently processing */
      if (laterJob == -1) {
        laterJob = GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_M_PROCESSING_ID);
#ifdef LATER_JOB
printf("after Q_M_PROCESSING_ID... laterJob is %d\n", laterJob);
#endif
        if (laterJob != -1)
          deferMsg = DEFER_JOB_RUN;
      }

#ifdef LATER_JOB
printf("checking later jobs... laterJob is %d\n", laterJob);
#endif
      if (laterJob != -1 && laterJob != currJobId ) {
                                 /* debug stuff */
#ifdef PRINT_ALL_DEFERRED
if (laterJob == currJobId)
  printf("%4d Found top job (%d) IS running job\n", entryNum, laterJob);
else { if (laterJob != -1)
  printf("%4d Found later job %d instead of job %d\n",
                                     entryNum, laterJob, currJobId);
}

          printfLLog(LOG_DEBUG, deferMsg, currJobId, laterJob, namePtr);
#else
        if (!(lastDeferRunId == currJobId &&
              lastDeferRunStatus == GetStatus__MSTRQLIST(namePtr, currJobId))
            && laterJob != currJobId)
/*          printfLLog(LOG_DEBUG,deferMsg, currJobId, laterJob, namePtr); */
#endif
        lastDeferRunId = currJobId;
        lastDeferRunStatus = GetStatus__MSTRQLIST(namePtr, currJobId);

#ifdef SYSQ
printf("findLaterJob: %s currJobId %d topStatus %d, returning job %d\n", 
        namePtr, currJobId, topStatus, laterJob);
#endif
        return(laterJob);
      }
    }


  return(-1);
}

/*----------------------------------------------------------
 * NAME:
 *  handleSysQ
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc
 *  and distribute them to the correct subsystem queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

static int handleSysQ(char *namePtr)
{
 int sqPos, sockfd, status, subsysState, ppid, resetJobId;
 int prioJob=-1, topStatus, catStep, cat, numCalStored;
 sysQueElemType    *sqel;
 ODL writeOutODL;
 char *reqTypePtr, *productType, *compensated, *p, *statusStr;
 subsysFilesType sf, *sfPtr = &sf;
 struct timeval endTime, ackTime;
 ppsStatusType ppsType = P_LAST;
 static int lastJobId=0, lastStatus=0;
#ifdef SYSQ
 int zz, i;
#endif
  baseListElemType *nPtr;

#ifdef READ_LOCK_DEBUG
printf("handleSysQ locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
     return(0);

#ifdef TIMESTAMP
  printf("handleSysQ: %s\n", timestamp() );
#endif

#ifdef LISTS
if (strcmp(namePtr, "RDS") == NULL || strcmp(namePtr, "CP-IMS") == NULL ) {
  printListInfo(namePtr, GetStatusListGivenName_PIDLIST(namePtr), 1);
}
#endif

#ifdef FULL_SYSQ
dumpSysq(namePtr);
#endif

/* update the color state table for job-related lights */
  cat = GetSubsystemCategoryGivenName(namePtr);
  if (cat == PPS_CATEGORY_ID || cat== IMS_CATEGORY_ID) 
    updateJobStateDisplay(CPexternRC, namePtr);
  else
    updateJobStateDisplay(CPstatusRC, namePtr);


/* first check if there is a non-subsystem job (qc, job completion) that 
   can be processed even if the subsystem is not running at the moment */

  prioJob = findNonSubsysJob(namePtr);

  subsysState = GetProcessState_PIDLIST(namePtr); /* see if process is running*/
  if (subsysState != SUBSYS_RUNNING_ID && 
      subsysState != SUBSYS_READY_ID &&
                                      /* subsysState != SUBSYS_HALTING_ID && */
      subsysState != SUBSYS_HEALTHING_ID &&
      prioJob == -1) {
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return(0);
  }

                                              /* check if anything on queue */
  sqel = GetFirstNonHoldElem_SYSQUE(namePtr, DO_COPY); 
  if (sqel == NULL) {
    if (GetSubsystemCategoryGivenName(namePtr) != PPS_CATEGORY_ID) {
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
       unLockNameList_CORE(nPtr);
       return(0); 
    }
    else {                      /* is pps and we need to ask for another job */
      doPPS();                  /* if not already waiting for a pps message */
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
      unLockNameList_CORE(nPtr);
      return(1);
    }
  }  /* found a non-hold elem on sysq */


/* if later job = PROCESSING and this job = ON SYSQ, skip this job and
   make the later job the currently processed one */

    topStatus = GetStatus__MSTRQLIST(namePtr, sqel->jobId);

                                  /* prio jobs - done, qc's: need no subsys */
    prioJob = findPriorityJob(namePtr, sqel->jobId); 
    if (prioJob == -1)            /* later jobs - running, checking tape */
      prioJob = findLaterJob(namePtr, topStatus, sqel->jobId); 

    if (prioJob != -1) { /* found one -- process it instead */
                                 /* actual stuff that does the work */
        FreeSysQueElement_SYSQUE(namePtr, sqel);  /* free the last one */
        sqel = GetElemGivenJobId_SYSQUE(namePtr, prioJob, DO_COPY);
        if (sqel == NULL) {
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
          unLockNameList_CORE(nPtr);
          return(0);
        }
    }

  status = GetStatus__MSTRQLIST(namePtr, sqel->jobId);


#ifdef FULL_SYSQ
  printf("\tSQ: found jobid %d %s sysq, status is %s (%d)\n", 
    sqel->jobId, namePtr, GetQstatusAsText_MSTRQLIST(status, namePtr), status);
#endif 

if (!(lastJobId == sqel->jobId && lastStatus == status)) {
  if (status != Q_M_IMAGE_READY_FOR_QC_ID &&   /* don't want to fill up */
      status != Q_M_IMAGE_PRE_QC_ID &&     /* syslog with jobs that */
      status != Q_M_SCAN_READY_FOR_QC_ID &&    /* just sit here for a   */
      status != Q_M_SENT_ID &&             /* while... */
      status != Q_M_GOT_ITEM_OFF_ID &&             /* tape prompt... */
      status != Q_M_PLACED_ON_HOLD_ERROR_ID &&      /* checking tape->hold */
      status != Q_M_CHECKING_TAPE_ID &&             /* checking tape */
      status != Q_M_PROCESSING_ID) {

     statusStr = GetQstatusAsText_MSTRQLIST(status, namePtr);
     if (statusStr == NULL) {
       printfLLog(LOG_ERR, MALLOC_ERROR, "processing subsystem queue");
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
       unLockNameList_CORE(nPtr);
       return(0);
     }
     printfLLog(LOG_DEBUG, JOB_STATUS_SUBSYS, namePtr, sqel->jobId, 
                statusStr, status);  
     doFree(statusStr);
    }

#ifdef SYSQ
  printf("\tSQ: found jobid %d on %s sysq, status is %s (%d)\n", 
             sqel->jobId, namePtr, GetQstatusAsText_MSTRQLIST(status, namePtr), status);
#endif 
}
lastJobId = sqel->jobId;
lastStatus = status;

  sqPos = GetListPosGivenJobId_SYSQUE(namePtr, sqel->jobId);
/* printf("SQ: sqPos for %s job %d is %d\n", namePtr, sqel->jobId, sqPos); */
  if (status != Q_M_PLACED_ON_SYSQ_ID) {  /* list element doesn't exist yet */
    updateStatusLabel(namePtr, sqPos, status, namePtr);
  }
  switch(status) {
    case Q_S_PLACED_ON_SYSQ_ID:
      updateStatusLabel(namePtr, sqPos, status, namePtr);

      if (ChangeStatus__MSTRQLIST(sqel->jobId,namePtr,Q_S_GOT_ITEM_OFF_ID)
            == -1)
             printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
      break;

    case Q_S_DONE_ID:
      reqTypePtr = ODLGetStr(sqel->sysODLreq, HDR_MSG_TYPE);
      if (reqTypePtr == NULL ) {
         printfLLog(LOG_ERR, CANNOT_GET, "request type");
         break;
      } 

#ifdef JOB_DONE
printf("Q_S_DONE_ID: pid %d job %d cat %d type %s\n", getpid(), sqel->jobId, 
GetSubsystemCategoryGivenName(namePtr), reqTypePtr);
#endif

      ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_CURRENT_COMPLETED_TEXT,
        sqel->jobId);
                        /* remove element from the queue if RDS only
                         * NOTE: if this is last then we can free and get the 
                         * head of the queue without convoluting the logic */

      getEndTime_MSTRQLIST(sqel->jobId, P_LAST, &endTime);
      printfLLog(LOG_DEBUG, "%s job %d is done", namePtr, sqel->jobId);

/* reset media check in case job has an error and is resubmitted later */
      SetMediaFlagGivenJobId_SYSQUE(namePtr, sqel->jobId, MEDIA_CHECK_YES);

      switch(cat = GetSubsystemCategoryGivenName(namePtr)) {
        case RDS_CATEGORY_ID:
          if (ChangeStatus__MSTRQLIST(sqel->jobId,namePtr,Q_M_NEXT_STEP_ID)
              == -1) 
            printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);

/* if the element was a scan, we need to keep it.  otherwise, remove it */
/* actually removing the following three lines caused problems for */
/* jobs moving from rds to ssp after decoding.  so we still need to */
/* fix it for the rds scan case */
          if (strcmp(reqTypePtr, "FRAME_REQUEST") == 0) {
            setEndTime_MSTRQLIST(sqel->jobId, P_DECODE, endTime);

            FreeSysQueElement_SYSQUE(namePtr, sqel);
            sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId);
            removeLineForJobId(namePtr, sqel->jobId);
            clearQueueID(sqel-> sysODLreq,namePtr);         
/* sqel = GetFirstNonHoldElem_SYSQUE(namePtr, DO_REMOVE); bad with insert top */
          }
          else if (strcmp(reqTypePtr, "SCAN_REQUEST") == 0) {
            setEndTime_MSTRQLIST(sqel->jobId, P_SCAN, endTime);
          }
          break;

        case ASP_CATEGORY_ID:
                  /* if was a frame request, set status to "ready for qc" */
          if (strcmp(reqTypePtr, "FRAME_REQUEST") == 0) { 
            setEndTime_MSTRQLIST(sqel->jobId, P_FRAME, endTime);
            if (GetQCtype_MSTRQLIST(sqel->jobId) != QC_TYPE_NONE) {
              SetQCflag_SYSQUE(namePtr, sqel->jobId);
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                 Q_M_IMAGE_PRE_QC_ID) == -1)
                printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
            }
            else { /* no QC for this image type... need to catalog it so */
              SetQCtype_MSTRQLIST(sqel->jobId, QC_TYPE_DONE); /* make it done */
              if (ChangeStatus__MSTRQLIST(sqel->jobId, ASF_CP,
                                          Q_M_NEXT_STEP_ID) == -1)
                printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
              FreeSysQueElement_SYSQUE(namePtr, sqel);
              sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId);
              removeLineForJobId(namePtr, sqel->jobId);
            }
          }
          else { /* was scan request: want to auto qc next  */
            if (ChangeStatus__MSTRQLIST(sqel->jobId,namePtr,
              Q_M_NEXT_STEP_ID) == -1)
              printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
            setEndTime_MSTRQLIST(sqel->jobId, P_SCAN, endTime);
          }
          break;

        case SSP2_CATEGORY_ID:
          productType = ODLGetStr(sqel->sysODLreq, BODY_PRODUCT_TYPE);
          compensated = ODLGetStr(sqel->sysODLreq, BODY_COMPENSATION_FLAG);
          if (productType == NULL)
            printfLLog(LOG_ERR, CANNOT_GET, "product type");
          else if (compensated == NULL)
            printfLLog(LOG_ERR, CANNOT_GET, "compensation type");
          else {
            decrementSSP2Time(getSSP2numGivenName(namePtr), 
                              getExpectedTime(productType, compensated));

            SetQCflag_SYSQUE(namePtr, sqel->jobId);
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                 Q_M_IMAGE_PRE_QC_ID) == -1)
                printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
            setEndTime_MSTRQLIST(sqel->jobId, P_FRAME, endTime);
          }

          break;


        case PPS_CATEGORY_ID: /* we are really truly done */
          {
            int retries=0;

            if ((retries = GetRetry_MSTRQLIST(sqel->jobId)) > 0) {
printf("########### job done, retry count %d #################\n", retries);

/* queue the job up to re-send all the intermediate status messages first */
/* and when they have all been sent, we can send the final status message */

              DecrementRetry_MSTRQLIST(sqel->jobId);
              ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                     Q_S_PLACED_ON_SYSQ_ID);
            }
            else {
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                   Q_M_FINAL_ID) == -1)
                printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
              deleteProducts(sqel->jobId);
    
              FreeSysQueElement_SYSQUE(namePtr, sqel);
              sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId); 
              removeLineForJobId(namePtr, sqel->jobId);  
            }
          }
          break;  

        case IMS_CATEGORY_ID:
            sendAck(sqel->sysODLreq, namePtr, sqel->jobId);
            catStep = GetCatalogStep_MSTRQLIST(sqel->jobId);
#ifdef JOB_DONE
printf("Q_S_DONE_ID: IMS job %d catstep %d\n", sqel->jobId, catStep);
#endif
                                     /* if just finished storing, we're done */
            if (catStep == IMS_TYPE_STORE_CAL_PRODUCTS ) {
              numCalStored = GetNumCalProductsStored_MSTRQLIST(sqel->jobId);
              IncrementNumCalProductsStored_MSTRQLIST(sqel->jobId);
              if (numCalStored >= MAX_CAL_PRODUCTS)
                IncrementCatalogStep_MSTRQLIST(sqel->jobId);
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                 Q_M_NEXT_STEP_ID) == -1)
                 printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
              SetRepeatFlagGivenJobId_SYSQUE( namePtr, sqel->jobId);

            }
            else if (catStep == IMS_TYPE_STORE_PRODUCTS || 
                catStep == IMS_TYPE_STORE_SCAN_METADATA ) {
#ifdef JOB_DONE
printf("Q_S_DONE_ID: IMS job %d catstep just finished storing\n",  sqel->jobId);
#endif
              SetCatalogStep_MSTRQLIST(sqel->jobId, IMS_TYPE_DONE);
              setEndTime_MSTRQLIST(sqel->jobId, P_CATALOG, endTime);
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                 Q_M_NEXT_STEP_ID) == -1)
                 printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
              ClearRepeatFlagGivenJobId_SYSQUE( namePtr, sqel->jobId);
            }
            else {   /* didn't do a final store, just increment to next step */
#ifdef JOB_DONE
printf("Q_S_DONE_ID: IMS job %d catstep incrementing\n",  sqel->jobId);
#endif
              IncrementCatalogStep_MSTRQLIST(sqel->jobId);
              if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                 Q_M_NEXT_STEP_ID) == -1)
                 printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
              SetRepeatFlagGivenJobId_SYSQUE( namePtr, sqel->jobId);
            }
#ifdef JOB_DONE 
printf("Q_S_DONE_ID: deciding whether to remove job %d repeating %d catStep %d type %s\n",
sqel->jobId, GetRepeatFlagGivenJobId_SYSQUE(namePtr, sqel->jobId), 
GetCatalogStep_MSTRQLIST(sqel->jobId), reqTypePtr);
#endif 
          catStep = GetCatalogStep_MSTRQLIST(sqel->jobId);
          if (catStep > IMS_TYPE_GET_CAL_PARAMS || /* heading to asp */
              strcmp(reqTypePtr, "SCAN_REQUEST") == 0) {
#ifdef JOB_DONE
printf("Q_S_DONE_ID: IMS job %d removing line & sqel \n",  sqel->jobId);
#endif
            FreeSysQueElement_SYSQUE(namePtr, sqel);
            sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId);
            removeLineForJobId(namePtr, sqel->jobId);
                    
          }
          else /* don't want to add a NEW line to sysq window next time */
              SetRepeatFlagGivenJobId_SYSQUE( namePtr, sqel->jobId);


          break;  
        default:
          printfLLog(LOG_ERR, INVALID_SUBSYS_CATEGORY, namePtr, cat); 
          break;
      }
      if (GetProcessState_PIDLIST(namePtr) == SUBSYS_PAUSING_ID)
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);
#ifdef JOB_DONE
printf("leaving Q_S_DONE_ID case, job %d\n",  sqel->jobId);
#endif
    break;

    case Q_S_RCVD_ACK_ID:
      printfLLog(LOG_INFO, SENT_ACK_TEXT, namePtr, sqel->jobId);
      printfLLog(LOG_DEBUG, STARTED_PROCESSING_TEXT, namePtr, sqel->jobId);

      getStartTime_MSTRQLIST(sqel->jobId, P_LAST, &ackTime);

      ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_CURRENT_PROCESSING_TEXT,
                   sqel->jobId);


      reqTypePtr = ODLGetStr(sqel->sysODLreq, HDR_MSG_TYPE);
      if (reqTypePtr == NULL) {
         printfLLog(LOG_ERR, CANNOT_GET, "request type");
         break;  /* get out of this switch case */
      } 
      switch( GetSubsystemCategoryGivenName(namePtr)) {
        case RDS_CATEGORY_ID:
          if (strcmp(reqTypePtr, "FRAME_REQUEST") == 0) 
            ppsType = P_DECODE;
          else
            ppsType = P_SCAN;
          break;

        case ASP_CATEGORY_ID:
          if (strcmp(reqTypePtr, "FRAME_REQUEST") == 0) 
            ppsType = P_FRAME;
          else
            ppsType = P_SCAN;
          break;

        case SSP2_CATEGORY_ID:
            ppsType = P_FRAME;
          break;
        case PPS_CATEGORY_ID:
/* need to keep this around until we receive SUBSYSTEM_COMPLETED from pps */
/*          removeLineForJobId(namePtr, sqel->jobId);  
          sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId); */

          break;
        default:
          break;
      }
      if (ppsType != P_LAST)
          setStartTime_MSTRQLIST(sqel->jobId, ppsType, ackTime);

      if (ChangeStatusRestrict__MSTRQLIST(sqel->jobId, namePtr, 
              Q_M_PROCESSING_ID, Q_S_DONE_ID) == -1)
          printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
      break;

    case Q_S_CHECKING_TAPE_ID:
      updateStatusLabel(namePtr, sqPos, status, namePtr);
      SetMediaFlagGivenJobId_SYSQUE(namePtr, sqel->jobId, MEDIA_CHECK_PENDING);
      if (ChangeStatus__MSTRQLIST(sqel->jobId,namePtr,Q_M_CHECKING_TAPE_ID)
           == -1)
          printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);

      break;

    case Q_S_GOT_ITEM_OFF_ID:
      if (ChangeStatus__MSTRQLIST(sqel->jobId,namePtr,Q_M_GOT_ITEM_OFF_ID)
           == -1)
          printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
       break;

    case Q_S_IMAGE_QC_READY_ID:
     {
       char *imageFileNamePtr, *leaderFileNamePtr, *avgFileNamePtr;

/* if image averaging program created an .avg file, add the file name */
/* to the list of products that need to be deleted at the end of the job */

       if (GetQCFileNames_MSTRQLIST(sqel->jobId, &leaderFileNamePtr,
                                &imageFileNamePtr, &avgFileNamePtr) == -1) {
         printfLLog(LOG_ERR, CANNOT_GET_QC_FILES);
       }
       if (strcmp(avgFileNamePtr, "") != 0 )  { /* if there is one */

         sfPtr = GetSubsysFilesGivenJobId_MSTRQLIST(sqel->jobId);
         if (sfPtr == NULL) {
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
           unLockNameList_CORE(nPtr);
       /*     return(0); */
         }
         strcpy(sfPtr->productName[sfPtr->numProducts++], avgFileNamePtr);
         SetSubsysFilesGivenJobId_MSTRQLIST(sqel->jobId, sfPtr);
         doFree(sfPtr);
       }


      
      SetQCflag_SYSQUE(namePtr, sqel->jobId);
      if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr,
                                Q_M_IMAGE_READY_FOR_QC_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);
      updateStatusLabel(namePtr, sqPos, status, namePtr);

     break;
     }


    case Q_S_IMAGE_QC_ON_HOLD_ID:
      SetQCflag_SYSQUE(namePtr, sqel->jobId);
      if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                                Q_M_IMAGE_QC_ON_HOLD_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);
      updateStatusLabel(namePtr, sqPos, status, namePtr);

     break;

    case Q_S_IMAGE_QC_ACCEPT_ID:

       if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
              Q_M_IMAGE_QC_DONE_ACCEPT_ID) == -1)
           printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
       FreeSysQueElement_SYSQUE(namePtr, sqel);
       sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId);
       removeLineForJobId(namePtr, sqel->jobId);
       break;

    case Q_S_IMAGE_QC_REJECT_ID:
      if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                                Q_M_IMAGE_QC_DONE_REJECT_ID) == -1)
           printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
      FreeSysQueElement_SYSQUE(namePtr, sqel);
      sqel = RemoveElemWithJobId_SYSQUE(namePtr, sqel->jobId);
      removeLineForJobId(namePtr, sqel->jobId);
      break;

    case Q_M_INTERRUPTED_ID:
      resetJobId = ResetFirstInterruptedStatus_MSTRQLIST(namePtr);
      printfLLog(LOG_DEBUG, JOB_INTERRUPTED, resetJobId);

      break;
    case  Q_M_PROCESSING_RESET_ID: 
      if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                                Q_M_REPEAT_LAST_STEP_ID) == -1)
           printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, namePtr);
      break;

    case Q_S_READY_TO_SEND_ID:
                                   /* get the connected processes socket */
      if ((sockfd = GetSocketGivenName_PIDLIST(namePtr)) == -1) {
        printfLLog(LOG_ERR, CANNOT_GET, "process socket");
        FreeSysQueElement_SYSQUE(namePtr, sqel);
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
        unLockNameList_CORE(nPtr);
        return(0);
       }

      sfPtr = GetSubsysFilesGivenJobId_MSTRQLIST(sqel->jobId);
      if (sfPtr == NULL) {
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
        unLockNameList_CORE(nPtr);
        return(0);
      }

      if ((writeOutODL = buildSubsys_Msg(namePtr, sqel, sfPtr)) == NULL) {
        if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                               Q_M_PLACED_ON_HOLD_ERROR_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);
        FreeSysQueElement_SYSQUE(namePtr, sqel);
        doFree(sfPtr);
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
        unLockNameList_CORE(nPtr);
        return(0);
      }
      if ((ppid = GetPidGivenName_PIDLIST(namePtr)) != 0) {
        SetSubsysFilesGivenJobId_MSTRQLIST(sqel->jobId, sfPtr);
#ifdef SYSQ
        printf("handleSQ: set sfPtr jobid %d numFiles %d\n",sfPtr->jobId,sfPtr->numFiles);
        for (zz = 0; zz < sfPtr->numFiles; zz++)
          printf("%s\n", sfPtr->fileName[zz]);
#endif
      }

      printfLLog(LOG_DEBUG, SENDING_ODL_TO, ASF_CP, namePtr);

      if (doSendMsg(namePtr, sockfd, writeOutODL, EXPECT_ACK_MSG) < 0) {
        if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, 
                             Q_M_PLACED_ON_HOLD_ERROR_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);
        if (errno)
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, CANNOT_SEND_MSG_ERRNO, 
                             namePtr, strerror(errno) );
        else
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, CANNOT_SEND_MSG_SOCKET, namePtr);
        resetSubsysByName(namePtr);
      }
      else /* successful send */
        if (ChangeStatus__MSTRQLIST(sqel->jobId, namePtr, Q_M_SENT_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, namePtr);

      ODLFree(writeOutODL);
      doFree(sfPtr);

#ifdef HANDLE_RETRY
/* the code under #ifdef HANDLE_RETTY is used if we want to send the retry   */
/* message right away, without regard to the state of the CP-PPS.  if the    */
/* CP-PPS happens to be down, this message is dropped on the floor and will  */
/* never be sent again.  that's the problem with doing it this way!          */
/* but this code works, and if anyone chooses to do it this way, we're done. */
 
/* now, if this is a retry, send the retry message */
      if ((determineClassFromName(namePtr) == SUBSYS_CLASS_ID) &&
          GetRetry_MSTRQLIST(sqel->jobId) > 0) {
        int retval;
        char *statusType = "INTERMEDIATE";
        char *statusVal = "RETRY";
        if ((sockfd = GetSocketGivenName_PIDLIST(getPPSname())) == -1) {
          printfLLog(LOG_ERR, CANNOT_GET, "process socket");
          FreeSysQueElement_SYSQUE(namePtr, sqel);
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
          unLockNameList_CORE(nPtr);
          return(0);
       }
       writeOutODL = (ODL) GetNewMsg(SPS_JOB_STATUS);
       if (writeOutODL == NULL) {
          unLockNameList_CORE(nPtr);
          return(0);
       }

       retval = buildJobStatusMsg(sqel, writeOutODL, 1); /* retry */
        if(retval < 0)  {
          printf("error building status message \n");
          unLockNameList_CORE(nPtr);
          return(0);
        }

        retval = doSendMsg(getPPSname(), sockfd, writeOutODL, EXPECT_ACK_MSG);
        if(retval < 0) {
          if (errno) /* if errno set, use that info in the error message */
            printfLLog(LOG_ERR, CANNOT_SEND_MSG_ERRNO, getPPSname(), 
             strerror(errno));
        else
            printfLLog(LOG_ERR, CANNOT_SEND_MSG_SOCKET, getPPSname());
        }


      } /* end of retry stuff */
#endif

      break;

    case Q_M_DONE_ERROR_ID:
    case Q_M_PROCESSING_ID:
    case Q_M_IMAGE_READY_FOR_QC_ID:
    case Q_M_IMAGE_PRE_QC_ID:
    case Q_M_SCAN_READY_FOR_QC_ID:
    case Q_M_SENT_ID:
    default:
      break;

  } /* end status switch */

         /* every subsystem allocates this element except pps */
/* this leaks MAJORLY if we add this check.  so some pps cases
   do need to be freed, and some dont. :-p */
/***  if (GetSubsystemCategoryGivenName(namePtr) != PPS_CATEGORY_ID) ***/
    FreeSysQueElement_SYSQUE(namePtr, sqel);


#ifdef LISTS
if (strcmp(namePtr, "RDS") == NULL || strcmp(namePtr, "CP-IMS") == NULL )
  printListInfo(namePtr, GetStatusListGivenName_PIDLIST(namePtr), 1);
#endif
#ifdef READ_LOCK_DEBUG
printf("handleSysQ unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);
  return(0);

} /* end handleSysQ ........................*/


/*----------------------------------------------------------
 * NAME:
 *  determineNextProcessForRequest 
 *
 * DESCRIPTION:
 *  Determine which subsystem should receive the processsing
 *  request next
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static char *determineNextProcessForRequest(mstrqListElemType *mqel, 
                                     int *catalogStep, int *mode_ID)
{
 char *currentNamePtr, *instModePtr, *reqTypePtr, *retptr, *platformPtr;
 char *processor,*mediaIDPtr ,*mediaTypePtr;      
 int   reqType_ID, newReq_ID, cat, numCalProducts, numCalProductsStored;
 int  RDSnum;

printfLLog(LOG_ERR,"determineNextProcessForRequest IN %s job %d catalogStep %d\n", 
mqel->namePtr, mqel->jobId, *catalogStep); fflush(stdout);

  retptr = ASF_CP;
  currentNamePtr = mqel->namePtr;

  platformPtr  = ODLGetStr(mqel->mstrqODLreq, BODY_PLATFORM);
  if (platformPtr == NULL ) {
    printfLLog(LOG_ERR, CANNOT_GET, "platform");
    return(ASF_CP);
  }

  instModePtr  = ODLGetStr(mqel->mstrqODLreq, BODY_MODE);
  if (instModePtr == NULL ) {
    printfLLog(LOG_ERR, CANNOT_GET, "instrument mode");
    return(ASF_CP);
  }
  *mode_ID = getModeId(instModePtr);

  mediaIDPtr = ODLGetStr(mqel->mstrqODLreq, BODY_MEDIA_ID);
  if (mediaIDPtr == NULL ) {
    printfLLog(LOG_ERR, CANNOT_GET, "media ID ");
    return(ASF_CP);
  }
  mediaTypePtr = ODLGetStr(mqel->mstrqODLreq, BODY_MEDIA_TYPE);
  if (mediaTypePtr == NULL ) {
    printfLLog(LOG_ERR, CANNOT_GET, "media TYPE ");
    return(ASF_CP);
  }
  reqTypePtr = ODLGetStr(mqel->mstrqODLreq, HDR_MSG_TYPE);
  if (reqTypePtr == NULL ) {
    printfLLog(LOG_ERR, CANNOT_GET, "request type");
    return(ASF_CP);
  }
  reqType_ID = getTypeId(reqTypePtr);

  newReq_ID = FALSE;                           /* determine if a new request */
  if (strcmp(currentNamePtr, getGPRname()) == 0 ||
      strcmp(currentNamePtr, getPPSname()) == 0 )
       newReq_ID = TRUE;

printf("determineNext: IN job %d mode %d reqtype %d qctype %d catStep %d\n", 
mqel->jobId, *mode_ID, reqType_ID, mqel->qcType, *catalogStep); fflush(stdout);

if (*catalogStep == IMS_TYPE_DONE) 
  return(getPPSname()); 

if (newReq_ID) {
   *catalogStep = IMS_TYPE_GET_VERSION;
   retptr = getIMSnameGivenCatalogStep(*catalogStep);
}
else {
   if (reqType_ID == REQ_TYPE_SCAN_ID) {
printf("determineNext scan catStep %d\n", *catalogStep); fflush(stdout);
      if (mqel->qcType == QC_TYPE_ACCEPT || 
          mqel->qcType == QC_TYPE_SCAN_REJECT) { /* bump up to STORE... */
         if (*catalogStep != IMS_TYPE_STORE_SCAN_METADATA) 
             *catalogStep = IMS_TYPE_STORE_SCAN_RESULT;
         retptr = getIMSnameGivenCatalogStep(*catalogStep);
      }
      else if (mqel->qcType == QC_TYPE_SCAN) {
         ; /* time for qc */
      }
      else if (*catalogStep == IMS_TYPE_GET_VERSION) { /* do subsystem work */
         if (*mode_ID == MODE_SCANSAR_ID ||
             *mode_ID == MODE_EXT_HIGH_ID ||
             *mode_ID == MODE_EXT_LOW_ID ||
             *mode_ID == MODE_FINE_ID ||
             *mode_ID == MODE_WIDE_ID) {
printf("determineNext scan SS\n"); fflush(stdout);
          if (GetRetry_MSTRQLIST(mqel->jobId) > 0) 
            retptr = getPPSname();
          else {
            RDSnum = assignRDS(mediaIDPtr,mediaTypePtr);
            retptr = getRDSname(RDSnum);
            if (retptr != NULL)
               SetRDSname_MSTRQLIST( mqel->jobId, retptr);
          }
         }
         else if (*mode_ID == MODE_CONTINUOUS_ID) {
printf("determineNext scan ST\n"); fflush(stdout);
            if (mqel->qcType == QC_TYPE_ACCEPT || 
                mqel->qcType == QC_TYPE_SCAN_REJECT) {
printf("determineNext scan ST qc type DONE, going on to ims store\n"); fflush(stdout);
               *catalogStep = IMS_TYPE_STORE_SCAN_RESULT;
               retptr = getIMSnameGivenCatalogStep(*catalogStep);
            }
            else if (mqel->qcType == QC_TYPE_NONE) {
printf("platform 1 %s\n", platformPtr);
               if (strcmp(platformPtr, SPS_platforms[SPS_platform_J1]) == 0 &&
                    isDCRSI(mediaTypePtr))
                  retptr = getASPname();
               else if (getToggleScanDest() == RDS_CATEGORY_ID)
               {
                 RDSnum = assignRDS(mediaIDPtr,mediaTypePtr);
                 retptr = getRDSname(RDSnum);
                if (retptr != NULL)
                   SetRDSname_MSTRQLIST( mqel->jobId, retptr);
               }
               else if (isDCRSI(mediaTypePtr))
                  retptr = getASPname();
               else {
                  printfLLog(LOG_ERR, CANNOT_GET, "the job assigned ");
                  return(ASF_CP);
               }
            }

         } /* end of continuous mode choices */
         else /* bad mode? */
printf("determineNext: mode not SS or ST!\n"); fflush(stdout);
      }
      else if (*catalogStep == IMS_TYPE_GET_VERSION+1) { /* initial get done */
printf("detemineNext done with initial ims get -- go to scan qc next \n"); fflush(stdout);
         *catalogStep = IMS_TYPE_CP_WORKING;
printf("platform 1.1 %s\n", platformPtr);
          if (strcmp(platformPtr, SPS_platforms[SPS_platform_J1]) == 0 &&
                     isDCRSI(mediaTypePtr))
               retptr = getASPname();
          else if (*mode_ID == MODE_SCANSAR_ID  ||
             *mode_ID == MODE_EXT_HIGH_ID ||
             *mode_ID == MODE_EXT_LOW_ID ||
             *mode_ID == MODE_FINE_ID ||
             *mode_ID == MODE_WIDE_ID)
             {
                 RDSnum = assignRDS(mediaIDPtr,mediaTypePtr);
                 retptr = getRDSname(RDSnum);
                 if (retptr != NULL)
                    SetRDSname_MSTRQLIST( mqel->jobId, retptr);
             }
         else if (*mode_ID == MODE_CONTINUOUS_ID) 
            if (getToggleScanDest() == RDS_CATEGORY_ID)
            {
                 RDSnum = assignRDS(mediaIDPtr,mediaTypePtr);
                 retptr = getRDSname(RDSnum);
                 if (retptr != NULL)
                    SetRDSname_MSTRQLIST( mqel->jobId, retptr);
            }
            else if (isDCRSI(mediaTypePtr))
               retptr = getASPname();
            else {
               printfLLog(LOG_ERR, CANNOT_GET, "the job assigned ");
               return(ASF_CP);
            }
      } 
      else if (*catalogStep == IMS_TYPE_CP_WORKING)  {
         *catalogStep = IMS_TYPE_STORE_SCAN_RESULT;
         retptr = getIMSnameGivenCatalogStep(*catalogStep);
      }
   } /* end of scan choices */
   else if (reqType_ID == REQ_TYPE_FRAME_ID) {
      numCalProducts = GetNumCalProducts_MSTRQLIST(mqel->jobId) ;
      numCalProductsStored = GetNumCalProductsStored_MSTRQLIST(mqel->jobId) ;
printf("determineNext frame catalogStep %d numCalProducts %d qcType %d\n", 
*catalogStep, numCalProducts, mqel->qcType); fflush(stdout);
      if (mqel->qcType == QC_TYPE_DONE || mqel->qcType == QC_TYPE_ACCEPT || 
          mqel->qcType == QC_TYPE_SCAN_REJECT) {
printf("determineNext frame qc done numCalProd %d numCalProdStored %d\n", 
numCalProducts, numCalProductsStored); fflush(stdout);

         if (numCalProducts == 0) {  /* no pvs - go right to store */
           *catalogStep = IMS_TYPE_STORE_PRODUCTS;
           retptr = getIMSnameGivenCatalogStep(*catalogStep);
         }
         else if (numCalProductsStored < numCalProducts) {
           *catalogStep = IMS_TYPE_STORE_CAL_PRODUCTS; /* still storing pvs */
           retptr = getIMSnameGivenCatalogStep(*catalogStep);
         }
         else if (numCalProductsStored == numCalProducts) {
           *catalogStep = IMS_TYPE_STORE_PRODUCTS; /* finished storing pvs */
           retptr = getIMSnameGivenCatalogStep(*catalogStep);
         }
         else if (*catalogStep == IMS_TYPE_STORE_PRODUCTS) /* all done */
           *catalogStep = IMS_TYPE_DONE;
      }
      else if (*catalogStep < IMS_TYPE_STORE_CAL_PRODUCTS) {
/*         (*catalogStep)++;  */
         retptr = getIMSnameGivenCatalogStep(*catalogStep);
      } 
      else { /* do subsystem work */

         processor = GetProcessor_MSTRQLIST(mqel->jobId);
         if ((cat = GetSubsystemCategoryGivenProcessor(processor))
                  ==SSP2_CATEGORY_ID) {              /* includes SSP and PP */

printf("determineNext frame SS currentName %s\n",currentNamePtr);fflush(stdout);
            cat = GetSubsystemCategoryGivenName(currentNamePtr);
            if (cat == IMS_CATEGORY_ID)
            {
               RDSnum= assignRDS(mediaIDPtr,mediaTypePtr);
               retptr = getRDSname(RDSnum);
               if (retptr != NULL)
                    SetRDSname_MSTRQLIST( mqel->jobId, retptr);
            }
            else if (cat == RDS_CATEGORY_ID){
printf("determineNext frame SS old RDS: qctype %d\n", mqel->qcType ); fflush(stdout);
               retptr = GetSSPname_MSTRQLIST(mqel->jobId);
            }
   
         }
         else if (cat == ASP_CATEGORY_ID) {  
printf("determineNext frame NOT SS currentName %s, cat %d\n", currentNamePtr,
GetSubsystemCategoryGivenName(processor)); fflush(stdout);

           retptr = getASPname();
               
         }

      } /* end of cts mode */
   } /* end of frame processing */
   else
printf("neither frame nor scan!\n");
}

printf("determineNext: OUT job %d returning %s mode %d catalogStep %d qctype %d\n", 
   mqel->jobId, retptr, *mode_ID, *catalogStep, mqel->qcType); fflush(stdout);
  return(retptr);

} /* determineNextProcessForRequest */

/*----------------------------------------------------------
 * NAME:
 *  handleMSTRq 
 *
 * DESCRIPTION:
 *  handle entries taken off a queue intended for the X workproc 
 *  and distribute them to the master queue for processing
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int handleMSTRq(int *mode, int *msgType)
{
 mstrqListElemType *mqel, *mqelcopy;
 char *nextNamePtr, *oldNamePtr, qLocBuf[50], mountStr[512], *p, *statusStr;
 int elemIndex=-1, cat, i, repeating, mqPos, sqPos, qcType, catStep;
 int rdsNum;
 struct timeval mqTime;
 Widget w;
  baseListElemType *nPtr;

#ifdef READ_LOCK_DEBUG
printf("handleMQ locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
     return(0);

#ifdef TIMESTAMP
printf("handleMSTRQ: %s\n", timestamp() );
#endif


  if ((mqel = GetFirstModifiedElem__MSTRQLIST(&elemIndex)) == NULL) {
#ifdef READ_LOCK_DEBUG
printf("handleMQ unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return(0);  /* found no modified elements */
  }

/* do this in ChangeStatus__MSTRQLIST */
/*  writeStateFile(); /* mq was just changed -- save to disk file */

                               /* place request in master queue view window */
  statusStr =  GetQstatusAsText_MSTRQLIST(mqel->status,mqel->namePtr);
  printfLLog(LOG_DEBUG, MSTRQ_REQ_STAT, mqel->jobId, statusStr, mqel->status);

  mqPos = GetDispPos_MSTRQLIST(mqel->jobId);
  sqPos = GetListPosGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
#ifdef MSTR
printf("    MQ: handling status %s (%d) jobid %d name %s, mp %d sp %d\n", 
GetQstatusAsText_MSTRQLIST(mqel->status, mqel->namePtr), mqel->status, mqel->jobId, mqel->namePtr, mqPos, sqPos);
#endif
  switch(mqel->status) {

     case Q_M_PLACED_ON_SYSQ_ID:
       repeating = GetRepeatFlagGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId);
#ifdef LISTS
printf("MQ Q_M_PLACED_ON_SYSQ_ID: repeating %d, sqPos %d\n", repeating, sqPos);
#endif
       if (repeating == TRUE) {
         updateStatusLabel(mqel->namePtr,sqPos,mqel->status, mqel->namePtr);
       }
       else {
         for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
           ASFlogMessageInsert_direct(mqel->namePtr, i, sqPos, mqel->mqFmt[i],
                 mqel->mqLabel[i]); 
         }
/* this causes an "Error updating display" warning when this job is in  */
/* transition from one subsystem queue to another... it's just a display */
/* warning, and will fix itself as soon as this job is looked at again */
/* by handleSysQ */
         updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
       }
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);

       if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
            Q_S_PLACED_ON_SYSQ_ID) == -1)
         printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);

      break;
/******************************************************************************/
     case Q_M_PROCESSING_ID:
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
 
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       ChangeProcessState_PIDLIST(mqel->namePtr, SUBSYS_RUNNING_ID, NOT_SET_ID);
       break;
/******************************************************************************/

     case Q_M_INCOMING_ID:
         printfLLog(LOG_INFO, RCVD_JOB, mqel->jobId);

         for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
           ASFlogMessageInsert_direct(ASF_CP, i, mqPos, mqel->mqFmt[i],
             mqel->mqLabel[i]);
         }
         if (ChangeStatus__MSTRQLIST(mqel->jobId, getPPSname(),
                    Q_M_NEXT_STEP_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
  
      break;
/******************************************************************************/

     case Q_M_CLEANING_UP_ID:
        printfLLog(LOG_INFO, DOING_CLEANUP, mqel->namePtr, mqel->jobId);

        updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
        updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
 
      break;

/******************************************************************************/
     case Q_M_NEXT_STEP_ID:
         if (strcmp(mqel->namePtr, getGPRname()) == 0 ||
             strcmp(mqel->namePtr, getPPSname()) == 0 ) {

                                /* when from gpr/pps, set message to "next" */
         printfLLog(LOG_INFO, RCVD_JOB, mqel->jobId);
#ifdef OLD

         for (i=0; i < MAX_MAIN_LIST_ITEMS; i++) {
           ASFlogMessageInsert_direct(ASF_CP, i, mqPos, mqel->mqFmt[i],   
             mqel->mqLabel[i]);   
         }
#endif
         catStep = IMS_TYPE_GET_VERSION; /* initialize ims processing */
       }
       else{     /* when from anyone but gpr, set message to "completed" */
           sprintf(qLocBuf, Q_M_NEXT_STEP_COMP_TEXT, mqel->namePtr);

           updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, qLocBuf);

       } 
                               /* find out who gets the request next */

       catStep = GetCatalogStep_MSTRQLIST(mqel->jobId);
       nextNamePtr = determineNextProcessForRequest(mqel, &catStep, mode); 
       SetCatalogStep_MSTRQLIST(mqel->jobId, catStep);
       oldNamePtr = mqel->namePtr; /* for clarity */

       switch(determineClassFromName(nextNamePtr) ) {
         case CP_CLASS_ID:  /* if rds or asp, change status to scan completed */
                            /* otherwise change the status to ready to QC */
             qcType = GetQCtype_MSTRQLIST(mqel->jobId) ;
             if (qcType == QC_TYPE_SCAN) {
               SetQCflag_SYSQUE(mqel->namePtr, mqel->jobId);
               if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
                       Q_M_SCAN_READY_FOR_QC_ID) == -1)
                 printfLLog(LOG_ERR,  CANNOT_CHANGE_STATUS, mqel->namePtr);
             }
             else if (qcType == QC_TYPE_IMAGE) {
               if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP,
                       Q_M_IMAGE_READY_FOR_QC_ID) == -1)
                   printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, ASF_CP);
             }
             else if (qcType == QC_TYPE_NONE) {
               if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP,
                       /*Q_M_CYCLE_COMPLETED_ID) == -1) */
                       Q_M_FINAL_ID) == -1)
                 printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, ASF_CP);
             }
             else if (qcType == QC_TYPE_DONE || QC_TYPE_ACCEPT || 
                      qcType == QC_TYPE_SCAN_REJECT) {
               if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP,
                       Q_M_CYCLE_COMPLETED_ID) == -1)
                 printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, ASF_CP);
             }
           break;

         case PPS_CLASS_ID:  
         case IMS_CLASS_ID:  
         case SUBSYS_CLASS_ID:           /* change to next subsystem */
           if (ChangeStatus__MSTRQLIST(mqel->jobId, nextNamePtr, 
                    Q_M_PLACED_ON_SYSQ_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
           if (GetSubsystemCategoryGivenName(oldNamePtr)==RDS_CATEGORY_ID){
             updatePosLabel(ASF_CP, MAIN_REQ_LIST, 
               GetDispPos_MSTRQLIST(mqel->jobId), "FRAME");
             SetRequestLabel_MSTRQLIST(mqel->jobId,MAIN_REQ_LIST, "FRAME");
           }

           if (!JobExists_SYSQUE(nextNamePtr, mqel->jobId) ) {
             AddSysReqToQue_SYSQUE(nextNamePtr, NOT_SET, mqel->jobId, 
                FALSE, FALSE, mqel->mstrqODLreq,mqel->dataReadyFlag); 
#ifdef EXTRA_DEBUG
             printfLLog(LOG_DEBUG, PLACED_ON_QUE_TEXT, ASF_CP,
                  mqel->jobId,nextNamePtr);
#endif
           }
           break;  /* end SUBSYS_CLASS_ID case */

         default:
           ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, 
                "Job %d is stuck!\nProbable mismatch between "
                "configuration file and available subsystems", 
                 mqel->jobId);
           printfLLog(LOG_ERR, CANNOT_DETERMINE, "next processing step");
           break;
       } /* end switch */ 

      break;  /* end of case Q_M_NEXT_STEP_ID */
/******************************************************************************/
     case Q_M_SENT_ID:
       printfLLog(LOG_DEBUG,CP_SENT_REQUEST_TEXT, ASF_CP, mqel->jobId, 
             mqel->namePtr);
      break;
/******************************************************************************/

     case Q_M_RCVD_ACK_ID:      /* log message to main log msg window */

       printfLLog(LOG_INFO, SENT_ACK_TEXT, mqel->namePtr, mqel->jobId);

       cat = GetSubsystemCategoryGivenName(mqel->namePtr);
/****       if (cat == PPS_CATEGORY_ID) {
         if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                                     Q_M_FINAL_ID) == -1)
           printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
       }
       else
******/
         if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                                     Q_M_PROCESSING_ID) == -1)
           printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
      break;
/******************************************************************************/

     case Q_M_CYCLE_COMPLETED_ID:
       printfLLog(LOG_DEBUG, CP_COMPLETED_TEXT, ASF_CP, mqel->jobId);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       if (ChangeStatus__MSTRQLIST(mqel->jobId,ASF_CP, Q_M_NEXT_STEP_ID) == -1)
         printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
       break;
/******************************************************************************/

     case Q_M_IMAGE_PRE_QC_ID:
       if (getToggleQCstart() == QC_START_INTERACTIVE) {
         printfLLog(LOG_INFO, PRE_QC_TEXT, mqel->namePtr, mqel->jobId);

         updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
         updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);


         if (handleSubsysPreQCspawn(mqel->namePtr, mqel->jobId)) {
           updatePosLabel(ASF_CP,MAIN_STATUS_LIST,mqPos,Q_M_IMAGE_PRE_QC_TEXT);
           updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos,
                       Q_M_IMAGE_PRE_QC_TEXT);
         }
       }
       else  { /* pretend we did a "qc accept" and go right to ims */
       ClearQCflag_SYSQUE(mqel->namePtr, mqel->jobId);
       SetQCtype_MSTRQLIST(mqel->jobId, QC_TYPE_NONE); /* set to go to ims */
       if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                                   Q_S_IMAGE_QC_ACCEPT_ID) == -1)
         printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
       }

       break;

/******************************************************************************/

     case Q_M_IMAGE_READY_FOR_QC_ID:
       printfLLog(LOG_INFO, READY_TO_QC_COMPLETED_PROCESSING_TEXT,
         mqel->namePtr, mqel->jobId);

       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);


       if (getToggleQCstart() == QC_START_INTERACTIVE)
         if (handleSubsysAutoQCspawn(mqel->namePtr, mqel->jobId)) {
           updatePosLabel(ASF_CP,MAIN_STATUS_LIST,mqPos,Q_M_PERFORMING_QC_TEXT);
           updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos, 
                       Q_M_PERFORMING_QC_TEXT);
           gettimeofday(&mqTime);
           setStartTime_MSTRQLIST(mqel->jobId, P_QC, mqTime);
         } else {
           SetHoldFlag_SYSQUE(mqel->namePtr, mqel->jobId);
         }

    
       break;
/******************************************************************************/
     case Q_M_SCAN_READY_FOR_QC_ID:
       printfLLog(LOG_INFO, READY_TO_QC_COMPLETED_PROCESSING_TEXT,
         mqel->namePtr, mqel->jobId);

       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);

       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);

/* force scan qc no matter what */
/*       if (getToggleQCstart() == QC_START_AUTOMATIC) */
         if (handleSubsysAutoScanQCspawn(mqel->namePtr, mqel->jobId)) {
           updatePosLabel(ASF_CP,MAIN_STATUS_LIST,mqPos,Q_M_PERFORMING_QC_TEXT);
           updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos, 
                       Q_M_PERFORMING_QC_TEXT);
         } else {
           SetHoldFlag_SYSQUE(mqel->namePtr, mqel->jobId); 
         }

       break;
/******************************************************************************/

     case Q_M_IMAGE_QC_ON_HOLD_ID:
       /* SetHoldFlag_SYSQUE(mqel->namePtr, mqel->jobId); */
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);

       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
      break;
/******************************************************************************/
     case  Q_M_IMAGE_QC_DONE_ACCEPT_ID:
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       gettimeofday(&mqTime);
       setEndTime_MSTRQLIST(mqel->jobId, P_QC, mqTime);
       if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP, Q_M_CYCLE_COMPLETED_ID)
                 == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
       /*SetQCtype_MSTRQLIST(mqel->jobId, QC_TYPE_DONE); /* set to go to ims */
       SetQCtype_MSTRQLIST(mqel->jobId, QC_TYPE_ACCEPT); 
       clearQueueID( mqel->mstrqODLreq,mqel->namePtr);
      break;

/******************************************************************************/
     case  Q_M_IMAGE_QC_DONE_REJECT_ID:
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       gettimeofday(&mqTime);
       setEndTime_MSTRQLIST(mqel->jobId, P_QC, mqTime);
           /* if image rejected, don't catalog; if scan result rejected,
              we still want to catalog the scan results */
       if (GetQCtype_MSTRQLIST(mqel->jobId) == QC_TYPE_IMAGE)
         SetCatalogStep_MSTRQLIST(mqel->jobId, IMS_TYPE_DONE); 
       else
         SetQCtype_MSTRQLIST(mqel->jobId, QC_TYPE_SCAN_REJECT); 
/* ask operator to supply reason the job was deleted */

       ASFlogMessage(ASF_CP, WP_TEXT_BOX, "%d", mqel->jobId);

/**** move this to  callback for text box
       if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP, Q_M_CYCLE_COMPLETED_ID)
             == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
****/
      break;

/******************************************************************************/

     case Q_M_INTERRUPTED_ID:
       ASFlogMessage_direct(mqel->namePtr, QUE_STATUS_LABEL_ID,
         SYS_CURRENT_INTERRUPTED_TEXT, mqel->jobId);

       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);

/* set the sysq repeat flag so the job will be restarted next time */
       SetRepeatFlagGivenJobId_SYSQUE( mqel->namePtr, mqel->jobId);

/* set media flag so we check the media id next time (when applicable) */
       SetMediaFlagGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId, 
                                     MEDIA_CHECK_YES); 
       break;
/******************************************************************************/

     /* may not need next case */
     case  Q_M_RESTART_HERE_ID:
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       break;
/******************************************************************************/
     case Q_M_ENTRY_DELETED_ID:
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
                                 /* remove display line from subsys window */

       SetCatalogStep_MSTRQLIST(mqel->jobId, IMS_TYPE_DONE);

/* ask operator to supply reason the job was deleted */

       ASFlogMessage(ASF_CP, WP_TEXT_BOX, "%d", mqel->jobId);

/**** move this to the callback for the text entry box 
       if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP,Q_M_NEXT_STEP_ID) == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
******/
       break;
/******************************************************************************/

     case Q_M_SCAN_COMPLETED_ID:
        printfLLog(LOG_DEBUG,CP_COMPLETED_TEXT, ASF_CP, mqel->jobId);

       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
                     
      if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP, Q_M_CYCLE_COMPLETED_ID) 
                                   == -1)
          printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);

      break;
/******************************************************************************/
     case Q_M_HANDLING_ERROR_ID:
       if (GetSubsystemCategoryGivenName(mqel->namePtr) == IMS_CATEGORY_ID)
         if (GetSendAck_MSTRQLIST(mqel->jobId) == TRUE) {
          sendAck(mqel->mstrqODLreq, mqel->namePtr, mqel->jobId);
          ClearSendAck_MSTRQLIST(mqel->jobId) ;
         }
       break;
/******************************************************************************/

     case Q_M_FINAL_ID:
       /* possible deletion from mstrq here */
       /* the following code deletes the item from the 
        * queue window as well as the queue itself */
      /*****
         * -- following removes element from queue --  *
        FreeMstrqListElem_MSTRQLIST(mqel); 
        mqel = RemoveMstrqFromList_MSTRQLIST(elemIndex);
      ******/

        /* removes ONLY odl part of element -- 
         * but maintains integerity of queue for display */
        /* nancy: now that we display the odl part, let's keep it around */

/* this is the pps-related stuff...*/
       ODLGetVal(mqel->mstrqODLreq, BODY_MODE, &GLOBAL_ppsLastModeStr);
       ODLGetVal(mqel->mstrqODLreq, HDR_MSG_TYPE, &GLOBAL_ppsLastTypeStr);
       GLOBAL_ppsLastMode = getModeId(GLOBAL_ppsLastModeStr);
       GLOBAL_ppsLastType = getTypeId(GLOBAL_ppsLastTypeStr);
/* end is the pps-related stuff... not sure where it should go */

         FreeMstrqListElem_MSTRQLIST(mqel); 
         mqel = RemoveMstrqFromList_MSTRQLIST(elemIndex+1);
         removeLineForJobId(ASF_CP, mqel->jobId);
       /* RemoveMstrqFromList_MSTRQLIST also deletes the odl */
       /* RemoveODLreqFromList_MSTRQLIST(elemIndex);   */

       break;
/******************************************************************************/
     case  Q_M_PROCESSING_RESET_ID: 
       ChangeProcessState_PIDLIST(mqel->namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);

       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
                              /*change current message in subsystem window */
       ASFlogMessage_direct(mqel->namePtr, QUE_STATUS_LABEL_ID, 
         SYS_RESET_PROCESSING_TEXT, mqel->jobId);
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);

/* set the sysq repeat flag so the job will be restarted next time */
       SetRepeatFlagGivenJobId_SYSQUE( mqel->namePtr, mqel->jobId);
/* set media flag so we check the media id next time (when applicable) */
       SetMediaFlagGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId, 
                                     MEDIA_CHECK_YES); 

      cat = GetSubsystemCategoryGivenName(mqel->namePtr);
      if (cat == RDS_CATEGORY_ID && rdsMountDialog)      /* popdown rds tape */
        popdownMsgDialog(rdsMountDialog);                /* mount dialog box */
      else if (cat == ASP_CATEGORY_ID && aspMountDialog) /* popdown asp tape */
        popdownMsgDialog(aspMountDialog);                /* mount dialog box */


       break;
/******************************************************************************/
     case  Q_M_DONE_ERROR_ID: 
      updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);

      updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
      ChangeProcessState_PIDLIST(mqel->namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);

      SetMediaFlagGivenJobId_SYSQUE(mqel->namePtr, mqel->jobId,
                                     MEDIA_CHECK_YES);

/* job will stay in this state until the cleanup box is closed */

       break;
/******************************************************************************/
     case Q_M_INPUT_SOURCE_READY_ID:
        if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                Q_S_READY_TO_SEND_ID) == -1)
           printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);
       break;
/******************************************************************************/

     case Q_M_INPUT_SOURCE_NOT_READY_ID:
                /* update messages in main and subsys windows */
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       break;
/******************************************************************************/
     case Q_M_CHECKING_TAPE_ID:
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       break;
/******************************************************************************/
     case Q_M_REPEAT_LAST_STEP_ID:

       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);

                               /* find out who gets the request next */
       nextNamePtr = (char *) doMalloc(strlen(mqel->namePtr) + 1) ;
       strcpy(nextNamePtr, mqel->namePtr);
       switch(determineClassFromName(nextNamePtr) ) {
         case CP_CLASS_ID:   /* if rds, change the status to scan completed */
                             /* otherwise change the status to ready to QC */
           if (GetSubsystemCategoryGivenName(mqel->namePtr)==RDS_CATEGORY_ID) {
             if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP, 
                                    Q_M_SCAN_COMPLETED_ID) == -1)
                printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, ASF_CP);
           } 
           else {         
               if (ChangeStatus__MSTRQLIST(mqel->jobId, ASF_CP, 
                      Q_M_CYCLE_COMPLETED_ID) == -1)
                   printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, ASF_CP);
           }
           break;

         case PPS_CLASS_ID:  
         case IMS_CLASS_ID:  
         case SUBSYS_CLASS_ID:           /* put running jobs back on queue */
           if (GetQCtype_MSTRQLIST(mqel->jobId) != QC_TYPE_DONE) 
             if (ChangeStatus__MSTRQLIST(mqel->jobId, nextNamePtr, 
                                   Q_M_PLACED_ON_SYSQ_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_MSTRQ_STATUS);

           break;
         default:
           printfLLog(LOG_ERR, CANNOT_DETERMINE, "next processing step");
           break;
        } /* end switch determine... */ 

      break;  /* end of case Q_M_REPEAT_LAST_STEP_ID */
/******************************************************************************/

     case Q_M_PLACED_ON_HOLD_ERROR_ID:
       SetHoldFlag_SYSQUE(mqel->namePtr, mqel->jobId);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);

       break;

/******************************************************************************/
     case Q_M_PLACED_ON_HOLD_DATA_ID:
       SetHoldFlag_SYSQUE(mqel->namePtr, mqel->jobId);
       updateStatusLabel(ASF_CP, mqPos, mqel->status, mqel->namePtr);
       updateStatusLabel(mqel->namePtr, sqPos, mqel->status, mqel->namePtr);


       break;
/******************************************************************************/

     case Q_M_READY_TO_SEND_ID:
            if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                Q_M_GOT_ITEM_OFF_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
      break;

/******************************************************************************/
    case Q_M_GOT_ITEM_OFF_ID:

        cat = GetSubsystemCategoryGivenName(mqel->namePtr);
        if (cat == RDS_CATEGORY_ID ) {
                   /* put data ready/not ready message in both queue windows */
          rdsNum = getRDSnumGivenName(mqel ->namePtr);
          if (mqel->dataSource == DATA_FROM_DISK)
            sprintf(qLocBuf, Q_M_INPUT_SOURCE_READY_TEXT, mqel->namePtr);
          else
            sprintf(qLocBuf, Q_M_INPUT_SOURCE_NOT_READY_TEXT, mqel->namePtr);
          updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, qLocBuf);
          updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos, qLocBuf);

                       /* prompt for operator to mount tape if needed */
          if (mqel->dataSource == DATA_FROM_TAPE) {
            printfLLog(LOG_DEBUG, 
                 "%s data from tape - not mounted, last tape mounted %s", 
                       mqel->namePtr, GLOBAL_lastRDSMediaMounted[rdsNum]);

            strcpy(mountStr, BuildMountRequestLabel(mqel));
                  /* only prompt if this media is different than the last */
            if ( (strcmp(GLOBAL_lastRDSMediaMounted[rdsNum], "") == 0) ||
              strcmp(GLOBAL_lastRDSMediaPrompted[rdsNum],GLOBAL_lastRDSMediaMounted[rdsNum]) 
                              != 0) {
              mqelcopy = GetCopyGivenJobId__MSTRQLIST(mqel->jobId);
              w = display_msg(mountStr, "Mount Tape", TapeMountedCB, mqelcopy,
                      TapeNotMountedCB, mqelcopy, 0);
              rdsMountDialog = w;
            }
            else  { /* tape already mounted: same media id as last time */
              printfLLog(LOG_DEBUG, 
                 "%s data from tape - already mounted, last tape mounted %s", 
                       mqel->namePtr, GLOBAL_lastRDSMediaMounted[rdsNum]);
              if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
                Q_S_READY_TO_SEND_ID) == -1)
               printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
              SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
              SetDataReady_MSTRQLIST(mqel->jobId);
            }
          }
          else {   /* data source is from disk: data is already ready */
            printfLLog(LOG_DEBUG, "%s data from disk - no need to mount",
                       mqel->namePtr);
            if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
                Q_S_READY_TO_SEND_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
            SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
            SetDataReady_MSTRQLIST(mqel->jobId);
          }
        }
        else if (cat == ASP_CATEGORY_ID) {
                   /* put data ready/not ready message in both queue windows */
          if (mqel->dataSource == DATA_FROM_DISK)
            sprintf(qLocBuf, Q_M_INPUT_SOURCE_READY_TEXT, mqel->namePtr);
          else
            sprintf(qLocBuf, Q_M_INPUT_SOURCE_NOT_READY_TEXT, mqel->namePtr);
          updatePosLabel(ASF_CP, MAIN_STATUS_LIST, mqPos, qLocBuf);
          updatePosLabel(mqel->namePtr, QUE_STATUS_LIST, sqPos, qLocBuf);

                       /* prompt for operator to mount tape if needed */
          if (mqel->dataSource == DATA_FROM_TAPE) {
            strcpy(mountStr, BuildMountRequestLabel(mqel));
                  /* only prompt if this media is different than the last */
            if ( (strcmp(GLOBAL_lastASPMediaMounted, "") == 0) ||
              strcmp(GLOBAL_lastASPMediaPrompted,GLOBAL_lastASPMediaMounted) 
                              != 0) {
              mqelcopy = GetCopyGivenJobId__MSTRQLIST(mqel->jobId);
              w = display_msg(mountStr, "Mount Tape", TapeMountedCB, mqelcopy,
                      TapeNotMountedCB, mqelcopy, 0);
              aspMountDialog = w;
            }
            else  { /* tape already mounted: same media id as last time */
              if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
                Q_S_READY_TO_SEND_ID) == -1)
               printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
              SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
              SetDataReady_MSTRQLIST(mqel->jobId);
            }
          }
          else {   /* data source is from disk: data is already ready */
            if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
                Q_S_READY_TO_SEND_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
            SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
            SetDataReady_MSTRQLIST(mqel->jobId);
          }


        }
        else if (cat == SSP2_CATEGORY_ID) {  
                                         /* ssp : data is already ready */
          if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
              Q_S_READY_TO_SEND_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
          else {
            SetDataReady_MSTRQLIST(mqel->jobId);
            SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
          }
        }
        else if (cat == IMS_CATEGORY_ID || cat == PPS_CATEGORY_ID) {
          if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr,
              Q_S_READY_TO_SEND_ID) == -1)
             printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
        }

       break;

     case Q_S_CHECKING_TAPE_ID: /* pass these along to sysq */
     case Q_S_DONE_ID:
     case Q_S_PLACED_ON_SYSQ_ID:
     case Q_S_READY_TO_SEND_ID:
     case Q_S_IMAGE_QC_READY_ID:
     case Q_S_IMAGE_QC_ON_HOLD_ID:
     case Q_S_IMAGE_QC_ACCEPT_ID:
     case Q_S_IMAGE_QC_REJECT_ID:
     case Q_S_GOT_ITEM_OFF_ID:
     case Q_S_RCVD_ACK_ID:
       /* set in cpreadThread and above, so handle here as null and let
          sysq deal with state change */
       printfLLog(LOG_DEBUG, MSTRQ_CAUGHT, mqel->jobId, statusStr,mqel->status);
       break;

/******************************************************************************/
     default:
       printfLLog(LOG_ERR, NO_PRODUCT_REQUEST_STATUS_TEXT, mqel->jobId);

       break;
/******************************************************************************/
    } /* end switch mstrq->status */

  if (mqel != NULL)
    FreeMstrqListElem_MSTRQLIST(mqel); 

#ifdef READ_LOCK_DEBUG
printf("handleMQ unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);
  doFree(statusStr);
  return(0);

} /* end handleMSTRq ...........................*/


/*----------------------------------------------------------
 * NAME:
 *  handleQueVisibility 
 *
 * DESCRIPTION:
 *  make the visibilty of SYSQueue window 
 *
 * NOTES:
 *  -- pidList elem is a copy of the first modified
 *  state changed element, once copied the state
 *  is set to unmodifed
 *
 *---------------------------------------------------------*/
static int handleQueVisibility(pidListElemType  *pidEl)
{
 char titleBuf[MAX_INV_NAME_LEN];
 int  isIconic, retval = -1;
                                 /* if wid is null, this is not a subsystem */
  if (pidEl->mainWid != NULL) {
    XtVaGetValues(pidEl->mainWid, XmNiconic, &isIconic, NULL);
    switch(pidEl->state) {
      case SUBSYS_STARTED_ID:
        UxPopupInterface(pidEl->mainWid, no_grab);
        retval = 0;
        break;


      case SUBSYS_WAITING_ID:
        showNotListening(GetJobIdListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetPlatRevSeqListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetFrameListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetMediaListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetModeListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetRequestListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetTypeListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetStatusListGivenName_PIDLIST(pidEl->procNamePtr));

        sprintf(titleBuf, "%s Queue", pidEl->procNamePtr);
        XtVaSetValues(pidEl->mainWid, XmNtitle, titleBuf,
           XmNbackground, 0x0, 
           NULL);
        UxPopupInterface(pidEl->mainWid, no_grab);
        retval = 0;
        break;

      case SUBSYS_READY_ID:
      case SUBSYS_RUNNING_ID:
        showListening(GetJobIdListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetPlatRevSeqListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetFrameListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetMediaListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetModeListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetRequestListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetTypeListGivenName_PIDLIST(pidEl->procNamePtr)); 
        showListening(GetStatusListGivenName_PIDLIST(pidEl->procNamePtr)); 
        sprintf(titleBuf, "%s Queue", pidEl->procNamePtr);
        XtVaSetValues(pidEl->mainWid, XmNtitle, titleBuf,
          XmNbackground, 0x0, 
          NULL);
        UxPopupInterface(pidEl->mainWid, no_grab);
        retval = 0;
        break;

      case SUBSYS_NOT_RUNNING_ID:
        showNotListening(GetJobIdListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetPlatRevSeqListGivenName_PIDLIST(pidEl->procNamePtr))
;
        showNotListening(GetFrameListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetMediaListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetModeListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetRequestListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetTypeListGivenName_PIDLIST(pidEl->procNamePtr));
        showNotListening(GetStatusListGivenName_PIDLIST(pidEl->procNamePtr));

        iconifyWindow(pidEl->mainWid); 
 
        UxPopdownInterface(pidEl->mainWid);  
        retval = 0;
        break;

      default:
        break;
    } /* end switch */
  }

  return(retval);

} /* end * handleQueVisibility .................*/ 


/*----------------------------------------------------------
 * NAME:
 *  handleProcessStateDisplay
 *
 * DESCRIPTION:
 *  Maintain the display of each subsystem's current status in
 *  the Subsystem Control box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static int handleProcessStateDisplay(pidListElemType  *pidEl)
{
   switch(GetSubsystemCategoryGivenName(pidEl->procNamePtr)) {
    case RDS_CATEGORY_ID:
    case SSP2_CATEGORY_ID:
    case ASP_CATEGORY_ID:

#ifdef DEBUG
printf("handleProcessStateDisplay: update color listpos %d, display col %d\n",  pidEl->listPos, mapStateForDisplay(GetProcessState_PIDLIST(pidEl->procNamePtr)));
#endif

/* for these list positions passed to updateStateColor, the first
   column in the row column widget is taken by the subsytem name.
   the pid el list pos starts at 1 and the rc columns start at 0,
   so if the first column wasn't taken up by the subsys name, we 
   would have to pass listpos - 1 to updateStateColor */

       if (pidEl->state == SUBSYS_STARTED_ID)
         resetStateColor(pidEl->listPos-1,  DISP_COL_ERROR, CPstatusRC);

       updateStateColor(pidEl->listPos-1,  /* rc widget rows start at 0 */
         mapStateForDisplay(GetProcessState_PIDLIST(pidEl->procNamePtr)),
         CPstatusRC );
        /*  CPstateRC[pidEl->listPos] ); */

       if (GetQCstatus_SYSQUE(pidEl->procNamePtr))
         updateStateColor(pidEl->listPos-1, DISP_COL_QC, CPstatusRC);
       else  /* if no job is in qc mode, reset the qc entry */
         resetStateColor(pidEl->listPos-1, DISP_COL_QC, CPstatusRC);
     break;

    case PPS_CATEGORY_ID:
    case IMS_CATEGORY_ID:
       if (pidEl->state == SUBSYS_STARTED_ID)
         resetStateColor(pidEl->externPos-1,  DISP_COL_ERROR, CPexternRC);

       updateStateColor(pidEl->externPos-1,  /* rc widget rows start at 0 */
         mapStateForDisplay(GetProcessState_PIDLIST(pidEl->procNamePtr)),
         CPexternRC );

     break;

    default:
     break;
   } /* end switch */

   /* FreePidListElem_PIDLIST(pidEl); */
  return(0);

} /* end handleProcessStateDisplay.................................*/

/*----------------------------------------------------------
 * NAME:
 *  handleProcTermination
 *
 *
 * DESCRIPTION:
 *  as process terminate following the handling in the sig
 *  cld,  perform clean necessary up.
 *
 * NOTES:
 *  the following code was used to kill the associated
 *  read thread:
 *     if (pidEl->assocThread != 0)
 *     {
 *       retval = kill(pidEl->assocThread, SIGKILL);
 *       printfLLog("could not kill %s associated thread %d\n", 
 *                                   pidEl->procNamePtr,
 *                                   pidEl->assocThread);
 *     }
 *  however that caused the process to block in fprintf
 *  do to a locked semaphore that was not unlocked when 
 *  the thread died. to deal with this, let the thread
 *  terminate when the socket and other process die
 *
 *---------------------------------------------------------*/
static void handleProcTermination(pidListElemType *pidEl)
{
 int    retval;
 char   *fileDeletePtr;
 char   *logLocPtr;

#ifdef PROC
printf("handleProcTermination: %s state %s(%d)\n", pidEl->procNamePtr,
        (char *) GetProcessStateAsText(pidEl->state), pidEl->state);
#endif
  if (pidEl->state == SUBSYS_NOT_RUNNING_ID) {

             /* check if we have any normal or abnormal terminations */
    if (pidEl->termination_reason == NORMAL_TERMINATION_ID) {
      printfLLog(LOG_DEBUG, TERMINATED_TEXT, pidEl->procNamePtr);
    }
    else{
/* pop up error box telling the user that the subsystem died unexpectedly */
      ASFlogMessage(ASF_CP, WP_ERROR_BOX, DIED_UNEXPECTEDLY_TEXT,
                   pidEl->procNamePtr );

     printfLLog(LOG_ERR,DIED_UNEXPECTEDLY_TEXT,pidEl->procNamePtr);
    } /* end if termination reason */


    switch(pidEl->classID) {
      case SUBSYS_CLASS_ID:
      case RUTIL_CLASS_ID: 
         /* log the normal termination; abnormal has been logged elsewhere */
        close(pidEl->socket);      /* close the socket */

                                      /* if a subsystem, clear it  */
        if (pidEl->classID == SUBSYS_CLASS_ID) {
          logLocPtr = (char *)getLogLocation();
          if (isLogFileSyslog(logLocPtr) == FALSE) {
            fclose(pidEl->logfp);     /* close the log file */

                                               /* clear the log file fp */
          ClearlogfpGivenName_PIDLIST(pidEl->procNamePtr);

                                               /* delete the log file */
          fileDeletePtr = (char *)doMalloc(strlen("/bin/rm -f") +
                            strlen(pidEl->logFileNamePtr) + 2);  
          strcpy(fileDeletePtr, "/bin/rm -f");
          strcat(fileDeletePtr, pidEl->logFileNamePtr);
          system(fileDeletePtr);
          doFree(fileDeletePtr);
          }
                              /* find the first job processing on this 
                               * system and change its status to interrupted */
          retval = ChangeStatusToInterrupted_MSTRQLIST( pidEl->procNamePtr, 
                       SUBSYS_STOPPING_ID);

                                /* change process state to dormant */
          retval = ChangeProcessState_PIDLIST(pidEl->procNamePtr, 
            SUBSYS_DORMANT_ID, NOT_SET_ID);
          if (retval == -1)
            printfLLog(LOG_ERR, CANNOT_SET "process state to dormant");

        } /* end if subsys */

                                  /* if RUTIL class, remove the element */
        if (pidEl->classID ==  RUTIL_CLASS_ID) 
          RemovePidFromListGivenPid_PIDLIST(pidEl->ppid);
        break;  /* end SUBSYS and RUTIL case */

      case CP_CLASS_ID:
        break;

      case QC_CLASS_ID: 
        /* RemovePidFromListGivenPid_PIDLIST(pidEl->ppid); */
        break;

      case IMS_CLASS_ID:
      case PPS_CLASS_ID:
        close(pidEl->socket);      /* close the socket */

                             /* find the first job processing on this
                               * system and change its status to interrupted */
          retval = ChangeStatusToInterrupted_MSTRQLIST( pidEl->procNamePtr,
                       SUBSYS_STOPPING_ID);

          retval = ChangeProcessState_PIDLIST(pidEl->procNamePtr,
            SUBSYS_DORMANT_ID, NOT_SET_ID);
          if (retval == -1)
            printfLLog(LOG_ERR, CANNOT_SET "process state to dormant");
          if (pidEl->classID == PPS_CLASS_ID) {
            ClearMsgOut_PPS(TRUE);
          }

        break;

      default:
        break;
    } /* end switch classID */

  } 

} /* end handleProcTermination.................*/


/*----------------------------------------------------------
 * NAME:
 *  handleProcessStateChange
 *
 * DESCRIPTION:
 *   perform processing when a subsystem state changes: remove
 *   timeouts, update control box, update queue lists in main
 *   and subsystem queue windows
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int handleProcessStateChange()
{
 int retval=0;
 pidListElemType  *pidEl;
 baseListElemType *nPtr;

#ifdef READ_LOCK_DEBUG
printf("handleProcessStateChange locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
     return(0);

  while ( (pidEl = GetFirstProcessWithModifedState_PIDLIST(DO_COPY)) != NULL) {
#ifdef PROC
printf("handleProcessStateChange: %s state %s(%d)\n", pidEl->procNamePtr,
        (char *) GetProcessStateAsText(pidEl->state), pidEl->state);
#endif
    switch(pidEl->state) {
     case SUBSYS_WAITING_ID:
     case SUBSYS_READY_ID:
     case SUBSYS_RUNNING_ID:
       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      START_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "start timeout");

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      HALT_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "halt timeout");

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      HEALTH_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "health timeout");

/******* */
       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      CLEANUP_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "cleanup timeout");
/***** 
       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      MSG_SENT_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "sent timeout");
/*********/
       break;

     case SUBSYS_DORMANT_ID:
     case SUBSYS_NOT_RUNNING_ID:
      /*  try and remove the proc timeout -- note that 
       *  even though this is really only for subsystems
       *  if we remove the timeout quickly enough there wont
       *  be as many error messages to confuse the user */

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                     START_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "start timeout");

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      STOP_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "stop timeout");

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      MSG_SENT_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "sent timeout");

       if ((retval = RemoveTimeOut_PIDLIST(pidEl->procNamePtr,
                                      CLEANUP_INTERVAL_ID)) == -1)
             printfLLog(LOG_ERR, CANNOT_REMOVE, "cleanup timeout");

       break;
     default:
       break;

    } /* end switch */

    handleQueVisibility(pidEl);
    handleProcessStateDisplay(pidEl);
    handleProcTermination(pidEl);
    FreePidListElem_PIDLIST(pidEl);
  } /* while */

#ifdef READ_LOCK_DEBUG
printf("handleProcessStateChange unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);

  return(retval);
} /* end handleProcessStateChange .............................*/


/*----------------------------------------------------------
 * NAME:
 *  doFilterMatch
 *
 * DESCRIPTION:
 *  determine if log file string belongs in this window 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int doFilterMatch(char *namePtr, char *buf)
{
 int i;
 char *basePtr, *freePtr,  *tokenPtr;
 char *lastPtr = NULL; 

  basePtr = freePtr = (char *)doMalloc(strlen(buf) + 1);
  strcpy(basePtr, buf);
  
  for (i = 0; i < 5; i++) {
    tokenPtr = strtok_r(basePtr, " ", &lastPtr);
    basePtr      = NULL;
  }
  doFree(freePtr);
  if (tokenPtr == NULL)
   return(FALSE);

  if (strncmp(namePtr, tokenPtr, strlen(namePtr)) == 0)
     return(TRUE);
  return(FALSE);

} /* end doFilterMatch.....................................*/



/*----------------------------------------------------------
 * NAME:
 *  cpXworkProc 
 *
 * DESCRIPTION:
 *  the X workproc routine that handles the subsystem's
 *  queues, the master queue, the subsystem's executable
 *  state display, termination, current product processing
 *  display, and the display of log updates. 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void handleSignalQueue()
{
  int i;
  baseListElemType *sPtr, *rPtr;

  sPtr = lockNameList_CORE(SIGCLD_LIST);
  if (sPtr == NULL)
     return;


#ifdef READ_LOCK_DEBUG
printf("handleSignalQueue locking READ_LIST\n"); fflush(stdout);
#endif

  rPtr = lockNameList_CORE(READ_LIST);
  if (rPtr == NULL) {
     unLockNameList_CORE(sPtr);
     return;
  }


#ifdef SIG_DEBUG
if (GLOBAL_signalPidCount)
  printf("handleSignalQueue: process id count %d\n", GLOBAL_signalPidCount);
#endif

  if (!GLOBAL_signalPidCount) {
    unLockNameList_CORE(sPtr);

#ifdef READ_LOCK_DEBUG
printf("handleSignalQueue unlocking READ_LIST\n"); fflush(stdout);
#endif

    unLockNameList_CORE(rPtr);
    return;
  }

  for (i = 0; i < GLOBAL_signalPidCount; i++) {

#ifdef SIG_DEBUG
  printf("handleSignalQueue: to process pid %d status %d\n", GLOBAL_signalPid[i]
, GLOBAL_signalStatus[i]);
#endif

    do_sig_cld(GLOBAL_signalPid[i], GLOBAL_signalStatus[i]);
}

#ifdef SIG_DEBUG
if (GLOBAL_signalPidCount)
  printf("handleSignalQueue: process id count %d\n", GLOBAL_signalPidCount);
#endif

  GLOBAL_signalPidCount = 0;

#ifdef READ_LOCK_DEBUG
printf("handleSignalQueue unlocking READ_LIST\n"); fflush(stdout);
#endif

  unLockNameList_CORE(rPtr);
  unLockNameList_CORE(sPtr);

}


int cpXworkProc(XtPointer client_data)
{
 int retval, i, acceptThread=0, anyHung=0;
 static int mode, type;
 char *namePtr;
 FILE *fp;
 static int count = 0, notified=0;
 pidListElemType  *pidEl;

  getEvent();
  handleSignalQueue();

  if (GLOBAL_exitingCP) {   /* first check to see if any outstanding children */
                            /* exist; wait for them to stop, or we must kill */
                            /* them explicitly before cancelling the CP's */
                            /* accept thread */
    i=0;
    while ((pidEl = GetCopyOfElemIn_PIDLIST(i++) ) != NULL) {
      printPidEl(pidEl);
      if ((pidEl->classID == PPS_CLASS_ID || pidEl->classID == IMS_CLASS_ID))
        if (pidEl-> ppid != 0 && pidEl->state == SUBSYS_STOPPING_ID ) {
          anyHung = 1;
        }

      if (pidEl->classID == THREAD_CLASS_ID) 
        acceptThread = pidEl->ppid;

    }

    if (!anyHung) { /* no children left -- kill accept thread and we're done! */
      baseListElemType *nPtr;

      nPtr = lockNameList_CORE(READ_LIST); /* if everone is gone we shouldn't */
      if (nPtr == NULL)                    /* need to lock this, but go ahead */
        return(-1); /* not sure returning is the right thing to do... */

      pthread_cancel(acceptThread); /* this is the true exit point for the CP */
      unLockNameList_CORE(nPtr);    /* this should never get executed */
                                    /* unless the pthread_cancel fails */

    }
    else {
      if (!notified)
        ASFlogMessage(ASF_CP, WP_INFO_BOX, "CP waiting for all threads to terminate...");
        notified = 1;
    }
  }

  i = 0;
  while ((namePtr = (char *)getSubsysNameNo(i++)) != NULL) {
    handleSysQ(namePtr);
  } /* end while */

fflush(stdout); /* nancy: for debugging */

  handleMSTRq(&mode, &type);
  handleProcessStateChange();
  handleXwpq(NULL);

  return(FALSE);

} /* end cpXworkProc ...........................*/
