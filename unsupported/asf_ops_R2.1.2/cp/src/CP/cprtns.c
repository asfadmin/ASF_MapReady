


#define PTHREAD_CANCEL /* cancel the accept thread using pthread_cancel */
                       /* how else will we do it???? */





static char sccsid_cprtns_c[] = "@(#)cprtns.c	4.175 97/08/12 09:47:57";

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <bstring.h>  /* bzero */
#include <string.h>
#include <unistd.h> /* gethostname */
#include <sys/param.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>

/* adde for popupCleanupBox */
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/Frame.h>
#include <Xm/Protocols.h>



#include <Xm/MainW.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushB.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/Label.h>

#include <task.h>

#include "UxXt.h"
#include "asf.h"    
#include "odl.h"    /* contains tuan's value_data for agg */
#include "asfcommon.h"
#include "cpdefines.h"
#include "cprtns.h"
#include "cprtns_xwp.h"
#include "display.h"
#include "cpconfig.h"
#include "cpreadThread.h"
#include "serverSocketXport.h"
#include "memUtils.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"
#include "que_sys.h"
#include "que_xwp.h"
#include "inet.h"
#include "cplogs.h"
#include "utils.h"
#include "xutils.h"
#include "logUtils.h"
#include "cpmainrtns.h"
#include "cpbuildMsg.h" /* take this out!*/
#include "CPmainQ.h"
#include "CPquestionBox.h"
#include "cpmenu.h"
#include "cpworkProc.h"
#include "pps.h" 
#include "qexit_stati.h" 
#include "pthread_wrapper.h" 

#define HELP_PATH "cp"

/* parameters for the look of the state-table area on the main window */

#define SUBSYS_XPM "/usr/include/X11/bitmaps/hlines2"


/* global variables ***********************/

extern int GLOBAL_pos;

int GLOBAL_exitingCP = FALSE;
int GLOBAL_signalPidCount = 0;
int GLOBAL_signalPid[MAX_CHILD_PROCESSES];
int GLOBAL_signalStatus[MAX_CHILD_PROCESSES];
char GLOBAL_lastRDSMediaMounted[MAX_RDS][512];
char GLOBAL_lastRDSMediaPrompted[MAX_RDS][512];
char GLOBAL_lastASPMediaMounted[512];
char GLOBAL_lastASPMediaPrompted[512];

#ifdef MULTIPLE_INFO_BOXES
Widget create_CPdetailedInfo();
#endif

extern Widget           detailedQueItemInfoBoxWidget;
extern Widget           CPdetailedInputFiles;
extern Widget           CPdetailedOutputFiles;
extern Widget           CPdetailedJobId;
extern Widget           CPdetailedSubsys;
extern Widget           CPdetailedOdlText;

extern Widget           toggleScanDest_RDS;
extern Widget           toggleScanDest_ASP;
extern Widget           toggleQC_automatic;
extern Widget           toggleQC_manual;
extern Widget		UxTopLevel;
extern Widget		CPmainQ;
extern Widget           CPfileBox;
extern Widget           CPhelp;
extern Widget           CPhelpText;
/* end global variables .................*/

#define COPYRIGHT_PROG_NAME  "$ASF/bin/CP_copyright" 
/* #define COPYRIGHT_PROG_NAME  "$HOME/src/cp/CP_copyright" */

#define RSH_PROG_NAME      "rsh"
#define RSH_PROG_PATH_NAME "/usr/bsd/rsh"
#define RSH_PROG_ARGS      "-n"
 
#ifndef NDEBUG
    int   DebugLevel;
#endif

#define titleMsg ""
#define howMsg "Cleanup processing must be performed for job %d due to subsystem error.\nPlease select a cleanup method and job resubmittal option.\n" 

typedef struct {
    char    *namePtr;
    int     jobId;
    char    *platform;
    int     rev;
    int     discard;
    int     resubmit;
} CleanupStruct;

char *getQCstatusAsText(int status)
{
  char *qcStatusPtr;

      switch(WEXITSTATUS(status)) {
/****** these are the qc values ***********/
        case QC_NO_LEADER_FILE:
          qcStatusPtr  = QC_NO_LEADER_FILE_TXT;
          break;

        case QC_MALLOC_ERROR:
          qcStatusPtr  = QC_MALLOC_ERROR_TXT;
          break;

        case QC_DISPLAY_ERROR:
          qcStatusPtr  = QC_DISPLAY_ERROR_TXT;
          break;

        case QC_CALLOC_ERROR:
          qcStatusPtr  = QC_CALLOC_ERROR_TXT;
          break;

        case QC_XAllocColor_ERROR:
          qcStatusPtr  = QC_XAllocColor_ERROR_TXT;
          break;

        case QC_OPEN_LEADER_FILE_ERROR:
          qcStatusPtr  = QC_OPEN_LEADER_FILE_ERROR_TXT;
          break;

        case QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR:
          qcStatusPtr  = QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT;
          break;

        case QC_PROGRAMMER_ERROR:
          qcStatusPtr  = QC_PROGRAMMER_ERROR_TXT;
          break;

        case QC_OPEN_CEOS_FILE_ERROR:
          qcStatusPtr  = QC_OPEN_CEOS_FILE_ERROR_TXT;
          break;

        case QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR:
          qcStatusPtr  = QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT;
          break;

        case QC_NON_8_BIT_IMAGE:
          qcStatusPtr  = QC_NON_8_BIT_IMAGE_TXT;
          break;

        case QC_LINE_SIZE_MISMATCH:
          qcStatusPtr  = QC_LINE_SIZE_MISMATCH_TXT;
          break;

        case QC_CHANNEL_SIZE_MISMATCH:
          qcStatusPtr  = QC_CHANNEL_SIZE_MISMATCH_TXT;
          break;

        case QC_OPEN_AVERAGE_FILE_ERROR:
          qcStatusPtr  = QC_OPEN_AVERAGE_FILE_ERROR_TXT;
          break;

        case QC_AVERAGE_FILE_UNEXPECTED_READ_RETURN_ERROR:
          qcStatusPtr  = QC_AVERAGE_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT;
          break;

        case QC_IMPROPER_CEOS_VALUE:
          qcStatusPtr  = QC_IMPROPER_CEOS_VALUE_TXT;
          break;

        case QC_UNKNOWN_IMAGE_TYPE:
          qcStatusPtr  = QC_UNKNOWN_IMAGE_TYPE_TXT;
          break;

        case QC_PIXMAP_CREATE_PROBLEM:
          qcStatusPtr  = QC_PIXMAP_CREATE_PROBLEM_TXT;
          break;

        case QC_NO_CEOS_FILE: 
          qcStatusPtr  = QC_NO_CEOS_FILE_TXT;
          break;

/****** these are the scan qc values ***********/
        case QC_SCAN_ERROR: 
          qcStatusPtr  = QC_SCAN_ERROR_TXT;
          break;

        case QC_SRF_ERROR: 
          qcStatusPtr  = QC_SRF_ERROR_TXT;
          break;

        case QC_PARSE_SRF_ERROR: 
          qcStatusPtr  = QC_PARSE_SRF_ERROR_TXT;
          break;

        case QC_BODY_SRF_ERROR: 
          qcStatusPtr  = QC_BODY_SRF_ERROR_TXT;
          break;

        case QC_CFG_ERROR: 
          qcStatusPtr  = QC_CFG_ERROR_TXT;
          break;

        case QC_CFG_RD_ERROR: 
          qcStatusPtr  = QC_CFG_RD_ERROR_TXT;
          break;

        case QC_NO_CFG_ERROR: 
          qcStatusPtr  = QC_NO_CFG_ERROR_TXT;
          break;

        case QC_PARSE_CFG_ERROR: 
          qcStatusPtr  = QC_PARSE_CFG_ERROR_TXT;
          break;

        case QC_SEG_CNT_LESS_ERROR: 
          qcStatusPtr  = QC_SEG_CNT_LESS_ERROR_TXT;
          break;

        case QC_SEG_CNT_LARG_ERROR: 
          qcStatusPtr  = QC_SEG_CNT_LARG_ERROR_TXT;
          break;

        case QC_SEG_CNT_SRF_ERROR: 
          qcStatusPtr  = QC_SEG_CNT_SRF_ERROR_TXT;
          break;

        case QC_BAD_SEG_CNT_ERROR: 
          qcStatusPtr  = QC_BAD_SEG_CNT_ERROR_TXT;
          break;

        case QC_FRM_ID_SRF_ERROR: 
          qcStatusPtr  = QC_FRM_ID_SRF_ERROR_TXT;
          break;

        case QC_SCLAT_SRF_ERROR: 
          qcStatusPtr  = QC_SCLAT_SRF_ERROR_TXT;
          break;

        case QC_SCLON_SRF_ERROR: 
          qcStatusPtr  = QC_SCLON_SRF_ERROR_TXT;
          break;

        case QC_SCTM_SRF_ERROR: 
          qcStatusPtr  = QC_SCTM_SRF_ERROR_TXT;
          break;

        case QC_MALLOC_FRM_ERROR: 
          qcStatusPtr  = QC_MALLOC_FRM_ERROR_TXT;
          break;

        case QC_PLATFM_SRF_ERROR: 
          qcStatusPtr  = QC_PLATFM_SRF_ERROR_TXT;
          break;

        case QC_STATION_SRF_ERROR: 
          qcStatusPtr  = QC_STATION_SRF_ERROR_TXT;
          break;

        case QC_SEGCNT_SRF_ERROR: 
          qcStatusPtr  = QC_SEGCNT_SRF_ERROR_TXT;
          break;

        case QC_FRM_MD_SRF_ERROR: 
          qcStatusPtr  = QC_FRM_MD_SRF_ERROR_TXT;
          break;
        case QC_REV_SRF_ERROR: 
          qcStatusPtr  = QC_REV_SRF_ERROR_TXT;
          break;

        case QC_SEQ_SRF_ERROR: 
          qcStatusPtr  = QC_SEQ_SRF_ERROR_TXT;
          break;

        case QC_SRT_TM_SRF_ERROR: 
          qcStatusPtr  = QC_SRT_TM_SRF_ERROR_TXT;
          break;

        case QC_END_TM_SRF_ERROR: 
          qcStatusPtr  = QC_END_TM_SRF_ERROR_TXT;
          break;

        case QC_SEG_ID_SRF_ERROR: 
          qcStatusPtr  = QC_SEG_ID_SRF_ERROR_TXT;
          break;

        case QC_FRM_CT_SRF_ERROR: 
          qcStatusPtr  = QC_FRM_CT_SRF_ERROR_TXT;
          break;
        case QC_SEG_SRT_TM_ERROR: 
          qcStatusPtr  = QC_SEG_SRT_TM_ERROR_TXT;
          break;

        case QC_SEG_STP_TM_ERROR: 
          qcStatusPtr  = QC_SEG_STP_TM_ERROR_TXT;
          break;

        case QC_LARGE_TM_GAP_ERROR: 
          qcStatusPtr  = QC_LARGE_TM_GAP_ERROR_TXT;
          break;

        case QC_MALLOC_FRAME_ERROR: 
          qcStatusPtr  = QC_MALLOC_FRAME_ERROR_TXT;
          break;

        case QC_WRG_SEG_TM_ERROR: 
          qcStatusPtr  = QC_WRG_SEG_TM_ERROR_TXT;
          break;

        case QC_WRG_FRM_TM_ERROR: 
          qcStatusPtr  = QC_WRG_FRM_TM_ERROR_TXT;
          break;
        case QC_IVL_SRT_TM_ERROR: 
          qcStatusPtr  = QC_IVL_SRT_TM_ERROR_TXT;
          break;
        case QC_IVL_STP_TM_ERROR: 
          qcStatusPtr  = QC_IVL_STP_TM_ERROR_TXT;
          break;
        case QC_IVL_SCN_SRT_ERROR: 
          qcStatusPtr  = QC_IVL_SCN_SRT_ERROR_TXT;
          break;
        case QC_IVL_SCN_STP_ERROR: 
          qcStatusPtr  = QC_IVL_SCN_STP_ERROR_TXT;
          break;
        case QC_IVL_CNT_TM_ERROR: 
          qcStatusPtr  = QC_IVL_CNT_TM_ERROR_TXT;
          break;


/****** didn't match any of the qc or the scan qc values ***********/
        default:
          qcStatusPtr  = "UNKNOWN_STATE"; 
          break;
      } /* end switch */

  return(qcStatusPtr);

} /* getQCstatusAsText */


static void cbRadio(Widget w, XtPointer client_data, XtPointer call_data)
{
  XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct *) call_data;
  CleanupStruct *svs = (CleanupStruct *) client_data;

/*printf("cb radio\n"); */
  if (strcmp(XtName(w), "Discard") == 0)
    svs->discard = (cbs->set) ? 1 : 0;
  if (strcmp(XtName(w), "Save") == 0)
    svs->discard = (cbs->set) ? 0 : 1;
  if (strcmp(XtName(w), "Place on Hold") == 0)
    svs->resubmit = (cbs->set) ? 1 : 0;
  if (strcmp(XtName(w), "Cancel") == 0)
    svs->resubmit = (cbs->set) ? 0 : 1;

/*
 printf("leaving cbRadio - %s pos %d discard %d resubmit %d\n", svs->namePtr, svs->jobId, svs->discard, svs->resubmit);   */
}



static Widget createButtonBox(Widget parent, char *name)
{
  Widget frame, frame2, label, rc;

  frame = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, parent, NULL);

  frame2 = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, frame,
              XmNchildType,                  XmFRAME_TITLE_CHILD,
              XmNchildHorizontalAlignment,   XmALIGNMENT_CENTER,
              XmNchildVerticalAlignment,     XmALIGNMENT_CENTER,
              XmNmarginWidth,                4,
              NULL);

  label = XtVaCreateManagedWidget(name, xmLabelWidgetClass, frame2, NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, frame,
                                 XmNchildType,      XmFRAME_WORKAREA_CHILD,
                                 XmNadjustLast,     False,
                                 XmNradioBehavior,  True,
                                 XmNradioAlwaysOne, True,
                                 XmNorientation,    XmHORIZONTAL,
                                 XmNnumColumns,     3,
                                 XmNpacking,        XmPACK_COLUMN,
                                 NULL);
  return rc;
}

/*----------------------------------------------------------
 * NAME:
 *  popupStartupBox
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void popupStartupBox ()
{
  char *pathName;

  pathName = convertEnvFile(COPYRIGHT_PROG_NAME);
  system(pathName);

/*    system("/user/nancy/src/cp/startup/startup"); */

} /* popupStartupBox */


/*----------------------------------------------------------
 * NAME:
 *  popupCleanupBox
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void popupCleanupBox (xwpQueElemType *xwpEl)
{
  int x, y;
  Widget CPcleanupBox,form, titleLabel, errLabel, howLabel, separator, okButton;
  Widget cleanup_rc, resubmittal_rc, discard_tb, save_tb; /* choice */
  CleanupStruct *svs ;
  char *errorMsg = xwpEl->reqStrPtr, reason[200];

  svs = (CleanupStruct *) doMalloc(sizeof(CleanupStruct)+1);

  svs->jobId = xwpEl->cbData.pos;
  svs->rev = xwpEl->cbData.rev;
  svs->platform = doMalloc(strlen(xwpEl->cbData.platform)+1);
  strcpy(svs->platform , xwpEl->cbData.platform);
  svs->discard = 0;
  svs->resubmit = 1;
  svs->namePtr = doMalloc(strlen(xwpEl->cbData.namePtr)+1);
  strcpy(svs->namePtr, xwpEl->cbData.namePtr);

  queryPointer(&x, &y);

  CPcleanupBox = XtVaCreatePopupShell( "Cleanup Confirmation", 
           xmDialogShellWidgetClass, UxTopLevel, XmNallowShellResize, TRUE, 
           XmNdeleteResponse, XmDO_NOTHING, 
           XmNx, x, XmNy, y, 
           XmNdefaultPosition, FALSE, 
           NULL );
  form = XtVaCreateWidget( "form", xmFormWidgetClass, CPcleanupBox,
                        XmNresizePolicy, XmRESIZE_GROW,
                        XmNunitType, XmPIXELS,
                        XmNhorizontalSpacing, 10,
                        XmNverticalSpacing, 10,
                        NULL );
  titleLabel =  XtVaCreateManagedWidget( "titleLabel", xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_CENTER,
          XmNtopAttachment, XmATTACH_FORM,
          XmNleftAttachment, XmATTACH_FORM,
          XmNleftOffset, 20,
          XmNrightAttachment, XmATTACH_FORM,
          XmNrightOffset, 20,
          RES_CONVERT( XmNlabelString, titleMsg ),  
          NULL);
  errLabel =  XtVaCreateManagedWidget( "errLabel", xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_CENTER,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, titleLabel,
          XmNleftAttachment, XmATTACH_FORM,
          XmNleftOffset, 20,
          XmNrightAttachment, XmATTACH_FORM,
          XmNrightOffset, 20,
          RES_CONVERT( XmNlabelString, errorMsg ),  
          NULL);
  sprintf(reason, howMsg, svs->jobId);
  howLabel =  XtVaCreateManagedWidget( "howLabel", xmLabelWidgetClass, form,
          XmNalignment, XmALIGNMENT_BEGINNING,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, errLabel,
          XmNtopOffset, 0,
          XmNleftAttachment, XmATTACH_FORM,
          XmNleftOffset, 20,
          XmNrightAttachment, XmATTACH_FORM,
          XmNrightOffset, 20,
          RES_CONVERT( XmNlabelString, reason ),  
          NULL);

/**** cleanup choice *****/
  cleanup_rc = createButtonBox(form, "Cleanup Options");

  save_tb = XtVaCreateManagedWidget("Save", xmToggleButtonWidgetClass, 
                     cleanup_rc, 
                     XmNset, True,
                     NULL);
  discard_tb = XtVaCreateManagedWidget("Discard", xmToggleButtonWidgetClass, 
                     cleanup_rc, 
                     NULL);

  XtAddCallback(discard_tb, XmNvalueChangedCallback, cbRadio, (XtPointer) svs);
  XtAddCallback(save_tb, XmNvalueChangedCallback, cbRadio, (XtPointer) svs);

/* attach the frame */
  XtVaSetValues(XtParent(cleanup_rc), 
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, howLabel,
      XmNleftAttachment, XmATTACH_FORM,
      NULL);

/**** resubmittal choice *****/
  resubmittal_rc = createButtonBox(form, "Job Resubmittal");

  discard_tb = XtVaCreateManagedWidget("Place on Hold", 
           xmToggleButtonWidgetClass, resubmittal_rc, 
           XmNset, True,
           NULL);
  save_tb = XtVaCreateManagedWidget("Cancel", xmToggleButtonWidgetClass,
                     resubmittal_rc, NULL);

  XtAddCallback(discard_tb, XmNvalueChangedCallback, cbRadio, (XtPointer) svs);
  XtAddCallback(save_tb, XmNvalueChangedCallback, cbRadio, (XtPointer) svs);

/* attach the frame */
  XtVaSetValues(XtParent(resubmittal_rc),
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, howLabel,
      XmNleftAttachment, XmATTACH_WIDGET,
      XmNleftWidget, XtParent(cleanup_rc),
      XmNrightAttachment, XmATTACH_FORM,
      NULL);


  separator = XtVaCreateManagedWidget( "sep", xmSeparatorWidgetClass, form,
          XmNtopAttachment, XmATTACH_WIDGET,
          XmNtopWidget, XtParent(resubmittal_rc), 
                        XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_POSITION,
                        XmNbottomPosition, 80,
                        XmNleftOffset, 2,
                        XmNrightOffset, 2,
                        NULL );
  okButton = XtVaCreateManagedWidget( "okButton",
                        xmPushButtonWidgetClass,
                        form,
                        XmNwidth, 60, XmNheight, 30,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 45, 
                        XmNtopAttachment, XmATTACH_WIDGET,
                        XmNtopWidget, separator,
                        RES_CONVERT( XmNlabelString, "OK" ),
                        NULL );
  XtAddCallback(okButton, XmNactivateCallback, CleanupConfirmCB,  (XtPointer) svs);

  UxPopupInterface(CPcleanupBox, no_grab);

} /* popupCleanupBox */


/*----------------------------------------------------------
 * NAME:
 *  pthread_cancelCWS
 *
 * DESCRIPTION:
 *
 * NOTES:
 *  -- for evaluation
 *
 *---------------------------------------------------------*/
static int pthread_cancelCWS (int thread)
{
    
  flockfile(stdout);
  flockfile(stdin);

  return(pthread_cancel(thread)); 

/*   return(kill((pid_t) thread, KILL_SIGNAL) ); */

  /* return kill((pid_t)thread, SIGHUP); */
}

/*----------------------------------------------------------
 * NAME:
 *  mapStateForDisplay
 *
 * DESCRIPTION:
 *  maps a state to the value displayed 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int mapStateForDisplay(int state)
{
  int disp=-1;

  switch(state) {
    case SUBSYS_DORMANT_ID:
    case SUBSYS_NOT_RUNNING_ID:
      disp = DISP_COL_NOT_RUNNING;
      break;
    case SUBSYS_WAITING_ID:
      disp = DISP_COL_WAITING;
      break;
    case SUBSYS_READY_ID:
      disp = DISP_COL_READY;
      break;
    case SUBSYS_RUNNING_ID:
      disp = DISP_COL_RUNNING;
      break;
    case SUBSYS_DIDNT_START_ID:
      disp = DISP_COL_ERROR; /* errors */
      break;
    default:
      break;
  }

  return(disp);

} /* mapStateForDisplay */

/*----------------------------------------------------------
 * NAME:
 *  updateStateWidth
 *
 * DESCRIPTION:
 *  update the state color of one rc entry to the default color
 *  additionally update other state colors if needed
 *
 *---------------------------------------------------------*/

void updateStateWidth(Widget rc, Dimension wid)
{
  Widget child, w[100];
  char widName[50];
  int row, col, numCols, i=0, numChildren;
  short numRows;
  Dimension newwid, oldwid;

  if (strcmp(XtName(rc), "CPstatusRC") == 0) {
    w[i++] = CPstatusBlank_label;
    w[i++] = CPstatusNotRunning_label;
    w[i++] = CPstatusStarted_label;
    w[i++] = CPstatusReady_label;
    w[i++] = CPstatusRunning_label;
    w[i++] = CPstatusQC_label;
    w[i++] = CPstatusHold_label;
    w[i++] = CPstatusError_label;
  }
  else {
    w[i++] = CPexternBlank_label;
    w[i++] = CPexternNotRunning_label;
    w[i++] = CPexternStarted_label;
    w[i++] = CPexternReady_label;
    w[i++] = CPexternRunning_label;
    w[i++] = CPexternQC_label;
    w[i++] = CPexternHold_label;
    w[i++] = CPexternError_label;
  }
  numChildren=i;

  for (i=0; i < numChildren; i++) {
    XtVaGetValues(w[i], XmNwidth, &oldwid, NULL);
    printf("%s oldwid %d\n", XtName(w[i]), oldwid);
    XtVaSetValues(w[i], XmNwidth, wid, NULL);
    printf("%s wid %d\n", XtName(w[i]), wid);
    XtVaGetValues(w[i], XmNwidth, &newwid, NULL);
    printf("%s newwid %d\n", XtName(w[i]), newwid);
  }


  numCols = XtNumber(states)+1;
  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
printf("\n\n\tupdateStateWidth: rc %s wid %d rows %d cols %d\n", XtName(rc), wid, numRows, numCols);

  for (row=0; row < numRows; row++) {
    for (col=0; col < numCols; col++) {
    sprintf(widName, STATE_FORMAT, states[col], row);
    child = XtNameToWidget(rc, widName);
    if (child == NULL) {
printf("updateStateWidth2: XtNameToWidget returned null child for widget named %s\n", widName);  
      return;
    }
    assert(XmIsLabel(child));
    assert(XtIsManaged(child));
/*printf("updateStateWidth setting %s wid to %d\n", XtName(child), wid); */
    XtVaSetValues(child, XmNwidth, wid, NULL);

    XtVaGetValues(child, XmNwidth, &newwid, NULL);
printf("updateStateWidth did %s wid to %d\n", XtName(child), wid); 

    }
  }

#ifdef OLD
  for (row=0; row < numRows; row++) {
    sprintf(widName, STATE_FORMAT, states[0], row);

    child = XtNameToWidget(rc, widName);
    if (child == NULL) {
printf("updateStateWidth2: XtNameToWidget returned null child for widget named %s\n", widName);  
      continue;
    }
    assert(XmIsLabel(child));
    assert(XtIsManaged(child));
    XtVaSetValues(child, XmNwidth, wid, NULL);
  }
#endif

} /* updateStateWidth */

/*----------------------------------------------------------
 * NAME:
 *  updateStateFont
 *
 * DESCRIPTION:
 *  update the state color of one rc entry to the default color
 *  additionally update other state colors if needed
 *
 *---------------------------------------------------------*/
void updateStateFont(Widget rc, char *fontName, Dimension newWidth)
{
  Widget child;
  char widName[50];
  int row;
  short numRows;
  Dimension wid, totalWid=0;
printf("\n\n\tupdateStateFont: rc %s font %s\n", XtName(rc), fontName);

  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
  for (row=0; row < numRows; row++) {
    sprintf(widName, STATE_FORMAT, states[0], row);

    child = XtNameToWidget(rc, widName);
    if (child == NULL) {
printf("couldn't find child %s\n", widName); fflush(stdout);
      return;
    }
printf("found child %s\n", widName); fflush(stdout);
    XtVaGetValues(child, XmNwidth, &wid, NULL);
    printf("table %s child %s width %d\n", XtName(rc), XtName(child), wid);

    assert(XmIsLabel(child));
    assert(XtIsManaged(child));
    XtVaSetValues(child, RES_CONVERT(XmNfontList, fontName),
                  XmNwidth, newWidth, 
                  NULL);
    totalWid += newWidth;
  }

printf("leaving, table %s child %s width %d\n", XtName(rc), XtName(child), wid);

} /* updateStateFont */



/*----------------------------------------------------------
 * NAME:
 *  updateStateColor
 *
 * DESCRIPTION:
 *  update the state color of one rc entry to the default color
 *  additionally update other state colors if needed
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void updateStateColor(int row, int col, Widget rc)
{
  Widget child;
  char widName[50];
  int i;

  if (col < 0 || row < 0 ) return;

  sprintf(widName, STATE_FORMAT, states[col], row);
#ifdef DEBUG
  if (col != DISP_COL_HOLD && col != DISP_COL_QC)
  printf("updateStateColor row %d col %d name %s color %s len %d\n", 
                        row, col, widName, colors[col], strlen(colors[col])); 
#endif

  child = XtNameToWidget(rc, widName);
  if (child == NULL) {
/*    printf("updateStateColor: XtNameToWidget returned null child for widget named %s\n", widName); */
    return;
  }
  assert(XmIsLabel(child));
  assert(XtIsManaged(child));
  XtVaSetValues(child,
    XtVaTypedArg, XmNbackground, XmRString, colors[col], strlen(colors[col])+1,
    NULL);
               /* reset all but this particular column to default */
               /* and don't reset the error column either, in case it was on */

  if (col >= DISP_COL_WAITING && col < DISP_COL_QC) { /*turn off NOT_RUNNING*/
    sprintf(widName, STATE_FORMAT, states[DISP_COL_NOT_RUNNING], row);
    child = XtNameToWidget(rc, widName);
    XtVaSetValues(child, XtVaTypedArg, XmNbackground, XmRString,
      def_colors[DISP_COL_NOT_RUNNING],
      strlen(def_colors[DISP_COL_NOT_RUNNING])+1, NULL);
  }

  if (col == DISP_COL_NOT_RUNNING || col == DISP_COL_WAITING) { 
    for (i=col+1; i < DISP_COL_MAX-1; i++) {    
      sprintf(widName, STATE_FORMAT, states[i], row);
      child = XtNameToWidget(rc, widName);
      XtVaSetValues(child, XtVaTypedArg, XmNbackground, XmRString,
        def_colors[i],strlen(def_colors[i])+1,NULL);
      }
  }


                                 /* if ready, turn waiting on too */
  if (col == DISP_COL_READY) {   
    sprintf(widName, STATE_FORMAT, states[DISP_COL_WAITING], row);
    child = XtNameToWidget(rc, widName);
    XtVaSetValues(child, XtVaTypedArg, XmNbackground, XmRString, 
      colors[DISP_COL_WAITING],strlen(colors[DISP_COL_WAITING])+1,NULL);
  }


                                 /* reset running color to default */
  if (col == DISP_COL_READY || col == DISP_COL_ERROR) {   
  /*if (col == DISP_COL_READY || col == DISP_COL_ERROR) {   */
/*  if (col == DISP_COL_ERROR) {   */
    sprintf(widName, STATE_FORMAT, states[DISP_COL_RUNNING], row);
    child = XtNameToWidget(rc, widName);
    XtVaSetValues(child, XtVaTypedArg, XmNbackground, XmRString, 
      def_colors[DISP_COL_RUNNING],strlen(def_colors[DISP_COL_RUNNING])+1,NULL);
  }

} /* updateStateColor */

/*----------------------------------------------------------
 * NAME:
 *  resetStateColor
 *
 * DESCRIPTION:
 *  reset the state color of one rc entry to the default color
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void resetStateColor(int row, int col, Widget rc)
{
  Widget child;
  char widName[50];

  if (col < 0 || row < 0) return;

  sprintf(widName, STATE_FORMAT, states[col], row); 
#ifdef DEBUG
  if (col != DISP_COL_QC && col != DISP_COL_HOLD)
    printf("resetStateColor row %d col %d name %s color %s\n", 
                        row, col, widName, colors[col]);   
#endif

  child = XtNameToWidget(rc, widName);
  if (child == NULL) {
/*    printf("resetStateColor: XtNameToWidget returned null child for widget named %s\n", widName); */
    return;
  }
  XtVaSetValues(child,
    XtVaTypedArg, XmNbackground, XmRString, 
                  def_colors[col], strlen(def_colors[col])+1, NULL);
         /* reset all but this particular column to default */
         /* and don't reset the error column either, in case it was on */

} /* resetStateColor */

/*----------------------------------------------------------
 * NAME:
 *  updateSelectedSubsystem
 *
 * DESCRIPTION:
 *  set the toggle button for the specified subsystem and 
 *  clear the toggle buttons for all other subsystems
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void updateSelectedSubsystem(int row, Widget tb)
{
  Widget child, rc = XtParent(tb);
  char widName[50];
  short numRows=0;
  int i;
  Boolean set;

  assert(XmIsRowColumn(rc));

  sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], row);
#ifdef DEBUG
  printf("updateSelectedSubsystem row %d name %s \n", row, widName);
#endif

  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
  for (i=0; i < numRows-1; i++) {  /* 0th-row is columne headings */
    sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], i);
    child = XtNameToWidget(rc, widName);
    if (child == NULL) {
      printf("updateSelectedSubsystem: XtNameToWidget returned null child for widget %s\n", widName);
      return;
    }
    assert(XmIsToggleButton(child));

    XtVaGetValues(child, XmNset, &set, NULL);
    if (row == i) {
#ifdef USE_XPM
      XtVaSetValues(child,XmNbackgroundPixmap,UxConvertPixmap(SUBSYS_XPM),NULL);
#endif
#ifdef USE_COLOR
      reverseColor(child);
#endif
      XmToggleButtonSetState(child, True, True);
    }
    else {
      XtVaSetValues(child, XmNbackgroundPixmap, UxConvertPixmap(""), NULL);
      XmToggleButtonSetState(child, False, True);
    }
  }
} /* updateSelectedSubsystem */

/*----------------------------------------------------------
 * NAME:
 *  getSelectedSubsystem
 *
 * DESCRIPTION:
 *  return the name of the subsystem connected to 
 *  the subsystem toggle button that is currently selected
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char *getSelectedSubsystem()
{
  extern Widget CPstatusRC;
  Widget child;
  short numRows;
  int i;
  char widName[50], *text = NULL;
  XmString xmstr;

  XtVaGetValues(CPstatusRC, XmNnumColumns, &numRows, NULL);
  for (i=0; i < numRows-1; i++) {
    sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], i);
    child = XtNameToWidget(CPstatusRC, widName);
    if (child == NULL) {
      printf("XtNameToWidget 2 returned null child for widget %s\n", widName);
      return (NULL);
    }
    assert(XmIsToggleButton(child));
    if (XmToggleButtonGetState(child)) {
      XtVaGetValues(child, XmNlabelString, &xmstr, NULL);
      XmStringGetLtoR(xmstr , XmFONTLIST_DEFAULT_TAG, &text);
      return(text);  /* get out of loop and return this one */
    }
  }
  return(NULL);

} /* getSelected Subsystem */

/*----------------------------------------------------------
 * NAME:
 *  updateSelectedExternal
 *
 * DESCRIPTION:
 *  set the toggle button for the specified subsystem and 
 *  clear the toggle buttons for all other subsystems
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

void updateSelectedExternal(int row, Widget tb)
{
  Widget child, rc = XtParent(tb);
  char widName[50];
  short numRows=0;
  int i;
  Boolean set;

  assert(XmIsRowColumn(rc));

  sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], row);
#ifdef DEBUG
  printf("updateSelectedExternal row %d name %s \n", row, widName);
#endif

  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
  for (i=0; i < numRows-1; i++) {  /* 0th-row is columne headings */
    sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], i);
    child = XtNameToWidget(rc, widName);
    if (child == NULL) {
      printf("updateSelectedExternal: XtNameToWidget returned null child for widget %s\n", widName);
      return;
    }
    assert(XmIsToggleButton(child));

    XtVaGetValues(child, XmNset, &set, NULL);
    if (row == i) {
#ifdef USE_XPM
      XtVaSetValues(child,XmNbackgroundPixmap,UxConvertPixmap(SUBSYS_XPM),NULL);
#endif
#ifdef USE_COLOR
      reverseColor(child);
#endif
      XmToggleButtonSetState(child, True, True);
    }
    else {
      XtVaSetValues(child, XmNbackgroundPixmap, UxConvertPixmap(""), NULL);
      XmToggleButtonSetState(child, False, True);
    }
  }
} /* updateSelectedExternal */
/*----------------------------------------------------------
 * NAME:
 *  getSelectedExternal
 *
 * DESCRIPTION:
 *  return the name of the subsystem connected to
 *  the subsystem toggle button that is currently selected
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

char *getSelectedExternal()
{
  extern Widget CPstatusRC;
  Widget child;
  short numRows;
  int i;
  char widName[50], *text = NULL;
  XmString xmstr;

  XtVaGetValues(CPexternRC, XmNnumColumns, &numRows, NULL);
  for (i=0; i < numRows-1; i++) {
    sprintf(widName, STATE_FORMAT, states[DISP_COL_NAME], i);
    child = XtNameToWidget(CPexternRC, widName);
    if (child == NULL) {
      printf("XtNameToWidget 2 returned null child for widget %s\n", widName);
      return (NULL);
    }
    assert(XmIsToggleButton(child));
    if (XmToggleButtonGetState(child)) {
      XtVaGetValues(child, XmNlabelString, &xmstr, NULL);
      XmStringGetLtoR(xmstr , XmFONTLIST_DEFAULT_TAG, &text);
      return(text);  /* get out of loop and return this one */
    }
  }
  return(NULL);

} /* getSelectedExternal */

/*----------------------------------------------------------
 * NAME:
 *  setQueueSizeCB
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static XtCallbackProc setQueueSizeCB(Widget w, XtPointer client_data, 
                                     XtPointer cb_arg)
{
  int size;
  char *str;
 
  assert(XmIsTextField(w));
  str = XmTextGetString (w);
  printf ("queue size string: %s\n", str);
  size = atoi(str);

  
  XtFree (str);

} /* setQueueSizeCB */

void resetQueueDisplaySize(Widget w)
{
/*  XtVaSetValues(w, RES_CONVERT(XmNbackground, Q_UNCHANGED_COLOR), NULL); */
  XtSetSensitive(maxQueueSizeReset_pb, FALSE);

  ASFlogMessage(ASF_CP, WP_MAX_JOBS_TEXT_ID, "%d", getPPSqueueSize());
}


/*----------------------------------------------------------
 * NAME:
 *  subsysValueChangedCB
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void subsysValueChangedCB(Widget w, XtPointer client_data, XtPointer cb_arg)
{
  int rowSelected = (int) client_data;
#ifdef DEBUG
  printf("subsysValueChangedCB: rowSelected %d\n", rowSelected);
#endif

  updateSelectedSubsystem(rowSelected, w);  

} /* subsysValueChangedCB */

/*----------------------------------------------------------
 * NAME:
 *  externValueChangedCB
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void externValueChangedCB(Widget w, XtPointer client_data, XtPointer cb_arg)
{
  int rowSelected = (int) client_data;
#ifdef DEBUG
  printf("externValueChangedCB: rowSelected %d\n", rowSelected);
#endif

  updateSelectedExternal(rowSelected, w);

} /* externValueChangedCB */


/*----------------------------------------------------------
 * NAME:
 *  addStatusLine
 *
 * DESCRIPTION:
 *  make the desired list position visible in the specified scrolled list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

int addStatusLine(char *namePtr, Widget parent, Widget *CPstateRCline,
                     XtPointer disarmCB)
{
  static int widest = 0;
  int i=0;
  Dimension totalHeight = 0, ht, wid;
  char thisName[30];
  short rc_lines;
  int numLines;

#ifdef DEBUG
  short cols;
  Dimension h;
#endif

  if (namePtr == NULL)
    return(-1);


  XtVaGetValues(parent, XmNnumColumns, &rc_lines, NULL);
  numLines = rc_lines-1;

#ifdef DEBUG
printf("addStatusLine %s, rc_lines %d\n", namePtr, rc_lines);
#endif

  sprintf(thisName, STATE_FORMAT, states[i], numLines);
  CPstateRCline[0] = XtVaCreateManagedWidget(thisName, 
                                             xmToggleButtonWidgetClass,
      parent, 
      RES_CONVERT( XmNlabelString, namePtr ),
      XmNindicatorType, XmONE_OF_MANY,
      XmNalignment, XmALIGNMENT_BEGINNING, 
      NULL);


  XtAddCallback(CPstateRCline[0], XmNdisarmCallback, 
    (XtCallbackProc) disarmCB, (XtPointer) numLines);

  if (rc_lines == 1) { /* make first toggle button set, others will be unset */
    XmToggleButtonSetState(CPstateRCline[0], True, True);
#ifdef USE_XPM
    XtVaSetValues(CPstateRCline[0], XmNbackgroundPixmap,
                  UxConvertPixmap(SUBSYS_XPM), NULL);
#endif
#ifdef USE_COLOR
    XtVaSetValues(CPstateRCline[0],RES_CONVERT( XmNbackground, "white" ), NULL);
#endif
  }

  for (i=1; i < XtNumber(states); i++)  {  /* adds XtNumber(states) columns */
    sprintf(thisName, STATE_FORMAT, states[i], numLines);
   CPstateRCline[i] = XtVaCreateManagedWidget(thisName, xmLabelWidgetClass, 
     parent,
     XmNsensitive, False, 
     XmNwidth, 200,
     RES_CONVERT( XmNlabelString, "   " ),
     XtVaTypedArg, XmNbackground, XmRString, def_colors[i], 
                               strlen(def_colors[i]) + 1,
     NULL);
    XtVaGetValues(CPstateRCline[i], XmNheight, &ht, NULL);  
    totalHeight += ht;
#ifdef DEBUG
/*printf("added entry %d for %s, color %s\n", i, thisName, def_colors[i]); */
#endif
  }
  numLines++;
#ifdef DEBUG
  XtVaGetValues(parent, XmNnumColumns, &cols, XmNheight, &h, NULL);  
  XtVaGetValues(CPstateRCline[0], XmNheight, &ht, NULL);  
  printf("numCol in RC is %d, numLines %d, height %d, field ht %d total ht %d\n", 
      cols, numLines, h, ht, totalHeight);
  printf("addstatus line: set numcolumns to %d, new ht %d\n", 
         rc_lines+1, totalHeight);
#endif

  XtVaSetValues(parent, 
                XmNnumColumns, rc_lines+1, /* set # rows because*/
                                        /* somehow the RC doesn't do it right*/ 
                XmNheight, totalHeight, /* trying to resize pane! */
                NULL);
  XtVaGetValues(CPstateRCline[0], XmNwidth, &wid, NULL);
  widest = Max(widest, wid);
#ifdef DEBUG
printf("addstatus returning %d\n", widest);
#endif
  return(widest);

} /* addStatusLine */


/*----------------------------------------------------------
 * NAME:
 *  getToggleScanDest
 *
 * DESCRIPTION:
 *  toggles the destination subsystem that will receive
 *  continuous scan jobs
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getToggleScanDest()
{
  int set=0;
  if (XmToggleButtonGadgetGetState(toggleScanDest_RDS))
    set = RDS_CATEGORY_ID;
  else /* ASP set */
    set = ASP_CATEGORY_ID;
  
  return(set);

} /* getToggleScanDest */

/*----------------------------------------------------------
 * NAME:
 *  getToggleQCstart
 *
 * DESCRIPTION:
 *  toggles the flag that controls whether QC is automatically
 *  or manually invoked
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getToggleQCstart()
{
  int set=0;
  if (XmToggleButtonGadgetGetState(toggleQC_automatic))
    set = QC_START_AUTOMATIC;
  else /* ASP set */
    set = QC_START_INTERACTIVE;
 
  return(set);

} /* getToggleQCstart */



/*----------------------------------------------------------
 * NAME:
 *  soundBell 
 *
 * DESCRIPTION:
 *  sound bell to notify operator of serious error conditions
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void soundBell()
{
/* XBell(XtDisplay(UxTopLevel), 10); */
} /* end soundBell........................*/


/*----------------------------------------------------------
 * NAME:
 *  getNameGivenObject
 *
 * DESCRIPTION:
 *  given the name of a subystem (eg RDS_0, ASP_1) return
 *  the display name of the object found in the odl configuration 
 *  file. If the name is not a subsystem, no match will be found 
 *  and a null string is returned
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getNameGivenObject(char *namePtr)
{
   char *name = NULL;

   if (strncmp(RDS_CATEGORY, namePtr, 3) == 0)
      name = getRDSname(getRDSnumGivenName(namePtr));

   if (strncmp(ASP_CATEGORY, namePtr, 3) == 0)
      name = getASPname();

   if (strncmp(PPS_CATEGORY, namePtr, 3) == 0)
      name = getPPSname();

   if (strncmp(IMS_CATEGORY, namePtr, 3) == 0)
      name = getIMSname(IMS_DEFAULT_EXE);


   return(name);

} /* end getNameGivenObject ..................*/

/*----------------------------------------------------------
 * NAME:
 *  getRDSnumGivenName
 *
 * DESCRIPTION:
 *  returns the RDS2 number (0, 1, 2...) given its name
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getRDSnumGivenName(char *namePtr)
{
  int i;

  for (i=0; i < getNumRDSs(); i++)
    if (!strcmp(namePtr, getRDSname(i))) {
      return(i);
    }

  return(-1);
} /* get RDSnumGivenName */


/*----------------------------------------------------------
 * NAME:
 *  getSSP2numGivenName
 *
 * DESCRIPTION:
 *  returns the SSP-2 number (0, 1, 2...) given its name
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getSSP2numGivenName(char *namePtr)
{
  int i;

  for (i=0; i < getNumSSP2s(); i++)
    if (!strcmp(namePtr, getSSP2name(i))) {
      return(i);
    }

  return(-1);
} /* get SSP2numGivenName */

/*----------------------------------------------------------
 * NAME:
 *  getIMSnumGivenName
 *
 * DESCRIPTION:
 *  returns the IMS number given its name
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getIMSnumGivenName(char *namePtr)
{
  int i;

  for (i=0; i < getNumIMSs(); i++)
    if (!strcmp(namePtr, getIMSname(i))) {
      return(i);
    }

  return(-1);
} /* get IMSnumGivenName */




/*----------------------------------------------------------
 * NAME:
 *  getSSPNameGivenObject
 *
 * DESCRIPTION:
 *  given the name of a subystem and index,  return the display 
 *  name of the object found in the odl configuration file. 
 *  If the name is not a subsystem, no match will be found 
 *  and a null string is returned
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getSSPNameGivenObject(char *namePtr, int i)
{
   char *name = NULL;

   if (strncmp(SSP2_CATEGORY, namePtr, 4) == 0)
         name = getSSP2name(i);

   return(name);

} /* end getSSPNameGivenObject ..................*/


/*----------------------------------------------------------
 * NAME:
 *  getIMSNameGivenObject
 *
 * DESCRIPTION:
 *  given the name of a subystem and index,  return the display
 *  name of the object found in the odl configuration file.
 *  If the name is not a subsystem, no match will be found
 *  and a null string is returned
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getIMSNameGivenObject(char *namePtr, int i)
{
   char *name = NULL;

   if (strncmp(IMS_CATEGORY, namePtr, 3) == 0)
         name = getIMSname(i);

   return(name);

} /* end getIMSNameGivenObject ..................*/
/*---------------------------------------------------------*/

 /* getRDSNameGivenObject ..................*/
char * getRDSNameGivenObject(char *namePtr, int i)
{
   char *name = NULL;

   if (strncmp(RDS_CATEGORY, namePtr, 3) == 0)
         name = getRDSname(i);

   return(name);

} /* end getRDSNameGivenObject ..................*/


/*----------------------------------------------------------
 * NAME:
 *  createSysqGivenMstrqElem
 *
 * DESCRIPTION:
 *  given a master queue element, creates the corresponding
 *  subsystem queue element
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static sysQueElemType *createSysqGivenMstrqElem(mstrqListElemType *mqel)
{
  sysQueElemType *sqel ;

  sqel = (sysQueElemType *)doMalloc(sizeof(sysQueElemType));
  if (sqel == NULL)
    return(NULL);
  sqel->reqStrPtr = (char *)doMalloc(strlen(mqel->namePtr) + 1);
  if (sqel->reqStrPtr == NULL) {
    return(NULL);
  }
  else
    strcpy(sqel->reqStrPtr, mqel->namePtr);
  sqel->jobId = mqel->jobId;
  sqel->qcFlag = (mqel->status == Q_M_IMAGE_READY_FOR_QC_ID ||
                  mqel->status == Q_M_IMAGE_PRE_QC_ID ||
                  mqel->status == Q_M_SCAN_READY_FOR_QC_ID) ? 1 : 0;
  sqel->dataReadyFlag = mqel->dataReadyFlag;  
  sqel->repeatFlag = TRUE;  /* assume repeat */
  sqel->checkMediaId = MEDIA_CHECK_YES;  /* assume want to check media id */
  /*sqel->msgType = mqel->msgType;   */

  sqel->sysODLreq  = (ODL) ODLcopy(mqel->mstrqODLreq);
   

  return(sqel);

} /* end createSysqGivenMstrqElem ..................*/


/*----------------------------------------------------------
 * NAME:
 *  GetSubsystemCategoryGivenProcessor
 *
 * DESCRIPTION:
 *  given the name of a subystem (eg RDS_0, ASP_1) return
 *  the category id. If the name is not a subsystem, no match 
 *  will be found and -1 will be returned.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetSubsystemCategoryGivenProcessor(char *namePtr)
{
  int i;
  subsysConfigType *cfg;
  cfg = getCPconfig();

    for (i=0; i < getNumRDSs(); i++) {
        if (strcmp(cfg->RDSlogicalPtr[i], namePtr) == 0)
            return(RDS_CATEGORY_ID);
    }
    if (strcmp(ASP_CATEGORY, namePtr) == 0)
        return(ASP_CATEGORY_ID);

    if (strcmp(cfg->GPRlogicalPtr, namePtr) == 0)  /* return pps category */
        return(PPS_CATEGORY_ID);                   /* for gpr */

    if (strcmp(PPS_CATEGORY, namePtr) == 0)
        return(PPS_CATEGORY_ID);

    for (i=0; i < getNumSSP2s(); i++) {
      if (strcmp(SSP2_CATEGORY, namePtr) == 0 ||
          strcmp(PP_CATEGORY, namePtr) == 0)
        return(SSP2_CATEGORY_ID);
  }

    for (i=0; i < getNumIMSs(); i++) {
      if (strcmp(IMS_CATEGORY, namePtr) == 0)
        return(IMS_CATEGORY_ID);
    }

   /* indicate no match */
   return(-1);

} /* end GetSubsystemCategoryGivenProcessor ..................*/




/*----------------------------------------------------------
 * NAME:
 *  GetSubsystemCategoryGivenName
 *
 * DESCRIPTION:
 *  given the name of a subystem (eg RDS_0, ASP_1) return
 *  the category id. If the name is not a subsystem, no match 
 *  will be found and -1 will be returned.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int GetSubsystemCategoryGivenName(char *namePtr)
{
  int i;
  subsysConfigType *cfg;
  cfg = getCPconfig();

    for (i=0; i < getNumRDSs(); i++) {
      if (strcmp(cfg->RDSlogicalPtr[i], namePtr) == 0)
        return(RDS_CATEGORY_ID);
    }

    if (strcmp(cfg->ASPlogicalPtr, namePtr) == 0)
        return(ASP_CATEGORY_ID);

    if (strcmp(cfg->GPRlogicalPtr, namePtr) == 0)  /* return pps category */
        return(PPS_CATEGORY_ID);                   /* for gpr */

    if (strcmp(cfg->PPSlogicalPtr, namePtr) == 0)
        return(PPS_CATEGORY_ID);

    for (i=0; i < getNumSSP2s(); i++) {
      if (strcmp(cfg->SSP2logicalPtr[i], namePtr) == 0)
        return(SSP2_CATEGORY_ID);
  }

    for (i=0; i < getNumIMSs(); i++) {
      if (strcmp(cfg->IMSlogicalPtr[i], namePtr) == 0)
        return(IMS_CATEGORY_ID);
/*****
      else
printf("GetSubsystemCategoryGivenName, failed comparing %s and %s\n", cfg->IMSlo
gicalPtr[i], namePtr);
******/
    }

   /* indicate no match */
   return(-1);

} /* end GetSubsystemCategoryGivenName ..................*/


/*----------------------------------------------------------
 * NAME:
 *  getColorGivenName
 *
 * DESCRIPTION:
 *  return the SAVE_DIR for a subystem given its name.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getColorGivenName(char *namePtr)
{
  int i;
  subsysConfigType *cfg;
  Pixel bg;
  Colormap cmap;
  XColor color;
  char *colorStr = doMalloc(15);  /* #aaaabbbbcccc is longest string actually */
  cfg = getCPconfig();

  if (strcmp(ASF_CP, namePtr) == 0) {
    XtVaGetValues(UxTopLevel, XmNbackground, &bg, XmNcolormap, &cmap, NULL);
    color.pixel = bg;
    XQueryColor(XtDisplay(UxTopLevel), cmap, &color);
    sprintf(colorStr, "#%04x%04x%04x", color.red, color.green, color.blue);
    return(colorStr);
  }

  for (i=0; i < getNumRDSs(); i++)
  if (strcmp(cfg->RDSlogicalPtr[i], namePtr) == 0)
    return(cfg->RDSbackground[i]);

  if (strcmp(cfg->ASPlogicalPtr, namePtr) == 0)
    return(cfg->ASPbackground);

  for (i=0; i < getNumSSP2s(); i++)
    if (strcmp(cfg->SSP2logicalPtr[i], namePtr) == 0)
      return(cfg->SSP2background[i]);

  if (strcmp(cfg->PPSlogicalPtr, namePtr) == 0)
    return(cfg->PPSbackground);

  for (i=0; i < getNumIMSs(); i++)
    if (strcmp(cfg->IMSlogicalPtr[i], namePtr) == 0)
      return(cfg->IMSbackground[i]);


  /* indicate no match */
  return(NULL);

} /* end getColorGivenName ..................*/



/*----------------------------------------------------------
 * NAME:
 *  getSaveDirGivenName
 *
 * DESCRIPTION:
 *  return the SAVE_DIR for a subystem given its name.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getSaveDirGivenName(char *namePtr)
{
  int i;
  subsysConfigType *cfg;
  cfg = getCPconfig();

    for (i=0; i < getNumRDSs(); i++)
    if (strcmp(cfg->RDSlogicalPtr[i], namePtr) == 0)
        return(getRDSsaveDir(i));

    if (strcmp(cfg->ASPlogicalPtr, namePtr) == 0)
        return(getASPsaveDir());

    for (i=0; i < getNumSSP2s(); i++)
      if (strcmp(cfg->SSP2logicalPtr[i], namePtr) == 0)
        return(getSSP2saveDir(i));

   /* indicate no match */
   return(NULL);

} /* end getSaveDirGivenName ..................*/



/*----------------------------------------------------------
 * NAME:
 *  determineClassFromName
 *
 * DESCRIPTION:
 *  given a name, return the class id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int determineClassFromName(char *namePtr)
{
  if (namePtr == NULL)
    return(-1);


/* need to check PPS and IMS first, because they belong to their */
/* own classes, and if we don't check them first, this routine will */
/* incorrectly return SUBSYS_CLASS_ID */

  if (strcmp(namePtr, getPPSname()) == 0)            /* the PPS app ? */
    return(PPS_CLASS_ID);

/* for ims, use category because we have two CP-IMSs, and both cannot */
/* start with the string "CP-IMS" or the remote-kill script kills both */

  if (GetSubsystemCategoryGivenName(namePtr) == IMS_CATEGORY_ID)  /* IMS ? */
    return(IMS_CLASS_ID);

  if (GetSubsystemCategoryGivenName(namePtr) != -1)   /* see if a subsystem */
    return(SUBSYS_CLASS_ID);

  if (strcmp(namePtr, ASF_CP) == 0)                  /* the cp ?*/
    return(CP_CLASS_ID);

  if (strcmp(namePtr, getGPRname()) == 0)            /* the GPR ?*/
    return(RUTIL_CLASS_ID);

  if (strcmp(namePtr, CP_QC) == 0)                   /* the QC app ? */
    return(QC_CLASS_ID);

  return(-1);                              /* cannot identify */

} /* end determineClassFromName...........................*/

/*----------------------------------------------------------
 * NAME:
 *  getSubsysHostName
 *
 * DESCRIPTION:
 *  given a name, return the host name 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
char * getSubsysHostName(char *namePtr)
{
  int i;
  subsysConfigType *cfg;

  cfg = getCPconfig();

  for (i=0; i < getNumRDSs(); i++)
  if (strcmp(cfg->RDSlogicalPtr[i], namePtr) == 0)
        return(cfg->RDShostPtr[i]);

  if (strcmp(cfg->ASPlogicalPtr, namePtr) == 0)
        return(cfg->ASPhostPtr);

  if (strcmp(cfg->PPSlogicalPtr, namePtr) == 0)
        return(cfg->PPShostPtr);

  for (i=0; i < getNumIMSs(); i++)
    if (strcmp(cfg->IMSlogicalPtr[i], namePtr) == 0)
      return(cfg->IMShostPtr[i]);

  for (i=0; i < getNumSSP2s(); i++)
    if (strcmp(cfg->SSP2logicalPtr[i], namePtr) == 0)
      return(cfg->SSP2hostPtr[i]);

   /* indicate no match */
   return(NULL);


} /* end getSubsysHostName...........................*/


/*----------------------------------------------------------
 * NAME:
 *  logSubsysControlMessage 
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void logSubsysControlMessage(char *namePtr, int  action)
{
 int retval;
 
  retval = GetFirstElemWithStatus_MSTRQLIST(namePtr, Q_M_PROCESSING_ID);
  switch(action) {
   case SUBSYS_STARTED_ID:
    printfLLog(LOG_INFO, SENDING_SHUTDOWN_NOT_STARTED, namePtr);
    break;

   case SUBSYS_STOPPING_ID:
    if (retval == -1) 
     printfLLog(LOG_INFO, SENDING_SHUTDOWN, namePtr );
    else
     printfLLog(LOG_INFO, SENDING_SHUTDOWN_WHILE_PROC, namePtr, retval );
    break;

   case SUBSYS_HALTING_ID:
    if (retval == -1) 
     printfLLog(LOG_INFO, SENDING_HALT, namePtr);
    else
     printfLLog(LOG_INFO,SENDING_HALT_WHILE_PROC, namePtr, retval );
    break;

   case SUBSYS_PAUSING_ID:
    if (retval == -1) 
     printfLLog(LOG_INFO, PAUSING_IDLE_SUBSYS, namePtr);
    else
     printfLLog(LOG_INFO, PAUSING_ACTIVE_SUBSYS, namePtr, retval );
    break;

   default:
    break;
  } /* end switch */

} /* end logSubsysControlMessage.....................................*/


/*-----------------------------------------------------------
 * NAME:
 *  showJobInfo
 *
 * DESCRIPTION:
 *  popup a box that contains detailed job information
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void showJobInfo(int jobId, char *namePtr, ODL odl)
{
  XmString xmstr, xmJobIdstr, xmSubsysStr, xmInputStr, xmOutputStr;
  char *odlStr, jobIdStr[10], subsysStr[10];
#ifdef MULTIPLE_INFO_BOXES
  Widget wid;
  wid = create_CPdetailedInfo(UxTopLevel);
#endif

  odlStr = ODLToStr(odl, NULL) ;
  xmstr = XmStringCreateLtoR(odlStr, XmFONTLIST_DEFAULT_TAG);
  sprintf(jobIdStr, "%d", jobId);
  sprintf(subsysStr, "%s", namePtr);

  xmJobIdstr = XmStringCreateSimple(jobIdStr);
  xmSubsysStr = XmStringCreateSimple(subsysStr);

  xmInputStr = XmStringCreateSimple(GetInputDir_MSTRQLIST(jobId));
  xmOutputStr = XmStringCreateSimple(GetOutputDir_MSTRQLIST(jobId));


  XtVaSetValues(CPdetailedJobId, XmNlabelString, xmJobIdstr, NULL);
  XtVaSetValues(CPdetailedSubsys, XmNlabelString, xmSubsysStr, NULL);
  XtVaSetValues(CPdetailedInputFiles, XmNlabelString, xmInputStr, NULL);
  XtVaSetValues(CPdetailedOutputFiles, XmNlabelString, xmOutputStr, NULL);
  XmTextSetString(CPdetailedOdlText, odlStr);
#ifdef MULTIPLE_INFO_BOXES
  UxPopupInterface(wid, no_grab);
#else
  UxPopupInterface(detailedQueItemInfoBoxWidget, no_grab);
#endif

  doFree(odlStr);
  XmStringFree(xmstr);
  XmStringFree(xmJobIdstr);
  XmStringFree(xmSubsysStr);

} /* showJobInfo */

/*-----------------------------------------------------------
 * NAME:
 *  showHelp
 *
 * DESCRIPTION:
 *  load a text file into the help text widget, and then 
 *  pop up the help interface
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void showHelp(char *help_text, char *version)
{
  char *text = doMalloc(strlen(help_text) + strlen(version) + 2);
  sprintf(text, help_text, version);

  /*XmTextSetString(CPhelpText, help_text); */
  XmTextSetString(CPhelpText, text);

  UxPopupInterface(CPhelp, no_grab);

  doFree(text);


} /* end showHelp..........................*/

/*-----------------------------------------------------------
 * NAME:
 *  showDetailedInfo
 *
 * DESCRIPTION:
 *   callback from doubleclicking a master queue list item
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void showDetailedInfo(Widget mainWid, XtPointer ptr)
{
  mstrqListElemType *mqel;
  XmListCallbackStruct *list;

  list = (XmListCallbackStruct *)  ptr;
  mqel = GetReqGivenListPos_MSTRQLIST(list->item_position);
  if (mqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "master queue element");
   return;
  }
  showJobInfo(mqel->jobId, mqel->namePtr, mqel->mstrqODLreq);


} /* end showDetailedInfo..........................*/

/*-----------------------------------------------------------
 * NAME:
 *  showSubsysDetailedInfo
 *
 * DESCRIPTION:
 *   callback from doubleclicking a subsystem queue list item
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void showSubsysDetailedInfo(Widget subsysWid, XtPointer ptr)
{
  sysQueElemType *sqel;
  XmListCallbackStruct *list;
  char *namePtr;

  namePtr = GetNameGivenMainWid_PIDLIST(subsysWid);
  list = (XmListCallbackStruct *)  ptr;
  sqel = GetReqGivenListPos_SYSQUE(namePtr, list->item_position);
  if (sqel == NULL) {
   printfLLog(LOG_DEBUG, CANNOT_FIND, "subsystem queue element");
   return;
  }
  showJobInfo(sqel->jobId, namePtr, sqel->sysODLreq);

} /* end showSubsysDetailedInfo..........................*/


/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotHealth
 *
 * DESCRIPTION:
 *  notify user that a subsystem did not respond to a heartbeat message
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotHealth(caddr_t namePtr,
              XtIntervalId healthID)
{
  printfLLog(LOG_DEBUG, DID_NOT_HEALTH_ACK, namePtr);
  /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, HEALTH_INTERVAL_ID, 0);

  ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, SUBSYSTEM_DID_NOT_HEALTH_TEXT,
                namePtr); 
  printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_HEALTH_TEXT, namePtr );
} /* end NotifyUserThatProcDidNotHealth...................*/


/*----------------------------------------------------------
 * NAME:
 *  doSendHeartbeat(Widget mainWid)
 *
 * DESCRIPTION:
 *  send the heartbeat message to the correct subsystem
 *  when the user request it
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void doSendHeartbeat(Widget mainWid)
{
 ODL writeOutODL;
 int ok=0, retval;
 int socket;
 char *namePtr, *errmsg;
 XtIntervalId healthID;
 sysQueElemType *sysQueElemPtr; 
 int jobId;
 
  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr ==  NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_PROCNAME_TO, "send heartbeat to\n");
   return;
  }
 
  socket = GetSocketGivenName_PIDLIST(namePtr);
  if (socket < 0) {
   printfLLog(LOG_ERR, CANNOT_GET_PID_TO, "send heartbeat to \n");
   return;
  }
  printfLLog(LOG_DEBUG, SENDING_HEARTBEAT, namePtr, socket);
  jobId = 0;
  sysQueElemPtr = GetFirstActiveElem_SYSQUE(namePtr, DO_COPY); 
  if (sysQueElemPtr != NULL) {
   jobId = sysQueElemPtr->jobId;
   FreeSysQueElement_SYSQUE(namePtr, sysQueElemPtr);
  }
  
  writeOutODL = (ODL) GetNewMsg("SUBSYSTEM_HEARTBEAT");

  if (ODLSetVal(writeOutODL, errmsg = HEARTBEAT_HDR_DEST, namePtr)) 
    if (ODLSetVal(writeOutODL, errmsg = HEARTBEAT_JOB_ID, jobId)) 
      if (ODLSetVal(writeOutODL, errmsg = HEARTBEAT_HDR_SOURCE, ASF_CP)) 
        ok = 1;  /* if got to here all is ok; 
                      otherwise errmsg contains your error string */
  if (!ok) {
   printfLLog(LOG_ERR, CANNOT_SET, errmsg);
   ODLFree(writeOutODL);
   return;
  }


  /* change the process state to indicate that is gathering health */
/* 01/16/97: do we really use this information??? */
/*  ChangeProcessState_PIDLIST(namePtr, SUBSYS_HEALTHING_ID, NOT_SET_ID); */

  retval = WriteMsgToClient(socket, writeOutODL); 
  if (retval < 0) {
    printfLLog(LOG_ERR, CANNOT_SEND_HEARTBEAT);
    ODLFree(writeOutODL);
    return;
  }

  ODLFree(writeOutODL);

  healthID = XtAppAddTimeOut(UxAppContext, 
                      getHealthTimeInterval(), 
                      (XtTimerCallbackProc)NotifyUserThatProcDidNotHealth, 
                      namePtr);

  SetTimeOutID_PIDLIST(namePtr, HEALTH_INTERVAL_ID, healthID);
 
} /* end doSendHeartbeat..................................*/


/*----------------------------------------------------------
 * NAME:
 *  acceptSig_cld 
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static void acceptSig_cld()
{
 int pid, status;
 char *namePtr, *exePtr;

#ifdef SIG_DEBUG
  printfLLog(LOG_DEBUG, ACCEPT_SIG_CLD);
#endif

  while ((pid = waitpid(ANYKID, &status, WNOHANG)) > 0) {
/*     printfLLog(LOG_DEBUG, ACCEPT_STATUS, status);  */
    namePtr = GetNameGivenPid_PIDLIST(pid);
    if (namePtr == NULL) 
      return;
    printfLLog(LOG_DEBUG, "CP detected %s thread has terminated; exit status %d, pid %d\n", namePtr, status, pid);
    exePtr = GetExeNameGivenName_PIDLIST(namePtr);
    if (strcmp(exePtr, "orphan.subsys") == 0)  /* was orphan */
      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID, NOT_SET_ID);

#ifdef SIG_DEBUG
/*    printfLLog(LOG_DEBUG, ACCEPT_STATUS, status, pid);  */
#endif
  } /* end while */

  /* signal(SIGCLD, acceptSig_cld); */

} /* end acceptSig_cld ....................*/


/*----------------------------------------------------------
 * NAME:
 *  doAccept 
 *
 * DESCRIPTION:
 *  asynchronously perfrom the tcp/ip accept function and
 *  assign read/write sockets to processes as they are 
 *  created 
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
#ifdef ACCEPT_SIGNAL
static void myAcceptSigUsr1()
{
printf("myAcceptSigUsr1... exiting thread %d\n", getpid());
exit(0);
}

static void myAcceptSigUsr2()
{
printf("myAcceptSigUsr2... exiting thread %d\n", getpid());
exit(0);
}
#endif

void doAccept()
{
 int tmp;
 int clilen;
 struct sockaddr_in cli_addr, serv_addr;
 tid_t readThread;
 int bindCount = 0;
 int on = 1, listenSocket;
 struct sigaction sigact;
 sigset_t sigset;
 struct linger Linger;	/* struct to tell system how long 
				 * to hang out on the socket after shutdown
				 */
  /* install the sig handler for kill signal  */
#ifdef ACCEPT_SIGNAL
  sigaddset(&sigset, SIGCHLD);
  sigemptyset(&sigset);
  sigact.sa_handler = ( void (* ))myAcceptSigUsr1;
  sigact.sa_mask    =  sigset;
  sigaction(KILL_SIGNAL, &sigact, NULL);
#endif

   /* Set the "linger" option on the socket */
  Linger.l_onoff = 0;          /* no lingering here */
  Linger.l_linger = 0;

  sigfillset(&sigset);
  /* sigemptyset(&sigset); */
  sigact.sa_handler = ( void (* ))acceptSig_cld;
  sigact.sa_mask    =  sigset;
  sigact.sa_flags   =  SA_RESTART;
  sigaction(SIGCHLD, &sigact, NULL);

  if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        printfLLog(LOG_ERR, SOCKET_OPEN_ERROR);

  setsockopt(listenSocket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
  bzero((char *)&serv_addr, sizeof(serv_addr));
  serv_addr.sin_family      = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port        = htons(getPortID());

                               /* attempting to bind socket */
  if (bind(listenSocket,(struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
    for (;;) {
      printfLLog(LOG_DEBUG, ATTEMPT_BIND, bindCount++, getPortID() );
      sleep(2);
      setsockopt(listenSocket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

      bzero((char *)&serv_addr, sizeof(serv_addr));
      serv_addr.sin_family      = AF_INET;
      serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
      serv_addr.sin_port        = htons(getPortID());
      if (bind(listenSocket,
          (struct sockaddr *) &serv_addr, sizeof(serv_addr)) == 0)
               break;
    }
  }
  printfLLog(LOG_DEBUG, SOCKET_BOUND, listenSocket,getPortID());

  /* place the socket into a list for later access 
   * but note that there is no assoceated read thread
   * for the accept thread so set value to 0
   */ 
  AddSocketTo_PIDLIST(ACCEPT_THREAD, listenSocket,0);
  listen(listenSocket, 5);

  for (;;) {
    clilen = sizeof(cli_addr);
    tmp  = accept(listenSocket, (struct sockaddr *) &cli_addr, &clilen);
    if (tmp < 0)
     printfLLog(LOG_ERR, SOCKET_ACCEPT_ERROR, errno ); 
    else {
     if (setsockopt(tmp, SOL_SOCKET, SO_LINGER, &Linger, sizeof(Linger)) < 0)
        printfLLog(LOG_ERR, SOCKET_LINGER_ERROR);

     if (pthread_create(&readThread, 1, (void (*))doRead, tmp) == 0)
       printfLLog(LOG_DEBUG, "Created read thread %d, pid = %d \n", 
                  tmp, readThread );
     else
       printfLLog(LOG_ERR, "Error creating read thread: %s", strerror(errno));
    } /* end if tmp */
  } /* end for */

} /* end doAccept............................ */



/*----------------------------------------------------------
 * NAME:
 *  doSpawn
 *
 * DESCRIPTION:
 *  spawn any process and return the pid of that process on
 *  successful completion
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static int doSpawn(char *procNamePtr, char *exeNamePtr, char *exePathNamePtr,
                   char **argp)
{
 int pid, retval;
 int i = 0;


  if (!fileExists(exePathNamePtr)) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, ERROR_SPAWNING,
                procNamePtr, exePathNamePtr); 
    printfLLog(LOG_DEBUG, FILE_NOT_EXIST, exePathNamePtr);
    return(-1);
  }

  printfLLog(LOG_DEBUG, SPAWNING, exePathNamePtr);
  pid = fork();
  if (pid == 0) {             /* this is the child */
    printfLLog(LOG_DEBUG, CHILD_SPAWNED, exePathNamePtr);
    retval = execv(exePathNamePtr, argp);
    printfLLog(LOG_ERR, EXEC_ERROR, exePathNamePtr,  strerror(errno));
    /* return(-1); */
    exit(-1);
  } /* end successful fork */

  if (pid < 0) {
    printfLLog(LOG_ERR, CANNOT_SPAWN, procNamePtr, strerror(errno));
    return(-1);
  } else{             /* this is the parent */
    printfLLog(LOG_DEBUG, LAUNCHED, exePathNamePtr, pid);
    while (argp[i] != NULL)
       printfLLog(LOG_DEBUG, LAUNCH_ARGS, i, argp[i++]);

    printfLLog(LOG_DEBUG,LAUNCHED_TEXT, procNamePtr);
    return(pid);
  }

} /* doSpawn .............................*/

/*----------------------------------------------------------
 * NAME:
 *  doRemoteSpawn
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int doRemoteSpawn(char *host, char *procNamePtr, char *exeNamePtr,
                   char *exePathNamePtr, char **argp)
{
 int i, j, ppid;
 char *remoteArgp[50];
 char *exeProgNamePtr, *exeProgPathNamePtr;

  bzero(remoteArgp, sizeof(remoteArgp) );
  exeProgNamePtr     = doMalloc(strlen(RSH_PROG_NAME) + 1);
  exeProgPathNamePtr = doMalloc(strlen(RSH_PROG_PATH_NAME) + 1);
  strcpy(exeProgNamePtr, RSH_PROG_NAME);
  strcpy(exeProgPathNamePtr, RSH_PROG_PATH_NAME);
  j = 0;
  remoteArgp[j++] = exeProgNamePtr;
  remoteArgp[j++] = "-n";
  remoteArgp[j++] = host;
  remoteArgp[j++] = exePathNamePtr;

  /* skip the standard argp[0] == to progname */
  i = 1;
  while (remoteArgp[i] != NULL)
    remoteArgp[j++] = argp[i++];

  ppid = doSpawn(procNamePtr, exeProgNamePtr, exeProgPathNamePtr, remoteArgp);
  return(ppid);
} /* end doRemoteSpawn ................*/

/*----------------------------------------------------------
 * NAME:
 *  doRemoteKill
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int doRemoteKill(char *namePtr)
{
  char killCmdBuf[256];
  subsysConfigType  *subsysConfigPtr;

  subsysConfigPtr  = getCPconfig();
  if (namePtr == NULL || subsysConfigPtr == NULL) {
   ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, CANT_PERFORM_REMOTE_KILL_TEXT);
   return (0);
  }
  sprintf(killCmdBuf, "%s/remote-kill %s %s", subsysConfigPtr->CPscriptsDirPtr,
          getSubsysHostName(namePtr), namePtr);

  printfLLog(LOG_DEBUG, REMOTE_KILL_CMD_TEXT, namePtr, killCmdBuf);
  system(killCmdBuf);

  return (1);

} /* end doRemoteKill ................*/

/*----------------------------------------------------------
 * NAME:
 *  StopSubsystemCB 
 *
 * DESCRIPTION:
 *  invoked from the OkQuestionCB callback this routine
 *  finds the socket associated with the selected process,
 *  passed in through call_data, then invokes the routine to send 
 *  the stop
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void StopSubsystemCB(Widget w, XtPointer client_data, 
                            XtPointer call_data)
{
 int   assocThread, sockfd;
 char  *namePtr, *exeNamePtr;

  namePtr = (char *)call_data;
/*  printfLLog(LOG_DEBUG, STOPPING_SUBSYS, namePtr); */

  /* get the socket  to send to */
  sockfd = GetSocketGivenName_PIDLIST(namePtr);
  if (sockfd == -1) {
    printfLLog(LOG_ERR, CANNOT_FIND_SOCKET, namePtr, sockfd);
    return;
  }
  exeNamePtr = GetExeNameGivenName_PIDLIST(namePtr);
  if (exeNamePtr == NULL)
    return;

  if (strcmp(exeNamePtr, "orphan.subsys") == 0) { /* if orphan */
    if (doRemoteKill(namePtr)) {
#ifdef COMPLETELY_KILL /* new: thread isn't getting destroyed for orphan */
                       /* but this code doesn't quite work yet... */
      if (sockfd != 0) {         /* close the socket */
printfLLog(LOG_DEBUG, "cprtns: StopSubsystemCB: CLOSING_SOCKET");

          printfLLog(LOG_DEBUG, CLOSING_SOCKET, namePtr, sockfd);
          close(sockfd);
      }
      if ((assocThread = GetAssocThreadGivenName_PIDLIST(namePtr)) != 0) {
          printfLLog(LOG_DEBUG, "Cancelling %s thread %d",namePtr,assocThread);
          pthread_cancel(assocThread);
      }
#endif


      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID, 
                               NORMAL_TERMINATION_ID);
      }
  }
  else
    SendStopMsg(namePtr, sockfd, DO_TIMEOUT);
  
  /* NOTE: process removed from list and associated read socket
   * closed in the workproc 
   */

} /* end StopSubsystemCB ......................*/

/*----------------------------------------------------------
 * NAME:
 *   doStopAll 
 *
 * DESCRIPTION:
 *  stops all the process and their associated read
 *  threads when the user selects the "ExitAll" or "StopALL"
 *  from the menu bar. Stops subsystems through use of the
 *  subsystem stop message, stops the request utility
 *  by issuing a sig term.
 *
 * NOTES:
 * -- since stopping everything is more
 *  complicated than a single process
 *  it has been relegated to its own 
 *  routine.
 *
 * -- returns the pid of the accept thread to the 
 *  calling process since that process may not need to be 
 *  stopped, eg "StopAll".
 *
 * -- the sig term causes the QC program and
 *  the request utility to issue an exit(0) and results
 *  in a graceful shutdown rather than exiting with an
 *  error condition status.
 *
 * -- when states are stable, put in more specific code
 *  to stop the process, until then stop anything that
 *  has a resonable pid and close any sockets associated 
 * 
 * -- the following code was used to stop the read thread
 *  but removed it
 *     if (pidListElemPtr->assocThread != 0)
 *   {
 *      retval = kill(pidListElemPtr->assocThread, SIGKILL);
 *      if (retval != 0)
 *        printfLLog(LOG_ERR,"could not kill %s associated thread %d\n", 
 *                                    pidListElemPtr->procNamePtr,
 *                                    pidListElemPtr->assocThread);
 *     }
 *   see note for handleProcessTermination for details
 *---------------------------------------------------------*/
static int  doStopAll()
{
 int retval, acceptThread = 0;
 pidListElemType *pidEl;
 pidListElemType *realPidEl;
 int i=0;

  printfLLog(LOG_DEBUG, STOP_ALL_MSG);

         /* remove the head of the pid list ,ie until none left */
  i=0;
  while ((pidEl = GetCopyOfElemIn_PIDLIST(i++) ) != NULL) {
   printPidEl(pidEl);

/*    printfLLog(LOG_DEBUG, "Stop pid %d?? Name %s State %d", 
      pidEl->ppid, pidEl->procNamePtr, pidEl->state); */
    if (pidEl->state == SUBSYS_DORMANT_ID) 
         continue;

    printfLLog(LOG_DEBUG, ATTEMPT_STOP, pidEl->procNamePtr, 
                pidEl->ppid, pidEl->socket);

    switch(pidEl->classID) {
      case THREAD_CLASS_ID:
        printfLLog(LOG_DEBUG, CAPTURE_ACCEPT_THREAD);
        acceptThread = pidEl->ppid;
        break;

      case SUBSYS_CLASS_ID:
      case PPS_CLASS_ID:
      case IMS_CLASS_ID:
      /* 
       * NOTE: when states are stable, put in more specific code
       */
        if (pidEl->socket != 0) {
          logSubsysControlMessage(pidEl->procNamePtr,
                               SUBSYS_STOPPING_ID);
          realPidEl = GetElemGivenName_PIDLIST(pidEl->procNamePtr, DO_GET_REAL);
          SendStopMsg(realPidEl->procNamePtr, realPidEl->socket,
                   DO_TIMEOUT); 
        }
        else {
          logSubsysControlMessage(pidEl->procNamePtr,
                               SUBSYS_STARTED_ID);
          if (doRemoteKill(pidEl->procNamePtr))
            ChangeProcessState_PIDLIST(pidEl->procNamePtr, 
              SUBSYS_NOT_RUNNING_ID, NORMAL_TERMINATION_ID);

        }
          
#ifdef DONT_CLOSE /* is this what we added recently??? */
        if (pidEl->socket != 0) {         /* close the socket */
printfLLog(LOG_DEBUG, "cprtns: doStopAll 1: CLOSING_SOCKET");
          printfLLog(LOG_DEBUG, CLOSING_SOCKET,
	    pidEl->procNamePtr, pidEl->socket);
          close(pidEl->socket);
        }
#endif /* */
        break;

      case RUTIL_CLASS_ID: 
        printfLLog(LOG_INFO, SENDING_SHUTDOWN,
                            pidEl->procNamePtr );
      /* 
       * stop the request utility with a sigterm -- this 
       * allows the utility to catch the signal, do any
       * clean up and return exit with a 0
       */
        retval = kill(pidEl->ppid, SIGTERM);
        if (retval != 0)
           printfLLog(LOG_DEBUG, CANNOT_KILL_PID, pidEl->procNamePtr,
                                     pidEl->ppid);
      /* close the socket */
        if (pidEl->socket != 0) {
printfLLog(LOG_DEBUG, "cprtns: doStopAll 2: CLOSING_SOCKET");

          printfLLog(LOG_DEBUG, CLOSING_SOCKET,
	     pidEl->procNamePtr, pidEl->socket);
          close(pidEl->socket);
        }
        break;

      case CP_CLASS_ID:
        break;

      case QC_CLASS_ID: 
      case LOG_BROWSER_CLASS_ID: 
        printfLLog(LOG_INFO, SENDING_SHUTDOWN,
                            pidEl->procNamePtr );
        retval = kill(pidEl->ppid, SIGTERM);
        if (retval != 0)
          printfLLog(LOG_DEBUG, CANNOT_KILL_PID, pidEl->procNamePtr, 
                                pidEl->ppid);
        break;

      default:
        break;
    } /* end switch */

    /* free the element */
    FreePidListElem_PIDLIST(pidEl);

  } /* end while */

/************
  i=0;
  while ((pidEl = GetCopyOfElemIn_PIDLIST(i++) ) != NULL) {
    if ((pidEl->classID == PPS_CLASS_ID || pidEl->classID == IMS_CLASS_ID) && 
        pidEl-> ppid != 0 || 
        pidEl->state == SUBSYS_STOPPING_ID ) {
    printfLLog(LOG_DEBUG, "Name %s pid %d State %d: stopping... ",
      pidEl->procNamePtr, pidEl->ppid, pidEl->state);
      sleep(3);
    }
  }
***********/


  if (acceptThread != 0)  /* we are exiting the CP */
    GLOBAL_exitingCP = TRUE;

  return(acceptThread);
} /* end doStopAll.........................*/





/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotStart
 *
 * DESCRIPTION:
 *  notify user that a subsystem did not return a 'subsystem ready'
 *  message after it was started
 *
 * NOTES:
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotStart(caddr_t namePtr, XtIntervalId startID)
{
  int sockfd;

  /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, START_INTERVAL_ID, 0);

  /* try to stop the process */
  sockfd = GetSocketGivenName_PIDLIST(namePtr);
  if (sockfd != -1)
    SendStopMsg(namePtr, sockfd, DO_TIMEOUT);

  ChangeProcessState_PIDLIST(namePtr, SUBSYS_DIDNT_START_ID, 
                             NORMAL_TERMINATION_ID);

  ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, SUBSYSTEM_DID_NOT_START_TEXT,
                namePtr); 

  printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_START_TEXT, namePtr);
} /* end NotifyUserThatProcDidNotStart...................*/


/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotStop
 *
 * DESCRIPTION:
 *  notify user that the CP did not receive a SIGCLD signal
 *  for a subsystem that was sent a 'subsystem stop'
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotStop(char * namePtr, XtIntervalId stopID)
{
  /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, STOP_INTERVAL_ID, 0);

  printfLLog(LOG_ERR, SUBSYSTEM_DID_NOT_STOP_TEXT, namePtr);

  if (doRemoteKill(namePtr)) {
    ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);

    printfLLog(LOG_ERR, PERFORMED_REMOTE_KILL_TEXT, namePtr);
  }

} /* end NotifyUserThatProcDidNotStop...................*/


/*----------------------------------------------------------
 * NAME:
 *  NotifyUserThatProcDidNotReset
 *
 * DESCRIPTION:
 *  notify user that a subsystem did not return a 'subsystem ready'
 *  message after it was reset
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void NotifyUserThatProcDidNotReset(caddr_t namePtr, XtIntervalId haltID)
{
  int state;

  /* set the interval id to 0 */
  SetTimeOutID_PIDLIST(namePtr, HALT_INTERVAL_ID, 0);

  ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, SUBSYSTEM_DID_NOT_HALT_TEXT,
                namePtr); 

  state = GetProcessState_PIDLIST(namePtr);
  if (state == SUBSYS_HALTING_ID || state == SUBSYS_HEALTHING_ID ||
      state == SUBSYS_STOPPING_ID || state == SUBSYS_PAUSING_ID)
    ChangeProcessState_PIDLIST(namePtr, SUBSYS_HUNG_ID, NORMAL_TERMINATION_ID);

  printfLLog(LOG_ERR,SUBSYSTEM_DID_NOT_HALT_TEXT, namePtr );

} /* end NotifyUserThatProcDidNotReset...................*/

/*----------------------------------------------------------
 * NAME:
 *  SendResetMsg 
 *
 * DESCRIPTION:
 *  send the halt message out to the subsystem denoted by
 *  the name "namePtr" -- called after the user has confimed
 *  that he wants to halt a subsytem from the Subsystem Control
 *  panel as well as the "Reset All" menu item
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static int SendResetMsg(char *namePtr, int socket)
{
 ODL          writeOutODL;
 int          retval, ok=0;
 XtIntervalId haltID;
 char *errmsg;

  /* change the process state to indicate that it is halting */
  retval = ChangeProcessState_PIDLIST(namePtr, SUBSYS_HALTING_ID, NOT_SET_ID);
  if (retval == -1) {
    printfLLog(LOG_ERR, CANNOT_CHANGE_TO_HALTED, namePtr);
    return(-1);
  }
                                     /* get the halt message to send */
  writeOutODL = (ODL) GetNewMsg("SUBSYSTEM_HALT");
  if (writeOutODL == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, "SUBSYSTEM_HALT");
   return(-1);
  }
                                    /* set up the source and destination */
  ok = 0;
  if (ODLSetVal(writeOutODL, errmsg = HALT_HDR_DEST, namePtr)) 
    if (ODLSetVal(writeOutODL, errmsg = HALT_HDR_SOURCE, ASF_CP)) 
      ok = 1;

  if (!ok) {
    ODLFree(writeOutODL);
    printfLLog(LOG_ERR, ERROR_SETTING_MSG);
    printfLLog(LOG_DEBUG, DEB_ERROR_SETTING_MSG, "Subsystem Reset", errmsg);
    return(1);
  }

  printfLLog(LOG_DEBUG, SENDING_HALT_SOCKET, namePtr, socket);
  WriteMsgToClient(socket,writeOutODL); /* do the actual write */

  ODLFree(writeOutODL);
  haltID = XtAppAddTimeOut(UxAppContext, getResetTimeInterval(namePtr), 
              (XtTimerCallbackProc)NotifyUserThatProcDidNotReset, namePtr);
  SetTimeOutID_PIDLIST(namePtr, HALT_INTERVAL_ID, haltID);
  return(0);
} /* end SendResetMsg ...................*/

/*----------------------------------------------------------
 * NAME:
 *  resetSubsysByName
 *
 * DESCRIPTION:
 *  given a subsystem name, send the subsystem a Reset message
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void resetSubsysByName(char *namePtr)
{
  int sockfd;

  sockfd = GetSocketGivenName_PIDLIST(namePtr);
  if (sockfd == -1) {
    printfLLog(LOG_ERR, CANNOT_FIND_SOCKET, namePtr, sockfd);
    return;
  }

/*  do we really want to DO the reset, or just make the CP reset its
    internal state so the CP doesn't try to send new messages to the subsys?
  SendResetMsg(namePtr, sockfd);
  printfLLog(LOG_DEBUG, SUBSYS_RESET_TEXT, namePtr );
*/

  ChangeProcessState_PIDLIST(namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);

} /* resetSubsysByName */




/*----------------------------------------------------------
 * NAME:
 *  isHostRemote
 *
 * DESCRIPTION:
 *  determine if host is on a remote machine 
 * NOTES:
 *
 *---------------------------------------------------------*/
static int isHostRemote(char *hostNamePtr)
{
 char hostNameBuf[MAXHOSTNAMELEN];
 int  retval;

  retval = gethostname(hostNameBuf, sizeof(hostNameBuf)-1 );
  if (retval == -1)
    return(-1);

  if (strcmp(hostNamePtr, hostNameBuf) == 0)
      return(FALSE);

  return(TRUE);
  
} /* end isHostRemote....................................*/


/*----------------------------------------------------------
 * NAME:
 *  spawnLocalSubsystem 
 *
 * DESCRIPTION:
 *  setup the parameters needed to spawn a subsystem, eg 
 *  RDS|SSP, using doSpawn
 *
 * NOTES:
 * special code for testing example: 
 *  if (strcmp(namePtr, "SSP_1") == 0)
 *  {
 *   strcpy(pathBuf, "/u/agate/local/sps/dev/ssp/");
 *   strcpy(remoteMachBuf, "ssp1");
 *   remoteFlag = TRUE;
 *  }
 *---------------------------------------------------------*/
static int spawnLocalSubsystem(char *namePtr,  Widget buttonWid)
{
 char *argp[50];
 int  i= 0, ppid, haltTime, stopTime, remoteFlag = FALSE, cat;
 XtIntervalId  startID;
 subsysConfigType *subsysConfigPtr;
 char *exeNamePtr, *pathPtr, *hostPtr, *fullPathPtr, *templatePtr,
      *configFilePtr, *loglocPtr;

 bzero(argp, sizeof(argp) );

 /* get the exe name , path name etc for the process */
 subsysConfigPtr  = getCPconfig();

 /* map the name to an executable name with path */
 switch(cat = GetSubsystemCategoryGivenName(namePtr)) {
  case RDS_CATEGORY_ID:
   exeNamePtr  = subsysConfigPtr->RDSexeNamePtr[getRDSnumGivenName(namePtr)];
   pathPtr     = subsysConfigPtr->RDSpathPtr[getRDSnumGivenName(namePtr)];
   hostPtr     = subsysConfigPtr->RDShostPtr[getRDSnumGivenName(namePtr)];
   templatePtr = subsysConfigPtr->RDStemplatePtr[getRDSnumGivenName(namePtr)];
   configFilePtr = subsysConfigPtr->RDSconfigFilePtr[getRDSnumGivenName(namePtr)];
   haltTime = subsysConfigPtr->RDShaltTimeInterval[getRDSnumGivenName(namePtr)];
   stopTime = subsysConfigPtr->RDSstopTimeInterval[getRDSnumGivenName(namePtr)];
   break;

  case SSP2_CATEGORY_ID:
   exeNamePtr  = subsysConfigPtr->SSP2exeNamePtr[getSSP2numGivenName(namePtr)];
   pathPtr     = subsysConfigPtr->SSP2pathPtr[getSSP2numGivenName(namePtr)];
   hostPtr     = subsysConfigPtr->SSP2hostPtr[getSSP2numGivenName(namePtr)];
   templatePtr = subsysConfigPtr->SSP2templatePtr[getSSP2numGivenName(namePtr)];
   configFilePtr = subsysConfigPtr->SSP2configFilePtr[getSSP2numGivenName(namePtr)];
/*
printf("SSP2 exe %s path %s host %s temp %s cfg %s\n", exeNamePtr , pathPtr,
hostPtr, templatePtr, configFilePtr); 
*/
   break; 

  case ASP_CATEGORY_ID:
   exeNamePtr  = subsysConfigPtr->ASPexeNamePtr;
   pathPtr     = subsysConfigPtr->ASPpathPtr;
   hostPtr     = subsysConfigPtr->ASPhostPtr;
   templatePtr = subsysConfigPtr->ASPtemplatePtr;
   configFilePtr = subsysConfigPtr->ASPconfigFilePtr;
   break;

  case PPS_CATEGORY_ID:
   exeNamePtr  = subsysConfigPtr->PPSexeNamePtr;
   pathPtr     = subsysConfigPtr->PPSpathPtr;
   hostPtr     = subsysConfigPtr->PPShostPtr;
   templatePtr = subsysConfigPtr->PPStemplatePtr;
   configFilePtr = subsysConfigPtr->PPSconfigFilePtr;
   break;

  case IMS_CATEGORY_ID:

   exeNamePtr  = subsysConfigPtr->IMSexeNamePtr[getIMSnumGivenName(namePtr)];
   pathPtr     = subsysConfigPtr->IMSpathPtr[getIMSnumGivenName(namePtr)];
   hostPtr     = subsysConfigPtr->IMShostPtr[getIMSnumGivenName(namePtr)];
   templatePtr = subsysConfigPtr->IMStemplatePtr[getIMSnumGivenName(namePtr)];
   configFilePtr = subsysConfigPtr->IMSconfigFilePtr[getIMSnumGivenName(namePtr)];

   break;

  default:
   printfLLog(LOG_ERR, INVALID_SUBSYS_CATEGORY, namePtr, cat); 
   return(-1);
   break;
 } /* end switch */

 remoteFlag = isHostRemote(hostPtr);
 if (remoteFlag == -1) {
  printfLLog(LOG_ERR, CANNOT_DETERMINE,  "host");
  return(-1);
 }
/* nancy: force remote flag so we always use rsh */
 remoteFlag = TRUE;

 fullPathPtr = doMalloc(strlen(pathPtr) + strlen("/") + strlen(exeNamePtr) + 3);

 strcpy(fullPathPtr, pathPtr);
 strcat(fullPathPtr, "/");
 strcat(fullPathPtr, exeNamePtr);
 printfLLog(LOG_INFO, STARTING_SUBSYS, namePtr );

 /* 
  * NOTE: -asfp indicates the tcp/ip port 
  *       -asfn indicates the program invocation name 
  */ 
 if (remoteFlag == FALSE) {
  argp[i++] = exeNamePtr;
  argp[i++] = "-asfn";
  argp[i++] = namePtr;
  argp[i++] = ASF_CP_INET_ARGFLG;
  argp[i++] = get_subnet_inetPtr(namePtr);
  argp[i++] = "-asfp";
  argp[i++] = getPortIDString();
  if (configFilePtr != NULL) {
   argp[i++] = "-configfile";
   argp[i++] = configFilePtr;
  }
  if (templatePtr != NULL) {
   argp[i++] = "-config";
   argp[i++] = templatePtr;
  }
  if ( (loglocPtr = (char *)getLogLocation()) != NULL) {
   argp[i++] = "-asflogloc";
   argp[i++] = loglocPtr;
  }
  argp[i++] = NULL;
  ppid = doSpawn(namePtr, exeNamePtr, fullPathPtr, argp);

 } else{
  argp[i++] = hostPtr;
  argp[i++] = exeNamePtr;
  argp[i++] = "-asfn";
  argp[i++] = namePtr;
  argp[i++] = ASF_CP_INET_ARGFLG;
  argp[i++] = get_subnet_inetPtr(namePtr);
  argp[i++] = "-asfp";
  argp[i++] = getPortIDString();
  if (configFilePtr != NULL) {
   argp[i++] = "-configfile";
   argp[i++] = configFilePtr;
  }
  if (templatePtr != NULL) {
   argp[i++] = "-config";
   argp[i++] = templatePtr;
  }
  if ( (loglocPtr = (char *)getLogLocation()) != NULL) {
   argp[i++] = "-asflogloc";
   argp[i++] = loglocPtr;
  }
  argp[i++] = NULL;

  ppid = doRemoteSpawn(hostPtr, namePtr, exeNamePtr, fullPathPtr, argp);
 }

 doFree(fullPathPtr);

 if (ppid > 0) {
  SetPidGivenName_PIDLIST(namePtr, ppid);
  startID = XtAppAddTimeOut(UxAppContext, 
                      getStartTimeInterval(), 
                      (XtTimerCallbackProc)NotifyUserThatProcDidNotStart, 
                      namePtr);

  SetTimeOutID_PIDLIST(namePtr, START_INTERVAL_ID, startID);

  ASFlogMessage_direct(namePtr, QUE_HEARTBEAT_LABEL_ID,
                SUBSYSTEM_HEARTBEAT_LABEL_TEXT);

/*  ASFlogMessage_direct(namePtr, QUE_INFO_LABEL_1_ID,
                SUBSYSTEM_RUNNING_ON_HOST_TEXT, namePtr, hostPtr);
*/
 }

 return(ppid);

} /* end spawnLocalSubsystem ....................*/


/*----------------------------------------------------------
 * NAME:
 *  spawnLogBrowser
 *
 * DESCRIPTION:
 *  launch the log browser
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void spawnLogBrowser(char *logLevel, Widget mainWid)
{
 int i=0, ppid, class;
 char *argp[50], *exeFileName, *namePtr, *colorPtr;
 char  iconPtr[256], titlePtr[256], titleXrmPtr[256];
 subsysConfigType *getCPconfigPtr;

  namePtr = GetNameGivenMainWid_PIDLIST(mainWid);
  if (namePtr == NULL) /* is the CP window, not a subsystem */
      namePtr = ASF_CP;

  sprintf(titlePtr, "%s_Log_Browser", namePtr);
  sprintf(iconPtr, "log_browser.iconName: %s", titlePtr);
  sprintf(titleXrmPtr, "*.title: %s", titlePtr);

  getCPconfigPtr = getCPconfig();
  if (getCPconfigPtr == NULL)
    return;

  bzero(argp, sizeof(argp) );
  exeFileName = convertEnvFile(getCPconfigPtr->logBrowserCmd);

  argp[i++] = exeFileName;

  argp[i++] = "-first";
  argp[i++] = "-log";

  class = GetClassGivenName_PIDLIST(namePtr);

  if (class == IMS_CLASS_ID || class == PPS_CLASS_ID ||
     (class == CP_CLASS_ID && XtParent(mainWid) == CP_logBrowser_pane) ) {
    if (strcmp(logLevel, "info") == 0)
      argp[i++] =  getCPconfigPtr->cp_info_logfile;
    else if (strcmp(logLevel, "error") == 0)
      argp[i++] =  getCPconfigPtr->cp_err_logfile;
    else
      argp[i++] =  getCPconfigPtr->cp_debug_logfile;
  }
  else if (class == SUBSYS_CLASS_ID ||
          (class == CP_CLASS_ID && XtParent(mainWid) == SPS_logBrowser_pane) ) {
    if (strcmp(logLevel, "info") == 0)
      argp[i++] =  getCPconfigPtr->sps_info_logfile;
    else if (strcmp(logLevel, "error") == 0)
      argp[i++] =  getCPconfigPtr->sps_err_logfile;
    else
      argp[i++] =  getCPconfigPtr->sps_debug_logfile;
  }

  if (class != CP_CLASS_ID) {     /* for all but the main window, set filter */
    argp[i++] =  "-filter";
    if (class == IMS_CLASS_ID)
      argp[i++] =  getCPconfigPtr->IMSlogicalPtr[getIMSnumGivenName(namePtr)];
    else if (class == PPS_CLASS_ID) 
      argp[i++] =  getCPconfigPtr->PPSlogicalPtr;
    else
      argp[i++] =  namePtr; 

  }
  if ((colorPtr = getColorGivenName(namePtr)) != NULL) {
    argp[i++] = "-bg";
    argp[i++] = colorPtr;
  }

  argp[i++] = "-xrm";                 /* set icon name */
  argp[i++] = iconPtr;


#ifdef LOGB_DEBUG
  argp[i++] =  "-filter";
  argp[i++] =  "CP_nancy";
#endif

  argp[i++] = NULL;

  ppid = doSpawn(ASF_LOG_BROWSER, "log_browser", exeFileName, argp);

 /* if spawned successfuly, create the entry */
 if (ppid > 0)
   AddPidToList_PIDLIST(LOG_BROWSER_CLASS_ID, ASF_LOG_BROWSER,
        exeFileName, "logfile", NOT_SET_ID, ppid);

} /* spawnLogBrowser */

/*----------------------------------------------------------
 * NAME:
 *  spawnRequestUtil 
 *
 * DESCRIPTION:
 *  launch the product request utility
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void spawnRequestUtil()
{
 char *argp[20];
 int  i= 0, ppid, retval;
 char *loglocPtr;
 char fullPathPtr[256];
 subsysConfigType *subsysConfigPtr;

 retval =  GetNumInstances_PIDLIST(getGPRname());
 if (retval >= getNumGPRs()) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, MAX_RUNNING, getGPRname());
   return;
  }

 bzero(argp, sizeof(argp) );
 /* get the exe name , path name etc for the process */
 subsysConfigPtr  = getCPconfig();

 sprintf(fullPathPtr, "%s/%s", 
         subsysConfigPtr->GPRpathPtr, subsysConfigPtr->GPRexeNamePtr);

 printfLLog(LOG_INFO, STARTING_GPR);

 argp[i++] = subsysConfigPtr->GPRexeNamePtr;
 argp[i++] = "-asfn";
 argp[i++] = getGPRname();
 argp[i++] = ASF_CP_INET_ARGFLG;
 argp[i++] = (char *)get_CP_inetPtr();
 argp[i++] = "-asfp";
 argp[i++] = getPortIDString();
 argp[i++] = "-configfile";
 argp[i++] = subsysConfigPtr->GPRconfigFilePtr;
/*****
 argp[i++] = GPR_DATA_DIR_ARGFLG;
 argp[i++] = subsysConfigPtr->GPRdataPathPtr;
 argp[i++] = GPR_SAVRST_DIR_ARGFLG;
 argp[i++] = subsysConfigPtr->GPRsaveDirPtr;

 argp[i++] = "-asfcpconfig";
 argp[i++] = subsysConfigPtr->configFilePtr;
*****/
  if (subsysConfigPtr->GPRtemplatePtr != NULL) {
   argp[i++] = "-config";
   argp[i++] = subsysConfigPtr->GPRtemplatePtr;
  }

 if ( (loglocPtr = (char *)getLogLocation()) != NULL) {
  argp[i++] = "-asflogloc";
  argp[i++] = loglocPtr;
 }
 argp[i++] = NULL;

ppid = doSpawn(getGPRname(), subsysConfigPtr->GPRexeNamePtr, fullPathPtr, argp);


 /* if spawned successfuly, create the entry */
 if (ppid > 0)
   AddPidToList_PIDLIST(RUTIL_CLASS_ID, getGPRname(), fullPathPtr, "logfile",
                        NOT_SET_ID, ppid);


} /* end spawnRequestUtil....................*/


/*----------------------------------------------------------
 * NAME:
 *  spawnPreQC_CB
 *
 * DESCRIPTION:
 *  launch the image averaging task for an image file
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int spawnPreQC_CB(qcReqType *qcReqPtr)
{
 char *argp[20];
 int  i = 0, ppid,retval;
 char fullPathPtr[256];
 subsysConfigType *subsysConfigPtr;
 char procReqNoBuf[50];

#ifdef QC_DEBUG
printf("spawnPreQC_CB: scan %d\n", qcReqPtr->qcType);
#endif

retval =  GetNumInstances_PIDLIST(CP_PRE_QC);
 if (retval >= getNumPreQCs()) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, MAX_RUNNING, CP_PRE_QC);
   return(0);
  }

  bzero(argp, sizeof(argp) );
 /* get the exe name , path name etc for the process */
  subsysConfigPtr  = getCPconfig();


  sprintf(fullPathPtr, "%s/%s", subsysConfigPtr->PreQCpathPtr,
                                subsysConfigPtr->PreQCexeNamePtr);

  printfLLog(LOG_DEBUG, SPAWNING_QC_REQ, "Image Averaging", qcReqPtr->jobId);
 /*
  * convert req number to a buffer -- for time being use req no
  * as processing request. Processing request may become order ID
  */
  sprintf(procReqNoBuf, "%d", qcReqPtr->jobId);
  argp[i++] =  subsysConfigPtr->PreQCexeNamePtr;
  argp[i++] = "-imagefile";
  argp[i++] = qcReqPtr->imageFileBuf;
  argp[i++] = "-leaderfile";
  argp[i++] = qcReqPtr->leaderFileBuf;

  argp[i++] = "-outputfile";
  argp[i++] = qcReqPtr->avgFileBuf;

  argp[++i] = NULL;
  ppid = doSpawn(CP_PRE_QC,subsysConfigPtr->PreQCexeNamePtr,fullPathPtr,argp);

  if (ppid > 0) {
    AddPidToList_PIDLIST(QC_CLASS_ID, CP_PRE_QC, fullPathPtr, "logfile",
                        NOT_SET_ID, ppid);

    if (SetQCreqGivenPid_PIDLIST(ppid, qcReqPtr) == -1)
       printfLLog(LOG_ERR, CANNOT_SET, "qcReq");
    return(1);
  }

  return(0);

} /* spawnPreQC_CB */

/*----------------------------------------------------------
 * NAME:
 *  spawnScanQC_CB
 *
 * DESCRIPTION:
 *  launch the Quality Control task for the scan results file
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int spawnScanQC_CB(qcReqType *qcReqPtr)
{
 char *argp[20], procReqNoBuf[50], fullPathPtr[256];
 int  i = 0, ppid, retval;
 subsysConfigType *subsysConfigPtr;
 Display *disp;

#ifdef QC_DEBUG
printf("spawnScanQC_CB: scan %d\n", qcReqPtr->qcType);
#endif
 retval =  GetNumInstances_PIDLIST(CP_SCAN_QC);
 if (retval >= getNumScanQCs()) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, MAX_RUNNING, "Scan QC");
   return(0);
  }

  bzero(argp, sizeof(argp) );
 /* get the exe name , path name etc for the process */
  subsysConfigPtr  = getCPconfig();


  sprintf(fullPathPtr, "%s/%s", subsysConfigPtr->ScanQCpathPtr,
                                subsysConfigPtr->ScanQCexeNamePtr);

  printfLLog(LOG_DEBUG, SPAWNING_QC_REQ, "Scan QC", qcReqPtr->jobId);
 /*
  * convert req number to a buffer -- for time being use req no
  * as processing request. Processing request may become order ID
  */
  sprintf(procReqNoBuf, "%d", qcReqPtr->jobId);
  argp[i++] =  subsysConfigPtr->ScanQCexeNamePtr;
  argp[i++] = qcReqPtr->scanResultsFileBuf;
  if (subsysConfigPtr->ScanQCdisplayPtr != NULL) {
    if ((disp = XOpenDisplay(subsysConfigPtr->ScanQCdisplayPtr)) == NULL ) {
      ASFlogMessage(ASF_CP, WP_ERROR_BOX, QC_CANNOT_START);
      return(0);
    }
    XCloseDisplay(disp);
    argp[i++] = "-disp";
    argp[i++] =  subsysConfigPtr->ScanQCdisplayPtr;
  }
  argp[i++] = "-asfcpconfig";
  argp[i++] = subsysConfigPtr->configFilePtr;

  argp[i++] = NULL;
  ppid = doSpawn(CP_SCAN_QC, subsysConfigPtr->ScanQCexeNamePtr, 
                fullPathPtr, argp);

  if (ppid > 0) {
    AddPidToList_PIDLIST(QC_CLASS_ID, CP_SCAN_QC, fullPathPtr, "logfile",
                        NOT_SET_ID, ppid);

    if (SetQCreqGivenPid_PIDLIST(ppid, qcReqPtr) == -1)
       printfLLog(LOG_ERR, CANNOT_SET, "qcReq");
    return(1);
  }

  return(0);

} /* spawnScanQC_CB */

/*----------------------------------------------------------
 * NAME:
 *  spawnQC_CB 
 *
 * DESCRIPTION:
 *  launch the Quality Control task
 *
 * NOTES:
 *  bottom color seems not to be used anymore
 *
 *---------------------------------------------------------*/
int spawnQC_CB(qcReqType *qcReqPtr)
{
 char *argp[20], fullPathPtr[256], procReqNoBuf[50], scaleStr[50];
 int  i = 0, ppid, retval;
 subsysConfigType *subsysConfigPtr;
 Display *disp;

#ifdef QC_DEBUG
printf("spawnQC_CB: scan %d\n", qcReqPtr->qcType);
#endif
 retval =  GetNumInstances_PIDLIST(CP_QC);
 if (retval >= getNumQCs()) {
   ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, MAX_RUNNING, "QC");
   return(0);
  }

  bzero(argp, sizeof(argp) );
 /* get the exe name , path name etc for the process */
  subsysConfigPtr  = getCPconfig();


  sprintf(fullPathPtr, "%s/%s", subsysConfigPtr->QCpathPtr, 
                                subsysConfigPtr->QCexeNamePtr);

  printfLLog(LOG_DEBUG, SPAWNING_QC_REQ, "Image QC", qcReqPtr->jobId);
 /* 
  * convert req number to a buffer -- for time being use req no 
  * as processing request. Processing request may become order ID
  */
  sprintf(procReqNoBuf, "%d", qcReqPtr->jobId);
  argp[i++] =  subsysConfigPtr->QCexeNamePtr;
  argp[i++] = "-imagefile";
  argp[i++] = qcReqPtr->imageFileBuf;
  argp[i++] = "-leaderfile";
  argp[i++] = qcReqPtr->leaderFileBuf;

  if (qcReqPtr->avgFileBuf != NULL) {
    if (doesFileExist(qcReqPtr->avgFileBuf) )  {
      argp[i++] = "-avgfile";
      argp[i++] = qcReqPtr->avgFileBuf;
    }
  }
  argp[i++] = "-job";
  argp[i++] = procReqNoBuf;
  if (subsysConfigPtr->QCdisplayPtr != NULL) {
    if ((disp = XOpenDisplay(subsysConfigPtr->QCdisplayPtr)) == NULL ) {
      ASFlogMessage(ASF_CP, WP_ERROR_BOX, QC_CANNOT_START);
      return(0);
    }
    XCloseDisplay(disp);
    argp[i++] = "-disp";
    argp[i++] =  subsysConfigPtr->QCdisplayPtr; 
  }
  argp[i++] = "-complex_scale_fac";
  sprintf(scaleStr, "%lf", subsysConfigPtr->QCcomplexScaleFac);
  argp[i++] = scaleStr;

  argp[i++] = NULL;

  ppid = doSpawn(CP_QC, subsysConfigPtr->QCexeNamePtr, fullPathPtr, argp);

  if (ppid > 0) {
    AddPidToList_PIDLIST(QC_CLASS_ID, CP_QC, fullPathPtr, "logfile",
                        NOT_SET_ID, ppid);

    if (SetQCreqGivenPid_PIDLIST(ppid, qcReqPtr) == -1)
       printfLLog(LOG_ERR, CANNOT_SET, "qcReq");
    return(1);
  }
  
  return(0);

} /* spawnQC_CB .............................*/


/*----------------------------------------------------------
 * NAME:
 *  initMediaMount
 *
 * DESCRIPTION:
 *  initialize global variables used for tracking the
 *  last media id mounted
 *
 * NOTES:
 *--------------------------------------------------------*/
void initMediaMount()
{
  int i;

  for ( i = 0; i < MAX_RDS; i++) {
  strcpy(GLOBAL_lastRDSMediaMounted[i], "");
  strcpy(GLOBAL_lastRDSMediaPrompted[i], "");
  }
  strcpy(GLOBAL_lastASPMediaMounted, "");
  strcpy(GLOBAL_lastASPMediaPrompted, "");

} /* initMediaMount */

/*----------------------------------------------------------
 * NAME:
 *  TapeMountedCB
 *
 * DESCRIPTION:
 *  callback for when operator selected "Ok" from the
 *  tape mount prompt box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void TapeMountedCB(Widget w, XtPointer client_data, XtPointer cb_arg)
{
  int cat,rdsNum;
  mstrqListElemType *mqel ;
  mqel = ( mstrqListElemType *) client_data;

printfLLog(LOG_DEBUG, "Tape mounted for %s job %d", mqel->namePtr, mqel->jobId);
  SetDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
  SetDataReady_MSTRQLIST(mqel->jobId);
  if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
          Q_M_INPUT_SOURCE_READY_ID)== -1)
     printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);
  GLOBAL_pos++;
  cat = GetSubsystemCategoryGivenName(mqel->namePtr);
  if (cat == RDS_CATEGORY_ID){
    rdsNum = getRDSnumGivenName(mqel ->namePtr);
    strcpy(GLOBAL_lastRDSMediaMounted[rdsNum], GLOBAL_lastRDSMediaPrompted[rdsNum]);
  }
  else if (cat == ASP_CATEGORY_ID)
    strcpy(GLOBAL_lastASPMediaMounted, GLOBAL_lastASPMediaPrompted);

  FreeMstrqListElem_MSTRQLIST(mqel);

} /* TapeMountedCB */

/*----------------------------------------------------------
 * NAME:
 *  TapeNotMountedCB
 *
 * DESCRIPTION:
 *  callback for when operator selected "Cancel" from the
 *  tape mount prompt box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void TapeNotMountedCB(Widget w,XtPointer client_data,XtPointer cb_arg)
{
  mstrqListElemType *mqel = ( mstrqListElemType *) client_data;

printfLLog(LOG_DEBUG, "Tape not mounted for %s job %d", mqel->namePtr, mqel->jobId);
  ClearDataReady_SYSQUE(mqel->namePtr, mqel->jobId);
  ClearDataReady_MSTRQLIST(mqel->jobId);
  if (ChangeStatus__MSTRQLIST(mqel->jobId, mqel->namePtr, 
          Q_M_PLACED_ON_HOLD_DATA_ID)== -1)
     printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, mqel->namePtr);

  GLOBAL_pos++;

  FreeMstrqListElem_MSTRQLIST(mqel);

} /* TapeNotMountedCB */

/*----------------------------------------------------------
 * NAME:
 *  CleanupConfirmCB
 *
 * DESCRIPTION:
 *  a callback function installed to handle the closing
 *  of the cleanup confirmation box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void CleanupConfirmCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  ODL writeOutODL;
  char *namePtr;
  CleanupStruct *svs = (CleanupStruct *) client_data;
  int sockfd, state, sqPos, retval;
  baseListElemType *nPtr;

printfLLog(LOG_DEBUG, "Cleanup Confirmation for %s job %d: resubmit %d discard %d", svs->namePtr, svs->jobId, svs->resubmit, svs->discard);
#ifdef DEBUG
printf("CleanupConfirmCB: svs 0x%x job %d resubmit %d discard %d\n", 
svs, svs->jobId, svs->resubmit, svs->discard);
#endif

#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return;



        /* need to check that this job still exists.  it could have possibly */
        /* been removed between the time the dialog box was popped up and    */
        /* when the operator finally clicked the 'OK' button                 */
  if (!JobExists_SYSQUE(svs->namePtr, svs->jobId)) {
    printfLLog(LOG_ERR, JOB_NOT_ON_QUEUE, svs->jobId, svs->namePtr, "cleanup processing");
    unLockNameList_CORE(nPtr);
    return;
  }

  if (IncrementRetry_MSTRQLIST(svs->jobId) == -1) {
    printfLLog(LOG_ERR, "Cannot increment error retry count");
#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return;
  }

  if ((sockfd = GetSocketGivenName_PIDLIST(svs->namePtr)) == -1) {
    printfLLog(LOG_ERR, CANNOT_GET, "process socket");
#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return;
  }

  if (svs->resubmit) {  /* place job on hold for later resubmission */
    if (ChangeStatus__MSTRQLIST(svs->jobId, svs->namePtr,
                               /* Q_M_PLACED_ON_HOLD_ERROR_ID) == -1) */
                               Q_M_CLEANING_UP_ID) == -1)
      printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, svs->namePtr);
  }
  else {                /* operator wants to cancel the job altogether */
     if (ChangeStatus__MSTRQLIST(svs->jobId, svs->namePtr,
                               Q_M_ENTRY_DELETED_ID) == -1)
       printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "entry deleted"); 
          /* remove entry from subsystem queue and display, if there is one */
      sqPos = GetListPosGivenJobId_SYSQUE(svs->namePtr, svs->jobId);
      FreeSysQueElemGivenPos_SYSQUE(svs->namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(svs->namePtr, svs->jobId) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s subsystem queue",
          svs->namePtr);
      removeLineForJobId(svs->namePtr, svs->jobId);
  }

  state = GetProcessState_PIDLIST(svs->namePtr); /* see if process running */
  if (state != SUBSYS_RUNNING_ID && state != SUBSYS_READY_ID &&
      state != SUBSYS_WAITING_ID ) {
      printfLLog(LOG_INFO, PROCESS_INVALID_STATE_TEXT, svs->namePtr, state,
         "sending cleanup message");
      doFree(svs);
#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB unlocking READ_LIST\n"); fflush(stdout);
#endif
      unLockNameList_CORE(nPtr);
      return ;
  }

  if ((writeOutODL = 
    buildCleanup_Msg(svs->namePtr, svs->jobId, svs->platform,
                         svs->rev, svs->discard)) == NULL) {
#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB unlocking READ_LIST\n"); fflush(stdout);
#endif
    unLockNameList_CORE(nPtr);
    return;
  }
  namePtr = doMalloc(strlen(svs->namePtr)+1);  /* creating a leak */
  strcpy(namePtr, svs->namePtr);  

  retval = doSendMsg(namePtr, sockfd, writeOutODL, EXPECT_STATUS_MSG);
  if(retval < 0) {
   if (errno) /* if errno set, use that info in the error message */
     printfLLog(LOG_ERR, CANNOT_SEND_MSG_ERRNO, namePtr, strerror(errno));
   else
     printfLLog(LOG_ERR, CANNOT_SEND_MSG_SOCKET, namePtr);
  }
 
  ODLFree(writeOutODL);

  updateStateColor(getSubsysNoGivenName(svs->namePtr), DISP_COL_WAITING,
               CPstatusRC);

  doFree(svs);
#ifdef READ_LOCK_DEBUG
printf("CleanupConfirmCB unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);
  return;

} /* CleanupConfirmCB */

/*----------------------------------------------------------
 * NAME:
 *  CancelQuestionCB
 *
 * DESCRIPTION:
 *  a callback function installed to handle the cancel selection
 *  from the question box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
static void CancelQuestionCB(Widget w, XtPointer client_data, 
                             XtPointer call_data)
{
 dialogCBdata *ptr ;
 int jobId, sqPos, mqPos;
  baseListElemType *nPtr;

  ptr = (dialogCBdata *)client_data;
  jobId = ptr->pos;

#ifdef READ_LOCK_DEBUG
printf("CancelQuestionCB locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return;

#ifdef CB_DEBUG
printCBdata("Cancel_CB", ptr);
#endif

  if (ptr->action == SUBSYS_WRONG_TAPE_ID || 
      ptr->action == SUBSYS_PPS_ERROR_ID  ||
      ptr->action == SUBSYS_ERROR_ID)
    printfLLog(LOG_DEBUG, QUESTION_CHOICE, "Cancel", ptr->namePtr, jobId);

  switch(ptr->action) {
    case SUBSYS_WRONG_TAPE_ID:
        /* need to check that this job still exists.  it could have possibly */
        /* been removed between the time the dialog box was popped up and    */
        /* when the operator finally clicked the 'cancel' button             */
      if (!JobExists_SYSQUE(ptr->namePtr, jobId)) {
      /* if (!JobExists_MSTRQLIST(jobId)) { */
        printfLLog(LOG_ERR, JOB_NOT_ON_QUEUE, jobId, ptr->namePtr, "tape label processing");
        break;
      }
/* set flag so media id gets checked next time */
      SetMediaFlagGivenJobId_SYSQUE(ptr->namePtr, jobId, MEDIA_CHECK_YES); 
      if (ChangeStatus__MSTRQLIST(jobId, ptr->namePtr,
                                Q_M_PLACED_ON_HOLD_ERROR_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "on hold/error");
/* here we also want to check how the user wants to perform cleanup.
   like whether to save or discard subsystem-generated files */

      break;

    case SUBSYS_ERROR_ID:
        /* need to check that this job still exists.  it could have possibly */
        /* been removed between the time the dialog box was popped up and    */
        /* when the operator finally clicked the 'cancel' button             */
      /*if (!JobExists_MSTRQLIST(jobId)) {  */
      if (!JobExists_SYSQUE(ptr->namePtr, jobId)) {
        printfLLog(LOG_ERR, JOB_NOT_ON_QUEUE, jobId, ptr->namePtr, "cancel processing");
        break;
      }
      if (ChangeStatus__MSTRQLIST(jobId, ptr->namePtr,
                               /* Q_M_DONE_ERROR_ID) == -1) */
                               Q_M_ENTRY_DELETED_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "entry deleted");

          /* remove entry from subsystem queue and display, if there is one */
      sqPos = GetListPosGivenJobId_SYSQUE(ptr->namePtr, jobId);
      FreeSysQueElemGivenPos_SYSQUE(ptr->namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(ptr->namePtr, jobId) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s subsystem queue",
          ptr->namePtr);
      removeLineForJobId(ptr->namePtr, jobId);

      break;

    case SUBSYS_PPS_ERROR_ID:
      mqPos = GetDispPos_MSTRQLIST(ptr->pos);
      RemoveODLreqFromList_MSTRQLIST(mqPos);  /* free the odl part */
      if (RemoveMstrqFromList_MSTRQLIST(mqPos) == NULL)
        printfLLog(LOG_ERR, "Error removing job from master queue ");
      else {
        printfLLog(LOG_INFO, "Job %d removed from master queue", ptr->pos);
      }
      removeLineForJobId(ASF_CP, jobId);

           /* remove entry from subsystem queue and display, if there is one */
      sqPos = GetListPosGivenJobId_SYSQUE(ptr->namePtr, ptr->pos);
      FreeSysQueElemGivenPos_SYSQUE(ptr->namePtr, sqPos);
      if (RemoveElemWithJobId_SYSQUE(ptr->namePtr, ptr->pos) == NULL)
        printfLLog(LOG_ERR, "Error removing job from %s subsystem queue",
          ptr->namePtr);
      removeLineForJobId(ptr->namePtr, jobId);


      break;

    case SUBSYS_STOPPING_ID:
    case SUBSYS_PAUSING_ID:
    case SUBSYS_HALTING_ID:
    case EXIT_ALL_ID:
      break;

    default:
      printfLLog(LOG_ERR, UNEXPECTED_VALUE, ptr->action, "CancelQuestionCB\n");

      break;
  } /* end switch */

#ifdef READ_LOCK_DEBUG
printf("CancelQuestionCB unlocking READ_LIST\n"); fflush(stdout);
#endif
 unLockNameList_CORE(nPtr);
 return;

} /* CancelQuestionCB */

/*----------------------------------------------------------
 * NAME:
 *  OkQuestionCB 
 *
 * DESCRIPTION:
 *  a callback function installed to handle the OK selection
 *  from the question box
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
#define STOP_TYPE_INVALID 0  /* subsystem in invalid state for stopping */
#define STOP_TYPE_NORMAL 1   /* can send SUBSYSTEM_STOP message to stop */
#define STOP_TYPE_KILL   2   /* when subsys hung; use system kill to stop */

static void OkQuestionCB(Widget w, XtPointer client_data, 
                             XtPointer call_data)
{
 int sockfd, retval, acceptThread, jobId, state, stopType = STOP_TYPE_NORMAL;
 dialogCBdata *ptr ;
 baseListElemType *nPtr;

  ptr = (dialogCBdata *)client_data; 
  jobId = ptr->pos;
#ifdef READ_LOCK_DEBUG
printf("OkQuestionCB locking READ_LIST\n"); fflush(stdout);
#endif
  nPtr = lockNameList_CORE(READ_LIST);
  if (nPtr == NULL)
    return;



#ifdef CB_DEBUG
printCBdata("Ok_CB", ptr);
#endif
  if (ptr->action == SUBSYS_WRONG_TAPE_ID || 
      ptr->action == SUBSYS_PPS_ERROR_ID  ||
      ptr->action == SUBSYS_ERROR_ID)
    printfLLog(LOG_DEBUG, QUESTION_CHOICE, "OK", ptr->namePtr, jobId);
  switch(ptr->action) {
    case SUBSYS_WRONG_TAPE_ID:
        /* need to check that this job still exists.  it could have possibly */
        /* been removed between the time the dialog box was popped up and    */
        /* when the operator finally clicked the 'OK' button                 */
      if (!JobExists_SYSQUE(ptr->namePtr, jobId)) {
      /* if (!JobExists_MSTRQLIST(jobId)) { */
        printfLLog(LOG_ERR, JOB_NOT_ON_QUEUE, jobId, ptr->namePtr, "OK processing");
        break;
      }
      state = GetProcessState_PIDLIST(ptr->namePtr); /* see if still up */
      if (state == SUBSYS_PAUSING_ID ) /* if pausing, make subsys waiting */
        ChangeProcessState_PIDLIST(ptr->namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);

  
      SetMediaFlagGivenJobId_SYSQUE( ptr->namePtr, jobId, MEDIA_CHECK_NO);

/* don't increment retry count for OK cb: only cleanups */

      if (ChangeStatus__MSTRQLIST(jobId, ptr->namePtr,
                                Q_M_PLACED_ON_SYSQ_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "on queue");
      /* set repeat flag so a second entry doesn't get added to sysq display */
      SetRepeatFlagGivenJobId_SYSQUE(ptr->namePtr, jobId);
      break;


    case SUBSYS_PPS_ERROR_ID:
    case SUBSYS_ERROR_ID:
        /* need to check that this job still exists.  it could have possibly */
        /* been removed between the time the dialog box was popped up and    */
        /* when the operator finally clicked the 'OK' button                 */
      if (!JobExists_SYSQUE(ptr->namePtr, jobId)) {
      /* if (!JobExists_MSTRQLIST(jobId)) { */
        printfLLog(LOG_ERR, JOB_NOT_ON_QUEUE, jobId, ptr->namePtr, "OK processing");
        break;
      }
/* don't increment retry count for OK cb: only cleanups */


      if (ChangeStatus__MSTRQLIST(jobId, ptr->namePtr,
                                Q_M_PLACED_ON_HOLD_ERROR_ID) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "on hold/error");

      updateStateColor(getSubsysNoGivenName(ptr->namePtr), DISP_COL_WAITING, 
               CPstatusRC);

/* here we also want to check how the user wants to perform cleanup.
   like whether to save or discard subsystem-generated files */
      break;

    case SUBSYS_STOPPING_ID:  /* if subsystem is running, stop it */ 

      state = GetProcessState_PIDLIST(ptr->namePtr); /* see if still up */
     if ((state == SUBSYS_HUNG_ID) || (state == SUBSYS_STOPPING_ID))
        stopType = STOP_TYPE_KILL;
      else if (state == SUBSYS_RUNNING_ID || state == SUBSYS_READY_ID ||
          state == SUBSYS_PAUSING_ID ||
          state == SUBSYS_HEALTHING_ID || state == SUBSYS_WAITING_ID ) {
        logSubsysControlMessage(ptr->namePtr, SUBSYS_STOPPING_ID);
        stopType = STOP_TYPE_NORMAL;
      }
      else
        stopType = STOP_TYPE_INVALID;

      if (stopType == STOP_TYPE_INVALID)
        printfLLog(LOG_ERR, PROCESS_INVALID_STATE_TEXT, ptr->namePtr, 
                   state, "stopping");
      else {  /* subsystem can be stopped */
        logSubsysControlMessage(ptr->namePtr, SUBSYS_STOPPING_ID);
        if (stopType == STOP_TYPE_NORMAL)
          StopSubsystemCB(NULL, NULL, (XtPointer) ptr->namePtr);
        else
          doRemoteKill(ptr->namePtr);
      }

      break;

    case SUBSYS_PAUSING_ID:
      logSubsysControlMessage(ptr->namePtr, SUBSYS_PAUSING_ID);

      state = GetProcessState_PIDLIST(ptr->namePtr); /* see if still up */
      if (state == SUBSYS_RUNNING_ID )
        ChangeProcessState_PIDLIST(ptr->namePtr, SUBSYS_PAUSING_ID, NOT_SET_ID);
      else if (state != SUBSYS_PAUSING_ID) 
                  /* if not pausing or running, can go directly to 'waiting' */
        ChangeProcessState_PIDLIST(ptr->namePtr, SUBSYS_WAITING_ID, NOT_SET_ID);

      break;

    case SUBSYS_HALTING_ID:
    /* get the socket  to send to */
      sockfd = GetSocketGivenName_PIDLIST(ptr->namePtr);
      if (sockfd == -1) {
        printfLLog(LOG_ERR, CANNOT_FIND_SOCKET, ptr->namePtr, sockfd);
#ifdef READ_LOCK_DEBUG
printf("OkQuestionCB unlocking READ_LIST\n"); fflush(stdout);
#endif
        unLockNameList_CORE(nPtr);
          return;
      }

    /* 
     * within sendhaltmsg we 
     * mark the process as halted.. so we wait for 
     * receipt of subsystem_ready to reset to 
     * running
     */

      state = GetProcessState_PIDLIST(ptr->namePtr); /* see if still up */
      if (state == SUBSYS_RUNNING_ID || state == SUBSYS_READY_ID ||
          state == SUBSYS_PAUSING_ID ||
          state == SUBSYS_HEALTHING_ID || state == SUBSYS_WAITING_ID ) {
        logSubsysControlMessage(ptr->namePtr, SUBSYS_HALTING_ID);
        SendResetMsg(ptr->namePtr, sockfd);

/* reset pps flags so we aren't expecting to receive a job */
        if (determineClassFromName(ptr->namePtr) == PPS_CLASS_ID)
            ClearMsgOut_PPS(TRUE);
      }
      else
        printfLLog(LOG_ERR, PROCESS_INVALID_STATE_TEXT, ptr->namePtr, 
                   state, "resetting");

      break;

    case EXIT_ALL_ID:
    /*
     * stop all and subsystems and associated threads 
     * including the accept thread
     */
 /********************
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, CP_SHUTTING_DOWN_TEXT, ASF_CP);
*********************/
      acceptThread =  doStopAll();
      if (acceptThread != 0) {
#ifdef PTHREAD_CANCEL
         baseListElemType *nPtr;

#ifdef READ_LOCK_DEBUG
printf("OkQuestionCB locking READ_LIST b/f pthread_cancel\n"); fflush(stdout);
#endif
                  /* lock the read list so that all read threads that have */
                  /* existing locks can complete their processing before we */
                  /* perform pthread_cancel on the accept thread.  if any of */
                  /* the read threads has the read list locked, while we */
                  /* pthread_cancel the accept thread, the CP will end up */
                  /* in deadlock because the read list is never unlocked  by */
                  /* the read thread (killing its parent kills it too) */

        nPtr = lockNameList_CORE(READ_LIST);
        if (nPtr == NULL)
          return; /* not sure returning is the right thing to do... */

/*don't do it yet        retval = pthread_cancelCWS(acceptThread); */
#else
        exit(0); 
#endif
      }
      else
        printfLLog(LOG_ERR, CANNOT_GET, "accept thread process id\n");

      printfLLog(LOG_INFO, CP_GOODBYE);
/*      freeCPconfig(); */
/*       exit(0);     */
      break;

    default:
      printfLLog(LOG_ERR, UNEXPECTED_VALUE, ptr->action, "OkQuestionCB\n");
      break;
  } /* end switch */

#ifdef READ_LOCK_DEBUG
printfLLog(LOG_DEBUG, "OkQuestionCB unlocking READ_LIST\n"); 
printf("OkQuestionCB unlocking READ_LIST\n"); fflush(stdout);
#endif
  unLockNameList_CORE(nPtr);
  return;
} /* end OkQuestionCB......................*/


/*----------------------------------------------------------
 * NAME:
 *  doCreateMainWindowFileBox
 *
 * DESCRIPTION:
 *  create the file selection dialog box
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void doCreateMainWindowFileBox(Widget wid)
{
  XmString xmstring;
  Arg arg[5];
  int i;
  subsysConfigType *subsysConfigPtr;

  subsysConfigPtr = getCPconfig();

  XtUnmanageChild(XmFileSelectionBoxGetChild(wid, XmDIALOG_HELP_BUTTON) );
  xmstring = XmStringCreateSimple(subsysConfigPtr->CPsaveDirPtr);
  i = 0;
  XtSetArg(arg[i], XmNdirectory, xmstring);i++;
  XtSetValues(wid, arg, i);
  XmStringFree(xmstring);

} /* end doCreateMainWindowFileBox.........................*/


/*----------------------------------------------------------
 * NAME:
 *  doCreateQuestionDlg 
 *
 * DESCRIPTION:
 *  Create the question dialog box. Do it here rather than
 *  in the UIMX create callback so that we have more control
 *  over the code. 
 *
 * NOTES:
 *   This call is only made in the main thread.
 *
 *---------------------------------------------------------*/
dialogCBdata *doCreateQuestionDlg(Widget wid, dialogCBdata *questionCbData)
{
  dialogCBdata *ptr;

#ifdef CB_DEBUG_CREATE
printCBdata("entering doCreateQuestionDlg", questionCbData);
#endif
  ptr = doMalloc(sizeof(dialogCBdata));
#ifdef CB_DEBUG_CREATE
printf("doing memcopy from 0x%x to 0x%x of %d bytes\n", questionCbData, ptr, 
         sizeof(dialogCBdata));  fflush(stdout);
#endif

  memcpy(ptr, questionCbData, sizeof(dialogCBdata));


  XtUnmanageChild(XmMessageBoxGetChild(wid, XmDIALOG_HELP_BUTTON) );
  XtAddCallback(wid, XmNokCallback, (XtCallbackProc) OkQuestionCB, 
                (XtPointer) ptr);
  XtAddCallback(wid, XmNcancelCallback, (XtCallbackProc) CancelQuestionCB, 
                (XtPointer) ptr);


#ifdef CB_DEBUG_CREATE
printCBdata("leaving doCreateQuestionDlg", questionCbData);
#endif
  return(questionCbData); 

} /* doCreateQuestionDlg...............*/


/*----------------------------------------------------------
 * NAME:
 *  doStartSubsystem
 *
 * DESCRIPTION:
 *  start the subsystem denoted by "namePtr" after 
 *  the user selection has been correctly prepared
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int doStartSubsystem(char *namePtr, Widget buttonWid)
{
 int retval;

  /* see if the proc is already running, if so tell user and just return */
  retval =  GetProcessState_PIDLIST(namePtr);

  /* 
   * use _DORMANT rather than  _RUNNING_ID to allow the susbsys to 
   * shut down while halting
   */
  if (retval != SUBSYS_DORMANT_ID  && retval != SUBSYS_DIDNT_START_ID) {
    ASFlogMessage_direct(ASF_CP, WP_INFO_BOX, 
       "Cannot start %s: subsystem in invalid state (%d)", namePtr, retval); 
    return(0);
  }

  /* subsys not running, so spawn it */
  retval = spawnLocalSubsystem(namePtr, buttonWid);

  /* if we cannot launch, just return for now */
  if (retval == -1) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, COULD_NOT_START_TEXT, namePtr); 
    return(-1);
  } /* end if retval cannot launch */

  /* change the process state to indicate that it is started */
  retval = ChangeProcessState_PIDLIST(namePtr, SUBSYS_STARTED_ID, NOT_SET_ID);

  if (retval == -1) {
    printfLLog(LOG_ERR, CANNOT_CHANGE_TO_STARTED, namePtr);
    return(-1);
  }

  return(0);
} /* end doStartSubsystem ...............*/


/*----------------------------------------------------------
 * NAME:
 *  handleToggleRaise
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Raise"
 *  button on the Subsystem Control panel
 *
 * NOTES:
 *  changes the state from waiting to ready
 *
 *---------------------------------------------------------*/

void handleToggleRaise(char *categoryNamePtr)
{
 Widget w;
#ifdef EVER_STARTED  /* this is debug code that we don't use */
 int pid;
#endif
 int state;


  w = GetMainWidGivenName_PIDLIST(categoryNamePtr);
  if (w == NULL) { /* this should NEVER happen b/c is based on config entries */
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, 
       "No main widget for %s -- cannot raise", categoryNamePtr);
    return;
  }
#ifdef EVER_STARTED /* check if this subsys has EVER started */
  pid = GetPidGivenName_PIDLIST(categoryNamePtr);
  if (pid == 0) {
      ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, 
        "Cannot raise %s window until subsystem has run", categoryNamePtr);
      return;
  }
#else  /* check if this subsys is CURRENTLY running right now */
  state = GetProcessState_PIDLIST(categoryNamePtr) ;
  if (state == SUBSYS_DORMANT_ID || state == SUBSYS_NOT_RUNNING_ID ) {
      ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, PROCESS_NOT_STARTED_TEXT,
                 categoryNamePtr, "raise");
      return;
  }
#endif
    XtMapWidget(w);
    XRaiseWindow(XtDisplay(w), XtWindow(w));

  return;

} /* end handleToggleRaise...................*/



/*----------------------------------------------------------
 * NAME:
 *  handleToggleReady 
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Ready" 
 *  button on the Subsystem Control panel 
 *
 * NOTES:
 *  changes the state from waiting to ready
 *
 *---------------------------------------------------------*/

void handleToggleReady(char  *namePtr)
{
 int  state;

           /* only do something if current state is 'waiting' */
  if ((state = GetProcessState_PIDLIST(namePtr)) == SUBSYS_WAITING_ID) {
    ChangeProcessState_PIDLIST(namePtr, SUBSYS_READY_ID, NOT_SET_ID);
    ASFlogMessage_direct(namePtr, QUE_STATUS_LABEL_ID, SYS_READY_TEXT);
  }
  else if (state == SUBSYS_DORMANT_ID || state == SUBSYS_NOT_RUNNING_ID)
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, "Subsystem %s must be started",
                 namePtr);

  return;

} /* end handleToggleReady...................*/


/*----------------------------------------------------------
 * NAME:
 *  handleToggleStart 
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Start" 
 *  button on the Subsystem Control panel -- parameters 
 *  are set up the subsystem in question and passed to 
 *  doStartSubsystem
 *
 * NOTES:
 *  the queue widget is poped up and the 
 *  title changed when the subsystem is 
 *  added to the pid list -- this happens
 *  in the spawn routine so that the pid can 
 *  be captured
 *
 *---------------------------------------------------------*/

void handleToggleStart(char  *namePtr, Widget buttonWid)
{
 int     retval;

  retval = doStartSubsystem(namePtr, buttonWid);
  if (retval == -1)
    return;

} /* end handleToggleStart...................*/

/*----------------------------------------------------------
 * NAME:
 *  handleToggleStop 
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Stop" 
 *  button on the Subsystem Control panel -- sets up 
 *  the parameters for the OkQuestionCB callback and 
 *  places a question dialog box up so that the user
 *  may confirm his choice.
 *
 * NOTES:
 *  The callback is already installed for the question 
 *  dialog.
 *
 *---------------------------------------------------------*/
void handleToggleStop(char  *namePtr, Widget buttonWid)
{
 int     retval;
 dialogCBdata *cbData;

  /*
   * check to see if process is running
   * NOTE:: need to check for invocation
   */
  retval =  GetProcessState_PIDLIST(namePtr);
  /*
   * use _DORMANT_ID rather than
   * _RUNNING_ID  to allow the system
   * to shut down while  halting
   */
  if (retval == SUBSYS_DORMANT_ID ) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, PROCESS_NOT_STARTED_TEXT,
                 namePtr, "shutdown");
    return;
  }

  /* process running, set up the callback data */
  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_STOPPING_ID;
  cbData->buttonWid = buttonWid;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_STOP_TEXT, cbData->namePtr);


} /* end handleToggleStop...................*/



/*----------------------------------------------------------
 * NAME:
 *  handleTogglePause
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Pause" 
 *  button on the Subsystem Control panel -- sets up 
 *  the parameters for the OkQuestionCB callback and 
 *  places a question dialog box up so that the user
 *  may confirm his choice.
 *
 * NOTES:
 *  The callback is already installed for the question 
 *  dialog.
 *
 *---------------------------------------------------------*/

void handleTogglePause(char *namePtr,Widget buttonWid)
{
 int    retval;
 dialogCBdata *cbData;

  retval =  GetProcessState_PIDLIST(namePtr);

  /* use dormant rather than _RUNNING_ID
   * to allow the system to halt between states
   */

  if (retval == SUBSYS_DORMANT_ID ) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, PROCESS_NOT_STARTED_TEXT,
                 namePtr, "pause");
    return;
  }

  /* process running, set up the callback data */
  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_PAUSING_ID;
  cbData->buttonWid = buttonWid;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_PAUSE_TEXT, cbData->namePtr);


} /* end handleTogglePause.........................*/


/*----------------------------------------------------------
 * NAME:
 *  handleToggleReset
 *
 * DESCRIPTION:
 *  the callback invoked when user presses the "Reset" 
 *  button on the Subsystem Control panel -- sets up 
 *  the parameters for the OkQuestionCB callback and 
 *  places a question dialog box up so that the user
 *  may confirm his choice.
 *
 * NOTES:
 *  The callback is already installed for the question 
 *  dialog.
 *
 *---------------------------------------------------------*/

void handleToggleReset(char *namePtr,Widget buttonWid)
{
 int    retval;
 dialogCBdata *cbData;

  retval =  GetProcessState_PIDLIST(namePtr);

  /* use dormant rather than _RUNNING_ID
   * to allow the system to halt between states
   */

  if (retval == SUBSYS_DORMANT_ID ) {
    ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, PROCESS_NOT_STARTED_TEXT,
                 namePtr, "reset");
    return;
  }

  /* process running, set up the callback data */
  cbData = doMalloc(sizeof(dialogCBdata));
  cbData->action    = SUBSYS_HALTING_ID;
  cbData->buttonWid = buttonWid;
  cbData->pos       = 0;
  cbData->rev       = 0;
  strcpy(cbData->namePtr   , namePtr);
  strcpy(cbData->platform  , "");

  ASFlogMessage_direct_CB(ASF_CP, WP_QUESTION_BOX, cbData,
                       REALLY_WANT_TO_HALT_TEXT, cbData->namePtr);


} /* end handleToggleReset.........................*/




/*----------------------------------------------------------
 * NAME:
 *  SendStopMsg 
 *
 * DESCRIPTION:
 *  send the stop message out to the subsystem denoted by
 *  the name "namePtr" -- called after the user has confimed
 *  that he wants to stop a subsytem from the Subsystem Control
 *  panel as well as the "Stop All" menu item
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int SendStopMsg(char *namePtr, int socket, int timeOutFlag)
{
  ODL   writeOutODL;
  XtIntervalId stopID;
  int ok=0;
  char *errmsg;

  /* change the process state to indicate that is terminating */
  /* ChangeProcessState_PIDLIST(namePtr, SUBSYS_STOPPING_ID, NOT_SET_ID); */
  ChangeProcessState_PIDLIST(namePtr, SUBSYS_STOPPING_ID,NORMAL_TERMINATION_ID);

  /* get the stop message to send */
  writeOutODL = (ODL) GetNewMsg("SUBSYSTEM_STOP");
  if (writeOutODL == NULL) {
   printfLLog(LOG_ERR, CANNOT_GET_NEW_ODL, "SUBSYSTEM_STOP");
    return(-1);
  }

                                    /* set up the message source and dest */
  if (ODLSetVal(writeOutODL, errmsg = STOP_HDR_SOURCE, ASF_CP)) {
    if (ODLSetVal(writeOutODL, errmsg = STOP_HDR_DEST, namePtr)) 
      ok = 1;
  }

  if (!ok) {
    ODLFree(writeOutODL);
    printfLLog(LOG_ERR, ERROR_SETTING_MSG);
    printfLLog(LOG_DEBUG, DEB_ERROR_SETTING_MSG, "Subsystem Stop", errmsg);
    return(1);
  }


  /* do the actual write */
  WriteMsgToClient(socket,writeOutODL);
  ODLFree(writeOutODL);

  /* install timeout ? */
  if (timeOutFlag == DO_TIMEOUT) {
   stopID = XtAppAddTimeOut(UxAppContext, getStopTimeInterval(namePtr), 
              (XtTimerCallbackProc)NotifyUserThatProcDidNotStop, namePtr);

   SetTimeOutID_PIDLIST(namePtr, STOP_INTERVAL_ID, stopID);
  }

  return(0);

} /* end SendStopMsg ...................*/



/*----------------------------------------------------------
 * NAME:
 *  doResetAll 
 *
 * DESCRIPTION:
 *  halts all subsytems when the user confirms that all 
 *  subsystems should be halted
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
static void doResetAll()
{
 int i = 0;
 pidListElemType *pidListElemPtr;

  /* get a COPY of the ith element in the pid list  */
  while ((pidListElemPtr = GetCopyOfElemIn_PIDLIST(i++) ) != NULL) {
    if (pidListElemPtr->state == SUBSYS_DORMANT_ID)
      continue;
                         /* check to see if process is a subsystem */
    if (GetSubsystemCategoryGivenName(pidListElemPtr->procNamePtr) != -1) {
      logSubsysControlMessage(pidListElemPtr->procNamePtr, SUBSYS_HALTING_ID);

     /* 
      * if socket is valid, halt
      * NOTE: state is changed in the send message routine
      */
      if (pidListElemPtr->socket != 0)
        SendResetMsg(pidListElemPtr->procNamePtr, pidListElemPtr->socket);

    } /* end if a subsystem */ 

    /* free the copy of the pid list element */
    FreePidListElem_PIDLIST(pidListElemPtr);

  } /* end while */

} /* end doResetAll.........................*/



/*----------------------------------------------------------
 * NAME:
 *  sig_cld
 *
 * DESCRIPTION:
 *  signal handler for when a child process terminates
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void sig_cld()
{
  int pid, status;
  baseListElemType *nPtr;

  nPtr = lockNameList_CORE(SIGCLD_LIST);
  if (nPtr == NULL)
     return;

#ifdef SIG_DEBUG /* this might break things */
printf("entering sig_cld: pidCount %d\n", GLOBAL_signalPidCount);
#endif

  while ((pid = waitpid(ANYKID, &status, WNOHANG)) > 0) {
#ifdef SIG_DEBUG 
printf("sig_cld: waitpid returned pid %d status %d\n", pid, status);
#endif
    GLOBAL_signalPid[GLOBAL_signalPidCount] = pid;
    GLOBAL_signalStatus[GLOBAL_signalPidCount++] = status;
    if (GLOBAL_signalPidCount >= MAX_CHILD_PROCESSES) {
      printfLLog(LOG_DEBUG, 
        "Dropping SIGCLD for pid %d, status %d; more than %d processes exist", 
         pid, status, MAX_CHILD_PROCESSES);
      break;  /* break out of while loop be fore we core dump */
    }
#ifdef SIG_DEBUG  /* this part might cause SIGCLDs to get lost! */
    else
      printf("sig_cld: pidCount %d, added pid %d status %d\n",
              GLOBAL_signalPidCount, pid, status);
#endif
  }
#ifdef SIG_DEBUG
printf("leaving sig_cld\n");
#endif
  unLockNameList_CORE(nPtr);
}


/*----------------------------------------------------------
 * NAME:
 *  do_sig_cld
 *
 * DESCRIPTION:
 *  handle the death of a spawned process for the main thread
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

void do_sig_cld(int pid, int status)
{
 int listenSocket, jobId, qcStatusVal, errorFlag, cat, assocThread;
 char *namePtr, *logLocPtr, subsysNamePtr[MAX_SUBSYS_NAME_LEN], killCmdBuf[256];
 static char *qcStatusPtr;
 qcReqType *qcReqPtr;
 subsysConfigType  *subsysConfigPtr;

#ifdef SIG_DEBUG 
    printfLLog(LOG_DEBUG, ENTER_SIG_CLD, pid, status);
#endif


                /* make sure we still have an entry for this proc */
    namePtr = GetNameGivenPid_PIDLIST(pid);
    if (namePtr == NULL) {
#ifdef SIG_DEBUG
    printf("do_sig_cld: No PIDLIST entry for pid %d exists\n", pid);
#endif

      return;
}
    printfLLog(LOG_DEBUG, SIG_CLD_SHUTDOWN, namePtr, pid);

/* if we're exiting the CP and the read thread still exists, cancel it */
/* if all read threads doesn't get cancelled, the CP can hang at exit */

    if (GLOBAL_exitingCP) {
      printfLLog(LOG_DEBUG, "Exiting CP, check for hanging threads");
      if ((assocThread = GetAssocThreadGivenName_PIDLIST(namePtr)) != 0) {
          printfLLog(LOG_DEBUG, "Cancelling %s thread %d",namePtr,assocThread);
          pthread_cancel(assocThread);
      }
    }
 
    cat = GetSubsystemCategoryGivenName(namePtr);

                /* if NOT the accept thread and NOT a process, just return */
    if (IsPidNoForProcess_PIDLIST(pid) == -1) 
                               /* not a process, the accept thread ? */
      if (strcmp(namePtr, ACCEPT_THREAD) != 0) {
#ifdef SIG_DEBUG
    printf("do_sig_cld: returning because %s did not match %s\n", namePtr, ACCEPT_THREAD);
#endif
        return;
    }

    if (strcmp(namePtr, ACCEPT_THREAD) == 0) {
      funlockfile(stdout);
      funlockfile(stdin);
     
      listenSocket = GetSocketGivenName_PIDLIST(ACCEPT_THREAD);
      close(listenSocket);
     /* 
      * close all open file descriptors
      * for (fd = getdtablesize() - 1; fd >= 0; fd--)
      * (void) close (fd);
     */

      freeCoreSema();
     /* usdetach((usptr_t *)getusPtrHandle()); */

     /* close the syslog */
      closelog();

     /* remove the sgi thread arena files */
      system("/bin/rm -f *arena.*");

     /* remove the log files */
      logLocPtr = (char *)getLogLocation();
      if (isLogFileSyslog(logLocPtr) == FALSE)
        system("/bin/rm -f *.log");

      subsysConfigPtr  = getCPconfig();
      if (subsysConfigPtr == NULL) {
        ASFlogMessage_direct(ASF_CP,WP_ERROR_BOX,CANT_PERFORM_REMOTE_KILL_TEXT);
        return;
      }
      /*sprintf(killCmdBuf, "%s/killall %s", subsysConfigPtr->CPscriptsDirPtr, 
                               namePtr); */
/*nancy: do we need this?  -- if accept thread dies, the CP display doesn't. */
/**** do we always have to do the cp-kill?  seems to work fine without it ...*/
      if (!purify_is_running()) {
#ifdef CP_KILL
        sprintf(killCmdBuf, "%s/cp-kill", subsysConfigPtr->CPscriptsDirPtr);
        printfLLog(LOG_INFO, "CP doing cp-kill, %s, cmd %s\n", 
                             namePtr, killCmdBuf);
        system(killCmdBuf);  
#else
        printfLLog(LOG_INFO, "CP main thread exiting\n");
        if (purify_is_running())
          purify_all_leaks();
        exit(0); 
#endif
      }
      else {
        printfLLog(LOG_INFO, "CP-purify main thread exiting\n");
/*        freeCPconfig(); */
        if (purify_is_running())
          purify_all_leaks();
        exit(0); 
      }
    }

   /*
    *see if this is the GPR utility
    */

    if (strcmp(namePtr, getGPRname()) == 0) {
      printfLLog(LOG_DEBUG, "%s exit status %d", getGPRname(), WEXITSTATUS(status) );
      if (WIFEXITED(status) )  /* if program exited normally */
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);
      else /* abnormal exit status */
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         ABNORMAL_TERMINATION_ID);

 
    }
   /* 
    *see if this is the QC or Scan QC process
    */

    else if (strcmp(namePtr, CP_QC) == 0 ||
             strcmp(namePtr, CP_SCAN_QC) == 0) {
      printfLLog(LOG_DEBUG, QC_STATUS, namePtr, WEXITSTATUS(status) );
      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);

      qcReqPtr =  GetQCreqGivenPid_PIDLIST(pid);
      if (qcReqPtr == NULL) {
#ifdef SIG_DEBUG
    printf("do_sig_cld: QC/Scan QC: cannot get qc reqno for pid %d\n", pid);
#endif
        printfLLog(LOG_ERR, CANNOT_GET_QC_REQNO);
        return;
      }
      strcpy(subsysNamePtr , qcReqPtr->namePtr);
      jobId = qcReqPtr->jobId;
      doFree(qcReqPtr);

      errorFlag = -1;
      switch(WEXITSTATUS(status)) {
        case QC_ACCEPT:
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);
          qcStatusPtr  = QC_ACCEPT_TXT;
          qcStatusVal  = Q_S_IMAGE_QC_ACCEPT_ID;
          errorFlag = 0;
          break;

        case QC_REJECT:
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);
          qcStatusPtr  = QC_REJECT_TXT;
          qcStatusVal  = Q_S_IMAGE_QC_REJECT_ID;
          errorFlag = 0;
          break;

        case QC_HOLD:
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);
          qcStatusPtr  = QC_HOLD_TXT;
          qcStatusVal  =  Q_S_IMAGE_QC_ON_HOLD_ID;
          errorFlag = 0;
          break;

        case QC_HELP:
          qcStatusPtr  = QC_HELP_TXT;
          qcStatusVal  = Q_S_IMAGE_QC_ON_HOLD_ID;
          errorFlag = 0;
          break;

        default:  /* all others : are errors, will put job on hold */
          qcStatusPtr  = getQCstatusAsText(status);
          qcStatusVal  = Q_S_IMAGE_QC_ON_HOLD_ID;
          break;
      } /* end switch */

      printfLLog(LOG_DEBUG, SIGCLD_QC_STATUS, namePtr,jobId, qcStatusVal);
      if (ChangeStatus__MSTRQLIST(jobId, subsysNamePtr, qcStatusVal) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "QC");

      printfLLog(LOG_DEBUG,QC_TERMINATED_WITH_STATUS, namePtr, qcStatusPtr, 
                 qcStatusVal);

      if (qcStatusVal == Q_S_IMAGE_QC_ON_HOLD_ID) /* clear qc for all errors */
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);

      if (errorFlag == -1) {             /* log error for scan qc and qc */
        if (strcmp(namePtr, CP_QC) == 0) /* only pop up error box for QC */
          ASFlogMessage(ASF_CP, WP_ERROR_BOX, QC_HAS_TERMINATED_BECAUSE_TEXT,
                       qcStatusPtr); 
        printfLLog(LOG_INFO, BAD_QC_COMPLETED_PROCESSING_TEXT,
              subsysNamePtr, jobId, qcStatusVal);
      }
#ifdef SIG_DEBUG
    printf("do_sig_cld: returning, CP or Scan QC case\n");
#endif
      RemovePidFromListGivenPid_PIDLIST(pid);
      return; 

    } /* end if strcmp QC app */
   /*
    *see if this is the image averaging utility
    */

    else if (strcmp(namePtr, CP_PRE_QC) == 0 ) {
      printfLLog(LOG_DEBUG, QC_STATUS, namePtr, WEXITSTATUS(status) );
      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);
     qcReqPtr =  GetQCreqGivenPid_PIDLIST(pid);
      if (qcReqPtr == NULL) {
#ifdef SIG_DEBUG
    printf("do_sig_cld: Image Avg: cannot get qc reqno for pid %d\n", pid);
#endif
        printfLLog(LOG_ERR, CANNOT_GET_QC_REQNO);
        return;
      }
      strcpy(subsysNamePtr , qcReqPtr->namePtr);
      jobId = qcReqPtr->jobId;
      doFree(qcReqPtr);

      errorFlag = -1;
      switch(WEXITSTATUS(status)) {
        case 0:
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);
          qcStatusPtr  = QC_ACCEPT_TXT;
          qcStatusVal  = Q_S_IMAGE_QC_READY_ID;
          errorFlag = 0;
          break;

        default:
          ClearQCflag_SYSQUE(subsysNamePtr, jobId);
          qcStatusPtr  = "UNKNOWN_STATE";
          qcStatusVal  = Q_S_IMAGE_QC_READY_ID;
/*          strcpy(qcReqPtr->avgFileBuf, ""); */
          break;
      } /* end switch */

      printfLLog(LOG_DEBUG, SIGCLD_QC_STATUS, namePtr,jobId, qcStatusVal);
      if (ChangeStatus__MSTRQLIST(jobId, subsysNamePtr, qcStatusVal) == -1)
        printfLLog(LOG_ERR, CANNOT_CHANGE_STATUS, "QC");

      RemovePidFromListGivenPid_PIDLIST(pid);

    }

   /*
    *see if this is the Log Browser utility
    */

    else if (strcmp(namePtr, ASF_LOG_BROWSER) == 0) {
      printfLLog(LOG_DEBUG, "Log Browser exit status %d", WEXITSTATUS(status) );
      if (WIFEXITED(status) )  /* if program exited normally */
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);
      else /* abnormal exit status */
        ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         ABNORMAL_TERMINATION_ID);
    }


   /* Process has terminated change the process  --
    * and mark the reason for termination
    * NOTE:: remove process from pid list in 
    * workproc thread
    */
#ifdef STATUS_WORKS  /* status checking only works when the process is */
    if (status != 0) {  /* exec'd on the same machine -- not with rsh! */
     /* 
      * unexpected termination -- log the message in an error box 
      * and mark the abnormal termination
      */

      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                          ABNORMAL_TERMINATION_ID);

      ASFlogMessage(ASF_CP, WP_ERROR_BOX, DIED_UNEXPECTEDLY_TEXT, 
                   GetNameGivenPid_PIDLIST(pid)); 

    } else{
      /* process terminated normally, mark as such */
      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         NORMAL_TERMINATION_ID);


    } /* end if status */
#endif /* STATUS_WORKS */

/* change state to not running and retain the termination reason as
  it was set if the user requested a stop or not */

      ChangeProcessState_PIDLIST(namePtr, SUBSYS_NOT_RUNNING_ID,
                         RETAIN_TERM_REASON);

}  /* end sig_cld ........................*/

/* ########################### TEST CODE ############################## */
#ifdef TABLE_DEBUG  /* this block of code is not used right now */
void resizeExternTable()
{
  int i=0, row, col, numChildren, numCols;
  Dimension wid, ht;
  Widget w[100], rc, child;
  char widName[100];
  short numRows;

printf("entering resizeExternTable\n");
  rc = CPexternRC;
  w[i++] = CPexternBlank_label;
  w[i++] = CPexternNotRunning_label;
  w[i++] = CPexternStarted_label;
  w[i++] = CPexternReady_label;
  w[i++] = CPexternRunning_label;
  w[i++] = CPexternQC_label;
  w[i++] = CPexternHold_label;
  w[i++] = CPexternError_label;

  numCols = XtNumber(states)+1;
  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);

  for (row=0; row < numRows; row++) {
    for (col=0; col < numCols; col++) {
      sprintf(widName, STATE_FORMAT, states[col], row);
      child = XtNameToWidget(rc, widName);
      if (child == NULL)
        continue;
      w[i++] = child;
    }
  }

  numChildren = i;
  printf("resizing %d children\n", numChildren);

  for (i=0; i < numChildren; i++) {
printf("resizing %s\n", XtName(w[i]) );
    XtVaGetValues(w[i], XmNwidth, &wid, XmNheight, &ht, NULL);
printf("resizing %d wid %d h t %d\n", i, wid, ht); fflush(stdout);
    XResizeWindow(XtDisplay(w[i]), XtWindow(w[i]), wid, ht);
    sendEvent(w[i], ConfigureNotify);
  }


}

void printStatusWidths(char *str)
{
  int i=0, row, col, numChildren, numCols;
  Dimension wid;
  Widget w[100], rc, child;
  char widName[100];
  short numRows;

printf("entering printStatusWidths: %s\n", str); fflush(stdout);
  rc = CPstatusRC;
  w[i++] = CPstatusBlank_label;
  w[i++] = CPstatusNotRunning_label;
  w[i++] = CPstatusStarted_label;
  w[i++] = CPstatusReady_label;
  w[i++] = CPstatusRunning_label;
  w[i++] = CPstatusQC_label;
  w[i++] = CPstatusHold_label;
  w[i++] = CPstatusError_label;

  numCols = XtNumber(states)+1;
  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
  printf("printStatusWidths: rows %d cols %d\n", numRows, numCols);
  for (row=0; row < numRows; row++) {
    for (col=0; col < numCols; col++) {
      sprintf(widName, STATE_FORMAT, states[col], row);
      child = XtNameToWidget(rc, widName);
      if (child == NULL) 
        continue;
      w[i++] = child;
    }
  }
  
  numChildren = i;
  printf("found %d children\n", numChildren);

  for (i=0; i < numChildren; i++) {
    XtVaGetValues(w[i], XmNwidth, &wid, NULL);
    printf("%s wid %d\n", XtName(w[i]), wid);
  }

}
void printExternWidths(char *str)
{
  int i=0, row, col, numChildren, numCols;
  Dimension wid;
  Widget w[100], rc, child;
  char widName[100];
  short numRows;

printf("entering printExternWidths: %s\n", str); fflush(stdout);

  rc = CPexternRC;
  w[i++] = CPexternBlank_label;
  w[i++] = CPexternNotRunning_label;
  w[i++] = CPexternStarted_label;
  w[i++] = CPexternReady_label;
  w[i++] = CPexternRunning_label;
  w[i++] = CPexternQC_label;
  w[i++] = CPexternHold_label;
  w[i++] = CPexternError_label;

  numCols = XtNumber(states)+1;
  XtVaGetValues(rc, XmNnumColumns, &numRows, NULL);
  printf("printExternWidths: rows %d cols %d\n", numRows, numCols);
  for (row=0; row < numRows; row++) {
    for (col=0; col < numCols; col++) {
      sprintf(widName, STATE_FORMAT, states[col], row);
      child = XtNameToWidget(rc, widName);
      if (child == NULL) 
        continue;
      w[i++] = child;
    }
  }
  
  numChildren = i;
  printf("found %d children\n", numChildren);

  for (i=0; i < numChildren; i++) {
    XtVaGetValues(w[i], XmNwidth, &wid, NULL);
    printf("%s wid %d\n", XtName(w[i]), wid);
  }

}

#endif  /* TABLE_DEBUG */
/* ########################### END OF TEST CODE ########################### */
