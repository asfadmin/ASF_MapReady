/*-----------------------------------------------------------------------
 *
 * Executable:  CP_scan_qc
 *
 * Function:    Scan scan_results_file for QC
 *
 * Author:      John Ho
 *
 * Date:        12/12/95
 *
 * Notes:       This process provides graphic user interface for a guick
 *              look of scan_results_file so that user can Accept, Hold,
 *              or Reject to store the scan_results_file.
 *
 *
 * CP_scan_qc.1.8.1 4/25/96  add err into syslog
 * CP_scan_qc.1.9.3 5/30/96  handle huge time, info/err dialog for MODE missing
 * CP_scan_qc.1.9.4 6/3/96   save message to syslog, set resources free
 * CP_scan_qc.1.9.5 6/10/96  change display error message and exit message
 *                           return error status for a bad scan results file
 * CP_scan_qc.1.9.6 6/17/96  handle SEGMENT_COUNT and SEGMENTs in SRF
 *                           handle missing MODE in SRF
 * CP_scan_qc.1.9.7 6/19/96  X-resource with CP_Scan_qc, Print_cmd, err-dialog
 *                           size, error prevention
 * CP_scan_qc.2.0   6/21/96  modify help, Makefile, show error dialog,
 * CP_scan_qc.2.1   6/25/96  resources, scrolledBar, -asfcpconfig,
 *                           dependancy, warnings, flashing,...
 * CP_scan_qc.2.2   7/12/96  processor exits if close/quit from window manager
 *                           handle additional RADARSAT modes, fix configfile
 *                           option.
 * CP_scan_qc.2.3   7/19/96  relink with lib and release R1B'
 * CP_scan_qc.2.x   8/28/96  handle invalid SEGMENT START/STOP TIME in SRF
 * CP_scan_qc.2.x   10/30/96 use mkTime instead of mktime from library
 * CP_scan_qc.2.9   12/26/96 allow start time larger than end time
 * CP_scan_qc.2.10  1/14/96  return -1 to all error conditions
 *----------------------------------------------------------------------*/
static char sccsid_scan_qc_proc_c[] = "@(#)scan_qc_proc.c	1.29 97/04/30 14:45:36";

#define SCAN_QC_ON_PROD_INFO       PROGRAM_VERSION

#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/syslog.h>
#include <syslog.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>
#include <Xm/Protocols.h>
#include <Xm/SelectioB.h>
#include "asf.h"
#include "asf_syslog.h"
#include "asfcommon.h"
#include "UxXt.h"
#include "reqErrorDialog.h"
#include "reqHelpDialog.h"
#include "scan_qc_gui.h"
#include "scan_qc_def.h"
#include "version.h"

typedef struct {
               satelliteType      sat;            /* RADARSAT, ERS1, ... */
               stationType        station;        /* ASF, CSA, Wallops */
               stateVectorType    sv;             /* predicted, restituted */
               modeType           mode;           /* continuous, scansar */
               requestType        rt;             /* scan, frame */
               instrumentModeType instrumentMode; /* ST1-ST7, SWA, SWB, SNA */
               frameModeType      frameMode;      /* arctic, antarctic */
               char               scan_results_file[100];
               char               print_cmd[100];
               double             sceneCntLat;
               double             sceneCntLon;
               struct timeval     sceneCntTime;
               struct timeval     segmentStartTime;
               struct timeval     segmentEndTime;
               char               revBuf[REV_LEN+1];
               char               jobidBuf[REV_LEN+1];
               char               seqBuf[SEQ_LEN+1];
              } menuScanQcType;

#define MAX_ERROR_LEN   512
#define MAX_MESSAGE_LEN 2048
#define platformLen     2
#define revLen          5
#define seqLen          2
#define modeLen         3
#define platformStart   0
#define revStart        2
#define seqStart        7
#define modeStart       9

static  char defaultPlatform[revLen+1];
static  char defaultRev[revLen+1];
static  char defaultSeq[revLen+1];
static  char defaultMode[revLen+1];
static  int  preSegmentSelPos = 0;
static  int  curSegmentSelPos = 0;
static  int  scrollSegmentPos = 0;
static  int  scrollSegmentPage = 0;
static  int  preFrameSelPos = 0;
static  int  curFrameSelPos = 0;
static  int  scrollFramePos = 0;
static  int  scrollFramePage = 0;
static  int  Global_error = 0;
static  long datatakeLen;
static  long duration;
static  char GLOBAL_print_cmd[100];

static  struct segment_struct *segmentPtr[64];
static  struct frame_struct   *framePtr[128];
static  menuScanQcType        GLOBAL_ScanQc;

int     handleReqErrorBox(char *format, ...);
int     handleReqHelpBox(char *format, ...);
void    doExit(Widget, XtPointer, XtPointer);
void    moveSegmentScrollBar();
void    moveFrameScrollBar();
void    highlitSegmentList(int);
void    lowdownSegmentList(int);
void    highlitFrameList(int);
void    lowdownFrameList(int);
void    make_announcement(char *, int);
void    printflog(int, char *, ...);

/*---------------------------------------------------------------------
 * NAME:
 *  printflog
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void printflog(int level, char *format, ...)
{
   char scanQcMsg[255];
   char newformat[255];
   va_list  args;

   va_start(args, format);
   sprintf(newformat, "(CP_scan_qc) %s", format);
   vsprintf(scanQcMsg, newformat, args);
   if (level == LOG_ERR)   syslog(LOG_ERR    ,scanQcMsg);
   if (level == LOG_INFO)  syslog(LOG_INFO   ,scanQcMsg);
   if (level == LOG_DEBUG) syslog(LOG_DEBUG  ,scanQcMsg);
   va_end(args);
}


/*--------------------------------------------------------------------- 
 * NAME:
 *  help_cb
 *
 * DESCRIPTION:
 *  The help button in the help menu from the menubar was selected.
 *  Display help information defined above for how to use the program.
 *  This is done by creating a Motif information dialog box.  Again,
 *  make the dialog static so we can reuse it.
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
help_cb(int item_no)
{
  
   if (item_no == 0) {             /* the "help : overview" item */ 
      handleReqHelpBox(SCAN_QC_ON_OVERVIEW);
   }
   else if (item_no == 1) {        /* the "help : using help item */
      handleReqHelpBox(SCAN_QC_ON_USE_HELP);
   }
   else if (item_no == 2) {        /* the "help : product information" item */
      handleReqHelpBox(SCAN_QC_ON_PROD_INFO);
   }
} /* end of help_cb */


/*---------------------------------------------------------------------
 * NAME:
 *  error_cb
 *
 * DESCRIPTION:
 *  The error callback 
 *  Exit with a status defined in qdefines.h 
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
error_cb(Widget widget, XtPointer client_data, XtPointer call_data)
{
   char  logMsg[255];

   if (mode != 1)
      exit(rv);
} /* end of error_cb */


/*---------------------------------------------------------------------
 * NAME:
 *  error_exit
 *
 * DESCRIPTION:
 *  The error and exit function
 *  Exit with a error status defined in qdefines.h
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
error_exit(int retErrStatus, Widget mainIface)
{
   char  logMsg[255];

   Global_error = 1;
   /* set default error for CP_scan_qc */
   sprintf(logMsg, ASSIGN_STATUS_TO_ERROR,
           retErrStatus, GLOBAL_ScanQc.scan_results_file);
   syslog(LOG_ERR, logMsg);

   handleReqErrorBox(CANT_PARSE_CFG_FILE);
   printflog(LOG_ERR, CANT_PARSE_CFG_FILE);

   /* Now show it to stdout */
   fprintf(stdout, "%s\n", logMsg);
/***
   XtPopdown(mainIface);
   exit(retErrStatus);
***/
} /* end of error_exit */


/*---------------------------------------------------------------------
 * NAME:
 *  accept_cb
 *
 * DESCRIPTION:
 *  The accept button's callback 
 *  Exit with a status defined in qdefines.h 
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
accept_cb(Widget widget, XtPointer client_data, XtPointer call_data)
{
   char  logMsg[255];

   sprintf(logMsg, ASSIGN_STATUS_TO_ACCEPT, GLOBAL_ScanQc.scan_results_file);
   syslog(LOG_INFO, logMsg);

   /* Now show it to stdout */
   fprintf(stdout, "%s\n", logMsg);
   fflush(stdout);
   exit(SQC_ACCEPT);
} /* end of accept_cb */


/*---------------------------------------------------------------------
 * NAME:
 *  reject_cb
 *
 * DESCRIPTION:
 *  The reject button's callback 
 *  Exit with a status defined in qdefines.h 
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
reject_cb(Widget widget, XtPointer client_data, XtPointer call_data)
{
   char  logMsg[255];

   sprintf(logMsg, ASSIGN_STATUS_TO_REJECT, GLOBAL_ScanQc.scan_results_file);
   syslog(LOG_INFO, logMsg);

   /* Now show it to stdout */
   fprintf(stdout, "%s\n", logMsg);
   fflush(stdout);
   exit(SQC_REJECT);
} /* end of reject_cb */


/*---------------------------------------------------------------------
 * NAME:
 *  hold_cb
 *
 * DESCRIPTION:
 *  The hold button's callback 
 *  Exit with a status defined in qdefines.h 
 *
 * NOTES:
 *
 *--------------------------------------------------------------------*/
void
hold_cb(Widget widget, XtPointer client_data, XtPointer call_data)
{
   char logMsg[255];

   sprintf(logMsg, ASSIGN_STATUS_TO_HOLD, GLOBAL_ScanQc.scan_results_file);
   syslog(LOG_INFO, logMsg);

   /* Now show it to stdout */
   fprintf(stdout, "%s\n", logMsg);
   fflush(stdout);
   exit(SQC_HOLD);
} /* end of hold_cb */


/*----------------------------------------------------------
 * NAME:
 *  print_cb
 *
 * DESCRIPTION:
 *  call print function which specified by PRINT_COMMAND.
 *  PRINT_COMMAND = "$ASF/bin/xgrabsc -id %d | lpr".
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void print_cb(Widget w, XtPointer client_data, XtPointer cb_arg)
{
   char cmd[256];

   sprintf(cmd, GLOBAL_ScanQc.print_cmd, XtWindow(w));
   printflog(LOG_INFO, PRINT_SCREEN_DUMP, cmd);
   system(cmd);
} /* end of print_cb */


/*----------------------------------------------------------
 * NAME:
 *  scanQcHeaders
 *
 * DESCRIPTION:
 *  based on the name and content of scan_results_file, 
 *  this routine tries to show all the information on 
 *  header of CP_scan_qc window.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void scanQcHeaders()
{
   XmString xmstring, xmstring1;
   int      i;
   Arg      arg[5];
   char     LOCAL_numberOfSequence[sequenceN_SZ+1];
   char     LOCAL_totalIndex[revolutionN_SZ+1];
   char     totalDatatake[20];

   char *satArray[] = {"RADARSAT",
                       "ERS-1",
                       "ERS-2",
                       "JERS-1",
                       ""};

   char *staArray[] = {"ASF", "CSA", "Wallops", "   "};

   char *stateVectorArray[] = {"Predicted", "Restituted"};

   char *modeArray[] = {"ST1", "ST2", "ST3", "ST4", "ST5", "ST6", "ST7",
                        "SWA", "SWB", "SNA", "SNB", 
                        "EH1", "EH2", "EH3", "EH4", "EH5", "EH6",
                        "WD1", "WD2", "WD3",
                        "FN1", "FN2", "FN3", "FN4", "FN5",
                        "EL1", "STD", "UNKNOWN"};

   char *frameModeArray[] = {"Arctic", "Antarctic"};

   xmstring = XmStringCreateSimple(satArray[GLOBAL_ScanQc.sat]);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(platformData, arg, i);
   XmStringFree(xmstring);

   xmstring = XmStringCreateSimple(staArray[GLOBAL_ScanQc.station]);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(stationData, arg, i);
   XmStringFree(xmstring);

   xmstring = XmStringCreateSimple(GLOBAL_ScanQc.revBuf);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(revolutionData, arg, i);
   XmStringFree(xmstring);

   xmstring = XmStringCreateSimple(GLOBAL_ScanQc.jobidBuf);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(jobIDData, arg, i);
   XmStringFree(xmstring);

   xmstring = XmStringCreateSimple(modeArray[GLOBAL_ScanQc.instrumentMode]);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(instrumentModeData, arg, i);
   XmStringFree(xmstring);

   xmstring = XmStringCreateSimple(GLOBAL_ScanQc.seqBuf);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(sequenceData, arg, i);
   XmStringFree(xmstring);

   if (GLOBAL_ScanQc.sat != radarsat)
      xmstring = XmStringCreateSimple("100");
   else if (GLOBAL_ScanQc.instrumentMode == st1 ||
            GLOBAL_ScanQc.instrumentMode == st2 ||
            GLOBAL_ScanQc.instrumentMode == st3 ||
            GLOBAL_ScanQc.instrumentMode == st4 ||
            GLOBAL_ScanQc.instrumentMode == st5 ||
            GLOBAL_ScanQc.instrumentMode == st6 ||
            GLOBAL_ScanQc.instrumentMode == st7) {
      xmstring = XmStringCreateSimple("100");
   }
   else if (GLOBAL_ScanQc.instrumentMode == ss1 ||
            GLOBAL_ScanQc.instrumentMode == ss2) {
      xmstring = XmStringCreateSimple("500");
   }
   else if (GLOBAL_ScanQc.instrumentMode == ss3 ||
            GLOBAL_ScanQc.instrumentMode == ss4) {
      xmstring = XmStringCreateSimple("300");
   }
   else {
      xmstring = XmStringCreateSimple("100");
   }

   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(frameSizeData, arg, i);
   XmStringFree(xmstring);

   if (GLOBAL_ScanQc.sat != radarsat) {
      xmstring = XmStringCreateSimple("15");
      xmstring1 = XmStringCreateSimple("30");
   }
   else if (GLOBAL_ScanQc.instrumentMode == st1 ||
            GLOBAL_ScanQc.instrumentMode == st2 ||
            GLOBAL_ScanQc.instrumentMode == st3 ||
            GLOBAL_ScanQc.instrumentMode == st4 ||
            GLOBAL_ScanQc.instrumentMode == st5 ||
            GLOBAL_ScanQc.instrumentMode == st6 ||
            GLOBAL_ScanQc.instrumentMode == st7) {
      xmstring = XmStringCreateSimple("17");
      xmstring1 = XmStringCreateSimple("30");
   }
   else if (GLOBAL_ScanQc.instrumentMode == ss1 ||
            GLOBAL_ScanQc.instrumentMode == ss2) {
      xmstring = XmStringCreateSimple("75");
      xmstring1 = XmStringCreateSimple("150");
   }
   else if (GLOBAL_ScanQc.instrumentMode == ss3 ||
            GLOBAL_ScanQc.instrumentMode == ss4) {
      xmstring = XmStringCreateSimple("45");
      xmstring1 = XmStringCreateSimple("90");
   }
   else {
      xmstring = XmStringCreateSimple("15");
      xmstring1 = XmStringCreateSimple("30");
   }

   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(nominalFrameData, arg, i);
   XmStringFree(xmstring);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring1); i++;
   XtSetValues(nominalSegmentData, arg, i);
   XmStringFree(xmstring1);

   sprintf(totalDatatake, "%d", datatakeLen);
   xmstring = XmStringCreateSimple(totalDatatake);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(totalDatatakeData, arg, i);
   XmStringFree(xmstring);

} /* end of scanQcHeaders.......................*/


/*----------------------------------------------------------
 * NAME:
 *  checkIfFileExists
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int checkIfFileExists(char *fileNamePtr)
{
   struct   stat fs;
   int      retval, len, i, j;
   char     tmpBuff[100];

   len = strlen(fileNamePtr);
   for (i=0; i<len; i++) {
       if (fileNamePtr[i] == ':') { /* hostname ? */
          i++;
          for (j=0; i<len; ) {
             fileNamePtr[j++] = fileNamePtr[i++];
          }
          fileNamePtr[j] = '\0';
       }
   }

   /* check if file exist */
   retval = stat(fileNamePtr, &fs);
   if (retval == -1) {
      sprintf(tmpBuff, CANT_ACCESS_FILE, fileNamePtr);
      handleReqErrorBox(tmpBuff);
      printflog(LOG_ERR, tmpBuff);
      return(-1);
   }
   return(0);
} /* end of checkIfFileExists */


/*----------------------------------------------------------
 * NAME:
 * handleReqHelpBox
 *
 * DESCRIPTION:
 *  format the text using a format statement and va_args,
 *  create the request utility's info box, with the
 *  text and pop it up on the screen
 *
 * NOTES:
 *
 * Assuming that the arguments are less than 512 bytes in length total
 * is probably safe, but it is not the most robust way to handle this
 * situation.  Unfortunately, both varargs(3) and stdarg(3) require the
 * calling routine to specify the number of arguments passed in.  For
 * example, a better method of handling the parameters would be to either
 * (1) parse the format to determine the number of parameters that follow;
 * or (2) require the calling routine to pass in NULL as the last parameter.
 * Neither method is foolproof, though.
 *
 *---------------------------------------------------------*/
int handleReqHelpBox(char *format, ...)
{
   Widget   reqHelpBox;
   XmString xmstring;
   va_list  args;
   Arg      xtargs[5];
   char     *messagePtr;
   int      i;

   if ((messagePtr = (char *) doMalloc(MAX_MESSAGE_LEN)) != NULL) {
      va_start(args, format);
      vsprintf(messagePtr, format, args);
      reqHelpBox = (Widget) create_reqHelpDialog(UxTopLevel);
      xmstring = XmStringCreateLtoR(messagePtr, XmFONTLIST_DEFAULT_TAG);
      i = 0;
      XtSetArg(xtargs[i], XmNmessageString, xmstring); i++;
      XtSetValues(reqHelpBox, xtargs, i);
      XmStringFree(xmstring);

      XBell(XtDisplay(UxTopLevel), 25);
      UxPopupInterface(reqHelpBox, no_grab);

      doFree(messagePtr);
      va_end(args);
      return(0);
   }

   return(-1);
} /* end of handleReqHelpBox..............................*/


/*----------------------------------------------------------
 * NAME:
 *  handleReqErrorBox
 *
 * DESCRIPTION:
 *  format the text using a format statement and va_args,
 *  create the request utility's error box, with the
 *  text and pop it up on the screen
 *
 * NOTES:
 *
 *  Assuming that the arguments are less than 512 bytes in length total
 *  is probably safe, but it is not the most robust way to handle this
 *  situation.  Unfortunately, both varargs(3) and stdarg(3) require the
 *  calling routine to specify the number of arguments passed in.  For
 *  example, a better method of handling the parameters would be to either
 *  (1) parse the format to determine the number of parameters that follow;
 *  or (2) require the calling routine to pass in NULL as the last parameter.
 *  Neither method is foolproof, though.
 *
 *---------------------------------------------------------*/
int handleReqErrorBox(char *format, ...)
{
   Widget   reqErrorBox;
   XmString xmstring;
   va_list  args;
   Arg      xtargs[5];
   char     *messagePtr;
   int      i, messageLen, msgWidth;

   if ((messagePtr = (char *) doMalloc(MAX_ERROR_LEN)) != NULL) {
      va_start(args, format);
      vsprintf(messagePtr, format, args);
      messageLen = strlen(messagePtr) + 1;
      if (messageLen < 40)
         msgWidth = 400;
      else if (messageLen < 60)
         msgWidth = 500;
      else if (messageLen < 80)
         msgWidth = 600;
      else
         msgWidth = 800;

      reqErrorBox = create_reqErrorDialog(UxTopLevel);
      xmstring = XmStringCreateLtoR(messagePtr, XmFONTLIST_DEFAULT_TAG);
      i = 0;
      XtSetArg(xtargs[i], XmNmessageString, xmstring); i++;
      XtSetValues(reqErrorBox, xtargs, i);
      XmStringFree(xmstring);

      i = 0;
      XtSetArg(xtargs[i], XmNwidth, msgWidth); i++;
      XtSetValues(reqErrorBox, xtargs, i);

      XBell(XtDisplay(UxTopLevel), 25);
      UxPopupInterface(reqErrorBox, no_grab);

      fprintf(stdout, "%s\n", messagePtr);
      fflush(stdout);
      doFree(messagePtr);
      va_end(args);
      return(0);
   }
   return(-1);
} /* end of handleReqErrorBox..............................*/

/*----------------------------------------------------------
 * NAME:
 *  make_announcement
 *
 * DESCRIPTION:
 *  write message to standard output, also to LOG if specified.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void
make_announcement(char *announcement_string, int log_option)
{

   if (log_option == LOG) {
      /* First, syslog the error */
      syslog(LOG_ERR, announcement_string);
   }
   fprintf(stdout, "%s\n", announcement_string);
   fflush(stdout);
}


/*----------------------------------------------------------
 * NAME:
 *  doExit
 *
 * DESCRIPTION:
 *  exit the CP_scan_qc application with return value 0
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void doExit(Widget w, XtPointer dPtr, XtPointer cPtr)
{
    char  logMsg[255];
 
    sprintf(logMsg, ASSIGN_STATUS_TO_HOLD, GLOBAL_ScanQc.scan_results_file);
    syslog(LOG_INFO, logMsg);
 
    /* Now show it to stdout */
    fprintf(stdout, "%s\n", logMsg);
 
    fflush(stdout);
    exit(SQC_HOLD);

} /* end of doExit..........................................*/

/*----------------------------------------------------------
 * NAME:
 *  addOneSecond
 *
 * DESCRIPTION:
 *  increment one second on the GMT_t inout time.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void addOneSecond(GMT_t *updateTime)
{
   if ((updateTime->tm.tm_sec++) == 60) {
      updateTime->tm.tm_sec = 0;
      if ((updateTime->tm.tm_min++) == 60) {
         updateTime->tm.tm_min = 0;
         if ((updateTime->tm.tm_hour++) == 24) {
            updateTime->tm.tm_hour = 0;
            if ((updateTime->tm.tm_mday++) == 365) {
               if (updateTime->tm.tm_mday % 4 == 0   && 
                   updateTime->tm.tm_mday % 100 != 0 || 
                   updateTime->tm.tm_mday % 400 == 0) {
                  return;
               }
               else {
                  updateTime->tm.tm_mday = 0;
                  updateTime->tm.tm_year++;
               }
            }
         }
      }
   }       
}

/*----------------------------------------------------------
 * NAME:
 *  setFrameScrollList
 *
 * DESCRIPTION:
 *  open frame file as selected, setup the Frame Scroll Lists.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int setFrameScrollList()
{
   int      fd, i, j, k, *frame_id, err;
   ODL      odl, *bodyODL, *segmtODL, *imageODL;
   char     segmentStrN[6], errstr[256];
   Arg      arg[5];
   double   *scene_cnt_lat;
   double   *scene_cnt_lon;
   GMT_t    *scene_cnt_time;
   struct   stat stbuf;
   char     tmpSegmentID[segmentID_SZ];
   char     tmpFrameID[frameID_SZ];
   char     tmpCenterLat[centerLat_SZ];
   char     tmpCenterLon[centerLon_SZ];
   char     tmpCenterTime[centerTime_SZ];
   XmString xmstring;
   int      value, slide, inc, page;

   if ((odl = ODLparse(GLOBAL_ScanQc.scan_results_file, 0, errstr)) == NULL) {
      handleReqErrorBox(CANT_OPEN_SCAN_RESULT_FILE, 
                GLOBAL_ScanQc.scan_results_file, errstr);
      printflog(LOG_ERR, CANT_OPEN_SCAN_RESULT_FILE, 
                GLOBAL_ScanQc.scan_results_file, errstr);
      return(SQC_PARSE_SRF_ERROR);
   }
   /* add up all the frame number in this frame file */
   frameN = 0;

   if ((bodyODL = (ODL*) Val(Lookup(odl, "SCAN_RESULTS_FILE.BODY"))) == NULL) {
      handleReqErrorBox(CANT_FIND_SRF_BODY, GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, CANT_FIND_SRF_BODY, GLOBAL_ScanQc.scan_results_file);
      ODLFree(odl);
      return(SQC_BODY_SRF_ERROR);
   }
   if (segmentN = ODLGetInt(odl, 
                  "SCAN_RESULTS_FILE.BODY.SEGMENT_COUNT", &err), err == -1) {
      handleReqErrorBox(CANT_FIND_SEGMENT_COUNT);
      printflog(LOG_ERR, CANT_FIND_SEGMENT_COUNT);
      ODLFree(odl);
      return(SQC_SEG_CNT_SRF_ERROR);
   }

   /* set the segment count */
   sprintf(segmentStrN, "%d", segmentN);
   xmstring = XmStringCreateSimple(segmentStrN);
   i = 0;
   XtSetArg(arg[i], XmNlabelString, xmstring); i++;
   XtSetValues(segmentNbrData, arg, i);
   XmStringFree(xmstring);

   for (i=0; bodyODL[i]!=NULL; ++i) {
      /* find SEGMENT */
      if (strcasecmp(Name(bodyODL[i]), SEGMENT) ||
          (segmtODL = (ODL*) Val(bodyODL[i])) == NULL) {
         continue;
      }
      segment_ID++;
      for (j=0; segmtODL[j]!=NULL; j++) {
         /* find FRAME */
         if (strcasecmp(Name(segmtODL[j]), FRAME) ||
             (imageODL = (ODL*) Val(segmtODL[j])) == NULL)
            continue;
         else
            frameN++;
         /* find FRAME_ID from FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), FRAME_ID) == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            handleReqErrorBox(CANT_FIND_FRAME_ID);
            printflog(LOG_ERR, CANT_FIND_FRAME_ID);
            ODLFree(odl);
            return(SQC_FRM_ID_SRF_ERROR);
         }
         frame_id = (int*) Val(imageODL[k]);
 
         /* find SCENE_CENTER_LAT from FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), SCENE_CENTER_LAT) == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            handleReqErrorBox(CANT_FIND_SCENE_CENTER_LAT);
            printflog(LOG_ERR, CANT_FIND_SCENE_CENTER_LAT);
            ODLFree(odl);
            return(SQC_SCLAT_SRF_ERROR);
         }
         scene_cnt_lat = (double*) Val(imageODL[k]);
 
         /* find SCENE_CENTER_LON from FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), SCENE_CENTER_LON) == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            handleReqErrorBox(CANT_FIND_SCENE_CENTER_LON);
            printflog(LOG_ERR, CANT_FIND_SCENE_CENTER_LON);
            ODLFree(odl);
            return(SQC_SCLON_SRF_ERROR);
         }
         scene_cnt_lon = (double*) Val(imageODL[k]);
 
         /* find SCENE_CENTER_TIME from FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), SCENE_CENTER_TIME) == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            handleReqErrorBox(CANT_FIND_SCENE_CENTER_TIME);
            printflog(LOG_ERR, CANT_FIND_SCENE_CENTER_TIME);
            ODLFree(odl);
            return(SQC_SCTM_SRF_ERROR);
         }
         scene_cnt_time = (GMT_t*) Val(imageODL[k]);
         if (scene_cnt_time == NULL) {
            handleReqErrorBox(INVALID_SCENE_CENTER_TIME);
            printflog(LOG_ERR, INVALID_SCENE_CENTER_TIME);
            return(SQC_IVL_CNT_TM_ERROR);
         }

         if (scene_cnt_time->tv_usec >= 400000) {
            addOneSecond(scene_cnt_time);
         }
 
         /* setup entries for select frame scrolllists */
         sprintf(tmpSegmentID, "%d", segment_ID);
         sprintf(tmpFrameID, "%d", *frame_id);
         sprintf(tmpCenterLat, "%.3f", *scene_cnt_lat);
         sprintf(tmpCenterLon, "%.3f", *scene_cnt_lon);
         sprintf(tmpCenterTime, "%d-%03dT%02d:%02d:%02d", 
            (scene_cnt_time->tm.tm_year>80 ? scene_cnt_time->tm.tm_year+1900
             :scene_cnt_time->tm.tm_year+2000),
            scene_cnt_time->tm.tm_mday, scene_cnt_time->tm.tm_hour, 
            scene_cnt_time->tm.tm_min, scene_cnt_time->tm.tm_sec);

         if ((framePtr[frameN-1]=malloc(sizeof(struct frame_struct))) != NULL) {
            strcpy(framePtr[frameN-1]->segmentID, tmpSegmentID);
            strcpy(framePtr[frameN-1]->frameID, tmpFrameID);
            strcpy(framePtr[frameN-1]->centerLat, tmpCenterLat);
            strcpy(framePtr[frameN-1]->centerLon, tmpCenterLon);
            strcpy(framePtr[frameN-1]->centerTime, tmpCenterTime);
         }
         else {
            handleReqErrorBox(CANT_MALLOC_FRAME_ENTRY);
            printflog(LOG_ERR, CANT_MALLOC_FRAME_ENTRY);
            ODLFree(odl);
            return(SQC_MALLOC_FRM_ERROR);
         }
      }
   }

   if (segmentN < 0) {
      handleReqErrorBox(INCORRECT_SEGMENT_COUNT,
                        GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, INCORRECT_SEGMENT_COUNT,
                        GLOBAL_ScanQc.scan_results_file);
      return(SQC_BAD_SEG_CNT_ERROR);
   }

   if (segmentN > segment_ID) {
      handleReqErrorBox(SEGMENT_COUNT_LARGER, GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, SEGMENT_COUNT_LARGER, GLOBAL_ScanQc.scan_results_file);
      return(SQC_SEG_CNT_LARG_ERROR);
   }

   if (segmentN < segment_ID) {
      handleReqErrorBox(SEGMENT_COUNT_SMALLER, 
                        GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, SEGMENT_COUNT_SMALLER, 
                        GLOBAL_ScanQc.scan_results_file);
      return(SQC_SEG_CNT_LESS_ERROR);
   }

   /* set the maximum value of scroll bar */
   XmScrollBarGetValues(frameScrollBar, &value, &slide, &inc, &page);
   value = 0;
   XmScrollBarSetValues(frameScrollBar, value, slide, inc, page, False);

   i = 0;
   if (frameN == 0)
      XtSetArg(arg[i], XmNmaximum, 1);
   else
      XtSetArg(arg[i], XmNmaximum, frameN);
   i++;
   if (frameN == 0)
      XtSetArg(arg[i], XmNsliderSize, 1);
   else if (frameN < 10)
      XtSetArg(arg[i], XmNsliderSize, frameN);
   else
      XtSetArg(arg[i], XmNsliderSize, 10);
   i++;
   XtSetValues(frameScrollBar, arg, i);

   preFrameSelPos = curFrameSelPos = 0;    /* 0 denotes "no selection yet" */
   scrollFramePos = 0;

   /* XmScrollBarGetValues */
   moveFrameScrollBar();

   ODLFree(odl);
   return(0);

} /* end of setFrameScrollList................*/


/*----------------------------------------------------------
 * NAME:
 *  moveFrameScrollBar
 *
 * DESCRIPTION:
 *  delete all items in lists, set up previous/current
 *  positions, then low down previous selection and
 *  highlight current selection.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void moveFrameScrollBar()
{
   int      value, slide, inc, page;
   int      i, j;
   char     tmpTime[24];
   XmString xmstring;

   /* XmListDeleteAllItems */
   XmListDeleteAllItems(frameSegmentScrollList);
   XmListDeleteAllItems(frameFrameScrollList);
   XmListDeleteAllItems(frameCenterLatScrollList);
   XmListDeleteAllItems(frameCenterLonScrollList);
   XmListDeleteAllItems(frameCenterTimeScrollList);

   XmScrollBarGetValues(frameScrollBar, &value, &slide, &inc, &page);
   scrollFramePos = value;
   scrollFramePage = page;

   /* XmListAddItemUnselected */
   for (i=1, j=value; j<value+(frameN>page?page:frameN); i++, j++) {
      xmstring = XmStringCreateSimple(framePtr[j]->segmentID);
      XmListAddItemUnselected(frameSegmentScrollList, xmstring, i);
      XmStringFree(xmstring);
      xmstring = XmStringCreateSimple(framePtr[j]->frameID);
      XmListAddItemUnselected(frameFrameScrollList, xmstring, i);
      XmStringFree(xmstring);
      xmstring = XmStringCreateSimple(framePtr[j]->centerLat);
      XmListAddItemUnselected(frameCenterLatScrollList, xmstring, i);
      XmStringFree(xmstring);
      xmstring = XmStringCreateSimple(framePtr[j]->centerLon);
      XmListAddItemUnselected(frameCenterLonScrollList, xmstring, i);
      XmStringFree(xmstring);
      strcpy(tmpTime, framePtr[j]->centerTime);
      tmpTime[8] = ':';
      xmstring = XmStringCreateSimple(tmpTime);
      XmListAddItemUnselected(frameCenterTimeScrollList, xmstring, i);
      XmStringFree(xmstring);
   }

   /* highlight the selected row if within the list */
   if (curFrameSelPos > 0) {
      i = curFrameSelPos - scrollFramePos;
      if (i> 0 && i <= scrollFramePage)
         highlitFrameList(i); 
   }

} /* end of moveFrameScrollBar */


/*----------------------------------------------------------
 * NAME:
 *  pickFrameListEntry(int)
 *
 * DESCRIPTION:
 *  free memory allocation, then switch windows
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void pickFrameListEntry(int list)
{
   int i = 0;

   if (list == segmentList) {
      i = XmListGetKbdItemPos(frameSegmentScrollList);
   }
   else if (list == frameList) {
      i = XmListGetKbdItemPos(frameFrameScrollList);
   }
   else if (list == latitudeList) {
      i = XmListGetKbdItemPos(frameCenterLatScrollList);
   }
   else if (list == longitudeList) {
      i = XmListGetKbdItemPos(frameCenterLonScrollList);
   }
   else if (list == timeList) {
      i = XmListGetKbdItemPos(frameCenterTimeScrollList);
   }

   preFrameSelPos = curFrameSelPos;
   curFrameSelPos = i + scrollFramePos;

   /* low down previous highlight if existed */
   i = preFrameSelPos - scrollFramePos;
   if (i > 0 && i <= scrollFramePage) {
      lowdownFrameList(i);
   }

   /* highlight the new selection */
   i = curFrameSelPos - scrollFramePos;
   highlitFrameList(i);

} /* end of pickFrameListEntry..........................*/


/*----------------------------------------------------------
 * NAME:
 *  highlitFrameList
 *
 * DESCRIPTION:
 *  highlight the selected entry in all the lists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void highlitFrameList(int item)
{
   XmListSelectPos(frameSegmentScrollList, item, NULL);
   XmListSelectPos(frameFrameScrollList, item, NULL);
   XmListSelectPos(frameCenterLatScrollList, item, NULL);
   XmListSelectPos(frameCenterLonScrollList, item, NULL);
   XmListSelectPos(frameCenterTimeScrollList, item, NULL);
}


/*----------------------------------------------------------
 * NAME:
 *  lowdownFrameList
 *
 * DESCRIPTION:
 *  de-select the previous selected entry in all the lists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void lowdownFrameList(int item)
{
   XmListDeselectPos(frameSegmentScrollList, item);
   XmListDeselectPos(frameFrameScrollList, item);
   XmListDeselectPos(frameCenterLatScrollList, item);
   XmListDeselectPos(frameCenterLonScrollList, item);
   XmListDeselectPos(frameCenterTimeScrollList, item);
}


/*----------------------------------------------------------
 * NAME:
 *  setSegmentScrollList
 *
 * DESCRIPTION:
 *  open scan results file, setup the Segment Scroll Lists.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int setSegmentScrollList()
{
   int      fd, i, j, k, *frame_id, err;
   int      *segment_id = 0;
   int      seg_id = 0;
   int      seqN, jobidN, revN, retval;
   ODL      odl, *bodyODL, *segmtODL;
   char     *buf;
   char     undefined[10] = "undefined";
   char     unknown[10] = "UNKNOWN";
   char     segmentStrN[6], errstr[256];
   Arg      arg[5];
   GMT_t    *scan_start_time;
   GMT_t    *scan_stop_time;
   GMT_t    *segment_start_time;
   GMT_t    *segment_stop_time;
   struct   stat stbuf;
   char     tmpSegmentID[segmentID_SZ];
   char     tmpFrameID[frameID_SZ];
   char     tmpDuration[duration_SZ];
   char     tmpStartTime[centerTime_SZ];
   char     tmpStopTime[centerTime_SZ];
   int      value, slide, inc, page;
   XmString xmstring;

   if ((odl = ODLparse(GLOBAL_ScanQc.scan_results_file, 0, errstr)) == NULL) {
      handleReqErrorBox(CANT_OPEN_SCAN_RESULT_FILE, 
                GLOBAL_ScanQc.scan_results_file, errstr);
      printflog(LOG_ERR, CANT_OPEN_SCAN_RESULT_FILE, 
                GLOBAL_ScanQc.scan_results_file, errstr);
      return(SQC_PARSE_SRF_ERROR);
   }
   /* add up all the segment number in this file */
   segmentN = 0;

   if ((bodyODL = (ODL*) Val(Lookup(odl, "SCAN_RESULTS_FILE.BODY"))) == NULL) {
      handleReqErrorBox(CANT_FIND_SRF_BODY, GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, CANT_FIND_SRF_BODY, GLOBAL_ScanQc.scan_results_file);
      ODLFree(odl);
      return(SQC_BODY_SRF_ERROR);
   }

   /* get and set the platform */
   if (buf = ODLGetString(odl, "SCAN_RESULTS_FILE.BODY.PLATFORM", &err),
      err == -1) {
      handleReqErrorBox(CANT_FIND_PLATFORM);
      printflog(LOG_ERR, CANT_FIND_PLATFORM);
      buf = &defaultPlatform[0];
   }
   if (strcmp(buf, "R1") == NULL) {
      GLOBAL_ScanQc.sat = radarsat;
   }
   else if (strcmp(buf, "E1") == NULL) {
      GLOBAL_ScanQc.sat = ers_1;
   }
   else if (strcmp(buf, "E2") == NULL) {
      GLOBAL_ScanQc.sat = ers_2;
   }
   else if (strcmp(buf, "J1") == NULL) {
      GLOBAL_ScanQc.sat = jers_1;
   }
   else if (strcmp(buf, "SEA") == NULL) {
      GLOBAL_ScanQc.sat = seasat;
   }
   else {
      handleReqErrorBox(CANT_FIND_PLAT_NAME, GLOBAL_ScanQc.scan_results_file);
      printflog(LOG_ERR, CANT_FIND_PLAT_NAME, GLOBAL_ScanQc.scan_results_file);
      ODLFree(odl);
      return(SQC_PLATFM_SRF_ERROR);
   }

   /* get and set the station */
   if (buf = ODLGetString(odl, "SCAN_RESULTS_FILE.BODY.STATION_ID", &err),
      err == -1) {
      handleReqErrorBox(CANT_FIND_STATION_ID);
      printflog(LOG_ERR, CANT_FIND_STATION_ID);
      ODLFree(odl);
      return(SQC_STATION_SRF_ERROR);
   }
   if (strcmp(buf, "FA") == NULL) {
      GLOBAL_ScanQc.station = asf;
   }
   else if (strcmp(buf, "MC") == NULL) {
      GLOBAL_ScanQc.station = wallops;
   }

   /* get and set the segment count */
   if (segmentN = ODLGetInt(odl, 
                  "SCAN_RESULTS_FILE.BODY.SEGMENT_COUNT", &err), err == -1) {
      handleReqErrorBox(CANT_FIND_SEGMENT_COUNT);
      printflog(LOG_ERR, CANT_FIND_SEGMENT_COUNT);
      buf = &undefined[0];
   }
   sprintf(segmentStrN, "%d", segmentN);

   /* get and set the instrument mode */
   mode = 0;
   if (buf = ODLGetString(odl, "SCAN_RESULTS_FILE.BODY.SEGMENT.MODE", &err),
      err == -1) {
      mode = 1;
      if (segmentN == 0) {
         handleReqErrorBox(CANT_FIND_SEGMENT_IN_SCAN, 
                           GLOBAL_ScanQc.scan_results_file);
         printflog(LOG_ERR, CANT_FIND_SEGMENT_IN_SCAN,
                           GLOBAL_ScanQc.scan_results_file);
         buf = &defaultMode[0];
      }
      else {
         handleReqErrorBox(CANT_FIND_MODE_FROM_DATATAKE);
         printflog(LOG_ERR, CANT_FIND_MODE_FROM_DATATAKE);
         buf = &unknown[0];
      }
      i = 0;
      XtSetArg(arg[i], XmNx, 90); i++;
      XtSetValues(backSelectFrame, arg,i);
      XtSetArg(arg[i], XmNy, 770); i++; 
      XtSetValues(backSelectFrame, arg,i);
   }
   if (strcmp(buf, "ST1") == 0)
      GLOBAL_ScanQc.instrumentMode = st1;
   else if (strcmp(buf, "ST2") == 0)
      GLOBAL_ScanQc.instrumentMode = st2;
   else if (strcmp(buf, "ST3") == 0)
      GLOBAL_ScanQc.instrumentMode = st3;
   else if (strcmp(buf, "ST4") == 0)
      GLOBAL_ScanQc.instrumentMode = st4;
   else if (strcmp(buf, "ST5") == 0)
      GLOBAL_ScanQc.instrumentMode = st5;
   else if (strcmp(buf, "ST6") == 0)
      GLOBAL_ScanQc.instrumentMode = st6;
   else if (strcmp(buf, "ST7") == 0)
      GLOBAL_ScanQc.instrumentMode = st7;
   else if (strcmp(buf, "SWA") == 0)
      GLOBAL_ScanQc.instrumentMode = ss1;
   else if (strcmp(buf, "SWB") == 0)
      GLOBAL_ScanQc.instrumentMode = ss2;
   else if (strcmp(buf, "SNA") == 0)
      GLOBAL_ScanQc.instrumentMode = ss3;
   else if (strcmp(buf, "SNB") == 0)
      GLOBAL_ScanQc.instrumentMode = ss4;
   else if (strcmp(buf, "EH1") == 0)
      GLOBAL_ScanQc.instrumentMode = eh1;
   else if (strcmp(buf, "EH2") == 0)
      GLOBAL_ScanQc.instrumentMode = eh2;
   else if (strcmp(buf, "EH3") == 0)
      GLOBAL_ScanQc.instrumentMode = eh3;
   else if (strcmp(buf, "EH4") == 0)
      GLOBAL_ScanQc.instrumentMode = eh4;
   else if (strcmp(buf, "EH5") == 0)
      GLOBAL_ScanQc.instrumentMode = eh5;
   else if (strcmp(buf, "EH6") == 0)
      GLOBAL_ScanQc.instrumentMode = eh6;
   else if (strcmp(buf, "WD1") == 0)
      GLOBAL_ScanQc.instrumentMode = wd1;
   else if (strcmp(buf, "WD2") == 0)
      GLOBAL_ScanQc.instrumentMode = wd2;
   else if (strcmp(buf, "WD3") == 0)
      GLOBAL_ScanQc.instrumentMode = wd3;
   else if (strcmp(buf, "FN1") == 0)
      GLOBAL_ScanQc.instrumentMode = fn1;
   else if (strcmp(buf, "FN2") == 0)
      GLOBAL_ScanQc.instrumentMode = fn2;
   else if (strcmp(buf, "FN3") == 0)
      GLOBAL_ScanQc.instrumentMode = fn3;
   else if (strcmp(buf, "FN4") == 0)
      GLOBAL_ScanQc.instrumentMode = fn4;
   else if (strcmp(buf, "FN5") == 0)
      GLOBAL_ScanQc.instrumentMode = fn5;
   else if (strcmp(buf, "EL1") == 0)
      GLOBAL_ScanQc.instrumentMode = el1;
   else if (strcmp(buf, "STD") == 0)
      GLOBAL_ScanQc.instrumentMode = std_e;
   else
      GLOBAL_ScanQc.instrumentMode = 27;

   /* get and set the frame mode */
   if (buf = ODLGetString(odl, "SCAN_RESULTS_FILE.BODY.FRAME_MODE", &err),
      err == -1) {
      handleReqErrorBox(CANT_FIND_FRAME_MODE);
      printflog(LOG_ERR, CANT_FIND_FRAME_MODE);
      return(SQC_FRM_MD_SRF_ERROR);
   }
   if (strcmp(buf, "ARCTIC") == 0)
      GLOBAL_ScanQc.frameMode = arctic;
   else if (strcmp(buf, "ANTARCTIC") == 0)
      GLOBAL_ScanQc.frameMode = antarctic;

   /* get and set the revolution */
   if (revN = ODLGetInt(odl, 
              "SCAN_RESULTS_FILE.BODY.REVOLUTION", &err), err == -1) {
      handleReqErrorBox(CANT_FIND_REVOLUTION);
      printflog(LOG_ERR, CANT_FIND_REVOLUTION);
      return(SQC_REV_SRF_ERROR);
   }
   sprintf(GLOBAL_ScanQc.revBuf, "%d", revN);

   /* get and set the job id */
   if (jobidN = ODLGetInt(odl,
              "SCAN_RESULTS_FILE.BODY.JOB_ID", &err), err == -1) {
      handleReqErrorBox(CANT_FIND_JOB_ID);
      printflog(LOG_ERR, CANT_FIND_JOB_ID);
      return(SQC_JOBID_SRF_ERROR);
   }
   sprintf(GLOBAL_ScanQc.jobidBuf, "%d", jobidN);

   /* get and set the segment count */
   if (seqN = ODLGetInt(odl, 
              "SCAN_RESULTS_FILE.BODY.SEQUENCE", &err), err == -1) {
      handleReqErrorBox(CANT_FIND_SEQUENCE);
      printflog(LOG_ERR, CANT_FIND_SEQUENCE);
      return(SQC_SEQ_SRF_ERROR);
   }
   sprintf(GLOBAL_ScanQc.seqBuf, "%d", seqN);

   /* get and set the start time */
   for (j=0; bodyODL[j]!=NULL; j++) {
      if (strcasecmp(Name(bodyODL[j]), "START_TIME") == 0)
         break;
   }
   if (bodyODL[j] == NULL) {
      handleReqErrorBox(CANT_FIND_SCAN_START_TIME);
      printflog(LOG_ERR, CANT_FIND_SCAN_START_TIME);
      return(SQC_SRT_TM_SRF_ERROR);
   }
   scan_start_time = (GMT_t*) Val(bodyODL[j]);
   if (scan_start_time == NULL) {
      handleReqErrorBox(INVALID_SCANT_START_TIME);
      printflog(LOG_ERR, INVALID_SCANT_START_TIME);
      return(SQC_IVL_SCN_SRT_ERROR);
   }

   for (j=0; bodyODL[j]!=NULL; j++) {
      if (strcasecmp(Name(bodyODL[j]), "END_TIME") == 0)
         break;
   }
   if (bodyODL[j] == NULL) {
      handleReqErrorBox(CANT_FIND_SCAN_STOP_TIME);
      printflog(LOG_ERR, CANT_FIND_SCAN_STOP_TIME);
      return(SQC_END_TM_SRF_ERROR);
   }
   scan_stop_time = (GMT_t*) Val(bodyODL[j]);
   if (scan_stop_time == NULL) {
      handleReqErrorBox(INVALID_SCANT_STOP_TIME);
      printflog(LOG_ERR, INVALID_SCANT_STOP_TIME);
      return(SQC_IVL_SCN_STP_ERROR);
   }

   datatakeLen = mkTime(&scan_stop_time->tm) -
                 mkTime(&scan_start_time->tm) + 1;

   for (i=0; bodyODL[i]!=NULL; ++i) {
      /* find SEGMENT */
      if (strcasecmp(Name(bodyODL[i]), SEGMENT) ||
          (segmtODL = (ODL*) Val(bodyODL[i])) == NULL) {
         continue;
      }
      else {
         for (j=0; segmtODL[j]!=NULL; j++) {
            /* find SEGMENT_ID */
            if (strcasecmp(Name(segmtODL[j]), SEGMENT_ID) == 0)
               break;
         }
         seg_id++;
         if (segmtODL[j] == NULL) {
            handleReqErrorBox(CANT_FIND_SEGMENT_ID, seg_id);
            printflog(LOG_ERR, CANT_FIND_SEGMENT_ID, seg_id);
            return(SQC_SEG_ID_SRF_ERROR);
         }
         segment_id = (int*) Val(segmtODL[j]);
   
         for (j=0; segmtODL[j]!=NULL; j++) {
            /* find FRAME_COUNT */
            if (strcasecmp(Name(segmtODL[j]), FRAME_COUNT) == 0)
               break;
         }
         if (segmtODL[j] == NULL) {
            handleReqErrorBox(CANT_FIND_FRAME_COUNT, seg_id);
            printflog(LOG_ERR, CANT_FIND_FRAME_COUNT, seg_id);
            return(SQC_FRM_CT_SRF_ERROR);
         }
         frame_id = (int*) Val(segmtODL[j]);
   
         for (j=0; segmtODL[j]!=NULL; j++) {
            if (strcasecmp(Name(segmtODL[j]), SEGMENT_START_TIME) == 0)
               break;
         }
         if (segmtODL[j] == NULL) {
            handleReqErrorBox(CANT_FIND_SEGMENT_START_TIME, seg_id);
            printflog(LOG_ERR, CANT_FIND_SEGMENT_START_TIME, seg_id);
            return(SQC_SEG_SRT_TM_ERROR);
         }
         segment_start_time = (GMT_t*) Val(segmtODL[j]);

         if (segment_start_time == NULL) {
            handleReqErrorBox(INVALID_SEGMENT_START_TIME, seg_id);
            printflog(LOG_ERR, INVALID_SEGMENT_START_TIME, seg_id);
            return(SQC_IVL_SRT_TM_ERROR);
         }
         if (segment_start_time->tv_usec >= 500000) {
            addOneSecond(segment_start_time);
         }
   
         for (j=0; segmtODL[j]!=NULL; j++) {
            if (strcasecmp(Name(segmtODL[j]), SEGMENT_STOP_TIME) == 0)
               break;
         }
         if (segmtODL[j] == NULL) {
            handleReqErrorBox(CANT_FIND_SEGMENT_STOP_TIME, seg_id);
            printflog(LOG_ERR, CANT_FIND_SEGMENT_STOP_TIME, seg_id);
            return(SQC_SEG_STP_TM_ERROR);
         }
         segment_stop_time = (GMT_t*) Val(segmtODL[j]);

         if (segment_stop_time == NULL) {
            handleReqErrorBox(INVALID_SEGMENT_STOP_TIME, seg_id);
            printflog(LOG_ERR, INVALID_SEGMENT_STOP_TIME, seg_id);
            return(SQC_IVL_STP_TM_ERROR);
         }
         if (segment_stop_time->tv_usec >= 500000) {
            addOneSecond(segment_stop_time);
         }

         /* setup entries for select segment scrolllists */
         sprintf(tmpSegmentID, "%d", *segment_id);
         sprintf(tmpFrameID, "%d", *frame_id);
         sprintf(tmpStartTime, "%d-%03dT%02d:%02d:%02d", 
            (segment_start_time->tm.tm_year>80 ? 
             segment_start_time->tm.tm_year+1900 :
             segment_start_time->tm.tm_year+2000),
             segment_start_time->tm.tm_mday, segment_start_time->tm.tm_hour, 
             segment_start_time->tm.tm_min, segment_start_time->tm.tm_sec);
   
         sprintf(tmpStopTime, "%d-%03dT%02d:%02d:%02d", 
            (segment_stop_time->tm.tm_year>80 ? 
             segment_stop_time->tm.tm_year+1900 :
             segment_stop_time->tm.tm_year+2000),
             segment_stop_time->tm.tm_mday, segment_stop_time->tm.tm_hour, 
             segment_stop_time->tm.tm_min, segment_stop_time->tm.tm_sec);
   
         duration = mkTime(&segment_stop_time->tm) - 
                    mkTime(&segment_start_time->tm);
         sprintf(tmpDuration, "%d", duration);
   
         if ((segmentPtr[*segment_id-1] = 
              malloc(sizeof(struct segment_struct))) != NULL) {
            strcpy(segmentPtr[*segment_id-1]->segmentID, tmpSegmentID);
            strcpy(segmentPtr[*segment_id-1]->frameID, tmpFrameID);
            strcpy(segmentPtr[*segment_id-1]->duration, tmpDuration);
            strcpy(segmentPtr[*segment_id-1]->startTime, tmpStartTime);
            strcpy(segmentPtr[*segment_id-1]->stopTime, tmpStopTime);
         }
         else {
            handleReqErrorBox(CANT_MALLOC_FRAME_ENTRY, seg_id);
            printflog(LOG_ERR, CANT_MALLOC_FRAME_ENTRY, seg_id);
            return(SQC_MALLOC_FRAME_ERROR);
         }
      }
   }

   /* set the maximum value of scroll bar */
   XmScrollBarGetValues(segmentScrollBar, &value, &slide, &inc, &page);
   value = 0;
   XmScrollBarSetValues(segmentScrollBar, value, slide, inc, page, False);

   i = 0;
   if (segmentN == 0)
      XtSetArg(arg[i], XmNmaximum, 1);
   else
      XtSetArg(arg[i], XmNmaximum, segmentN);
   i++;
   if (segmentN == 0)
      XtSetArg(arg[i], XmNsliderSize, 1);
   else if (segmentN < 5)
      XtSetArg(arg[i], XmNsliderSize, segmentN);
   else
      XtSetArg(arg[i], XmNsliderSize, 5);
   i++;
   XtSetValues(segmentScrollBar, arg, i);

   preSegmentSelPos = curSegmentSelPos = 0; /* 0 denotes "no selection yet" */
   scrollSegmentPos = 0;

   /* XmScrollBarGetValues */
   moveSegmentScrollBar();

   ODLFree(odl);
   return(0);

} /* end of setSegmentScrollList................*/


/*----------------------------------------------------------
 * NAME:
 *  moveSegmentScrollBar
 *
 * DESCRIPTION:
 *  delete all items in lists, set up previous/current
 *  positions, then low down previous selection and
 *  highlight current selection.
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void moveSegmentScrollBar()
{
   int      value, slide, inc, page;
   int      i, j;
   char     tmpTime[24];
   XmString xmstring;

   /* XmListDeleteAllItems */
   XmListDeleteAllItems(segmentSegmentScrollList);
   XmListDeleteAllItems(segmentFrameScrollList);
   XmListDeleteAllItems(segmentDurationScrollList);
   XmListDeleteAllItems(segmentStartTimeScrollList);
   XmListDeleteAllItems(segmentStopTimeScrollList);

   XmScrollBarGetValues(segmentScrollBar, &value, &slide, &inc, &page);
   scrollSegmentPos = value;
   scrollSegmentPage = page;

   /* XmListAddItemUnselected */
   for (i=1, j=value; j<value+(segmentN>page?page:segmentN); i++, j++) {
      xmstring = XmStringCreateSimple(segmentPtr[j]->segmentID);
      XmListAddItemUnselected(segmentSegmentScrollList, xmstring, i);
      XmStringFree(xmstring);
      xmstring = XmStringCreateSimple(segmentPtr[j]->frameID);
      XmListAddItemUnselected(segmentFrameScrollList, xmstring, i);
      XmStringFree(xmstring);
      xmstring = XmStringCreateSimple(segmentPtr[j]->duration);
      XmListAddItemUnselected(segmentDurationScrollList, xmstring, i);
      XmStringFree(xmstring);
      strcpy(tmpTime, segmentPtr[j]->startTime);
      tmpTime[8] = ':';
      xmstring = XmStringCreateSimple(tmpTime);
      XmListAddItemUnselected(segmentStartTimeScrollList, xmstring, i);
      XmStringFree(xmstring);
      strcpy(tmpTime, segmentPtr[j]->stopTime);
      tmpTime[8] = ':';
      xmstring = XmStringCreateSimple(tmpTime);
      XmListAddItemUnselected(segmentStopTimeScrollList, xmstring, i);
      XmStringFree(xmstring);
   }

   /* highlight the selected row if within the list */
   if (curSegmentSelPos > 0) {
      i = curSegmentSelPos - scrollSegmentPos;
      if (i> 0 && i <= scrollSegmentPage)
         highlitSegmentList(i); 
   }

} /* end of moveSegmentScrollBar */


/*----------------------------------------------------------
 * NAME:
 *  pickSegmentListEntry(int)
 *
 * DESCRIPTION:
 *  free memory allocation, then switch windows
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void pickSegmentListEntry(int list)
{
   int i = 0;

   if (list == segmentList) {
      i = XmListGetKbdItemPos(segmentSegmentScrollList);
   }
   else if (list == frameList) {
      i = XmListGetKbdItemPos(segmentFrameScrollList);
   }
   else if (list == latitudeList) {
      i = XmListGetKbdItemPos(segmentDurationScrollList);
   }
   else if (list == longitudeList) {
      i = XmListGetKbdItemPos(segmentStartTimeScrollList);
   }
   else if (list == timeList) {
      i = XmListGetKbdItemPos(segmentStopTimeScrollList);
   }

   preSegmentSelPos = curSegmentSelPos;
   curSegmentSelPos = i + scrollSegmentPos;

   /* low down previous highlight if existed */
   i = preSegmentSelPos - scrollSegmentPos;
   if (i > 0 && i <= scrollSegmentPage) {
      lowdownSegmentList(i);
   }

   /* highlight the new selection */
   i = curSegmentSelPos - scrollSegmentPos;
   highlitSegmentList(i);

} /* pickSegmentListEntry()..........................*/


/*----------------------------------------------------------
 * NAME:
 *  highlitSegmentList
 *
 * DESCRIPTION:
 *  highlightthe previous selected entry in all the lists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void highlitSegmentList(int item)
{
   XmListSelectPos(segmentSegmentScrollList, item, NULL);
   XmListSelectPos(segmentFrameScrollList, item, NULL);
   XmListSelectPos(segmentDurationScrollList, item, NULL);
   XmListSelectPos(segmentStartTimeScrollList, item, NULL);
   XmListSelectPos(segmentStopTimeScrollList, item, NULL);
} /* end of highlitSegmentList */


/*----------------------------------------------------------
 * NAME:
 *  lowdownSegmentList
 *
 * DESCRIPTION:
 *  de-select the previous selected entry in all the lists
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void lowdownSegmentList(int item)
{
   XmListDeselectPos(segmentSegmentScrollList, item);
   XmListDeselectPos(segmentFrameScrollList, item);
   XmListDeselectPos(segmentDurationScrollList, item);
   XmListDeselectPos(segmentStartTimeScrollList, item);
   XmListDeselectPos(segmentStopTimeScrollList, item);
} /* end of lowdownSegmentList */


/*----------------------------------------------------------
 * NAME:
 *  getDefaultFromFile
 *
 * DESCRIPTION:
 *  get default platform, revolution. sequence, ..
 *  from scan results file name
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void getDefaultFromFile(char *scan_results_file)
{
   int  len, i;
   char filename[30];

   len = strlen(scan_results_file);
   for (i = len; i > 0; i--) { /* filename ? */
      if (scan_results_file[i] == '\/') {
         break;
      }
   }
   /* get sourceDir and filename */
   if (i == 0) {
      strcpy(filename, &scan_results_file[i]);
   }
   else {
      strcpy(filename, &scan_results_file[i+1]);
   }

   strncpy(defaultPlatform, &filename[platformStart], platformLen);
   defaultPlatform[platformLen] = '\0';
   strncpy(defaultRev, &filename[revStart], revLen);
   defaultRev[revLen] = '\0';
   strncpy(defaultSeq, &filename[seqStart], seqLen);
   defaultSeq[seqLen] = '\0';
   strncpy(defaultMode, &filename[modeStart], modeLen);
   defaultMode[modeLen] = '\0';

   if (strcmp(defaultPlatform, "R1") &&
       strcmp(defaultPlatform, "E1") &&
       strcmp(defaultPlatform, "E2") &&
       strcmp(defaultPlatform, "J1") ) {
      strcpy(defaultPlatform, "ERR");
   }
   if ((strcmp(defaultRev, "00000") < 0) ||
       (strcmp(defaultRev, "99999") > 0)) {
      strcpy(defaultRev, "ERR");
   }
   if ((strcmp(defaultSeq, "00") < 0) ||
       (strcmp(defaultSeq, "99") > 0)) {
      strcpy(defaultSeq, "ERR");
   }
   if (strcmp(defaultMode, "ST1") &&
       strcmp(defaultMode, "ST2") &&
       strcmp(defaultMode, "ST3") &&
       strcmp(defaultMode, "ST4") &&
       strcmp(defaultMode, "ST5") &&
       strcmp(defaultMode, "ST6") &&
       strcmp(defaultMode, "ST7") &&
       strcmp(defaultMode, "SWA") &&
       strcmp(defaultMode, "SWB") &&
       strcmp(defaultMode, "SNA") &&
       strcmp(defaultMode, "SNB") &&
       strcmp(defaultMode, "EH1") &&
       strcmp(defaultMode, "EH2") &&
       strcmp(defaultMode, "EH3") &&
       strcmp(defaultMode, "EH4") &&
       strcmp(defaultMode, "EH5") &&
       strcmp(defaultMode, "EH6") &&
       strcmp(defaultMode, "WD1") &&
       strcmp(defaultMode, "WD2") &&
       strcmp(defaultMode, "WD3") &&
       strcmp(defaultMode, "FN1") &&
       strcmp(defaultMode, "FN2") &&
       strcmp(defaultMode, "FN3") &&
       strcmp(defaultMode, "FN4") &&
       strcmp(defaultMode, "FN5") &&
       strcmp(defaultMode, "EL1") &&
       strcmp(defaultMode, "STD") ) {
      strcpy(defaultMode, "UNKNOWN");
   }
} /* end of getDefaultFromFile */


/*----------------------------------------------------------
 * NAME:
 *  scan_qc (CP_scan_qc)
 *
 * DESCRIPTION:
 *  encapsulate and separate the routines from the uimx
 *  generated interface code for the request utility
 *
 *  This is the main function of CP_can_qc. Initializes the ODL
 *  first, checks the scan_results_file name, then prepare
 *  the view window and shows to the user. User watchs the
 *  provided frame and segment lists to decide the status
 *  of this file. A choice of "Hold", "Reject", or "Accept"
 *  can be made.

 * NOTES:
 *
 *---------------------------------------------------------*/
scan_qc(Widget mainIface, int argc, char **argv)
{
   int    i, fd;
   Arg    args[5];
   char   titleBuf[256];
   char   GLOBAL_configPtr[100], *filePtr;
   ODL    odl, *configODL, ODLconfig;
   char   errstr[256];
   Atom   WM_DELETE_WINDOW;
   Widget shell;
   struct stat fs;

   ODLinit();
   openlog("CP_scan_qc", LOG_NOWAIT|LOG_NDELAY|LOG_PID, LOG_CP);

   shell = XtParent(mainIface);
   i = 0;
   XtSetArg(args[i], XmNdeleteResponse, XmDO_NOTHING); i++;
   XtSetValues(shell, args, i);

   WM_DELETE_WINDOW = XmInternAtom(XtDisplay(UxTopLevel), 
                                   "WM_DELETE_WINDOW", 
                                   FALSE);
   /* set up the primary window */
   XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW,
                           doExit, NULL);

   strcpy(GLOBAL_ScanQc.scan_results_file, argv[1]);
   getDefaultFromFile(GLOBAL_ScanQc.scan_results_file);

   GLOBAL_configPtr[0] = '\0';
   for (i = 0; i < argc; i++) {
      if (strcmp(argv[i], "-configfile") == 0) {
         strcpy(GLOBAL_configPtr, (char *)convertEnvFile(argv[i+1]));
      }
      if (strcmp(argv[i], "-asfcpconfig") == 0) {
         strcpy(GLOBAL_configPtr, (char *)convertEnvFile(argv[i+1]));
      }
   }

   if (strlen(GLOBAL_configPtr) == 0) {
      handleReqErrorBox(CFG_FILE_NOT_SPECIFIED);
      printflog(LOG_ERR, CFG_FILE_NOT_SPECIFIED);
      rv = SQC_NO_CFG_ERROR;
      error_exit(rv, mainIface);
      goto end;
   }

   fd = open(GLOBAL_configPtr, 0);
   if (fd == -1) {
      handleReqErrorBox(CANT_OPEN_CFG_FILE, GLOBAL_configPtr);
      printflog(LOG_ERR, CANT_OPEN_CFG_FILE, GLOBAL_configPtr);
      rv = SQC_CFG_ERROR;
      error_exit(rv, mainIface);
      goto end;
   }

   if ((rv = fstat(fd, &fs)) == -1) {
      handleReqErrorBox(CFG_FILE_STATUS_ERROR, GLOBAL_configPtr);
      printflog(LOG_ERR, CFG_FILE_STATUS_ERROR, GLOBAL_configPtr);
      rv = SQC_CFG_ERROR;
      error_exit(rv, mainIface);
      goto end;
   }

   filePtr = (char *)malloc(fs.st_size);
   if (read(fd, filePtr, fs.st_size) != fs.st_size) {
      handleReqErrorBox(CFG_FILE_READ_ERROR, fs.st_size, GLOBAL_configPtr);
      printflog(LOG_ERR, CFG_FILE_READ_ERROR, fs.st_size, GLOBAL_configPtr);
      free(filePtr);
      close(fd);
      rv = SQC_CFG_RD_ERROR;
      error_exit(rv, mainIface);
      goto end;
   }
   close(fd);

   if ((ODLconfig = StrToODL(filePtr, fs.st_size)) == NULL) {
      handleReqErrorBox(CANT_PARSE_CFG_FILE);
      printflog(LOG_ERR, CANT_PARSE_CFG_FILE);
      free(filePtr);
      rv = SQC_PARSE_CFG_ERROR;
      error_exit(rv, mainIface);
      goto end;
   }
   free(filePtr);

   strcpy(GLOBAL_ScanQc.print_cmd, 
          (char *)convertEnvFile(ODLGetString(ODLconfig, PRINT_COMMAND, &rv)));
   if (GLOBAL_ScanQc.print_cmd == NULL || rv == -1)
      strcpy(GLOBAL_ScanQc.print_cmd, (char *)convertEnvFile(DEF_PRINT_CMD));

   rv = SQC_SCAN_ERROR;
   if (checkIfFileExists(GLOBAL_ScanQc.scan_results_file) == 0) {
      if ((rv = setFrameScrollList()) == 0) {
         if ((rv = setSegmentScrollList()) == 0)
            scanQcHeaders();
         else {
            error_exit(rv, mainIface);
         }
      }
      else {
         error_exit(rv, mainIface);
      }
   }
   else {
      error_exit(SQC_SRF_ERROR, mainIface);
   }
end:
   if (Global_error != 1)
      UxPopupInterface(mainIface, no_grab);
   else {
      printflog(LOG_ERR, EXIT_ERROR);
      return(-1);
   }
} /* end of scan_qc */
