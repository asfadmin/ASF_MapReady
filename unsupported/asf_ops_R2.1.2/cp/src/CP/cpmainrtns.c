/* #define POSITION_SUBSYS /* position subsystem windows (breaks sun console) */

/* #define NEW_MOVE_JOB /* allow operator to select destination */
/* #define START_EXTERNALS /* uncomment to start pps and ims from jasper test */

static char sccsid_cpmainrtns_c[] = "@(#)cpmainrtns.c	4.40 96/11/26 10:20:43";

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>  /* gethostname, read, write */
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>

/* cws cp includes .................................*/

#include <task.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include "odl.h"            /* sps library includes */
#include "logUtils.h"
#include "memUtils.h"
#include "inet.h"
#include "asfcommon.h"
#include "pthread_wrapper.h"

#include "CPmainQ.h"        /* CP uim/x-generated includes */ 
#include "CPsubsysQ.h"
#include "CPdetailedInfo.h"
#include "CPfileBox.h"
#include "CPhelp.h"

#include "cpdefines.h"      /* CP normal (handwritten) includes */
#include "cplogs.h"
#include "cpworkProc.h"
#include "cpconfig.h"
#include "cprtns.h"
#include "display.h"
#include "listcore.h"
#include "listpid.h"
#include "listmstrq.h"
#include "que_xwp.h"
#include "que_sys.h"
#include "pps.h"
#include "version.h"

/* end cws includes ....................................*/


/* structure defs used in this file */
/* end structure defs used in this file */

/* begin global variables **************************/

Widget       detailedQueItemInfoBoxWidget;
Widget CPstateRC[20][20];
Widget CPext_stateRC[20][20];
Widget newRC;
int addStatusLine(char *namePtr, Widget parent, Widget *CPstateRCline,
                     XtPointer disarmCB);
extern void subsysValueChangedCB();
extern void configure();
extern void externValueChangedCB();
extern Widget getQueWidget();  /* defined in CPsubsysQ.i */
extern ODL InitODLtemplate();  /* proto should be in library but isn't */

extern Widget MoveJob_tmp_pb;
extern Widget CPmainQ;
RDSqueueIDtype RDSqueue[MAX_TAPE_ID];
int GLOBAL_MAX_TAPE_ID;
int numRDSqueue[MAX_RDS];

Widget       CPinformationWidget;
Widget       queueFileSelectionBox;
Widget       helpWidget;

static char         *GLOBAL_loglocPtr = NULL;
static int  GLOBAL_pipe[2];

	/* the following might be better off in one struct */
static int GLOBAL_numSubsys = 0;
static char **GLOBAL_subsysNamePtr;
static char **GLOBAL_subsysDispNamePtr;
static char GLOBAL_CP_inetBuf[50];
static char GLOBAL_RDS_subnet_inetBuf[MAX_RDS][50];
static char GLOBAL_SSP2_subnet_inetBuf[MAX_SSP2][50];
static char GLOBAL_ASP_subnet_inetBuf[50];
static char GLOBAL_PPS_subnet_inetBuf[50];
static char GLOBAL_IMS_subnet_inetBuf[MAX_IMS][50];
	/* end the following might be better off in one struct */

/* end global variables **************************/


/*----------------------------------------------------------
 * NAME:
 *  set_subnetBuf
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *  for evaluation -- man not be used
 *--------------------------------------------------------*/
void set_subnetBuf()
{
 struct hostent *hostptr;
 struct in_addr *in_addr_ptr;
 int i, retval;
 char host_nameBuf[256];
 subsysConfigType *subsysConfigPtr;


  /* set up the CP own inet addr */
  retval = gethostname(host_nameBuf, sizeof(host_nameBuf));
  if (retval == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_HOSTNAME);
    exit(-1);
  }

  if ( (hostptr = gethostbyname(host_nameBuf)) == NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_INET_ADDR, "CP");
    exit(-1);
  }
  in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
  strcpy(GLOBAL_CP_inetBuf, inet_ntoa(*in_addr_ptr));

  subsysConfigPtr = getCPconfig();

  /* setup the RDS based on the configfile */
  for (i=0; i < getNumRDSs(); i++) {
    if ( (hostptr = 
    gethostbyname(subsysConfigPtr->RDS_CP_hostNamePtr[i])) == NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_CONFIG_INET_ADDR, getRDSname(i));
    exit(-1);
    }
    in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
    strcpy(GLOBAL_RDS_subnet_inetBuf[i], inet_ntoa(*in_addr_ptr));

  }
  for (i=0; i < getNumSSP2s(); i++) {
  /* setup the SSP2 based on the configfile */
    if ( (hostptr =
      gethostbyname(subsysConfigPtr->SSP2_CP_hostNamePtr[i])) == NULL) {
      printfLLog(LOG_ERR, CANNOT_GET_CONFIG_INET_ADDR, SSP2_CATEGORY);
      exit(-1);
    }
    in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
    strcpy(GLOBAL_SSP2_subnet_inetBuf[i],inet_ntoa(*in_addr_ptr));
  }


  /* setup the ASP based on the configfile */
  if ((hostptr=gethostbyname(subsysConfigPtr->ASP_CP_hostNamePtr)) == NULL){
    printfLLog(LOG_ERR, CANNOT_GET_CONFIG_INET_ADDR, getASPname());
    exit(-1);
  }
  in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
  strcpy(GLOBAL_ASP_subnet_inetBuf, inet_ntoa(*in_addr_ptr));

  /* setup the PPS based on the configfile */
  if ((hostptr=gethostbyname(subsysConfigPtr->PPS_CP_hostNamePtr))==NULL) {
    printfLLog(LOG_ERR, CANNOT_GET_CONFIG_INET_ADDR, getPPSname());
    exit(-1);
  }
  in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
  strcpy(GLOBAL_PPS_subnet_inetBuf, inet_ntoa(*in_addr_ptr));

  for (i=0; i < getNumIMSs(); i++) {
  /* setup the IMS based on the configfile */
    if ( (hostptr =
      gethostbyname(subsysConfigPtr->IMS_CP_hostNamePtr[i])) == NULL) {
      printfLLog(LOG_ERR, CANNOT_GET_CONFIG_INET_ADDR, "IMS");
      exit(-1);
    }
    in_addr_ptr = (struct in_addr *) *hostptr->h_addr_list;
    strcpy(GLOBAL_IMS_subnet_inetBuf[i],inet_ntoa(*in_addr_ptr));
  }

} /* end set_subnetBuf.................................*/


/*----------------------------------------------------------
 * NAME:
 *  get_CP_inetPtr
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *--------------------------------------------------------*/
char *get_CP_inetPtr()
{
 return(&GLOBAL_CP_inetBuf[0]);

} /* get_CP_inetPtr......................................*/

/*----------------------------------------------------------
 * NAME:
 *  get_subnet_inetPtr 
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *--------------------------------------------------------*/
char *get_subnet_inetPtr(char *namePtr)
{
 int classID;

  classID =  GetSubsystemCategoryGivenName(namePtr);
  if (classID == -1) {
    printfLLog(LOG_ERR, INVALID_SUBSYS_CATEGORY, namePtr, classID);
    return(NULL);
  }
  switch(classID) {
    case RDS_CATEGORY_ID:
      return(&GLOBAL_RDS_subnet_inetBuf[getRDSnumGivenName(namePtr)][0]);
      break;
    case SSP2_CATEGORY_ID:
      return(&GLOBAL_SSP2_subnet_inetBuf[getSSP2numGivenName(namePtr)][0]); 
      break;
    case ASP_CATEGORY_ID:
      return(&GLOBAL_ASP_subnet_inetBuf[0]);
      break;
    case PPS_CATEGORY_ID:
      return(&GLOBAL_PPS_subnet_inetBuf[0]);
      break;
    case IMS_CATEGORY_ID:
      return(&GLOBAL_IMS_subnet_inetBuf[getIMSnumGivenName(namePtr)][0]); 
      break;
    default:          
      printfLLog(LOG_ERR, CANNOT_GET_SUBNET_ADDR);
      return(NULL);
      break;
  } /* end switch */

} /* end get_subnet_inetPtr.................................*/


/*----------------------------------------------------------
 * NAME:
 *  addCPevent
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void addCPevent()
{
  char buf[2];

  write(GLOBAL_pipe[1], buf, sizeof(buf));
  XtAppAddTimeOut(UxAppContext, 
                 CP_EVENT_TIMEOUT_VALUE, 
                 (XtTimerCallbackProc)addCPevent, 
                 NULL);
}
/*----------------------------------------------------------
 * NAME:
 *  getEvent
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

void getEvent()
{
  char buf[2];

  read(GLOBAL_pipe[0], buf, sizeof(buf));

}

/*----------------------------------------------------------
 * NAME:
 *  isLogFileSyslog
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int isLogFileSyslog(char *namePtr)
{
  if (namePtr == NULL)
   return(FALSE);

  if (strncmp(namePtr, "SYSLOG", strlen("SYSLOG")) == 0)
    return(TRUE);
  else
    return(FALSE);

} /* isLogFileSyslog......................................*/

/*----------------------------------------------------------
 * NAME:
 *  getLogLocation
 *
 *
 * DESCRIPTION:
 *  an access function that returns the log location
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
char *getLogLocation()
{

  return(GLOBAL_loglocPtr);

} /* end getLogLocation...................................*/


/*----------------------------------------------------------
 * NAME:
 *  getSubsysNameNo
 *
 * DESCRIPTION:
 *  a function that allows access to  the subsystem
 *  names that were specified and read in at start up from (an
 *  [TBD]ODL configuration file). The ith value is returned. 
 *
 * NOTES:
 *  Names are stored as a global, null terminated list of names.
 *
 *---------------------------------------------------------*/
char *getSubsysNameNo(int i)
{
  return(GLOBAL_subsysNamePtr[i]);

} /* getSubsysNameNo.......................................*/

/*----------------------------------------------------------
 * NAME:
 *  getSubsysNoGivenName
 *
 * DESCRIPTION:
 *  a function that allows access to  the subsystem
 *  names that were specified and read in at start up from (an
 *  [TBD]ODL configuration file). The ith value is returned.
 *
 * NOTES:
 *  Names are stored as a global, null terminated list of names.
 *
 *---------------------------------------------------------*/
int getSubsysNoGivenName(char *namePtr)
{
  int i;
  for (i =0; i < GLOBAL_numSubsys; i++)
    if (strcmp(namePtr, GLOBAL_subsysNamePtr[i]) == 0)
      return(i);

  return(-1);

} /* getSubsysNameNo.......................................*/


void initTapeQueue()
{
  int i;

  GLOBAL_MAX_TAPE_ID = 0;
  for (i = 0; i < MAX_RDS; i++) 
      numRDSqueue[i] = 0;
  for (i = 0; i < MAX_TAPE_ID; i++) {
      strcpy(RDSqueue[i].IDstr, "");
      RDSqueue[i].queueIndx = -1;
      RDSqueue[i].useCount = 0;
  }
}

      
/*----------------------------------------------------------
 * NAME:
 *  initStateTables
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

void initStateTables(subsysConfigType *subsysConfigPtr)
{
  char nameBuf[MAX_INV_NAME_LEN], lognameBuf[MAX_INV_NAME_LEN];
  char *ppsNameBuf, *imsNameBuf;
  char dispnameBuf[MAX_INV_NAME_LEN], *loglocPtr, *dispname = "";
  int  i, listPos, cpListPos=1, iname = 0, widest=0;
#ifdef POSITION_SUBSYS
  int  wid_x_pos = 0, wid_y_pos =0, y_inc = 100; 
  Dimension w, h;
  Position x, y;
#endif
  Widget wid, qc_wid;


  strcpy(nameBuf, "");
  strcpy(lognameBuf, "");

                     /* allocate the list that will contain the names for
                      *  handling the subsys queues in the work proc */
 GLOBAL_subsysNamePtr = (char **)doMalloc((subsysConfigPtr->RDSnum +
                          subsysConfigPtr->ASPnum + 
                          subsysConfigPtr->SSP2num + subsysConfigPtr->PPSnum + 
                          subsysConfigPtr->IMSnum + 1)*sizeof(char **)); 

 GLOBAL_subsysDispNamePtr = (char **)doMalloc((subsysConfigPtr->RDSnum +
                          subsysConfigPtr->ASPnum + 
                          subsysConfigPtr->SSP2num + subsysConfigPtr->PPSnum + 
                          subsysConfigPtr->IMSnum + 1)*sizeof(char **)); 

#ifdef POSITION_SUBSYS
  XtVaGetValues(CPmainQ, XmNx, &x, XmNy, &y, XmNwidth,&w, XmNheight, &h,NULL);

/* position subsys queues to the right of the main window, and tile downward */
/*  wid_x_pos = x + w;
  wid_y_pos = y ; */

/* position subsys queues starting at the bottom of the main window, 
   and tile downward */
  wid_x_pos = x ;
  wid_y_pos = y + h;
#endif

  loglocPtr = getLogLocation();

  for (i = 0; i < subsysConfigPtr->RDSnum; i++) { 
    dispname = getRDSNameGivenObject(RDS_CATEGORY,i);
    sprintf(dispnameBuf, "%s",  dispname);
    sprintf(nameBuf,  "%s",  dispname);
    if (isLogFileSyslog(loglocPtr) == FALSE) {
      if (loglocPtr == NULL)
        sprintf(lognameBuf, "%s.log", nameBuf); 
      else
        sprintf(lognameBuf, "%s/%s.log", loglocPtr, nameBuf); 
    }

    listPos = cpListPos++; 

             /* NOTE: the subsystem queue window is created in the add call */ 
    AddPidToList_PIDLIST(SUBSYS_CLASS_ID, nameBuf, 
       subsysConfigPtr->RDSexeNamePtr[i], lognameBuf, listPos, PID_NOT_SET_ID);

  /* for the RDS only, make the QC menu item insensitive */
/*** now that we can perform a QC on scan results files, we need this item */
    wid = GetMainWidGivenName_PIDLIST(nameBuf);
    qc_wid = (Widget )getQueWidget(wid, "subsysQC_item");
/****
    XtSetSensitive(qc_wid, FALSE);
*********/

#ifdef POSITION_SUBSYS
    XtVaGetValues(wid, XmNheight, &h, NULL);  /* use height of this queue */
    y_inc = h;                                /* to get y increment */

                              /* position the queue widget */
    XtVaSetValues(wid, XmNx, wid_x_pos, XmNy, wid_y_pos, XmNwidth, w, NULL);
    wid_y_pos += y_inc;       /* set Y position for next subsys queue widget */
#endif

    CreateSysReqQue_SYSQUE(nameBuf);    /* create the associated queue */

    GLOBAL_subsysNamePtr[iname] =(char *)doMalloc(strlen(nameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysNamePtr[iname], nameBuf);

    GLOBAL_subsysDispNamePtr[iname] = 
             (char *)doMalloc(strlen(dispnameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysDispNamePtr[iname], dispnameBuf);
    iname++;

    widest = addStatusLine(nameBuf, CPstatusRC, CPstateRC[i], 
                                subsysValueChangedCB);
    GLOBAL_numSubsys++;

#ifdef NEW_MOVE_JOB
    addItemToMenu(MoveJob_pane, nameBuf);
#endif

  } /* end RDSnum */


/* initialize SSP-2 subsystems */
  for (i = 0; i < subsysConfigPtr->SSP2num; i++) {
    dispname = getSSPNameGivenObject(SSP2_CATEGORY, i);
    sprintf(dispnameBuf, "%s",  dispname);
    sprintf(nameBuf, "%s",  dispname);

    if (isLogFileSyslog(loglocPtr) == FALSE) {
      if (loglocPtr == NULL)
        sprintf(lognameBuf, "%s.log", nameBuf); 
      else
        sprintf(lognameBuf, "%s/%s.log", loglocPtr, nameBuf); 
    }

    listPos = cpListPos++;

  /* NOTE: the subsystem queue window is created in the add call */ 
    AddPidToList_PIDLIST(SUBSYS_CLASS_ID, nameBuf, 
      subsysConfigPtr->SSP2exeNamePtr[i], lognameBuf, listPos, PID_NOT_SET_ID);

#ifdef POSITION_SUBSYS
                                /* position the queue widget */
    wid = GetMainWidGivenName_PIDLIST(nameBuf);
    XtVaSetValues(wid, XmNx, wid_x_pos, XmNy, wid_y_pos, NULL);
    XtVaGetValues(wid, XmNheight, &h, NULL);  /* use height of this queue */
    y_inc = h;                                /* to get y increment */
    wid_y_pos += y_inc;       /* set Y position for next subsys queue widget */
#endif

    CreateSysReqQue_SYSQUE(nameBuf); /* create the associated queue */

    GLOBAL_subsysNamePtr[iname] =(char *)doMalloc(strlen(nameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysNamePtr[iname], nameBuf);

    GLOBAL_subsysDispNamePtr[iname] = 
             (char *)doMalloc(strlen(dispnameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysDispNamePtr[iname], dispnameBuf);
    iname++;

    widest = addStatusLine(nameBuf, CPstatusRC, CPstateRC[i], 
                                subsysValueChangedCB);
    GLOBAL_numSubsys++;
#ifdef NEW_MOVE_JOB
    addItemToMenu(MoveJob_pane, nameBuf);
#endif
  } /* end SSP2num */

  for (i = 0; i < subsysConfigPtr->ASPnum; i++) {
    dispname = getNameGivenObject(ASP_CATEGORY);
    sprintf(dispnameBuf, "%s",  dispname);
    sprintf(nameBuf, "%s",  dispname);

    if (isLogFileSyslog(loglocPtr) == FALSE) {
      if (loglocPtr == NULL)
        sprintf(lognameBuf, "%s.log", nameBuf); 
      else
        sprintf(lognameBuf, "%s/%s.log", loglocPtr, nameBuf); 
    }

    listPos = cpListPos++;

  /* NOTE: the subsystem queue window is created in the add call */ 
    AddPidToList_PIDLIST(SUBSYS_CLASS_ID, nameBuf, 
        subsysConfigPtr->ASPexeNamePtr, lognameBuf, listPos, PID_NOT_SET_ID);

#ifdef POSITION_SUBSYS
                                             /* position the queue widget */
    wid = GetMainWidGivenName_PIDLIST(nameBuf);
    XtVaSetValues(wid, XmNx, wid_x_pos, XmNy, wid_y_pos, NULL);
    XtVaGetValues(wid, XmNheight, &h, NULL);  /* use height of this queue */
    y_inc = h;                                /* to get y increment */
    wid_y_pos += y_inc;       /* set Y position for next subsys queue widget */
#endif

    CreateSysReqQue_SYSQUE(nameBuf);    /* create the associated queue */

    GLOBAL_subsysNamePtr[iname] =(char *)doMalloc(strlen(nameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysNamePtr[iname], nameBuf);

    GLOBAL_subsysDispNamePtr[iname] = 
             (char *)doMalloc(strlen(dispnameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysDispNamePtr[iname], dispnameBuf);
    iname++;

    widest = addStatusLine(nameBuf, CPstatusRC, CPstateRC[i], 
                                subsysValueChangedCB);
    GLOBAL_numSubsys++;
#ifdef NEW_MOVE_JOB
    addItemToMenu(MoveJob_pane, nameBuf);
#endif
  } /* end ASPnum */

  for (i = 0; i < subsysConfigPtr->PPSnum; i++) {
    dispname = getNameGivenObject(PPS_CATEGORY);
    sprintf(dispnameBuf, "%s",  dispname);
    sprintf(nameBuf, "%s",  dispname);
    ppsNameBuf = (char *) doMalloc(strlen(nameBuf)+1);
    sprintf(ppsNameBuf, "%s",  dispname);

    if (isLogFileSyslog(loglocPtr) == FALSE) {
      if (loglocPtr == NULL)
        sprintf(lognameBuf, "%s.log", nameBuf); 
      else
        sprintf(lognameBuf, "%s/%s.log", loglocPtr, nameBuf); 
    }

    listPos = cpListPos++;

  /* NOTE: the subsystem queue window is created in the add call */ 
    AddPidToList_PIDLIST(PPS_CLASS_ID, nameBuf, 
      subsysConfigPtr->PPSexeNamePtr, lognameBuf, listPos, PID_NOT_SET_ID);

#ifdef POSITION_SUBSYS
                                             /* position the queue widget */
    wid = GetMainWidGivenName_PIDLIST(nameBuf);
    XtVaSetValues(wid, XmNx, wid_x_pos, XmNy, wid_y_pos, NULL);
    XtVaGetValues(wid, XmNheight, &h, NULL);  /* use height of this queue */
    y_inc = h;                                /* to get y increment */
    wid_y_pos += y_inc;       /* set Y position for next subsys queue widget */
#endif

    CreateSysReqQue_SYSQUE(nameBuf); /* create the associated queue */

    GLOBAL_subsysNamePtr[iname] =(char *)doMalloc(strlen(nameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysNamePtr[iname], nameBuf);

    GLOBAL_subsysDispNamePtr[iname] = 
             (char *)doMalloc(strlen(dispnameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysDispNamePtr[iname], dispnameBuf);
    iname++;

    widest = addStatusLine(nameBuf, CPexternRC, CPext_stateRC[i], 
                                externValueChangedCB);  
    GLOBAL_numSubsys++;

#ifdef START_EXTERNALS 
    doStartSubsystem(ppsNameBuf, NULL); 
#endif 
  doFree(ppsNameBuf);
  } /* end PPSnum */

/* initialize IMS subsystems */
  for (i = 0; i < subsysConfigPtr->IMSnum; i++) {
    dispname = getIMSNameGivenObject(IMS_CATEGORY, i);
    sprintf(dispnameBuf, "%s",  dispname);
    sprintf(nameBuf, "%s",  dispname);
    imsNameBuf = (char *) doMalloc(strlen(nameBuf)+1);
    sprintf(imsNameBuf, "%s",  dispname);


    if (isLogFileSyslog(loglocPtr) == FALSE) {
      if (loglocPtr == NULL)
        sprintf(lognameBuf, "%s.log", nameBuf);
      else
        sprintf(lognameBuf, "%s/%s.log", loglocPtr, nameBuf);
    }

    listPos = cpListPos++;

  /* NOTE: the subsystem queue window is created in the add call */
    AddPidToList_PIDLIST(IMS_CLASS_ID, nameBuf,
      subsysConfigPtr->IMSexeNamePtr[i], lognameBuf, listPos, PID_NOT_SET_ID);

#ifdef POSITION_SUBSYS
                                /* position the queue widget */
    wid = GetMainWidGivenName_PIDLIST(nameBuf);
    XtVaSetValues(wid, XmNx, wid_x_pos, XmNy, wid_y_pos, NULL);
    XtVaGetValues(wid, XmNheight, &h, NULL);  /* use height of this queue */
    y_inc = h;                                /* to get y increment */
    wid_y_pos += y_inc;       /* set Y position for next subsys queue widget */
#endif

    CreateSysReqQue_SYSQUE(nameBuf); /* create the associated queue */

    GLOBAL_subsysNamePtr[iname] =(char *)doMalloc(strlen(nameBuf)*sizeof(char)+1
);
    strcpy(GLOBAL_subsysNamePtr[iname], nameBuf);

    GLOBAL_subsysDispNamePtr[iname] =
             (char *)doMalloc(strlen(dispnameBuf)*sizeof(char)+1);
    strcpy(GLOBAL_subsysDispNamePtr[iname], dispnameBuf);
    iname++;

    widest = addStatusLine(nameBuf, CPexternRC, CPext_stateRC[i], 
                            externValueChangedCB); 
    GLOBAL_numSubsys++;

#ifdef START_EXTERNALS
    doStartSubsystem(imsNameBuf, NULL); 
#endif
  doFree(imsNameBuf);

  } /* end IMSnum */

 /* null terminate the subsystem name list */
  GLOBAL_subsysNamePtr[iname] = NULL;
  GLOBAL_subsysDispNamePtr[iname] = NULL;


#ifdef SIZE_TABLES  /* argh.  first part works (makes all fonts the same),  */
                    /* second part doesn't resize the widgets right */
  if (widest > STATE_TABLE_NAME_MAX_PIXELS) {
    updateStateFont(CPstatusRC, SMALL_FONT, STATE_TABLE_NAME_MAX_PIXELS);
    updateStateFont(CPexternRC, SMALL_FONT, STATE_TABLE_NAME_MAX_PIXELS);
    widest = STATE_TABLE_NAME_MAX_PIXELS;  /* shorten the width */
  }

printStatusWidths("before updateStateWidth");
  updateStateWidth(CPstatusRC, widest);
printStatusWidths("after updateStateWidth");
printExternWidths("before updateStateWidth");
  updateStateWidth(CPexternRC, widest);
printExternWidths("after updateStateWidth");

  /* sendEvent(CPstatusRC, VisibilityNotify);
  sendEvent(CPexternRC, VisibilityNotify); */
#endif

} /* end initStateTables .........................*/


/*----------------------------------------------------------
 * NAME: colorCodeQUEwindow
 *
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *  look at XtGetApplicationResources if necessary to move code
 *  out of configfile.odl --  and perhaps a widget list for
 *  menu items not currently being set 
 *
 *---------------------------------------------------------*/
int colorCodeQUEwindow(char *namePtr, Widget *widlist)
{
 Widget *widchild0;
 int    imenu0 = 0;
 int      i;
 Pixel    cval = 0;
 XrmValue source, dest;
 Arg      args[5];
 char     *colorString;
 subsysConfigType *getCPconfigPtr;
 Boolean  retval;

  getCPconfigPtr = getCPconfig();
  if (getCPconfigPtr == NULL)
    return(0);

  switch(GetSubsystemCategoryGivenName(namePtr)) {
   case RDS_CATEGORY_ID:
     colorString = getCPconfigPtr->RDSbackground[getRDSnumGivenName(namePtr)];
     break;

   case SSP2_CATEGORY_ID:
     colorString = getCPconfigPtr->SSP2background[getSSP2numGivenName(namePtr)];
     break;

   case ASP_CATEGORY_ID:
     colorString = getCPconfigPtr->ASPbackground;
     break;

   case PPS_CATEGORY_ID:
     colorString = getCPconfigPtr->PPSbackground;   
     break;

   case IMS_CATEGORY_ID:
     colorString = getCPconfigPtr->IMSbackground[getIMSnumGivenName(namePtr)];  
     break;

   default:
     colorString="";
     break;
  }

  if (colorString == NULL)
     return(0);

  source.size = strlen(colorString);
  source.addr = colorString;
  dest.size   = sizeof(Pixel);
  dest.addr   = (char*) &cval;
  retval = XtConvertAndStore(*widlist, XtRString, &source, 
                             XtRPixel, &dest);
  if (retval == FALSE) {
   printfLLog(LOG_ERR, CANNOT_GET_COLOR);
   return(0);
  }

  i = 0;
  XtSetArg(args[i], XmNbackground, cval); i++;
  while (*widlist != NULL) {
    XtSetValues(*widlist, args, i);
    XtVaGetValues(*widlist, 
                 XmNchildren,    &widchild0,
                 XmNnumChildren, &imenu0,
                 NULL);
  
    XmChangeColor(*widlist, cval);
    while (imenu0-- > 0) {
      XtSetValues(widchild0[imenu0], args, i);
      XmChangeColor(widchild0[imenu0], cval);
    }
    widlist++;
  }

  return(0);

} /* colorCodeQUEWindow */


/*----------------------------------------------------------
 * NAME:
 *  examine_CP_args 
 *
 * DESCRIPTION:
 *  do a cursory check to make sure that all the arguments
 *  the CP needs are supplied
 *
 * NOTES:
 * 
 *
 *---------------------------------------------------------*/

int examine_CP_args(int argc, char *argv[])
{
 int i, configFOUND, asfnFOUND, asfloglocFOUND, asfcpconfigFOUND;

  asfnFOUND = asfloglocFOUND = asfcpconfigFOUND = 0;

  for (i = 0; i < argc; i++) {

   if (strcmp(argv[i], "-asfn") == 0) {
      asfnFOUND = 1;
      if (i+1 == argc) {
        asfnFOUND = -1;
        printf(CP_ARG_ERROR, "name");
      }else{
        if (strncmp(argv[i+1], "-", 1) == 0) {
          asfnFOUND = -2;
          printf(CP_ARG_ERROR, "name");
        }
      }
    } /* end if strcmp */


    if (strcmp(argv[i], "-asflogloc") == 0) {
      asfloglocFOUND = 1;
      if (i+1 == argc) {
        asfloglocFOUND = -1;
        printf(CP_ARG_ERROR, "log location");
       }else{
         if (strncmp(argv[i+1], "-", 1) == 0) {
           asfloglocFOUND = -2;
           printf(CP_ARG_ERROR, "log location");
         }
       }
    } /* end if strcmp */

    if (strcmp(argv[i], "-asfcpconfig") == 0) {
      asfcpconfigFOUND = 1;
     if (i+1 == argc) {
       asfcpconfigFOUND = -1;
       printf(CP_ARG_ERROR, "config file");
      }else{
        if (strncmp(argv[i+1], "-", 1) == 0) {
          asfcpconfigFOUND = -2;
          printf(CP_ARG_ERROR, "config file");
        }
      }
    } /* end if strcmp */
  } /* end for */

  if (asfnFOUND          == 0)
    printf(CP_ARG_ERROR, "name");
  if (asfloglocFOUND     == 0)
    printf(CP_ARG_ERROR, "log location");
  if (asfcpconfigFOUND   == 0)
    printf(CP_ARG_ERROR, "config file");

  if (asfnFOUND <= 0 || asfloglocFOUND <= 0 || asfcpconfigFOUND <= 0   )
    return(-1);

  return(0);
} /* end examine_CP_args........................................*/


/*----------------------------------------------------------
 * NAME:
 *  cpMainrtn
 *
 * DESCRIPTION:
 *  encapsulate and separate the routines from the uimx
 *  generated interface code for the CP 
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int cpMainrtn(int argc, char *argv[], Widget mainIface)
{
 tid_t acceptThreadPid;    /* cws */
 int   i, retval;
 Arg   args[10];
 Widget shell;
 Atom   WM_DELETE_WINDOW;
 subsysConfigType     *subsysConfigPtr;
 struct stat fs;
 struct sigaction sigact;
 sigset_t sigset;
 char *configFilePtr=NULL, *env;
 char *msgTemplateName = NULL;

  env =  getenv("ASF");
  if (env == NULL) {
     printf("Environment variable ASF not defined: CP exiting.\n");
     exit(-1);
  }
  env =  getenv("LOCAL");
  if (env == NULL) {
     printf("Environment variable LOCAL not defined: CP exiting.\n");
     exit(-1);
  }

  retval = examine_CP_args(argc, argv);  
  if (retval == -1) {
    printf("usage:: CP -asfn <CP> \
-asflogloc <SYSLOG> -asfcpconfig <configfile.odl> \n");
    exit(-1);
  }
  retval = ODLinit();


  /* 
   * init flags for the logging utils 
   * NOTE: the asfn flag is picked up in 
   *       the log utils and used for the 
   *       output messages 
   * last parm may be an ident string (in addition
   * to asfn name
   */

  for (i = 0; i < argc; i++) {

    if (strcmp(argv[i], "-asflogloc") == 0) {
    /* dont stat the syslog file */
      retval = 0;
      if (strncmp(argv[i+1], "SYSLOG", strlen("SYSLOG")) != 0) 
     /* check to see if directory really exists */
        retval = stat(argv[i+1], &fs); 
      if (retval == 0) {
     /* found log location on the command line, overide default */
        GLOBAL_loglocPtr = (char *)doMalloc(strlen(argv[i+1]) + 1);
       strcpy(GLOBAL_loglocPtr, argv[i+1]);
      }
    } /* end if strcmp */
    if (strcmp(argv[i], "-asfcpconfig") == 0) {
    /* check to see if file really exists */
      retval = stat(argv[i+1], &fs); 
      if (retval == 0) {
     /* found config file on the command line */
        configFilePtr = (char *)doMalloc(strlen(argv[i+1]) + 1);
        strcpy(configFilePtr, argv[i+1]);
      }
    } /* end if strcmp */


    if (strcmp(argv[i], "-config") == 0) {
    /* check to see if file really exists */
      retval = stat(argv[i+1], &fs);
      if (retval == 0) {
     /* found msgTemplate file on the command line */
        msgTemplateName = (char *)doMalloc(strlen(argv[i+1]) + 1);
        strcpy(msgTemplateName, argv[i+1]);
      }
    } /* end if strcmp */


  } /* end for */

  if (!InitODLtemplate(msgTemplateName)) {
    printf("Cannot initialize ODL template, CP exiting\n");
    exit (-1);
  }

  if (usconfig(CONF_INITUSERS, MAX_CP_THREADS) == -1) {
    printfLLog(LOG_ERR, "can't usconfig %d CONF_INITUSERS", MAX_CP_THREADS);
    exit(0);
  }

  /* get the configuration data out of the ODL file */
  retval = setupCPconfig(configFilePtr);
  if (retval == -1) {
    printfLLog(LOG_ERR, CANNOT_GET_CONFIG);
    exit(0);
  }

  /*
   * setup the subnets inet addr for the subsystems
   * NOTE: program will exit is unable to map host name to
   *       inet addr
   */
  set_subnetBuf();

  /* create the popup widgets */
  queueFileSelectionBox      = create_CPfileBox(UxTopLevel); 
  helpWidget   = create_CPhelp(UxTopLevel);
  detailedQueItemInfoBoxWidget   = create_CPdetailedInfo(UxTopLevel);


  shell = XtParent(mainIface);
  i = 0;
  XtSetArg(args[i], XmNdeleteResponse, XmDO_NOTHING); i++;
  XtSetValues(shell, args, i);
  
  WM_DELETE_WINDOW = XmInternAtom(XtDisplay(UxTopLevel),
                                  "WM_DELETE_WINDOW",
                                  FALSE);
  XmAddWMProtocolCallback(shell, 
                          WM_DELETE_WINDOW, 
                          handleMenuExitCB,
                          NULL);

  /*
   * NOTE:: the subsystem que window is created when
   *        the subsystem is added to the pid list
   *        and stays around until cp termination 
   */

  /* create the queues */
  CreatePidList_PIDLIST();              /* pid/proc name list    */
  CreateMstrqList_MSTRQLIST();          /* master list Q         */
  CreateXwpReqQue_XWPQUE();             /* cp X work proc que    */


  retval =  createOneNameListEntry_CORE(READ_LIST);
  if (retval == -1) {
    printfLLog(LOG_ERR, "Error creating read queue\n");
    exit(0);
  }

  retval =  createOneNameListEntry_CORE(SIGCLD_LIST);
  if (retval == -1) {
    printfLLog(LOG_ERR, "Error creating signal queue\n");
    exit(0);
  }
  retval =  createOneNameListEntry_CORE(PPS_LIST);
  if (retval == -1) {
    printfLLog(LOG_ERR, "Error creating pps message lock queue\n");
    exit(0);
  }

  /* 
   * initialize the control subsystems box 
   * the subsystems names and the associated queues 
   * using the config file
   */
  subsysConfigPtr = getCPconfig();
  initStateTables(subsysConfigPtr);
  initStateFile();           /* save last CP state file for recovery */
  initMediaMount();          /* init variables for tracking media mounting */
  initTapeQueue();          /* init variables for tracking media IDs */
  initPPS(subsysConfigPtr);  /* init variables for keeping track of pps jobs */

  /* install the workproc for handling queues, X-windows, etc */
/*******
  XtAppAddWorkProc(UxAppContext,(XtWorkProc) cpXworkProc, NULL);
******/

 pipe(GLOBAL_pipe);
 XtAppAddInput(UxAppContext, 
               GLOBAL_pipe[0], 
               (void *)XtInputReadMask, 
               (void *)cpXworkProc, NULL);

 XtAppAddTimeOut(UxAppContext, 
                 CP_EVENT_TIMEOUT_VALUE, 
                (XtTimerCallbackProc)addCPevent, 
                 NULL);

  /* install the sig handler for death of a child */
  /* signal(SIGCLD, sig_cld);  s5 style */
  sigaddset(&sigset, SIGCHLD);
 /*  sigfillset(&sigset); */
  sigemptyset(&sigset);
  sigact.sa_handler = ( void (* ))sig_cld;
  sigact.sa_mask    =  sigset;
  sigact.sa_flags   =  SA_RESTART;
  sigaction(SIGCHLD, &sigact, NULL);

                         /* place the "main thread" into the pid list */
  AddPidToList_PIDLIST(CP_CLASS_ID, ASF_CP, "asf_cp", 
                       "CP_SYSLOG_DEBUG", NOT_SET_ID, (int) getpid());

  if (pthread_create(&acceptThreadPid, 1, (void (*))doAccept, NULL) == -1)
    printfLLog(LOG_ERR, "Error creating accept thread: %s", strerror(errno));

  printfLLog(LOG_DEBUG, ACCEPT_TH, acceptThreadPid);
  AddPidToList_PIDLIST(THREAD_CLASS_ID, ACCEPT_THREAD, ACCEPT_THREAD, 
                       "logfile", NOT_SET_ID, acceptThreadPid);

  /* 
   * change main window border name -- get the parent 'shell'  
   */
  i = 0;
  XtSetArg(args[i], XmNtitle, "SPS Processing Queue"); i++;
  XtSetValues(XtParent(mainIface), args, i);


/*  handleMsgLogSnooper_ForSyslog(FILL_MODE); */

  printfLLog(LOG_INFO, STARTING_CP);
  printfLLog(LOG_INFO, CP_VER_READY, CP_version_id);

  if (autoLoadState()) {
      retval =  ReadIn_MSTRQLIST(getSavedStateFile(LAST_STATE_FILE));
      if (retval == -2)
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, RESTORE_FILE_BAD,
          getSavedStateFile(LAST_STATE_FILE));
      else if (retval == -1)
        ASFlogMessage_direct(ASF_CP, WP_ERROR_BOX, RESTORE_FILE_GONE,
          getSavedStateFile(LAST_STATE_FILE));

  }


  return(0);

}  /* end cpMainrtn.......................................*/
