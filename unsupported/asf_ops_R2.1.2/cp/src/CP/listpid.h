#ifndef _listpid_h__
#define _listpid_h__

static char sccsid_listpid_h[] = "@(#)listpid.h	4.19 96/12/10 12:14:52";

#include <task.h>
#include <sys/param.h>  /* for MAXPATHLEN */
#include "listcore.h"

#define ACCEPT_THREAD "acceptThread"
#define READ_THREAD   "readThread"


typedef struct {
   char namePtr[MAX_SUBSYS_NAME_LEN];
   int  qcType;  /* 0 = image; 1 = scan ; defined in cpdefines.h*/
   int  jobId;
   char scanResultsFileBuf[256];
   char imageFileBuf[256];
   char leaderFileBuf[256]; 
   char avgFileBuf[256]; 
}qcReqType;

typedef struct pidListElemTag{
                           int   classID;
                           char  *procNamePtr;
                           char  *exeNamePtr;
                           char  *logFileNamePtr;
                           FILE  *logfp; 
                           int    listPos;     /* list pos in state box */
                           int    externPos;   /* list pos in extern box */
                           int        ppid;
                           tid_t      ttid;
                           int        state;
                           int        stateModFlag;
                           int        termination_reason;
                           int        socket;
		           tid_t      assocThread; /* read thread */
                           Widget     mainWid;
                           Widget     infoLabel;
                           Widget     jobIdList;
                           Widget     platRevSeqList;
                           Widget     frameList;
                           Widget     mediaList;
                           Widget     modeList;
                           Widget     requestList;
                           Widget     typeList;
                           Widget     statusList;
                           Widget     scrollBar;
                           Widget     queBottomInfoLabel_0;
                           XtIntervalId startID;
                           XtIntervalId stopID;
                           XtIntervalId haltID;
                           XtIntervalId healthID;
                           XtIntervalId msg_sentID;
                           XtIntervalId cleanupID;
                           qcReqType    qcReq;
                          }pidListElemType;

int CreatePidList_PIDLIST();


int AddPidToList_PIDLIST(int classID,
                     char *procNamePtr, 
                     char *exeNamePtr, 
                     char *logFileNamePtr,
                     int  listPos,
                     int  ppid);

int IsPidNoForProcess_PIDLIST(int ppid);
int ChangeProcessState_PIDLIST(char *procNamePtr, int state, 
                               int termination_reason);
int GetNumInstances_PIDLIST(char *procNamePtr);
int GetProcessState_PIDLIST(char *procNamePtr);
void FreePidListElem_PIDLIST(pidListElemType *pidListElemPtr);
int RemovePidFromListGivenName_PIDLIST(char *namePtr);
int RemoveAll_PIDLIST();
char *GetNameGivenPid_PIDLIST(int ppid);

char *GetNameGivenListPos_PIDLIST(int listPos);
char *GetNameGivenMainWid_PIDLIST(Widget mainWid);
int GetListPosGivenName_PIDLIST(char *namePtr);
int GetPidGivenName_PIDLIST(char *namePtr);
int SetPidGivenName_PIDLIST(char *namePtr, int ppid);
int SetlogfpGivenName_PIDLIST(char *namePtr, FILE *logfp);
int ClearlogfpGivenName_PIDLIST(char *namePtr);

Widget GetMainWidGivenName_PIDLIST(char *namePtr);
Widget GetMsgListGivenName_PIDLIST(char *namePtr);
Widget GetInfoLabelGivenName_PIDLIST(char *namePtr);
Widget GetQueListGivenName_PIDLIST(char *namePtr);
Widget GetJobIdListGivenName_PIDLIST(char *namePtr);
Widget GetPlatRevSeqListGivenName_PIDLIST(char *namePtr);
Widget GetFrameListGivenName_PIDLIST(char *namePtr);
Widget GetMediaListGivenName_PIDLIST(char *namePtr);
Widget GetModeListGivenName_PIDLIST(char *namePtr);
Widget GetRequestListGivenName_PIDLIST(char *namePtr);
Widget GetTypeListGivenName_PIDLIST(char *namePtr);
Widget GetStatusListGivenName_PIDLIST(char *namePtr);
Widget GetScrollBarGivenName_PIDLIST(char *namePtr);
Widget GetBottomInfoLabelGivenName_PIDLIST(char *namePtr, int labelID);

int AddSocketTo_PIDLIST(char *namePtr, int socket, tid_t assocThread);
int GetSocketGivenName_PIDLIST(char *namePtr);
char *GetNameGivenSocket_PIDLIST(int socket);
int GetAssocThreadGivenName_PIDLIST(char *namePtr);
char *GetExeNameGivenName_PIDLIST(char *namePtr);
int ChangeExeName_PIDLIST(char *namePtr, char *exeNamePtr, int pid);


pidListElemType *GetHeadOf_PIDLIST();
pidListElemType *GetCopyOfElemIn_PIDLIST(int i);

pidListElemType *GetElemGivenName_PIDLIST(char *namePtr, int mode);

pidListElemType *GetFirstProcessWithStateOf_PIDLIST(int state, int mode);

pidListElemType *GetFirstProcessWithModifedState_PIDLIST(int mode);
int GetClassGivenName_PIDLIST(char *namePtr);
int SetTimeOutID_PIDLIST(char *namePtr, int intervalType,
                         XtIntervalId intervalID);
int RemoveTimeOut_PIDLIST(char *namePtr, int  intervalType);

int SetQCreqGivenPid_PIDLIST(int ppid, qcReqType *qcReqPtr);
qcReqType *GetQCreqGivenPid_PIDLIST(int ppid);

int GetScrollStateGivenName_PIDLIST(char *namePtr, int winID);

int SetScrollStateGivenName_PIDLIST(char *namePtr, int winID, int state);

int  InsertWidgets_PIDLIST(Widget mainWid);

#endif       /* !_listpid_h__ */
