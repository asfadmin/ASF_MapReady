#ifndef _que_sys_h__
#define _que_sys_h__

/*----------------------------------------------------------
 * SCCS Header
 * File:que_sys.h   Rev:4.2.0.0   Date:95/02/10
 *
 *---------------------------------------------------------*/

static char sccsid_que_sys_h[] = "@(#)que_sys.h	4.14 96/12/12 10:17:33";

#define MEDIA_CHECK_NO      0
#define MEDIA_CHECK_YES     1
#define MEDIA_CHECK_PENDING 2

#define CAL_PARAMS_PRIMARY_ID 1
#define CAL_PARAMS_SECONDARY_ID 2

typedef struct sysQueElemTag{
                           char       *reqStrPtr;
                           int        jobId;
                           int        holdFlag;
                           int        qcFlag;
                           int        repeatFlag;
                           int        dataReadyFlag;
                           int        checkMediaId;
                           ODL  sysODLreq;
                          }sysQueElemType;



int  CreateSysReqQue_SYSQUE(char *namePtr);

int  AddSysReqToQue_SYSQUE(char  *namePtr,
                           char  *reqStrPtr, 
                           int    mstrqReqNum, 
                           int    useInsertTop, 
                           int    holdFlag, 
                           ODL    sysODLreq,
                           int    dataReady); 

sysQueElemType *FindSysReqInQue_SYSQUE(baseListElemType *nPtr, 
                                       ODL sysODLreq, 
                                       int *index);

void FreeSysQueElement_SYSQUE(char *namePtr, sysQueElemType *sysQueElemPtr);

sysQueElemType *GetElemGivenJobId_SYSQUE(char *namePtr, int jobId, int mode);
sysQueElemType *GetReqGivenListPos_SYSQUE(char *namePtr, int listPos);
sysQueElemType *CopyElemAtPrivate_SYSQUE(sysQueElemType *sysQueElemPtr);
sysQueElemType *GetHeadOfSysQue_SYSQUE(char *namePtr, int mode);
sysQueElemType *GetSysQueElemAt_SYSQUE(char *namePtr, int  elemNo, int  mode);
sysQueElemType *GetFirstNonQCelem_SYSQUE(char *namePtr, int  mode);
sysQueElemType *GetFirstNonHoldElem_SYSQUE(char *namePtr, int  mode);
sysQueElemType *GetFirstActiveElem_SYSQUE(char *namePtr, int  mode);
sysQueElemType *GetNextNeedingMountElem_SYSQUE(char *namePtr, int mode, 
                                                int *startPos);

char *GetTypeStringGivenSysReq_SYSQUE(char *namePtr, int i);
int   GetIndexGivenJobId_SYSQUE(char *namePtr, int jobId);
int   GetListPosGivenJobId_SYSQUE(char *namePtr, int jobId);
int   GetJobIdGivenListPos_SYSQUE(char *namePtr, int listPos);

ODL GetSysReqGivenJobId_SYSQUE(char *namePtr, int jobId);
ODL GetSysReqGivenString_SYSQUE(char *namePtr, char *stringPtr);

sysQueElemType *RemoveElemWithJobId_SYSQUE(char *namePtr, int  jobId);

int  MoveElemPrivate_SYSQUE(baseListElemType *nPtr, int elemTo, int elemFrom );
int  MoveElem_SYSQUE(char *namePtr, int  elemTo, int  elemFrom );
int  MoveToHead_SYSQUE(char *namePtr, int elemToMove );
int  MoveToTail_SYSQUE(char *namePtr, int elemToMove );

int SetQCflag_SYSQUE(char *namePtr, int jobId);
int ClearQCflag_SYSQUE(char *namePtr, int jobId);

int SetHoldFlag_SYSQUE(char *namePtr, int jobId);
int ClearHoldFlag_SYSQUE(char *namePtr, int jobId);

int SetDataReady_SYSQUE(char *namePtr, int jobId);
int ClearDataReady_SYSQUE(char *namePtr, int jobId);

int SetRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId);
int GetRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId);
int ClearRepeatFlagGivenJobId_SYSQUE(char *namePtr, int jobId);

int GetQCstatus_SYSQUE(char *namePtr);
int GetHoldStatus_SYSQUE(char *namePtr);

int JobExists_SYSQUE(char *namePtr, int jobId);
int SetMediaFlagGivenJobId_SYSQUE(char *namePtr, int jobId, int checkMediaVal);
int GetMediaFlagGivenJobId_SYSQUE(char *namePtr, int jobId);
int GetFirstIdWithMedia_SYSQUE(char *namePtr, int checkMediaVal);
int GetFirstIdWithStatus_SYSQUE(char *namePtr, int status);

void FreeSysQueElemGivenPos_SYSQUE(char *namePtr, int sqPos);





#endif       /* !_que_sys_h__ */
