#define SAVE_PROCESSOR_NAME /* */

#ifndef _listmstrq_h__
#define _listmstrq_h__

static char sccsid_listmstrq_h[] = "@(#)listmstrq.h	4.29 97/04/14 12:17:05";

#include <sys/time.h>  /* for struct timeval below */
#include <sys/param.h>  /* MAXPATHLEN */

#include "odl.h"

typedef enum ppsStatusTime { P_SCAN, P_DECODE, P_FRAME, P_QC, P_CATALOG, P_LAST } ppsStatusType;

#define MAX_SUBSYSTEM_CREATED_FILES 20 /* max output files that can be */
                                       /* created by any subsystem */
                                       /* (.D .L .M) + 5*(.D .L .M) = 18 */
#define MAX_JOB_CREATED_FILES 40       /* max output files that can be */
                                       /* created by a running a whole job */
#define MAX_JOB_CREATED_DIRS  10       /* max directories that can be */
                                       /* created by running a whole job */
                                /* 2*scan results + 2*cal params + 2*image  */
typedef struct {
   char namePtr[MAX_SUBSYS_NAME_LEN];
   int  jobId;
   int  numProducts;  /* number of entries in fileName array to delete at end */
   int  numFiles;     /* number of active entries in fileName array */
   int  numDirs;      /* number of active entries in dirName array */
   char dirName[MAX_JOB_CREATED_DIRS][MAXPATHLEN];
   char fileName[MAX_SUBSYSTEM_CREATED_FILES][MAXPATHLEN];
   char productName[MAX_JOB_CREATED_FILES][MAXPATHLEN];
}subsysFilesType;


/* may remove */
typedef struct {
                int jobId;
                int subsysCategoryID;
                int instNo;
                int pos;
               }subsysListLocatorType;
/* end may remove */

#define MAX_PPS_TIME_ITEMS 10  /* total number of 'struct timeval' values from
                                  mstrqListElemType struct below that actually
                                  get sent to PPS (is total # - 2)  */

typedef struct mstrqListElemTag{
                 int  jobId;
                 struct timeval lastStartTime;
                 struct timeval lastEndTime;
                 struct timeval scanStartTime;
                 struct timeval scanEndTime;
                 struct timeval decodeStartTime;
                 struct timeval decodeEndTime;
                 struct timeval frameStartTime;
                 struct timeval frameEndTime;
                 struct timeval qcStartTime;
                 struct timeval qcEndTime;
                 struct timeval catalogStartTime;
                 struct timeval catalogEndTime;
                 char mqFmt[MAX_MAIN_LIST_ITEMS][MAX_LEN_Q_FMT];  
                 char mqLabel[MAX_MAIN_LIST_ITEMS][MAX_LEN_Q_STATUS_TEXT];  
                 char *namePtr;
                 char *SSP2namePtr;
                 char *RDSnamePtr;
                 char *inputDir;
                 char *outputDir;
                 int retryCount;
                 int qcType;  /* 0 = image; 1 = scan ; defined in cpdefines.h */
                 char *scanResultsFileNamePtr;
                 char *imageFileNamePtr;
                 char *leaderFileNamePtr;
                 char *avgFileNamePtr;
                 char *pmfFileNamePtr;
                 char failureReason[MAX_PPS_COMMENT_LENGTH];
#ifdef SAVE_PROCESSOR_NAME
                 char *processor;
#endif
                 int  numCalProductsStored;
                 int  numCalProducts;
                 int  sendAck; /* whether need to send ACK to IMS */
                 int  fileVersion;
                 int  catalogStep;
                 int  catalogStepDone; /* 0 = done; 1 = in progress */
            
                 int  modifiedFlag;
                 int  dataSource;  /* 0 = from disk;  1 = from tape */
                 int  dataReadyFlag;  /* set when tape mounted */
                 int  status;
                 ODL  mstrqODLreq;
                 subsysFilesType    subsysFiles;

}mstrqListElemType;

#define DATA_FROM_DISK 0
#define DATA_FROM_TAPE 1

char * BuildMountRequestLabel(mstrqListElemType *mstrqListElemPtr);
mstrqListElemType *GetCopyGivenJobId__MSTRQLIST(int jobId);



int CreateMstrqList_MSTRQLIST();

int GenProdNoPrivate__MSTRQLIST();

int AddMstrqToList_MSTRQLIST(char *mstrqTypePtr, 
                             int  modifiedFlag,
                             char *processor,
                             ODL  mstrqODLreq);

int  InsertNewElemAt_MSTRQLIST(int  pos,
                               char *mstrqTypePtr, 
                               int  modifiedFlag,
                               ODL  mstrqODLreq);

mstrqListElemType *GetFirstModifiedElem__MSTRQLIST();
mstrqListElemType *GetFirstWaitingElem__MSTRQLIST(int *index);
mstrqListElemType *GetNextNeedingMountElem__MSTRQLIST(int *pos);

int RemoveODLreqFromList_MSTRQLIST(int elemIndex);

mstrqListElemType *RemoveMstrqFromList_MSTRQLIST(int elemIndex);

int ChangeStatus__MSTRQLIST(int jobId, char *namePtr, int status);

int GetStatus__MSTRQLIST(char *namePtr, int  jobId);

int ChangeStatusToInterrupted_MSTRQLIST(char *namePtr, int mode);

int ResetFirstInterruptedStatus_MSTRQLIST(char *namePtr);

int GetFirstElemWithStatus_MSTRQLIST(char *namePtr, int  status);
mstrqListElemType *newGetFirstElemWithStatus_MSTRQLIST(int  status);

int  MoveElemPrivate_MSTRQLIST(baseListElemType *nPtr,
                               int elemTo,
                               int elemFrom );

int  MoveElem_MSTRQLIST(int elemTo,
                        int elemFrom );

int  MoveToHead_MSTRQLIST(int elemToMove );

int  MoveToTail_MSTRQLIST(int elemToMove );

int  setSubsysListPos_MSTRQLIST(subsysListLocatorType *subsysListLocatorPtr);

int  getSubsysListPos_MSTRQLIST(subsysListLocatorType *subsysListLocatorPtr);

int SetQCFileNames_MSTRQLIST(int   jobId, char *leaderFileNamePtr,
                                         char *imageFileNamePtr,
                                         char *avgFileNamePtr);

int GetQCFileNames_MSTRQLIST(int   jobId, char **leaderFileNamePtr,
                                         char **imageFileNamePtr,
                                         char **avgFileNamePtr);

char *GetProductType_MSTRQLIST(int jobId);
char *GetCompensationType_MSTRQLIST(int jobId);

char *GetFailureReason_MSTRQLIST(int jobId);
int SetFailureReason_MSTRQLIST(int jobId, char *reason);

int GetCatalogStep_MSTRQLIST(int jobId);
int SetCatalogStep_MSTRQLIST(int jobId, int catalogStep);
int IncrementCatalogStep_MSTRQLIST(int jobId);

int setStartTime_MSTRQLIST(int jobId, ppsStatusType pType, 
                           struct timeval ackTime);
int getStartTime_MSTRQLIST(int jobId, ppsStatusType pType, 
                           struct timeval *retTime);

int setEndTime_MSTRQLIST(int jobId, ppsStatusType pType, 
                           struct timeval ackTime);
int getEndTime_MSTRQLIST(int jobId, ppsStatusType pType, 
                           struct timeval *retTime);

int SetFileVersion_MSTRQLIST(int jobId, int fileVersion);
int GetFileVersion_MSTRQLIST(int jobId);



mstrqListElemType *ReadElem_MSTRQLIST(FILE *fp,int *err,int *insert,int *hold);
int ReadIn_MSTRQLIST(char *filePtr); 
int WriteOut_MSTRQLIST(FILE *fp);

mstrqListElemType * GetReqGivenListPos_MSTRQLIST(int listPos);

char *GetSSPname_MSTRQLIST(int jobId);
int   SetSSPname_MSTRQLIST(int jobId, char *SSP2name);

char *GetRDSname_MSTRQLIST(int jobId);
int   SetRDSname_MSTRQLIST(int jobId, char *RDSname);

char *GetProcessor_MSTRQLIST(int jobId);
int   SetProcessor_MSTRQLIST(int jobId, char *processor);


char *GetInputDir_MSTRQLIST(int jobId);
int   SetInputDir_MSTRQLIST(int jobId, char *inputDir);

char *GetOutputDir_MSTRQLIST(int jobId);
int   SetOutputDir_MSTRQLIST(int jobId, char *outputDir);


int GetDispPos_MSTRQLIST(int jobId);
int ChangeSubsys__MSTRQLIST(int jobId, char *namePtr);

int SetScanResultsFileName_MSTRQLIST(int jobId, char *scanResultsFileNamePtr);
int GetScanResultsFileName_MSTRQLIST(int jobId, char **scanResultsFileNamePtr);

void FreeMstrqListElem_MSTRQLIST(mstrqListElemType *mqel);
int IncrementRetry_MSTRQLIST(int jobId);
int DecrementRetry_MSTRQLIST(int jobId);
int GetRetry_MSTRQLIST(int jobId);

int GetSendAck_MSTRQLIST(int jobId);
int SetSendAck_MSTRQLIST(int jobId);
int ClearSendAck_MSTRQLIST(int jobId);

int SetDataReady_MSTRQLIST(int jobId);
int ClearDataReady_MSTRQLIST(int jobId);

int SetRequestLabel_MSTRQLIST(int jobId, int labelNo, char *string);

int SetSubsysFilesGivenJobId_MSTRQLIST(int jobId, subsysFilesType *subsysFiles);
subsysFilesType *GetSubsysFilesGivenJobId_MSTRQLIST(int jobId);

int GetQCtype_MSTRQLIST(int jobId);
int SetQCtype_MSTRQLIST(int jobId, int qcType);

int JobExists_MSTRQLIST(int jobId);

int SetPMFfileName_MSTRQLIST(int jobId, char *pmfFileNamePtr);
int GetPMFfileName_MSTRQLIST(int jobId, char **pmfFileNamePtr);

int GetNumCalProductsStored_MSTRQLIST(int jobId);
int GetNumCalProducts_MSTRQLIST(int jobId);
int SetNumCalProducts_MSTRQLIST(int jobId, int numCalProducts);
int IncrementNumCalProductsStored_MSTRQLIST(int jobId);

int ChangeStatusRestrict__MSTRQLIST(int jobId, char *namePtr, int status, 
       int currStatus);
char *GetQstatusAsText_MSTRQLIST(int status, char *namePtr);
int GetJobRank_MSTRQLIST(int *rank);


char *GetCatStepAsText_MSTRQLIST(int catStep);
char *GetQCTypeAsText_MSTRQLIST(int qcType);
char *GetDataSourceAsText_MSTRQLIST(int qcType);


#endif   /* _listmstrq_h__ */
