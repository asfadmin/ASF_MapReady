static char sccsid_pps_h[] = "@(#)pps.h	1.7 96/08/08 09:25:31";

typedef enum ppsReqTypes { P_REQ_FIRST = -1, 
                           P_SS_SCAN, P_CTS_SCAN, P_SS_FRAME, P_CTS_FRAME,
                           P_REQ_LAST 
} ppsReqType;

#define PPS_MSG_TYPE_SCAN  "SCAN"
#define PPS_MSG_TYPE_FRAME "FRAME"
#define PPS_MSG_MODE_SS    "SCANSAR"
#define PPS_MSG_MODE_CTS   "CONTINUOUS"

int GetMsgOut_PPS();
int SetMsgOut_PPS();
void ClearMsgOut_PPS();

int GetAvail_PPS();
void SetAvail_PPS();
void ClearAvail_PPS();

char * GetLastModeStr_PPS();
char * GetLastTypeStr_PPS();

char *getPPSlastMode();
char *getPPSlastType();
int getPPSlastRequested();

void doPPS();
void initPPS();

