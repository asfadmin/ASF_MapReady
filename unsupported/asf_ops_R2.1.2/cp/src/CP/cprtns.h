#ifndef _cprtns_h__
#define _cprtns_h__
/*-------------------------------------------
* FILE:
*   cprtns.h
*
*------------------------------------------*/

static char sccsid_cprtns_h[] = "@(#)cprtns.h	4.20 96/10/25 11:20:21";

#include <Xm/Xm.h>
#include "listpid.h"
#include "que_xwp.h"

extern dialogCBdata *doCreateQuestionDlg(Widget wid, dialogCBdata *cbData);


#define MAX_CHILD_PROCESSES 20  /* used for dimension of global signal arrays */


void soundBell();
void do_sig_cld(int pid, int status);
void sig_cld();
void doAccept();
void initMediaMount();

void showHelp(char *help_text, char *version);
void doSendHeartbeat(Widget mainWid);
void spawnLogBrowser(char *logLevel, Widget mainWid);
void showSubsysDetailedInfo(Widget subsysWid, XtPointer ptr);


                                                       /* display-related */
void resetStateColor(int row, int col, Widget rc);
void updateStateColor(int row, int col, Widget rc);
int getToggleScanDest();
int getToggleQCstart();
int mapStateForDisplay(int state);

                                                       /* subsystem-related */
char *getNameGivenObject(char *namePtr);
char *getRDSNameGivenObject(char *namePtr, int i);
char *getSSPNameGivenObject(char *namePtr, int i);
char * getIMSNameGivenObject(char *namePtr, int i);
char *getSSP2name(int i);
int getSSP2numGivenName(char *namePtr);
int getIMSnumGivenName(char *namePtr);
char * getSaveDirGivenName(char *namePtr);

void resetSubsysByName(char *namePtr);
int determineClassFromName(char *namePtr) ;
int GetSubsystemCategoryGivenName(char *namePtr);
int doStartSubsystem(char *namePtr, Widget buttonWid);

void spawnRequestUtil();
int spawnScanQC_CB();
int spawnQC_CB();
int spawnPreQC_CB(qcReqType *qcReqPtr);

void handlePanelStart(char *namePtr, Widget buttonWid);
int SendStopMsg(char *namePtr, int socket, int timeOutFlag);
void handleMenuStopAllCB();
void handleMenuResetAllCB();
void handleMenuExitCB(Widget w, XtPointer dPtr, XtPointer cPtr);

                                              /* dialog callback routines */

void popupCleanupBox (xwpQueElemType *xwpEl);

void CleanupConfirmCB(Widget w, XtPointer dPtr, XtPointer cPtr);

 
#define QC_START_AUTOMATIC     0
#define QC_START_INTERACTIVE   1

#define EXPECT_NO_MSG      0
#define EXPECT_ACK_MSG     1
#define EXPECT_STATUS_MSG  2
#define EXPECT_REQUEST_MSG 3


#define TIME_STRING_SIZE 21  /* string length returned by timeval_to_string */
#endif       /* !_cprtns_h__ */
