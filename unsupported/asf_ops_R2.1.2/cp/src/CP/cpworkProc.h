/********************************
* cpworkproc.h
********************************/

static char sccsid_cpworkProc_h[] = "@(#)cpworkProc.h	4.7 96/11/22 14:34:15";

#include "odl.h"
#include "que_xwp.h"


void updatePosLabel(char *namePtr, int whichWidget , int listPos, char *value);
void updateStatusLabel(char *queue, int listPos, int status, char *namePtr);

int cpXworkProc(XtPointer cd);
void removeLineForJobId(char *namePtr, int jobId);
void removeLine(char *namePtr, int listPos);
void selectLine(Widget w, int listPos);


int doSendMsg(char *namePtr, int socket, ODL writeOutODL, int expectedMsg);

int handleXwpq(xwpQueElemType    *direct_xwpQueElemPtr);


int getTypeId(char *str);
int getModeId(char *str);

void initStateFile();
