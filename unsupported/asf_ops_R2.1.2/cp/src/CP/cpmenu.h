#ifndef _cpmenu_h__
#define _cpmenu_h__

static char sccsid_cpmenu_h[] = "@(#)cpmenu.h	1.7 96/09/12 12:08:10";

#include "odl.h"

/* structure defs used within this file */
typedef struct {
                int    action;
                char   *namePtr;
                Widget buttonWid;
                int    pos;
                int    rev;
                char   platform[30];
} haltOrStopType;


void handleMenuStop(Widget mainWid);
void handleSubsysMenuExitCB(Widget w,XtPointer client_data,XtPointer call_data);


void handleMenuStartAllCB();
void handleMenuFileAsCB(char *ptr);
void handleSubsysMenuDelete(Widget mainWid);

void handleSubsysMenuDetailedInfo(Widget mainWid, XtPointer client_data, 
     XtPointer call_data);
void showJobInfo(int jobId, char *namePtr, ODL odl);

int handleSubsysAutoQCspawn(char *namePtr, int jobId);
int handleSubsysAutoScanQCspawn(char *namePtr, int jobId);
int handleSubsysPreQCspawn(char *namePtr, int jobId);

void performMenuItem(void *fn(), char *namePtr, Widget wid);

#endif /* _cpmenu_h__ */
