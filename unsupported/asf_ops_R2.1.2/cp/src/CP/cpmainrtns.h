#ifndef _cpmainrtns_h__
#define _cpmainrtns_h__

static char sccsid_cpmainrtns_h[] = "@(#)cpmainrtns.h	1.3 96/10/25 10:56:49";

char *getSubsysNameNo(int i);
int isLogFileSyslog(char *namePtr);

void getEvent();
char *get_CP_inetPtr();
char *getLogLocation();
int getSubsysNoGivenName(char *namePtr);

int colorCodeQUEwindow(char *namePtr, Widget *widlist);


#endif       /* _cpmainrtns_h__ */


