#ifndef _xutils_h__
#define _xutils_h__

static char sccsid_xutils_h[] = "@(#)xutils.h	1.2 96/08/08 09:25:45";

void popdownMsgDialog(Widget w);
void queryPointer(int *x, int *y);
void loadFile(Widget textW, char *filename);

void showListening(Widget w) ;
void showNotListening(Widget w) ;
void makePosVisible(Widget listW, int listPos);

void sendEvent(Widget w, int type);
void iconifyWindow();

#endif       /* _xutils_h__ */
