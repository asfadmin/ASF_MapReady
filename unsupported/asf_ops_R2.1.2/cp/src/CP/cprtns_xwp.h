#ifndef _cprtns_xwp_h__
#define _cprtns_xwp_h__
/*-------------------------------------------
* FILE:
*   cprtns_xwp.h
*
*------------------------------------------*/

static char sccsid_cprtns_xwp_h[] = "@(#)cprtns_xwp.h	1.3 96/08/08 09:27:57";

#include "listcore.h"
#include "listmstrq.h"
#include "que_xwp.h"
#include "odl.h"


int ASFlogMessage_CB(char *namePtr, int logDest, dialogCBdata *cbData,
                  char *format, ...);
int ASFlogMessage(char *namePtr, int logDest, char *format, ...);
int ASFlogMessageRepl(char *namePtr,int logDest, int replPos, char *format,...);
int ASFlogMessageDel(char *namePtr,int logDest, int replPos, char *format, ...);

int ASFlogMessage_direct(char *namePtr, int logDest, char *format, ...);
int ASFlogMessageRepl_direct(char *namePtr, int logDest, int replPos,
              char *format, ...);
int ASFlogMessageDel_direct(char *namePtr, int logDest, int replPos,
              char *format, ...);
int ASFlogMessageInsert_direct(char *namePtr, int logDest, int pos,
              char *format, ...);
int ASFlogMessageSel_direct(char *namePtr, int logDest, int replPos,
              char *format, ...);

int ASFlogMessage_direct_CB(char *namePtr, int logDest, dialogCBdata *cbData,
                            char *format, ...);

#endif       /* !_cprtns_xwp_h__ */






