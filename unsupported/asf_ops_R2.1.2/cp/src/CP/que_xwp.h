#ifndef _que_xwp_h__
#define _que_xwp_h__

/*----------------------------------------------------------
 * SCCS Header
 * File:que_xwp.h   Rev:4.2.0.0   Date:95/02/10
 *
 *---------------------------------------------------------*/

static char sccsid_que_xwp_h[] = "@(#)que_xwp.h	4.4 96/04/10 19:47:22";

#include <Xm/Xm.h>
#include "odl.h"
#include "listcore.h"
#include "cpdefines.h"

typedef struct {
  int action;
  char namePtr[MAX_SUBSYS_NAME_LEN];
  Widget buttonWid;
  int pos;
  int rev;
  char platform[MAX_PLATFORM_NAME_LEN];
} dialogCBdata;


typedef struct xwpQueElemTag{
                           char       *reqStrPtr;
                           int        actionFlag;
                           int        replPos;
                           char       *srcPtr; 
                           int        xEntity;
                           dialogCBdata cbData;
                          }xwpQueElemType;

xwpQueElemType *FindXwpReqInQue_XWPQUE(baseListElemType *nPtr,
                                       ODL xwpODLreq, int *index);
int  CreateXwpReqQue_XWPQUE();
int  AddXwpReqToQue_XWPQUE(char *reqStrPtr, 
                           int  actionFlag,
                           int  pos,
                           char *srcPtr,
                           /* char *destPtr, */
                           dialogCBdata *cbData,
                           int   xEntity);

void FreeXwpQueElement_XWPQUE(xwpQueElemType *xwpQueElemPtr);
xwpQueElemType *GetHeadOfXwpQue_XWPQUE();
char *GetTypeStringGivenXwpReq_XWPQUE(int i);

#endif       /* !_que_xwp_h__ */
