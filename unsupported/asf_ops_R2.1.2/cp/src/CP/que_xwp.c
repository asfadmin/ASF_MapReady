/* #define CB_DEBUG /* for debugging new dialogCBdata callback info */

/*----------------------------------------------------------
 * SCCS Header
 * File:que_xwp.c   Rev:4.2.0.0   Date:95/02/10
 *
 * Tag range 20,000-29,999
 *---------------------------------------------------------*/

static char sccsid_que_xwp_c[] = "@(#)que_xwp.c	4.10 96/08/07 16:23:53";


#include <stdio.h>
#include <syslog.h>

#include "odl.h"    
#include "logUtils.h"    
#include "memUtils.h"    

#include "listcore.h"
#include "que_xwp.h"
#include "cplogs.h"


/*----------------------------------------------------------
 * NAME:
 *  CreateXwpReqQue_XWPQUE 
 *
 * DESCRIPTION:
 *  create a thread safe x workproc queue and sema
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int  CreateXwpReqQue_XWPQUE()
{
 int retval;

  retval =  createOneNameListEntry_CORE(XWPREQ_QUE);
  return(retval);

} /* CreateXwpReqQue_XWPQUE...................*/


/*----------------------------------------------------------
 * NAME:
 *  AddXwpReqToQue_XWPQUE 
 *
 * DESCRIPTION:
 *  add an element to the work proc queue
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int  AddXwpReqToQue_XWPQUE(char *reqStrPtr, int  actionFlag, int  replPos,
                           char *srcPtr, dialogCBdata *cbData, int  xEntity)
{
 xwpQueElemType *xwpQueElemPtr;
 int retval;
 void *nPtr;

#ifdef CB_DEBUG
printf("AddXwpReqToQue_XWPQUE: reqstr %s srcptr %s\n", reqStrPtr, srcPtr);
printCBdata("AddXwpReqToQue_XWPQUE", cbData);
#endif
  nPtr = lockNameList_CORE(XWPREQ_QUE);
  if (nPtr == NULL)
     return(-1);

  /* set up the element to add */
  xwpQueElemPtr = (xwpQueElemType *)doMalloc(sizeof(xwpQueElemType));
  if (xwpQueElemPtr == NULL)
    return(-1);
  if (reqStrPtr == NULL)
    return(-1);
  xwpQueElemPtr->reqStrPtr = (char *)doMalloc(strlen(reqStrPtr) + 1);
  if (xwpQueElemPtr->reqStrPtr == NULL) {
   unLockNameList_CORE(nPtr);
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(-1);
  }
  xwpQueElemPtr->actionFlag = actionFlag;
  xwpQueElemPtr->replPos = replPos;
  if (srcPtr == NULL)
    return(-1);
  xwpQueElemPtr->srcPtr = (char *)doMalloc(strlen(srcPtr) + 1);
  if (xwpQueElemPtr->srcPtr == NULL) {
   unLockNameList_CORE(nPtr);
   printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
   return(-1);
  }
  
  if (cbData != NULL) {
#ifdef CB_DEBUG
printf("sizeof x->cbdata %d\n", sizeof(xwpQueElemPtr->cbData));
printf("doing memcopy from 0x%x to 0x%x of %d bytes\n", cbData, &xwpQueElemPtr->cbData, sizeof(dialogCBdata));  fflush(stdout);
#endif
  memcpy(&xwpQueElemPtr->cbData, cbData, sizeof(dialogCBdata));
#ifdef CB_DEBUG
printf("set action %d at 0x%x\n", xwpQueElemPtr->cbData.action, &xwpQueElemPtr->cbData.action); fflush(stdout);
printCBdata("AddXwpReqToQue_XWPQUE new item", &xwpQueElemPtr->cbData);
#endif
  }
  else {
    xwpQueElemPtr->cbData.action = actionFlag;
    xwpQueElemPtr->cbData.pos = 0;
    xwpQueElemPtr->cbData.rev = 0;
    xwpQueElemPtr->cbData.buttonWid = NULL;
    strcpy(xwpQueElemPtr->cbData.namePtr, srcPtr);
    strcpy(xwpQueElemPtr->cbData.platform, "");
  }

  strcpy(xwpQueElemPtr->reqStrPtr, reqStrPtr);
  strcpy(xwpQueElemPtr->srcPtr,    srcPtr);
  /* strcpy(xwpQueElemPtr->destPtr,   destPtr); */
  xwpQueElemPtr->xEntity         = xEntity;


  /* add the element to the xwpODLreq list */
  retval = AddElemToNameList_CORE(nPtr, XWPREQ_QUE, xwpQueElemPtr);

  unLockNameList_CORE(nPtr);
#ifdef CB_DEBUG
if (cbData != NULL) printCBdata("leaving AddXwpReqToQue_XWPQUE", &xwpQueElemPtr->cbData);
#endif
  return(retval);

} /* end AddXwpReqToQue_XWPQUE....................*/


/*----------------------------------------------------------
 * NAME:
 *  FindXwpReqInQue_XWPQUE 
 *
 * DESCRIPTION:
 *  find a specific element in the queue 
 *
 * NOTES:
 *  may be removed
 *
 *---------------------------------------------------------*/
xwpQueElemType *FindXwpReqInQue_XWPQUE(baseListElemType *nPtr, 
                                       ODL xwpODLreq, int *index)
{
   return(NULL);

} /* end FindXwpReqInQue_XWPQUE.................*/


/*----------------------------------------------------------
 * NAME:
 *  FreeXwpQueElement_XWPQUE
 *
 * DESCRIPTION:
 *  do all calls necessary to free memory associated
 *  with the queue element
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void FreeXwpQueElement_XWPQUE(xwpQueElemType *xwpQueElemPtr)
{
  doFree(xwpQueElemPtr->reqStrPtr);
  doFree(xwpQueElemPtr->srcPtr);
/*  doFree(xwpQueElemPtr->destPtr); */
  doFree(xwpQueElemPtr); 

}  /* FreeXwpQueElement_XWPQUE........*/


/*----------------------------------------------------------
 * NAME:
 *  GetHeadOfXwpQue_XWPQUE 
 *
 *
 * DESCRIPTION:
 *  remove the head of the x work proc queue
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
xwpQueElemType *GetHeadOfXwpQue_XWPQUE()
{
 void *retvalPtr;
 xwpQueElemType *xwpQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(XWPREQ_QUE);
  if (nPtr == NULL)
      return(NULL);

  retvalPtr = RemoveNameListElemAt_CORE(nPtr, XWPREQ_QUE,0);
  if (retvalPtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(NULL);
  }
  xwpQueElemPtr = (xwpQueElemType *)retvalPtr;
  unLockNameList_CORE(nPtr);

  return(xwpQueElemPtr);

} /* GetHeadOfXwpQue_XWPQUE.............*/



/*----------------------------------------------------------
 * NAME:
 *  GetTypeStringGivenXwpReq_XWPQUE 
 *
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *  may not be needed
 *
 *
 *---------------------------------------------------------*/
char *GetTypeStringGivenXwpReq_XWPQUE(int i)
{
 xwpQueElemType *xwpQueElemPtr;
 baseListElemType *nPtr;

  nPtr = lockNameList_CORE(XWPREQ_QUE);

  xwpQueElemPtr =  nPtr->arr[i];
  if (xwpQueElemPtr == NULL) {
   unLockNameList_CORE(nPtr);
   return(NULL);
  } 

  unLockNameList_CORE(nPtr);
  return(xwpQueElemPtr->reqStrPtr);
} /* end GetTypeStringGivenXwpReq_XWPQUE...........*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveAll_XWPQUE 
 *
 * DESCRIPTION:
 *  remove all elements from the x work proc queue
 *
 * NOTES:
 *  VISIBLE
 *
 *---------------------------------------------------------*/
int RemoveAll_XWPQUE()
{
 xwpQueElemType *xwpQueElemPtr;
 baseListElemType *nPtr;
 int i;

  nPtr = lockNameList_CORE(XWPREQ_QUE);
  if (nPtr == NULL)
     return(-1);

  for (i = 0; i < nPtr->nsize; i++) {
   xwpQueElemPtr = (xwpQueElemType *)nPtr->arr[i];
   if (xwpQueElemPtr == NULL)
         return(-1);

   FreeXwpQueElement_XWPQUE(xwpQueElemPtr);
  } /* end for loop */

  RemoveNameListAll_CORE(nPtr, XWPREQ_QUE);

  return(0);

} /* RemoveAll_XWPQUE.................*/

