#ifndef _listcore_h__
#define _listcore_h__

static char sccsid_listcore_h[] = "@(#)listcore.h	4.9 96/06/07 08:19:37";

/*******************************
* listcore.h
********************************/
#include <ulocks.h>

#define PID_LIST	"pids"
#define MSTRQ_LIST	"masterQ"
#define SIGCLD_LIST	"signals"
#define XWPREQ_QUE	"xwpreqque"
#define PPS_LIST	"pps_msgs"
#define READ_LIST	"reading"

/* the actual sublist is an array of type void 
   so that it can hold a pointer to any type
   as specified at the next layer up */

typedef struct baseListElemTypeTag{
                         char *name;
                         int  nsize;
                         int  nCurrentMaxSize;
                         usema_t *nSema;
                         void **arr; 
                       }baseListElemType;

usema_t *getbaseListSema();
int   getBaseSize();
void  *RemoveNameListElemAt_CORE(baseListElemType *nPtr, 
                                 char *namePtr, 
                                 int index);

int RemoveNameListAll_CORE(baseListElemType *nPtr,
                           char *namePtr);

baseListElemType  *lockNameList_CORE(char *namePtr);
void unLockNameList_CORE(baseListElemType *nPtr);
int AddElemToNameList_CORE(baseListElemType *nPtr, 
                           char             *namePtr, 
                           void             *value);

int InsertNewValueAt_CORE(baseListElemType *nPtr,
                      void                 *valToInsert,
                      int                  pos);

int createOneNameListEntry_CORE(char *namePtr);
int MoveToPos_CORE(baseListElemType *nPtr, int posTo, int posFrom);

int freeCoreSema();
#endif   /* !_listcore_h__ */
