/* #define LOCK_DEBUG /* */
/* #define DEBUG /* */

static char sccsid_listcore_c[] = "@(#)listcore.c	4.9 96/11/12 16:14:34";

#include <stdio.h>
#include <ulocks.h>
#include <malloc.h>
#include <syslog.h>
#include <string.h>  /* memmove */
#include <bstring.h> /* bzero */
#include "listcore.h"
#include "pthread_wrapper.h"

#include "logUtils.h" /* printLLog */
#include "memUtils.h" /* doMalloc, doRealloc */
#include "cplogs.h"

#define BASE_ARRAY_SIZE 20
#define NAME_ARRAY_SIZE 1000 

/* base list is array of pointers to base list elems */
baseListElemType    **baseList;
int                 baseListSize   = 0;
int                 currentMaxSize = 0;

void **GLOBAL_arr = NULL;
usptr_t *GLOBAL_usPtrHandle;
usema_t *GLOBAL_baseListSema;

/* internal prototypes */
baseListElemType *getBaseListEntryForName_CORE(char *namePtr);
baseListElemType *initNameListEntry_CORE();

/* end internal prototypes ................................*/

/*----------------------------------------------------------
 * NAME:
 *  getBaseListSize 
 *
 * DESCRIPTION:
 *  get the current size of the base list
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int getBaseListSize()
{
 return(baseListSize);

} /* end getBaseListSize ............*/


/*----------------------------------------------------------
 * NAME:
 *  freeCoreSema 
 *
 * DESCRIPTION:
 *  the core routine that frees the associated sema's for 
 *  the created lists and queues
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int freeCoreSema()
{
 int i;
 baseListElemType *nPtr;

  /* 
   * not needed if all lists created before threads
   *  usfreesema(GLOBAL_baseListSema, GLOBAL_usPtrHandle);
   */
  for (i = 0; i < getBaseListSize(); i++) {
   if (baseList[i] == NULL)
    return(-1);
   nPtr = baseList[i];
   pthread_mutex_destroy (&nPtr->nSema);
  }
  return(0);
} /* end freeCoreSema .......*/


/*----------------------------------------------------------
 * NAME:
 *  getbaseListSema 
 *
 * DESCRIPTION:
 *  an access routine that returns the base list sema
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
usema_t *getbaseListSema()
{
  return(GLOBAL_baseListSema);

} /* end getbaseListSema........*/


/*----------------------------------------------------------
 * NAME:
 *  checkBaseListAlloc 
 *
 *
 * DESCRIPTION:
 *  do the size check on the base list and increase it if
 *  necessary 
 *
 * NOTES:
 *  realloc never seems to work the 
 *  way man says the first time so 
 *  break into 2 cases
 *  -- do sgi ARENA malloc here
 *
 *---------------------------------------------------------*/
int checkBaseListAlloc()
{
 /* if base list does not exist, create */
 if (baseListSize == 0) {
  printfLLog(LOG_DEBUG, CREATE_BASE_LIST);
  /* pthread_init(); */
  baseList = (baseListElemType **)doMalloc(BASE_ARRAY_SIZE*sizeof(void **));
  if (baseList == NULL)
    return(-1);
  bzero(baseList, BASE_ARRAY_SIZE*sizeof(void **) );

  /* 
   * no base list sema needed if all lists created before threads
   * GLOBAL_baseListSema = usnewsema(GLOBAL_usPtrHandle, 1);
   * if (GLOBAL_baseListSema == NULL)
   *            return(-1);
   *
   *  retvalSemaCtl = usctlsema(GLOBAL_baseListSema, CS_RECURSIVEON);
   *  if (retvalSemaCtl == -1)
   *  {
   *    printf( "Cannot set sema recursive\n");
   *    return(-1);
   *  }
   * end no base list needed 
   */

  currentMaxSize = BASE_ARRAY_SIZE;
  return(0);
 } /* end initial case */

 /* 
  * see if we need more space  -- realloc never seems to
  * work the way man says the first time so break into 2 
  * cases  
  */
 if (baseListSize == currentMaxSize) {
  baseList = (baseListElemType **)doRealloc(baseList, 
                     (currentMaxSize + BASE_ARRAY_SIZE)*sizeof(void **)); 
  if (baseList == NULL)
       return(-1);

  currentMaxSize = currentMaxSize + BASE_ARRAY_SIZE;
  return(0);
 } 

  return(0);
} /* end checkBaseListAlloc..........*/



/*----------------------------------------------------------
 * NAME:
 *  initNameListEntry_CORE
 *
 * DESCRIPTION:
 *  initialize the named list entry on creation
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
baseListElemType *initNameListEntry_CORE()
{
 int i, retval;
 baseListElemType *nPtr;

  retval = checkBaseListAlloc();
  if (retval == -1)
          return(NULL);

  i = getBaseListSize();
  nPtr =  (baseListElemType *)doMalloc(sizeof(baseListElemType) );
  baseList[i] = nPtr;

  baseListSize++;

  return(nPtr); 
} /* end initNameListEntry_CORE */


/*----------------------------------------------------------
 * NAME:
 *  checkNameListAlloc 
 *
 * DESCRIPTION:
 *  check the size of the name list, is necessary make it 
 *  incrementally larger
 *
 * NOTES:
 *  realloc never seems to work the 
 *  way man says the first time so 
 *  break into 2 cases
 * -- do sgi ARENA malloc here
 *
 *---------------------------------------------------------*/
int checkNameListAlloc(baseListElemType *nPtr)
{
 int newSizeInBytes;

 /* see if we need more space  -- realloc never seems to
    work the way man says the first time so break into 2
    cases  */
 if (nPtr->nsize == nPtr->nCurrentMaxSize) {
   newSizeInBytes = (nPtr->nCurrentMaxSize + NAME_ARRAY_SIZE)*sizeof(void**);

   printf("attempting to realloc %d bytes for pointer 0x%x\n",
                    newSizeInBytes, nPtr->arr);
   nPtr->arr = (void **)doRealloc(nPtr->arr, newSizeInBytes);

   if (nPtr->arr == NULL) {
     printfLLog(LOG_ERR, CANNOT_GET_MEMORY);
     return(-1);
   }

   nPtr->nCurrentMaxSize = nPtr->nCurrentMaxSize + NAME_ARRAY_SIZE;
 } /* end grow list case */

 /* everythings okay, just return */
 return(0);

} /* end checkNameListAlloc..........*/


/*----------------------------------------------------------
 * NAME:
 *  getBaseListEntryForName_CORE
 *
 * DESCRIPTION:
 *  return the base list entry for list "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
baseListElemType *getBaseListEntryForName_CORE(char *namePtr)
{
 baseListElemType *nPtr;
 int i;

#ifdef DEBUG
printf("getBaseListEntryForName_CORE: %s baselist size %d\n", namePtr, getBaseListSize());
#endif

  for (i = 0; i < getBaseListSize(); i++) {
   nPtr = baseList[i];
   if (nPtr == NULL)
      return(NULL);

   if (nPtr->name == NULL)
      return(NULL);

#ifdef DEBUG
printf("getBaseListEntryForName_CORE: comparing name %s\n", nPtr->name);
#endif
 
   if (strcmp(nPtr->name, namePtr) == 0)
      return(nPtr);
  }
  return(NULL);
      
} /* end getBaseListEntryForName_CORE..*/


/*----------------------------------------------------------
 * NAME:
 *  createOneNameListEntry_CORE
 *
 * DESCRIPTION:
 *  create name list entry "namePtr" and install it in the
 *  base list
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int createOneNameListEntry_CORE(char *namePtr)
{
 baseListElemType *nPtr;
 void** arr;
 int retval, retvalSemaCtl;

   nPtr = initNameListEntry_CORE();
   if (nPtr == NULL)
        return(-1);

   arr = (void **)doMalloc(NAME_ARRAY_SIZE*sizeof(void **) );
   if (arr == NULL)
      return(-1);

   bzero(arr, NAME_ARRAY_SIZE);

   nPtr->name  = (char *)doMalloc(strlen(namePtr)+1);
   strcpy(nPtr->name, namePtr);
   retval = pthread_mutex_init (&nPtr->nSema, 0);
   if (retval == -1) {
    printfLLog(LOG_ERR, CANNOT_INIT_PTHREADS);
    exit(-1);
   }
   retvalSemaCtl = usctlsema(nPtr->nSema, CS_RECURSIVEON);
   if (retvalSemaCtl == -1) {
     printfLLog(LOG_ERR, CANNOT_SET_RECURSIVE);
     return(-1);
   }

   nPtr->arr   = arr;
   nPtr->nsize = 0;
   nPtr->nCurrentMaxSize = NAME_ARRAY_SIZE;

  return(0);

} /* end createOneNameListEntry_CORE.............*/


/*----------------------------------------------------------
 * NAME:
 *  getElemInNameListAt_CORE 
 *
 * DESCRIPTION:
 *  the be baselist element for list "namePtr" at position
 *  "pos"
 *
 * NOTES:
 *  -- may be obsoleted --
 *
 *---------------------------------------------------------*/
void *getElemInNameListAt_CORE(baseListElemType *nPtr, char *namePtr, int pos)
{
 int k;

  if (nPtr == NULL) {
    nPtr =  getBaseListEntryForName_CORE(namePtr);
    if (nPtr == NULL)
      return(NULL);
  }

  for (k = 0; k < nPtr->nsize; k++)
   if (pos == k)
    return(nPtr->arr[k]);

  return(NULL);
} /* getElemInNameListAt_CORE.....................*/


/*----------------------------------------------------------
 * NAME:
 *  lockNameList_CORE 
 *
 * DESCRIPTION:
 *  lock the name list "namePtr" so that it is thread safe
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
baseListElemType *lockNameList_CORE(char *namePtr)
{
 baseListElemType *nPtr;

#ifdef LOCK_DEBUG
if (strcmp(namePtr , READ_LIST) == 0 || strcmp(namePtr , SIGCLD_LIST) == 0)
printf("%d lockNameList_CORE: %s.....................................", getpid(), namePtr);
#endif
  nPtr =  getBaseListEntryForName_CORE(namePtr);
  if (nPtr == NULL)
   return(NULL);
  pthread_mutex_lock (&nPtr->nSema);
#ifdef LOCK_DEBUG
if (strcmp(namePtr , READ_LIST) == 0  || strcmp(namePtr , SIGCLD_LIST) == 0)
printf("GOT it\n");
#endif

  return(nPtr);

} /* lockNameList_CORE .......*/



/*----------------------------------------------------------
 * NAME:
 *  unLockNameList_CORE 
 *
 * DESCRIPTION:
 *  unlock name list "namePtr"
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void unLockNameList_CORE(baseListElemType *nPtr)
{
#ifdef LOCK_DEBUG
if (strcmp(nPtr->name , READ_LIST) == 0  || strcmp(nPtr->name, SIGCLD_LIST) == 0)
printf("%d unlockNameList_CORE: %s\n", getpid(), nPtr->name);
#endif

  pthread_mutex_unlock (&nPtr->nSema);

} /* unLockNameList_CORE .......*/



/*----------------------------------------------------------
 * NAME:
 *  getNameListSize_CORE 
 *
 * DESCRIPTION:
 *  get the current size of name list "namePtr"
 *
 * NOTES:
 *  -- may be obsoleted --
 *
 *---------------------------------------------------------*/
int getNameListSize_CORE(baseListElemType *nPtr, char *namePtr)
{
  if (nPtr == NULL) {
    nPtr = getBaseListEntryForName_CORE(namePtr);
    if (nPtr == NULL)
           return(-1);
  }
  return(nPtr->nsize);

} /* end getNameListSize_CORE................*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveNameListElemAt_CORE 
 *
 * DESCRIPTION:
 *  remove the element at "pos" from the name list 
 *  "namePtr" 
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
void *RemoveNameListElemAt_CORE(baseListElemType *nPtr, char *namePtr,int pos)
{
 void **ptr;
 int  size;
 void* retvalPtr;

  if (nPtr == NULL) {
    nPtr = getBaseListEntryForName_CORE(namePtr);
    if (nPtr == NULL)
      return(NULL);
  } /* end if */

  /* for readability... */
  size = nPtr->nsize;
  if (size == 0)
    return(NULL);
 
  ptr = nPtr->arr;
  retvalPtr = ptr[pos];

  /* bcopy(&ptr[pos+1], &ptr[pos], (size-pos-1)*sizeof(void **)); */
  memmove(&ptr[pos], &ptr[pos+1], (size-pos-1)*sizeof(void **));
 
  ptr[size-1] = 0;

  nPtr->nsize--; 

  return(retvalPtr);

} /* end RemoveNameListElemAt_CORE..*/


/*----------------------------------------------------------
 * NAME:
 *  RemoveNameListAll_CORE
 *
 * DESCRIPTION:
 *  remove the entire name list "namePtr"
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int RemoveNameListAll_CORE(baseListElemType *nPtr, char *namePtr)
{
 int  size;

  if (nPtr == NULL) {
    nPtr = getBaseListEntryForName_CORE(namePtr);
    if (nPtr == NULL)
      return(-1);
  } /* end if */

  /* for readability... */

  size = nPtr->nsize;
  if (size == 0)
    return(-1);


  bzero(nPtr->arr,(size)*sizeof(void **));
  nPtr->nsize = 0;

  return(0);

} /* end RemoveNameListAll_CORE..*/


/*----------------------------------------------------------
 * NAME:
 *  AddElemToNameList_CORE 
 *
 * DESCRIPTION:
 *  add an element to the name list "namePtr" 
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int AddElemToNameList_CORE(baseListElemType *nPtr, char *namePtr, void *value)
{
 int pos;
 int retval;

  retval = checkNameListAlloc(nPtr);
  if (retval == -1)
      return(-1);

  pos = nPtr->nsize; 
  nPtr->arr[pos] = value;
  nPtr->nsize++;

  return(0);

} /* end AddElemToNameList_CORE.....*/


/*----------------------------------------------------------
 * NAME:
 *  InsertNewValueAt_CORE 
 *
 * DESCRIPTION:
 *  a private routine to insert an element into a name list
 *  at position pos
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
int InsertNewValueAt_CORE(baseListElemType *nPtr, void *valToInsert, int pos)
{
 int retval;
 void **ptr;

  retval = checkNameListAlloc(nPtr);
  if (retval == -1)
      return(-1);

  /* for readability... */
  ptr = nPtr->arr;

  /* bcopy(&ptr[pos], &ptr[pos+1], (nPtr->nsize - pos)*sizeof(void **)); */
  memmove(&ptr[pos+1], &ptr[pos], (nPtr->nsize - pos)*sizeof(void **));

  ptr[pos] = valToInsert; 

  nPtr->nsize++;

  return(0);

} /* end InsertNewValueAt_CORE.............*/


/*----------------------------------------------------------
 * NAME:
 *  MoveToPos_CORE 
 *
 * DESCRIPTION:
 *  a private routine to move an element in a name list to 
 *  a new position
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
int MoveToPos_CORE(baseListElemType *nPtr, int posTo, int posFrom)
{
 void *valToMove;
 void **ptr;
 int  old, new, size;

  if (posTo < 0   || posTo   >= nPtr->nsize)
      return(-1);
  if (posFrom < 0 || posFrom >= nPtr->nsize)
      return(-1);

  /* see if we are really doing a move, 
      if not, just return normal status */
  if (posTo == posFrom)
      return(0);

  if (posTo < posFrom) {
   old   = posTo;
   new   = posTo + 1;
   size  = posFrom - posTo;
  }else{
   old   = posFrom + 1;
   new   = posFrom;
   size  = posTo - posFrom;
  }

  /* for readability... */
  ptr = nPtr->arr;

  /* get value to move */
  valToMove = ptr[posFrom];
  /* bcopy(&ptr[old], &ptr[new], (size)*sizeof(void **)); */
  memmove(&ptr[new], &ptr[old], (size)*sizeof(void **));

  ptr[posTo] = valToMove;

  return(0);

} /* end MoveToPos_CORE.............*/
