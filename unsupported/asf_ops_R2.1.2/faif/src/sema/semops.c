/*==============================================================================
Filename:	semops.c

Description:	
	This module contains the routines that set up the semaphores
used by FAIF.  It includes the semaphore initialization, lock, unlock
and destroy functions.

External Functions:
	sem_init
	sem_destroy
	sem_lock
	sem_unlock
	
Static Functions:
	sem_error
	
External Variables Defined:
	None
	
File Scope Static Variables:
	semval
	
Notes:

==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <syslog.h>
#include "semops.h"
#include "faifdefs.h"

#ifdef __STDC__
int sem_init(int) ;
int sem_destroy(int) ;
int sem_lock(int) ;
int sem_unlock(int) ;
static void sem_error(void) ;
#else
int sem_init() ;
int sem_destroy() ;
int sem_lock() ;
int sem_unlock() ;
static void sem_error() ;
#endif

/* Semaphore value storage
*/
static union semun
{
   int val ;
   struct semid_ds *buf ;
   ushort *array ;
} semval ;



/*==============================================================================
Function:	int sem_init(int semkey)

Description:	
	This function tries to get the semaphore for semkey.  If it cannot
get the semaphore, it means that there is not one existing for the specified
key so it creates one.  The value of the semaphore is initialized to 1
(for unlocked) when it is created.  The semaphore id obtained is returned.

Parameters:
	int semkey - key for semaphore to create 

Returns:	semaphore id, ERROR
Creator:	Norbert Piega	
Creation Date:	09/28/94
Notes:		
==============================================================================*/
#ifdef __STDC__
int
sem_init(int semkey)
#else
int
sem_init(semkey)
   int semkey ;
#endif
{
   char logmsg[MAX_SYSLOG_MSGLEN] ;
   int semid ;

   /* Get the semaphore with the specified key
   */
   if ((semid = semget(semkey, 1, 0400))== -1)
   {
      /* Can't get semaphore, create it
      */
      if (errno == ENOENT)
      {
         if ((semid = semget(semkey, 1, IPC_CREAT | 0600)) == -1)
	 {
	    sem_error() ;
	    sprintf(logmsg,
	       "WARNING, Failed to create a semaphore, key %d", semkey) ; 
	    syslog(LOG_DEBUG, logmsg) ; 
	    return(ERROR) ; 
	 }
	 else 
	 { 
	    sprintf(logmsg,
	       "NOTICE: Successfully created semaphore, key %d\n", semkey) ; 
	    syslog(LOG_NOTICE, logmsg) ;
	    semval.val = 1 ; 
	    semctl(semid, 0, SETVAL, semval) ; 
	 } 
      }
      else 
      { 
	 sprintf(logmsg, 
	    "WARNING, Failed to create a semaphore, key %d", semkey) ; 
	 syslog(LOG_DEBUG, logmsg) ;
	 return(ERROR) ; 
      }
   }
   else
   {
      sprintf(logmsg, "NOTICE: Got already existing semaphore, key %d\n",
	 semkey) ;
      syslog(LOG_DEBUG, logmsg) ;
   }

   semctl(semid, 0, GETVAL, semval) ;
   sprintf(logmsg, 
      "NOTICE: Value of semaphore (id %d) currently set to %d\n",
      semid, semval.val) ;
   syslog(LOG_NOTICE, logmsg) ;

   return(semid) ;

} /* sem_init */




/*==============================================================================
Function:	int sem_destroy(int semid)

Description:	
	This function destroys the semaphore with the specified id.

Parameters:
	int semid - id associated with the semaphore to be destroyed

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	09/28/94
Notes:		
==============================================================================*/
#ifdef __STDC__
int
sem_destroy(int semid)
#else
int
sem_destroy(semid)
   int semid ;
#endif
{
   char logmsg[MAX_SYSLOG_MSGLEN] ;

   if (semctl(semid, 0, IPC_RMID, semval) == -1)
   {
      sprintf(logmsg, "WARNING, Semaphore (id %d) destroy failed\n", semid) ;
      syslog(LOG_DEBUG, logmsg) ; 
      return(ERROR) ;
   }
   return(OK) ;

} /* sem_destroy */





/*==============================================================================
Function:	int sem_lock(int semid)

Description:	
	This function "locks" the semaphore with the specified semaphore
id.  It checks if the semaphore value is 0, in which case it is already
locked and thus returns ERROR.  Otherwise, the semop function is used to
raise the semaphore value ("lock" it).

Parameters:
	int semid - id of semaphore to lock

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	09/28/94
Notes:		
==============================================================================*/
#ifdef __STDC__
int
sem_lock(int semid)
#else
int
sem_lock(semid)
   int semid ;
#endif
{
   static struct sembuf sops = {0, -1, 0} ;
   int retval =  -1 ;
   char logmsg[MAX_SYSLOG_MSGLEN] ;

   semctl(semid, 0, GETVAL, semval) ;
   if (semval.val == 0)
   {
      sprintf(logmsg, "WARNING, Semaphore with id %d is locked\n", semid) ;
      syslog(LOG_DEBUG, logmsg) ;
      return(ERROR) ;
   }

   if ((retval = semop(semid, &sops, 1)) == -1)
   {
      sprintf(logmsg, 
	 "WARNING, Failed to acquire the semaphore with id %d\n", semid) ;
      syslog(LOG_DEBUG, logmsg) ;
      return(ERROR) ;
   }

   semctl(semid, 0, GETVAL, semval );
   sprintf(logmsg, "NOTICE: Value of semaphore is now %d\n", semval.val);
   syslog(LOG_NOTICE, logmsg) ;

   return(OK) ;

} /* sem_lock */




/*==============================================================================
Function:	int sem_unlock(int semid)

Description:	
	This function "unlocks" the semaphore with the specified semaphore
id.  It checks if the semaphore value is 1, in which case it is already
unlocked and thus simply returns OK.  Otherwise, the semop function is used 
to lower the semaphore value ("unlock" it).


Parameters:
	int semid - id of semaphore to unlock

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	09/28/94
Notes:		
==============================================================================*/
#ifdef __STDC__
int
sem_unlock(int semid)
#else
int
sem_unlock(semid)
   int semid ;
#endif
{
   static struct sembuf sops = {0, 1, 0} ;
   int retval = -1 ;
   char logmsg[MAX_SYSLOG_MSGLEN] ;

   semctl(semid, 0, GETVAL, semval) ;
   if (semval.val == 1)
      return(OK) ;

   if ((retval = semop(semid, &sops, 1)) == -1)
   {
      sprintf(logmsg, 
	 "WARNING, Unable to release semaphore with id %d\n", semid) ;
      syslog(LOG_DEBUG, logmsg) ;
      return(ERROR) ;
   }

   semctl(semid, 0, GETVAL, semval );
   sprintf(logmsg, "NOTICE: Value of semaphore is now %d\n", semval.val );
   syslog(LOG_NOTICE, logmsg) ;

   return(OK) ;

} /* sem_unlock */






/*==============================================================================
Function:	static void sem_error(void)

Description:	
	This routine simply prints out a syslog message based on the
current value of errno.

Parameters:	None
Returns:	None
Creator:	Norbert Piega	
Creation Date:	12/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static void
sem_error(void)
#else
static void
sem_error()
#endif
{
   switch(errno)
   {
      case EACCES:
	 syslog(LOG_DEBUG, 
	    "WARNING, semget EACCES error: semaphore access error\n") ;
         break ;

      case EEXIST:
	 syslog(LOG_DEBUG, 
	    "WARNING, semget EEXIST error: semaphore semflg error\n") ;
         break ;

      case EINVAL:
	 syslog(LOG_DEBUG, 
	    "WARNING, semget EINVAL error: semaphore limit error\n") ;
         break ;

      case ENOENT:
	 syslog(LOG_DEBUG, 
	    "WARNING, semget ENOENT error: semaphore does not exist\n") ;
         break ;

      case ENOSPC:
	 syslog(LOG_DEBUG, 
	    "WARNING, semget ENOSPC error: semaphore system limit error\n") ;
         break ;
   }

} /* sem_error */


/* End of File */
