static char *sccs = "@(#)ims_shm.c	5.1  17 Mar 1996";
/******************************************************************************
**
** File:	ims_shm.c
**
** Function: Perform shared memory services for the IMS/DADS function.
**
** Author: Dan Crichton	
**
** Date:	6/15/95
**
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
** These are needed by ipc.h           
*/

typedef unsigned long   ulong;
typedef unsigned short   ushort;

#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <signal.h>

#include <ims_shm.h>

#define MAX_WAIT_PID 50
#define MAX_TABLES   100
#define MAX_SEGMENTS 50

typedef struct POOL_TABLE
{
	int active;
	int shmkey;
	int shmid;
	int semid;
	int wait_list[MAX_WAIT_PID];
	int owner;
	int creator;
	int size;
} POOL_TABLE;

typedef struct 
{
	int shmid;
	int active;
	int pid;
} KEY_TABLE;


/*
** Local Functions
*/

int ims_shm_detach(char *);
int ims_shm_remove(int key);
char *ims_shm_attach(int shmkey);
int ims_shm_create(int shmkey, int size);
int santiyCheck(int shmkey);

/*
** Global 
*/

extern int errno;


/******************************************************************************
** ims_shm_getid
**
** Allocate a shared memory key to the caller.
******************************************************************************/
int ims_shm_getid(void)
{
	KEY_TABLE *key_table, *ptr;
	int keys;
	int shmid;
	int i;


	/*
	** Try to attach to key table.  If we can't attach, then 
	** rebuild the key table and allocate it.
	*/


	key_table = (KEY_TABLE *) ims_shm_attach(MEMORY_SHM_KEY);

	keys = IMS_SHM_PRIV_END - IMS_SHM_PRIV_START + 1;

	if (key_table == NULL)
	{
		
		if (ims_shm_create(MEMORY_SHM_KEY, sizeof(KEY_TABLE) * keys) < 0)
		{
			return(-1);
		}

		key_table = (KEY_TABLE *) ims_shm_lock(MEMORY_SHM_KEY);
		ptr = key_table;

		/*
		** Initialize the key_table structure. 
		*/

		for (i = 0; i < keys; i++)
		{
				ptr->active = 0;
				ptr->shmid = i + IMS_SHM_PRIV_START;
				ptr->pid = -1;
				ptr = (void *) ((char *) ptr + sizeof(KEY_TABLE));

		}

	}
	else
	{
		ims_shm_detach((void *) key_table);
		key_table = (KEY_TABLE *) ims_shm_lock(MEMORY_SHM_KEY);

	}

	for (i = 0; i < keys; i++)
	{
		if (key_table[i].active == 0)
	   		break;
		
		/*
		** Make sure the key is owned by a valid PID.
		*/

		if (getpgid(key_table[i].pid) == -1)
		{
			/*
			** PID does not exist so free slot.
			*/

			shmid = key_table[i].shmid;
			(void) ims_shm_unlock(MEMORY_SHM_KEY, (void *) key_table);
			(void) ims_shm_remove(shmid);
			key_table = (KEY_TABLE *) ims_shm_lock(MEMORY_SHM_KEY);

			key_table[i].pid = -1;
			key_table[i].active = 0;

			break;
		}
	}

	/*
	** Make sure a key was found.
	*/

	if (i == keys)
	{
		(void) ims_shm_unlock(MEMORY_SHM_KEY, (void *) key_table);
		return(-1);
	}


	key_table[i].active = 1;
	key_table[i].pid = getpid();
	shmid = key_table[i].shmid;


	(void) ims_shm_unlock(MEMORY_SHM_KEY, (void *) key_table);
	return(shmid);

}



/******************************************************************************
**
** ims_shm_attach
**
******************************************************************************/
char *ims_shm_attach(int shmkey)
{
	char *shmaddr;
	int shmid;

	/*
	** Get the shared memory id for read/write access.
	*/

	shmid = shmget(shmkey, 0, 0666);

	if (shmid < 0)
	{
		
		return(NULL);
	}	

	shmaddr = (void *) shmat(shmid, (char *) 0, 0);			
	return(shmaddr);
}


/******************************************************************************
**
** addNewBlock
**
** Adds a new block into the pool area for monitoring
******************************************************************************/
int addNewBlock(
	int shmkey, 
	int size,
	int shmid2,
	int semid)
{
	POOL_TABLE *pool_table;
	int i;
	int shmid;

	shmid = shmget(MEMORY_POOL_KEY, sizeof(POOL_TABLE) * MAX_TABLES, 
				0666 | IPC_CREAT);

	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	if (pool_table == NULL)
	{
		/*
		** Could not map into user space.
		*/
		return(-1);
	}

	/*
	** Check to see if the memory is still active in the pool table.
	*/

	for (i = 0; i < MAX_TABLES; i++)
	{
		if (pool_table[i].shmkey == shmkey)
		{
			break;
		}
	}


	/*
	** If we didn't find an old slot for this memory, then
	** get a new one.
	*/

	if (i == MAX_TABLES)
	{
		for (i = 0; i < MAX_TABLES; i++)
		{
			if (!pool_table[i].active)
			{
				pool_table[i].owner = 0;
				pool_table[i].creator = getpid();
				break;
			}
		}

	}
	pool_table[i].shmkey = shmkey;
	pool_table[i].size = size;
	memset(pool_table[i].wait_list, 0,
					sizeof(pool_table[i].wait_list));
	pool_table[i].active = 1;
	pool_table[i].shmid = shmid2;
	pool_table[i].semid = semid;
	(void) ims_shm_detach((char *) pool_table);
	return(0);

}

/******************************************************************************
**
** ims_shm_create
**
******************************************************************************/
int ims_shm_create(
	int shmkey, 
	int size)
{
	int shmid;
	int semid;

	static struct sembuf op_lock[2] = 
	{
		{0, 0, IPC_NOWAIT},   /* No wait for sem #0 to become 0 */
		{0, 1, IPC_NOWAIT}    /* then incremetn by 1 */
	};

	static struct sembuf op_unlock[1] = 
	{
		0, -1, IPC_NOWAIT		/* Decrement sem #0 by 1 */
	};

	/*
	** Get the semaphore id of a new or existing semaphore.
	*/

	shmid = shmget(shmkey, size, 0666 | IPC_CREAT);

	/*
	** Use semaphore service to provide mutual exclusion for 
	** locking/unlocking shared memory segments between competing
	** processes.  
	*/
	
/*	if (((semid = semget(shmkey, 1, 0666 | IPC_CREAT)) < 0) && (shmid))
*/
	if (((semid = semget(MEMORY_SHM_SEG, 1, 0666 | IPC_CREAT)) < 0) && (shmid))
	{
		perror("Could not create binary semaphore");
		printf("Errno = %d\n", errno);
		(void) shmctl(shmkey, IPC_RMID, NULL);
		shmid = -1;
		return(shmid);
	}


	/*
	** Update memory pool table to reflect new allocated block  
	*/

	addNewBlock(shmkey, size, shmid, semid);

	/*
	** Verify that no process is trying to block yet, or is left blocking
	** from a previous use of this key
	*/
#if 0

	semid = semget(MEMORY_SHM_SEG, 0, 0666); 

	if (semid < 0)
	{
		return(-1);
	}

	if (semop(semid, &op_lock[0], 2) < 0)
	{
		/*
		** Attempt to clear semaphore block case
		*/
		(void) semop(semid, &op_unlock[0], 1);
	}
	else
	{
		/*
		** Could lock it so unlock.
		** Note: Could just try to lock and always unlock.
		** but we might want to make this is a little smarter.
		*/

		(void) semop(semid, &op_unlock[0], 1);
	}
#endif

		
	return(shmid);
}

/******************************************************************************
**
** ims_shm_detach
**
******************************************************************************/
int ims_shm_detach(char *memptr)
{
	shmdt(memptr);
}

/******************************************************************************
**
** ims_shm_lock
**
** Exclusive lock of shared memory region.
******************************************************************************/
char *ims_shm_lock(int key)
{
	int semid;
	int i;
	POOL_TABLE *pool_table;
	static struct sembuf op_lock[2] = 
	{
		{0, 0, IPC_NOWAIT},   /* wait for sem #0 to become 0 */
		{0, 1, IPC_NOWAIT}    /* then incremetn by 1 */
	};
	struct timeval 
	{
		long tv_sec;
		long tv_usec;
	} tvptr;

	/*
	** Get semaphore id, and then lock it.
	*/

	semid = semget(MEMORY_SHM_SEG, 0, 0666); 

	if (semid < 0)
		return(NULL);

	/*
	** Block signals.
	*/

	if (key != IMS_JOB_SHMID)
		sighold(SIGCHLD);

	/*
	** This is a blocking call to wait for semaphore to be 0. 
	** Based on value of op_lock.
	*/


	while (semop(semid, &op_lock[0], 2) < 0)
	{
		/*
		** Could not get semaphore, so perform sanity check and wait.
		*/

		/*
		** Need to fix this sleep 1 second is too long!...
		*/

		tvptr.tv_sec = 0;
		tvptr.tv_usec = 1000;  /* 10 milliseconds. */
		select (0, 0, 0, 0, &tvptr);

		sanityCheck(key);

	}

	/*
	** It is locked for this pid, so update pool table.
	*/
	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	for (i = 0; i < MAX_TABLES; i++)
	{

		if (pool_table[i].shmkey == key)
		{
			/*
			** Found the shmid, so update it.
			*/

			pool_table[i].owner = getpid(); 
			break;

		}
	}
	(void) ims_shm_detach((char *) pool_table);


    return(ims_shm_attach(key));
}

/******************************************************************************
**
** ims_shm_unlock
**
** Unlock of shared memory region to unblock any other processes.
******************************************************************************/
int ims_shm_unlock(
	int key, 
	char *addr)
{
	int semid;
	int i;
	POOL_TABLE *pool_table;
	static struct sembuf op_unlock[1] = 
	{
		0, -1, IPC_NOWAIT		/* Decrement sem #0 by 1 */
	};
	int pid;



	/*
	** Get semaphore id, and then unlock it.
	*/

	semid = semget(MEMORY_SHM_SEG, 0, 0666); 


	/*
	** Go ahead and error out because most likely 
	** create/lock was not called prior to unlock.
	*/

	if (semid < 0)
		return(-1);

	/*
	** This is a blocking call to wait for semaphore to be 0. 
	** Based on value of op_unlock.
	*/


	/*
	** Clear pid out of wait_list.
	*/ 
	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	for (i = 0; i < MAX_TABLES; i++)
	{
		if (pool_table[i].shmkey == key)
		{
			pool_table[i].owner = 0; 
			for (pid = 0; i < MAX_WAIT_PID; i++)
			{
				if (pool_table[i].wait_list[pid] == getpid())
				{
					pool_table[i].wait_list[pid] == 0;
					break;
				}
			}
		}
	}
	(void) ims_shm_detach((char *) pool_table);

	if (semop(semid, &op_unlock[0], 1) < 0)
	{
		/*
		** Go ahead and error out because most likely 
		** lock was not called prior to unlock.
		*/

		return(-1);
	}
	if (key != IMS_JOB_SHMID)
		sigrelse(SIGCHLD);  /* Unblock signal */

    return(ims_shm_detach(addr));
}


/******************************************************************************
**
** ims_shm_remove
**
******************************************************************************/
int ims_shm_remove(int key)
{
	int i;
	POOL_TABLE *pool_table;
	KEY_TABLE *key_table;
	int shmid = -1;
	int semid = -1;
	int keys;

	/*
	** Remove semaphore and shm area
	*/

	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	for (i = 0; i < MAX_TABLES; i++)
	{

		if (pool_table[i].shmkey == key)
		{
			pool_table[i].active = 0;	
			break;
		}
	}
	(void) ims_shm_detach((char *) pool_table);

	shmid = shmget(key, 0, 0666);

	if (shmid < 0)
	{
		fprintf(stderr, "Could not find shared memory id in pool table\n");
		return(-1);
	}



	/*
	** If this is a unique key managed by the ims_shm library, 
	** then free the shared memory key.
	*/

	if ((key >= IMS_SHM_PRIV_START) && (key <= IMS_SHM_PRIV_END))
	{
		keys = IMS_SHM_PRIV_END - IMS_SHM_PRIV_START + 1;
		key_table = (KEY_TABLE *) ims_shm_lock(MEMORY_SHM_KEY);
		if (key_table != NULL)
		{
			for (i = 0; i < keys; i++)
			{
				if (key_table[i].shmid == key)
				{
					key_table[i].active = 0;
					key_table[i].pid = -1;
					break;
				}

			}
			(void) ims_shm_unlock(MEMORY_SHM_KEY, (void *) key_table);	
		}
	}

	/*
	** Okay,  remove the semaphore and shared memory segment...
	*/

#if 0
	/*
	** Not needed right now since we use one semaphore.
	*/

	semid = semget(MEMORY_SHM_SEG, 0, 0666); 

	if (semid > 0)
	{
		if (semctl(semid, 0, IPC_RMID, 0) < 0)
		{
			fprintf(stderr, "Could not remove binary semaphore.\n");
			return(-1);
		}
	}
#endif


	return(shmctl(shmid, IPC_RMID, NULL));
}


/******************************************************************************
**
** sanityCheck
**
** Make sure that processes in the wait queue, and the owner of the 
** current shared memory lock are still in existence.
******************************************************************************/
int sanityCheck(int shmkey)
{
	int i, x; 
	int owner, wait_pid;
	int semid;
	POOL_TABLE *pool_table;
	int stat;
	static struct sembuf op_unlock[1] = 
	{
		0, -1, IPC_NOWAIT		/* Decrement sem #0 by 1 */
	};

	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	for (i = 0; i < MAX_TABLES; i++)
	{
		if (pool_table[i].shmkey == shmkey)
		{
			/*
			** Found the block, now sanity check it...
			*/

			owner = pool_table[i].owner;

			if (owner > 0)
			{

				/*
				* Check existence of pid...
				*/
				

				if (getpgid(owner) == -1)
				{
					/*
					** Unlock block, pid is in a bad state.
					*/

					semid = semget(MEMORY_SHM_SEG, 0, 0666); 
					if (semid > 0)
						semop(semid, &op_unlock[0], 1);
					pool_table[i].owner = 0;

				}

			}

			/*
			** Now check to make sure that no non-existent pids are waiting 
			*/

			for (x = 0; x < MAX_WAIT_PID; x++)
			{
				wait_pid = pool_table[i].wait_list[x];

				if (getpgid(wait_pid) == -1)
				{
					/*
					** Clear pid from wait list
					*/

					pool_table[i].wait_list[x] = 0;

				}

			}

		}

	}
	(void) ims_shm_detach((char *) pool_table);
}

/******************************************************************************
**
** ims_dumpPoolTable
**
******************************************************************************/
int ims_dumpPoolTable()
{
	POOL_TABLE *pool_table;
	int i, x;
	
	pool_table = (POOL_TABLE *) ims_shm_attach(MEMORY_POOL_KEY);

	if (pool_table == NULL)
	   return(-1);

	
	fprintf(stderr, "       DUMP OF IMS/DADS SHARED MEMORY POOL\n");
	fprintf(stderr, "-----------------------------------------------------\n");
	fprintf(stderr, "\n");

	for (i = 0; i < MAX_TABLES; i ++)
	{
		if (pool_table[i].active)
		{
			fprintf(stderr, "Active Memory Segment %d\n", 
						pool_table[i].shmkey);
			fprintf(stderr, "   size =     %d\n", pool_table[i].size);
			fprintf(stderr, "   lock pid = %d\n", pool_table[i].owner);
			fprintf(stderr, "   creator  = %d\n", pool_table[i].creator);

			for (x = 0; x < MAX_WAIT_PID; x++)
			{
				if (pool_table[i].wait_list[x] > 0)
				{
					fprintf(stderr, " pid in wait queue = %d\n", 
							pool_table[i].wait_list[x]);

				}
			}
		}
	}


	(void) ims_shm_detach((char *) pool_table);
	return(0);
}

