/******************************************************************************
**
** File:        ims_shm.h
**
** Function:    Include file for shared memory services.
**
** Author:      Dan Crichton	
**
** Date:        6/15/95
**
******************************************************************************/

#ifndef _IMS_SHM_H
#define _IMS_SHM_H

static char *sccsShm = "@(#)ims_shm.h	5.1  03/17/96";

/*
** Constant Shared Memory Keys....
** Each should be unique.
*/

#define IMS_SHM_PRIV_START  65000
#define IMS_SHM_PRIV_END    65400
#define MEMORY_POOL_KEY     65401 /* Pool Table */
#define MEMORY_SHM_KEY      65402 /* Shared Memory Key Table */
#define MEMORY_SHM_SEG      65403 /* Shared Memory Concurrency Table */
#define IMS_JOB_SHMID       65444
#define IMS_MEDIA_SHMID     65445
#define IMS_FA_SHMID   		65446

/*
** Shared Message Queue Type
*/

typedef struct IMS_SHM_QUEUE 
{
	long mtype;
	int severity;
	char mtext[100];
} IMS_SHM_QUEUE;


/*
** Function prototypes for ims_shm.c module.
*/

int ims_shm_detach(char *);
int ims_shm_remove(int);
char *ims_shm_attach(int);
int ims_shm_create(int, int);
int ims_shm_unlock(int, char *);
char *ims_shm_lock(int);
int ims_dumpPoolTable();

#endif	/* !_IMS_SHM_H */

