static char *sccs = "@(#)ims_job_tool.c	5.1  03/17/96";
/******************************************************************************
**
** File:	ims_job_tool.c
**
** Function: Shared memory tool. 
**
** Author: Dan Crichton	
**
** Date:	6/21/95
**
**
******************************************************************************/

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
#include <sys/wait.h>

#include <ims_dbms.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <signal.h>
#include <syslog.h>
 
#include <ims_shm.h>
#include <ims_job_control.h>

void main(int argc, char *argv[])
{
	IMS_JOB_QUEUE_HDR *qhdr;
	int i;


	fprintf(stderr, "    DUMP OF JOB CONTROL QUEUE\n");
	fprintf(stderr, "----------------------------------\n");

	qhdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

    for (i = 0; i < IMS_JOB_QUEUE_MAX;  i++)
	{
		if (qhdr[i].active)
		{

			fprintf(stderr,"    Job %d is active\n", qhdr[i].job_id);
			fprintf(stderr,"    pid         = %d\n",qhdr[i].pid);
			fprintf(stderr,"    parent_pid  = %d\n", qhdr[i].parent_pid);
			fprintf(stderr,"    status      = %d\n\n", qhdr[i].status);

		}
	}
	(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) qhdr);
	exit(0);

}

