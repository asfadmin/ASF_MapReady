#ifndef IMS_JOB_CONTROL_H
#define IMS_JOB_CONTROL_H

static char *sccsJobControl = "@(#)ims_job_control.h	5.1  03/17/96";

#define IMS_JOB_QUEUE_MAX   50


#define IMS_JOB_STARTING    1
#define IMS_JOB_PENDING     2
#define IMS_JOB_COMPLETE    3
#define IMS_JOB_ABORTED     4
#define IMS_JOB_TIMEOUT     5

typedef struct ims_job_user_spec
{
	char username[IMS_COL15_LEN+1];
	char password[IMS_COL15_LEN+1];
	char program[IMS_COL30_LEN+1];
	char server[IMS_COL30_LEN+1];
	char database[IMS_COL30_LEN+1];
} IMS_JOB_USER_SPEC;

typedef struct ims_job_queue_hdr 
{
	int active;
 	int job_id;
	int pid;
	int parent_pid;
	int report_id;
	int status;
	void *callback;
	char *data;
	int shmid;
	int msgid; 				/* Message Queue ID */
	int perform_cb; 		/* Flag indicating to perform callback */	
	IMS_MSG_STRUCT *msgDesc;
	IMS_JOB_USER_SPEC userSpec;
} IMS_JOB_QUEUE_HDR;


int ims_addJob(IMS_MSG_STRUCT *, IMS_JOB_USER_SPEC *, int, int, int *, int, int, void (* ) (), char *);
int ims_updateJobStatus(IMS_MSG_STRUCT *, int, int, int);
int ims_init_job_queue(IMS_MSG_STRUCT *);
int ims_remove_job(int);
int ims_startJob(IMS_MSG_STRUCT *, int, int, int, char *); 
int ims_jobComplete(IMS_MSG_STRUCT *, int);
int ims_sendJobMsgs (IMS_MSG_STRUCT *, int);

#endif  /* !IMS_JOB_CONTROL_H */
