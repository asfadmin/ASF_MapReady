static char *sccs = "@(#)ims_FA_task.c	5.17 07/22/97";
/******************************************************************************
**
** File:	ims_FA_task.c
**
** Function: Perform processing of FA Reports for the various required 
**           flight agency reports.
**
** Author: Dan Crichton	
**
** Date:	6/14/95
** Modification: 2/18/97 D. Ting
**               Type Unavailable Message is deleted after archived.
**               HC_ACTIVITY_TYPE should be equal to FLIGHT_AGENCY_SHIPMENT
**               File name should be ta_<MEDIA_ID>_timestamp
**
**							 3/25/97 D.Ting
**							 Fixed setting the glbl_dataset_idx problem for ERS-1, ERS-2 in
**							 creating extract and acquisition reports.
**
**							 5/5/97 D. Ting
**							 R2.1 Appended the unique identifier onto the file name before calling
**							 FAIF to send the file
**				
**							 5/15/97 D. Ting
**							 R2.1 Changed HC_ACTIVITY_TYPE to SYSTEM_ACTIVITY_TYPE and added
**							 SYSTEM_ACTIVITY_ID
**
**							 7/21/97 D. Timg
**							 Fixed 'platform' problem for ERS-1, ERS-2
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <syslog.h>
#include <signal.h>
#include <sys/wait.h>

#include <ims_job_control.h>
#include <ims_fa_reports.h>
#include <ims_fa_track.h>
#include <ims_archive.h>
#include <odldef.h>
#include <ims_odl.h>

#define IMS_FA_STAGE	4	 /* FA Staging Area */

#define MACHINE_ASF		1
#define MACHINE_FA		2

/*
** Local Types
*/

/*
** Local Functions
*/

int createReport(IMS_MSG_STRUCT *, int, IMS_FA_INTERFACE *);
int startSubmission(char *, char *, char *);
int storeQueryData(IMS_MSG_STRUCT *msgDesc, int id, char **qdata, int *len);

static char *glb_programName;
static IMS_QI_DESC_OBJ *glbl_qDesc;
static int ingestFiles(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, int, char *, 
		char *, char *, 
		char *, char *, char *, char *, int);
static int getStageArea(IMS_MSG_STRUCT *, char *);
static int sendTapeUnavail(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, int);
static int getTapeInfo(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, char *, 
		IMS_KEYWORD_ARRAY keyword[]);
static int addHistoryInfo(IMS_MSG_STRUCT *,int,int,int,int,int,int,char *);
static int getReportGranuleIndex(IMS_MSG_STRUCT *, int *, int, char *);

static IMS_JOB_USER_SPEC userSpec;
static int glbl_dataset_idx;
static char glbl_reportName[IMS_COL30_LEN+1];


/****************************************************************************
**
** main
**
****************************************************************************/

void main(int argc, char *argv[])
{
	int key;
	char *ptr;
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
	IMS_FA_INTERFACE fa_data, *fa_ptr;
	int job_id;
	int shmid;
	int report_id;
	int i;

	/*
	** Setup message facility.
	*/

	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */
	
    /*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
				"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (IMS_FATAL);
	}

    glb_programName = ims_extractFileName (argv[0]);
		 
	  
	(void) ims_msgSubSystem (msgDesc, "IMS");
	/*(void) ims_msgProgramName (msgDesc, glb_programName); R2.1 */ 
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	/*(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);R2.1 */
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag(msgDesc, IMS_ON);
	(void) ims_msgStderrFlag(msgDesc, IMS_OFF);

	if (IK_NameSyslog("./syslog.fa") < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not create syslog error log file");
	}

	/*
	** Ignore death of a parent or other interuptions
	*/

	signal(SIGINT, SIG_IGN);
								 

	/*
	** Do some processing....
	*/

	if (argc < 2)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not get report id.\n");
		IK_CloseSyslog();
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	report_id = atoi(argv[1]);

	/*
	** Validate report id, should use min/max constants
	*/

	if (report_id < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Report id is invalid.\n");
		IK_CloseSyslog();
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	if (ims_updateJobStatus(msgDesc, atoi(argv[2]), IMS_JOB_PENDING, 
				getpid()) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not set job status.\n");
		IK_CloseSyslog();
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	shmid = atoi(argv[3]);

	/*
	** Lock the memory and extract the interface data.
	*/

	if (shmid > -1)
	{
		fa_ptr = (void *) ims_shm_lock(shmid);

		if (fa_ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not lock memory to receive FA Interface Data");
			IK_CloseSyslog();
			exit(1);			
		}

		/*
		** Move shared memory over to local data so we can unlock it.
		*/

		memcpy(&fa_data, fa_ptr, sizeof(fa_data));
		memcpy((void *) &userSpec, (void *) &(fa_data.jobSpec), 
			sizeof(IMS_JOB_USER_SPEC));

		(void) ims_shm_unlock(shmid, (void *) fa_ptr);
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"FA Interface Data not passed to FA Job");
		IK_CloseSyslog();
		(void) ims_msgStructFree(msgDesc);
		exit(1);			
	}



	if (createReport(msgDesc, report_id, &fa_data) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not create report %d\n",
				report_id);
		ims_sendJobMsgs(msgDesc, atoi(argv[2]));
		IK_CloseSyslog();
		(void) ims_msgStructFree(msgDesc);
		exit(1);  /* Cause an abort */
	}	

#ifdef DEBUG
	(void) ims_msg(msgDesc, IMS_INFO, "FA Task completing");
#endif

	ims_sendJobMsgs(msgDesc, atoi(argv[2]));

	(void) ims_jobComplete(msgDesc, atoi(argv[2]));
	(void) ims_msgQueueFlag(msgDesc, IMS_OFF);

	IK_CloseSyslog();
	(void) ims_msgStructFree (msgDesc);
	exit(0);
}


/****************************************************************************
**
** createReport
**
****************************************************************************/
int createReport(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	IMS_FA_INTERFACE *fa_data)
{
	char filename[IMS_PATH_LEN];
	char tempName[IMS_PATH_LEN+1]; /* R2.1 */
	char dest[IMS_PATH_LEN];
	char faif_filetype[IMS_NAME_LEN];
	char filetype[IMS_NAME_LEN];
	char platform[15];
	char satellite[6];
	char general[65];
	int status = IMS_OK;
	int granuleIdx;
	int counter, addhistory; /* R2.1 */

	/*
	** Find an execute the correct FA report.
	*/

	memset(fa_data->archive_name, 0, sizeof(fa_data->archive_name));

	fa_data->start_rev = fa_data->end_rev = -1;

	if (getStageArea(msgDesc, filename) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine staging area to generate FA report.");
		return(IMS_ERROR);
	}

	switch(report_id)
	
	{
		case IMS_CSA_ARCHSTRGRPT:
			glbl_dataset_idx = IMS_DATASET_R1_ASR;
			strcpy(fa_data->platform, "RADARSAT-1");
			strcpy(dest, "CSA");
			strcpy(filetype, "CSA_ARCHSTRGRPT");
			strcpy(faif_filetype, "CSA_ARCHSTRGRPT");
			strcpy(platform, IMS_FA_R1);		
			strcpy(satellite, "R1");
			strcpy(general, "CATALOG_REPORT");
			if (ims_csa_archstrgrpt(msgDesc, filename, 
				fa_data->tape_num[0]) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"CSA_ARCHSTRGRPT report not successful.");

				/*
				** Put info in history table.
				*/

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}
				return(IMS_ERROR);
			}

/*
**
** Removed since unavailable is not currently sent for this report.
**

			if (sendTapeUnavail(msgDesc, fa_data, report_id) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not send tape unavailable message.");
				return(IMS_ERROR);
			}
*/

			break;

		case IMS_CSA_RECRPT:
			glbl_dataset_idx = IMS_DATASET_R1_RECRPT;
			strcpy(fa_data->platform, "RADARSAT-1");
			strcpy(dest, "CSA");
			if (strcmp(fa_data->station_id, "FA") == 0)
			{
			    strcpy(filetype, "CSA_RECRPT");
			    strcpy(faif_filetype, "CSA_RECRPT");
      }else{ /* MC*/
			    strcpy(filetype, "CSA_RRP_MCM");
			    strcpy(faif_filetype, "CSA_RRP_MCM");
      }

			strcpy(platform, IMS_FA_R1);		
			strcpy(satellite, "R1");
			strcpy(general, "ACQUISITION_REPORT");
			if (ims_csa_recrpt(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"CSA_RECRPT report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;

		case IMS_ESA_RESM:
		case IMS_ESA2_RESM:


			strcpy(dest, "ESA");
			strcpy(filetype, "RESM");
			strcpy(faif_filetype, "ESA_RESM");

			if (strcmp(fa_data->platform, "E1") == 0)
			{
				strcpy(fa_data->platform, "ERS-1");
			}

			if (strcmp(fa_data->platform, "E2") == 0)
			{
				strcpy(fa_data->platform, "ERS-2");
			}

			if (strncmp(fa_data->platform, "ERS-1", 5) == 0)
			{
				strcpy(platform, "ERS-1");		
				strcpy(satellite, "E1");
				glbl_dataset_idx = IMS_DATASET_E1_RESM;
			}
			else if (strncmp(fa_data->platform, "ERS-2", 5) == 0)
			{
				strcpy(platform, "ERS-2");		
				strcpy(satellite, "E2");
				glbl_dataset_idx = IMS_DATASET_E2_RESM;
			}
			else
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Invalid platform '%s' specified for ESA Report",
					fa_data->platform);		
			}

			strcpy(general, "SHIPMENT_REPORT");

			if (ims_esa_resm(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_RESM report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}

			if (sendTapeUnavail(msgDesc, fa_data, report_id) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not send tape unavailable message.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}
				return(IMS_ERROR);
			}

			break;

		case IMS_ESA_REAQ:
		case IMS_ESA2_REAQ:
			strcpy(dest, "ESA");
			strcpy(filetype, "REAQ");
			strcpy(faif_filetype, "ESA_REAQ");
			
			if (strcmp(fa_data->platform, "E1") == 0)
			{
				strcpy(fa_data->platform, "ERS-1");
			}

			if (strcmp(fa_data->platform, "E2") == 0)
			{
				strcpy(fa_data->platform, "ERS-2");
			}

			if (strncmp(fa_data->platform, "ERS-1", 5) == 0)
			{
				strcpy(platform, "ERS-1");		
				strcpy(satellite, "E1");
				glbl_dataset_idx = IMS_DATASET_E1_REAQ;
			}
			else if (strncmp(fa_data->platform, "ERS-2", 5) == 0)
			{
				strcpy(platform, "ERS-2");		
				strcpy(satellite, "E2");
				glbl_dataset_idx = IMS_DATASET_E2_REAQ;
			}
			else
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Invalid platform '%s' specified for ESA Report",
					fa_data->platform);		
			}

			strcpy(general, "ACQUISITION_REPORT");
			if (ims_esa_reaq(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_REAQ report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;

		case IMS_ESA_REEX:
		case IMS_ESA2_REEX:
			strcpy(dest, "ESA");
			strcpy(filetype, "REEX");
			strcpy(faif_filetype, "ESA_REEX");

			if (strcmp(fa_data->platform, "E1") == 0)
			{
				strcpy(fa_data->platform, "ERS-1");
			}

			if (strcmp(fa_data->platform, "E2") == 0)
			{
				strcpy(fa_data->platform, "ERS-2");
			}

			if (strncmp(fa_data->platform, "ERS-1", 5) == 0)
			{
				strcpy(platform, "ERS-1");		
				strcpy(satellite, "E1");
				glbl_dataset_idx = IMS_DATASET_E1_REEX;
			}
			else if (strncmp(fa_data->platform, "ERS-2", 5) == 0)
			{
				strcpy(platform, "ERS-2");		
				strcpy(satellite, "E2");
				glbl_dataset_idx = IMS_DATASET_E2_REEX;
			}
			else
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Invalid platform '%s' specified for ESA Report",
					fa_data->platform);		
			}

			strcpy(general, "ARCHIVED_DATA_EXTRACTED");

			if (ims_esa_reex(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_REEX report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;

		case IMS_NASDA_CATA:
			glbl_dataset_idx = IMS_DATASET_J1_CATA;
			strcpy(dest, "NASDA");
			strcpy(filetype, "CATA");
			strcpy(faif_filetype, "NASDA_CATA");
			strcpy(platform, "JERS-1");		
			strcpy(satellite, "J1");
			strcpy(general, "CATALOG_REPORT");
			strcpy(fa_data->platform, "JERS-1");
			if (ims_jers_cata(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_CATA report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;

		case IMS_NASDA_REAC:
			glbl_dataset_idx = IMS_DATASET_J1_REAC;
			strcpy(dest, "NASDA");
			strcpy(filetype, "REAC");
			strcpy(faif_filetype, "NASDA_REAC");
			strcpy(platform, "JERS-1");		
			strcpy(satellite, "J1");
			strcpy(general, "ACQUISITION_REPORT");
			strcpy(fa_data->platform, "JERS-1");
			if (ims_jers_reac(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_REAC report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;

		case IMS_NASDA_MSGM:
			glbl_dataset_idx = IMS_DATASET_J1_MSGM;
			strcpy(dest, "NASDA");
			strcpy(filetype, "MSGM");
			strcpy(faif_filetype, "NASDA_MSGM");
			strcpy(platform, "JERS-1");		
			strcpy(satellite, "J1");
			strcpy(general, "SHIPMENT_REPORT");
			strcpy(fa_data->platform, "JERS-1");
			if (ims_jers_msgm(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_MSGM report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}

			if (sendTapeUnavail(msgDesc, fa_data, report_id) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not send tape unavailable message.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}

			break;


		case IMS_ADEOS_SRRD:
			glbl_dataset_idx = IMS_DATASET_A1_SRRD;
			strcpy(dest, "ADEOS");
			strcpy(filetype, "SRRD");
			strcpy(faif_filetype, "ADEOS_SRRD");
			strcpy(platform, "ADEOS-1");		
			strcpy(satellite, "A1");
			strcpy(general, "SHIPMENT_REPORT");
			strcpy(fa_data->platform, "ADEOS-1");
			if (ims_adeos_srrd(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ADEOS_SRRD report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			
			if (sendTapeUnavail(msgDesc, fa_data, report_id) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not send tape unavailable message.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}

			break;


		case IMS_ADEOS_REAC:
			glbl_dataset_idx = IMS_DATASET_A1_REAC;
			strcpy(dest, "ADEOS");
			strcpy(filetype, "REAC");
			strcpy(faif_filetype, "ADEOS_REAC");
			strcpy(platform, "ADEOS-1");		
			strcpy(satellite, "A1");
			strcpy(general, "ACQUISITION_REPORT");
			strcpy(fa_data->platform, "ADEOS-1");
			if (ims_adeos_reac(msgDesc, filename, fa_data) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ADEOS_REAC report not successful.");

				if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
						-1, FALSE, FALSE, FALSE, filename) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not add statistics to report_history table.");
				}

				return(IMS_ERROR);
			}
			break;
		default:
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Unknown report type %d not recognized", 
				report_id);
	}

	/*
	** Send file to flight agency
	*/
	/* The following has been added for R2.1 */
	if (report_id == IMS_CSA_RECRPT)
	{
	 /*
	 ** For the Reception Report we will add the counter the end of the
	 ** file for uniqueness.
	 */

	   if (getReportCounter(msgDesc, report_id, &counter) < 0)
		 {
				 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not get counter for RADARSAT report %d", report_id);
					 return(IMS_ERROR);
		 }

		 counter --;  /* So it matches what is inside of the report */

		 sprintf(tempName, "%s.%d", filename, counter);
		 if (rename(filename, tempName) < 0)
		 {
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not rename %s to %s for ingestion.", filename, 
				tempName);
			return(IMS_ERROR);
		 }
		 strcpy(filename, tempName);
	 }
	 /* End of added */

	if (startSubmission(filename, dest, faif_filetype) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not send file %s to flight agency",
			filename);
		/*
		** Continue on here to ingest the files...
		*/

		status = IMS_ERROR;
	}

	/*
	** Ingest the files into the FTS archive.
	*/

  addhistory = 0;
	if (fa_data->archive_name[0] != '\0')
	{
		/*
		** Archive the unswapped ASF version.
		*/

		if (ingestFiles(msgDesc, fa_data, report_id, fa_data->archive_name,
			filename, satellite, filetype,
			general, fa_data->date_start, fa_data->date_end,
			MACHINE_ASF) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not ingest file %s.", filename);

			if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
					-1, TRUE, FALSE, FALSE, filename) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not add statistics to report_history table.");
			}

			return(IMS_ERROR);
		}

		/* The following has been moved to here R2.1 */
		/*
		** Get the granule index and add statistics for report history.
		*/

		if (getReportGranuleIndex(msgDesc, &granuleIdx, glbl_dataset_idx,
			glbl_reportName) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"could not locate granule for report in granule table.");
			return(IMS_ERROR);

		}

    	if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
					granuleIdx, TRUE, (status == IMS_ERROR)?FALSE:TRUE, 
					TRUE, glbl_reportName) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not add statistics to report_history table.");
			return(IMS_ERROR);				
		}
		/* end of move */

		strcpy(fa_data->archive_name, filename);
		strcat(fa_data->archive_name, ".vax");

		if (rename(filename, fa_data->archive_name) < 0)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not rename %s to %s for ingestion.", filename, 
				fa_data->archive_name);
			return(IMS_ERROR);
		}

	}
	else
	{
		addhistory = 1;
		strcpy(fa_data->archive_name, filename);
	}

	/*
	** Ingest the file going to the flight agency.
	*/

	if (ingestFiles(msgDesc, fa_data, report_id, fa_data->archive_name,
			filename, satellite, filetype,
			general, fa_data->date_start, fa_data->date_end,
			MACHINE_FA) < IMS_OK)
	{
		if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
				-1, TRUE, (status == IMS_ERROR)?FALSE:TRUE, 
				FALSE, filename) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not add statistics to report_history table.");
		}

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not ingest file %s.", filename);
		return(IMS_ERROR);
	}
	else
	{
		if (addhistory )
		{
		/*
		** Get the granule index and add statistics for report history.
		*/

		  if (getReportGranuleIndex(msgDesc, &granuleIdx, glbl_dataset_idx,
			  glbl_reportName) < IMS_OK)
		  {
			  (void) ims_msg(msgDesc, IMS_ERROR,
				  "could not locate granule for report in granule table.");
			  return(IMS_ERROR);

		  }

    	  if (addHistoryInfo(msgDesc, report_id, glbl_dataset_idx,
					  granuleIdx, TRUE, (status == IMS_ERROR)?FALSE:TRUE, 
					  TRUE, glbl_reportName) < IMS_OK)
		  {
			  (void) ims_msg(msgDesc, IMS_ERROR,
				  "Could not add statistics to report_history table.");
			  return(IMS_ERROR);				
		  }
		}
	}
	return(status);
}



char *util_do_malloc(int size)
{
	return(malloc(size));
}

/*******************************************************************
**
** setupQuery
**
*******************************************************************/

int setupQuery(
	IMS_MSG_STRUCT *msgDesc,
	char *query_string)
{
	int status;
	static char global_data[IMS_COL1024_LEN];

	/*
	** Allocate a query descriptor.
	*/

	if ((glbl_qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}

	glbl_qDesc->cmd  = (char *) global_data;
	
	if ((char *) glbl_qDesc->cmd == NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL, 
			"Could not allocate the command area for the query descriptor.");
		(void) ims_qiFreeDesc(glbl_qDesc);
	}

	/*
	** Setup login parameters. 
	*/


	IMS_SETUSER (glbl_qDesc, userSpec.username);
	IMS_SETPSWD (glbl_qDesc, userSpec.password);
	IMS_SET_VERBOSE(glbl_qDesc, 10);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(glbl_qDesc, userSpec.server);
		 
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(glbl_qDesc, userSpec.database);
	  
	status = ims_qiLogin(glbl_qDesc);
		   
	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(glbl_qDesc->cmd);
		(void) ims_qiFreeDesc(glbl_qDesc);
		return(status);
	}

	IMS_SET_USERDATA(glbl_qDesc);

	sprintf(glbl_qDesc->cmd, query_string);
	return(IMS_OK);
}

/*******************************************************************
**
** performQuery
**
*******************************************************************/

int performQuery(IMS_MSG_STRUCT *msgDesc)
{
	int status;

	/*
	** Currently only one thing to do....
	*/

	status = ims_qiNextRow(glbl_qDesc);

	return(status);

}
/*******************************************************************
**
** getQueryData
**
*******************************************************************/

int getQueryData(IMS_MSG_STRUCT *msgDesc, int id, char **qdata, int *len)
{
	*qdata = (char *) glbl_qDesc->valAddr[id];
	*len = glbl_qDesc->valLength[id];
	return(IMS_OK);
}

/*******************************************************************
**
** storeQueryData
**
*******************************************************************/

int storeQueryData(IMS_MSG_STRUCT *msgDesc, int id, char **qdata, int *len)
{
	char *ptr;

	*len = glbl_qDesc->valLength[id];

	ptr = malloc(*len + 1);
	if (ptr == NULL)
	{
		(void) (msgDesc, IMS_FATAL,
			"Could not allocate memory to store returned row");
			return(IMS_FATAL);
	}
	memset(ptr, 0, *len+1);
	memcpy(ptr, (char *) glbl_qDesc->valAddr[id], *len);
	*qdata = ptr;


	return(IMS_OK);
}


/*******************************************************************
**
** endQuery
**
*******************************************************************/

int endQuery(IMS_MSG_STRUCT *msgDesc)
{
/*	free(glbl_qDesc->cmd);
*/
	ims_qiFreeDesc(glbl_qDesc);
	return(IMS_OK);
}

/*******************************************************************
**
** updateTracking
**
*******************************************************************/

int updateTracking(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_TRACKING *track)
{
	int status;

	if (setupQuery(msgDesc, "NOT USED") < IMS_OK)
	{
		return(IMS_ERROR);
	}

	status = ims_deleteFromTrackingTable(msgDesc, glbl_qDesc, track);

	endQuery(msgDesc);

	return(status);
}



/******************************************************************************
**
** getGranuleTableName ()
**
******************************************************************************/

int getGranuleTableName(
	IMS_MSG_STRUCT *msgDesc,
	char *tableName,
	char *dataset_name,
	char *platform)
{
	int status;
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN+1];

	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Setup login parameters. 
	*/


  IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE(qDesc, 10);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
		 
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
		 
	  
	status = ims_qiLogin(qDesc);
		   
	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		(void) ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	qDesc->cmd = qbuf;


	sprintf(qDesc->cmd,
		"select p.granules_table from dataset_relation r, \
			dataset_policy p where r.dataset = '%s' and \
			p.dataset_idx = r.dataset_idx and platform = '%s'",
			dataset_name, platform);
		
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of dataset relation.");
			(void) ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) memcpy((char *) tableName,
					qDesc->valAddr[0], qDesc->valLength[0]);
		tableName[qDesc->valLength[0]] = '\0';
		ims_trim(tableName);

	}

	if (IMS_AFFECTED(qDesc) != 1) 
	{
		/* 
		** This is a problem ...
		*/

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not locate granule name for dataset %s", dataset_name);

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			(void) ims_qiFreeDesc(qDesc);
			return (IMS_FATAL);
		}

		return(IMS_ERROR);

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		(void) ims_qiFreeDesc(qDesc);
		return (IMS_FATAL);
	}

	(void) ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}




/*******************************************************************
**
** startSubmission
**
** Send the file to FAIF.  Also archive the file at the sametime 
** through FTS ingestion process.
*******************************************************************/


int startSubmission(
	char *filename,
	char *dest,
	char *filetype)
{
	int pid, pid_id;
	int status;
	char *argv[10];
	char taskName[255];
	int i;
	char *exec_path;

#ifdef DEBUG
	fprintf(stderr, "sending file %s to client %s\n", filename, dest);
#endif

	exec_path = getenv("IMS_EXEC_PATH");

	if (exec_path != NULL)
		sprintf(taskName, "%s/faif_xmitClient", exec_path);
	else
		strcpy(taskName, "faif_xmitClient");	

	for (i = 0; i < 10; i++)
	{
		argv[i] = 0;
	}

	argv[0] = (char *) taskName;
	argv[1] = "-f";
	argv[2] = filename;
	argv[3] = "-d";
	argv[4] = dest;
	argv[5] = "-t";
	argv[6] = filetype;

	pid = fork();

	if (pid < 0)
	{
		return(IMS_ERROR);
	}	
	else if (pid == 0)
	{
		/*
		** Perform exec
		*/

		execv(taskName, argv);
		exit(1);

	}
	pid_id = wait(&status );


	if (!WEXITSTATUS(status))
	{
		return(IMS_OK);
	}
	else
	{
		return(IMS_ERROR);
	}


}


/********************************************************************
** ingestFiles
**
** Ingest flight agency report into FTS
**
********************************************************************/
static int ingestFiles(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_INTERFACE *fa_data,
	int report_id,
	char *name,
	char *origname,
	char *platform,
	char *filetype,
	char *general,
	char *start_time,
	char *end_time,
	int machine_type)
{
	IMS_CLNT_EVENT request;
	char *ext[] = {"M","D"};
	char *pmf_name;
	char *dat_name;
	int status;
	char path[IMS_PATH_LEN + 1]; 
	char tempName[IMS_PATH_LEN+1];
	IMS_ODL_TREE *cmn_hdr;
	IMS_KEYWORD_ARRAY keyword[] =
	{ 	{"FILE_NAME", NULL, TV_STRING},
		{"PLATFORM", NULL, TV_STRING},
	    {"FA_FILE_TYPE", NULL, TV_STRING},
	    {"GEN_FILE_TYPE", NULL, TV_STRING},
		{"VALID_START_TIME", NULL, TV_DATE_TIME},
		{"VALID_END_TIME", NULL, TV_DATE_TIME},
		{"FILE_CREATION_TIME", NULL, TV_DATE_TIME},
		{"FILE_FORMAT", NULL, TV_STRING},
		{"ARRIVAL_TIME", NULL,  TV_DATE_TIME},
		{"DESTINATION", NULL, TV_STRING},
		{"SOURCE", NULL, TV_STRING},
		{"START_REV", NULL, TV_INTEGER},
		{"END_REV", NULL, TV_INTEGER}
	};
	char stime[IMS_DATETIME_LEN+1];
	char etime[IMS_DATETIME_LEN+1];
	char ctime[IMS_DATETIME_LEN+1];
	IMS_NUMERIC_DATE dateStruct;
	int counter;
	int keywordCount = 11;
	char startRev[10], endRev[10];

	/*
	** Setup keywords for MTA file
	*/

	keyword[0].value =  ims_extractFileName(origname); 
	keyword[1].value = platform;
	keyword[2].value = filetype;
	keyword[3].value = general;
	keyword[4].value = start_time;
	keyword[5].value = end_time;

	if ((strcmp(name, origname) == 0) || (machine_type == MACHINE_FA))
		keyword[7].value = "ORIGINAL";
	else
		keyword[7].value = "ASF";

	keyword[9].value = "IMS";
	keyword[10].value = "ASF";

	if (fa_data->start_rev > -1)
	{
		keywordCount += 2;
		
		sprintf(startRev, "%d", fa_data->start_rev);
		keyword[11].value = startRev;

		sprintf(endRev, "%d", fa_data->end_rev);
		keyword[12].value = endRev;
	}


    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		unlink(name);
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get current date for ingestion");
		return(IMS_ERROR);

	}
	ims_numericDateToIMSA(&dateStruct, ctime);
	keyword[6].value = ctime;
	keyword[8].value = ctime;

	if ((start_time == NULL) || (start_time[0] == '\0'))
	{
	    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
		{
			unlink(name);
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not get current date for ingestion");
			return(IMS_ERROR);

		}
		ims_numericDateToIMSA(&dateStruct, stime);
		keyword[4].value = stime; 

	}

	if ((end_time == NULL) || (end_time[0] == '\0'))
	{

	    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
		{
			unlink(name);
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not get current date for ingestion");
			return(IMS_ERROR);

		}
		ims_numericDateToIMSA(&dateStruct, etime);
		keyword[5].value = etime; 
	}


	/*
	** Create MTA and DAT filenames.
	*/

	pmf_name = malloc(strlen(name) + 3 + IMS_COL15_LEN);
	if (pmf_name == NULL)
	{
		unlink(name);
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not allocate memory for metadata filename");
		return(IMS_ERROR);
	}

	/*
	** If this is a JERS-1 report, then add a counter to the
	** filename since their filenames are always the same.
	*/

	strcpy(tempName, name);   /* Save the old name */

	if (strcmp(platform, "J1") == 0)
	{
		if (getReportCounter(msgDesc, report_id, &counter) < 0)
		{
			unlink(name);
			free(pmf_name);
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get counter for NASDA report");
			return(IMS_ERROR);
		}
		if (updateReportCounter(msgDesc, report_id, counter + 1) < 0)
		{
			unlink(name);
			free(pmf_name);
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get counter for NASDA report");
			return(IMS_ERROR);
		}

		sprintf(pmf_name, "%s.%d.M", name, counter);
		sprintf(tempName, "%s.%d", name, counter);
		fa_data->report_counter = counter;
	}
	else if (report_id == IMS_CSA_ARCHSTRGRPT)
	{
		/*
		** For the Reception Report the counter already has been added at the
		** end of the file for uniqueness. So we are just going to do this
		** for the Archive Storage Report. R2.1
		*/
		if (getReportCounter(msgDesc, report_id, &counter) < 0)
		{
			unlink(name);
			free(pmf_name);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get counter for RADARSAT report %d", report_id);
			return(IMS_ERROR);
		}

		counter --;  /* So it matches what is inside of the report */

		sprintf(pmf_name, "%s.%d.M", name, counter);
		sprintf(tempName, "%s.%d", name, counter);
	}
	else
		sprintf(pmf_name, "%s.M", name);


	dat_name = malloc(strlen(name) + 3 + IMS_COL15_LEN);
	if (dat_name == NULL)
	{
		unlink(name);
		free(pmf_name);
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not allocate memory for metadata filename");
		return(IMS_ERROR);
	}

	if (strcmp(platform, "J1")  == 0)
		sprintf(dat_name, "%s.%d.D", name, counter);
	else if (report_id == IMS_CSA_ARCHSTRGRPT)
		sprintf(dat_name, "%s.%d.D", name, counter);
	else
		sprintf(dat_name, "%s.D", name);

	/*
	** Rename report filename to filename.D
	*/

	if (rename(name, dat_name) < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not rename %s to %s for ingestion.", name, dat_name);
		free(pmf_name);
		return(IMS_ERROR);
	}

	/* 
	** Create PMF file.
	*/
	
	if (ims_create_cmn_hdr(msgDesc, &cmn_hdr, "FA REPORT", 1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not create common header for metadata file");
		(void) ims_delete_tree(msgDesc, cmn_hdr);
		return(IMS_ERROR);
	}

	if (ims_create_catalog(msgDesc, cmn_hdr, keyword, keywordCount) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not create catalog section for metadata file");
		(void) ims_delete_tree(msgDesc, cmn_hdr);
		return(IMS_ERROR);
	}

	status = ims_buildPMF(msgDesc, cmn_hdr, pmf_name, NULL);
	(void) ims_delete_tree(msgDesc, cmn_hdr);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not generate ODL metadata file");
		unlink(pmf_name);
		unlink(dat_name);
		free(pmf_name);
		free(dat_name);
		return(IMS_ERROR);
	}


	/*
	** Ingest into IMS with ims_archive()
	*/
	
	request.username = "ims_clnt";
	request.password = "ims_clnt";
	request.accountId = "ACCT_IMS";
	request.sensor = NULL;
	request.platform = fa_data->platform;  /* Long form of platform. */

	switch (report_id)
	{
		case IMS_CSA_RECRPT:
			request.dataset = "RADARSAT-1 CSA_RECRPT";
			break;

		case IMS_CSA_ARCHSTRGRPT:
			request.dataset = "RADARSAT-1 CSA_ARCHSTRGRPT";
			break;

		case IMS_ADEOS_REAC:
			request.dataset = "ADEOS-1 REAC";
			break;

		case IMS_ADEOS_SRRD:
			request.dataset = "ADEOS-1 SRRD";
			break;

		case IMS_NASDA_MSGM:
			request.dataset = "JERS-1 MSGM";
			break;

		case IMS_NASDA_REAC:
			request.dataset = "JERS-1 REAC";
			break;

		case IMS_NASDA_CATA:
			request.dataset = "JERS-1 CATA";
			break;

		case IMS_ESA_RESM:
			request.dataset = "ERS-1 RESM";
			break;

		case IMS_ESA_REEX:
			request.dataset = "ERS-1 REEX";
			break;

		case IMS_ESA_REAQ:
			if (machine_type == MACHINE_ASF)
				request.dataset = "ERS-1 REAQ";
			else
				request.dataset = "ERS-1 REAQ DELIVERED";
			break;

		case IMS_ESA2_REAQ:
			if (machine_type == MACHINE_ASF)
				request.dataset = "ERS-2 REAQ";
			else
				request.dataset = "ERS-2 REAQ DELIVERED";
			break;

		case IMS_ESA2_RESM:
			request.dataset = "ERS-2 RESM";
			break;

		case IMS_ESA2_REEX:
			request.dataset = "ERS-2 REEX";
			break;

	}
	request.name = ims_extractFileName(tempName);
	strcpy(glbl_reportName, request.name);
	request.format = "FA";
	request.version = -1;
	request.fileCount = 2;
	request.extensions = ext;
	(void) ims_extractPath(name, path);
	request.sourceDir = path;
	request.localArchiveFlag = 'N';
	request.programName = "ims_reporter";
	request.catSrvName = userSpec.server;
	request.catDbName = userSpec.database;
	request.ftsSrvName = "";
	request.msgDesc = msgDesc;
	request.requestType = IMS_ADD;

	if (ims_archive(&request) < IMS_OK)
	{
		free(pmf_name);
		free(dat_name);
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not ingest flight agency report into IMS/DADS");
		return(IMS_ERROR);
	}

	unlink(pmf_name);
	unlink(dat_name);
	free(pmf_name);
	free(dat_name);
	return(IMS_OK);		
}



/********************************************************************
**
** getStageArea
**
********************************************************************/

static int getStageArea(
	IMS_MSG_STRUCT *msgDesc,
	char *path)
{
	int len;
	int status, count;
	char query[255];
	char *tmp;

	sprintf(query, "select path from stage_area where stage_type = %d",
		IMS_FA_STAGE);

	if (setupQuery(msgDesc, query) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup query to determine FA Staging Area");
		return(IMS_ERROR);
	}

	count = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to determine FA Staging Area");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		if (getQueryData(msgDesc, 0, &tmp, &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get path to determine FA Staging Area");
			return(IMS_ERROR);
		}
		memcpy(path, tmp, len);
		path[len] = '\0';
		count++;
	}

	/*
	** Check that a row was returned.
	*/

	if (count == 0)
	{

		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows returned for determining FA Staging Area.");
		(void) endQuery(msgDesc);
		return(IMS_ERROR);
	}

	(void) endQuery(msgDesc);
	return(IMS_OK);
}

/********************************************************************
**
** getReportCounter
**
********************************************************************/

int getReportCounter(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	int *counter)
{
	int len;
	int status, count;
	char query[255];
	char *tmp;

	sprintf(query, "select last_report_counter from fa_history where \
			report_id = %d", report_id);

	if (setupQuery(msgDesc, query) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup query to determine report counter");
		return(IMS_ERROR);
	}

	count = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to determine next report counter.");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) getQueryData(msgDesc, 0, &tmp, &len);
		memcpy((void *) counter, tmp, len);
		count++;
	}

	/*
	** Check that a row was returned.
	*/

	if (count == 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"No rows returned for fa report %d.", report_id); 
		(void) endQuery(msgDesc);
		return(IMS_ERROR);
	}

	(void) endQuery(msgDesc);

	return(IMS_OK);
}

/********************************************************************
**
** updateReportCounter
**
********************************************************************/

int updateReportCounter(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	int counter)
{
	int status, count;
	char query[255];
	char *tmp;

	sprintf(query, "update fa_history set last_report_counter = %d \
			where report_id = %d", counter, report_id);

	if (setupQuery(msgDesc, query) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup query to determine report counter");
		return(IMS_ERROR);
	}

	count = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to determine next report counter.");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

	}

	(void) endQuery(msgDesc);

	return(IMS_OK);
}
/********************************************************************
** getTapeInfo
**
** Send a tape unavailable message to the HC dataset
**
********************************************************************/
static int getTapeInfo(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_INTERFACE *fa_data,
	char *tape_num,
	IMS_KEYWORD_ARRAY keyword[])
{
	char qbuf[IMS_COL255_LEN+1];
	char granuleName[IMS_COL30_LEN+1];
	int status;
	int len;
	char workbuf[64];

	/*
	** Get tape available granule name
	*/

	if (getGranuleTableName(msgDesc, granuleName, IMS_FA_TA_E1, 
		"HC") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get the tape available granule table name");
		return(IMS_ERROR);
	}

	/*
	** Set status to UNAVAILABLE
	*/

	keyword[1].value = (void *) malloc(strlen("HST_S_NOT_AVAILABLE") + 1);

	if (keyword[1].value == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate storage for tape avail status");
		return(IMS_ERROR);
	}

	strcpy(keyword[1].value, "HST_S_NOT_AVAILABLE");

	/*
	** Set SYSTEM_ACTIVITY_ID to "IMS_0"
	*/

	keyword[7].value = (void *) malloc(strlen("IMS_0") + 1);

	if (keyword[7].value == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate storage for SYSTEM_ACTIVITY_ID");
		return(IMS_ERROR);
	}

	strcpy(keyword[7].value, "IMS_0");

	/*
	** Store tape number in the keyword list
	*/

	keyword[3].value = (void *) malloc(strlen(tape_num) + 1);

	if (keyword[3].value == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate storage for tape avail MEDIA_ID");
		return(IMS_ERROR);
	}

	strcpy(keyword[3].value, tape_num);


	/*
	** The rest of the information is a duplicate of the last 
	** record for the tape.
	*/

 /* change Default HC_ACTIVITY_TYPE to FLIGHT_AGENCY_SHIPMENT */
	sprintf(qbuf, 
		"select SYSTEM_ACTIVITY_TYPE='FLIGHT_AGENCY_SHIPMENT', \
		MEDIA_ID_ALIAS, \
		MEDIA_ID_TYPE_NAME, convert(char(10), MEDIA_DOG_TAG),  \
		RECORDER_MEDIA_TYPE from \
		%s where MEDIA_ID = '%s' and MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' \
		order by received_time desc",
		granuleName, tape_num);


	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup query to determine tape information");
		return(IMS_ERROR);
	}

	IMS_SETROWCOUNT(glbl_qDesc, "1");


	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to determine tape info.");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		if (storeQueryData(msgDesc, 0, &(keyword[0].value), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get returned data #1 for tape info.");
			return(IMS_ERROR);
		}

		if (storeQueryData(msgDesc, 1, &(keyword[2].value), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get returned data #2 for tape info.");
			return(IMS_ERROR);
		}

		if (storeQueryData(msgDesc, 2, &(keyword[4].value), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get returned data #3 for tape info.");
			return(IMS_ERROR);
		}

		if (storeQueryData(msgDesc, 3, &(keyword[5].value), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get returned data #4 for tape info.");
			return(IMS_ERROR);
		}

		if (storeQueryData(msgDesc, 4, &(keyword[6].value), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get returned data #5 for tape info.");
			return(IMS_ERROR);
		}


	}

	IMS_SETROWCOUNT(glbl_qDesc, "0");

	(void) endQuery(msgDesc);

	return(IMS_OK);

}


/********************************************************************
** sendTapeUnavail
**
** Send a tape unavailable message to the HC dataset to since the
** tape has been shipped.
**
********************************************************************/
static int sendTapeUnavail(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_INTERFACE *fa_data,
	int report_id)
{
	IMS_CLNT_EVENT request;
	char *ext[] = {"M"};
	char pmf_name[IMS_PATH_LEN+1];
	char product[IMS_COL30_LEN+1];
	int status;
	char path[IMS_PATH_LEN + 1]; 
	char tempName[IMS_PATH_LEN+1];
	IMS_ODL_TREE *cmn_hdr;
	IMS_KEYWORD_ARRAY keyword[] =
	{	{"SYSTEM_ACTIVITY_TYPE", NULL, TV_STRING},
	    {"STATUS", NULL, TV_STRING},
	    {"MEDIA_ID_ALIAS", NULL, TV_STRING},
		{"MEDIA_ID", NULL, TV_STRING},
		{"MEDIA_ID_TYPE_NAME", NULL, TV_STRING},
		{"MEDIA_DOG_TAG", NULL, TV_INTEGER},
		{"RECORDER_MEDIA_TYPE", NULL, TV_STRING},
		{"SYSTEM_ACTIVITY_ID", NULL, TV_STRING}
	};
	IMS_NUMERIC_DATE dateStruct;
	int i;
	int x;
	char temp[13];

	/*
	** Get path to where to write metadata file.
	*/

	if (getStageArea(msgDesc, path) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine staging area to generate FA report.");
		return(IMS_ERROR);
	}



	/*
	** For each tape build a PMF file and ingest...
	*/

	for (i = 0; i < fa_data->tape_count; i++)
	{


		/*
		** Setup keywords for MTA file
		*/

		if (getTapeInfo(msgDesc, fa_data, fa_data->tape_num[i], 
						 keyword) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get tape information to send tape unavailable to IMS.");
			return(IMS_ERROR);		
		}
			
			
		/*
		** Create MTA filename. 
		** tuna = Tape Unavailable, not Tuna Fish
		*/

    strcpy(temp, keyword[3].value);
		temp[12] = '\0'; /* truncated to 12 chars if greater */
		sprintf(product, "ta_%s_%s", temp, ims_timeStamp());
		sprintf(pmf_name, "%s.M", product);

		sprintf(tempName, "%s/%s", path, pmf_name);




		/* 
		** Create PMF file.
		*/
	
		if (ims_create_cmn_hdr(msgDesc, &cmn_hdr, "TAPE_AVAILABILITY_MESSAGE", 
				1) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not create common header for metadata file");
			(void) ims_delete_tree(msgDesc, cmn_hdr);
			return(IMS_ERROR);
		}

		if (ims_create_catalog(msgDesc, cmn_hdr, keyword, 8) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not create catalog section for metadata file");
			(void) ims_delete_tree(msgDesc, cmn_hdr);
			return(IMS_ERROR);
		}

		status = ims_buildPMF(msgDesc, cmn_hdr, tempName, NULL);
		(void) ims_delete_tree(msgDesc, cmn_hdr);

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not generate ODL metadata file");
			unlink(tempName);
			return(IMS_ERROR);
		}

		/*
		** Free keywords
		*/

		for (x = 0; x < 7; x++)
		{
			free(keyword[x].value);
		}


		/*
		** Ingest into IMS with ims_archive()
		*/
	
		request.username = "ims_clnt";
		request.password = "ims_clnt";
		request.accountId = "ACCT_IMS";
		request.sensor = NULL;
		request.platform = "HC";
		request.dataset = "HC TAPE AVAILABILITY";

		request.name = product;
		request.format = "PMF";
		request.version = -1;
		request.fileCount = 1;
		request.extensions = ext;
		request.sourceDir = path;
		request.localArchiveFlag = 'N';
		request.programName = "ims_reporter";
		request.catSrvName = "";
		request.catDbName = "";
		request.ftsSrvName = "";
		request.msgDesc = msgDesc;
		request.requestType = IMS_ADD;

		if (ims_archive(&request) < IMS_OK)
		{
			unlink(tempName);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not send tape unavailable to FTS server");
			return(IMS_ERROR);
		}

		unlink(pmf_name);
		unlink(tempName); /* change Type Unavailable Message should be deleted */


	}
	return(IMS_OK);		
}


/********************************************************************
**
** loadDatasetPaths
**
********************************************************************/
int loadDatasetPaths(
	IMS_MSG_STRUCT *msgDesc,
	char *dataset_name,	
	char *platform,
	IMS_FA_PATH_LIST **ptrPath)
{
	char qbuf[IMS_COL255_LEN+1];
	int status;
	int len;
	int rowCount;
	IMS_FA_PATH_LIST *ptr;
	char *qptr;
	int qlen;
	IMS_QI_DESC_OBJ *qDesc;

	/*
	** The rest of the information is a duplicate of the last 
	** record for the tape.
	*/

	ptr = *ptrPath = NULL;

	qDesc = glbl_qDesc; /* Save so we can restore. */

	sprintf(qbuf, "select path, start_granule_idx, end_granule_idx \
		from dataset_path_policy p, dataset_relation r where \
		r.dataset_idx = p.dataset_idx and r.platform = '%s' and \
		r.dataset = '%s'",
		platform, dataset_name);


	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup query to determine policy path information");
		glbl_qDesc = qDesc;
		return(IMS_ERROR);
	}

	rowCount = 0;


	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to determine tape info.");
			glbl_qDesc = qDesc;
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(IMS_FA_PATH_LIST));
			*ptrPath = ptr;

		}
		else
		{
			ptr->next = (void *) malloc(sizeof(IMS_FA_PATH_LIST));

			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not allocate memory for path list.");
			glbl_qDesc = qDesc;
			return(IMS_ERROR);
		}

		/*
		** Get path
		*/

		if (storeQueryData(msgDesc, 0, &(ptr->path), &len) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not store path");
			glbl_qDesc = qDesc;
			return(IMS_ERROR);
		}

		/*
		** Get start_granule_idx
		*/ 
		getQueryData(msgDesc, 1, &qptr, &qlen);
		memcpy(&(ptr->start_granule_idx), qptr, qlen);

		if (qlen == 0)
			ptr->start_granule_idx = -1;



		/*
		** Get end_granule_idx
		*/

		getQueryData(msgDesc, 2, &qptr, &qlen);
		memcpy(&(ptr->end_granule_idx), qptr, qlen);

		if (qlen == 0)
			ptr->end_granule_idx = -1;

		ptr->next = NULL;

	}

	endQuery(msgDesc);
	glbl_qDesc = qDesc;
	return(IMS_OK);
}

/********************************************************************
**
** freeDatasetPaths
**
********************************************************************/
int freeDatasetPaths(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_PATH_LIST *ptrPath)
{
	IMS_FA_PATH_LIST *ptr;

	while (ptrPath != NULL)
	{
		ptr = ptrPath;
		ptrPath = ptrPath->next;
		free(ptr->path);
		free(ptr);
	}
	return(IMS_OK);
}

/*********************************************************************
**
** locateRepositoryFile
**
*********************************************************************/

int locateRepositoryFile(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_PATH_LIST *datasetPath,
	char *fullPathName,
	char *name,
	char *format,
	int granule_idx,
	char *ext)
{
	IMS_FA_PATH_LIST *ptrPath;

	ptrPath = datasetPath;

	
	/*
	** Determine appropriate path
	*/

	for (ptrPath = datasetPath; ptrPath != NULL; 
			ptrPath = ptrPath->next)
	{
		
		if ((ptrPath->start_granule_idx  == -1) &&
		   (ptrPath->end_granule_idx == -1))
		{
			break;
		}

		if ((ptrPath->start_granule_idx <= granule_idx) &&
			(ptrPath->end_granule_idx == -1))
 		{
			break;
		}

		if ((ptrPath->start_granule_idx <= granule_idx) &&
			(granule_idx <= ptrPath->end_granule_idx))
		{
			break;
		}
	}

	if (ptrPath == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"No path was found for granule index %d.", granule_idx);
		return(IMS_ERROR);
	}

	sprintf(fullPathName, "%s/%s.%s",
			ptrPath->path, name, ext );
	
	return(IMS_OK);


}

/*********************************************************************
**
** byte_swap_int
**
*********************************************************************/

void byte_swap_int(int *fld)
{
	char temp_old[4];
	char temp_new[4];

	memcpy(temp_old, (char *) fld, 4);

	temp_new[0] = temp_old[3];
	temp_new[1] = temp_old[2];
	temp_new[2] = temp_old[1];
	temp_new[3] = temp_old[0];

	memcpy((char *) fld, (char *) temp_new, 4);

}

/*********************************************************************
**
** byte_swap_word
**
*********************************************************************/
void byte_swap_word(short int *fld)
{
	char temp_old[2];
	char temp_new[2];

	memcpy(temp_old, (char *) fld, 2);

	temp_new[0] = temp_old[1];
	temp_new[1] = temp_old[0];

	memcpy((char *) fld,  (char *) temp_new, 2);

}


/********************************************************************
**
** getStorageNumber
**
** Return the ordinal number based on the START_ADRESS for the
** particular media id.
********************************************************************/
int getStorageNumber(
	IMS_MSG_STRUCT *msgDesc,
	char *granule_table,
	char *media_id,
	char *media_id_alias,
	int counter)
	
{
	char qbuf[IMS_COL255_LEN+1];
	int status;
	char *qptr;
	int qlen;
	IMS_QI_DESC_OBJ *qDesc;
	int ord_number = 0;


	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Setup login parameters. 
	*/


    IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE(qDesc, 10);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
		 
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
		 
	  
	status = ims_qiLogin(qDesc);
		   
	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		(void) ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	qDesc->cmd = qbuf;


	if (media_id_alias != NULL)
	{
		sprintf(qDesc->cmd,
			"select count(*) from  %s where \
		MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' and MEDIA_ID_ALIAS = '%s'  \
		and START_ADDRESS < %d order by START_ADDRESS",
			granule_table, media_id_alias, counter);
	}
	else
	{
		sprintf(qDesc->cmd,
			"select count(*) from  %s where \
		MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' and MEDIA_ID = '%s'  \
		and START_ADDRESS < %d order by START_ADDRESS",
			granule_table, media_id, counter);
	}

	IMS_SETROWCOUNT(qDesc, "1");

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of media information.");
			(void) ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void) memcpy((char *) &ord_number,
					qDesc->valAddr[0], qDesc->valLength[0]);

	}

	ims_qiFreeDesc(qDesc);


	return(ord_number+1);
}

/********************************************************************
**
** addHistoryInfo
**
** Add the statistics to the history information table fa_report_history.
********************************************************************/
static int addHistoryInfo(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	int dataset_idx,
	int granule_idx,
	int attempt,
	int sent,
	int archived,
	char *name)
	
{
	char qbuf[IMS_COL255_LEN+1];
	int status;
	IMS_QI_DESC_OBJ *qDesc;
	char gentime[IMS_DATETIME_LEN+1];
	IMS_NUMERIC_DATE dateStruct;

    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		unlink(name);
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get current date for ingestion");
		return(IMS_ERROR);

	}

	ims_numericDateToDBMSA(&dateStruct, gentime);


	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Setup login parameters. 
	*/


    IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE(qDesc, 10);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
		 
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
		 
	  
	status = ims_qiLogin(qDesc);
		   
	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		(void) ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	qDesc->cmd = qbuf;



	sprintf(qbuf, "insert into fa_report_history \
		(report_id, name, dataset_idx, granule_idx, report_attempt, \
		report_fasent, report_archived, gen_time) values \
		(%d, '%s', %d, %d, '%c', '%c', '%c', '%s')",
		report_id, ims_extractFileName(name), dataset_idx, granule_idx,
		(attempt == TRUE)?'Y':'N',
		(sent == TRUE)?'Y':'N',
		(archived == TRUE)?'Y':'N', gentime);


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform insert record into history table.");
			(void) ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}
	}

	ims_qiFreeDesc(qDesc);

	return(IMS_OK);


}

/********************************************************************
**
** getReportGranuleIndex
**
** Return the granule index for the indicated report
********************************************************************/
static int getReportGranuleIndex(
	IMS_MSG_STRUCT *msgDesc,
	int *granule_idx,
	int dataset_idx,
	char *name)
	
{
	char qbuf[IMS_COL255_LEN+1];
	int status;
	char *qptr;
	int qlen;
	IMS_QI_DESC_OBJ *qDesc;
	int rowCount = 0;


	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Setup login parameters. 
	*/


    IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE(qDesc, 10);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
		 
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
		 
	  
	status = ims_qiLogin(qDesc);
		   
	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		(void) ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	qDesc->cmd = qbuf;


	sprintf(qDesc->cmd,
		"select granule_idx from granules_%d where name = '%s'",
		dataset_idx, ims_extractFileName(name));

	IMS_SETROWCOUNT(qDesc, "1");

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of media information.");
			(void) ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void) memcpy((char *) granule_idx,
					qDesc->valAddr[0], qDesc->valLength[0]);

		rowCount ++;

	}

	ims_qiFreeDesc(qDesc);

	if (rowCount == 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not locate granule index for dataset_idx = %d name = '%s'",
			dataset_idx, ims_extractFileName(name));
		return(IMS_ERROR);
	}


	fprintf(stderr, "Returning granule %d\n", *granule_idx);


	return(IMS_OK);
}
