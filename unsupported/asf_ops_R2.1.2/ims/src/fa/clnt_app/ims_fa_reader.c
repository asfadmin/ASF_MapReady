static char *sccs = "@(#)ims_fa_reader.c	5.2 03/10/97";
/******************************************************************************
**
** File:	ims_fa_read.c
**
** Function: Read FA Reports.
**
** Author: Dan Crichton	
**
** Date:	7/18/95
**
**
******************************************************************************/

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

/*
** Local Types
*/

/*
** Local Functions
*/

int readReport(IMS_MSG_STRUCT *, int, char *filename);
int startSubmission(char *, char *, char *);

static char *glb_programName;
static IMS_QI_DESC_OBJ *glbl_qDesc;


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
	int job_id;
	int report_id;

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
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);


	/*
	** Do some processing....
	*/

	if (argc < 2)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not get report id.\n");
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	if (argc < 3)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not get filename.\n");
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
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}


	if (readReport(msgDesc, report_id, argv[2]) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read report %d\n",
				report_id);
		(void) ims_msgStructFree(msgDesc);
		exit(1);  /* Cause an abort */
	}	


	(void) ims_msgStructFree (msgDesc);
	exit(0);
}


/****************************************************************************
**
** readReport
**
****************************************************************************/
int readReport(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	char *filename)
{
	/*
	** Find an execute the correct FA report.
	*/

	switch(report_id)
	{
#if 0
		case IMS_CSA_ARCHSTRGRPT:
			if (ims_csa_archstrgrpt(msgDesc) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"CSA_ARCHSTRGRPT report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_CSA_RECRPT:
			if (ims_csa_recrpt(msgDesc) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"CSA_RECRPT report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_CSA_CALIBAVAILRPT:
			if (ims_csa_calibavailrpt(msgDesc) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"CSA_CALIBAVAILRPT report not successful.");
				return(IMS_ERROR);
			}
			break;
#endif


		case IMS_ESA_REEX:
		case IMS_ESA2_REEX:
			if (ims_read_esa_reex(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_REEX report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_ESA_REAQ:
		case IMS_ESA2_REAQ:
			if (ims_read_esa_reaq(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_REAQ report not successful.");
				return(IMS_ERROR);
			}
			break;


		case IMS_ESA_RESM:
		case IMS_ESA2_RESM:
			if (ims_read_esa_resm(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ESA_RESM report not successful.");
				return(IMS_ERROR);
			}
			break;


		case IMS_NASDA_MSGM:
			if (ims_read_jers_msgm(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_MSGM report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_NASDA_REAC:
			if (ims_read_jers_reac(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_REAC report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_NASDA_CATA:
			if (ims_read_jers_cata(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"NASDA_REAC report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_ADEOS_SRRD:
			if (ims_read_adeos_srrd(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"ADEOS_SRRD report not successful.");
				return(IMS_ERROR);
			}
			break;

		case IMS_ADEOS_REAC:
			if (ims_read_adeos_reac(msgDesc, filename) < IMS_OK)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"Read ADEOS_REAC report not successful.");
				return(IMS_ERROR);
			}
			break;

	}
	return(IMS_OK);
}



void ims_printf(char *format, char *str, int len)
{
	char *ptr;

	ptr = malloc(len+1);

	if (ptr == NULL)
	{
		return;
	}

	if (len == 1)
	   	*ptr == (char) str; 
	else
		strncpy(ptr, str, len);

	ptr[len] = 0;

	printf(format, ptr);
	free(ptr);
}
