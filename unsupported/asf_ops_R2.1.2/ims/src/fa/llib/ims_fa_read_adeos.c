static char *sccs = "@(#)ims_fa_read_adeos.c	5.1  03/17/96";
/******************************************************************************
**
** File:	ims_fa_adeos.c
**
** Function: Parse a FA generated file for testing
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
** Local NASDA ADEOS header type
*/

typedef struct
{
	char filename[10];
	char blank1;
	char project[6];
	char blank2;
	char send_grnd_code[4];
	char blank3;
	char rec_grnd_code[4];
	char blank4;
	char gen_date[8];
	char blank5;
	char gen_time[8];
	char blank6;
	char rec_len[4];
	char blank7;
	char no_of_records[5];
	char blank8;
	char begin_date[8];
	char blank9;
	char end_date[8];
	char blank10;
	char file_fmt_date[8];
	char blank11;
	char file_ver_date[3];
	char blank12;
	char reserved[39];
	char record_end;
} IMS_ADEOS_HEADER;

typedef struct 
{
	char blank1;
	char rec_num[5];
	char blank2;
	char pass_num[4];
} IMS_SRRD_REC_INFO;

typedef struct ims_srrd_desc_list
{
	IMS_SRRD_REC_INFO rec_info;
	struct ims_srrd_desc_list *next;
} IMS_SRRD_DESC_LIST;

/*
** Local Functions
*/




/****************************************************************************
**
** read_format_adeos_reac
**
** Format the ADEOS REAC record.
****************************************************************************/
int read_format_adeos_reac(
	IMS_MSG_STRUCT *msgDesc,
	FILE *fptr)
{
	struct 
	{
		char date[8];
		char blank1;
		char path_num[5];
		char blank2;
		char plan_num[11];
		char blank3;
		char x_band[2];
		char blank4;
		char tape_num[10];
		char blank5;
		char begin_tape[6];
		char blank6;
		char end_tape[6];
		char blank7;
		char begin_rec_date[21];
		char blank8;
		char end_rec_date[21];
		char blank9;
		char bit_sync_start[21];
		char blank10;
		char bit_sync_end[21];
		char blank11;
		char acq_status;
		char record_end;
	} reac;

	reac.record_end = 0xa;

	if ((int) fread((char *) &reac, sizeof(reac), 1, fptr) < 1) 
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read ADEOS REAC Record.");
		return(IMS_ERROR);
	}

	printf("** Parsing REAC Record **\n");
	ims_printf("Date = %s\n", reac.date, 8);	
	ims_printf("Path Number = %s\n", reac.path_num, 5);	
	ims_printf("Plan Number = %s\n", reac.plan_num, 11);	
	ims_printf("x_band = %s\n", reac.x_band, 2);
	ims_printf("tape_num = %s\n", reac.tape_num, 10);
	ims_printf("begin_tape = %s\n", reac.begin_tape, 6);
	ims_printf("end_tape = %s\n", reac.end_tape, 6);
	ims_printf("begin_rec_date = %s \n", reac.begin_rec_date, 21);
	ims_printf("end_rec_date = %s \n", reac.end_rec_date, 21);
	ims_printf("bit_sync_start = %s\n", reac.bit_sync_start, 21);
	ims_printf("bit_sync_end = %s\n", reac.bit_sync_end, 21);
	printf("acq_status = %c\n", reac.acq_status);
	printf("record_end = %d\n", reac.record_end);
	return(IMS_OK);
}

/****************************************************************************
**
** ims_read_adeos_reac
**
** ADEOS Aquisition Report
****************************************************************************/

int ims_read_adeos_reac(
	IMS_MSG_STRUCT *msgDesc, 
	char *filename) 
{
	IMS_ADEOS_HEADER fixed_header;	
	FILE *fptr;
	int no_of_recs;
	int i;

	/*
	** Open the report file.
	*/

	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fread((void *) &fixed_header, sizeof(IMS_ADEOS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read ADEOS REAC header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Dump Header 
	*/

	printf("** Parsing REAC Header **\n");
	ims_printf("Filename = %s\n",  fixed_header.filename, 10);
	ims_printf("Project = %s\n",  fixed_header.project, 6);
	ims_printf("Sending Ground Code = %s\n", fixed_header.send_grnd_code, 4);
	ims_printf("Receive Ground Code = %s\n", fixed_header.rec_grnd_code, 4);
	ims_printf("Generation Date = %s\n", fixed_header.gen_date, 8);
	ims_printf("Generation Time = %s\n", fixed_header.gen_time, 8);
	ims_printf("Record Length = %s\n", fixed_header.rec_len, 4);
	ims_printf("No. of Records = %s\n", fixed_header.no_of_records,5);
	ims_printf("Begin Date = %s\n", fixed_header.begin_date, 8);
	ims_printf("End Date = %s\n", fixed_header.end_date, 8);
	ims_printf("File Fmt Date = %s\n", fixed_header.file_fmt_date, 8);
	ims_printf("File Ver Date = %s\n", fixed_header.file_ver_date, 8);
	ims_printf("Reserved = %s\n", fixed_header.reserved, 39);
	ims_printf("Record End = %s \n", fixed_header.record_end, 1);

	no_of_recs = atoi(fixed_header.no_of_records);

	/*
	** Build and process 1..n REAC Records
	*/

	for (i = 0; i < no_of_recs; i++)
	{
		if (read_format_adeos_reac(msgDesc, fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format ADEOS REAC Record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}


	fclose(fptr);
	return(IMS_OK);
	
}


/****************************************************************************
**
** read_srrd_desc
**
** Format SRRD Descriptor.
****************************************************************************/

int read_srrd_desc(
	IMS_MSG_STRUCT *msgDesc, 
	int *total_recs,
	IMS_SRRD_DESC_LIST **head,
	FILE *fptr) 
{
	int i;
	char asc_total_recs[6];
	char blank;
	char rec_num[6];
	char pass_info[5];
	IMS_SRRD_REC_INFO *rec_info;
	IMS_SRRD_DESC_LIST *ptr;
	char end_rec = 0x0A;

	*head = NULL;

	/*
	** Read total number of tape records
	*/

	printf("** SRRD File Descriptor Block **\n");

	if ((int) fread((void *) asc_total_recs, 4, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write SRRD Descriptor");
		return(IMS_ERROR);
	}
	*total_recs =  atoi(asc_total_recs);

	ims_printf("Total Records = %s\n", asc_total_recs, 4);


	/*
	** Build a list of record number, and number of pass info records.
	** This is based on the total number of records and therefore, this
	** is a variable section of the report.
	*/

	for (i = 0; i < *total_recs; i++)
	{
		if (i == 0)
			ptr = (void *) malloc(sizeof(IMS_SRRD_DESC_LIST));
		else
		{
			ptr->next = (void *) malloc(sizeof(IMS_SRRD_DESC_LIST));
			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not allocate SRRD Descriptor");
			return(IMS_ERROR);
		}

		if (i == 0)
		{
			*head = ptr;
		}
		
		rec_info = (void *) &(ptr->rec_info);
		ptr->next = NULL;


		/*
		** Setup values for this record.
		*/

		if ((int) fread((void *) rec_info, sizeof(IMS_SRRD_REC_INFO), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not read SRRD Descriptor");
			return(IMS_ERROR);
		}

		ims_printf("Record Number = %s\n", rec_info->rec_num, 5);
		ims_printf("Pass Info = %s\n", rec_info->pass_num, 4);
	}

	/*
	** Write end delimiter
	*/

	if ((int) fread((void *) &end_rec, 1, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read SRRD Descriptor");
		return(IMS_ERROR);
	}

	printf("End Record = %d\n", end_rec);

	return(IMS_OK);
}

/****************************************************************************
**
** read_srrd_pass_info
**
** Format pass information record for ADEOS SRRD Report
****************************************************************************/

int read_srrd_pass_info(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr) 
{

	struct
	{
		char aq_date[8];
		char blank1;
		char path_num[5];
		char blank2;
		char op_plan[11];
		char blank3;
		char x_band[2];
		char blank4;
		char tape_num[10];
		char blank5;
		char pos_start[6];
		char blank6;
		char pos_end[6];
		char blank7;
		char begin_data[17];
		char blank8;
		char end_data[17];
		char blank9;
		char sync_start[17];
		char blank10;
		char sync_stop[17];
		char blank11;
		char aq_status;
		char record_end;
	} pass_record;

	/*
	** Write out tape record.
	*/

	pass_record.record_end = 0x0A;

	if ((int) fread((char *) &pass_record, sizeof(pass_record), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out pass record");
		return(IMS_ERROR);
	}
	printf("** Pass Record **\n");

	ims_printf("Acquisition Date = %s\n", pass_record.aq_date, 8);
	ims_printf("Path Number = %s\n", pass_record.path_num, 5);
	ims_printf("Operation Plan = %s\n", pass_record.op_plan, 11);
	ims_printf("X Band = %s\n", pass_record.x_band, 2);
	ims_printf("Tape Number = %s\n", pass_record.tape_num, 10);
	ims_printf("Position Start = %s\n", pass_record.pos_start, 6);
	ims_printf("Position End = %s\n", pass_record.pos_end, 6);
	ims_printf("Begin Data = %s\n", pass_record.begin_data, 17);
	ims_printf("End Data = %s\n", pass_record.end_data, 17);
	ims_printf("Sync Start = %s\n", pass_record.sync_start, 17);
	ims_printf("Sync Stop = %s\n", pass_record.sync_stop, 17);
	printf("Acquisition Status = %c\n", pass_record.aq_status);
	printf("Record End = %d\n", pass_record.record_end);
	return(IMS_OK);
}

/****************************************************************************
**
** read_adeos_srrd
**
** Format SRRD Tape Record.
****************************************************************************/

int read_adeos_srrd(
	IMS_MSG_STRUCT *msgDesc, 
	char *rec_num,
	char *pass_info,	
	FILE *fptr) 
{
	char end_rec = 0x0A;
	struct
	{
		char date[8];
		char blank1;
		char tape_num[10];
		char blank2;
		char num_passes[5];
		char record_end;
	} tape_info;

	int i;

	memset((char *) &tape_info, 32, sizeof(tape_info));
	tape_info.record_end = 0xa;

	/*
	** Read tape record.
	*/

	printf("** SRRD Tape Record **\n");

	if ((int) fread((char *) &tape_info, sizeof(tape_info), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read tape record");
		return(IMS_ERROR);
	}

	ims_printf("date = %s\n", tape_info.date, 8);
	ims_printf("tape number = %s\n", tape_info.tape_num, 10);
	ims_printf("number of passes = %s\n", tape_info.num_passes, 5);
	printf("record end = %d\n", tape_info.record_end);

	

	/*
	** Build and write-out 1..n pass information records
	*/

	fprintf(stderr, "# of Passes = %s\n", pass_info);  

	for (i = 0; i < atoi(pass_info); i++)
	{

		if (read_srrd_pass_info(msgDesc, fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, 
						"Could not read SRRD Pass Record");
			return(IMS_ERROR);
		}
	}

	return(IMS_OK);

}

/****************************************************************************
**
** ims_adeos_srrd
**
** ADEOS Shipment Report
****************************************************************************/

int ims_read_adeos_srrd(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	IMS_ADEOS_HEADER fixed_header;	
	FILE *fptr;
	int no_of_tapes = 2;
	IMS_SRRD_DESC_LIST *head, *ptr;
	int total_recs;
	int i;


	/*
	** Open the report file.
	*/

/*    strcpy(filename, "SRRD000001");
*/

	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fread((void *) &fixed_header, sizeof(IMS_ADEOS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write CATA header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	printf("** Parsing SRRD Header **\n");
	ims_printf("Filename = %s\n",  fixed_header.filename, 10);
	ims_printf("Project = %s\n",  fixed_header.project, 6);
	ims_printf("Sending Ground Code = %s\n", fixed_header.send_grnd_code, 4);
	ims_printf("Receive Ground Code = %s\n", fixed_header.rec_grnd_code, 4);
	ims_printf("Generation Date = %s\n", fixed_header.gen_date, 8);
	ims_printf("Generation Time = %s\n", fixed_header.gen_time, 8);
	ims_printf("Record Length = %s\n", fixed_header.rec_len, 4);
	ims_printf("No. of Records = %s\n", fixed_header.no_of_records,5);
	ims_printf("Begin Date = %s\n", fixed_header.begin_date, 8);
	ims_printf("End Date = %s\n", fixed_header.end_date, 8);
	ims_printf("File Fmt Date = %s\n", fixed_header.file_fmt_date, 8);
	ims_printf("File Ver Date = %s\n", fixed_header.file_ver_date, 8);
	ims_printf("Reserved = %s\n", fixed_header.reserved, 39);


	/*
	** Build the SRRD Descriptor Record.
	*/

	if (read_srrd_desc(msgDesc, &total_recs, &head, fptr) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format ADEOS REAC Record.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build and process 1..n SRRD Records
	*/

	ptr = head;
	total_recs = atoi(fixed_header.no_of_records);

	for (i = 0; i < total_recs; i++)
	{
		if (read_adeos_srrd(msgDesc, ptr->rec_info.rec_num, 
					ptr->rec_info.pass_num,
					fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format ADEOS SRRD Record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
		head = ptr;
		ptr = ptr->next;
		free(head);
	}

	fclose(fptr);
	return(IMS_OK);
}

