static char *sccs = "@(#)ims_fa_esa.c	5.11  10/27/97";
/******************************************************************************
**
** File:	ims_fa_esa.c
**
** Function: Perform processing of FA Reports for the European Space Agency.
**
** Author: Dan Crichton	
**
** Date:	7/12/95
**
** Modification History:
**     R1B' 8/26/96 D. Crichton - Byte swapped all of REAQ integer and 
**     short int fields for VAX destination.  Keep local file as Unix.
**
**		 3/25/97 D. Ting
**		 R2.1 Changes. Changed the query from granules to downlink_entry only
**		 if necessary.
**		 5/5/97 D. Ting
**		 R2.1 Changes. Query based on station_id and revolution for reaq report.
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
#include <ims_fa_track.h>

/*
** Local Types
*/

typedef struct 
{
	char date[8];  	/* YYYYMMDD */
	char time[6];	/* HHMMSS */	

} IMS_ESA_DATE_TIME;

typedef struct 
{
	int days, msec;
} IMS_ESA_UTC;

typedef struct
{
		char satellite[2];
		char sensor[3];
		char orbit_no[5];
		char frame[4];
		char fac_id[2];
} X_UMP_ENTRY_ID;

/*
** HDDT Label Definition.
*/

typedef struct
{
	char num_acquisitions[4];
	char medium_id[8];
	char sat_id;
	char start1[8];
	char stop1[8];
	char start2[8];
	char stop2[8];
	char start3[8];
	char stop3[8];
	char station_id;
	char drive;
	char demodulator;
} HDDT_LABEL;

typedef struct hddt_label_list
{
	HDDT_LABEL hddt_label;
	struct hddt_label_list *next;
} HDDT_LABEL_LIST;


typedef struct pcd_record_info
{
	char pcdvalf[2];	/* PCD Validity Flag */
	char hrorlr[2];		/* HR or LR carrier lock */
	char agcpcd[2];		/* AGC PCD */
	char rtber[2];		/* Real Time Bit Error Rate */
	char pbber[2];		/* Playback Bit Error Rate */
	char qbitcl[2];		/* Q Bit Clock Lock. */
	char ibitcl[2];		/* I Bit Clock Lock. */
	char rtlock[2];		/* Real Time Lock. */
	char pblock[2];		/* Playback Lock. */
	char pcdsum[2];		/* PCD Summary */
} PCD_RECORD_INFO;

typedef struct pcd_record_list
{
	PCD_RECORD_INFO rec;
	struct pcd_record_list *next;
} PCD_RECORD_LIST;

typedef struct pcd_header
{
	char common[50];	/* Common Header */
	char datatake[14];
	char hddr_id[2];
	char year[5];
	char time[17];
	char numrecs[5];	/* Number of PCD Records */
} PCD_HEADER;

typedef struct acquisition_list
{
	char pcd_file_name[IMS_COL30_LEN+1];
	int dataset_idx;
	int granule_idx;
	int sequence;
	char station_id[IMS_COL15_LEN+1]; /* R2.1 */
	char format[IMS_COL10_LEN+1];
	struct acquisition_list *next;
} ACQUISITION_LIST;


static char glbl_grnlTbl[IMS_COL30_LEN+1];
static IMS_NUMERIC_DATE *glbl_gendate;

/*
** Local Functions
*/

static int read_PCD_file(IMS_MSG_STRUCT *, char *, PCD_HEADER *, PCD_RECORD_LIST **);
static int format_esa_header(IMS_MSG_STRUCT *, int, char *, char *, IMS_FA_INTERFACE *);
static int build_resm_record(IMS_MSG_STRUCT *, int, char *, char *, IMS_FA_INTERFACE *);
static int esa_report_header(IMS_MSG_STRUCT *, char *, int, int, int, FILE *, FILE *);
static HDDT_LABEL_LIST *reaq_bld_hddt_list(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *);
static int reaq_format_records(IMS_MSG_STRUCT *, FILE *, FILE *, IMS_FA_INTERFACE *);
static int reaqTracking(IMS_MSG_STRUCT *, int, char *, char *);
static int reex_format_record(IMS_MSG_STRUCT *, FILE *, IMS_FA_INTERFACE *);
static ACQUISITION_LIST *reaq_bld_acq_list(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, int *);

/****************************************************************************
**
** format_esa_header
**
** Format the ESA header string
****************************************************************************/
static int format_esa_header(
	IMS_MSG_STRUCT *msgDesc,
	int report_id, 
	char *format_str,
	char *filename,
	IMS_FA_INTERFACE *fa_data)
{
	char report_str[5];
	char report_date[7];
	char report_time[9];
	char dest_id[3];
  	static IMS_NUMERIC_DATE dateDef;
	int counter;
	int year;
	char platform[3];
	char orig_id[8];



	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		strcpy(platform, "E1");
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		strcpy(platform, "E2");
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Invalid platform specified '%s'.", fa_data->platform);
		return(IMS_ERROR);
	}

	/*
	** Get counter
	*/
	if (getReportCounter(msgDesc, report_id, &counter) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get report counter.");
				return(IMS_ERROR);
	}

	fa_data->report_counter = counter;
												 
	/*
	** Update counter value.
	*/
																	  
	if (updateReportCounter(msgDesc, report_id, counter + 1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not update report counter.");
		return(IMS_ERROR);
	}

	/*
	** Get the current date  
	*/

  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	/*
	** Save global date/time...
	*/

	glbl_gendate = &dateDef;

	/*
	** 2 digit year.
	*/

	year = dateDef.year;

	if (year >= 2100)
	{
		year = year - 2100;
	}

	if (year >= 2000)
	{
		year = year - 2000;
	}
	else 
		year = year - 1900;

	sprintf(report_date, "%02d%02d%02d", year,
		dateDef.month, dateDef.day);

	sprintf(report_time, "%02d:%02d:%02d",
		dateDef.hours, dateDef.minutes, dateDef.seconds);


	switch (report_id)
	{
		case IMS_ESA_RESM:
		case IMS_ESA2_RESM:
			strcpy(report_str, "RESM");
			strcpy(dest_id, "EC");
			strcpy(orig_id, "AF");
			break;

		case IMS_ESA_REAQ:
		case IMS_ESA2_REAQ:
			if(strcmp(fa_data->station_id, "FA") == 0)
			    strcpy(orig_id, "AF");
      else
			    strcpy(orig_id, "MM");

			strcpy(report_str, "REAQ");
			strcpy(dest_id, "CF");
			break;

		case IMS_ESA_REEX:
		case IMS_ESA2_REEX:

			strcpy(report_str, "REEX");
			strcpy(dest_id, "CF");
			strcpy(orig_id, "AF");
			break;
	}	

	sprintf(format_str, "%s_%s%s%s%04d.%s%s",report_str, 
			report_date, orig_id,dest_id,  counter, platform, report_time);

	sprintf(filename, "%s_%s%s%s%04d.%s",report_str, 
			report_date, orig_id, dest_id,  counter, platform);
	return(IMS_OK);
			
}


/****************************************************************************
**
** build_resm_record
**
** Format a line for the ESA Storage Report.
****************************************************************************/

static int build_resm_record(
	IMS_MSG_STRUCT *msgDesc, 
	int ship_id,
	char *ship_date,
	char *line,
	IMS_FA_INTERFACE *fa_data)
{
	struct
	{
		char ship_id[4];
		char ship_date[24];
		char facility[2];
		char medium_id[8];
		char medium_type[2];
		char orig_med_id[8];
		char orig_med_type[2];
		char dataset_info[38];
		char remarks[60];
	} resm_record;

	struct 
	{
	    X_UMP_ENTRY_ID x_ump_entry_id;
		char prod_type[5];
		char scene[1];
		char fac_id[2];
		IMS_ESA_DATE_TIME date;
	} prod_dataset;
	char tmpDate[IMS_DATETIME_LEN+1];
	X_UMP_ENTRY_ID *x_ump_entry_id;
	char workbuf[64];
	IMS_NUMERIC_DATE dateStruct;
	char *qdata;
	int qlen;
	char year[5], month[3], day[3];
	char formatDate[IMS_DATETIME_LEN+1];
	int i_orbit;


	memset((char *) &resm_record, 0, sizeof(resm_record));

	sprintf(workbuf, "%04d", ship_id);

	memcpy(resm_record.ship_id, workbuf, 4);

	memcpy(resm_record.facility, "CF", 2);

	/*
	** ship_date
	** This field is provided by the user or caller in YYYYMMDD format.
	*/

	sscanf(ship_date, "%4s%2s%2s", year, month, day);

	sprintf(formatDate, "%s-%s-%sT00:00:00.000", year, month, day);


	if (ims_timeToNumericDate(msgDesc, formatDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not convert ship date '%s' for RESM report", formatDate);
		return(IMS_ERROR);
	}

	ims_numericDateToESAA(&dateStruct, workbuf);
	memcpy(resm_record.ship_date, workbuf, sizeof(resm_record.ship_date));



	/* 
	** medium_id
	** TAPE AVAILABLE MESSAGE: MEDIA_ID
	** Format of MEDIA_ID
	*/
	getQueryData(msgDesc, 0, &qdata, &qlen);
	memcpy(resm_record.medium_id, qdata, sizeof(resm_record.medium_id));

	/*
	** medium_type
	*/

	memcpy(resm_record.medium_type, "H ",2); /* from ACS for HDDT */

	/*
	** original medium_id
	** Currently using downlink: ARCHIVE_SIGNAL.
	*/

	getQueryData(msgDesc, 1, &qdata, &qlen);
	strcpy(workbuf, "AF");
	memcpy(workbuf + 2, qdata + 6, 6);
	memcpy(resm_record.orig_med_id, workbuf, 8);


	/*
	** orig_medium_type: Currently using H.
	*/

	memcpy(resm_record.orig_med_type, "H ",2); /* from ACS for HDDT */


	/*
	** Dataset Id
	** Following product id information used in ACS generation.
	** See X_PRODUCT_ID
	*/

	memset(&prod_dataset, 32, sizeof(prod_dataset));


	/*
	** Fill in X_UMP_ENTRY_ID
	*/

	x_ump_entry_id = &(prod_dataset.x_ump_entry_id);

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		memcpy(x_ump_entry_id->satellite,  "E1", 2);
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		memcpy(x_ump_entry_id->satellite,  "E2", 2);
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Invalid platform '%s' for ESA RESM report.", fa_data->platform);
		return(IMS_ERROR);
	}

	memcpy(x_ump_entry_id->sensor, "SAR", 3);	

	getQueryData(msgDesc, 2, &qdata, &qlen);
	memcpy(&i_orbit, qdata, qlen);
	sprintf(workbuf,"%05d", i_orbit);
	memcpy(x_ump_entry_id->orbit_no, workbuf, 5);



	/*
	** Frame number - Use 0000 to match ACS for now???
	*/

	memcpy(x_ump_entry_id->frame, "0000", 4);

	memcpy(x_ump_entry_id->fac_id,"AF", 2);


	/*
	** prod_type = UNP (Unprocessed data) from ACS
	*/

	memcpy(prod_dataset.prod_type, "UNP  ", 5);

	memcpy(prod_dataset.scene, "0", 1);  /* Full Scene from ACS */

	memcpy(prod_dataset.fac_id, "AF", 2); 

	
	/*
	** Product Date:
	** Downlink Message: START_TIME
	*/

	getQueryData(msgDesc, 3, &qdata, &qlen);
	memset(tmpDate, 0, sizeof(tmpDate));
	memcpy(tmpDate, qdata, qlen);

	if (ims_timeToNumericDate(msgDesc, tmpDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not convert date '%s' for RESM report", tmpDate);
		return(IMS_ERROR);
	}

	memset(tmpDate, 0, sizeof(tmpDate));
	sprintf(tmpDate, "%04d%02d%02d%02d%02d%02d", dateStruct.year, 
		dateStruct.month,
		dateStruct.day, dateStruct.hours, dateStruct.minutes, 
		dateStruct.seconds);

	memcpy((char *) &(prod_dataset.date), tmpDate, 14);

	/*
	** Save prod_dateset information into the resm_record.
	*/

	memcpy((char *) &(resm_record.dataset_info), (char *) &prod_dataset,
		sizeof(prod_dataset));

	memset(resm_record.remarks, 32, sizeof(resm_record.remarks));

#if 0
	/*
	** Leave the remarks blank.
	*/

	memcpy(resm_record.remarks, 
		"ASF RAW DATA SHIPPED PER MEDIA SPECIFICATION",
		44);
#endif

	memcpy(line,(char *) &resm_record,  sizeof(resm_record));
	
	return(IMS_OK);
}

/****************************************************************************
**
** ims_esa_resm
**
** ESA shipment report
****************************************************************************/

int ims_esa_resm(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	char fixed_header[81];
	char line[150]; /* Per ESA document */
	char filename[IMS_PATH_LEN+1];
	char tempName[IMS_PATH_LEN+1];
	char ta_gran_table[IMS_COL30_LEN+1];
	FILE *fptr;
	int status;
	int rowCount;
	int i;
	short int record_length; 
	char qbuf[IMS_COL512_LEN];

    /*
	** Get Downlink Granules Table
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E1, 
				IMS_FA_E1) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-1 RESM.");
			 return(IMS_ERROR);
		}
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E2, 
				IMS_FA_E2) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-2 RESM.");
			 return(IMS_ERROR);
		}
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Unknown platform type found in ESA RESM");
		return(IMS_ERROR);
	}

	/*
	** Get Tape Available Granule Table
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (getGranuleTableName(msgDesc, ta_gran_table,IMS_FA_TA_E1, 
				"HC") < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-1 RESM.");
			 return(IMS_ERROR);
		}
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		if (getGranuleTableName(msgDesc, ta_gran_table, IMS_FA_TA_E2, 
				"HC") < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-2 RESM.");
			 return(IMS_ERROR);
		}
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Unknown platform type found in ESA RESM");
		return(IMS_ERROR);
	}


	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (format_esa_header(msgDesc, IMS_ESA_RESM, fixed_header, filename,
				fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			return(IMS_ERROR);
		}
	}
	else
	{
		if (format_esa_header(msgDesc, IMS_ESA2_RESM, fixed_header, filename,
				fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			return(IMS_ERROR);
		}
	}

	/*
	** Open the report file.
	*/
	ims_concatFilePath(tempName, fullPathName, filename);
	strcpy(fullPathName, tempName);

	fptr = fopen(fullPathName, "wb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	record_length = strlen(fixed_header);	
	fixed_header[record_length] = 0xa;
	record_length++;


	if ((int) (fwrite((void *) &fixed_header, record_length, 1, fptr)) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_RESM header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	
	/*
	** Build variable data records by looping through the tape ids
	** and pulling back the appropriate DOWNLINK MESSAGES in the granules
	** table.
	*/

	for (i = 0; i < fa_data->tape_count; i++)
	{
		/*
		** Setup query.
		*/

		sprintf(qbuf, "select distinct t.MEDIA_ID_ALIAS , d.MEDIA_ID,\
						d.REVOLUTION, d.START_TIME\
						from %s d, %s t, platforms p where t.MEDIA_ID = '%s' and \
						PLATFORM = p.acronym and d.status = 1 and t.status = 1 and \
						d.MEDIA_ID = '%s' and p.platform = '%s' and \
						t.STATUS = 'HST_S_AVAILABLE'", 
						glbl_grnlTbl, ta_gran_table,
						fa_data->tape_num[i],
						fa_data->tape_num[i], fa_data->platform);

printf("\n TEST -- qbuf=%s\n", qbuf);
		if (setupQuery(msgDesc, qbuf) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not setup query from table for RESM report.");
			fclose(fptr);
			return(IMS_ERROR);
		}

		/*
		** Currently scan through entire database ...
		*/

		rowCount = 0;
		while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				fclose(fptr);
				return(IMS_ERROR);
			}	

			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}

			rowCount ++;


			if (build_resm_record(msgDesc, fa_data->report_counter, 
					fa_data->tape_date[i], line, fa_data) < IMS_OK)
			{
				fclose(fptr);
				return(IMS_ERROR);
			}
			record_length = 149;
			line[148] = 0xa; /* end of record */

			if ((int) fwrite((void *) line, record_length, 1,  fptr) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_RESM record");
				fclose(fptr);
				return(IMS_ERROR);
			}
		}

		if (rowCount < 1)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not find data for media id %s",
				fa_data->tape_num[i]);
			fclose(fptr); 
			return(IMS_ERROR);

		}


		(void) endQuery(msgDesc);

	}

	fclose(fptr);
	return(IMS_OK);
	
}

/****************************************************************************
**
** esa_report_header
**
** Generate X_REPORT_HEADER per ESA specification.
****************************************************************************/

static int esa_report_header(
	IMS_MSG_STRUCT *msgDesc, 
	char *report,
	int counter, 
	int pass,
	int size,
	FILE *fptr,
	FILE *fptr_swapped) 
{

  	IMS_NUMERIC_DATE dateDef;
	struct 
	{
		IMS_ESA_UTC utc;
        char cmd_type[2];
        char src_of_update;
		char schedule_number[4];
		char command[4];
		char reserved[4];
		char report_id[4];
		char soft_desc[8];
		char report_size[4];
	} esa_report_hdr;
	char s_esa_report_hdr[39];

	short int cmd_type; 
	int sch_number; /* 1000 * pass + sequential number */
	int report_id;

	struct
	{
		int cmd_number;
		int reserved;
	} command;

	memset(&esa_report_hdr, 0, sizeof(esa_report_hdr));
	
	/*
	** Get current generation date and time.
	*/

	memcpy((char *) &dateDef, glbl_gendate, sizeof(IMS_NUMERIC_DATE));

	ims_numericDateToESAI(&dateDef, &(esa_report_hdr.utc.days),
			&(esa_report_hdr.utc.msec));
	
	sch_number = 1000 * pass + 1;
	memcpy(esa_report_hdr.schedule_number, (char *) &sch_number, 4); 
	memcpy(esa_report_hdr.report_size, &size, 4); 
	memcpy(&(esa_report_hdr.reserved), "    ", 4);

	memcpy(s_esa_report_hdr, &(esa_report_hdr), 39);

	if ((int) fwrite((void *) 
			&esa_report_hdr, 39, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report header");
		return(IMS_ERROR);
	}

	/*
	** Write out byte swapped version.
	*/

	byte_swap_int((int *) esa_report_hdr.schedule_number);
	byte_swap_int((int *) esa_report_hdr.report_size);
	byte_swap_int((int *) &(esa_report_hdr.utc.days));
	byte_swap_int((int *) &(esa_report_hdr.utc.msec));

	if ((int) fwrite((void *) 
			&esa_report_hdr, 39, 1, fptr_swapped) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report header");
		return(IMS_ERROR);
	}
	return(IMS_OK);	
}

/****************************************************************************
**
** reaq_bld_acq_list
**
** Build a list of acquisitions which have PCD files.
****************************************************************************/

static ACQUISITION_LIST *reaq_bld_acq_list(
		IMS_MSG_STRUCT *msgDesc, 
		IMS_FA_INTERFACE *fa_data,
		int *acq_count)
{
	ACQUISITION_LIST *ptr, *head, *oldPtr;
	char qbuf[IMS_COL512_LEN+1];
	char granuleTable[IMS_COL30_LEN+1];
	char workbuf[64];
	int rowCount = 0;
	int status;
	char *qdata;
	int qlen;
	short int sequence_temp; /* R2.1 */

	*acq_count = 0;

	head = ptr = NULL;


	/*
	** Get PCD File Information from the tracking ingest message
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_TI_E1, 
				IMS_FA_E1) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
		 "Could not determine granule table name for ERS-1 REAQ track ingest.");
			 return(NULL);
		}
	}
	else
	{

		if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_TI_E2, 
				IMS_FA_E2) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
		 "Could not determine granule table name for ERS-2 REAQ track ingest.");
			 return(NULL);
		}
	}

  /* query change for R2.1 */
	
	sprintf(qbuf, "select d.dataset_idx, d.granule_idx, d.name, r.SEQUENCE, \
					format from %s d, platforms p, downlink_entry r where r.REVOLUTION = %d \
					and d.status = 1 and r.PLATFORM = p.acronym and \
					p.platform = '%s' and r.STATION_ID='%s' and r.ANTENNA_ID = 'ANTENNA_1' \
		      and r.REVOLUTION = d.REVOLUTION \
		      and r.PLATFORM = d.PLATFORM \
		      and r.SEQUENCE = d.SEQUENCE \
					order by r.SEQUENCE", 
					granuleTable,
					fa_data->pass, fa_data->platform, fa_data->station_id); /* R2.1 change */

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query from table for REAQ report.");
		return(NULL);
	}

	/*
	** Currently scan through entire database ...
	*/

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return(NULL);
		}	

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		oldPtr = ptr;

		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(ACQUISITION_LIST));
			head = ptr;
		}
		else
		{
			
			ptr->next = (void *) malloc(sizeof(ACQUISITION_LIST));
			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_FATAL, "Could not allocate memory.");
			endQuery(msgDesc);
			return(NULL);
		}

		/*
		** Dataset Index
		*/

		getQueryData(msgDesc, 0, &qdata, &qlen);
		memcpy(&(ptr->dataset_idx), qdata, qlen);

		/*
		** Granule Index
		*/

		getQueryData(msgDesc, 1, &qdata, &qlen);
		memcpy(&(ptr->dataset_idx), qdata, qlen);


		/*
		** Store the PCD location path 
		*/

		memset(ptr->pcd_file_name, 0, sizeof(ptr->pcd_file_name));
		getQueryData(msgDesc, 2, &qdata, &qlen);
		memcpy(ptr->pcd_file_name, qdata, qlen);
		ims_trim(ptr->pcd_file_name);

		/*
		** Sequence
		*/

		getQueryData(msgDesc, 3, &qdata, &qlen);
		memcpy(&(sequence_temp), qdata, qlen);
		ptr->sequence = (int) sequence_temp; /* R2.1 */

		/*
		** Store the format
		*/

		memset(ptr->format, 0, sizeof(ptr->format));
		getQueryData(msgDesc, 4, &qdata, &qlen);
		memcpy(ptr->format, qdata, qlen);
		ims_trim(ptr->format);


		ptr->next = NULL;

		(*acq_count) ++;


	}

	endQuery(msgDesc);

	return(head);

}

/****************************************************************************
**
** reaq_bld_hddt_list
**
** Create a linked list of the HDDT information. ESA expects up to three
** acquisitions on each HDDT Label.  Therefore we rescan the list each time
** we find a new acquisition row.
****************************************************************************/

static HDDT_LABEL_LIST *reaq_bld_hddt_list(
		IMS_MSG_STRUCT *msgDesc, 
		IMS_FA_INTERFACE *fa_data)
{
	HDDT_LABEL_LIST *ptr, *head;
	HDDT_LABEL hddt_label;
	char qbuf[IMS_COL512_LEN+1];
	char workbuf[64];
	IMS_NUMERIC_DATE dateStruct;
	IMS_ESA_UTC start, end;
	int status;
	char *qdata;
	int qlen;
	int num_aq = 0;
	char startDate[IMS_DATETIME_LEN+1];
	char endDate[IMS_DATETIME_LEN+1];

	head = ptr = NULL;


	/*
	** Query downlink data for all acquisitions for the revolution.
	*/

	memset(startDate, 0, sizeof(startDate));
	memset(endDate, 0, sizeof(endDate));


  /* Changed for R2.1 */
	sprintf(qbuf, "select d.MEDIA_ID, d.START_TIME, d.END_TIME \
					from %s d, platforms p, downlink_entry r where r.REVOLUTION = %d \
					and d.status = 1 and r.PLATFORM = p.acronym  and \
					d.MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' and p.platform = '%s' and r.STATION_ID = '%s' \
					and r.REVOLUTION = d.REVOLUTION \
					and r.PLATFORM = d.PLATFORM \
					and r.SEQUENCE = d.SEQUENCE",
					glbl_grnlTbl, 
					fa_data->pass, fa_data->platform, fa_data->station_id);
	

	ptr = NULL;

#ifdef DEBUG
	fprintf(stderr, "REAQ: %s\n", qbuf); 
#endif

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query from table for REAQ report.");
		return(NULL);
	}

	/*
	** Currently scan through entire database ...
	*/

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return(NULL);
		}	

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** First find the tape in the list if it exists.
		*/

    if (strcmp(fa_data->station_id, "MC") == 0)
				strcpy(workbuf, "MM000000");
    else
		{
		    memset(workbuf, 0, sizeof(workbuf));
		    getQueryData(msgDesc, 0, &qdata, &qlen);
		    strcpy(workbuf, "AF");
		    memcpy(workbuf + 2, qdata + 6, 6);
		}


		ptr = head;

		while ((ptr != NULL) && (strcmp(ptr->hddt_label.medium_id, workbuf)))
		{
			ptr = ptr->next;
		}

		/*
		** Check if we found a match already in our list for the
		** tape.
		*/

		if (ptr == NULL)
		{
			/*
			** Add a new link.
			*/

			num_aq = 0;
			
			if ((ptr = malloc(sizeof(HDDT_LABEL_LIST))) == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate any memory for the HDDT LABEL List.");
				return(NULL);
			}
			memset(ptr,0, sizeof(HDDT_LABEL_LIST));

			ptr->next = head;
			head = ptr;

			/*
			** populate new hddt label
			*/

			strcpy(ptr->hddt_label.medium_id, workbuf);

			if (!strcmp(fa_data->platform, IMS_FA_E1))
			{
				ptr->hddt_label.sat_id = 1;
			}
			else if (!strcmp(fa_data->platform, IMS_FA_E2))
			{
				ptr->hddt_label.sat_id = 2;
			}
			else
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not determine satellite id from '%s'", 
					fa_data->platform);
			}

			/*
			** The following are constants from ESA documentation.
			*/

		  if (strcmp(fa_data->station_id, "FA") == 0) /* R2.1 */
				ptr->hddt_label.station_id = 6;
			else
				ptr->hddt_label.station_id = 5;

			ptr->hddt_label.drive = 1;
			ptr->hddt_label.demodulator = 0;

		}

		num_aq ++;
		memcpy(ptr->hddt_label.num_acquisitions, &num_aq, 4);


		memset(workbuf, 0, sizeof(workbuf));
		getQueryData(msgDesc, 1, &qdata, &qlen);
		memcpy(workbuf, qdata, qlen);

		if (startDate[0] == '\0')
			strcpy(startDate, workbuf);

		if (strcmp(startDate, workbuf) > 0)
			strcpy(startDate, workbuf);

		if (ims_timeToNumericDate(msgDesc, workbuf, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not convert start date '%s' for REAQ report.", workbuf);
			endQuery(msgDesc);
			return(NULL);
		}

		ims_numericDateToESAI(&dateStruct, &(start.days), &(start.msec));


		memset(workbuf, 0, sizeof(workbuf));
		getQueryData(msgDesc, 2, &qdata, &qlen);
		memcpy(workbuf, qdata, qlen);

		if (endDate[0] == '\0')
			strcpy(endDate, workbuf);

		if (strcmp(endDate, workbuf) > 0)
			strcpy(endDate, workbuf);


		if (ims_timeToNumericDate(msgDesc, workbuf, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not convert end date '%s' for REAQ report.", workbuf);
			endQuery(msgDesc);
			return(NULL);
		}

		ims_numericDateToESAI(&dateStruct, &(end.days), &(end.msec));

		/*
		** Convert start/end dates to ESAI format.
		*/

		switch (num_aq)
		{
			case 1:
				memcpy((void *) ptr->hddt_label.start1, &start, 8);
				memcpy((void *) ptr->hddt_label.stop1, &end, 8);
				break;
			case 2:
				memcpy((void *) ptr->hddt_label.start2, &start, 8);
				memcpy((void *) ptr->hddt_label.stop2, &end, 8);
				break;
			case 3:
				memcpy((void *) ptr->hddt_label.start3, &start, 8);
				memcpy((void *) ptr->hddt_label.stop3, &end, 8);
				break;
			default:
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Number of acqusitions on media %s not in range 1-3", 
					workbuf);
				endQuery(msgDesc);
				return(NULL);
		}
		
	}

	if (startDate[0] != '\0')
		strcpy(fa_data->date_start, startDate);

	if (endDate[0] != '\0')
		strcpy(fa_data->date_end, endDate);

	endQuery(msgDesc);
	return(head);

}


/****************************************************************************
**
** reaq_format_records
**
** Format the Application Data Record section of the REAQ report.
**
** Need to handle records over 48000 still per ESA documentation!
****************************************************************************/

static int reaq_format_records(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr,
	FILE *fptr_swapped,
	IMS_FA_INTERFACE *fa_data) 
{
	HDDT_LABEL_LIST *label_head, *label_ptr;
	PCD_HEADER hdr;
	PCD_RECORD_LIST *pcd_list, *ptr;
	int pass = 0;
	int size = 100;
	int num_ac_recs = 0;
	int swap_num_ac_recs = 0;
	int num_tape_labels = 0;
	int swap_num_tape_labels = 0;
	int i,x;
	char *end_of_record = " "; 
	char datatake_date[23];
	IMS_NUMERIC_DATE dateStruct;
	char fullPathName[IMS_PATH_LEN+1];

	/*
	** Fixed portion of record.
	*/

	char hddr;

	struct 
	{
		IMS_ESA_UTC utc;
		int num_of_recs;
	} pcd_hdr;

	/*
	** PCD Records
	*/

	struct
	{
		char valid_flag;
		struct  
		{
			char carrier_lock;	
 			char agc_level;
			char realtime_ebit_rate;
			char playback_ebit_rate;
			char q_bit_clock_lock;
			char i_bit_clock_lock;
		} tcr;
		struct
		{
			char realtime_lock;
			char playback_lock;
		} hdr_lock_signals;
		char pcd_summary;
	} pcd_rec;

	ACQUISITION_LIST *acq_list, *old_acq, *save_list;
	IMS_FA_PATH_LIST *datasetPath;

	/*
	** Load the datset path information
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (loadDatasetPaths(msgDesc, IMS_FA_TI_E1, IMS_FA_E1,
			&datasetPath) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not load and dataset path policy information");
			return(IMS_ERROR);
		}
	}
	else
	{
		if (loadDatasetPaths(msgDesc, IMS_FA_TI_E2, IMS_FA_E2,
			&datasetPath) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not load and dataset path policy information");
			return(IMS_ERROR);
		}
	}

	/*
	** Calculate # of tape label records
	*/

	if ((label_head = reaq_bld_hddt_list(msgDesc, fa_data)) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get any tape records for REAQ report.");
		return(IMS_ERROR);
	}
	num_tape_labels = 0;
	label_ptr = label_head;
	while (label_ptr != NULL)
	{
		num_tape_labels ++;
		label_ptr = label_ptr->next;
	}

	/*
	** Calculate # of acquisitions with PCD information.
	*/

	if ((acq_list = reaq_bld_acq_list(msgDesc, fa_data, &num_ac_recs)) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_WARNING,
			"Could not get any acquisitions for the REAQ PCD information.");
		num_ac_recs = 0;
	}


	/*
	** Writeout Report Header portion of record.
	*/

	/*
	** First Sum size of report by going through all acquisitions and headers.
	*/


	size = num_ac_recs * 13 + sizeof(num_ac_recs) + 
			sizeof(num_tape_labels) + num_tape_labels * sizeof(HDDT_LABEL)
			+ 39;  /* 39 is the size of the report header. */

	save_list = acq_list;

	for (i = 0; i < num_ac_recs; i++)
	{
		/*
		** Get PCD Information
		*/

		if (locateRepositoryFile(msgDesc, datasetPath, fullPathName,
			acq_list->pcd_file_name, acq_list->format, acq_list->granule_idx,
			"D") < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get path for PCD file");
			return(IMS_ERROR);

		}

		if (read_PCD_file(msgDesc, fullPathName, &hdr, 
				&pcd_list) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not read PCD information"); 
			return(IMS_ERROR);
		}
		size += atoi(hdr.numrecs) * sizeof(pcd_rec);

		while (pcd_list != NULL)
		{
			ptr = pcd_list;
			pcd_list = pcd_list->next;
			free(ptr);
		}

		old_acq = acq_list;
		acq_list = acq_list->next;
		free(old_acq);
	}

	acq_list = save_list;

	if (esa_report_header(msgDesc, "REAQ", fa_data->report_counter, 
				fa_data->pass, 
				size, fptr, fptr_swapped) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format REAQ record.");
		return(IMS_ERROR);
	}

	/*
	** Write out # of acquisition PCD records
	*/


	if ((int) fwrite((void *) &num_ac_recs, sizeof(num_ac_recs), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report record");
		return(IMS_ERROR);
	}

	/*
	** Write out # of acquisition PCD records - Swapped
	*/

	swap_num_ac_recs = num_ac_recs;

	byte_swap_int(&swap_num_ac_recs);


	if ((int) fwrite((void *) &swap_num_ac_recs, 
			sizeof(swap_num_ac_recs), 1, fptr_swapped) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report record");
		return(IMS_ERROR);
	}



	if ((int) fwrite((void *) &num_tape_labels, sizeof(num_tape_labels), 1,
					fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report record");
		return(IMS_ERROR);
	}

	/*
	** Write out # tape labels - swapped
	*/

	swap_num_tape_labels = num_tape_labels;

	byte_swap_int(&swap_num_tape_labels);

	if ((int) fwrite((void *) &swap_num_tape_labels, 
					sizeof(swap_num_tape_labels), 1,
					fptr_swapped) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out report record");
		return(IMS_ERROR);
	}



	/*
	** Loop through the 1..n Acquisition PCD Summary 
	*/
	for (i = 0; i < num_ac_recs; i++)
	{
		/*
		** Get PCD Information
		*/

		if (locateRepositoryFile(msgDesc, datasetPath, fullPathName,
			acq_list->pcd_file_name, acq_list->format, acq_list->granule_idx,
			"D") < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get path for PCD file. PCD info skipped.");
			goto NO_PCD_INFO;

		}

		if (read_PCD_file(msgDesc, fullPathName, &hdr, 
				&pcd_list) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not read PCD information"); 
			goto NO_PCD_INFO;
		}

		/*
		** Write out PCD summary header
		*/

		hddr = hdr.hddr_id[0] - 48;


		/* 
		** Get the number of days/ mseconds  from the data take date.
		*/

		memcpy(datatake_date, &(hdr.year), sizeof(datatake_date));
		datatake_date[sizeof(datatake_date) - 1] = '\0';

		if (ims_timeToNumericDate(msgDesc, datatake_date, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not convert data take date '%s' from PCD file. PCD info skipped.",
				datatake_date);
			goto NO_PCD_INFO;
		}

		ims_numericDateToESAI(&dateStruct, &(pcd_hdr.utc.days),
			&(pcd_hdr.utc.msec));


		pcd_hdr.num_of_recs = atoi(hdr.numrecs);


		/*
		** Write the PCD Summary Record
		*/

		if ((int) fwrite((void *) &hddr,  1, 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}

		/*
		** Write the PCD Summary Record to swapped file.
		*/

		if ((int) fwrite((void *) &hddr,  1, 1, fptr_swapped) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}



		if ((int) fwrite((void *) &pcd_hdr,  sizeof(pcd_hdr), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}

		/*
		** Write the pcd_hdr swapped.
		*/

		byte_swap_int(&(pcd_hdr.num_of_recs));
		byte_swap_int(&(pcd_hdr.utc.days));
		byte_swap_int(&(pcd_hdr.utc.msec));
		
		if ((int) fwrite((void *) &pcd_hdr,  sizeof(pcd_hdr), 1, fptr_swapped) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}




		/* 
		** Write out 1..n PCD records (Max 600)
		*/

		byte_swap_int(&(pcd_hdr.num_of_recs));
		for (x = 0; x < pcd_hdr.num_of_recs; x++)
		{
			pcd_rec.valid_flag = atoi(pcd_list->rec.pcdvalf);

			pcd_rec.tcr.carrier_lock = atoi(pcd_list->rec.hrorlr);
			pcd_rec.tcr.agc_level = atoi(pcd_list->rec.agcpcd);
			pcd_rec.tcr.realtime_ebit_rate = atoi(pcd_list->rec.rtber);
			pcd_rec.tcr.playback_ebit_rate = atoi(pcd_list->rec.pbber);
			pcd_rec.tcr.q_bit_clock_lock = atoi(pcd_list->rec.qbitcl);
			pcd_rec.tcr.i_bit_clock_lock = atoi(pcd_list->rec.ibitcl);

			pcd_rec.hdr_lock_signals.realtime_lock = atoi(pcd_list->rec.rtlock);
			pcd_rec.hdr_lock_signals.playback_lock = atoi(pcd_list->rec.pblock);

			pcd_rec.pcd_summary = atoi(pcd_list->rec.pcdsum);
			
			if ((int) fwrite((void *) &pcd_rec,  sizeof(pcd_rec), 1, fptr) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, 
						"Could not write out pcd report record");
				return(IMS_ERROR);
			}

			if ((int) fwrite((void *) &pcd_rec,  sizeof(pcd_rec), 1, fptr_swapped) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, 
						"Could not write out pcd report record");
				return(IMS_ERROR);
			}
			ptr = pcd_list;
			pcd_list = pcd_list->next;
			free(ptr);
		}

NO_PCD_INFO:

		old_acq = acq_list;
		acq_list = acq_list->next;
		free(old_acq);
	}

	/* 
	** Write out HDDT Tape Label Records.
	*/
	while (label_head != NULL)
	{
		/*
		** Setup and write out HDDT Label
		*/

		if ((int) fwrite((void *) &(label_head->hddt_label), 
				sizeof(HDDT_LABEL), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}

		/*
		** Write out swapped version.
		*/

		byte_swap_int((int *) (label_head->hddt_label.num_acquisitions));
		byte_swap_int((int *) (&label_head->hddt_label.start1[0]));
		byte_swap_int((int *) (&label_head->hddt_label.stop1[0]));
		byte_swap_int((int *) (&label_head->hddt_label.start2[0]));
		byte_swap_int((int *) (&label_head->hddt_label.stop2[0]));
		byte_swap_int((int *) (&label_head->hddt_label.start3[0]));
		byte_swap_int((int *) (&label_head->hddt_label.stop3[0]));

		byte_swap_int((int *) (&label_head->hddt_label.start1[4]));
		byte_swap_int((int *) (&label_head->hddt_label.stop1[4]));
		byte_swap_int((int *) (&label_head->hddt_label.start2[4]));
		byte_swap_int((int *) (&label_head->hddt_label.stop2[4]));
		byte_swap_int((int *) (&label_head->hddt_label.start3[4]));
		byte_swap_int((int *) (&label_head->hddt_label.stop3[4]));


		if ((int) fwrite((void *) &(label_head->hddt_label), 
				sizeof(HDDT_LABEL), 1, fptr_swapped) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not write out report record");
			return(IMS_ERROR);
		}


		label_ptr = label_head;
		free(label_ptr);
		label_head = label_head->next;

	}

	(void) freeDatasetPaths(msgDesc, datasetPath);

	return(IMS_OK);
}

/****************************************************************************
**
** reaqTracking
**
** Clean the tracking table for the reports generated.
**
****************************************************************************/
 
static int reaqTracking(
	IMS_MSG_STRUCT *msgDesc,
	int pass,
	char *platform,
	char *station_id)
{
	IMS_FA_TRACKING track;
	  
	memset((char *) &track, 0, sizeof(track));
	strcpy(track.platform, platform);
	track.orbit_id = pass;
	strcpy(track.station_id, station_id);/*R2.1*/

	if (updateTracking(msgDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not update tracking table for REAQ report");
		 return(IMS_ERROR);
	}

}


/****************************************************************************
**
** ims_esa_reaq
**
** ESA Aquisition Report
****************************************************************************/

int ims_esa_reaq(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	char filename[IMS_PATH_LEN+1];
	char tempName[IMS_PATH_LEN+1];
	char fixed_header[81];
	char line[149]; /* Per ESA document */
	FILE *fptr, *fptr_swapped;
	int no_of_media = 1;
	int no_of_data_sets = 100;
	int i;
	short int record_length; 
	struct {
		char mtype[2];
		char mid[4];
	} msg_hdr;
	short int mtype;
	int mid;

    /*
	** Get Downlink Granules Table
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E1, 
				IMS_FA_E1) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-1 REAQ.");
			 return(IMS_ERROR);
		}
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E2, 
				IMS_FA_E2) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-2 REAQ.");
			 return(IMS_ERROR);
		}
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Unknown platform type found in ESA REAQ");
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id); /*R2.1*/
		return(IMS_ERROR);
	}

	fa_data->start_rev = fa_data->end_rev = fa_data->pass;


	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (format_esa_header(msgDesc, IMS_ESA_REAQ, fixed_header, 
			filename, fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id); /*R2.1*/
			return(IMS_ERROR);
		}
	}
	else
	{
		if (format_esa_header(msgDesc, IMS_ESA2_REAQ, fixed_header, 
			filename, fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id); /*R2.1*/
			return(IMS_ERROR);
		}
	}

	/*
	** Open the report file.
	*/
	ims_concatFilePath(tempName, fullPathName, filename);
	strcpy(fullPathName, tempName);
	strcat(tempName, ".unix");

	fptr = fopen(tempName, "wb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", tempName);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);/*R2.1*/
		return(IMS_ERROR);
	}

	/*
	** Save local unix version name.
	*/

	strcpy(fa_data->archive_name, tempName); 

	record_length = strlen(fixed_header);	


	fptr_swapped = fopen(fullPathName, "wb");

	if (fptr_swapped == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", fullPathName);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);/*R2.1*/
		return(IMS_ERROR);
	}

#if 0 /* Currently no end of line here */
	fixed_header[record_length] = 0xa;
	record_length++;
#endif

	if ((int) fwrite((void *) &fixed_header, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_REAQ header");
		fclose(fptr);
		fclose(fptr_swapped);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);
		return(IMS_ERROR);
	}

	if ((int) fwrite((void *) &fixed_header, record_length, 1,
			fptr_swapped) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_REAQ header");
		fclose(fptr);
		fclose(fptr_swapped);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);
		return(IMS_ERROR);
	}

	/*
	** Build/Write Variable Portion
	*/


	record_length = 6;
	mtype = 251; /* Per ESA documentation */

	mid = fa_data->report_counter;

	/*	mid = 0001;  */

	memcpy(msg_hdr.mtype, &mtype, 2);
	memcpy(msg_hdr.mid, &mid, 4);

	if ((int) fwrite((void *) &msg_hdr, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write variable portion.");
		fclose(fptr);
		fclose(fptr_swapped);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);
		return(IMS_ERROR);
	}

	/*
	** Write out swapped version.
	*/

	byte_swap_int((int *) msg_hdr.mid);
	byte_swap_word((short int *) msg_hdr.mtype);

	if ((int) fwrite((void *) &msg_hdr, record_length, 1, fptr_swapped) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write variable portion.");
		fclose(fptr);
		fclose(fptr_swapped);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);
		return(IMS_ERROR);
	}

	/*
	** Application Data Record.
	*/

	if (reaq_format_records(msgDesc, fptr, fptr_swapped, fa_data) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write application data record");
		fclose(fptr);
		fclose(fptr_swapped);
		(void) reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id);
		return(IMS_ERROR);
	}
	fclose(fptr);
	fclose(fptr_swapped);
	return(reaqTracking(msgDesc, fa_data->pass, fa_data->platform, fa_data->station_id));
}

/****************************************************************************
**
** reex_format_record
**
** Format application data record portion of the ESA Extracted Data Report.
****************************************************************************/

static int reex_format_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data) 
{

	X_UMP_ENTRY_ID x_ump_entry_id;

	struct
	{
		char orbit_number[5];
		char fac_id[2];
		char orig;
	} sched_ref;

	IMS_ESA_DATE_TIME x_date;
	IMS_NUMERIC_DATE dateStruct;
	char reserved[9];
	char orbit_num[6];
	char tmpDate[IMS_DATETIME_LEN+1];
	char raw_data;
	char *qdata;
	int qlen;
	char record_end;
	int i_orbit;

	short int record_length = 48; /* Size of extracted data record */


	/*
	** Write out X_UMP_ENTRY_ID.
	*/
	memset(&x_ump_entry_id, 0, sizeof(x_ump_entry_id));

	/*
	** Platform
	*/

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		memcpy(x_ump_entry_id.satellite,  "E1", 2);
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		memcpy(x_ump_entry_id.satellite,  "E2", 2);
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Invalid platform '%s' for ESA REEX report.", fa_data->platform);
		return(IMS_ERROR);
	}

	/*
	** Sensor
	*/

	memcpy(x_ump_entry_id.sensor, "SAR", 3);	

	/*
	** Orbit #
	** DOWNLINK MESSAGE: ORBIT ID
	*/

	getQueryData(msgDesc, 0, &qdata, &qlen);

	memcpy(&i_orbit, qdata, qlen);

	sprintf(orbit_num, "%05d", (int) i_orbit);
	memcpy(x_ump_entry_id.orbit_no, orbit_num, 5);	

	/*
	** Set start/end rev values.
	*/

	if (fa_data->start_rev < 2)
	{
		fa_data->start_rev = fa_data->end_rev = i_orbit;
	}

	if (fa_data->start_rev > i_orbit)
		fa_data->start_rev = i_orbit;

	if (fa_data->end_rev < i_orbit)
		fa_data->end_rev = i_orbit;



	/*
	** Frame Number
	** Default: 0 based on ACS reports.  Need to consider this???
	*/

	memcpy(x_ump_entry_id.frame, "0000", 4);



	/*
	** Facility Id
	*/

	memcpy(x_ump_entry_id.fac_id, "AF", 2);	


	if ((int) fwrite((void *) &x_ump_entry_id, 
				sizeof(x_ump_entry_id), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}

	/*
	** Write out schedule reference.
	*/

	/*
	** Orbit Number
	*/

	memcpy(sched_ref.orbit_number, orbit_num, 5);

	/*
	** Facility Id
	*/

	memcpy(sched_ref.fac_id, "AF", 2);

	/*
	** Originator
	*/

	sched_ref.orig = 'U';
	

	if ((int) fwrite((void *) &sched_ref, sizeof(sched_ref), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}

	/* 
	**  Add date time
	**  DOWNLINK MESSAGE: TIME DATA START
	*/


	getQueryData(msgDesc, 1, &qdata, &qlen);
	memset(tmpDate, 0, sizeof(tmpDate));
	memcpy(tmpDate, qdata, qlen);

	if (ims_timeToNumericDate(msgDesc, tmpDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not convert date '%s' for REEX report", tmpDate);
		return(IMS_ERROR);
	}

	memset(tmpDate, 0, sizeof(tmpDate));
	sprintf(tmpDate, "%04d%02d%02d%02d%02d%02d", dateStruct.year, 
		dateStruct.month,
		dateStruct.day, dateStruct.hours, dateStruct.minutes, 
		dateStruct.seconds);

	memcpy((char *) &x_date, tmpDate, 14);

	if ((int) fwrite((void *) &x_date, sizeof(x_date), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}


	/*
	** Raw data extraction
	** Currently hardcoded to be zero.
	*/

	raw_data = 'I';

	if ((int) fwrite((void *) &raw_data, 1, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}

	/*
	** Add Reserved Portion...
	*/

	memset(reserved, 32, 9);

	if ((int) fwrite((void *) &reserved, 9, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}

	/*
	** Write out record_end
	*/

	record_end = 0xa;

	if ((int) fwrite((void *) &record_end, 1, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write out record.");
		return(IMS_ERROR);
	}

	return(IMS_OK);

}

/****************************************************************************
**
** ims_esa_reex
**
** ESA Extracted Data Report
****************************************************************************/

int ims_esa_reex(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	char fixed_header[81];
	char line[149]; /* Per ESA document */
	FILE *fptr;
	int i, status;
	short int record_length; 
	char filename[IMS_PATH_LEN+1];
	char tempName[IMS_PATH_LEN+1];
	char qbuf[IMS_COL512_LEN];


    /*
	** Get Downlink Granules Table
	*/
	fa_data->start_rev = fa_data->end_rev = 1;

	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E1, 
				IMS_FA_E1) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-1 REEX.");
			 return(IMS_ERROR);
		}
	}
	else if (strcmp(fa_data->platform, IMS_FA_E2) == 0)
	{
		if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_E2, 
				IMS_FA_E2) < IMS_OK)
		{
			 (void) ims_msg(msgDesc, IMS_ERROR,
				 "Could not determine granule table name for ERS-2 REEX.");
			 return(IMS_ERROR);
		}
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Unknown platform type found in ESA REEX");
		return(IMS_ERROR);
	}


	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/


	if (strcmp(fa_data->platform, IMS_FA_E1) == 0)
	{
		if (format_esa_header(msgDesc, IMS_ESA_REEX, fixed_header, filename,
				fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			return(IMS_ERROR);
		}
	}
	else
	{
		if (format_esa_header(msgDesc, IMS_ESA2_REEX, fixed_header, filename,
				fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
			return(IMS_ERROR);
		}

	}

	/*
	** Open the report file.
	*/

	ims_concatFilePath(tempName, fullPathName, filename);
	strcpy(fullPathName, tempName);


	fptr = fopen(fullPathName, "wb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	record_length = strlen(fixed_header);	
	fixed_header[record_length] = 0xa;
	record_length++;


	if ((int) fwrite((void *) &fixed_header, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_REEX header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Add Application Data Record section.
	*/

	sprintf(qbuf, "select REVOLUTION, START_TIME\
					from %s, platforms p where '%s' <= START_TIME\
					and '%s' >= END_TIME and \
					PLATFORM = p.acronym and status = 1 and \
					MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' and p.platform = '%s' ", 
					glbl_grnlTbl, 
					fa_data->date_start, fa_data->date_end,
					fa_data->platform);
	
	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query from table for REEX report.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Currently scan through entire database ...
	*/

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			fclose(fptr);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		if (reex_format_record(msgDesc, fptr, fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not write ESA_REEX record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	endQuery(msgDesc);

	fclose(fptr);
	return(IMS_OK);

}


/****************************************************************************
**
** read_PCD_file
**
****************************************************************************/

static int read_PCD_file(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	PCD_HEADER *hdr,
	PCD_RECORD_LIST **pcd_list)
{
	FILE *fptr;
	int i;
	PCD_RECORD_LIST *ptr;

	fptr = fopen(fullPathName, "r");
	 
	if (fptr == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not open PCD file %s.", fullPathName);
		return(IMS_ERROR);
	}

	/*
	** Read Header 
	*/

	if (fread(hdr, sizeof(PCD_HEADER), 1, fptr) < (size_t) 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not read PCD header from file %s", fullPathName);
		fclose(fptr);
		return(IMS_ERROR);
	}
		
	/*
	** Read in PCD List
	*/

	for (i = 0; i < atoi(hdr->numrecs); i++)
	{
		/*
		** If this is the first element ...
		*/

		if (i == 0)
		{
			ptr = malloc(sizeof(PCD_RECORD_LIST));
			

			*pcd_list = ptr;
		}
		else
		{
			ptr->next = malloc(sizeof(PCD_RECORD_LIST));
			ptr = ptr->next;
		}

		/*
		** Check if no space was allocated.
		*/

		if (ptr == NULL)
		{ 
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not allocate node for PCD record");
			fclose(fptr);

			while (*pcd_list != NULL)
			{
				ptr = *pcd_list;
				*pcd_list = (*pcd_list)->next;
				free(ptr);
			}
			return(IMS_ERROR);
		}

		/*
		** Get the PCD Record from the file.
		*/

		if (fread((char *) &(ptr->rec), sizeof(PCD_RECORD_INFO), 
				1, fptr) < (size_t) 1)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not read PCD record from file %s", fullPathName);

			fclose(fptr);

			while (*pcd_list != NULL)
			{
				ptr = *pcd_list;
				*pcd_list = (*pcd_list)->next;
				free(ptr);
			}
			return(IMS_ERROR);
		}

		ptr->next = NULL;

	}

	fclose(fptr);
	return(IMS_OK);

}


