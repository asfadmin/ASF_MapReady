static char *sccs = "@(#)ims_fa_adeos.c	5.15  06/27/97";
/******************************************************************************
**
** File:	ims_fa_adeos.c
**
** Function: Perform processing of FA Reports for the National Space
**			 Development Agency of Japan for the ADEOS Spacecraft.
**
** Author: Dan Crichton	
**
** Date:	7/14/95
**
** Modification:  July 11, 1996 R1B' D. Crichton 
**                Change START_TIME, END_TIME to TIME_AOS, TIME_LOS everywhere
**
**                November 26, 1996 R1B' D. Crichton
**                Change TIME_AOS, TIME_LOS back to START_TIME, END_TIME
**                Called passlog calculation twice to get bit sync start/end 
**
**								3/25/97 D. Ting
**								R2.1 Changes. Changed the query from granules to downlink_entry only
**								if necessary.
**								4/25/96 D. Ting
**								Added dummy for downlink status
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
** Global variables
*/


/*
** Local Functions
*/
int determine_date_range (IMS_MSG_STRUCT *, char *, char *, IMS_FA_INTERFACE *);
static int getAcqStatus(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, int, int, char *, char *);
static int getPassInfo(IMS_MSG_STRUCT *, IMS_FA_INTERFACE *, int, 
	int, char *, char *, char *, char *, char *);
static int getConfigInfo (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *,
	char *, int , int *);


static char glbl_grnlTbl[IMS_COL30_LEN+1];


/****************************************************************************
**
** format_adeos_header
**
** Format the NASDA ADEOS header string
****************************************************************************/
int format_adeos_header(
	IMS_MSG_STRUCT *msgDesc,
	int report_id, 
	int no_of_records,
	char *format_str,
	IMS_FA_INTERFACE *fa_data,
	char *filename)
{
  	IMS_NUMERIC_DATE dateDef;
	IMS_ADEOS_HEADER header;
	int counter;
	char asc_no_recs[6];
	char date[9];  		/* YYYYMMDD */
	char time[9];
	char temp_begin_date[IMS_DATETIME_LEN+1];
	char temp_end_date[IMS_DATETIME_LEN+1];


	char begin_date[9]; /* YYYYMMDD */
	char end_date[9];   /* YYYYMMDD */
	int i;


	memset((char *) &header, 32, sizeof(header));

	/*
	** Get counter 
	*/
	if (getReportCounter(msgDesc, report_id, &counter) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get report counter.");
		return(IMS_ERROR);
	}

	/*
	** Update counter value.
	*/

	if (updateReportCounter(msgDesc, report_id, counter + 1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not update report counter.");
		return(IMS_ERROR);
	}

	fa_data->report_counter = counter;

	/*
	** Get current date...
	*/

  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	sprintf(date, "%04d%02d%02d", dateDef.year, dateDef.month, dateDef.day);
	sprintf(time, "%02d:%02d:%02d", dateDef.hours, dateDef.minutes, 
				dateDef.seconds);


	memcpy(header.gen_date, date, sizeof(header.gen_date)); 
	memcpy(header.gen_time, time, sizeof(header.gen_time)); 

	switch (report_id)
	{
		case IMS_ADEOS_REAC:
			sprintf(filename, "REAC%06d", counter);
			memcpy(header.filename, filename, sizeof(header.filename));
			memcpy(header.project, "ADEOS ", sizeof(header.project));
			memcpy(header.send_grnd_code, "ASF ", 4);
			memcpy(header.rec_grnd_code, "HMMO", 4);
			memcpy(header.rec_len, " 145", 4);
			
			/*
			** Setup Begin and End dates
			*/
			
			if (ims_timeToNumericDate(msgDesc, fa_data->date_start,
					&dateDef) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not extract start date for REAC header.");
				return(IMS_ERROR);
			}

			sprintf(date, "%04d%02d%02d", dateDef.year, dateDef.month, 
					dateDef.day);

			memcpy(header.begin_date, date, 8);

			if (ims_timeToNumericDate(msgDesc, fa_data->date_end,
					&dateDef) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not extract end date for REAC header.");
				return(IMS_ERROR);
			}

			sprintf(date, "%04d%02d%02d", dateDef.year, dateDef.month, 
				dateDef.day);

			memcpy(header.end_date, date, 8);
			break;


		case IMS_ADEOS_SRRD:
			sprintf(filename, "SRRD%06d", counter);
			memcpy(header.filename, filename, sizeof(header.filename));
			memcpy(header.project, "ADEOS ", sizeof(header.project));
			memcpy(header.send_grnd_code, "ASF ", 4);
			memcpy(header.rec_grnd_code, "HMMO", 4);
			memcpy(header.rec_len, "0000", 4);


			/*
			** Check if there is a date range for the report
			*/

			memset(temp_begin_date, 0, sizeof(temp_begin_date));
			memset(temp_end_date, 0, sizeof(temp_end_date));

			if (determine_date_range (msgDesc, temp_begin_date, temp_end_date, 
					fa_data) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Error determining SRRD Tape Date Range");
				return(IMS_ERROR);
			}

			if (ims_timeToNumericDate(msgDesc, temp_begin_date,
					&dateDef) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not extract start date for SRRD header.");
				return(IMS_ERROR);
			}

			sprintf(begin_date, "%04d%02d%02d", dateDef.year, dateDef.month, 
					dateDef.day);

			if (ims_timeToNumericDate(msgDesc, temp_end_date,
					&dateDef) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not extract end date for SRRD header.");
				return(IMS_ERROR);
			}

			sprintf(end_date, "%04d%02d%02d", dateDef.year, dateDef.month, 
					dateDef.day);


			/*
			** Add begin/end dates
			*/
			memcpy(header.begin_date, begin_date, 8);
			memcpy(header.end_date, end_date, 8);
			break;				

	}

	header.record_end = 0x0A;

	/*
	** Version information from NASDA document AFD-MO-E03
	*/

	memcpy(header.file_fmt_date, "19950601", 8);
	memcpy(header.file_ver_date, "V01", 3);

	sprintf(asc_no_recs, "%5d", no_of_records);
	memcpy(header.no_of_records, asc_no_recs, sizeof(header.no_of_records));
	memcpy(format_str, (char *) &header, sizeof(header));
	return(IMS_OK);
}

/****************************************************************************
**
** format_adeos_reac
**
** Format the ADEOS REAC record.
****************************************************************************/
int format_adeos_reac(
	IMS_MSG_STRUCT *msgDesc,
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data)
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

	char *tPtr;
	int len;
	int status;
	int tempFreq; 
	int tempInt;
	char workbuf[64];
	char tempbuf[64];
	char tempDate[IMS_DATETIME_LEN + 1];
	char startDate[IMS_DATETIME_LEN+1];
	char endDate[IMS_DATETIME_LEN+1];
	char timeOn[IMS_DATETIME_LEN+1];
	char timeOff[IMS_DATETIME_LEN+1];
	char bitStart[IMS_DATETIME_LEN+1];
	char bitStop[IMS_DATETIME_LEN+1];
	char antenna_id[IMS_COL30_LEN+1];
	char trans[IMS_COL30_LEN+1];
	IMS_NUMERIC_DATE dateStruct;
	double angle1, angle2;
	long path;
	int orbit;
	short int sequence;
	int band;

	memset(&reac, 32, sizeof(reac));



	/*
	** Tape_Num
	** Downlink Message: MEDIA_ID
	*/

	(void) getQueryData(msgDesc, 1, &tPtr, &len);
	memset(tempbuf, 0, sizeof(tempbuf));
	memset(workbuf, 0, sizeof(workbuf));
	memcpy(tempbuf, tPtr, len);

	memcpy(workbuf, tPtr, len);
	memcpy(reac.tape_num, workbuf, strlen(workbuf));


	/*
	** Begin_Tape
	** Downlink Message: START_ADDRESS
	*/

	(void) getQueryData(msgDesc, 2, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	sprintf(workbuf, "%6d", (int) tempInt);
	memcpy(reac.begin_tape, workbuf, 6);

	/*
	** End_Tape
	** Downlink Message: STOP_ADDRESS
	*/

	(void) getQueryData(msgDesc, 3, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	sprintf(workbuf, "%6d", (int) tempInt);
	memcpy(reac.end_tape, workbuf, 6);

	/* 
	** Begin_Rec_Date
	** Downlink Message: START_TIME
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);
	memcpy(tempDate, tPtr, len);
	tempDate[len] = '\0';

	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_START field.");
		return(IMS_ERROR);
	}

	sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
		dateStruct.msecs);

	memcpy(reac.begin_rec_date, tempDate, strlen(tempDate));

	/*
	** Date
	** Downlink Message: END_TIME
	** Need to convert IMS to structure and create the 
	** YYMMDD string.
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);
	memcpy(tempDate, tPtr, len);
	tempDate[len] = '\0';


	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_START field.");
		return(IMS_ERROR);
	}

	strcpy(startDate, tempDate);


	sprintf(tempDate, "%04d%02d%02d", dateStruct.year, dateStruct.month, 
			dateStruct.day);

	memcpy(reac.date, tempDate, 8);


	/*
	** End_Rec_date
	** Downlink Message: TIME_LOS
	*/

	(void) getQueryData(msgDesc, 5, &tPtr, &len);
	memcpy(tempDate, tPtr, len);
	tempDate[len] = '\0';

	strcpy(endDate, tempDate);

	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_STOP field.");
		return(IMS_ERROR);
	}

	sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
		dateStruct.msecs);

	memcpy(reac.end_rec_date, tempDate, strlen(tempDate));

	/*
	** Path_Num
	** Downlink Message: REVOLUTION
	*/

	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	orbit = tempInt;

	if (fa_data->end_rev < orbit)
		fa_data->end_rev = orbit;

	if ((fa_data->start_rev == 1) || (fa_data->start_rev > orbit))
		fa_data->start_rev = orbit;

	/*
	** Need to convert revolution to path number.
	*/

	if (ims_j1rt2rsp(msgDesc, &(fa_data->jobSpec), tempInt, "A1",
			startDate, &path, &angle1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not calculate path number information.");
		return(IMS_ERROR);
	}
									 

	sprintf(workbuf, "%5d", (int) path);
	memcpy(reac.path_num, workbuf, 5);



	/*
	** Plan Number
	** Downlink Message: FA_SCHEDULE_LINK
	*/

	(void) getQueryData(msgDesc, 7, &tPtr, &len);

	if (len > 0)
		memcpy(reac.plan_num, tPtr, len);
	else
		memset(reac.plan_num, '*', sizeof(reac.plan_num));





	/*
	** x_band frequency
	** X1: 8.15 GHz  X2: 8.35 GHz  X3: 8.25 GHz
	**
	** Need to query seperate table for this value.
	*/ 

	(void) getQueryData(msgDesc, 8, &tPtr, &len);

	/*
	** Convert frequency to correct X-Band of transmission
	*/

	if (memcmp(tPtr, "ADEOS-1_8150", 12) == 0) 
	{
		memcpy(reac.x_band, "X1", 2);
		band = 0;
	}
	else if (memcmp(tPtr, "ADEOS-1_8350", 12) == 0)
	{
		band = 1;
		memcpy(reac.x_band, "X2", 2);
	}
	else if (memcmp(tPtr, "ADEOS-1_8250", 12) == 0)
	{
		band = 2;
		memcpy(reac.x_band, "X3", 2);
	}
	else
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unknown transmission frequency specified in downlink message");

	memset(trans, 0, sizeof(trans));
	memcpy(trans, tPtr, len);

	/*
	** Acq_Status, Bit_Sync_Start, Bit_Sync_Stop, Antenna_ID
	*/

	(void) getQueryData(msgDesc, 6, &tPtr, &len);
	memcpy(&sequence, tPtr, len);

	(void) getQueryData(msgDesc, 9, &tPtr, &len);
	memcpy(timeOn, tPtr, len);
	timeOn[len] = '\0';

	(void) getQueryData(msgDesc, 10, &tPtr, &len);
	memcpy(timeOff, tPtr, len);
	timeOff[len] = '\0';

	(void) getQueryData(msgDesc, 11, &tPtr, &len);
	memcpy(antenna_id, tPtr, len);
	antenna_id[len] = '\0';

	/*
	** 11 M Antenna should look for the pass-log file.
	*/

	if (strcmp(antenna_id, "ANTENNA_2") == 0)
	{
		if (getPassInfo(msgDesc, fa_data, orbit,  band, timeOn, timeOff,
			(char *) &(reac.acq_status), bitStart, bitStop) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not query the pass log file.");
			return(IMS_ERROR);
		}
	}
	else
	/*
	** 10 M Antenna should look for the ADEOS Data Quality File.
	*/

	{
		if (getAcqStatus(msgDesc, fa_data, orbit, sequence, trans,
					&(reac.acq_status)) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not set reception quality flag");
			return(IMS_ERROR);
		}

		bitStart[0] = '\0';
		bitStop[0] = '\0';

	}

	/*
	** Bit_Sync_Start
	*/

	if ((bitStart[0] == '\0') && (reac.acq_status!='N'))
		strcpy(bitStart, timeOn);


	if (bitStart[0] != '\0')
	{
		if (ims_timeToNumericDate(msgDesc, bitStart, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not extract year from BIT_SYNC_START.");
			return(IMS_ERROR);
		}

		sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
			dateStruct.year, dateStruct.month, dateStruct.day,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
			dateStruct.msecs);

		memcpy(reac.bit_sync_start, tempDate, strlen(tempDate));
	}
	else
	{
		memset(reac.bit_sync_start, '*', 21);
	}


	/*
	** Bit_Sync_End
	*/

	if ((bitStop[0] == '\0') && (reac.acq_status!='N'))
		strcpy(bitStop, timeOff);

	if (bitStop[0] != '\0')
	{
		if (ims_timeToNumericDate(msgDesc, bitStop, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not extract year from BIT SYNC STOP.");
			return(IMS_ERROR);
		}

		sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
			dateStruct.year, dateStruct.month, dateStruct.day,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
			dateStruct.msecs);

		memcpy(reac.bit_sync_end, tempDate, strlen(tempDate));
	}
	else
	{

		memset(reac.bit_sync_end, '*', 21);
	}

	reac.record_end = 0xa;

	if ((int) fwrite((char *) &reac, sizeof(reac), 1, fptr) < 1) 
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ADEOS REAC Record.");
		return(IMS_ERROR);
	}

	return(IMS_OK);
}

/****************************************************************************
**
** reacTracking
**
** Clean the tracking table for the reports generated.
**
****************************************************************************/

static int reacTracking(
	IMS_MSG_STRUCT *msgDesc,
	char *start_time,
	char *end_time)
{
	IMS_FA_TRACKING track;

	memset((char *) &track, 0, sizeof(track));

	/*
	** Remove all previous tracked items if they exist.
	** This is a preventitve step.
	*/

	strcpy(track.start_time, start_time);
	strcpy(track.end_time, end_time);
	strcpy(track.platform, "A1");

	if (updateTracking(msgDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not update tracking table for ADEOS REAC report");
		return(IMS_ERROR);
	}

}


/****************************************************************************
**
** ims_adeos_reac
**
** ADEOS Aquisition Report
****************************************************************************/

int ims_adeos_reac(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data)
{
	char fixed_header[128];
	FILE *fptr;
	int no_of_recs = 2;
	int i;
	int status;
	char qbuf[1024];
	char filename[IMS_PATH_LEN];
	char tempname[IMS_PATH_LEN];
	char *start_time, *end_time;

	if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_A1, 
			IMS_FA_A1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not determine granule table name for ADEOS REAC.");
		return(IMS_ERROR);
	}


	fa_data->start_rev = fa_data->end_rev = 1;
	start_time = fa_data->date_start;
	end_time = fa_data->date_end;

  /* query change for R2.1 */

	sprintf(qbuf,
		"select r.REVOLUTION, d.MEDIA_ID_ALIAS, d.START_ADDRESS, \
		d.STOP_ADDRESS, d.START_TIME, d.END_TIME, \
		r.SEQUENCE, r.FA_SCHEDULE_LINK, r.TRANSMITTER_ID, r.TIME_ON, r.TIME_OFF, r.ANTENNA_ID \
		from %s d, downlink_entry r where d.START_TIME >= '%s' and \
		d.START_TIME <= '%s' and r.PLATFORM = 'A1' and \
		d.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' and d.status = 1 and \
		r.REVOLUTION = d.REVOLUTION and \
		r.PLATFORM = d.PLATFORM and \
		r.SENSOR = d.SENSOR and \
		r.SEQUENCE = d.SEQUENCE \
		order by d.START_TIME", 
		glbl_grnlTbl, start_time, end_time); /* Changed for R2.1 */

	/*
	** Get a count of the number of records for the report.
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup query for REAC report.");
		return(IMS_ERROR);
	}

	no_of_recs = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR, 
				"Could not perform query to count REAC records");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		no_of_recs ++ ;
	}

	(void) endQuery(msgDesc);

	/*
	** Build Fixed Portion
	*/

	if (format_adeos_header(msgDesc, IMS_ADEOS_REAC, no_of_recs, 
							fixed_header, fa_data, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}

	/*
	** Open the report file.
	*/
	ims_concatFilePath(tempname, fullPathName, filename);
	strcpy(fullPathName, tempname);

	fptr = fopen(fullPathName, "wb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fwrite((void *) &fixed_header, sizeof(IMS_ADEOS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write CATA header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build and process 1..n REAC Records
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup query for REAC report.");
		return(IMS_ERROR);
	}

	for (i = 0; i < no_of_recs; i++)
	{

		if (performQuery(msgDesc) < IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR, "Could not query for ADEOS REAC Record.");
			fclose(fptr);
			return(IMS_ERROR);
		}

		if (format_adeos_reac(msgDesc, fptr, fa_data) < IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR, "Could not format ADEOS REAC Record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	/*
	** Allow Sybase to finish out the query.
	*/

	(void) performQuery(msgDesc);

	(void) endQuery(msgDesc);

	fclose(fptr);

	/*
	** Update the tracking table
	*/

	return(reacTracking(msgDesc, start_time, end_time));
}

/****************************************************************************
** determine_date_range
**
** Determine the SRRD date range (Begin/End) for the acquisitions.
**
**
****************************************************************************/
int determine_date_range (
	IMS_MSG_STRUCT *msgDesc,
	char *begin_date,
	char *end_date,
	IMS_FA_INTERFACE *fa_data)
{
	char qbuf[1024];
	char tape_buf[1024];
	char workbuf[128];
	char tape_granule[IMS_COL30_LEN+1];
	int i;
	int count;
	int status;
	char *tPtr;
	int len;

	/*
	** Get the Tape Available Granule Table Name.
	*/

	if (getGranuleTableName(msgDesc, tape_granule, IMS_FA_TA_A1, 
			"HC") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not determine granule table name for %s.", IMS_FA_TA_A1);
		return(IMS_ERROR);
	}


	count = 0;
	memset(tape_buf, 0, sizeof(tape_buf));
	for (i = 0; i < fa_data->tape_count; i++)
	{
		memset(workbuf, 0, sizeof(workbuf));
		if (i + 1 < fa_data->tape_count)
		{
			sprintf(workbuf, "(d.MEDIA_ID = '%s' AND t.MEDIA_ID = '%s') OR ", 
				fa_data->tape_num[i],
				fa_data->tape_num[i]);
		}
		else 
			sprintf(workbuf, "(d.MEDIA_ID = '%s' AND t.MEDIA_ID = '%s') ", 
				fa_data->tape_num[i],
				fa_data->tape_num[i]);
		strcat(tape_buf, workbuf);
	}

	sprintf(qbuf, "select START_TIME, END_TIME from  \
		%s d, %s t where PLATFORM = 'A1' and (%s) and d.status = 1", 
		glbl_grnlTbl, tape_granule,  tape_buf);

	
	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup query for SRRD report.");
		return(IMS_ERROR);
	}
	
	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) endQuery(msgDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not perform query to count SRRD records");
			/*fprintf(stderr, "Query:%s\n", qbuf);*/
			return(IMS_ERROR);
		}
		if (status == IMS_ENDOFQUERY)
			continue;
	
		/*
		** START_TIME
		*/


		(void) getQueryData(msgDesc, 0, &tPtr, &len);

		if (count == 0)
		{
			memset(begin_date, 0, len + 1);
			memcpy(begin_date, tPtr, len);
		}	
		else
		{
			if (strncmp(begin_date, tPtr, len) > 0)
			{
				memset(begin_date, 0, len + 1);
				memcpy(begin_date, tPtr, len);
			}

		}


		/*
		** TIME_LOS
		*/

		(void) getQueryData(msgDesc, 1, &tPtr, &len);
		if (count == 0)
		{
			memset(end_date, 0, len + 1);
			memcpy(end_date, tPtr, len);
		}	
		else
		{
			if (strncmp(end_date, tPtr, len) < 0)
			{
				memset(end_date, 0, len + 1);
				memcpy(end_date, tPtr, len);
			}
		}

		count++;
	}

	(void) endQuery(msgDesc);

	if (count == 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"No Tape Records found for report");
		return(IMS_ERROR);
	}

	return(IMS_OK);

}





/****************************************************************************
**
** format_srrd_desc
**
** Format SRRD Descriptor and return a list which contains the descriptor
** information for future use.
**
** This descriptor has the format:
**     Number of Tapes
**     Record Number 1
**     Number of Pass Records for Record 1
**     Record Number 2
**     Number of Pass Records for Record 2
**     ...
****************************************************************************/

int format_srrd_desc(
	IMS_MSG_STRUCT *msgDesc, 
	int *total_recs,
	IMS_SRRD_DESC_LIST **head,
	IMS_FA_INTERFACE *fa_data,
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
	char qbuf[IMS_COL512_LEN];
	int pass_count;
	int status;
	int old_pass_cnt = 0;
	int rec_cnt = 2;  /* 2 is descriptor record */

	*head = NULL;

	/*
	** Build a list of record number, and number of pass info records.
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
		memset((char *) rec_info, 32, sizeof(IMS_SRRD_REC_INFO));

		rec_cnt = rec_cnt + old_pass_cnt + 1;
		sprintf(rec_num,"%5d", rec_cnt);

		/*
		** Do a query for the number of pass_info records.
		*/

/* Change for R2.1 */
		sprintf(qbuf, 
			"select r.received_time from %s d, downlink_entry r where \
				d.MEDIA_ID = '%s' and r.PLATFORM = 'A1' and d.status = 1 \
				and d.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' \
		    and r.REVOLUTION = d.REVOLUTION \
		    and r.PLATFORM = d.PLATFORM \
		    and r.SENSOR = d.SENSOR \
		    and r.SEQUENCE = d.SEQUENCE",
				glbl_grnlTbl, fa_data->tape_num[i]);

		if (setupQuery(msgDesc, qbuf) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query for SRRD report.");
			return(IMS_ERROR);
		}

		pass_count = 0;
	
		while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				(void) endQuery(msgDesc);
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not perform query to count SRRD records");
			}
			if (status == IMS_ENDOFQUERY)
				continue;
		
			pass_count ++ ;
		}

		(void) endQuery(msgDesc);

		sprintf(pass_info,"%4d", pass_count);


		/*
		** Update structure with record number and pass records.
		*/

		memcpy(rec_info->rec_num, rec_num, 5);
		memcpy(rec_info->pass_num, pass_info, 4);
		ptr->next = NULL;
		old_pass_cnt = pass_count;

	}

	/*
	** Write out records count.
	*/

	sprintf(asc_total_recs, "%4d", *total_recs);

	/*
	** Write out total number of tape records
	*/

	if ((int) fwrite((void *) asc_total_recs, 4, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write SRRD Descriptor");
		return(IMS_ERROR);
	}

	ptr = *head;

	/*
	** Scan through list and write out rec_info records.
	*/

	for (i = 0; i < *total_recs; i++)
	{
		rec_info = (void *) &(ptr->rec_info);

		if ((int) fwrite((void *) rec_info, sizeof(IMS_SRRD_REC_INFO), 
					1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not write SRRD Descriptor");
			return(IMS_ERROR);
		}

		ptr = ptr->next;
	}

	/*
	** Write end delimiter
	*/

	if ((int) fwrite((void *) &end_rec, 1, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write SRRD Descriptor");
		return(IMS_ERROR);
	}

	return(IMS_OK);
}

/****************************************************************************
**
** format_srrd_pass_info
**
** Format pass information record for ADEOS SRRD Report
****************************************************************************/

int format_srrd_pass_info(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data)
{

	struct
	{
		char aq_date[8];
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
		char begin_rec_date[17];
		char blank8;
		char end_rec_date[17];
		char blank9;
		char bit_sync_start[17];
		char blank10;
		char bit_sync_stop[17];
		char blank11;
		char acq_status;
		char record_end;
	} pass_record;

	char *tPtr;
	int len;
	int status;
	int tempFreq; 
	int tempInt;
	char workbuf[64];
	char tempDate[IMS_DATETIME_LEN + 1];
	IMS_NUMERIC_DATE dateStruct;
	char startDate[IMS_DATETIME_LEN+1];
	char endDate[IMS_DATETIME_LEN+1];
	char bitStart[IMS_DATETIME_LEN+1];
	char bitStop[IMS_DATETIME_LEN+1];
	char timeOn[IMS_DATETIME_LEN+1];
	char timeOff[IMS_DATETIME_LEN+1];
	char antenna_id[IMS_COL30_LEN+1];
	char trans[IMS_COL30_LEN+1];
	long path;
	double angle1, angle2;
	int orbit;
	short int sequence;
	int band;

	memset(&pass_record, 32, sizeof(pass_record));



	/*
	** Tape_Num
	** Downlink Message: MEDIA_ID_ALIAS
	*/

	(void) getQueryData(msgDesc, 1, &tPtr, &len);
	memcpy(pass_record.tape_num, tPtr, len);

	/*
	** Begin_Tape
	** Downlink Message: START_ADDRESS
	*/

	(void) getQueryData(msgDesc, 2, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	sprintf(workbuf, "%6d", (int) tempInt);
	memcpy(pass_record.begin_tape, workbuf, 6);

	/*
	** End_Tape
	** Downlink Message: STOP_ADDRESS
	*/

	(void) getQueryData(msgDesc, 3, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	sprintf(workbuf, "%6d", (int) tempInt);
	memcpy(pass_record.end_tape, workbuf, 6);

	/* 
	** Begin_Rec_Date
	** Downlink Message: START_TIME
	** Truncate off miliseconds which is why 17 is used.
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);

	memcpy(tempDate, tPtr, len);

	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_START field.");
		return(IMS_ERROR);
	}

	sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
		dateStruct.msecs);

	memcpy(pass_record.begin_rec_date, tempDate, 17);

	/*
	** Date
	** Downlink Message: END_TIME
	** Need to convert IMS to structure and create the 
	** YYMMDD string.
	*/

	memcpy(tempDate, tPtr, len);
	tempDate[len] = '\0';

	strcpy(startDate, tempDate);

	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_START field.");
		return(IMS_ERROR);
	}

	sprintf(tempDate, "%04d%02d%02d", dateStruct.year, dateStruct.month,
		dateStruct.day);

	memcpy(pass_record.aq_date, tempDate, 8);

	/*
	** End_Rec_date
	** Downlink Message: TIME_LOS
	** Truncate off miliseconds which is why 17 is used.
	*/

	(void) getQueryData(msgDesc, 5, &tPtr, &len);
	memcpy(tempDate, tPtr, len);

	strcpy(endDate, tempDate);

	if (ims_timeToNumericDate(msgDesc, tempDate, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_DATA_STOP field.");
		return(IMS_ERROR);
	}

	sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
		dateStruct.msecs);

	memcpy(pass_record.end_rec_date, tempDate, 17);

	/*
	** Path_Num
	** Downlink Message: REVOLUTION
	*/

	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);

	/*
	** Need to convert revolution to path number.
	*/

	orbit = tempInt;

	if (ims_j1rt2rsp(msgDesc, &(fa_data->jobSpec), tempInt, "A1",
			startDate, &path, &angle1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not calculate path number information.");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%5d", (int) path);
	memcpy(pass_record.path_num, workbuf, 5);



	/*
	** Plan Number
	** Downlink Message: FA_SCHEDULE_LINK
	*/

	(void) getQueryData(msgDesc, 7, &tPtr, &len);

	if (len > 0)
		memcpy(pass_record.plan_num, tPtr, len);
	else
		memset(pass_record.plan_num, '*', sizeof(pass_record.plan_num));


	/*
	** x_band frequency
	** X1: 8.15 GHz  X2: 8.35 GHz  X3: 8.25 GHz
	**
	** Need to query seperate table for this value.
	*/ 

	(void) getQueryData(msgDesc, 8, &tPtr, &len);

	/*
	** Convert frequency to correct X-Band of transmission
	*/

	if (memcmp(tPtr, "ADEOS-1_8150", 12) == 0) 
	{
		memcpy(pass_record.x_band, "X1", 2);
		band = 0;
	}

	else if (memcmp(tPtr, "ADEOS-1_8350", 12) == 0)
	{
		memcpy(pass_record.x_band, "X2", 2);
		band = 1;
	}		

	else if (memcmp(tPtr, "ADEOS-1_8250", 12) == 0)
	{
		memcpy(pass_record.x_band, "X3", 2);
		band = 2;
	}

	else
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unknown transmission frequency specified in downlink message");

	memset(trans, 0, sizeof(trans));
	memcpy(trans, tPtr, len);

	/*
	** Acq_Status, Bit_Sync_Start, Bit_Sync_Stop
	** Calculated from Pass Log
	*/

	(void) getQueryData(msgDesc, 6, &tPtr, &len);
	memcpy(&sequence, tPtr, len);


	(void) getQueryData(msgDesc, 9, &tPtr, &len);
	memcpy(timeOn, tPtr, len);
	timeOn[len] = 0;

	(void) getQueryData(msgDesc, 10, &tPtr, &len);
	memcpy(timeOff, tPtr, len);
	timeOff[len] = 0;

	(void) getQueryData(msgDesc, 11, &tPtr, &len);
	memcpy(antenna_id, tPtr, len);
	antenna_id[len] = '\0';

	/*
	** 11 M Antenna should look for the pass-log file.
	*/

	if (strcmp(antenna_id, "ANTENNA_2") == 0)
	{
		if (getPassInfo(msgDesc, fa_data, orbit,  band, timeOn, timeOff,
			(char *) &(pass_record.acq_status), bitStart, bitStop) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not query the pass log file.");
			return(IMS_ERROR);
		}
	}
	else
	/*
	** 10 M Antenna should look for the ADEOS Data Quality File.
	*/

	{
		if (getAcqStatus(msgDesc, fa_data, orbit, sequence, trans,
					&(pass_record.acq_status)) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not set reception quality flag");
			return(IMS_ERROR);
		}

		bitStart[0] = '\0';
		bitStop[0] = '\0';

	}


	/*
	** Bit_Sync_Start
	*/

	if ((bitStart[0] == '\0') && (pass_record.acq_status!='N'))
		strcpy(bitStart, timeOn);

	if (bitStart[0] != '\0')
	{
		if (ims_timeToNumericDate(msgDesc, bitStart, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not extract year from BIT SYNC START");
			return(IMS_ERROR);
		}

		sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
			dateStruct.year, dateStruct.month, dateStruct.day,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
			dateStruct.msecs);

		memcpy(pass_record.bit_sync_start, tempDate, 17);
	}
	else
	{
		/*
		** This path not followed per ASF request.
		*/

		memset(pass_record.bit_sync_start, '*', 17);
	}


	/*
	** Bit_Sync_Stop
	*/

	if ((bitStop[0] == '\0') && (pass_record.acq_status!='N'))
		strcpy(bitStop, timeOff);

	if (bitStop[0] != '\0')
	{
		if (ims_timeToNumericDate(msgDesc, bitStop, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not extract year from BIT_SYNC_STOP");
			return(IMS_ERROR);
		}

		sprintf(tempDate, "%04d%02d%02d %02d:%02d:%02d.%03d", 
			dateStruct.year, dateStruct.month, dateStruct.day,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds, 
			dateStruct.msecs);

		memcpy(pass_record.bit_sync_stop, tempDate, 17);
	}
	else
	{
		/*
		** This path not followed per ASF request.
		*/

		memset(pass_record.bit_sync_stop, '*', 17);
	}


	pass_record.record_end = 0xa;

	if ((int) fwrite((char *) &pass_record, sizeof(pass_record), 1, fptr) < 1) 
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write ADEOS REAC Record.");
		return(IMS_ERROR);
	}

	return(IMS_OK);

}

/****************************************************************************
**
** format_adeos_srrd
**
** Format SRRD Tape Record.
****************************************************************************/

int format_adeos_srrd(
	IMS_MSG_STRUCT *msgDesc, 
	char *rec_num,
	char *pass_info,	
	char *tape_num,
	char *ship_date,
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data) 
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

	int i,status;
	char qbuf[IMS_COL512_LEN];
	int pass_count = 0;
	char workbuf[64];
	char *tPtr;
	int len;


	memset((char *) &tape_info, 32, sizeof(tape_info));
	tape_info.record_end = 0xa;

	sprintf(workbuf, "%5d", atoi(pass_info));
	memcpy(tape_info.num_passes, workbuf, 5);
	memcpy(tape_info.date, ship_date, sizeof(tape_info.date));


	/*
	** Build and write-out 1..n pass information records
	*/

  /* query change for R2.1 */

	sprintf(qbuf,
		"select r.REVOLUTION, d.MEDIA_ID_ALIAS, d.START_ADDRESS, \
		d.STOP_ADDRESS, d.START_TIME, d.END_TIME, \
		r.SEQUENCE, r.FA_SCHEDULE_LINK, r.TRANSMITTER_ID, r.TIME_ON, r.TIME_OFF, r.ANTENNA_ID \
		from %s d, downlink_entry r where d.MEDIA_ID = '%s' and r.PLATFORM = 'A1' and \
		d.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' and d.status = 1 \
		and r.REVOLUTION = d.REVOLUTION \
		and r.PLATFORM = d.PLATFORM \
		and r.SENSOR = d.SENSOR \
		and r.SEQUENCE = d.SEQUENCE \
		order by \
		d.START_TIME", 
		glbl_grnlTbl, tape_num);

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup query for SRRD report.");
		return(IMS_ERROR);
	}
	

	for (i = 0; i < atoi(pass_info); i++)
	{

		status = performQuery(msgDesc);

		if (status != IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not query granules table for SRRD information");
			return(IMS_ERROR);
		}

		/*
		** If this is the first record, then write out the header
		** now that we know what the Flight Agencies Tape Id is.
		*/

		if (i == 0)
		{

			(void) getQueryData(msgDesc, 1, &tPtr, &len);

			memcpy(tape_info.tape_num, tPtr, sizeof(tape_info.tape_num));


			/*
			** Write out tape record.
			*/

			if ((int) fwrite((char *) &tape_info, 
					sizeof(tape_info), 1, fptr) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, "Could not write out tape record");
				return(IMS_ERROR);
			}
		}


		if (format_srrd_pass_info(msgDesc, fptr, fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, 
						"Could not write out SRRD Pass Record");
			return(IMS_ERROR);
		}
	}

	/*
	** Allow Sybase to finish out query.
	*/

	(void) performQuery(msgDesc);

	(void) endQuery(msgDesc);

	return(IMS_OK);

}

/****************************************************************************
**
** ims_adeos_srrd
**
** ADEOS Shipment Report
****************************************************************************/

int ims_adeos_srrd(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data
	) 
{
	char fixed_header[128];
	char filename[IMS_PATH_LEN];
	char tempname[IMS_PATH_LEN];
	FILE *fptr;
	IMS_SRRD_DESC_LIST *head, *ptr;
	int total_recs;
	int i;


	/*
	** Determine granule table for downlink message.
	*/

	if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_A1, 
			IMS_FA_A1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not determine granule table name for ADEOS REAC.");
		return(IMS_ERROR);
	}

	/*
	** Build Fixed Portion
	*/

	if (format_adeos_header(msgDesc, IMS_ADEOS_SRRD, fa_data->tape_count, 
							fixed_header, fa_data, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}

	/*
	** Open the report file.
	*/

	ims_concatFilePath(tempname, fullPathName, filename);
	strcpy(fullPathName, tempname);

	fptr = fopen(fullPathName, "wb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fwrite((void *) &fixed_header, sizeof(IMS_ADEOS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write CATA header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build the SRRD Descriptor Record.
	*/

	total_recs = fa_data->tape_count;

	if (format_srrd_desc(msgDesc, &total_recs, &head, 
			fa_data, fptr) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format ADEOS REAC Record.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build and process 1..n SRRD Records
	*/

	ptr = head;


	for (i = 0; i < total_recs; i++)
	{
		/*
		** Get next tape number
		*/

		if (format_adeos_srrd(msgDesc, ptr->rec_info.rec_num, 
					ptr->rec_info.pass_num,
					fa_data->tape_num[i],
					fa_data->tape_date[i],
					fptr, fa_data) < IMS_OK)
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

/****************************************************************************
**
** getAcqStatus
**
** Get Acquisition Status
** 
****************************************************************************/

static int getAcqStatus(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_INTERFACE *fa_data,
	int rev,
	int sequence,
	char *trans,
	char *qual_code)
{
	FILE *fptr;
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN + 1];
	IMS_JOB_USER_SPEC *userSpec;
	char granuleTable[IMS_COL30_LEN+1];
	char fullPathName[IMS_PATH_LEN+1];
	float pqual;
	int quality;
	int status;
	int granule_idx;
	char name[IMS_COL30_LEN+1];
	char format[IMS_COL10_LEN+1];
	IMS_FA_PATH_LIST *datasetPath;

	struct adeos_header
	{
		char common[50];    /* Common Header */
	} adeos_header;

	struct data_quality
	{
		char datatakeid[14];
		char trans_id[3];
		char total_secs[4];
		char loss_secs[4];
	} data_quality;

	
	*qual_code = 'N';

	/*
	** Get connection to SQL server.
	*/

	if((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ*)NULL)
	{
		(void)ims_msg(msgDesc, IMS_FATAL, "Could not alloc a query decriptor.");
		return (IMS_ERROR);
	}

	userSpec = &(fa_data->jobSpec);
	 
	IMS_SETUSER(qDesc, userSpec->username);
	IMS_SETPSWD(qDesc, userSpec->password);
	IMS_SETPROG(qDesc, userSpec->program);
	IMS_SETSERVER(qDesc, userSpec->server);
	IMS_SETDBNAME(qDesc, userSpec->database);
	IMS_SET_VERBOSE(qDesc, 10);

	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not login to database");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	
	/*
	** Determine the file to parse.
	*/

	qDesc->cmd = qbuf;

	if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_TI_A1,
			IMS_FA_A1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine granule table name for ADEOS-1 Tracking Msg.");
		return(NULL);
	}


  /* query change for R2.1 */

	sprintf(qbuf,
		"select granule_idx, name, format from %s d, platforms p, downlink_entry r where\
	  	r.REVOLUTION = %d and r.PLATFORM = p.acronym and p.platform = '%s' and \
		r.SEQUENCE = %d and r.TRANSMITTER_ID = '%s' \
		and r.REVOLUTION = d.REVOLUTION \
		and r.PLATFORM = d.PLATFORM \
		and r.SEQUENCE = d.SEQUENCE",
		granuleTable, rev, fa_data->platform, sequence, trans);

	while((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if(status < IMS_OK)
		{
			(void)ims_msg(msgDesc,IMS_ERROR,
				  "Could not preform query of ADEOS tracking table");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}
							   
		if(status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void)memcpy((char *) &granule_idx,
			  qDesc->valAddr[0],qDesc->valLength[0]);
		(void)memcpy((char *) name,
			  qDesc->valAddr[1],qDesc->valLength[1]);
		ims_trim(name);
		name[qDesc->valLength[1]] = '\0';


		memset(format, 0, sizeof(format));
		(void)memcpy((char *) format,
			  qDesc->valAddr[2],qDesc->valLength[2]);
		ims_trim(format);
		format[qDesc->valLength[2]] = '\0';
	}

	if (IMS_AFFECTED(qDesc) < 1)
	{
		/*
		** No Reception Statistics Available. 
		*/

		(void) ims_msg(msgDesc, IMS_WARNING,
	     "ADEOS Data Quality Information Unavailable for Rev = %d Seq = %d.",
		 rev, sequence);
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);
	}


	if (loadDatasetPaths(msgDesc, IMS_FA_TI_A1, IMS_FA_A1,
			&datasetPath) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not load and dataset path policy information");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}


	if (locateRepositoryFile(msgDesc, datasetPath, fullPathName,
			name, format, granule_idx, "D") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get path for ADEOS Data Quality File");
		(void) freeDatasetPaths(msgDesc, datasetPath);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	(void) freeDatasetPaths(msgDesc, datasetPath);

	/*
	**  Now that we have the file,  we need to parse it.
	*/

	fptr = fopen(fullPathName, "r");
	if (fptr == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open file %s to read quality information",
			fullPathName);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if (fread(&adeos_header, sizeof(adeos_header), 1, fptr) < (size_t) 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not read ADEOS Data Quality File %s", fullPathName);
		fclose(fptr);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if (fread(&data_quality, sizeof(data_quality), 1, fptr) < (size_t) 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not read ADEOS Data Quality File %s", fullPathName);
		fclose(fptr);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	fclose(fptr);

	pqual = (float) (100.0 - ((float) atof(data_quality.loss_secs) /  
			atof(data_quality.total_secs)) * 100.0);

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc(qDesc);
		return (IMS_FATAL);
	}

	/*
	** Next, we need to take the percentage of loss seconds and compare
	** against the configured G, P, N in the database.
	*/

	sprintf(qbuf, 
		"select quality from  fa_config_table where \
		value <= %d  and item='RECEPTION_QUALITY' order by quality",
		(int) (pqual * 1000));

	IMS_SETROWCOUNT(qDesc, "1");

	while((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if(status < IMS_OK)
		{
			(void)ims_msg(msgDesc,IMS_ERROR,
				  "Could not preform query of fa_config_table");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}
							   
		if(status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void)memcpy((char *) &quality,
			  qDesc->valAddr[0],qDesc->valLength[0]);
	}			 

	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Percentage Quality Not Supported. Percentage = %f",
			pqual);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}


	switch (quality)
	{
		case 1:
			*qual_code = 'G';
			break;
		case 2:
			*qual_code = 'P';
			break;
		case 3:
			*qual_code = 'N';
			break;
	}
				  
	ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}

/****************************************************************************
**
** getPassInfo
**
** Get Passlog Information
** 
****************************************************************************/

static int getPassInfo(
	IMS_MSG_STRUCT *msgDesc, 
	IMS_FA_INTERFACE *fa_data, 
	int rev,
	int band,
	char *timeOn, 
	char *timeOff,
	char *qual_code,
	char *bitStart,
	char *bitStop)
{
	FILE *fptr;
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN + 1];
	IMS_JOB_USER_SPEC *userSpec;
	char granuleTable[IMS_COL30_LEN+1];
	char fullPathName[IMS_PATH_LEN+1];
	float pqual;
	int quality;
	int status;
	int granule_idx;
	char name[IMS_COL30_LEN+1];
	char format[IMS_COL10_LEN+1];
	IMS_FA_PATH_LIST *datasetPath;
	int channel;
	int value;
	char startTime[IMS_DATETIME_LEN+1];
	char endTime[IMS_DATETIME_LEN+1];
	IMS_NUMERIC_DATE dateDef;
	int msecs, days;
	int sync, outsync;
	int dummy;/*R2.1*/



	/*
	** Get connection to SQL server.
	*/

	if((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ*)NULL)
	{
		(void)ims_msg(msgDesc, IMS_FATAL, "Could not alloc a query decriptor.");
		return (IMS_ERROR);
	}

	userSpec = &(fa_data->jobSpec);
	 
	IMS_SETUSER(qDesc, userSpec->username);
	IMS_SETPSWD(qDesc, userSpec->password);
	IMS_SETPROG(qDesc, userSpec->program);
	IMS_SETSERVER(qDesc, userSpec->server);
	IMS_SETDBNAME(qDesc, userSpec->database);
	IMS_SET_VERBOSE(qDesc, 10);

	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not login to database");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	
	/*
	** Determine the file to parse.
	*/

	qDesc->cmd = qbuf;

	if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_TI_A1,
			IMS_FA_A1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine granule table name for ADEOS-1 Tracking Msg.");
		return(NULL);
	}


	sprintf(qbuf,
		"select granule_idx, name, format from %s, platforms p where\
	  	REVOLUTION = %d and PLATFORM = p.acronym and p.platform = '%s'",
		granuleTable, rev, fa_data->platform);


	while((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if(status < IMS_OK)
		{
			(void)ims_msg(msgDesc,IMS_ERROR,
				  "Could not preform query of ADEOS tracking table (11M)");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}
							   
		if(status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void)memcpy((char *) &granule_idx,
			  qDesc->valAddr[0],qDesc->valLength[0]);

		memset((char *) name, 0, sizeof(name));
		(void)memcpy((char *) name,
			  qDesc->valAddr[1],qDesc->valLength[1]);
		ims_trim(name);			  


		memset(format, 0, sizeof(format));
		(void)memcpy((char *) format,
			  qDesc->valAddr[2],qDesc->valLength[2]);
		ims_trim(format);			  
	}

	if (IMS_AFFECTED(qDesc) < 1)
	{
		/*
		** No Reception Statistics Available. 
		*/

		(void) ims_msg(msgDesc, IMS_WARNING,
	     "Pass Log Information Unavailable for Rev = %d.", rev);
		ims_qiFreeDesc(qDesc);
		bitStart[0] = '\0';
		bitStop[0] = '\0';
		*qual_code = 'N';
		return(IMS_OK);
	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc(qDesc);
		return (IMS_FATAL);
	}


	if (loadDatasetPaths(msgDesc, IMS_FA_TI_A1, IMS_FA_A1,
			&datasetPath) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not load and dataset path policy information");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}


	if (locateRepositoryFile(msgDesc, datasetPath, fullPathName,
			name, format, granule_idx, "D") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get path for ADEOS Data Quality File");
		(void) freeDatasetPaths(msgDesc, datasetPath);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	(void) freeDatasetPaths(msgDesc, datasetPath);

	/*
	**  Now that we have the file call the pass log API function.
	*/

	/*
	** Need to calculate the start/end fudge factor values.
	*/

	if (getConfigInfo(msgDesc, qDesc, "START_ACQ_DIFF", fa_data->platform,
			0, &value) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get start acquisition time difference");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if (ims_timeToNumericDate(msgDesc, timeOn, &dateDef) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_ON field.");
		return(IMS_ERROR);
	}

	ims_numericDateToESAI(&dateDef, &days, &msecs);
	days ++;  /* Relative to zero */

	msecs = msecs + value * 1000;
	if (msecs > 24 * 3600 * 1000)
	{
		days ++;
		msecs -= 24 * 3600 * 1000;
	}

	ims_dateItoDef(days, msecs, startTime);


	if (getConfigInfo(msgDesc, qDesc, "END_ACQ_DIFF",  fa_data->platform, 
		0, &value) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get end acquisition time difference");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if (ims_timeToNumericDate(msgDesc, timeOff, &dateDef) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not extract year from TIME_OFF field.");
		return(IMS_ERROR);
	}


	ims_numericDateToESAI(&dateDef, &days, &msecs);

	days ++;  /* Relative to zero */
	msecs = msecs - value * 1000;
	if (msecs < 0)
	{
		days --;
		msecs += 24 * 3600 * 1000;
	}

	ims_dateItoDef(days, msecs, endTime);


	switch (band)
	{
		case 0:
			if (getConfigInfo(msgDesc, qDesc, "X1",  fa_data->platform, 
				0, &channel) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get map of channel for X1.");
				ims_qiFreeDesc(qDesc);
				return(IMS_ERROR);
			}
			break;

		case 1:
			if (getConfigInfo(msgDesc, qDesc, "X2",  fa_data->platform, 
				0, &channel) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get map of channel for X2.");
				ims_qiFreeDesc(qDesc);
				return(IMS_ERROR);
			}
			break;

		case 2:
			if (getConfigInfo(msgDesc, qDesc, "X3",  fa_data->platform, 
				0, &channel) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get map of channel for X3.");
				ims_qiFreeDesc(qDesc);
				return(IMS_ERROR);
			}
			break;
	}



	/*
	** Get the start/stop sync times...
	*/

	if (ims11m_times(fullPathName, channel, timeOn, timeOff,
		bitStart, bitStop) < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
	"Could not get the Bit Sync Start/Stop Information from passlog %s",
			fullPathName);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	
	if (ims11m_counts(fullPathName, channel, startTime, endTime,
		&sync,  &outsync, &dummy) < 0) /*R2.1*/
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get the Acq Status Information from passlog %s",
			fullPathName);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
		
	}

	/*
	** Check the number of events returned for sanity reasons...
	*/


	(void) ims_numericDateDiff(msgDesc, startTime, endTime, &days, &msecs);

	if ((outsync + sync + 5 < (int) (msecs / 1000)) ||
	    (outsync + sync - 5 > (int) (msecs / 1000))) 
	{
		(void) ims_msg(msgDesc, IMS_WARNING, 
"The HC TIME_ON/OFF values differ from the pass-log for REV=%d startTime = '%s'  endTime = '%s'.", rev, startTime, endTime);
	}
	 

	if (outsync + sync > 0)
		pqual =   100.0 - ((float) ((float) outsync / (outsync + sync))) * 100.0;  
	else
	{
		/*
		** No data was received for the time sync.
		*/

		bitStart[0] = '\0';
		bitStop[0] = '\0';
		*qual_code = 'N';
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);

	}

	if (strcmp(bitStart, "0000-000T00:00:00.000") == 0)
	{
		(void) ims_msg(msgDesc, IMS_WARNING, 
"The pass-log for REV=%d returned zero bit start with sync = %d outsync = %d.", rev, sync, outsync);
		bitStart[0] = '\0';
	}

	if (strcmp(bitStop, "0000-000T00:00:00.000") == 0)
	{

		(void) ims_msg(msgDesc, IMS_WARNING, 
"The pass-log for REV=%d returned zero bit stop with sync = %d outsync = %d.", rev, sync, outsync);
		bitStop[0] = '\0';
	}



	/*
	** Check for GOOD quality.
	*/

	if (getConfigInfo(msgDesc, qDesc, "RECEPTION_QUALITY",  fa_data->platform, 
		1, &value) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get reception quality for GOOD.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if ((float) value / 1000.0 <= pqual)
	{
		*qual_code = 'G';
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);
	}

	/*
	** Check for POOR quality.
	*/

	if (getConfigInfo(msgDesc, qDesc, "RECEPTION_QUALITY",  fa_data->platform,
		2, &value) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get reception quality for POOR.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if ((float) value / 1000.0 <= pqual)
	{
		*qual_code = 'P';
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);
	}

	/*
	** Check for NO_DATA quality.
	*/

	if (getConfigInfo(msgDesc, qDesc, "RECEPTION_QUALITY",  fa_data->platform, 
		3, &value) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get reception quality for NO_DATA.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	if ((float) value / 1000.0 <= pqual)
	{
		*qual_code = 'N';
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);
	}

	(void) ims_msg(msgDesc, IMS_ERROR, 
		"Could not determine the reception quality.");

	ims_qiFreeDesc(qDesc);
	return(IMS_ERROR);


}

/****************************************************************************
**
** getConfigInfo
**
** Get information from the fa_config_table.
** 
****************************************************************************/

static int getConfigInfo (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *item,
	char *platform,
	int quality,
	int *value)

{
	char qbuf[IMS_COL255_LEN+1];
	int status;


	sprintf(qbuf,
		"select value from fa_config_table where item = '%s' and \
		platform = '%s' and quality = %d",
		item, platform, quality);

	qDesc->cmd = qbuf;

	while((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if(status < IMS_OK)
		{
			(void)ims_msg(msgDesc,IMS_ERROR,
				  "Could not preform query of FA Config information");
			return(IMS_ERROR);
		}
							   
		if(status == IMS_ENDOFQUERY)
		{
			continue;
		}


		(void)memcpy((char *) value,
			  qDesc->valAddr[0],qDesc->valLength[0]);
	}

	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not find the fa config information as follows: %s",
			qbuf);
		return(IMS_ERROR);
	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc(qDesc);
		return (IMS_FATAL);
	}


	return(IMS_OK);
}
