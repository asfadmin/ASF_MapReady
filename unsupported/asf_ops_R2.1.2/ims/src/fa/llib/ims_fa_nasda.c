static char *sccs = "@(#)ims_fa_nasda.c	5.14  08/29/97";
/******************************************************************************
**
** File:	ims_fa_nasda.c
**
** Function: Perform processing of FA Reports for the National Space
**			 Development Agency of Japan.
**
** Author: Dan Crichton	
**
** Date:	7/13/95
**
**
** Modification:
**
**								3/25/97 D. Ting
**								R2.1 Changes. Changed the query from granules to downlink_entry only
**								if necessary.
**
**							  8/21/97 D. Ting
**								media id filed is a copy of MEDIA_ID_ALIAS and limited to 10 digits for REAC 
**								report
**
**							  8/29/97 D. Ting
**								Changed from 0050 to 0052
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


#include <ims_math.h>

/*
** Local NASDA JERS-1 header type
*/

typedef struct
{
	char filename[8];
	char project[4];
	char send_grnd_code[4];
	char rec_grnd_code[4];
	char gen_date[8];
	char gen_time[8];
	char file_desc;
	char rec_len[4];
	char no_of_records[5];
	char preliminary[82];
} IMS_JERS_HEADER;

typedef struct segment_info
{
	char gendate[8];  /* Generation Date */
	char aq_date[8];  /* Acquistion Date */
	char ob_date[8];  /* Observation Date */
	char start_date[14]; /* Start Date/Time of Observation */
	char end_date[14]; /* End Date/Time of Observation */
	char grnd_code[4];
	char aq_mode; 
	char segment_id[11];
	char no_of_scenes[4];
	char hddr_no[1];	/* NASDA use */
	char hddt_no[8];
	char record_time[18];
	char tape_pos[3];
	char tape_cntr[4];
	char hddt_date[8];
	char op_disk_date[8]; /* Date of Optical Disk formation */
	char ql_id[8];   	/* NASDA use */
	char qual_code;
	char abnormal_code[2];
	char preliminary[123];
} SEGMENT_INFO;

typedef struct scene_info
{
	char grs[6];
	char rsp[9];
	char orbit[5];
	char ascend;
	char solar_angle[6];
	char start_time[10]; 	/* DDDHHMMSSs where s = 100th m.s. */
	char center_time[10];
	char center_lat[7];
	char center_lon[8];
	char ul_lat[7];			/* Upper-Left */
	char ul_lon[8];
	char ur_lat[7];			/* Upper-Right */
	char ur_lon[8];
	char ll_lat[7];			/* Lower-Left */
	char ll_lon[8];
	char lr_lat[7];			/* Lower-Right */
	char lr_lon[8];
	char sensor_cond[7];
	char cloud[6];
	char addr_of_ql[5];		/* Blank, NASDA use. */
	char record_state[8];
	char no_lost_lines[4];
	char qual_code;
	char preliminary[103];
} SCENE_INFO;

typedef struct scene_list
{
	SCENE_INFO scene;
	struct scene_list *next;
} SCENE_LIST;

typedef struct segment_list
{
	SEGMENT_INFO segment;
	int revolution;
	SCENE_LIST *scene_list;
	struct segment_list *next;
} SEGMENT_LIST;


static char glbl_grnlTbl[IMS_COL30_LEN+1];

/*
** Local Functions
*/

/****************************************************************************
**
** format_jers_header
**
** Format the NASDA JERS-1 header string
****************************************************************************/
int format_jers_header(
	IMS_MSG_STRUCT *msgDesc,
	int report_id, 
	int no_of_records,
	char *format_str,
	char *filename)
{
  	IMS_NUMERIC_DATE dateDef;
	IMS_JERS_HEADER header;
	int counter;
	char date[9];
	char time[9];
	char asc_no_of_records[6];


  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	sprintf(date, "%04d%02d%02d", dateDef.year, dateDef.month, dateDef.day);
	sprintf(time, "%02d:%02d:%02d", dateDef.hours, dateDef.minutes, 
				dateDef.seconds);

	memset((char *) &header, 32, sizeof(header));

	switch (report_id)
	{
		case IMS_NASDA_CATA:
			strcpy(filename, "CATA");
			memcpy(header.filename, "CATA    ", 8);
			header.file_desc = 'Y';			
			memcpy(header.rec_len, "0256", 4);
			break;
				
		case IMS_NASDA_REAC:
			strcpy(filename, "REAC");
			memcpy(header.filename, "REAC    ", 8);
			header.file_desc = 'Y';			
			memcpy(header.rec_len, "0052", 4);
			break;

		case IMS_NASDA_MSGM:
			strcpy(filename, "MSGM");
			memcpy(header.filename, "MSGM    ", 8);
			header.file_desc = 'N';			
			memcpy(header.rec_len, "0016", 4);
			break;
				
	}

	memcpy(header.project, "ERS1", 4);
	memcpy(header.send_grnd_code, "FAIS", 4); /* ASF Code */
	memcpy(header.rec_grnd_code, "HMMO", 4);
	memcpy(header.gen_date, date, 8);
	memcpy(header.gen_time, time, 8);

	sprintf(asc_no_of_records, "%05d", no_of_records);
	memcpy(header.no_of_records, asc_no_of_records, 5);

	memcpy(format_str, (char *) &header, sizeof(header));
	return(IMS_OK);
}

/****************************************************************************
**
** format_cata_segments
**
** Format the segment sections of the CATA report.
**
** The old_hddt_num and rec_pos variables are used to keep track of
** how many segments are on each hddt.  When the hddt changes, then
** the rec_pos is set back to zero.
****************************************************************************/

int format_cata_segments(
	IMS_MSG_STRUCT *msgDesc, 
	SEGMENT_LIST *segment_list,
	char **old_hddt_num,
	int *rec_pos)
{
	char *tPtr;
	int len;
	char workbuf[64];
	IMS_NUMERIC_DATE dateStruct, dateDef;
	short int i_segment_id;
	char segment_id[12];
	SEGMENT_INFO *segment; 
	int workInt;
	int store_num;
	char media_id[IMS_COL30_LEN+1];
	
	
	segment = (SEGMENT_INFO *) &(segment_list->segment);

	memset(segment, 32, sizeof(SEGMENT_INFO));

	/*
	** Date of Catalogue Formation
	** Current Date in format YYYYMMDD
	*/
	if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not get current date");
		return(IMS_ERROR);
	}
	
	sprintf(workbuf, "%04d%02d%02d", dateStruct.year, dateStruct.month,
		dateStruct.day);

	memcpy(segment->gendate, workbuf, 8);

	/*
	** Acquisition Date
	** DOWNLINK MESSAGE: TIME_AOS
	*/

	(void) getQueryData(msgDesc, 1, &tPtr, &len);
	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse TIME_AOS date in CATA segment record");
		return(IMS_ERROR);
	}
	sprintf(workbuf, "%04d%02d%02d", dateStruct.year, dateStruct.month,
			dateStruct.day);

	memcpy(segment->aq_date, workbuf, 8);

	/*
	** Observation Date
	** DOWNLINK MESSAGE: TIME_AOS
	*/

	memcpy(segment->ob_date, workbuf, 8);

	/*
	** Start date/time of observation 
	** DOWNLINK MESSAGE: TIME_AOS
	*/

	sprintf(workbuf, "%04d%02d%02d%02d%02d%02d",
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds);

	memcpy(segment->start_date, workbuf, 14);

	/*
	** End date/time of observation
	** DOWNLINK MESSAGE: TIME_LOS
	*/

	(void) getQueryData(msgDesc, 2, &tPtr, &len);

	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse TIME_LOS date in CATA segment record");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%04d%02d%02d%02d%02d%02d",
		dateStruct.year, dateStruct.month, dateStruct.day,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds);

	memcpy(segment->end_date, workbuf, 14);

	/*
	** Ground Station Code
	** Fixed to FAIS for ASF
	*/

	memcpy(segment->grnd_code, "FAIS", 4);

	/*
	** Acquisition Mode
	** Realtime VS. Playback.
	*/

	(void) getQueryData(msgDesc, 3, &tPtr, &len);

	if (memcmp(tPtr, "RLT", 3) == 0)
	{
		segment->aq_mode = 'R';
	}
	else if (memcmp(tPtr, "DMP", 3) == 0)
	{
		segment->aq_mode = 'M';
#if 0
		(void) ims_msg(msgDesc, IMS_ERROR,
			"ACTIVITY ID not RLT.  Activity not supported.");
		return(IMS_ERROR);
#endif
	}

	/*
	** Segment ID
	** DOWNLINK MESSAGE: SEQUENCE
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);
	memcpy((void *) &i_segment_id, tPtr, len);
	sprintf(segment_id, "J1SAR%06d", i_segment_id);
	memcpy(segment->segment_id, segment_id, 11);
	
	/*
	** HDDT No.
	** 
	** Currently set to MEDIA_ID_ALIAS.
	** We extract the last 3 digits based on ASFs mechanism.
	*/

	(void) getQueryData(msgDesc, 5, &tPtr, &len);

  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	memset(media_id, 0, sizeof(media_id)); 
	memcpy(media_id, tPtr, len);
	ims_trim(media_id);

	memset(workbuf, 0, sizeof(workbuf));

	if (dateDef.year < 2000)
	{
		sprintf(workbuf, "H%02d50",dateDef.year - 1900);
	}
	else
	{
		sprintf(workbuf, "H%02d50",dateDef.year - 2000);
	}

	memcpy(workbuf + 5, (tPtr + len - 3), 3); 

	memcpy(segment->hddt_no, workbuf, 8);

	/*
	** Record Time Information (Start Time)
	** DOWNLINK MESSAGE: START_TIME
	*/

	(void) getQueryData(msgDesc, 6, &tPtr, &len);

	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse START_TIME in CATA segment record");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%03d%02d%02d%02d",
		dateStruct.doy,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds);

	memcpy(segment->record_time, workbuf, 9);

	/*
	** Record Time Information (End Time)
	** DOWNLINK MESSAGE: STOP_TIME
	*/

	(void) getQueryData(msgDesc, 7, &tPtr, &len);

	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse TIME_DATA_STOP in CATA segment record");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%03d%02d%02d%02d",
		dateStruct.doy,
		dateStruct.hours, dateStruct.minutes, dateStruct.seconds);
	
	memcpy(segment->record_time + 9, workbuf, 9);



	/*
	** Tape Counter
	** Downlink Message: START_ADDRESS
	*/

	(void) getQueryData(msgDesc, 8, &tPtr, &len);
	memcpy(&workInt, tPtr, len);

	sprintf(workbuf, "%04d",  workInt);
	memcpy(segment->tape_cntr, workbuf, 4);

	/*
	** Tape Position
	** Record number on this tape
	*/

	if ((*old_hddt_num == NULL) ||
		(memcmp(*old_hddt_num, segment->hddt_no,  sizeof(segment->hddt_no))))
	{
		/*
		** No match, must be a new tape.
		*/
		
		*rec_pos = 0;
		*old_hddt_num = segment->hddt_no;
	}
	else
		*rec_pos = *rec_pos + 1;

	store_num = getStorageNumber(msgDesc, glbl_grnlTbl, NULL, media_id,
		(int) segment->tape_cntr);

	if (store_num <= 0)
	{
		/*
		** Error getting number.
		*/

		ims_msg(msgDesc, IMS_WARNING, 
			"Could not determine the storage number for media '%s'.  1 is assumed. ",
			media_id);
		
		store_num = 1;

	}


	sprintf(workbuf, "%03d", store_num);
	memcpy(segment->tape_pos, workbuf, 3);

	/*
	** Date of Formation of HDDT
	** granule_xxx received_time
	*/
	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy(segment->hddt_date, tPtr, len);
	
	/*
	** Path Quality Code
	** DOWNLINK MESSAGE: status
	*/
	(void) getQueryData(msgDesc, 9, &tPtr, &len);

	if (memcmp(tPtr, "HST_S_OK",8) == 0)
	{
		segment->qual_code = 'G';
	}
	else if (memcmp(tPtr, "HST_S_PROBLEM", 13) == 0)
	{
		segment->qual_code = 'R';
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unknown quality code in Downlink Message");
		return(IMS_ERROR);
	}

	/*
	** Acqusition Abnormal Code
	** Currently hardcoded to NORMAL (00)
	*/
	memcpy(segment->abnormal_code, "00", 2);


	return(IMS_OK);	 
}

/****************************************************************************
**
** format_cata_scenes
**
** Format the scene sections of the CATA report by retrieving all of the
** frame information from the scanned results table which has a matching
** segment id.
****************************************************************************/

int format_cata_scenes(
	IMS_MSG_STRUCT *msgDesc,
	SEGMENT_LIST *segment_list,
	IMS_FA_INTERFACE *fa_data)
{
	/*
	** Perform query and populate scene information for the 
	** current segment.
	*/

	int scene_count = 0;
	float tempFloat;
	int tempInt;
	int status;
	char *tPtr;
	int len;
	SCENE_LIST *scene_ptr;
	char no_of_scenes[4];
	char qbuf[IMS_COL512_LEN];
	char workbuf[64], tempbuf[64];
	IMS_NUMERIC_DATE dateStruct;
	char granuleTable[IMS_COL30_LEN+1];
	double centerLat;
	double centerLon;
	long path, row;
	double angle1, angle2;
	long orbitNum;
	char start_time[IMS_DATETIME_LEN+1];
	char end_time[IMS_DATETIME_LEN+1];
	char center_time[IMS_DATETIME_LEN+1];
	long solarAngle;
	long solarAzAngle;

	/*
	** Get Scanned Results Granules Table 
	*/

    if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_SR_J1, 
			IMS_FA_J1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine granule table name for JERS-1 CATA Scene.");
		return(IMS_ERROR);
	}



	memset(workbuf, 0, sizeof(workbuf));
	memcpy(workbuf, segment_list->segment.segment_id + 5, 6);

	sprintf(qbuf,
			"select CENTER_LAT, CENTER_LON, REVOLUTION, ASC_DESC, \
			CENTER_TIME, \
			START_TIME, NEAR_START_LAT, NEAR_START_LON, \
			NEAR_END_LAT, \
			NEAR_END_LON, FAR_START_LAT, FAR_START_LON, FAR_END_LAT, \
			FAR_END_LON, END_TIME from %s where SEQUENCE = %d and \
			PLATFORM = 'J1' and status = 1 and \
			REVOLUTION = %d", granuleTable, atoi(workbuf), 
			segment_list->revolution);

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not query scanned_results");
		return(IMS_ERROR);
	}

	segment_list->scene_list = NULL;

	while((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) endQuery(msgDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query to get scene records");
				return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue; 

		if (scene_count == 0)
		{
			if ((scene_ptr = (void *) malloc(sizeof(SCENE_LIST))) == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate memory for scene list");
					return(IMS_ERROR);
			}
			segment_list->scene_list = scene_ptr;
		}
		else
		{
			if ((scene_ptr->next = 
					(void *) malloc(sizeof(SCENE_LIST))) == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate memory for scene list");
			
				/*
				** Probably should dump memory here
				*/

				return(IMS_ERROR);
			}
			scene_ptr = scene_ptr->next;
		}

		scene_count++;

		scene_ptr->next = NULL;
	
		memset(&(scene_ptr->scene), 32, sizeof(SCENE_INFO));

		/*
		** Populate the SCENE_INFO structure with the query results.
		*/





		/*
		** [2] REV->orbit
		*/

	
		memset(workbuf, 0, sizeof(workbuf));
		memset(tempbuf, 0, sizeof(tempbuf));
		(void) getQueryData(msgDesc, 2, &tPtr, &len);
		memcpy((void *) &tempInt, tPtr, len);
		sprintf(workbuf, "%05d", (int) tempInt);
		memcpy(scene_ptr->scene.orbit, workbuf, strlen(workbuf));
		orbitNum = tempInt;

		/*
		** [3] ASC_DESC->ascend
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 3, &tPtr, &len);
		sprintf(workbuf, "%c", (char) *tPtr);
		scene_ptr->scene.ascend = workbuf[0];

		/*
		** Solar Angle
		*/

		/*
		** [5] START_TIME->start_time
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 5, &tPtr, &len);
		memcpy(workbuf, tPtr, len);
		if (ims_timeToNumericDate(msgDesc, workbuf, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not convert START_TIME in scanned data table");
			return(IMS_ERROR);
		}

		ims_numericDateToIMSA(&dateStruct, start_time);

		sprintf(workbuf, "%03d%02d%02d%02d%01d", dateStruct.doy,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds,
			(int) dateStruct.msecs / 10);
		memcpy(scene_ptr->scene.start_time, workbuf, 10);

		/*
		** [14] END_TIME->end_time
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 14, &tPtr, &len);
		memcpy(workbuf, tPtr, len);

		if (ims_timeToNumericDate(msgDesc, workbuf, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not convert START_END in scanned data table");
			return(IMS_ERROR);
		}

		strcpy(end_time, workbuf);


		/*
		** [4] CENTER_TIME->center_time
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 4, &tPtr, &len);
		memcpy(workbuf, tPtr, len);

		if (ims_timeToNumericDate(msgDesc, workbuf, &dateStruct) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not convert CENTER_TIME in scanned data table");
			return(IMS_ERROR);
		}

		strcpy(center_time, workbuf);

		sprintf(workbuf, "%03d%02d%02d%02d%01d", dateStruct.doy,
			dateStruct.hours, dateStruct.minutes, dateStruct.seconds,
			(int) dateStruct.msecs / 10);

		memcpy(scene_ptr->scene.center_time, workbuf, 10);

			
		/*
		** [0] CENTER_LAT -> center_lat
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 0, &tPtr, &len);
		memcpy(&tempFloat, tPtr, len);
		if (tempFloat > 0)
			sprintf(workbuf, "+%06.3f", (float) tempFloat);
		else
			sprintf(workbuf, "%07.3f", (float) tempFloat);
		memcpy(scene_ptr->scene.center_lat, workbuf, 7);
		centerLat = tempFloat;


		/*
		** [1] CENTER_LON -> center_lon
		*/

		memset(workbuf, 0, sizeof(workbuf));
		(void) getQueryData(msgDesc, 1, &tPtr, &len);
		memcpy(&tempFloat, tPtr, len);

		if (tempFloat > 0)
			sprintf(workbuf, "+%07.3f", (float) tempFloat);
		else
			sprintf(workbuf, "%08.3f", (float) tempFloat);
		memcpy(scene_ptr->scene.center_lon, workbuf, 8);
		centerLon = tempFloat;

		/*
		** GRS Information.
		*/ 

		(void) ims_j1ll2grs(97.662000, 659, 44, -1.153000, 600,
		   centerLat, centerLon,  &path, &row);

		sprintf(workbuf, "%03d%03d",  path, row);

		memcpy(scene_ptr->scene.grs, workbuf, 6);

		/*
		** RSP Information.
		*/

		if (ims_j1rt2rsp(msgDesc, &(fa_data->jobSpec), orbitNum, "J1", 
			start_time, &path, &angle1) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not calculate RSP information.");
			return(IMS_ERROR);
		}

		sprintf(workbuf, "%03d%03.2f",  path, angle1);
		memcpy(scene_ptr->scene.rsp, workbuf, 9);
			

		/*
		** Solar Angle 
		*/

		(void) ims_solarangs(center_time,  centerLat, centerLon,
			&solarAngle, &solarAzAngle);

		sprintf(workbuf, "%03d%03d", solarAngle, solarAzAngle);
		memcpy(scene_ptr->scene.solar_angle, workbuf, 6);


		

		if (scene_ptr->scene.ascend == 'A')
		{
			/*
			** Ascending
			**  
			**   (near-end lat/lon)                 (far-end lat/lon)
			**   x----------------------------------x
			**   |                                  |
			**   |                                  |
			**   x----------------------------------x
			**   (near-start lat/lon)               (far-start lat/lon)
			*/

			/*
			** [8] NEAR_END_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 8, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);

			if (tempFloat > 0)
			{
			   sprintf(workbuf, "+%06.3f", (float) tempFloat);
            }
			else
			{
			   sprintf(workbuf, "%07.3f", (float) tempFloat);
            }
			memcpy(scene_ptr->scene.ul_lat, workbuf, 7);


			/*
			** [9] NEAR_END_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 9, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);

			if (tempFloat > 0)
			{
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			}
			else
			{
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			}
			memcpy(scene_ptr->scene.ul_lon, workbuf, 8);

			/*
			** [12] FAR_END_LAT 
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 12, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ur_lat, workbuf, 7);


			/*
			** [13] FAR_END_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 13, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ur_lon, workbuf, 8);


			/*
			** [6] NEAR_START_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 6, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else	
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ll_lat, workbuf, 7);


			/*
			** [7] NEAR_START_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 7, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ll_lon, workbuf, 8);


			/*
			** [10] FAR_START_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 10, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.lr_lat, workbuf, 7);


			/*
			** [11] FAR_START_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 11, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.lr_lon, workbuf, 8);


		}
		else if (scene_ptr->scene.ascend == 'D')
		{
			/*
			** Descending
			**  
			**   (far-end lat/lon)                  (near-end lat/lon)
			**   x----------------------------------x
			**   |                                  |
			**   |                                  |
			**   x----------------------------------x
			**   (far-start lat/lon)                (near-start lat/lon)
			*/

			/*
			** [12] FAR_END_LAT 
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 12, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ul_lat, workbuf, 7);


			/*
			** [13] FAR_END_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 13, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ul_lon, workbuf, 8);



			/*
			** [8] NEAR_END_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 8, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ur_lat, workbuf, 7);

			/*
			** [9] NEAR_END_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 9, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);

			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ur_lon, workbuf, 8);


			/*
			** [10] FAR_START_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 10, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ll_lat, workbuf, 7);


			/*
			** [11] FAR_START_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 11, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else		
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.ll_lon, workbuf, 8);



			/*
			** [6] NEAR_START_LAT
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 6, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%06.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%07.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.lr_lat, workbuf, 7);


			/*
			** [7] NEAR_START_LON
			*/

			memset(workbuf, 0, sizeof(workbuf));
			(void) getQueryData(msgDesc, 7, &tPtr, &len);
			memcpy(&tempFloat, tPtr, len);
			if (tempFloat > 0)
				sprintf(workbuf, "+%07.3f", (float) tempFloat);
			else
				sprintf(workbuf, "%08.3f", (float) tempFloat);
			memcpy(scene_ptr->scene.lr_lon, workbuf, 8);



		}
		else
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Ascend is invalid for scene information segment_id = %d",
				atoi(segment_list->segment.segment_id));
			return(IMS_ERROR);

		}

		/*
		** Sensor Condition
		** Currently left blank
		*/

		/*
		** Cloud Cover
		** 999999  (to match ACS)
		*/

		memcpy(scene_ptr->scene.cloud, "999999", 6);

		/*
		** Record State
		** 00000000 (to match ACS)
		*/

		memcpy(scene_ptr->scene.record_state, "00000000", 8);

		/*
		** no_lost_lines
		** Left blank to match ACS
		*/

		/*
		** Quality Code
		** 'G' to match ACS
		*/

		scene_ptr->scene.qual_code = 'G';


	} /* end of while */

	/*
	** Update scene count for the segment.
	*/

	sprintf(workbuf, "%04d", scene_count);
	memcpy(segment_list->segment.no_of_scenes, workbuf, strlen(workbuf));

	(void) endQuery(msgDesc);
	 
	return(IMS_OK);	 
}

/****************************************************************************
**
** ims_jers_cata
**
** JERS-1 Catalog Report
**
** The Catalog Report builds a graph structure as follows:
**      segment1 ----> (scene1)---->(scene2)---->(scene3) ...
**       |
**       |
**      segment2 ----> (scene1)---->(scene2)---->(scene3) ...
**       |
**       |
**      segment3 ----> (scene1)---->(scene2)---->(scene3) ...
**       ...
**       ...
**
**  The graph structure is then traversed and dumped to a file to 
**  create the report.
****************************************************************************/

int ims_jers_cata(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	char fixed_header[128];
	FILE *fptr;
	struct
	{
		char open_date[8];
		char close_date[8];
		char no_catalog[5];
	} file_desc;
	char filename[IMS_PATH_LEN+1];
	char tempName[IMS_PATH_LEN+1];
	char qbuf[IMS_COL1024_LEN+1];
	char workbuf[128];
	int i, j;
	int no_of_segments = 0;
	int no_of_scenes;
	int status;
	SEGMENT_LIST *segment_ptr, *ptr;
	SCENE_LIST *scene_ptr, *sptr;
	IMS_NUMERIC_DATE dateStruct;
	char *old_hddt_num;
	int rec_pos;
	char *tPtr;
	int len;
	int rec_count = 0;

	/*
	** Get Downlink Granules Table 
	*/

    if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_J1, 
			IMS_FA_J1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine granule table name for JERS-1 CATA.");
		return(IMS_ERROR);
	}


	fa_data->start_rev = fa_data->end_rev = 1;

  /* query change for R2.1 */

	sprintf(qbuf,
			"select convert(char(8), r.received_time, 112), r.TIME_AOS, \
			r.TIME_LOS, r.ACTIVITY_ID, r.SEQUENCE, \
			d.MEDIA_ID_ALIAS, r.TIME_ON, r.TIME_OFF, d.START_ADDRESS, \
			d.STATUS, r.REVOLUTION from %s d, downlink_entry r where r.TIME_ON >= '%s' and \
			r.TIME_ON <= '%s' and r.PLATFORM = 'J1' and status = 1 and \
			d.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' \
		  and r.REVOLUTION = d.REVOLUTION \
		  and r.PLATFORM = d.PLATFORM \
		  and r.SENSOR = d.SENSOR \
		  and r.SEQUENCE = d.SEQUENCE \
			order by d.MEDIA_ID_ALIAS, d.START_ADDRESS", glbl_grnlTbl,
  			fa_data->date_start, fa_data->date_end);

	/*
	** Count the number of granules.
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not query database.");
		return(IMS_ERROR);
	}

	ptr = segment_ptr = NULL; 
	old_hddt_num = NULL;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status == IMS_ENDOFQUERY)
			continue;

		if (status < IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR,
					"Could not perform query to count CATA records");
			return(IMS_ERROR);
		}

		if (no_of_segments == 0)
		{
			if ((segment_ptr = (void *) malloc(sizeof(SEGMENT_LIST))) == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not allocate memory for segment list");
				return(IMS_ERROR);
			}
			ptr = segment_ptr;
		}
		else
		{
			if ((ptr->next = (void *) malloc(sizeof(SEGMENT_LIST))) == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not allocate memory for segment list");
				/*
				** Should free memory
				*/
				return(IMS_ERROR);
			}
			ptr = ptr->next;
		}
		ptr->next = NULL;

		if (format_cata_segments(msgDesc, ptr, 
				&old_hddt_num, &rec_pos) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not format cata_segment information");
			/*
			** Should free memory
			*/

			return(IMS_ERROR);
		}
		/*
		** REVOLUTION for the segment list.
		*/

		(void) getQueryData(msgDesc, 10, &tPtr, &len);
		memcpy(&(ptr->revolution),tPtr, len);

    	if ((fa_data->start_rev > ptr->revolution) || 
			(fa_data->start_rev == 1))
	     	fa_data->start_rev = ptr->revolution;


		if (fa_data->end_rev < ptr->revolution)
		{
			fa_data->end_rev = ptr->revolution;
		}


		ptr->scene_list = NULL;
		no_of_segments ++;


	}

	(void) endQuery(msgDesc);


	ptr = segment_ptr;
    while (ptr != NULL)
	{

		/*
		** Need to format the scene information so we can get an
		** accurate count.   We do this here and retain the list info.
		*/

		if (format_cata_scenes(msgDesc,  ptr, fa_data) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not format scene information");
			return(IMS_ERROR);
		}

		rec_count ++;
		rec_count += atoi(ptr->segment.no_of_scenes);
		ptr = ptr->next;

	}



	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/

	if (format_jers_header(msgDesc, IMS_NASDA_CATA, rec_count,
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}

	/*
	** Open the report file.
	*/

	ims_concatFilePath(tempName, fullPathName, filename);
	strcpy(fullPathName, tempName);

	fptr = fopen(fullPathName, "wb");

	if (fptr == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", 
				filename);
		return(IMS_ERROR);
	}

	if ((int) fwrite((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not write CATA header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build File Descriptor Record.
	*/


	/*
	** Start Date for Descriptor.
	*/
	
	if (ims_timeToNumericDate(msgDesc, fa_data->date_start, 
				&dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse start date in CATA desc. record");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%04d%02d%02d", dateStruct.year, dateStruct.month,
		dateStruct.day);
	
	memcpy(file_desc.open_date, workbuf, 8);


	/*
	** End Date for Descriptor.
	*/

	if (ims_timeToNumericDate(msgDesc, fa_data->date_end, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not parse end date in CATA desc. record");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%04d%02d%02d", dateStruct.year, dateStruct.month,
		dateStruct.day);
	
	memcpy(file_desc.close_date, workbuf, 8);

	/*
	** Format number of segments
	*/

	sprintf(workbuf, "%05d", no_of_segments);
	memcpy(file_desc.no_catalog, workbuf, 5);
	
	if ((int) fwrite((void *) &file_desc, sizeof(file_desc), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write CATA file descriptor.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Extract 1..n segments
	*/


	while (segment_ptr != NULL)
	{

		if ((int) fwrite((void *) &(segment_ptr->segment), 
				sizeof(SEGMENT_INFO), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
				"Could not write CATA segment.");
			fclose(fptr);
			return(IMS_ERROR);
		}

		/*
		** Extract 1..n scenes to disk
		*/

		scene_ptr = segment_ptr->scene_list;
		memset(workbuf, 0, sizeof(workbuf));
		memcpy(workbuf, segment_ptr->segment.no_of_scenes, 
			sizeof(segment_ptr->segment.no_of_scenes));

		for (j = 0; j < atoi(workbuf); j++)
		{
			if ((int) fwrite((void *) &(scene_ptr->scene), 
					sizeof(SCENE_INFO), 1, fptr) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, 
					"Could not write CATA scene.");
				fclose(fptr);
				return(IMS_ERROR);
			}

			sptr = scene_ptr;
			scene_ptr = scene_ptr->next;
			free(sptr);
		}

		ptr = segment_ptr;
		segment_ptr = segment_ptr->next;
		free(ptr);
	}
	

	fclose(fptr);
	return(IMS_OK);
	
}



/****************************************************************************
**
** format_reac_record
**
** Format the REAC record.
****************************************************************************/

int format_reac_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data)
{

	struct  
	{
		char aq_date[8]; 			/* YYYYMMDD */
		char plan_num[6];
		char path_number[3];
		struct 
		{
			char freq[2];
			char store_num;
			char counter[4];
			char hddt_num[10];
			char record_date[18]; 	 /* DDDhhmmss */
		} hddt;

	} acq_result;
	char *tPtr;
	int len;
	char workbuf[64];
	char media_id[IMS_COL30_LEN+1];
	IMS_NUMERIC_DATE dateStruct, dateDef;
	int tempInt;
	static char *old_Media="xxxxxxxxxxxxxxxxxxxx";
	static int store_num = 0;
	char start_time[IMS_DATETIME_LEN+1];
	int path;
	double angle1;
	char *mptr;
	int i;

	memset(&acq_result, 32, sizeof(acq_result));

	/*
	** path_number
	** Downlink Message: REVOLUTION
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);
	memset(start_time, 0, sizeof(start_time));
	memcpy(start_time, tPtr, len);

	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);

	if (ims_j1rt2rsp(msgDesc, &(fa_data->jobSpec), tempInt, "J1", 
		start_time, &path, &angle1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not calculate RSP information.");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%3d", path);

	memcpy(acq_result.path_number, workbuf, 3);


	if ((fa_data->start_rev > tempInt) || (fa_data->start_rev == 1))
		fa_data->start_rev = tempInt;


	if (fa_data->end_rev < tempInt)
	{
		fa_data->end_rev = tempInt;
	}

	/*
	** hddt_num
	** Downlink Message: MEDIA_ID_ALIAS
	*/

	(void) getQueryData(msgDesc, 1, &tPtr, &len);
	
  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	memset(media_id, 0, sizeof(media_id));
	memcpy(media_id, tPtr, len);
	ims_trim(media_id);

	/* trim leading blanks */
	mptr = (char *) media_id;
	for ( i=0; i < strlen(media_id); i++)
	{
		if ( *mptr != ' ' )
			 break;
	  else
			 mptr++;
  }
	memset(workbuf, 0, sizeof(workbuf));
	strcpy( workbuf, mptr); 
	if ( strlen(workbuf) >= 10 )
	{
	  memcpy(acq_result.hddt.hddt_num, workbuf, 10);
  }
	else{
			 for (i=0; i < (10 - strlen(workbuf)); i++)
					workbuf[9 - i] = ' ';
	     memcpy(acq_result.hddt.hddt_num, workbuf, 10);
			 }

	if (memcmp(old_Media, tPtr, len) != 0)
	{
		memset(old_Media, 0, strlen(old_Media));
		memcpy(old_Media, tPtr, len);
	}



	/*
	** counter
	** Downlink Message: START_ADDRESS
	*/

	(void) getQueryData(msgDesc, 2, &tPtr, &len);
	memcpy(&tempInt, tPtr, len);
	sprintf(workbuf, "%04d", (int) tempInt);
	memcpy(acq_result.hddt.counter, workbuf, 4);


	/*
	** Go and calculate the storage number.
	*/
	
	store_num = getStorageNumber(msgDesc, glbl_grnlTbl, NULL, media_id,
		tempInt);

	if (store_num <= 0)
	{
		/*
		** Error getting number.
		*/

		ims_msg(msgDesc, IMS_WARNING, 
			"Could not determine the storage number for media '%s'.  1 is assumed. ",
			media_id);
		
		store_num = 1;

	}

	sprintf(workbuf, "%1d", store_num);
	memcpy(&(acq_result.hddt.store_num), workbuf, 1);


	/*
	** record_date
	** Downlink Message: TIME_ON + TIME_OFF (YYYYMMDD)
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);

	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not extract year from TIME_ON field.");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%04d%02d%02d", dateStruct.year, dateStruct.month,
		dateStruct.day);

	memcpy(acq_result.aq_date, workbuf, 8);

	sprintf(workbuf, "%03d%02d%02d%02d", dateStruct.doy, dateStruct.hours,
		dateStruct.minutes, dateStruct.seconds);

	memcpy(acq_result.hddt.record_date, workbuf, 9);

	(void) getQueryData(msgDesc, 5, &tPtr, &len);

	if (ims_timeToNumericDate(msgDesc, tPtr, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not extract year from TIME_OFF field.");
		return(IMS_ERROR);
	}

	sprintf(workbuf, "%03d%02d%02d%02d", dateStruct.doy, dateStruct.hours,
		dateStruct.minutes, dateStruct.seconds);

	memcpy(acq_result.hddt.record_date + 9, workbuf, 9);

	/*
	** Plan Number
	** Downlink Message: FA_SCHEDULE_LINK
	*/

	(void) getQueryData(msgDesc, 7, &tPtr, &len);
	memcpy(acq_result.plan_num, tPtr, 6); /*R2.1*/

	/*
	** x_band frequency
	** F1: 8.15 GHz    F2: 8.35 GHz
	** Downlink Message: frequency
	*/

	(void) getQueryData(msgDesc, 8, &tPtr, &len);

	/*
	** Convert frequency to correct band transmission
	*/

	if (memcmp(tPtr, "JERS-1_8150", 11) == 0)
		memcpy(acq_result.hddt.freq, "F1", 2);
	else if (memcmp(tPtr, "JERS-1_8350", 11) == 0)
		memcpy(acq_result.hddt.freq, "F2", 2);
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Transmitter ID not recognized in acquisition");
	}


	if ((int) fwrite((void *) &acq_result, sizeof(acq_result), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write REAC record");
		fclose(fptr);
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

	strcpy(track.start_time, start_time);
	strcpy(track.end_time, end_time);
	strcpy(track.platform, IMS_FA_J1);
				   
	if (updateTracking(msgDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not update tracking table for ADEOS REAC report");
	   return(IMS_ERROR);
	}

}


/****************************************************************************
**
** ims_jers_reac
**
** JERS-1 Aquisition Result Report
****************************************************************************/

int ims_jers_reac(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	IMS_NUMERIC_DATE dateStruct;
	char fixed_header[128];
	FILE *fptr;
	char tempName[IMS_PATH_LEN+1];
	char filename[IMS_PATH_LEN+1];
	struct
	{
		char duration[16];
	} file_desc;
	int acq_result_total = 0;
	int i;
	int status;
	char tempDate[9];
	char qbuf[IMS_COL1024_LEN+1];

	/*
	** Get Downlink Granules Table 
	*/

    if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_J1, 
			IMS_FA_J1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine granule table name for JERS-1 REAC");
		return(IMS_ERROR);
	}

	fa_data->start_rev = fa_data->end_rev = 1;




	/*
	** Could total number of records.
	*/ 

  /* query change for R2.1 */

	sprintf(qbuf,
			"select r.REVOLUTION, d.MEDIA_ID_ALIAS, d.START_ADDRESS,  \
			d.STOP_ADDRESS, d.START_TIME, d.END_TIME, \
			d.STATUS, r.FA_SCHEDULE_LINK, r.TRANSMITTER_ID, d.START_TIME, d.END_TIME \
			from %s d, downlink_entry r where d.START_TIME >= '%s' and \
			d.START_TIME <= '%s' and r.PLATFORM = 'J1' and \
			d.status = 1 and d.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' \
		  and r.REVOLUTION = d.REVOLUTION \
		  and r.PLATFORM = d.PLATFORM \
		  and r.SENSOR = d.SENSOR \
		  and r.SEQUENCE = d.SEQUENCE",
			glbl_grnlTbl,
			fa_data->date_start, fa_data->date_end); /* R2.1 change */

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not query database.");
		return(IMS_ERROR);
	}

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

	  	acq_result_total ++ ;
	}

	(void) endQuery(msgDesc);

	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/

	if (format_jers_header(msgDesc, IMS_NASDA_REAC, acq_result_total, 
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
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

	if ((int) fwrite((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write REAC header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Build File Descriptor Record.
	*/

	/*
	** Convert start/end dates to YYYYMMDD format
	*/

	if (ims_timeToNumericDate(msgDesc, fa_data->date_start,
		&dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not convert start date %s",
			fa_data->date_start);
	}
	sprintf(tempDate, "%04d%02d%02d", dateStruct.year,
		dateStruct.month, dateStruct.day);
		

	memcpy(file_desc.duration, tempDate, 8);

	if (ims_timeToNumericDate(msgDesc, fa_data->date_end,
		&dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not convert end date %s",
			fa_data->date_end);
	}
	sprintf(tempDate, "%04d%02d%02d", dateStruct.year,
		dateStruct.month, dateStruct.day);
		

	memcpy(file_desc.duration + 8, tempDate, 8);

	
	if ((int) fwrite((void *) &file_desc, sizeof(file_desc), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write REAC file descriptor.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** 1..n Acquisition Result 
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not query database.");
		fclose(fptr);
		return(IMS_ERROR);
	}


	for (i = 0; i < acq_result_total; i++)
	{
		if (performQuery(msgDesc) != IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR,
					"Could not perform query to count REAC records");
			fclose(fptr);
			return(IMS_ERROR);
		}


		
		if (format_reac_record(msgDesc, fptr, fa_data) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format REAC record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	/*
	** Allow Sybase to finish out the query
	*/

	(void) performQuery(msgDesc);

	(void) endQuery(msgDesc);
	fclose(fptr);
	return(reacTracking(msgDesc, fa_data->date_start, fa_data->date_end));

}

/****************************************************************************
**
** format_msgm_record
**
** Format the msgm shipment record.
****************************************************************************/

int format_msgm_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr,
	IMS_FA_INTERFACE *fa_data,
	int count)
{

	struct  
	{
		char ship_date[8];
		char hddt_no[8];
	} msgm_data;
	char qbuf[IMS_COL512_LEN]; 
	char granuleTable[IMS_COL30_LEN+1];
	int rowCount = 0;
	char *tPtr;
	int len;
	int status;

	/*
	** Get JERS-1 Tape Available Granules Table 
	*/

    if (getGranuleTableName(msgDesc, granuleTable, IMS_FA_TA_J1, 
			"HC") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine granule table name for JERS-1 CATA Scene.");
		return(IMS_ERROR);
	}

	sprintf(qbuf, 
		"select MEDIA_ID_ALIAS from %s where MEDIA_ID = '%s' and \
		MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY'",
		granuleTable, fa_data->tape_num[count]);

	if (setupQuery(msgDesc, qbuf) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not query database.");
		return(IMS_ERROR);
	}

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) endQuery(msgDesc);
			ims_msg(msgDesc, IMS_ERROR,
					"Could not perform query to get shipment records");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		rowCount ++;

		(void) getQueryData(msgDesc, 0, &tPtr, &len);


		memcpy(msgm_data.hddt_no, tPtr,
			sizeof(msgm_data.hddt_no));

	}

	/*
	** Make sure the tape existed to be shipped.
	*/

	if (rowCount < 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get data for the specified tape %s",
			fa_data->tape_num[count]);
		(void) endQuery(msgDesc);
		return(IMS_ERROR);

	}

	(void) endQuery(msgDesc);

	memcpy(msgm_data.ship_date, fa_data->tape_date[count], 
		sizeof(msgm_data.ship_date));


	if ((int) fwrite((void *) &msgm_data, sizeof(msgm_data), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write MSGM record");
		fclose(fptr);
		return(IMS_ERROR);
	}

	return(IMS_OK);

}


/****************************************************************************
**
** ims_jers_msgm
**
** JERS-1 Shipment Report
****************************************************************************/

int ims_jers_msgm(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data) 
{
	char fixed_header[128];
	FILE *fptr;
	int i;
	int num_ship_records;
	char qbuf[IMS_COL512_LEN+1];
	char filename[IMS_PATH_LEN+1];
	char tempName[IMS_PATH_LEN+1];


	/*
	** Get Downlink Granules Table 
	*/

    if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_J1, 
			IMS_FA_J1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine granule table name for JERS-1 MSGM.");
		return(IMS_ERROR);
	}



	/*
	** Build Fixed Portion which is <filename><generation time>.
	*/

	num_ship_records = fa_data->tape_count;

	if (format_jers_header(msgDesc, IMS_NASDA_MSGM, num_ship_records, 
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
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

	if ((int) fwrite((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write NASDA MSGM header");
		fclose(fptr);
		return(IMS_ERROR);
	}


	/*
	** Format 1..n medium shipment report records
	*/

	for (i = 0; i < num_ship_records; i++)
	{
		if (format_msgm_record(msgDesc, fptr, fa_data, i) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format MSGM record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	fclose(fptr);
	return(IMS_OK);

}
