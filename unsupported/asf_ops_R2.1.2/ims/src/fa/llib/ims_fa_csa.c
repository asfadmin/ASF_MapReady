static char *sccs = "@(#)ims_fa_csa.c	6.3  03/25/98";
/******************************************************************************
**
** File:	ims_fa_csa.c
**
** Function: Perform processing of FA Reports for the Canadian Space 
**			 Agency.
**
** Author: Dan Crichton	
**
** Date:	6/14/95
**
** Modification: 
**	 2/21/97 D. Ting
**	 PR# 2425 CSA reception report format changes
**	 PR# 2424 CSA archive storage report format changes
**
**	 3/25/97 D. Ting
**	 R2.1 Changes. Changed query from granules to downlink_entry, 
**	 if necessary.
**
**	 5/5/97 D. Ting
**	 R2.1 Changed the query based on revolution and station_id.
**	 If more than two objects found in RECRPT then error out.
**
**	 8/19/97 D. Ting
**	 PR# ???? CSA reception report changes; combine common downlink
**       activities.
**
**	 9/17/97 D. Ting
**	 PR#2791 - set to default M0000000 or F0000000 if HDDT_ID is NULL
** 
**       3/9/98  J. Ho
**       PR#???? - fixed HDDT_ID in fxxxxxxx format, NB_OF_ARCH_REF and 
**                 whole ODL format in CSA ARCHIVE_STORAGE_REPORT
**       PR#???? - fixed FILENAME in CSA ARCHIVE_STORAGE_REPORT and
**                 CSA RECEPTION_REPORT
**       3/25/98 J. Ho
**       PR#???? - correct NB_OF_MEDIA_SPEC in CSA Reception_report
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
#include <CSAparse.h>

/*
** Local Types
*/

static char glbl_grnlTbl[IMS_COL30_LEN+1];

/*
** Local Functions
*/

/* PR# 2425 */
void replace_T(char *, int);


/****************************************************************************
**
** ims_csa_archstrgrpt
**
****************************************************************************/

int ims_csa_archstrgrpt(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	char *tape_num)
{

	CSA_STMT temp;
  	CSA_STMT temp2;
  	CSA_STMT temp3;
  	CSA_STMT temp4;
  	CSA_STMT tree_root;
	char *tPtr;
	int len;
  	IMS_NUMERIC_DATE dateDef;
  	char currentDate[IMS_DATETIME_LEN + 1];
	char count_str[IMS_COL15_LEN+1];
	char count_rec_str[IMS_COL15_LEN+1];
	char orbit_id_str[IMS_COL15_LEN+1];
	char temp_str[IMS_COL15_LEN+1];
	char filename[IMS_PATH_LEN+1];
	char tempname[IMS_PATH_LEN+1];
	char qbuf[1024];
	char temp_tape[IMS_COL15_LEN+1];
	char tape_granule[IMS_COL30_LEN+1];
	int year;
	int counter;
	int count_rec;
	int status;
	int i;
	int orbit_id;
	char tape_id[IMS_COL15_LEN+1];
	int FA_flag, media_entry;

        /*
	** Get Downlink Granules Table
	*/
 
	if (getGranuleTableName(msgDesc, glbl_grnlTbl, 
		IMS_FA_DL_R1, IMS_FA_R1) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine granule table name for RADARSAT ASR.");
		return(IMS_ERROR);
	}

        /*
	** Get Tape Available Granules Table
	*/
 
	if (getGranuleTableName(msgDesc, tape_granule, IMS_FA_TA_R1, "HC") < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine granule table name for RADARSAT ASR.");
		return(IMS_ERROR);
	}

  	/*
  	** Get current date/time.
  	*/

  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

	ims_numericDateToCSAA(&dateDef, currentDate);

	year = dateDef.year;

	if (year >= 2100) {
		year = year - 2100;
	}

	if (year >= 2000) {
		year = year - 2000;
	}
	else 
		year = year - 1900;

	/*
	** Get counter
	*/

	if (getReportCounter(msgDesc, IMS_CSA_ARCHSTRGRPT, &counter) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not get report counter.");
		return(IMS_ERROR);
	}

	/*
	** Update counter value.
	*/

	if (updateReportCounter(msgDesc, IMS_CSA_ARCHSTRGRPT, counter + 1) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not update report counter.");
		return(IMS_ERROR);
	}

	/*
	** Setup query to count number of records
	*/
	/* query changed for R2.1 */

	 sprintf(qbuf, 
		"select distinct r.REVOLUTION, d.MEDIA_ID_ALIAS, \
                r.ACTIVITY_ID, r.STATION_ID \
		from %s d, %s t ,downlink_entry r where t.MEDIA_ID = '%s' and \
		d.MEDIA_ID = '%s' and \
		d.status = 1 and \
		d.MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' \
		and r.REVOLUTION = d.REVOLUTION \
		and r.PLATFORM = d.PLATFORM \
		and r.SENSOR = d.SENSOR \
		and r.SEQUENCE = d.SEQUENCE",
		glbl_grnlTbl, tape_granule,
		tape_num, tape_num); 

	if (setupQuery(msgDesc, qbuf) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not setup query for CSA Reception report.");
		return(IMS_ERROR);
	}

	count_rec = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION) {
		if (status < IMS_OK) {
			(void) endQuery(msgDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not perform query to count CSA ASR records.");
			return(IMS_ERROR);	
		}

		if (status == IMS_ENDOFQUERY) {
			continue;
		}
		count_rec ++;
	}

        /* R2.1.2 : get STATION_ID and set the filename correct */

	/*
  	** construct the header and set the root of the tree 
	*/

        getQueryData(msgDesc, 3, &tPtr, &len);
        if (memcmp(tPtr,"FA",2) == 0) {
	        sprintf(filename, "f%02d%03d%02d.asr", year, dateDef.doy, 
			counter);
  		tree_root = (CSA_STMT) create_header(filename, "RADARSAT_1", 
		    currentDate, "FBDROC", "MCS", "ARCHIVE_STORAGE_REPORT");
        }
        else if (memcmp(tPtr,"MC",2) == 0) {
	        sprintf(filename, "m%02d%03d%02d.asr", year, dateDef.doy, 
			counter);
  		tree_root = (CSA_STMT) create_header(filename, "RADARSAT_1", 
		    currentDate, "MMDROC", "MCS", "ARCHIVE_STORAGE_REPORT");
        }
        else {
                ims_msg(msgDesc, IMS_ERROR, "Station id is unacceptable");
                return(IMS_ERROR);
        }

	(void) endQuery(msgDesc);

	sprintf(count_str, "%d", count_rec);

	/*
	** Create level-1 keywords and their values
	*/
        /* R2.1.2 : set valid value of NB_OF_ARCH_REF */
        temp = (CSA_STMT) create_keyword_value("NB_OF_ARCH_REF", count_str);

    	temp2 = (CSA_STMT) create_keyword_value("ARCHIVE_FACILITY_ID", "FAIRBANK");

  	append_statement(temp2, temp);

	/*
	** If there is no acquisition data associated with this tape, then
	** we are done with the report.
	*/

	if (count_rec == 0) {
		goto DONE_ASR_REPORT;
	}

	/*
	** Loop from 1..count_rec acqusitions and write out the 
	** medium records.
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not setup query for CSA Reception report.");
		return(IMS_ERROR);
	}

	for (i = 0; i < count_rec; i++) {
		/*
		** Create level-2 keywords
		*/
                sprintf(count_rec_str, "%d", i+1);
  		temp2 = (CSA_STMT) create_keyword_value("ARCH_REF", 
			count_rec_str);
  		temp2 = (CSA_STMT) create_ag(temp2);
		status = performQuery(msgDesc);
		
		if (status != IMS_OK) {
			(void) endQuery(msgDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Error occurred on building medium information for CSA ASR.");
		}

		/*
		** Create level-3 keywords
		*/

  		temp3 = (CSA_STMT) create_keyword_value("MEDIUM_ID", "");
  		temp3 = (CSA_STMT) create_ag(temp3);
 		append_substatement(temp3, temp2);

		/*
		** Insert body into level-3
		*/

  		temp4 = (CSA_STMT) create_keyword_value("SAT_ID", "RADARSAT_1");
  		append_substatement(temp4, temp3);

		/*
		** DOWNLINK_ORBIT_ID
		*/

		getQueryData(msgDesc, 0, &tPtr, &len);
		memcpy((char *) &orbit_id, tPtr, len);
		memset(orbit_id_str, 0, sizeof(orbit_id_str));
		sprintf(orbit_id_str, "%d", orbit_id);

  		temp4 = (CSA_STMT) create_keyword_value("RECEPTION_ORBIT_NUMBER", orbit_id_str);
  		append_substatement(temp4, temp3);

		/* The following added for R2.1 */
		getQueryData(msgDesc, 3, &tPtr, &len);
		FA_flag = 0;
		if (memcmp(tPtr,"FA",2) == 0)
		{
  			temp4 = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "F");
			FA_flag = 1;
    		} else if (memcmp(tPtr,"MC",2) == 0) {
  		  	temp4 = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "M");
		} else {
		        ims_msg(msgDesc, IMS_ERROR, "Station id is unacceptable");
  		        delete_tree(tree_root);
		        return(IMS_ERROR);
		}
		/* End of Added R2.1 */
				
  		append_substatement(temp4, temp3);

		/*
		** REALTIME vs. PLAYBACK
		*/

		getQueryData(msgDesc, 2, &tPtr, &len);
		if (memcmp(tPtr, "RLT", 3) == 0) {
  			temp4 = (CSA_STMT) create_keyword_value("RT_PB_FLG", "REALTIME");
		}
		else {
  			temp4 = (CSA_STMT) create_keyword_value("RT_PB_FLG", "PLAYBACK");
		}

  		append_substatement(temp4, temp3);

		/*
		** Save the MEDIA_ID_ALIAS for this spacecraft.
		*/
		getQueryData(msgDesc, 1, &tPtr, &len);
		memset(temp_tape, 0, sizeof(temp_tape));
		memcpy(temp_tape, tPtr, len);
		ims_trim(temp_tape);
  		/*
  		** Append archive information 
  		*/

  		temp3 = (CSA_STMT) create_keyword_value("ARCH_STATUS", "ARCHIVED");
  		append_substatement(temp3, temp2);

        	/* R2.1.2 : replace tPtr with temp_tape for HDDT_ID */
        	media_entry = 0;
        	for (i=0;i < len;i++) {
			if ((*(temp_tape+i) != '\0') && 
			    (*(temp_tape+i) != ' ')) {
			media_entry = 1;
			break;
      			}
		}

		/* NULL entry and first time */
  		if (media_entry == 0 && i == 0) { 
			if (FA_flag)
				strcpy(tape_id,"F0000000");
    			else
				strcpy(tape_id,"M0000000");
  		}
		else if (media_entry != 0) {
			if (FA_flag)
	       	 		strcpy(tape_id, "F0");
       			else
		 	 	strcpy(tape_id, "M0");
	        	memcpy(tape_id + 2, temp_tape + 6, 6);
        	}
                tape_id[8] = '\0';

  		temp3 = (CSA_STMT) create_keyword_value("HDDT_ID", tape_id);
  		append_substatement(temp3, temp2);

  		temp3 = (CSA_STMT) create_keyword_value("CREO_HDDT_ID", "");
  		append_substatement(temp3, temp2);

		/*
		** Append levels...
		*/

  		append_statement(temp2, temp);

	} /* end of for */

	(void) endQuery(msgDesc);

	/* PR# 2424 */
  	temp2 = (CSA_STMT) create_comment(";###END_OF_FILE");
  	append_statement(temp2, temp);

DONE_ASR_REPORT:

  	/* append all statements to the tree root */
  	append_statement(temp, tree_root);

	/*
	** Output the tree to the specified file.
	*/

	ims_concatFilePath(tempname, fullPathName, filename);
	strcpy(fullPathName, tempname);

  	if (print_tree_2(tree_root, tempname) < 0) { /* PR# 2424 */
		ims_msg(msgDesc,IMS_ERROR, "Could not write out report file.");
  		delete_tree(tree_root);
		return(IMS_ERROR);
	}

	fflush(NULL);

  	delete_tree(tree_root);

	return(IMS_OK);
} /* end of ims_csa_archstrgrpt */


/****************************************************************************
**
** createMediumInfo
**
****************************************************************************/

int createMediumInfo(
	IMS_MSG_STRUCT *msgDesc,
	int count,
	CSA_STMT temp,
	CSA_STMT temp2,
	CSA_STMT temp3)
{
	char count_str[IMS_COL15_LEN+1];
	int orbit_id;
	char orbit_id_str[IMS_COL15_LEN+1];
	char tape_id[IMS_COL15_LEN+1];
	char *tPtr;
	int len;
	char activity_id[IMS_COL30_LEN+1];
	int FA_flag ;
	int i, media_entry; 

	/*
	** Create MEDIUM-INFO level and the keyword values
	*/

	sprintf(count_str, "%d", count);

  	temp2 = (CSA_STMT) create_keyword_value("MEDIUM_INFO", count_str);

  	temp2 = (CSA_STMT) create_ag(temp2);

  	temp3 = (CSA_STMT) create_keyword_value("SATELLITE_ID", 
						"RADARSAT_1");
  	append_substatement(temp3, temp2);

	/*
	** Downlink Orbit ID 
	*/

	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy((char *) &orbit_id, tPtr, len);

	sprintf(orbit_id_str, "%d", orbit_id);

  	temp3 = (CSA_STMT) create_keyword_value("RECEPTION_ORBIT_NUMBER", 
						orbit_id_str);
  	append_substatement(temp3, temp2);

		/* The following added for R2.1 */
		getQueryData(msgDesc, 7, &tPtr, &len);
		FA_flag = 0;
		if (memcmp(tPtr,"FA",2) == 0)
		{
  		temp3 = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "F");
			FA_flag = 1;
    } else if
			(memcmp(tPtr,"MC",2) == 0)
			{
  		  temp3 = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "M");
			} else {
		      ims_msg(msgDesc, IMS_ERROR, "Station id is unacceptable");
		      return(IMS_ERROR);
					}
		/* End of Added R2.1 */

  	append_substatement(temp3, temp2);


	/*
	** Check here for REALTIME or PLAYBACK
	*/

	(void) getQueryData(msgDesc, 6, &tPtr, &len);
	memset(activity_id, 0, sizeof(activity_id));
	memcpy((char *) activity_id, tPtr, len);

	if (strncmp(activity_id, "RLT",3) == 0)
	{
  		temp3 = (CSA_STMT) create_keyword_value("REALTIME_PLAYBACK_FLG", 
						"REALTIME");
	}
	else
	{
  		temp3 = (CSA_STMT) create_keyword_value("REALTIME_PLAYBACK_FLG", 
						"PLAYBACK");
	}
  	append_substatement(temp3, temp2);

	/*
	** MEDIA ID
	** Convert ASF Media Id to CSA's format.
	*/

	(void) getQueryData(msgDesc, 1, &tPtr, &len);

	memset(tape_id, 0, sizeof(tape_id));

  media_entry = 0;
  for (i=0;i < len;i++)
	{
			if ( (*(tPtr+i) != '\0') && (*(tPtr+i) != ' ') )
			{
					media_entry = 1;
					break;
      }
  }

  if (media_entry == 0) /* NULL entry */
	{
		if (FA_flag)
			strcpy(tape_id,"F0000000");
    else
			strcpy(tape_id,"M0000000");
  }else{
			  if (FA_flag)
	        strcpy(tape_id, "F0");
        else
			 	 strcpy(tape_id, "M0");
	      memcpy(tape_id + 2, tPtr + 6, 6);
      }

  	temp3 = (CSA_STMT) create_keyword_value("HDDT_ID", 
						tape_id);
  	append_substatement(temp3, temp2);

  	temp3 = (CSA_STMT) create_keyword_value("NB_OF_BER_SAMPLES", 
						"1"); /* PR# 2425 0 has been changed to 1*/
  	append_substatement(temp3, temp2);

	/* PR# 2425  */
	temp3 = (CSA_STMT) create_array("BER_SAMPLE", "2");
  	append_substatement(temp3, temp2);
		/* PR - end */
#if 0 /* No BER samples */


	temp3 = (CSA_STMT) create_array("BER_SAMPLE", "2");

	append_element("1995-300-12:40:05.230", temp3);
	append_element("1.0E-5", temp3);

	append_element("1995-330-12:40:05.330", temp3);
	append_element("2.0E-4", temp3);

  	append_substatement(temp3, temp2);
#endif

  	append_statement(temp2, temp);

	return(IMS_OK);
}

/****************************************************************************
**
** recrptTracking 
**
** Clean the tracking table for the reports generated.
**
****************************************************************************/
 
static int recrptTracking(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_INTERFACE *fa_data)
	
{
	IMS_FA_TRACKING track;

	memset((char *) &track, 0, sizeof(track));
	track.segment_id = fa_data->seq_num;
	track.orbit_id = fa_data->pass;
	strcpy(track.platform, IMS_FA_R1);
	strcpy(track.station_id, fa_data->station_id);/*R2.1*/


	if (updateTracking(msgDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			 "Could not update tracking table for R1 Reception Report");
		 return(IMS_ERROR);
	}
	return(IMS_OK);

}


/****************************************************************************
**
** ims_csa_recrpt
**
****************************************************************************/

int ims_csa_recrpt(
	IMS_MSG_STRUCT *msgDesc,
	char *fullPathName,
	IMS_FA_INTERFACE *fa_data)
{
	CSA_STMT temp;
  	CSA_STMT temp2;
  	CSA_STMT temp3;
  	CSA_STMT temp4;
  	CSA_STMT tree_root;
  	IMS_NUMERIC_DATE dateDef;
  	char currentDate[IMS_DATETIME_LEN + 1];
	char tempname[IMS_PATH_LEN+1];
	char filename[IMS_PATH_LEN+1];
	char qbuf[1024];
	char tdate[IMS_DATETIME_LEN+1];
	char count_str[IMS_COL15_LEN+1];
	char orbit_id_str[IMS_COL15_LEN+1];
	char sched_id[IMS_COL30_LEN+1];
	char sched_id_save[IMS_COL30_LEN+1];
	int year;
	int count;
	int counter;
	int len;
	int orbit_id;
	int status;
	char *tPtr;
	int i; /* PR# 2425 */
	char activity_id[IMS_COL30_LEN+1];
	char activity_id_save[IMS_COL30_LEN+1];
	int first_query, finished;

	/*
	** Get Downlink Granules Table
	*/
 
	if (getGranuleTableName(msgDesc, glbl_grnlTbl, IMS_FA_DL_R1, 
					IMS_FA_R1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine granule table name for RADARSAT RECRPT.");
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
	}

  	/*
  	** Get current date/time.
  	*/

  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
 	}

	ims_numericDateToCSAA(&dateDef, currentDate);

	/*
	** Setup the report counter info...
	*/

	if (getReportCounter(msgDesc, IMS_CSA_RECRPT, &counter) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get report counter.");
		return(IMS_ERROR);
	}

	/*
	** Update counter value.
	*/

	if (updateReportCounter(msgDesc, IMS_CSA_RECRPT, counter+1) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not update report counter.");
		return(IMS_ERROR);
	}


	/*
	** Setup query.
	*/

  	/* query Changed for R2.1 */

	sprintf(qbuf, 
		"select r.REVOLUTION, d.MEDIA_ID, r.TIME_AOS, \
		r.TIME_LOS, d.STATUS, r.FA_SCHEDULE_LINK, r.ACTIVITY_ID, STATION_ID from %s d, \
    		downlink_entry r \
		where r.REVOLUTION = %d and \
		r.STATION_ID = '%s' and \
		d.status = 1 and \
		d.MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' \
		and r.REVOLUTION = d.REVOLUTION \
		and r.PLATFORM = d.PLATFORM \
		and r.SENSOR = d.SENSOR \
		and r.SEQUENCE = d.SEQUENCE",
		glbl_grnlTbl, fa_data->pass, fa_data->station_id); 
		/* Changed for R2.1 */

  	/*
	fprintf(stderr, "\nTEST--%s\n", qbuf);
	fprintf(stderr, "TEST--%s\n", fa_data->station_id);
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not setup query for CSA Reception report.");
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
	}

	/*
	** Query for the first item...
	*/

	status = performQuery(msgDesc);

	if ((status == IMS_ENDOFTRANSACTION) || (status < IMS_OK)) {
		(void) ims_msg(msgDesc, IMS_ERROR,
		"No data found for rev %d seq %d in CSA Reception Report",
		fa_data->pass, fa_data->seq_num);
		(void) ims_msg(msgDesc, IMS_ERROR, "Query failed for: %s", qbuf);
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
	}

	/*
	** Downlink Orbit ID 
	*/

	(void) getQueryData(msgDesc, 0, &tPtr, &len);
	memcpy((char *) &orbit_id, tPtr, len);

	/*
  	** construct the header and set the root of the tree 
	*/

        /* R2.1.2 : get STATION_ID and set the filename correct */
        getQueryData(msgDesc, 7, &tPtr, &len);
        if (memcmp(tPtr,"FA",2) == 0) {
                sprintf(filename, "f%07d.rrp", orbit_id);
  		tree_root = (CSA_STMT) create_header(filename, "RADARSAT_1", 
		    currentDate, "FBDROC", "MCS", "RECEPTION_REPORT");
        }
        else if (memcmp(tPtr,"MC",2) == 0) {
                sprintf(filename, "m%07d.rrp", orbit_id);
  		tree_root = (CSA_STMT) create_header(filename, "RADARSAT_1", 
		    currentDate, "MMDROC", "MCS", "RECEPTION_REPORT");
        }
        else {
                ims_msg(msgDesc, IMS_ERROR, "Station id is unacceptable");
                return(IMS_ERROR);
        }

	/*
	** Create level-1 keywords and their values
	*/

	/* The following added for R2.1 */
	getQueryData(msgDesc, 7, &tPtr, &len);
	if (memcmp(tPtr,"FA",2) == 0) {
  	  temp = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "F");
        }
	else if (memcmp(tPtr,"MC",2) == 0) {
  	  temp = (CSA_STMT) create_keyword_value("RECEPTION_FACILITY_ID", "M");
	} 
	else {
	  ims_msg(msgDesc, IMS_ERROR, "Station id is unacceptable");
	  return(IMS_ERROR);
	}
	/* End of Added R2.1 */

	/* PR# 2425 deleted--sprintf(sched_id , "%d", counter); */
	(void) getQueryData(msgDesc, 5, &tPtr, &len);
	memset(sched_id, 0, sizeof(sched_id));
	memcpy(sched_id, tPtr, len);
	for(i = 1; i < len; i++)
	{
		if ((sched_id[i] == '_') || (sched_id[i] == '-'))
			break;
  	}
	sched_id[i] = '\0';
	/* PR# --end */

  	temp2 = (CSA_STMT) create_keyword_value("RECEPTION_SCHEDULE_ID", 
			sched_id);
  
  	append_statement(temp2, temp);

	fa_data->start_rev = fa_data->end_rev = orbit_id;

	sprintf(orbit_id_str, "%d", orbit_id);
  	temp2 = (CSA_STMT) create_keyword_value("RECEPTION_ORBIT_NUMBER", 
			orbit_id_str);

  	append_statement(temp2, temp);

	/*
	** TIME_AOS
	*/

	(void) getQueryData(msgDesc, 2, &tPtr, &len);
	memset(tdate, 0, sizeof(tdate));
	memcpy(tdate, tPtr, len);

	strcpy(fa_data->date_start, tdate);

	(void) replace_T(tdate, len); 		/* PR# 2425 */

  	temp2 = (CSA_STMT) create_keyword_value("ACTUAL_X_BAND_AOS", tdate);

  	append_statement(temp2, temp);

	/*
	** TIME_LOS
	*/

	(void) getQueryData(msgDesc, 3, &tPtr, &len);
	memset(tdate, 0, sizeof(tdate));
	memcpy(tdate, tPtr, len);

	strcpy(fa_data->date_end, tdate);
	
	(void) replace_T(tdate, len); 		/* PR# 2425 */

  	temp2 = (CSA_STMT) create_keyword_value("ACTUAL_X_BAND_LOS", tdate);

  	append_statement(temp2, temp);

	/*
	** STATUS
	*/

	(void) getQueryData(msgDesc, 4, &tPtr, &len);

	if (memcmp(tPtr, "HST_S_OK", len) == 0) {
  		temp2 = (CSA_STMT) create_keyword_value(
				"PASS_COMPLETION_STATUS", "SUCCESS");
	}
	else if (memcmp(tPtr, "HST_S_PROBLEM", len) == 0) {
  		temp2 = (CSA_STMT) create_keyword_value(
				"PASS_COMPLETION_STATUS", "FAIL");
	}
	else {
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unknown status in granules table for rev %d seq %d",
			fa_data->pass, fa_data->seq_num);
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
	}

  	append_statement(temp2, temp);

  	temp2 = (CSA_STMT) create_keyword_value("RECEPTION_COMMENTS", "");

  	append_statement(temp2, temp);

	(void) endQuery(msgDesc);

	/*
	** Scan through all downlink messages with the same schedule id.
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query for CSA report.");
		return(IMS_ERROR);
	}

	/*
	** Finish out the query to count the number of media specs.
	*/
	count = 0;
	first_query = 1;
	finished = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION) {
	    if (status < IMS_OK) {
	    	(void) endQuery(msgDesc);
	    	(void) ims_msg(msgDesc, IMS_ERROR,
	    		"Could not perform query to count CSA Reception records.");
	    	(void) recrptTracking(msgDesc, fa_data);
	    	return(IMS_ERROR);	
	    }

	    if (status == IMS_ENDOFQUERY) {
	    	continue;
	    }
	    /* The following added for combining */

  	    (void) getQueryData(msgDesc, 5, &tPtr, &len);
	    memset(sched_id, 0, sizeof(sched_id));
	    memcpy(sched_id, tPtr, len);
	    ims_trim(sched_id);

	    for (i = 1; i < len; i++) {
	        if ((sched_id[i] == '_') || (sched_id[i] == '-'))
		    break;
    	    }
	    sched_id[i] = '\0';
  
	    (void) getQueryData(msgDesc, 6, &tPtr, &len);
	    memset(activity_id, 0, sizeof(activity_id));
	    memcpy((char *) activity_id, tPtr, len);
	    ims_trim(activity_id);

	    if (first_query) {
		strcpy(sched_id_save, sched_id);
		strcpy(activity_id_save, activity_id);
		first_query = 0;
		count++;
    	    }
            else {
		if (strcmp(sched_id, sched_id_save) == 0) {
		    if (strcmp(activity_id, activity_id_save) == 0)
			continue;
		    else if (finished)
			continue;
		    else {
			finished = 1;
			count ++;
	            }
      		} else {
		    (void) ims_msg(msgDesc, IMS_ERROR,
			 "More than one schedule id in CSA Reception report");
			 (void) recrptTracking(msgDesc, fa_data);
			 return(IMS_ERROR);
	        }
    	    }
	} /* end of while */
        /* R2.1.2 */
        /*
        if (count == 0)  
        	count++;
        */

	sprintf(count_str, "%d", count);

  	temp2 = (CSA_STMT) create_keyword_value("NB_OF_MEDIA_SPECS", count_str);
  	append_statement(temp2, temp);
	(void) endQuery(msgDesc);

	/*
	** Scan through all downlink messages with the same schedule id.
	*/

	if (setupQuery(msgDesc, qbuf) < IMS_OK) {
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not setup query for CSA report.");
		return(IMS_ERROR);
	}

	count = 0;

	first_query = 1;
	finished = 0;

	while ((status = performQuery(msgDesc)) != IMS_ENDOFTRANSACTION) {
	    if (status < IMS_OK) {
		(void) endQuery(msgDesc);
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not perform query to count CSA Reception records.");
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);	
	    }

	    if (status == IMS_ENDOFQUERY) {
		continue;
	    }

	    /* The following added for combining */

	    (void) getQueryData(msgDesc, 5, &tPtr, &len);
	    memset(sched_id, 0, sizeof(sched_id));
	    memcpy(sched_id, tPtr, len);
	    ims_trim(sched_id);
	    for (i = 1; i < len; i++) {
	        if ((sched_id[i] == '_') || (sched_id[i] == '-'))
		    break;
            }
	    sched_id[i] = '\0';
  
	    (void) getQueryData(msgDesc, 6, &tPtr, &len);
	    memset(activity_id, 0, sizeof(activity_id));
	    memcpy((char *) activity_id, tPtr, len);
	    ims_trim(activity_id);

	    if (first_query) {
			strcpy(sched_id_save, sched_id);
			strcpy(activity_id_save, activity_id);
			first_query = 0;
			count++;
            }
            else {
	        if (strcmp(sched_id, sched_id_save) == 0) {
		    if (strcmp(activity_id, activity_id_save) == 0)
			continue;
		    else
			if (finished)
		            continue;
			else {
			    finished = 1;
			    count ++;
			}
                } 
                else {
		    (void) ims_msg(msgDesc, IMS_ERROR,
		    "More than one schedule id in CSA Reception report");
		    (void) recrptTracking(msgDesc, fa_data);
		    return(IMS_ERROR);
		}
            }

	    /*
	    ** Create medium record.
	    */

	    if (createMediumInfo(msgDesc, count, temp, temp2, temp3) < IMS_OK)
	    {
	    	(void) ims_msg(msgDesc, IMS_ERROR,
	    	"Could not create medium record in CSA Reception report");
	    	(void) recrptTracking(msgDesc, fa_data);
	    	return(IMS_ERROR);
	    }
	} /* end of whle loop */
	(void) endQuery(msgDesc);

	/*
	** Failure Period Information.
	*/

  	temp2 = (CSA_STMT) create_keyword_value("NB_OF_FAILURE_PERIODS", "0");
  	append_statement(temp2, temp);

#if 0

  	temp2 = (CSA_STMT) create_keyword_value("FAILURE_PERIOD_INFO", "1");
  	temp2 = (CSA_STMT) create_ag(temp2);


  	temp3 = (CSA_STMT) create_keyword_value("FAILURE_PERIOD_NB", "1");
  	append_substatement(temp3, temp2);

  	temp3 = (CSA_STMT) create_keyword_value("REALTIME_PLAYBACK_FLG", "REALTIME");
  	append_substatement(temp3, temp2);

  	temp3 = (CSA_STMT) create_keyword_value("FAILURE_START", 
						"1995-300-12:26:23.800");
  	append_substatement(temp3, temp2);

  	temp3 = (CSA_STMT) create_keyword_value("FAILURE_STOP", 
						"1995-300-12:26:23.800");
  	append_substatement(temp3, temp2);

  	temp3 = (CSA_STMT) create_keyword_value("FAILURE_COMMENTS", 
				"Failure occurred in realtime receiver.");
  	append_substatement(temp3, temp2);

  	append_statement(temp2, temp);
#endif

  	temp2 = (CSA_STMT) create_comment(";###END_OF_FILE"); /* PR# 2425 */
  	append_statement(temp2, temp);
	/*
	** Attach root
	*/

  	append_statement(temp, tree_root);

	/*
	** Output the tree to the specified file.
	*/

	ims_concatFilePath(tempname, fullPathName, filename);
	strcpy(fullPathName, tempname);

  	if (print_tree_2(tree_root, fullPathName) < 0) { /* PR# 2425 change */
		ims_msg(msgDesc, IMS_ERROR,"Could not write out report file.");
  		delete_tree(tree_root);
		(void) recrptTracking(msgDesc, fa_data);
		return(IMS_ERROR);
	}

	fflush(NULL);
  	delete_tree(tree_root);
	return(recrptTracking(msgDesc, fa_data));
} /* end od ims_csa_recrpt */

/* PR# 2425 */
void replace_T(char *s, int len)
{
int i;

	for (i = 0; i < len; i++)
	{
		if (s[i] == 'T')
			break;
  }
	s[i] = '-';
}

