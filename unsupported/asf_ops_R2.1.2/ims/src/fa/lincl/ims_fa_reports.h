/*****************************************************************************
*
**
** File:    ims_fa_report.h
**
** Function: Include file for FA report system. 
**
** Author: Dan Crichton
**
** Date:    11/27/95
**
**
*****************************************************************************/

#ifndef IMS_FA_REPORTS_H

#define IMS_FA_REPORTS_H

static char *sccsFAReports = "@(#)ims_fa_reports.h	5.8 05/13/97";


/* 
** Report Definitions
*/

#define IMS_MAX_REPORTS         14

#define IMS_CSA_ARCHSTRGRPT     1
#define IMS_CSA_RECRPT          2
#define IMS_CSA_CALIBAVAILRPT   3

#define IMS_ESA_RESM			4
#define IMS_ESA_REAQ			5
#define IMS_ESA_REEX			6

#define IMS_NASDA_CATA			7	
#define IMS_NASDA_REAC			8	
#define IMS_NASDA_MSGM			9	

#define IMS_ADEOS_REAC			10
#define IMS_ADEOS_SRRD			11

#define IMS_ESA2_RESM			12
#define IMS_ESA2_REAQ			13
#define IMS_ESA2_REEX			14




#define IMS_MAX_SHIP_TAPES 		30


/*
** Period information for ims_fa_scheduler
*/

#define	IMS_FA_NOT_PERIODIC		0
#define IMS_FA_EVERYDAY			1
#define	IMS_FA_EVERYWEEK		2
#define IMS_FA_EVERYMONTH		4
#define	IMS_FA_LASTMONTHDAY		8

/*
** Event information for ims_fa_scheduler
*/

#define IMS_FA_TIMEBASED		1
#define	IMS_FA_MESSAGE			2
#define	IMS_FA_END_OF_DAY		4
#define IMS_FA_PASS_CHANGE		8
#define IMS_RESET_COUNTER		16
#define IMS_FA_TIMEOUT_DOWNLINK 32


/*
** Message information for ims_fa_scheduler
*/

#define IMS_FA_NO_MESSAGE		0
#define IMS_FA_DOWNLINK			1
#define IMS_FA_SCANNED_RESULTS	2
#define IMS_FA_PCD				4
#define IMS_FA_TAPE_AVAIL		8

/*
** Define Dataset Constants
*/

#define IMS_FA_DL_A1			"ADEOS-1 RAW SIGNAL SEGMENT"
#define IMS_FA_DL_E1			"ERS-1 RAW SIGNAL SEGMENT"
#define IMS_FA_DL_E2			"ERS-2 RAW SIGNAL SEGMENT"
#define IMS_FA_DL_J1			"JERS-1 RAW SIGNAL SEGMENT"
#define IMS_FA_DL_R1			"RADARSAT-1 RAW SIGNAL SEGMENT"

#define IMS_FA_SR_J1			"JERS-1 SAR FRAMES"


#define IMS_FA_TA_A1			"HC TAPE AVAILABILITY"
#define IMS_FA_TA_E1			"HC TAPE AVAILABILITY"
#define IMS_FA_TA_E2			"HC TAPE AVAILABILITY"
#define IMS_FA_TA_J1			"HC TAPE AVAILABILITY"
#define IMS_FA_TA_R1			"HC TAPE AVAILABILITY"

#define IMS_FA_TI_A1			"ADEOS-1 TRACKING INGEST"
#define IMS_FA_TI_E1			"ERS-1 TRACKING INGEST"
#define IMS_FA_TI_E2			"ERS-2 TRACKING INGEST"

/*
** Define dataset map
*/

#define IMS_DATASET_R1_ASR		169
#define IMS_DATASET_R1_RECRPT	170
#define IMS_DATASET_E1_REAQ		160
#define IMS_DATASET_E1_REEX		164
#define IMS_DATASET_E1_RESM		162
#define IMS_DATASET_E2_REAQ		161
#define IMS_DATASET_E2_REEX		165
#define IMS_DATASET_E2_RESM		163
#define IMS_DATASET_J1_MSGM		168
#define IMS_DATASET_J1_REAC		167
#define IMS_DATASET_J1_CATA		166
#define IMS_DATASET_A1_REAC		172
#define IMS_DATASET_A1_SRRD		171


/*
** Define Spacecraft Constants
*/

#define IMS_FA_A1		"ADEOS-1"
#define IMS_FA_E1		"ERS-1"
#define IMS_FA_E2		"ERS-2"
#define IMS_FA_J1		"JERS-1"
#define IMS_FA_R1		"RADARSAT-1"

/*
** Define interface structure from client to FA jobs
*/

typedef struct 
{
	char date_start[IMS_DATETIME_LEN+1];
	char date_end[IMS_DATETIME_LEN+1];
	int tape_count;
	char tape_num[IMS_COL15_LEN+1][IMS_MAX_SHIP_TAPES];
	char tape_date[IMS_COL10_LEN+1][IMS_MAX_SHIP_TAPES]; /* YYYYMMDD */
	char platform[IMS_COL15_LEN+1];
	int report_counter;
	int pass;
	int start_rev;
	int end_rev;
	char plan_num[IMS_COL15_LEN+1];
	IMS_JOB_USER_SPEC jobSpec;
	int seq_num;
	char station_id[IMS_COL15_LEN+1];
	char archive_name[IMS_PATH_LEN + 1];
} IMS_FA_INTERFACE;


typedef struct IMS_FA_PATH_LIST
{
	char *path;
	int start_granule_idx;
	int end_granule_idx;
	struct IMS_FA_PATH_LIST *next;
} IMS_FA_PATH_LIST;



int ims_submitReport(IMS_MSG_STRUCT *, IMS_JOB_USER_SPEC *, 
		int, int, void (* ) (), int, char *);

int getGranuleTableName(IMS_MSG_STRUCT *,  char *, char *, char *);

int loadDatasetPaths(IMS_MSG_STRUCT *, char *, char *, IMS_FA_PATH_LIST **);
int freeDatasetPaths(IMS_MSG_STRUCT *, IMS_FA_PATH_LIST *);
int locateRepositoryFile(IMS_MSG_STRUCT *, IMS_FA_PATH_LIST *,
	char *, char *, char *, int, char *);

void byte_swap_int(int *);
void byte_swap_word(short int *);
int getStorageNumber(IMS_MSG_STRUCT *, char *, char *, char *, int);


#endif
