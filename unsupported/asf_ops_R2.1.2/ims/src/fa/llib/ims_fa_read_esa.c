static char *sccs = "@(#)ims_fa_read_esa.c	5.3 10/27/97";
/******************************************************************************
**
** File:	ims_fa_read_esa.c
**
** Function: Read FA Reports for the European Space Agency.
**
** Author: Dan Crichton	
**
** Date:	7/19/95
**
** Modification: D. Ting 10/14/97 PR 2674
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

typedef struct 
{
	char date[8];  	/* YYYYMMDD */
	char time[6];	/* HHMMSS */	

} IMS_ESA_DATE_TIME;

typedef struct 
{
	int days, msec;
} IMS_ESA_UTC;
/*
** Local Functions
*/

/****************************************************************************
**
** format_esa_header
**
** Format the ESA header string
****************************************************************************/
int format_esa_header(
	IMS_MSG_STRUCT *msgDesc,
	int report_id, 
	char *format_str,
	char *filename)
{
	char report_str[5];
	char report_date[7];
	char report_time[9];
	char dest_id[3];
  	IMS_NUMERIC_DATE dateDef;
	int counter;
	int year;


  	if (ims_getCurrentDate(msgDesc, &dateDef) < IMS_OK)
  	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date.");
		return(IMS_ERROR);
 	}

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
			strcpy(report_str, "RESM");
			strcpy(dest_id, "EC");
			counter = 1;
			break;

		case IMS_ESA_REAQ:
			strcpy(report_str, "REAQ");
			strcpy(dest_id, "CF");
			counter = 1;
			break;

		case IMS_ESA_REEX:
			strcpy(report_str, "REEX");
			strcpy(dest_id, "CF");
			counter = 1;
			break;
	}	

	sprintf(format_str, "%s_%s%s%s%04d.E1%s",report_str, 
			report_date, "FB",dest_id,  counter, report_time);

	sprintf(filename, "%s_%s%s%s%04d.E1",report_str, 
			report_date, dest_id, "FB", counter);
	return(IMS_OK);
			
}


/****************************************************************************
**
** read_resm_record
**
** Read a line for the ESA Storage Report.
****************************************************************************/

int read_resm_record(
	IMS_MSG_STRUCT *msgDesc, 
	char *line, 
	int count) 
{
	char ship_id[5], date[25];
	char fac_id[3], medium_id[9];
	char medium_type[3], orig_medium_id[9];
	char orig_medium_type[3], dataset_id[39];
	char remarks[61];

	strcpy(ship_id, "AAAA");
	strcpy(date, "1995-001:23:10:12.001");
	strcpy(fac_id, "FB");
	strcpy(medium_id, "4MM-TAPE");
	strcpy(medium_type, "TP");
	strcpy(orig_medium_id, "8MM-TAPE");
	strcpy(orig_medium_type, "LS");
	strcpy(dataset_id, "STATE VECTOR RESTITUTED");
	strcpy(remarks, "RAW DATA SHIPPED PER MEDIA SPECIFICATION");
	sprintf(line, "%-4s%-24s%-2s%-8s%-8s%-2s%-38s%-60s",
		ship_id, date, fac_id, medium_id, medium_type, orig_medium_id,
		orig_medium_type, dataset_id, remarks);
	
	return(IMS_OK);
}

/****************************************************************************
**
** ims_read_esa_resm
**
** Read ESA shipment report
****************************************************************************/

int ims_read_esa_resm(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[81];
	char line[150]; /* Per ESA document with end of record deliminator */
	FILE *fptr;
	int no_of_media = 1;
	int no_of_data_sets = 100;
	int i;
	short int record_length; 


#if 0
	if (format_esa_header(msgDesc, IMS_ESA_RESM, fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}
#endif

	/*
	** Open the report file.
	*/


	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}


	record_length = 31;

	if ((int) fread((void *) &fixed_header, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read ESA_RESM header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	
	ims_printf("fixed_header = %s\n", fixed_header, record_length);

	/*
	** Build variable data records 
	*/

	while (!feof(fptr))
	{
		int bytes;

		record_length = 149;

	  if ((int) fread((void *) &line, record_length, 1, fptr) < 1)
	  {
		  ims_msg(msgDesc, IMS_ERROR, "Could not read ESA_RESM data");
		  fclose(fptr);
		  return(IMS_ERROR);
	  }
		/*
		if (bytes  < 149)
			continue;
			*/

		printf("** Recording Medium Information ** \n");
		printf("Record Length = %d\n", record_length);
		ims_printf("line = %s\n", line, record_length);
	}

	fclose(fptr);
	return(IMS_OK);
	
}

/****************************************************************************
**
** read_esa_report_header
**
** Read X_REPORT_HEADER per ESA specification.
****************************************************************************/

int read_esa_report_header(
	IMS_MSG_STRUCT *msgDesc, 
	char *report,
	int counter, 
	int pass,
	int size,
	FILE *fptr) 
{

  	IMS_NUMERIC_DATE dateDef;
	struct 
	{
		IMS_ESA_UTC utc;
		char cmd_type[2];
		char src_of_update;
	    char sch_number[4]; /* 1000 * pass + sequential number */
		char cmd_number[4];
		char reserved[4];
		char report_id[4];
		char soft_desc[8];
		char report_size[4];
	} esa_report_hdr;

	int schedule_number;
	int report_size;
	

	if ((int) fread((void *) &esa_report_hdr, 39, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out report header");
		return(IMS_ERROR);
	}

	printf("** X_REPORT_HEADER **\n");
	printf("UTC Days = %d\n", esa_report_hdr.utc.days);
	printf("UTC MSECS = %d\n", esa_report_hdr.utc.msec);

	memcpy(&schedule_number, esa_report_hdr.sch_number, 4);
	printf("Schedule # = %d\n", schedule_number);

	memcpy(&report_size, esa_report_hdr.report_size, 4);
	printf("Report Size = %d\n", report_size); 
	return(IMS_OK);	
}


/****************************************************************************
**
** reaq_read_records
**
** Read the Application Data Record section of the REAQ report.
****************************************************************************/

int reaq_read_records(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr) 
{
	int counter = 1;
	int pass = 1;
	int size = 100;
	int num_ac_recs = 1;
	int num_tape_labels = 1;
	int i,x;

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

	/*
	** HDDT Label Definition.
	*/

	struct
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
	} hddt_label;

	int num_aq;
	IMS_ESA_UTC start1;
	IMS_ESA_UTC stop1;
	IMS_ESA_UTC start2;
	IMS_ESA_UTC stop2;
	IMS_ESA_UTC start3;
	IMS_ESA_UTC stop3;


	char end_of_record[2];


	printf("** APPLICATION DATA RECORDS **\n");


	/*
	** Writeout length of record 
	*/


	/*
	** Read Report Header portion of record.
	*/

	if (read_esa_report_header(msgDesc, "REAQ", counter, pass, size, fptr) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format REAQ record.");
		return(IMS_ERROR);
	}


	/*
	** Read # of acquisition PCD records
	*/
	if ((int) fread((void *) &num_ac_recs, sizeof(num_ac_recs), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out report record");
		return(IMS_ERROR);
	}

	printf("Number of Acquisition PCD Records = %d\n", num_ac_recs);

	/*
	** Read # of tape label records
	*/
	if ((int) fread((void *) &num_tape_labels, sizeof(num_tape_labels), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out report record");
		return(IMS_ERROR);
	}

	printf("Number of Tape Labels = %d\n", num_tape_labels);

	/*
	** Loop through the 1..n Acquisition PCD Summary 
	*/
	for (i = 0; i < num_ac_recs; i++)
	{
		printf("** PCD SUMMARY  **\n");


		if ((int) fread((void *) &hddr,  1, 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not read out report record");
			return(IMS_ERROR);
		}

		if ((int) fread((void *) &pcd_hdr,  sizeof(pcd_hdr), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not read out report record");
			return(IMS_ERROR);
		}

		printf("PCD UTC Days = %d\n", pcd_hdr.utc.days);
		printf("PCD UTC MSECS = %d\n", pcd_hdr.utc.msec);


		/* 
		** Write out 1..n PCD records
		*/

		printf("EXTRACTING %d PCD RECORDS\n", pcd_hdr.num_of_recs);

		for (x = 0; x < pcd_hdr.num_of_recs; x++)
		{
			if ((int) fread((void *) &pcd_rec,  sizeof(pcd_rec), 1, fptr) < 1)
			{
				ims_msg(msgDesc, IMS_ERROR, 
						"Could not read out pcd report record");
				return(IMS_ERROR);
			}

			printf("** PCD RECORDS ** \n");
			printf("valid flag = %d\n", pcd_rec.valid_flag);
			printf("carrier_lock = %d\n", pcd_rec.tcr.carrier_lock); 
			printf("agc level = %d\n", pcd_rec.tcr.agc_level); 
			printf("realtime ebit rate = %d\n", pcd_rec.tcr.realtime_ebit_rate);
			printf("playback ebit rate = %d\n", pcd_rec.tcr.playback_ebit_rate);
			printf("q_bit_clock_lock = %d\n", pcd_rec.tcr.q_bit_clock_lock);
			printf("i_bit_clock_lock = %d\n", pcd_rec.tcr.i_bit_clock_lock);
			printf("realtime lock = %d\n", pcd_rec.hdr_lock_signals.realtime_lock);
			printf("playback lock = %d\n", pcd_rec.hdr_lock_signals.playback_lock);
			printf("pcd_summary = %d\n", pcd_rec.pcd_summary);
		}
	}

	/* 
	** Write out HDDT Tape Label Records.
	*/
	for (i = 0; i < num_tape_labels; i++)
	{
		/*
		** Setup and read in HDDT Label
		*/

		printf("** HDDT Label ** \n");



		if ((int) fread((void *) &hddt_label,  sizeof(hddt_label), 1, fptr) < 1)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not read out report record");
			return(IMS_ERROR);
		}

		memcpy(&num_aq, hddt_label.num_acquisitions, 4);
		printf("Number of Acquisitions = %d\n", num_aq);
		ims_printf("Medium Id = %s\n", hddt_label.medium_id, 8);
		printf("Satellite Id = %d\n", hddt_label.sat_id);

		memcpy(&start1, hddt_label.start1, 8);
		memcpy(&stop1, hddt_label.stop1, 8);
		memcpy(&start2, hddt_label.start2, 8);
		memcpy(&stop2, hddt_label.stop2, 8);
		memcpy(&start3, hddt_label.start3, 8);
		memcpy(&stop3, hddt_label.stop3, 8);

		printf("Start 1 - Days = %d\n",start1.days);
		printf("Start 1 - Time (MSECS) = %d\n", start1.msec);
		printf("Stop 1 - Days = %d\n", stop1.days);
		printf("Stop 1 - Time (MSECS) = %d\n", stop2.msec);
		printf("Start 2 - Days = %d\n", start2.days);
		printf("Start 2 - Time (MSECS) = %d\n", start2.msec);
		printf("Stop 2 - Days = %d\n", stop2.days);
		printf("Stop 2 - Time (MSECS) = %d\n", stop2.msec);
		printf("Start 3 - Days = %d\n", start3.days);
		printf("Start 3 - Time (MSECS) = %d\n", start3.msec);
		printf("Stop 3 - Days = %d\n", stop3.days);
		printf("Stop 3 - Time (MSECS) = %d\n", stop3.msec);
		printf("Station Id = %d\n", hddt_label.station_id);
		printf("drive = %d\n", hddt_label.drive);
		printf("Demodulator = %d\n", hddt_label.demodulator);

	}


	return(IMS_OK);
}

/****************************************************************************
**
** ims_read_esa_reaq
**
** ESA Aquisition Report
****************************************************************************/

int ims_read_esa_reaq(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[80];
	char line[149]; /* Per ESA document */
	FILE *fptr;
	int no_of_media;
	int no_of_data_sets;
	int i;
	short int record_length; 
	struct {
		char mtype[2];
		char mid[4];
	} msg_hdr;
	char end_of_record[2];
	short int mtype;
	int mid;


	/*
	** Open the report file.
	*/

	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	record_length = 30;


	if ((int) fread((void *) &fixed_header, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read ESA_REAQ header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	ims_printf("Fixed_Header = %s\n", fixed_header, record_length);

	/*
	** Build/Write Variable Portion
	*/

	record_length = 6;
	if ((int) fread((void *) &msg_hdr, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read variable portion.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	memcpy(&mtype, msg_hdr.mtype, 2);
	memcpy(&mid, msg_hdr.mid, 4);


	printf("** Message Header **\n");

	printf("Message Type = %d\n", mtype);
	printf("Message Id = %d\n", mid);
	

	/*
	** Application Data Record.
	*/

	if (reaq_read_records(msgDesc, fptr) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read application data record");
		fclose(fptr);
		return(IMS_ERROR);
	}
	fclose(fptr);
	return(IMS_OK);
}


/****************************************************************************
**
** reex_read_record
**
** Read application data record portion of the ESA Extracted Data Report.
****************************************************************************/

int reex_read_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr) 
{

	struct
	{
		char satellite[2];
		char sensor[3];
		char orbit_no[5];
		char frame[4];
		char fac_id[2];
	} x_ump_entry_id;

	struct
	{
		char orbit_number[5];
		char fac_id[2];
		char orig;
	} sched_ref;

	IMS_ESA_DATE_TIME x_date;
	char reserved[10];  /* included is end of record */
	char raw_data;
	short int record_length;



	/*
	**  X_UMP_ENTRY_ID.
	*/

	if ((int) fread((void *) &x_ump_entry_id, sizeof(x_ump_entry_id), 1, fptr) < 1)
	{
		return(IMS_OK); /* Could be end of file */
	}

	ims_printf("X_UMP_ENTRY_ID = %s\n", &x_ump_entry_id, sizeof(x_ump_entry_id));	

	/*
	** schedule reference.
	*/

	if ((int) fread((void *) &sched_ref, sizeof(sched_ref), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out record.");
		return(IMS_ERROR);
	}

	ims_printf("Schedule Reference = %s\n", &sched_ref, sizeof(sched_ref));	


	/* 
	**  date time
	*/


	if ((int) fread((void *) &x_date, sizeof(x_date), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out record.");
		return(IMS_ERROR);
	}

	ims_printf("X_DATE = %s\n", (char *) &x_date, sizeof(x_date)); 

	/*
	** Raw data extraction
	*/


	if ((int) fread((void *) &raw_data, 1, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out record.");
		return(IMS_ERROR);
	}

	printf("Raw Data = %c\n", raw_data);

	/*
	** Add Reserved Portion...
	*/


	if ((int) fread((void *) &reserved, 10, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read out reserved record.");
		return(IMS_ERROR);
	}


	return(IMS_OK);

}

/****************************************************************************
**
** ims_read_esa_reex
**
** ESA Extracted Data Report
****************************************************************************/

int ims_read_esa_reex(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[80];
	char line[149]; /* Per ESA document */
	FILE *fptr;
	int i;
	short int record_length; 



	/*
	** Open the report file.
	*/

	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	record_length = 31;


	if ((int) fread((void *) &fixed_header, record_length, 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read ESA_REEX header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	ims_printf("fixed_header = %s\n", fixed_header, record_length);

	/*
	** Add Application Data Record section.
	*/

	while (!feof(fptr))
	{
		if (reex_read_record(msgDesc, fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not read ESA_REEX record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	fclose(fptr);
	return(IMS_OK);

}



