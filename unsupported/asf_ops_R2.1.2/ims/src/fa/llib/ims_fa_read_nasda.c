static char *sccs = "@(#)ims_fa_read_nasda.c	5.3  08/28/97";
/******************************************************************************
**
** File:	ims_fa_read_nasda.c
**
** Function: Read/Parse NASDA JERS-1 FA Report Files
**
** Author: Dan Crichton	
**
** Date:	7/18/95
**
** Modifications: D. Ting 8/28/97
**							  Changed read REAC report hddt_num[] size from 8 to 10
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

/*
** Local Functions
*/

/****************************************************************************
**
** read_format_jers_header
**
** Format the NASDA JERS-1 header string
****************************************************************************/
int read_jers_header(
	IMS_MSG_STRUCT *msgDesc,
	int report_id, 
	int *rec_tot,
	char *format_str,
	char *filename)
{
  	IMS_NUMERIC_DATE dateDef;
	IMS_JERS_HEADER *header;
	int counter;
	char date[9];
	char time[9];
	char asc_no_of_records[6];


	printf("***** HEADER *****\n");
	header = (IMS_JERS_HEADER *) format_str;

	ims_printf("filename = %s\n", header->filename, 8);
	ims_printf("project = %s\n", header->project, 4);
	ims_printf("send ground code = %s\n", header->send_grnd_code, 4);
	ims_printf("rec ground code = %s\n", header->rec_grnd_code, 4);
	ims_printf("gen date = %s\n", header->gen_date, 8);
	ims_printf("gen time = %s\n", header->gen_time, 8);
	ims_printf("file desc = %s\n", header->file_desc, 1);
	ims_printf("record length = %s\n", header->rec_len, 4);
	ims_printf("number of records = %s\n", header->no_of_records, 5);
	ims_printf("preliminary = %s\n", header->preliminary, 82);

	strcpy(filename, header->filename);

	*rec_tot = atoi(header->no_of_records); 
	return(IMS_OK);
}

/****************************************************************************
**
** read_cata_segments
**
** Read the segment sections of the CATA report.
****************************************************************************/

int read_cata_segments(IMS_MSG_STRUCT *msgDesc, int *no_of_scenes, FILE *fptr)
{
	struct 
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
	} segment;


	if ((int) fread((void *) &segment, sizeof(segment), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write CATA segment.");
		return(IMS_ERROR);
	}

	printf("** CATA SEGMENTS RECORD **\n");

	ims_printf("gendate = %s\n", segment.gendate, 8);
	ims_printf("aq_date = %s\n", segment.aq_date, 8);
	ims_printf("ob_date = %s\n", segment.ob_date, 8);
	ims_printf("start_date = %s\n", segment.start_date, 14);
	ims_printf("end_date = %s\n", segment.end_date, 14);
	ims_printf("ground code = %s\n", segment.grnd_code, 4);
	ims_printf("acqusition mode = %s\n", segment.aq_mode, 1);
	ims_printf("segment id = %s\n", segment.segment_id, 11);
	ims_printf("number of scenes = %s\n", segment.no_of_scenes, 4);
	ims_printf("hddr_no = %s\n", segment.hddr_no, 1);
	ims_printf("hddt_no = %s\n", segment.hddt_no, 8);
	ims_printf("record time = %s\n", segment.record_time, 18);
	ims_printf("tape position = %s\n", segment.tape_pos, 3);
	ims_printf("tape_cntr = %s\n", segment.tape_cntr, 4);
	ims_printf("hddt date = %s\n", segment.hddt_date, 8);
	ims_printf("op disk date = %s\n", segment.op_disk_date, 8);
	ims_printf("ql id = %s\n", segment.ql_id, 8);
	ims_printf("qual_code = %s\n", segment.qual_code, 1);
	ims_printf("abnormal_code = %s\n", segment.abnormal_code, 2);
	ims_printf("preliminary = %s\n", segment.preliminary, 123);

	*no_of_scenes = (int) atoi((char *) segment.no_of_scenes);

	return(IMS_OK);	 
}

/****************************************************************************
**
** read_cata_scenes
**
** Read the scene sections of the CATA report.
****************************************************************************/

int read_cata_scenes(IMS_MSG_STRUCT *msgDesc, FILE *fptr)
{
	struct 
	{
		char grs[6];
		char rsp[9];
		char orbit[5];
		char ascend;
		char solar_angle[6];
		char start_time[10]; 	/* DDDHHMMSSs where s = 100th m.s. */
		char center_time[10];
		struct
		{
			char center_lat[7];
			char center_lon[8];
			char ul_lat[7];
			char ul_lon[8];
			char ur_lat[7];
			char ur_lon[8];
			char ll_lat[7];
			char ll_lon[8];
			char lr_lat[7];
			char lr_lon[8];
		} scene_info;
		char sensor_cond[7];
		char cloud[6];
		char addr_of_ql[5];		/* Blank, NASDA use. */
		char record_state[8];
		char no_lost_lines[4];
		char qual_code;
		char preliminary[103];
	} scene;


	if ((int) fread((void *) &scene, sizeof(scene), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read CATA scene.");
		return(IMS_ERROR);
	}

	printf("** CATA SCENES RECORD **\n");

	ims_printf("grs = %s\n", scene.grs, 6);
	ims_printf("rsp = %s\n", scene.rsp, 9);
	ims_printf("orbit = %s\n", scene.orbit, 5);
	printf("ascend = %c\n", scene.ascend);
	ims_printf("solar_angle = %s\n", scene.solar_angle, 6);
	ims_printf("start_time = %s\n", scene.start_time, 10);
	ims_printf("center_time = %s\n", scene.center_time, 10);
	ims_printf("center lat = %s\n", scene.scene_info.center_lat, 7);
	ims_printf("center lon = %s\n", scene.scene_info.center_lat, 8);
	ims_printf("ul lat = %s\n", scene.scene_info.ul_lat, 7);
	ims_printf("ul lon = %s\n", scene.scene_info.ul_lon, 8);
	ims_printf("ur lat = %s\n", scene.scene_info.ur_lat, 7);
	ims_printf("ur lon = %s\n", scene.scene_info.ur_lon, 8);
	ims_printf("ll lat = %s\n", scene.scene_info.ll_lat, 7);
	ims_printf("ll lon = %s\n", scene.scene_info.ll_lon, 8);
	ims_printf("lr lat = %s\n", scene.scene_info.lr_lat, 7);
	ims_printf("lr lon = %s\n", scene.scene_info.lr_lon, 8);
	ims_printf("sensor_cond = %s\n", scene.sensor_cond, 7);
	ims_printf("cloud = %s\n", scene.cloud, 6);
	ims_printf("addr_of_ql = %s\n", scene.addr_of_ql, 5);
	ims_printf("record_state = %s\n", scene.record_state, 8);
	ims_printf("no_lost_lines = %s\n", scene.no_lost_lines, 4);
	ims_printf("qual_code = %s\n", scene.qual_code, 1);
	ims_printf("preliminary = %s\n", scene.preliminary, 103);
	 
	return(IMS_OK);	 
}

/****************************************************************************
**
** ims_read_jers_cata
**
** JERS-1 Catalog Report
****************************************************************************/

int ims_read_jers_cata(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[128];
	FILE *fptr;
	struct
	{
		char open_date[8];
		char close_date[8];
		char no_catalog[5];
	} file_desc;

	int i, j;
	int no_of_segments;
	int no_of_scenes;




	/*
	** Open the report file.
	*/


	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fread((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read CATA header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	if (read_jers_header(msgDesc, IMS_NASDA_CATA, &no_of_segments, 
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}


	/*
	** Build File Descriptor Record.
	*/

	
	if ((int) fread((void *) &file_desc, sizeof(file_desc), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read CATA file descriptor.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	/*
	** Extract 1..n segments
	*/

    for (i = 0; i < no_of_segments; i++)
	{
		read_cata_segments(msgDesc, &no_of_scenes, fptr);

		/*
		** Extract 1..n scenes
		*/

		for (j = 0; j < no_of_scenes; j++)
		{
			read_cata_scenes(msgDesc, fptr);
		}
	}
	

	fclose(fptr);
	return(IMS_OK);
	
}



/****************************************************************************
**
** read_reac_record
**
** Read the REAC record.
****************************************************************************/

int read_reac_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr)
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


	if ((int) fread((void *) &acq_result, sizeof(acq_result), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read REAC record");
		fclose(fptr);
		return(IMS_ERROR);
	}

	printf("** NASDA REAC RECORD **\n");
	ims_printf("aq_date = %s\n", acq_result.aq_date, 8);
	ims_printf("plan number = %s\n", acq_result.plan_num, 6);
	ims_printf("path number = %s\n", acq_result.path_number, 3);
	ims_printf("HDDT Freq = %s\n", acq_result.hddt.freq, 2);
	ims_printf("HDDT Store Number = %s\n", acq_result.hddt.store_num, 1);
	ims_printf("HDDT Counter = %s\n", acq_result.hddt.counter, 4);
	ims_printf("HDDT Number = %s\n", acq_result.hddt.hddt_num, 10);
	ims_printf("HDDT Record Date = %s\n", acq_result.hddt.record_date, 18);

	return(IMS_OK);

}

/****************************************************************************
**
** ims_jers_reac
**
** JERS-1 Aquisition Result Report
****************************************************************************/

int ims_read_jers_reac(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[128];
	FILE *fptr;
	struct
	{
		char duration[16];
	} file_desc;
	int acq_result_total = 2;
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

	if ((int) fread((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not write REAC header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	if (read_jers_header(msgDesc, IMS_NASDA_REAC, &acq_result_total, 
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}

	/*
	** Build File Descriptor Record.
	*/

	
	if ((int) fread((void *) &file_desc, sizeof(file_desc), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read REAC file descriptor.");
		fclose(fptr);
		return(IMS_ERROR);
	}

	printf("** FILE DESCRIPTOR **\n");
	ims_printf("File Descriptor = %s\n", file_desc.duration, 16);

	/*
	** 1..n Acquisition Result 
	*/

	for (i = 0; i < acq_result_total; i++)
	{
		if (read_reac_record(msgDesc, fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format REAC record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	fclose(fptr);
	return(IMS_OK);

}


/****************************************************************************
**
** read_msgm_record
**
** Format the msgm shipment record.
****************************************************************************/

int read_msgm_record(
	IMS_MSG_STRUCT *msgDesc, 
	FILE *fptr)
{

	struct  
	{
		char ship_date[8];
		char hddt_no[8];
	} msgm_data;


	if ((int) fread((void *) &msgm_data, sizeof(msgm_data), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read MSGM record");
		fclose(fptr);
		return(IMS_ERROR);
	}

	printf("** MSGM RECORD **\n");
	
	ims_printf("Shipment Date = %s\n", msgm_data.ship_date);
	ims_printf("HDDT Number = %s\n", msgm_data.hddt_no);

	return(IMS_OK);

}


/****************************************************************************
**
** ims_read_jers_msgm
**
** JERS-1 Shipment Report
****************************************************************************/

int ims_read_jers_msgm(
	IMS_MSG_STRUCT *msgDesc,
	char *filename) 
{
	char fixed_header[128];
	FILE *fptr;
	int i;
	int num_ship_records = 2;



	/*
	** Open the report file.
	*/

	strcpy(filename, "MSGM");

	fptr = fopen(filename, "rb");

	if (fptr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not open report file %s.", filename);
		return(IMS_ERROR);
	}

	if ((int) fread((void *) &fixed_header, sizeof(IMS_JERS_HEADER), 1, fptr) < 1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not read REAC header");
		fclose(fptr);
		return(IMS_ERROR);
	}

	if (read_jers_header(msgDesc, IMS_NASDA_MSGM, &num_ship_records, 
							fixed_header, filename) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not format fixed header");
		return(IMS_ERROR);
	}


	/*
	** Format 1..n medium shipment report records
	*/

	for (i = 0; i < num_ship_records; i++)
	{
		if (read_msgm_record(msgDesc, fptr) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, "Could not format REAC record.");
			fclose(fptr);
			return(IMS_ERROR);
		}
	}

	fclose(fptr);
	return(IMS_OK);

}
