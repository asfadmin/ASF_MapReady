
/************************************************************************* 
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File :  ims_step.h 
**
** Function: This is the header file for the IMS routines to get next step.
**
** Creator: Julie Wang
**
** Date:    June 13, 1995 
**
*************************************************************************/

#ifndef _IMS_STEP_H
#define _IMS_STEP_H 

static char *sccsstep = "@(#)ims_step.h	5.5  10/24/97";
 
/*
** general purpose value list structure
*/
typedef struct step_value_list
{
	DBSMALLINT   smallint_value;
	DBREAL       real_value;
	DBCHAR       char_value1[IMS_COL255_LEN+1];
	DBCHAR       char_value2[IMS_COL255_LEN+1];
	DBCHAR       char_value3[IMS_COL255_LEN+1];
	DBCHAR       char_value4[IMS_COL255_LEN+1];
	DBCHAR       char_value5[IMS_COL255_LEN+1];
	struct step_value_list *next_p;
} STEP_VALUE_LIST;
			 

/*
** structure to receive step handling related information from ims_catalog
*/
typedef struct item_info_list
{
	DBSMALLINT    order_item_type;
	DBSMALLINT    media_class;
	DBSMALLINT    media_type;
	DBSMALLINT    process_type;
	DBCHAR        step_name[IMS_COL30_LEN+1];
	DBSMALLINT    step_sequence;
	DBSMALLINT    status;
	DBCHAR        step_started_p[IMS_COL10_LEN+1];
	DBSMALLINT    quantity;
	DBSMALLINT    priority;
	DBCHAR        quicklook_p[IMS_COL10_LEN+1];
	DBREAL        cost;
	DBCHAR        account_id[IMS_COL20_LEN+1];
	DBSMALLINT    process_status;
	DBSMALLINT    media_fmt_type;
	DBCHAR        v0_process_type[IMS_COL30_LEN+1];
	struct item_info_list  *next_p;
} ITEM_INFO_LIST;

/*
** structure to store all possible values required to submit any request
** to PPS
*/
typedef struct pps_req_struct
{
	int           dataset_idx;
	char          granules_table[IMS_COL30_LEN+1];
	int           temporal_type;
	int           granule_idx;
	int           priority;
	char          char_priority[IMS_COL10_LEN+1];
	char          quicklook_p[IMS_COL10_LEN+1];
	char          output_format[IMS_COL20_LEN+1];   /* i.e. media_fmt_type */
	int           process_type;
	char          v0_process_type[IMS_COL30_LEN+1];
	char          platform[IMS_COL20_LEN+1];
	char          sensor[IMS_COL255_LEN+1];
	int           rev;
	char          mode[IMS_COL10_LEN+1];
	int           sequence;
	char          activity_id[IMS_COL20_LEN+1];
	int           frame_id;
	int           subframe_id;
	char          compensation_p[IMS_COL10_LEN+1];
	char          frame_mode[IMS_COL20_LEN+1];
	char          media_type[IMS_COL20_LEN+1];      /* disk or tape */
	char          media_id_type[IMS_COL10_LEN+1];
	char          media_id[IMS_COL20_LEN+1];
	char          media_location[IMS_COL255_LEN+1];
	char          data_direction[IMS_COL20_LEN+1];
	int           start_address;
	int           end_address;
	char          start_time[IMS_COL30_LEN+1];
	char          end_time[IMS_COL30_LEN+1];
	char          center_time[IMS_COL30_LEN+1];
	char          recorder_id[IMS_COL20_LEN+1];
	char          station_id[IMS_COL20_LEN+1];
	char          site_name[IMS_COL30_LEN+1];
	char          product_type[IMS_COL20_LEN+1];
	float         pixel_spacing;
	char          projection[IMS_COL20_LEN+1];
	int           processing_gain;
	float         avg_terrain_ht;
	char          deskew_p[IMS_COL10_LEN+1];
	float         ps_reference_lat;
	float         ps_reference_lon;
	int           utm_zone;
	char          terrain_correct_p[IMS_COL10_LEN+1];
	float         lambert_lat_n;
	float         lambert_lat_s;
}PPS_REQ_STRUCT;

/* structure to store all required information for step handling */
typedef struct step_info_struct
{
	long          order_id; 
	short         item_id;
	int           order_item_type;
	char          order_type[IMS_COL10_LEN+1];
	int           media_class;
	int           media_type;
	int           media_fmt_type;
	int           process_type;
	char          v0_process_type[IMS_COL30_LEN+1];
	char          step_name[IMS_COL30_LEN+1];
	int           step_sequence;
	int           status;
	char          step_started_p[IMS_COL10_LEN+1];
	int           start_status;
	int           end_status;
	int           quantity;
	int           priority;
	char          quicklook_p[IMS_COL10_LEN+1];
	char          curr_time[IMS_COL30_LEN+1];
	float         cost;
	char          account_id[IMS_COL15_LEN+1];
	int           process_status;
	char          op_comment[IMS_COL255_LEN+1];
	int           dce_err_flag;
	int           dataset_idx;
	int           granule_idx;
	char          platform[IMS_COL15_LEN];
	int           start_rev;
	int           end_rev;
	char          start_time[IMS_COL30_LEN];
	char          end_time[IMS_COL30_LEN];
	char          sv_precision[IMS_COL15_LEN];
	PPS_REQ_STRUCT pps_req;
	/* DISP_FULL_LIST *list_item; */
} STEP_INFO;

/* a structure to receive basic information for submitting PPS requests */
typedef struct pps_basic_info 
{
	DBSMALLINT     dataset_idx;
	DBINT          granule_idx;
	DBSMALLINT     output_format;
	struct pps_basic_info *next_p;
}PPS_BASIC_INFO;

typedef struct rfc_process_list
{
	DBCHAR         product_type[IMS_COL20_LEN+1];
	DBFLT8         pixel_spacing;
	DBCHAR         projection[IMS_COL20_LEN+1];
	DBINT          processing_gain;
	DBFLT8         avg_terrain_ht;
	DBCHAR         deskew_p[IMS_COL10_LEN+1];
	DBREAL         ps_reference_lat;
	DBREAL         ps_reference_lon;
	DBINT          utm_zone;
	DBCHAR         terrain_correct_p[IMS_COL10_LEN+1];
	DBREAL         lambert_lat_n;
	DBREAL         lambert_lat_s;
	DBSMALLINT     subframe_id;
	DBCHAR         compensation_p[IMS_COL10_LEN+1];
	struct rfc_process_list *next_p;
}RFC_PROCESS_LIST;

typedef struct rfc_gnul_list
{
	DBCHAR         platform[IMS_COL20_LEN+1];
	DBCHAR         sensor[IMS_COL255_LEN+1];
	DBINT          rev;
	DBCHAR         mode[IMS_COL10_LEN+1];
	DBSMALLINT     sequence;
	DBSMALLINT     frame_id;
	DBCHAR         frame_mode[IMS_COL20_LEN+1];
	DBCHAR         media_id[IMS_COL20_LEN+1];
	DBCHAR         station_id[IMS_COL20_LEN+1];
	DBCHAR         start_time[IMS_COL30_LEN+1];
	DBCHAR         end_time[IMS_COL30_LEN+1];
	DBCHAR         center_time[IMS_COL30_LEN+1];
	DBCHAR         activity_id[IMS_COL20_LEN+1];
	DBCHAR         scan_results_file[IMS_COL128_LEN+1];
	struct rfc_gnul_list *next_p;
}RFC_GNUL_LIST;

typedef struct tsr_info_list
{
	DBCHAR         platform[IMS_COL20_LEN+1];
	DBCHAR         sensor[IMS_COL255_LEN+1];
	DBINT          rev;
	DBCHAR         mode[IMS_COL10_LEN+1];
	DBSMALLINT     sequence;
	DBCHAR         activity_id[IMS_COL20_LEN+1];
	DBCHAR         frame_mode[IMS_COL20_LEN+1];
	DBCHAR         media_type[IMS_COL20_LEN+1];
	DBCHAR         media_id[IMS_COL20_LEN+1];
	DBINT          start_address;
	DBINT          end_address;
	DBCHAR         recorder_id[IMS_COL20_LEN+1];
	DBCHAR         station_id[IMS_COL20_LEN+1];
	DBCHAR         site_name[IMS_COL20_LEN+1];
	DBCHAR         start_time[IMS_COL30_LEN+1];
	DBCHAR         end_time[IMS_COL30_LEN+1];
	DBCHAR         media_location[IMS_COL255_LEN+1];
	DBCHAR         data_direction[IMS_COL20_LEN+1];
	struct tsr_info_list *next_p;
}TSR_INFO_LIST;

/* structure to receive information for the next step */
typedef struct next_step_info_struct
{
	DBSMALLINT     step_sequence;
	DBCHAR         step_name[IMS_COL30_LEN+1];
	DBSMALLINT     start_status;
	DBSMALLINT     end_status;
	struct next_step_info_struct *next_p;
} NEXT_STEP_INFO;


/*
** Function declarations
*/

/* ims_step.c */
int ims_do_next_step (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, DISP_FULL_LIST *);

/* ims_stepMsgTree.c */
int ims_step_buildRFCTree (IMS_MSG_STRUCT *, STEP_INFO *, AGGREGATE);
int ims_step_buildTSRTree (IMS_MSG_STRUCT *, STEP_INFO *, AGGREGATE);
int ims_step_buildCancelTree (IMS_MSG_STRUCT *, STEP_INFO *, AGGREGATE);
int ims_step_buildSVTree (IMS_MSG_STRUCT *, STEP_INFO *, AGGREGATE);

#endif /* !_IMS_STEP_H */
