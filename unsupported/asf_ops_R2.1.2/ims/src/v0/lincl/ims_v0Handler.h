/************************************************************************* 
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
** 
** File :  ims_v0Handler.h 
**
** Function: This is the header file for the IMS V0 server application.
**
** Creator: Hoshyar Sayah, Julie Wang
**
** Date:    April 28, 1994
**
*************************************************************************/

#ifndef _IMS_V0HANDLER_H
#define _IMS_V0HANDLER_H

static char *sccsv0Handler = "@(#)ims_v0Handler.h	5.13  09/05/97";
 
/*
** Enumerated list for requests
*/
typedef enum v0_msg_type
{
	V0_UNIDENTIFIED, 
	V0_INVENTORY_SEARCH, V0_INVENTORY_RESULT, 
	V0_DIRECTORY_SEARCH, V0_DIRECTORY_RESULT, 
	V0_DAR_ACCT_SEARCH, V0_DAR_ACCT_RESULT,
	V0_DAR_REQUEST, V0_DAR_RESULT, 
	V0_PRODUCT_REQUEST, V0_PRODUCT_RESULT, 
	V0_SCAN_REQUEST, 
	V0_BROWSE_REQUEST,
	V0_DAR_LIST_QUERY, V0_DAR_LIST_RESULT,
	V0_DAR_STATUS_QUERY, V0_DAR_STATUS_RESULT,
	V0_DAR_CONTENT_QUERY, V0_DAR_CONTENT_RESULT,
	V0_DAR_GRANULE_QUERY, V0_DAR_GRANULE_RESULT,
	V0_ACKNOWLEDGE, V0_QUIT, V0_ABORT
} V0_MSG_TYPE;

/*
** Enumerated types of spatial regions
*/
typedef enum v0_region_type
{ 
	V0_UNIDENTIFIED_REGION, POINT_LOC, RANGE_LOC, POLYGON_LOC
} V0_REGION_TYPE;

/*
** Enumerated types for order_item_type 
*/
typedef enum ims_order_item_type
{
	APR_TYPE             = 1,            /* Archive Production Request       */
	RPR_TYPE             = 2,            /* Retrospective Production Request */
	FPR_TYPE             = 3,            /* Future Production Request        */
	COR_TYPE             = 4,            /* CSA Order Request                */
	TDR_TYPE             = 5,            /* Tape Dub Request                 */
	TSR_TYPE             = 6,            /* Tape Scan Request                */
	DAR_TYPE             = 7             /* Data Acquisition Request         */
}IMS_ORDER_ITEM_TYPE;

/*
** Enumerated list for DAR statuses 
*/
typedef enum ims_dar_status
{
	DAR_NEW              = 1,
	DAR_VALIDATED        = 2,
	DAR_IN_PLANNING      = 3,
	DAR_REJECTED         = 4,
	DAR_COMPLETED	     = 5
}IMS_DAR_STATUS_TYPE;

/*
** Enumerated list for order_queue statuses 
*/
typedef enum ims_oq_status
{
	OQ_NEW              =  1,
	OQ_PENDING          =  2,
	OQ_GENERATED        =  3,
	OQ_COMPLETE         =  4,
	OQ_CANCEL           = 11,
	OQ_CANCELLED        = 12,
	OQ_ERROR            = 13,
	OQ_FAILED           = 14,
	OQ_HOLD             = 15,
	OQ_LOCKED           = 20
}IMS_OQ_STATUS;

/*
** Enumerated list for v0_message statuses
*/
typedef enum ims_v0_msg_status
{
        V0_RECEIVED         =  1,
        V0_COMPLETED        =  2,
        V0_FAILED           =  3
}IMS_V0_MSG_STATUS;

/*
** Enumerated list for query types 
*/
typedef enum ims_query_type
{
	START_TIME_TYPE      =  1,
	END_TIME_TYPE        =  2,
	CENTER_TIME_TYPE     =  3,
	CENTER_LAT_TYPE      =  4,
	CENTER_LON_TYPE      =  5,
	NEAR_START_LAT_TYPE  =  6,
	NEAR_START_LON_TYPE  =  7,
	NEAR_END_LAT_TYPE    =  8,
	NEAR_END_LON_TYPE    =  9,
	FAR_START_LAT_TYPE   = 10,
	FAR_START_LON_TYPE   = 11,
	FAR_END_LAT_TYPE     = 12,
	FAR_END_LON_TYPE     = 13,
	NORTH_LAT_TYPE       = 14,
	SOUTH_LAT_TYPE       = 15,
	WEST_LON_TYPE        = 16,
	EAST_LON_TYPE        = 17,
	ASC_DESC_TYPE        = 18,
	FRAME_STATUS_TYPE    = 19, 
	DIR_PATH_TYPE        = 20,
	DETAILED_DATA_TYPE   = 21
}IMS_QUERY_TYPE;

/*
** Enumerated list for dataset info types
*/
typedef enum ims_ds_info_type
{
	DS_INFO_COMMENT      = 1,
	DS_INFO_RESTRICTION  = 2
}IMS_DS_INFO_TYPE;

/*
** general purpose value list structure
*/
typedef struct v0_value_list
{
	DBCHAR        char_value1[IMS_COL255_LEN+1];/* string */
	DBCHAR        char_value2[IMS_COL255_LEN+1];/* string */
	DBCHAR        char_value3[IMS_COL255_LEN+1];   /* string */
	DBTINYINT     tinyint_value;                   /* 1-byte integer */
	DBFLT8        flt8_value;                      /* 8-byte float */
	DBREAL        real_value;                      /* 4-byte float */
	DBINT         int_value;                       /* 4-byte integer */
	DBSMALLINT    smallint_value;                  /* 2-byte integer */
	struct v0_value_list *next_p;
} V0_VALUE_LIST;

typedef struct v0_platform_list
{
	DBCHAR		platform[IMS_COL30_LEN+1];
	DBCHAR		acronym[IMS_COL10_LEN+1];	
	DBCHAR		dar_p[IMS_COL10_LEN+1];
	struct v0_platform_list *next_p;
} V0_PLATFORM_LIST;

typedef struct v0_err_list
{
	char         err_buf[IMS_COL255_LEN+1];
	struct v0_err_list *next_p;
}V0_ERR_LIST;

typedef struct v0_comment_list
{
	char         comment_info[IMS_COL1024_LEN*10+1];
	struct v0_comment_list *next_p;
}V0_COMMENT_LIST;

typedef struct v0_ds_info_list
{
	DBSMALLINT    ds_idx; 
	DBSMALLINT    order_item_type; 
	DBCHAR        granules_table[IMS_COL30_LEN+1];
	struct v0_ds_info_list *next_p;
} V0_DS_INFO_LIST;

typedef struct v0_proc_media_info_list
{
	DBSMALLINT    process_type; 
	DBSMALLINT    media_type; 
	DBSMALLINT    media_class; 
	DBSMALLINT    media_fmt_type; 
	DBCHAR        quicklook_p[IMS_COL10_LEN+1];
	struct v0_proc_media_info_list *next_p;
} V0_PROC_MEDIA_INFO_LIST;

typedef struct v0_account_info_list
{
	DBSMALLINT    resource_type; 
	DBREAL        rate_multiplier;
	DBCHAR        op_validate_p[IMS_COL10_LEN+1];
	struct v0_account_info_list *next_p;
} V0_ACCOUNT_INFO_LIST;

typedef struct v0_granule_info
{
	DBINT   granule_idx;
	DBINT   data_kbytes;
	DBINT   metadata_kbytes;
	struct v0_granule_info *next_p;
} V0_GRANULE_INFO;

typedef struct v0_line_item_list
{
	DBSMALLINT order_item_type;             /* APR, RPR, FPR ... */
	char  dataset_id[IMS_COL80_LEN+1];
	int   dataset_idx;
	char  granule_id[IMS_COL30_LEN+1];
	int   granule_idx;
	int   data_kbytes;
	int   metadata_kbytes;
	char  v0_process_type[IMS_COL30_LEN+1];
	int   process_type;
	char  v0_media_type[IMS_COL30_LEN+1];
	int   media_type;
	int   media_class;
	char  v0_media_fmt_type[IMS_COL30_LEN+1];
	int   media_fmt_type;
	char  media_label[IMS_COL30_LEN+1];
	float line_item_cost;
	char  account_id[IMS_COL30_LEN+1];
	char  platform[IMS_COL30_LEN+1]; 
	char  platform_alias[IMS_COL10_LEN+1];
	char  sensor_name[IMS_COL30_LEN+1]; 
	char  sensor_alias[IMS_COL10_LEN+1];
	char  pi_name[IMS_COL50_LEN+1];
	char  pi_discipline[IMS_COL40_LEN+1];
	char  quicklook_p[IMS_COL10_LEN+1];
	/* the following fields are used for processing scan request only */
	DBINT revolution;
	DBSMALLINT sequence;	
	char  time_on[IMS_COL30_LEN+1];
	char  time_off[IMS_COL30_LEN+1];
	char  mode[IMS_COL10_LEN+1];
	char  frame_mode[IMS_COL20_LEN+1];
	char  activity_id[IMS_COL20_LEN+1];
	char  station_id[IMS_COL20_LEN+1];
	char  site_name[IMS_COL20_LEN+1];
	struct v0_line_item_list *next_p; 
} V0_LINE_ITEM_LIST;

typedef struct v0_contact_info
{
	DBCHAR  contact_name[IMS_COL50_LEN+1];
	DBCHAR  organization[IMS_COL40_LEN+1];
	DBCHAR  street[IMS_COL128_LEN+1];
	DBCHAR  city[IMS_COL30_LEN+1];
	DBCHAR  state[IMS_COL20_LEN+1];
	DBCHAR  zipcode[IMS_COL10_LEN+1];
	DBCHAR  country[IMS_COL20_LEN+1];
	DBCHAR  phone[IMS_COL30_LEN+1];
	DBCHAR  fax[IMS_COL30_LEN+1];
	DBCHAR  email[IMS_COL128_LEN+1];
	struct v0_contact_info *next_p;
} V0_CONTACT_INFO;

typedef struct v0_profile_info
{
	char  organization[IMS_COL40_LEN+1];
	char  street[IMS_COL128_LEN+1];
	char  city[IMS_COL30_LEN+1];
	char  state[IMS_COL20_LEN+1];
	char  zipcode[IMS_COL10_LEN+1];
	char  country[IMS_COL20_LEN+1];
	char  phone[IMS_COL30_LEN+1];
	char  email[IMS_COL128_LEN+1];
	char  fax[IMS_COL30_LEN+1];
	struct v0_profile_info *next_p;
} V0_PROFILE_INFO;

typedef struct v0_profile_struct
{
	char  title[IMS_COL10_LEN+1];
	char  last_name[IMS_COL20_LEN+1];
	char  first_name[IMS_COL50_LEN+1];
	char  initial_name[IMS_COL10_LEN+1];
	char  organization[IMS_COL40_LEN+1];
	char  street[IMS_COL128_LEN+1];
	char  city[IMS_COL30_LEN+1];
	char  state[IMS_COL20_LEN+1];
	char  zipcode[IMS_COL10_LEN+1];
	char  country[IMS_COL20_LEN+1];
	char  phone[IMS_COL30_LEN+1];
	char  fax[IMS_COL30_LEN+1];
	char  email[IMS_COL128_LEN+1];
} V0_PROFILE_STRUCT;

typedef struct v0_user_affliation_struct
{
	char category[IMS_COL10_LEN+1];
	char type[IMS_COL15_LEN+1];
} V0_UA_STRUCT;

typedef struct v0_user_info_struct
{
	char  first_name[IMS_COL20_LEN+1];
	char  last_name[IMS_COL20_LEN+1];
} V0_USER_INFO_STRUCT;

/*
** Client/User Request Structure.
** Additions are made as needed.
*/
typedef struct v0_request_struct
{
	int           callerFlag;
	V0_VALUE_LIST *dataset_id;
	V0_VALUE_LIST *sensor;
	V0_VALUE_LIST *platform;
	V0_VALUE_LIST *parameter;
	V0_VALUE_LIST *campaign;
	V0_VALUE_LIST *process_level;
	V0_VALUE_LIST *granule_id;
	char browse_p[IMS_COL10_LEN+1];
	char start_time[IMS_COL30_LEN+1];      /* IMS datetime format */
	char start_time_mmdd[IMS_COL10_LEN+1]; /* mmdd */
	char start_time_year[IMS_COL10_LEN+1]; /* yyyy */
	char end_time[IMS_COL30_LEN+1];        /* IMS datetime format */
	char end_time_mmdd[IMS_COL10_LEN+1];   /* mmdd */
	char end_time_year[IMS_COL10_LEN+1];   /* yyyy */
	int  start_day_of_year;
	int  stop_day_of_year;
	char day_night[IMS_COL10_LEN+1];
	int  granule_limit;  
	int  select_all;
	char global_granules_p;                /* Y, or N */
	char global_search;                    /* Y, or N */
	V0_REGION_TYPE region_type;
	char pole_included;                    /* 'N', 'S', 'B', or ' ' */
	char mode[IMS_COL30_LEN+1];
	char site_name[IMS_COL60_LEN+1];
	enum Map_Projections map_projection_type;
	float tangent_latitude;
	float tangent_longitude;
	float u_lat[PT_MAX];             /* Coordinates specified by  user*/
	float u_lon[PT_MAX];             /* for POINT, or POLYGON searches */
	float u_min_lat;                 /* Boundaries specified by user for */
	float u_max_lat;                 /* RANGE_LOC*/
	float u_min_lon;                 /* longitude values are ranged -180/180 */ 
	float u_max_lon;                 /* for both u_lon[PT_MAX] and u_min_lon */
	                                 /* u_max_lon  */
	float minimum_latitude;          /* v0 server calculated search boundaries*/
	float maximum_latitude;          /* for pass1 spatial search */
	float minimum_longitude;         
	float maximum_longitude;
	int   cross_180;                 /* set to 1 if the user area crosses 180 */
	int   north_pole_only;           /* set to 1 if the area is a point and   */
	int   south_pole_only;           /* located exactly on 90/-90 latitude    */
	char  asc_desc[IMS_COL10_LEN+1];
	char  spatial_type; 
	float center_lat;
	float center_lon;
	float radius;
	float nw_lat;
	float nw_lon;
	float ne_lat;
	float ne_lon;
	float se_lat;
	float se_lon;
	float sw_lat;
	float sw_lon;
	char  order_request_id[IMS_COL60_LEN+1];
	char  user_email[IMS_COL128_LEN+1];
	V0_VALUE_LIST *data_center_id;
	int   order_id;
	int   item_id;
	int   item_count;                     /* count of line item within an order*/
	char  authenticator[IMS_COL60_LEN+1]; /* an encrypted passwd recvd from v0 */
	char  user_id[IMS_COL30_LEN+1];
	int   priority;
	int   dar_priority;
	int   dar_status;
	float rate_multiplier;
	int   resource_type;
	char  account_id[IMS_COL30_LEN+1];
	char  op_validate_p[IMS_COL10_LEN+1];
	char  quicklook_p[IMS_COL10_LEN+1];
	int   observation_num;
	char  observation_freq[IMS_COL30_LEN+1];
	char  active_p[IMS_COL10_LEN+1];
	char  activity_start_date[IMS_COL60_LEN+1];
	char  activity_end_date[IMS_COL60_LEN+1];
	char  dar_comment[IMS_COL512_LEN+1];
	float total_cost; 
	V0_UA_STRUCT      user_affiliation;
	int   contact_p;                  /* set to 1 if CONTACT_ADDRESS provided */
	int   shipping_p;                 /* set to 1 if SHIPPING_ADDRESS provided*/
	int   billing_p;                  /* set to 1 if BILLING_ADDRESS provided */
	int   user_aborted_p;
	V0_PROFILE_STRUCT contact_profile;
	V0_PROFILE_STRUCT shipping_profile;
	V0_PROFILE_STRUCT billing_profile;
	V0_LINE_ITEM_LIST *line_item_list;
	V0_LINE_ITEM_LIST *curr_line_item;
	V0_USER_INFO_STRUCT user_info;
} V0_REQUEST_STRUCT;


/*
** Query creation structure.
** May later adopt dynamic allocation.
*/
typedef struct v0_query_struct
{
	char select[IMS_COL1024_LEN+1];
	char from[IMS_COL255_LEN+1];
	char where[V0_MAX_WHERE_LEN+1];
	char sql[IMS_COL1024_LEN*11];
} V0_QUERY_STRUCT;

/*
** Granule result structure.
*/
typedef struct v0_granule_list
{
	DBINT granule_idx;
	DBCHAR granule_id[IMS_COL40_LEN+1];
	DBCHAR start_time[IMS_COL30_LEN+1];
	DBCHAR stop_time[IMS_COL30_LEN+1];
	DBREAL max_lat;
	DBREAL min_lat;
	DBREAL north_lat;
	DBREAL south_lat;
	DBREAL west_lon;
	DBREAL east_lon;
	DBREAL ns_lat;
	DBREAL ns_lon;
	DBREAL ne_lat;
	DBREAL ne_lon;
	DBREAL fs_lat;
	DBREAL fs_lon;
	DBREAL fe_lat;
	DBREAL fe_lon;
	DBREAL center_lat;
	DBREAL center_lon;
	DBCHAR asc_desc[IMS_COL10_LEN+1];
	DBCHAR pole_included[IMS_COL10_LEN+1];
	V0_VALUE_LIST *detailed_keyword; 
	V0_VALUE_LIST *formatted_list;
	struct v0_granule_list *next_p;
} V0_GRANULE_LIST;

/*
** Structure to include keyword specifications.
*/
typedef struct v0_keyword_list
{
	DBCHAR keyword[IMS_COL30_LEN+1];
	DBSMALLINT data_type;
	struct v0_keyword_list *next_p;
} V0_KEYWORD_LIST;


/*
** Structure to hold a list of account id related to the current user
** Will be sent together with Dataset group in the INVENTORY RESULT
** or in the USER_ACCT_RESULT for DARs
*/
typedef struct v0_user_acct_list
{
	DBCHAR account_id[IMS_COL20_LEN+1];
	DBREAL balance;
	V0_ERR_LIST *err_msg;
  struct v0_user_acct_list *next_p;
}V0_USER_ACCT_LIST;

/*
** Structure to include dataset result listing.
** Additions are made as needed.
*/
typedef struct v0_dataset_list
{
	DBSMALLINT dataset_idx;
	DBCHAR dataset_id[IMS_COL80_LEN+2];
	DBCHAR md_id[IMS_COL30_LEN+2];
	DBCHAR sensor[IMS_COL30_LEN+1];
	DBCHAR platform[IMS_COL30_LEN+1];
	DBCHAR day_night[IMS_COL10_LEN+1];
	DBSMALLINT process_level;
	DBCHAR granules_table[IMS_COL30_LEN+1];
	DBSMALLINT temporal_type;
	DBSMALLINT spatial_type;
	DBCHAR campaign[IMS_COL80_LEN+1];
	V0_VALUE_LIST *parameter;
	V0_USER_ACCT_LIST *account_list;
	int temporal_key_count;
	V0_KEYWORD_LIST *temporal_key_list;
	int spatial_key_count;
	V0_KEYWORD_LIST *spatial_key_list;
	int detail_key_count;
	V0_KEYWORD_LIST *detail_key_list;
	int granule_count;
	V0_GRANULE_LIST *granule_list;
	V0_GRANULE_LIST *old_granule_list;
	V0_GRANULE_LIST *unordered_granule_list;
	struct v0_dataset_list *next_p;
	struct v0_dataset_list *prev_p;
} V0_DATASET_LIST;

/*
** Structures to store results for queries from DARnet.
*/
typedef struct v0_dar_frame
{
	DBCHAR		platform[IMS_COL30_LEN+1];
	DBCHAR		sensor[IMS_COL30_LEN+1];
	DBCHAR		asc_desc[IMS_COL10_LEN+1];
 	DBINT		revolution;
	DBSMALLINT	frame_id;
	DBCHAR		frame_mode[IMS_COL10_LEN+1];
	DBCHAR		frame_status[IMS_COL10_LEN+1];
	DBCHAR		granule_id[IMS_COL30_LEN+1];
	DBCHAR		pole_included[IMS_COL10_LEN+1];
	DBREAL 		north_lat;
	DBREAL 		south_lat;
	DBREAL 		west_lon;
	DBREAL 		east_lon;
	DBREAL		center_lat;
	DBREAL		center_lon;
	DBCHAR		center_time[IMS_COL30_LEN+1];
	struct v0_dar_frame   *next_p;
} V0_DAR_FRAME_LIST;

typedef struct dar_frame_result
{
	int		number_of_frames;
	V0_DAR_FRAME_LIST *frame_list;
	V0_DAR_FRAME_LIST *curr_frame;
} DAR_FRAME_RESULT;

typedef struct v0_dar_result
{
	DBINT		order_id;
	DBSMALLINT	item_id;	
	DBCHAR		account_id[IMS_COL30_LEN+1];
	DBCHAR		user_id[IMS_COL30_LEN+1];
	DBCHAR		start_date[IMS_COL30_LEN+1];
	DBCHAR		end_date[IMS_COL30_LEN+1];	
	DBCHAR		platform[IMS_COL30_LEN+1];
	DBCHAR		sensor[IMS_COL30_LEN+1];
	DBCHAR		mode[IMS_COL10_LEN+1];
	DBCHAR		site_name[IMS_COL30_LEN+1];
	DBSMALLINT	dar_status;
        DBCHAR		pi_name[IMS_COL50_LEN+1];
        DBCHAR		pi_discipline[IMS_COL40_LEN+1];
        DBCHAR		quicklook_p[IMS_COL10_LEN+1];
	DBCHAR		ob_frequency[IMS_COL30_LEN+1];
	DBINT		ob_number;
	DBCHAR		asc_desc[IMS_COL10_LEN+1];
	DBSMALLINT	spatial_type;
	DBREAL		radius;
	DBREAL		center_lat;
	DBREAL		center_lon;
	DBREAL		nw_lat;
	DBREAL		nw_lon;
	DBREAL		ne_lat;
	DBREAL		ne_lon;
	DBREAL		sw_lat;
	DBREAL		sw_lon;
	DBREAL		se_lat;
	DBREAL		se_lon;
	DBCHAR		priority[IMS_COL10_LEN+1];
	DBCHAR		active_p[IMS_COL10_LEN+1];
	DBCHAR		activity_start_date[IMS_COL30_LEN+1];
	DBCHAR		activity_end_date[IMS_COL30_LEN+1];
	DBCHAR		user_comment[IMS_COL255_LEN+1];
	DBCHAR		planner_comment[IMS_COL255_LEN+1];
	DBCHAR		op_comment[IMS_COL255_LEN+1];
	char		rl_frame_table_name[IMS_COL30_LEN+1];	
	char 		ll_frame_table_name[IMS_COL30_LEN+1];
	DAR_FRAME_RESULT dar_frames;
	struct v0_dar_result *next_p;
} V0_DAR_RESULT_LIST;

typedef struct dar_query_result
{
	V0_DAR_RESULT_LIST	*dar_result_list;
	int			number_of_items;	
	V0_DAR_RESULT_LIST	*curr_dar_item;
} DAR_QUERY_RESULT;

/*
** Structure to include all results.
*/
typedef struct v0_result_struct
{
	V0_DATASET_LIST *dataset_list;
	int dataset_count;
	V0_DATASET_LIST *curr_dataset;
	int curr_dataset_count;
	V0_GRANULE_LIST *curr_granule;
	int first_chunk_sent;
	int curr_granule_count;
	int last_dataset_flag;
	int last_msg_flag;
	int previous_ds_idx;
	V0_CONTACT_INFO *contact_info;
	char  user_id[IMS_COL15_LEN+1];
	int   order_id;
	char  directory[IMS_COL50_LEN+1];
	char  dataset_comment[IMS_COL1024_LEN*10+1];
	char  dataset_restriction[IMS_COL1024_LEN*10+1];
	V0_VALUE_LIST *dataset_info_list1;
	V0_VALUE_LIST *dataset_info_list2;
	V0_VALUE_LIST *user_list;
	V0_ERR_LIST *odl_status_code_comment;
	DAR_QUERY_RESULT    dar_query_result;
	int   no_match_found;
} V0_RESULT_STRUCT;

/*
** Descriptor structure; having access to all
** globally referenced data items.
*/
typedef struct v0_desc_struct
{
	int msgId;
	IMS_MSG_STRUCT *msgDesc;
	V0_MSG_TYPE RxType;
	V0_MSG_TYPE TxType;
	AGGREGATE RxTree;
	AGGREGATE TxTree;
	V0_REQUEST_STRUCT request;
	V0_RESULT_STRUCT result;
	V0_QUERY_STRUCT query;
	V0_CAT_STRUCT catReq;
	char odl_status[IMS_COL10_LEN];
	char message_id[IMS_COL30_LEN+1];
	char message_name[IMS_COL30_LEN+1];
} V0_DESC_STRUCT;



/*
** Function declarations
*/

/* ims_v0Handler */

int v0_handler (IMS_MSG_STRUCT *, int, char *, char *, char *);

int v0_handler__send (IMS_MSG_STRUCT *, AGGREGATE, int);

int v0_handler__get (IMS_MSG_STRUCT *, AGGREGATE *, int);

void v0_handler__v0Desc_init (IMS_MSG_STRUCT *, AGGREGATE, V0_MSG_TYPE, int, 
	V0_DESC_STRUCT *, char *, char *);

void v0_handler__v0Desc_cleanup (V0_DESC_STRUCT *);

/* ims_v0MsgTree */

V0_MSG_TYPE v0_msgTree__identify (IMS_MSG_STRUCT *, AGGREGATE);

void v0_msgTree__get_message_type (IMS_MSG_STRUCT *, AGGREGATE, char *);

int v0_msgTree__get_message_id (IMS_MSG_STRUCT *, AGGREGATE, char *);

void v0_msgTree__destroy (AGGREGATE );

int v0_msgTree__create (IMS_MSG_STRUCT *, AGGREGATE, AGGREGATE *, 
	V0_MSG_TYPE, char *, V0_RESULT_STRUCT *, int);

VALUE NewSqValue ( PARAMETER, VALUE_DATA *);

/* ims_v0Process */  

int v0_process (IMS_MSG_STRUCT *, V0_DESC_STRUCT *); 

int v0_process__parse_RxTree (V0_DESC_STRUCT *);
int v0_process__prepare_string (char *);

/* ims_v0Time */ 

int v0_time__OdlToDatetime (VALUE, char *, char *, char *);

int v0_time__OdlToIMSTime (VALUE, char *);

int v0_time__DatetimeToOdl (char *);

int v0_time__DoyToDate (int, int, char *, char *);

int v0_time__compareIMSA (char *, char *);

/* ims_v0Query */

int v0_query__build_ds (V0_DESC_STRUCT *);

int v0_query__build_granule (V0_DESC_STRUCT *);

/* ims_v0Spatial */

int v0_spatial__v0_calcb (float *, float *, float *, float *, 
	float *, float *, char *);

int v0_spatial__v0_boundaries (V0_REQUEST_STRUCT *);

int v0_spatial__refine_srch (V0_DESC_STRUCT *);

int v0_spatial__rectTo4pt (float *, float *, float *, float *,
  float *,  float *, float *,  float *, float *,  float *, float *,  float *);
/* ims_v0Order */

/* Prototype for ims_order() is now in ims_order.h. */

int v0_order__place_order(IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
int v0_order__report_order_error(IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int, 
	char *, char *);

/* ims_v0Dar */
int v0_dar__process_query(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);

/* ims_v0Util */
int v0_util__send_email (IMS_MSG_STRUCT *, char *, char *, char *, char *);
int v0_util__set_status_code_comment (V0_DESC_STRUCT *, char *, char *);

#endif /* !_IMS_V0HANDLER_H */
