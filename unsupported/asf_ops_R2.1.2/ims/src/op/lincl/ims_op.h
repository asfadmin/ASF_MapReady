/*****************************************************************************
**
** File:			ims_op.h
**
** Date:			12/12/1994
**
** Author:    Jennifer Ting, Hoshyar Sayah
**
** Modified:
**    11/15/95	A. Tilden     Modified OP_ACCOUNT and OP_ACC_DATA structures.
**
**		01/07/96 	J. Ting		    Modified for catalog schema version 3.30.
**
**    05/09/96  J. Ting       Added Order Item Process Status
**
**    06/04/96  J. Ting       Added v0_process_type in OP_ORDER_ITEM_LIST.
**                            This change is for PR 900.
**
**    06/05/96  J. Ting       Added glbData.fileSelectDlg - toplevel widget
**                            for Save Results File Selection Dialog.
**
**    07/19/96  J. Ting       Added catalog item op user type - PR 986
**
**    10/08/96  J. Ting       Modified OP_ACCOUNT to comment out account
**                            manager information.
**
**    10/08/96  J. Ting       Modified OP_ACC_DATA to comment out account
**                            manager query structs.
**
**    10/14/96  J. Ting       Added mgr_user_id to OP_ACC_DATA
**
**    10/14/96  J. Ting       Commented out quicklook_p from OP_ACC_DATA for
**                            schema 3.5.0
**
**    11/05/96  J. Ting       Modified local_dir to ftp_dir, add access_dir
**
**    03/26/97  T. McKillop   Added dlSearchW and dl2dtkW top-level widgets.
**                            Added dlSearchFlag and downlinkFlag screen flags.
**                            Added keyword_value table data
**                            Added downlink (to datatake) structures
*****************************************************************************/

#ifndef _IMS_OP_H
#define _IMS_OP_H

static char *sccsOp = "@(#)ims_op.h	6.1  03/06/98";


/**********************************************************************
**
** The following structures are used in Order Search, Order Display,
** Photo Order, Photo Job, Film Generation, Shipping, ShipView,
** Billing, BillView, and Downlink Search screens.
**
**********************************************************************/

/*
** Macro Definition
*/
#define OP_FREECHARPTR(p) {if((p)) free((p)); (p)=NULL;}

/*
** Selection Dialog definitions
*/
#define ORDER_PRIORITY 1
#define ITEM_PRIORITY  2
#define ORDER_STATUS   3
#define ITEM_STATUS    4
#define PHOTO_TYPE		 5
#define PHOTOJOB_ID 	 6
#define FIRE_STATUS 	 7
#define LASER_STATUS 	 8


/*
** Comment Dialog definitions
*/
#define ORDER_COMMENT 			1
#define ITEM_COMMENT  			2
#define EDIT_PHOTO_COMMENT  3
#define VIEW_PHOTO_COMMENT  4
#define FIRE_COMMENT 				5
#define LASER_COMMENT 			6
#define SHIPPING_COMMENT  	7
#define BILLING_COMMENT   	8


/* Maximum # of tape items */
#define MAX_TAPE_ITEMS 1000

/*
** Temporary Print files directory
*/
#define DEF_PRINT_DIRECTORY "/tmp/"


/*
** Enumerated types for User Type
** op_user_type - schema version 3.30
*/
typedef enum opUserType
{
	OP_NOTYPE,
	OP_GENERAL,
	OP_PRODUCTION,
	OP_GDC,
	OP_SYSTEM
} OP_USER_TYPE;


/*
** Enumerated types for order status
** order status - schema version 3.30
*/
enum ORDER_QUEUE_STATUS
{
	ORDER_NO_STATUS,
	ORDER_NEW,
	ORDER_PENDING,
	ORDER_GENERATED,
	ORDER_COMPLETE,
	ORDER_CANCEL = 11,
	ORDER_CANCELLED,
	ORDER_ERROR,
	ORDER_FAILED,
	ORDER_HOLD,
	ORDER_LOCKED = 20
};


/*
** Enumerated types for item status
** order_item status - schema version 3.30
*/
enum ORDER_ITEM_STATUS
{
	ITEM_NO_STATUS,
	ITEM_NEW,
	ITEM_VALIDATED,
	ITEM_IN_PROCESS,
	ITEM_ON_LINE,
	ITEM_IN_FILM,
	ITEM_ON_FILM,
	ITEM_IN_PHOTO,
	ITEM_IN_MEDIA,
	ITEM_GENERATED,
	ITEM_COMPLETE,
	ITEM_CANCEL = 11,
	ITEM_CANCELLED,
	ITEM_ERROR,
	ITEM_FAILED,
	ITEM_HOLD,
	ITEM_LOCKED = 20
};


/*
** Enumerated types for item process status
** order_item process_status - schema version 3.30
*/
enum ORDER_ITEM_PROCESS_STATUS
{
  ITEM_NO_PPS_STATUS,
	ITEM_PPS_PENDING,
	ITEM_PPS_READY,
	ITEM_PPS_AVAILABLE,
	ITEM_PPS_SUBMITTED,
	ITEM_PPS_COMPLETED,
	ITEM_PPS_CANCEL
};


/*
** Enumerated types for order item type
** order_item_type -  schema version 3.30
*/
enum ORDER_ITEM_TYPE
{
	ITEM_NO_TYPE,
	APR,
	RPR,
	FPR,
	COR,
	TDR,
	TSR,
	DAR
};


/*
** Enumerated types for fire_queue status
** schema version 3.30
*/
enum FIRE_QUEUE_STATUS
{
	FIRE_NO_STATUS,
	FIRE_NEW,
	FIRE_IN_FIRE,
	FIRE_GENERATED,
	FIRE_COMPLETE,
	FIRE_CANCEL = 11,
	FIRE_CANCELLED,
	FIRE_ERROR,
	FIRE_FAILED,
	FIRE_HOLD,
	FIRE_LOCKED = 20
};


/*
** Enumerated types for laser_queue status
** schema version 3.30
*/
enum LASER_QUEUE_STATUS
{
	LASER_NO_STATUS,
	LASER_NEW,
	LASER_IN_LASER,
	LASER_GENERATED,
	LASER_COMPLETE,
	LASER_CANCEL = 11,
	LASER_CANCELLED,
	LASER_ERROR,
	LASER_FAILED,
	LASER_HOLD,
	LASER_LOCKED = 20
};


/*
** Enumerated types for photo_queue status
** schema version 3.30
*/
enum PHOTO_QUEUE_STATUS
{
	PHOTO_QUEUE_NO_STATUS,
	PHOTO_QUEUE_NEW,
	PHOTO_QUEUE_IN_PHOTO,
	PHOTO_QUEUE_GENERATED,
	PHOTO_QUEUE_COMPLETE,
	PHOTO_QUEUE_CANCEL = 11,
	PHOTO_QUEUE_CANCELLED,
	PHOTO_QUEUE_ERROR,
	PHOTO_QUEUE_FAILED,
	PHOTO_QUEUE_HOLD,
	PHOTO_QUEUE_LOCKED = 20
};


/*
** Enumerated types for photo_job status
** schema version 3.30
*/
enum PHOTO_JOB_STATUS
{
	PHOTO_JOB_NO_STATUS,
	PHOTO_JOB_NEW,
	PHOTO_JOB_IN_PHOTO,
	PHOTO_JOB_GENERATED,
	PHOTO_JOB_COMPLETE,
	PHOTO_JOB_CANCEL = 11,
	PHOTO_JOB_CANCELLED,
	PHOTO_JOB_ERROR,
	PHOTO_JOB_FAILED,
	PHOTO_JOB_HOLD,
	PHOTO_JOB_LOCKED = 20
};



/*
** Order item list structure
** Modified Date - 12/05/1995, schema version 3.30
*/
typedef struct opOrderItemList
{
	DBINT      order_id;
	DBSMALLINT item_id;
	DBSMALLINT order_item_type;
	DBREAL     cost;
	DBINT      shipping_id;
	DBINT      billing_id;
	DBSMALLINT priority;
	DBSMALLINT dataset_idx;
	DBINT      granule_idx;
	DBCHAR		 granule_name[IMS_COL30_LEN+1];
	DBSMALLINT p_dataset_idx;
	DBINT      p_granule_idx;
	DBCHAR		 p_granule_name[IMS_COL30_LEN+1];
	DBCHAR     media_id[IMS_COL15_LEN+1];
	DBSMALLINT media_type;
	DBSMALLINT media_class;
	DBSMALLINT media_fmt_type;
	DBCHAR     validated_p;
	DBCHAR     cost_debited_p;
	DBCHAR     shipped_p;
	DBCHAR     billed_p;
	DBCHAR     quicklook_p;
	DBINT      data_kbytes;		/* corresponds to p_data_kbytes */
	DBINT      p_metadata_kbytes; 	/* added on 4-9-1996 */
	DBSMALLINT process_type;
	DBSMALLINT process_status;
	DBSMALLINT status;
	DBCHAR     step_name[IMS_COL30_LEN+1];
	DBSMALLINT step_sequence;
	DBCHAR     step_started_p;
	DBCHAR 		 op_comment[IMS_COL255_LEN+1];
	DBCHAR 		 process_comment[IMS_COL255_LEN+1];
	DBCHAR 		 granules_table[IMS_COL30_LEN+1];
	DBCHAR 		 step_avgtime[IMS_COL15_LEN+1];
	DBSMALLINT quantity;
	DBCHAR     platacronym[IMS_COL10_LEN+1];
	DBCHAR     platform[IMS_COL30_LEN+1];
	DBCHAR     sensor[IMS_COL30_LEN+1];
	DBCHAR     dataset[IMS_COL80_LEN+1];
	DBCHAR     v0_process_type[IMS_COL30_LEN+1];
	struct     opOrderItemList *prev;
	struct     opOrderItemList *next;
	int position;
	int selectFlag;
} OP_ORDER_ITEM_LIST;



/*
** Data-take list structure
** Modified Date - 04/08/1997, schema version 3.60
*/
typedef struct opDlDtkList
{
	DBCHAR			platform[IMS_COL2_LEN+1] ;
	DBCHAR			sensor[IMS_COL1_LEN+1] ;
	DBINT				revolution ;		/* 4/97: max of 5 characters (1 - 99999) */
	DBSMALLINT	sequence ;			/* 4/97: max of 2 characters (1 - 89) */
	DBCHAR			dt_sensor[IMS_COL1_LEN+1] ;
	DBINT				dt_revolution ;		/* 4/97: max of 5 characters (1 - 99999) */
	DBSMALLINT	dt_sequence ;			/* 4/97: max of 2 characters (1 - 89) */
	DBCHAR			dt_platform[IMS_COL2_LEN+1] ;
	DBCHAR			quicklook[IMS_COL3_LEN+1] ;
	DBCHAR			process_auth_flag[IMS_COL3_LEN+1] ;
	DBCHAR			mode[IMS_COL5_LEN+1] ;
	DBCHAR			frame_mode[IMS_COL10_LEN+1] ;
	DBCHAR			time_on[IMS_DATETIME_LEN+1] ;
	DBCHAR			time_off[IMS_DATETIME_LEN+1] ;
	DBCHAR			site_name[IMS_COL30_LEN+1];
	DBCHAR			updated_time[IMS_COL30_LEN+1];
	struct			opDlDtkList *prev;
	struct			opDlDtkList *next;
	int					position;
	int					selectFlag;
} OP_DL_DTK_LIST;


/*
**  User Profile & Account structure.
*/
typedef struct opUserAccountData
{
	DBCHAR first_name[IMS_COL30_LEN+1];
	DBCHAR initial_name[IMS_COL10_LEN+1];
	DBCHAR last_name[IMS_COL30_LEN+1];
	DBCHAR organization[IMS_COL40_LEN+1];
	DBCHAR street[IMS_COL128_LEN+1];
	DBCHAR city[IMS_COL40_LEN+1];
	DBCHAR state[IMS_COL30_LEN+1];
	DBCHAR country[IMS_COL30_LEN+1];
	DBCHAR zipcode[IMS_COL15_LEN+1];
	DBCHAR phone[IMS_COL30_LEN+1];
	DBCHAR fax[IMS_COL30_LEN+1];
	DBCHAR email[IMS_COL128_LEN+1];
	DBSMALLINT resource_type;
	DBREAL curr_balance;
} OP_USER_ACCOUNT_DATA;


/*
**  Order list structure.
*/
typedef struct opOrderList
{
	DBINT  order_id;
	DBCHAR user_id[IMS_COL30_LEN+1];
	DBCHAR account_id[IMS_COL30_LEN+1];
	DBCHAR received_time[IMS_COL30_LEN+1];
	DBCHAR completed_time[IMS_COL30_LEN+1];
	DBSMALLINT  priority;
	DBSMALLINT  item_count;
	DBSMALLINT status;
	DBCHAR op_comment[IMS_COL255_LEN+1];
	DBCHAR quicklook;
	DBINT  item_generated;
	DBINT  item_online;
	DBINT  item_hold;
	DBINT  item_error;
	OP_ORDER_ITEM_LIST *itemList;
	OP_USER_ACCOUNT_DATA userData;
	struct opOrderList *prev;
	struct opOrderList *next;
	int position;
	int selectFlag;
} OP_ORDER_LIST;


/*
**  Downlink list structure.
*/
typedef struct opDlList
{
	DBCHAR	platform[IMS_COL2_LEN+1] ;
	DBCHAR	sensor[IMS_COL1_LEN+1] ;
	DBINT		revolution ;		/* 4/97: max of 5 characters (1 - 99999) */
	DBSMALLINT	sequence ;			/* 4/97: max of 2 characters (1 - 89) */
	DBCHAR	downlink_status[IMS_COL10_LEN+1] ;
	DBCHAR	activity_id[IMS_COL10_LEN+1] ;
	DBCHAR	station_id[IMS_COL4_LEN+1] ;
	DBCHAR	antenna_id[IMS_COL16_LEN+1] ;
	DBCHAR	transmitter_id[IMS_COL16_LEN+1] ;
	DBCHAR	fa_schedule_link[IMS_COL11_LEN+1] ;
	DBCHAR	time_on[IMS_DATETIME_LEN+1] ;
	DBCHAR	time_off[IMS_DATETIME_LEN+1] ;
	DBCHAR	time_aos[IMS_DATETIME_LEN+1] ;
	DBCHAR	time_los[IMS_DATETIME_LEN+1] ;
	DBINT		number_of_dtk_entry;
	DBCHAR	received_time[IMS_COL30_LEN+1];
	OP_DL_DTK_LIST	*dtkList;
	int			*dtkChanged ;	/* alloc one int/dtk; value == 1 if changed, else 0 */
	struct opDlList	*prev ;
	struct opDlList	*next ;
	int			position;
	int			selectFlag;
} OP_DL_LIST;


/*
** Operator user specification
*/
typedef struct opUserSpec
{
	char *userName;        /* operator user name */
	char *password;        /* password */
	char *server;          /* SQL-Server name */
	char *dbName;          /* catalog database name */
	OP_USER_TYPE userType; /* operator user type */
} OP_USER_SPEC;


/*
** Query creation structure for search criteria
*/
typedef struct op_query_struct
{
	char select[IMS_COL255_LEN*2+1]; /*** new size ***/
	char *sPtr;
	char from[IMS_COL255_LEN+1];
	char *fPtr;
	char where[IMS_COL1024_LEN*5+1];
	char *wPtr;
	char sqlBuf[IMS_COL1024_LEN*8+1];/*** new size ***/
} OP_QUERY_STRUCT;


/*
** Structure for items from the catalog items table
*/
typedef struct opCatalogItem
{
	DBCHAR item_name[IMS_COL30_LEN+1];
	DBSMALLINT  item_id;
} OP_CATALOG_ITEM;

/*
** Operator Interface Device Status structure.
*/
typedef struct opDeviceStatsWidget
{
	Widget device;
	Widget available;
	Widget allocated;
	Widget jobStart;
	Widget qualCheck;
	Widget jobDone;
	Widget jobFail;
	Widget offLine;
	Widget msgText;
} OP_DEVICE_STATS_WIDGET;


/*
**  Tape Item List
*/
typedef struct opTapeItemList
{
	DBINT order_id;
	DBSMALLINT item_id;
	DBSMALLINT status;
	DBSMALLINT media_type;
	DBSMALLINT media_fmt_type;
	DBINT      data_kbytes;
	DBCHAR 		 granules_table[IMS_COL30_LEN+1];
	struct opTapeItemList *prev;
	struct opTapeItemList *next;
} OP_TAPE_ITEM_LIST;

/*
**  Media Item
*/
typedef struct opMediaItemType
{
	DBSMALLINT item_id;
	DBSMALLINT status;
} OP_MEDIA_ITEM_TYPE;


/*
** Media Batch list structure
*/
typedef struct opMediaBatchList
{
	int batch_id;
	int job_id;
	int status;
	int no_items;
	OP_MEDIA_ITEM_TYPE mediaItemArray[MAX_TAPE_ITEMS];
	DBSMALLINT media_type;
	DBSMALLINT media_fmt_type;
	IMS_MSG_STRUCT *msgDesc;
	DEVICE_INFO *deviceInfo;
	char mediaId[IMS_COL15_LEN+1];
	struct opMediaBatchList *prev;
	struct opMediaBatchList *next;
} OP_MEDIA_BATCH_LIST;


/*
** Media Job list structure
*/
typedef struct opMediaJobList
{
	DBINT order_id;
	int totalCount;
	int startedCount;
	int completedCount;
	OP_MEDIA_BATCH_LIST 	*mediaBatchList;
	struct opMediaJobList *prev;
	struct opMediaJobList *next;
} OP_MEDIA_JOB_LIST;

/*
** Media Batch Data structure
*/
typedef struct opMediaBatchData
{
	int job_id;
	int order_id;
	int status;
	int shmid;
	char *data;
} OP_MEDIA_BATCH_DATA;


/*
** Order Client data structure,
** used by Order Search and Order Display screens
*/
typedef struct opClientData
{
	OP_CAT_STRUCT catReq;       		/* catalog database access */
	OP_QUERY_STRUCT queryStruct;    /* sql query structure */
	int orderCount;
	int orderWindowTop;
	int itemWindowTop;
	int orderConnectFlag;
	OP_ORDER_LIST *orderList;				/* list of orders from catalog database */
	OP_ORDER_LIST *currOrder;				/* order currently being processed */
	OP_MEDIA_JOB_LIST *mediaJobList; /* list of media jobs */
	DEVICE_INFO *deviceInfoList;    /* list of device info from catalog */
	} OP_CLIENT_DATA;


	/*
	** Downlink/DTK  data structure,
	** used by Downlink Search and Downlink Display screens
	*/
	typedef struct opDl2DtkData
{
	OP_CAT_STRUCT catReq;       		/* catalog database access */
	OP_QUERY_STRUCT queryStruct;    /* sql query structure */
	int dlCount;
	int dlWindowTop;
	int dtkWindowTop;
	int dlConnectFlag;
	OP_DL_LIST *dlList;							/* list of downlinks from catalog database */
	OP_DL_LIST *currDL;							/* downlink currently being processed */
} OP_DL2DTK_DATA;


/*
**  Photo Queue list structure.
*/
typedef struct opPhotoQueueList
{
	DBINT  			order_id;
	DBSMALLINT 	item_id;
	DBINT  			photojob_id;
	DBSMALLINT  photo_type;
	DBSMALLINT 	quantity;
	DBCHAR 			quality[IMS_COL10_LEN+1];
	DBSMALLINT 	status;
	DBCHAR 			op_comment[IMS_COL255_LEN+1];
	DBCHAR 			user_id[IMS_COL30_LEN+1];
	DBCHAR 			product_id[IMS_COL30_LEN+1];
	struct opPhotoQueueList *prev;
	struct opPhotoQueueList *next;
	int 				position;
	int 				selectFlag;
} OP_PHOTO_QUEUE_LIST;


/*
**  Photo Job list structure.
*/
typedef struct opPhotoJobList
{
	DBINT  			photojob_id;
	DBSMALLINT  photo_type;
	DBSMALLINT 	no_items;
	DBINT  			total_prints;
	DBREAL  		total_cost;
	DBCHAR 			work_order[IMS_COL10_LEN+1];
	DBREAL			print_cost;
	DBCHAR 			start_time[IMS_COL30_LEN+1];
	DBCHAR 			end_time[IMS_COL30_LEN+1];
	DBSMALLINT 	status;
	DBCHAR 			op_comment[IMS_COL255_LEN+1];
	OP_PHOTO_QUEUE_LIST *photoQueueList;
	struct opPhotoJobList *prev;
	struct opPhotoJobList *next;
	int 				position;
	int 				selectFlag;
} OP_PHOTO_JOB_LIST;


/*
**  Photo client data structure,
**  used by Photo Product Orders and Photo Product Jobs screens
*/
typedef struct opPhotoClientData
{
	OP_CAT_STRUCT catReq;       		/* catalog database access */
	int photoConnectFlag;
	OP_QUERY_STRUCT queryStruct;    /* sql query structure */
	int photoQueueCount;
	OP_PHOTO_QUEUE_LIST *photoQueueList;
	int photoQSelectCount;
	OP_PHOTO_QUEUE_LIST *photoQSelectList;
	int photoJobCount;
	OP_PHOTO_JOB_LIST *photoJobList;
	OP_PHOTO_JOB_LIST *currPhotoJob;	/* used in the Photo Job Screen */

} OP_PHOTO_CLIENT_DATA;


/*
**  Fire Queue list structure.
*/
typedef struct opFireQueueList
{
	DBINT  			order_id;
	DBSMALLINT 	item_id;
	DBSMALLINT 	status;
	DBCHAR 			op_comment[IMS_COL255_LEN+1];
	struct opFireQueueList *prev;
	struct opFireQueueList *next;
	int 				position;
	int 				selectFlag;
} OP_FIRE_QUEUE_LIST;


/*
**  Laser Queue list structure.
*/
typedef struct opLaserQueueList
{
	DBINT  			order_id;
	DBSMALLINT 	item_id;
	DBSMALLINT 	status;
	DBCHAR 			op_comment[IMS_COL255_LEN+1];
	struct opLaserQueueList *prev;
	struct opLaserQueueList *next;
	int 				position;
	int 				selectFlag;
} OP_LASER_QUEUE_LIST;

/*
**  TTDL Queue list structure.
*/
typedef struct opTTDLQueueList
{
	DBINT  			order_id;
	DBSMALLINT 	item_id;
	DBSMALLINT 	status;
	DBSMALLINT  queue_type;
	struct opTTDLQueueList *prev;
	struct opTTDLQueueList *next;
	int 				position;
	int 				selectFlag;
} OP_TTDL_QUEUE_LIST;


/*
**  Film client data structure,
**  used by Film Generation screen
*/
typedef struct opFilmClientData
{
	OP_CAT_STRUCT catReq;       		/* catalog database access */
	int fireWindowTop;
	int laserWindowTop;
	int filmConnectFlag;
	OP_QUERY_STRUCT queryStruct;    /* sql query structure */
	int fireQueueCount;
	OP_FIRE_QUEUE_LIST *fireQueueList;
	int laserQueueCount;
	OP_LASER_QUEUE_LIST *laserQueueList;
	int ttdlQueueCount;
	OP_TTDL_QUEUE_LIST *ttdlQueueList;

} OP_FILM_CLIENT_DATA;


/*
** Order Search Screen and Downlink Search Screen - OP_CHECKBOX_TOGGLE
*/
typedef struct opCheckboxToggle
{
	Widget 		toggle_w;
	char			name[IMS_COL30_LEN+1];
	int 			dbID;
} OP_CHECKBOX_TOGGLE;


/*
**  Account ID list structure.
*/
typedef struct opAccountList
{
	DBCHAR account_id[IMS_COL30_LEN+1];
	struct     opAccountList *prev;
	struct     opAccountList *next;
} OP_ACCOUNT_LIST;


/*
**  User ID list structure.
*/
typedef struct opUserList
{
	DBCHAR user_id[IMS_COL30_LEN+1];
	struct     opUserList *prev;
	struct     opUserList *next;
} OP_USER_LIST;


/*
**  PHOTO JOB ID list structure.
*/
typedef struct opPhotoJobIDList
{
	int photojob_id;
	struct     opPhotoJobIDList *prev;
	struct     opPhotoJobIDList *next;
} OP_PHOTOJOB_ID_LIST;


/*
** Messages concatenate structure
*/
typedef struct
{
	char *Msg;
	char *msgPtr;
	int Msg_size;
} CONCAT_STR;


/*
**  Shipping Item data structure.
*/
typedef struct opShipItemList
{
	DBSMALLINT item_id;
	DBCHAR 		 name[IMS_COL30_LEN+1];
	DBREAL     cost;
	DBSMALLINT process_type;
	DBSMALLINT quantity;
	DBSMALLINT media_type;
	DBSMALLINT item_status;		/* item status in order_item table */
	int 			 status;
	struct     opShipItemList *next;
} OP_SHIP_ITEM_LIST;


/*
**  Shipping data structure.
*/
typedef struct opShippingData
{
	DBINT  shipping_id;
	DBINT  order_id;
	DBCHAR shipping_time[IMS_COL30_LEN+1];
	DBCHAR carrier[IMS_COL40_LEN+1];
	DBCHAR op_comment[IMS_COL255_LEN+1];
	DBCHAR account_id[IMS_COL30_LEN+1];
	DBCHAR order_date[IMS_COL30_LEN+1];
	DBSMALLINT profile_id;
	DBCHAR first_name[IMS_COL30_LEN+1];
	DBCHAR initial_name[IMS_COL10_LEN+1];
	DBCHAR last_name[IMS_COL30_LEN+1];
	DBCHAR title[IMS_COL15_LEN+1];
	DBCHAR organization[IMS_COL40_LEN+1];
	DBCHAR street[IMS_COL128_LEN+1];
	DBCHAR city[IMS_COL40_LEN+1];
	DBCHAR state[IMS_COL30_LEN+1];
	DBCHAR country[IMS_COL30_LEN+1];
	DBCHAR zipcode[IMS_COL15_LEN+1];
	DBCHAR phone[IMS_COL30_LEN+1];
	DBCHAR fax[IMS_COL30_LEN+1];
	DBCHAR email[IMS_COL128_LEN+1];
	int 	 total_qty;
	double total_cost;
	OP_SHIP_ITEM_LIST *shipItemList;
} OP_SHIPPING_DATA;

/*
**  Shipping ID, Shipping Time structure.
*/
typedef struct opOrderShipIdList
{
	DBINT  shipping_id;
	DBCHAR create_time[IMS_COL30_LEN+1];
	struct opOrderShipIdList *next;
} OP_ORDER_SHIPID_LIST;


/*
**  Billing Item data structure.
*/
typedef struct opBillItemList
{
	DBSMALLINT item_id;
	DBCHAR 		 name[IMS_COL30_LEN+1];
	DBREAL     cost;
	DBSMALLINT process_type;
	DBSMALLINT quantity;
	DBSMALLINT media_type;
	DBSMALLINT item_status;		/* item status in the order_item table */
	int 			 status;
	struct     opBillItemList *next;
} OP_BILL_ITEM_LIST;


/*
**  Billing data structure.
*/
typedef struct opBillingData
{
	DBINT  billing_id;
	DBINT  order_id;
	DBCHAR create_time[IMS_COL30_LEN+1];
	DBCHAR op_comment[IMS_COL255_LEN+1];
	DBCHAR user_first_name[IMS_COL30_LEN+1];
	DBCHAR user_initial_name[IMS_COL10_LEN+1];
	DBCHAR user_last_name[IMS_COL30_LEN+1];
	DBCHAR order_date[IMS_COL30_LEN+1];
	DBCHAR account_id[IMS_COL30_LEN+1];
	DBREAL curr_balance;
	DBSMALLINT resource_type;
	DBSMALLINT profile_id;
	DBCHAR first_name[IMS_COL30_LEN+1];
	DBCHAR initial_name[IMS_COL10_LEN+1];
	DBCHAR last_name[IMS_COL30_LEN+1];
	DBCHAR title[IMS_COL15_LEN+1];
	DBCHAR organization[IMS_COL40_LEN+1];
	DBCHAR street[IMS_COL128_LEN+1];
	DBCHAR city[IMS_COL40_LEN+1];
	DBCHAR state[IMS_COL30_LEN+1];
	DBCHAR country[IMS_COL30_LEN+1];
	DBCHAR zipcode[IMS_COL15_LEN+1];
	DBCHAR phone[IMS_COL30_LEN+1];
	DBCHAR fax[IMS_COL30_LEN+1];
	DBCHAR email[IMS_COL128_LEN+1];
	double invoice_amount;
	int 	 total_qty;
	OP_BILL_ITEM_LIST *billItemList;
} OP_BILLING_DATA;


/*
**  Billing ID, Billing Time structure.
**  This is for screen BillView
*/
typedef struct opOrderBillIdList
{
		DBINT  billing_id;
		DBCHAR create_time[IMS_COL30_LEN+1];
		struct opOrderBillIdList *next;
} OP_ORDER_BILLID_LIST;


/*
** Dar structure to hold information
** from table dar
*/
typedef struct opDarData
{
	DBINT order_id;
	DBSMALLINT item_id;
	DBCHAR platform[IMS_COL30_LEN+1];
	DBCHAR sensor[IMS_COL30_LEN+1];
	DBCHAR mode[IMS_COL10_LEN+1];
	DBCHAR asc_desc;
	DBCHAR start_date[IMS_COL30_LEN+1];
	DBCHAR end_date[IMS_COL30_LEN+1];
	DBCHAR site_name[IMS_COL30_LEN+1];
	DBSMALLINT spatial_type;
	DBREAL radius;
	DBREAL center_lat;
	DBREAL center_lon;
	DBREAL north_west_lat;
	DBREAL north_west_lon;
	DBREAL north_east_lat;
	DBREAL north_east_lon;
	DBREAL south_west_lat;
	DBREAL south_west_lon;
	DBREAL south_east_lat;
	DBREAL south_east_lon;
	DBCHAR observation_freq[IMS_COL30_LEN+1];
	DBINT  observation_num;
	DBCHAR pi_name[IMS_COL30_LEN+1];
	DBCHAR pi_discipline[IMS_COL40_LEN+1];
	DBCHAR active_p;
	DBCHAR activity_start_date[IMS_COL30_LEN+1];
	DBCHAR activity_end_date[IMS_COL30_LEN+1];
	DBSMALLINT status;
	DBCHAR user_comment[IMS_COL255_LEN+1];
	DBCHAR planner_comment[IMS_COL255_LEN+1];
	DBCHAR op_comment[IMS_COL255_LEN+1];
} OP_DAR_DATA;
typedef struct opGetPhotoJobid
{
	DBINT order_id;
	DBSMALLINT item_id;
	DBINT photojob_id;
} OP_PHOTOJOB_DATA;

/*
** Structure to hold information
** from keyword_value table
*/
typedef struct opKeywordValueData
{
	char	value[IMS_COL20_LEN+1] ;
} OP_KEYWORD_VAL_DATA ;


/*
** End of Order, Photo, Film Management, Downlink
*/


/**********************************************************************
**
** The following structures are used in Accounting and User Screens
** Accounting and Users Management types
**
**********************************************************************/

typedef enum selected_button { OK = 1, CANCEL = 2, NOTHING = 3 }
                                                        SELECTED_BUTTON ;
typedef enum account_user_data { CREATE = 1 , DATA = 2 } ACCOUNT_USER_DATA ;

typedef enum assignment_status
			 { UNCHANGED = 1, MODIFIED = 2, DELETED = 3, NEW = 4 }
			 ASSIGNMENT_STATUS ;

/*
** ACCOUNTING - OP_USER_PROFILE
*/
typedef struct OpUserProfile
{
	DBCHAR			user_id[IMS_COL15_LEN+1];
	DBSMALLINT 	user_type ;
	DBCHAR			auth_key [IMS_COL30_LEN+1];    /*20*/
	DBCHAR			crypt_key [IMS_COL30_LEN+1];   /*20*/
	DBSMALLINT 	priority;
	DBCHAR 			first_name[IMS_COL30_LEN+1];   /*20*/
	DBCHAR 			initial_name[IMS_COL10_LEN+1]; /*3*/
	DBCHAR 			last_name[IMS_COL30_LEN+1];    /*20*/
	DBCHAR 			title[IMS_COL10_LEN+1];
	DBCHAR 			organization[IMS_COL40_LEN+1]; /*35*/
	DBCHAR 			street[IMS_COL128_LEN+1];
	DBCHAR 			city[IMS_COL40_LEN+1];         /*35*/
	DBCHAR 			state[IMS_COL30_LEN+1];        /*20*/
	DBCHAR 			country[IMS_COL30_LEN+1];      /*20*/
	DBCHAR 			zipcode[IMS_COL10_LEN+1];
	DBCHAR 			phone[IMS_COL30_LEN+1];        /*25*/
	DBCHAR 			fax[IMS_COL30_LEN+1];          /*25*/
	DBCHAR 			email[IMS_COL128_LEN+1];
	DBCHAR			ftp_dir [IMS_COL255_LEN+1];
	DBCHAR			access_dir [IMS_COL255_LEN+1];
  int 				selected ;
  int 				position ;
  struct OpUserProfile	* prev ;
 	struct OpUserProfile	*next ;
} OP_USER_PROFILE ;


/*
** ACCOUNTING - OP_ACCOUNT_USERS
*/
typedef struct OpAccountUser
{
	DBCHAR		            user_id[IMS_COL15_LEN+1];
	ASSIGNMENT_STATUS     account_user_status;
	struct OpAccountUser  *prev;
	struct OpAccountUser  *next;
} OP_ACCOUNT_USER ;


/*
** ACCOUNTING - OP_ASSIGN_USERS
*/
typedef struct OpAssignUsers
{
	DBCHAR   user_id[IMS_COL15_LEN+1];
	DBCHAR   first_name[IMS_COL30_LEN+1];   /*20*/
	DBCHAR   initial_name[IMS_COL10_LEN+1]; /*3*/
	DBCHAR   last_name[IMS_COL30_LEN+1];    /*20*/
	int      selected ;
	int      position ;
	struct OpAccountUser  *account_user_ptr;
  struct OpAssignUsers	*prev ;
 	struct OpAssignUsers	*next ;

} OP_ASSIGN_USERS ;


/*
** ACCOUNTING - OP_ACCOUNT_DATASET
*/
typedef struct OpAccountDataset
{
	DBSMALLINT	       	dataset_idx;
	DBTINYINT	         	permissions;
	ASSIGNMENT_STATUS	 	account_dataset_status;
	int									dataset_count;
	struct OpAccountDataset *prev;
	struct OpAccountDataset *next;

} OP_ACCOUNT_DATASET ;


/*
** ACCOUNTING - OP_ASSIGN_DATASETS
*/
typedef struct OpAssignDatasets
{
	DBSMALLINT	dataset_idx;
	DBCHAR 		  dataset[IMS_COL128_LEN+1]; /*80*/
	DBCHAR 		  sensor[IMS_COL30_LEN+1];
	DBCHAR 		  platform[IMS_COL30_LEN+1];
	int    			selected ;
	int    			position ;
	struct OpAccountDataset *account_dataset_ptr;
	struct OpAssignDatasets	*prev ;
 	struct OpAssignDatasets	*next ;

} OP_ASSIGN_DATASETS ;


/*
** ACCOUNTING - OP_ACCOUNT
*/
typedef struct OpAccount
{
	DBCHAR			account_id[IMS_COL15_LEN+1];
	DBSMALLINT 	account_type;
	DBCHAR 			create_time[IMS_COL30_LEN+1];
	DBCHAR 			expire_time[IMS_COL30_LEN+1];
	DBSMALLINT 	resource_type;
	DBREAL			rate_multiplier;
	DBREAL			begin_balance;
	DBREAL			curr_balance;
	DBREAL 			hold_balance;
	DBCHAR			op_validate_p[IMS_COL10_LEN+1];	/*1*/
	/*************************************************
	DBCHAR      quicklook_p[IMS_COL10_LEN+1];		**1**
	*************************************************/
	DBCHAR 			account_proc[IMS_COL255_LEN+1];
	DBCHAR			op_comment[IMS_COL255_LEN+1];
	DBCHAR			mgr_user_id[IMS_COL15_LEN+1];
	/**************************************************
	DBCHAR 			first_name[IMS_COL30_LEN+1];   	**20**
	DBCHAR 			initial_name[IMS_COL10_LEN+1]; 	**3**
	DBCHAR 			last_name[IMS_COL30_LEN+1];    	**20**
	DBCHAR 			title[IMS_COL10_LEN+1];
	DBCHAR 			zipcode[IMS_COL10_LEN+1];
	DBCHAR 			phone[IMS_COL30_LEN+1];       	**25**
	DBCHAR 			fax[IMS_COL30_LEN+1];         	**25**
	DBCHAR 			organization[IMS_COL40_LEN+1];	**35**
	DBCHAR 			street[IMS_COL128_LEN+1];
	DBCHAR 			city[IMS_COL40_LEN+1];        	**35**
	DBCHAR 			state[IMS_COL30_LEN+1];       	**20**
	DBCHAR 			country[IMS_COL30_LEN+1];     	**20**
	DBCHAR 			email[IMS_COL128_LEN+1];
	***************************************************/
  int selected ;
  int position ; /* for the scrollbar, not the list */
  struct OpAccount	*prev ;
 	struct OpAccount	*next ;

} OP_ACCOUNT ;


/*
** ACCOUNTING - OP_USR_DATA
*/
typedef struct OpUsrData {
  int	assigned_users_cnt ;							/* an account */
  int	users_cnt;
  int	assign_users_cnt;
	OP_QUERY_STRUCT queryStruct;           /* sql query structure */
	OP_QUERY_STRUCT search_queryStruct;    /* sql user search */
	OP_USER_PROFILE *op_user_profile_ptr;  /* user data */
  OP_ASSIGN_USERS *op_assign_users_ptr;  /* used to assign users to */
  OP_ASSIGN_USERS *op_assigned_users_ptr;
	OP_ACCOUNT_USER *op_account_user_ptr;
									    /* list of modifications to account_user table */
} OP_USR_DATA ;


/*
** ACCOUNTING - OP_ACC_DATA
*/
typedef struct OpAccData
{
	int accounts_cnt ;
  int assign_datasets_cnt ;
  int assigned_datasets_cnt ;      	    	/* an account */
	OP_QUERY_STRUCT queryStruct;           	/* sql query structure */
	OP_QUERY_STRUCT search_queryStruct;    			/* sql account search */
	/************************************************************************
	OP_QUERY_STRUCT acc_mgr_queryStruct;   	** acc mgr sql query structure **
	OP_QUERY_STRUCT search_mgr_queryStruct;			** sql account_mgr search **
	************************************************************************/
	OP_ACCOUNT *op_account_ptr;									/* account data */
  OP_ASSIGN_DATASETS *op_assign_datasets_ptr; /* assign datasets to */
  OP_ASSIGN_DATASETS *op_assigned_datasets_ptr ;
	OP_ACCOUNT_DATASET *op_account_dataset_ptr;
								 		 /* list of modifications to account_dataset table */

} OP_ACC_DATA ;


/*
** ACCOUNTING - OP_ACC_USR_DATA
*/
typedef struct OpAccUsrData
{
	OP_CAT_STRUCT catReq;       		/* catalog database access */
  OP_USR_DATA user_data;
	OP_ACC_DATA account_data;
  int connect_flag;               /* db connection flag */

} OP_ACC_USR_DATA;

/*
** End of Account and Users types
*/


/****************************************************************
**
** Global data structure; passed to all operator interfaces
**
****************************************************************/
typedef struct opGlobalData
{
	IMS_MSG_STRUCT *msgDesc;          /* CDB msg facility */
	OP_USER_SPEC userSpec;            /* database user structure */
	char *program;                    /* program name */
	char *workDirectory;              /* working directory */

	/* Global Screen Flags */
	int welcomeFlag;                  /* welcome screen flag */
	int searchFlag;	                  /* search screen flag */
	int orderFlag;	                  /* order screen flag */
	int photoOrderFlag;               /* photo order screen flag */
	int photoJobFlag;                 /* photo job screen flag */
	int mediaFlag;    		            /* media device screen flag */
	int filmFlag;                 		/* film generation screen flag */
	int shippingFlag;									/* shipping screen flag */
	int shipViewFlag;									/* shipView screen flag */
	int billingFlag;									/* billing screen flag */
	int billViewFlag;									/* billView screen flag */
	int	dlSearchFlag;									/* downlink to dtk search screen flag */
	int	dl2dtkFlag;										/* downlink to dtk screen flag */

	/* Global Screen Widgets */
	Widget welcomeW;                  /* toplevel welcome widget */
	Widget searchW;                   /* toplevel order search widget */
	Widget orderW;                    /* toplevel order widget */
	Widget msgDlgW;                   /* toplevel message widget */
	Widget reportW;                   /* toplevel report widget */
	Widget photoOrderW;               /* toplevel photo order widget */
	Widget photoJobW;               	/* toplevel photo job widget */
	Widget mediaW; 		              	/* toplevel media device widget */
	Widget filmW; 		              	/* toplevel film generation widget */
	Widget shippingW;            			/* toplevel shipping widget */
	Widget shipViewW;            			/* toplevel shipping folder widget */
	Widget billingW;            			/* toplevel billing widget */
	Widget billViewW;            			/* toplevel billing folder widget */
	Widget fileSelectDlg;         		/* toplevel file selection widget */
	Widget dlSearchW;                 /* toplevel down. to dtk search widget */
	Widget dl2dtkW;                   /* toplevel down. to dtk widget */

	/* Client Data Structures */
	OP_CLIENT_DATA				orderClientData;	/* client data for order screen */
	OP_PHOTO_CLIENT_DATA	photoClientData;	/* client data for photo screen */
	OP_FILM_CLIENT_DATA		filmClientData;		/* client data for film screen */
	OP_DL2DTK_DATA				dlClientData ;		/* client data for downlink screen */

	/* Items table data */
	int order_status_count;
	OP_CATALOG_ITEM order_status[IMS_COL30_LEN+1];
	int item_status_count;
	OP_CATALOG_ITEM item_status[IMS_COL30_LEN+1];
	int pps_status_count;
	OP_CATALOG_ITEM pps_status[IMS_COL30_LEN+1];
	int priority_count;
	OP_CATALOG_ITEM priority[IMS_COL10_LEN+1];
	int photo_type_count;
	OP_CATALOG_ITEM photo_type[IMS_COL10_LEN+1];
	int photo_queue_status_count;
	OP_CATALOG_ITEM photo_queue_status[IMS_COL30_LEN+1];
	int photojob_status_count;
	OP_CATALOG_ITEM photojob_status[IMS_COL30_LEN+1];
	int laser_status_count;
	OP_CATALOG_ITEM laser_status[IMS_COL30_LEN+1];
	int fire_status_count;
	OP_CATALOG_ITEM fire_status[IMS_COL30_LEN+1];
	int media_type_count;
	OP_CATALOG_ITEM media_type[IMS_COL30_LEN+1];
	int media_fmt_type_count;
	OP_CATALOG_ITEM media_fmt_type[IMS_COL10_LEN+1];
	int order_item_type_count;
	OP_CATALOG_ITEM order_item_type[IMS_COL10_LEN+1];
	int process_type_count;
	OP_CATALOG_ITEM process_type[IMS_COL30_LEN+1];
	int resource_type_count;
	OP_CATALOG_ITEM resource_type[IMS_COL10_LEN+1];
	int device_list_count;
	OP_CATALOG_ITEM device_list[IMS_COL30_LEN+1];
	OP_DEVICE_STATS_WIDGET deviceStats[IMS_COL50_LEN+1];
	int spatial_type_count;
	OP_CATALOG_ITEM spatial_type[IMS_COL10_LEN+1];
	int dar_status_count;
	OP_CATALOG_ITEM dar_status[IMS_COL10_LEN+1];
	int op_user_type_count;
	OP_CATALOG_ITEM op_user_type[IMS_COL10_LEN+1];
	OP_MEDIA_JOB_LIST *mediaJobList;

	/* keyword_value table data */
	int platform_count;
	OP_KEYWORD_VAL_DATA	*platform ;
	int sensor_count;
	OP_KEYWORD_VAL_DATA *sensor ;
	int activity_id_count;
	OP_KEYWORD_VAL_DATA *activity_id ;
	int station_id_count;
	OP_KEYWORD_VAL_DATA *station_id ;
	int antenna_id_count;
	OP_KEYWORD_VAL_DATA *antenna_id ;
	int transmitter_id_count;
	OP_KEYWORD_VAL_DATA *transmitter_id ;
	int downlink_status_count;
	OP_KEYWORD_VAL_DATA *downlink_status ;

	/*
	** Accounts and Users data
	*/
  int accounts_usersFlag ;          /* accounts_users flag */
	int search_accountsFlag ;
	int search_usersFlag ;
	int assign_usersFlag ;
	int assign_datasetsFlag ;
  Widget accounts_usersW ;				  /* top level accounts_users widget */
	Widget search_accountsW ;
	Widget search_usersW ;
	Widget assign_usersW ;
	Widget assign_datasetsW;
	OP_ACC_USR_DATA accounts_users_data ;/* client data acc & usr*/
	int user_type_count;
	OP_CATALOG_ITEM user_type[IMS_COL10_LEN+1];
	int account_type_count;
	OP_CATALOG_ITEM account_type[IMS_COL10_LEN+1];
	/*
	** end of Account and Users
	*/

} OP_GLOBAL_DATA;


#endif	/* !_IMS_OP_H */
