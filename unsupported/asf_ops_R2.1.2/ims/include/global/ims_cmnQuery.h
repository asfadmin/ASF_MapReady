/******************************************************************************
**
** File:        ims_cmnQuery.h
**
** Function:    Include file for subsystem interface queries.
**
** Author:      Dan Crichton, David Pass
**
** Date:        5/1/95
**
******************************************************************************/

#ifndef _IMS_CMNQUERY_H
#define _IMS_CMNQUERY_H

static char *sccsCmnQuery = "@(#)ims_cmnQuery.h	5.6  09/05/97";

/*
** Query return status.
*/
#define IMS_NOCONNECT -4
#define IMS_DEADLOCK  -5
#define IMS_NOROWS    -6
#define IMS_QUERYFAIL -7

/*
** Define common query structure.
*/
typedef struct
{
    char username[IMS_NAME_LEN+1];
    char password[IMS_NAME_LEN+1];
    char program[IMS_PROGRAM_LEN+1];
    char server[IMS_NAME_LEN+1];
    char database[IMS_NAME_LEN+1];
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int retStatus;
    char *retPtr;
} IMS_CMN_QUERY;

/*
** Define GHA return structure.
*/
typedef struct
{
    char date[22];
    char angle[16];
} IMS_GHA_STRUCT;

/*
** Define calParam return structure.
*/
typedef struct ims_calParamStruct
{
    char processor[31];
    char file_name[256];
    struct ims_calParamStruct *next;
} IMS_CAL_PARAM_STRUCT;

/*
** Define frame return structure.
*/
typedef struct Ims_Frame_Struct
{
    int   frame_id;
    char  station_id[5];
    char  start_time[22];
    char  end_time[22];
    char  center_time[22];
    char  data_direction[9];
    struct  Ims_Frame_Struct * next;
} IMS_FRAME_STRUCT;

/*
** Define frame source media return structure
*/
typedef struct ims_frame_source_media_struct
{
	char	frame_status[16];
	char	start_time[23];
	char	end_time[23];
	char	center_time[23];
	char	media_id[16];
	char	media_location[255];
	char	media_type[10];
	char	data_direction[9];
	char	station_id[5];
	char	recorder_id[20];
	char	scan_results_file[256];
	char	platform[30];
} IMS_FRAME_SOURCE_MEDIA_STRUCT;

static struct ims_platformModes
{
    char mode[4];
    char dataset[80];
} IMS_PLATFORM_MODES [] =
{
    {"FN1", " FINE RESOLUTION"},
    {"FN2", " FINE RESOLUTION"},
    {"FN3", " FINE RESOLUTION"},
    {"FN4", " FINE RESOLUTION"},
    {"FN5", " FINE RESOLUTION"},
    {"ST1", " STANDARD BEAM"},
    {"ST2", " STANDARD BEAM"},
    {"ST3", " STANDARD BEAM"},
    {"ST4", " STANDARD BEAM"},
    {"ST5", " STANDARD BEAM"},
    {"ST6", " STANDARD BEAM"},
    {"ST7", " STANDARD BEAM"},
    {"WD1", " WIDE BEAM"},
    {"WD2", " WIDE BEAM"},
    {"WD3", " WIDE BEAM"},
    {"SNA", " SCANSAR"},
    {"SNB", " SCANSAR"},
    {"SWA", " SCANSAR"},
    {"SWB", " SCANSAR"},
    {"EH1", " HIGH INCIDENCE"},
    {"EH2", " HIGH INCIDENCE"},
    {"EH3", " HIGH INCIDENCE"},
    {"EH4", " HIGH INCIDENCE"},
    {"EH5", " HIGH INCIDENCE"},
    {"EH6", " HIGH INCIDENCE"},
    {"EL1", " LOW INCIDENCE"},
    {"STD", " SAR"},
    {"RLT", " SAR"},
    {"",    " SAR"}
};
#define IMS_MAXPLATFORMMODES 29

/*
** Define scan return structure.
*/
typedef struct
{
    char scan_results_file[256];
} IMS_SCAN_STRUCT;


/*
** Define TCE return structure.
*/
typedef struct
{
    char rev[6];
    char date[22];
    char sat_time[12];
    char clk_cycle[12];
} IMS_TCE_STRUCT;

/*
** Define state vector return structure.
*/
typedef struct  Ims_SV_Struct
{
    char rev[6];
    char date[22];
    char coord_sys[17];
    char x_pos[12];
    char y_pos[12];
    char z_pos[12];
    char x_vel[12];
    char y_vel[12];
    char z_vel[12];
    struct Ims_SV_Struct * next;
} IMS_SV_STRUCT;

/*
** Define dar status reporting structure.
*/
typedef struct imsDarStatus
{
    int   order_id;
    short item_id;
    char  status[31]; /* VALIDATED, IN-PLANNING, REJECTED, COMPLETED */
    char  planner_comment[256];
}IMS_DAR_STATUS;

/*
** Define dar query return structure.
*/
typedef struct imsDarList
{
    int   order_id;
    short item_id;
    char  user_id[16];
    char  account_id[16];
    char  platform[31];
    char  sensor[31];
    char  mode[6];
    char  quicklook_p;
    char  asc_desc;
    char  priority[16];
    char  receive_time[22];
    char  complete_time[22];
    char  start_time[22];
    char  end_time[22];
    char  site_name[31];
    char  site_shape;
    float radius;
    float center_lat;
    float center_lon;
    float north_west_lat;
    float north_west_lon;
    float north_east_lat;
    float north_east_lon;
    float south_west_lat;
    float south_west_lon;
    float south_east_lat;
    float south_east_lon;
    char  observation_freq[31];
    int   observation_num;
    char  pi_name[31];
    char  pi_discipline[36];
    char  active_p;
    char  activity_start_date[22];
    char  activity_end_date[22];
    char  status[31];
    char  user_comment[256];
    char  planner_comment[256];
    char  op_comment[256];
    struct imsDarList *next;
} IMS_DAR_LIST;

/*
** Define items table retrieved data structure.
*/
typedef struct itemsList
{
    short             instance;
    char              description[IMS_COL255_LEN+1];
    struct itemsList *next;
} ITEMS_LIST;


/*
** Define dar statistics reporting structure.
*/
typedef struct imsDarStatistics
{
    int     order_id;         /* 4-byte integer */
    short   item_id;          /* 2-byte integer */
    char    time_stamp[22];   /* YYYY-DDDTHH:MM:SS.fff */
    char    status[31];       /* REQUESTED, SUBMITTED, PLANNED */
    int     seconds;
} IMS_DAR_STATISTICS;

/*
** Define dar frame status and spatial query (for RGPS) reporting
**      structure.
*/
typedef  struct  ims_FrameResults__t *pnt_ims_FrameResults_t;
typedef  struct  ims_FrameResults__t {
    pnt_ims_FrameResults_t  next; /* next buffer if any  */
    char    start_time[23];   /* YYYY-DDDTHH:MM:SS.fff note: sometimes Z
                at end  */
    char    frame_status[16]; /* values are: PLANNED, SCHEDULED,
                REJECTED, ACQUIRED, MISSED or SCANNED.  */
    char    media_id[16];
    int     revolution;
    short   sequence;
    char    mode[6];
    char    platform[11];
    short   frame_id;
    char    sensor[2];
    char    activity_id[11];
    char    asc_desc[3];
    char    end_time[23];
    char    center_time[23];
    float   center_lat;
    float   center_lon;
    float   near_start_lat;
    float   near_start_lon;
    float   near_end_lat;
    float   near_end_lon;
    float   far_start_lat;
    float   far_start_lon;
    float   far_end_lat;
    float   far_end_lon;
    char    frame_mode[11];
    char    station_id[5];
    char    scan_results_file[129];
} ims_FrameResults_t;


/*
** Define order query output (for RGPS) reporting structure.
*/
typedef  struct  ims_OrderResults__t *pnt_ims_OrderResults_t;
typedef  struct  ims_OrderResults__t {
    pnt_ims_OrderResults_t  next; /* next buffer if any  */
    int       order_id;
    short     item_id;
    char      status_id[20];
    char      order_item_type_id[4];
    short     priority;
    int       granule_idx;
    char      granule_name[30];
    char      platform[16];
    char      sensor[6];
    char      dataset[81];
    char      p_granule_name[30];
    int       p_granule_idx;
    int       p_data_kbytes;
    int       p_metadata_kbytes;
    char      media_id[16];
    char      quicklook_p;
    char      deleted_p;
    char      process_status_id[30];
    char      step_name[31];
    char      op_comment[256];
    char      process_comment[256];
} ims_OrderResults_t;

/*
**  Define dlToDtk structure: for dl and dtk
**      all values map to name in capitals in the data
**      dictionary.
*/
typedef  struct  ims_dtkStruct *pnt_ims_dtkStruct;
typedef  struct  ims_dtkStruct{
    char  platform[3];  /* two character version  */
    char  sensor[2];    /* one character version */
    int   revolution;
    int   sequence;
    char  quicklook_flag[4]; /* yes or no: caps */
    char  process_auth_flag[4]; /* yes or no: caps */
    char  mode[6];
    char  frame_mode[11];
    char  time_on[22]; /* not same as dl times  */
    char  time_off[22];
    char  site_name[31];
    pnt_ims_dtkStruct  next;
}   IMS_DTK_STRUCT;

typedef  struct  ims_dlStruct *pnt_ims_dlStruct;
typedef  struct  ims_dlStruct{
    char  platform[3];  /* two character version  */
    char  sensor[2];    /* one character version: ususally Z */
    int   revolution;
    int   sequence;
    char  activity_id[11];  /* usually RLT or DMP  */
    char  station_id[5];    /* usually FA or MC  */
    char  antenna_id[17];
    char  transmitter_id[17];
    char  fa_schedule_link[12];
    char  time_on[22];      /* inclusive of dtk values if RLT */
    char  time_off[22];
    char  time_aos[22];
    char  time_los[22];
    char  downlink_status[11];
    int   number_of_dtk_entry; /* no. of datatake entries: not used */
    pnt_ims_dtkStruct  datatakePtr; /* ptr to datatake linked list */
    pnt_ims_dlStruct  downlinkPtr; /* ptr to next downlink link */
}  IMS_DL_STRUCT;

/*
**  put values in for inputMatch values of arg for ims_dlToDtkQuery
*/
#define  IMS_DL_MATCH   1
#define  IMS_DTK_MATCH  2
#define  IMS_NO_MATCH   3


/*
** Define IMS version reporting structure.
*/
typedef struct imsVersionStruct
{
    int     version;
    char   *name;
} IMS_VERSION_STRUCT;

/*
** Function prototypes for ims_cmnQuery.c module.
*/
int ims_ghaQuery (IMS_CMN_QUERY *, char *, char *);
int ims_ghaPointQuery (IMS_CMN_QUERY *, char * );
int ims_ghaRangeQuery (IMS_CMN_QUERY *, char *, char *);
int ims_tceQuery (IMS_CMN_QUERY *, char *, char *, char *);
int ims_svQuery (IMS_CMN_QUERY *, char *, char, char *, char *, int);
int ims_calParamQuery (IMS_CMN_QUERY *, char *, char *, char *);
int ims_scanQuery (IMS_CMN_QUERY *, char *, int, int, char *, char *);
int ims_frameQuery (IMS_CMN_QUERY *, char *, int, int, char *, char *, char *);
int ims_orderStatus (IMS_CMN_QUERY *, char *);
int ims_openQueryConnection (IMS_CMN_QUERY *);
int ims_closeQueryConnection (IMS_CMN_QUERY *);
void ims_frameFree (IMS_FRAME_STRUCT *);
void ims_calParamFree (IMS_CAL_PARAM_STRUCT *);
int ims_darStatus (IMS_CMN_QUERY *, IMS_DAR_STATUS *);
int ims_darQuery (IMS_CMN_QUERY *, char *, char *, char *);
int ims_darStatistics (IMS_CMN_QUERY *query, IMS_DAR_STATISTICS *darStats);
int ims_incrVersion (IMS_CMN_QUERY *query, char *name);
int ims_darFrameStatus( IMS_CMN_QUERY * , int, short, long * );
int ims_spatialQuery( IMS_CMN_QUERY * , char *, char *, char *, char *, float,
        float, float, float, char *, long * );
int ims_orderQuery( IMS_CMN_QUERY * , long, long * );
void freeFrameResults( pnt_ims_FrameResults_t );
void freeOrderResults( pnt_ims_OrderResults_t );
int ims_dlToDtkQuery( IMS_CMN_QUERY * , char *, char *, int, int, int * );
void  ims_dlToDtkFree( IMS_DL_STRUCT * );
int ims_frameStatus (IMS_CMN_QUERY * , int, int);

#endif  /* !_IMS_CMNQUERY_H */
