/******************************************************************************
**
** File:        ims_media.h
**
** Function:    This file includes structure definitions and function
**              prototypes for the media functions.
**
** Author:      S. Hardman
**
** Date:        5/18/95
**
******************************************************************************/

#ifndef _IMS_MEDIA_H
#define _IMS_MEDIA_H

static char *sccsMedia = "@(#)ims_media.h	5.5 08/15/97";

/*
** Dependent Include Files.
*/
#include <ims_product.h>
#include <ims_vdfCat.h>

/*
** Constant Definitions.
*/
#define NULL_ORDER   -1
#define NO_VERSION   (DBSMALLINT)-1
#define TRAILER_FILE (DBSMALLINT)4

/*
** Media Order Item Status Values.
*/
#define MEDIA_NO_STATUS (DBSMALLINT)0
#define MEDIA_GENERATED (DBSMALLINT)9
#define MEDIA_COMPLETE  (DBSMALLINT)10
#define MEDIA_ERROR     (DBSMALLINT)13
#define MEDIA_LOCKED    (DBSMALLINT)20

/*
** Film Status Values.
*/
#define FILM_GENERATED (DBSMALLINT)3
#define FILM_ERROR     (DBSMALLINT)13
#define FILM_LOCKED    (DBSMALLINT)20

/*
** Granule Status Values.
*/
#define GRANULE_AVAILABLE (DBSMALLINT)1

/*
** Film target values.
*/
#define LASER  1
#define FIRE   2

/*
** Enumerated types for media job status.
*/
enum MEDIA_JOB_STATUS
{
    MEDIA_JOB_NO_STATUS,
    MEDIA_JOB_IN_PROGRESS,
    MEDIA_JOB_COMPLETE,
    MEDIA_JOB_FAILED
};

/*
** Enumerated types for device status.
*/
enum DEVICE_STATUS
{
    DEVICE_NO_STATUS,
    DEVICE_AVAILABLE,
    DEVICE_INUSE,
    DEVICE_OFFLINE,
    DEVICE_INSERVICE
};

/*
** User specification structure definition.
*/
typedef struct mediaUserSpec
{
    char *username;
    char *password;
    char *program;
    char *server;
    char *database;
} MEDIA_USER_SPEC;

/*
** Device list structure defintion.
*/
typedef struct deviceList
{
    DBSMALLINT device_id;
    DBCHAR name[IMS_COL20_LEN+1];
    DBCHAR path[IMS_PATH_LEN+1];
    DBCHAR path_extension[6];
    struct deviceList *next;
} DEVICE_LIST;

/*
** Device information return structure definition.
*/
typedef struct deviceInfo
{
    DBSMALLINT device_id;
    DBCHAR name[IMS_COL20_LEN+1];
    DBCHAR host[IMS_HOST_LEN+1];
    DBCHAR path[IMS_PATH_LEN+1];
    DBCHAR path_extension[6];
    DBCHAR description[IMS_COL255_LEN+1];
    DBSMALLINT status;
    DBINT order_id;
    DBCHAR last_requested[IMS_DBMS_DATETIME_LEN+1];
    DBCHAR op_comment[IMS_COL255_LEN+1];
    int status_conflict;    /* This item does not come from the database. */
    struct deviceInfo *next;
} DEVICE_INFO;

/*
** Enumerated types for media types.
*/
enum IMS_MEDIA_TYPE
{
    IMS_NO_MEDIA_TYPE,
    IMS_4_MM,
    IMS_4_MM_HD,
    IMS_8_MM,
    IMS_8_MM_HD,
    IMS_9_TRACK,
    IMS_9_TRACK_HD,
    IMS_DISK,
    IMS_LT_NEG,
    IMS_LT_POS,
    IMS_LT_PRINT,
    IMS_FR_NEG,
    IMS_FR_POS,
    IMS_FR_PRINT,
    IMS_PHOTO_CPR,
    IMS_PHOTO_PTP,
    IMS_PHOTO_NTP,
    IMS_MEDIA_TYPE_RESERVE1,
    IMS_MEDIA_TYPE_RESERVE2,
    IMS_MEDIA_TYPE_RESERVE3,
    IMS_DCRSI,
    IMS_ID_1,
    IMS_HD_96,
    IMS_EMI
};

/*
** Enumerated types for media id types.
*/
enum IMS_MEDIA_ID_TYPE
{
    IMS_NO_MEDIA_ID_TYPE,
    IMS_4M,
    IMS_8M,
    IMS_9T,
    IMS_ET
};

/*
** Enumerated types for media formats.
*/
enum IMS_MEDIA_FORMAT
{
    IMS_NO_MEDIA_FORMAT,
    IMS_TAR,
    IMS_CEOS,
    IMS_HDF,
    IMS_8X10,
    IMS_20X24_2X,
    IMS_20X24_5X,
    IMS_MEDIA_FORMAT_RESERVE1,
    IMS_MEDIA_FORMAT_RESERVE2,
    IMS_MEDIA_FORMAT_RESERVE3,
    IMS_ESA,
    IMS_CSA,
    IMS_NASDA
};

/*
** Media item list structure definition.
*/
typedef struct mediaItemList
{
    DBSMALLINT item_id;
    DBSMALLINT status;
    struct mediaItemList *next;
} MEDIA_ITEM_LIST;

/*
** Film Request list structure definition.
*/
typedef struct filmRequestList
{
    DBINT order_id;
    DBSMALLINT item_id;
    DBSMALLINT film_target;
    DBSMALLINT status;
    struct filmRequestList *next;
} FILM_REQUEST_LIST;

/*
** Film Request list structure definition.
*/
typedef struct filmRequestInfo
{
    DBINT order_id;
    DBSMALLINT item_id;
    DBSMALLINT film_target;
    DBSMALLINT priority;
    DBSMALLINT media_type;
    DBSMALLINT process_type;
    DBSMALLINT dataset_idx;
    DBINT granule_idx;
    DBCHAR granules_table[IMS_COL30_LEN+1];
    DBINT  target_pixel;   /*  used for full res pictures: center of
                           data. default is 0: rec_len/2 added  */
    DBINT  target_line;    /*  used for full res pictures:  center of
                           data. default is 0: num_recs/2 added  */
    DBCHAR name[IMS_COL30_LEN+1];
    DBCHAR format[IMS_COL10_LEN+1];
    DBSMALLINT version;
    DBSMALLINT status;
    DBINT rec_len;        /* record length of .dat file (constant length
                           records)  */
    DBINT  num_recs;       /* no. records in .dat file  */
    char film_type[4];  /* FRF, GFR, GLP, GLT, SLP, SLT */
} FILM_REQUEST_INFO;

/*
** Film file list structure definition.
*/
typedef struct filmFileList
{
    DBSMALLINT type;
    DBCHAR extension[IMS_COL10_LEN+1];
    DBCHAR path[IMS_COL255_LEN+1];
    struct filmFileList *next;
} FILM_FILE_LIST;

/*
** Film stage list structure definition.
*/
typedef struct filmStageList
{
    int copiedToStage;
    char stageSpec[IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
    struct filmStageList *next;
} FILM_STAGE_LIST;

/*
** Enumerated types for Quality check report.
*/
enum QUAL_REPORT_TYPE
{
    NO_REPORT_TYPE,
    FULL_REPORT,
    BRIEF_REPORT
};

/*
** Enumerated types for Stage Areas.
*/
enum STAGE_AREA_TYPE
{
    NO_STAGE_AREA_TYPE,
    CEOS_VDF,
    FPS_TTDL,
    QC_RPT,
    FA_RPT,
    QC_TAR
};

/*
** Film Status Information structure definition.
*/
typedef struct filmQueueList
{
    DBCHAR queue_type[IMS_COL10_LEN+1];
    DBINT order_id;
    DBSMALLINT item_id;
    DBSMALLINT p_dataset_idx;
    DBINT p_granule_idx;
    DBCHAR p_granule_name[IMS_COL30_LEN+1];
    DBCHAR granules_table[IMS_COL30_LEN+1];
    struct filmQueueList *next;
} FILM_QUEUE_LIST;

/*
** Function Prototypes for the ims_device.c module.
*/
int ims_deviceAlloc (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBINT,
    DEVICE_INFO *);
int ims_deviceStatusList (IMS_MSG_STRUCT *, char *, DBSMALLINT, DEVICE_INFO *);
int ims_deviceStatusChange (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBSMALLINT,
    DBCHAR *);
int ims_deviceFree (IMS_MSG_STRUCT *, char *, DBSMALLINT);
int ims_deviceTapeCheck (IMS_MSG_STRUCT *, DEVICE_INFO *);
int ims_deviceTapeEject (IMS_MSG_STRUCT *, DEVICE_INFO *);
int ims_deviceTapeInfo (IMS_MSG_STRUCT *, DEVICE_INFO *);
int ims_deviceMediaId (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBSMALLINT,
    DBCHAR *, int);

/*
** Function prototypes for the ims_mediaDist.c module.
*/
int ims_mediaDist (IMS_MSG_STRUCT *, pnt_vdf_cat_request_t, DBSMALLINT,
    DBSMALLINT, DBINT, MEDIA_ITEM_LIST *, DEVICE_INFO *, char *, int, int *);
int  ims_calMediaSpace( float, float, int, int, char *, double * );

/*
** Function prototypes for the ims_filmList.c module.
*/
int ims_filmList (IMS_MSG_STRUCT *, char *, FILM_REQUEST_LIST *);
int ims_filmUpdate (IMS_MSG_STRUCT *, char *, FILM_REQUEST_LIST *);

/*
** Function prototypes for the ims_filmStatus.c module.
*/
int ims_filmStatus (IMS_MSG_STRUCT *, char *, DBINT, DBSMALLINT,
    DBSMALLINT, DBCHAR *);

/*
** Function prototypes for the ims_qc.c module.
*/
int ims_qc (IMS_MSG_STRUCT *, char *, int, char *, char *, int);

/*
** Function prototypes for the ims_mediaUtil.c module.
*/
char *ims_mediaDesc (int);
char *ims_statusDesc (int);
char *ims_filmStatusDesc (int);

#endif  /* !_IMS_MEDIA_H */
