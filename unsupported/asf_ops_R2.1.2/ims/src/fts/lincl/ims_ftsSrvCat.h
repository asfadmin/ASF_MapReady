/******************************************************************************
**
** File:        ims_ftsSrvCat.h
**
** Function:    This is the header file for the ims_ftsCat.c functions which
**              perform catalog queries and updates for the FTS process.
**
** Author:      Hoshyar Sayah, Dana Freeborn
**
** Date:        10/90
**
** Modified:    8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/20/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              3/1/95 - D. Crichton - R1B
**              Modify Granule Spec structure.
**
** 				3/28/95 - D. Crichton - R1B
**				Add spatial data type for processing of spatial query during
**				ingestion process.
**
**				4/3/95 - D. Crichton - R1B
**				Add stored procedure check_user_login.
**
**				5/12/95 - D. Crichton - R1B
**				Introduce new aux function to update the granule file 
**				sizes in the granules table.
**
******************************************************************************/

#ifndef IMS_FTSSRVCAT_H
#define IMS_FTSSRVCAT_H

static char *sccsFtsSrvCat = "@(#)ims_ftsSrvCat.h	5.1  18 Mar 1996";

#ifndef IMS_COL15_LEN
#define IMS_COL15_LEN 15
#endif

/*
** Internal IMS file descriptor.  Contains information on files, most
** of which comes from the catalog.
*/
typedef struct ftsCatUserSpec 
{
	char *dbUserName;
	char *dbPassword;
	char *program;
	char *server;
	char *database;
} FTS_CAT_USERSPEC;   

typedef struct ftsCatGranuleSpec 
{
	char name[IMS_COL30_LEN+1];
	char name_stamp[IMS_COL15_LEN+1];
	char format[IMS_COL10_LEN+1];
	char contributor[IMS_COL30_LEN+1];
	CS_INT granule_idx;
	CS_INT oldGranule_idx;
	CS_SMALLINT version;
	CS_SMALLINT status;
	CS_TINYINT o_gdr;
	CS_SMALLINT readCount;
	struct ftsCatGranuleSpec *next;
} FTS_CAT_GRANULESPEC; 

typedef struct ftsCatStateSpec
{
	CS_SMALLINT status;
	char *eventType;
	int eventNumber;
	char *msg;
	short msgNumber;
} FTS_CAT_STATESPEC;

typedef struct ftsCatUserCap
{
	char *userName;          /* User's name for catalog access. */
	char *accountId;         /* User's account id for catalog access. */
	unsigned char capMask;   /* Capability mask. a=8, g=4, d=2, r=1. */
} FTS_CAT_USERCAP;

typedef struct ftsCatKeyValue
{
	char keyword[IMS_COL255_LEN+1];
	char value[IMS_COL255_LEN+1];
	struct ftsCatKeyValue *next;
} FTS_CAT_KEYVALUE;

/*
** Following is the request structure passed by FTS to the FTS
** catalog function, ims_ftsCat.  It contains only the information
** pertinent to the current request.
*/
typedef struct ftsCatStruct
{
	FTS_CAT_USERSPEC userSpec;         /* Database login information. */
	FTS_CAT_GRANULESPEC *granuleSpec;  /* File specification returned. */
	FTS_CAT_STATESPEC stateSpec;       /* State specicification input. */
	FTS_CAT_USERCAP userCap;           /* User capability. */
	FTS_CAT_KEYVALUE *keyValue;        /* Used for showFileKeywords. */
} FTS_CAT_STRUCT;

/*
** Enumerated types for ims_srvCat events.
*/
typedef enum fts_cat_event
{
	FTS_OPEN_CONNECTION,
	FTS_CHECK_LOGIN,
	FTS_SEARCH_GRANULE,
	FTS_SEARCH_GRANULES,
	FTS_GET_READ_COUNT,
	FTS_UPDATE_GRANULE_SIZES,
	FTS_INCR_GRANULEIDX,
	FTS_INSERT_GRANULE,
	FTS_GET_LATEST_GRANULE,
	FTS_CHECK_FILE_TYPES,
	FTS_GET_MAX_GRANULE,
	FTS_CHANGE_GRANULE_STATE, 
	FTS_DELETE_GRANULE_RECORD,
	FTS_DELETE_OLD_GRANULE_RECORD,
	FTS_ADD_GRANULE_METADATA,
	FTS_UPDATE_GRANULE_INDEX,
	FTS_CLEAR_NAME_STAMP,
	FTS_GET_USER_CAP,
	FTS_ADD_SIG_EVENT,
	FTS_BEGIN_TRANSACTION,
	FTS_ROLLBACK_TRANSACTION,
	FTS_COMMIT_TRANSACTION, 
	FTS_CLOSE_CONNECTION
} FTS_CAT_EVENT;

/*
** Structure for performing update of spatial query data.
*/ 
typedef struct fts_spatial_data
{
	char asc_data;						/* Ascent Description. */
	float near_s_lat;					/* Near Start Latitude. */
	float near_s_lon;					/* Near Start Longitude. */ 
	float near_e_lat;					/* Near End Latitude. */
	float near_e_lon;					/* Near End Longitude. */
	float far_s_lat;					/* Far Start Latitude. */
	float far_s_lon;					/* Far Start Longitude. */
	float far_e_lat;					/* Far End Latitude. */
	float far_e_lon;					/* Far End Longitude. */
	float center_lon;					/* Point Center Longitude. */
	float center_lat; 					/* Point Center Latitude. */
	float east_lon;						/* Rectangle East Longitude. */
	float west_lon;						/* Rectangle West Longitude. */
	float north_lat;					/* Rectangle North Latitude. */
	float south_lat;					/* Rectangle South Latitude. */
} FTS_SPATIAL_DATA;

/*
** Function Prototype for the ims_srvCat.c module.
*/
extern int ims_srvCat (SRV_PROC *, FTS_CAT_EVENT);

#endif	/* !IMS_FTSSRVCAT_H */
