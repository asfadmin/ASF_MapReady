/******************************************************************************
**
** File:        ims_vdfCat.h
**
** Function:    This is the header file for the ims_vdfCat.c functions
**              which perform catalog queries and updates for the VDF
**              process.
**
** Author:      David Pass
**
** Date:        4/27/95
**
******************************************************************************/

#ifndef _IMS_VDFCAT_H
#define _IMS_VDFCAT_H

static char *sccsVdfCat = "@(#)ims_vdfCat.h	5.2  07/25/96";

/*
** Internal IMS file descriptor.  Contains information on files, most
** of which comes from the catalog.
*/
typedef struct vdf_cat_request__t  *pnt_vdf_cat_request_t;
typedef struct vdf_cat_request__t{
    pnt_product_t  pnt_save_prod; /* for calls from product_list, etc */
    char    l_sat[3];  /*  satalite id - 2 chars:  for gettxrinfo */
    int     l_rev;     /*  sat revolution:  for gettxrinfo   */
    char    l_time[22];/*  time of data collection: for gettxrinfo */
    float   l_lat;     /*  latitude of frame :  for gettxrinfo  */
    float   l_lon;     /*  longitude of frame:  for gettxrinfo  */
    char    ftype[5];  /*  format type (i.e. FIXD): for mediaspace */
    int     recl;      /*  record length: for mediaspace   */
    float   blkgap;    /*  block gap for medium: for mediaspace  */
    float   filegap;   /*  file gap for medium:  for mediaspace  */
    long    numrec;    /*  no. records - for writevoldir  */
    long    reclen1;   /*  length of first record - for writevoldir  */
    long    maxlen;    /*  max len of records - for writevoldir  */
    char    granule_table[33];   /* granule table name for item  */
		long    order_id;  /* order identifier */
    long    job_id;    /* job number: see get_start_job  */
		char    media_id[IMS_COL15_LEN+1]; /* media identifier */
    short   status;    /* status of job  */
    short   media_type;/* media type: 4-mm, 8-mm hd,9-track,etc */
    short   device_id; /* device id: ?? */
    short   stage_type;/* stage for job  */
    short   no_data;/*  if true, no data for these values */
    char    path_name[256];     /* path name for granule products */
    char    user_name[13];      /* user name:  for writeordproc  */
    char    username[125];      /* needed to log onto data base  */
    char    password[125];      /* needed to log onto data base  */
    char    programName[125];   /* needed to log onto data base  */
    char    catSrvName[125];    /* needed to log onto data base  */
    char    catDbName[125];     /* needed to log onto data base  */
    IMS_MSG_STRUCT *msgDesc;
    /* ***  note:  for job processes, media_id is saved in user_name,
        host is in granule_table, and path is in path_name  */
} vdf_cat_request_t;


/*
** Following is the request structure passed to the VDF
** catalog function, ims_vdfCat.
*/

/*
** Auxiliary Catalog Events
*/
typedef enum vdf_cat_event__t
{
    VDF_OPEN_CONNECTION,
    VDF_GET_PRODUCT_LIST,
    VDF_GET_CEOS_INFO,
    VDF_GET_PRODUCT_FORMAT,
    VDF_GET_TEXT_INFO,
    VDF_GET_MEDIA_SPACE,
    VDF_GET_FILE_NAME,
    VDF_GET_PATH,
		VDF_GET_EXT_LIST,
    VDF_START_JOB,
    VDF_END_JOB,
    VDF_ADD_JOB_ITEM,
    VDF_UPDATE_JOB_ITEM,
    VDF_GET_STAGE_AREA,
    VDF_GET_FTP_AREA,
    VDF_CLOSE_CONNECTION
} vdf_cat_event_t;

/*
** Function Prototype for the ims_vdfCat.c module.
*/
extern int ims_vdfCat ( pnt_vdf_cat_request_t, vdf_cat_event_t );

/*
**  common args for vdfCat calls - extern  ?????
*/
static  vdf_cat_event_t    event;
            /* event no. for vdfCat requests  */

#endif  /* !_IMS_VDFCAT_H */
