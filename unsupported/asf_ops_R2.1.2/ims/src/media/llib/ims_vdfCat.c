static char *sccs = "@(#)ims_vdfCat.c	5.5  08/15/97";
/* *********************************************************
**
** File:        ims_vdfCat.c
**
** Function:    Executes the Sybase functions for create_vdf
**
** Author:      David Pass from ims_auxCat by Hoshyar Sayah
**
** Date:        4/22/95
**
** Modified:
**
** Notes:
**
**
** The function determines the catalog access requested by the event
** argument.  The vdfReq structure contains all necessary information
** for accessing the catalog as specified by the event.
**
**  This is all the variables used here:
**  tables:
**        order_item  o               new_order_item  n
**        keyword_policy  k           keyword_set  s
**        dataset_policy  d           dataset_relation  r
**        user_profile  u             order_queue  q
**        granule table  g  Note:  granule table name may
**              vary, as will the g. names below
**
**    variables:            type
**      o.item_id           sint   no. of product in order
**      o.process_type      c30    FULL,LOWR,GCFR-type of data - txr
**      o.p_granule_idx     sint   index in granule table
**      o.order_type        c30    ACS_SPSIN,OUT,ACS_GCOUT,AREA - fpr
**      o.media_label       c30    CCTH,etc- type of medium
**      o.order_id          int    order number
**      o.p_dataset_idx       sint   pntr to dataset_policy table
**      o.media_fmt_type    sint   flag for CEOS format
**
**      n.process_f_type    c4     FIXD, VARE
**      n.media_cap         int    no. of mb on medium
**      n.rec_count         int    no. records in reference files
**      n.byte_count        int    length of first record (all records)
**      n.order_id          int    order number
**      n.item_id           int    no. of product in order
**      n.recl              int    record length
**      n.filegap           float  gap between files in mb
**      n.blkgap            flaot  gap between records (blocks) in mb
**      n.num_recs          int    number of records: in all files
**      n.rec_len_first     int    record length of first record (data?)
**      n.rec_len_max       int    max record length in data?
**      n.process_create    c3     SPS is usual value
**
**      k.keyword           c30    keyword name for query_type
**      k.keyword_idx       sint   keyword index for s.
**
**      s.query_type        sint   query value for desired function
**      s.keyword_idx       sint   keyword index for k.
**      s.dataset_idx       sint   dataset_idx value in order_item
**
**      d.granules_table    c30    name of granules table to be used
**      d.dataset_idx       sint   dataset index in order_item
**
**      g.(center_lat,lon,  float  (granule info)
**      g.REV       time    int
**      g.name              sint   name (number) of granule files
**
**      r.platform          c30    platform name (ERS-1+) - need 2 chars
**      r.dataset_idx       sint   dataset index in order_item
**
**      u.first_name        c20    user name - for id on tape
**      u.last_name         c20
**      u.user_id           c15    from q. below
**
**      q.user_id           c15    find user_id for u.
**      q.order_id          int    order_id from order_item.
**
**
** Note:   byte_count is used if process_f_type is VARE (all
**             files same length) and rec_len_first and
**             re_len_max used if process_f_type is FIXD.
**            (Craig added this change.)
**
**
********************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_util.h>

/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_vdfCat.h.
** It is listed here for reference.
**
**    int ims_vdfCat (VDF_CAT_REQUEST *, VDF_CAT_EVENT);
*/

/*
** Definition of local constants
*/
#define BUF_SIZE     1024

/*
** Local Functions
*/
int get_items_value( pnt_vdf_cat_request_t, char * , short, char * );
int openConnection2( pnt_vdf_cat_request_t );
int get_product_list( pnt_vdf_cat_request_t, short );
int get_CEOS_info( pnt_vdf_cat_request_t );
int get_product_format( pnt_vdf_cat_request_t );
int start_media_job( pnt_vdf_cat_request_t );
int end_media_job( pnt_vdf_cat_request_t );
int get_text_info( pnt_vdf_cat_request_t );
int get_media_space( pnt_vdf_cat_request_t );
int get_stage_area( pnt_vdf_cat_request_t );
int get_ftp_area( pnt_vdf_cat_request_t );
int get_path( pnt_vdf_cat_request_t );
int getFileName (pnt_vdf_cat_request_t);
int getExtList (pnt_vdf_cat_request_t);
int execCmd (IMS_MSG_STRUCT *);
int checkRetStatus (IMS_MSG_STRUCT *);

/*
** The command buffer. Used by all routines in this file when a
** SQL command string must be built.
*/
static char cmdBuf[BUF_SIZE];

/*
** Pointer to the catalog query descriptor used by all routines in this
** file when a Catalog Query Interface function is called.  This
** descriptor is allocated and this pointer initialized the first
** time ims_ftsCat iscalled.  The pointer is initialized to
** (IMS_QI_DESC_OBJ *) to inidicate that the process has not yet logged
** into the catalog database. The catalog connection is not closed until
** the CLOSE_CONNECTION event is called by the FTR process as it exits.
*/
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;

/* *************************************************************
**
** ims_vdfCat ()
**
** function handling VDF catalog queries within media program.
**
**************************************************************** */

int ims_vdfCat (
    pnt_vdf_cat_request_t   pnt_vdf_Req,
    vdf_cat_event_t event)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    static  short  first_time;

    msgDesc = pnt_vdf_Req->msgDesc;

    /*
    ** We must first make sure that we have a descriptor if the
    ** event is anything but VDF_OPEN_CONNECTION.
    */
    if ((qDesc == (IMS_QI_DESC_OBJ *) NULL) &&
        (event != VDF_OPEN_CONNECTION)){
        (void) ims_msg (msgDesc, IMS_FATAL,
    "VDF_OPEN_CONNECTION must be the first vdfCat event called.");
        return (IMS_FATAL);
    }

    /*
    ** Now, let's do our 'catalog' business according to the type of
    ** event passed into the function.
    */
    switch (event){
    case VDF_OPEN_CONNECTION:
        status = openConnection2( pnt_vdf_Req );
        first_time = TRUE;
        /*
        ** Return from here rather than break because there is no need
        ** to call ims_qiCancel() before leaving as there is for all
        ** other events, except VDF_CLOSE_CONNECTION
        */
        pnt_vdf_Req->l_sat[0] = '\0';
        pnt_vdf_Req->l_rev = -1;
        pnt_vdf_Req->l_time[0] = '\0';
        pnt_vdf_Req->l_lat = -1000.0;
        pnt_vdf_Req->l_lon = -1000.0;
        pnt_vdf_Req->ftype[0] = '\0';
        pnt_vdf_Req->recl = -1;
        pnt_vdf_Req->blkgap = 0.0;
        pnt_vdf_Req->filegap = 0.0;
        pnt_vdf_Req->numrec = -1;
        pnt_vdf_Req->reclen1 = -1;
        pnt_vdf_Req->maxlen = -1;
        pnt_vdf_Req->no_data = 0;
        pnt_vdf_Req->path_name[0] = '\0';
        return (status);

    case VDF_GET_PRODUCT_LIST:
        status = get_product_list (pnt_vdf_Req, first_time );
        first_time = FALSE;
        break;

    case VDF_GET_CEOS_INFO:
        status = get_CEOS_info (pnt_vdf_Req );
        break;

    case VDF_GET_PRODUCT_FORMAT:
        status = get_product_format (pnt_vdf_Req );
        break;

    case VDF_GET_TEXT_INFO:
        status = get_text_info (pnt_vdf_Req );
        break;

    case VDF_GET_MEDIA_SPACE:
        status = get_media_space (pnt_vdf_Req );
        break;

    case VDF_GET_PATH:
        status = get_path( pnt_vdf_Req );
        break;

    case VDF_GET_FILE_NAME:
        status = getFileName (pnt_vdf_Req);
        break;

    case VDF_GET_EXT_LIST:
        status = getExtList (pnt_vdf_Req);
        break;

    case VDF_END_JOB:
        /* End a media job. */
        status = end_media_job (pnt_vdf_Req);
        break;

    case VDF_START_JOB:
        /* Start a media job. */
        status = start_media_job (pnt_vdf_Req);
        break;

    case VDF_GET_STAGE_AREA:
        /* need to get staging area (directory for temp storage) */
        status = get_stage_area( pnt_vdf_Req );
        break;

    case VDF_GET_FTP_AREA:
        /* Obtain the FTP user directory for product distribution. */
        status = get_ftp_area( pnt_vdf_Req );
        break;

    case VDF_CLOSE_CONNECTION:
        /* Close the catalog connection. */
        status = ims_qiResetDesc( qDesc );
        ims_qiExit ();
        return (IMS_OK);

    default:
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Invalid catalog event passed to ims_vdfCat.");
        status = IMS_FATAL;
        break;
    }

    /*
    ** Release all query-allocated space and re-initialize qDesc for the
    ** next time in, leaving open the connection to the catalog.
    */
    if (qDesc->dbproc != (DBPROCESS *) NULL)
    {
        (void) ims_qiCancel (qDesc);
    }

    /*
    ** Return with the appropriate status
    */
    if (status < IMS_OK)
    {
        return (status);
    }

    return ( IMS_OK );
}   /*  ims_vdfCat   */

/* *****************************************************
**
**  subr openConnection2()
**
******************************************************** */

int openConnection2(
    pnt_vdf_cat_request_t  pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;

    msgDesc = pnt_vdf_Req->msgDesc;

    /*
    ** We only want to generate the descriptor once, the first
    ** time in.  We also want to stay logged into the catalog
    ** until the VDF_CLOSE_CONNECTION event is called by the
    ** VDF process.
    */

    /*
    ** Since this is the first time to access the catalog, we
    ** need a query descriptor allocated.  If we can't get a
    ** descriptor, return with a bad status ... we can't go on.
    */
    if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return (IMS_FATAL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, pnt_vdf_Req->username);
    IMS_SETPSWD (qDesc, pnt_vdf_Req->password);
    IMS_SETPROG (qDesc, pnt_vdf_Req->programName);

    if ((int) strlen (pnt_vdf_Req->catSrvName) > 0)
    {
        IMS_SETSERVER (qDesc, pnt_vdf_Req->catSrvName);
    }

    if ((int) strlen (pnt_vdf_Req->catDbName) > 0)
    {
        IMS_SETDBNAME (qDesc, pnt_vdf_Req->catDbName);
    }

    IMS_SET_VERBOSE (qDesc, 10);
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Login to the catalog database.
    */
    if ((status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        (void) ims_qiFreeDesc (qDesc);
        return (status);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (IMS_OK);
}   /*  openConnection2   */


/* *****************************************************************
**
**  subr  get_product_list is called by getProductList to obtain the
**      necessary table values.
**
**  5/17/95: modified by D. Pass because now the item_id is input.
**      return the data for the given item_id.
**
***************************************************************** */

int get_product_list (
    pnt_vdf_cat_request_t   pnt_vdf_Req, short  first_time )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    static  int row_count;
    long  j;
    char  str[1024];
    short i1, i2, i3;


    /* set constants:  were variable in old system, but not now  */
    (void) strcpy( pnt_vdf_Req->pnt_save_prod->prodftype, "FIXD" );
    (void) strcpy( pnt_vdf_Req->pnt_save_prod->mediaid, "ACS_SPSIN" );
    pnt_vdf_Req->numrec = 720; /* header (1st) file for leader and
        trailer files are always this size.  */

    msgDesc = pnt_vdf_Req->msgDesc;

    (void) strcpy( cmdBuf, "select o.media_fmt_type, o.process_type, ");
    (void) strcat( cmdBuf, "m.capacity, o.media_type, " );
    (void) strcat( cmdBuf, "d.granules_table  from  order_item o,  " );
    (void) strcat( cmdBuf, "media_policy m,  dataset_policy d  where ");
    (void) strcat( cmdBuf, " o.order_id  =  " );
    (void) strcat( cmdBuf,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
    (void) strcat( cmdBuf, "  and  o.item_id  =  " );
    (void) strcat( cmdBuf,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
    (void) strcat( cmdBuf, "  and o.p_dataset_idx  =  d.dataset_idx" );
    (void) strcat( cmdBuf, "  and o.media_type  =  m.media_type" );


    pnt_vdf_Req->no_data = FALSE;
    if(  first_time )  row_count = 0;
    else{ /*  need to reset descriptor  */
        status = ims_qiResetDesc( qDesc );
    }
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK)
    {
        return (ims_msgGetSeverity (msgDesc));
    }

    /*
    ** If ENDOFQUERY, we want to finish out command and return.
    */
    if (status == IMS_ENDOFQUERY)
    {
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }

    /*
    ** A row has been returned.
    */
    row_count++;

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  o.media_fmt_type  */
    (void) memcpy ((char *) &i1,
        qDesc->valAddr[j], qDesc->valLength[j]);
    /* ***  finished below  **** */

    j++;                                /*  o.process_type  */
    (void) memcpy ((char *) &i2,
        qDesc->valAddr[j], qDesc->valLength[j]);
    /* ***  finished below  **** */

    j++;                                /*  m.capacity  */
    (void) memcpy ((char *) &(pnt_vdf_Req->pnt_save_prod->mediacap),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;    /* **** j=4  */             /*  o.media_type  */
    (void) memcpy ((char *) &i3,
        qDesc->valAddr[j], qDesc->valLength[j]);
    /* ***  finished below  **** */

    j++;                                /* d.granule_table  */
    (void) memcpy ((char *) pnt_vdf_Req->granule_table,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->granule_table[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( pnt_vdf_Req->granule_table );
    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_product_list had error(1)." );
        return( IMS_ERROR );
    }

    /* **********  now get ascii names for indices  */
    status = get_items_value( pnt_vdf_Req, "media_fmt_type", i1, str );
    /* this should be CEOS - check  */
    if(  strcmp( str, "CEOS"  )  ==  0 )
        pnt_vdf_Req->pnt_save_prod->CEOS = TRUE;
    else  pnt_vdf_Req->pnt_save_prod->CEOS = FALSE;
    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat: get_product_list had error(2)." );
        return( IMS_ERROR );
    }

    status = get_items_value( pnt_vdf_Req, "process_type", i2, str );
    /*  convert it to old keyword:  5 chars  */
    /*  note:  this done as 5 chars are put into the ceos records */
    if (strcmp (str, "STANDARD (FULL-RES)") == 0)
        (void) strcpy (str, "FULL");
    else if (strcmp (str, "STANDARD (LOW-RES)") == 0)
        (void) strcpy (str, "LOWR");
    else if (strcmp (str, "CCSD") == 0)
        (void) strcpy (str, "CCSD");
    else if (strcmp (str, "COMPLEX") == 0)
        (void) strcpy (str, "CMPX");
    else if (strcmp (str, "STANDARD (MED-RES)") == 0)
        (void) strcpy (str, "MEDR");
    else if (strcmp (str, "COMP (LOW-RES)") == 0)
        (void) strcpy (str, "CMPL");
    else if (strcmp (str, "COMP (MED-RES)") == 0)
        (void) strcpy (str, "CMPM");
    else if (strcmp (str, "COMP (FULL-RES)") == 0)
        (void) strcpy (str, "CMPF");
    else if (strcmp (str, "UNCOMP (LOW-RES)") == 0)
        (void) strcpy (str, "UCPL");
    else if (strcmp (str, "UNCOMP (MED-RES)") == 0)
        (void) strcpy (str, "UCPM");
    else if (strcmp (str, "UNCOMP (FULL-RES)") == 0)
        (void) strcpy (str, "UCPF");
    else  (void) strcpy (str, "UNKN");
    (void) strcpy( pnt_vdf_Req->pnt_save_prod->prodtype, str );

    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_product_list had error(3)." );
        return( IMS_ERROR );
    }

    status = get_items_value( pnt_vdf_Req, "media_type", i3, str );
    /*  convert mediacode to what it used to be:  CCTH, etc */
    if(  strcmp( str, "4-MM" )  ==  0  )  (void) strcpy( str, "4MML" );
    else  if(  strcmp( str, "4-MM HD" )  ==  0  )
        (void) strcpy( str, "4MMH" );
    else  if(  strcmp( str, "8-MM" )  ==  0  )
        (void) strcpy( str, "8MML" );
    else  if(  strcmp( str, "8-MM HD" )  ==  0  )
        (void) strcpy( str, "8MMH" );
    else  if(  strcmp( str, "9-TRACK" )  ==  0  )
        (void) strcpy( str, "CCTL" );
    else  if(  strcmp( str, "9-TRACK HD" )  ==  0  )
        (void) strcpy( str, "CCTH" );
    (void) strcpy( pnt_vdf_Req->pnt_save_prod->mediacode, str );

    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_product_list had error(4)." );
        return( IMS_ERROR );
    }
    return (IMS_OK);
}   /*  get_product_list   */


/* *****************************************************************
**
**  subr  get_CEOS_info is called by GetProdList() to obtain the
**      necessary table values.  get_product_list has already been
**      called:  this is to make the call to the granule_table table.
**
***************************************************************** */

int get_CEOS_info (
    pnt_vdf_cat_request_t   pnt_vdf_Req )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    static  int row_count;
    long  i,j;


    msgDesc = pnt_vdf_Req->msgDesc;
    i =  strlen( pnt_vdf_Req->granule_table );
    if(  i  <=  0 ){
        ims_msg( msgDesc, IMS_ERROR,
            "get_CEOS_info:  granule table is empty for item %d.",
            pnt_vdf_Req->pnt_save_prod->item_id );
        return( IMS_ERROR );
    }

    (void) strcpy( cmdBuf, "select g.PRODUCT_CREATOR, "  );
    (void) strcat( cmdBuf, "g.IMAGE_RECORD_COUNT, "   );
    (void) strcat( cmdBuf, "g.IMAGE_MAX_RECORD_LENGTH, " );
    (void) strcat( cmdBuf, "g.LEADER_RECORD_COUNT, " );
    (void) strcat( cmdBuf, "g.LEADER_MAX_RECORD_LENGTH, " );
    (void) strcat( cmdBuf, "g.format " );
    (void) strcat( cmdBuf, "   from   order_item o , " );
    (void) strcat( cmdBuf, pnt_vdf_Req->granule_table );
    (void) strcat( cmdBuf, " g   where  "   );
    (void) strcat( cmdBuf, " o.order_id  =  " );
    (void) strcat( cmdBuf,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
    (void) strcat( cmdBuf, "  and  o.item_id  =  " );
    (void) strcat( cmdBuf,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
    (void) strcat( cmdBuf, "  and o.p_granule_idx  =  g.granule_idx" );


    pnt_vdf_Req->no_data = FALSE;
    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK){
        return (ims_msgGetSeverity (msgDesc));
    }

    /*
    ** If ENDOFQUERY, we want to finish out command and return.
    */
    if (status == IMS_ENDOFQUERY){
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }

    /*
    ** A row has been returned.
    */
    row_count++;

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  g.PRODUCT_CREATOR */
    (void) memcpy ((char *) pnt_vdf_Req->pnt_save_prod->prodftype,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->pnt_save_prod->prodftype[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( pnt_vdf_Req->pnt_save_prod->prodftype );

    j++;                                /*  g.IMAGE_RECORD_COUNT */
    (void) memcpy ((char *) &(pnt_vdf_Req->pnt_save_prod->num_recs),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;    /* **** j=2  */       /*  g.IMAGE_MAX_RECORD_LENGTH */
    (void) memcpy ((char *) &(pnt_vdf_Req->pnt_save_prod->rec_len),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;                         /*   g.LEADER_RECORD_COUNT  */
    (void) memcpy ((char *) &(pnt_vdf_Req->numrec),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;                        /*  g.LEADER_MAX_RECORD_LENGTH */
    (void) memcpy ((char *) &(pnt_vdf_Req->maxlen),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;                              /*  g.format */
    (void) memcpy ((char *) pnt_vdf_Req->pnt_save_prod->format,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->pnt_save_prod->format[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr (pnt_vdf_Req->pnt_save_prod->format);

    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_CEOS_info had error." );
        return( IMS_ERROR );
    }

    return (IMS_OK);
}   /*  get_CEOS_info   */

/******************************************************************************
**
** get_product_format ()
**
** Obtain the format for a given granule.
**
******************************************************************************/

int get_product_format (
    pnt_vdf_cat_request_t pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdf_Req->msgDesc;
    pnt_vdf_Req->no_data = FALSE;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Populate the command buffer.
    */
    (void) sprintf (cmdBuf,
        "select format "
        "from order_item o, %s g "
        "where o.order_id = %d "
        "and o.item_id = %d "
        "and o.p_granule_idx = g.granule_idx",
        pnt_vdf_Req->granule_table,
        pnt_vdf_Req->pnt_save_prod->order_id,
        pnt_vdf_Req->pnt_save_prod->item_id);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    /*
    ** Check to see if we got a row back.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (IMS_ERROR);
    }

    /*
    ** Copy the returned data into the structure.
    */
    (void) memcpy ((char *) pnt_vdf_Req->pnt_save_prod->format,
        qDesc->valAddr[0], qDesc->valLength[0]);
    pnt_vdf_Req->pnt_save_prod->format[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (pnt_vdf_Req->pnt_save_prod->format);

    return (IMS_OK);
}

/* *****************************************************************
**
**  subr  get_text_info is called by getTXRInfo to obtain the
**      necessary table values.  each call depended on what
**      the product was.  this is now done by the different
**      granule tables, so only one call made.  note that the
**      last,long,time values may have different nemes, so the
**      generic name must be used.
**
**  This is one of the Ingres statements with given products:
##        repeat retrieve (    l_sat = left(sarcpxccsd.dtid,2),
##              l_rev = int4(left(shift(sarcpxccsd.dtid,-5),5)),
##              l_time = sarcpxccsd.centertime,
##              l_lat = sarcpxccsd.centerlat,
##              l_lon = sarcpxccsd.centerlon)
##        where    sarcpxccsd.pid = int4(@prodid);
***************************************************************** */

int get_text_info (
    pnt_vdf_cat_request_t   pnt_vdf_Req  )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    char  str[1024],str2[1024];
    char  c_time_name[33];
    char  c_lat_name[33];
    char  c_lon_name[33];
    char  granule_name[33];
    long  i,j;

    union  float_char{  /*  needed because type-casting a   */
            float rl;  /*   float or double vars is now illegal */
            double  fl;
            char   ch[16];
    };
    union  float_char  float_chars;


    msgDesc = pnt_vdf_Req->msgDesc;

    /*  need to build up the sql command.  this
        is complicated by the fact that some of the names change
        between the granules for the same function.  however, a generic
        name has been set up in the table items, and corresponds to
        query_type in table keyword_set.  the actual name is in
        keyword from table keyword_policy.  the names that have to be
        built up are  center_time, center_lat, and center_lon which
        are 3,4, and 5 respectively as query_type.  the name REV is
        used on all the tables.
        the satalite name is found as platform in table data_relation.
        */
    (void) strcpy( str, "select k.keyword  from  keyword_policy k, \
    keyword_set s, order_item o  \
    where o.order_id  =  " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
    (void) strcat( str, "  and  o.item_id  =  " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
    (void) strcat( str,
        "  and  o.p_dataset_idx  =  s.dataset_idx  and  " );
    (void) strcat( str, " s.keyword_idx  =  k.keyword_idx  and  " );
    (void) strcat( str, "s.query_type  =  " );
    /*  save the string here so that the the searches for the names for
        4 and 5 (center_lat and center_lon) can be added easily.  */
    (void) strcpy( str2, str );
    (void) strcat( str, "3" );
    (void) strcpy( cmdBuf, str );

    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    /*  get center_time :  select built up */
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK){
        return (ims_msgGetSeverity (msgDesc));
    }
    j = 0;
    (void) memcpy ((char *) c_time_name,
        qDesc->valAddr[j], qDesc->valLength[j]);
    c_time_name[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( c_time_name );

    /* *****  now query for ceter_lat  */
    (void) strcpy( str, str2 );
    (void) strcat( str, "4" );
    (void) strcpy( cmdBuf, str );
    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    /*  get center_lat  */
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK)
    {
        return (ims_msgGetSeverity (msgDesc));
    }
    j = 0;
    (void) memcpy ((char *) c_lat_name,
        qDesc->valAddr[j], qDesc->valLength[j]);
    c_lat_name[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( c_lat_name );

    /* ***** now query for center_lon  */
    (void) strcpy( str, str2 );
    (void) strcat( str, "5" );
    (void) strcpy( cmdBuf, str );
    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    /*  get center_lat  */
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK){
        return (ims_msgGetSeverity (msgDesc));
    }
    j = 0;
    (void) memcpy ((char *) c_lon_name,
        qDesc->valAddr[j], qDesc->valLength[j]);
    c_lon_name[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( c_lon_name );

    if(  pnt_vdf_Req->granule_table[0]  ==  '\0' ){
        /* granule_table probably already set - check  */
        /*  now we need granule table name - from dataset_policy  */
        (void) strcpy( str,
            "select d.granules_table   from  dataset_policy d, " );
        (void) strcat( str, " order_item o where o.order_id  =  " );
        (void) strcat( str,
            ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
        (void) strcat( str, "  and  o.item_id  =  " );
        (void) strcat( str,
            ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
        (void) strcat( str, "  and  o.p_dataset_idx  =  d.dataset_idx" );
        (void) strcpy( cmdBuf, str );
        /*  need to reset descriptor  */
        status = ims_qiResetDesc( qDesc );

        /*  get table name :  select built up */
        status = ims_qiNextRow (qDesc);
        if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
            pnt_vdf_Req->no_data = TRUE;
            status = IMS_OK;
            return( status );
        }
        if (status < IMS_OK){
            return (ims_msgGetSeverity (msgDesc));
        }
        j = 0;
        (void) memcpy ((char *) granule_name,
            qDesc->valAddr[j], qDesc->valLength[j]);
        granule_name[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( granule_name );
        if(  pnt_vdf_Req->granule_table[0]  ==  '\0' ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_text_info: No granule table name for item %d.",
                pnt_vdf_Req->pnt_save_prod->item_id );
            return( IMS_ERROR );
        }
    }
    else  (void) strcpy( granule_name, pnt_vdf_Req->granule_table );
    /*  now we can build up the select statement for
        all the values  */
    (void) strcpy( str, "select g." );
    (void) strcat( str, c_time_name );
    (void) strcat( str, ", g." );
    (void) strcat( str, c_lat_name );
    (void) strcat( str, ", g." );
    (void) strcat( str, c_lon_name );
    (void) strcat( str, ", g.REVOLUTION, p.acronym, g.name  from " );
    (void) strcat( str, granule_name );
    (void) strcat( str, " g, dataset_relation r, order_item o," );
    (void) strcat( str, "   platforms p  where " );
    (void) strcat( str, " o.order_id  =  " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
    (void) strcat( str, "  and  o.item_id  =  " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
    (void) strcat( str,
        "  and  o.p_dataset_idx  =  r.dataset_idx  and  " );
    (void) strcat( str, " g.granule_idx  =  o.p_granule_idx" );
    (void) strcat( str, "   and  r.platform  =  p.platform" );
    (void) strcpy( cmdBuf, str );
    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    /*  get values :  select built up */
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK){
        return (ims_msgGetSeverity (msgDesc));
    }
    j = 0;                              /* center_time */
    (void) memcpy ((char *) (pnt_vdf_Req->l_time),
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->l_time[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( pnt_vdf_Req->l_time );

    j++;                                /* center_lat  */
    (void) memcpy ( float_chars.ch,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->l_lat = float_chars.rl;

    j++;                                /* center_lon */
    (void) memcpy ( float_chars.ch,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->l_lon = float_chars.rl;

    j++;                                /* rev  */
    (void) memcpy ((char *) &(pnt_vdf_Req->l_rev),
        qDesc->valAddr[j], qDesc->valLength[j]);

    j++;                        /* acronym (platform)  */
    (void) memcpy ((char *) str,
        qDesc->valAddr[j], qDesc->valLength[j]);
    str[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( str );

    j++;                                /* name  */
    (void) memcpy( str,
        qDesc->valAddr[j], qDesc->valLength[j]);
    str[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( str );

    i = strlen( str );
    if(  i  >  15  ){
        /*
        **  database allows 30 chars, but ceos only allows
        **      15, so truncate and print warning.
        */
        (void) ims_msg( msgDesc, IMS_WARNING,
            "Product ID '%s' greater then the 15 characters allowed"
            " by CEOS: trucnated.", str );
        str[16] = '\0';
    }
    (void) strcpy( pnt_vdf_Req->pnt_save_prod->prodid, str );

    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_text_info had error." );
        return( IMS_ERROR );
    }

    return (IMS_OK);
}   /*  get_text_info   */


/* *****************************************************************
**
**  subr  get_media_space is called by MediaSpace to obtain the
**      necessary table values.  all the values are in the new
**      table, as currently not in the database.
**
**  This is the Ingres statements:
## RETRIEVE ( ftype = ordproduct.prodftype, recl = ordproduct.prodrecl )
##  WHERE ordproduct.prodtype = ptype
**
**  and:
**
## RETRIEVE ( blkgap = ordmedia.mediablkgap, filegap =
##    ordmedia.mediafilegap )
##  WHERE ordmedia.mediacode = code
## INQUIRE_INGRES ( errnum = ERRORNO, nrows = ROWCOUNT )
***************************************************************** */

int get_media_space (
    pnt_vdf_cat_request_t   pnt_vdf_Req )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    char  str[1024];
    long  j;

    union  float_char{  /*  needed because type-casting a   */
            double  fl;  /*   float var is now illegal   */
            char   ch[16];
    };
    union  float_char  float_chars;


    msgDesc = pnt_vdf_Req->msgDesc;

    /*  need to build up the sql command.  the new data is
        in the new table.
        */
    (void) strcpy( str, "select m.file_gap, m.block_gap  from  " );
    (void) strcat( str, "media_policy m, order_item o  where   " );
    (void) strcat( str, "o.order_id  = " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->order_id ) );
    (void) strcat( str, "  and  o.item_id  =  " );
    (void) strcat( str,
        ims_ltoa( pnt_vdf_Req->pnt_save_prod->item_id ) );
    (void) strcat( str, "  and  o.media_type  =  m.media_type" );
    (void) strcpy( cmdBuf, str );

    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    /*  get values :  select built up */
    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK)
    {
        return (ims_msgGetSeverity (msgDesc));
    }

    pnt_vdf_Req->recl = pnt_vdf_Req->pnt_save_prod->rec_len;

    j = 0;                                  /*  m.file_gap  */
    (void) memcpy (  float_chars.ch,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->filegap = float_chars.fl;

    j++;                                    /*  m.block_gap  */
    (void) memcpy ( float_chars.ch,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->blkgap = float_chars.fl;
    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_media_space had error." );
        return( IMS_ERROR );
    }

    return (IMS_OK);
}   /*  get_media_space   */


/* *****************************************************************
**
**  subr  get_items_value is called by these Cat pgms to get a
**      name from the items table given an instance and name of
**      table type.  For instance, media_type in order_item table
**      has an sint type, and the character string representing
**      that value is in the items table with type = "media_type"
**      and the given integer as instance under the name description.
***************************************************************** */

int   get_items_value( pnt_vdf_cat_request_t   pnt_vdf_Req,
    char * type_name,  short instance, char * out_name )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    long  i,j;
    char  str[1024];


    msgDesc = pnt_vdf_Req->msgDesc;

    (void) strcpy( str, "select description from items   where  " );
    (void) strcat( str, " type  =  " );
    (void) strcat( str, "\""  );
    (void) strcat( str, type_name );
    (void) strcat( str, "\"   and   instance   =   "  );
    i = instance;
    (void) strcat( str, ims_ltoa( i ) );
    (void) strcpy( cmdBuf, str );

    out_name[0] = '\0';
    pnt_vdf_Req->no_data = FALSE;
    status = ims_qiResetDesc( qDesc );

    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_ERROR;
        return ( status );
    }
    if (status < IMS_OK){
        return ( status );
    }

    /*
    ** If ENDOFQUERY, we want to finish out command and return.
    */
    if (status == IMS_ENDOFQUERY){
        pnt_vdf_Req->no_data = TRUE;
        status = IMS_ERROR;
        return (status );
    }

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  description  */
    (void) memcpy ((char *) out_name,
        qDesc->valAddr[j], qDesc->valLength[j]);
    out_name[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( out_name );

    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  get_items_value had error." );
        return ( IMS_ERROR );
    }

    return( IMS_OK ) ;
}   /*  get_items_value   */


/* *****************************************************************
**
** subr start_media_job
**
** Starts a media job. The inputs are media_type,
** order_id, device_id and media_id. The outputs are job_id.
**
***************************************************************** */

int start_media_job (
    pnt_vdf_cat_request_t pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    long j;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdf_Req->msgDesc;
    pnt_vdf_Req->no_data = FALSE;

    /*
    ** Populate the command buffer.
        */
    if (strlen (pnt_vdf_Req->media_id) == 0)
    {
        (void) sprintf(cmdBuf, "med_start_job  %ld, %d, %d",
            pnt_vdf_Req->order_id,
            pnt_vdf_Req->device_id, pnt_vdf_Req->media_type);
    }
    else
    {
        (void) sprintf(cmdBuf, "med_start_job  %ld, %d, %d, '%s'",
            pnt_vdf_Req->order_id,
            pnt_vdf_Req->device_id, pnt_vdf_Req->media_type,
            pnt_vdf_Req->media_id);
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    /*
    ** Check to see if we got a row back.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (IMS_ERROR);
    }

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  job_id */
    (void) memcpy ((char *) &(pnt_vdf_Req->job_id),
        qDesc->valAddr[j], qDesc->valLength[j]);

    return (IMS_OK);
}   /*  start_media_job   */


/* *****************************************************************
**
** subr end_media_job
**
** Ends a media job. The inputs are job_id and status.
** There are no outputs.
**
***************************************************************** */

int end_media_job (
        pnt_vdf_cat_request_t pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    long j;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdf_Req->msgDesc;
    pnt_vdf_Req->no_data = FALSE;

    /*
    ** Populate the command buffer.
        */
    (void) sprintf (cmdBuf, "med_end_job  %ld, %d",
        pnt_vdf_Req->job_id, pnt_vdf_Req->status);

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    return (IMS_OK);
}   /*  end_media_job   */


/* *****************************************************************
**
**  subr get_stage_area gets storage directory.  note that host
**      array is saved in granule_table and path is in path_name.
**      input:  stage_type.
***************************************************************** */

int get_stage_area(
    pnt_vdf_cat_request_t   pnt_vdf_Req )
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    long j;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdf_Req->msgDesc;
    pnt_vdf_Req->no_data = FALSE;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Populate the command buffer.
    */
    (void) sprintf (cmdBuf, "med_get_stage_area  %d",
        pnt_vdf_Req->stage_type);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    /*
    ** Check to see if we got a row back.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (IMS_ERROR);
    }

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  host */
    (void) memcpy ((char *) pnt_vdf_Req->granule_table,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->granule_table[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( pnt_vdf_Req->granule_table );

    j++;                                /*  path */
    (void) memcpy ((char *) pnt_vdf_Req->path_name,
        qDesc->valAddr[j], qDesc->valLength[j]);
    pnt_vdf_Req->path_name[qDesc->valLength[j]] = '\0';
    (void) ims_truncStr( pnt_vdf_Req->path_name );

    return (IMS_OK);
}   /*  get_stage_area   */


/* *****************************************************************
**
**  subr get_ftp_area
**
**  Obtain the FTP user directory for product distribution.
**
***************************************************************** */

int get_ftp_area (
    pnt_vdf_cat_request_t pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    long j;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdf_Req->msgDesc;
    pnt_vdf_Req->no_data = FALSE;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Populate the command buffer.
    */
    (void) sprintf (cmdBuf, "med_get_ftp_area  %d",
        pnt_vdf_Req->pnt_save_prod->order_id);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    /*
    ** Check for one row returned.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (IMS_ERROR);
    }

        if (IMS_AFFECTED (qDesc) > 1)
        {
            pnt_vdf_Req->no_data = TRUE;
            return (IMS_ERROR);
        }

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                                /* path */
    if ((qDesc->valLength[j] == 0) ||
        (qDesc->valAddr[j] == (char *) NULL))
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The FTP staging area specification was NULL for order '%ld'.",
            pnt_vdf_Req->pnt_save_prod->order_id);
        return (IMS_ERROR);
    }
    else
    {
        (void) memcpy (pnt_vdf_Req->path_name,
            qDesc->valAddr[j], qDesc->valLength[j]);
        pnt_vdf_Req->path_name[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr (pnt_vdf_Req->path_name);
    }

    return (IMS_OK);
}   /*  get_ftp_area   */

/******************************************************************************
**
** getFileName ()
**
** Obtains the product name and format from the granules table.
**
******************************************************************************/

int getFileName (
    pnt_vdf_cat_request_t  pnt_vdfReq)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    int rowCount;
    char str[1024];
    char granuleName[32];
    int granuleIdx;

    msgDesc = pnt_vdfReq->msgDesc;

    /*
    ** First search for the granule table name.
    */
    (void) sprintf (str, "select granules_table, p_granule_idx from \
        dataset_policy d, order_item o where \
        d.dataset_idx = o.p_dataset_idx and order_id = %d and\
        item_id = %d", pnt_vdfReq->pnt_save_prod->order_id,
        pnt_vdfReq->pnt_save_prod->item_id);

    qDesc->cmd = (char *)  &str[0];
    rowCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return(ims_msgGetSeverity(msgDesc));
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Grab granuleName and granuleIdx
        */

        if (rowCount == 1)
        {
            (void) memcpy((char *) granuleName,
                qDesc->valAddr[0], qDesc->valLength[0]);
            granuleName[qDesc->valLength[0]] = '\0';

            (void) memcpy((char *) &granuleIdx,
                qDesc->valAddr[1], qDesc->valLength[1]);
        }

    }

    /*
    ** Now use the granuleName and granuleIdx to determine the
    ** name of the product.
    */

    status = ims_qiResetDesc( qDesc );

    if (status < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not reset descriptor in getFileName");
        return(IMS_ERROR);
    }

    (void) sprintf(str,
        "select name, format, version, status \
        from %s where granule_idx = %d",
        granuleName, granuleIdx);

    rowCount = 0;
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return(ims_msgGetSeverity(msgDesc));
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Copy the result row.
        */

        if (rowCount == 1)
        {
                        /* name */
            (void) memcpy (pnt_vdfReq->pnt_save_prod->name,
                qDesc->valAddr[0], qDesc->valLength[0]);
            pnt_vdfReq->pnt_save_prod->name[qDesc->valLength[0]] = '\0';

                        /* format */
            (void) memcpy (pnt_vdfReq->pnt_save_prod->format,
                qDesc->valAddr[1], qDesc->valLength[1]);
            pnt_vdfReq->pnt_save_prod->format[qDesc->valLength[1]] = '\0';
            (void) ims_truncStr (pnt_vdfReq->pnt_save_prod->format);

                        /* version */
            if ((IMS_VALUELENGTH (qDesc, 2) == 0) ||
                (IMS_VALUE (qDesc, 2) == (char *)NULL))
            {
                /* Version is null. */
                pnt_vdfReq->pnt_save_prod->version = NO_VERSION;
            }
            else
            {
                (void) memcpy (&(pnt_vdfReq->pnt_save_prod->version),
                   qDesc->valAddr[2], qDesc->valLength[2]);
            }

            /* status */
            (void) memcpy (&(pnt_vdfReq->pnt_save_prod->status),
                   qDesc->valAddr[3], qDesc->valLength[3]);
        }
    }

    /*
    ** Check the return status
    */

    if (checkRetStatus (msgDesc) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ims_vdfCat:  getFileName had error." );
        return ( IMS_ERROR );
    }

    /* reset the cmd to cmdBuf  */
    IMS_SETCMD (qDesc, cmdBuf);

    return (IMS_OK);
}

/******************************************************************************
**
** get_path ()
**
** Obtains the path for the product files associated with the given
** order item. This function also determines whether a trailer file
** exists for the current product.
**
******************************************************************************/

int get_path (
    pnt_vdf_cat_request_t pnt_vdf_Req)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;

    /*
    ** Initialize variables.
    */
    pnt_vdf_Req->no_data = FALSE;
    msgDesc = pnt_vdf_Req->msgDesc;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Populate the command buffer with the stored procedure call.
    */
    (void) sprintf (cmdBuf, "med_get_granule_path %d, %d",
        pnt_vdf_Req->pnt_save_prod->order_id,
        pnt_vdf_Req->pnt_save_prod->item_id);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        pnt_vdf_Req->no_data = TRUE;
        return (status);
    }

    /*
    ** Check for a row returned.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not obtain a product path for item '%d' of order '%ld'.",
            pnt_vdf_Req->pnt_save_prod->item_id,
            pnt_vdf_Req->pnt_save_prod->order_id);
        pnt_vdf_Req->no_data = TRUE;
                return (IMS_ERROR);
    }

    /*
    ** Check for more than one row returned.
    */
    if (IMS_AFFECTED (qDesc) > 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "More than one row returned for the product path.");
        pnt_vdf_Req->no_data = TRUE;
                return (IMS_ERROR);
    }

    /*
    ** Get the path.
    */
    (void) memcpy (pnt_vdf_Req->path_name,
        qDesc->valAddr[0], qDesc->valLength[0]);
    pnt_vdf_Req->path_name[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (pnt_vdf_Req->path_name);

    /*
    ** Check for a trailer file.
    */

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select type \
        from dataset_file_policy d, order_item o \
        where order_id = %d \
        and item_id = %d \
        and o.p_dataset_idx = d.dataset_idx \
        and format = '%s' \
                and type = %d",
        pnt_vdf_Req->pnt_save_prod->order_id,
        pnt_vdf_Req->pnt_save_prod->item_id,
                pnt_vdf_Req->pnt_save_prod->format,
        TRAILER_FILE);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc)) < IMS_OK)
    {
        return (status);
    }

    /*
    ** Determine whether trailer exists.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        /* Trailer file does not exist. */
        pnt_vdf_Req->maxlen = 1;
    }
    else
    {
        /* Trailer file does exist. */
        pnt_vdf_Req->maxlen = 0;
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    return (IMS_OK);
}   /*   get_path   */

/******************************************************************************
**
** getExtList ()
**
** Get the list of file extensions for the product associated with the
** given order, item and product format.
**
******************************************************************************/

int getExtList (
    pnt_vdf_cat_request_t pnt_vdfReq)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    int extCount;

    /*
    ** Initialize variables.
    */
    msgDesc = pnt_vdfReq->msgDesc;
    extCount = 0;

    /*
    ** Populate the command buffer with the stored procedure call.
    */
    (void) sprintf (cmdBuf, "med_get_ext_list %d, %d, '%s'",
        pnt_vdfReq->pnt_save_prod->order_id,
        pnt_vdfReq->pnt_save_prod->item_id,
        pnt_vdfReq->pnt_save_prod->format);

    /*
    ** Allocate space for the extension array.
    ** We allocate enough room for 10 extensions, I
    ** hope that will be enough.
    */
    if ((pnt_vdfReq->pnt_save_prod->extArray = (char **) calloc
        ((size_t) 10, (size_t) sizeof (char *))) == (char **) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Memory allocation for the extension pointer array failed.");
        return (IMS_FATAL);
    }

    /*
    ** Execute the command and process the result rows.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (ims_msgGetSeverity(msgDesc));
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }

        /*
        ** Increment and check extension count.
        */
        if (++extCount > 10)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Exceeded '10' extensions for a single product.");
            return (IMS_ERROR);
        }

        /*
        ** Allocate space for the current extension.
        */
        pnt_vdfReq->pnt_save_prod->extArray[extCount-1] =
            (char *) malloc (qDesc->valLength[0] + 1);

        /*
        ** Get the extension.
        */
        (void) memcpy (pnt_vdfReq->pnt_save_prod->extArray[extCount-1],
            qDesc->valAddr[0], qDesc->valLength[0]);
        pnt_vdfReq->pnt_save_prod->extArray[extCount-1][qDesc->valLength[0]] = '\0';
        (void) ims_truncStr (pnt_vdfReq->pnt_save_prod->extArray[extCount-1]);
    }

    /*
    ** Set the extension count.
    */
    pnt_vdfReq->pnt_save_prod->extCount = extCount;

    /*
    ** Check the return status for the stored procedure.
    */
    if ((status = checkRetStatus (msgDesc)) < IMS_OK)
    {
        return (status);
    }

    /*
    ** Check to see if we got a row back.
    */
    if (IMS_AFFECTED (qDesc) <= 0)
    {
        pnt_vdfReq->no_data = TRUE;
        return (IMS_ERROR);
    }

    return (IMS_OK);
}

/**************************************************************
**
**  subr  execCmd ()
**
**  Execute an SQL procedure that writes data into the catalog database.
**  We don't pass a parameter, but assume that when this function is
**  called, the declared static buffer 'cmdBuf' has been properly filled
**  in with the SQL statement to be executed.
**
**  THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL NOT
**  RETURN ROWS FROM THE DATABASE.
**
**  If a deadlock occurs, re-execute the operation from the restart
**  point.
**
*************************************************************** */

int execCmd (
    IMS_MSG_STRUCT *msgDesc)
{
    int status;
    int severity;

    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (ims_msgGetSeverity (msgDesc));
        }
    }

    /*
    ** Check the stored procedure status returned value.
    */
    if ((severity = checkRetStatus (msgDesc)) < IMS_OK){
        return (severity);
    }

    if (qDesc->msgNo != 0){
        return (ims_msgGetSeverity (msgDesc));
    }

    return (IMS_OK);
}   /*  execCmd   */


/***************************************************************
**
**  subr checkRetStatus ()
**
**  Check the procedure returned status value.
**  When status returned is not an error, then return IMS_OK.
**
**************************************************************** */

int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc)
{
    int procReturn;
    int severity;

    /*
    ** Check to see if the Sybase procedure returned a status. If it did
    ** and it is not 0 (the OK value for a return), deal with the error.
    ** Return status of less than -100 correspond to message facility
    ** severity levels modulo 100.
    */
    if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
    {
        if ((procReturn = IMS_PROCRETURN (qDesc)) < 0){
            if (procReturn == -103){
                severity = IMS_FATAL;
            }
            else if (procReturn == -102){
                severity = IMS_ERROR;
            }
            else if (procReturn == -101){
                severity = IMS_WARNING;
            }
            else{
                severity = IMS_ERROR;
            }
            (void) ims_msg (msgDesc, severity,
                "Sybase procedure '%s' returned a status of %d",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }

    return (IMS_OK);
}   /*  checkRetStatus   */
