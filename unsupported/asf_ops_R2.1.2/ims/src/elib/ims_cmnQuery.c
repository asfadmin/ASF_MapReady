static char *sccs = "@(#)ims_cmnQuery.c	5.15  09/05/97";
/*******************************************************************
**
** File:        ims_cmnQuery.c
**
** Function:    Perform queries on data as requested by the caller.
**
** Author:      Dan Crichton, David Pass
**
** Date:        6/5/95
**
** Modified:    9/14/95 - D. Crichton - R1B
**              Fix query to allow user to pass in NULL end date.
**              On a NULL end date search for the nearest date
**              either greater or less than and return the angle.
**
**              9/18/95 -  D. Crichton - R1B'
**              Add state vector query.
**
**              10/16/95 - David Pass - R1B'
**              Add orderStatus query for PPS.  This will update
**              the order status and add a comment, and if finished,
**              will update the p_dataset_idx and p_granule_idx
**              values in the order_item table.
**
**              10/16/95 - David Pass - R1B'
**              Add calParamQuery query for PPS.  This will obtain the
**              FILE_NAME parameter in a given granule.
**
**              10/25/95 - David Pass - R1B'
**              Add scanQuery query for PPS.  This will obtain the
**              SCAN_RESULTS_NAME parameter in a given granule.
**
**              10/25/95 - David Pass - R1B'
**              Modified svQuery (state vectory query, see above)
**              to handle lists, not just one.
**
**              10/30/95 - David Pass - R1B'
**              Modified all queries to handle connection being
**              input.  ghaQuery split into ghaPointQuery and
**              ghaRangeQuery.
**
**              10/30/95 - David Pass - R1B'
**              Added frame query.
**
**              1/12/95  - Alin Tilden, Paul Brosche - R1B'
**              Added darFrame, darQuery, darStatus, darStatistics,
**              and incrVersion APIs.
**
**              2/9/96 - Sean Hardman - R1B'
**              Moved ims_orderStatus() and ims_darFrame() and
**              supporting functions to ims_cmnODLQuery.c.
**
**              5/8/96 - David Pass - R2
**              Added dar query for RGPS: #1 on their list.  determine
**              data on all frames of a given order_id, item_id.
**              ims_darFrameStatus
**
**              5/8/96 - David Pass - R2
**              Added spatial query for RGPS: #3 on their list.
**              Get information on all frames in a given area and time.
**              ims_spatialQuery
**
**              6/7/96 - David Pass - R2
**              Added order query for RGPS: #4 on their list.
**              Get information on all items for a given order_id.
**              ims_orderQuery
**
**              4/30/97 - David Pass - R2
**              Added ims_dlToDtkQuery and ims_dlToDtkFree to obtain
**              downlink to datatake info.
**
******************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <odldef.h>
#include <ims_odl.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_cmnQuery.h.
**
**   int ims_ghaPointQuery (IMS_CMN_QUERY *, char * );
**   int ims_ghaRangeQuery (IMS_CMN_QUERY *, char *, char *);
**   int ims_tceQuery (IMS_CMN_QUERY *, char *, char *, char *);
**   int ims_svQuery (IMS_CMN_QUERY *, char *, char, char *, char *,
**          int);
**   int ims_calParamQuery (IMS_CMN_QUERY *, char *, char *, char *);
**   int ims_scanQuery (IMS_CMN_QUERY *, char *, int, int, char *,
**          char *);
**   int ims_frameQuery (IMS_CMN_QUERY *, char *, int, int, char *,
**           char *, char *);
**   int ims_frameStatus (IMS_CMN_QUERY *, int, int);
**   int ims_orderStatus (IMS_CMN_QUERY *, char *);
**   int ims_openQueryConnection (IMS_CMN_QUERY *);
**   int ims_closeQueryConnection (IMS_CMN_QUERY *);
**   void ims_frameFree (IMS_FRAME_STRUCT *);
**   void ims_calParamFree (IMS_CAL_PARAM_STRUCT *);
**   int ims_darStatus (IMS_CMN_QUERY *, IMS_DAR_STATUS *);
**   int ims_darQuery (IMS_CMN_QUERY *, char *, char *, char *);
**   int ims_darStatistics (IMS_CMN_QUERY *query, IMS_DAR_STATISTICS
**          *darStats);
**   int ims_incrVersion (IMS_CMN_QUERY *query, char *name);
**   int ims_darFrameStatus( IMS_CMN_QUERY * , int, short, long * );
**   int ims_spatialQuery( IMS_CMN_QUERY * , char *, char *, char *,
**          char *, float, float, float, float, long * );
**   int ims_orderQuery( IMS_CMN_QUERY * , long, long * );
**   void freeFrameResults( pnt_ims_FrameResults_t );
**   void freeOrderResults( pnt_ims_OrderResults_t );
**   int ims_dlToDtkQuery( IMS_CMN_QUERY * , char *, char *, int, int,
**          int * );
**   void ims_dlToDtkFree( IMS_DL_STRUCT * );
*/

/*
** Local function prototypes
*/
static int getItemsValue (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char * ,
    char *, int *, int *);
static int getGranuleTable (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char *, char *, int *);
static int getPlatformAcronym (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char *, char *, int *);
static int getCalFileName( IMS_CMN_QUERY *, char *, char *, char *);
static int getScanFileName (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char *, char *, int, int, char *, char *, int *);
static int getFrameData (IMS_CMN_QUERY *, char *, char *, char *, int, int,
            char *, char *);
static IMS_QI_DESC_OBJ *openConnection (IMS_CMN_QUERY *);
static int checkConnection (IMS_CMN_QUERY *);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int checkRetStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int getDarData (IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    char *, int , char *, char *, IMS_DAR_LIST *, int *);
static int getItemsData (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char *, ITEMS_LIST *, int *);
static int setTransState (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *);
static int getDarStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int , short, int *, int *);
static int setDarStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int , short , int , char *, int *);
static int getOrderItemStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int , short, int *, int *);
static int setOrderItemStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int , short , int, int *);
static int setDarStatistics (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int, short, char *, char *, int, int *);
static int incrProductVersion (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        char *, IMS_VERSION_STRUCT *, int *);
static int validateOrderItem (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
        int, short, int *);
static int getGeneralLock (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static void freeItemsList (ITEMS_LIST *);
static int get_dar_info( IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    int, short, char *, char * );
static int get_gran_data( IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    char *, int, short, pnt_ims_FrameResults_t *, long * );
static int get_gran_name( IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    char *, char *, char * );
static int get_sp_gran_data( IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    char *, char *, char *, float, float, float, float,
    char *, pnt_ims_FrameResults_t *, long * );
static int get_item_list( IMS_MSG_STRUCT  *, IMS_QI_DESC_OBJ *,
    long, pnt_ims_OrderResults_t  *, long * );
static  int read_dtk( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    pnt_ims_dlStruct, int * );
static  int read_dtk_check( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    pnt_ims_dlStruct );
static  int read_dl(  IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    pnt_ims_dlStruct, pnt_ims_dlStruct, int * );
static int getDatatakeInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char *, int, int, int *, char *, char *, int *, int *, short *, short *);
static int getOrderDataset (IMS_CMN_QUERY *, int , int, char *, int *);
static int getFrameStatus (IMS_CMN_QUERY *, char *,int);
static int getFrameSourceMedia (IMS_CMN_QUERY *,int, char *, char *, char *);

/*******************************************************************
**
** subr ims_ghaPointQuery ()
**
** Query the GHA table based on a given date/time point and return
** the corresponding angle that has a date closest to that point.
**
******************************************************************** */

int ims_ghaPointQuery (
    IMS_CMN_QUERY *query,
    char *targetDate)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    int rowCount1, rowCount2;
    int days1, days2;
    int msecs1, msecs2;
    char tempAngle[IMS_COL15_LEN];
    char tempDate[IMS_COL30_LEN];
    char temp2Angle[IMS_COL15_LEN];
    char temp2Date[IMS_COL30_LEN];
    char tempStart[22];
    IMS_GHA_STRUCT *retDef;
    IMS_NUMERIC_DATE dateStruct;
    static char cmdBuf[IMS_COL512_LEN];

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_GHA_STRUCT *) query->retPtr;
    (void) memset ((void *) retDef, 0, sizeof (IMS_GHA_STRUCT));

    /*
    ** Validate and format the target date.
    */
    if ((status = ims_timeToNumericDate (msgDesc, targetDate,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Target date '%s' is invalid.", targetDate);
        return (status);
    }
    ims_numericDateToIMSA (&dateStruct, tempStart);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) == IMS_OK)
    {
        /* Close this connection and free the query descriptor. */
        (void) ims_qiFreeDesc (query->qDesc);
        query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
    }

    /*
    ** Open the database server connection.
    */
    if ((qDesc = openConnection (query)) ==
        (IMS_QI_DESC_OBJ *) NULL)
    {
        status = ims_msgGetSeverity (msgDesc);
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    IMS_SETROWCOUNT (qDesc, "1");

    /*
    ** Return the date just after the target date.
    */
    (void) sprintf (qDesc->cmd,
        "select date, angle \
        from gha_all \
        where date >= '%s' \
        order by date",
        tempStart);

    rowCount1 = 0;

    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                query->retStatus = IMS_DEADLOCK;
            }
            else
            {
                query->retStatus = IMS_QUERYFAIL;
            }

            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }

        rowCount1++;

        /*
        ** Date/Time of data point
        */
        (void) memcpy(tempDate, qDesc->valAddr[0], qDesc->valLength[0]);
        tempDate[qDesc->valLength[0]] = '\0';
        (void) strcpy(retDef->date, ims_truncStr(tempDate));

        /*
        ** Angle of data point
        */
        (void) memcpy(tempAngle, qDesc->valAddr[1],qDesc->valLength[1]);
        tempAngle[qDesc->valLength[1]] = '\0';
        (void) strcpy(retDef->angle, ims_truncStr(tempAngle));
    }

    /*
    ** Reset Descriptor and get a 2nd Angle/Time.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        (void) ims_qiFreeDesc (qDesc);
        return (status);
    }

    /*
    ** Perform query and search for angle and data point time by getting
    ** the date just before the target date.
    */
    (void) sprintf (qDesc->cmd,
        "select date, angle \
        from gha_all \
        where date <= '%s' \
        order by date desc",
        tempStart);

    rowCount2 = 0;

    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                query->retStatus = IMS_DEADLOCK;
            }
            else
            {
                query->retStatus = IMS_QUERYFAIL;
            }

            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount2++;

        /*
        ** Date/Time of data point
        */
        (void) memcpy(temp2Date, qDesc->valAddr[0],
            qDesc->valLength[0]);
        temp2Date[qDesc->valLength[0]] = '\0';

        /*
        ** Angle of data point
        */
        (void) memcpy(temp2Angle, qDesc->valAddr[1],
            qDesc->valLength[1]);
        temp2Angle[qDesc->valLength[1]] = '\0';
    }

    /*
    ** If neither queries found anything, then just return.
    */
    if ((rowCount1 == 0) && (rowCount2 == 0))
    {
        (void) ims_qiFreeDesc (qDesc);
        query->retStatus = IMS_NOROWS;
        return (IMS_ERROR);
    }

    /*
    ** If the 2nd query didn't find anything, then just return since
    ** the first query saves the date and angle in the return structure
    ** as part of its query processing.
    */
    if (rowCount2 == 0)
    {
        (void) ims_qiFreeDesc (qDesc);
        return (IMS_OK);
    }

    /*
    ** If the 1st query didn't find anything, then return with the
    ** 2nd query results.
    */
    if (rowCount1 == 0)
    {
        (void) strcpy(retDef->angle, ims_truncStr(temp2Angle));
        (void) strcpy(retDef->date, ims_truncStr(temp2Date));

        (void) ims_qiFreeDesc (qDesc);
        return (IMS_OK);
    }

    /*
    ** Do a date comparison.
    */
    if (ims_numericDateDiff (msgDesc, targetDate, tempDate,
        &days1, &msecs1) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not get 1st date difference.");
        (void) ims_qiFreeDesc (qDesc);
        return (IMS_ERROR);
    }

    if (ims_numericDateDiff(msgDesc, targetDate, temp2Date,
        &days2, &msecs2) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not get 2nd date difference.");
        (void) ims_qiFreeDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Check the number of days.  If they are equivelent, then check the
    ** msecs.
    */
    /*
    ** If the first date is closer then just return since the retPtr is
    ** already populated with this date and angle.
    */

    if (days1 < days2)
    {
        (void) ims_qiFreeDesc (qDesc);
        return(IMS_OK);
    }
    else if (days2 < days1)
    {
        /*
        ** 2nd date is closer so populate retPtr and return.
        */
        (void) strcpy(retDef->angle, ims_truncStr(temp2Angle));
        (void) strcpy(retDef->date, ims_truncStr(temp2Date));

        (void) ims_qiFreeDesc (qDesc);
        return(IMS_OK);
    }

    /*
    ** Dates are equivelent, check time.  By default return the 2nd
    ** date/angle if the msecs are equivelent.
    */
    if (msecs1 < msecs2)
    {
        /*
        ** Just return since again the retPtr is already populated with
        ** the results from the first query
        */
        (void) ims_qiFreeDesc (qDesc);
        return (IMS_OK);
    }

    /*
    ** 2nd date is closer or equivelent so populate retPtr and return.
    */
    (void) strcpy(retDef->angle, ims_truncStr(temp2Angle));
    (void) strcpy(retDef->date, ims_truncStr(temp2Date));

    (void) ims_qiFreeDesc (qDesc);
    return (IMS_OK);
}  /* ims_ghaPointQuery */


/*******************************************************************
**
** subr ims_ghaRangeQuery ()
**
** Query the GHA table based on a given date/time range and return
** the corresponding angle that has a date closest to the end point
** inside the range.
**
******************************************************************** */
int ims_ghaRangeQuery (
    IMS_CMN_QUERY *query,
    char *start_time,
    char *end_time)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    char tempAngle[IMS_COL15_LEN];
    char tempDate[IMS_COL30_LEN];
    char tempStart[22];
    char tempEnd[22];
    IMS_GHA_STRUCT *retDef;
    IMS_NUMERIC_DATE dateStruct;
    static char cmdBuf[IMS_COL512_LEN];

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_GHA_STRUCT *) query->retPtr;
    (void) memset((void *) retDef, 0, sizeof (IMS_GHA_STRUCT));

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting GHA range query for start time %s and end time %s", 
	start_time, end_time); 

    /*
    ** Validate and format the start and end dates.
    */
    if ((status = ims_timeToNumericDate (msgDesc, start_time,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Start date '%s' is invalid.", start_time);
        return (status);
    }

    ims_numericDateToIMSA (&dateStruct, tempStart);

    if ((status = ims_timeToNumericDate (msgDesc, end_time,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "End date '%s' is invalid.", end_time);
        return (status);
    }

    ims_numericDateToIMSA (&dateStruct, tempEnd);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    qDesc = query->qDesc;
    IMS_SETCMD (qDesc, cmdBuf);
    IMS_SETROWCOUNT (qDesc, "1");

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (qDesc->cmd,
        "select date, angle \
        from gha_all \
        where date >= '%s' and date <= '%s' \
        order by date desc",
        tempStart, tempEnd);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        query->retStatus = status;
        IMS_SETROWCOUNT (qDesc, "0");
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        query->retStatus = IMS_NOROWS;
        IMS_SETROWCOUNT (qDesc, "0");
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /* date */
    (void) memcpy (retDef->date, qDesc->valAddr[0],
        qDesc->valLength[0]);
    retDef->date[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (retDef->date);

    /* angle */
    (void) memcpy (retDef->angle, qDesc->valAddr[1],
        qDesc->valLength[1]);
    retDef->angle[qDesc->valLength[1]] = '\0';
    (void) ims_truncStr (retDef->angle);

    /*
    ** Set the rows returned back to the default and
    ** reset the query descriptor.
    */
    IMS_SETROWCOUNT (qDesc, "0");
    (void) ims_qiResetDesc (qDesc);

    return (IMS_OK);
}  /* ims_ghaRangeQuery */


/*******************************************************************
**
** subr ims_tceQuery ()
**
** Query the TCE table for the given platform based on a given
** date/time range and return the corresponding angle that has a
** date closest to the end point inside the range.
**
******************************************************************** */
int ims_tceQuery (
    IMS_CMN_QUERY *query,
    char *platform,
    char *start_date,
    char *end_date)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    IMS_TCE_STRUCT *retDef;
    IMS_NUMERIC_DATE dateStruct;
    char tempStart[22];
    char tempEnd[22];
    char tablename[32];
    static char cmdBuf[IMS_COL512_LEN];

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_TCE_STRUCT *) query->retPtr;
    (void) memset((void *) retDef, 0, sizeof (IMS_TCE_STRUCT));

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting TCE query for platform %s, start date %s, end date %s",
	platform, start_date, end_date);

    /*
    ** Validate and format the start and end dates.
    */
    if ((status = ims_timeToNumericDate (msgDesc, start_date,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Start date '%s' is invalid.", start_date);
        return(status);
    }

    ims_numericDateToIMSA(&dateStruct, tempStart);

    if ((status = ims_timeToNumericDate (msgDesc, end_date,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "End date '%s' is invalid.", end_date);
        return(status);
    }

    ims_numericDateToIMSA(&dateStruct, tempEnd);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    qDesc = query->qDesc;
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Get the name of the table to query the TCE data from.
    */

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (qDesc->cmd,
        "select tce_table_name \
        from platform_tables \
        where platform = '%s' \
        or acronym = '%s'",
        platform, platform);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not get TCE table name for platform '%s'.",
           platform);
        query->retStatus = status;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** See if we got something.
    */
    if ((IMS_AFFECTED (qDesc) < 1) || (qDesc->valLength[0] < 1))
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not get TCE table name for platform '%s'.",
           platform);
        query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Get the table name from the query descriptor.
    */
    (void) memcpy (tablename, qDesc->valAddr[0], qDesc->valLength[0]);
    tablename[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (tablename);

    /*
    ** Perform query and search for angle and data point time by getting
    ** last returned value
    */

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Set the number of rows to be returned.
    */
    IMS_SETROWCOUNT (qDesc, "1");

    /*
    ** Get the date and angle values for the given time range.
    */

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (qDesc->cmd,
        "select rev, date, sat_time, clk_cycle \
        from %s \
        where date >= '%s' and date <= '%s' \
        order by date desc",
        tablename, tempStart, tempEnd);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        query->retStatus = status;
        IMS_SETROWCOUNT (qDesc, "0");
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not obtain TCE values for the given date/time range."
            );
        query->retStatus = IMS_NOROWS;
        IMS_SETROWCOUNT (qDesc, "0");
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Copy in the returned data.
    */

    /* rev */
    (void) memcpy (retDef->rev, qDesc->valAddr[0], qDesc->valLength[0]);
    retDef->rev[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (retDef->rev);

    /* date */
    (void) memcpy (retDef->date, qDesc->valAddr[1],qDesc->valLength[1]);
    retDef->date[qDesc->valLength[1]] = '\0';
    (void) ims_truncStr (retDef->date);

    /* sat_time */
    (void) memcpy (retDef->sat_time, qDesc->valAddr[2],
        qDesc->valLength[2]);
    retDef->sat_time[qDesc->valLength[2]] = '\0';
    (void) ims_truncStr (retDef->sat_time);

    /* clk_cycle */
    (void) memcpy (retDef->clk_cycle, qDesc->valAddr[3],
        qDesc->valLength[3]);
    retDef->clk_cycle[qDesc->valLength[3]] = '\0';
    (void) ims_truncStr (retDef->clk_cycle);

    /*
    ** Set the rows returned back to the default and
    ** reset the query descriptor.
    */
    IMS_SETROWCOUNT (qDesc, "0");
    (void) ims_qiResetDesc (qDesc);

    return(IMS_OK);
}   /*  ims_tceQuery   */


/*******************************************************************
**
** subr ims_svQuery ()
**
** Query the SV table for the given platform based on a given
** date/time range and return the corresponding values that have a
** date closest to the end point inside the range.
**
******************************************************************** */
int ims_svQuery (
    IMS_CMN_QUERY *query,
    char *platform,
    char precision,
    char *start_date,
    char *end_date,
    int listResultsFlag)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    char temp[IMS_COL30_LEN]; /* Size at worst case */
    IMS_SV_STRUCT *retDef;
    IMS_NUMERIC_DATE dateStruct;
    char tempStart[22];
    char tempEnd[22];
    char tablename[32];
    static char cmdBuf[IMS_COL512_LEN];
    IMS_SV_STRUCT *retDef_curr;
    IMS_SV_STRUCT *retDef_last;

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_SV_STRUCT *) query->retPtr;
    (void) memset ((void *) retDef, 0, sizeof (IMS_SV_STRUCT));
    retDef->next = (IMS_SV_STRUCT *) NULL;

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting SV query for platform %s, start date %s, end date %s",
        platform, start_date, end_date);

    /*
    ** Validate and format the start and end dates.
    */
    if ((status = ims_timeToNumericDate (msgDesc, start_date,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Start date '%s' is invalid.", start_date);
        return (status);
    }

    ims_numericDateToIMSA (&dateStruct, tempStart);

    if ((status = ims_timeToNumericDate (msgDesc, end_date,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "End date '%s' is invalid.", end_date);
        return (status);
    }

    ims_numericDateToIMSA (&dateStruct, tempEnd);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    qDesc = query->qDesc;
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Get the name of the table to query the SV data from.
    */

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (qDesc->cmd,
        "select sv_table_name \
        from platform_tables \
        where platform = '%s' \
        or acronym = '%s'",
        platform, platform);

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not get SV table name for platform '%s'.",
           platform);
        query->retStatus = status;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** See if we got something.
    */
    if ((IMS_AFFECTED (qDesc) < 1) || (qDesc->valLength[0] < 1))
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not get SV table name for platform '%s'.",
           platform);
        query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Get the table name from the query descriptor.
    */
    (void) memcpy (tablename, qDesc->valAddr[0], qDesc->valLength[0]);
    tablename[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (tablename);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Get the state vector values for the given time range.
    */

    /*
    ** Populate the command buffer with the SQL statement.
    */
    if (listResultsFlag == IMS_FALSE)
    {
        /* Set the number of rows to be returned. */
        IMS_SETROWCOUNT (qDesc, "1");

        (void) sprintf (qDesc->cmd,
        "select rev, date, 'TRUE_EQUATORIAL', \
        x_pos, y_pos, z_pos, x_vel, y_vel, z_vel \
        from %s \
        where date >= '%s' and date <= '%s' \
        and prec = '%c' \
        order by date desc",
        tablename, tempStart, tempEnd, precision);
    }
    else
    {
        (void) sprintf (qDesc->cmd,
        "select rev, date, 'TRUE_EQUATORIAL',\
        x_pos, y_pos, z_pos, x_vel, y_vel, z_vel \
        from %s \
        where date >= '%s' and date <= '%s' \
        and prec = '%c' \
        order by date",
        tablename, tempStart, tempEnd, precision);
    }

    /*
    ** Initialize pointers.
    */
    retDef_curr = retDef;
    retDef_last = (IMS_SV_STRUCT *) NULL;

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                query->retStatus = IMS_DEADLOCK;
            }
            else
            {
                query->retStatus = IMS_QUERYFAIL;
            }

            IMS_SETROWCOUNT (qDesc, "0");
            (void) ims_qiResetDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }

        /*
        ** Allocate space for the IMS_SV_STRUCT structure.
        */
        if (retDef_curr == (IMS_SV_STRUCT *) NULL)
        {
            if ((retDef_curr = (IMS_SV_STRUCT *) malloc (
                (size_t) sizeof (IMS_SV_STRUCT))) ==
                (IMS_SV_STRUCT *) NULL)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate memory for IMS_SV_STRUCT "
                    "structure.");
                while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION){}
                IMS_SETROWCOUNT (qDesc, "0");
                (void) ims_qiResetDesc (qDesc);
                return (IMS_FATAL);
            }

            (void) memset ((void *) retDef_curr, 0,
                sizeof (IMS_SV_STRUCT));
            retDef_curr->next = (IMS_SV_STRUCT *) NULL;
            retDef_last->next = retDef_curr;
        }

        /*
        ** Copy in the returned data.
        */

        /* rev */
        (void) memcpy(temp, qDesc->valAddr[0], qDesc->valLength[0]);
        temp[qDesc->valLength[0]] = '\0';
        (void) strcpy(retDef_curr->rev, ims_truncStr(temp));

        /* date */
        (void) memcpy(temp, qDesc->valAddr[1], qDesc->valLength[1]);
        temp[qDesc->valLength[1]] = '\0';
        (void) strcpy(retDef_curr->date, ims_truncStr(temp));

        /* coord_sys (defaulted to "TRUE_EQUATORIAL") */
        (void) memcpy(temp, qDesc->valAddr[2], qDesc->valLength[2]);
        temp[qDesc->valLength[2]] = '\0';
        (void) strcpy(retDef_curr->coord_sys, ims_truncStr(temp));

        /* x_pos */
        (void) memcpy(temp, qDesc->valAddr[3], qDesc->valLength[3]);
        temp[qDesc->valLength[3]] = '\0';
        (void) strcpy(retDef_curr->x_pos, ims_truncStr(temp));

        /* y_pos */
        (void) memcpy(temp, qDesc->valAddr[4], qDesc->valLength[4]);
        temp[qDesc->valLength[4]] = '\0';
        (void) strcpy(retDef_curr->y_pos, ims_truncStr(temp));

        /* z_pos */
        (void) memcpy(temp, qDesc->valAddr[5], qDesc->valLength[5]);
        temp[qDesc->valLength[5]] = '\0';
        (void) strcpy(retDef_curr->z_pos, ims_truncStr(temp));

        /* x_vel */
        (void) memcpy(temp, qDesc->valAddr[6], qDesc->valLength[6]);
        temp[qDesc->valLength[6]] = '\0';
        (void) strcpy(retDef_curr->x_vel, ims_truncStr(temp));

        /* y_vel */
        (void) memcpy(temp, qDesc->valAddr[7], qDesc->valLength[7]);
        temp[qDesc->valLength[7]] = '\0';
        (void) strcpy(retDef_curr->y_vel, ims_truncStr(temp));

        /* z_vel */
        (void) memcpy(temp, qDesc->valAddr[8], qDesc->valLength[8]);
        temp[qDesc->valLength[8]] = '\0';
        (void) strcpy(retDef_curr->z_vel, ims_truncStr(temp));

        /*
        ** Set the pointer for the next set of data.
        */
        retDef_last = retDef_curr;
        retDef_curr = retDef_curr->next;
    }

    /*
    ** Check the number of rows returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not obtain SV values for the given date/time "
            "range and precision.");
        query->retStatus = IMS_NOROWS;
        IMS_SETROWCOUNT (qDesc, "0");
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Set the rows returned back to the default and
    ** reset the query descriptor.
    */
    IMS_SETROWCOUNT (qDesc, "0");
    (void) ims_qiResetDesc (qDesc);

    return(IMS_OK);
}   /*  ims_svQuery  */


/*******************************************************************
**
** subr ims_frameQuery ()
**
** Query the associated the FRAMES granules table
** and return frame information for the given arguments.
**
******************************************************************** */
int ims_frameQuery (
    IMS_CMN_QUERY *query,
    char *platform,
    int  rev,
    int  sequence,
    char *mediaId,
    char *mode,
    char *frameMode)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    char dataset[IMS_COL80_LEN+1];
    char frames_granules_table[IMS_COL30_LEN+1];
    char segment_granules_table[IMS_COL30_LEN+1];
    char platform_acronym[IMS_COL10_LEN+1];
    int i;
    int status;

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting Frame query for '%s' with mode = '%s', "
        "frame mode = '%s', "
        "revolution = '%d', sequence = '%d' and media_id = '%s'.",
        platform, mode, frameMode, rev, sequence, mediaId);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Compose dataset name.
    */
    dataset[0] = '\0';
    for (i = 0; i < IMS_MAXPLATFORMMODES; i++)
    {
        if (strcmp (mode, IMS_PLATFORM_MODES[i].mode) == 0)
        {
        (void) strcpy (dataset, platform);
        (void) strcat (dataset, IMS_PLATFORM_MODES[i].dataset);
        if (strcmp (frameMode, "ANTARCTIC") == 0)
        {
            (void) strcat (dataset, " LEFT LOOKING");
        }
        (void) strcat (dataset, " FRAMES");
        break;
        }
    }

    /*
    ** Check for invalid mode
    */
    if ((int) strlen (dataset) == 0)
    {
                (void) ims_msg (msgDesc, IMS_ERROR,
                        "Could not get dataset name for mode '%s'.",
                        mode);
                return (IMS_ERROR);

    }

    /*
    ** Get the FRAMES granule table name.
    */
    if ((status = getGranuleTable (msgDesc, qDesc, dataset,
        frames_granules_table, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule table name for dataset '%s'.",
            dataset);
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

	/*
        ** Get the RAW SIGNAL SEGMENT granule table name.
        */
        (void) strcpy (dataset, platform);
        (void) strcat (dataset, " RAW SIGNAL SEGMENT");

        if ((status = getGranuleTable (msgDesc, qDesc, dataset,
                segment_granules_table, &(query->retStatus))) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get granule table name for dataset '%s'.",
                dataset);
            (void) ims_qiResetDesc (qDesc);
            return (status);
        }

	/*
	** Get the platform acronym 
	*/
	if ((status = getPlatformAcronym(msgDesc, qDesc, platform,
		platform_acronym, &(query->retStatus))) < IMS_OK)
	{
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get acronym for platform '%s'.",
                platform);
            (void) ims_qiResetDesc (qDesc);
            return (status);
        }


    /*
    ** Now query for the frame information.
    */
    if ((status = getFrameData (query, frames_granules_table,
        segment_granules_table, platform_acronym, rev, sequence, 
	mode, mediaId)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain frame information.");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    (void) ims_msg (msgDesc, IMS_INFO,
        "The Frame query succeeded.");

    return (IMS_OK);
}   /*  ims_frameQuery  */


/*******************************************************************
**
** subr getFrameData ()
**
** Get the frame data from the granule table given
** rev, sequence and mediaId.
**
******************************************************************** */
static int getFrameData (
    IMS_CMN_QUERY *query,
    char *frames_granules_table,
    char *segment_granules_table,
    char *platform,
    int rev,
    int sequence,
    char *mode,
    char *mediaId)
{
    IMS_QI_DESC_OBJ *qDesc;
    IMS_MSG_STRUCT *msgDesc;
    IMS_FRAME_STRUCT *currPtr;
    IMS_FRAME_STRUCT *prevPtr;
    static char cmdBuf[IMS_COL512_LEN];
    int status;
    int rowCount;
    int j;
    short frame_id;

    char dl_platform[IMS_COL10_LEN+1];
    char dt_platform[IMS_COL10_LEN+1];
    int  dl_rev, dt_rev;
    short dl_sequence, dt_sequence;

    /* 
    ** Get the platform, rev, sequence info from the datatake
    */
    qDesc = query->qDesc;
    msgDesc = query->msgDesc;

    if ((status = getDatatakeInfo(msgDesc, qDesc, platform,
                rev, sequence, &(query->retStatus), dl_platform, dt_platform,
		&dl_rev, &dt_rev, &dl_sequence, &dt_sequence)) < IMS_OK)
    {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get datake info for platform '%s', "
		"rev %d, sequence %d.",
                platform, rev, sequence);
            (void) ims_qiResetDesc (qDesc);
            return (status);
    }
 
    /*
    ** Initialize variables.
    */
    qDesc = query->qDesc;
    msgDesc = query->msgDesc;
    currPtr = (IMS_FRAME_STRUCT *) query->retPtr;
    if (currPtr != (IMS_FRAME_STRUCT *) NULL)
    {
        (void) memset ((void *) currPtr, 0, sizeof (IMS_FRAME_STRUCT));
        currPtr->next = (IMS_FRAME_STRUCT *) NULL;
    }
    prevPtr = (IMS_FRAME_STRUCT *) NULL;
    rowCount = 0;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select f.FRAME_ID, f.STATION_ID, f.START_TIME, "
        "f.END_TIME, f.CENTER_TIME, s.DATA_DIRECTION "
        "from %s f, %s s "
        "where f.MEDIA_ID = '%s' and f.FRAME_STATUS = 'SCANNED' and "
        "f.MODE = '%s' and s.MEDIA_ID = f.MEDIA_ID and "
	"((f.PLATFORM = '%s' and f.REVOLUTION = %d and " 
	"  f.SEQUENCE = %d) or "
	" (f.PLATFORM = '%s' and f.REVOLUTION = %d and "
	"  f.SEQUENCE = %d)) and "
        "((s.PLATFORM = '%s' and s.REVOLUTION = %d and "
        "  s.SEQUENCE = %d) or "
        " (s.PLATFORM = '%s' and s.REVOLUTION = %d and "
        "  s.SEQUENCE = %d))",
        frames_granules_table, segment_granules_table,
	mediaId, mode,
	dl_platform, dl_rev, dl_sequence,
	dt_platform, dt_rev, dt_sequence,
        dl_platform, dl_rev, dl_sequence,
        dt_platform, dt_rev, dt_sequence);

#ifdef QDEBUG
	printf("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                query->retStatus = IMS_DEADLOCK;
            }
            else
            {
                query->retStatus = IMS_QUERYFAIL;
            }

            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }

        /*
        ** Allocate space for the IMS_FRAME_STRUCT structure,
        ** if needed.
        */
        if (currPtr == (IMS_FRAME_STRUCT *) NULL)
        {
            if ((currPtr = (IMS_FRAME_STRUCT *) malloc (
            (size_t) sizeof (IMS_FRAME_STRUCT))) ==
            (IMS_FRAME_STRUCT *) NULL)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate memory for IMS_FRAME_STRUCT "
                    "structure.");
                while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
                IMS_SETROWCOUNT (qDesc, "0");
                (void) ims_qiResetDesc (qDesc);
                return (IMS_FATAL);
            }
            (void) memset ((void *) currPtr, 0, sizeof(
                IMS_FRAME_STRUCT));
            currPtr->next = (IMS_FRAME_STRUCT *) NULL;

            if (prevPtr != (IMS_FRAME_STRUCT *) NULL)
            {
            prevPtr->next = currPtr;
            }
            else
            {
            query->retPtr = (char *) currPtr;
            }
        }

        /*
        ** Copy in the returned data.
        */

        j = 0;                          /*  frame_id  */
        (void) memcpy ((char *) &(frame_id),
            qDesc->valAddr[j], qDesc->valLength[j]);
        currPtr->frame_id = frame_id;

        j++;                            /*  station_id  */
        (void) memcpy ((char *) currPtr->station_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        currPtr->station_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( currPtr->station_id );

        j++;                            /*  start_time  */
        (void) memcpy ((char *) currPtr->start_time,
            qDesc->valAddr[j], qDesc->valLength[j]);
        currPtr->start_time[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( currPtr->start_time );

        j++;                            /*  end_time  */
        (void) memcpy ((char *) currPtr->end_time,
            qDesc->valAddr[j], qDesc->valLength[j]);
        currPtr->end_time[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( currPtr->end_time );

        j++;                            /*  center_time  */
        (void) memcpy ((char *) currPtr->center_time,
            qDesc->valAddr[j], qDesc->valLength[j]);
        currPtr->center_time[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( currPtr->center_time );

                j++;                            /*  data_direction */
                (void) memcpy ((char *) currPtr->data_direction,
                        qDesc->valAddr[j], qDesc->valLength[j]);
                currPtr->data_direction[qDesc->valLength[j]] = '\0';
                (void) ims_truncStr( currPtr->data_direction );


        prevPtr = currPtr;
        currPtr = currPtr->next;
    }

    /*
    ** Check the number of rows returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "No matching rows were found.");
        query->retStatus = IMS_NOROWS;
        return (IMS_ERROR);
    }

    return (IMS_OK);
} /* getFrameData  */


/*******************************************************************
**
** subr getItemsValue ()
**
** This function queries for the integer representation of the value
** given from the items table.
**
******************************************************************** */
static int getItemsValue (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *type,
    char *description,
    int *instance,
    int *retStatus)
{
    static char cmdBuf[IMS_COL512_LEN];
    DBSMALLINT instanceTemp;
    int status;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select instance from items \
        where type = '%s' and description = '%s'",
        type, description);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        *retStatus = status;
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }

    /*
    ** Get the instance value.
    */
    (void) memcpy (&instanceTemp,
        qDesc->valAddr[0], qDesc->valLength[0]);

    *instance = instanceTemp;

    return (IMS_OK);
}   /*  ims_getItemsValue  */


/*******************************************************************
**
** subr getGranuleTable ()
**
** Get the granule table name for the given dataset.
**
******************************************************************** */
static int getGranuleTable (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *dataset,
    char *granules_table,
    int *retStatus)
{
    static char cmdBuf[IMS_COL512_LEN];
    int status;
    int j;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select granules_table \
        from dataset_policy d, dataset_relation r \
        where r.dataset = '%s' and r.dataset_idx = d.dataset_idx",
        dataset);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        *retStatus = status;
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }

    /*
    ** Get the granules_table name.
    */
    (void) memcpy (granules_table,
        qDesc->valAddr[0], qDesc->valLength[0]);
    granules_table[qDesc->valLength[0]] = '\0';

    return (IMS_OK);
}   /*  getGranuleTable  */

/*********************************************************************
**
** subr getPlatformAcronym ()
**
** Get the acronym for the given platform from the "platforms" table.
**
******************************************************************** */
static int getPlatformAcronym(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *platform,
    char *platform_acronym,
    int *retStatus)
{
    static char cmdBuf[IMS_COL512_LEN];
    int status;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select acronym from platforms \
        where platform = '%s'",
        platform);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        *retStatus = status;
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }

    /*
    ** Get the platform acronym.
    */
    (void) memcpy (platform_acronym,
        qDesc->valAddr[0], qDesc->valLength[0]);
    platform_acronym[qDesc->valLength[0]] = '\0';

    return (IMS_OK);

}   /*  getPlatformAcronym*/


/*******************************************************************
**
** subr ims_scanQuery ()
**
** This function gets the SCAN_RESULTS_FILE
** value from the granules_ table.  The search is done on the
** mode, rev, sequence and mediaId values.
**
******************************************************************** */
int ims_scanQuery (
    IMS_CMN_QUERY *query,
    char *platform,
    int  rev,
    int  sequence,
    char *mediaId,
    char *mode)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_SCAN_STRUCT *retDef;
    IMS_QI_DESC_OBJ *qDesc;
    char dataset[IMS_COL80_LEN+1];
    char granules_table[IMS_COL30_LEN+1];
    int status;

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_SCAN_STRUCT *) query->retPtr;
    (void) memset ((void *) retDef, 0, sizeof (IMS_SCAN_STRUCT));

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting Scan Results File query for '%s' with mode = '%s', "
        "revolution = '%d', sequence = '%d' and media_id = '%s'.",
        platform, mode, rev, sequence, mediaId);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Get the granule table name.
    */
    (void) strcpy (dataset, platform);
    (void) strcat (dataset, " SCAN RESULTS FILE");
    if ((status = getGranuleTable (msgDesc, qDesc, dataset,
        granules_table, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule table name for dataset '%s'.",
            dataset);
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Get the scan_results_file name.
    */
    if ((status = getScanFileName (msgDesc, qDesc, granules_table,
        mode, rev, sequence, mediaId, retDef->scan_results_file,
        &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain a Scan Results File name.");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    (void) ims_msg (msgDesc, IMS_INFO,
        "The Scan Results File query succeeded.");

    return (IMS_OK);
} /* ims_scanQuery */


/*******************************************************************
**
** subr getScanFileName ()
**
** Get the scan_results_file name from the granules
** table given mode, rev, sequence and mediaId.
**
** PR 2124 - Due to a bug in the ODL library formatting long path
** strings, we have implemented a work-around.  Instead of passing
** back the whole path of the Scan Results File we will only pass
** back the archived name of the file. See the "select" statement below.
**
******************************************************************** */
static int getScanFileName (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *granules_table,
    char *mode,
    int rev,
    int sequence,
    char *mediaId,
    char *scan_results_file,
    int *retStatus)
{
    static char cmdBuf[IMS_COL512_LEN];
    int status;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    IMS_SETROWCOUNT (qDesc, "1");

    /*
    ** Populate the command buffer with the SQL statement.
    ** Order the query by name descending in order to pick the
    ** latest file in case of multiple records. In this case
    ** the name will be the same except for the last three characters
    ** which represent a version number.  We are ordering by the
    ** name column because it is indexed and better represents the
    ** intended order of the files, better than received_time.
    */
    (void) sprintf (cmdBuf,
        "select name from %s \
        where MODE = '%s' and REVOLUTION = %d \
        and SEQUENCE = %d and MEDIA_ID = '%s' \
        order by name desc",
        granules_table, mode, rev, sequence, mediaId );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        *retStatus = status;
        IMS_SETROWCOUNT (qDesc, "0");
        return (IMS_ERROR);
    }

    /*
    ** Check the rows returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "No matching rows were found.");
        *retStatus = IMS_NOROWS;
        IMS_SETROWCOUNT (qDesc, "0");
        return (IMS_ERROR);
    }

    /*
    ** Get the scan results file name from the query descriptor.
    */
    (void) memcpy (scan_results_file,
        qDesc->valAddr[0], qDesc->valLength[0]);
    scan_results_file[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr (scan_results_file);

    /*
    ** Set the rows returned back to the default.
    */
    IMS_SETROWCOUNT (qDesc, "0");

    return (IMS_OK);
} /* getScanFileName */


/*******************************************************************
**
** subr ims_calParamQuery ()
**
** This function gets the FILE_NAME
** value from the granules_table.  The search is done on the
** mode and centerTime values.
**
******************************************************************** */
int ims_calParamQuery (
    IMS_CMN_QUERY *query,
    char *platform,
    char *mode,
    char *centerTime)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    IMS_NUMERIC_DATE dateStruct;
    char dataset[IMS_COL80_LEN+1];
    char granules_table[IMS_COL30_LEN+1];
    char queryTime[IMS_DATETIME_LEN+1];
    int status;

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting Calibration Parameter File query for '%s' "
        "with mode = '%s' and center_time = '%s'.",
        platform, mode, centerTime);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Get the granule table name.
    */
    (void) strcpy (dataset, platform);
    (void) strcat (dataset, " CALIBRATION PARAMETER FILE");
    if ((status = getGranuleTable (msgDesc, qDesc, dataset,
        granules_table, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule table name for dataset '%s'.",
            dataset);
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Validate and format the center time.
    */
    if ((status = ims_timeToNumericDate (msgDesc, centerTime,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Center Time '%s' is invalid.", centerTime);
        return (status);
    }
    ims_numericDateToIMSA (&dateStruct, queryTime);

    /*
    ** Get the Calibration Parameter File names.
    */
    if ((status = getCalFileName (query, granules_table, mode,
        queryTime)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain a Calibration Parameter File name.");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    (void) ims_msg (msgDesc, IMS_INFO,
        "The Calibration Parameter File query succeeded.");

    return (IMS_OK);
} /* ims_calParamQuery */


/*******************************************************************
**
** subr getCalFileName ()
**
** Get the file_name from the granules table given mode and date.
**
******************************************************************** */
static int getCalFileName (
    IMS_CMN_QUERY *query,
    char *granules_table,
    char *mode,
    char *center_time)
{
        IMS_MSG_STRUCT *msgDesc;
        IMS_QI_DESC_OBJ *qDesc;
    IMS_CAL_PARAM_STRUCT *currPtr;
    IMS_CAL_PARAM_STRUCT *prevPtr;
    static char cmdBuf[IMS_COL512_LEN];
    int status;
    int j;

        /*
        ** Initialize variables.
        */
        qDesc = query->qDesc;
        msgDesc = query->msgDesc;
        currPtr = (IMS_CAL_PARAM_STRUCT *) query->retPtr;
        if (currPtr != (IMS_CAL_PARAM_STRUCT *) NULL)
        {
            (void) memset ((void *) currPtr, 0, sizeof(
                IMS_CAL_PARAM_STRUCT));
            currPtr->next = (IMS_CAL_PARAM_STRUCT *) NULL;
        }
        prevPtr = (IMS_CAL_PARAM_STRUCT *) NULL;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
		** We had to move the query criteria down below the having
		** statement because of files that contained the identical
		** FILE_CREATION_TIME values.  For some reason the previous
		** criteria was bypassed in this situation.  We had to keep
		** the "where MODE" statement above the having statement because
		** in some cases rows were not picked up.  Unfortunately this
		** kludge will still not differentiate between rows that have
		** the same mode, time range and identical FILE_CREATION_TIME.
    */
    (void) sprintf (cmdBuf,
        "select SYS_ID, FILE_NAME from %s \
        where MODE = '%s' \
				and VALID_START_TIME <= '%s' \
				and VALID_END_TIME >= '%s' \
        group by SYS_ID \
        having FILE_CREATION_TIME = max(FILE_CREATION_TIME) \
        and MODE = '%s' \
        and VALID_START_TIME <= '%s' \
        and VALID_END_TIME >= '%s'",
        granules_table, mode, center_time, center_time,
				mode, center_time, center_time);

        /*
        ** Execute the command.
        */
        while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
        {
                if (status < IMS_OK)
                {
                        if (qDesc->msgNo == IMS_SYB_DEADLOCK)
                        {
                                query->retStatus = IMS_DEADLOCK;
                        }
                        else
                        {
                                query->retStatus = IMS_QUERYFAIL;
                        }

                        return (status);
                }

                /*
                ** If ENDOFQUERY, finish out command and return.
                */
                if (status == IMS_ENDOFQUERY)
                {
                        continue;
                }

                /*
                ** Allocate space for IMS_CAL_PARAM_STRUCT structure,
                ** if needed.

                */
                if (currPtr == (IMS_CAL_PARAM_STRUCT *) NULL)
                {
                        if ((currPtr = (IMS_CAL_PARAM_STRUCT *) malloc(
                        (size_t) sizeof (IMS_CAL_PARAM_STRUCT))) ==
                        (IMS_CAL_PARAM_STRUCT *) NULL)
                        {
                            (void) ims_msg (msgDesc, IMS_FATAL,
                                "Could not allocate memory for "
                                "IMS_CAL_PARAM_STRUCT structure.");
                            while (ims_qiNextRow (qDesc) !=
                                IMS_ENDOFTRANSACTION) {}
                                IMS_SETROWCOUNT (qDesc, "0");
                                (void) ims_qiResetDesc (qDesc);
                                return (IMS_FATAL);
                        }
                        (void) memset ((void *) currPtr, 0, sizeof(
                            IMS_CAL_PARAM_STRUCT));
                        currPtr->next = (IMS_CAL_PARAM_STRUCT *) NULL;

                        if (prevPtr != (IMS_CAL_PARAM_STRUCT *) NULL)
                        {
                        prevPtr->next = currPtr;
                        }
                        else
                        {
                        query->retPtr = (char *) currPtr;
                        }
                }

                /*
                ** Copy in the returned data.
                */

                j = 0;                         /* sys_id */
                (void) memcpy ((char *) currPtr->processor,
                        qDesc->valAddr[j], qDesc->valLength[j]);
                currPtr->processor[qDesc->valLength[j]] = '\0';
                (void) ims_truncStr( currPtr->processor );

                j++;                         /* file_name */
                (void) memcpy ((char *) currPtr->file_name,
                        qDesc->valAddr[j], qDesc->valLength[j]);
                currPtr->file_name[qDesc->valLength[j]] = '\0';
                (void) ims_truncStr( currPtr->file_name );

                prevPtr = currPtr;
                currPtr = currPtr->next;
        }

        /*
        ** Check the number of rows returned.
        */
        if (IMS_AFFECTED (qDesc) < 1)
        {
                (void) ims_msg (msgDesc, IMS_ERROR,
                   "No matching rows were found.");
                query->retStatus = IMS_NOROWS;
                return (IMS_ERROR);
        }

        return (IMS_OK);

} /* getCalFileName  */


/*******************************************************************
**
** subr ims_openQueryConnection ()
**
** Open a database server connection.
**
******************************************************************** */
int ims_openQueryConnection (
    IMS_CMN_QUERY *query)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;

    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) == IMS_OK)
    {
        /* Close this connection and free the query descriptor. */
        (void) ims_qiFreeDesc (query->qDesc);
    }

    /*
    ** Open the connection.
    */
    if ((query->qDesc = openConnection (query)) ==
        (IMS_QI_DESC_OBJ *) NULL){
        status = ims_msgGetSeverity (msgDesc);
        return (status);
    }

    return (IMS_OK);
}  /* ims_openQueryConnection */


/*******************************************************************
**
** subr ims_closeQueryConnection ()
**
** Close a database server connection.
**
******************************************************************** */
int ims_closeQueryConnection (
    IMS_CMN_QUERY *query)
{
    int status;

    if (query->qDesc != (IMS_QI_DESC_OBJ *) NULL)
    {
        if ((status = ims_qiFreeDesc (query->qDesc)) < IMS_OK)
        {
            (void) ims_msg (query->msgDesc, status,
                "Could not free the query descriptor.");
            return (status);
        }
    }

    query->qDesc = (IMS_QI_DESC_OBJ *) NULL;

    return (IMS_OK);
}  /* ims_closeQueryConnection */


/*******************************************************************
**
** subr ims_frameFree ()
**
** Free the IMS_FRAME_STRUCT structure.
**
******************************************************************** */
void ims_frameFree (
    IMS_FRAME_STRUCT *currPtr)
{
    IMS_FRAME_STRUCT *nextPtr;

    while (currPtr != (IMS_FRAME_STRUCT *) NULL)
    {
        nextPtr = currPtr->next;
        free (currPtr);
        currPtr = nextPtr;
    }

    return;
}   /*  ims_frameFree  */


/*******************************************************************
**
** subr ims_calParamFree ()
**
** Free the IMS_CAL_PARAM_STRUCT structure.
**
******************************************************************** */
void ims_calParamFree (
        IMS_CAL_PARAM_STRUCT *currPtr)
{
        IMS_CAL_PARAM_STRUCT *nextPtr;

        while (currPtr != (IMS_CAL_PARAM_STRUCT *) NULL)
        {
                nextPtr = currPtr->next;
                free (currPtr);
                currPtr = nextPtr;
        }

        return;
}   /*  ims_calParamFree  */


/*******************************************************************
**
** subr openConnection ()
**
** Open a database server connection.
**
******************************************************************** */
static IMS_QI_DESC_OBJ *openConnection (
    IMS_CMN_QUERY *query)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;

    msgDesc = query->msgDesc;

    /*
    ** Since this is the first time to access the catalog, we
    ** need a query descriptor allocated.  If we can't get a
    ** descriptor, return with a bad status ... we can't go on.
    */
    if ((qDesc = ims_qiDescAlloc (msgDesc)) ==
        (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, query->username);
    IMS_SETPSWD (qDesc, query->password);
    IMS_SETPROG (qDesc, query->program);

    if ((int) strlen (query->server) > 0)
    {
        IMS_SETSERVER (qDesc, query->server);
    }

    if ((int) strlen (query->database) > 0)
    {
        IMS_SETDBNAME (qDesc, query->database);
    }

    IMS_SET_VERBOSE (qDesc, 10);

    /*
    ** Login to the catalog database.
    */
    if ((status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        (void) ims_qiFreeDesc (qDesc);
        query->retStatus = IMS_NOCONNECT;
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (qDesc);
}  /* openConnection */


/*******************************************************************
**
** subr checkConnection ()
**
** Check the qDesc pointer for a valid database server connection.
**
******************************************************************** */
static int checkConnection (
    IMS_CMN_QUERY *query)
{
    int status;

    /*
    ** If this in NULL we can't possibly have a connection.
    */
    if (query->qDesc  == (IMS_QI_DESC_OBJ *) NULL)
    {
        return (IMS_ERROR);
    }
    else
    {
        /*
        ** Resetting the query descriptor will validate it.
        */
        if ((status = ims_qiResetDesc (query->qDesc)) < IMS_OK)
        {
            return (status);
        }
        else
        {
            /*
            ** See if the DBPROCESS has been marked dead.
            */
            if (DBDEAD (query->qDesc->dbproc) == TRUE)
            {
                return (IMS_ERROR);
            }
        }
    }
    return (IMS_OK);
}  /* checkConnection */


/*******************************************************************
**
** subr execCmd ()
**
** Execute a query. This function should only be used for commands
** that return one or no rows.
**
******************************************************************** */
static int execCmd (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc)
{
    int status;

    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                status = IMS_DEADLOCK;
            }
            else
            {
                status = IMS_QUERYFAIL;
            }
            return (status);
        }
    }

    /*
    ** Check the stored procedure status returned value.
    */
    if (checkRetStatus (msgDesc, qDesc) < IMS_OK)
    {
        return (IMS_QUERYFAIL);
    }

    return (IMS_OK);
}  /* execCmd */


/*******************************************************************
**
** subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************** */
static int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc)
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
        if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
        {
            if (procReturn == -103)
            {
                severity = IMS_FATAL;
            }
            else if (procReturn == -102)
            {
                severity = IMS_ERROR;
            }
            else if (procReturn == -101)
            {
                severity = IMS_WARNING;
            }
            else
            {
                severity = IMS_ERROR;
            }
            (void) ims_msg (msgDesc, severity,
                "Sybase procedure '%s' returned a status of %d",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }

    return (IMS_OK);
}  /* checkRetStatus */


/********************************************************************
**
** subr ims_darQuery ()
**
** This function will return a list of IMS dar entries matching
** the query criteria for status, start_time, and end_time.
** The inputs status, start_time, and end_time are optional.
** This function requires a valid, and active database connection
** in its IMS_CMN_QUERY input.
** List of DAR entries matching the search criteria are collected
** using the (IMS_DAR_LIST *) query->retPtr pointer value.
**
**********************************************************************/
int ims_darQuery (
          IMS_CMN_QUERY  *query,
          char           *status,
          char           *start_time,
          char           *end_time)
{
    int               return_status;
    IMS_MSG_STRUCT   *msgDesc;
    IMS_QI_DESC_OBJ  *qDesc;
    IMS_DAR_LIST     *retDef;
    IMS_NUMERIC_DATE  dateStruct;
    int               dar_status;
    char              tempStart[22];
    char              tempEnd[22];
    static char       cmdBuf[IMS_COL512_LEN];

    /*
    ** Initialize variables.
    */
    return_status = IMS_OK;
    query->retStatus = IMS_OK;
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    retDef = (IMS_DAR_LIST *) query->retPtr;
    (void) memset ((void *) retDef, 0, sizeof (IMS_DAR_LIST));
    retDef->next = (IMS_DAR_LIST *) NULL;

    /*
    ** Check for an active database connection.
    */
    if ((return_status = checkConnection (query)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, return_status,
        "ims_darQuery: The database server connection was not valid.");
    query->retStatus = IMS_NOCONNECT;
    return (return_status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Translate the status string status to its integer representation.
    */
    dar_status = 0;
    if (status != (char *) NULL)
        {
        /*
         ** Verify that the status string is a valid input value.
         ** NEW is not a valid input.
         */
        if (strcmp (status, "NEW") == 0)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
            "ims_darQuery: Status '%s' is not a valid input value.",
            status);
            (void) ims_qiResetDesc (qDesc);
            return (IMS_ERROR);
        }

        if((return_status = getItemsValue (msgDesc, qDesc, "dar_status",
             status, &dar_status, &(query->retStatus))) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
               "ims_darQuery: Status '%s' is not a valid string for "
               "dar status.", status);
            (void) ims_qiResetDesc (qDesc);
            return_status = IMS_ERROR;
            return (return_status);
        }
    }

    /*
    ** Format start_time
    */
    tempStart[0] = '\0';

    if (start_time != (char *) NULL)
    {
        if ((return_status = ims_timeToNumericDate (msgDesc, start_time,
                &dateStruct)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, return_status,
                    "Start date '%s' is invalid.", start_time);
            (void) ims_qiResetDesc (qDesc);
            return_status = IMS_ERROR;
            return (return_status);
        }
        (void) ims_numericDateToIMSA (&dateStruct, tempStart);
    }

    /*
    ** Format end_time
    */
    tempEnd[0] = '\0';

    if (end_time != (char *) NULL)
    {
    if ((return_status = ims_timeToNumericDate (msgDesc, end_time,
            &dateStruct)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, return_status,
                    "End date '%s' is invalid.", end_time);
            (void) ims_qiResetDesc (qDesc);
            return_status = IMS_ERROR;
            return (return_status);
        }

        (void) ims_numericDateToIMSA (&dateStruct, tempEnd);
    }

    /*
    ** Execute query for dar entries.
    */
    if ((return_status = getDarData (msgDesc, qDesc, status, dar_status,
             tempStart, tempEnd, retDef, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
                "ims_darQuery: Could not retrieve dar info.");
        (void) ims_qiResetDesc (qDesc);
        return (return_status);
    }

    (void) ims_qiResetDesc (qDesc);

    return (return_status);
}   /* ims_darQuery */


/********************************************************************
**
** subr getDarData ()
**
** This function will retrieve the data from the dar table for
** the given status, start_time, and end_time.
**
**********************************************************************/
static int getDarData (
               IMS_MSG_STRUCT  *msgDesc,
               IMS_QI_DESC_OBJ *qDesc,
               char            *status,
               int              dar_status,
               char            *start_time,
               char            *end_time,
               IMS_DAR_LIST    *retDef,
               int             *retStatus)
{
    int               return_status;
    static char       cmdBuf[IMS_COL1024_LEN+1];
    char             *cmdBufPtr;
    ITEMS_LIST        priorityItemsFirst;
    ITEMS_LIST       *priorityItemsPtr;
    ITEMS_LIST        spatialTypeItemsFirst;
    ITEMS_LIST       *spatialTypeItemsPtr;
    ITEMS_LIST        darStatusItemsFirst;
    ITEMS_LIST       *darStatusItemsPtr;
    IMS_DAR_LIST     *retDef_curr;
    IMS_DAR_LIST     *retDef_last;
    int               index;
    int               temp_int_type;
    short             temp_short_type;
    float             temp_float_type;
    short             temp_priority;
    short             temp_spatial_type;
    short             temp_status;
    char              temp_time[22];
    IMS_NUMERIC_DATE  dateStruct;
    int               found;
    int               dar_status_new;


    /*
    ** Initialize local variables.
    */
    return_status = IMS_OK;
    *retStatus = IMS_OK;
    cmdBuf[0] = '\0';
    cmdBufPtr = cmdBuf;
    priorityItemsFirst.next = (ITEMS_LIST *) NULL;
    priorityItemsPtr = (ITEMS_LIST *) NULL;
    spatialTypeItemsFirst.next = (ITEMS_LIST *) NULL;
    spatialTypeItemsPtr = (ITEMS_LIST *) NULL;
    darStatusItemsFirst.next = (ITEMS_LIST *) NULL;
    darStatusItemsPtr = (ITEMS_LIST *) NULL;
    retDef_curr = (IMS_DAR_LIST *) NULL;
    retDef_last = (IMS_DAR_LIST *) NULL;

    /*
    ** Query items table for priority values and descriptions.
    */
    if ((return_status = getItemsData (msgDesc, qDesc, "priority",
         &priorityItemsFirst, retStatus)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, return_status,
            "getDarData: Could not retrieve priority values from Items"
            " table.");
        return (return_status);
    }

    /*
    ** Query items table for spatial type values and descriptions.
    */
    if ((return_status = getItemsData (msgDesc, qDesc, "spatial_type",
         &spatialTypeItemsFirst, retStatus)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, return_status,
            "getDarData: Could not retrieve spatial type values from "
            "Items table.");
        (void) freeItemsList (&priorityItemsFirst);
        return (return_status);
    }

    /*
    ** Query items table for dar_status values and their descriptions.
    */
    if ((return_status = getItemsData (msgDesc, qDesc, "dar_status",
         &darStatusItemsFirst, retStatus)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, return_status,
            "getDarData: Could not retrieve dar status values from "
            "items table.");
        (void) freeItemsList (&priorityItemsFirst);
        (void) freeItemsList (&spatialTypeItemsFirst);
        return (return_status);
    }

    /*
    ** Set up the dar table query.
    */
    (void) strcpy (cmdBufPtr,
    "select dar.order_id, dar.item_id, oq.user_id, oq.account_id,\n"
    "   dar.platform, dar.sensor, dar.mode, oi.quicklook_p,\n"
    "   dar.asc_desc, oq.priority,\n"
    "   received_time = convert (char(10), oq.received_time, 111)\n"
    "                   + 'T'\n"
    "                   + convert (char(12), oq.received_time, 108),\n"
    "   completed_time = convert (char(10), oq.completed_time, 111)\n"
    "                   + 'T'\n"
    "                   + convert (char(12), oq.completed_time, 108),\n"
    "   dar.start_date, dar.end_date, dar.site_name,dar.spatial_type,\n"
    "   dar.radius, dar.center_lat,dar.center_lon,dar.north_west_lat,\n"
    "   dar.north_west_lon, dar.north_east_lat, dar.north_east_lon,\n"
    "   dar.south_west_lat, dar.south_west_lon, dar.south_east_lat,\n"
    "   dar.south_east_lon,dar.observation_freq,dar.observation_num,\n"
    "   dar.pi_name, dar.pi_discipline, dar.active_p,\n"
    "   dar.activity_start_date, dar.activity_end_date, dar.status,\n"
    "   dar.user_comment, dar.planner_comment, dar.op_comment\n"
    "from   dar, order_queue oq, order_item oi\n"
    "where  dar.order_id = oq.order_id\n"
    "       and dar.order_id = oi.order_id\n"
    "       and dar.item_id = oi.item_id\n");
    cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);

    if (status && *status)
    {
        (void) sprintf (cmdBufPtr, "and dar.status = %d\n", dar_status);
        cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);
    }

    if (start_time && *start_time)
    {
        (void) sprintf (cmdBufPtr,
            "and dar.start_date >= '%s'\n",
            start_time);
        cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);
    }

    if (end_time && *end_time)
    {
        (void) sprintf (cmdBufPtr,
            "and dar.end_date <= '%s'\n",
            end_time);
        cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);
    }

    /*
    ** Get the correct instance for dar status value NEW.
    */
    if ((return_status = getItemsValue (msgDesc, qDesc, "dar_status",
        "NEW", &dar_status_new, retStatus)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, return_status,
            "Could not translate STATUS value NEW.");
        (void) ims_qiResetDesc (qDesc);
        (void) freeItemsList (&priorityItemsFirst);
        (void) freeItemsList (&spatialTypeItemsFirst);
        (void) freeItemsList (&darStatusItemsFirst);
        return (return_status);
    }

    /*
    ** Make sure entries with status NEW are not retrieved.
    ** Only validated entries are returned.
    */
    (void) sprintf (cmdBufPtr, "and dar.status != %d\n",
    dar_status_new);
    cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);

    (void) sprintf (cmdBufPtr, "order by dar.order_id, dar.item_id");
    cmdBufPtr = cmdBufPtr + strlen (cmdBufPtr);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((return_status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, return_status,
            "Could not reset the query descriptor.");
        (void) freeItemsList (&priorityItemsFirst);
        (void) freeItemsList (&spatialTypeItemsFirst);
        (void) freeItemsList (&darStatusItemsFirst);
        return (return_status);
    }

    retDef_curr = retDef;
    retDef_last = (IMS_DAR_LIST *) NULL;
    while ((return_status = ims_qiNextRow (qDesc)) !=
        IMS_ENDOFTRANSACTION){
    if (return_status < IMS_OK)
    {
        if (qDesc->msgNo == IMS_SYB_DEADLOCK)
        {
        *retStatus = IMS_DEADLOCK;
        }
        else
        {
        *retStatus = IMS_QUERYFAIL;
        }

        (void) freeItemsList (&priorityItemsFirst);
        (void) freeItemsList (&spatialTypeItemsFirst);
        (void) freeItemsList (&darStatusItemsFirst);
        return (return_status);
    }

    /*
     ** If ENDOFQUERY, finish out command and return.
     */
    if (return_status == IMS_ENDOFQUERY)
    {
        continue;
    }

    /*
    ** Allocate next row, if needed.
    */
    if (retDef_curr == (IMS_DAR_LIST *) NULL)
    {
        retDef_curr = (IMS_DAR_LIST *) malloc (sizeof (IMS_DAR_LIST));
        (void) memset ((void *) retDef_curr, 0, sizeof (IMS_DAR_LIST));
        retDef_curr->next = (IMS_DAR_LIST *) NULL;
        retDef_last->next = retDef_curr;
    }

    /*
    ** Copy the returned data into the structure.
    */

    /* order_id */
    index = 0;
    temp_int_type = 0;
    (void) memcpy ((char *) &temp_int_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->order_id = temp_int_type;

    /* item_id */
    index++;
    temp_short_type = 0;
    (void) memcpy ((char *) &temp_short_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->item_id = temp_short_type;

    /* user_id */
    index++;
    (void) memcpy ((char *) retDef_curr->user_id,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->user_id[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->user_id);

    /* account_id */
    index++;
    (void) memcpy ((char *) retDef_curr->account_id,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->account_id[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->account_id);

    /* platform */
    index++;
    (void) memcpy ((char *) retDef_curr->platform,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->platform[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->platform);

    /* sensor */
    index++;
    (void) memcpy ((char *) retDef_curr->sensor,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->sensor[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->sensor);

    /* mode */
    index++;
    (void) memcpy ((char *) retDef_curr->mode,
        qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->mode[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->mode);

    /* quicklook_p */
    index++;
    (void) memcpy ((char *) &retDef_curr->quicklook_p,
        qDesc->valAddr[index], qDesc->valLength[index]);

    /* asc_desc */
    index++;
    (void) memcpy ((char *) &retDef_curr->asc_desc,
        qDesc->valAddr[index], qDesc->valLength[index]);

    /* priority */
    index++;
    (void) memcpy ((char *) &temp_priority,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->priority[0] = '\0';
    priorityItemsPtr = &priorityItemsFirst;
    found = IMS_FALSE;
    while ((found != IMS_TRUE) && (priorityItemsPtr !=
        (ITEMS_LIST *) NULL)){
        if (temp_priority == priorityItemsPtr->instance)
        {
            found = IMS_TRUE;
            strcpy (retDef_curr->priority,
                priorityItemsPtr->description);
        }
        priorityItemsPtr = priorityItemsPtr->next;
    }

    /* received_time */
    index++;
    (void) memcpy ((char *) temp_time,
               qDesc->valAddr[index], qDesc->valLength[index]);
    temp_time[qDesc->valLength[index]] = '\0';
    ims_truncStr (temp_time);

    if (ims_timeToNumericDate(msgDesc, temp_time, &dateStruct) < IMS_OK)
    {
        retDef_curr->receive_time[0] = '\0';
    }
    else
    {
        (void) ims_numericDateToIMSA (&dateStruct,
                      retDef_curr->receive_time);
    }

    /* completed_time */
    /*
    ** PR 1031 - The select statement above for this column will always
    ** result in a 'T' being returned if the actual value is NULL.  So
    ** we will check for a 'T' instead of a NULL.
    */
    index++;
    if (strncmp (qDesc->valAddr[index], "T", (size_t) 1) == 0)
    {
        /* completed_time is NULL */
        retDef_curr->complete_time[0] = '\0';
    }
    else
    {
        (void) memcpy ((char *) temp_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        temp_time[qDesc->valLength[index]] = '\0';
        ims_truncStr (temp_time);

        if (ims_timeToNumericDate (msgDesc, temp_time, &dateStruct)
            < IMS_OK){
            retDef_curr->complete_time[0] = '\0';
        }
        else
        {
            (void) ims_numericDateToIMSA (&dateStruct,
                retDef_curr->complete_time);
        }
    }

    /* start_time */
    index++;
    (void) memcpy ((char *) retDef_curr->start_time,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->start_time[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->start_time);

    /* end_time */
    index++;
    (void) memcpy ((char *) retDef_curr->end_time,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->end_time[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->end_time);

    /* site_name */
    index++;
    (void) memcpy ((char *) retDef_curr->site_name,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->site_name[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->site_name);

    /* spatial_type */
    index++;
    (void) memcpy ((char *) &temp_spatial_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->site_shape = '\0';
    spatialTypeItemsPtr = &spatialTypeItemsFirst;
    found = IMS_FALSE;
    while ((found != IMS_TRUE)
           &&
           (spatialTypeItemsPtr != (ITEMS_LIST *) NULL))
    {
        if (temp_spatial_type == spatialTypeItemsPtr->instance)
        {
        found = IMS_TRUE;
        retDef_curr->site_shape = spatialTypeItemsPtr->description[0];
        }
        spatialTypeItemsPtr = spatialTypeItemsPtr->next;
    }

    /* radius */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->radius = temp_float_type;

    /* center_lat */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->center_lat = temp_float_type;

    /* center_lon */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->center_lon = temp_float_type;

    /* north_west_lat */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->north_west_lat = temp_float_type;

    /* north_west_lon */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->north_west_lon = temp_float_type;

    /* north_east_lat */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->north_east_lat = temp_float_type;

    /* north_east_lon */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->north_east_lon = temp_float_type;

    /* south_west_lat */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->south_west_lat = temp_float_type;

    /* south_west_lon */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->south_west_lon = temp_float_type;

    /* south_east_lat */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->south_east_lat = temp_float_type;

    /* south_east_lon */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) &temp_float_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->south_east_lon = temp_float_type;

    /* observation_freq */
    index++;
    temp_float_type = 0.0;
    (void) memcpy ((char *) retDef_curr->observation_freq,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->observation_freq[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->observation_freq);

    /* observation_num */
    index++;
    temp_int_type = 0;
    (void) memcpy ((char *) &temp_int_type,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->observation_num = temp_int_type;

    /* pi_name */
    index++;
    (void) memcpy ((char *) retDef_curr->pi_name,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->pi_name[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->pi_name);

    /* pi_discipline */
    index++;
    (void) memcpy ((char *) retDef_curr->pi_discipline,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->pi_discipline[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->pi_discipline);

    /* active_p */
    index++;
    (void) memcpy ((char *) &retDef_curr->active_p,
               qDesc->valAddr[index], qDesc->valLength[index]);

    /* activity_start_date */
    index++;
    (void) memcpy ((char *) retDef_curr->activity_start_date,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->activity_start_date[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->activity_start_date);

    /* activity_end_date */
    index++;
    (void) memcpy ((char *) retDef_curr->activity_end_date,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->activity_end_date[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->activity_end_date);

    /* status */
    index++;
    (void) memcpy ((char *) &temp_status,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->status[0] = '\0';
    darStatusItemsPtr = &darStatusItemsFirst;
    found = IMS_FALSE;
    while ((found != IMS_TRUE) && (darStatusItemsPtr !=
        (ITEMS_LIST *) NULL)){
        if (temp_status == darStatusItemsPtr->instance)
        {
        found = IMS_TRUE;
        strcpy (retDef_curr->status, darStatusItemsPtr->description);
        }
        darStatusItemsPtr = darStatusItemsPtr->next;
    }

    /* user_comment */
    index++;
    (void) memcpy ((char *) retDef_curr->user_comment,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->user_comment[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->user_comment);

    /* planner_comment */
    index++;
    (void) memcpy ((char *) retDef_curr->planner_comment,
           qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->planner_comment[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->planner_comment);

    /* op_comment */
    index++;
    (void) memcpy ((char *) retDef_curr->op_comment,
           qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->op_comment[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->op_comment);

    /*
    ** Go to next row.
    */
    retDef_last = retDef_curr;
    retDef_curr = retDef_curr->next;
    }

    if (IMS_AFFECTED(qDesc) < 1)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "getDarData: No rows found for dar query.");
        *retStatus = IMS_NOROWS;
        (void) freeItemsList (&priorityItemsFirst);
        (void) freeItemsList (&spatialTypeItemsFirst);
        (void) freeItemsList (&darStatusItemsFirst);
        return (IMS_ERROR);
    }

    (void) freeItemsList (&priorityItemsFirst);
    (void) freeItemsList (&spatialTypeItemsFirst);
    (void) freeItemsList (&darStatusItemsFirst);
    return (IMS_OK);
} /* getDarData  */


/********************************************************************
**
** subr getItemsData ()
**
** This function will retrieve the available values, and descriptions
** of a given type from the items table.
**
**********************************************************************/
static int getItemsData (
               IMS_MSG_STRUCT  *msgDesc,
               IMS_QI_DESC_OBJ *qDesc,
               char            *type_name,
               ITEMS_LIST      *retDef,
               int             *retStatus)
{
    int          status;
    int          index;
    static  char cmdBuf[IMS_COL512_LEN];
    ITEMS_LIST  *retDef_curr;
    ITEMS_LIST  *retDef_last;
    short        temp_instance;

    status = IMS_OK;
    *retStatus = IMS_OK;

    (void) sprintf (cmdBuf,
           "select instance, description from items where type = '%s'",
           type_name);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "Could not reset the query descriptor.");
    return (status);
    }

    retDef_curr = retDef;
    retDef_last = (ITEMS_LIST *) NULL;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
    if (status < IMS_OK)
    {
        if (qDesc->msgNo == IMS_SYB_DEADLOCK)
        {
        *retStatus = IMS_DEADLOCK;
        }
        else
        {
        *retStatus = IMS_QUERYFAIL;
        }

        return (status);
    }

    /*
     ** If ENDOFQUERY, we want to finish out command and return.
     */
    if (status == IMS_ENDOFQUERY)
    {
        continue;
    }

    /*
    ** Allocate next row, if needed.
    */
    if (retDef_curr == (ITEMS_LIST *) NULL)
    {
        retDef_curr = (ITEMS_LIST *) malloc (sizeof (ITEMS_LIST));
        (void) memset ((void *) retDef_curr, 0, sizeof (ITEMS_LIST));
        retDef_curr->next = (ITEMS_LIST *) NULL;
        retDef_last->next = retDef_curr;
    }

    /*
    ** Copy the returned data into the structure.
    */

    /* instance */
    index = 0;
    (void) memcpy ((char *) &temp_instance,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->instance = temp_instance;

    /* description */
    index++;
    (void) memcpy ((char *) retDef_curr->description,
               qDesc->valAddr[index], qDesc->valLength[index]);
    retDef_curr->description[qDesc->valLength[index]] = '\0';
    ims_truncStr (retDef_curr->description);

    /*
        ** Go to next row.
        */
    retDef_last = retDef_curr;
    retDef_curr = retDef_curr->next;
    }

    if (IMS_AFFECTED (qDesc)  <  1)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "getItemsData: No rows: item name %s not found.",
        type_name);
    *retStatus = IMS_NOROWS;
    return (IMS_ERROR);
    }

    return (IMS_OK);
}   /*  getItemsData  */


/********************************************************************
**
** subr ims_darStatus ()
**
** Reports dar status and planner comment values to IMS.
** Note that dar entries with the IMS status value COMPLETED can not
** be updated.
** This function requires a valid, and active database connection
** in its IMS_CMN_QUERY input.
**
**********************************************************************/
int ims_darStatus (
           IMS_CMN_QUERY  *query,
                   IMS_DAR_STATUS *darStatus)
{
    int              status;
    IMS_MSG_STRUCT  *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int              dar_status;
    int              current_dar_status;
    int              dar_status_rejected;
    int              dar_status_completed;
    int              dar_status_validated;
    int              current_item_status;
    int              item_status_validated;
    int              item_status;
    char             comment_text[255+1];
    char            *planner_comment;
    static char      cmdBuf[IMS_COL512_LEN];

    /*
    ** Initialize variables.
    */
    status = IMS_OK;
    query->retStatus = IMS_OK;
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;

    /*
    ** Check for an active database connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "ims_darStatus: The database server connection was not valid.");
    query->retStatus = IMS_NOCONNECT;
    return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Translate the character string status to integer.
    */
    if ((status = getItemsValue (msgDesc, qDesc, "dar_status",
        darStatus->status, &dar_status, &(query->retStatus))) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "ims_darStatus: Status '%s' is not a valid string for dar"
        " status.", darStatus->status);
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** Verify that the status string is a valid input value.
    ** Valid values are: IN-PLANNING, REJECTED, COMPLETED.
    */
    if ((strcmp (darStatus->status, "IN-PLANNING") != 0) &&
        (strcmp (darStatus->status, "REJECTED") != 0) &&
    (strcmp (darStatus->status, "COMPLETED") != 0))
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "ims_darStatus: Status '%s' is not a valid input value.",
        darStatus->status);
    (void) ims_qiResetDesc (qDesc);
    status = IMS_ERROR;
    return (status);
    }

    /*
    ** Begin the update transaction.
    */
    if ((status = setTransState (msgDesc, qDesc, "begin")) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
            "Could not begin the update transaction.");
    query->retStatus = IMS_QUERYFAIL;
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** Lock the dar table until update is complete.
    */
    if ((status = getGeneralLock (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status, "Could not get general_lock.");
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** Get the current status of this dar entry.
    */
    if ((status = getDarStatus (msgDesc, qDesc, darStatus->order_id,
         darStatus->item_id, &current_dar_status, &(query->retStatus)))
             < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "ims_darStatus: Could not get status from dar table for "
        "order_id:%d, item_id:%d.",
        darStatus->order_id, darStatus->item_id);
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** Get the correct instance for dar status value REJECTED.
    */
    if ((status = getItemsValue (msgDesc, qDesc, "dar_status",
        "REJECTED", &dar_status_rejected, &(query->retStatus)))
        < IMS_OK){
        (void) ims_msg (msgDesc, status,
            "ims_darStatus: Could not translate STATUS value REJECTED."
            );
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Get the correct instance for dar status value COMPLETED.
    */
    if ((status = getItemsValue (msgDesc, qDesc, "dar_status",
        "COMPLETED", &dar_status_completed, &(query->retStatus)))
        < IMS_OK){
        (void) ims_msg (msgDesc, status,
            "ims_darStatus: Could not translate STATUS value "
            "COMPLETED.");
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Make sure the current dar status is not COMPLETED or REJECTED.
    ** Dar entries with IMS status value COMPLETED or REJECTED
    ** can not be updated.
    */
    if ((current_dar_status == dar_status_completed) ||
    (current_dar_status == dar_status_rejected))
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "ims_darStatus: Dar status is already COMPLETED or REJECTED.");
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    status = IMS_ERROR;
    return (status);
    }

    /*
    ** Get the correct instance for dar status value VALIDATED.
    */
    if ((status = getItemsValue (msgDesc, qDesc, "dar_status",
        "VALIDATED", &dar_status_validated, &(query->retStatus)))
        < IMS_OK){
        (void) ims_msg (msgDesc, status,
            "ims_darStatus: Could not translate STATUS value "
            "VALIDATED.");
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Make sure the current dar status is at least VALIDATED.
    ** Dar entries which are not yet VALIDATED can not be updated.
    */
    if (current_dar_status < dar_status_validated)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not perform update: Dar entry has not been "
            "VALIDATED.");
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        status = IMS_ERROR;
        return (status);
    }

    /*
    ** If the new dar status is COMPLETED or REJECTED,
    ** make sure that the order_item status is VALIDATED.
    */
    if ((dar_status == dar_status_completed) ||
        (dar_status == dar_status_rejected))
        {
        /*
        ** Get the current status of this order_item entry.
        */
        if ((status = getOrderItemStatus (msgDesc, qDesc,
            darStatus->order_id, darStatus->item_id,
            &current_item_status, &(query->retStatus))) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get status from order_item table for "
                "order_id:%d, item_id:%d.",
                darStatus->order_id, darStatus->item_id);
            (void) setTransState (msgDesc, qDesc, "rollback");
            (void) ims_qiResetDesc (qDesc);
            return (status);
        }

        /*
        ** Get the correct instance for order_item status value
        **      VALIDATED.
        */
        if ((status = getItemsValue (msgDesc, qDesc, "item_status",
            "VALIDATED", &item_status_validated,
                    &(query->retStatus))) < IMS_OK)
        {
            (void) ims_msg (msgDesc, status,
            "Could not translate order_item status value VALIDATED.");
            (void) setTransState (msgDesc, qDesc, "rollback");
            (void) ims_qiResetDesc (qDesc);
            return (status);
        }

        /*
        ** If order_item status is not at least VALIDATED, don't update.
        */
        if (current_item_status < item_status_validated)
        {
            (void) ims_msg (msgDesc, status,
                "Could not perform update: order_item entry has not "
                "been VALIDATED.");
            (void) setTransState (msgDesc, qDesc, "rollback");
            (void) ims_qiResetDesc (qDesc);
            status = IMS_ERROR;
            return (status);
        }
    }

    comment_text[0] = '\0';
    (void) strncat (comment_text, darStatus->planner_comment,
            strlen (darStatus->planner_comment));
    planner_comment = (char *) &comment_text;

    /*
    ** Update the dar table with the status and planner_comment info.
    */
    if ((status = setDarStatus (msgDesc, qDesc, darStatus->order_id,
            darStatus->item_id, dar_status, planner_comment,
            &(query->retStatus))) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "ims_darStatus: Could not update dar table for order_id:%d, "
        "item_id:%d.", darStatus->order_id, darStatus->item_id);
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** If the new dar status is COMPLETED or REJECTED,
    ** update order_item status.
    */
    if ((dar_status == dar_status_completed) ||
    (dar_status == dar_status_rejected))
    {
    /*
    ** Get the correct instance for the order_item status GENERATED.
    */
    if ((status = getItemsValue (msgDesc, qDesc, "item_status",
         "GENERATED", &item_status, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "ims_darStatus: Could not translate order_item status "
            "value GENERATED.");
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Update status of order_item table to GENERATED.
    */
    if ((status = setOrderItemStatus (msgDesc, qDesc,
                darStatus->order_id, darStatus->item_id,
            item_status, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
                "ims_darStatus: Could not update order_item table for "
                "order_id:%d, item_id:%d.",
                darStatus->order_id, darStatus->item_id);
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }
    }

    /*
    ** Commit the update transaction.
    */
    if ((status = setTransState (msgDesc, qDesc, "commit")) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "Could not commit the update transaction.");
    query->retStatus = IMS_QUERYFAIL;
    }

    (void) ims_qiResetDesc (qDesc);

    return (status);
}   /* ims_darStatus */


/*******************************************************************
**
** subr setTransState ()
**
** Set the state of the transaction.
**
******************************************************************** */
static int setTransState (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *transType)
{
    static char cmdBuf[IMS_COL512_LEN];
    int status;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf, "%s transaction", transType);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Execute the cammand.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        return (IMS_ERROR);
    }
    return (IMS_OK);
}   /*  setTransState  */


/********************************************************************
**
** subr getDarStatus ()
**
** This function will retrieve the status value from the dar table.
**
**********************************************************************/
static int getDarStatus (
               IMS_MSG_STRUCT  *msgDesc,
               IMS_QI_DESC_OBJ *qDesc,
                           int              order_id,
                           short            item_id,
                           int             *dar_status,
                           int             *retStatus)
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];
    short          temp_status;


    status = IMS_OK;
    *retStatus = IMS_OK;

    (void) sprintf (cmdBuf,
            "select status \
                     from   dar \
                     where  order_id = %d \
                            and item_id = %d",
                     order_id, item_id);

    /*
     ** Assign the command buffer to the query descriptor.
     */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "Could not reset the query descriptor.");
    return (status);
    }

    /*
    ** Execute dar table query.
    ** This query will return a single row.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
            "getDarStatus: Dar table query failed.");
    *retStatus = status;
    status = IMS_ERROR;
    return (status);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "getDarStatus: Dar entry order_id:%d, item_id:%d not found.",
         order_id, item_id);
    *retStatus = IMS_NOROWS;
    status = IMS_ERROR;
    return (status);
    }

    /*
    ** Copy the returned data into the output variable.
    */
    (void) memcpy ((char *) &temp_status,
           qDesc->valAddr[0], qDesc->valLength[0]);
    *dar_status = temp_status;

    return (IMS_OK);
} /* getDarStatus */


/********************************************************************
**
** subr setDarStatus ()
**
** This function will update the dar table with the dar status and
** planner comment values passed in.
**
**********************************************************************/
static int setDarStatus (
               IMS_MSG_STRUCT  *msgDesc,
               IMS_QI_DESC_OBJ *qDesc,
                           int              order_id,
                           short            item_id,
                           int              dar_status,
                           char            *planner_comment,
                           int             *retStatus)
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];


    status = IMS_OK;
    *retStatus = IMS_OK;

    (void) sprintf (cmdBuf,
            "update dar \
                     set    status = %d, \
                            planner_comment = '%s' \
                     where  order_id = %d \
                            and item_id = %d",
                     dar_status, planner_comment, order_id, item_id);

    /*
     ** Assign the command buffer to the query descriptor.
     */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
     ** Reset the query descriptor.
     */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
        "Could not reset the query descriptor.");
        return (status);
    }

    /*
     ** Execute the database update.
     */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "setDarStatus: Dar table update failed.");
        *retStatus = status;
            status = IMS_ERROR;
        return (status);
    }

    if (IMS_AFFECTED (qDesc) > 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "setDarStatus: Dar table update affected more than 1 row.");
        *retStatus = IMS_QUERYFAIL;
            status = IMS_ERROR;
        return (status);
    }

    return (IMS_OK);
} /* setDarStatus */


/*******************************************************************
**
** subr getOrderItemStatus ()
**
** This function will retrieve the status value from
** the order_item table.
**
******************************************************************** */
static int getOrderItemStatus (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    int              order_id,
    short            item_id,
    int             *item_status,
    int             *retStatus)
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];
    short          temp_status;

    (void) sprintf (cmdBuf,
        "select status \
        from   order_item \
        where  order_id = %d \
        and item_id = %d",
        order_id, item_id);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Execute order_item table query.
    ** This query will return a single row.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Order_item table query failed.");
        *retStatus = status;
        status = IMS_ERROR;
        return (status);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Order_item entry order_id:%d, item_id:%d not found.",
            order_id, item_id);
        *retStatus = IMS_NOROWS;
        status = IMS_ERROR;
        return (status);
    }

    /*
    ** Copy the returned data into the output variable.
    */
    (void) memcpy ((char *) &temp_status,
        qDesc->valAddr[0], qDesc->valLength[0]);
    *item_status = temp_status;

    return (IMS_OK);
} /* getOrderItemStatus */


/********************************************************************
**
** subr setOrderItemStatus ()
**
** This function will update the order_item table with the process
** status value passed in.
**
**********************************************************************/
static int setOrderItemStatus (
               IMS_MSG_STRUCT  *msgDesc,
               IMS_QI_DESC_OBJ *qDesc,
                           int              order_id,
                           short            item_id,
                           int              item_status,
                           int             *retStatus)
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];


    status = IMS_OK;
    *retStatus = IMS_OK;

    (void) sprintf (cmdBuf,
            "update order_item \
                     set    status = %d \
                     where  order_id = %d \
                            and item_id = %d",
                     item_status, order_id, item_id);

    /*
     ** Assign the command buffer to the query descriptor.
     */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
     ** Reset the query descriptor.
     */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "Could not reset the query descriptor.");
    return (status);
    }

    /*
     ** Execute the database update.
     */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "setOrderItemStatus: Order_item table update failed.");
    *retStatus = status;
        status = IMS_ERROR;
    return (status);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "setOrderItemStatus: Order_item table update affected more "
            "than 1 row.");
        *retStatus = IMS_QUERYFAIL;
            status = IMS_ERROR;
        return (status);
    }

    return (IMS_OK);
} /* setOrderItemStatus */


/********************************************************************
**
** subr ims_darStatistics ()
**
** This function reports dar statistical information to IMS.
** This function will update the dar_statistics table with the data
** reported via the IMS_DAR_STATISTICS structure.
**
**********************************************************************/

int ims_darStatistics (
               IMS_CMN_QUERY       *query,
               IMS_DAR_STATISTICS  *darStats)
{
    int               status;
    IMS_MSG_STRUCT   *msgDesc;
    IMS_QI_DESC_OBJ  *qDesc;
    IMS_NUMERIC_DATE  dateStruct;
    char              timeStamp[22];
    static char       cmdBuf[IMS_COL512_LEN];


    /*
    ** Initialize variables.
    */
    status = IMS_OK;
    query->retStatus = IMS_OK;
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;

    /*
    ** Check for an active database connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "ims_darStatistics: The database server connection was not"
        " valid.");
    query->retStatus = IMS_NOCONNECT;
    return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Verify the format of the time stamp and complete the string
    ** if necessary.
    */
    timeStamp[0] = '\0';

    if (darStats->time_stamp != (char *) NULL)
    {
    if ((status = ims_timeToNumericDate (msgDesc, darStats->time_stamp,
            &dateStruct)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
        "Time stamp '%s' is invalid.", darStats->time_stamp);
        return (status);
    }

    (void) ims_numericDateToIMSA (&dateStruct, timeStamp);
    }

    /*
    ** Verify that the status string is a valid input value.
    ** Valid values are: REQUESTED, SUBMITTED, PLANNED.
    */
    if ((strcmp (darStats->status, "REQUESTED") != 0) &&
        (strcmp (darStats->status, "SUBMITTED") != 0) &&
    (strcmp (darStats->status, "PLANNED") != 0))
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "ims_darStatistics: Status '%s' is not a valid input value.",
        darStats->status);
    (void) ims_qiResetDesc (qDesc);
    status = IMS_ERROR;
    return (status);
    }

    /*
    ** Verify that an order_item table entry exists for the
    ** given order_id and item_id values.
    */
    if ((status = validateOrderItem (msgDesc, qDesc, darStats->order_id,
          darStats->item_id, &(query->retStatus))) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
    "ims_darStatistics: Could not find order_item entry with order "
        "id: %d, item_id: %d.",
        darStats->order_id, darStats->item_id);
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    /*
    ** Update the dar_statistics table.
    */
    if ((status = setDarStatistics (msgDesc, qDesc,
         darStats->order_id, darStats->item_id, timeStamp,
         darStats->status, darStats->seconds, &(query->retStatus)))
        < IMS_OK){
    (void) ims_msg (msgDesc, status,
        "ims_darStatistics: Error in inserting dar_statistics table "
            "for order_id: %d, item_id: %d.",
            darStats->order_id, darStats->item_id);
    (void) ims_qiResetDesc (qDesc);
    return (status);
    }

    (void) ims_qiResetDesc (qDesc);

    return (status);
}   /* ims_darStatistics */


/********************************************************************
**
** subr setDarStatistics ()
**
** This function will update the dar statistics table with the reported
** information for the given order_id, and item_id.
**
**********************************************************************/
static int setDarStatistics (
                 IMS_MSG_STRUCT  *msgDesc,
                 IMS_QI_DESC_OBJ *qDesc,
                 int              order_id,
                 short            item_id,
                 char            *time_stamp,
                 char            *inStatus,
                 int              seconds,
                 int             *retStatus)
{
    int               status;
    static char       cmdBuf[IMS_COL512_LEN+1];


    status = IMS_OK;
    *retStatus = IMS_OK;

    (void) sprintf (cmdBuf,
        "insert dar_statistics (order_id, item_id, TIME_STAMP, STATUS,"
        " SECONDS)\n  values (%d, %d, '%s', '%s', %d)",
        order_id, item_id, time_stamp, inStatus, seconds);

    /*
     ** Assign the command buffer to the query descriptor.
     */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, status,
        "Could not reset the query descriptor.");
    return (status);
    }

    /*
     ** Process the result row for this query.
     */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "setDarStatistics: Dar Statistics table insert failed.");
    *retStatus = status;
    status = IMS_ERROR;
    return (status);
    }

    return (IMS_OK);
} /* setDarStatistics */


/***************************************************************
**
**  subr ims_incrVersion ()
**
**  Increments and returns the assigned IMS version number for
**  the specified product name.
**  The generated version, for the specified name, is
**  returned using the (IMS_VERSION_STRUCT *) query->retPtr pointer
**  value.
**  Note: The maximum length for the product name is 20.
**
*****************************************************************/
int ims_incrVersion (
             IMS_CMN_QUERY *query,
             char          *name)
{
  int                  status;
  IMS_MSG_STRUCT      *msgDesc;
  IMS_QI_DESC_OBJ     *qDesc;
  IMS_VERSION_STRUCT  *retDef;
  int                  version;
  int                  name_length;
  static char       cmdBuf[IMS_COL512_LEN];


  /*
  **  Initialize variables.
  */
  status = IMS_OK;
  query->retStatus = IMS_OK;
  msgDesc = query->msgDesc;
  qDesc = query->qDesc;
  retDef = (IMS_VERSION_STRUCT *) query->retPtr;
  (void) memset ((void *) retDef, 0, sizeof (IMS_VERSION_STRUCT));

  /*
  **  Check for an active database connection.
  */
  if ((status = checkConnection (query)) < IMS_OK )
  {
      (void) ims_msg (msgDesc, status,
              "The database server connection was not valid.");
      query->retStatus = IMS_NOCONNECT;
      return (status);
  }

  /*
  **  Assign the comand buffer to the query descriptor.
  */
  IMS_SETCMD (qDesc, cmdBuf);

  /*
  ** Verify the product name.
  */
  if (!(name && *name))
  {
      (void) ims_msg (msgDesc, IMS_ERROR,
      "ims_incrVersion: Product name can not be null.");
      status = IMS_ERROR;
      return (status);
  }

  name_length = strlen (name);
  if (name_length > 20)
  {
      (void) ims_msg (msgDesc, IMS_ERROR,
          "ims_incrVersion: Product name '%s' is greater than the "
          "maximum length.", name);
      status = IMS_ERROR;
      return (status);
  }

  /*
  ** Begin the update transaction.
  */
  if ((status = setTransState (msgDesc, qDesc, "begin")) < IMS_OK)
  {
    (void) ims_msg (msgDesc, status,
            "Could not begin the update transaction.");
    query->retStatus = IMS_QUERYFAIL;
    (void) ims_qiResetDesc (qDesc);
    return (status);
  }

  /*
  ** Lock the version table until update is complete.
  */
  if ((status = getGeneralLock (msgDesc, qDesc)) < IMS_OK)
  {
        (void) ims_msg (msgDesc, status, "Could not get general_lock.");
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    return (status);
  }

  /*
  ** Increment product version.
  */
  if ((status = incrProductVersion (msgDesc, qDesc,
                name, retDef, &(query->retStatus))) < IMS_OK)
  {
        (void) ims_msg (msgDesc, status,
      "ims_incrVersion: Could not increment version of product '%s'.",
      name);
    (void) setTransState (msgDesc, qDesc, "rollback");
    (void) ims_qiResetDesc (qDesc);
    return (status);
  }

  /*
  ** Commit the update transaction.
  */
  if ((status = setTransState (msgDesc, qDesc, "commit")) < IMS_OK)
  {
    (void) ims_msg (msgDesc, status,
        "Could not commit the update transaction.");
    query->retStatus = IMS_QUERYFAIL;
  }

  (void) ims_qiResetDesc (qDesc);

  return (status);
}   /* ims_incrVersion */


/********************************************************************
**
** subr incrProductVersion ()
**
** This function will increment the version of the specified product,
** and return the new version.
**
**********************************************************************/
static int incrProductVersion (
                   IMS_MSG_STRUCT      *msgDesc,
                   IMS_QI_DESC_OBJ     *qDesc,
                   char                *name,
                   IMS_VERSION_STRUCT  *retDef,
                   int                 *retStatus)
{
    int               status;
    static char       cmdBuf[IMS_COL512_LEN+1];
    short             temp_version;


    /*
    ** Initialize local variables.
    */
    status = IMS_OK;
    *retStatus = IMS_OK;

    /*
    ** incr_product_version will increment the product version number,
    ** starting with version 1.
    */
    (void) sprintf (cmdBuf, "incr_product_version '%s'", name);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "Could not reset the query descriptor.");
    return (IMS_ERROR);
    }

    /*
     ** Execute the stored procedure.
     */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
    *retStatus = status;
    return (IMS_ERROR);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "Could not obtain version information for product '%s'.",
        name);
    *retStatus = IMS_NOROWS;
    return (IMS_ERROR);
    }

    if (IMS_AFFECTED (qDesc) > 1)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "More than one row of version information returned for "
        "product '%s'.", name);
    return (IMS_ERROR);
    }

    /*
    ** Copy the returned version data into the return structure.
    */
    temp_version = 0;
    (void) memcpy ((char *) &temp_version, qDesc->valAddr[0],
        qDesc->valLength[0]);
    retDef->version = temp_version;

    /*
    ** Copy the product name into the return structure.
    */
    retDef->name = name;

    return (IMS_OK);
}  /*  incrProductVersion  */


/********************************************************************
**
** subr validateOrderItem ()
**
** This function will validate order_id and item_id values by
** verifying the existence of an order_id table entry with
** the given order_id and item_id values.
**
**********************************************************************/
static int validateOrderItem (
                  IMS_MSG_STRUCT      *msgDesc,
                  IMS_QI_DESC_OBJ     *qDesc,
                  int                  order_id,
                  short                item_id,
                  int                 *retStatus)
{
    int               status;
    static char       cmdBuf[IMS_COL512_LEN+1];


    /*
    ** Initialize local variables.
    */
    status = IMS_OK;
    *retStatus = IMS_OK;

    /*
    ** validate_order will verify the existence of an order_item entry.
    */
    (void) sprintf (cmdBuf, "validate_order %d, %d", order_id, item_id);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
    (void) ims_msg (msgDesc, IMS_ERROR,
        "Could not reset the query descriptor.");
    return (IMS_ERROR);
    }

    /*
     ** Execute the stored procedure.
     */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
    *retStatus = status;
    return (IMS_ERROR);
    }

    return (IMS_OK);
}  /*  validateOrderItem  */


/*******************************************************************
**
** subr getGeneralLock ()
**
** Execute stored procedure get_general_lock
**
******************************************************************** */
static int getGeneralLock (
    IMS_MSG_STRUCT      *msgDesc,
    IMS_QI_DESC_OBJ     *qDesc)
{
    /*
    ** Execute stored procedure getGeneralLock
    */
    (void) sprintf (qDesc->cmd, "get_general_lock");

    if (execCmd (msgDesc, qDesc) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "execution of stored procedure get_general_lock failed.");
        return (IMS_FATAL);
    }

    return (IMS_OK);
}   /*  getGeneralLock   */


/********************************************************************
**
**  subr ims_darFrameStatus
**
**  This function takes order_id and frame_id, and obtains information
**      about all the frames that are valid for them.  this is returned
**      in the linked structure ims_FrameResults.
**  The tricky thing here is to obtain the dataset_idx for the given
**      order_id, item_id.  This is done by getting the platform and
**      mode from the dar table, and then building the frame dataset
**      name.  note that for all except RADARSAT, the result is SAR.
**
**
**********************************************************************/
int ims_darFrameStatus( IMS_CMN_QUERY * query , int order_id,
    short item_id, long * frameCount ){

    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    static  pnt_ims_FrameResults_t  pnt_f_results;
    IMS_NUMERIC_DATE dateStruct;
    char dataset[IMS_COL80_LEN+1];
    char granule_name[IMS_COL30_LEN+1];
    char platform[16];
    char mode[6];
    int status;
    long  i;


    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Get the dar table data: mode and platform
    */
    if( (status = get_dar_info (msgDesc, qDesc, order_id, item_id,
        platform, mode ) )  <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "No dar table entry for order_id %ld and item_id %d.",
            order_id, item_id );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = IMS_NOROWS;
        return (status);
    }

    /*
    ** Compose dataset name.
    */
    dataset[0] = '\0';
    for (i = 0; i < IMS_MAXPLATFORMMODES; i++)
    {
        if (strcmp (mode, IMS_PLATFORM_MODES[i].mode) == 0)
        {
            (void) strcpy (dataset, platform);
            (void) strcat (dataset, IMS_PLATFORM_MODES[i].dataset);
            (void) strcat (dataset, " FRAMES");
            break;
        }
    }
    if(  dataset[0]  ==  '\0' ){
        if(  strcmp( platform, "RADARSAT-1" )  ==  0  ){
            /*
            **  could not find the mode
            */
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not match mode %s for order_id %ld and "
                "item_id %d.", mode, order_id, item_id );
            (void) ims_qiResetDesc (qDesc);
            return ( IMS_ERROR );
        }
        else{ /* assume SAR for all other platforms  */
            (void) strcpy (dataset, platform);
            (void) strcat (dataset, " SAR FRAMES");
        }
    }
    /*
    **  now get the granule table name
    */
    if( (status = get_gran_name (msgDesc, qDesc, platform, dataset,
        granule_name ) )  <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule name for platform %s, dataset '%s'.",
            platform, dataset );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = status;
        return (status);
    }

    /*
    **  now that we have the granule table, we need to go to the
    **      dar_frame table to get all the frames for that item.
    **      use the FRAME_ID in that table to get the granule.
    */
    if( (status = get_gran_data (msgDesc, qDesc, granule_name,
        order_id, item_id, &pnt_f_results, frameCount ) )  <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule data for granule %s with order_id "
            "%ld, item_id %d.", granule_name, order_id, item_id );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = status;
        return (status);
    }

    query->retPtr = (char *) pnt_f_results;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);
    return (IMS_OK);
} /* ims_darFrameStatus */


/*******************************************************************
**
**  subr get_dar_info ()
**
**  This function will retrieve the platform and mode from the dar
**      table given the order_id and item_id.
**
******************************************************************** */
static int get_dar_info(
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    int              order_id,
    short            item_id,
    char *           platform,
    char *           mode )
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];
    short          temp_status;

    (void) sprintf (cmdBuf,
        "select platform, mode  from dar where  order_id = %ld "
        "  and  item_id = %d", order_id, item_id );

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Execute order_item table query.
    ** This query will return a single row.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Dar table query failed.");
        status = IMS_ERROR;
        return (status);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
        status = IMS_ERROR;
        return (status);
    }

    /*
    ** Copy the returned data into the output variable.
    */
    (void) memcpy ((char *) platform,
               qDesc->valAddr[0], qDesc->valLength[0]);
    platform[qDesc->valLength[0]] = '\0';
    ims_truncStr ( platform );

    (void) memcpy ((char *) mode,
               qDesc->valAddr[1], qDesc->valLength[1]);
    mode[qDesc->valLength[1]] = '\0';
    ims_truncStr ( mode );

    return (IMS_OK);
} /* get_dar_info */


/*******************************************************************
**
**  subr get_gran_name ()
**
**  This function will retrieve the granule table name given the
**      platform and mode.
**
******************************************************************** */
static int get_gran_name(
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *           platform,
    char *           dataset,
    char *           granule_name )
{
    int            status;
    static char    cmdBuf[IMS_COL512_LEN];
    short          temp_status;

    (void) sprintf (cmdBuf,
        "select p.granules_table from dataset_policy p, "
        "dataset_relation d  where "
        "  d.platform = '%s'  and "
        "  d.dataset = '%s'  and "
        "  d.dataset_idx = p.dataset_idx", platform, dataset );

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    ** Execute order_item table query.
    ** This query will return a single row.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Dataset_policy table query failed.");
        status = IMS_ERROR;
        return (status);
    }

    if (IMS_AFFECTED (qDesc) < 1)
    {
        status = IMS_ERROR;
        return (status);
    }

    /*
    ** Copy the returned data into the output variable.
    */
    (void) memcpy ((char *) granule_name,
               qDesc->valAddr[0], qDesc->valLength[0]);
    granule_name[qDesc->valLength[0]] = '\0';
    ims_truncStr ( granule_name );

    return (IMS_OK);
} /* get_gran_name */


/***************************************************************
**
**  subr get_gran_data ()
**
**  This routine gets the granule data for all the frames for the
**      dar.  the FRAME_ID is used to link from the dar_frame to
**      the granule.
**
**************************************************************** */
static  int get_gran_data (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *           granule_name,
    int              order_id,
    short            item_id,
    pnt_ims_FrameResults_t * pnt_f_results_in,
    long *          frameCount )
{
    int status;
    int rowCount;
    static char    cmdBuf[2048];
    int granuleIdx;
    pnt_ims_FrameResults_t  pnt_f_results,pnt_f_results2;
    long  index;


    /*
    ** set up the select
    */
    (void) sprintf (cmdBuf, "select  "
        " g.START_TIME, g.FRAME_STATUS, g.MEDIA_ID, g.REVOLUTION, "
        " g.SEQUENCE, g.MODE, g.PLATFORM, g.FRAME_ID, g.SENSOR, "
        " g.ACTIVITY_ID, g.ASC_DESC, g.END_TIME, g.CENTER_TIME, "
        " g.CENTER_LAT, g.CENTER_LON, g.NEAR_START_LAT, "
        " g.NEAR_START_LON, g.NEAR_END_LAT, g.NEAR_END_LON, "
        " g.FAR_START_LAT, g.FAR_START_LON, g.FAR_END_LAT, "
        " g.FAR_END_LON, g.FRAME_MODE, g.STATION_ID, "
        " g.SCAN_RESULTS_FILE  "
        "  from %s g, dar_frame d where  "
        "  d.order_id = %ld   and  "
        "  d.item_id = %d   and  "
        "  d.FRAME_ID = g.FRAME_ID", granule_name, order_id, item_id );


    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    rowCount = 0;
    pnt_f_results = NULL;

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
        ** Grab data
        */

        /*
        **  need to allocate another result structure
        */
        pnt_f_results2 = (pnt_ims_FrameResults_t) malloc(
            sizeof( ims_FrameResults_t ) );
        if(  pnt_f_results2  ==  NULL  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_gran_data cannot malloc results structure: "
                "rowCount = %ld." , rowCount );
            return( IMS_ERROR );
        }
        if(  rowCount  !=  1  )  pnt_f_results->next = pnt_f_results2;
        else  *pnt_f_results_in = pnt_f_results2;
        pnt_f_results = pnt_f_results2;
        pnt_f_results->next = NULL;

        index = 0;
        (void) memcpy((char *) pnt_f_results->start_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->start_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->start_time );

        index++;
        (void) memcpy((char *) pnt_f_results->frame_status,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->frame_status[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->frame_status );

        index++;
        /*  this is optional: check  */
        if(  qDesc->valLength[index]  ==  0  )
            pnt_f_results->media_id[0] = '\0';
        else{
            (void) memcpy((char *) pnt_f_results->media_id,
                qDesc->valAddr[index], qDesc->valLength[index]);
            pnt_f_results->media_id[qDesc->valLength[index]] = '\0';
            ims_truncStr ( pnt_f_results->media_id );
        }

        index++;
        (void) memcpy((char *) &(pnt_f_results->revolution),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->sequence),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->mode,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->mode[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->mode );

        index++;
        (void) memcpy((char *) pnt_f_results->platform,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->platform[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->platform );

        index++;
        (void) memcpy((char *) &(pnt_f_results->frame_id),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->sensor,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->sensor[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->sensor );

        index++;
        (void) memcpy((char *) pnt_f_results->activity_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->activity_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->activity_id );

        index++;
        (void) memcpy((char *) pnt_f_results->asc_desc,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->asc_desc[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->asc_desc );

        index++;
        (void) memcpy((char *) pnt_f_results->end_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->end_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->end_time );

        index++;
        (void) memcpy((char *) pnt_f_results->center_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->center_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->center_time );

        index++;
        (void) memcpy((char *) &(pnt_f_results->center_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->center_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_start_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_start_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_end_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_end_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_start_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_start_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_end_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_end_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->frame_mode,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->frame_mode[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->center_time );

        index++;
        (void) memcpy((char *) pnt_f_results->station_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->station_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->station_id );

        index++;
        /*  this is optional: check  */
        if(  qDesc->valLength[index]  ==  0  )
            pnt_f_results->scan_results_file[0] = '\0';
        else{
            (void) memcpy((char *) pnt_f_results->scan_results_file,
                qDesc->valAddr[index], qDesc->valLength[index]);
            pnt_f_results->scan_results_file[qDesc->valLength[index]] =
                '\0';
            ims_truncStr ( pnt_f_results->scan_results_file );
        }
    }

    /*
    ** Check the return status
    */

    *frameCount = rowCount;
    if (checkRetStatus (msgDesc, qDesc ) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "No frames returned (error) for order_id %ld, item_id %d "
            "and table %s.", order_id, item_id, granule_name );
        return ( IMS_ERROR );
    }
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "No frames returned for order_id %ld, item_id %d and "
            "table %s.", order_id, item_id, granule_name );
        return ( IMS_ERROR );
    }
    return (IMS_OK);
}   /*  get_gran_data  */


/********************************************************************
**
**  subr ims_spatialQuery
**
**  This function takes north_lat, south_, east_, west_ and a start
**      and end time, and obtains all the frames that are in the
**      area (even one point or line) and in the time span.
**  The tricky thing here is to obtain the dataset_idx for the given
**      platform and mode, which are also input.
**
**
**********************************************************************/
int ims_spatialQuery( IMS_CMN_QUERY * query , char * platform,
    char * mode, char * start_time, char * end_time, float north_lat,
    float  south_lat, float west_lon, float  east_lon,
    char * frame_status, long * frameCount )
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    static pnt_ims_FrameResults_t  pnt_f_results;
    IMS_NUMERIC_DATE dateStruct;
    char dataset[IMS_COL80_LEN+1];
    char granule_name[IMS_COL30_LEN+1];
    int status;
    long  i;


    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Check for valid lat,lons
    */
    if(  abs( west_lon )  >  180.0   ||   abs( east_lon )
        >  180.0 ){
        status = IMS_ERROR;
        (void) ims_msg (msgDesc, status,
            "Longitude values > 180.0 are not valid.");
        return (status);
    }
    if(  abs( north_lat )  >  90.0   ||   abs( south_lat )
        >  90.0 ){
        status = IMS_ERROR;
        (void) ims_msg (msgDesc, status,
            "Latitude values > 90.0 are not valid.");
        return (status);
    }
    if(  north_lat <  south_lat ){
        status = IMS_ERROR;
        (void) ims_msg (msgDesc, status,
            "North latitude must be greater than or equal to "
            "south latitude." );
        return (status);
    }
    /*
    ** Compose dataset name.
    */
    dataset[0] = '\0';
    for (i = 0; i < IMS_MAXPLATFORMMODES; i++)
    {
        if (strcmp (mode, IMS_PLATFORM_MODES[i].mode) == 0)
        {
            (void) strcpy (dataset, platform);
            (void) strcat (dataset, IMS_PLATFORM_MODES[i].dataset);
            (void) strcat (dataset, " FRAMES");
            break;
        }
    }
    if(  dataset[0]  ==  '\0' ){
        if(  strcmp( platform, "RADARSAT-1" )  ==  0  ){
            /*
            **  could not find the mode
            */
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not match mode %s for platform %s.",
                mode, platform );
            (void) ims_qiResetDesc (qDesc);
            return ( IMS_ERROR );
        }
        else{ /* assume SAR for all other platforms  */
            (void) strcpy (dataset, platform);
            (void) strcat (dataset, " SAR FRAMES");
        }
    }
    /*
    **  now get the granule table name
    */
    if( (status = get_gran_name (msgDesc, qDesc, platform, dataset,
        granule_name ) )  <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule name for platform %s, dataset '%s'.",
            platform, dataset );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = status;
        return (status);
    }

    /*
    **  now that we have the granule table, we need to go to the
    **      granules table and do the select.
    */
    if( (status = get_sp_gran_data (msgDesc, qDesc,
        granule_name, start_time, end_time, north_lat, south_lat,
        west_lon, east_lon, frame_status, &pnt_f_results, frameCount ) )
        <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule data for granule %s with dataset"
            " '%s'.",granule_name, dataset );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = status;
        return (status);
    }

    query->retPtr = (char *) pnt_f_results;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    return (IMS_OK);
} /* ims_spatialQuery */


/***************************************************************
**
**  subr get_sp_gran_data ()
**
**  This routine gets the granule data for all the frames for the
**      dar.  the FRAME_ID is used to link from the dar_frame to
**      the granule.
**
**************************************************************** */
static  int get_sp_gran_data (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *           granule_name,
    char *           start_time,
    char *           end_time,
    float            north_lat,
    float            south_lat,
    float            west_lon,
    float            east_lon,
    char *           frame_status,
    pnt_ims_FrameResults_t  * pnt_f_results_in,
    long *           frameCount )
{
    int status;
    int rowCount;
    static char    cmdBuf[2048];
    char  str2[129];
    int granuleIdx;
    pnt_ims_FrameResults_t  pnt_f_results,pnt_f_results2;
    IMS_NUMERIC_DATE dateStruct,dateStruct2;
    long  index;
    char  db_start_time[30];
    char  db_end_time[30];
    long  i;


    /*
    **  convert the time to sybase time
    */
    if ((status = ims_timeToNumericDate (msgDesc, start_time,
        &dateStruct)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
        "Start time '%s' is invalid.", start_time);
        return (status);
    }

    if ((status = ims_timeToNumericDate (msgDesc, end_time,
        &dateStruct2)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
        "End time '%s' is invalid.", end_time);
        return (status);
    }

    (void) ims_numericDateToIMSA (&dateStruct, db_start_time );
    (void) ims_numericDateToIMSA (&dateStruct2, db_end_time );

    /*
    **  if the frame status is not input, then do not test on
    **      it: may be null or no characters.
    */
    str2[0] = '\0';
    if( frame_status  !=  NULL  ){
        i = strlen( frame_status );
        if(  i  >  0  ){
            /*
            **  put the test in the select statement
            */
            sprintf( str2, "  g.FRAME_STATUS = '%s'  and  ",
                frame_status );
        }
    }

    /*
    **  set up the select.  two cases:  where input lon values
    **      do and do not go over -180.  the case where they do
    **      not, I call contiguous.
    */
    if(  west_lon  <=  east_lon ){
        /*
        ** input lons are contiguous.  need to test the two cases
        **      where the granule lons are and are not contiguous.
        */
        (void) sprintf (cmdBuf, "select  "
            " g.START_TIME, g.FRAME_STATUS, g.MEDIA_ID, g.REVOLUTION, "
            " g.SEQUENCE, g.MODE, g.PLATFORM, g.FRAME_ID, g.SENSOR, "
            " g.ACTIVITY_ID, g.ASC_DESC, g.END_TIME, g.CENTER_TIME, "
            " g.CENTER_LAT, g.CENTER_LON, g.NEAR_START_LAT, "
            " g.NEAR_START_LON, g.NEAR_END_LAT, g.NEAR_END_LON, "
            " g.FAR_START_LAT, g.FAR_START_LON, g.FAR_END_LAT, "
            " g.FAR_END_LON, g.FRAME_MODE, g.STATION_ID, "
            " g.SCAN_RESULTS_FILE  "
            "  from %s g where  %s "
            "  g.CENTER_TIME  >=  '%s'  and "
            "  g.CENTER_TIME  <=  '%s'  and "
            "  (  ( g.west_lon  <=  g.east_lon  and "
            "   %f  <=  g.east_lon  and  %f >=  g.west_lon )  or "
            "   ( g.west_lon  >  g.east_lon  and "
            "   ( %f  <=  g.east_lon  or  %f  >=  g.west_lon ) ) ) and "
            "   ( %f  >=  g.south_lat   and "
            "   %f  <=  g.north_lat )",
            granule_name, str2, db_start_time, db_end_time, west_lon,
            east_lon, west_lon, east_lon, north_lat, south_lat );
    }
    else{
        /*
        ** input lons are not contiguous.  need to test the two cases
        **      where the granule lons are and are not contiguous.
        */
        (void) sprintf (cmdBuf, "select  "
            " g.START_TIME, g.FRAME_STATUS, g.MEDIA_ID, g.REVOLUTION, "
            " g.SEQUENCE, g.MODE, g.PLATFORM, g.FRAME_ID, g.SENSOR, "
            " g.ACTIVITY_ID, g.ASC_DESC, g.END_TIME, g.CENTER_TIME, "
            " g.CENTER_LAT, g.CENTER_LON, g.NEAR_START_LAT, "
            " g.NEAR_START_LON, g.NEAR_END_LAT, g.NEAR_END_LON, "
            " g.FAR_START_LAT, g.FAR_START_LON, g.FAR_END_LAT, "
            " g.FAR_END_LON, g.FRAME_MODE, g.STATION_ID, "
            " g.SCAN_RESULTS_FILE  "
            "  from %s g where  %s "
            "  g.CENTER_TIME  >=  '%s'  and "
            "  g.CENTER_TIME  <=  '%s'  and "
            "  (  ( g.west_lon  <=  g.east_lon  and "
            "   ( %f  <=  g.east_lon  or  %f >=  g.west_lon ) )  or "
            "   ( g.west_lon  >  g.east_lon  ) )  and "
            "   ( %f  >=  g.south_lat   and "
            "   %f  <=  g.north_lat )",
            granule_name, str2, db_start_time, db_end_time, west_lon,
            east_lon, west_lon, east_lon, north_lat, south_lat );
    }

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    rowCount = 0;
    pnt_f_results = NULL;

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
        ** Grab data
        */

        /*
        **  need to allocate result structure
        */
        pnt_f_results2 = (pnt_ims_FrameResults_t) malloc(
            sizeof( ims_FrameResults_t ) );
        if(  pnt_f_results2  ==  NULL  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_sp_gran_data cannot malloc results structure: "
                "rowCount = %ld." , rowCount );
            return( IMS_ERROR );
        }
        if(  rowCount  !=  1  )  pnt_f_results->next = pnt_f_results2;
        else  *pnt_f_results_in = pnt_f_results2;
        pnt_f_results = pnt_f_results2;
        pnt_f_results->next = NULL;

        index = 0;
        (void) memcpy((char *) pnt_f_results->start_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->start_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->start_time );

        index++;
        (void) memcpy((char *) pnt_f_results->frame_status,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->frame_status[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->frame_status );

        index++;
        /*  this is optional: check  */
        if(  qDesc->valLength[index]  ==  0  )
            pnt_f_results->media_id[0] = '\0';
        else{
            (void) memcpy((char *) pnt_f_results->media_id,
                qDesc->valAddr[index], qDesc->valLength[index]);
            pnt_f_results->media_id[qDesc->valLength[index]] = '\0';
            ims_truncStr ( pnt_f_results->media_id );
        }

        index++;
        (void) memcpy((char *) &(pnt_f_results->revolution),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->sequence),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->mode,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->mode[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->mode );

        index++;
        (void) memcpy((char *) pnt_f_results->platform,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->platform[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->platform );

        index++;
        (void) memcpy((char *) &(pnt_f_results->frame_id),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->sensor,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->sensor[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->sensor );

        index++;
        (void) memcpy((char *) pnt_f_results->activity_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->activity_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->activity_id );

        index++;
        (void) memcpy((char *) pnt_f_results->asc_desc,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->asc_desc[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->asc_desc );

        index++;
        (void) memcpy((char *) pnt_f_results->end_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->end_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->end_time );

        index++;
        (void) memcpy((char *) pnt_f_results->center_time,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->center_time[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->center_time );

        index++;
        (void) memcpy((char *) &(pnt_f_results->center_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->center_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_start_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_start_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_end_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->near_end_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_start_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_start_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_end_lat),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_f_results->far_end_lon),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_f_results->frame_mode,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->frame_mode[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->frame_mode );

        index++;
        (void) memcpy((char *) pnt_f_results->station_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_f_results->station_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_f_results->station_id );

        index++;
        /*  this is optional: check  */
        if(  qDesc->valLength[index]  ==  0  )
            pnt_f_results->scan_results_file[0] = '\0';
        else{
            (void) memcpy((char *) pnt_f_results->scan_results_file,
                qDesc->valAddr[index], qDesc->valLength[index]);
            pnt_f_results->scan_results_file[qDesc->valLength[index]] =
                '\0';
            ims_truncStr ( pnt_f_results->scan_results_file );
        }
    }

    /*
    ** Check the return status
    */

    *frameCount = rowCount;
    if (checkRetStatus (msgDesc, qDesc ) < IMS_OK)
    {
        if(  str2[0]  ==  '\0'  )  (void) ims_msg( msgDesc, IMS_ERROR,
            "No frames returned (error) for spatial query on "
            "table %s.", granule_name );
        else     (void) ims_msg( msgDesc, IMS_ERROR,
            "No frames returned (error) for spatial query on "
            "table %s, frame_status %s.", granule_name, frame_status );
        return ( IMS_ERROR );
    }
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "No frames returned for spatial query on table %s.",
            granule_name );
        return ( IMS_ERROR );
    }
    return (IMS_OK);
}   /*  get_sp_gran_data  */


/********************************************************************
**
**  subr ims_orderQuery
**
**  This function takes the order_id and gets information on all
**      its items.  the results are returned in ims_OrderResults
**      structure, which is a linked list.
**
**********************************************************************/
int ims_orderQuery( IMS_CMN_QUERY * query , long  order_id,
    long * itemCount )
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    static pnt_ims_OrderResults_t  pnt_i_results;
    IMS_NUMERIC_DATE dateStruct;
    char dataset[IMS_COL80_LEN+1];
    char granule_name[IMS_COL30_LEN+1];
    int status;
    long  i;


    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    **  do the select on the items.
    */
    if( (status = get_item_list(msgDesc, qDesc,
        order_id, &pnt_i_results, itemCount ) )  <  IMS_OK )
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get item data for order_id %ld", order_id );
        (void) ims_qiResetDesc (qDesc);
        query->retStatus = status;
        return (status);
    }

    query->retPtr = (char *) pnt_i_results;

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    return (IMS_OK);
} /* ims_orderQuery */


/***************************************************************
**
**  subr get_item_list ()
**
**  This routine gets the item list for a given order_id.
**
**************************************************************** */
static  int get_item_list (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    long        order_id,
    pnt_ims_OrderResults_t  * pnt_i_results_in,
    long *           itemCount )
{
    int status;
    int rowCount;
    static char    cmdBuf[2048];
    int granuleIdx;
    pnt_ims_OrderResults_t  pnt_i_results,pnt_i_results2;
    long  index;
    short  i_s;

    /*
    **  set up the select.  note that the two status values
    **      use the items table to get the chararcter
    **      representation.
    */
    (void) sprintf (cmdBuf, "select  "
    " o.item_id, i.description, i2.description, o.priority,"
    " o.granule_idx, o.granule_name, o.platform, o.sensor, "
    " o.dataset, o.p_granule_name, o.p_granule_idx, o.p_data_kbytes,"
    " o.p_metadata_kbytes, o.media_id, o.quicklook_p, o.deleted_p,"
    " o.process_status, o.step_name, o.op_comment,"
    " o.process_comment   from  order_item o, items i, items i2  "
    "   where   o.order_id = %ld  and  "
    "   (o.status  =  i.instance  and  i.type  =  'order_status')"
    "   and  (o.order_item_type  =  i2.instance  and  i2.type  =  "
    " 'item_type')", order_id );


    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    rowCount = 0;
    pnt_i_results = NULL;

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
        ** Grab data
        */

        /*
        **  need to allocate result structure
        */
        pnt_i_results2 = (pnt_ims_OrderResults_t) malloc(
            sizeof( ims_OrderResults_t ) );
        if(  pnt_i_results2  ==  NULL  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_item_list cannot malloc results structure: "
                "rowCount = %ld." , rowCount );
            return( IMS_ERROR );
        }
        if(  rowCount  !=  1  )  pnt_i_results->next = pnt_i_results2;
        else  *pnt_i_results_in = pnt_i_results2;
        pnt_i_results = pnt_i_results2;
        pnt_i_results->next = NULL;

        pnt_i_results->order_id = order_id;

        index = 0;
        (void) memcpy((char *) &(pnt_i_results->item_id),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_i_results->status_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->status_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->status_id );

        index++;
        (void) memcpy((char *) pnt_i_results->order_item_type_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->order_item_type_id[qDesc->valLength[index]] =
            '\0';
        ims_truncStr ( pnt_i_results->order_item_type_id );

        index++;
        (void) memcpy((char *) &(pnt_i_results->priority),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_i_results->granule_idx),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_i_results->granule_name,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->granule_name[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->granule_name );

        index++;
        (void) memcpy((char *) pnt_i_results->platform,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->platform[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->platform );

        index++;
        (void) memcpy((char *) pnt_i_results->sensor,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->sensor[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->sensor );

        index++;
        (void) memcpy((char *) pnt_i_results->dataset,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->dataset[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->dataset );

        index++;
        (void) memcpy((char *) pnt_i_results->p_granule_name,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->p_granule_name[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->p_granule_name );

        index++;
        (void) memcpy((char *) &(pnt_i_results->p_granule_idx),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_i_results->p_data_kbytes ),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_i_results->p_metadata_kbytes ),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) pnt_i_results->media_id,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->media_id[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->media_id );

        index++;
        (void) memcpy((char *) &(pnt_i_results->quicklook_p ),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &(pnt_i_results->deleted_p ),
            qDesc->valAddr[index], qDesc->valLength[index]);

        index++;
        (void) memcpy((char *) &i_s,
            qDesc->valAddr[index], qDesc->valLength[index]);
        /*
        **  need to convert to the char equivalent.  would like
        **  to query items table, but the value of 0 is not in
        **  the table, but is often a value (initialized to it).
        **  so, I am building it in.
        */
        if(        i_s  ==  0 )   (void) strcpy(
            pnt_i_results->process_status_id, "NOT SET" );
        else  if(  i_s  ==  1 )   (void) strcpy(
            pnt_i_results->process_status_id, "PENDING" );
        else  if(  i_s  ==  2 )   (void) strcpy(
            pnt_i_results->process_status_id, "READY" );
        else  if(  i_s  ==  3 )   (void) strcpy(
            pnt_i_results->process_status_id, "AVAILABLE" );
        else  if(  i_s  ==  4 )   (void) strcpy(
            pnt_i_results->process_status_id, "SUBMITTED" );
        else  if(  i_s  ==  5 )   (void) strcpy(
            pnt_i_results->process_status_id, "COMPLETED" );
        else  if(  i_s  ==  6 )   (void) strcpy(
            pnt_i_results->process_status_id, "CANCEL" );
        else{ /* should never be here  */
            (void) strcpy( pnt_i_results->process_status_id,
                "UNKNOWN VALUE" );
        }

        index++;
        (void) memcpy((char *) pnt_i_results->step_name,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->step_name[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->step_name );

        index++;
        (void) memcpy((char *) pnt_i_results->op_comment,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->op_comment[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->op_comment );

        index++;
        (void) memcpy((char *) pnt_i_results->process_comment,
            qDesc->valAddr[index], qDesc->valLength[index]);
        pnt_i_results->process_comment[qDesc->valLength[index]] = '\0';
        ims_truncStr ( pnt_i_results->process_comment );
    }

    /*
    ** Check the return status
    */

    *itemCount = rowCount;
    if (checkRetStatus (msgDesc, qDesc ) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "No items returned (error) for order query on "
            "order_id %ld.", order_id );
        return ( IMS_ERROR );
    }
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "No items returned for order query on order_id %ld.",
            order_id );
        return ( IMS_ERROR );
    }
    return (IMS_OK);
}   /*  get_item_list  */


/*******************************************************************
**
** subr ims_dlToDtkQuery ()
**
** Query the downlink_entry and datatake_entry tables given the
**  user's input.  if the sequence is -1, get all dl for the given
**  revolution.
**
******************************************************************** */
int ims_dlToDtkQuery (
    IMS_CMN_QUERY *query,
    char *  platform,
    char * sensor,
    int  revolution,
    int  sequence,
    int  *inputMatch )
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    pnt_ims_dlStruct  pnt_dl_1st;
    pnt_ims_dlStruct  pnt_dl;
    pnt_ims_dlStruct  pnt_dl_last;
    pnt_ims_dlStruct  pnt_dl_1st_old;
    static char cmdBuf[IMS_COL512_LEN];
    short  all_revs; /* if true, sequence was -1: all rev dls  */
    char  dl_platform[11];
    char  dl_sensor[5];
    int   dl_rev;
    int   dl_sequence;
    int   num_dls; /* no. of downlinks found  */
    int   num_dtks; /* no. of datatakes for dl  */
    int  i;


    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    query->retStatus = IMS_OK;

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) == IMS_OK)
    {
        /* Close this connection and free the query descriptor. */
        (void) ims_qiFreeDesc (query->qDesc);
        query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
    }

    /*
    ** Open the database server connection.
    */
    if ((qDesc = openConnection (query)) ==
        (IMS_QI_DESC_OBJ *) NULL)
    {
        status = ims_msgGetSeverity (msgDesc);
        query->retStatus = IMS_NOCONNECT;
        *inputMatch = IMS_NO_MATCH;
        return (status);
    }
    query->qDesc = qDesc;
    qDesc->msgDesc = msgDesc;

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    **  allocate the first dl structure
    */
    pnt_dl_1st = (IMS_DL_STRUCT *) malloc( sizeof( IMS_DL_STRUCT ) );
    query->retPtr = (char *) pnt_dl_1st;

    strcpy( pnt_dl_1st->platform, platform );
    strcpy( pnt_dl_1st->sensor, sensor );
    pnt_dl_1st->revolution = revolution;
    pnt_dl_1st->sequence = sequence;
    pnt_dl_1st->activity_id[0] = '\0';
    pnt_dl_1st->station_id[0] = '\0';
    pnt_dl_1st->antenna_id[0] = '\0';
    pnt_dl_1st->transmitter_id[0] = '\0';
    pnt_dl_1st->fa_schedule_link[0] = '\0';
    pnt_dl_1st->time_on[0] = '\0';
    pnt_dl_1st->time_off[0] = '\0';
    pnt_dl_1st->time_aos[0] = '\0';
    pnt_dl_1st->time_los[0] = '\0';
    pnt_dl_1st->downlink_status[0] = '\0';
    pnt_dl_1st->number_of_dtk_entry = -1;
    pnt_dl_1st->datatakePtr = NULL;
    pnt_dl_1st->downlinkPtr = NULL;

    if(  sequence  ==  -1  )  all_revs = IMS_TRUE;
    else  all_revs = IMS_FALSE;

    status = read_dl( msgDesc, query->qDesc, pnt_dl_1st, pnt_dl_1st,
        &num_dls );
    if(  status  <  IMS_OK ){
        *inputMatch = IMS_NO_MATCH;
        free( pnt_dl_1st );
        query->retPtr = NULL;
        (void) ims_qiFreeDesc ( query->qDesc);
        query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
        return( status );
    }
    *inputMatch = IMS_DL_MATCH;

    if(  num_dls  ==  0  ){
        /*
        **  this may be for a datatake value.  search for that
        **      case.
        */
        status = read_dtk_check( msgDesc, query->qDesc, pnt_dl_1st );
        if(  status  <  IMS_OK ){
            *inputMatch = IMS_NO_MATCH;
            free( pnt_dl_1st );
            query->retPtr = NULL;
            (void) ims_qiFreeDesc ( query->qDesc);
            query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
            return( status );
        }

        if(  pnt_dl_1st->revolution  ==  -2  ){
            /*
            **  no match found
            */
            *inputMatch = IMS_NO_MATCH;
            free( pnt_dl_1st );
            query->retPtr = NULL;
            (void) ims_qiFreeDesc ( query->qDesc);
            query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
            return( IMS_OK );
        }

        *inputMatch = IMS_DTK_MATCH;
        /*
        **  have a problem: may be more than one downlink returned.
        **      cannot keep the old 1st pointer: get new one.
        */
        pnt_dl_1st_old = pnt_dl_1st;
        pnt_dl_1st = (IMS_DL_STRUCT *) malloc( sizeof( IMS_DL_STRUCT ));
        query->retPtr = (char *) pnt_dl_1st;

        strcpy( pnt_dl_1st->platform, pnt_dl_1st_old->platform );
        strcpy( pnt_dl_1st->sensor, pnt_dl_1st_old->sensor );
        pnt_dl_1st->revolution = pnt_dl_1st_old->revolution;
        pnt_dl_1st->sequence = pnt_dl_1st_old->sequence;
        pnt_dl_1st->activity_id[0] = '\0';
        pnt_dl_1st->station_id[0] = '\0';
        pnt_dl_1st->antenna_id[0] = '\0';
        pnt_dl_1st->transmitter_id[0] = '\0';
        pnt_dl_1st->fa_schedule_link[0] = '\0';
        pnt_dl_1st->time_on[0] = '\0';
        pnt_dl_1st->time_off[0] = '\0';
        pnt_dl_1st->time_aos[0] = '\0';
        pnt_dl_1st->time_los[0] = '\0';
        pnt_dl_1st->downlink_status[0] = '\0';
        pnt_dl_1st->number_of_dtk_entry = -1;
        pnt_dl_1st->datatakePtr = NULL;
        pnt_dl_1st->downlinkPtr = NULL;

        pnt_dl = pnt_dl_1st_old;
        while( pnt_dl  !=  NULL  ){
            /*
            **  try again with the new dl values
            */
            status = read_dl( msgDesc, query->qDesc, pnt_dl_1st,
                pnt_dl, &num_dls );
            if(  status  <  IMS_OK ){
                *inputMatch = IMS_NO_MATCH;
                free( pnt_dl_1st );
                query->retPtr = NULL;
                (void) ims_qiFreeDesc ( query->qDesc);
                query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
                return( status );
            }
            if(  num_dls  ==  0  ){
                /*
                **  the found datatake does not have an associated
                **      downlink: this is an error
                */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Could not find datatake's downlink: '%s', "
                    "'%s', %ld (rev), %ld (sequence).",
                    pnt_dl->platform, pnt_dl->sensor,
                    pnt_dl->revolution, pnt_dl->sequence );
                *inputMatch = IMS_NO_MATCH;
                free( pnt_dl_1st );
                query->retPtr = NULL;
                (void) ims_qiFreeDesc ( query->qDesc);
                query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
                return( IMS_ERROR );
            }
            pnt_dl_last = pnt_dl;
            pnt_dl = pnt_dl->downlinkPtr;
            free( pnt_dl_last );
        }
    }

    /*
    **  now get the associated dtk values
    */
    num_dtks = 0;
    num_dls = 0;
    pnt_dl = pnt_dl_1st;
    while(  pnt_dl  !=  NULL  ){
        status = read_dtk( msgDesc, query->qDesc, pnt_dl, &i );
        if(  status  <  IMS_OK  ){
            (void) ims_qiFreeDesc ( query->qDesc);
            query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
            return( status );
        }
        num_dtks +=  i;
        num_dls++;
        pnt_dl = pnt_dl->downlinkPtr;
    }

    (void)  ims_msg( msgDesc, IMS_INFO,
        "ims_dlToDtkQuery: found %ld downlinks, %ld datatakes.",
        num_dls, num_dtks );
    if(  num_dls  ==  0  )  *inputMatch = IMS_NO_MATCH;

    (void) ims_qiFreeDesc ( query->qDesc);
    query->qDesc = (IMS_QI_DESC_OBJ *) NULL;
    return (IMS_OK);
}  /* ims_dlToDtkQuery  */


/***************************************************************
**
**  subr read_dl
**
**  Read the downlink_entry table for all the dls: save into the
**      linked structure.
**
**************************************************************** */
static  int read_dl(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    pnt_ims_dlStruct  pnt_dl_1st,
    pnt_ims_dlStruct  pnt_dl_data,
    int  *  num_dls )
{
    int status;
    short  i_s;
    short  all_revs;
    int  j;
    pnt_ims_dlStruct  pnt_dl, pnt_dl_last;
    static  char cmdBuf[IMS_COL512_LEN];
    int  rowCount;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "read_dl: Could not reset the query descriptor.");
        return (status);
    }

    if(  pnt_dl_1st->sequence  ==  -1  )  all_revs = IMS_TRUE;
    else  all_revs = IMS_FALSE;

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    if(  all_revs  ){
        (void) sprintf (cmdBuf,
            "select SEQUENCE, ACTIVITY_ID, STATION_ID, ANTENNA_ID, "
            "TRANSMITTER_ID, FA_SCHEDULE_LINK, TIME_ON, TIME_OFF, "
            "TIME_AOS, TIME_LOS, DOWNLINK_STATUS, NUMBER_OF_DTK_ENTRY"
            "   from downlink_entry  where   PLATFORM  = '%s'  and  "
            "SENSOR = '%s'  and  REVOLUTION = %ld",
            pnt_dl_data->platform, pnt_dl_data->sensor,
            pnt_dl_data->revolution );
    }
    else{
        (void) sprintf (cmdBuf,
            "select SEQUENCE, ACTIVITY_ID, STATION_ID, ANTENNA_ID, "
            "TRANSMITTER_ID, FA_SCHEDULE_LINK, TIME_ON, TIME_OFF, "
            "TIME_AOS, TIME_LOS, DOWNLINK_STATUS, NUMBER_OF_DTK_ENTRY"
            "   from downlink_entry  where   PLATFORM  = '%s'  and  "
            "SENSOR = '%s'  and  REVOLUTION = %ld  and  SEQUENCE = %d",
            pnt_dl_data->platform, pnt_dl_data->sensor,
            pnt_dl_data->revolution, pnt_dl_data->sequence );
    }

    pnt_dl = pnt_dl_1st;
    /*
    **  need to get to the last dl in the linked list
    */
    while(  pnt_dl  !=  NULL  ){
        pnt_dl_last = pnt_dl;
        pnt_dl = pnt_dl->downlinkPtr;
    }
    pnt_dl = pnt_dl_last;
    /*
    **  check to see if this is a valid dl: if not, then values are
    **      not set, if valid, values are set.  check antenna_id, as
    **      it must be one of two values (cannot be void).
    */
    if(  pnt_dl->antenna_id[0]  !=  '\0'  ){
        /*
        **  do not over-write this dl: get new one.
        */
        pnt_dl_last = pnt_dl;
        pnt_dl = (IMS_DL_STRUCT *) malloc( sizeof( IMS_DL_STRUCT ));
        pnt_dl_last->downlinkPtr = pnt_dl;

        strcpy( pnt_dl->platform, pnt_dl_1st->platform );
        strcpy( pnt_dl->sensor, pnt_dl_1st->sensor );
        pnt_dl->revolution = pnt_dl_1st->revolution;
        pnt_dl->sequence = 0;
        pnt_dl->activity_id[0] = '\0';
        pnt_dl->station_id[0] = '\0';
        pnt_dl->antenna_id[0] = '\0';
        pnt_dl->transmitter_id[0] = '\0';
        pnt_dl->fa_schedule_link[0] = '\0';
        pnt_dl->time_on[0] = '\0';
        pnt_dl->time_off[0] = '\0';
        pnt_dl->time_aos[0] = '\0';
        pnt_dl->time_los[0] = '\0';
        pnt_dl->downlink_status[0] = '\0';
        pnt_dl->number_of_dtk_entry = -1;
        pnt_dl->datatakePtr = NULL;
        pnt_dl->downlinkPtr = NULL;
    }

    /*
    ** Execute the command.
    */
    rowCount = 0;

    while( (status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "read_dl: Could not read db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }
        rowCount++;

        if(  pnt_dl  ==  NULL  ){
            /*
            **  allocate the next dl structure
            */
            pnt_dl = (IMS_DL_STRUCT *) malloc( sizeof( IMS_DL_STRUCT ));
            pnt_dl_last->downlinkPtr = pnt_dl;

            strcpy( pnt_dl->platform, pnt_dl_1st->platform );
            strcpy( pnt_dl->sensor, pnt_dl_1st->sensor );
            pnt_dl->revolution = pnt_dl_1st->revolution;
            pnt_dl->sequence = 0;
            pnt_dl->activity_id[0] = '\0';
            pnt_dl->station_id[0] = '\0';
            pnt_dl->antenna_id[0] = '\0';
            pnt_dl->transmitter_id[0] = '\0';
            pnt_dl->fa_schedule_link[0] = '\0';
            pnt_dl->time_on[0] = '\0';
            pnt_dl->time_off[0] = '\0';
            pnt_dl->time_aos[0] = '\0';
            pnt_dl->time_los[0] = '\0';
            pnt_dl->downlink_status[0] = '\0';
            pnt_dl->number_of_dtk_entry = -1;
            pnt_dl->datatakePtr = NULL;
            pnt_dl->downlinkPtr = NULL;
        }

        /*
        ** Grab data
        */
        j = 0;
        (void) memcpy ( &i_s, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->sequence = i_s;

        j++;
        (void) memcpy ( pnt_dl->activity_id, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->activity_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->activity_id );

        j++;
        (void) memcpy ( pnt_dl->station_id, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->station_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->station_id );

        j++;
        (void) memcpy ( pnt_dl->antenna_id, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->antenna_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->antenna_id );

        j++;
        (void) memcpy ( pnt_dl->transmitter_id, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->transmitter_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->transmitter_id );

        j++;
        (void) memcpy ( pnt_dl->fa_schedule_link, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->fa_schedule_link[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->fa_schedule_link );

        j++;
        (void) memcpy ( pnt_dl->time_on, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->time_on[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->time_on );

        j++;
        (void) memcpy ( pnt_dl->time_off, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->time_off[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->time_off );

        j++;
        (void) memcpy ( pnt_dl->time_aos, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->time_aos[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->time_aos );

        j++;
        (void) memcpy ( pnt_dl->time_los, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->time_los[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->time_los );

        j++;
        (void) memcpy ( pnt_dl->downlink_status, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dl->downlink_status[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dl->downlink_status );

        j++;
        (void) memcpy ( &(pnt_dl->number_of_dtk_entry),
            qDesc->valAddr[j], qDesc->valLength[j]);

        pnt_dl_last = pnt_dl;
        pnt_dl = NULL;
    }

    /*
    **  set the number of dls
    */
    *num_dls = rowCount;
    return( IMS_OK );
}   /*  read_dl  */


/***************************************************************
**
**  subr read_dtk_check
**
**  Read the datatake_entry table for the dtk that matches the
**      dl values.  if none, dl_rev is set to -1.
**
**************************************************************** */
static  int read_dtk_check(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    pnt_ims_dlStruct  pnt_dl_1st )
{
    int status;
    short  i_s;
    int  j;
    int  rowCount;
    static  char cmdBuf[IMS_COL512_LEN];
    short  all_revs;
    pnt_ims_dlStruct  pnt_dl, pnt_dl_last;
    char  dl_platform[5];
    char  dl_sensor[4];
    int  dl_rev;
    int  dl_sequence;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "read_dtk_check: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    pnt_dl = pnt_dl_1st;
    if(  pnt_dl->sequence  ==  -1  )  all_revs = 1;
    else  all_revs = 0;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    if(  all_revs  ){
        (void) sprintf (cmdBuf,
            "select PLATFORM, SENSOR, REVOLUTION, SEQUENCE"
            "   from datatake_entry  where   DT_PLATFORM  = '%s'  and"
            "  DT_SENSOR = '%s'  and  DT_REVOLUTION = %ld  order "
            "by SEQUENCE",
            pnt_dl->platform, pnt_dl->sensor,
            pnt_dl->revolution );
    }
    else{
        (void) sprintf (cmdBuf,
            "select PLATFORM, SENSOR, REVOLUTION, SEQUENCE"
            "   from datatake_entry  where   DT_PLATFORM  = '%s'  and"
            "  DT_SENSOR = '%s'  and  DT_REVOLUTION = %ld  and  "
            "DT_SEQUENCE = %ld", pnt_dl->platform, pnt_dl->sensor,
            pnt_dl->revolution, pnt_dl->sequence );
    }

    /*
    ** Execute the command.
    */
    rowCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "read_dtk_check: Could not read db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }
        rowCount++;

        /*
        ** Grab data
        */
        j = 0;
        (void) memcpy ( dl_platform, qDesc->valAddr[j],
            qDesc->valLength[j]);
        dl_platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( dl_platform );

        j++;
        (void) memcpy ( dl_sensor, qDesc->valAddr[j],
            qDesc->valLength[j]);
        dl_sensor[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( dl_sensor );

        j++;
        (void) memcpy ( &dl_rev, qDesc->valAddr[j],
            qDesc->valLength[j]);

        j++;
        (void) memcpy ( &i_s, qDesc->valAddr[j],
            qDesc->valLength[j]);
        dl_sequence = i_s;

        if(  pnt_dl  ==  NULL  ){
            /*
            **  have more than one entry.  need to check if it is
            **      different than the last.  Note that they
            **      can be different only in the sequence.
            */
            if(  pnt_dl_last->sequence  !=  dl_sequence  ){
                /*
                **  allocate the next dl structure, as this is
                **      a different dl.
                */
                pnt_dl = (IMS_DL_STRUCT *) malloc( sizeof(
                    IMS_DL_STRUCT ));
                pnt_dl_last->downlinkPtr = pnt_dl;

                strcpy( pnt_dl->platform, dl_platform );
                strcpy( pnt_dl->sensor, dl_sensor );
                pnt_dl->revolution = dl_rev;
                pnt_dl->sequence = dl_sequence;
                pnt_dl->activity_id[0] = '\0';
                pnt_dl->station_id[0] = '\0';
                pnt_dl->antenna_id[0] = '\0';
                pnt_dl->transmitter_id[0] = '\0';
                pnt_dl->fa_schedule_link[0] = '\0';
                pnt_dl->time_on[0] = '\0';
                pnt_dl->time_off[0] = '\0';
                pnt_dl->time_aos[0] = '\0';
                pnt_dl->time_los[0] = '\0';
                pnt_dl->downlink_status[0] = '\0';
                pnt_dl->number_of_dtk_entry = -1;
                pnt_dl->datatakePtr = NULL;
                pnt_dl->downlinkPtr = NULL;
            }
            else  pnt_dl = pnt_dl_last;
        }
        else{
            /*
            **  first time: put in the values
            */
            strcpy( pnt_dl->platform, dl_platform );
            strcpy( pnt_dl->sensor, dl_sensor );
            pnt_dl->revolution = dl_rev;
            pnt_dl->sequence = dl_sequence;
        }

        pnt_dl_last = pnt_dl;
        pnt_dl = NULL;
    }

    /*
    **  if no rows, set rev to -2
    */
    if(  rowCount  ==  0  )  pnt_dl_1st->revolution = -2;
    return( IMS_OK );
}   /*  read_dtk_check  */


/***************************************************************
**
**  subr read_dtk
**
**  Read the datatake_entry table for the dtk that matches the
**      input dl values.
**
**************************************************************** */
static  int read_dtk(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    pnt_ims_dlStruct  pnt_dl,
    int  *  num_dtks )   /* the number of dtks found  */
{
    int status;
    short  i_s;
    int  j;
    pnt_ims_dtkStruct  pnt_dtk;
    pnt_ims_dtkStruct  pnt_dtk_last;
    static  char cmdBuf[IMS_COL512_LEN];
    int  rowCount;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "read_dtk_check: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select DT_PLATFORM, DT_SENSOR, DT_REVOLUTION, DT_SEQUENCE, "
        "QUICKLOOK_FLAG, PROCESS_AUTH_FLAG, MODE, FRAME_MODE, TIME_ON,"
        " TIME_OFF, SITE_NAME   from datatake_entry  where   PLATFORM  "
        "= '%s'  and  SENSOR = '%s'  and  REVOLUTION = %ld  and  "
        "SEQUENCE = %ld",
        pnt_dl->platform, pnt_dl->sensor,
        pnt_dl->revolution, pnt_dl->sequence );

    pnt_dtk = (IMS_DTK_STRUCT *) malloc( sizeof( IMS_DTK_STRUCT ) );

    pnt_dtk->platform[0] = '\0';
    pnt_dtk->sensor[0] = '\0';
    pnt_dtk->revolution = -1;
    pnt_dtk->sequence = -1;
    pnt_dtk->quicklook_flag[0] = '\0';
    pnt_dtk->process_auth_flag[0] = '\0';
    pnt_dtk->mode[0] = '\0';
    pnt_dtk->frame_mode[0] = '\0';
    pnt_dtk->time_on[0] = '\0';
    pnt_dtk->time_off[0] = '\0';
    pnt_dtk->site_name[0] = '\0';
    pnt_dtk->next = NULL;

    pnt_dl->datatakePtr = pnt_dtk;

    /*
    ** Execute the command.
    */
    rowCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "read_dtk: Could not read db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }
        rowCount++;


        /*
        **  if not first time, allocate the dtk structure
        */
        if(  pnt_dtk  ==  NULL  ){
            pnt_dtk = (IMS_DTK_STRUCT *) malloc( sizeof(
                IMS_DTK_STRUCT ) );

            pnt_dtk->platform[0] = '\0';
            pnt_dtk->sensor[0] = '\0';
            pnt_dtk->revolution = -1;
            pnt_dtk->sequence = -1;
            pnt_dtk->quicklook_flag[0] = '\0';
            pnt_dtk->process_auth_flag[0] = '\0';
            pnt_dtk->mode[0] = '\0';
            pnt_dtk->frame_mode[0] = '\0';
            pnt_dtk->time_on[0] = '\0';
            pnt_dtk->time_off[0] = '\0';
            pnt_dtk->site_name[0] = '\0';
            pnt_dtk->next = NULL;

            pnt_dtk_last->next = pnt_dtk;
        }

        /*
        ** Grab data
        */
        j = 0;
        (void) memcpy ( pnt_dtk->platform, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->platform );

        j++;
        (void) memcpy ( pnt_dtk->sensor, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->sensor[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->sensor );

        j++;
        (void) memcpy ( &(pnt_dtk->revolution), qDesc->valAddr[j],
            qDesc->valLength[j]);

        j++;
        (void) memcpy ( &i_s, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->sequence = i_s;

        j++;
        (void) memcpy ( pnt_dtk->quicklook_flag, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->quicklook_flag[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->quicklook_flag );

        j++;
        (void) memcpy ( pnt_dtk->process_auth_flag, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->process_auth_flag[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->process_auth_flag );

        j++;
        (void) memcpy ( pnt_dtk->mode, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->mode[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->mode );

        j++;
        (void) memcpy ( pnt_dtk->frame_mode, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->frame_mode[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->frame_mode );

        j++;
        (void) memcpy ( pnt_dtk->time_on, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->time_on[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->time_on );

        j++;
        (void) memcpy ( pnt_dtk->time_off, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->time_off[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->time_off );

        j++;
        (void) memcpy ( pnt_dtk->site_name, qDesc->valAddr[j],
            qDesc->valLength[j]);
        pnt_dtk->site_name[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( pnt_dtk->site_name );

        pnt_dtk_last = pnt_dtk;
        pnt_dtk = NULL;
    }

    /*
    **  set output no. of datatakes
    */
    if(  rowCount  ==  0  ){
        /*
        **  did not find one datatake
        */
        free( pnt_dtk_last );
        pnt_dl->datatakePtr = NULL;
    }
    *num_dtks = rowCount;
    return( IMS_OK );
}   /*  read_dtk  */


/*****************************************************************
**
**  subr getDatatakeInfo
**
**  Read the datatake_entry to get the platform, rev, and sequence 
**
**************************************************************** */
static int getDatatakeInfo(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *platform,
    int  rev,
    int  sequence,
    int  *retStatus,
    char *dl_platform, char *dt_platform,
    int  *dl_rev, int *dt_rev,
    short *dl_sequence, short *dt_sequence)
{
    int status, j;
    static  char cmdBuf[IMS_COL512_LEN];

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "getDatatakeInfo: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select PLATFORM, REVOLUTION, SEQUENCE,"
	"DT_PLATFORM, DT_REVOLUTION, DT_SEQUENCE "
        "from datatake_entry where (PLATFORM = '%s' and REVOLUTION = %d and "
        "SEQUENCE = %d) or (DT_PLATFORM = '%s' and DT_REVOLUTION = %d and "
	"DT_SEQUENCE = %d)",
	platform, rev, sequence, platform, rev, sequence);

#ifdef QDEBUG
    printf ("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
	    (void) ims_msg( msgDesc, IMS_ERROR,
				"getDatatakeInfo: Could not read db." );
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
                *retStatus = IMS_DEADLOCK;
            }
            else
            {
                *retStatus = IMS_QUERYFAIL;
            }
 
            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Grab data
        */
        j = 0;
        (void) memcpy (dl_platform, qDesc->valAddr[j],
            qDesc->valLength[j]);
        dl_platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr(dl_platform );

        j++;
        (void) memcpy ((char *)dl_rev, qDesc->valAddr[j],
            qDesc->valLength[j]);

        j++;
        (void) memcpy ((char *)dl_sequence , qDesc->valAddr[j],
            qDesc->valLength[j]);

	j++;
        (void) memcpy (dt_platform, qDesc->valAddr[j],
            qDesc->valLength[j]);
        dt_platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr(dt_platform );
 
        j++;
        (void) memcpy ((char *)dt_rev, qDesc->valAddr[j],
            qDesc->valLength[j]);
 
        j++;
        (void) memcpy ((char *)dt_sequence, qDesc->valAddr[j],
            qDesc->valLength[j]);

    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        IMS_SETROWCOUNT (qDesc, "0");
	*retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }
    (void) ims_qiResetDesc (qDesc);

    return( IMS_OK );

}   /*  getDatatakeInfo */

/*******************************************************************
**
** subr freeItemsList ()
**
** Free ITEMS_LIST structure.
**
******************************************************************** */
void freeItemsList (
              ITEMS_LIST *currPtr)
{
    ITEMS_LIST *nextPtr;

    while (currPtr != (ITEMS_LIST *) NULL)
    {
    nextPtr = currPtr->next;
    free (currPtr);
    currPtr = nextPtr;
    }

    return;
}   /*  freeItemsList  */


/*******************************************************************
**
** subr freeFrameResults()
**
** Free ims_FrameResults structure.  This is for the RGPS calls, among
**      others.  this is a linked list, so all must be freeded.  the
**      first one is sent here; it is also freed.
**
******************************************************************** */
void freeFrameResults(
    pnt_ims_FrameResults_t  pnt_f_results  )
{
    pnt_ims_FrameResults_t  pnt_curr, pnt_next;


    pnt_curr = pnt_f_results;
    while ( pnt_curr != (pnt_ims_FrameResults_t) NULL)
    {
        pnt_next = pnt_curr->next;
        free( pnt_curr );
        pnt_curr = pnt_next;
    }
    return;
}   /*  freeFrameResults   */


/*******************************************************************
**
** subr freeOrderResults()
**
** Free ims_OrderResults structure.  This is for the RGPS calls, among
**      others.  this is a linked list, so all must be freeded.  the
**      first one is sent here; it is also freed.
**
******************************************************************** */
void freeOrderResults(
    pnt_ims_OrderResults_t  pnt_i_results  )
{
    pnt_ims_OrderResults_t  pnt_curr, pnt_next;


    pnt_curr = pnt_i_results;
    while ( pnt_curr != (pnt_ims_OrderResults_t) NULL)
    {
        pnt_next = pnt_curr->next;
        free( pnt_curr );
        pnt_curr = pnt_next;
    }
    return;
}   /*  freeOrderResults   */


/*******************************************************************
**
** subr ims_dlToDtkFree ()
**
** Free IMS_DL_STRUCT structure.  This is for the dlToDtkQuery call.
**      this is a linked list, so all must be freeded.  the
**      first one is sent here; it is also freed.
**
******************************************************************** */
void ims_dlToDtkFree(  pnt_ims_dlStruct pnt_dl_1st )
{
    pnt_ims_dlStruct  pnt_dl, pnt_dl_next;
    pnt_ims_dtkStruct  pnt_dtk, pnt_dtk_next;

    pnt_dl = pnt_dl_1st;
    while ( pnt_dl != (pnt_ims_dlStruct) NULL)
    {
        pnt_dl_next = pnt_dl->downlinkPtr;
        pnt_dtk = pnt_dl->datatakePtr;
        free( pnt_dl );
        while( pnt_dtk  !=  (pnt_ims_dtkStruct) NULL ){
            pnt_dtk_next = pnt_dtk->next;
            free( pnt_dtk );
            pnt_dtk = pnt_dtk_next;
        }
        pnt_dl = pnt_dl_next;
    }
    return;
}   /*  ims_dlToDtkFree   */

/*******************************************************************
**
** subr ims_frameStatus ()
**
** Query the associated the FRAMES granules table
** and return frame status for the given frame order.
**
********************************************************************/
int ims_frameStatus (
    IMS_CMN_QUERY *query,
    int  order_id, 
    int  item_id)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    IMS_FRAME_SOURCE_MEDIA_STRUCT *retDef;
    char dataset[IMS_COL80_LEN+1];
    char frames_granules_table[IMS_COL30_LEN+1];
    char scan_results_table[IMS_COL30_LEN+1];
    char raw_signal_table[IMS_COL30_LEN+1];
    int i;
    int status;
    int granule_idx;

    /*
    ** Initialize variables.
    */
    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;


    /*
    ** Announce our intentions.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Attempting Frame Status query for order id %d, item id %d", 
	order_id, item_id);

    /*
    ** Check for an active database server connection.
    */
    if ((status = checkConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The database server connection was not valid.");
        query->retStatus = IMS_NOCONNECT;
        return (status);
    }

    /*
    ** Get the dataset name containing the frame for this order and its granule_idx 
    */
    if ((status = getOrderDataset (query, order_id, item_id, 
				   dataset, &granule_idx)) < IMS_OK)
    {
	(void) ims_msg (msgDesc, IMS_ERROR,
		"Could not get dataset name for order %d item %d",
		order_id, item_id);
	(void) ims_qiResetDesc (qDesc);
	return (status);
    }
    /*
    ** Get the FRAMES granule table name.
    */
    if ((status = getGranuleTable (msgDesc, qDesc, dataset,
        frames_granules_table, &(query->retStatus))) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get granule table name for dataset '%s'.",
            dataset);
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** Initialize all returned fields
    */
    retDef = (IMS_FRAME_SOURCE_MEDIA_STRUCT *) query->retPtr;
    retDef->frame_status[0] = '\0';
    retDef->start_time[0] = '\0';
    retDef->end_time[0] = '\0';
    retDef->center_time[0] = '\0';
    retDef->media_id[0] = '\0';
    retDef->media_location[0] = '\0';
    retDef->media_type[0] = '\0';
    retDef->data_direction[0] = '\0';
    retDef->station_id[0] = '\0';
    retDef->recorder_id[0] = '\0';
    retDef->scan_results_file[0] = '\0';
    retDef->platform[0] = '\0';
    
    /*
    ** Now query for the frame status information.
    */
    if ((status = getFrameStatus (query, frames_granules_table,
       				  granule_idx))  < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain frame status for %s granule_idx %d.",
	    dataset, granule_idx);
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    ** If the frame status is SCANNED, and scan results file is available,
    ** we go on to get the rest of source media info from the scan results table,
    ** and the raw signal segment table.
    */
    if (strcmp(retDef->frame_status,"SCANNED") == 0)
    {
	if ((retDef->media_id[0] != '\0') &&
	    (retDef->scan_results_file[0] != '\0'))
	{
		/*
		** get the granules table for the scan results
		*/
		sprintf(dataset,"%s SCAN RESULTS FILE",
			retDef->platform);
		if ((status = getGranuleTable (msgDesc, qDesc, dataset,
			scan_results_table, &(query->retStatus))) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
			    "Could not get granule table name for dataset '%s'.",
				dataset);
			(void) ims_qiResetDesc (qDesc);
			return (status);
		}

		/*
		** get the granules table for the raw signal segment
		*/
                sprintf(dataset,"%s RAW SIGNAL SEGMENT",
			 retDef->platform);
                if ((status = getGranuleTable (msgDesc, qDesc, dataset,
                        raw_signal_table, &(query->retStatus))) < IMS_OK)
                {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                            "Could not get granule table name for dataset '%s'.",
                                dataset);
                        (void) ims_qiResetDesc (qDesc);
                        return (status);
                }

		if ((status = getFrameSourceMedia (query, granule_idx,
			frames_granules_table, scan_results_table,
			raw_signal_table))  < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
			   "Could not obtain frame source media for order %d "
				"item %d", order_id, item_id);
			(void) ims_qiResetDesc (qDesc);
			return (status);
		}
	}
	else
	{
		(void)ims_msg(msgDesc,IMS_ERROR,"Either MEDIA_ID or "
			"SCAN_RESULTS_FILE is missing for the SCANNED frame with "
			"granule_idx %d from %s", granule_idx, frames_granules_table); 
		(void) ims_qiResetDesc (qDesc);
		return (IMS_ERROR);
	}
    }
 
    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    (void) ims_msg (msgDesc, IMS_INFO,
        "The Frame Status query succeeded.");

    return (IMS_OK);

}   /*  ims_frameStatus */


/*****************************************************************
**
**  subr getOrderDataset 
**
**  Read the order_item table to get the dataset name for a given
**  order and its granule_idx. 
**
**************************************************************** */
static int getOrderDataset(
    IMS_CMN_QUERY  *query,
    int  order_id,
    int  item_id,
    char *dataset,
    int  *granule_idx)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    static  char cmdBuf[IMS_COL512_LEN];

    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "getOrderDataset: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
	"select dataset, granule_idx from order_item "
	"where order_id = %d and item_id = %d",
	order_id, item_id);

#ifdef QDEBUG
    printf ("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
	    (void) ims_msg( msgDesc, IMS_ERROR,
				"getOrderDataset: Could not read db." );
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
		query->retStatus = IMS_DEADLOCK;
            }
            else
            {
		query->retStatus = IMS_QUERYFAIL;
            }
 
            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Grab data
        */
        (void) memcpy (dataset, qDesc->valAddr[0],
            qDesc->valLength[0]);
        dataset[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr(dataset);

        (void) memcpy ((char *)granule_idx, qDesc->valAddr[1],
            qDesc->valLength[1]);

    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        IMS_SETROWCOUNT (qDesc, "0");
	query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }
    (void) ims_qiResetDesc (qDesc);

    return( IMS_OK );

}   /*  getOrderDataset */

/*****************************************************************
**
**  subr getFrameStatus
**
**  Read the FRAMES granule table to get the frame status and
**  media info for the given granule. 
**
******************************************************************/
static int getFrameStatus (
    IMS_CMN_QUERY  *query,
    char *granules_table,
    int  granule_idx)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    static  char cmdBuf[IMS_COL512_LEN];
    IMS_FRAME_SOURCE_MEDIA_STRUCT	*retDef;

    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_FRAME_SOURCE_MEDIA_STRUCT *) query->retPtr;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "getFrameStatus: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
	"select f.START_TIME, f.END_TIME, f.CENTER_TIME, f.MEDIA_ID, f.FRAME_STATUS, "
	"f.SCAN_RESULTS_FILE, p.platform from %s f, platforms p "
	"where f.granule_idx = %d and f.PLATFORM = p.acronym", 
	granules_table, granule_idx); 

#ifdef QDEBUG
    printf ("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
	    (void) ims_msg( msgDesc, IMS_ERROR,
				"getFrameStatus: Could not read db." );
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
		query->retStatus = IMS_DEADLOCK;
            }
            else
            {
		query->retStatus = IMS_QUERYFAIL;
            }
 
            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Grab data
        */
        (void) memcpy (retDef->start_time, qDesc->valAddr[0],
            qDesc->valLength[0]);
        retDef->start_time[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr(retDef->start_time);

        (void) memcpy (retDef->end_time, qDesc->valAddr[1],
            qDesc->valLength[1]);
        retDef->end_time[qDesc->valLength[1]] = '\0';
        (void) ims_truncStr(retDef->end_time);

        (void) memcpy (retDef->center_time, qDesc->valAddr[2],
            qDesc->valLength[2]);
        retDef->center_time[qDesc->valLength[2]] = '\0';
        (void) ims_truncStr(retDef->center_time);

        (void) memcpy (retDef->media_id, qDesc->valAddr[3],
            qDesc->valLength[3]);
        retDef->media_id[qDesc->valLength[3]] = '\0';
        (void) ims_truncStr(retDef->media_id);

        (void) memcpy (retDef->frame_status, qDesc->valAddr[4],
            qDesc->valLength[4]);
        retDef->frame_status[qDesc->valLength[4]] = '\0';
        (void) ims_truncStr(retDef->frame_status);

        (void) memcpy (retDef->scan_results_file, qDesc->valAddr[5],
            qDesc->valLength[5]);
        retDef->scan_results_file[qDesc->valLength[5]] = '\0';
        (void) ims_truncStr(retDef->scan_results_file);

        (void) memcpy (retDef->platform, qDesc->valAddr[6],
            qDesc->valLength[6]);
        retDef->platform[qDesc->valLength[6]] = '\0';
        (void) ims_truncStr(retDef->platform);

    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        IMS_SETROWCOUNT (qDesc, "0");
	query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }
    (void) ims_qiResetDesc (qDesc);

    return( IMS_OK );

}   /*  getFrameStatus */

/*****************************************************************
**
**  subr getFrameSourceMedia
**
**  Read the scan results granule table and the raw signal segment 
**  table to get source media info for the given SCANNED frame. 
**
******************************************************************/
static int getFrameSourceMedia (
    IMS_CMN_QUERY  *query,
    int  granule_idx,
    char *frames_table,
    char *scan_results_table,
    char *raw_signal_table)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_QI_DESC_OBJ *qDesc;
    int status;
    static  char cmdBuf[IMS_COL1024_LEN];
    IMS_FRAME_SOURCE_MEDIA_STRUCT	*retDef;
    char	platform[IMS_COL10_LEN+1];
    int		revolution;
    short	sequence;

    msgDesc = query->msgDesc;
    qDesc = query->qDesc;
    query->retStatus = IMS_OK;
    retDef = (IMS_FRAME_SOURCE_MEDIA_STRUCT *) query->retPtr;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "getFrameSourceMedia: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement to get data
    ** from the scan results table.
    */
    (void) sprintf (cmdBuf,
	"select s.MEDIA_TYPE, s.RECORDER_ID, s.STATION_ID, s.MEDIA_LOCATION, "
	"f.PLATFORM, f.REVOLUTION, f.SEQUENCE "
	"from %s f, %s s where f.granule_idx = %d " 
	"and s.MEDIA_ID = f.MEDIA_ID "
	"and s.REVOLUTION = f.REVOLUTION "
	"and s.PLATFORM = f.PLATFORM "
	"and s.SEQUENCE = f.SEQUENCE "
	"and s.SCAN_RESULTS_FILE = f.SCAN_RESULTS_FILE",
	frames_table,scan_results_table, granule_idx); 

#ifdef QDEBUG
    printf ("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
	    (void) ims_msg( msgDesc, IMS_ERROR,
				"getFrameSourceMedia: Could not read db." );
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
		query->retStatus = IMS_DEADLOCK;
            }
            else
            {
		query->retStatus = IMS_QUERYFAIL;
            }
 
            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Grab data
        */
        (void) memcpy (retDef->media_type, qDesc->valAddr[0],
            qDesc->valLength[0]);
        retDef->media_type[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr(retDef->media_type);

        (void) memcpy (retDef->recorder_id, qDesc->valAddr[1],
            qDesc->valLength[1]);
        retDef->recorder_id[qDesc->valLength[1]] = '\0';
        (void) ims_truncStr(retDef->recorder_id);

        (void) memcpy (retDef->station_id, qDesc->valAddr[2],
            qDesc->valLength[2]);
        retDef->station_id[qDesc->valLength[2]] = '\0';
        (void) ims_truncStr(retDef->station_id);

        (void) memcpy (retDef->media_location, qDesc->valAddr[3],
            qDesc->valLength[3]);
        retDef->media_location[qDesc->valLength[3]] = '\0';
        (void) ims_truncStr(retDef->media_location);

	(void) memcpy (platform, qDesc->valAddr[4],
	    qDesc->valLength[4]);
	platform[qDesc->valLength[4]] = '\0';
	(void) ims_truncStr(platform);

	(void) memcpy (&revolution, qDesc->valAddr[5],
	    qDesc->valLength[5]);

	(void) memcpy (&sequence, qDesc->valAddr[6],
	    qDesc->valLength[6]);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        IMS_SETROWCOUNT (qDesc, "0");
	query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    /*
    ** Reset the query descriptor for the next query.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "getFrameSourceMedia: Could not reset the query descriptor.");
        return (status);
    }
 
 
    /*
    ** Populate the command buffer with the SQL statement to get DATA_DIRECTION 
    ** from the raw signal segment table.
    */
    (void)sprintf(cmdBuf,
		"select g.DATA_DIRECTION from %s g, datatake_entry m "
		"where MEDIA_ID = '%s' and "
		"((g.PLATFORM = m.PLATFORM and g.REVOLUTION = m.REVOLUTION and"
		"  g.SEQUENCE = m.SEQUENCE) or"
		" (g.PLATFORM = m.DT_PLATFORM and g.REVOLUTION = m.DT_REVOLUTION and"
		"  g.SEQUENCE = m.DT_SEQUENCE)) and "
		"((m.PLATFORM = '%s' and m.REVOLUTION = %d and m.SEQUENCE = %d) or"
		" (m.DT_PLATFORM = '%s' and m.DT_REVOLUTION = %d and "
		"  m.DT_SEQUENCE = %d))",
		raw_signal_table,
		retDef->media_id,
		platform, revolution,sequence,
		platform, revolution,sequence);
#ifdef QDEBUG
    printf ("sql => %s\n", cmdBuf);
#endif

    /*
    ** Execute the command.
    */
    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
	    (void) ims_msg( msgDesc, IMS_ERROR,
				"getFrameSourceMedia: Could not read db." );
            if (qDesc->msgNo == IMS_SYB_DEADLOCK)
            {
		query->retStatus = IMS_DEADLOCK;
            }
            else
            {
		query->retStatus = IMS_QUERYFAIL;
            }
 
            (void) ims_qiFreeDesc (qDesc);
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Grab data
        */
        (void) memcpy (retDef->data_direction, qDesc->valAddr[0],
            qDesc->valLength[0]);
        retDef->data_direction[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr(retDef->data_direction);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        IMS_SETROWCOUNT (qDesc, "0");
	query->retStatus = IMS_NOROWS;
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }
    (void) ims_qiResetDesc (qDesc);

    return( IMS_OK );

}   /*  getFrameSourceMedia */


