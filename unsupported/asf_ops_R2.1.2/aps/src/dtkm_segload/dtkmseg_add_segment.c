/*  #define PRING_DIAG   */
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkmseg_add_segment.c

Description:    contains functions to support the function 
                dtkmseg_add_segment()

External Functions Defined:

    int dtkmseg_add_segment(
        DBPROCESS   *APS_dbproc,        Sybase db process     
        DB_RECORD   **proposed_dtk_seg,     data-take segment to be added.        
        DB_RECORD   **result_dtk,       data-take added, if any               
        int         minimum_seconds,    minimum time-duration of data-take.    
        llist       *dtk_conflicts,     list of conflicting data-takes         
        llist       *dtk_combines,      list of data-takes that the proposed   
                                        dtk should be combined with instead    
                                        of going into the dtk relation.        
        llist       *dtk_changes  )     list of data-takes that the proposed   
    
File Scope Functions:

    dtkmseg_update_seg_dtkid()
    dtkmseg_seg_exists()
    dtkmseg_insert_seg()
    dtkmseg_delete_seg()

External Variables Defined:
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)dtkmseg_add_segment.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/dtkm_segload/SCCS/s.dtkmseg_add_segment.c"

#include <string.h>

#include "dtkm_segload.h"
#include <dtkm_utilities.h>



/*==============================================================================

Function:       dtkmseg_seg_exists

Description:    checks to see if the segment already exists in the database.

Parameters:
    DB_RECORD   **proposed_dtk  NOTE:  this is a dtk relation DB_RECORD.
                                values in it are used to search the 
                                seg relation.
Returns:
    DTKM_SEGLOAD_SEG_EXISTS
    DTKM_SEGLOAD_SEG_DOES_NOT_EXIST

    < 0 :  an error occurred.

Creator:    Lawrence Stevens

Creation Date:  03/13/1995

Notes:

==============================================================================*/
static int dtkmseg_seg_exists(
    DB_RECORD   **proposed_dtk ) /* proposed data-take    */
{

    int         number_of_recs ;

    sprintf(where_clause, 
    "where %s = %ld and %s = '%s' and %s = '%s' and %s = %ld and %s = '%s' and %s = '%s' and %s = '%c'",
        APS_COL(SEG, SEG_DARID),  CAST_DTK_DARID proposed_dtk[DTK_DARID],
        APS_COL(SEG, SEG_SAT),    CAST_DTK_SAT proposed_dtk[DTK_SAT],
        APS_COL(SEG, SEG_SENSOR), CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
        APS_COL(SEG, SEG_REV),    CAST_DTK_REV proposed_dtk[DTK_REV],
        APS_COL(SEG, SEG_STRTTIME),CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
        APS_COL(SEG, SEG_STOPTIME),CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
        APS_COL(SEG, SEG_ASCDSC), CAST_DTK_ASCDSC proposed_dtk[DTK_ASCDSC]) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, 
        where_clause) ;
#endif

    number_of_recs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(SEG), where_clause ) ;

    if ( number_of_recs < 0)
        return DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED ;

#ifdef PRINT_DIAG
    printf("%s(%d):  number_of_recs = %d\n", __FILE__, __LINE__, 
        number_of_recs ) ;
#endif

    if ( number_of_recs == 0 )
        return DTKM_SEGLOAD_SEG_DOES_NOT_EXIST ;
    else
        return DTKM_SEGLOAD_SEG_EXISTS ;

}


/*==============================================================================

Function:       dtkmseg_insert_seg

Description:    inserts a segment into the db from a dtk relation DB_RECORD.
seg.segid and seg.segdate values are created.  

Parameters:
    DBPROCESS   *APS_dbproc
    DB_RECORD   **proposed_dtk  NOTE:  this is a dtk relation DB_RECORD.
                                values in it are used to search the 
                                seg relation.
    int         *new_segid      the id of the segment inserted, generated here.
Returns:

Creator:    Lawrence Stevens

Creation Date:  03/14/1995

Notes:

==============================================================================*/
static int dtkmseg_insert_seg(
    DBPROCESS   *APS_dbproc,     /* Sybase db process     */
    DB_RECORD   **proposed_dtk,  /* proposed data-take    */
    int         *new_segid )     /* the new segid created here  */
{

    DB_RECORD       **seg_rec ;
    llist           *seg_list = NULL ;
    cursor          seg_list_ptr ;

    char            segdate[12] ;
    int             nrecs_inserted ;

    /* 
    -- get the maximum seg id plus one.
    -- retrieve the segs for this darid, sorting by seg id.
    -- then just get the last one, if there are any.
    */

    sprintf(where_clause, "where %s = %ld",
        APS_COL(SEG, SEG_DARID), CAST_DTK_DARID proposed_dtk[DTK_DARID] ) ;

    sprintf(orderby_cols, "%s", APS_COL(SEG, SEG_SEGID) ) ;

    seg_list = db_get_records(APS_dbproc, APS_TABLE(SEG),
        where_clause, orderby_cols, APS_CDEFS(SEG), SEG_SEGID, END_COLS ) ;

    if ( seg_list == NULL )
        return DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED ;

    if ( NUMELTS( seg_list ) > 0 )
    {
        seg_rec = (DB_RECORD **) LAST(seg_list, seg_list_ptr) ;
        *new_segid = CAST_SEG_SEGID seg_rec[SEG_SEGID] + 1 ;
    }
    else if ( NUMELTS( seg_list ) < 0 )
        return DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED ;
    else
        *new_segid = 1 ;

#ifdef PRINT_DIAG
    printf("%s(%d):  new seg.segid will be:  %d\n", __FILE__, __LINE__,
        *new_segid) ;
#endif

    /* through with the list.  */
    DEL_LIST( seg_list ) ;

    /*
    -- get today's date:  yyyy:ddd
    */
    tc_systime2yyyycddd(segdate) ;

    /*
    -- Want to append a seg relation DB_RECORD.  First create one, 
    -- then copy values into its fields.  Then insert to db.  
    -- then free_db_record() it.  
    */

    seg_rec =  new_table_record(APS_CDEFS(SEG)) ;

    CAST_SEG_DARID seg_rec[SEG_DARID] = 
        CAST_DTK_DARID proposed_dtk[DTK_DARID] ;

    CAST_SEG_SEGID seg_rec[SEG_SEGID] = *new_segid ;

    strcpy(CAST_SEG_SAT seg_rec[SEG_SAT], 
        CAST_DTK_SAT proposed_dtk[DTK_SAT]) ;

    strcpy(CAST_SEG_SENSOR seg_rec[SEG_SENSOR], 
        CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR]) ;

    CAST_SEG_REV seg_rec[SEG_REV] = 
        CAST_DTK_REV proposed_dtk[DTK_REV] ;

    strcpy(CAST_SEG_DTKID seg_rec[SEG_DTKID], "" ) ; 

    CAST_SEG_ASCDSC seg_rec[SEG_ASCDSC] = 
        CAST_DTK_ASCDSC proposed_dtk[DTK_ASCDSC] ;

    strcpy(CAST_SEG_STRTTIME seg_rec[SEG_STRTTIME], 
        CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME]) ;

    strcpy(CAST_SEG_STOPTIME seg_rec[SEG_STOPTIME], 
        CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME]) ;

    CAST_SEG_STRTLAT seg_rec[SEG_STRTLAT] = 
        CAST_DTK_STRTLAT proposed_dtk[DTK_STRTLAT] ;

    CAST_SEG_STOPLAT seg_rec[SEG_STOPLAT] = 
        CAST_DTK_STOPLAT proposed_dtk[DTK_STOPLAT] ;

    CAST_SEG_NRLAT1 seg_rec[SEG_NRLAT1] = 
        CAST_DTK_NRLAT1 proposed_dtk[DTK_NRLAT1] ;
    CAST_SEG_NRLON1 seg_rec[SEG_NRLON1] = 
        CAST_DTK_NRLON1 proposed_dtk[DTK_NRLON1] ;
    CAST_SEG_FARLAT1 seg_rec[SEG_FARLAT1] = 
        CAST_DTK_FARLAT1 proposed_dtk[DTK_FARLAT1] ;
    CAST_SEG_FARLON1 seg_rec[SEG_FARLON1] = 
        CAST_DTK_FARLON1 proposed_dtk[DTK_FARLON1] ;

    CAST_SEG_NRLAT2 seg_rec[SEG_NRLAT2] = 
        CAST_DTK_NRLAT2 proposed_dtk[DTK_NRLAT2] ;
    CAST_SEG_NRLON2 seg_rec[SEG_NRLON2] = 
        CAST_DTK_NRLON2 proposed_dtk[DTK_NRLON2] ;
    CAST_SEG_FARLAT2 seg_rec[SEG_FARLAT2] = 
        CAST_DTK_FARLAT2 proposed_dtk[DTK_FARLAT2] ;
    CAST_SEG_FARLON2 seg_rec[SEG_FARLON2] = 
        CAST_DTK_FARLON2 proposed_dtk[DTK_FARLON2] ;

    CAST_SEG_LOOKANGL seg_rec[SEG_LOOKANGL] = 
        CAST_DTK_LOOKANGL proposed_dtk[DTK_LOOKANGL] ;

    strcpy(CAST_SEG_SEGSTAT seg_rec[SEG_SEGSTAT], 
        CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT]) ;

    strcpy(CAST_SEG_SEGDATE seg_rec[SEG_SEGDATE], segdate ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  printing DB_RECORD to insert:  \n", __FILE__, __LINE__) ;
    db_print_record(seg_rec, APS_CDEFS(SEG) );
#endif

    /* insert into seg table.  */
    nrecs_inserted = db_insert_single_record(APS_dbproc, 
        seg_rec, APS_TABLE(SEG), APS_CDEFS(SEG) ) ;
    free_db_record( seg_rec ) ;
    if ( nrecs_inserted != 1 )
        return DTKM_SEGLOAD_ERROR_SEG_NOT_INSERTED ;

    return TRUE ;

}


/*==============================================================================

Function:       dtkmseg_delete_seg

Description:    checks to see if the segment already exists in the database,
                then deletes it.

Parameters:
    DBPROCESS   *APS_dbproc
    DB_RECORD   **proposed_dtk  NOTE:  this is a dtk relation DB_RECORD.
                                values in it are used to search the 
                                seg relation.
Returns:

Creator:    Lawrence Stevens

Creation Date:  03/21/1995

Notes:

==============================================================================*/
static int dtkmseg_delete_seg(
    DBPROCESS   *APS_dbproc,     /* Sybase db process     */
    DB_RECORD   **proposed_dtk ) /* proposed data-take    */
{

    int         number_of_recs ;

    sprintf(where_clause, 
    "where %s = %ld and %s = '%s' and %s = '%s' and %s = %ld and %s = '%s' and %s = '%s' and %s = '%c'",
        APS_COL(SEG, SEG_DARID),  CAST_DTK_DARID proposed_dtk[DTK_DARID],
        APS_COL(SEG, SEG_SAT),    CAST_DTK_SAT proposed_dtk[DTK_SAT],
        APS_COL(SEG, SEG_SENSOR), CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
        APS_COL(SEG, SEG_REV),    CAST_DTK_REV proposed_dtk[DTK_REV],
        APS_COL(SEG, SEG_STRTTIME),CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
        APS_COL(SEG, SEG_STOPTIME),CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
        APS_COL(SEG, SEG_ASCDSC), CAST_DTK_ASCDSC proposed_dtk[DTK_ASCDSC]) ;

    number_of_recs = db_delete_records(APS_dbproc, 
        APS_TABLE(SEG), where_clause) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  number_of_recs = %d\n", __FILE__, __LINE__, 
        number_of_recs ) ;
#endif

    if ( number_of_recs < 0 )
        return DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED ;
    else if ( number_of_recs == 0 )
        return DTKM_SEGLOAD_ERROR_SEG_NOT_DELETED ;
    else if ( number_of_recs == 1 )
        return TRUE ;
    else 
        return DTKM_SEGLOAD_ERROR_GT_1_SEG_REC_DELETED ;

}


/*==============================================================================
Function:       dtkmseg_add_segment

Description:  accepts a segment record from the mapper in the form of a dtk
DB_RECORD and creates a data-take out of it with status QUE.  It checks 
a number of things, including checking to see if the dtk can be a real-time
data-take.  

If the segment already exists, the new segment will replace it.  the 
criterion for already existing is that the darid, sat, sensor, rev, start and
stop times and the ascdsc indicator are all the same.  

If this routine is called with the same segment given several times, it 
will have the same effect as if it was submitted one time.  

If conflicts have been removed between submissions of the same segment, 
it could create a data-take the next time.  

Parameters:     
    DBPROCESS   *dbproc           Sybase db process
    DB_RECORD   **proposed_dtk    data-take to be added; updated to show result
    int         minimum_seconds   minimum time-duration of data-take.    
    llist       *dtk_conflicts    list of conflicting dtks 
    llist       *dtk_combines     list of dtks to combine with after change.

Returns:        
    int
    >= 0 :
        DTK_ADD_SEGMENT_OK   the seg and dtk were added.
        DTK_SEG_NOT_ADDED_EXISTS_ALREADY  nothing was added.
        DTK_NOT_ADDED_NO_RECORDER  the seg was added but not the dtk.
        DTK_NOT_ADDED_TOO_SHORT    the seg was added but not the dtk.
        DTK_NOT_ADDED_CONFLICTS    the seg was added but not the dtk.
        DTK_NOT_ADDED_COMBINES_NO_CONFLICTS
                                   the seg was added but not the dtk.
        DTK_NOT_ADDED_EQUIPMENT_DOWN
                                   the seg was added but not the dtk.


    < 0 ERROR:  
        DTK_RECORD_NULL 
        DTK_SHOULD_BE_QUE_STATUS 
        DTK_ADD_SEGMENT_ERROR_SEG_NOT_INSERTED 
        DTK_ERROR_IN_CODE_dtk_seg_exists 
        DTK_ERROR_IN_CODE_dtk_add_segment 
        DTK_COMBINES_LIST_NOT_INITIALIZED
        DTK_COMBINES_LIST_NOT_EMPTY

Creator:        Lawrence Stevens

Creation Date:  03/10/1995

Notes:      

==============================================================================*/
int dtkmseg_add_segment(
    DBPROCESS   *APS_dbproc,         /* Sybase db process                     */
    DB_RECORD   **proposed_dtk_seg,  /* data-take segment to be added.        */
    DB_RECORD   **proposed_dtk,    /* completed data-take added, if seg added */
    int         minimum_seconds,   /*   minimum time-duration of data-take.   */
    FILE        *report_fp   )     /* log file report    */
{

    int    return_code ;
    int    new_segid ;
    double duration_days ;
    char   reduced_strttime[22];
    char   reduced_stoptime[22];
    char   station_id[4] ;

    /* error checking  */
    if ( proposed_dtk_seg == NULL ) 
        return DTKM_SEGLOAD_ERROR_NULL_PROPOSED_DTK_SEG ;

    if ( proposed_dtk == NULL ) 
        return DTKM_SEGLOAD_ERROR_NULL_DTK_PROPOSAL ;

    /* check status of data-take seg */
    if ( strcmp( CAST_DTK_DTKSTAT proposed_dtk_seg[DTK_DTKSTAT], "QUE" ) != 0 )
        return DTKM_SEGLOAD_ERROR_SHOULD_BE_QUE_STATUS ;

    /* check time duration minimum against the data-take time bracket. */
    if ( !tc_et_ASF_datetime_diff ( 
        CAST_DTK_STRTTIME proposed_dtk_seg[DTK_STRTTIME], 
        CAST_DTK_STOPTIME proposed_dtk_seg[DTK_STOPTIME], 
        &duration_days ) )
        return DTKM_SEGLOAD_ERROR_IN_SEG_TIMES ;

#ifdef PRINT_DIAG
    PRINT_DIAG("%s(%d):  duration_days = %f\n", __FILE__, __LINE__, 
        duration_days ) ;
#endif

    if ( duration_days < (double) (minimum_seconds / 3600.0 / 24.0) )
        return DTKM_SEGLOAD_NOT_ADDED_TOO_SHORT ;

    /* 
    -- copy the segment to the internal proposed_dtk 
    -- record.  this is because some of the routines which 
    -- do things write on the proposed record, like the date
    -- and such.  we want the input proposed_dtk_seg to be unchanged
    -- and the result of the data-take actually created in the 
    -- result_dtk db record returned to the calling routine.  
    */
    return_code = db_copy_record ( APS_CDEFS(DTK), 
        proposed_dtk, proposed_dtk_seg ) ;

    /* 
    -- see if the segment already exists:
    */
    return_code = dtkmseg_seg_exists( proposed_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    if ( return_code == DTKM_SEGLOAD_SEG_EXISTS )
    {
#ifdef PRINT_DIAG
        PRINT_DIAG("%s(%d):  segment exists; delete it first:\n", 
            __FILE__, __LINE__ );
#endif
        return_code = dtkmseg_delete_seg( APS_dbproc, proposed_dtk ) ;
        if ( return_code < 0 )
            return return_code ;
    }
    else if ( return_code != DTKM_SEGLOAD_SEG_DOES_NOT_EXIST )
        return DTKM_SEGLOAD_ERROR_IN_CODE_dtkmseg_seg_exists ;
#ifdef PRINT_DIAG
    else 
        printf("%s(%d):  segment does not exist.\n", __FILE__, __LINE__ );
#endif

    /* 
    -- segment does not exist; add it to the segment relation
    */
    return_code = dtkmseg_insert_seg(APS_dbproc, proposed_dtk, &new_segid) ;
    if ( return_code < 0 ) 
        return return_code ;

#ifdef PRINT_DIAG
    printf("%s(%d):  segment inserted OK.\n", __FILE__, __LINE__ );
#endif

    /* segment record was inserted OK.  */

    /* 
    -- check to see if the dtk is within a station mask  
    -- if so, then put the station_id into the dtk record:
    */
    return_code = dtkm_determine_station_mask( proposed_dtk, 
        minimum_seconds, reduced_strttime, reduced_stoptime, station_id ) ;
    if ( return_code < 0 ) 
        return return_code ;

    /* set up real-time (RT) or record (RE) activity id characters */
    if ( return_code == DTKM_DTK_HAS_TIME_IN_MASK )
    {

#ifdef PRINT_DIAG
        printf("%s(%d):  segment in mask.\n", __FILE__, __LINE__ );
#endif

        strcpy(CAST_DTK_ACTID proposed_dtk[DTK_ACTID], 
                DTKM_ACTID_REALTIME_OBSERVATION_CODE ) ;
        strcpy(CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME], reduced_strttime);
        strcpy(CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME], reduced_stoptime);
        strcpy(CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID], station_id);
    }
    else
    {
#ifdef PRINT_DIAG
        printf("%s(%d):  segment not in mask.\n", __FILE__, __LINE__ );
#endif

        /* check to see if the sat has a recorder   */
        return_code = dtkm_sat_has_recorder(proposed_dtk) ;
        if ( return_code < 0 ) 
        {
            if ( report_fp )
            {
                fprintf(report_fp, "ERROR:  %s\n", 
                    DTKM_ERROR_MESSAGE( return_code ) ) ;
            }

            return DTKM_SEGLOAD_ERROR_ADDING_SEGMENT ; 
        }
        if ( return_code == TRUE )
        {

#ifdef PRINT_DIAG
            printf("%s(%d):  segment has a recorder.\n", 
                __FILE__, __LINE__ ) ;
#endif
            /* satellite has a recorder  */
            strcpy( CAST_DTK_ACTID proposed_dtk[DTK_ACTID], 
                    DTKM_ACTID_RECORDING_OBSERVATION_CODE ) ;
            /* assume that the downlink will come eventually to ASF.  */
            strcpy(CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID], "ASF");
        }
        else
        {
            /* satellite has no recorder, cannot make a data-take.  */
            return DTKM_SEGLOAD_NOT_ADDED_NO_RECORDER ;
        }
    }
    /* init the unknown antenna id.  */
    CAST_DTK_ANTENNA_ID proposed_dtk[DTK_ANTENNA_ID] = 0 ;

#ifdef PRINT_DIAG
    printf("%s(%d):  BEFORE default values:\n", __FILE__, __LINE__ );
    db_print_record( proposed_dtk, APS_CDEFS(DTK) ) ;
    dtkm_print( stdout, proposed_dtk) ;
#endif

    /* 
    -- complete the values of the observation data-take, values not 
    -- given in the data-take opportunities relation (sscvrg):  
    */
    return_code = dtkm_default_values( proposed_dtk, proposed_dtk ) ;
#ifdef PRINT_DIAG
    printf("%s(%d):  AFTER default values:\n", __FILE__, __LINE__ );
    db_print_record( proposed_dtk, APS_CDEFS(DTK) ) ;
    dtkm_print( stdout, proposed_dtk) ;
#endif
    if ( return_code < 0 ) 
    {
        if ( report_fp )
        {
            fprintf( report_fp, "ERROR:  %s\n", 
                DTKM_ERROR_MESSAGE( return_code )  ) ;
        }

        return DTKM_SEGLOAD_ERROR_ADDING_SEGMENT ;
    }

    /* 
    -- the first part of this routine was completing various 
    -- parameters while converting the input from a data-take 
    -- opportunity - sensor on/off - into a data-take db 
    -- record with the rest of the paramaters added.  
    -- such parameters described whether or not the data-take 
    -- was real-time or record, or what downlink frequency was used. 
    --
    -- at this point, the proposed_dtk record has been processed 
    -- so that it describes a data-take.  
    -- now it may be used as a dtk proposal 
    */

    return DTKM_SEGLOAD_ADD_SEGMENT_OK ;

}
