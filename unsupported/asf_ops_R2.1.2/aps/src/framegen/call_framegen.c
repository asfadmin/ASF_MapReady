#undef TEST_ARGS_ONLY

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       call_framegen.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)call_framegen.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.call_framegen.c"

/* APS includes:  */
#include <DARconversions.h> /* for table_lookupAPS2IMS()            */
#include <IMSconversions.h> /* for fa_table_lookupAPS2()            */
#include <ODLconversions.h> /* for WOS_satellite, etc.              */
#include <timeconv.h>   

#include "aps_framegen.h"   /* local include file for this executable.  */

/*  IMS include files.  for IMS_MSG_STRUCT, SCONV  */
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_query.h>   /* for ims_msgSetSyslogSeverity()   */
 
/* unix std includes  */
#include <string.h>         /* for strcmp() etc.   */

static int frame_generator_printargs( FILE *fp, FRMGEN *frame_gen )
{
    int j ;

    (void)fprintf(fp, 
        "%s:(%d):  PRINTING frame_generator() arg values in structure:\n",
        __FILE__, __LINE__ ) ;

    (void)fprintf(fp, "\tframe_gen->platform = %s\n", frame_gen->platform ) ;

    (void)fprintf(fp, "\tframe_gen->revolution = %ld\n", frame_gen->revolution  ) ;

    (void)fprintf(fp, "\tframe_gen->sequence = %hd\n", frame_gen->sequence  ) ;

    (void)fprintf(fp, "\tframe_gen->sensor = %c\n", frame_gen->sensor  ) ;

    (void)fprintf(fp, "\tframe_gen->mode = %s\n", frame_gen->mode  ) ;

    (void)fprintf(fp, "\tframe_gen->activity_id = %s\n", frame_gen->activity_id  ) ;

    (void)fprintf(fp, "\tframe_gen->dar_id = %ld\n", frame_gen->dar_id  ) ;

    (void)fprintf(fp, "\tframe_gen->start_time = %s\n", frame_gen->start_time  ) ;

    (void)fprintf(fp, "\tframe_gen->end_time = %s\n", frame_gen->end_time  ) ;

    (void)fprintf(fp, "\tframe_gen->station_id = %s\n", frame_gen->station_id  ) ;

    if ( frame_gen->site_name )
        (void)fprintf(fp, "\tframe_gen->site_name = %s\n", frame_gen->site_name  ) ;
    else
        (void)fprintf(fp, "\tframe_gen->site_name = NULL\n" ) ;

    (void)fprintf(fp, "\tframe_gen->datatake_status = %s\n", 
        frame_gen->datatake_status  ) ;

    if ( frame_gen->media_id )
        (void)fprintf(fp, "\tframe_gen->media_id = %s\n", frame_gen->media_id  ) ;
    else
        (void)fprintf(fp, "\tframe_gen->media_id = NULL\n" ) ;

    if ( frame_gen->scan_results_file )
        (void)fprintf(fp, "\tframe_gen->scan_results_file = %s\n", 
            frame_gen->scan_results_file  ) ;
    else
        (void)fprintf(fp, "\tframe_gen->scan_results_file = NULL\n" ) ;

    (void)fprintf(fp, "\tframe_gen->time_pairs_count = %hd\n", 
        frame_gen->time_pairs_count  ) ;
    for ( j = 0 ; j < frame_gen->time_pairs_count ; j ++ )
    {
        (void)fprintf(fp, "\t\tframe_gen->time_pairs[%d] = \n", j ) ;
        (void)fprintf(fp, "\t\t\tstart_time = %s\n", 
            frame_gen->time_pairs[j].start_time  ) ;
        (void)fprintf(fp, "\t\t\tend_time = %s\n", 
            frame_gen->time_pairs[j].end_time  ) ;
    }


#ifdef RANDOM_CODES
    /* test the OK and ERROR returns.  */
    j = rand() ;
    if (!( j % 4 ))
        return FG_OK ;
    if (!( -1 + j % 4 ))
        return FG_INPUT_ERROR ;
    if (!( -2 + j % 4 ))
        return FG_FATAL ;
    if (!( -3 + j % 4 ))
        return 123 ;
#endif

    return FG_OK ;

}


/*==============================================================================
Function:       call_framegen()

Description:    function that calls frame generation for APS.  It 
                formats the data that has been given, then writes the data 
                to the IMS DB via the frame generator call.  

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 25 11:19:25 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int 
call_framegen( 
    char            *progname,
    FILE            *logfp,           /* log file pointer.  */
    DBPROCESS       *APS_dbproc,
    DB_RECORD       **dtk_rec,        /* report this dtk   */
    char            *dtkstat,         /* use this status   */
    int             time_pairs_count, /* report this number of time-pairs */
    TimePair        *time_pairs,      /* array of TimePair structures     */
    DB_RECORD       **framegen_calls_rec ) /* write rec values here.      */
{

    int         return_code ;
    char        msg[MSG_LEN];

    int         rec_does_exist ;

    IMS_MSG_STRUCT *msgDesc = NULL ;
    FRMGEN      *framegen_ptr = NULL ;

    int         nrecs_inserted ;
    int         nrecs_deleted ;

    /* 
    -- count FG_FATALS:  if there are 3 in a row, then 
    -- we will terminate the aps_framegen run.  no more calls.  
    */
    static int  fg_fatal_count = 0 ;

    TimePair    *time_pair ;
    char        *frame_mode ;
    char        *site_name ;
    char        *datatake_status ;
    char        *media_id ;

    /* quick error checking.  */
    if ( dtk_rec == NULL )
        return APS_FRAMEGEN_ERROR_DTK_REC_IS_NULL ;
    if ( strlen( dtkstat ) != 3 )
        return APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE ;
    if ( strcmp( dtkstat, "DEL" ) && strcmp( dtkstat, "REJ" ) 
    &&   strcmp( dtkstat, "PLN" ) && strcmp( dtkstat, "SCH" ) )
    {
        (void)sprintf(msg,
            "%s(%d):  dtkstat value is neither DEL, REJ, PLN, nor SCH.",
            __FILE__, __LINE__ ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE ;
    }

    if ( time_pairs_count > 0 )
        if ( time_pairs == NULL )
            return APS_FRAMEGEN_ERROR_IN_TIME_PAIRS_PTR ;
    if ( time_pairs_count == 0 )
        if ( time_pairs != NULL )
            return APS_FRAMEGEN_ERROR_IN_TIME_PAIRS_PTR ;
    if ( strncmp( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], "S", 1 ) )
        return APS_FRAMEGEN_ERROR_SENSOR_IS_NOT_A_SAR ;

#ifdef DEBUG
    printf("\n%s(%d):  arguments:\n", __FILE__, __LINE__ ) ;
    dtkm_print( stdout, dtk_rec ) ;
    printf("dtkstat = %s, time_pairs_count = %d\n", dtkstat, time_pairs_count) ;
    for ( j = 0 ; j < time_pairs_count ; j ++ )
    {
        printf("time_pairs[%d] %s  %s\n", j, time_pairs[j].start_time, 
            time_pairs[j].end_time ) ;
    }
#endif


    /*
    -- Allocate frame gen info structure.
    */
    framegen_ptr = malloc( sizeof( FRMGEN ) );

    if ( framegen_ptr == (FRMGEN *) NULL )
    {
        (void)sprintf(msg, "%s(%d):  FRMGEN data structure could not be allocated.",
            __FILE__, __LINE__ ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_ALLOCATING_MEMORY ;
    }

    /* 
    -- load up the message structure for the call 
    -- to the frame generator routine.  
    */

    /*
    --  fill framegen_ptr->   Structure.
    */
    (void)strcpy( framegen_ptr->platform, "R1" ) ;
    framegen_ptr->revolution = 10526 ;
    framegen_ptr->sequence = 6 ;
    framegen_ptr->sensor = 'S' ;
    (void)strcpy( framegen_ptr->mode, "ST7" ) ;
    (void)strcpy( framegen_ptr->activity_id, "RLT" ) ;
    framegen_ptr->dar_id = 1 ;
    (void)strcpy( framegen_ptr->start_time, "1997-314T02:38:32.695" ) ;
    (void)strcpy( framegen_ptr->end_time, "1997-314T02:40:06.908" ) ;
    (void)strcpy( framegen_ptr->station_id, "MC" ) ;

    frame_mode = malloc(strlen("ARCTIC")+1) ;
    (void)strcpy( frame_mode, "ARCTIC" ) ;
    framegen_ptr->frame_mode = frame_mode ;

   (void)strcpy( framegen_ptr->station_id, "MC" ) ;

    site_name = malloc(strlen(" ")+1) ;
    (void)strcpy( site_name, " " ) ;
    framegen_ptr->site_name = site_name ;

    datatake_status = malloc(strlen("PLANNED")+1) ;
    (void)strcpy( datatake_status, "PLANNED" ) ;
    framegen_ptr->datatake_status = datatake_status ;

    media_id = malloc(strlen("FAWS_C011427")+1) ;
    (void)strcpy( media_id, "FAWS_C011427" ) ;
    framegen_ptr->media_id = media_id ;

    framegen_ptr->scan_results_file = NULL ;

    framegen_ptr->time_pairs_count = 1 ;

    time_pair = malloc(sizeof(TimePair)) ;
    (void)strcpy( time_pair->start_time, "1997-314T02:38:33.561" ) ;
    (void)strcpy( time_pair->end_time  , "1997-314T02:40:05.745" ) ;
	framegen_ptr->time_pairs = time_pair ;

    (void)frame_generator_printargs( logfp, framegen_ptr ) ;

#ifndef TEST_ARGS_ONLY
    /*************************************************************
    *                                                            *
    *                                                            *
    *        NOW MAKE THE IMS CALL                               *
    *                                                            *
    *                                                            *
    *************************************************************/
    /*
    -- Allocate IMS message facility structure.
    */
    msgDesc = ims_msgStructAlloc ();
    if ( msgDesc == (IMS_MSG_STRUCT *) NULL )
    {
        /* pop up window message */
        (void)sprintf(msg,
            "IMS message structure could not be allocated." ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_ALLOCATING_MEMORY ;
    }
    /*
    --  framegen_ptr->msgDesc
    */
    framegen_ptr->msgDesc = msgDesc ;

    /*
    -- Specify the syslog facility for logging of the messages generated
    -- by the IMS Message Facility.
    */
    (void) ims_msgOpenSyslog (msgDesc, "IMS:", LOG_LOCAL1);

    /*
    -- Specify the severity of messages:
    */
    (void) ims_msgSetSyslogSeverity(msgDesc, IMS_INFO ) ;

    /*
    -- Specify the program name to be displayed in the syslog message.
    */
    (void) ims_msgProgramName (msgDesc, progname);
 
    /*
    -- Use the Sybase error and message handlers provided by IMS.
    */
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

    /* turn stderr off, direct log file output to logfp.  */
    (void) ims_msgStderrFlag(msgDesc, IMS_OFF) ;
    (void) ims_msgLogFilePtr(msgDesc, logfp) ;

    /* here is the call to make the frames in the IMS:   */
    return_code = frame_generator( framegen_ptr ) ;

    /*
    -- Restore the syslog config info:  
    */
    (void) ims_msgOpenSyslog (msgDesc, "APS:", LOG_LOCAL1);

    /*
    -- Restore the Sybase message and error handlers provided by APS.
    */
    db_install_message_handler(db_default_message_handler);
    db_install_error_handler(error_handler_exit);

     /*************************************************************
     *                                                            *
     *                                                            *
     *        END OF THE IMS CALL                                 *
     *                                                            *
     *                                                            *
     *************************************************************/
#endif

    switch( return_code )
    {
    case FG_OK :
        /* 
        -- count FG_FATALS:  if there are 3 in a row, then 
        -- we will terminate the aps_framegen run.  no more calls.  
        */
        fg_fatal_count = 0 ;

        (void)sprintf(msg, "FG_OK return code from frame_generator() call.\n\n" ) ;
        (void)fprintf(logfp, "%s:  %s\n", progname, msg ) ;

        /*
        --  if neither REJ nor DEL:
        --  record this successful call in the framegen_calls
        --  relation:  sat, sensor, rev, dtkid, strttime, stoptime, 
        --  dtkstat, dtkdate, station_id.
        --  use the input argument framegen_calls_rec for the storage
        --  area for it.  
        --  if it already exists, then edit the record.  If not, then 
        --  insert a new record.  
        --  if REJ nor DEL: then delete the framegen_calls record.  
        */
        /* 
        -- check if dtk was previously reported; 
        -- keep return_code as status.  
        */
        return_code = dtk_was_reported( progname, logfp, dtk_rec, 
            framegen_calls_rec) ;
        if ( return_code < 0 )
        {
            if( msgDesc )
                free( msgDesc ) ;
            if ( framegen_ptr->site_name )
                free( framegen_ptr->site_name ) ;
            free( framegen_ptr ) ;
            return return_code ;
        }

        /* 
        -- set the flag indicating if the framegen_calls 
        -- record exists:  [i.e., if the dtk was previously reported.]
        -- return_code is {TRUE | FALSE}, from dtk_was_reported() 
        */
        rec_does_exist = return_code ;

        /* 
        -- first create the record for possible use by 
        -- the calling program:  
        */
        (void)strcpy( 
			CAST_FRAMEGEN_CALLS_SAT framegen_calls_rec[FRAMEGEN_CALLS_SAT],
                       CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_SENSOR 
            framegen_calls_rec[FRAMEGEN_CALLS_SENSOR],
                  CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) ;
        CAST_FRAMEGEN_CALLS_REV framegen_calls_rec[FRAMEGEN_CALLS_REV] = 
                   CAST_DTK_REV dtk_rec[DTK_REV] ;
        CAST_FRAMEGEN_CALLS_DTKID framegen_calls_rec[FRAMEGEN_CALLS_DTKID] = 
                   CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_STRTTIME 
            framegen_calls_rec[FRAMEGEN_CALLS_STRTTIME],
                CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_STOPTIME 
            framegen_calls_rec[FRAMEGEN_CALLS_STOPTIME],
                CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_DTKSTAT 
            framegen_calls_rec[FRAMEGEN_CALLS_DTKSTAT],
                 dtkstat ) ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_DTKDATE 
            framegen_calls_rec[FRAMEGEN_CALLS_DTKDATE],
                 CAST_DTK_DTKDATE dtk_rec[DTK_DTKDATE] ) ;
        (void)strcpy( CAST_FRAMEGEN_CALLS_STATION_ID 
            framegen_calls_rec[FRAMEGEN_CALLS_STATION_ID],
              CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;

        if (rec_does_exist != TRUE )
        {
            /* rec does NOT exist:  */
            if ( strcmp( dtkstat, "REJ" ) && strcmp( dtkstat, "DEL" ) )
            {
                /* status is neither REJ nor DEL:  */
                nrecs_inserted = db_insert_single_record( APS_dbproc, 
                    framegen_calls_rec, APS_TABLE(FRAMEGEN_CALLS), 
                    APS_CDEFS(FRAMEGEN_CALLS) ) ;
                if ( nrecs_inserted != 1 )
                {
                    (void)sprintf(msg,
"%s(%d):  Error in Sybase insertion in framegen_calls relation",
                        __FILE__, __LINE__ ) ;
                    (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
                    aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, 
                        DO_PRINT);
                    db_print_record(framegen_calls_rec, 
                        APS_CDEFS(FRAMEGEN_CALLS) ) ;
                    if( msgDesc )
                        free( msgDesc ) ;
                    if ( framegen_ptr->site_name )
                        free( framegen_ptr->site_name ) ;
                    free( framegen_ptr ) ;
                    return APS_FRAMEGEN_ERROR_INSERTING_REC_IN_DB ;
                }
            }
            else
            {
                /* 
                -- if status is REJ or DEL, then rec should 
                -- have existed. 
                -- however, here we are with rec does not exist 
                -- and status is REJ or DEL.  this is an error 
                -- by the calling program.  
                */
                (void)sprintf(msg,
"%s(%d):  Error in APS source code.  Check recent mods to APS frame generator",
                        __FILE__, __LINE__ ) ;
                (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
                aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, 
                    DO_PRINT);
                if( msgDesc )
                    free( msgDesc ) ;
                if ( framegen_ptr->site_name )
                    free( framegen_ptr->site_name ) ;
                free( framegen_ptr ) ;
                return APS_FRAMEGEN_ERROR_IN_APS_SOURCE_CODE ;
            }
        }
        else
        {
            /* 
            -- rec does exist.  set up where_clause for 
            -- use if the framegen_calls record is to be deleted or updated:
            -- take the key field values from the input dtk record.  
            */
            (void)sprintf( where_clause,
                "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
                APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SAT),
                    CAST_DTK_SAT dtk_rec[DTK_SAT],
                APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SENSOR),
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_REV),
                    CAST_DTK_REV dtk_rec[DTK_REV],
                APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_DTKID),
                    CAST_DTK_DTKID dtk_rec[DTK_DTKID]  ) ;

            if ( strcmp( dtkstat, "REJ" ) && strcmp( dtkstat, "DEL" ) )
            {
                /* 
                -- rec does exist; must delete, then update the 
                -- framegen_calls record:  
                */
                nrecs_deleted = db_delete_records(APS_dbproc,
                        APS_TABLE(FRAMEGEN_CALLS), where_clause ) ;
                if ( nrecs_deleted != 1 )
                {
                    (void)sprintf(msg,
"%s(%d):  Error in Sybase deletion on framegen_calls relation %s",
                        __FILE__, __LINE__, where_clause ) ;
                    (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
                    aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, 
                        DO_PRINT);
                    if( msgDesc )
                        free( msgDesc ) ;
                    if ( framegen_ptr->site_name )
                        free( framegen_ptr->site_name ) ;
                    free( framegen_ptr ) ;
                    return APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB ;
                }

                nrecs_inserted = db_insert_single_record( APS_dbproc, 
                    framegen_calls_rec, APS_TABLE(FRAMEGEN_CALLS), 
                    APS_CDEFS(FRAMEGEN_CALLS) ) ;
                if ( nrecs_inserted != 1 )
                {
                    (void)sprintf(msg,
"%s(%d):  Error in Sybase insertion in framegen_calls relation",
                        __FILE__, __LINE__ ) ;
                    (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
                    aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, 
                        DO_PRINT);
                    db_print_record(framegen_calls_rec, 
                        APS_CDEFS(FRAMEGEN_CALLS) ) ;
                    if( msgDesc )
                        free( msgDesc ) ;
                    if ( framegen_ptr->site_name )
                        free( framegen_ptr->site_name ) ;
                    free( framegen_ptr ) ;
                    return APS_FRAMEGEN_ERROR_INSERTING_REC_IN_DB ;
                }
            }
            else
            {
                /* 
                -- if status is REJ or DEL, then delete rec:
                */
                nrecs_deleted = db_delete_records(APS_dbproc,
                        APS_TABLE(FRAMEGEN_CALLS), where_clause ) ;
                if ( nrecs_deleted != 1 )
                {
                    (void)sprintf(msg,
"%s(%d):  Error in Sybase deletion on framegen_calls relation %s",
                        __FILE__, __LINE__, where_clause ) ;
                    (void)fprintf( logfp, "%s:\n\n%s\n", progname, msg ) ;
                    aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, 
                        DO_PRINT);
                    if( msgDesc )
                        free( msgDesc ) ;
                    if ( framegen_ptr->site_name )
                        free( framegen_ptr->site_name ) ;
                    free( framegen_ptr ) ;
                    return APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB ;
                }
            }
        }
        break ;

    case FG_INPUT_ERROR :
        /* 
        -- count FG_FATALS:  if there are 3 in a row, then 
        -- we will terminate the aps_framegen run.  no more calls.  
        */
        fg_fatal_count = 0 ;

        (void)sprintf(msg,
"FG_INPUT_ERROR return code from frame_generator() call for %s %d %s/%s/%5.5ld.%2.2d  %s",
        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID],
        CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID],
        CAST_DTK_SAT dtk_rec[DTK_SAT],
        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
        CAST_DTK_REV dtk_rec[DTK_REV],
        CAST_DTK_DTKID dtk_rec[DTK_DTKID],
        dtkstat ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n\n\n", progname, msg ) ;
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        (void)frame_generator_printargs( stderr, framegen_ptr ) ;
        /* now clean up and return.  */
        if ( framegen_ptr->site_name )
            free( framegen_ptr->site_name ) ;
        free( framegen_ptr ) ;
        if( msgDesc )
            free( msgDesc ) ;
        return APS_FRAMEGEN_FG_INPUT_ERROR ;

    case FG_FATAL :
        /* 
        -- count FG_FATALS:  if there are 3 in a row, then 
        -- we will terminate the aps_framegen run.  no more calls.  
        */
        fg_fatal_count ++ ;

        (void)sprintf(msg,
"FG_FATAL return code from frame_generator() call for %s %d %s/%s/%5.5ld.%2.2d  %s",
            CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID],
            CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID],
            CAST_DTK_SAT dtk_rec[DTK_SAT],
            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            CAST_DTK_REV dtk_rec[DTK_REV],
            CAST_DTK_DTKID dtk_rec[DTK_DTKID],
            dtkstat ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void)frame_generator_printargs( stderr, framegen_ptr ) ;
        /* now clean up and return.  */
        if ( framegen_ptr->site_name )
            free( framegen_ptr->site_name ) ;
        free( framegen_ptr ) ;
        if( msgDesc )
            free( msgDesc ) ;

        /* 
        -- count FG_FATALS:  if there are 3 in a row, then 
        -- we will terminate the aps_framegen run.  no more calls.  
        */
        if( fg_fatal_count >= 3 )
        {
            (void)sprintf(msg, 
                "3 FATAL errors in a row; terminating the Frame Generator run.") ;
            (void)fprintf(logfp, "%s:\n%s\n\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            return APS_FRAMEGEN_FG_FATAL ;
        }
        else
            return APS_FRAMEGEN_FG_ERROR ;

    default :
        /* 
        -- count FG_FATALS:  if there are 3 in a row, then 
        -- we will terminate the aps_framegen run.  no more calls.  
        */
        fg_fatal_count = 0 ;

        (void)sprintf(msg,
"UNKNOWN return code from frame_generator() call for %s %d %s/%s/%5.5ld.%2.2d  %s",
            CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID],
            CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID],
            CAST_DTK_SAT dtk_rec[DTK_SAT],
            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            CAST_DTK_REV dtk_rec[DTK_REV],
            CAST_DTK_DTKID dtk_rec[DTK_DTKID],
            dtkstat ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void)frame_generator_printargs( stderr, framegen_ptr ) ;
        /* now clean up and return.  */
        if ( framegen_ptr->site_name )
            free( framegen_ptr->site_name ) ;
        free( framegen_ptr ) ;
        if( msgDesc )
            free( msgDesc ) ;
        return APS_FRAMEGEN_FG_UNKNOWN_CODE ;
    }

    /* 
    -- now clean up and return.  
    */
    free( frame_mode  )  ;
    free( site_name  )  ;
    free( media_id  )  ;
    free( datatake_status  )  ;
    free( time_pair  )  ;

    free( framegen_ptr ) ;
    if( msgDesc )
        free( msgDesc ) ;

    return APS_FRAMEGEN_FG_OK ;

} /* aps_framegen()   */
