#undef DEBUG
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_segload.c

Description:    Contains routines for the dtkm_segload executable, which 
                reads a Segment File from the Mapper and creates data-takes
                from them.

External Functions Defined:
    
File Scope Functions:
                main()
                int dtk_seg_file_load()
                int dtk_seg_list_load()
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_segload.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/dtkm_segload/SCCS/s.dtkm_segload.c"


/* VARIOUS LIBRARY AND SYSTEM ROUTINES:  */
#include <fcntl.h>          /* for open             */
#include <stdio.h>          /* for setbuf, stdout   */
#include <string.h>         /* for strcpy           */

#include <stdlib.h>                   /* for getopt()     */
extern char *optarg ;                 /* for getopt()     */
extern int  optind, opterr, optopt ;  /* for getopt()     */

#include <sys/stat.h>       /* for open             */
#include <sys/types.h>      /* for open             */
#include <sys/uio.h>        /* for read             */
#include <unistd.h>         /* for read             */

#include "aps_log_msg.h"
#include "aps_defs.h"
#include "dtkm_segload.h"

#include <mu_utilities.h>   /* for multi-user permission stuff.  */

static      char    msg[MSG_LEN];
static      char    buf[SEG_FILE_REC_LEN+1] = {0} ;
static      char    *progname;


/*==============================================================================
Function:       usage_exit()

Description:    print usage and then exit.  

Creator:        Lawrence Stevens

Creation Date:  Sat Jan 18 15:06:30 PST 1997

==============================================================================*/
static void usage_exit(char *progname )
{
    printf("\nusage:  %s [-p permission_id] [-U sybase_userid]\n", progname);
    printf("                         -P sybase_password   seg_file\n" );
    printf("\n\tNOTES:  \n");
    printf("\n\tpermission_id is the integer permission id > 0 indicating\n");
    printf("\t              that Multi-user permission has been granted\n");
    printf("\t              for both the DAR activity and the planning \n");
    printf("\t              activities.  Both permissions have this id.\n");
    printf("\n\tThe seg_file is a \"segment file\" created by the APS Mapper.\n");
    printf("\tData-takes will be created from each segment in the file unless \n");
    printf("\tthere is a conflict or other problem.  A log file of all planning\n");
    printf("\tactivities or errors is written to standard output.\n");

    printf("\n\t[%s  Version Dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(progname, APS_ERROR, "usage error", DO_SYSLOG, DO_PRINT);
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);

    exit (APS_EXIT_ERROR);
}

/*==============================================================================
Function:       error_exit()
Description:    prints syslog error message and exits with an error code.
Creator:        Lawrence Stevens
Creation Date:  Fri Jan 10 16:59:52 PST 1997
==============================================================================*/
static void error_exit(char *progname, int code )
{
    void        banner_exit(int) ;
 
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
        DO_SYSLOG, DO_PRINT);
    banner_exit(code);
}


/*==============================================================================
Function:       dtk_seg_list_load

Description:    processes a list of DB_RECORDS that are data-take 
opportunities, turning them into data-takes with QUE status.  The processing 
includes checking for dtk within mask, and other stuff.

Returns:        
    OK:  TRUE

    < 0 Errors:  

Creator:        Lawrence Stevens

Creation Date:  Fri May 19 13:52:16 PDT 1995

Notes:      
==============================================================================*/

static int dtk_seg_list_load(
    DBPROCESS   *APS_dbproc,     /* Sybase db process                    */
    llist       *input_dtks,     /* input segment list of dtk DB_RECORDs */
    llist       *accepted_dtks,  /* output list of accepted data-take records
                                 -- containing values input to the database.
                                 -- Note that the output list will have added
                                 -- data, such as the dtkdate and dtkid, which
                                 -- are not in the input records.
                                 */
    llist       *rejected_dtks,  /* output list rejected data-take records
                                 -- a rejected data-take is one that could
                                 -- not be placed into the ASF plans
                                 -- as kept in the dtk relation.
                                 */
    llist       *CON_dtks,       /* output list of rejected data-take records */
                                 /* that can still be put into the schedule   */
                                 /* with some work, usually by altering it.   */
    llist       *error_dtks,     /* output list data-takes with errors.      */
    llist       *dtk_updates,    /* output list of all changed dtks.         */

    FILE        *report_fp   )   /* log file report    */
{

    /* declarations     */
    cursor        input_dtks_ptr ;
    DB_RECORD     **dtk_seg_rec ;
    DB_RECORD     **dtk_rec ;
    DB_RECORD     **result_dtk ;

    DB_RECORD     **downlink_dtk = NULL ;
    cursor        QUE_dtks_ptr_save ;
    DB_RECORD     **dtk_rec_save = NULL ;

    int           return_code ;

    llist       *dtk_proposal_list = NULL ;
    llist       *deleted_dtks ;
    llist       *REJ_omission_dtks ;
    llist       *other_sat_dtks ;
    llist       *same_sat_dtks ;

    llist       *QUE_dtks ;
    cursor      *QUE_dtks_ptr ;

    /* error checking  */
 
    /*
    -- check input dtk list
    */
 
    if ( input_dtks == NULL )
        return DTKM_SEGLOAD_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( input_dtks ) <= 0 )
        return DTKM_SEGLOAD_ERROR_INPUT_DTK_LIST_NOT_EMPTY ;

    /*
    -- check input accepted dtk list
    */
 
    if ( accepted_dtks == NULL )
        return DTKM_SEGLOAD_ERROR_OUTPUT_ACCEPTED_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( accepted_dtks ) != 0 )
        return DTKM_SEGLOAD_ERROR_OUTPUT_ACCEPTED_DTK_LIST_IS_NOT_EMPTY ;
 
    /*
    -- check output rejected dtk list
    */
 
    if ( rejected_dtks == NULL )
        return DTKM_SEGLOAD_ERROR_OUTPUT_REJECTED_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( rejected_dtks ) != 0 )
        return DTKM_SEGLOAD_ERROR_OUTPUT_REJECTED_DTK_LIST_IS_NOT_EMPTY ;
 
    /*
    -- check output CON_dtks list
    */
 
    if ( CON_dtks == NULL )
        return DTKM_SEGLOAD_ERROR_OUTPUT_CON_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( CON_dtks ) != 0 )
        return DTKM_SEGLOAD_ERROR_OUTPUT_CON_DTK_LIST_IS_NOT_EMPTY ;

    /*
    -- check output error_dtks list
    */
 
    if ( error_dtks == NULL )
        return DTKM_SEGLOAD_ERROR_OUTPUT_ERROR_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( error_dtks ) != 0 )
        return DTKM_SEGLOAD_ERROR_OUTPUT_ERROR_DTK_LIST_IS_NOT_EMPTY ;

    /*
    -- check output dtk_updates list
    */
 
    if ( dtk_updates == NULL )
        return DTKM_SEGLOAD_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_updates ) != 0 )
        return DTKM_SEGLOAD_ERROR_DTK_UPDATES_LIST_NOT_EMPTY ;

    /* 
    -- set up storage to store the result data-take 
    -- proposal list for later processing.  
    */
    dtk_proposal_list = create_dyn_llist() ;

    /* set up storage to store the result data-take.  */
    result_dtk = new_table_record(APS_CDEFS(DTK)) ;

    /**********************************************************************/
    /*                                                                    */
    /*              LOOP ON THE INPUT LIST OF SEGMENTS                    */
    /*              STORED IN DTK DB_RECORDS                              */
    /*                                                                    */
    /**********************************************************************/
    for (
        dtk_seg_rec = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr) ;
        dtk_seg_rec ;
        dtk_seg_rec = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr) 
        )
    {

#ifdef DEBUG
        printf("dtk_seg_list_load:  DB_RECORD from list:\n") ;
        db_print_record( dtk_seg_rec, APS_CDEFS(DTK) ) ;
#endif

        if ( report_fp )
        {
            fprintf(report_fp, "\nProcessing segment for DAR %ld:\n",
                CAST_DTK_DARID dtk_seg_rec[DTK_DARID] ) ;
            dtkm_print(report_fp, dtk_seg_rec ) ;
        }

        /*
        -- process this record; add data to some fields to 
        -- make it complete.  result will be in result_dtk:
        */
        return_code = dtkmseg_add_segment( APS_dbproc, 
            dtk_seg_rec, result_dtk, 0, report_fp ) ;
        if ( return_code < 0 )
        {
            sprintf(msg, "%s   Check the GUI-created log file for this segment file for details", 
            DTKM_SEGLOAD_ERROR_MESSAGE( return_code ) ) ;
            aps_log_msg(progname, APS_ERROR, 
                DTKM_SEGLOAD_ERROR_MESSAGE( return_code ),
                DO_SYSLOG, DO_PRINT);

            /* error  */
            if ( report_fp )
            {
                fprintf(report_fp, "%s\n", 
                    DTKM_SEGLOAD_ERROR_MESSAGE( return_code ) ) ;
                fprintf(report_fp, "SKIPPING THIS RECORD\n" ) ;
            }
            continue ;
        }

        switch( return_code )
        {
        case DTKM_SEGLOAD_NOT_ADDED_NO_RECORDER :
            if ( report_fp )
            {
                fprintf(report_fp, 
                "The segment must be a recording; there is no sat recorder\n");
                fprintf(report_fp, "SKIPPING THIS RECORD\n" ) ;
            }
            continue ;
        case DTKM_SEGLOAD_NOT_ADDED_TOO_SHORT :
            if ( report_fp )
            {
                fprintf(report_fp, "The segment is too short.\n");
                fprintf(report_fp, "SKIPPING THIS RECORD\n" ) ;

            }
            continue ;
        case DTKM_SEGLOAD_SEG_NOT_ADDED_EXISTS_ALREADY :
            if ( report_fp )
            {
                fprintf(report_fp, "The segment already exists.\n");
            }
            break ;
        case DTKM_SEGLOAD_ADD_SEGMENT_OK :
            if ( report_fp )
            {
                fprintf(report_fp, "The segment was created.\n");
            }
            break ;
        default :
            sprintf( msg, 
"%s(%d):  UNKNOWN RETURN CODE %d from dtkmseg_add_segment().  Check the GUI-created log file for this segment file;  review recent code changes.\n",
                __FILE__, __LINE__, return_code );
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);

            if ( report_fp )
            {
                fprintf(report_fp, msg ) ;
            }
            continue ;
        }

        /*
        -- if this data-take is a real-time observation, then 
        -- we will later create the downlinking data-take, too, the RDL 
        -- data-take record.  The reason is to do conflicts.  
        -- but here we will put in a value for fa_schedule_link, 
        -- and the same value in the downlink, too.  This will 
        -- link them up; the APS will know that one dtk downlinks 
        -- the other.  
        */
        if( strncmp( CAST_DTK_ACTID result_dtk[DTK_ACTID], 
                     DTKM_ACTID_REALTIME_OBSERVATION_CODE,
                     strlen(DTKM_ACTID_REALTIME_OBSERVATION_CODE) ) == 0  )
        {
            /* 
            -- this is a real-time observation.  
            -- put in a value for fa_schedule_link
            */
            sprintf( CAST_DTK_FA_SCHEDULE_LINK result_dtk[DTK_FA_SCHEDULE_LINK],
                "%ld_%2.2s", 
                CAST_DTK_REV result_dtk[DTK_REV], 
                CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID] ) ;
        }
        /*
        -- add the data-take to the list to be processed; 
        */
        APPEND( dtk_proposal_list, result_dtk, free_db_record, result_dtk ) ;
        if ( report_fp )
        {
            fprintf(report_fp, 
                "The data-take is added to the list for processing:\n") ;
            dtkm_print( report_fp, result_dtk ) ;
        }

        /*
        -- if this data-take is a real-time observation, then 
        -- we will now create the downlinking data-take, too, the RDL 
        -- data-take record.  The reason is to do conflicts.  
        */
        if( strncmp( CAST_DTK_ACTID result_dtk[DTK_ACTID], 
                     DTKM_ACTID_REALTIME_OBSERVATION_CODE,
                     strlen(DTKM_ACTID_REALTIME_OBSERVATION_CODE) ) == 0  )
        {
            /* 
            -- this is a real-time observation.  
            -- now create the downlink record for it.  
            -- set up NEW storage to store the record
            */

            downlink_dtk = new_table_record(APS_CDEFS(DTK)) ;
            db_copy_record( APS_CDEFS(DTK), downlink_dtk, result_dtk ) ;
            /*
            -- use the same times; transform this copy into a 
            -- real-time downlink record.  
            */
            strcpy(CAST_DTK_SENSOR downlink_dtk[DTK_SENSOR],
                   DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;

            strncpy(CAST_DTK_ACTID downlink_dtk[DTK_ACTID],
                    DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
                    strlen(DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ) ;
            /* 
            -- set the TRANSID to "" to be picked up by dtkm_default values
            */
            strcpy( CAST_DTK_TRANSID downlink_dtk[DTK_TRANSID], "" ) ;

            return_code = dtkm_default_values( downlink_dtk, downlink_dtk ) ;
            if ( return_code < 0 )
            {
                if ( report_fp )
                {
                    fprintf(report_fp,"ERROR from dtkm_default_values():  %s\n",
                        DTKM_ERROR_MESSAGE( return_code )  ) ;
                    fprintf(report_fp, 
"Real-time downlink data-take NOT ADDED to the list for processing:\n") ;
                    dtkm_print( report_fp, downlink_dtk ) ;
                }
                continue ;
            }

            /* that is all that is needed. */
            /*
            -- add the data-take to the list to be processed; 
            */
            APPEND(dtk_proposal_list,downlink_dtk,free_db_record,downlink_dtk);
            if ( report_fp )
            {
                fprintf(report_fp, 
"This real-time downlink data-take is added to the list for processing:\n") ;
                dtkm_print( report_fp, downlink_dtk ) ;
            }
        }

        /* 
        -- allocate separate storage for the next data-take.  
        -- set up NEW storage to store the 
        -- next observation data-take, because the current 
        -- result_dtk was just APPENDed.  
        */
        result_dtk = new_table_record(APS_CDEFS(DTK)) ;

    }  /* end of the for-loop on input data-takes.    */

    /* free unneeded storage:  */
    free_db_record( result_dtk ) ;

    /*
    -- dtkm_process_dtk_proposal_list() will print the input list.
    -- now allocate the extra lists to process the data-take list 
    -- created;
    */
    deleted_dtks = create_dyn_llist() ;
    REJ_omission_dtks = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks = create_dyn_llist() ;

    return_code = dtkm_process_dtk_proposal_list(
        APS_dbproc,    dtk_proposal_list,   
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks, error_dtks,
        REJ_omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates,
        report_fp ) ;
    /* free all of the lists no longer needed.  */
    DEL_LIST( deleted_dtks ) ;
    DEL_LIST( REJ_omission_dtks ) ;
    DEL_LIST( other_sat_dtks ) ;
    DEL_LIST( same_sat_dtks ) ;
    DEL_LIST( dtk_proposal_list ) ;
    if ( return_code < 0 )
    {
        aps_log_msg(progname, APS_ERROR, DTKM_ERROR_MESSAGE( return_code ),
            DO_SYSLOG, DO_PRINT);

        if ( report_fp )
        {
            fprintf( report_fp, "ERROR:  %s\n", 
                DTKM_ERROR_MESSAGE( return_code ) ) ;
        }

        return DTKM_SEGLOAD_ERROR_PROCESSING_LIST ;
    }

    /* 
    -- copy the updated dtks with dtkstat value = QUE 
    -- into a separate list.  This includes both 
    -- real-time downlinks and observations.  
    -- actually, we would like to put only the 
    -- QUE observations into the list, but at the 
    -- moment, Tue Mar 18 20:21:30 PST 1997, there is 
    -- no utility to remove data-takes based on a value.  
    --
    -- next, update the corresponding seg record for each.  
    */
    QUE_dtks = create_dyn_llist() ;
    return_code = dtkm_copy_dtks_value( DTK_DTKSTAT, "QUE", 
        dtk_updates, QUE_dtks ) ;

    if ( report_fp && NUMELTS( QUE_dtks ) > 0 )
    {
        fprintf(report_fp, 
            "\n\nUPDATING DTKID IN SEG RECORDS IN DATABASE:\n\n");
    }

    /* now update the segment records.  */
    for (   dtk_rec = (DB_RECORD **) FIRST(QUE_dtks, QUE_dtks_ptr);
            dtk_rec != NULL ;
        )
    {
        /* 
        -- process the current dtk_rec right 
        -- here.  
        -- at this point, there is a 1 to 1 correspondence 
        -- between the QUE observation dtks in this run and the segments 
        -- that need to be updated.  
        -- if this record is a downlink, we continue to the next record.  
        */
        if( dtkm_is_a_downlink( dtk_rec ) == TRUE )
        {
            /* 
            -- we need to delete this record and continue with 
            -- the next record.  
            -- this is not nearly as easy as it sounds.  
            */

            /* must save next rec and pointer to prevent difficulties.  */
            QUE_dtks_ptr_save = QUE_dtks_ptr ;
            dtk_rec_save = (DB_RECORD **) NEXT(QUE_dtks, QUE_dtks_ptr_save) ;
 
            /* now delete the current Downlink data-take, free the storage.  */
            DEL_AT_CURSOR( QUE_dtks, QUE_dtks_ptr ) ;

            /* check for unusual circumstance:  don't trust.  */
            if( NUMELTS( QUE_dtks ) == 0 )
                break ;
 
            /* restore the correct rec and pointer to NEXT record:  */
            dtk_rec = dtk_rec_save ;
            QUE_dtks_ptr = QUE_dtks_ptr_save ;

            /* back to the top of the loop with this data-take:  */
            continue ;
        }

        return_code = dtkmseg_update_seg_dtkid( APS_dbproc, dtk_rec, 
            report_fp ) ;   
        if ( return_code < 0 )
            return return_code ;

        /* now process the next record, move to the top of the list */
        dtk_rec = (DB_RECORD **) NEXT(QUE_dtks, QUE_dtks_ptr) ;
    }

    if ( report_fp && NUMELTS( QUE_dtks ) > 0 )
    {
        fprintf(report_fp, 
            "\nTOTAL OF %d SEGMENTS UPDATED:\n\n", NUMELTS( QUE_dtks ) );
    }


    /* free the last of the lists.  */
    DEL_LIST( QUE_dtks ) ;

    return TRUE ;

}  /* end of dtk_seg_list_load  */



/*==============================================================================
Function:       dtk_seg_file_load

Description:    processes a segment file created by the APS Mapper 
that consists of data-take opportunities, turning them into data-takes 
with QUE status.  The processing includes checking for dtk within 
mask, and other stuff.  It reads the file, creates a llist of DB_RECORDS, 
and calls dtk_seg_list_load().  

Parameters:     
    DBPROCESS *APS_dbproc,        Sybase db process    
    int       seg_file_fd,        input segment file   
    FILE      *report_fp   )      output report file; if = 0, no report   

Returns:        
    OK:  
        DTK_SEG_FILE_LOAD_OK ;

    < 0  Error:

Creator:        Lawrence Stevens

Creation Date:  Fri May 19 14:11:26 PDT 1995

Notes:      
==============================================================================*/

static int dtk_seg_file_load(
    DBPROCESS *APS_dbproc,     /* Sybase db process  */
    int       seg_file_fd,     /* input segment file */
    FILE      *report_fp   )   /* output report file; if NULL, no report */
{


    int           return_code ;

    llist         *input_dtk_list ;
    DB_RECORD     **input_dtk_rec ;

    llist         *accepted_dtks ;
    llist         *rejected_dtks ;
    llist         *alter_dtks ;
    llist         *error_dtks ;
    llist         *new_dtks ;


    input_dtk_list = create_dyn_llist() ;

    for (;;) 
    {

        /*   read a seg record from the seg file.  */
        if( read(seg_file_fd, buf, SEG_FILE_REC_LEN)   < SEG_FILE_REC_LEN )
        {
            /* end of file.  */
            break ;
        }

        /*
        -- Make a new empty DBRECORD to use when reading each record from the 
        -- file.  
        */
        input_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

        /* 
        -- blank out values to prevent later problems 
        -- experienced with diagnostic and maybe other printing.  
        */
        return_code = dtkm_blank_values( input_dtk_rec, input_dtk_rec ) ;

        return_code = 
        sscanf(buf, "%ld %s %s %ld %s %s %f %f %f %f %f %f  %f %f %f %f %c",
            &CAST_DTK_DARID   input_dtk_rec[DTK_DARID],
            CAST_DTK_SAT      input_dtk_rec[DTK_SAT],
            CAST_DTK_SENSOR   input_dtk_rec[DTK_SENSOR],
            &CAST_DTK_REV     input_dtk_rec[DTK_REV],
            CAST_DTK_STRTTIME input_dtk_rec[DTK_STRTTIME],
            CAST_DTK_STOPTIME input_dtk_rec[DTK_STOPTIME],
            &CAST_DTK_STRTLAT input_dtk_rec[DTK_STRTLAT],
            &CAST_DTK_STOPLAT input_dtk_rec[DTK_STOPLAT],
            &CAST_DTK_NRLAT1  input_dtk_rec[DTK_NRLAT1],
            &CAST_DTK_NRLON1  input_dtk_rec[DTK_NRLON1],
            &CAST_DTK_FARLAT1 input_dtk_rec[DTK_FARLAT1],
            &CAST_DTK_FARLON1 input_dtk_rec[DTK_FARLON1],
            &CAST_DTK_NRLAT2  input_dtk_rec[DTK_NRLAT2],
            &CAST_DTK_NRLON2  input_dtk_rec[DTK_NRLON2],
            &CAST_DTK_FARLAT2 input_dtk_rec[DTK_FARLAT2],
            &CAST_DTK_FARLON2 input_dtk_rec[DTK_FARLON2],
            &CAST_DTK_ASCDSC  input_dtk_rec[DTK_ASCDSC]    ) ;

#ifdef DEBUG
        printf("%s(%d):  return code %d from sscanf\n", __FILE__, __LINE__, 
            return_code);
        printf("DBRECORD from file:\n") ;
        db_print_record( input_dtk_rec, APS_CDEFS(DTK) ) ;
        dtkm_print(stdout, input_dtk_rec ) ;
#endif

        if ( return_code != 17)
            return DTKM_SEGLOAD_ERROR_SSCANF ;

        /* set status of data-take from segment file:  */
        strcpy(CAST_DTK_DTKSTAT  input_dtk_rec[DTK_DTKSTAT], "QUE" ) ;

        /* append the rec to the list:  */
        APPEND( input_dtk_list, input_dtk_rec, free_db_record, input_dtk_rec) ;

        /* continue with the next record until end of file.  */

    }  /* end of for - should be EOF  */

    accepted_dtks = create_dyn_llist() ;
    rejected_dtks = create_dyn_llist() ;
    alter_dtks    = create_dyn_llist() ;
    error_dtks    = create_dyn_llist() ;
    new_dtks    = create_dyn_llist() ;

#ifdef DEBUG
    printf("dtk_seg_file_load:  number of input segments: %d\n",
        NUMELTS( input_dtk_list ) ) ;
    dtkm_print_list(stdout, input_dtk_list ) ;
#endif

    if ( NUMELTS( input_dtk_list ) <= 0 )
    {
        /* no data-takes to process.  */
        fprintf( report_fp,"NO SEGMENTS TO PROCESS\n" ) ;
        return TRUE ;
    }

    return_code = dtk_seg_list_load(APS_dbproc, input_dtk_list, 
        accepted_dtks, rejected_dtks, alter_dtks, error_dtks, new_dtks, 
        report_fp ) ;

    DEL_LIST ( input_dtk_list ) ;
    DEL_LIST ( accepted_dtks ) ;
    DEL_LIST ( rejected_dtks ) ;
    DEL_LIST ( alter_dtks ) ;
    DEL_LIST ( error_dtks ) ;
    DEL_LIST ( new_dtks ) ;

    if ( return_code < 0 )
    {
        return return_code ;
    }
        
    return TRUE ;

}  /* end of dtk_seg_file_load  */

/*==============================================================================
Function:       Main routine:  main()

Description:    
    load segments from a segment file created by the MAPPER.  
    This routine handles the command line arguments, opens the database, 
    opens the file, then calls dtk_seg_file_load to perform the functions.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri May 19 14:25:24 PDT 1995

Notes:      
==============================================================================*/


main(int argc, char *argv[])
{

    void        usage_exit(char *progname ) ;
    DBPROCESS   *APS_dbproc;
    RETCODE     return_code;

    extern int  init_vec_lib();      /* initializes vector lib with stoicfile */
    extern int  init_vec_lib_exit(int); /* prints msg, exits if stoic error */

    char    *dbname = NULL ;
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *segfile = NULL ;
     
    int     fd;
    int     rcode ;

    double  et1 ;

    int     c;      /* used as return character from getopt()       */
    int     j;

    int     Pflag = 0;  /* used to check for mandatory password     */
    int     Uflag = 0;  /* used to check for optional sybase_userid */
    int     pflag = 0;  /* used to check for optional permission_id */

    char    flag_list[20] = "p:U:P:"; /* list of flags for getopts  */
     
    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */
    int     permission_id   = 0 ;  /* optional permission_id passed     */

    /* permission values needed.  */
    int     darid   = 0 ; 
    /* 
    -- declare 3 strings as pointers so that later, possibly, 
    -- they can be set to NULL.  
    */
    char    *min_strttime = "9999:ddd:hh:mm:ss.sss" ;
    char    *max_stoptime = "1111:ddd:hh:mm:ss.sss" ;
    char    *station_id = "ALL" ;
    char    strttime[ASF_TIME_STR_LENGTH+1] = "yyyy:ddd:hh:mm:ss.sss" ;
    char    stoptime[ASF_TIME_STR_LENGTH+1] = "yyyy:ddd:hh:mm:ss.sss" ;

    /*
    -- Activity id for create data-takes from dar
    -- (segment file loader)
    -- is a DAR and a planning activity.
    */
    char    *mu_activity_id   = MU_CREATE_DATATAKES_FROM_DAR ;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    /* 
    -- get the command name, without path, for the progname:
    -- find last occurrence of '/', if any, 
    -- in the string:  
    */
    progname = strrchr( argv[0], '/' ) ;
    if( progname == NULL )
        progname = argv[0] ;
    else
        progname ++ ;

    /* open aps_syslog */
    aps_open_syslog();
    sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(msg, " " ) ;
        strcat(msg, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
          
    /* 
    -- check for a dash '-' at the start of the last  
    -- argument which should be a file name, not a dash:   
    */
    if(*argv[argc-1] == '-')
        usage_exit(progname);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'P':
                if(Pflag != 0)
                    usage_exit(progname);
                Pflag++;
                password = optarg ;
                break;
            case 'U':
                if(Uflag != 0)
                    usage_exit(progname);
                Uflag++;
                sybase_userid = optarg ;
                break;
            case 'p':
                if(pflag != 0)
                    usage_exit(progname);
                pflag++;
                return_code = sscanf( optarg, "%d", &permission_id ) ;
                if( return_code != 1 )
                {
                    fprintf(stderr,
                    "%s(%d): error: %s :  permission id must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case '?':
                usage_exit(progname);
                break;
            default:
                /* do  nothing  */
                break;
        }

    /* check for extraneous words not attached to a flag.   */
    if(optind != argc - 1)
        usage_exit(progname);

    /* manditory flag:  */
    if(Pflag <= 0)
        usage_exit(progname);
     
    /* optional flag    */
    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            sprintf(msg, "sybase_userid not found in environment variable APS_SYBASE_USERID.  Use -U sybase_userid or setenv APS_SYBASE_USERID.");
            aps_log_msg( progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR) ;
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
        sprintf(msg, "ERROR:  dbname not found in environment variable APSDB.  Use setenv APSDB dbname. ");
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    dbname = env_dbname ;

    if( (int) strlen(argv[argc-1]) > 150)
    {
        sprintf(msg, "dtk_seg_file_main.c:  ERROR:  first argument is longer than 150 characters.   Expecting the name of the input segment file created by the mapper.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }


    rcode = system("banner DTKM SEGLOAD");
    printf("\t[%s  Version Dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;



    /* initialize the vector library; exit with a message if an error   */
    rcode = init_vec_lib();
    if(rcode)
    {
        aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(APS_EXIT_ERROR);
    }

    segfile = argv[argc - 1] ;

    fd = open(segfile, O_RDONLY);
    if(fd <= 0)
    {
        sprintf(msg, 
"%s(%d):  ERROR on opening the input segment file: \n%s\n  Check existence and permissions.", 
            __FILE__, __LINE__, segfile ) ;

        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    /* the segment file is now open.  now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open(dbname,"dtkm_segload",sybase_userid,password,NULL,
        error_handler_exit,&rcode);
    if(rcode != DB_OPEN_OK)
    {
        db_open_errs( rcode, dbname, sybase_userid);
        sprintf( msg, "ERROR:  dbname %s could not be opened", dbname ) ; 
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    /************************************************************************
    *                                                                       *
    *                 GET PERMISSIONS                                       *
    *                                                                       *
    ************************************************************************/
    /* 
    -- obtain permission for both the DAR and the 
    -- planning activities.  
    */
    if( pflag != 0 )
    {
        /*
        -- permission_id is passed in the command line.  
        -- this means that the permissions have all been 
        -- obtained.  Therefore, we do not need to obtain 
        -- the darid or any of the planning permission 
        -- values.  Set then to 0 or NULL.  
        -- the 3 character strings were declared as pointers, 
        -- not arrays, so that we could do this.  
        */
        darid = 0 ;
        min_strttime = NULL ;
        max_stoptime = NULL ;
        station_id = NULL ;
    }
    else
    {
        /* 
        -- permission_id NOT given in the command line.  
        -- NEED to obtain darid to be able to request 
        -- permission.  Also will need to obtain start and stop 
        -- time bracket.  
        */
        /*
        -- determine darid from the file.  
        */
        /*   read the first seg record from the seg file.  */
        if( read(fd, buf, SEG_FILE_REC_LEN)   < SEG_FILE_REC_LEN )
        {
            /* 
            -- end of file.
            -- error:  premature end of file.  
            */
            sprintf( buf, 
                "ERROR:  end of file on reading first record of input file.\n");
            fprintf( stderr, "%s:\n\n%s\n", progname, buf ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR) ;
        }
        return_code = sscanf(buf, "%d ", &darid ) ;
        if( return_code != 1)
        {
            /* ERROR decoding the darid.  */
            sprintf( buf, 
                "error decoding darid in first record of input file.\n" ) ;
            fprintf( stderr, "%s:\n\n%s\n", progname, buf ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR) ;
        }
        /* darid obtained OK.  */

        /* done.  rewind the file to start for the next step.  */
        if( lseek(fd, (off_t)0, SEEK_SET ) == -1 )
        {
            /*  error rewinding file.  */
            sprintf( buf, "error rewinding input file.\n" ) ;
            fprintf( stderr, "%s:\n\n%s\n", progname, buf ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR) ;
        }
        /*
        -- determine min strttime and max stoptime of the file.  
        */

        for (;;) 
        {

            /*   read a seg record from the seg file.  */
            if( read(fd, buf, SEG_FILE_REC_LEN)   < SEG_FILE_REC_LEN )
            {
                /* end of file.  */
                break ;
            }
            return_code = sscanf(buf, "%*ld %*s %*s %*ld %s %s ", 
                strttime, stoptime ) ;
            if( return_code != 2)
            {
                /* ERROR decoding the strttime and stoptime.  */
                sprintf( buf, 
                    "error decoding strttime and stoptime in input file.\n" ) ;
                fprintf( stderr, "%s:\n\n%s\n", progname, buf ) ;
                aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
                error_exit(progname, APS_EXIT_ERROR) ;
            }

            /* update min and max if necessary:  */
            if( strcmp( min_strttime, strttime ) > 0 )
                strcpy( min_strttime, strttime ) ;

            if( strcmp( max_stoptime, stoptime ) < 0 )
                strcpy( max_stoptime, stoptime ) ;
        }
        /* 
        -- done.  rewind the file to initialize 
        -- for processing.  
        */
        if( lseek(fd, (off_t)0, SEEK_SET ) == -1 )
        {
            /*  error rewinding file.  */
            sprintf( buf, "error rewinding input file.\n" ) ;
            fprintf( stderr, "%s:\n\n%s\n", progname, buf ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR) ;
        }
    }
    /*
    -- now darid, min_strttime, max_strttime, 
    -- and station_id all have the appropriate values, 
    -- They are 0 or NULL if permission has already 
    -- been granted, with permission_id being passed
    -- on the command line.  
    -- The values have been determined from the input file 
    -- if permission has not yet been granted.  
    */

    /*
    -- At this point, we need to either validate a permission 
    -- or to request a new permission.  
    -- mu_get_permission() takes care of
    -- the logic and the syslog printing.  for
    -- example, it will determine to request a new 
    -- permission or to validate an old permission.  
    -- it is all taken care of in mu_get_permission()
    */
    return_code = mu_get_permission( progname, APS_dbproc,
        permission_id, MU_DAR_ACTIVITY_TYPE, mu_activity_id,
        NULL, NULL, NULL, /* parameters not used for DAR permission.       */
        darid,    /* value will be 0 if permission_id passed in cmd line.  */
        0, 0 ) ;  /* parameters not used here (no retries).  */
    if ( return_code < 0 )
    {
        /* permission not granted/validated.  */
        error_exit (progname, APS_EXIT_ERROR);
    }
    /* permission obtained; set id.  */
    permission_id = return_code ;

    /* 
    -- now obtain the planning activity.  Note that the 
    -- permission_id value already obtained for the 
    -- DAR activity will also be used for the planning 
    -- activity.  At this point, the permission_id will 
    -- always be != 0
    */

    /*
    -- At this point, we need to either validate a permission 
    -- or to request a new permission.  
    -- mu_get_permission() takes care of
    -- the logic and the syslog printing.  for
    -- example, it will determine to request a new 
    -- permission or to validate an old permission.  
    -- it is all taken care of in mu_get_permission()
    */
    return_code = mu_get_permission( progname, APS_dbproc,
        permission_id, MU_PLANNING_ACTIVITY_TYPE, mu_activity_id,
        min_strttime, /* value will be NULL if permission_id passed in cmd  */
        max_stoptime, /* value will be NULL if permission_id passed in cmd  */
        station_id,   /* value will be NULL if permission_id passed in cmd  */
        0,       /* darid not used in planning permission.  */
        0, 0 ) ;  /* parameters not used here (no retries).  */
    if ( return_code < 0 )
    {
        /* permission not granted/validated.  */
        error_exit (progname, APS_EXIT_ERROR);
    }
    /* permission obtained; set id.  */
    permission_id = return_code ;

    /* access the vector lib. just to get the WARNING messages out of the way.*/
    return_code = tc_asf2et ( "1994:001:00:00:00.000", &et1 ) ;

    /* print the heading for the log file.  */

    printf("\nSeg File name:  %s\nUpdate to datebase:  %s \n",
            segfile, dbname );

    rcode = system("date") ;
    printf("\n") ;

    return_code = dtk_seg_file_load(APS_dbproc, fd, stdout ) ;
    /* DO NOT CHANGE return_code */
    /* record time, then check return_code.  */
    printf("\n") ;
    /* DO NOT CHANGE return_code */
    rcode = system("date") ;
    /* DO NOT CHANGE return_code */
    if ( return_code < 0 )
    {
        fprintf( stderr, "%s:\n\n%s\n", progname, 
            DTKM_SEGLOAD_ERROR_MESSAGE(return_code) ) ;
        fprintf( stdout, "%s\n", DTKM_SEGLOAD_ERROR_MESSAGE(return_code) ) ;
        aps_log_msg(progname, APS_ERROR,
            DTKM_SEGLOAD_ERROR_MESSAGE(return_code),
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(progname, APS_ERROR, 
    "processing segment file.  Check the GUI-created log file error messages for this input file",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    /*
    -- Still OK.
    -- Terminate the permissions if they were NOT
    -- passed on the command line.
    -- (That is, if they were requested by and 
    -- granted in this routine.)
    */
    if ( pflag == 0 )
    {
        /*
        -- permission_id was NOT in command line; Terminate the
        -- permission_id; the permission was granted here.
        */
        /* DAR activity first:  */
        return_code = mu_permission_terminate( APS_dbproc,
            permission_id,
            mu_activity_id,
            MU_DAR_ACTIVITY_TYPE ) ;
        if( return_code < 0 )
        {
            fprintf( stderr,
                "%s:\n\nTrying to terminate DAR activity permission:\n%s\n", 
                progname, MU_ERROR_MESSAGE(return_code) ) ;
            sprintf( buf, "ERROR:  %s", MU_ERROR_MESSAGE(return_code) ) ;
            aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit (progname, APS_EXIT_ERROR);
        }
        /* planning activity next:  */
        return_code = mu_permission_terminate( APS_dbproc,
            permission_id,
            mu_activity_id,
            MU_PLANNING_ACTIVITY_TYPE ) ;
        if( return_code < 0 )
        {
            fprintf( stderr,
                "%s:\n\nTrying to terminate planning activity permission:\n%s\n",
                progname, MU_ERROR_MESSAGE(return_code) ) ;
            sprintf( buf, "ERROR:  %s", MU_ERROR_MESSAGE(return_code) ) ;
            aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit (progname, APS_EXIT_ERROR);
        }
        /* OK.  */
    }

    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);

    rcode = system("banner 'NORMAL END'") ;

    return APS_EXIT_OK ;

}   /* end of the main() dtk_seg_file_main    */
