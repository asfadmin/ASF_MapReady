#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       crt_grs_reqq.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)crt_grs_reqq.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.crt_grs_reqq.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <db_sybint.h>      /* for DBPROCESS */
#include <aps_log_msg.h>
#include <apspath.h>
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH */
#include <timeconv.h>       /* for tc_validate_asf_datetime() etc. */
#include <aps_db_table.h>   /* for DAR, DTK */
#include <db_dar.h>         /* for CAST_DAR_SAT etc. */

#include "list_handling.h"
#include "crt_grs_reqq.h"

/* REQQ observing period is:  yyyymmddYYYYMMDD    */
#define REQQ_OBS_PERIOD_STR_LEN    16


/*==============================================================================
Function:      crt_grs_reqq 

Description:    Reads a REQQ file and appends DAR requests on to 
                the end of if.  

Returns:        0  Normal completion 
               <0  Error status.

Creator:        Miguel Siu

Creation Date:  Thu Sep 11 15:56:45 PDT 1997

Notes:          
==============================================================================*/
int crt_grs_reqq(   
    FILE    *reqq_fp,      /* pointer to REQQ file.                   */
    FILE    *logfp,        /* log file.  could be stdout              */
    char    *phase_start,  /* start time of request period (phase).   */
    char    *phase_end,    /* end time of request period (phase)      */
    int     phase )        /* phase number                            */ 
{
    long        darid;
    PATH        *grslist;
    int         list_status;
    int         time_errors = 0 ;
    char        *observe_period[94];
    DB_RECORD   **dar_rec;
    llist       *dar_list;
    cursor      *dar_list_ptr;
    int         j ;
    int         J1_obs_freq_days ;

    char        dar_start[ASF_TIME_STR_LENGTH+1] ;
    char        dar_end[ASF_TIME_STR_LENGTH+1] ;
    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;
    int         images = 1;
    int         observe_period_size ;

    /* malloc().  free() later.  */
    observe_period_size = 94 ;
    for (j=0; j < observe_period_size; j++) 
        observe_period[j] = malloc(REQQ_OBS_PERIOD_STR_LEN+1 * sizeof(char));

    tc_systime2asf( now_asftime );


    /* retrieve dar recs only if the J1_obs_freq > 0   */
    (void) sprintf( where_clause, 
    "where %s < '%s' and %s > '%s' and %s = 'J1' and %s = 'SAR' and %s = 'PLN' and %s > 0 ",
        APS_COL(DAR, DAR_STRTTIME), phase_end,
        APS_COL(DAR, DAR_ENDTIME), phase_start,
        APS_COL(DAR, DAR_SAT),
        APS_COL(DAR, DAR_SENSOR),
        APS_COL(DAR, DAR_REQSTAT),
        APS_COL(DAR, DAR_J1_OBS_FREQ)  );

    dar_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DAR),
        where_clause, NULL, APS_CDEFS(DAR), ALL_COLS);
    if ( dar_list == NULL )
    {
        if( logfp ) 
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_DB_QUERY_ERROR ),
                where_clause ) ;
        /* free  */
        for (j=0; j < observe_period_size; j++) 
            free(observe_period[j]) ;
        return LOAD_DAR_ERROR_DB_QUERY_ERROR ;
    }

    if( NUMELTS( dar_list ) < 0 )
    {
        if( logfp )
            (void) fprintf( logfp, "ERROR:  %s %s\n", 
                CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_DAR_NOT_FOUND),
                where_clause ) ;
        DEL_LIST( dar_list ) ;
        /* free  */
        for (j=0; j < observe_period_size; j++) 
            free(observe_period[j]) ;
        return LOAD_DAR_ERROR_DAR_NOT_FOUND ;
    }

    if (NUMELTS(dar_list) == 0)
    {
        grslist = malloc(sizeof(PATH));
        initialize(grslist); 
        REQQ_append(reqq_fp, 0, grslist, 0, 0, (char **) NULL, logfp);
        free_list(grslist); 

        DEL_LIST( dar_list ) ;
        /* free  */
        for (j=0; j < observe_period_size; j++) 
            free(observe_period[j]) ;
        return 0 ;
    }

    for (dar_rec = FIRST(dar_list, dar_list_ptr) ; 
         dar_rec ;
         dar_rec = NEXT(dar_list, dar_list_ptr) )
    {

        darid = CAST_DAR_DARID dar_rec[DAR_DARID];
        J1_obs_freq_days 
            = CAST_DAR_J1_OBS_FREQ dar_rec[DAR_J1_OBS_FREQ] ;

        (void) fprintf(logfp, "\nNow processing dar: %ld\n", darid);

        /* check start time */
        if ( tc_validate_asf_datetime( CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] )
            != TRUE )
        {
            if( logfp )
                (void) fprintf( logfp, "ERROR:  %s   \nstrttime = %s\n", 
                    CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_BAD_STRTTIME ),
                    CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ) ;

            /* skip to the next DAR.  */
            time_errors++ ;
            continue ;
        }

        /* check end time */
        if ( tc_validate_asf_datetime( CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] )
             != TRUE )
        {
            if( logfp )
                (void) fprintf( logfp, "ERROR:  %s   \nendtime = %s\n", 
                    CALC_GRS_ERROR_MESSAGE( LOAD_DAR_ERROR_BAD_ENDTIME ),
                    CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] ) ;

            /* skip to the next DAR.  */
            time_errors++ ;
            continue ;
        }

        /* start and stop times were OK.  */
        (void) strcpy(dar_start, CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME]);
        (void) strcpy(dar_end, CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME]);

        /* develop list of observing time periods.  */
        images = get_observe_times(
            phase_start, phase_end, dar_start, dar_end,
            J1_obs_freq_days, observe_period, observe_period_size ) ;

        /*
        --- Create the list and see if any errors occured with the load
        --- or the creation.  If not then append the list, otherwise go
        --- to next dar on command line.
        */

        grslist = malloc(sizeof(PATH));
        initialize(grslist); 

        list_status = create_grs_list(darid, grslist, logfp);

        if (list_status == 1)
            REQQ_append(reqq_fp, darid, grslist, phase, images,
                observe_period, logfp);
        else if ((list_status == STATION_MASK_FLAG_CIRCLE_IN_MASK) ||
            (list_status == STATION_MASK_FLAG_QUAD_IN_MASK) ||
            (list_status == STATION_MASK_FLAG_MASK_IN_QUAD_DAR))
        {
             (void) fprintf(logfp, "DAR not appended:  %s \n\n",
                CALC_GRS_ERROR_MESSAGE(list_status));
        }
        else
            (void) fprintf(logfp, "ERROR:  %s \n\n",
                CALC_GRS_ERROR_MESSAGE(list_status));

        free_list(grslist); 

    }  /* end for loop.  */

    /* free  */
    for (j=0; j < observe_period_size; j++) 
        free(observe_period[j]) ;
    DEL_LIST( dar_list ) ;

    if( time_errors )
        return LOAD_DAR_ERROR_CHECK_LOG_FILE ;

    return 0 ;

}

