#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_migrate.c

Description:    for each record retrieved, migrate according to R2.1.2 delivery.

==============================================================================*/
#pragma ident   "@(#)stats_migrate.c	1.1 98/03/05 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/migrate/SCCS/s.stats_migrate.c"


/*==============================================================================
Function:       stats_migrate()

Description:    for each record retrieved, migrate according to R2.1.2 delivery.
                This means:  
                1.  stats_call relation has already been created.  
                2.  the fields fa_strttime, fa_stoptime, fa_duration_min, 
                    and asf_reduction_min have already been added to the dtk 
                    structure.  the times are "" string values, and the 
                    _min values are 0.0.  

                3.  This migration is done for every dtk record satisfying 
                    the sat value and >= first_rev and <= last_rev

                4.  The migration taking place in this routine is, for each 
                    retrieved record, to copy strttime and stoptime to 
                    fa_strttime and fa_stoptime.  Then compute 
                    fa_duration_min and set asf_reduction_min to 0.
                    Then save the new record.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Thu Feb 26 14:29:49 PST 1998

Notes:          
==============================================================================*/
#include <string.h>         /*    for strcpy() etc.                   */
#include <db_sybint.h>      /*    for DBPROCESS                       */
#include <timeconv.h>       /*    for tc_et_ASF_datetime_diff() etc.  */
#include <aps_db_table.h>   /*    for DTK etc.                        */
#include <db_dtk.h>         /*    for CAST_FA_STRTTIME etc.           */
int stats_migrate( 
    DBPROCESS   *APS_dbproc,
    FILE        *logfp,
    char        *sat, 
    int         first_rev,
    int         last_rev,
    int         *error_count )
{
    llist       *dtk_list ;
    cursor      dtk_list_ptr ;
    DB_RECORD   **dtk_rec ;
    char        add_field_to_set[50] ;
    double      delta_days ;
    int         nrecs ;
    int         nrecs_updated = 0 ;

    *error_count = 0 ;

    (void) sprintf(where_clause, "where %s = '%s' and %s >= %d and %s <= %d ",
        APS_COL(DTK, DTK_SAT), sat, 
        APS_COL(DTK, DTK_REV), first_rev, 
        APS_COL(DTK, DTK_REV), last_rev  ) ;
    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if (dtk_list == NULL)
    {
        (void) fprintf(logfp, "DB ERROR:  query failed %s.\n", where_clause ) ;
        return -1 ;
    }

    if( NUMELTS( dtk_list ) <= 0 )
    {
        DEL_LIST(dtk_list) ;
        return 0 ;
    }

    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        (void)fprintf(logfp, "processing dtk record %s/%ld/%d\n", 
            CAST_DTK_SAT dtk_rec[DTK_SAT],
            CAST_DTK_REV dtk_rec[DTK_REV],
            CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;

        /*  
        --  The migration taking place in this routine is, for each 
        --  retrieved record, to copy strttime and stoptime to 
        --  fa_strttime and fa_stoptime.  Then compute 
        --  fa_duration_min and set asf_reduction_min to 0.
        --  Then save the new record.  
        */
        (void) strcpy( CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME], 
                       CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;
        (void) strcpy( CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME], 
                       CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;

        (void) tc_et_ASF_datetime_diff( 
            CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
            CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
            &delta_days ) ;

        /* save this value in minutes:  */
        CAST_DTK_FA_DURATION_MIN dtk_rec[DTK_FA_DURATION_MIN] 
            = delta_days * 24.0 * 60.0 ;

        CAST_DTK_ASF_REDUCTION_MIN dtk_rec[DTK_ASF_REDUCTION_MIN] = 0.0 ;

        /* 
        -- record values updated OK.  
        -- save dtk record.  
        --
        -- now get ready for a call to db_update_records().
        -- first, set up the where_clause to identify the
        -- record to the database by its primary keys.
        */
        (void) sprintf(where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV dtk_rec[DTK_REV],
            APS_COL(DTK,DTK_DTKID), CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;

        /* 
        -- now, we need to set up fields_to_set[] which will be a
        -- long string like this:
        -- dtk.fa_strttime = "1996:001:00:00:00.000",
        -- dtk.fa_stoptime = "1996:001:00:00:00.000"
        --
        -- etc.  
        */

        /*  34 fa_strttime        AsftimeType      21   */
        (void)sprintf(fields_to_set, "%s = '%s', \n",
            APS_COL(DTK, DTK_FA_STRTTIME),
            CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME] ) ;
 
        /*  35 fa_stoptime        AsftimeType      21   */
        (void)sprintf(add_field_to_set, "%s = '%s', \n",
            APS_COL(DTK, DTK_FA_STOPTIME),
            CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME] ) ;
        (void) strcat( fields_to_set, add_field_to_set ) ;
 
        /*  36 fa_duration_min    real              4   */
        (void)sprintf(add_field_to_set, "%s = %.4f \n",
            APS_COL(DTK, DTK_FA_DURATION_MIN),
            CAST_DTK_FA_DURATION_MIN dtk_rec[DTK_FA_DURATION_MIN] ) ;
        (void) strcat( fields_to_set, add_field_to_set ) ;

        /*  37 asf_reduction_min  real              4   */
        (void)sprintf(add_field_to_set, ", %s = %.4f \n",
            APS_COL(DTK, DTK_ASF_REDUCTION_MIN),
            CAST_DTK_ASF_REDUCTION_MIN dtk_rec[DTK_ASF_REDUCTION_MIN] ) ;
        (void) strcat( fields_to_set, add_field_to_set ) ;

        nrecs = db_update_records(APS_dbproc, APS_TABLE(DTK),
            fields_to_set, where_clause);
        if( nrecs < 0 )
        {
            DEL_LIST(dtk_list ) ;
            return -1 ;
        }

        if ( nrecs != 1 )
            (*error_count) ++ ;
        else
            nrecs_updated ++ ;

    }

    DEL_LIST( dtk_list ) ;

    return nrecs_updated ;

}
