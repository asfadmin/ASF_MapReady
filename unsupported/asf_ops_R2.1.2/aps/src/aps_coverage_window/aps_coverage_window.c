#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_coverage_window.c

External Functions Defined:
aps_coverage_window()
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)aps_coverage_window.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_coverage_window/SCCS/s.aps_coverage_window.c"

#include "aps_coverage_window.h"
#include <db_satsensor.h>       /* for satsensor relation.     */
#include <db_phase.h>           /* for phase relation.     */
#include <db_cvrg.h>            /* for cvrg relation.          */
#include <db_cvrgmdat.h>        /* for cvrgmdat relation.      */
#include <db_maskinout.h>       /* for maskinout relation.     */
#include <db_maskinout_mdat.h>  /* for maskinout_mdat relation.     */

static char msg[MSG_LEN+1] ;  /* used for log messages.   */


/*==============================================================================
Function:       get_maskinout_run_strttime()

Description:    for the sat, determine the start time to 
                extend the planning window.  This means extending 
                an existing maskinout run from some point in the future 
                to some point further into the future.  

Creator:        Lawrence Stevens

Creation Date:  Fri May  2 23:28:16 PDT 1997

==============================================================================*/
static int 
get_maskinout_run_strttime( 
    char    *sat,
    char    *asftime_stamp,           /* this is the present run time.    */
    char    *maskinout_run_strttime,
    FILE    *logfp )
{
    int     return_code ;

    DB_RECORD   **maskinout_mdat_rec = NULL ;
    llist       *maskinout_mdat_list ;
    cursor      maskinout_mdat_list_ptr ;

    /* 
    -- get the latest (future) maskinout_mdat.stop_time for 
    -- this sat/sensor to extend only runs for the FUTURE, not 
    -- to extend runs for past times.  
    */
    (void) sprintf(where_clause, "where %s = '%s' and %s > '%s'",
        APS_COL(MASKINOUT_MDAT, MASKINOUT_MDAT_SAT), sat,
        APS_COL(MASKINOUT_MDAT, MASKINOUT_MDAT_STOP_TIME), asftime_stamp ) ;

    (void) sprintf(orderby_cols, "%s",
        APS_COL(MASKINOUT_MDAT, MASKINOUT_MDAT_STOP_TIME) ) ;

    maskinout_mdat_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(MASKINOUT_MDAT), where_clause, orderby_cols, 
        APS_CDEFS(MASKINOUT_MDAT), ALL_COLS) ;
    if ( maskinout_mdat_list == NULL)
    {
        (void) sprintf(msg, "ERROR: DB query failure %s order by %s", 
            where_clause, orderby_cols ) ;
        aps_log_msg( "", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    /* if there are no runs at all, then use current time.  */
    if ( NUMELTS( maskinout_mdat_list ) <= 0 )
    {
        return_code = tc_systime2asf(maskinout_run_strttime) ;
        DEL_LIST( maskinout_mdat_list ) ;
        return TRUE ;
    }

    maskinout_mdat_rec = 
        (DB_RECORD **) LAST(maskinout_mdat_list, maskinout_mdat_list_ptr);

    /* 
    -- subtract 2 minutes, in units of days, from 
    -- the last stop time.  
    */
    return_code = tc_asf_add_ndays( 
        CAST_MASKINOUT_MDAT_STOP_TIME 
            maskinout_mdat_rec[MASKINOUT_MDAT_STOP_TIME],
        (double) -2.0/24/60, maskinout_run_strttime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, 
            "ERROR: subtracting %f days = 2 minutes from current time %s",
            (double) -2.0/24/60, 
            CAST_MASKINOUT_MDAT_STOP_TIME 
                maskinout_mdat_rec[MASKINOUT_MDAT_STOP_TIME] ) ;
        aps_log_msg( "", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    DEL_LIST( maskinout_mdat_list ) ;
    return return_code ;

}


/*==============================================================================
Function:       get_coverage_run_strttime()

Description:    for the sat/sensor, determine the start time to 
                extend the planning window.  This means extending 
                an existing coverage from some point in the future 
                to some point further into the future.  

Creator:        Lawrence Stevens

Creation Date:  Thu Jun  6 09:53:47 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

static int 
get_coverage_run_strttime( 
    char    *sat,
    char    *sensor,
    char    *asftime_stamp,           /* this is the present run time.    */
    char    *coverage_run_strttime,
    FILE    *logfp )
{
    int     return_code ;

    DB_RECORD   **cvrgmdat_rec = NULL ;
    llist       *cvrgmdat_list ;
    cursor      cvrgmdat_list_ptr ;

    /* 
    -- get the latest (future) cvrgmdat.stop_time for 
    -- this sat/sensor to extend only runs for the FUTURE, not 
    -- to extend runs for past times.  
    */
    (void) sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s > '%s'",
        APS_COL(CVRGMDAT, CVRGMDAT_SAT), sat,
        APS_COL(CVRGMDAT, CVRGMDAT_SENSOR), sensor,
        APS_COL(CVRGMDAT, CVRGMDAT_STOP_TIME), asftime_stamp ) ;

    (void) sprintf(orderby_cols, "%s",
        APS_COL(CVRGMDAT, CVRGMDAT_STOP_TIME) ) ;

    cvrgmdat_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(CVRGMDAT), where_clause, orderby_cols, 
        APS_CDEFS(CVRGMDAT), ALL_COLS) ;
    if ( cvrgmdat_list == NULL)
    {
        (void) sprintf(msg, "ERROR: DB query failure %s order by %s", 
            where_clause, orderby_cols ) ;
        aps_log_msg( "", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    /* if there are no runs at all, then use current time.  */
    if ( NUMELTS( cvrgmdat_list ) <= 0 )
    {
        return_code = tc_systime2asf(coverage_run_strttime) ;
        DEL_LIST( cvrgmdat_list ) ;
        return TRUE ;
    }

    cvrgmdat_rec = (DB_RECORD **) LAST(cvrgmdat_list, cvrgmdat_list_ptr);

    /* 
    -- subtract 2 minutes, in units of days, from 
    -- the last stop time.  
    */
    return_code = tc_asf_add_ndays( 
        CAST_CVRGMDAT_STOP_TIME cvrgmdat_rec[CVRGMDAT_STOP_TIME],
        (double) -2.0/24/60, coverage_run_strttime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, 
            "ERROR: subtracting %f days = 2 minutes from current time %s",
            (double) -2.0/24/60, 
            CAST_CVRGMDAT_STOP_TIME cvrgmdat_rec[CVRGMDAT_STOP_TIME] ) ;
        aps_log_msg( "", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    DEL_LIST( cvrgmdat_list ) ;
    return return_code ;

}


/*==============================================================================
Function:       copyfile2fp()

Description:    read from a file (filename) and copy contents into a 
                file already opened (file pointer).  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 17:31:55 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int 
copyfile2fp(
    FILE    *fp,        /* write each record from filename into this file.  */
    char    *filename ) /* open and read this file and close it.            */
{
    FILE    *readfp ;

#define READ_BUFSIZE 100

    char    readbuf[READ_BUFSIZE+1] ;
    int     file_cannot_be_read ; 
    int     rbytes ; 
    int     wbytes ; 

    (void) fprintf(fp, "\n\n" ) ;
    (void) fprintf(fp, "Contents of file:  \n%s\n", filename ) ;
    /* 
    -- make sure that the filename file can be read.
    */
    aps_access( filename, "r", &file_cannot_be_read ) ;
    if ( file_cannot_be_read ) 
        return FALSE ;
 
    /*  Open the file  */
    if ((readfp = fopen(filename, "r")) == NULL)
        return FALSE ;

    while( rbytes = fread( readbuf, 1, READ_BUFSIZE, readfp )  )
    {
        /* 
        -- rbytes were read, now write them to output.  
        */
        wbytes = fwrite( readbuf, 1, rbytes, fp ) ;
        if ( wbytes < rbytes )
            return FALSE ;
    }

    (void) fprintf(fp, "END of C-shell script file\n" ) ;
    (void) fprintf(fp, "\n\n" ) ;
    return TRUE ;

}

/*==============================================================================
Function:       cshfilename2log()

Description:    take a csh full path filename like $APS_DATA/tmp/xxxxx.csh 
                and convert it to $APS_DATA/logs/xxxxx.LOG

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 17:07:02 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int 
cshfilename2log( 
    char    *csh_filename, 
    char    **aps_fullpath_LOG_filename )
{
    int     strlength;      /* length of csh file name.  */
    char    LOG_filename[100];   

    strlength = strlen( csh_filename ) ;

    /* just make sure the input filename ends in ".csh"  */
    if( strncmp(csh_filename+(strlength-4), ".csh", 4 ) != 0 )
        return FALSE ;

    (void) strcpy( LOG_filename, csh_filename ) ;
    (void) strcpy( LOG_filename+(strlength-4), ".LOG" ) ;

    /* 
    --  make full path file name 
    --  this is a malloc() and must be freed later.  
    */
    *aps_fullpath_LOG_filename = aps_fullpath(APS_LOGS, LOG_filename) ;

    return TRUE ;

}


/*==============================================================================
Function:       delete_old_maskinout_records()

Description:    deletes old maskinout records from the APS db.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 16:09:22 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int
delete_old_maskinout_records(
    char        *progname,
    DBPROCESS   *APS_dbproc,
    char        *asftime_stamp,
    int         mdays,
    FILE        *logfp )
{
    int     nrecs_deleted ;
    int     return_code ;

    char    del_asftime[ASF_TIME_STR_LENGTH+1] ;
    double  del_ejdate ;

    if( mdays > 10000)
    {
        /* 
        -- this very large number, about 27 years, 
        -- indicates not to delete anything.  
        */
        (void) fprintf( logfp, "Not deleting any maskinout records,\n" ) ;
        (void) fprintf( logfp, "indicated by mdays being too large:  %d\n\n",
            mdays ) ;
        return TRUE ;
    }

    (void) fprintf( logfp, "Deleting old maskinout records:\n" ) ;
    (void) fprintf( logfp, 
        "Current time = %s, data must pertain to %d days before now.\n", 
        asftime_stamp, mdays ) ;

    return_code = tc_asf_add_ndays( asftime_stamp, 
        - (double)mdays, del_asftime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: subtracting %d days from current time %s",
            mdays, asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "Data must pertain to before %s\n", del_asftime ) ;

    return_code = tc_asf2et(del_asftime, &del_ejdate ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: converting %s to ephemeris time",
            del_asftime ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) sprintf(where_clause, "where %s < %f",
        APS_COL(MASKINOUT, MASKINOUT_EJDATE), del_ejdate ) ;

    (void) fprintf( logfp, "Deleting maskinout %s\n", where_clause ) ;

    nrecs_deleted = db_delete_records(APS_dbproc, APS_TABLE(MASKINOUT), 
        where_clause ) ;

    (void)fprintf(logfp, "%d old maskinout records deleted.\n\n", 
        nrecs_deleted);

    /* stdout gets only terse notes.  print only if something happened: */
    if ( nrecs_deleted > 0 )
        (void)printf("\n%d old maskinout records deleted.\n\n", nrecs_deleted);

    return TRUE ;
}


/*==============================================================================
Function:       delete_old_cvrg_records()

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 16:10:32 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it could look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int
delete_old_cvrg_records(
    char        *progname,
    DBPROCESS   *APS_dbproc,
    char        *asftime_stamp,
    int         adays,
    int         bdays,
    FILE        *logfp ) 
{
    int     return_code ;
    int     nrecs_deleted ;

    DB_RECORD   **cvrgmdat_rec = NULL ;
    llist       *cvrgmdat_list ;
    cursor      cvrgmdat_list_ptr ;

    char    age_asftime[ASF_TIME_STR_LENGTH+1] ;
    char    before_asftime[ASF_TIME_STR_LENGTH+1] ;
    double  before_mjdate ;

    (void) fprintf( logfp, "Deleting old cvrg records:\n" ) ;
    (void) fprintf( logfp, 
        "Current time = %s, data must pertain to %d days before now.\n", 
        asftime_stamp, bdays ) ;
    (void) fprintf( logfp, 
        "and be from a Nominal Coverage older than %d days.\n", 
        adays ) ;

    /* 
    -- determine the ephemeris time for now - bdays to 
    -- help delete cvrg records.  
    */
    return_code = tc_asf_add_ndays( asftime_stamp, 
        - (double)bdays, before_asftime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: subtracting %d days from current time %s",
            bdays, asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "Data must pertain to before %s\n", before_asftime);

    return_code = tc_asf2et(before_asftime, &before_mjdate ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: converting %s to ephemeris time",
            before_asftime ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "cvrg.mjdate < %f\n", before_mjdate ) ;

    /* 
    -- determine the marker values for a cvrgmdat records older than 
    -- adays.  These are cvrgmdat with an age (cvrg.gen_time) older 
    -- than adays.  (age_asftime)
    -- In addition, these records must have start_time before 
    -- today - bdays.  (before_asftime)
    */

    /* subtract adays from the current time.    */
    return_code = tc_asf_add_ndays( asftime_stamp, 
        - (double)adays, age_asftime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: subtracting %d days from current time %s",
            adays, asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "Nominal Coverage must have been run before %s\n", 
        age_asftime ) ;

    /* 
    -- the cvrg data for each cvrgmdat rec must have an 
    -- age (cvrgmdat.gen_time) older than adays.  
    */
    (void) sprintf(where_clause, "where %s < '%s' and %s < '%s'",
        APS_COL(CVRGMDAT, CVRGMDAT_GEN_TIME), age_asftime,
        APS_COL(CVRGMDAT, CVRGMDAT_START_TIME), before_asftime ) ;

    (void) sprintf( orderby_cols, "%s, %s",
        APS_COL(CVRGMDAT, CVRGMDAT_SAT),
        APS_COL(CVRGMDAT, CVRGMDAT_SENSOR) ) ;
     
    cvrgmdat_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(CVRGMDAT), where_clause, orderby_cols, 
        APS_CDEFS(CVRGMDAT), ALL_COLS) ;
    if ( cvrgmdat_list == NULL)
    {
        (void) sprintf(msg, "ERROR: DB query failure %s order by %s", 
            where_clause, orderby_cols ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    /* if there are no runs old enough, then do not delete.  */
    if ( NUMELTS( cvrgmdat_list ) <= 0 )
    {
        (void) fprintf( logfp, "There were no runs old enough to delete.\n\n");
        DEL_LIST( cvrgmdat_list ) ;
        return TRUE ;
    }

    /*
    -- LOOP:  
    -- FOR EACH cvrgmdat record
    -- delete cvrg records using cvrgmdat.marker to match cvrg.marker
    */
    for (   cvrgmdat_rec = 
                (DB_RECORD **) FIRST(cvrgmdat_list, cvrgmdat_list_ptr);
            cvrgmdat_rec ;
            cvrgmdat_rec = 
                (DB_RECORD **) NEXT(cvrgmdat_list, cvrgmdat_list_ptr)  
        )
    {
        /* process the current cvrgmdat_rec right here.  */
        /*
        -- FOR EACH marker value, (which indicates the age of the data)
        -- delete cvrg data pertaining to a time bdays before now.  
        --
        -- NOTE:  each sat/sensor has its own set of marker values 
        --        starting with 1.  So the delete "where clause" needs 
        --        to have a sat/sensor/marker qualifier to go with the 
        --        time qualifier.  
        */
        (void) sprintf( where_clause, 
            "where %s = '%s' and %s = '%s' and %s = %ld and %s <= %f ",
            APS_COL(CVRG, CVRG_SAT), 
                CAST_CVRGMDAT_SAT cvrgmdat_rec[CVRGMDAT_SAT], 
            APS_COL(CVRG, CVRG_SENSOR), 
                CAST_CVRGMDAT_SENSOR cvrgmdat_rec[CVRGMDAT_SENSOR], 
            APS_COL(CVRG, CVRG_MARKER), 
                CAST_CVRGMDAT_MARKER cvrgmdat_rec[CVRGMDAT_MARKER], 
            APS_COL(CVRG, CVRG_MJDATE), before_mjdate ) ;

        (void) fprintf( logfp, "Deleting cvrg %s\n", where_clause ) ;

        nrecs_deleted = db_delete_records(APS_dbproc,
                APS_TABLE(CVRG), where_clause ) ;

        (void) fprintf(logfp, "%d old cvrg records deleted for %s %s.\n\n",
            nrecs_deleted, CAST_CVRGMDAT_SAT cvrgmdat_rec[CVRGMDAT_SAT],
            CAST_CVRGMDAT_SENSOR cvrgmdat_rec[CVRGMDAT_SENSOR] ) ;

        /* stdout gets only terse notes.  print only if something happened: */
        if ( nrecs_deleted > 0 )
            (void) printf("%d old cvrg records deleted for %s %s.\n\n", 
                nrecs_deleted, CAST_CVRGMDAT_SAT cvrgmdat_rec[CVRGMDAT_SAT],
                CAST_CVRGMDAT_SENSOR cvrgmdat_rec[CVRGMDAT_SENSOR] ) ;
    }

    DEL_LIST( cvrgmdat_list ) ;
    return TRUE ;
}


/*==============================================================================
Function:       delete_old_cvrgmdat_records()

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 16:11:07 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int
delete_old_cvrgmdat_records(
    char        *progname,
    DBPROCESS   *APS_dbproc,
    char        *asftime_stamp,
    int         adays,
    int         bdays,
    FILE        *logfp )
{
    int     nrecs_deleted ;
    int     return_code ;

    char    before_asftime[ASF_TIME_STR_LENGTH+1] ;
    char    age_asftime[ASF_TIME_STR_LENGTH+1] ;

    (void) fprintf( logfp, "Deleting old cvrgmdat records:\n" ) ;
    (void) fprintf( logfp, 
        "Current time = %s, data must pertain to %d days before now.\n", 
        asftime_stamp, bdays ) ;
    (void) fprintf( logfp, "and be older than %d days.\n", adays ) ;

    return_code = tc_asf_add_ndays( asftime_stamp, 
        - (double)bdays, before_asftime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: subtracting %d days from current time %s",
            bdays, asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "Data must pertain to before %s\n", before_asftime);

    return_code = tc_asf_add_ndays( asftime_stamp, 
        - (double)adays, age_asftime ) ;
    if ( return_code != TRUE ) 
    {
        (void) sprintf(msg, "ERROR: subtracting %d days from current time %s",
            adays, asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(FALSE) ;
    }

    (void) fprintf( logfp, "Nominal Coverage must have been run before %s\n", 
        age_asftime ) ;

    /* 
    -- the cvrg data for each cvrgmdat rec must pertain 
    -- to data Before now - bdays and the run must have an 
    -- age older than adays.  
    */
    (void) sprintf(where_clause, "where %s < '%s' and %s < '%s'",
        APS_COL(CVRGMDAT, CVRGMDAT_STOP_TIME), before_asftime,
        APS_COL(CVRGMDAT, CVRGMDAT_GEN_TIME), age_asftime ) ;

    (void) fprintf( logfp, "Deleting cvrgmdat %s\n", where_clause ) ;

    nrecs_deleted = db_delete_records(APS_dbproc, APS_TABLE(CVRGMDAT), 
        where_clause ) ;

    (void) fprintf( logfp, "%d old cvrgmdat records deleted.\n\n", 
        nrecs_deleted ) ;

    /* stdout gets only terse notes.  print only if something happened: */
    if ( nrecs_deleted > 0 )
        (void) printf( "\n%d old cvrgmdat records deleted.\n\n", nrecs_deleted);

    return TRUE ;

}

/*==============================================================================
Function:       aps_coverage_window()

Description:    driver function for APS coverage window, which maintains 
                create_nominal_coverage data in the planning window.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 09:58:31 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

int aps_coverage_window( 
    char        *progname, 
    DBPROCESS   *APS_dbproc,
    char        *sybase_userid, 
    char        *sybase_password, 
    int         n_retries,
    int         n_seconds_retry,
    char        *asftime_stamp,   /* asftime to use when making up filenames */
    int         adays,       /* age of data to delete                        */
    int         bdays,       /* data pertaining to Before this, delete it    */
    int         dminutes,    /* delay in minutes before submitting batch job */
                /*   It may seem strange to delay submitting the batch job. */
                /*   The only reason was that we were experiencing a case   */
                /*   of disappearing batch jobs when using batch instead    */
                /*   of at with a delay.  We could only conclude that the   */
                /*   batch job was lost due to a clock cycle problem.       */
                /*   So now we have a delay of at least 1 minute to insure  */
                /*   that the job is run.                                   */
    int         gdays,       /* coverage runs should be Good for this time   */
    int         mdays,       /* age of data to delete                        */
    int         wdays,       /* size of planning window.                     */
    FILE        *logfp )     /* log file pointer.                            */
{

    /* to help build C-shell filename:  */
    int     file_does_not_exist ;
    char    csh_filename[100] ;
    char    coverage_cmd[256] ;
    char    maskinout_cmd[256] ;
    char    *aps_fullpath_csh_filename ;
    char    *aps_fullpath_LOG_filename ;
    int     year, yy, doy, hour, min, sec, msec ;
    char    command[512] ;
    char    previous_sat[3] ;
    FILE    *cshfp ;

    DB_RECORD   **phase_rec = NULL ;
    llist       *phase_list ;
    cursor      phase_list_ptr ;

    DB_RECORD   **satsensor_rec = NULL ;
    llist       *satsensor_list ;
    cursor      satsensor_list_ptr ;

    int     return_code ;
    int     submit_flag = 1 ;  /* 1 -> Do submit the command file to run */

    /*
    -- fields used as arguments to 
    -- add_coverage_run_to_script()
    */
    char    begin_time[ASF_TIME_STR_LENGTH+1] ;
    char    end_time[ASF_TIME_STR_LENGTH+1] ;
    char    mask_flag[10] ;

    /* log info to log file:  */
    (void) fprintf( logfp, "STARTING %s PROCESSING at %s\n\n", 
        progname, asftime_stamp ) ;
    (void) fprintf( logfp, "Arguments to aps_coverage_window():  \n" ) ;
    (void) fprintf( logfp, "number_of_Re-tries       = %d\n", n_retries ) ;
    (void) fprintf( logfp, "Seconds_between_re-tries = %d\n", n_seconds_retry) ;
    (void) fprintf( logfp, "adays    = %d\n", adays ) ;
    (void) fprintf( logfp, "bdays    = %d\n", bdays ) ;
    (void) fprintf( logfp, "dminutes = %d\n", dminutes ) ;
    (void) fprintf( logfp, "gdays    = %d\n", gdays ) ;
    (void) fprintf( logfp, "mdays    = %d\n", mdays ) ;
    (void) fprintf( logfp, "wdays    = %d\n\n", wdays ) ;


    /* 
    -- delete old database records in the relations:
    -- maskinout, cvrg, and cvrgmdat
    -- this step was originally at the end of the 
    -- program.  
    */
    return_code = delete_old_maskinout_records( progname, APS_dbproc, 
        asftime_stamp, mdays, logfp ) ;
    if ( return_code != TRUE )
        return(APS_EXIT_ERROR) ;

    return_code = delete_old_cvrg_records( progname, APS_dbproc, 
        asftime_stamp, adays, bdays, logfp ) ;
    if ( return_code != TRUE )
        return(APS_EXIT_ERROR) ;

    return_code = delete_old_cvrgmdat_records( progname, APS_dbproc, 
        asftime_stamp, adays, bdays, logfp);
    if ( return_code != TRUE )
        return(APS_EXIT_ERROR) ;
    /*
    -- create the C-shell script file name.  
    -- <progname>.aps_crt_nom_cov.yyyyddd_hhmmss.csh
    */
 
    /* check asf time */
    if (!tc_parse_asftime(asftime_stamp,
            &year, &yy, &doy, &hour, &min, &sec, &msec))
    {
        (void) sprintf(msg, 
            "ERROR: Unable to create C-shell script filename from %s",
            asftime_stamp ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        return(APS_EXIT_ERROR) ;
    }
 
    /* 
    --  make file name 
    -- <progname>.aps_crt_nom_cov.yyyyddd_hhmmss.csh
    */
    (void)sprintf(csh_filename,
        "%s.aps_crt_nom_cov.%04d%03d_%02d%02d%02d.csh",
        progname, year, doy, hour, min, sec) ;

    /* this is a malloc() and must be freed later.  */
    aps_fullpath_csh_filename = aps_fullpath(APS_TEMP, csh_filename) ;
    (void) fprintf( logfp, "C-shell script file name = \n%s\n", 
        aps_fullpath_csh_filename ) ;
    /* 
    -- make sure that the csh file does not 
    -- already exist.  
    */
    aps_access( aps_fullpath_csh_filename, "F", &file_does_not_exist ) ;
    if ( !file_does_not_exist ) 
    {
        /* the file already exists.  */
        (void)sprintf(msg, "ERROR: file %s already exists",
            aps_fullpath_csh_filename) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }

    /*  Open the C-shell script file  */
    if ((cshfp = fopen(aps_fullpath_csh_filename, "a")) == NULL)
    {
        (void)sprintf(msg, "ERROR: Unable to open C-shell file %s",
            aps_fullpath_csh_filename) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }

    /* 
    -- csh was opened; add the first few lines.   
    -- source the $HOME/.apsrc file at the start.  
    */
    (void) fprintf( cshfp, "#!/bin/csh\n" ) ;
    (void) fprintf( cshfp, "#  %s\n", aps_fullpath_csh_filename ) ;
    (void) fprintf( cshfp, "echo ' '\n" ) ;
    (void) fprintf( cshfp, "date\n" ) ;
    (void) fprintf( cshfp, "echo START OF COVERAGE WINDOW MAINTENANCE RUNS\n");
    (void) fprintf( cshfp, "echo ' '\n" ) ;
    (void) fprintf( cshfp, 
"echo2 '----------------------------------------------------------------'\n" ) ;
    (void) fprintf( cshfp, "echo2 START OF COVERAGE WINDOW MAINTENANCE RUNS\n");
    (void) fprintf( cshfp, "echo2 ' '\n" ) ;
    (void) fprintf( cshfp, 
        "echo2 CHECK THIS E-MAIL FOR ERRORS IN COVERAGE RUNS \n");
    (void) fprintf( cshfp, "echo2 ' '\n" ) ;
    (void) fprintf( cshfp, "#\n# set up the APS env values.\n" ) ;
    (void) fprintf( cshfp, "source $HOME/.apsrc\n" ) ;
    (void) fprintf( cshfp, "echo ENVIRONMENT VARIABLES:\n" ) ;
    (void) fprintf( cshfp, "env\n" ) ;
    (void) fprintf( cshfp, "#\n" ) ;

    /*
    -- determine the run end time; the end time is the 
    -- same for every sat/sensor.
    */
    return_code = tc_asf_add_ndays(
        asftime_stamp, (double) (wdays+gdays), end_time ) ;
    if ( return_code != TRUE )
    {
        (void) sprintf(msg, "ERROR: adding %d + %d days (%f days) to %s", 
            wdays, gdays, (double) (wdays+gdays), asftime_stamp) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n", msg ) ;
        (void) fprintf( logfp, "Trying to determine the run end time.\n\n" ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }




    /*
    -- get a list of the active satellites
    -- order by satellite
    -- for each satellite, submit a maskinout run.  (aps_crt_nom_mask)
    */
    phase_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(PHASE), NULL, APS_COL(PHASE, PHASE_SAT), 
        APS_CDEFS(PHASE), ALL_COLS) ;
    if ( phase_list == NULL)
    {
        (void) sprintf(msg, "ERROR: DB query failure order by %s", 
            orderby_cols ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }
    if ( NUMELTS( phase_list ) <= 0 )
    {
        (void) sprintf(msg, "ERROR: no phase records found" ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        DEL_LIST( phase_list ) ;
        return(APS_EXIT_ERROR) ;
    }

    /*
    -- LOOP:  
    -- FOR EACH sat
    */
    (void) strcpy(previous_sat, "" ) ;
    for (   phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr);
            phase_rec ;
            phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr)  
        )
    {
        /* process the current phase_rec right here.  */
        /* do only once per satellite */
        if( strcmp( previous_sat, CAST_PHASE_SAT phase_rec[PHASE_SAT] ) == 0  )
            continue ;
        (void) strcpy( previous_sat, CAST_PHASE_SAT phase_rec[PHASE_SAT] ) ;

        /*
        -- determine the run begin time.
        */
        return_code = get_maskinout_run_strttime( 
            CAST_PHASE_SAT    phase_rec[PHASE_SAT],
            asftime_stamp, begin_time, logfp ) ;
        if( return_code != TRUE )
            return(APS_EXIT_ERROR) ;

        /* if begin time >= end time, there should be no run.  */
        if( strcmp( begin_time, end_time ) >= 0 )
            continue ;  

        /*
        -- now add the command to run aps_crt_nom_mask 
        -- to the C-shell script file and to the log file.  
        */
        if( n_retries > 0 )
        {
            /* there is re-try logic to pass to the command.  */
            (void) sprintf( maskinout_cmd,
"aps_crt_nom_mask -R %d -S %d -U %s -P %s -b %s -e %s %s",
                n_retries,
                n_seconds_retry,
                sybase_userid,
                sybase_password,
                begin_time,
                end_time,
                CAST_PHASE_SAT    phase_rec[PHASE_SAT] ) ;
        }
        else
        {
            (void) sprintf( maskinout_cmd,
"aps_crt_nom_mask -U %s -P %s -b %s -e %s %s ",
                sybase_userid,
                sybase_password,
                begin_time,
                end_time,
                CAST_PHASE_SAT    phase_rec[PHASE_SAT] ) ;
        }
        (void) fprintf( cshfp, "#\n%s\n", maskinout_cmd ) ;
        (void) fprintf( logfp, "Command added to C-shell file:\n" ) ;
        (void) fprintf( logfp, "%s\n", maskinout_cmd ) ;

    } /* END LOOP on sat using phase_list    */
    DEL_LIST( phase_list ) ;

    /*
    -- get a list of the active 
    -- satellite/sensors.
    */
    (void) sprintf(where_clause, "where %s = 'Y'",
        APS_COL(SATSENSOR, SATSENSOR_CVRG_ALLOWED) ) ;

    (void) sprintf(orderby_cols, "%s, %s",
        APS_COL(SATSENSOR, SATSENSOR_SAT),
        APS_COL(SATSENSOR, SATSENSOR_SENSOR)) ;
     
    satsensor_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(SATSENSOR), where_clause, orderby_cols, 
        APS_CDEFS(SATSENSOR), ALL_COLS) ;
    if ( satsensor_list == NULL)
    {
        (void) sprintf(msg, "ERROR: DB query failure %s order by %s", 
            where_clause, orderby_cols ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }
    if ( NUMELTS( satsensor_list ) <= 0 )
    {
        (void) sprintf(msg, 
            "ERROR: no active satsensor records found %s order by %s", 
            where_clause, orderby_cols ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        DEL_LIST( satsensor_list ) ;
        return(APS_EXIT_ERROR) ;
    }

    /*
    -- LOOP:  
    -- FOR EACH sat/sensor 
    */
    for (   satsensor_rec = 
                (DB_RECORD **) FIRST(satsensor_list, satsensor_list_ptr);
            satsensor_rec ;
            satsensor_rec = 
                (DB_RECORD **) NEXT(satsensor_list, satsensor_list_ptr)  
        )
    {
        /* process the current satsensor_rec right here.  */

        /*
        -- determine the run begin time.
        */
        return_code = get_coverage_run_strttime( 
            CAST_SATSENSOR_SAT    satsensor_rec[SATSENSOR_SAT],
            CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR],
            asftime_stamp, begin_time, logfp ) ;
        if( return_code != TRUE )
            return(APS_EXIT_ERROR) ;

        /* if begin time >= end time, there should be no run.  */
        if( strcmp( begin_time, end_time ) >= 0 )
            continue ;  

        /*
        -- determine the mask flag.  
        */
            /*
            -- NOTE:  this is klugey.  the argument is supposed 
            --        to be a record from the dtk relation.  
            --        the only field used, however, is the first 
            --        field in the record, the sat field.  
            --        the sat field is defined the same for both 
            --        dtk and satsensor, so this will work.  
            --        dtkm_sat_has_recorder(), if it changes, 
            --        would still work off the one field, and 
            --        this code would still be OK.  
            */
        if ( dtkm_sat_has_recorder( satsensor_rec ) == TRUE ) 
            (void) strcpy(mask_flag, "" ) ;
        else
            (void) strcpy(mask_flag, "-M" ) ;  /* in station mask(s) only */

        /*
        -- now add the command to run aps_crt_nom_cov 
        -- to the C-shell script file and to the log file.  
        */
        if( n_retries > 0 )
        {
            /* there is re-try logic to pass to the command.  */
            (void) sprintf( coverage_cmd,
"aps_crt_nom_cov -R %d -S %d -U %s -P %s -b %s -e %s %s %s %s",
                n_retries,
                n_seconds_retry,
                sybase_userid,
                sybase_password,
                begin_time,
                end_time,
                mask_flag,
                CAST_SATSENSOR_SAT    satsensor_rec[SATSENSOR_SAT],
                CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR] ) ;
        }
        else
        {
            (void) sprintf( coverage_cmd,
"aps_crt_nom_cov -U %s -P %s -b %s -e %s %s %s %s",
                sybase_userid,
                sybase_password,
                begin_time,
                end_time,
                mask_flag,
                CAST_SATSENSOR_SAT    satsensor_rec[SATSENSOR_SAT],
                CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR] ) ;
        }
        (void) fprintf( cshfp, "#\n%s\n", coverage_cmd ) ;
        (void) fprintf( logfp, "Command added to C-shell file:\n" ) ;
        (void) fprintf( logfp, "%s\n", coverage_cmd ) ;
    }

    DEL_LIST( satsensor_list ) ;

    /* END LOOP on sat/sensors   */

    /* 
    -- terminate the C-shell file 
    -- and now copy the file into the log file.  
    */

    (void) fprintf( cshfp, "echo ' '\n" ) ;
    (void) fprintf( cshfp, "date\n" ) ;
    (void) fprintf( cshfp, "echo END OF COVERAGE WINDOW MAINTENANCE RUNS\n" ) ;
    (void) fprintf( cshfp, "echo ' '\n" ) ;
    (void) fprintf( cshfp, "echo2 ' '\n" ) ;
    (void) fprintf( cshfp, 
"echo2 '----------------------------------------------------------------'\n" ) ;
    (void) fprintf( cshfp, 
        "echo2 END OF COVERAGE WINDOW MAINTENANCE MESSAGES\n" ) ;
    (void) fprintf(cshfp, "#\nexit 0\n" ) ;
    (void) fclose(cshfp) ;

    return_code = copyfile2fp(logfp, aps_fullpath_csh_filename ) ;
    if( return_code != TRUE )
    {
        (void)sprintf(msg, 
            "ERROR: Unable to copy C-shell script %s to log file",
            aps_fullpath_csh_filename) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }

    /* 
    --  now make the csh script log file name.  
    --  change:
    --  .../tmp/<progname>.aps_crt_nom_cov.yyyyddd_hhmmss.csh
    --  to:
    --  .../logs/<progname>.aps_crt_nom_cov.yyyyddd_hhmmss.LOG
    --  this is a malloc() and must be freed later.  
    */
    return_code = cshfilename2log( csh_filename, &aps_fullpath_LOG_filename ) ;
    if ( return_code != TRUE )
    {
        (void) sprintf(msg, 
            "ERROR: failed to compute LOG file name from csh file name %s",
            aps_fullpath_csh_filename ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }

    (void) fprintf( logfp, "C-shell script log file name = \n%s\n\n", 
        aps_fullpath_LOG_filename ) ;

    /* 
    -- make sure that the log file does not 
    -- already exist.  
    */
    aps_access( aps_fullpath_LOG_filename, "F", &file_does_not_exist ) ;
    if ( !file_does_not_exist ) 
    {
        /* the file already exists.  */
        (void)sprintf(msg, "ERROR: file %s already exists",
            aps_fullpath_LOG_filename) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        (void) fprintf( logfp, "%s\n\n", msg ) ;
        free( aps_fullpath_LOG_filename ) ;
        free( aps_fullpath_csh_filename ) ;
        return(APS_EXIT_ERROR) ;
    }

    (void) printf( "See the COVERAGE RUNS LOG FILE:\n%s\n", 
        aps_fullpath_LOG_filename ) ;
    (void) printf( "for results from the Coverage runs.\n\n" ) ;

    /* 
    -- now submit the csh file to run.  
    */
    (void) sprintf( command, "chmod 777 %s\n", aps_fullpath_csh_filename ) ;
    (void) system(command) ;
    (void) fprintf( logfp, "Command submitted to system:\n%s", command ) ;
    (void) printf( "Command submitted to system:\n%s", command ) ;
    (void) sprintf( command, "echo '%s > %s' | at now + %d min \n", 
        aps_fullpath_csh_filename, aps_fullpath_LOG_filename, dminutes ) ;

#ifdef DO_NOT_SUBMIT
    /* 
    -- for testing, we don't always want to run the 
    -- command file, too long. 
    -- If not set in this file, it could be set 
    -- in the make file.  if this line executes and 
    -- surpises you.  like it did me.  
    */
    submit_flag = 0 ;
#endif

    if( submit_flag )
        (void) system(command) ;
    else
        (void) printf( "\nTESTING:  Command NOT SUBMITTED:\n\n%s", command ) ;

    (void) fprintf( logfp, "Command submitted to system:\n%s\n", command ) ;
    (void) printf( "Command submitted to system:\n%s", command ) ;

    free( aps_fullpath_csh_filename ) ;
    free( aps_fullpath_LOG_filename ) ;

    return(APS_EXIT_OK) ;
}
