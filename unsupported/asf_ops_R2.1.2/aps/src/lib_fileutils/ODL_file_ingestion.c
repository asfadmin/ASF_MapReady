#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       ODL_file_ingestion.c

Description:    Routines used for ODL ingestion.    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)ODL_file_ingestion.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.ODL_file_ingestion.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read             */
#include <sys/uio.h>        /* for read             */
#include <sys/unistd.h>     /* for read             */
#include <mu_utilities.h>   /* for permissions      */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */
#include "aps_log_msg.h"    /* for syslogging                       */
#include "aps_defs.h"       /* for syslogging, exit codes           */

/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_antenna_down_times.h"         /* for dtk table             */

/* FOR DTK_UTILITIES DEFINITIONS MULTI_ANTENNA */
#include "dtkm_utilities.h"

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* for asftime2rev()  */
#include "phase_utilities.h"

/* FOR FILE UTILITIES DEFINITIONS */
#include "file_utilities.h"


#include "ODLconversions.h"
#include "GENconversions.h"


/* NOTE: the following messages MUST be matched by corresponding
--      entries in the file file_utilities.h 
--
--		Ignore LINT warning: declared global, could be static 
*/
char *fa_odl_error_message[] =
{
    "zero is not a valid error code",   /* 0*/
    "FA_ODL_UNABLE_TO_PARSE_FILE",      /* 1*/
    "FA_ODL_HEADER_KEYWORD_NOT_OK",     /* 2*/
    "FA_ODL_SEGMENT_COUNT_MISSING",     /* 3*/
    "FA_ODL_SEGMENT_MISSING",           /* 4*/
    "FA_ODL_ACTIVITY_MISSING",          /* 5*/
    "FA_ODL_DBRECORD_COPY_FAILED",      /* 6*/
    "FA_ODL_AGGREGATE_KEYWORD_NOT_OK",  /* 7*/
    "FA_ODL_DTK_DEFAULTS_NOT_FOUND",    /* 8*/
    "FA_ODL_DEFAULT_NOT_FOUND",         /* 9*/
    "FA_ODL_PERMISSIONS_DENIED",        /*10*/
    "FA_ODL_PERMISSIONS_FAILED",        /*11*/
    "FA_ODL_INDETERMINATE_FILE_TIMES",  /*12*/
    "-13 unknown error code.",          /*13*/
    "END OF fa_odl_error_message[] LIST"
};

/*==============================================================================
Function:       identify_ODL_file

Description:    Identify the given file type to a flight agency type.

Parameters:     

Returns:        
Type  Name                  Definition
                            FALSE = could not identify file
                            TRUE  = file successfully identfied

Creator:        Miguel Siu

Creation Date:  Fri Oct 11 07:57:04 PDT 1996

Notes:      
==============================================================================*/
int identify_ODL_file(
    char            *file_type,
    ODL_FILENAME     *fa_files,
    ODL_FILENAME     **file_descriptor)
{
    int i ;

    i = 0 ;
    while( fa_files[i].file_type != NULL)
    {
#ifdef PRINT_DIAG
        printf ("comparing %s to %s \n", file_type, fa_files[i].file_type) ;
#endif
        if (strncmp(file_type, fa_files[i].file_type, 
                        strlen(fa_files[i].file_type) ) == 0)
        {
            *file_descriptor = &fa_files[i] ;
#ifdef PRINT_DIAG
        printf ("FOUND MATCH: \n");
#endif
            return (TRUE) ;
        }
        i++ ;
    }

    return  (FALSE) ;

}


/*==============================================================================
Function:       retrieve_dtks_for_WOS

Description:    Given the type of WOS to be generated, return a list
                of datatakes selected for the WOS.
                NOTE:  this routine is here because a program 
                that ingests a file uses this routine.  It 
                is mostly used in file creation, however.  
                Don't move it to another library or file.  

                Return values:

                  < 0   ERROR:
                         -1    bad file_type argument.  
                         -2    error in DBMS query

                  >= 0  OK:
                        the number of data-takes retrieved.  

Creator:        Miguel Siu

Creation Date:  Wed Oct  9 10:28:14 PDT 1996

Notes:      This routine populates global variable fa_station_id

==============================================================================*/
int retrieve_dtks_for_WOS(
        char                *file_type,
        char                *strttime,
        char                *stoptime,
        llist               *WOS_dtks_list)
{
    llist   *dtklist = NULL ;
    int     nrecs ;
     
    /*
    -- Now retrieve the data-takes according to the 
    -- file type:
    */ 
    if ( strcmp(file_type, "AWOS") == 0 
    ||   strcmp(file_type, "ADDM") == 0 )
    {
        /* 
        -- this is an ASF WOS file or companion downlink-
        -- to-data-take map file.  
        -- retrieve SCH downlinks at ASF
        */
        strcpy( fa_station_id, "ASF" ) ;
        sprintf(where_clause,
"where (%s = '%s' or %s = '%s') \
and %s < '%s' and %s > '%s' and %s = '%s' and  %s = '%s' ",
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,

            APS_COL(DTK, DTK_STRTTIME), stoptime, 
            APS_COL(DTK, DTK_STOPTIME), strttime, 
            APS_COL(DTK, DTK_DTKSTAT), "SCH",
            APS_COL(DTK, DTK_STATION_ID), fa_station_id ) ;
    }
    else if ( strcmp(file_type, "MWOS") == 0 
         ||   strcmp(file_type, "MDDM") == 0 )
    {
        /* 
        -- this is an MCM WOS file or companion downlink-
        -- to-data-take map file.  
        -- retrieve SCH downlinks at MCM
        */
        strcpy( fa_station_id, "MCM" ) ;
        sprintf(where_clause,
"where (%s = '%s' or %s = '%s') \
and %s < '%s' and %s > '%s' and (%s = '%s' or %s = '%s') and  %s = '%s' ",
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,

            APS_COL(DTK, DTK_STRTTIME), stoptime, 
            APS_COL(DTK, DTK_STOPTIME), strttime, 
            APS_COL(DTK, DTK_DTKSTAT), "SCH",
            APS_COL(DTK, DTK_DTKSTAT), "PLN",
            APS_COL(DTK, DTK_STATION_ID), fa_station_id ) ;
    }
    else if ( strcmp(file_type, "AREQ") == 0 )
    {
        /* 
        -- this is a McMurdo Request for Availibility file.  
        -- it is the same as a WOS file except that it 
        -- puts in both SCH and PLN status dtks.  
        */
        strcpy( fa_station_id, "MCM" ) ;
        sprintf(where_clause,
"where (%s = '%s' or %s = '%s') \
and %s < '%s' and %s > '%s' and (%s = '%s' or %s = '%s') and  %s = '%s' ",
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,

            APS_COL(DTK, DTK_STRTTIME), stoptime, 
            APS_COL(DTK, DTK_STOPTIME), strttime, 
            APS_COL(DTK, DTK_DTKSTAT), "SCH",
            APS_COL(DTK, DTK_DTKSTAT), "PLN",
            APS_COL(DTK, DTK_STATION_ID), fa_station_id ) ;
    }
    else
    {
        sprintf(file_util_msg, 
            "Filetype '%s' is not acceptable\n", file_type ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        return (-1) ;
    }

    sprintf(orderby_cols, "%s", APS_COL(DTK, DTK_STRTTIME)) ;
    dtklist = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK), 
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;
    /* 
    -- the data-takes were retrieved ; check them
    */
    if (dtklist == NULL)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Error in retrieving Sybase data.\n", 
            DO_SYSLOG, DO_PRINT);
        sprintf(file_util_msg, 
            "where_clause = %s\n\torderbyu_cols = %s\n", 
            where_clause, orderby_cols ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR, 
        "Check the Sybase server, the dtk relation, the aps_reader account, \n", 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR, 
            "or the aps_reader access on the dtk relation. \n", 
            DO_SYSLOG, DO_PRINT);
        return (-2) ;
    }

    nrecs = NUMELTS(dtklist) ;

    if ( nrecs == 0 )
    {
        aps_log_msg(file_util_progname, APS_INFO, 
            "No downlinks were found for the input arguments.\n", 
            DO_SYSLOG, DO_PRINT);
        sprintf(file_util_msg, 
            "Retrieve was based on: \n%s\n", where_clause ) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        DEL_LIST (dtklist) ;                
        return (0) ;
    }

     db_record_llist_move (WOS_dtks_list, dtklist ) ;
     DEL_LIST (dtklist) ;

    return (nrecs) ;
}

/*==============================================================================
Function:      RES_rej_downtime 

Description:   For each of the REJected datatake proposals in the list,
               create a new antenna_down_times record in the database.

Parameters:     

Returns:   
    Type  Definition
    int     >= 0    Number of records created.
    int    -1.      Error encountered.

Creator:        Miguel Siu

Creation Date:  Tue Sep 17 10:42:20 PDT 1996

Notes:
    This routine uses the global variable fa_filename.

    The calling program should check that the number of downtime records 
    that are created is greater than or equal to 0.
    An error in this routine should not be treated as a fatal condition.
==============================================================================*/
static int 
RES_rej_downtime (
    DBPROCESS   *APS_dbproc,        /* open Sybase database process */
    llist       *input_dtk_list)    /* list of dtk DB_RECORD */
{
    int           nrecs_inserted ;
    int           downtime_records_inserted = 0 ;
    cursor        dtk_rec_ptr ;
    DB_RECORD     **dtk_rec ;
    DB_RECORD     **ant_down_rec = NULL ;

    for (
        dtk_rec = (DB_RECORD **)FIRST(input_dtk_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **)NEXT( input_dtk_list, dtk_rec_ptr)
        )
    {
        if ( strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ") == 0 )
        {
        /* 
        -- We have a REJected datatake proposal, create a antenna_down_times
        -- record which matches the proposal's start/stop times.
        */
        ant_down_rec =  
            new_table_record(APS_CDEFS(ANTENNA_DOWN_TIMES)) ;

        strcpy( CAST_ANTENNA_DOWN_TIMES_STATION_ID 
                ant_down_rec[ANTENNA_DOWN_TIMES_STATION_ID],
                    CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;

        CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID 
        ant_down_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID] =
                CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;

        strncpy(CAST_ANTENNA_DOWN_TIMES_STRTTIME 
                ant_down_rec[ANTENNA_DOWN_TIMES_STRTTIME],
                 CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], ASF_TIME_STR_LENGTH) ;

        strncpy(CAST_ANTENNA_DOWN_TIMES_STOPTIME 
                ant_down_rec[ANTENNA_DOWN_TIMES_STOPTIME],
                 CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], ASF_TIME_STR_LENGTH) ;

        strcpy (CAST_ANTENNA_DOWN_TIMES_COMMENTS 
                ant_down_rec[ANTENNA_DOWN_TIMES_COMMENTS], 
                    "By ARES--AVAILABILITY RESPONSE file ") ;
        strcat (CAST_ANTENNA_DOWN_TIMES_COMMENTS 
                ant_down_rec[ANTENNA_DOWN_TIMES_COMMENTS], fa_filename) ;

        nrecs_inserted = db_insert_single_record(APS_dbproc,
            ant_down_rec, APS_TABLE(ANTENNA_DOWN_TIMES),
            APS_CDEFS(ANTENNA_DOWN_TIMES) ) ;

        free_db_record( ant_down_rec ) ;
     
        if ( nrecs_inserted != 1 )
            /* 
            -- return -1 records updated, to warn calling program 
            */
            return (-1) ;
        else
            downtime_records_inserted++ ;
        }
    }

    return (downtime_records_inserted) ;
}

/*==============================================================================
Function:      RES_fillin_response 

Description:   Uses the fields sat/______/rev/dtkid for incoming dtk proposal
                to look up its corresponding db entry and acquire needed
                dtk field values.

                We will keep all the values from the db entry, except for
                DTK_DTKSTAT which will be updated to reflect the dtk proposal. 

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Mon Sep 23 09:53:22 PDT 1996

Notes:  
    The SENSOR value is not needed anymore, because in the downlink to datatake
    mapping model, each downlink (realtime or dump) and each observation or 
    recording activity will have its own, unique dtkid. We can therefore use the
    dtkid to lookup the SENSOR value in the database.
==============================================================================*/
static int
RES_fillin_response (
            DBPROCESS   *APS_dbproc,  /* open Sybase database process */
            FILE        *report_log_ptr,
            DB_RECORD    **original_rec,
            DB_RECORD    **result_rec )
{
    llist       *lookup_dtks_list  ;
    DB_RECORD   **lookup_rec ;
    DB_RECORD   **temp_rec ;
    cursor      lookup_rec_ptr ;

    if (original_rec == NULL)   
        return (FALSE) ;
    if (result_rec ==NULL)      
        return (FALSE) ;

    lookup_dtks_list  = create_dyn_llist();

    /*
    -- Pull the record's database counterpart 
    -- and use these fields from the corresponding datatake: 
    --      DTK_ACTID  DTK_ANTENNA_ID
    */

    {
        /*
        -- the where clause will get the record in the 
        -- db with the same sat/______/rev/dtkid
        -- regardless of status.  
        -- These primary keys are enough to uniquely identify the record.
        */
        sprintf(where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DTK,DTK_SAT), CAST_DTK_SAT original_rec[DTK_SAT],
            APS_COL(DTK,DTK_REV), CAST_DTK_REV original_rec[DTK_REV],
            APS_COL(DTK,DTK_DTKID), CAST_DTK_DTKID original_rec[DTK_DTKID] ) ;


#ifdef PRINT_DIAG
        printf("%s(%d):  AREQ_fillin_response():  where_clause = \n%s\n", 
            __FILE__, __LINE__, where_clause ) ;
        dtkm_print( stdout, original_rec ) ;
#endif
        /* 
        -- get the corresponding db record 
        -- If no db corresponding record exists, do not update.
        -- If no corresponding record exists, can't get the values 
        -- from the database.  
        */
        lookup_dtks_list = db_get_records(APS_dbproc, APS_TABLE(DTK),
            where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
        lookup_rec = (DB_RECORD **) FIRST(lookup_dtks_list, lookup_rec_ptr) ;

        if (lookup_rec != NULL)
        {
#ifdef PRINT_DIAG
            printf("%s(%d):  AREQ_fillin_response():  lookup_rec = \n", 
                __FILE__, __LINE__ ) ;
            dtkm_print( stdout, lookup_rec ) ;
#endif
            /* 
            -- copy the db record into our result_rec record
            -- update the DTKSTAT field in the result_rec record 
            -- NOTE: we make use of a 'temporary_rec' just in case
            -- both arguments in this routine refer to the same record!
            -- The user is not prevented from using the routine this way.
            */
            temp_rec = new_table_record(APS_CDEFS(DTK)) ;
            (void) db_copy_record ( APS_CDEFS(DTK), 
                                        temp_rec, original_rec ) ;

            (void) db_copy_record ( APS_CDEFS(DTK), 
                                        result_rec, lookup_rec ) ;

            strcpy( CAST_DTK_DTKSTAT result_rec[DTK_DTKSTAT],
                                CAST_DTK_DTKSTAT temp_rec[DTK_DTKSTAT] ) ;

            free_db_record (temp_rec) ;

        }
        else
        {
            /* 
            -- We have found a dtk proposal with no matching db record!!! 
            -- This is unacceptable for a Flight Agency reply.  All reply
            -- dtks must have a matching dtk record in the database.
            */
            aps_log_msg(file_util_progname, APS_ERROR, 
            "Data-take proposal in incoming file has no matching db record.\n",
                DO_SYSLOG, DO_PRINT);
            fprintf (report_log_ptr,
            "Data-take proposal in incoming file has no matching db record:\n");
            db_fprint_record( report_log_ptr, original_rec, APS_CDEFS(DTK) ) ;
            DEL_LIST ( lookup_dtks_list ) ;
            return (FALSE) ;
        }

    }

    return (TRUE) ;
}

/*==============================================================================
Function:       ODL_nano2msecs

Description:    This routine is similar to ims_nano2msecs() in the libims.a
                library.  We have a copy of it here because inclusion of the
                libims.a library conflicts with the common library libodl.a

******************************************************************************
**
** ims_nano2msecs ()
**
** This utility function will convert nanoseconds to milliseconds and
** support rounding.  This function fixes an apparent bug that ODL has
** converting a three digit number into nanoseconds.
**
******************************************************************************

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct 15 10:55:41 PDT 1996

Notes:      
==============================================================================*/
static int 
ODL_nano2msecs (long nano_secs)
{
        int msecs;
        float msecs_flt;
                
        msecs = (int) (nano_secs / 1000000);
                   
        msecs_flt = (float) (nano_secs / 1000000.0);
        if (msecs_flt - msecs > 0.499999)
        {
                msecs ++;
        }
                                                 
        return(msecs);
}

/*==============================================================================
Function:       ODL_find_and_convert

Description:    starting from the node specified, find the string which is 
                identified by the *keyword which is passed to this routine.
                Then, convert the string using the specified (*conversion)

Parameters:     
Type          Name              Definition
AGGREGATE     node              node address from which to start search
char          *keyword          string label identifier                   
int           (*conversion())   conversion to be applied to found string  
struct EQUIV_TABLE
              *conversion_table table holding the equivalences for target string
void          *dest             place conversion result at this adddress 

Returns:        
Type  Name                  Definition
*int  conversion_status     status of the conversion used on the string 
                            found by routine  FALSE = error
                                              TRUE  = success

Creator:        Miguel Siu

Creation Date:  Fri May 19 13:53:43 PDT 1995

Notes:  ASSUMPTION: all conversion routines return TRUE/FALSE   
==============================================================================*/
static int
ODL_find_and_convert(
    AGGREGATE   node, 
    char        *keyword, 
    int         (conversion()), 
    EQUIV_TABLE *conversion_table,
    void        *dest,
    char        **keyword_value )
{
    unsigned int    milliseconds = 0;
    int             conversion_status;
    VALUE           currValue = NULL;
    PARAMETER       currParameter = NULL;
 
#ifdef DEBUG
    printf("\nFIND_AND_CONVERT: %s\n", keyword); 
    printf("                :found %s\n",*keyword_value);
#endif

    /*
    -- assign initial value to the data described by the keyword
    */
    *keyword_value = malloc (256) ;
    strcpy (*keyword_value, "?????") ;

    if ((currParameter = FindParameter(node, keyword)) == NULL)
    {
        return(FALSE);
    }

    if ((currValue = FirstValue (currParameter)) == NULL)
    {
        return(FALSE);
    }


    switch (currValue->item.type)
    {
        case TV_INTEGER:
            ODLFormatInteger (*keyword_value, &currValue->item);
            if (conversion == NULL)
                *(unsigned int *)dest = currValue->item.value.integer.number ;
            else
            {
                conversion_status = conversion(conversion_table, 
                                &currValue->item.value.integer.number, dest);
                return(conversion_status);
            }
            break;
        case TV_SYMBOL:
            ODLFormatSymbol (*keyword_value, &currValue->item);
            if (conversion == NULL)
                sprintf(dest, "%s", currValue->item.value.string);
            else
            {
                conversion_status = conversion(conversion_table, 
                                    currValue->item.value.string, dest);
                return(conversion_status);
            }
            break;
        case TV_STRING:
            strcpy (*keyword_value, currValue->item.value.string) ;
            if (conversion == NULL)
                sprintf(dest, "%s", currValue->item.value.string);
            else
            {
                conversion_status = conversion(conversion_table, 
                                    currValue->item.value.string, dest);
                return(conversion_status);
            }
            break;
        case TV_DATE_TIME:
            ODLFormatDateTime (*keyword_value, &currValue->item);
            milliseconds = ODL_nano2msecs(
                                currValue->item.value.date_time.nanoseconds) ;
            sprintf(dest, "%04u:%03u:%02u:%02u:%02u.%03u",
                    currValue->item.value.date_time.year,
                    currValue->item.value.date_time.doy,
                    currValue->item.value.date_time.hours,
                    currValue->item.value.date_time.minutes,
                    currValue->item.value.date_time.seconds,
                    milliseconds);
            break;
        default:
            return(FALSE);
    }

    return(TRUE) ;
}             

/*==============================================================================
Function:      fa_odl_ingestion 

Description:    
    Creates and scans a tree structure for information 
    which is placed in a DB_RECORD linked list.

Parameters:     

Returns:        
Type        Name                Definition
int                             FA_ODL_INGESTION_OK = Success
                                other:
                                FA_ODL_UNABLE_TO_PARSE_FILE
                                FA_ODL_DEFAULT_NOT_FOUND
                                FA_ODL_HEADER_KEYWORD_NOT_OK
                                FA_ODL_SEGMENT_COUNT_MISSING
                                FA_ODL_SEGMENT_MISSING
                                FA_ODL_ACTIVITY_MISSING
                                FA_ODL_DBRECORD_COPY_FAILED
                                FA_ODL_AGGREGATE_KEYWORD_NOT_OK
                                FA_ODL_DTK_DEFAULTS_NOT_FOUND
                                FA_ODL_PERMISSION_DENIED
                                FA_ODL_PERMISSION_FAILED
                                FA_ODL_INDETERMINATE_FILE_TIMES

Creator:        Miguel Siu

Creation Date:  Thu Jun 15 11:51:51 PDT 1995

Notes:      
==============================================================================*/
int                  /* Ignore LINT warning: declared global, could be static */
fa_odl_ingestion(
    FILE        *file_name,
    DBPROCESS   *APS_dbproc,        /* open Sybase database process */
    int         *permission_id,
    ODL_FILEDEF *filedefs, 
    llist       *input_dtk_list, 
    FILE        *report_log)
{
    /* declarations         */
    int           i,j;
    int           return_code;
    int           number_of_errors_in_reception = 0 ; 
    char          *keyword_value ;

    DB_RECORD     **input_dtk_rec = NULL ;
    DB_RECORD     **template_dtk_rec = NULL ;
    DB_RECORD     *destination = NULL ;
    DB_RECORD     **min_dtk_rec ;
    DB_RECORD     **max_dtk_rec ;

    AGGREGATE       top_o_tree  = NULL ;
    AGGREGATE       file_node   = NULL ;
    AGGREGATE       header_node = NULL ;
    AGGREGATE       rec_node;
    AGGREGATE       current_node;


    fa_number_of_error_records = 0 ;
    if ((top_o_tree = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
                                                            (AGGREGATE) NULL)
    {
#ifdef DEBUG                                        
        printf ("Could not setup tools to parse the file.\n");
#endif
        return (FA_ODL_UNABLE_TO_PARSE_FILE);
    }


    if ((ReadLabel (file_name, top_o_tree)) == 0)
    {
#ifdef DEBUG                                        
        printf ("Could not parse the file.\n");
#endif
        return (FA_ODL_UNABLE_TO_PARSE_FILE);
    }


    if ((file_node = FindAggregate(top_o_tree, filedefs->file_object))
                                                        == (AGGREGATE) NULL)
    {
#ifdef DEBUG                                        
        printf ("Could not find the file identifier.\n");
#endif
        return (FA_ODL_UNABLE_TO_PARSE_FILE);
    }


    if ((header_node = FindAggregate(file_node, filedefs->header_object))
                                                        == (AGGREGATE) NULL)
    {
#ifdef DEBUG                                        
        printf ("Could not find the file header.\n");
#endif
        return (FA_ODL_UNABLE_TO_PARSE_FILE);
    }


    /*
    -- Make an empty    DB_RECORD template
    */
    template_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
    /*
    -- put various appropriate blank values into 
    -- the record.  
    */
    dtkm_blank_values(template_dtk_rec, template_dtk_rec ) ;

    /* 
    -- Process the header.
    -- Find values for FILE_HEADER and FA_DEFAULT fields
    */

    i = 0;
    while(filedefs->field_def[i].source_code)
    {
        /* 
        -- Assign a  destination, index or pointer as needed.
        */
        if (filedefs->field_def[i].source_code == FILE_HEADER
        ||  filedefs->field_def[i].source_code == FA_DEFAULT) 
        {
            if (filedefs->field_def[i].destination_code == REPORT_RECORD)
                destination = 
                    template_dtk_rec[filedefs->field_def[i].destination.index];
            else
                destination = (DB_RECORD *)
                    filedefs->field_def[i].destination.pointer; 
        }
        else
        {
            i++;
            continue;
        }

        if (filedefs->field_def[i].source_code == FA_DEFAULT)
        {
            /*
            -- assign values to the destination pointer
            */
            if (! (filedefs->field_def[i].conversion)(NULL, NULL, destination) )
            {
                free_db_record( template_dtk_rec ) ;
                return (FA_ODL_DEFAULT_NOT_FOUND);
            }
        }
        else if (! ODL_find_and_convert(header_node, 
                  filedefs->field_def[i].keyword, 
                  filedefs->field_def[i].conversion,
                  filedefs->field_def[i].conversion_table,
                  destination, &keyword_value ))
        {
            if (*keyword_value == NULL)strcpy(keyword_value,"<null value>");
            sprintf(file_util_msg, 
                "fa_odl_ingest():error parsing header:  %s = %s\n",
                filedefs->field_def[i].keyword, keyword_value ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            fprintf (report_log, 
                "fa_odl_ingest:error parsing header:  \n\t%s = %s\n", 
                filedefs->field_def[i].keyword, keyword_value ) ;

            free_db_record( template_dtk_rec ) ;
            free (keyword_value) ;
            return (FA_ODL_HEADER_KEYWORD_NOT_OK);
        }
        i++;
    }

    /*
    -- At this point, we have the fa_activity_type (it is derived from header)
    -- so we can request the single_activity permission.
 
       Here is the chain of events:
       0) Parse the header.
    -> 1) If activity permission not provided, get it.
       2) If activity permission is  provided, validate it.
       3) Parse the data records.
       4) Get planning permission.
       5) ACCESS DATABASE. PROCESS PROPOSAL LIST.
 
    */
 
    /*
    -- Item (1). Get ACTIVITY permission if it is not provided.
    -- NOTE: both (1) and (2) can be satified using mu_get_permission routine.
    */
    return_code = mu_get_permission(
        file_util_msg,				 /*  name of executable, for syslog()    */
        APS_dbproc,                  /*  Sybase process pointer      */
        *permission_id,              /*  if != 0, verify this permission.    */
        MU_SINGLE_ACTIVITY_TYPE,     /*  Multi-user activity type.   */
        fa_activity_type,            /*  Multi-user activity id.     */
        NULL,                        /*  start time of planning time bracket.*/
        NULL,                        /*  end time of planning time bracket.  */
        NULL,                        /*  station id of planning activity.    */
        0,                           /*  DAR id of dar activity.     */
        0,                           /*  retry logic n_retries       */
        0  ) ;                       /*  retry logic n_seconds_retry */
    if ( return_code < 0 )
        return FA_ODL_PERMISSIONS_FAILED ;
    if ( return_code == 0 )
        return FA_ODL_PERMISSIONS_DENIED ;
    /*
    -- we have gotten our activity permission.
    */
    *permission_id = return_code ;



    /*
    -- Process FILE_RECORD aggregate variables; append information to link list.
    -- NOTE: global variable fa_number_of_records carries the number
    --      of receptions expected for this file.
    */
    for(
        rec_node = FindAggregate(file_node, filedefs->data_record_object) ;
        rec_node != (AGGREGATE) NULL ;
        rec_node = NextAggregate (current_node) )
    {
        /*
        -- Make an empty    DB_RECORD template
        */
        input_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

        /*
        -- Copy a new empty DB_RECORD to use when reading each record
        -- from the file.
        */
        if (db_copy_record(APS_CDEFS(DTK),
                input_dtk_rec,template_dtk_rec) != DB_COPY_RECORD_OK)
        {
#ifdef DEBUG                                        
            printf ("Could not copy template of DB_RECORD. \n");
#endif
            return (FA_ODL_DBRECORD_COPY_FAILED);
        }


        /*
        -- Process all variables for this reception.
        */

        number_of_errors_in_reception = 0 ; 
        j=0;
        while(filedefs->field_def[j].source_code)     
        {
            if ( filedefs->field_def[j].source_code == FILE_RECORD )
            {
                if ( filedefs->field_def[j].destination_code == REPORT_RECORD )
                {
                    if ( !ODL_find_and_convert(rec_node, 
                            filedefs->field_def[j].keyword, 
                            filedefs->field_def[j].conversion,
                            filedefs->field_def[j].conversion_table,
                            input_dtk_rec[
                                filedefs->field_def[j].destination.index],
                             &keyword_value)  )
                    {
                        number_of_errors_in_reception ++ ;
                        if (*keyword_value == NULL)
                            strcpy(keyword_value,"<null value>");
                        sprintf(file_util_msg, 
                "ODL_find_and_convert:error parsing reception:  %s = %s\n",
                            filedefs->field_def[j].keyword, keyword_value ) ;
                        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                            DO_SYSLOG, DO_PRINT);

                        fprintf(report_log, 
                "ODL_find_and_convert:error parsing reception:  \n\t%s = %s\n",
                            filedefs->field_def[j].keyword, keyword_value ) ;
                        /*
                        free (keyword_value) ;
                        free_db_record( template_dtk_rec ) ;
                        return (FA_ODL_AGGREGATE_KEYWORD_NOT_OK);
                        */
                    }
                }
                else if ( filedefs->field_def[j].destination_code == 
                                                            GLOBAL_VARIABLE )
                {
                    destination = 
                       (DB_RECORD *)filedefs->field_def[j].destination.pointer ; 

                    if (! ODL_find_and_convert(rec_node, 
                        filedefs->field_def[j].keyword, 
                        filedefs->field_def[j].conversion,
                        filedefs->field_def[j].conversion_table,
                        destination, &keyword_value)  )
                    {
                        number_of_errors_in_reception ++ ;
                        if (*keyword_value == NULL)
                            strcpy(keyword_value,"<null value>");
                        sprintf(file_util_msg, 
                            "ODL_find_and_convert():error parsing:  %s = %s\n",
                            filedefs->field_def[j].keyword, keyword_value );
                        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                            DO_SYSLOG, DO_PRINT);

                        fprintf(report_log,
                            "ODL_find_and_convert:error parsing:  \n\t%s = %s\n",
                            filedefs->field_def[j].keyword, keyword_value );
                        /*
                        free (keyword_value) ;
                        free_db_record( template_dtk_rec ) ;
                        return (FA_ODL_AGGREGATE_KEYWORD_NOT_OK);
                        */
                    }
                }
            }
            j++;
        }

        /*
        -- the current data record has been ingested; all fields. 
        -- Now, carry out any pre-processing instructions.
        */
        if (fa_trigger_RES_fillin_response)
        {
            if(!RES_fillin_response (APS_dbproc, report_log,
                input_dtk_rec, input_dtk_rec))
            {
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "ERROR MATCHING RESPONSE RECORD TO DATABASE.\n\n", 
                    DO_SYSLOG, DO_PRINT);
                number_of_errors_in_reception++ ;
            }
        }


        if ( number_of_errors_in_reception <= 0 )
        {

            /*
            -- check the rec values, fill in with default values
            */
            return_code = dtkm_default_values (input_dtk_rec, input_dtk_rec);
            if (return_code < 0 )
            {
                aps_log_msg(file_util_progname, APS_ERROR, 
                    DTKM_ERROR_MESSAGE(return_code), 
                    DO_SYSLOG, DO_PRINT);
                number_of_errors_in_reception++ ;
                /*
                free_db_record( template_dtk_rec ) ;
                return (FA_ODL_DTK_DEFAULTS_NOT_FOUND);
                */
            }
            else
            {
#ifdef DEBUG
                /* -- print the DB_RECORD: */
                printf("%s(%d):  APPENDING DB_RECORD to input_dtk_list:\n", 
                    __FILE__, __LINE__ ) ;
                db_print_record( input_dtk_rec, APS_CDEFS(DTK) );
#endif 

                /*
                -- append the rec to the list: 
                */
                APPEND( input_dtk_list, input_dtk_rec, free_db_record, 
                    input_dtk_rec) ;

            } /* END of dtkm_default_values OK  */

        } /* END of   if ( number_of_errors_in_reception <= 0 )   */

        if ( number_of_errors_in_reception > 0 )
        {
            /* a global variable:  */
            fa_number_of_error_records ++ ;
            aps_log_msg(file_util_progname, APS_ERROR, 
                "SKIPPING A RECEPTION DUE TO ERRORS IN FILE\n\n", 
                DO_SYSLOG, DO_PRINT);
            fprintf( report_log, 
                "SKIPPING A RECEPTION DUE TO ERRORS IN FILE\n\n" ) ;
        }

        current_node = rec_node ;

    } /* END of for each reception  */

    /*
    -- Setup some global variables needed by permission logic, which follows.
    */
        min_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
        max_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
 
        if (db_get_min_record(input_dtk_list, APS_CDEFS(DTK), DTK_STRTTIME,
                &min_dtk_rec) != DB_MIN_RECORD_OK
        ||  db_get_max_record(input_dtk_list, APS_CDEFS(DTK), DTK_STOPTIME,
                &max_dtk_rec) != DB_MAX_RECORD_OK)
        {
            aps_log_msg(file_util_progname, APS_ERROR,
                "COULD NOT FIND START TIME OR STOP TIME FOR FILE\n",
                DO_SYSLOG, DO_PRINT);
            return (FA_ODL_INDETERMINATE_FILE_TIMES) ;
        }
 
        strcpy(fa_file_start_time, CAST_DTK_STRTTIME min_dtk_rec[DTK_STRTTIME]);
        strcpy(fa_file_stop_time,  CAST_DTK_STOPTIME max_dtk_rec[DTK_STOPTIME]);
 
        free_db_record (min_dtk_rec) ;
        free_db_record (max_dtk_rec) ;
 
    /*
    -- NOW, get more permissions before proceeding to the database changes.
    -- Until now, we have read the database for information, while processing
    -- our file into a list of datatake proposals.
    -- But before we actually CHANGE the database, we must get permissions.
    --
       Here is the chain of events:
       0) Parse the header.
       1) If activity permission not provided, get it.
       2) If activity permission is  provided, validate it.
       3) Parse the data records.
    -> 4) Get planning permission.
       5) ACCESS DATABASE. PROCESS PROPOSAL LIST.
 
    */
 
    /*
    -- Item (4). Get PLANNING permission.
    */
    return_code = mu_get_permission(
        file_util_progname,			 /*  name of executable, for syslog()    */
        APS_dbproc,                  /*  Sybase process pointer      */
        *permission_id,              /*  if != 0, verify this permission.    */
        MU_PLANNING_ACTIVITY_TYPE,   /*  Multi-user activity type.   */
        fa_activity_type,            /*  Multi-user activity id.     */
        fa_file_start_time,          /*  start time of planning time bracket.*/
        fa_file_stop_time,           /*  end time of planning time bracket.  */
        fa_station_id,               /*  station id of planning activity.    */
        0,                           /*  DAR id of dar activity.     */
        0,                           /*  retry logic n_retries       */
        0  ) ;                       /*  retry logic n_seconds_retry */
    if ( return_code < 0 )
        return FA_ODL_PERMISSIONS_FAILED ;
    if ( return_code == 0 )
        return FA_ODL_PERMISSIONS_DENIED ;
 

    free_db_record( template_dtk_rec ) ;
    return (FA_ODL_INGESTION_OK);

}/* end of fa_odl_ingestion() */


/*==============================================================================
Function:       fa_odl_processor

Description:    Accepts an ODL format file, and then
                calls routines to convert it to a tree structure 
                and to scan it for information which is placed on
                a DB_RECORD linked list.  
                This list is then processed by appropriate dtk routines and 
                its information incorporated into the DTK database table.

Parameters:    
Type     Name        Definition
*char    filename    Name of incoming ODL format file to be read.

Returns:        
                    FA_ODL_PROCESS_OK       = success
                    FA_ODL_PROCESS_ERROR    = error

Creator:        Miguel Siu

Creation Date:  Fri May 19 09:46:50 PDT 1995

Notes:      FA_ODL_PROCESS_OK follows convention   0 = no errors.
                                                  <0 = error.
                                                  >0 = warning.
==============================================================================*/
int fa_odl_processor(
    DBPROCESS       *APS_dbproc,   /* open Sybase database process          */
    int             *permission_id,
    ODL_FILENAME    *filedef,
    char            *file_name,    /* input CSA Reception Request file name */
    FILE            *report_log)   /* report file pointer                   */
{
    FILE        *file_name_ptr ;
    int         return_code;
    int         fa_odl_ingest_return_code ;
    int         dtkm_process_list_return_code ;
    int         planner_read_status = 0 ;

    llist       *input_dtk_list = NULL ;
    llist       *accepted_dtks  = NULL ;
    llist       *rejected_dtks  = NULL ;
    llist       *error_dtks     = NULL ;
    llist       *omission_dtks  = NULL ;
    llist       *CON_dtks       = NULL ;
    llist       *deleted_dtks   = NULL ;
    llist       *other_sat_dtks = NULL ;
    llist       *same_sat_dtks  = NULL ;
    llist       *dtk_updates    = NULL ;


    input_dtk_list = create_dyn_llist() ;
    if(input_dtk_list == NULL)
    {
#ifdef DEBUG                                        
        printf ("could not create dyn llist!!!\n");
#endif
        return(FA_ODL_PROCESS_ERROR);
    }


    /* Ingest the file */
    file_name_ptr = fopen( file_name, "r" ) ;
    if ( file_name_ptr == NULL )
    {
#ifdef DEBUG                                        
        printf ("could not open the ODL file !\n");
#endif
        return(FA_ODL_PROCESS_ERROR);
    }

    fa_odl_ingest_return_code = 
    fa_odl_ingestion(file_name_ptr, APS_dbproc, permission_id,
                filedef->file_definition, input_dtk_list, report_log );


    if ( fa_number_of_error_records > 0 )
    {
        sprintf(file_util_msg, 
            "NUMBER OF RECEPTIONS SKIPPED DUE TO ERRORS:  %d\n\n",
            fa_number_of_error_records ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf(report_log, 
            "\nNUMBER OF RECEPTIONS SKIPPED DUE TO ERRORS:  %d\n\n",
            fa_number_of_error_records ) ;
    }

    if ( fa_odl_ingest_return_code != FA_ODL_INGESTION_OK)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            FA_ODL_ERROR_MESSAGE(fa_odl_ingest_return_code), 
            DO_SYSLOG, DO_PRINT);
        fclose (file_name_ptr) ;

        if (fa_odl_ingest_return_code == FA_ODL_PERMISSIONS_DENIED)
            return(FA_ODL_PROCESS_PERMDENIED) ;
        else if (fa_odl_ingest_return_code == FA_ODL_PERMISSIONS_FAILED)
            return(FA_ODL_PROCESS_PERMFAILED) ;
        else
            return(FA_ODL_PROCESS_ERROR);
    }


    /* Create a report header */

    if ( !aps_report_header (report_log, 
                            filedef->flight_agency,
                            filedef->file_type,
                            file_name, 
                            fa_creation_date))
    {
#ifdef DEBUG                                        
        printf("Could not create dtk report header\n");
#endif
        aps_log_msg(file_util_progname, APS_INFO, 
            "Could not create dtk report header.  \n", 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, "\nCould not create dtk report header.  \n" ) ;

        fclose (file_name_ptr) ;
        return(FA_ODL_PROCESS_ERROR);
    }
/* ========================================================================= */
/*
-- Now, carry out any actions before processing the list into the database.
*/
    if (fa_trigger_RES_rej_downtime)
    {
        if (RES_rej_downtime (APS_dbproc, input_dtk_list) < 0)
        {
            aps_log_msg(file_util_progname, APS_INFO, 
                "Incomplete RES down times entered. Non-fatal.\n", 
                DO_SYSLOG, DO_PRINT);
            fprintf(report_log, 
                "\nIncomplete RES down times entered. Non-fatal.\n" ) ;
        }
    }

/*============================================================================
      ##     ####    ####   ######   ####    ####           #####   #####
     #  #   #    #  #    #  #       #       #               #    #  #    #
    #    #  #       #       #####    ####    ####           #    #  #####
    ######  #       #       #            #       #          #    #  #    #
    #    #  #    #  #    #  #       #    #  #    #          #    #  #    #
    #    #   ####    ####   ######   ####    ####           #####   #####
 =============================================================================*/
    /* NOW, process the link list into database records */
    accepted_dtks  = create_dyn_llist() ;
    rejected_dtks  = create_dyn_llist() ;
    CON_dtks       = create_dyn_llist() ;
    deleted_dtks   = create_dyn_llist() ;
    error_dtks     = create_dyn_llist() ;
    omission_dtks  = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks  = create_dyn_llist() ;
    dtk_updates    = create_dyn_llist() ;

#ifdef DEBUG                                        
    printf("\n\n\n\n\n about to call dtk_process_list()\n\n\n\n\n");
#endif

    dtkm_process_list_return_code = 
    dtkm_process_dtk_proposal_list (APS_dbproc, input_dtk_list,
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks, error_dtks, 
        omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates, 
        report_log);

    aps_log_msg(file_util_progname, APS_INFO, 
        "DTK STATISTICS\n", 
        DO_SYSLOG, DO_PRINT);
    sprintf(file_util_msg, 
        "DTKS FROM ODL FILE  : %d\n", NUMELTS (input_dtk_list)) ;
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);
    sprintf(file_util_msg, 
        "ACCEPTED            : %d\n", NUMELTS (accepted_dtks)) ;
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);

    sprintf(file_util_msg, 
        "REJECTED            : %d\n", NUMELTS (rejected_dtks)) ;
    if ( NUMELTS (rejected_dtks) > 0 )
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);

    sprintf(file_util_msg, 
        "BLOCKED BY CONFLICTS: %d\n", NUMELTS (CON_dtks)) ;
    if ( NUMELTS (CON_dtks) > 0 )
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);

    if ( NUMELTS (error_dtks) > 0 )
    {
        sprintf(file_util_msg, 
            "ERROR               : %d", NUMELTS (error_dtks)) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_INFO, 
            "        PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n", 
            DO_SYSLOG, DO_PRINT);

        fprintf (report_log, 
            "\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ; 
    }

    sprintf(file_util_msg, 
        "REJECTED BY OMISSION: %d\n", NUMELTS (omission_dtks)) ;
    if ( NUMELTS (omission_dtks) > 0 )
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

    sprintf(file_util_msg, 
        "TOTAL UPDATES       : %d\n", NUMELTS (dtk_updates)) ;
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);
 
    if ( fa_number_of_error_records > 0 )
    {
        sprintf(file_util_msg, 
            "ERROR RECS IN FA FILE  : %d", fa_number_of_error_records ) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        aps_log_msg(file_util_progname, APS_INFO, 
            "        PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n", 
            DO_SYSLOG, DO_PRINT);
 
        fprintf (report_log,
            "\nERROR RECS IN FA FILE  : %d", fa_number_of_error_records ) ;
        fprintf (report_log,
            "\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ;
    }
 
    if (fa_number_of_error_records  > 0
    ||  NUMELTS( error_dtks )    > 0 
    ||  NUMELTS( rejected_dtks ) > 0 
    ||  NUMELTS( CON_dtks )      > 0 
    ||  NUMELTS( omission_dtks ) > 0  )
    {
        /* 
        -- we want the planner to read the 
        -- report.  
        */
        planner_read_status = 1 ;
    }

    /* 
    -- clean up: delete storage structures
    */
    DEL_LIST ( accepted_dtks  ) ;
    DEL_LIST ( rejected_dtks  ) ;
    DEL_LIST ( CON_dtks     ) ;
    DEL_LIST ( deleted_dtks ) ;
    DEL_LIST ( error_dtks   ) ;
    DEL_LIST ( omission_dtks) ;
    DEL_LIST ( other_sat_dtks ) ;
    DEL_LIST ( same_sat_dtks  ) ;
    DEL_LIST ( dtk_updates ) ;
    DEL_LIST ( input_dtk_list ) ;
    fclose (file_name_ptr) ;

    /*
    -- COMPLETED processing.
    -- We no longer carry out Item (5) Terminate the planning permission.
    */
 
	/*
	-- We no longer carry out Item (6) Terminate ACTIVITY permission.
	*/


    if (dtkm_process_list_return_code < 0)
    {
        aps_log_msg(file_util_progname, APS_INFO, 
            DTKM_ERROR_MESSAGE(dtkm_process_list_return_code), 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, 
            "%s\n",DTKM_ERROR_MESSAGE(dtkm_process_list_return_code) );
        fprintf(report_log, "ODL ingestion completion: FAILED\n" ) ;
        return(FA_ODL_PROCESS_ERROR);
    }
    else if ( planner_read_status )
    {
        aps_log_msg(file_util_progname, APS_INFO, 
            "There were errors and/or rejections.  \n", 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, "\nThere were errors and/or rejections.  \n" ) ;
        fprintf(report_log, 
            "ODL PROCESS OK BUT PLANNER MUST READ THE REPORT\n" ) ;
        
        if(fa_number_of_error_records > 0)
            return FA_ODL_PROCESS_ERROR ;
        else
            return FA_ODL_PROCESS_OK_BUT_PLANNER_MUST_READ ;
    }
    else
    {
        aps_log_msg(file_util_progname, APS_INFO, 
            "There were no errors or rejections.  \n", 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, "There were no errors or rejections.  \n" ) ;
        return (FA_ODL_PROCESS_OK) ;
    }

}  /* end of fa_odl_processor  */
