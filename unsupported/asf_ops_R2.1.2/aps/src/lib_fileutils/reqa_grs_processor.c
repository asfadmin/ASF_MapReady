#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       reqa_grs_processor.c

Description:    Read and report on the GRS replies extractec by the 
                file processor from a REQQ file.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)reqa_grs_processor.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.reqa_grs_processor.c"

#include <dapps_defs.h>      /* for TRUE etc.         */
#include <stdio.h>           /* for FILE * etc.       */
#include <grs_utilities.h>   /* for reqqid_decode()   */

#include <db_sybint.h>       /* for db_get_records()  */
#include <aps_db_table.h>    /* for  APS DB tables sybase interface   */
#include <db_dar.h>          /* for CAST_DAR_SITENAME */


/*==============================================================================
Function:       grs_get_sitename()

Description:    get sitename for a dar.  

Creator:        Lawrence Stevens

Creation Date:  Mon Oct  6 21:23:43 PDT 1997

==============================================================================*/
static void grs_get_sitename( int darid, char *sitename ) 
{

    DB_RECORD   **dar_rec ;
    llist       *dar_list ;
    cursor      dar_list_ptr ;

    sprintf( where_clause, " where %s = %d ", APS_COL(DAR, DAR_DARID), darid ) ;

    dar_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DAR),
        where_clause, NULL, APS_CDEFS(DAR), ALL_COLS) ;

    if( dar_list == NULL )
        return ;

    if( NUMELTS(dar_list) != 1 )
    {
        DEL_LIST( dar_list ) ;
        return ;
    }

    dar_rec = (DB_RECORD **) FIRST(dar_list, dar_list_ptr) ;

    strcpy( sitename, CAST_DAR_SITENAME dar_rec[DAR_SITENAME] ) ;

    DEL_LIST( dar_list ) ;
    return ;

}




/*==============================================================================
Function:       reqa_grs_processor()

Description:    Read and report on the GRS replies extractec by the 
                file processor from a REQQ file.  

Creator:        Lawrence Stevens

Creation Date:  Fri Oct  3 13:18:34 PDT 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

int reqa_grs_processor( 
    FILE    *grs_fp,        /* input file of GRS replies.  */
    FILE    *report_fp )    /* output report file          */
{
    char    grs_buf[100] ;
    char    sitename[35] ;
    int     return_code ;

    int     prev_darid ;
    int     dar_count ;
    int     dar_reply_count ;
    int     dar_planned_count ;
    int     dar_unplanned_count ;

    long    darid ;
    int     path ;
    int     row ;
    int     reqq_phasenum ;
    int     prev_reqq_phasenum ;    /* previous reqq_phasenum    */
    int     image_num ;     /* this number starts at 0 for the first image.  */


    /* rewind to the start of the file:  */
    (void) fseek( grs_fp, 0, SEEK_SET ) ;

    grs_buf[82] = '\0' ;
    /*
    --     (From Revision 4, April 15, 1997  of 
    --     JERS-1 Operation Interface Specification)
    -- 
    -- EXPLANATION OF REQA 882-character RECORD:
    -- 
    -- PART 1:  Fields 1-3 (first 28 byte of 82-character records) 
    -- ------
    --
    --           1         2          \  offset
    -- 0123456789012345678901234567   /
    -- IIIIIIIINORMYYYYMMDDYYYYMMDD
    -- |       |   |       |
    -- |       |   |       | End observing period.   8 chars
    -- |       |   |
    -- |       |   | Start observing period.   8 chars
    -- |       |   
    -- |       | Format check condition.   4 chars
    -- |          
    -- | GRS Request ID, 8 characters:  APS GRS convention:  PDDDRRRI
    --   P   = phase number 
    --   DDD = Darid
    --   RRR = path and row numbers:  1000*path + row
    --   I   = image number, starting at 0
    --   The GRS Request ID numbers are decoded into separate numbers by:  
    --      void reqqid_decode(
    --          char reqqid[],
    --          long *darid,
    --          int *path,
    --          int *row,
    --          int *phase,
    --          int *image  ) ;
    --
    --
    -- PART 2:  Fields 5-8 (last  54 bytes of 82-character record) 
    -- ------
    -- 2 3         4         5         6         7         8    \  offset
    -- 890123456789012345678901234567890123456789012345678901   /
    -- XXXYYY.YYXXXYYY.YYYYYYMMDDYYYYMMDDhh:mm:sshh:mm:ssFAIS
    -- |  |     |  |     |       |       |       |       |
    -- |  |     |  |     |       |       |       |       | Ground station (4)
    -- |  |     |  |     |       |       |       |        
    -- |  |     |  |     |       |       |       | stop time of downlink (8)
    -- |  |     |  |     |       |       |       
    -- |  |     |  |     |       |       | start time of downlink (8 characters)
    -- |  |     |  |     |       |
    -- |  |     |  |     |       | Date of downlink (8 characters)
    -- |  |     |  |     | 
    -- |  |     |  |     | Date of observation (8 characters)
    -- |  |     |  |       
    -- |  |     |  | End angle of RSP observation (6 characters YYY.YY)
    -- |  |     |
    -- |  |     | End path of RSP observation ( 3 characters. )
    -- |  | 
    -- |  | Start angle of RSP observation (6 characters YYY.YY)
    -- |  
    -- | Start path of RSP observation ( 3 characters. )
    --
    -- BLANK values for any of the RSP fields mean that the request 
    -- was not planned.  
    --
    -- NOTE:  these records have been read by the APS and 
    --        written to a file with newlinew (\n) to separate the records.  
    --        This enables them to be sorted before processing here.  
    */

    /* 
    -- version 1:  print out every REQA record.  
    */
    fprintf( report_fp, "REPORT OF REPLIES TO GRS REQUESTS:\n" ) ;
    fprintf( report_fp, "----------------------------------\n\n" ) ;
    fprintf( report_fp, 
        "DARID  REPLIES  PLANNED  UNPLANNED  PCT PLANNED" ) ;
    fprintf( report_fp, " SITENAME\n" ) ;
    fprintf( report_fp, 
        "-----  -------  -------  ---------  -----------" ) ;
    fprintf( report_fp, " -------------------------------\n" ) ;

    prev_reqq_phasenum = 0 ;
    prev_darid = 0 ;
    dar_count = 0 ;
    dar_reply_count = 0 ;
    dar_planned_count = 0 ;
    dar_unplanned_count = 0 ;
    while ( ( fscanf( grs_fp, "%83c", grs_buf ) ) == 1 )
    {
        reqqid_decode( grs_buf, 
            &darid, &path, &row, &reqq_phasenum, &image_num ) ;
        if( darid != prev_darid || reqq_phasenum != prev_reqq_phasenum )
        {
            if( prev_darid )
            {
                dar_count ++ ;
                grs_get_sitename( darid, sitename ) ;

                /* print stats for this DAR.  */
                fprintf( report_fp, 
                    "%5d    %5d    %5d      %5d        %5.1f %-32s\n",
                    prev_darid, 
                    dar_reply_count, 
                    dar_planned_count, 
                    dar_unplanned_count, 
                    100.0 * dar_planned_count / dar_reply_count,
                    sitename ) ;
            }

            /* start of new darid; start counts.  */
            prev_darid = darid ;
            dar_reply_count = 0 ;
            dar_planned_count = 0 ;
            dar_unplanned_count = 0 ;

            if( reqq_phasenum != prev_reqq_phasenum )
            {
                /* start of new reqq phase; print title for it:  */
                prev_reqq_phasenum = reqq_phasenum ;
                fprintf( report_fp, "\nREQQ Phase %d\n", reqq_phasenum ) ;
            }
        }
        
        /*   fprintf( report_fp, "%82.82s\n", grs_buf ) ;    */

        /* accumulate stats  */
        dar_reply_count ++ ;

        if( strncmp( "     ", grs_buf+28, 5 ) == 0 )
            dar_unplanned_count ++ ;
        else
            dar_planned_count ++ ;

    }

    if( prev_darid )
    {
        /* print stats for last DAR.  */
        dar_count ++ ;
        grs_get_sitename( darid, sitename ) ;

        fprintf( report_fp, 
            "%5d    %5d    %5d      %5d        %5.1f %-32s\n",
            prev_darid, 
            dar_reply_count, 
            dar_planned_count, 
            dar_unplanned_count, 
            100.0 * dar_planned_count / dar_reply_count,
            sitename ) ;
    }

    fprintf( report_fp, "\n\nNUMBER OF DARS REPORTED:  %d \n", dar_count ) ;
    fprintf( report_fp, 
        "\nEND OF GRS REPORT (Report of RSP replies, if any, to follow)\n" ) ;
    fprintf( report_fp, 
        "-------------------------------------------------------------\n\n" ) ;

    return TRUE ;

}
