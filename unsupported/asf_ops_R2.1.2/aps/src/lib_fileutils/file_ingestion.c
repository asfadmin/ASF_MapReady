/* for debugging purposes */
#undef   PRINT_DIAG 

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   file_ingestion.c 

Description:    

External Functions Defined:
GLOBAL  Function: fa_ascii_processor 
                    Manages ingestion of ASCII files.
GLOBAL  Function: identify_file
                    Processes, scans a file's header.
GLOBAL  Function: identify_PMF_file
                    Processes, scans a file's data records.
                    Places info in linked list.

File Scope Functions:
STATIC  Function:       db_record_rev_comparison
STATIC  Function:       db_record_llist2ord_llist
STATIC  Function:       db_record_ord_llist2llist
STATIC  Function:       get_ascii_and_convert
STATIC  Function:       populate_subrecord_control
STATIC  Function:       fa_ascii_header_ingestion
STATIC  Function:       fa_ascii_record_ingestion
STATIC  Function:       dtk_status_by_blanking
STATIC  Function:       dtk_fillin_FA_response
STATIC  Function:       dtk_create_esa_observation_dtks
STATIC  Function:       dtk_delete_range

    
External Variables Defined:
VALUE_DEFS  [] array describing ASCII File and its
                        fields and conversion requirements, as well as 
                        database destinations for this information.
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)file_ingestion.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.file_ingestion.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read             */
#include <sys/uio.h>        /* for read             */
#include <unistd.h>         /* for read             */

/* FOR SYBASE INTERFACES   */
#include <db_sybint.h>      /* for APS sybase interface routines    */
#include <aps_db_table.h>   /* for APS DB table  definitions        */
#include <dapps_list.h>     /* for APS linked list macros           */
#include <dapps_defs.h>     /* for APS basic definitions            */
#include <apsfiledef.h>     /* for APS error definitions            */
#include <db_dtk.h>         /* for dtk table definitions */
#include <aps_log_msg.h>    /* for APS logging                      */
#include <grs_utilities.h>  /* for reqqid_is_rsp()                  */


/* FOR DTK UTILITIES DEFINITIONS - MULTI_ANTENNA  */
#include <dtkm_utilities.h>

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* FOR FA_ASCII_REC_PROCESSOR */
#include "GENconversions.h"
#include "ESAconversions.h"
#include "NASDAconversions.h"

/* FOR FILE UTILITIES DEFINITIONS */
#include "file_utilities.h"

/* FOR APSPMF_metadata DEFINITION */
#include "APSpmfutilities.h"

/* FOR permission mu_utilities */
#include <mu_utilities.h>

/* NOTE: the following messages MUST be matched by corresponding
--      entries in the file file_utilities.h 
--
--      Ignore LINT warning: declared global, could be static
*/
char *fa_ascii_rec_error_message[] = 
{
    "A ZERO ERROR CODE WAS ENCOUNTERED, INCORRECT",  /* 0*/
    "UNABLE TO OPEN THE ASCII DESTINATION FILE   ",  /* 1*/
    "A FIELD IN THE FILE HEADER WAS INCORRECT    ",  /* 2*/
    "INTERNAL ERROR COPYING A DBRECORD           ",  /* 3*/
    "A FIELD IN A FILE DATA RECORD WAS INCORRECT ",  /* 4*/
    "INTERNAL ERROR CALLING DTK_DEFAULTS FUNCTION",  /* 5*/
    "UNABLE TO FIND A FIELDS DEFAULT DEFINITION  ",  /* 6*/
    "UNABLE TO READ THE FILE DESCRIPTIVE HEADER  ",  /* 7*/
    "A FIELD IN A FILE DATA SUBRECORD WAS INCORRECT" /* 8*/

#ifdef MIGUEL_COMMENT_OUT
    "zero is not a valid error code",    /* 0*/
    "FA_ASCII_FILE_UNABLE_TO_OPEN",      /* 1*/
    "FA_ASCII_REC_HEADER_NOT_OK",        /* 2*/
    "FA_ASCII_REC_DBRECORD_COPY_FAILED", /* 3*/
    "FA_ASCII_REC_AGGREGATE_NOT_OK",     /* 4*/
    "FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND",/* 5*/
    "FA_ESAF_DEFAULT_NOT_FOUND",         /* 6*/
    "FA_ASCII_REC_DESCRIPTOR_NOT_FOUND", /* 7*/
    "FA_ASCII_REC_SUBRECORD_NOT_OK"     /* 8*/
#endif
};


/*==============================================================================
Function:       db_record_rev_comparison

Description:    Comparison routine used by create_dyn_ord_list() and 
                db_record_llist2ord_llist to create and maintain an
                ordered_llist.

Parameters:     (see function declaration below for definitions)

Returns:        
Type  Definition
int     0.      Both arguments are equal
int    -1.      Argument 2 is greater than argument 1. 
int     1.      Argument 1 is greater than argument 2.

Creator:        Miguel Siu

Creation Date:  Wed Nov 29 16:11:26 PST 1995

Notes:
==============================================================================*/
static int 
db_record_rev_comparison (DB_RECORD **arg1, DB_RECORD **arg2)
{
    if (CAST_DTK_REV arg1[DTK_REV] == CAST_DTK_REV arg2[DTK_REV]) 
        return(0) ;

    if (CAST_DTK_REV arg1[DTK_REV] < CAST_DTK_REV arg2[DTK_REV]) 
        return (-1) ;
    else
        return (1) ;
}

/*==============================================================================
Function:       db_record_llist2ord_llist 

Description:    Takes a populated llist input, and creates its ordered llist 
                equivalent.

Parameters:     (see function declaration below for definitions)

Returns:        
Type  Definition
int   TRUE. The ordered llist was created successfully.
int   FALSE. An error was encountered.  State of ordered llist is unknown.

Creator:        Miguel Siu

Creation Date:  Wed Nov 29 16:11:26 PST 1995

Notes:  If incoming list is empty, this routine will successfully
        return an empty ordered list !!  (return TRUE)
        This routine uses whatever comparison function was defined at the
        creation of the ord_llist. (See dapps_ord_list.c for more info)
==============================================================================*/
static int 
db_record_llist2ord_llist(
                    llist       *incoming_list, /* populated with data */
                    ord_llist   *outgoing_list) /* originally empty    */
{
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **unlinked_dtk_rec ;
    cursor      dtk_rec_ptr ;

    if (incoming_list == NULL)
    {
        outgoing_list = (ord_llist *)NULL ;
        return (TRUE) ;
    }

    /*
    -- move each record out of the list and 
    -- into another list.  
    */
    for (
        dtk_rec = (DB_RECORD **)FIRST(incoming_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **)FIRST(incoming_list, dtk_rec_ptr) 
        )
    {
        /* 
        -- remove from the list.  
        */
        unlinked_dtk_rec = (DB_RECORD **)
                                UNLINK_AT_CURSOR(incoming_list, dtk_rec_ptr) ;
        if (unlinked_dtk_rec != dtk_rec)
            return (FALSE) ;
 
        if (!INS_BY_KEY(outgoing_list,
                             dtk_rec,
                             free_db_record,
                             dtk_rec) ) return (FALSE) ;

    } 

    return (TRUE) ;
}

/*==============================================================================
Function:       db_record_ord_llist2llist 

Description:    Takes an ordered llist input, and moves it into a llist 

Parameters:     (see function declaration below for definitions)

Returns:        
Type  Definition
int   TRUE. The ordered llist was created successfully.
int   FALSE. An error was encountered.  State of ordered llist is unknown.

Creator:        Miguel Siu

Creation Date:  Wed Nov 29 16:11:26 PST 1995

Notes:  If incoming ordered llist is empty, this routine will successfully
        return an empty llist !!  (return TRUE)
==============================================================================*/
static int 
db_record_ord_llist2llist(
                    ord_llist   *incoming_list, /* populated with data */
                    llist       *outgoing_list) /* originally empty    */
{
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **unlinked_dtk_rec ;
    cursor      dtk_rec_ptr ;

    if (incoming_list == NULL)
    {
        outgoing_list = (llist *)NULL ;
        return (TRUE) ;
    }

    /* remove each rec from the list.  */
    for (
        dtk_rec = (DB_RECORD **)FIRST(incoming_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **)FIRST(incoming_list, dtk_rec_ptr) 
        )
    {
        unlinked_dtk_rec = (DB_RECORD **)
                                UNLINK_AT_CURSOR(incoming_list, dtk_rec_ptr) ;
        if (unlinked_dtk_rec != dtk_rec)
            return (FALSE) ;
 
        if (!APPEND(outgoing_list,
                             dtk_rec,
                             free_db_record,
                             dtk_rec) ) return (FALSE) ;

    }

    return (TRUE) ;
}

/*==============================================================================
Function:       get_ascii_and_convert

Parameters:     (see function declaration below for definitions)

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
get_ascii_and_convert(
    char        *ascii_buffer,
    int         offset,
    int         length,
    int         (conversion()), 
    EQUIV_TABLE *conversion_table,
    void        *dest)
{
    char          string[512];
    int           conversion_status;

    if ( length > 0 )
    {
        /* 
        -- use offset, length for source string.  
        */
        strncpy (string, &ascii_buffer[offset], length);
        string[length] = NULL;
    }
    else
    {
        if ( (char *)conversion_table != NULL )
        {
            /* 
            -- the conversion_table is really a pointer to
            -- the needed string value, because length <= 0 
            -- and this pointer is != NULL
            */
            strcpy(string, (char *) conversion_table ) ;
        }
    }

#ifdef PRINT_DIAG
    printf("\nget_ascii_and_convert:found |%s|\n", string); 
#endif

    conversion_status = conversion(conversion_table, string, dest);
    if (conversion_status < 0) 
        conversion_status = 0 ;
    return(conversion_status);
}             



/*==============================================================================
Function:       populate_subrecord_control

Description:    Take the input OPL1 descriptor and extract information regarding
                the number of subrecords in the OPL1 file

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Mon Oct 16 11:14:06 PDT 1995

Notes:      
==============================================================================*/
static int 
populate_subrecord_control (char * descriptor_string)
{
    char    four_bytes[] = "xxxx" ;
    int     i ;

    /* should copy from the start of the descriptor_string.  */
    /*
    strncpy (four_bytes, descriptor_string + 1, 4) ;
    */
    strncpy (four_bytes, descriptor_string, 4) ;

    if ( !gen_field_is_numeric( four_bytes, 4 ) )
        return FALSE ;
    fa_number_of_records = atoi(four_bytes) ;

    for (i=0; i<fa_number_of_records; i++) 
    {
        strncpy (four_bytes, descriptor_string + 11*(i+1), 4) ;

        if ( !gen_field_is_numeric( four_bytes, 4 ) )
            return FALSE ;
        fa_subrecord_control[i] = atoi(four_bytes) ;

    }

    return (TRUE) ;
}

/*==============================================================================
Function:       fa_ascii_header_ingestion

Description:    

Parameters:     

Returns:        
Type        Name                Definition
int                             FA_ASCII_HEADER_INGESTION_OK = Success
                                other:
                                FA_ASCII_REC_HEADER_NOT_OK
                                FA_DEFAULT_NOT_FOUND

Creator:        Miguel Siu

Creation Date:  Mon Jul 24 12:26:50 PDT 1995

Notes:      
==============================================================================*/
static int 
fa_ascii_header_ingestion(
    FILE            *ascii_file,
    DB_RECORD       **template_dtk_rec,
    FA_FILEDEF      *file_def, 
    FILE            *report_fp )      /* report file pointer.  */
{
    char    four_bytes[] = "    " ;
    char    *descriptor   = NULL ;
    char    *ascii_buffer = NULL ;
    int     testcount ;
    int     i, descriptor_size ;

    DB_RECORD     *destination;



    /*
    -- Read the ASCII header
    -- (There is special handling for the SHAQP variable header. 
    -- See the end of the routine).
    */
    ascii_buffer = malloc(file_def->header_length + 1) ;
    testcount=fread (ascii_buffer, file_def->header_length, 1, ascii_file);
    ascii_buffer[file_def->header_length] = '\0' ;

#ifdef PRINT_DIAG                                        
    printf("HEADER:\n%s\n\n",ascii_buffer);
#endif


    /* 
    -- find values for header and report definition fields
    */

    i = 0;
    while( file_def->field_def[i].source_code )
    {
        /*
        -- Assign a  destination, index or pointer as needed.
        */
        if (file_def->field_def[i].source_code == FILE_RECORD 
        ||  file_def->field_def[i].source_code == FILE_SUBRECORD ) 
        {
            i++;
            continue;
        }
        

        if (file_def->field_def[i].destination_code == REPORT_RECORD)
            destination = template_dtk_rec[
                            file_def->field_def[i].destination.index];
        else
            destination =(DB_RECORD *)
                            file_def->field_def[i].destination.pointer;


        if (file_def->field_def[i].source_code == FA_DEFAULT)
        {
            /*
            -- assign values to the destination pointer
            */
            if(! (file_def->field_def[i].conversion)(NULL, NULL, destination))
                return (FA_DEFAULT_NOT_FOUND);
        }
        else if (file_def->field_def[i].source_code == FILE_HEADER)
        {
            if (! get_ascii_and_convert(
                  ascii_buffer,
                  file_def->field_def[i].offset,
                  file_def->field_def[i].length,
                  file_def->field_def[i].conversion,
                  file_def->field_def[i].equiv_table,
                  destination ) )
            {
                /* error.  stop processing the file.  */
                sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR parsing HEADER offset=%d, length=%d current record:", __FILE__, __LINE__, 
                    file_def->field_def[i].offset, 
                    file_def->field_def[i].length ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                aps_log_msg(file_util_progname, APS_ERROR, ascii_buffer, 
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_fp, "%s(%d):  fa_ascii_header_ingestion():\
ERROR parsing HEADER offset=%d, length=%d \n", __FILE__, __LINE__, 
                    file_def->field_def[i].offset, 
                    file_def->field_def[i].length ) ;
                fprintf(report_fp, "current FA record:  \n%s\n", ascii_buffer);

                /* 
                -- OK to write a null byte onto 
                -- ascii_buffer; processing is ending. 
                */
                ascii_buffer[
                    file_def->field_def[i].offset 
                    + file_def->field_def[i].length 
                    ] = '\0' ;
                sprintf (file_util_msg, "current field = >%s<\n",
                    &ascii_buffer[file_def->field_def[i].offset] ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_fp,  "current field = >%s<\n", 
                    &ascii_buffer[file_def->field_def[i].offset] ) ;
                fprintf(report_fp,  "\nFA file processing terminating.\n" ) ;

                return (FA_ASCII_REC_HEADER_NOT_OK);
            }
        }
        else if (file_def->field_def[i].source_code == FILE_DESCRIPTOR)
        {
            int n_acquisition_info ;
            /* 
            -- see ADEOS FIle descriptions V4 June 21, 1995
            -- page 21, Table 2, for the OPL1 file descriptor 
            -- description.  
            */
            testcount = fread (four_bytes, 4, 1, ascii_file);
            if ( testcount != 1 )
            {
                /* error.  stop processing the file.  */
                sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR reading OPL1 descriptor (first four bytes)\n", __FILE__, __LINE__ ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                /*sprintf (file_util_msg, 
                    "current FA record:  \n%s\n", ascii_buffer ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);*/

                fprintf(report_fp,"%s(%d):  fa_ascii_header_ingestion():\
ERROR reading OPL1 descriptor (first four bytes)\n", __FILE__, __LINE__ ) ;
        /* fprintf(report_fp, "current FA record:  \n%s\n", ascii_buffer); */

                fprintf(report_fp,  "\nFA file processing terminating.\n" ) ;

                return (FA_ASCII_REC_HEADER_NOT_OK);
            }
            if ( !gen_field_is_numeric( four_bytes, 4 ) )
            {
                /* error.  stop processing the file.  */
                sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR decoding OPL1 Total Number of Acquisition Information field,\n", 
                    __FILE__, __LINE__) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                sprintf (file_util_msg, "offset=0, length=4 is NOT NUMERIC\n");
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);


                /* error.  stop processing the file.  */
                fprintf(report_fp, "%s(%d):  fa_ascii_header_ingestion():\
ERROR decoding OPL1 Total Number of Acquisition Information field \noffset=0, length=4 is NOT NUMERIC\n", __FILE__, __LINE__ ) ;

                sprintf (file_util_msg, "current field = >%s<\n",four_bytes ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                fprintf(report_fp, "current field = >%s<\n", four_bytes) ;
                return (FA_ASCII_REC_HEADER_NOT_OK);
            }
            n_acquisition_info = atoi(four_bytes) ; 

            descriptor_size = 5 + n_acquisition_info * 11 ; 
            descriptor = malloc( descriptor_size + 1 ) ;
            strncpy(descriptor, four_bytes, 4) ;
            testcount = fread(descriptor+4, descriptor_size - 4, 1, ascii_file);
            if ( testcount != 1 )
            {
                /* error.  stop processing the file.  */
                sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR reading OPL1 descriptor (full record)\n", __FILE__, __LINE__ ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                /* sprintf (file_util_msg, 
                    "current FA record:  \n%s\n", ascii_buffer ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT); */

                fprintf(report_fp, "%s(%d):  fa_ascii_header_ingestion():\
ERROR reading OPL1 descriptor (full record)\n", __FILE__, __LINE__ ) ;
        /* fprintf(report_fp, "current FA record:  \n%s\n", ascii_buffer); */

                fprintf(report_fp,  "\nFA file processing terminating.\n" ) ;
                return (FA_ASCII_REC_HEADER_NOT_OK);
            }

            /* 
            -- dynamically allocate space for the fa_subrecord_control array 
            */
            fa_subrecord_control = (int *)calloc(atoi(four_bytes), sizeof(int));
            if ( !fa_subrecord_control)
            {
                /* error.  stop processing the file.  */
                sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR allocating array for OPL1 file descriptor\n", __FILE__, __LINE__ ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_fp, "%s(%d):  fa_ascii_header_ingestion():\
ERROR allocating array for OPL1 file descriptor\n", __FILE__, __LINE__ ) ;

                fprintf(report_fp,  "\nFA file processing terminating.\n" ) ;
                return (FA_ASCII_REC_HEADER_NOT_OK);
            }

            /*
            -- now, populate the global variable fa_subrecord_control
            */
            populate_subrecord_control (descriptor) ;
        }
        else
        {
            /* 
            -- error.  unknown source code. 
            -- consult a programmer. 
            */
            /* error.  stop processing the file.  */
            sprintf (file_util_msg, "%s(%d):  fa_ascii_header_ingestion():\
ERROR in VALUEDEFS entry %d\n", __FILE__, __LINE__, i ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_ERROR, 
"UNKNOWN SOURCE CODE; consult programmer. \n", 
                DO_SYSLOG, DO_PRINT);
            /*sprintf (file_util_msg, 
                "current FA record:  \n%s\n", ascii_buffer ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);*/

            fprintf(report_fp, "%s(%d):  fa_ascii_header_ingestion():\
ERROR in VALUEDEFS entry %d\n", __FILE__, __LINE__, i ) ;
            fprintf(report_fp, 
"UNKNOWN SOURCE CODE; consult programmer. \n") ;
            fprintf(report_fp, "current FA record:  \n%s\n", ascii_buffer ) ;

            fprintf(report_fp,  "\nFA file processing terminating.\n" ) ;

            return (FA_ASCII_REC_HEADER_NOT_OK);
        }

        i++; 

    }

    if (fa_trigger_read_variable_SHAQP_header)
    {
        /* 
        -- Read header file_def->header_length (small number) bytes at a time.
        -- Read header until "---" is found, identifying last header record.
        -- Then read one character at a time until the end of record is found.
        -- Keep a counter to avoid an infinite loop; we know that the current
        -- header is ~358 characters long, so loop 1000 times or less.
        -- And the final header record should be 65, so loop 200 times or less. 
        */
        for 
        (
        i=0, 
        testcount=fread (ascii_buffer, file_def->header_length, 1, ascii_file);
        strcmp(ascii_buffer, "---") != 0 && i < 1000 ;
        testcount=fread (ascii_buffer, file_def->header_length, 1, ascii_file),
        i++
        ) ;
        if (i == 1000)
            return (FA_ASCII_REC_HEADER_NOT_OK);
        for
        (
        i=0, testcount=fread (ascii_buffer, 1, 1, ascii_file);
        ascii_buffer[0] != '\n' && i < 200 ;
        i++, testcount=fread (ascii_buffer, 1, 1, ascii_file)
        ) ;
        if (i == 200)
            return (FA_ASCII_REC_HEADER_NOT_OK);
    }

    free (ascii_buffer) ;
    free (descriptor) ;
    return (FA_ASCII_HEADER_INGESTION_OK) ;
}


/*==========================================================================
Function:      fa_ascii_record_ingestion 

Description:    
    Scans ASCII data records for information which is placed in a 
    DB_RECORD linked list.
    This routine reads a pre-determined number of data records, or 
    keeps on reading until end-of-file (EOF) is reached.


Parameters:     

Returns:        
Type        Name                Definition
int                             FA_ASCII_RECORD_INGESTION_OK = Success
                                other:
                                FA_ASCII_REC_DBRECORD_COPY_FAILED   -6
                                FA_ASCII_REC_AGGREGATE_NOT_OK       -7
                                FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND -8

Creator:        Miguel Siu

Creation Date:  Thu Jun 15 11:51:51 PDT 1995

Notes:      
    To read to end-of-file, pass the pre-defined READ_TO_EOF value for 
    the parameter fa_number_of_records to the FIRST call of this routine.
    One way to accomplish this:
        Place a value of READ_TO_EOF in the parameter fa_number_of records
        which is found in the VALUE_DEFS array accessed by the 
        FA_FILEDEF definition below.

    This routine may be recursively called to process subrecord groups.
    The routine stops when a zero (0) number of records is encountered.
    One way to accomplish this:
        Use a zero_default_routine (ie:NASDA_default_subrecords) to 
        initialize variable fa_number_of_subrecords, which is found in the 
        VALUE_DEFS array which is accessed by the FA_FILEDEF definition below.
==========================================================================*/
static int 
fa_ascii_record_ingestion(
    FILE        *ascii_file,
    int         level_indicator,
    DBPROCESS   *APS_dbproc,        /* open Sybase database process */
    DB_RECORD   **local_template_dtk_rec,
    llist       *input_dtk_list,
    FA_FILEDEF  *file_def,
    int         number_of_records,  /* MUST be stated,for subrecord processing*/
    FILE        *reqa_grs_file_ptr, /* output file for REQA GRS recs.     */
    FILE        *report_fp   )      /* output report file pointer.  */
{
    int         j ;
    int         data_records2process;
    int         return_code;
    char        *ascii_buffer = NULL;

    int         number_of_errors_in_record ;
    int         append_this_record_flag ;
    char        field_buf[256] ;

    DB_RECORD     *destination;
    DB_RECORD     **input_dtk_rec ;

    /* 
    -- these 3 strings are used in the mpsg file processing
    -- when determining if a data-take is within a 
    -- ground station mask, and if so, which mask, (ASF or MCM)
    -- in the call to dtkm_determine_station_mask()
    -- the time bracket is there since the data-take 
    -- may have to be trimmed just a bit to fit within 
    -- the station mask.  Nominal orbit is used in 
    -- determining if the satellite is within the 
    -- station mask for the given dtk time bracket.  
    */
    char    dtk_mpsg_strttime[ASF_TIME_STR_LENGTH+1] ;  
    char    dtk_mpsg_stoptime[ASF_TIME_STR_LENGTH+1] ;
    char    dtk_mpsg_station_id[10] ;

    ascii_buffer = malloc(file_def->record_length + 1) ;


    /*
    -- Process aggregation variables, append information to link list
    */

    data_records2process = number_of_records ;

    /*
    -- Allocate an empty    DBRECORD template
    */
    input_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

    while ( data_records2process ) 
    {
        number_of_errors_in_record = 0 ;

        /* set up appending flag to yes for now.  */
        append_this_record_flag = 1 ;

        if ( !fread (ascii_buffer, file_def->record_length, 1, ascii_file))
        {
            /*
            -- no more data records, terminate processing
            */
            break ;
        }
        ascii_buffer[file_def->record_length] = '\0' ;

#ifdef PRINT_DIAG                                        
        printf("RECORD:\n%s\n",ascii_buffer);
#endif

        if( reqa_grs_file_ptr != NULL ) 
        {
            /* 
            -- the input file is REQA and we will write 
            -- GRS replies into FILE *reqa_grs_file_ptr
            -- and not process them here.  
            -- check the first 8 chars in ascii_buffer 
            -- to determine of the REQQ ID was RSP or GRS.  
            */
            if( reqqid_is_rsp(ascii_buffer) != TRUE )
            {
                /* this is a reply to a GRS 
                -- request.  handle it differently  
                -- save it into the GRS file and skip it.  
                -- put in newlines, for sorting later.  
                */
                fprintf( reqa_grs_file_ptr, "%s\n", ascii_buffer ) ;

                /* 
                -- we have processed a GRS record, decrement counter
                -- (no effect on READ_TO_EOF condition)
                */
                data_records2process -- ;

                /* skip to the next record:  */
                continue ;

            }  /* END processing GRS reply.   */

        } /* END  if processing REQA file.           */

        /*
        -- Copy a new empty DBRECORD to use when reading each record
        -- from the file.
        */
        if (db_copy_record(APS_CDEFS(DTK),
                input_dtk_rec, local_template_dtk_rec) != DB_COPY_RECORD_OK)
        {
#ifdef PRINT_DIAG                                        
            printf ("Could not copy template of DBRECORD. \n");
#endif
            free_db_record( input_dtk_rec ) ;
            return (FA_ASCII_REC_DBRECORD_COPY_FAILED);
        }


        /*
        -- Process all fields in this record.
        */

        for (j = 0 ; file_def->field_def[j].source_code ; j++ )
        {
            /*
            -- Assign a  destination, index or pointer as needed.
            */
            if (file_def->field_def[j].source_code == FILE_HEADER
            ||  file_def->field_def[j].source_code == FILE_DESCRIPTOR 
            ||  file_def->field_def[j].source_code == FA_DEFAULT )
                continue;

            if (file_def->field_def[j].source_code == FILE_SUBRECORD )
            {
                if ( fa_number_of_subrecords )
                {
                    return_code = fa_ascii_record_ingestion (
                        ascii_file,
                        level_indicator + 1,
                        APS_dbproc,         
                        input_dtk_rec,
                        input_dtk_list,
                        file_def->field_def[j].equiv_table,
                        fa_number_of_subrecords, 
                        reqa_grs_file_ptr, 
                        report_fp ) ; 
                    /* 
                    -- last parameter MUST be stated, for subrecord processing
                    */

                    /* 
                    if ( return_code != FA_ASCII_RECORD_INGESTION_OK)
                    {
                        -- These subrecords had an error, which were noted
                        -- during fa_ascii_record_ingestion(), 
                        -- but continue processing the file.
                        --
                        -- We no longer return FA_ASCII_REC_SUBRECORD_NOT_OK
                        -- and we no longer terminate processing.
                    }
                    */;
                }
                continue ;
            }

            /*
            -- The file_def->field_def[j].source_code must be FILE_RECORD
            */
            if( file_def->field_def[j].source_code != FILE_RECORD )
                continue ;

            /*
            -- Process the field values in a data record.
            */
            if (file_def->field_def[j].destination_code == REPORT_RECORD)
                destination =input_dtk_rec[
                                file_def->field_def[j].destination.index];
            else
                destination =(DB_RECORD *)
                                file_def->field_def[j].destination.pointer;

            if ( ! get_ascii_and_convert(
                ascii_buffer,
                file_def->field_def[j].offset,
                file_def->field_def[j].length,
                file_def->field_def[j].conversion,
                file_def->field_def[j].equiv_table,
                destination))
            {
                /* error.  note error, do not append, and continue */
                number_of_errors_in_record++ ;
                append_this_record_flag = 0 ;

                sprintf (file_util_msg, "%s(%d):  fa_ascii_record_ingestion():\
ERROR IN RECORD offset=%d, length=%d:", __FILE__, __LINE__, 
                    file_def->field_def[j].offset, 
                    file_def->field_def[j].length ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_fp, "%s(%d):  fa_ascii_record_ingestion():\
ERROR IN RECORD offset=%d, length=%d:", __FILE__, __LINE__, 
                    file_def->field_def[j].offset, 
                    file_def->field_def[j].length ) ;

                /* 
                -- copy the field for printing.  
                */
strncpy(field_buf, ascii_buffer+file_def->field_def[j].offset, file_def->field_def[j].length);
                field_buf[file_def->field_def[j].length] = '\0' ;

                sprintf (file_util_msg,
                    "  field value = >%s<\n", field_buf ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                fprintf(report_fp, "  field value = >%s<\n", field_buf ) ;

                /* 
                -- continue with normal processing of the rest 
                -- of the fields. 
                */
            }
        }  /*  END for (j = 0 ; file_def->field_def[j].source_code ; j++ )  */

        /* 
        -- the current record has been 
        -- read; all fields.  
        */

        if ( number_of_errors_in_record != 0 )
        {
            /* 
            -- the record had 
            -- ERROR(s) in it. 
            -- we will not append it:  
            */
            append_this_record_flag = 0 ;

            if ( number_of_errors_in_record == 1 )
            {
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "SKIPPING THIS RECORD due to 1 error:\n", 
                    DO_SYSLOG, DO_PRINT);
                aps_log_msg(file_util_progname, APS_ERROR, 
                    ascii_buffer, 
                    DO_SYSLOG, DO_PRINT);
                fprintf(report_fp,
                    "SKIPPING RECORD due to 1 error:  \n%s\n",
                    ascii_buffer ) ;
            }
            else
            {
                sprintf (file_util_msg,
                    "SKIPPING THIS RECORD due to %d errors:\n",
                    number_of_errors_in_record ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                aps_log_msg(file_util_progname, APS_ERROR, 
                    ascii_buffer, 
                    DO_SYSLOG, DO_PRINT);
                fprintf(report_fp, 
                    "SKIPPING RECORD due to %d errors:  \n%s\n", 
                    number_of_errors_in_record, ascii_buffer ) ;
            }
        }   /* END if errors in record.  */
        else
        {
            /* 
            -- the record had no ERROR in it. we are still working 
            -- with it.  
            */
            /*
            -- if fa_processing_flag set to "READ" process the record,
            -- if "SKIP" there is no action.
            --
            -- if this is the acquisition record, check for 
            -- "SKIP"/"RITE" action.
            */
            if ( strcmp(fa_processing_flag,"READ") == 0 )
            {
                if ( strcmp(fa_record_write_flag,"SKIP") !=0 
                ||   level_indicator != 0 ) 
                {
                    /* do not skip this record, PROCESS it.  */

                    /* 
                    -- Before trying to get default values for coverage,
                    -- check that this datatake proposal is in the mask.
                    -- This operation is used for processing MPSG, which
                    -- may contain lots of out-of-mask data-takes.
                    */
                    if (fa_trigger_MPSG_station_mask_check)
                    {
                        fa_trigger_skip_default_values = 0 ;

                        return_code = dtkm_determine_station_mask( 
                            input_dtk_rec, 0, 
                            dtk_mpsg_strttime, dtk_mpsg_stoptime, 
                            dtk_mpsg_station_id ) ;

                        if( return_code < 0 )
                            return return_code ;

                        if( return_code == DTKM_DTK_HAS_TIME_IN_MASK )
                        {
                            /* 
                            -- OK.  dtk mask is found; update times and 
                            -- station id and continue.  
                            */
                            strcpy( CAST_DTK_STRTTIME 
                                input_dtk_rec[DTK_STRTTIME], 
                                dtk_mpsg_strttime ) ;
                            strcpy( CAST_DTK_STOPTIME 
                                input_dtk_rec[DTK_STOPTIME], 
                                dtk_mpsg_stoptime ) ;
                            strcpy( CAST_DTK_STATION_ID 
                                input_dtk_rec[DTK_STATION_ID], 
                                dtk_mpsg_station_id ) ;
                        }
                        else
                        {
                            /* 
                            -- NO TIME in any station mask.  
                            -- do not append this record.  
                            -- just skip this record and 
                            -- go to the next record.  
                            */
                            append_this_record_flag = 0 ;

                            /* no need to get default values */
                            fa_trigger_skip_default_values = 1 ;

                            fprintf(report_fp,
"SKIPPING record; data-take is within neither ASF nor MCM mask:  \n%s\n",
                                ascii_buffer ) ;
                        }
                    } /* end of [if (fa_trigger_MPSG_station_mask_check)]  */

                    /*
                    -- check the rec values, fill in with default values
                    */
                    if (!fa_trigger_skip_default_values)
                    {
                        return_code = dtkm_default_values(input_dtk_rec, 
                            input_dtk_rec);
                        if (return_code != DTKM_DEFAULT_VALUES_OK)
                        {
                            sprintf (file_util_msg, 
                            "ERROR detected by dtkm_default_values():%s\n",
                                DTKM_ERROR_MESSAGE(return_code) );
                            aps_log_msg(file_util_progname, APS_ERROR, 
                                file_util_msg, 
                                DO_SYSLOG, DO_PRINT);

                            fprintf(report_fp, 
                            "ERROR detected by dtkm_default_values():\n%s\n",
                                DTKM_ERROR_MESSAGE(return_code) );
                            dtkm_print(report_fp, input_dtk_rec ) ;

                            aps_log_msg(file_util_progname, APS_ERROR, 
                                "SKIPPING record due to error:\n", 
                                DO_SYSLOG, DO_PRINT);
                            aps_log_msg(file_util_progname, APS_ERROR, 
                                ascii_buffer, 
                                DO_SYSLOG, DO_PRINT);

                            fprintf(report_fp, 
                                "SKIPPING record due to error:  \n%s\n", 
                                ascii_buffer ) ;

                            /* error: */
                            number_of_errors_in_record++ ;

                            /* don't APPEND.  */
                            append_this_record_flag = 0 ;
                        }
                    }


                    if ( append_this_record_flag )
                    {
#ifdef PRINT_DIAG
                        printf ("------ APPENDING THIS RECORD -------- \n") ;
                        /* -- print the DBRECORD: */
                        db_print_record( input_dtk_rec, APS_CDEFS(DTK) );
#endif
                        /*
                        -- append the rec to the list: 
                        */
                        APPEND(input_dtk_list, input_dtk_rec, free_db_record, 
                            input_dtk_rec) ;
                        /*
                        -- Now we must Allocate a new empty    DBRECORD template
                        */
                        input_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

                    }
                } /* END don't skip record.  */
#ifdef PRINT_DIAG                                        
                else
                {
                    printf ("------------------------- SKIPPING RECORD \n");
                }
#endif
            }  /* END of read the record.  */

        } /* END (else) if no errors in record.  */

        /* 
        -- we have processed a data record, decrement counter
        -- (no effect on READ_TO_EOF condition)
        */
        data_records2process -- ;
        if ( number_of_errors_in_record > 0 )
            fa_number_of_error_records++ ;

    } /*   END while ( data_records2process )     */
    free (ascii_buffer) ;

    /* 
    -- the active DBRECORD template is 
    -- no longer needed.  
    */
    free_db_record( input_dtk_rec ) ;
    return (FA_ASCII_RECORD_INGESTION_OK);

}/* end of fa_ascii_record_ingestion() */


/*==============================================================================
Function:       dtk_status_by_blanking

Description:    given a list of DB_RECORD, update the status (DTK_DTKSTAT) of 
                each item if certain data fields are blank
                also the notes field (DTK_NOTES)

Parameters:     

Returns:        
Type        Definition
int         >=0 Number of dtk records whose status was adjusted. Success.

Creator:        Miguel Siu

Creation Date:  Fri Oct 27 17:12:41 PDT 1995

Notes:      
==============================================================================*/
static int     dtk_status_by_blanking (llist *dtk_list)
{
    DB_RECORD   **dtk_rec ;
    cursor      dtk_rec_ptr ;
    char        blanks[]="                                                    ";
    int         records_adjusted = 0 ;


/*
-- If the status DTKSTAT reflects an error condition, exit without changes.
-- If the status DTKSTAT is REP (a pre-PLN state)
-- there is a chance that it needs to be updated, so keep on going.
*/
    for (
        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_rec_ptr)
        )
    {
        if (strncmp("REP", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],
                strlen(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT]) ) != 0)
        {
            continue ;
        }

        if (strncmp(blanks, CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
                 strlen(CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME]) ) == 0
        ||  strncmp(blanks, CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
                 strlen(CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME]) ) == 0
        ||  strncmp(blanks, CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID],
                 strlen(CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID]) ) == 0
        ||  CAST_DTK_REV dtk_rec[DTK_REV] == 0 )
        {
            strcpy( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ" ) ;
            strcpy( CAST_DTK_NOTES dtk_rec[DTK_NOTES], 
                "Fields blanked in FA file indicating REJ" ) ;
            records_adjusted ++ ;
        }
    }

    return (records_adjusted) ;
}


/*==============================================================================
Function:       dtk_fillin_FA_response

Description:    given a list of DB_RECORD, fill in any missing fields in each
                item with data culled from the database.  Lookup of database
                information is done via the DTK_FADTKID

                given a list of DB_RECORDs from a reply file from a 
                flight agency, for each record:
                1.  retrieve the values for the existing db record into 
                    lookup_rec.  
                2.  copy the new values from the FA file (dtk_rec) into 
                    lookup_rec.  
                3.  APPEND lookup_rec to updated_dtks_list.  
                4.  substitute the original_list pointer value to point 
                    to updated_dtks_list and return.  

                Thus, the output original_list will point to the new list, 
                which has the OLD values from the database as well as 
                all of the NEW values from the FA file 

Returns:        
Type        Definition
int         >=0 Number of dtk records whose status was adjusted. Success.
            -1  An existing dtk record for the update was not found. ERROR.

Creator:        Miguel Siu

Creation Date:  Sat Oct 28 09:25:40 PDT 1995

Notes:      If a datatake can not be found in the database to match our dtk
            proposal, this routine will move forward using the information
            available in the dtk proposal.
            The dtk proposal will in fact, NOT have been 'filled' from the db.
==============================================================================*/
static int
dtk_fillin_FA_response (
            DBPROCESS   *APS_dbproc,  /* open Sybase database process */
            llist       **original_list)
{
    llist       *updated_dtks_list  ;
    llist       *lookup_dtks_list  ;
    llist       *dtks_list  ;
    DB_RECORD   **dtk_rec ;
    cursor      dtk_rec_ptr ;

    DB_RECORD   **lookup_rec ;
    cursor      lookup_rec_ptr ;

    int         records_adjusted = 0 ;

    updated_dtks_list = create_dyn_llist();
    lookup_dtks_list  = create_dyn_llist();

    /*
    -- In this routine, original_list is read from, 
    -- then updated_dtks_list is created.  
    -- Then original_list is destroyed, and then 
    -- *original_list, when returning to the called 
    -- routine, receives the value of the 
    -- updated_dtks_list.  
    */
    dtks_list         = *original_list ;


    /*
    -- Go through each record in list, pull its database counterpart 
    -- and use these fields from REJ, ERR, PLN, REP FA datatakes: 
    --      DTK_DTKSTAT DTK_NOTES (if not null, it contains an explanation)
    --
    -- The following fields should additionally be updated from PLN, REP 
    -- FA file datatakes ONLY:
    --      DTK_STRTTIME    DTK_STOPTIME    DTK_STATION_ID
    --      DTK_STRTLAT     DTK_STOPLAT     DTK_NRLAT1      DTK_NRLON1
    --      DTK_NRLAT2      DTK_NRLON2      DTK_FARLAT2     DTK_FARLON2
    */

    for (
        dtk_rec = (DB_RECORD **) FIRST(dtks_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT(dtks_list, dtk_rec_ptr)
        )
    {
        /*
        -- the where clause will get the record in the 
        -- db with the same sat/sensor/rev/dtkid
        -- regardless of status.  
        */
        sprintf(where_clause,
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DTK,DTK_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(DTK,DTK_SENSOR), CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            APS_COL(DTK,DTK_REV), CAST_DTK_REV dtk_rec[DTK_REV], 
            APS_COL(DTK,DTK_DTKID), CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;

#ifdef PRINT_DIAG
        printf("%s(%d):  dtk_fillin_FA_response():  where_clause = \n%s\n", 
            __FILE__, __LINE__, where_clause ) ;
        dtkm_print( stdout, dtk_rec ) ;
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
            printf("%s(%d):  dtk_fillin_FA_response():  lookup_rec = \n", 
                __FILE__, __LINE__ ) ;
            dtkm_print( stdout, lookup_rec ) ;
#endif
            /* 
            -- update the fields in the corresponding db record 
            */
            strcpy(CAST_DTK_DTKSTAT lookup_rec[DTK_DTKSTAT],
                    CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT]) ;
            if ( strlen(CAST_DTK_NOTES dtk_rec[DTK_NOTES]) != 0 )
            {
                strcpy( CAST_DTK_NOTES lookup_rec[DTK_NOTES],
                        CAST_DTK_NOTES    dtk_rec[DTK_NOTES] ) ;
            }

            /*
            -- if status of the FA file record is PLN or REP, copy 
            -- fields from the FA file record into the lookup_rec.  
            --
            -- if status of the FA file record is neither PLN nor REP, the 
            -- FA rec has no other values.  it was not accepted 
            -- by the FA.  
            */
            if (strncmp("PLN", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],
                        strlen(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT]) ) == 0
            ||  strncmp("REP", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],
                        strlen(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT]) ) == 0)
            {
                /* 
                -- update the lookup_rec with values from 
                -- the FA file record.  
                */
                strcpy(CAST_DTK_STRTTIME lookup_rec[DTK_STRTTIME],
                        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME]) ;
                strcpy(CAST_DTK_STOPTIME lookup_rec[DTK_STOPTIME],
                        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME]) ;
                strcpy(CAST_DTK_STATION_ID lookup_rec[DTK_STATION_ID],
                        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID]) ;

                CAST_DTK_STRTLAT lookup_rec[DTK_STRTLAT] = 
                        CAST_DTK_STRTLAT dtk_rec[DTK_STRTLAT]  ;
                CAST_DTK_STOPLAT lookup_rec[DTK_STOPLAT] =
                        CAST_DTK_STOPLAT dtk_rec[DTK_STOPLAT]  ;

                CAST_DTK_NRLAT1 lookup_rec[DTK_NRLAT1] =
                        CAST_DTK_NRLAT1 dtk_rec[DTK_NRLAT1]  ;
                CAST_DTK_NRLON1 lookup_rec[DTK_NRLON1] =
                        CAST_DTK_NRLON1 dtk_rec[DTK_NRLON1]  ;
                CAST_DTK_FARLAT1 lookup_rec[DTK_FARLAT1] =
                        CAST_DTK_FARLAT1 dtk_rec[DTK_FARLAT1]  ;
                CAST_DTK_FARLON1 lookup_rec[DTK_FARLON1] =
                        CAST_DTK_FARLON1 dtk_rec[DTK_FARLON1]  ;

                CAST_DTK_NRLAT2 lookup_rec[DTK_NRLAT2] =
                        CAST_DTK_NRLAT2 dtk_rec[DTK_NRLAT2]  ;
                CAST_DTK_NRLON2 lookup_rec[DTK_NRLON2] =
                        CAST_DTK_NRLON2 dtk_rec[DTK_NRLON2]  ;
                CAST_DTK_FARLAT2 lookup_rec[DTK_FARLAT2] =
                        CAST_DTK_FARLAT2 dtk_rec[DTK_FARLAT2]  ;
                CAST_DTK_FARLON2 lookup_rec[DTK_FARLON2] =
                        CAST_DTK_FARLON2 dtk_rec[DTK_FARLON2]  ;
            }

            /* 
            -- insert the updated db record into list of updated dtks 
            */
            dtkm_duplicate_dtk_into_list( lookup_rec, updated_dtks_list ) ;
        }
        else
        {
            /* 
            -- We have found a dtk proposal with no matching db record!!! 
            -- This is unacceptable for a Flight Agency reply.  All reply
            -- dtks must have a matching dtk record in the database.
            --
            -- Set the records_adjusted to error condition and return.
            */
            aps_log_msg(file_util_progname, APS_ERROR, 
                "Data-take proposal has no matching db record:\n",
                DO_SYSLOG, DO_PRINT);
            db_print_record( dtk_rec, APS_CDEFS(DTK) ) ;
            DEL_LIST ( lookup_dtks_list ) ;
            DEL_LIST ( updated_dtks_list ) ;
            return (-1) ;
        }

        records_adjusted ++ ;
 
    }

    /*
    -- now, subtitute our updated list for the incoming list
    */
    DEL_LIST ( lookup_dtks_list ) ;

    /*
    -- NOTE:  dtks_list = *original_list ;
    */
    DEL_LIST ( dtks_list ) ;

#ifdef PRINT_DIAG
    dtkm_print_list (stdout, updated_dtks_list);
#endif

    *original_list = updated_dtks_list ;

    return (records_adjusted) ;
}


/*==============================================================================
Function:       dtk_create_esa_observation_dtks

Description:    given a list of DB_RECORD, look up any db observation dtk which
                are linked to each DB_RECORD.  If there are no observations
                currently in the database, we create an observation record
                for the DB_RECORD.

Returns:        
Type        Definition
int         >=0     Number of dtk records added. Success.
            < 0     ERROR.

Creator:        Miguel Siu

Creation Date:  Thu Mar 27 10:06:31 PST 1997

Notes:     
    We assume that the incoming list has been reduced to one downlink per
    rev.  If not, there is a chance that there may be multiple downlinks
    resulting in multiple observations created.  Not a fatal condition,
    but it could be annoying.
==============================================================================*/
static int
dtk_create_esa_observation_dtks ( 
    llist       *downlinks_list,
    llist       *new_obs_list)
{
    int         num_obs_found ;
    int         num_obs_created = 0 ;
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **new_obs_rec ;
    cursor      dtk_rec_ptr ;

    for (
        dtk_rec = (DB_RECORD **) FIRST(downlinks_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT(downlinks_list, dtk_rec_ptr)
        )
    {
        num_obs_found = dtkm_count_obs_dtks4SHAQ (dtk_rec);
        if (num_obs_found > 0)
            continue ;              /* observations exist, no action needed */
        if (num_obs_found < 0)
            return num_obs_found ;  /* ERROR encountered, return ERROR code */

        new_obs_rec =  new_table_record(APS_CDEFS(DTK)) ;
        if (db_copy_record(APS_CDEFS(DTK),
                new_obs_rec, dtk_rec) != DB_COPY_RECORD_OK)
            return (DTKM_ERROR_DUPLICATING_DTK_RECORD) ;
        /*
        -- now, update some fields in order to create our new observation dtk
        -- We will be using default values of sensor=SAR
        -- (We can do this because we are dealing with ESA observations)
        */
        strcpy(CAST_DTK_SENSOR new_obs_rec[DTK_SENSOR], "SAR") ;
        strncpy(CAST_DTK_ACTID  new_obs_rec[DTK_ACTID], 
                        DTKM_ACTID_REALTIME_OBSERVATION_CODE, 
                        sizeof(DTKM_ACTID_REALTIME_OBSERVATION_CODE)-1 ) ;
        APPEND(new_obs_list, new_obs_rec, free_db_record, new_obs_rec) ;
        num_obs_created++ ;
    }


    return num_obs_created ;
}

/*==============================================================================
Function:       dtk_delete_range

Description:    given a list of DB_RECORD, delete the dtks which overlap
                the range described by the earliest starttime and the 
                latest stoptime on the list of DB_RECORD

Parameters:     

Returns:        
Type        Definition
int         >0 Number of dtk records deleted. Success.
            -1 No dtk records were deleted. Error.

Creator:        Miguel Siu

Creation Date:  Mon Oct 23 16:41:26 PDT 1995

Notes:      
==============================================================================*/
static int 
dtk_delete_range(DBPROCESS  *APS_dbproc,  /* open Sybase database process */
                 llist      *dtk_list)
{
    DB_RECORD   **master_dtk ;
    DB_RECORD   **browse_dtk ;
    cursor      dtk_ptr ;

    /* 
    -- must make new storage to use.  do not use 
    -- any data-takes that are in the list.  
    -- allocate the master data-take here for use 
    -- in this routine:  
    */
    browse_dtk = (DB_RECORD **) FIRST(dtk_list,dtk_ptr) ;
    if ( browse_dtk == NULL )
        return 0 ;
    master_dtk = new_table_record(APS_CDEFS(DTK)) ;
    db_copy_record( APS_CDEFS(DTK), master_dtk, browse_dtk ) ;

    browse_dtk = (DB_RECORD **) NEXT(dtk_list, dtk_ptr) ; 

    if ( master_dtk != NULL 
    &&   browse_dtk != NULL)
    do
    {
        /* check for new earliest strttime to build delete range */

        if (strcmp(CAST_DTK_STRTTIME master_dtk[DTK_STRTTIME],
                        CAST_DTK_STRTTIME browse_dtk[DTK_STRTTIME]) > 0 )
        {

            /* 
            -- NEW earliest strttime. Copy JUST the strttime data to master_dtk
            */
#ifdef PRINT_DIAG
            printf("comparing %s < %s \n",
                CAST_DTK_STRTTIME browse_dtk[DTK_STRTTIME],
                CAST_DTK_STRTTIME master_dtk[DTK_STRTTIME]) ;
#endif
            strcpy(CAST_DTK_STRTTIME master_dtk[DTK_STRTTIME],
            CAST_DTK_STRTTIME browse_dtk[DTK_STRTTIME]) ;

        }

        /* check for new latest stoptime to build delete range*/

        if ( strcmp(CAST_DTK_STOPTIME master_dtk[DTK_STOPTIME],
                        CAST_DTK_STOPTIME browse_dtk[DTK_STOPTIME]) < 0 )
        {

            /* 
            -- NEW latest stoptime. Copy JUST the stoptime data to master_dtk
            */
#ifdef PRINT_DIAG
            printf("comparing %s > %s \n",
                CAST_DTK_STOPTIME browse_dtk[DTK_STOPTIME],
                CAST_DTK_STOPTIME master_dtk[DTK_STOPTIME]) ;
#endif
            strcpy(CAST_DTK_STOPTIME master_dtk[DTK_STOPTIME],
            CAST_DTK_STOPTIME browse_dtk[DTK_STOPTIME]) ;

        }

    } while( (browse_dtk = (DB_RECORD **) NEXT(dtk_list, dtk_ptr))
             != NULL ) ;

#ifdef PRINT_DIAG
            printf("FINAL %s to %s \n",
                CAST_DTK_STRTTIME master_dtk[DTK_STRTTIME],
                CAST_DTK_STOPTIME master_dtk[DTK_STOPTIME]) ;
#endif

    /*
    -- The following where clause IS correct, even though it may look funny.
    -- It will correctly identify any datatakes that OVERLAP our time range.
    */
    sprintf(where_clause,
        "where %s = '%s' and %s < '%s' and %s > '%s'",
        APS_COL(DTK, DTK_SAT),      CAST_DTK_SAT      master_dtk[DTK_SAT],
        APS_COL(DTK, DTK_STRTTIME), CAST_DTK_STOPTIME master_dtk[DTK_STOPTIME],
        APS_COL(DTK, DTK_STRTTIME), CAST_DTK_STRTTIME master_dtk[DTK_STRTTIME]);
    free_db_record( master_dtk ) ;

    return (db_delete_records(APS_dbproc, APS_TABLE(DTK), where_clause) ) ;

}

/*==============================================================================
Function:       fa_ascii_processor

Description:    Accepts an ASCII File, and scans it for information  
                which is placed on a DBRECORD linked list.  
                This list is then processed by appropriate dtk routines and 
                its information incorporated into the DTK database table.

Parameters:    
Type     Name        Definition
*char    filename    Name of incoming ASCII file to be read.

Returns:        
        FA_ASCII_REC_PROCESS_OK             0
        FA_ASCII_REC_PROCESS_OK_CREATE_META 1
        FA_ASCII_REC_PROC_DONE_CREATE_META  2
        FA_ASCII_REC_PROC_PERMISSION_DENIED 3
        FA_ASCII_REC_PROCESS_ERROR          4


Creator:        Miguel Siu

Creation Date:  Fri May 19 09:46:50 PDT 1995

Notes:      FA_ASCII_REC_PROCESS_OK follow convention  0 = no errors.
                                                      !0 = error.
==============================================================================*/
int     /* Ignore LINT warning: declared global, could be static */
fa_ascii_processor(
    DBPROCESS   *APS_dbproc,        /* open Sybase database process         */
    PERMISSION_ permission,         /* permission id structure              */
    FA_FILENAME *file_descriptor,
    char        *file_name,
    FILE        *ascii_file,        /* input ASCII file pointer             */
    FILE        *report_fp,         /* report file pointer                  */
    FILE        *response_file)     /* response file pointer                */
{                                   

    /* to handle the REQA GRS replies.  */
    FILE        *REQA_grs_fp = NULL ;  /* used for REQA to store GRS recs.  */
    char        *aps_temp_dir = NULL ;
    char        REQA_scratch_filename[200] ;
    char        command[300] ;

    int         i ;
    int         return_code ;
    int         dtkm_process_list_return_code ;
    int         planner_read_status = 0 ;
    int         rejection_rev ;
    int         level_indicator = 0 ;
    int         TERMINATE_ACTIVITY_PERMISSION = 0 ;
    int         number_of_file_dtks = 0 ;
    int         number_of_new_dtks  = 0 ;


    double      ndays ;

    llist       *perm_list = NULL ;
    llist       *CON_dtks = NULL ;
    llist       *deleted_dtks = NULL ;
    llist       *other_sat_dtks = NULL ;
    llist       *same_sat_dtks = NULL ;
    llist       *dtk_updates = NULL ;

    llist       *additional_downlinks ;
    llist       *accepted_dtks  ;
    llist       *rejected_dtks  ;
    llist       *error_dtks     ;
    llist       *omission_dtks  ;
    llist       *additional_reject_dtks ;
    llist       *rejsort_dtks ;

    llist       *input_dtk_list ;
    ord_llist   *ord_rejected_dtks ;

    DB_RECORD   **dtk_rec ;
    DB_RECORD   **dtk_rec_inquestion ;
    DB_RECORD   **template_dtk_rec;
    DB_RECORD   **dtk_unlinked_rec = NULL ;
    DB_RECORD   **min_dtk_rec ;
    DB_RECORD   **max_dtk_rec ;


    cursor        dtk_rec_ptr ;
    cursor        dtk_rec_inquestion_ptr ;


#ifdef PRINT_DIAG                                        
        printf ("IN the ascii_processor              \n");
#endif


    /*
    -- Make an empty    DBRECORD template
    */
    template_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

    /* 
    -- And fill the template with blank default values:  null strings,
    -- blank single-characters, and zero numeric values.  
    */
    dtkm_blank_values( template_dtk_rec, template_dtk_rec ) ;


    /* Ingest the header */

    return_code = fa_ascii_header_ingestion (
                        ascii_file,
                        template_dtk_rec, 
                        file_descriptor->file_definition, 
                        report_fp ) ;
    if ( return_code != FA_ASCII_HEADER_INGESTION_OK)
    {
        sprintf (file_util_msg, 
            "Error encountered in fa_ascii_processor():%s\n",
            FA_ASCII_REC_ERROR_MESSAGE(return_code) );
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf(report_fp, "Error encountered in fa_ascii_processor():\n%s\n",
            FA_ASCII_REC_ERROR_MESSAGE(return_code) );
        free_db_record (template_dtk_rec) ;
        return(FA_ASCII_REC_PROCESS_ERROR);
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
       6) Terminate planning permission.
       7) If item(1) is true, terminate activity permission.

    */
    perm_list = create_dyn_llist() ;

    /*
    -- Item (1). Get ACTIVITY permission if it is not provided.
    */
    if (permission.id == 0)
    {
        /*
        -- we were not passed an activity permission; request one
        */
        for (i=0; i<(1+permission.number_o_retry); i++)
        {
            return_code = mu_permission_request( APS_dbproc, permission.id,
                fa_activity_type,           /*  mu_activity_id               */
                MU_SINGLE_ACTIVITY_TYPE,    /*  activity_type                */
                perm_list,                  /*  blocking_permission_list     */
                NULL,                       /*  strttime                     */
                NULL,                       /*  stoptime                     */
                NULL,                       /*  station_id                   */
                0 ) ;                       /*  darid                        */
            if( return_code < 0 )
            {
                fprintf(stderr,"%s(%d):  request single error code %d\n",
                    __FILE__, __LINE__, return_code ) ;
                fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
                return (FA_ASCII_REC_PROC_PERMISSION_DENIED) ;
            }
            else if (return_code == 0 )
            {
                if ( i == permission.number_o_retry )
                {
                    fprintf(stderr,
                    "%s(%d):  request for single activity permission DENIED \n",
                        __FILE__, __LINE__ ) ;
                    fprintf(stderr,"%s(%d):  Blocking single activities:\n", 
                        __FILE__, __LINE__ ) ;
                    db_fprint_list(stderr,
                        perm_list, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES) ) ;
                    return (FA_ASCII_REC_PROC_PERMISSION_DENIED) ;
                }
                else
                {
                    fprintf(stderr,
                    "%s(%d):  request for single activity permission DENIED \n",
                        __FILE__, __LINE__ ) ;
                    fprintf(stderr,"\tWAITING to re-acquire permission.\n") ;
                    DEL_ALL(perm_list) ; /* get ready for next try */
                    sleep (permission.seconds_between) ;
                    continue ;
                }
            }
            else
            {
                /* 
                -- we have gotten our planning permission.
                */
                permission.id = return_code ;
                TERMINATE_ACTIVITY_PERMISSION = 1 ;
                break ;
            }

        } /* end of 'retry' loop */
    }
    else
    {
        /*
        -- Item (2). Validate activity permission.
        */
        return_code = mu_permission_validate(
            permission.id,
            fa_activity_type,               /*  activity_id    */
            MU_SINGLE_ACTIVITY_TYPE ) ;     /*  activity_type  */
        if( return_code != permission.id )
        {
            fprintf(stderr,"%s(%d):  permission validate error code %d\n",
                __FILE__, __LINE__, return_code ) ;
            fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            return (FA_ASCII_REC_PROC_PERMISSION_DENIED) ;
        }
    }



    /*
    -- Make an empty DTK list
    */
    input_dtk_list = create_dyn_llist() ;
    if(input_dtk_list == NULL)
    {
        aps_log_msg(file_util_progname, APS_ERROR,
"Error encountered in fa_ascii_processor(): COULDNOT CREATE DYN LIST\n",
            DO_SYSLOG, DO_PRINT);
        fprintf(report_fp,
"Error encountered in fa_ascii_processor(): COULDNOT CREATE DYN LIST\n");
        free_db_record (template_dtk_rec) ;
        return(FA_ASCII_REC_PROCESS_ERROR);
    }


    /* 
    -- setup the global veriable for error reporting. 
    */
    fa_number_of_error_records = 0 ;

    /* Ingest the data records */

    /*
    -- Processing flags defaulted "READ", then assume value 
    -- of highest-level records (acquisition records) calling this routine.
    -- (process/scan the file's data records; place data-takes in linked 
    -- list "input_dtk_list" for later processing.)
    */
    if( strcmp( file_descriptor->file_type, "REQA" ) == 0  )
    {
        /*
        -- Now processing a REQA file.  
        -- must open a scratch file to hold GRS 
        -- replies for separate processing.  
        */
        /* 
        -- unique file name for scratch file in $APS_DATA/tmp area; 
        -- embed the process id into the name, for uniqueness:  
        */
        aps_temp_dir = aps_fullpath(APS_TEMP, NULL ) ;
        sprintf(REQA_scratch_filename, "%s/REQA_GRS_replies.%d.text", 
            aps_temp_dir, getpid() ) ;
        free( aps_temp_dir ) ;
        REQA_grs_fp = fopen( REQA_scratch_filename, "w" ) ;
    }
    return_code = fa_ascii_record_ingestion 
                    (ascii_file,
                    level_indicator,
                    APS_dbproc, 
                    template_dtk_rec,
                    input_dtk_list,
                    file_descriptor->file_definition,
                    fa_number_of_records,
                    REQA_grs_fp,
                    report_fp ) ;

    if ( return_code != FA_ASCII_RECORD_INGESTION_OK)
    {
        sprintf (file_util_msg, 
            "Error encountered in fa_ascii_processor():%s\n",
            FA_ASCII_REC_ERROR_MESSAGE(return_code) );
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf(report_fp, "Error encountered in fa_ascii_processor():\n%s\n",
            FA_ASCII_REC_ERROR_MESSAGE(return_code) );
        return(FA_ASCII_REC_PROCESS_ERROR);
    }
    if (fa_trigger_reqm_secondary_record_ingestion)
    {
        return_code = fa_ascii_record_ingestion 
                        (ascii_file,
                        level_indicator,
                        APS_dbproc, 
                        template_dtk_rec,
                        input_dtk_list,
                        &NASDA_filedef_secondary_reqm,
                        fa_number_of_secondary_recs,
                        REQA_grs_fp,
                        report_fp );

        if ( return_code != FA_ASCII_RECORD_INGESTION_OK)
        {
            sprintf (file_util_msg, 
                "Error encountered in secondary fa_ascii_processor():%s\n",
                FA_ASCII_REC_ERROR_MESSAGE(return_code) );
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
                "Error encountered in secondary fa_ascii_processor():\n%s\n",
                FA_ASCII_REC_ERROR_MESSAGE(return_code) );
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

        /* 
        -- NOTE:  space for this global variable array was allocated in the 
        -- routine NASDAc_populate_sch_link
        */
        free (fa_sch_link_control) ; 
    }

    free_db_record( template_dtk_rec ) ;
/*============================================================================
         #####   #####   ######           ####   #####    ####
         #    #  #    #  #               #    #  #    #  #
         #    #  #    #  #####           #    #  #    #   ####
         #####   #####   #               #    #  #####        #
         #       #   #   #               #    #  #       #    #
         #       #    #  ###### #######   ####   #        ####
  ============================================================================*/
    /*
    -- Carry out any db operations needed before DTK processing.
    -- There may be multiple operations carried out in sequence.
    */
    if (fa_trigger_dtk_delete_range) 
    {
        if (dtk_delete_range (APS_dbproc, input_dtk_list) < 0)
        {
            aps_log_msg(file_util_progname, APS_ERROR,
"Error encountered in fa_ascii_processor():\
COULD NOT DELETE PLANNED DTK BEFORE INSERTING SCHEDULED DTK\n", 
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
"Error encountered in fa_ascii_processor():\n\
COULD NOT DELETE PLANNED DTK BEFORE INSERTING SCHEDULED DTK\n") ;
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

#ifdef PRINT_DIAG                                        
        printf(" about to call dtk_delete_range\n\n");
        printf("deleted %d datatakes\n\n",
            dtk_delete_range (APS_dbproc, input_dtk_list) );
#endif
    }


    if (fa_trigger_dtk_status_by_blanking)
    {
        return_code = dtk_status_by_blanking (input_dtk_list) ;

#ifdef PRINT_DIAG
        printf(" called dtk_status_by_blanking\n\n");
        printf("adjusted status on %d datatakes\n\n",
            return_code );
#endif
    }


    if (fa_trigger_dtk_fillin_FA_response)
    {
        if (dtk_fillin_FA_response(APS_dbproc, &input_dtk_list) < 0 )
        {
            aps_log_msg(file_util_progname, APS_ERROR,
"Error encountered in fa_ascii_processor():\
FLIGHT AGENCY REPLY DTK DOES NOT HAVE A MATCHING ENTRY IN DB\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
"\nError encountered in fa_ascii_processor():\n\
FLIGHT AGENCY REPLY DTK DOES NOT HAVE A MATCHING ENTRY IN DB\n");
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

#ifdef PRINT_DIAG
        printf(" about to call dtk_fillin_FA_response with %x\n\n",
            input_dtk_list);
        printf("filled in %d datatakes\n\n",
        printf("DONE. here is the list to process with %x:\n",
            input_dtk_list);
        dtkm_print_list (stdout,input_dtk_list);
#endif
    }


    if (fa_trigger_SHAQP_dtk_consolidation)
    {
#ifdef PRINT_DIAG
        printf(" calling SHAQP_dtk_consolidation input_dtk_list:\n\n") ;
        dtkm_print_list (stdout,input_dtk_list);
#endif
        if (dtkm_dtk_consolidation_by_rev (input_dtk_list) < 0 )
        {
            aps_log_msg(file_util_progname, APS_ERROR,
"Error encountered in fa_ascii_processor():\
CONSOLIDATION OF DTKS FOR SHAQP DID NOT COMPLETE.\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
"\nError encountered in fa_ascii_processor():\n\
CONSOLIDATION OF DTKS FOR SHAQP DID NOT COMPLETE.\n");
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

    }/* end of fa_trigger_SHAQP_dtk_consolidation */



    /* 
    -- Create a report header
    */
    if ( !aps_report_header (report_fp, 
                            file_descriptor->flight_agency, 
                            file_descriptor->file_type, 
                            file_name, fa_creation_date))
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
"Error encountered in fa_ascii_processor(): COULDNOT CREATE DTK RPT HEADR\n",
            DO_SYSLOG, DO_PRINT);

        fprintf(report_fp,
"Error encountered in fa_ascii_processor():\nCOULDNOT CREATE DTK RPT HEADR\n");
        return(FA_ASCII_REC_PROCESS_ERROR);
    }

    if( strcmp( file_descriptor->file_type, "REQA" ) == 0 )
    {
        /*
        -- Finished processing the REQA RSP replies.   
        -- Now processing the GRS replies, if any.  
        -- must sort the scratch file which holds GRS replies:  
        -- save the sorted file, appending '.sorted' to file 
        -- name, remove the original file.  
        -- when done remove the sorted file, too.  
        */
        fclose(REQA_grs_fp) ;
        sprintf(command, "sort %s > %s.sorted ", 
            REQA_scratch_filename, 
            REQA_scratch_filename ) ;
        (void) system(command) ;

        /* clean up:  */
        (void) unlink( REQA_scratch_filename ) ;

        strcat( REQA_scratch_filename, ".sorted" ) ;
        REQA_grs_fp = fopen(REQA_scratch_filename, "r" ) ;

        /*
        -- the extracted GRS replies are 
        -- now sorted and ready to read.  
        */
        return_code = reqa_grs_processor( REQA_grs_fp, report_fp ) ;
        if ( return_code != TRUE)
        {
            /* 
            -- do not clean up the sorted file in the error case, 
            -- might want to take a look.  
            */
            sprintf (file_util_msg, 
                "%s(%d):  Error encountered in processing REQA GRS replies.\n",
                __FILE__, __LINE__ ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            fprintf(report_fp, file_util_msg ) ;
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

        /* clean up.  */
        (void) unlink( REQA_scratch_filename ) ;
    }

    if( NUMELTS(input_dtk_list) <= 0 )
    {
        /*****************************************************************
        *                                                                *
        *        TERMINATE FILE PROCESSING HERE                          *
        *                                                                *
        ******************************************************************/
        sprintf (file_util_msg, 
            "SUCCESS.  There were no data-take type records in the file.  \n") ;
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

        fprintf( report_fp, "%s\n", file_util_msg ) ;

        if (fa_trigger_always_create_response_file)
            return FA_ASCII_REC_PROCESS_OK_CREATE_META ;
        else
            return FA_ASCII_REC_PROCESS_OK ;

    }

    if (fa_trigger_shaq_observation_generation)
    {
        /*
        -- This action checks for existing observation datatake(s) which
        -- link to the current downlink datatakes in our list.
        -- If there are no existing observation datatake(s) we create 
        -- one at this point.
        */
        additional_downlinks = create_dyn_llist();

        return_code = dtk_create_esa_observation_dtks (
                            input_dtk_list, additional_downlinks) ;
        if (return_code < 0)
        {
            aps_log_msg(file_util_progname, APS_ERROR,
"Error encountered in fa_ascii_processor():\
CREATION OF OBSERVATION DTKS FOR SHAQ DID NOT COMPLETE.\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
"\nError encountered in fa_ascii_processor():\n\
CREATION OF OBSERVATION DTKS FOR SHAQ DID NOT COMPLETE.\n");
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

        fprintf(report_fp, 
            "DATATAKE PROPOSALS OBTAINED FROM   INPUT FA FILE:\n") ;
        dtkm_print_list (report_fp, input_dtk_list);
        fprintf(report_fp,
            "\n DTK  OBSERVATIONS CREATED TO COMPLEMENT FA FILE:\n") ;
        dtkm_print_list (report_fp, additional_downlinks);

        number_of_file_dtks = NUMELTS (input_dtk_list) ;
        number_of_new_dtks  = NUMELTS (additional_downlinks) ;

        if ( db_record_llist_copy(
                APS_CDEFS(DTK), input_dtk_list, additional_downlinks) == NULL)
        {
            aps_log_msg(file_util_progname, APS_ERROR,
                "Error encountered in fa_ascii_processor():\
ADDITION OF OBSERVATION DTKS DID NOT COMPLETE.\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
                "\nError encountered in fa_ascii_processor():\n\
ADDITION OF OBSERVATION DTKS DID NOT COMPLETE.\n");
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

    }

    if (fa_trigger_dtk_create_FA_downlinks)
    {
        additional_downlinks = create_dyn_llist();

        if( NUMELTS(input_dtk_list) > 0 )
        {
            return_code = dtkm_add_rdl2obs (input_dtk_list, 
                additional_downlinks) ;
            if (return_code < 0)
            {
                aps_log_msg(file_util_progname, APS_ERROR,
                    DTKM_ERROR_MESSAGE(return_code),
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_fp, 
                    "\nError encountered in fa_ascii_processor():\n %s\n",
                    DTKM_ERROR_MESSAGE(return_code) );
                DEL_LIST( additional_downlinks ) ;
                return(FA_ASCII_REC_PROCESS_ERROR);
            }
        }

        fprintf(report_fp,
            "DATATAKE PROPOSALS OBTAINED FROM   INPUT FA FILE: %d\n",
            NUMELTS(input_dtk_list) ) ;
        dtkm_print_list (report_fp, input_dtk_list);
        fprintf(report_fp,
            "\nDATATAKE DOWNLINKS CREATED TO COMPLEMENT FA FILE: %d\n",
            NUMELTS(additional_downlinks) ) ;
        dtkm_print_list (report_fp, additional_downlinks);

        number_of_file_dtks = NUMELTS (input_dtk_list) ;
        number_of_new_dtks  = NUMELTS (additional_downlinks) ;

#ifdef PRINT_DIAG
        printf(" calling dtkm_add_rdl2obs input_dtk_list:\n\n") ;
        dtkm_print_list (stdout,input_dtk_list);
        printf(" calling dtkm_add_rdl2obs additional_dtk_list:\n\n") ;
        dtkm_print_list (stdout,additional_downlinks);
#endif

        if ( db_record_llist_copy(
                APS_CDEFS(DTK), input_dtk_list, additional_downlinks) == NULL)
        {
            aps_log_msg(file_util_progname, APS_ERROR,
                "Error encountered in fa_ascii_processor():\
ADDITION OF DOWNLINK DTKS DID NOT COMPLETE.\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, 
                "\nError encountered in fa_ascii_processor():\n\
ADDITION OF DOWNLINK DTKS DID NOT COMPLETE.\n");
            return(FA_ASCII_REC_PROCESS_ERROR);
        }

    }/* end of fa_trigger_dtk_create_FA_downlinks */

/*============================================================================*/

#ifdef PRINT_DIAG                                        
    printf(
    "\n\n\n\n\n about to call dtkm_process_dtk_proposal_list()\n\n\n\n\n");
#endif


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
            return (FA_ASCII_REC_PROCESS_ERROR) ;
    }

    strcpy(fa_file_start_time, CAST_DTK_STRTTIME min_dtk_rec[DTK_STRTTIME]);
    strcpy(fa_file_stop_time,  CAST_DTK_STOPTIME max_dtk_rec[DTK_STOPTIME]);

    /*
    -- We do not need to do the padding of the time range ourselves,
    -- the mu_permission library does it for us....
    */
 
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
       6) Terminate planning permission.
       7) If item(1) is true, terminate activity permission.
      
    */

    /*
    -- Item (4). Get PLANNING permission.
    */
    for (i=0; i<(1+permission.number_o_retry); i++)
    {
        return_code = mu_permission_request( APS_dbproc, permission.id,
            fa_activity_type,           /*  mu_activity_id               */
            MU_PLANNING_ACTIVITY_TYPE,  /*  activity_type                */
            perm_list,                  /*  blocking_permission_list     */
            fa_file_start_time,         /*  strttime                     */
            fa_file_stop_time,          /*  stoptime                     */
            fa_station_id,              /*  station_id                   */
            0 ) ;                       /*  darid                        */
        if( return_code < 0 )
        {
            fprintf(stderr,"%s(%d):  request single error code %d\n",
                __FILE__, __LINE__, return_code ) ;
            fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            return (FA_ASCII_REC_PROC_PERMISSION_DENIED) ;
        }
        else if (return_code == 0 )
        {
            if ( i == permission.number_o_retry )
            {
            fprintf(stderr,"%s(%d):  request for planning permission DENIED \n",
                __FILE__, __LINE__ ) ;
            fprintf(stderr,"%s(%d):  Blocking planning activities:\n", 
                __FILE__, __LINE__ ) ;
            db_fprint_list( stderr,
                perm_list, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
            return (FA_ASCII_REC_PROC_PERMISSION_DENIED) ;
            }
            else
            {
            fprintf(stderr,"%s(%d):  request for planning permission DENIED \n",
                __FILE__, __LINE__ ) ;
            fprintf(stderr,"\tWAITING to re-acquire permission.\n") ;
            DEL_ALL(perm_list) ; /* get ready for next try */
            sleep (permission.seconds_between) ;
            continue ;
            }
        }
        else
            /* 
            -- we have gotten our planning permission.
            */
            break ;

    } /* end of 'retry' loop */


/*============================================================================
      ##     ####    ####   ######   ####    ####           #####   #####
     #  #   #    #  #    #  #       #       #               #    #  #    #
    #    #  #       #       #####    ####    ####           #    #  #####
    ######  #       #       #            #       #          #    #  #    #
    #    #  #    #  #    #  #       #    #  #    #          #    #  #    #
    #    #   ####    ####   ######   ####    ####           #####   #####
 =============================================================================*/
    /* NOW, process the link list into database records */

    accepted_dtks = create_dyn_llist();
    rejected_dtks = create_dyn_llist();
    error_dtks    = create_dyn_llist();
    rejsort_dtks  = create_dyn_llist();
    additional_reject_dtks = create_dyn_llist();

    /*
    -- PROCESS THE DTK LIST
    */
    CON_dtks = create_dyn_llist() ;
    deleted_dtks = create_dyn_llist() ;
    omission_dtks = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks = create_dyn_llist() ;
    dtk_updates = create_dyn_llist() ;



    dtkm_process_list_return_code = 
        dtkm_process_dtk_proposal_list (APS_dbproc, input_dtk_list,
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks,
        error_dtks, omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates, 
        report_fp);



    aps_log_msg(file_util_progname, APS_INFO, "DTK STATISTICS\n",
        DO_SYSLOG, DO_PRINT);
    sprintf (file_util_msg, 
        "DL/DTKS processed         : %d", NUMELTS (input_dtk_list)) ;
    aps_log_msg(file_util_progname, APS_INFO, 
        file_util_msg, DO_SYSLOG, DO_PRINT);
    if (number_of_new_dtks > 0)
    {
        sprintf (file_util_msg, 
            "DL/DTKS read from FA file : %d", number_of_file_dtks) ;
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);
        sprintf (file_util_msg, 
            "DL/DTK created for FA file: %d\n", number_of_new_dtks) ;
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);
    }
    sprintf (file_util_msg, 
        "ACCEPTED                  : %d", NUMELTS (accepted_dtks)) ;
    aps_log_msg(file_util_progname, APS_INFO, 
        file_util_msg, DO_SYSLOG, DO_PRINT);

    sprintf (file_util_msg, 
        "REJECTED/DELETED BY FA    : %d", NUMELTS (deleted_dtks)) ; 
    if ( NUMELTS (deleted_dtks) > 0 )
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

    sprintf (file_util_msg, 
        "REJECTED BY APS           : %d", NUMELTS (rejected_dtks)) ;
    if ( NUMELTS (rejected_dtks) > 0 )
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

    sprintf (file_util_msg, 
        "BLOCKED BY CONFLICTS      : %d", NUMELTS (CON_dtks)) ;     
    if ( NUMELTS (CON_dtks) > 0 )
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

    if ( NUMELTS (error_dtks) > 0 )
    {
        sprintf (file_util_msg, 
            "ERROR                     : %d", NUMELTS (error_dtks)) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        sprintf (file_util_msg, 
            "PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n") ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf (report_fp,
"\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ;
    }

    sprintf (file_util_msg, 
        "REJECTED BY OMISSION      : %d", NUMELTS (omission_dtks)) ;
    if ( NUMELTS (omission_dtks) > 0 )
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

    sprintf (file_util_msg, 
        "TOTAL UPDATES             : %d", NUMELTS (dtk_updates)) ;
    aps_log_msg(file_util_progname, APS_INFO, 
        file_util_msg, DO_SYSLOG, DO_PRINT);
 
    if ( fa_number_of_error_records > 0 )
    {
        sprintf (file_util_msg, 
"ERROR RECS IN FA FILE     : %d      \
PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n", fa_number_of_error_records) ;
        aps_log_msg(file_util_progname, APS_ERROR, 
            file_util_msg, DO_SYSLOG, DO_PRINT);


        fprintf (report_fp,
"\nERROR RECS IN FA FILE     : %d", fa_number_of_error_records ) ;
        fprintf (report_fp,
"\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ;
    }

    if (fa_number_of_error_records  > 0
    ||  NUMELTS( error_dtks )       > 0
    ||  NUMELTS( rejected_dtks )    > 0
    ||  NUMELTS( CON_dtks )         > 0
    ||  NUMELTS( deleted_dtks )     > 0
    ||  NUMELTS( omission_dtks )    > 0  )
    {
        /*
        -- we want the planner to read the
        -- report.
        */
        planner_read_status = 1 ;
    }

    DEL_LIST( deleted_dtks ) ;
    DEL_LIST( omission_dtks ) ;
    DEL_LIST( other_sat_dtks ) ;
    DEL_LIST( same_sat_dtks ) ;

    /* 
    -- CON_dtks were suspended due to antenna 
    -- conflicts and might be re-instated later.  
    -- however, for any FA, they all should be 
    -- considered as rejected for now.  
    */
    db_record_llist_move (rejected_dtks, CON_dtks) ;
    DEL_LIST( CON_dtks ) ;

/*============================================================================*/

    /*
    -- Carry out any operations needed after DTK processing.
    -- There may be multiple operations carried out in sequence.
    */
    if (fa_trigger_dtk_list_handling_for_STGS)
    {
        /*
        -- NOTE:  
        -- in the STGS handling for ADEOS, every data-take 
        -- in the rev must be either all rejected or all 
        -- accepted.  for that reason, for every rejected 
        -- data-take, we need to find any that might have 
        -- been accepted by the dtkm_process_dtk_proposal_list() 
        -- routine.  
        */

        /*
        -- Consolidate the rejected_dtks and error_dtks lists.
        -- Move rejected_dtks into error_dtks.  rejected_dtks is now empty.
        --         ________________       _____________
        --        /_rejected_dtks_/ ---> /_error_dtks_/
        */
        if ( NUMELTS(rejected_dtks)  > 0 )
            db_record_llist_move (error_dtks, rejected_dtks) ;


#ifdef PRINT_DIAG
        printf("\nfa_ascii_processor: NEW REJECTED_DTKS list:  \n");
        dtkm_print_list (stdout, error_dtks ) ;
#endif


        /*
        -- Sort the error_dtks
        -- Place sorted list in rejsort_dtks
        --         _________________         _____________
        --        /__rejsort_dtks__/  using /_error_dtks_/
        */
        ord_rejected_dtks = create_dyn_ord_llist(db_record_rev_comparison) ;
        db_record_llist2ord_llist (error_dtks, ord_rejected_dtks) ;
        db_record_ord_llist2llist (ord_rejected_dtks, rejsort_dtks) ;


#ifdef PRINT_DIAG
        printf("\nfa_ascii_processor: SORTED REJECTED_DTKS list:  \n");
        dtkm_print_list (stdout, rejsort_dtks ) ;
        printf("\nfa_ascii_processor: INCOMING_ACCEPTED_DTKS list:  \n");
        dtkm_print_list (stdout, accepted_dtks ) ;
#endif

        /*
        -- If there are rejected dtks, check them against accepted dtks
        -- Some accepted dtks may now be placed on additional_reject_dtks
        */
        if ( NUMELTS(rejsort_dtks)  > 0 )
        {
            rejection_rev = 0 ;

            for (
                dtk_rec = (DB_RECORD **)FIRST(rejsort_dtks, dtk_rec_ptr) ;
                dtk_rec ;
                dtk_rec = (DB_RECORD **)NEXT( rejsort_dtks, dtk_rec_ptr)
                )
            {
                /*
                -- set up the rev which will determine an additional rejection
                */
                if (CAST_DTK_REV dtk_rec[DTK_REV] != rejection_rev)
                    rejection_rev = CAST_DTK_REV dtk_rec[DTK_REV] ;
                else
                    continue ;

                /*
                -- go through the accepted record list looking for matches
                */
                for (
                    dtk_rec_inquestion = (DB_RECORD **)FIRST( accepted_dtks, 
                        dtk_rec_inquestion_ptr) ;
                    dtk_rec_inquestion ;
                    dtk_rec_inquestion = (DB_RECORD **)NEXT( accepted_dtks, 
                        dtk_rec_inquestion_ptr)
                    )
                {
                    /*
                    -- If an already accepted record matches a rejected record,
                    -- place this matching record in additional_reject_dtks
                    */
                    if (CAST_DTK_REV dtk_rec_inquestion[DTK_REV] 
                        == rejection_rev )
                    {
                        dtk_unlinked_rec = (DB_RECORD **) UNLINK_AT_CURSOR(
                                    accepted_dtks, dtk_rec_inquestion_ptr ) ;

                        APPEND(additional_reject_dtks, dtk_unlinked_rec, 
                                        free_db_record, dtk_unlinked_rec) ;
                    }

                } /* end loop on accepted_dtks */

            } /* end loop on rejsort_dtks */

        } /* ENDIF   if ( NUMELTS(rejsort_dtks)  > 0 )   */


        /*
        -- Condense rejected_dtks to retain only one rejected element per rev
        --         ________________               ________________
        --        /_rejsort_dtks__/ condenses to /_rejected_dtks_/
        */
        if ( NUMELTS(rejsort_dtks) > 0 )
        {
            rejection_rev = 0 ;
            for (
                dtk_rec = (DB_RECORD **)FIRST(rejsort_dtks, dtk_rec_ptr) ;
                dtk_rec ;
                dtk_rec = (DB_RECORD **)NEXT( rejsort_dtks, dtk_rec_ptr)
                )
            {
                if (CAST_DTK_REV dtk_rec[DTK_REV] != rejection_rev)
                    rejection_rev = CAST_DTK_REV dtk_rec[DTK_REV] ;
                else
                    continue ;

                dtk_unlinked_rec = (DB_RECORD **) UNLINK_AT_CURSOR(
                            rejsort_dtks, dtk_rec_ptr ) ;
                APPEND(rejected_dtks, dtk_unlinked_rec, 
                                free_db_record, dtk_unlinked_rec) ;

            }
        }



#ifdef PRINT_DIAG
        printf("\nfa_ascii_processor: ADDITIONAL REJECTED_DTKS list:  \n");
        dtkm_print_list (stdout, additional_reject_dtks ) ;
#endif
        /*
        -- If there are any additional_reject_dtks,
        -- report them in the report_fp
        */
        if ( NUMELTS(additional_reject_dtks)  > 0 )
        {
            sprintf (file_util_msg, "REJECTS BY AGREEMENT   : %d\n", 
                NUMELTS (additional_reject_dtks)) ;
            aps_log_msg(file_util_progname, APS_INFO, 
                file_util_msg, DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, "\n\nREPORT ADDENDUM:\n\n\
PREVIOUSLY ACCEPTED INPUT DATA-TAKES, NOW REJECTED BY AGREEMENT.\n\
================================================================\n\n") ;

            for (
                dtk_rec = (DB_RECORD **)FIRST(additional_reject_dtks, 
                    dtk_rec_ptr) ;
                dtk_rec ;
                dtk_rec = (DB_RECORD **)NEXT( additional_reject_dtks, 
                    dtk_rec_ptr)
                )
            {
                fprintf(report_fp,
    "\nREJECTED due to full path unavailability (by agreement with NASDA):\n");
                dtkm_print( report_fp, dtk_rec ) ;

            }

        }



        /*
        -- If there are any rejected_dtks, mark 'REJ' for ALL dtks in db
        -- which are in the same rev.  By agreement with NASDA. 
        -- rejected_dtks has already been condensed to one record per rev.
        */
        if (NUMELTS(rejected_dtks)  > 0 )
        {
            for (
                dtk_rec = (DB_RECORD **)FIRST(rejected_dtks, dtk_rec_ptr) ;
                dtk_rec ;
                dtk_rec = (DB_RECORD **)NEXT( rejected_dtks, dtk_rec_ptr)
                )
            {
                /*
                -- Preparatory step. Set up the clauses for any rejects.
                */
                sprintf(fields_to_set, "%s = 'REJ'", APS_COL(DTK, DTK_DTKSTAT) );

                sprintf(where_clause,
                "where %s = '%s' and %s = '%s' and %s = %ld",
                    APS_COL(DTK, DTK_SAT),
                    CAST_DTK_SAT dtk_rec[DTK_SAT],
                    APS_COL(DTK, DTK_SENSOR),
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                    APS_COL(DTK, DTK_REV),
                    CAST_DTK_REV dtk_rec[DTK_REV]  ) ;

                if (db_update_records(APS_dbproc,
                    APS_TABLE(DTK), fields_to_set, where_clause) < 0 )
                {
                    return_code = dtkm_rec2str( dtk_rec ) ;
                    if (return_code != 0)
                    {
                    aps_log_msg(file_util_progname, APS_ERROR, 
"REJECTION ACTION FAILED.  PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n",
                        DO_SYSLOG, DO_PRINT);
                    }
                    else
                    {
                    aps_log_msg(file_util_progname, APS_ERROR, 
                        "REJECTION ACTION FAILED. Notify planner:\n", 
                        DO_SYSLOG, DO_PRINT);
                        /* 
                        -- dtk_string is globally declared; 
                        -- see dtkm_utilities.h
                        */
                    aps_log_msg(file_util_progname, APS_ERROR, 
                        dtk_string, 
                        DO_SYSLOG, DO_PRINT);
                    }

                    fprintf(report_fp,
                        "REJECTION ACTION FAILED. Notify planner:\n");
                    dtkm_print( report_fp, dtk_rec ) ;
                }
            }

        }


        /*
        -- empty the error_dtks llist in order to maintain the response_file
        -- logic below
        --         ________________       _______________
        --        /__error_dtks___/ ---> /_rejsort_dtks_/
        */
        db_record_llist_move (rejsort_dtks, error_dtks) ;

        /*
        -- CLEAN UP, CLEAN UP
        */
        DEL_LIST( ord_rejected_dtks) ;

    } /* END of    if (fa_trigger_dtk_list_handling_for_STGS)   */

/*============================================================================*/

    /*
    -- If any rejections or errors were returned,
    -- AND there exists a definition for a response file, create it.
    -- NOTE: omissions are NOT included because they did not originate
    --       in an incoming file from flight agency
    --
    -- NEW, NEW, NEW!!!!  If fa_trigger_always_create_response_file is set,
    -- we will create a response file regardless of the
    -- rejects or errors.  It will always contain the header, plus any 
    -- rejects or errors (if any)
    --
    */
    if ( response_file )    /* if response file definition exists... */
    {
        if ( NUMELTS(error_dtks) + NUMELTS(rejected_dtks) > 0 )
        {
            /* Ignore LINT warning: constant operand to op: "!" */
            if (ascii_file_creator( !(PRINT_HEADER_ONLY),
                  error_dtks, rejected_dtks ,
                  file_descriptor->response_file_definition,
                  response_file )       < 0 )
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "ascii_file_creator() could not create response file\n", 
                    DO_SYSLOG, DO_PRINT);
        }

        else if ( fa_trigger_always_create_response_file
             && NUMELTS(dtk_updates)  > 0 )
        {
            if (ascii_file_creator( PRINT_HEADER_ONLY,
                  dtk_updates, NULL ,
                  file_descriptor->response_file_definition,
                  response_file )           < 0 )
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "ascii_file_creator() could not create response file\n", 
                    DO_SYSLOG, DO_PRINT);
        }

    } 

#ifdef PRINT_DIAG
    dtkm_print_list (report_fp,input_dtk_list);
#endif


    DEL_LIST ( accepted_dtks  ) ;
    DEL_LIST ( rejected_dtks  ) ;
    DEL_LIST ( error_dtks     ) ;
    DEL_LIST ( rejsort_dtks   ) ;
    DEL_LIST ( dtk_updates   ) ;
    DEL_LIST ( additional_reject_dtks ) ;
    DEL_LIST ( input_dtk_list ) ;

    /*
    -- COMPLETED processing. 
    -- Item (5). Terminate the planning permission.
    */
    return_code = mu_permission_terminate( APS_dbproc,
        permission.id,
        fa_activity_type,               /*  mu_activity_id  */
        MU_PLANNING_ACTIVITY_TYPE) ;    /*  activity_types  */

    if( return_code < 0 )
    {
        fprintf(stderr,"%s(%d):  permission terminate error code %d\n",
            __FILE__, __LINE__, return_code ) ;
        fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
    }

    /*
    -- Item (6).IF we requested ACTIVITY permission ourselves, terminate it.
    */
    if (TERMINATE_ACTIVITY_PERMISSION)
    {
        return_code = mu_permission_terminate( APS_dbproc,
            permission.id,
            fa_activity_type,           /*  mu_activity_id               */
            MU_SINGLE_ACTIVITY_TYPE) ;  /*  activity_type                */
        /*
        -- At this point, we should not discard the results of a
        -- full run, just because we could not terminate a permission.
        -- Take no action.
        */
        if( return_code < 0 )
        {
            fprintf(stderr,"%s(%d):  permission terminate error code %d\n",
                __FILE__, __LINE__, return_code ) ;
            fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
        }
    }

    if (dtkm_process_list_return_code < 0 )
    {
        sprintf (file_util_msg, 
            "Error encountered in dtkm_process_dtk_proposal_list():%s\n",
            DTKM_ERROR_MESSAGE(dtkm_process_list_return_code) ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf(report_fp, "\nError encountered in fa_ascii_processor:\n%s\n",
            DTKM_ERROR_MESSAGE(dtkm_process_list_return_code) );
        return (FA_ASCII_REC_PROCESS_ERROR) ;
    }
    else if ( planner_read_status )
    {
        /* 
        -- If there were records found in error during file ingestion, 
        -- delete any possible response file.
        -- Else, if the file ingestion went well, and we have rejects 
        -- during db processing, go ahead and create the response file.
        */
        if (fa_number_of_error_records  > 0)
        {
            aps_log_msg(file_util_progname, APS_INFO, 
                "There were errors in the input file.\n", 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_INFO, 
                "FA PROCESS FILE IN ERROR; PLANNER MUST READ THE REPORT\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, "\nThere were errors in the input file.  \n" ) ;
            fprintf(report_fp,
                "FA PROCESS FILE IN ERROR; PLANNER MUST READ THE REPORT\n" ) ;

            return (FA_ASCII_REC_PROCESS_ERROR) ;
        }
        else
        {
            aps_log_msg(file_util_progname, APS_INFO, 
                "There were rejections and/or errors.\n", 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_INFO, 
                "FA PROCESS FILE OK BUT PLANNER MUST READ THE REPORT\n",
                DO_SYSLOG, DO_PRINT);

            fprintf(report_fp, "\nThere were errors and/or rejections.  \n" ) ;
            fprintf(report_fp,
                "FA PROCESS FILE OK BUT PLANNER MUST READ THE REPORT\n" ) ;

            return (FA_ASCII_REC_PROC_DONE_CREATE_META) ;
        }
    }
    else
    {
        sprintf (file_util_msg, 
            "SUCCESS.  There were no rejections or errors.  \n") ;
        aps_log_msg(file_util_progname, APS_INFO, 
            file_util_msg, DO_SYSLOG, DO_PRINT);

        fprintf(report_fp, "There were no errors or rejections.  \n" ) ;

        if (fa_trigger_always_create_response_file)
            return FA_ASCII_REC_PROCESS_OK_CREATE_META ;
        else
            return FA_ASCII_REC_PROCESS_OK ;
    }


}  /* end of fa_ascii_processor  */


/*==============================================================================
Function:       identify_file

Description:    Identify the given file type to a flight agency type.

Parameters:     

Returns:        
Type  Name                  Definition
                            FALSE = could not identify file
                            TRUE  = file successfully identfied

Creator:        Miguel Siu

Creation Date:  Thu Jul 27 15:38:16 PDT 1995

Notes:      
==============================================================================*/
int     /* Ignore LINT warning: declared global, could be static */
identify_file(
    char            *file_type,
    FA_FILENAME     *fa_files,
    FA_FILENAME     **file_descriptor)
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
Function:       identify_PMF_file

Description:    Identify the given PMF file type to a flight agency type.

Parameters:     

Returns:        
Type  Name                  Definition
                            FALSE = could not identify file
                            TRUE  = file successfully identfied

Creator:        Miguel Siu

Creation Date:  Fri Dec  8 10:03:23 PST 1995

Notes:      
==============================================================================*/
int     /* Ignore LINT warning: declared global, could be static */
identify_PMF_file(
    char            *file_type,
    PMF_FILENAME    *fa_files,
    PMF_FILENAME    **file_descriptor)
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
        printf ("FOUND PMF MATCH: \n");
#endif
            return (TRUE) ;
        }
        i++ ;
    }

    return  (FALSE) ;

}
