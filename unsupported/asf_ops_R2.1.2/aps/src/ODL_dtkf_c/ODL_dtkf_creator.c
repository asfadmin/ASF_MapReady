#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       ODL_dtkf_creator.c

Description:    source file for routines that create an ODL file from a dtk 
                llist of DB_RECORDS.  

External Functions Defined:
    
File Scope Functions:

External Variables Defined:
    
File Scope Variables:
                ODL_FILEDEFS asf_wos_file[] 
                        array describing ASF WOS (weekly operations schedule) 
                        keywords and value format requirements, as well as 
                        database sources for this information.
Notes:

==============================================================================*/
#pragma ident   "@(#)ODL_dtkf_creator.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/ODL_dtkf_c/SCCS/s.ODL_dtkf_creator.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absolute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read             */
#include <sys/uio.h>        /* for read             */
#include <unistd.h>         /* for unlink()         */
#include <sys/unistd.h>     /* for read             */
#include <mu_utilities.h>   /* for permissions      */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */
#include "aps_log_msg.h"    /* for APS syslogging                   */
#include "aps_defs.h"       /* exit codes for APS syslogging        */

/* FOR ODL_CREATOR */
#include <GENconversions.h>  /* for fa_* global variables */
#include <ODLconversions.h>
#include <ODL_defs.h>       /* ODL typedefs.  */
#include "ODL_dtkf_creator.h" /* for ODL defines  */

#include "timeconv.h"   /* for tc_validate_asf_datetime(), tc_systime2asf()  */


/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_schstat.h"     /* for schstat table             */

#include "dtkm_utilities.h"     /* for dtkm_print_list()  */
#include "APSpmfutilities.h"    /* for the APSPMF_metadata definitions      */

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* FOR WOS retrieve dtk routines */
#include "file_utilities.h"

/* for debugging purposes */
#define   PRINT_DIAG 1 
#undef    PRINT_DIAG

/* 
-- REPORT_DEF  element declarations need to be global:
*/
int           number_o_receptions;
int           report_code;
char          CSA_creation_date[] = "yyyy-mm-dd:hh:mm:ss";
char          *file_util_progname ;    /* required by the libfileutils.a */
char          file_util_msg[MSG_LEN] ; /* required by the libfileutils.a */

/* ERROR MESSAGES   */
/* they correspond to defined in ODL_dtkf_creator.h   */
static char *ODL_error_message[] =
{
    "zero is not a valid error code",      /*  0 */
    "ODL_UNABLE_TO_CREATE_FILE",           /*  1 */
    "ODL_NULL_FIRST_DTK_REC" ,             /*  2 */
    "ODL_CONTROL_DATA_NOT_OBTAINED",       /*  3 */
    "ODL_DEFAULT_DATA_NOT_OBTAINED",       /*  4 */
    "ODL_DTK_RECORD_FIELD_NOT_OBTAINED",   /*  5 */
    "ODL_DEFAULT_FIELD_NOT_OBTAINED",      /*  6 */
    "ODL_FAIL2CALCULATE_FIRSTLAST_TRACK",  /*  7 */
    "unknown code -8",
    "unknown code -9",
    "unknown code -10",
    "unknown code -11"  /* no comma here.  */
};


/*==============================================================================
Function:       retrieve_ODL_dtks()

Description:    get the data-takes appropriate for the file type.  

Creator:        Lawrence Stevens

Creation Date:  Thu Apr 10 19:23:41 PDT 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int retrieve_ODL_dtks(
    char    *filetype,            /* input file type.                       */
    char    *retrieve_strttime,   /* retrieve using this time.              */
    char    *retrieve_stoptime,   /* retrieve using this time.              */
    llist   *dtk_file_list,       /* put dataa-takes for the file here.     */
    llist   *dtk_error_list )     /* put error data-takes, if any, here.    */
                                  /* error dtks are downlinks with no obs   */
                                  /* only done when writing ADDM or MDDM    */
{

    int         nrecs ;
    int         return_code ;

    DB_RECORD   **dtk_rec ;
    cursor      dtk_file_list_ptr ;

    llist       *obs_dtks ;
    llist       *list_check ;

    /* 
    -- the routine retrieve_dtks_for_WOS() is located in 
    --   .../src/lib_fileutils/ODL_file_ingestion.c
    -- it is also used when ingesting a WOS for 
    -- WOS comparison.  
    */
    nrecs = retrieve_dtks_for_WOS(filetype, retrieve_strttime, 
        retrieve_stoptime, dtk_file_list ) ;
    if( nrecs < 0 )
        return nrecs ;

    if ( strcmp(filetype, "ADDM") != 0 
    &&   strcmp(filetype, "MDDM") != 0 )
    {
        /* 
        -- return unless a downlink-to-data-take 
        -- mapping file_type  
        */
        return nrecs ;
    }

    /* 
    -- This file is to be a downlink-to-data-take 
    -- mapping file.  must check every data-take in dtk_file_list:
    -- if a downlink has no observations linked to it, 
    -- then must move the record out of the dtk_file_list
    -- and into the dtk_error_list.  
    -- This is done by an in-loop rec-by-rec copy to dtk_error_list, 
    -- then a one-call deletion from dtk_file_list after the loop 
    -- is done.  
    -- NOTE:  this may seem goofy to move the record by 
    -- a 2 step copy then delete using dtkm_remove_dtks_from_list().  
    -- But we would be looping through a linked list and deleting as 
    -- we go, which messes up the pleasant NEXT() increment 
    -- in the for loop.  If you don't like this, 
    -- don't complain, re-code it, but you better be sure you 
    -- thoroughly test your new code.  It will be trickier 
    -- than this.  
    */
    obs_dtks = create_dyn_llist() ;
    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_file_list, dtk_file_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_file_list, dtk_file_list_ptr)  
        )
    {
        return_code = dtkm_dl2obs( dtk_rec, obs_dtks ) ;
        if( return_code < 0 )
            return return_code ;

        if( NUMELTS( obs_dtks ) <= 0 )
        {
            /* 
            -- this is an error data-take.  duplicate this data-take into 
            -- dtk_error_list.  when this loop is finished, we will 
            -- remove these data-takes from dtk_file_list as required.  
            */
            list_check = dtkm_duplicate_dtk_into_list( dtk_rec, dtk_error_list);
            if( list_check != dtk_error_list )
                return -1 ;
        }
        /* 
        -- delete each member in the obs_dtks 
        -- list, if any.  we are done with this downlink 
        -- and don't want to mess up the next count.  
        */
        DEL_ALL( obs_dtks ) ;
    }
    /* clean up memory before exiting.  */
    DEL_LIST( obs_dtks ) ;

    /* 
    -- remove every dtk from dtk_file_list if present 
    -- in dtk_error_list, too:  
    */
    return_code = dtkm_remove_dtks_from_list( dtk_error_list, dtk_file_list ) ;
    if( return_code < 0 )
        return return_code ;

    return ( NUMELTS( dtk_file_list ) ) ;

}

/*==============================================================================
Function:       get_odl_filedef()
Description:    look up filetype in the table to get the filedefs pointer.  
Creator:        Lawrence Stevens
Creation Date:  Wed Apr  9 14:24:28 PDT 1997
==============================================================================*/
static ODL_CREATE_VALUEDEFS *get_odl_filedef(
            char                *filetype ,
            ODL_CREATE_FILENAME *odl_create_file_info )
{

    int j = 0 ;

    /*
    -- search through the input structure list.  
    -- if there is a match on the input filetype, 
    -- return the value of the create_file_valuedefs 
    -- pointer that goes with it.  Otherwise, return NULL.  
    -- The input structure list is terminated by an entry 
    -- with NULL values.  
    */
    for( j = 0 ; 
         odl_create_file_info[j].create_file_type_name != NULL ; 
         j++ )
    {
        if( strcmp( odl_create_file_info[j].create_file_type_name, 
            filetype ) == 0 )
        {
            /* found.  */
            return ( odl_create_file_info[j].create_file_valuedefs ) ;
        }
    }
    return NULL ;
}



/*==============================================================================
Function:       usage_exit()
Description:    print usage and exit.  
Creator:        
Creation Date:  Mon Apr  7 17:58:06 PDT 1997
==============================================================================*/
static void usage_exit(char *progname, int error_code)
{
    if (error_code < 0)
    {
        aps_log_msg(file_util_progname, APS_INFO, 
            "Program terminated abnormally.\n", 
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR);
    }

    fprintf(stderr,     /* nicer format, keep it */
    "\nusage:  %s -P <password> -t <filetype> -b <strttime> -e <stoptime> \n", 
        progname);
    fprintf(stderr,     /* nicer format, keep it */
        "\t\t\t [-U <user name>] [-d] [-o <filename>] [-p <permission>]\n\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-U <user name> Sybase userid for Sybase account\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-P <password>  Sybase password for Sybase account\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-b <strttime>  start time in asf format:  yyyy:ddd:hh:mm:ss.sss\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-e <stoptime>  stop time  in asf format:  yyyy:ddd:hh:mm:ss.sss\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\n\t-t <filetype>  The %s will create the following reports:\n",
        file_util_progname) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t   AWOS    (this is the ASF Weekly Operations Schedule)\n" ) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t   MWOS    (this is the McMurdo Weekly Operations Schedule)\n" ) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t   AREQ    (this is the McMurdo Request for Availability)\n" ) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t   ADDM    (this is the ASF downlink-to-data-take mapping )\n" ) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t   MDDM    (this is the McMurdo downlink-to-data-take mapping )\n");

    fprintf(stderr,     /* nicer format, keep it */
        "\n\t-o <filename>  name of file to be produced.") ;
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-p <permission>  single-activity permission (from the calling program)\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\n\t-d      if given, %s also prints out the data-take \n", progname) ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t        list to standard output. \n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t        if given, output is written to this file instead\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t        of standard out, and the data-take list, if -d\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t        is given, is written to a file with the characters\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\t        '_DTK' appended to the given filename.\n") ;
    fprintf(stderr,     /* nicer format, keep it */
        "\n\tNOTES:  \n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tThe data-takes are read from the database according to the,\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tfiletype and the time bracket in the command line.\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tAlthough we require the use of a password to implement the multi-user\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\tfunction, the %s uses READ-ONLY database access.\n",file_util_progname) ;
    fprintf(stderr,     /* nicer format, keep it */
    "\n\t%s version %s %s\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(file_util_progname, APS_INFO, 
        "Program terminated abnormally.\n", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}


/*==============================================================================
Function:       odl_dtkfile_create_record

Description:    write an ODL header, data record, or trailer record
                according to:
                    record type 
                    file definition 
                    data-take record

Creator:        Lawrence Stevens

Creation Date:  Fri Aug 11 10:45:43 PDT 1995

NOTE:  This routine loops through the ODL_CREATE_VALUEDEFS odl_dmap_file[] array 
       or the ODL_CREATE_VALUEDEFS odl_wos_file[] array located in
       .../src/lib_interface/ODLconversions.c

EXAMPLE:  
        return_code = odl_dtkfile_create_record( ODL_FILE_RECORD, odl_dtkfile,
            input_dtk_rec, fp ) ;

==============================================================================*/
/* CAN'T be static; used in .../src/lib_interface/ODLconversions.c  */
int odl_dtkfile_create_record(
    int         odl_dtkfile_record_type,  
                  /* = ODL_FILE_HEADER | ODL_FILE_RECORD | ODL_FILE_TRAILER */
    ODL_CREATE_VALUEDEFS  *odl_dtkfile,
    DB_RECORD             **input_dtk_rec ,
    FILE                  *fp )               /*  output file pointer   */
{
    int     j ;
    char    destination_str[1024] ;
    char    *keyword ;

    for (j = 0 ; odl_dtkfile[j].source_code != NULL  ; j ++)
    {
        /* 
        -- on this loop thru odl_dtkfile[], process only 
        -- .destination_code == odl_dtkfile_record_type  
        */
        if ( odl_dtkfile[j].destination_code != odl_dtkfile_record_type )
            continue ;

        /* 
        -- First assign the keyword value, then assign the 
        -- destination string.  Finally write the keyword and 
        -- destination string to the output file.
        */

        keyword = odl_dtkfile[j].keyword ;

        switch(odl_dtkfile[j].source_code)
        {
            /* 
            -- take action according to the 
            -- source code:  
            */
        case ODL_DTK_RECORD :
            if (! (odl_dtkfile[j].conversion)(
                   odl_dtkfile[j].conversion_table, 
                   input_dtk_rec[odl_dtkfile[j].source.index], 
                   destination_str) )
                return (ODL_DTK_RECORD_FIELD_NOT_OBTAINED);
            break ;

        case ODL_CONTROL    :
            if (! (odl_dtkfile[j].conversion)(
                   odl_dtkfile[j].conversion_table, 
                   odl_dtkfile[j].source.pointer, 
                   destination_str) )
                return (ODL_CONTROL_DATA_NOT_OBTAINED);
            break ;

        case ODL_DEFAULT    :
            if ( odl_dtkfile[j].conversion != NULL)
            {
                if (! (odl_dtkfile[j].conversion)(
                       odl_dtkfile[j].conversion_table, 
                       odl_dtkfile[j].source.pointer, 
                       destination_str) )
                    return (ODL_DEFAULT_DATA_NOT_OBTAINED);
            }
            else
                strcpy(destination_str, "" ) ;
            break ;

        default:
            continue ;
            /* commented to satisfy the lint man:  */
            /*  break ;   */
        }

        /* 
        -- the destination string was assigned; now write to 
        -- output file.  
        */
        /* 
        -- some keyword values are optional according to data values.  
        -- this is implemented by the destination_str, which if NONE, 
        -- then we skip the keyword and value.  
        */
        if ( strcmp(destination_str, "NONE") != 0 
        &&   strcmp(destination_str, "'NONE'") != 0 )
            fprintf(fp, "%s%s\n", keyword, destination_str ) ;
    }

    return ODL_DTKFILE_CREATE_RECORD_OK ;
}


/*==============================================================================
Function:      odl_dtkfile_creation 

Description:    Writes the records for an ODL file from 
                data in a dtk DB_RECORD linked list.

Returns:        int                             
    = 0:        Success:  ODL_WOS_CREATION_OK 
    < 0:        Error:

Creator:        Lawrence Stevens

Creation Date:  Sun Aug  6 23:25:41 PDT 1995

Notes:      
==============================================================================*/
static int odl_dtkfile_creation(
    llist                 *input_dtk_list, 
    ODL_CREATE_VALUEDEFS  *odl_dtkfile,
    FILE                  *fp )               /*  output_file_ptr   */
{

    /* declarations         */
    int             return_code;

    /* helpers to navigate the input parameter:  llist  *input_dtk_list  */
    DB_RECORD       **input_dtk_rec ;
    cursor          input_dtk_list_ptr ;

    /*
    -- check for 0 records.  if none, just return
    */
    if( NUMELTS( input_dtk_list ) <= 0 )
        return TRUE ;

    /* get the first record from the input list to provide values if needed:  */
    input_dtk_rec = (DB_RECORD **) FIRST(input_dtk_list, input_dtk_list_ptr);
    if ( input_dtk_rec == NULL) 
        return ODL_NULL_FIRST_DTK_REC ;

    /*
    -- Now go through each dtk DB_RECORD in input_dtk_list 
    -- starting with the first record 
    -- and write out the data to the file.  
    */
    for     /* loop thru each dtk DB_RECORD in the list:   */
    (
        input_dtk_rec = (DB_RECORD **) FIRST(input_dtk_list,input_dtk_list_ptr);
        input_dtk_rec != NULL ;
        input_dtk_rec = (DB_RECORD **) NEXT(input_dtk_list, input_dtk_list_ptr) 
    )
    {
        /*
        -- initialize global variables fa_antenna_id and fa_sat_id
        -- NO ASSUMPTION MADE
        -- We do not assume that all entries in the input_dtk_rec list
        -- belong to the same satellite or the same antenna.
        -- A groundstation may support various satellites and multiple
        -- antennas, that is why we re-initialize fa_antenna_id, fa_sat_id.
        */
        fa_antenna_id = CAST_DTK_ANTENNA_ID input_dtk_rec[DTK_ANTENNA_ID] ;
        strcpy( fa_sat_id, CAST_DTK_SAT input_dtk_rec[DTK_SAT] ) ;

        /* set up global pointer for this dtk rec:  */
        odl_downlink_dtk_rec = input_dtk_rec ;

        /*
        -- WRITE THE DTK RECORD INFO 
        */
        return_code = odl_dtkfile_create_record( ODL_FILE_RECORD, odl_dtkfile,
            input_dtk_rec, fp ) ;
        if ( return_code < 0 )
            return return_code ;

        /*
        -- DTK RECORD IS COMPLETE
        */

    }  

    /*
    -- ALL DTK RECORDS COMPLETED
    */

    return TRUE ;

}/* end of odl_dtkfile_creation() */


/*==============================================================================
Function:       Main routine to create an ODL file from data-takes.  

Description:    

Parameters:     

Returns:        - APS_EXIT_OK       on success
                - APS_EXIT_ERROR    when no record found

Creator:        Lawrence Stevens

Creation Date:  Mon Aug  7 16:37:34 PDT 1995

Notes:      
==============================================================================*/
void
main(int argc, char *argv[])
{
    DBPROCESS       *APS_dbproc ;

    /* for getopt()   */
    extern char     *optarg ;
    extern int      optind ;

    /* declarations     */
    RETCODE     return_code;
     
    DB_RECORD             **min_dtk_rec ;
    DB_RECORD             **max_dtk_rec ;
    FILE                  *fp_output_file ;
    ODL_CREATE_VALUEDEFS  *odl_filedef_ptr = NULL ;
    APSPMF_metadata       *PMF_struct ;
    PMF_FILENAME          *pmf_descriptors = NULL ;
     
    int     slash = '/';
    int     print_dtk_list = FALSE ;  /* flag to also print dtk list to stdout*/
    int     create_PMF_file = FALSE ; /* PMF file needed only if corresponding
                                         data file is also created          */
    int     pflag = 0 ;               /* signals terminate permission       */
    int     permission_id = 0 ;       /* optional permission_id passed in   */
    char    *filename = NULL ;
    char    filename_DTK[128] = "" ;
    char    *filetype = NULL ;
    char    *strttime = NULL ;
    char    *stoptime = NULL ;
    char    *metafilename = NULL ;
    char    *outgoing_file = NULL ;
    char    *short_filename = NULL ;
    char    *temp_filename = NULL ;
    char    *dbname = NULL ;
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */

    llist       *dtk_error_list = NULL ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **first_dtk_rec ;
    cursor      dtk_list_ptr ;
     
    int         j, c;      /* used as return character from getopt()       */
     
    char    flag_list[20] = "P:U:p:t:b:e:do:"; /* list of flags for getopt() */
     
EQUIV_TABLE filetype2activity[]=
{   {"AWOS",         MU_AWOS},
    {"MWOS",         MU_MWOS},
    {"AREQ",         MU_AREQ},
    {"ADDM",         MU_ADDM},
    {"MDDM",         MU_MDDM},
    {NULL, NULL}
};
        
/*
-- ASSUMPTION
-- The following assumption can be made for WOS generation only:
-- we know that the entire file is destined for a single groundstation.
*/
EQUIV_TABLE filetype2station[]=
{   {"AWOS",         MU_ASF_STATIONID},
    {"MWOS",         MU_MCM_STATIONID},
    {"AREQ",         MU_MCM_STATIONID},
    {"ADDM",         MU_ASF_STATIONID},
    {"MDDM",         MU_MCM_STATIONID},
    {NULL, NULL}
};
        
    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;


    /* initialize the vector library; exit with a message if an error   */
    return_code = init_vec_lib();
    if(return_code)
    {
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(APS_EXIT_ERROR);
    }

    /*
    -- store the program name for system/error logging
    -- and start up the Syslog process
    */
    file_util_progname = strrchr( argv[0], '/' ) ;
    if (file_util_progname == NULL)
        file_util_progname = argv[0] ;
    else
        file_util_progname++ ;
    aps_open_syslog();

    sprintf(file_util_msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(file_util_msg, " " ) ;
        strcat(file_util_msg, argv[j] ) ;
    }
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg,
        DO_SYSLOG, DO_PRINT);

    /* if the user wants a usage:  */
    if( argc <= 2 ) 
        usage_exit(file_util_progname, 0 );

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'P':
                    if( password != NULL )      /* Check for duplicate flags */
                        usage_exit(file_util_progname, 0);
                    password = optarg ;
                    break;
            case 'U':
                    if( sybase_userid != NULL ) /* Check for duplicate flags */
                        usage_exit(file_util_progname, 0);
                    sybase_userid = optarg ;
                    break;
            case 'p':
                if (pflag != 0)                 /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                pflag++ ;
                return_code = sscanf( optarg, "%d", &permission_id ) ;
                if( return_code != 1 )
                {
                    fprintf(stderr,
                        "%s(%d): %s  can't be scanned into int permission_id\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(file_util_progname, 0);
                }
                break ;
            case 't':
                if(filetype != NULL)            /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                filetype = optarg ;

                /* 
                -- assign odl_filedef_ptr at this location:
                -- odl_create_file_info[] is located in 
                -- .../src/lib_interface/ODLconversions.c
                */
                odl_filedef_ptr = get_odl_filedef( filetype, 
                    odl_create_file_info );
                if( odl_filedef_ptr == NULL )
                {
                    fprintf(stderr, "Error in file type value:  %s\n", 
                        filetype ) ;
                    usage_exit(file_util_progname, 0);
                }
                break;
            case 'b':
                if(strttime != NULL)            /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                strttime = optarg ;
                if (tc_validate_asf_datetime(strttime) != TRUE)
                {
                    sprintf(file_util_msg, 
                        "Error in strttime, %s\n", strttime) ;
                    aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                        DO_SYSLOG, DO_PRINT);
                    usage_exit(file_util_progname, -1 ) ;
                }
                break;
            case 'e':
                if(stoptime != NULL)            /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                stoptime = optarg ;
                if (tc_validate_asf_datetime(stoptime) != TRUE)
                {
                    sprintf(file_util_msg, 
                        "Error in stoptime, %s\n", stoptime ) ;
                    aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                        DO_SYSLOG, DO_PRINT);
                    usage_exit(file_util_progname, -1 ) ;
                }
                break;
            case 'd':
                if( print_dtk_list )            /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                print_dtk_list = TRUE ;
                break;
            case 'o':
                if(filename != NULL)            /* Check for duplicate flags */
                    usage_exit(file_util_progname, 0);
                filename = optarg ;
                break;
            case '?':
                usage_exit(file_util_progname, 0);
                break;
            default:
                /* do  nothing  */
                break;
        }

    /* Check that all arguments are attached to flags */
    if(optind != argc )
        usage_exit(file_util_progname, 0);

    /* mandatory flags:  */
    if ( filetype == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Filetype not given\n", 
            DO_SYSLOG, DO_PRINT);
    if ( strttime  == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Strttime not given\n", 
            DO_SYSLOG, DO_PRINT);
    if ( stoptime  == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Stoptime not given\n", 
            DO_SYSLOG, DO_PRINT);
    if ( password  == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Password not given\n", 
            DO_SYSLOG, DO_PRINT);

    if ( filetype  == NULL
      || password  == NULL
      || strttime  == NULL
      || stoptime  == NULL )
        usage_exit(file_util_progname, 0) ;

    if ( filename == NULL )
        fp_output_file = stdout ;
    else
    {
        /* 
        -- filename was provided.
        --
        -- Check the filename.  If it is a fullpathname, keep it.
        -- If it is not a fullpathname, find out a destination directory for
        -- file and use it to precede the filename.
        --
        -- Also, create the metafilename at this time.  It is used to store the
        -- PMF information which accompanies the main file.
        */
        if ( strrchr(filename, slash) == NULL)
        {
            /*
            -- This is a filename, not a fullpathname.
            -- We need to determine outgoing_file, which is where
            -- any files that we create will be deposited.
            */
            if ( strcmp(filetype, "AWOS") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_WOS, filename);
            else if ( strcmp(filetype, "MWOS") == 0 )
                outgoing_file = aps_fullpath (APS_WFF_WOS, filename);
            else if ( strcmp(filetype, "AREQ") == 0 )
                outgoing_file = aps_fullpath (APS_WFF_AREQ, filename);
            else if ( strcmp(filetype, "ADDM") == 0 
                 ||   strcmp(filetype, "MDDM") == 0 )
                outgoing_file = aps_fullpath (APS_IMS_FILES, filename);
            else
            {
                /*
                -- There are no other choices. Only pre-approved filetypes
                -- can be handled by this process.
                */
                fprintf(stderr, "Error in file type value:  %s\n", 
                    filetype ) ;
                usage_exit(file_util_progname, -1);
            }
        }
        else
        {
            /*
            -- this is a fullpath file_name, take no action to shorten it.
            -- Send the PMF file to the same fullpath destination specified.
            */
            outgoing_file = filename ;
        }
        short_filename = aps_pathname2filename (outgoing_file);
        metafilename =   malloc(strlen(outgoing_file) +2 + 1) ;
        sprintf(metafilename,"%s.M", outgoing_file) ;
     
     
        if ( (fp_output_file = fopen(outgoing_file, "w")) == NULL )
        {
            sprintf(file_util_msg, 
                "Could not open file %s for writing.\n", outgoing_file ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            usage_exit(file_util_progname, -1 ) ;
        }
        create_PMF_file = TRUE ;
    }

    /* 
    -- very important; indicate file type and argument times globally:  
    */
    strcpy( fa_file_type, filetype ) ;
    strcpy( fa_argument_start_time, strttime ) ;
    strcpy( fa_argument_stop_time,  stoptime ) ;

    /*
    -- Determine the global variable fa_activity_type. 
    */
    if ( !table_lookupFA2APS(filetype2activity, filetype, fa_activity_type) )
    {
        aps_log_msg(file_util_progname, APS_ERROR,
            "Could not get the activity type for file.\n",
            DO_SYSLOG, DO_PRINT);

        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }

    /*
    -- initialize global variable fa_station_id.
    */
    if ( !table_lookupFA2APS(filetype2station, filetype, fa_station_id) )
    {
        aps_log_msg(file_util_progname, APS_ERROR,
            "Could not get the station id for file.\n",
            DO_SYSLOG, DO_PRINT);

        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }

    /* now open the database; used to get permissions ONLY.     */
    /* db_open will handle the errors.                          */

    /* 
    -- get the database name from the environment   
    */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
        aps_log_msg(file_util_progname, APS_ERROR,
            "dbname not found in environment variable APSDB\n",
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR,
            "Use setenv APSDB <dbname>. \n\n",
            DO_SYSLOG, DO_PRINT);

        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1);
    }
    dbname = env_dbname ;

    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            aps_log_msg(file_util_progname, APS_ERROR,
                "sybase_userid not given\n",
                DO_SYSLOG, DO_PRINT);
     
            if (fp_output_file != stdout)
                (void) unlink(outgoing_file) ;
            usage_exit(file_util_progname, -1);
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }

    APS_dbproc = db_open( dbname, file_util_progname, sybase_userid,
        password, NULL, error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        db_open_errs(return_code, dbname, sybase_userid);

        aps_log_msg(file_util_progname, APS_INFO,
            "DB could not be opened. Program terminated abnormally.\n",
            DO_SYSLOG, DO_PRINT);

        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }

    /*
    --
    -- Before accessing the database, get activity and planning permissions.
    --
       Here is the chain of events:
    -> 1) If activity permission not provided, request it.
          If activity permission is  provided, validate it.
       2) Get planning permission.
       3) ACCESS DATABASE. RETRIEVE DTKS. CREATE REPORT. UPDATE DTKS.
       4) Exit the process.  This will terminate every permission requested 
          in the process.  
    */
    return_code= mu_get_permission(file_util_progname, APS_dbproc,
                permission_id,              /*  may be populated, passed in */
                MU_SINGLE_ACTIVITY_TYPE,    /*  single, planning, dar....   */
                fa_activity_type,           /*  activity type               */
                NULL,                       /*  strttime                    */
                NULL,                       /*  stoptime                    */
                NULL,                       /*  station_id                  */
                NULL,                       /*  darid                       */
                0,                          /*  number of retries           */
                0 ) ;                       /*  number of seconds between   */
    if (return_code < 0)
    {
        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }
    else
        permission_id = return_code ;

    /*
    -- Item (2). Get planning permission.
    */
    return_code= mu_get_permission(file_util_progname, APS_dbproc,
                permission_id,              /*  is now populated            */
                MU_PLANNING_ACTIVITY_TYPE,  /*  single, planning, dar...    */
                fa_activity_type,           /*  activity type               */
                strttime,                   /*  starttime for report        */
                stoptime,                   /*  stoptime for report         */
                fa_station_id,              /*  station_id for J1           */
                NULL,                       /*  darid                       */
                0,                          /*  number of retries           */
                0 ) ;                       /*  number of seconds between   */
    if (return_code < 0)
    {
        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    } 

    /*
    -- Now retrieve the data-takes according to the file type:
    -- (We use the APS_READER account to query the database)
    */ 
    dtk_error_list = create_dyn_llist() ;
    dtk_list = create_dyn_llist() ;

    /*
    -- set up global dtk list pointer for use 
    -- in gen_rev2trackstart() and gen_rev2trackend() 
    */
    fa_dtk_report_list = dtk_list ;

    /* 
    -- fa_number_of_records is 
    -- used in the definitions for the 
    -- ODL_CREATE_VALUEDEFS *odl_filedef_ptr.  
    --
    -- get the dtks and also set the number of 
    -- records in the input_dtk_list:
    */
    fa_number_of_records = retrieve_ODL_dtks(filetype, 
        strttime, stoptime, dtk_list, dtk_error_list ) ;
    if ( fa_number_of_records < 0 )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Could not retrieve WOS dtks from db.\n", 
            DO_SYSLOG, DO_PRINT);
        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }
    sprintf(file_util_msg, 
        "Creation of %s file starting...\n", filetype ) ;
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);

    /*
    -- if there are any records in the dtk_error_list
    -- then print them to stdout.  
    -- these downlink dtks were supposed to have 
    -- observations for them, but they don't.  
    -- later, after the report is done, 
    -- exit with an error condition.  
    */
    if( NUMELTS( dtk_error_list ) != 0 )
    {
        /* error.  */
        sprintf( file_util_msg, 
"%d ODL file downlinks retrieved, but %d other downlinks were excluded due to no data-takes (sensing activities) for them.\n", 
            fa_number_of_records, NUMELTS( dtk_error_list ) ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        (void) fprintf( stderr, "%d Downlinks with no observations:\n",
            NUMELTS( dtk_error_list ) );
        dtkm_print_list( stderr, dtk_error_list ) ;
    }
    /*
    -- Now create the ODL file.  
    */
    /* 
    -- first set the creation time, 
    -- written more than once, but 
    -- all occurrances must be the same 
    -- time:  
    */
    tc_systime2asf(fa_creation_date) ;

    /*
    -- determine the first track_start, last track_end of the list.
    -- fa_first_track_start, fa_last_track_end are globals defined in
    -- #include "GENconversions.h"  
    -- These times will be in ASF format.  If needed in ODL format,
    -- use conversion routine gen_asftime2odl in the ODL file definition,
    -- wherever these times need to be used.
    -- these are defined as the TRACK_START of the first dtk in the list and 
    -- the TRACK_END of the last data-take in the list.  
    */
    if( fa_number_of_records == 0 )
    {
        /* 
        -- no records retrieved.  
        -- use values in strttime and stoptime  
        -- for the track times:
        */
        strcpy( fa_first_track_start, strttime ) ;
        strcpy( fa_last_track_end,  stoptime ) ;

        /* 
        -- set up the first data-take pointer.  
        */
        first_dtk_rec = NULL ;
    }
    else
    {
        /* compute the ASF times:  */
        if (!dtkm_get_firstlast_track( dtk_list,
            fa_first_track_start, fa_last_track_end) )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
                "Error getting first and last track times\n", 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_INFO, 
                "Program terminated abnormally.\n", DO_SYSLOG, DO_PRINT);
            exit (APS_EXIT_ERROR);
        }
        /* 
        -- set up the first data-take pointer.  
        */
        first_dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
    }

    /*
    -- set up global file pointer for use in odl_dmap_valuedefs[]
    */
    fa_output_file_fp = fp_output_file ;

    /*
    -- WRITE THE HEADER 
    */

    /* write the header record:   */
    return_code = odl_dtkfile_create_record( ODL_FILE_HEADER, odl_filedef_ptr,
        first_dtk_rec, fp_output_file ) ;
    if ( return_code < 0 )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            ODL_ERROR_MESSAGE( return_code ), DO_SYSLOG, DO_PRINT);
        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }
    /*
    -- HEADER IS COMPLETE
    */

    /*
    -- WRITE THE FILE
    */
    return_code = odl_dtkfile_creation( dtk_list, odl_filedef_ptr, 
        fp_output_file) ;
    if ( return_code < 0 )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            ODL_ERROR_MESSAGE( return_code ), DO_SYSLOG, DO_PRINT);

        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }
    /*
    -- THE FILE IS COMPLETE
    */

    /*
    -- WRITE THE TRAILER 
    */
    /* 
    -- use the first record from the input list, obtained above, 
    -- to provide values if needed:  
    */
    /* write the trailer record:   */
    return_code = odl_dtkfile_create_record( ODL_FILE_TRAILER, odl_filedef_ptr,
        first_dtk_rec, fp_output_file ) ;
    if ( return_code < 0 )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            ODL_ERROR_MESSAGE( return_code ), DO_SYSLOG, DO_PRINT);
        if (fp_output_file != stdout)
            (void) unlink(outgoing_file) ;
        usage_exit(file_util_progname, -1 ) ;
    }
    /*
    -- TRAILER IS COMPLETE
    */

    if ( strcmp(filetype, "ADDM") == 0 
    ||   strcmp(filetype, "MDDM") == 0 )
    {
        /* 
        -- just in case, check this.  
        -- for the downlink-to-data-take mapping, no 
        -- extra PMF file. 
        */
        create_PMF_file = FALSE ;
    }

    /*
    -- Now, select the correct PMF file and create it.
    -- ONLY if its corresponding data file was also created.
    */
    if (create_PMF_file)
    {
       /*
        -- START setting up the generation of PMF for this response file
        -- by finding the descriptor for the PMF file
        */
        if(!identify_PMF_file(filetype, PMF_files, &pmf_descriptors ))
        {
            sprintf(file_util_msg, 
                "COULD NOT MATCH PMF %s TO A FLIGHT AGENCY.\n",filetype);
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            /* 
            -- PMF creation not possible, but keep the data file.
            -- No unlink action at this time.
            */
            usage_exit(file_util_progname, -1 ) ;
        }
        PMF_struct = pmf_descriptors->file_descriptor ;

        /*
        -- populate any fields in the PMF_struct descriptor that
        -- are not populated by the APS_create_pmf function
        */
        strncpy(PMF_struct[FILE_NAME].field, short_filename,
        strlen(short_filename) );


        /*
        -- Populate global variables
        -- 1. fa_argument_start_time and fa_argument_stop_time
        -- 2. fa_file_start_time fa_file_stop_time
        -- 3. fa_first_track_start fa_last_track_end (already done)
        -- which will be used by the PMF creator
        -- NOTE: not all of these variables are used by all WOS files.
        -- Each WOS file has a corresponding set of instructions that
        -- will determine which variables are used.
        */
        min_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
        max_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

        if( fa_number_of_records == 0 )
        {
            /* 
            -- no records retrieved by the query.  
            -- instead, use values in retrieve strttime and stoptime  
            -- for the fa_file_start_time and fa_file_stop_time
            -- also for the fa_argument_start_time and fa_argument_stop_time
            */
            strcpy(fa_file_start_time, strttime ) ;
            strcpy(fa_file_stop_time,  stoptime ) ;
            strcpy(fa_argument_start_time, strttime) ;
            strcpy(fa_argument_stop_time, stoptime) ;
        }
        else
        {
            if (db_get_min_record(dtk_list, APS_CDEFS(DTK), DTK_STRTTIME,
                    &min_dtk_rec) != DB_MIN_RECORD_OK
            ||  db_get_max_record(dtk_list, APS_CDEFS(DTK), DTK_STOPTIME,
                    &max_dtk_rec) != DB_MAX_RECORD_OK)
            {
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "COULD NOT FIND START TIME OR STOP TIME FOR FILE\n", 
                    DO_SYSLOG, DO_PRINT);

                /* 
                -- PMF creation not possible, but keep the data file.
                -- No unlink action at this time.
                */
                usage_exit(file_util_progname, -1 ) ;
            }
            strcpy(fa_file_start_time, 
                CAST_DTK_STRTTIME min_dtk_rec[DTK_STRTTIME]);
            strcpy(fa_file_stop_time,  
                CAST_DTK_STOPTIME max_dtk_rec[DTK_STOPTIME]);
            strcpy(fa_argument_start_time, strttime) ;
            strcpy(fa_argument_stop_time, stoptime) ;
     
            free_db_record (min_dtk_rec) ;
            free_db_record (max_dtk_rec) ;
        }
     
        /*
        -- Global variables fa_file_start_time and fa_file_stop_time need to
        -- be converted to ODL format
        --
        -- Also convert the first_track_start and last_track_end times.  
        -- These are in ASF format, and they need to be converted to 
        -- ODL fmt before being used.
        */
        if ( !tc_asf2odl (fa_file_start_time,   fa_file_start_time)
        ||   !tc_asf2odl (fa_file_stop_time,    fa_file_stop_time)
        ||   !tc_asf2odl (fa_argument_start_time,   fa_argument_start_time)
        ||   !tc_asf2odl (fa_argument_stop_time,    fa_argument_stop_time)
        ||   !tc_asf2odl (fa_first_track_start, fa_first_track_start) 
        ||   !tc_asf2odl (fa_last_track_end,    fa_last_track_end)      )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
                "Error converting file start/stop/track times to ODL format", 
                DO_SYSLOG, DO_PRINT);

            /* 
            -- PMF creation not possible, but keep the data file.
            -- No unlink action at this time.
            */
            usage_exit(file_util_progname, -1 ) ;
        }
     
        /*
        -- FINALLY, create the PMF file
        */
        if (! APS_create_pmf(metafilename, PMF_struct) )
        {
            sprintf(file_util_msg, 
                "Error creating IMS PMF file for File Type: %s '%s'\n",
                filetype, metafilename) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            /* 
            -- PMF creation not possible, but keep the data file.
            -- No unlink action at this time.
            */
            usage_exit(file_util_progname, -1 ) ;
        }
    }

    /* 
    -- Now optionally print out the dtk list:
    -- NOTE that the dtk list is NOT placed in the same directory as the
    --      outgoing file or outgoing PMF file.  The dtk list is not a file
    --      which we want to archive in the IMS system, so we do not place
    --      it in an area where it may be mistakently transferred.
    --      The dtk list is placed in the current directory.
    */
    if ( print_dtk_list )
    {
        if ( fp_output_file != stdout )
        {
            /* let's reuse the FILE pointer fp_output_file */
            fclose(fp_output_file) ;

            temp_filename = aps_fullpath (APS_REPORTS, short_filename);
            strcpy(filename_DTK, temp_filename) ;
            strcat(filename_DTK, "_DTK") ;
            if ( (fp_output_file = fopen(filename_DTK, "w")) == NULL )
            {
                sprintf(file_util_msg, 
                    "Could not open file %s for writing.\n", filename_DTK ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);

                usage_exit(file_util_progname, -1 ) ;
            }
            free (temp_filename) ;
        }
        else
        {
            fprintf(fp_output_file, "\n" ) ;
        }

        fflush(fp_output_file) ;
        fprintf(fp_output_file, "LIST OF DATATAKES IN %s FILE\n", filetype ) ;
        fprintf(fp_output_file, "-----------------------------\n" ) ;
        if (outgoing_file!= NULL)
            fprintf(fp_output_file, "FILENAME     :  %s\n", outgoing_file ) ;

        fprintf(fp_output_file, "CREATION TIME:  %s\n", fa_creation_date ) ;
        fprintf(fp_output_file, "STATION ID   :  %s\n\n", fa_station_id ) ;
        dtkm_print_list(fp_output_file, dtk_list ) ;
    }

    sprintf(file_util_msg, 
        "%s file created OK with %d records", filetype, fa_number_of_records);
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);

    if ( filename != NULL )
    {
        sprintf(file_util_msg, 
            " and saved to %s", outgoing_file ) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
    }
    if ( strlen( filename_DTK ) != 0 )
    {
        sprintf(file_util_msg, 
            " with dtk list in %s", filename_DTK ) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
    }

    /*
    -- free any allocated memory used.
    */
    if ( fp_output_file != stdout )
    {
        free(metafilename) ;
        free (short_filename) ;
    }

    if( NUMELTS( dtk_error_list ) != 0 )
    {
        /* 
        -- this list is for ADDM and MDDM errors.  
        -- earlier, this list was printed out.  
        -- now, after the report is done, we 
        -- exit abnormally.  
        */
        sprintf( file_util_msg, "HOWEVER, %d Downlinks with no observations",
            NUMELTS( dtk_error_list ) ) ;
        aps_log_msg( file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        (void) fprintf( stderr, "%s\n", file_util_msg ) ;
        DEL_LIST( dtk_error_list ) ;
        usage_exit(file_util_progname, -1 ) ;
    }

    aps_log_msg(file_util_progname, APS_INFO, 
        "Program completed successfully.\n", 
        DO_SYSLOG, DO_PRINT);
    fclose (fp_output_file) ;
    exit (APS_EXIT_OK) ;

} /* end of main()  */

