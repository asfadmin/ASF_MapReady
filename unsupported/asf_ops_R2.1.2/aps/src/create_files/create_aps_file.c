#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif
/*============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================
*/
#pragma ident   "@(#)create_aps_file.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.create_aps_file.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <mu_utilities.h>       /* for the permissions process */

#include "db_sybint.h" 

#include "apsfiledef.h"
#include "apspath.h"            /* for the aps_pathname2filename prototype  */
#include "aps_log_msg.h"        /* for syslogging                           */
#include "aps_defs.h"           /* exit codes for syslogging                */

#include "APSpmfutilities.h"    /* for the PMS_files[] definitions          */
#include "GENconversions.h"     /* for flight agency global variables       */
#include "file_utilities.h"     /* for identify_PMF, syslogging             */

DBPROCESS       *APS_dbproc ;
char        *userid ;
char        *password ;
char        *file_util_progname ; /* required by libfileutils.a */
char        file_util_msg [MSG_LEN];

void usage_exit(char *) ;
void error_exit(char *) ;

static int  opt ;
static char *optlist = "t:b:e:o:p:U:P:" ;

extern REPORT reports[] ;



/*==============================================================================
Function:       usage_exit

Description:    

Parameters:     

Returns:        none

Creator:        Ron Green

Creation Date:  Fri May 26 10:40:52 PDT 1995

Notes:      
==============================================================================*/
void usage_exit(char *progname)
{
    int i ;


    fprintf (   stderr, /*usage*/   
"\nUsage:\t%s -t <file type> -o <file name> [-p <permission>]\n", progname) ;
    fprintf (   stderr, /*usage*/   
"\t\t -b <asf time> -e <asf time> -U <userid> -P <password>\n");
    fprintf (   stderr, /*usage*/   
"where....\n") ;
    fprintf (   stderr, /*usage*/   
"\t-t <file-type> can be:\n") ;

    i = 0 ;
    while (reports[i].type )
    {
        if (reports[i].func)
            fprintf (   stderr, /*usage*/   
"\n\t   %4s - %s", reports[i].type, reports[i].name);
        i++ ;
    }
    fprintf (   stderr, "\n\n") ; /*usage*/ 
    fprintf (   stderr, /*usage*/   
"\t-o <file name> is the name of the created output file\n") ;
    fprintf (   stderr, /*usage*/   
"\t   <file name> may not exceed %d characters\n", APS_MAX_FILENAME_LENGTH) ;
    fprintf (   stderr, /*usage*/   
"\t-b <asf time>  start time of file, in asf format YYYY:DDD:HH:MM:SS:CCC\n") ;
    fprintf (   stderr, /*usage*/   
"\t-e <asf time>  end time of file,   in asf format YYYY:DDD:HH:MM:SS:CCC\n") ;
    fprintf (   stderr, /*usage*/   
"\t-p <permission> single-activity permission (from the calling program)\n") ;
    fprintf (   stderr, /*usage*/   
"\t-U <userid>     Sybase userid of Sybase account\n") ;
    fprintf (   stderr, /*usage*/   
"\t-p <password>   Sybase password for Sybase account\n") ;
    fprintf (   stderr,  "\n") ; /*usage*/  
    fprintf (   stderr, /*usage*/   
"\tEnvironment variable APSDB must be set to the appropriate database name\n") ;
    fprintf (   stderr,  "\n") ; /*usage*/  

    aps_log_msg(progname, APS_INFO, 
        "Program terminated abnormally.\n",
        DO_SYSLOG, DO_PRINT) ;
    exit(APS_EXIT_ERROR) ;
}

void error_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO, 
        "Program terminated abnormally.\n",
        DO_SYSLOG, DO_PRINT) ;
    exit(APS_EXIT_ERROR) ;
}

/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  Fri May 26 10:41:15 PDT 1995

Notes:      
==============================================================================*/
void 
main(int argc, char *argv[])
{
    extern int  optind ;
    extern char *optarg ;

    char *report_id    = NULL ;
    char *report_start = NULL ;
    char *report_stop  = NULL ;
    char *filename     = NULL ;
    char *metafilename = NULL ;
    char *dbname       = NULL ;
    char *short_filename = NULL ;
    char *outgoing_file  = NULL ;

    int i, j ;
    int report_index ;
    int status ;
    int     pflag = 0 ;         /* signals terminate permission     */
    int     return_code;        /* for getting permission           */
    int     permission_id = 0;  /* for getting permission           */

    PMF_FILENAME    *pmf_descriptors ;
    APSPMF_metadata *PMF_struct ;
       
    int slash = '/';

EQUIV_TABLE id2activity[]=
{   {"CRAR",         MU_CRAR},
    {"MRAR",         MU_CRAR},
    {"REUG",         MU_REUG},
    {"MSGF",         MU_MSGF},
    {NULL, NULL}
};

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    userid = NULL ;
    password = NULL ;

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

    while((opt = getopt(argc, argv, optlist)) != EOF)
    {
        switch(opt)
        {
        case 't' :
            report_id = (char *) strdup(optarg) ;

            i = 0 ;
            while (reports[i].type != NULL)
            {

                if (strncmp(reports[i].type, report_id,
                        strlen(reports[i].type)) == 0)
                    break ;
                i++ ;
            }

            if (!reports[i].type)   /* incorrect type */
            {
                sprintf (file_util_msg, 
                    "Invalid file type '%s'\n", report_id) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                usage_exit(file_util_progname) ;
            }

            if (!reports[i].func)   /* no create function for type */
            {
                sprintf (file_util_msg, 
                    "No APS creation function available for:%s  %s\n", 
                    report_id, reports[i].name) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "The file is created elswhere in the system\n",
                    DO_SYSLOG, DO_PRINT);
                error_exit(file_util_progname) ;
            }

            /* save the index of the report */
            report_index = i ;

            /* get activity_id from the report_id */
            if ( !table_lookupFA2APS(id2activity, report_id, fa_activity_type) )
            {
                sprintf (file_util_msg, 
                    "Could not get the activity type for file %s\n", report_id);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                error_exit(file_util_progname) ;
            }

			/* save report_id globally, for use by reports[i].func */
			strcpy (fa_file_type, report_id) ;

            break ;

        case 'b' :
            report_start = (char *) strdup(optarg) ;
            break ;

        case 'e' :
            report_stop = (char *) strdup(optarg) ;
            break ;

        case 'o' :
            filename = (char *) strdup(optarg) ;
            break ;

        case 'p':
            if (pflag != 0)
                usage_exit(file_util_progname);
            pflag++ ;
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(file_util_progname);
            }
            break ;

        case 'U' :
            userid = (char *) strdup(optarg) ;
            break ;

        case 'P' :
            password = (char *) strdup(optarg) ;
                        break ;

        case '?':
            usage_exit(file_util_progname);
            break;
    
        default :
            usage_exit(file_util_progname) ;
            break ;
        }
    }
    /*
    -- 
    -- The following fragment was commented out;
    -- its origin and purpose, still, a total mystery.
        argv = argv + optind ; 
        argc = argc - optind ;
    */

    /* 
    -- check that all arguments are attached to flags 
	*/
    if(optind != argc )
        usage_exit(file_util_progname);


    if (!report_id)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No File Type specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }

    if (!report_start)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No Start Time specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }

    if (!report_stop)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No Stop Time specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }

    if (!filename)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No output filename specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }
    else 
    {
        /* filename was provided. 
        --
        -- Check the filename.  If it is a fullpathname, keet it.
        -- If it is not a fullpathname, find out destination directory for file
        -- and use it to precede the filename.
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
            if (reports[report_index].aps_fullpath_id == APS_NASDA_FILES)
                outgoing_file = aps_fullpath (APS_NASDA_FILES, filename);
            else if (reports[report_index].aps_fullpath_id == APS_ESA_FILES)
                outgoing_file = aps_fullpath (APS_ESA_FILES, filename);
            else if (reports[report_index].aps_fullpath_id == APS_RADARSAT_RAR)
                outgoing_file = aps_fullpath (APS_RADARSAT_RAR, filename);
            else
                outgoing_file = filename ;
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


        /*
        -- NOTE: we no longer generate the old-format metadata file.
        -- There is no need to check the length of filename.
        */
#ifdef MIGUEL_COMMENT_OUT
        /*
        -- verify the filename (excluding the path) doesn't exceed 14 chars 
        -- this limitation is due to the ACS meta message sent with the file
        */
#endif
    }


    if (!userid)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No Sybase User ID specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }

    if (!password)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "No Sybase Password Specified\n",
            DO_SYSLOG, DO_PRINT);
        usage_exit(file_util_progname) ;
    }

    dbname = getenv("APSDB") ;

    /* logon to the APS database */
    APS_dbproc = db_open(
        /* db name   */ dbname,
        /* prog name */ file_util_progname,
        /* db user   */ userid,
        /* db passwd */ password,
        /* msg handl */ NULL,
        /* err handl */ NULL,
        /* db status */ &status) ;

    if (status != DB_OPEN_OK)
    {
        db_open_errs(status, dbname, userid) ;
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Could not open database.\n",
            DO_SYSLOG, DO_PRINT) ;
        error_exit(file_util_progname) ;
    }

    /*
    --
    -- Before accessing the database, get activity and planning permissions.
    --
       Here is the chain of events:
    -> 1) If activity permission not provided, request it.
          If activity permission is  provided, validate it.
       2) ACCESS DATABASE. RETRIEVE DTKS. CREATE REPORT. UPDATE DTKS.
       3) If we requested activity permission ourselves, terminate it.
 
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
        error_exit(file_util_progname) ;
    else
        permission_id = return_code ;
 

 
    /* execute the report pointed to by report_index */
    status = (int)(*(reports[report_index].func))
                    (outgoing_file, report_start, report_stop) ;



    if (status != APS_REPORT_OK 
    &&  status != APS_REPORT_NONE
    &&  status != APS_REPORT_REPLY_FILE)
    {
        sprintf (file_util_msg, 
            "Error creating APS File Type: %s '%s'\n", 
            reports[report_index].type, outgoing_file) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        error_exit(file_util_progname) ;
    }

    if (status == APS_REPORT_NONE)
    {
        error_exit(file_util_progname) ;
    }

    /*
    -- Populate flight-agency-dependent global variables
    -- (ie: fa_sat_id)
    */
    if (reports[report_index].aps_fullpath_id == APS_NASDA_FILES)
        strncpy (fa_sat_id, J_SAT, strlen(fa_sat_id) );
    else if (reports[report_index].aps_fullpath_id == APS_ESA_FILES)
        strncpy (fa_sat_id, E_SAT, strlen(fa_sat_id) );
    else if (reports[report_index].aps_fullpath_id == APS_RADARSAT_RAR)
        strncpy (fa_sat_id, R_SAT, strlen(fa_sat_id) );


    /*
    -- Find the PMF_struct definition for the file in question 
    -- Then, create an ODL-type PMF to be sent with the file
    */
    if(!identify_PMF_file(reports[report_index].type, 
                                    PMF_files, &pmf_descriptors ) )
    {
        sprintf (file_util_msg, 
            "COULD NOT MATCH PMF %s TO A FLIGHT AGENCY.\n",
            reports[report_index].type) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        free (metafilename) ;
        free (short_filename) ;
        error_exit(file_util_progname) ;
    }
    else
    {
        if( strcmp(reports[report_index].type, CRAR_TYPE) == 0 
		||  strcmp(reports[report_index].type, MRAR_TYPE) == 0 )
        {
            /*
            -- This is a CRAR/MRAR file. Do not create a PMF at this point.
            -- CRAR/MRAR could actually be comprised of several files, so
            -- we create a PMF for each of those files when we
            -- execute the CRAR/MRARreport.
            */
            outgoing_file[strlen(outgoing_file) 
                            - strlen( strrchr(outgoing_file,'/') )] = '\0' ;
            sprintf (file_util_msg, 
            "Program completed successfully, with Availability file(s) in %s ", 
                outgoing_file);
            aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            fprintf(stdout, "directory.\n") ;
        }
        else /* Handle all non-CRAR/MRAR files */
        {
            PMF_struct = pmf_descriptors->file_descriptor ;
            /*
            -- Populate some PMF_struct fields here 
            */
            strncpy(PMF_struct[FILE_NAME].field, short_filename, 
                strlen(short_filename) );

            if( !(tc_asf2odl(report_start,  fa_file_start_time) 
                &&  tc_asf2odl(report_stop, fa_file_stop_time)
                &&  APS_create_pmf( metafilename, PMF_struct) )     )
            {
                sprintf (file_util_msg, 
                    "Error creating IMS PMF file for File Type: %s '%s'\n",
                    reports[report_index].type, outgoing_file) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
                error_exit(file_util_progname) ;
            }

            sprintf (file_util_msg, 
                "Program completed successfully, with %s in %s\n",
                reports[report_index].type, outgoing_file);
            aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
        }

        if (!pflag)
        {
            /*
            -- We acquired the single activity permission id ourselves.
            -- Now, we must terminate the single activity permission.
            -- If mu_permission_terminate() fails, it is non-fatal.
            */
            return_code = mu_permission_terminate( APS_dbproc,
                permission_id,
                fa_activity_type,               /*  mu_activity_id  */
                MU_SINGLE_ACTIVITY_TYPE) ;      /*  activity_types  */
        
            if( return_code < 0 )
            {
                fprintf(stderr,"%s(%d):  permission terminate error code %d\n",
                    __FILE__, __LINE__, return_code ) ;
                fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
            }
        }

        free (metafilename) ;
        free (short_filename) ;
        exit(APS_EXIT_OK) ;
    }



#ifdef MIGUEL_COMMENT_OUT
        /*
        -- THIS CODE IS NOW OBSOLETE.
        -- Create the 'old' format PMF file.
        -- Create a APS Metadata Message file to send with the datafile.
        */
        status = create_aps_meta_file(
            &(reports[report_index]), filename, report_start, report_stop) ;
        if (!status)
        {
        printf("Error creating APS Metadata MSG File\n") ;
        printf("File Type %s '%s' will not be able to be transferred...\n",
            reports[report_index].type, filename) ;
        error_exit(file_util_progname) ;
        }
#endif
}
