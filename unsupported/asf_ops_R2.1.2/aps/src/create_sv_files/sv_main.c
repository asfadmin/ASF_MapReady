#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       sv_main.c

Description:    This is the driver for the state vector query function

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)sv_main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_sv_files/SCCS/s.sv_main.c"

#include <stdio.h>
#include <mu_utilities.h>       /* for permissions                  */
#include "create_sv_file.h"
#include "GENconversions.h"     /* for global variables definitions */
#include "APSpmfutilities.h"    /* for PMF_files definitions        */
#include "apspath.h"            /* for default directory definitions*/
#include "apsfiledef.h"         /* for file creation return code    */
#include "aps_log_msg.h"

char *file_util_progname = NULL;
char file_util_msg[MSG_LEN];

/* External functions */

extern IMS_SV_STRUCT * sv_query(COMMANDS *);
extern int sv_create_odl(COMMANDS *, IMS_SV_STRUCT *);

/* Local Function */

void usage_exit(char*);
void error_exit(char*);
int  getPlatformAndDestFromType(char *pdid , char *dest , char *platform);

/* 
*************************************************************
Function:       main()
 
Description:    Main routine to create the Predicted state vector file
  
Parameters:
   
Returns:        - APS_EXIT_OK on success
                - APS_EXIT_ERROR on failure

**************************************************************** 
*/

void main (int argc, char *argv[])
{

    extern int  optind ;
    extern char *optarg ;

    int j,opt, opt_flag;
    int sv_create_odl_return ;
    int     status ;            /* for db_open                      */
    int     pflag = 0 ;         /* signals assignment of -p flag    */
    int     tflag = 0 ;         /* signals assignment of -t flag    */
    int     return_code;        /* for getting permission           */
    int     permission_id = 0;  /* for getting permission           */
    char *optlist = "hb:e:t:o:p:U:P:" ;
    char *username = NULL;
    char *password = NULL;
    char *dbname = NULL ;


    char *sv_ret_file = NULL;
    char *startTime = NULL;
    char *endTime = NULL;
    char *precision= "P";
    char dest[4];
    char platform[3];
    char pdid[] = "dple";  /*   Indicate the type of SV file.  
                        First character for destination, e.g., A,M; 
                        2nd and 3rd for platform, e.g., E1,E2,J1,A1,R1; 
                        4th character, E for Ephemeris. */
    char *progname = NULL;
    static char curr_time[SV_TIME_LENGTH + 1];
    static char odl_creation_time[SV_TIME_LENGTH + 1];

    DBPROCESS       *APS_dbproc ;
    COMMANDS        *commands;
    PMF_FILENAME        *pmf_descriptors ;
    APSPMF_metadata     *PMF_struct ;

    FILE                *SV_file_ptr ;
    char *metafilename = NULL ;
    char *outgoing_file = NULL ;
    char msg[MSG_LEN];

EQUIV_TABLE type2activity[]=
{   {"AE1E",         MU_AE1E},
    {"AE2E",         MU_AE2E},
    {"AJ1E",         MU_AJ1E},
    {"AA1E",         MU_AA1E},
    {"AR1E",         MU_AR1E},
    {"ME1E",         MU_ME1E},
    {"ME2E",         MU_ME2E},
    {"MR1E",         MU_MR1E},
    {NULL, NULL}
};

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    file_util_progname = strrchr( argv[0], '/' ) ;
    if (file_util_progname == NULL)
        file_util_progname = argv[0] ;
    else
        file_util_progname++ ;
    progname            = file_util_progname ;
    aps_open_syslog();

    sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(msg, " " ) ;
        strcat(msg, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

    opt_flag = 0;

    while((opt = getopt(argc, argv, optlist)) != EOF)
    {
        opt_flag = 1;
        switch(opt)
        {
        case 'h' :
            usage_exit (progname) ;
            break ;

        case 'b' :
            if( startTime != NULL)  /* Check for duplicate flags */
                usage_exit (progname) ;
            startTime = (char *) strdup(optarg);
            if( tc_validate_asf_datetime( startTime ) < 0 )
            {
                sprintf(msg, "Input error in start time '%s'.", startTime);
                aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
                usage_exit(progname) ;
            }
            break ;

        case 'e' :
            if( endTime != NULL)    /* Check for duplicate flags */
                usage_exit(progname) ;
            endTime = (char *) strdup(optarg);
            if( tc_validate_asf_datetime( endTime ) < 0 )
            {
                sprintf(msg, "Input error in end time '%s'.", endTime);
                aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
                usage_exit(progname) ;
            }
            break ;

        case 't' :
            if (tflag != 0)         /* Check for duplicate flags */
                usage_exit(progname);
            tflag++ ;
            strcpy( pdid, (char *) strdup(optarg));
            if( !getPlatformAndDestFromType( pdid , dest , platform ) )
            {
                sprintf(msg, "Input error in type '%s'.", pdid);
                aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
                usage_exit(progname) ;
            }

            /* get activity_id from the report type */
            if ( !table_lookupFA2APS(type2activity, pdid, fa_activity_type) )
            {
                sprintf (msg,
                    "Could not get the activity type for file %s\n", pdid);
                aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
                usage_exit(progname) ;
            }

            break ;

        case 'o' :
            if( sv_ret_file != NULL)    /* Check for duplicate flags */
                usage_exit(progname) ;
            sv_ret_file = (char *) strdup(optarg);
            break ;

        case 'P' :
            if( password != NULL)       /* Check for duplicate flags */
                usage_exit(progname) ;
            password = (char *) strdup(optarg);
            break ;

        case 'U' :
            if( username != NULL)       /* Check for duplicate flags */
                usage_exit(progname) ;
            username = (char *) strdup(optarg);
            break ;

        case 'p':
            if (pflag != 0)             /* Check for duplicate flags */
                usage_exit(progname);
            pflag++ ;
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(progname);
            }
            break ;

        case '?':
            usage_exit(progname);
            break;

        default :
            sprintf(msg, "Invalid option input on command line.");
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            usage_exit(progname) ;
            break ;
        }
     }

    /* 
    -- Check that there are no extraneous arguments:
    */
    if(optind != argc )
        usage_exit(progname);


     if (!opt_flag ||  !tflag )
     {
        sprintf(msg, "Missing input parameters.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if (!startTime)
     {
        sprintf(msg, "Missing start time parameter.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if (!endTime)
     {
        sprintf(msg, "Missing end time parameter.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if (!sv_ret_file)
     {
        sprintf(msg, "Missing output file name parameter.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if (strcmp(pdid,"dple") == 0)
     {
        sprintf(msg, "Missing output type parameter.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if (!username)
     {
        if ((username = (char *) getenv("APS_SYBASE_USERID")) == NULL)
        {
            sprintf(msg, "Missing Sybase username parameter.");
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            usage_exit(progname) ;
        }
     }

     if (!password)
     {
        sprintf(msg, "Missing Sybase login password parameter.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }
                                
     /*
     -- Get current time in odl format
     */
     tc_systime2asf(curr_time);
     tc_asf2odl(curr_time,odl_creation_time);

     /*
     -- Setup arguments to the routine which creates the state vector file
     */
     commands = (COMMANDS *) malloc( sizeof( COMMANDS ) );
     strcpy(commands->odl_creation_time, odl_creation_time);
     strcpy(commands->username, username);
     strcpy(commands->password, password);
     strcpy(commands->progname, progname);
     strcpy(commands->startTime, startTime);
     strcpy(commands->endTime, endTime);
     strcpy(commands->precision, precision);
     strcpy(commands->platform, platform);
     strcpy(commands->dest, dest);

     if ( (char *) getenv("IMS_SERVER") != NULL)
        strcpy(commands->server, (char *) getenv("IMS_SERVER"));
     else
     {
        sprintf(msg, "IMS_SERVER environment variable not set.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

     if ( (char *) getenv("IMS_DB") != NULL)
        strcpy(commands->database, (char *) getenv("IMS_DB"));
     else
     {
        sprintf(msg, "IMS_DB environment variable not set.");
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
     }

    /* 
    -- get APS database name from the environment   
    */
    dbname = getenv("APSDB");
    if(dbname == NULL)
    {
        /* database name not supplied   */
        aps_log_msg(file_util_progname, APS_ERROR,
            "dbname not found in environment variable APSDB\n",
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR,
            "Use setenv APSDB <dbname>. \n",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname);
    }

    /* logon to the APS database */
    APS_dbproc = db_open(
        /* db name   */ dbname,
        /* prog name */ progname,
        /* db user   */ username,
        /* db passwd */ password,
        /* msg handl */ NULL,
        /* err handl */ NULL,
        /* db status */ &status) ;
 
    if (status != DB_OPEN_OK)
    {
        db_open_errs(status, dbname, username) ;
        aps_log_msg(progname, APS_ERROR,
            "Could not open database.\n",
            DO_SYSLOG, DO_PRINT) ;
        error_exit(progname) ;
    }

     /*
     -- We need to determine outgoing_file, which is where
     -- any files that we create will be deposited.
     --
     -- NOTE: When the new filetypes come into use, the following
     --       case logic may be simplified.
     sprintf(sv_ret_file, "SV_%s", odl_creation_time);
     */

     if (sv_ret_file[0] == '/')
     {
        outgoing_file = sv_ret_file ;
        sv_ret_file = aps_pathname2filename (sv_ret_file);
     }
     else
     {
        if ( strcmp(dest , "HC") ==0 )
        {
            if (strcmp(platform ,"E1") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_E1SV, sv_ret_file);
            else if (strcmp(platform ,"E2") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_E2SV, sv_ret_file);
            else if (strcmp(platform ,"J1") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_J1SV, sv_ret_file);
            else if (strcmp(platform ,"A1") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_A1SV, sv_ret_file);
            else if (strcmp(platform ,"R1") == 0 )
                outgoing_file = aps_fullpath (APS_ASF_R1SV, sv_ret_file);
        }
        else if ( strcmp(dest , "WFF") ==0 )
        {
            if (strcmp(platform ,"E1") == 0 )
                outgoing_file = aps_fullpath (APS_WFF_E1SV, sv_ret_file);
            else if (strcmp(platform ,"E2") == 0 )
                outgoing_file = aps_fullpath (APS_WFF_E2SV, sv_ret_file);
            else if (strcmp(platform ,"R1") == 0 )
                outgoing_file = aps_fullpath (APS_WFF_R1SV, sv_ret_file);
        }
     }

     /*
     -- Open up the state vector file
     */
     if ((SV_file_ptr = fopen (outgoing_file, "w")) == (FILE *)NULL)
     {
        sprintf(msg, "Unable to open output SV file '%s'.", outgoing_file);
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
     }
     else
        commands->SV_file_ptr = SV_file_ptr;

    /*
    --
    -- Before accessing the database, get activity and planning permissions.
    --
       Here is the chain of events:
    -> 1) If activity permission not provided, request it.
          If activity permission is  provided, validate it.
       2) ACCESS IMS DATABASE. RETRIEVE SV RECORDS. CREATE SV_FILE.
       3) If we requested activity permission ourselves, terminate it.
 
    */
    return_code= mu_get_permission(progname, APS_dbproc,
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
        error_exit(progname) ;
    else
        permission_id = return_code ;


     /*
     -- Create the state vector file 
     */
     sv_create_odl_return = 
                sv_create_odl(commands, sv_query(commands) ) ;
     fclose(SV_file_ptr);

     if (!sv_create_odl_return)
     {
        unlink (outgoing_file) ;
        sprintf(msg, "Failed to create state vector file '%s'", outgoing_file);
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT); 
        error_exit(progname) ;
     }
     else 
     {
        sprintf(msg, "state vector file '%s' created.", outgoing_file);
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT); 

        /* create PMF file for sv_ret_file */

        /*
        -- Currently, use destination (HC/WFF) to determine the PMF descriptor
        -- to use. 
        -- In the future, there will be several file types available,
        -- but please note that they will ALL be mapped to the very same
        -- PMF descriptors that we are currently using.
        */

        if(!identify_PMF_file(dest, PMF_files, &pmf_descriptors ))
        {
            sprintf(msg, "Could not match PMF '%s' to a flight agency.", dest);
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT); 
            error_exit(progname) ;
        }
        PMF_struct = pmf_descriptors->file_descriptor ;


        /*
        -- populate any fields in the PMF_struct descriptor that
        -- are not populated by the APS_create_pmf function
        */
        strncpy(PMF_struct[FILE_NAME].field, sv_ret_file,
                                        strlen(sv_ret_file) );

        /*
        -- create PMF filename using outgoing_file, which is a fullpath name 
        */
        metafilename = malloc(strlen(outgoing_file) +2 + 1) ;
        sprintf(metafilename,"%s.M", outgoing_file) ;

        if (!APS_create_pmf (metafilename, PMF_struct) )
        {
            sprintf(msg, "Failed to create PMF file '%s'.\n", metafilename);
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT); 
            free (metafilename);
            error_exit(progname) ;
         }
         else
         {
            sprintf(msg, "PMF file '%s' created.", metafilename);
            aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT); 
            free (metafilename);
         }
     }

    if (outgoing_file != NULL)
        free(outgoing_file) ;
    if (commands != NULL)
        free(commands) ;

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

     sprintf(msg, "Program completed successfully.");
     aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT); 
     exit(APS_EXIT_OK);

} /* main */

/**************************************************************
**
** getPlatformAndDestFromType()
**
*************************************************************** */
int getPlatformAndDestFromType( char *pdid , char *dest , char *platform )
{
int i;
static char *plats[] = { "E1" , "E2" , "J1" , "R1" , "A1" };
   
    if( strncmp(pdid,"A",1) == 0 )
    {
        strcpy(dest,"HC");
    }
    else if ( strncmp(pdid,"M",1) == 0 )
    {
        strcpy(dest,"WFF");
    }
    else
    {
        fprintf(stderr,"ERROR: Invalid destination specification:%s\n",pdid);
        return (0);
    }
    
    /* Destination conversion was ok; do platform conversion now */
     
    for (i=0 ; i<5 ; i++)
    {
        if( strncmp(&pdid[1],plats[i],2) == 0 )
        {
            strncpy(platform,&pdid[1],2);
            platform[2]='\0';
#ifdef DEBUG
            printf("dest:%s  platform:%s\n",dest,platform);
#endif
            return (1);
        }
    }
    return (0);
}  /*  end getPlatformAndDestFromType*/

/***************************************************************
**
** usage_exit ()
**
**************************************************************** */

void usage_exit(char *progname)
{
    fprintf(stderr,  /* nicer format, keep it */
"Usage:\n%s  -b <start time> -e <end time> -t <type> -o <filename>\n",
progname) ;
    fprintf(stderr,  /* nicer format, keep it */
"\t\t-P <password>  [-U <user name>] [-p <permission>] [-h]\n") ;
    fprintf(stderr,  /* nicer format, keep it */
"where....\n") ;
    fprintf(stderr,  /* nicer format, keep it */
"\t-h Display this set of usage notes.\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-b <start time> time string in 'YYYY:DDD:HH:MM:SS.CCC' format\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-e <end time> time string in 'YYYY:DDD:HH:MM:SS.CCC' format\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-t <type> AE1E | AE2E | AJ1E | AA1E |\n"); 
    fprintf(stderr,  /* nicer format, keep it */
"\t          AR1E | ME1E | ME2E | MR1E\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-o <file name>  -- state vector output filename\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-p <permission> -- single-activity permission (from the calling program)\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-U <user name>  -- Sybase login user ID\n");
    fprintf(stderr,  /* nicer format, keep it */
"\t-P <password>   -- Sybase login password\n");
    fprintf(stderr,  /* nicer format, keep it */
"NOTE: The following enviroment variables should be set properly\n");
    fprintf(stderr,  /* nicer format, keep it */
"\tIMS_SERVER\n");
    fprintf(stderr,  /* nicer format, keep it */
"\tIMS_DB\n");
    fprintf(stderr,  /* nicer format, keep it */
"\tAPS_SYBASE_USERID (needed if user name is not specified)\n");
    exit (APS_EXIT_ERROR);
} /* usage_exit */

void error_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO, 
        "Program terminated abnormally.", DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
} /* error_exit */

