#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       do_j1_msgn.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)do_j1_msgn.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/j1_msgn/SCCS/s.do_j1_msgn.c"

#include "db_sybint.h"  /* for db_open  */

#include <aps_log_msg.h>    /* for the aps_log_msg() routines.    */
#include <aps_defs.h>       /* for APS_EXIT_OK and APS_EXIT_ERROR  */
#include <apspath.h>        /* for APS_FA_ERROR_FILES      */
#include <file_utilities.h> /* for fa_move_file()          */ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
 
/* these are sybase include files; see the makefile -I value for directory: */
#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
  
#define MSGN_LEN 36

/* end of line...   */
#define EOL      10

char     *file_util_progname ; /* required by libfileutils.a */
char     file_util_msg[MSG_LEN]; /* required by libfileutils.a */

char    infile[200];    /* the MSGN file name  */
char    buf[1000] ;    /* buffer for queries and other misc.   */

/* data from the MSGN header */
char filetime[22] ;

/* data from an MSGN record.  */
char strttime[22], stoptime[22] ;
char sar_status[2], ops_status[2], mdr_status[2], mdt_status[2] ;
int  mdr_op_time ;

int msgnfd ;
char late_time[22]= {"1999:365:00:00:00.000"}; /* time beyond end of mission */

extern int  init_vec_lib();         /* initializes vector lib with stoicfile */
extern int  init_vec_lib_exit(int); /* prints message, exits if stoic error */

/* needed for Sybase        */
extern          DBPROCESS*      opendb(char*, char*);
DBINT           return_code;
DBPROCESS       *dbproc;

struct rejects 
{
    char sat[100][3] ;
    char sensor[100][4] ;
    int rev[100]; 
    int dtkid[100] ;
    int rejcnt ;
};

void normal_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program completed successfully.",
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_OK) ;
}
void error_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_ERROR) ;
}
void usage_exit(char *progname )
{
    printf(
        "usage:  %s  [-k] [-U sybase_userid] -P password  MSGN_Filename\n\n", 
        progname);
    printf("   %s processes the MSGN file.  If there is an error,\n" ) ;
    printf("   the  input file is moved to the APS_DATA/FA_error_files\n");
    printf(
	"   directory.  If not, it is copied to the APS_DATA/FA_input_files \n");
    printf("   directory.\n" ) ;
    printf(
"\n   -k    Keeps the input file where it is; prevents the copy or move\n");
    printf("\n   %s Compiled %s %s\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname) ;
}

/**********************************************************************
*
*  Name : DO_J1_MSGN
*
*  Purpose:
*   read and process a MSGN file from NASDA.  
*
*  Input:
*   name        type    descrip
*
*  Local variables:
*   fp      FILE*   file pointer
*                            
**********************************************************************/
main(int argc, char *argv[])
{

    int     j ;
    int     status;     /* used for status of file  */

    /* getdup gets the Database name, Userid, and Password from cmd line, env */
    extern int getdup( int argc, char **argv, char
        *extra_flags, char *dbname, char *sybase_userid, char *password );
    char        *flag_list = "U:P:k" ;
    int         kflag = 0 ;
    extern int  optind ;
    extern char *optarg ;
    int         c ;
    char        dbname[100];        /* name of database         */
    char        sybase_userid[100]; /* sybase-approved userid   */
    char        password[100];      /* sybase-approved password */
    int         err_code;

    char    file_time_1[22], file_time_2[22] ;
    int     addcnt, errcnt, concnt, errflag, stat ;

    int n_msgn_err ;
    double      et1;    /* only used for an early call to vector lib    */
    int     rcode;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    n_msgn_err = 0 ;

    /* open the syslog system.  */
    aps_open_syslog();

    sprintf(buf, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(buf, " " ) ;
        strcat(buf, argv[j] ) ;
    }

    /* for lib_file utils  */
    file_util_progname = argv[0] ;

    aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);

    /* check for enough arguments:  */
    if(argc < 3 | argc > 7)
        usage_exit(argv[0] );

    err_code = getdup(argc, argv, "U:P:", dbname, sybase_userid, password);
    if(err_code == 1)
        usage_exit(argv[0]);
    if(err_code == 2)
    {
        /* sybase_userid not supplied       */
        printf(
"ERROR:  sybase_userid not found in environment variable APS_SYBASE_USERID\n");
        printf("Use -U sybase_userid or setenv APS_SYBASE_USERID.\n\n");
        fprintf(stderr, 
            "%s:\n\nERROR: environment variable APS_SYBASE_USERID not set\n", 
            argv[0] ) ;

        aps_log_msg(argv[0], APS_CRITICAL,
"ERROR:  sybase_userid not given; environment variable APS_SYBASE_USERID not set; cannot open database", 
            DO_SYSLOG, DO_PRINT);

        error_exit(argv[0]);
    }
    if(err_code == 3)
    {
        /* database name not supplied   */
        fprintf(stderr,
"%s:\n\nERROR:  environment variable APSDB not set; cannot open database\n",
            argv[0] ) ;

        printf(
        "ERROR:  environment variable APSDB not set; cannot open database\n" ) ;
        printf("Use setenv APSDB dbname. \n\n");

        aps_log_msg(argv[0], APS_CRITICAL,
            "ERROR:  environment variable APSDB not set; cannot open database",
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]);
    }
    if(err_code != 0)
        usage_exit(argv[0]);
     
    /* check for a dash '-' at the start of the last argument which should
       be a file name.  */
    if(*argv[argc-1] == '-' )
        usage_exit(argv[0]);

    /* check for the -k flag.  */
    /* restart with optind = 1.  */
    optind = 1 ;
    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
        case 'k':
            if(kflag != 0)
                usage_exit(argv[0]);
            kflag ++ ;
            break;
        case 'P':
            break;
        case 'U':
            break;
        case '?':
            usage_exit(argv[0]);
            break;
    default:
        /* do  nothing  */
            break;
        }

    /* initialize the vector library; EXIT with a message if an error   */
    rcode = init_vec_lib();
    if(rcode)
    {
        fprintf(stderr, "%s:\n\nError intializing stoic file\n", argv[0] ) ;
        aps_log_msg(argv[0], APS_CRITICAL, "Error intializing stoic file",
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(rcode);
    }

    /* now open the database:       */
    /* db_open will handle the errors.       */
    dbproc = db_open(dbname, "MSGN", sybase_userid, password,
        NULL,error_handler_exit,&rcode);
    if(rcode != DB_OPEN_OK)
    {
        db_open_errs(rcode, dbname, sybase_userid);

        sprintf(buf, "ERROR:  dbname %s could not be opened", dbname ) ;
        fprintf(stderr, "%s:\n\n%s\n", argv[0], buf ) ;

        aps_log_msg(argv[0], APS_CRITICAL, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);

        banner_exit(APS_EXIT_ERROR);
    }

    /* determine the input file name.       */
    strcpy(infile, argv[argc - 1]);

    if( (int)strlen(infile)  >  99)
    {
        sprintf(buf,
            "The input file name length > 99 characters:  '%s'\n", infile);
        printf(buf) ;
        fprintf(stderr, "%s:\n\n%s\n", argv[0], buf ) ;
        aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]);
    }


    printf("%s version %s %s\n", argv[0], __DATE__, __TIME__ ) ;
    sprintf(buf, "banner 'START MSGN'; banner '%s';date", infile);
    rcode = system(buf);

    /* zero out counters        */
    printf("SCHD FA Update:  J-ERS-1 MSGN file\n");
    printf(" Input file: %s\n\n",infile);

    /* Open the msgn file for reading */
    /*  if ((msgnfd = open(infile,O_RDONLY,0,"rfm=udf","ctx=stm")) == -1)  */
    if ((msgnfd = open(infile,O_RDONLY)) == -1) 
    {
        sprintf(buf, "%s(%d):  Error opening READONLY MSGN Input file:  '%s'\n",
            __FILE__, __LINE__, infile ) ;
        printf(buf) ;
        fprintf(stderr, "%s:\n\n%s\n", argv[0], buf ) ;
        aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        banner_exit(3);
    }

    /* header record processing... */
    stat = msgn_hdr ( msgnfd ) ;
    if ( stat == 1 ) 
    {
        sprintf(buf, "Error reading input file header record.\n" ) ;
        fprintf(stderr, "%s:\n\n%s\n", argv[0], buf ) ;
        aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);

        if ( kflag == 0 )
            if (!fa_move_file( infile, APS_FA_ERROR_FILES ) )
			{
				sprintf(buf, "Could not move %s to error area.\n", infile);
				aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);
			}

        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        close(msgnfd);
        banner_exit(APS_EXIT_ERROR);
    }

    /* Initialize counters and flags */
    addcnt = errcnt = concnt = 0;
    errflag = 0;


    /* Loop through records */
    for (;;) 
    {

        /* Read the file to get one record.  */
        printf ( 
"\n-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -\n");
        stat = msgn_rec(msgnfd);

        if (stat == -1)      /* end of file */
            break; 

        if (stat > 0 ) 
        {
            /* error in the msgn record.  */
            n_msgn_err ++ ;
            printf (
                " SKIPPING this record to process the next MSGN record. \n");
            continue ;
        }
        if (stat < 0) 
        {
            /* Read error */
            printf ( 
    " READ ERROR in MSGN file.  Terminating the reading of the MSGN file...\n");
            sprintf(buf, "Error reading input file data record.\n" ) ;
            fprintf(stderr, "%s:\n\n%s\n", argv[0], buf ) ;
            aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            if ( kflag == 0 )
                if ( !fa_move_file( infile, APS_FA_ERROR_FILES ) )
				{
					sprintf(buf, "Could not move %s to error area.\n", infile);
					aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);
				}
            error_exit(argv[0]) ;
        } 
        printf ( 
            "MSGN values:  SAR=%s   OPS=%s   MDR=%s   MDR time=%04.4d   MDT=%s\n",
            sar_status, ops_status, mdr_status, mdr_op_time, mdt_status ) ;
        printf ( "                %s  %s\n", strttime, stoptime ) ;

        /* Find conflicts.  see if any data-takes become invalidated:  */

        stat = msgn_conf ( &concnt ) ;

        if ( stat != -1  &&  stat != 0 ) 
        {
            errcnt ++;
            printf ( 
" ERROR: %d in activity_conf.c.  Continuing with next MSGN record...\n",
            stat );
            continue;
        } 

        printf ( 
"\n-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -\n\n");

        stat = msgn_add ( &addcnt ) ;
              
        if (stat != 0) 
        {
            printf ( "Error adding a data-take.\n");
            printf ( "Skipping to the next record in the MSGN file.\n" );
            errcnt ++;
            continue;
        }
    } /* end for END OF READ LOOP  */

    /* Close the input file */
    close(msgnfd);

    /* Write the totals to the LOG file */
    printf("\nTOTALS\n");

    printf(" MSGN records with errors:  %d\n\n",n_msgn_err );
    printf(" Additions  : %d\n",addcnt);
    printf(" Conflicts  : %d\n",concnt);

    /* The File PASSED if no errors occurred */
    printf("\n The File PASSED if no errors occurred.\n\n");
    if ((errcnt + n_msgn_err ) == 0) 
    {
        printf("\n THE FILE %s PASSED !\n",infile);
        rcode = system("banner PASSED");
        if ( kflag == 0 )
            if ( !fa_copy_file( infile, APS_FA_INPUT_FILES ) )
			{
				sprintf(buf, "Could not copy %s to file input area.\n", infile);
				aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);
			}
    } 
    else 
    {
        printf("\n THE FILE %s FAILED !\n",infile);
        if ( kflag == 0 )
            if ( !fa_move_file( infile, APS_FA_ERROR_FILES ) )
			{
				sprintf(buf, "Could not move %s to error area.\n", infile);
				aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);
			}
        rcode = system("banner FAILED");
    }

    normal_exit(argv[0]);

}

/**********************************************************************
*
*  Name : msgn_hdr
*
*  Purpose:
*   read and process the MSGN file headre.  
*
*  Input:
*   name        type    descrip
*   msgnfd      int input file descriptor number 
*  Output:  
*   return value    int = 1 error:  must end processing.
*               = 0 O.K.  continue processing.  
*
**********************************************************************/
int msgn_hdr ( 
    int     msgnfd )    /* input MSGN file descriptor       */
{

    /* data from the MSGN file header   */
    char file[9],proj[5],hmmo[5],fais[5],datef[9],timef[9],fdy[2];
    char drecl[5],no_recs[6],unused[83];

    /* declarations     */
    int errno, rcount, stat ;
    int errflag, n_rec, j ;
    char read_rec[279] ;

    /* Parse the header record */
    stat = read(msgnfd,read_rec,128);
    read_rec[128] = '\0' ;

    if ( stat < 0 ) 
    {
        /* error    */
        /* Close the input file */
        close(msgnfd);
        return 1;
    } 

    /* file name                */
    strncpy (file, read_rec, 8);
    file[8] = '\0' ;
    printf(" FILE HEADER INFO: \n");
    printf(" File name:            %s\n",file);
    if( strncmp(file, "MSGN", 4 ) != 0 )
    {
        printf("msgn_hdr:  According to the header, this file is not MSGN.\n");
        /* Close the input file */
        close(msgnfd);
        return 1;
    }
    /* project ERS1             */
    strncpy (proj, read_rec+8, 4);
    proj[4] = '\0' ;
    printf(" Project:              %s\n",proj);
    if( strncmp(proj, "ERS1", 4 ) != 0 )
    {
        printf(
            "msgn_hdr:  According to the header, the project is not ERS1.\n");
        /* Close the input file */
        close(msgnfd);
        return 1;
    }
    /* sending ground station HMMO      */
    strncpy (hmmo, read_rec+12, 4);
    hmmo[4] = '\0' ;
    printf(" Sending GS:           %s\n",hmmo);
    if( strncmp(hmmo, "HMMO", 4 ) != 0 )
    {
        printf("Header:  the sending ground station is not HMMO.\n");
        /* Close the input file */
        close(msgnfd);
        return 1;
    }
    /* receiving ground station FAIS        */
    strncpy (fais, read_rec+16, 4);
    fais[4] = '\0' ;
    printf(" Receiving GS:         %s\n",fais);

    /* file date                */
    strncpy (datef, read_rec+20, 8);
    datef[8] = '\0' ;
    printf(" File date:            %4.4s/%2.2s/%2.2s\n",
        datef,datef+4,datef+6 );

    /* file time                */
    strncpy (timef, read_rec+28, 8);
    timef[8] = '\0' ;
    printf(" File time:            %s\n",timef);

    /* file time ASF        */
    /*  printf("msgn_hdr 1:  datef = %s, timef = %s\n", datef, timef);  */
    /* 
    --  stat = j1date_time2asf ( datef, timef, filetime ) ;
    --  if ( stat != 0 )
    */
    stat = tc_yyyymmdd_hhmmss2asf(datef, timef, filetime ) ;
    if ( !stat ) 
    {
        printf ( "ERROR in MSGN header File Formation time\n" ) ;
        printf ( "Date = %s, time = %s\n", datef, timef ) ;
        return 1 ;
    }
    /*  printf("msgn_hdr 2:  filetime = %s\n", filetime);  */

    /* file descriptor flag = Y     */
    strncpy (fdy, read_rec+36, 1);
    fdy[1] = '\0' ;
    printf(" File descriptor existence flag:  %s\n",fdy);
    /* rec length = 80 ?            */
    strncpy (drecl, read_rec+37, 4);
    drecl[4] = '\0' ;
    printf(" Record length:        %s\n",drecl);
    /* Number of data records       */
    strncpy (no_recs, read_rec+41, 5);
    no_recs[5] = '\0' ;
    printf(" No. of data records:  %s\n",no_recs);

    return 0 ;

}
/*****************************************************************
* Name: msgn_rec
*
* Purpose
*   read in a record from the J1 MSGN file
*   parse and validate the record
*
* Input
*   fd      i*  input file descriptor
* Output        
*   e       str e*  msgn_act structure to hold msgn record
* Internal
*   urec        c   msgn data record buffer
*
*****************************************************************/
int msgn_rec(
    int fd )    /* input file descriptor    */
{
    char urec[MSGN_LEN+5] ;
    char buf[10] ;
    int stat ;
    char j1date[9], j1time[9];
    int error_count ;

    error_count = 0 ;

    /* now read station the status record.      */
    stat = read ( fd, urec, MSGN_LEN ) ;
    urec[MSGN_LEN] = '\0';
    /*  printf( "msgn_rec 1:  MSGN record = %s\n",urec) ;   */

    /* For end of file, record, or error */
    if (stat == 0 || urec[0] == '\0' || urec[0] == EOL)        
        return -1;     
    if (stat < 0) 
    {
        printf("ERROR: MSGN File read error; status=%d.\n");
        return -10 ;
    }

    printf( "MSGN record = %s\n",urec) ;

    /* state of SAR  */
    sar_status[0] = urec[0] ;
    sar_status[1] = '\0' ;
    if ( sar_status[0] != 'Y' && sar_status[0] != 'N' )
    {
        printf ( 
        "ERROR in SAR status field in MSGN record.  Value = %s\n", sar_status) ;
        error_count++ ;
    }

    /* state of OPS  */
    ops_status[0] = urec[1] ;
    ops_status[1] = '\0' ;
    if ( ops_status[0] != 'Y' && ops_status[0] != 'N' 
      && ops_status[0] != 'V' && ops_status[0] != 'S' )
    {
        printf ( 
        "ERROR in OPS status field in MSGN record.  Value = %s\n", ops_status) ;
        error_count++ ;
    }

    /* state of MDR  */
    mdr_status[0] = urec[2] ;
    mdr_status[1] = '\0' ;
    if ( mdr_status[0] != 'Y' && mdr_status[0] != 'N' )
    {
        printf ( 
        "ERROR in MDR status field in MSGN record.  Value = %s\n", mdr_status) ;
        error_count++ ;
    }

    /* operation time of MDR  */
    strncpy ( buf, urec+3, 4 ) ;
    buf[4] = '\0' ;
    /*  mdr_op_time = atoi ( buf ) ;        */
    /*  using sscanf for error detection opportunity...     */
    stat = sscanf(buf, "%4d", &mdr_op_time);

    if ( stat != 1 || mdr_op_time < 0 || mdr_op_time > 1200 )
    {
        printf(
            "ERROR in MDR operation time value in MSGN record. Field = %4.4s\n",
            urec+3 ) ;
        printf("                                Converted integer value = %d\n", 
            mdr_op_time ) ;
        error_count++ ;
    }

    /* state of MDT  */
    mdt_status[0] = urec[7] ;
    mdt_status[1] = '\0' ;
    if ( mdt_status[0] != 'Y' && mdt_status[0] != 'N' 
      && mdt_status[0] != '1' && mdt_status[0] != '2' )
    {
        printf ( 
        "ERROR in MDT status field in MSGN record.  Value = %s\n", mdt_status) ;
        error_count++ ;
    }

    /* halt date / time    -  START of time period: strttime  */
    /*
    -- what this means is that if HMMO can do anything, for 
    -- a time period, then it has a halt time and a re-start time. 
    -- not associated with the satellite operations.  
    */
    if ( strncmp ( urec+8, "              ", 14 ) != 0 )
    {
        /* field is non-blank   */
        strcpy ( j1date, "19" ) ;
        strncpy ( j1date+2,  urec+8, 2 ) ;
        strncpy ( j1date+4, urec+11, 2 ) ;
        strncpy ( j1date+6, urec+14, 2 ) ;
        j1date[8] = '\0' ;
        strncpy ( j1time, urec+17, 5 ) ;
        strcpy (  j1time+5, ":00" ) ;
        /*
        -- stat = j1date_time2asf ( j1date, j1time, strttime ) ;
        -- if ( stat != 0 )
        */
        stat = tc_yyyymmdd_hhmmss2asf(j1date, j1time, strttime ) ;
        if ( !stat ) 
        {
            printf (
                "ERROR in Halt MMO date of MSGN record.   Value = %14.14s\n", 
                urec+8 ) ;
            error_count++ ;
        }
    }
    else
    {
        /* field is blank; */
        strcpy ( strttime, "                     " ) ;
    }

    /* Start date / time    -  END of time period: stoptime  */
    if ( strncmp ( urec+22, "              ", 14 ) != 0 )
    {
        /* field is non-blank   */
        strcpy ( j1date, "19" ) ;
        strncpy ( j1date+2,  urec+22, 2 ) ;
        strncpy ( j1date+4, urec+25, 2 ) ;
        strncpy ( j1date+6, urec+28, 2 ) ;
        j1date[8] = '\0' ;
        strncpy ( j1time, urec+31, 5 ) ;
        strcpy (  j1time+5, ":00" ) ;
        /* 
        -- stat = j1date_time2asf ( j1date, j1time, stoptime ) ;
        -- if ( stat != 0 )
        */
        stat = tc_yyyymmdd_hhmmss2asf(j1date, j1time, stoptime ) ;
        if ( !stat ) 
        {
            printf ( 
            "ERROR in Start MMO date of MSGN record.   Value = %14.14s\n", 
                urec+22 ) ;
            error_count++ ;
        }
    }
    else
    {
        strcpy ( stoptime, "                     " ) ;
    }

    if ( error_count > 0 )
    {
        printf ( "%d ERRORS were found in this MSGN record. \n", 
        error_count ) ;
    }
    return error_count ;
} 

/*****************************************************************
* Name: msgn_conf
*
* Purpose
*   find dtk conflicts with the input MSGN activity.
*
* Input
* Output
*   concnt      i   total conflicts counter
*****************************************************************/
int msgn_conf(
    int     *concnt)    /* count of conflicts       */
{
    /*  arguments for the activity conflict analysis routine:  */
    struct a_conf_args 
    {   
        char station_id[4] ;
        char obj[3];
        char acty[4];
        char transid[3];
        char asft1[22];
        char asft2[22];
        int dtkid;
        char fadtkid[21];
        int n_confs;
        int n_combines;
        int n_sims;
        int n_concurs;
        int n_pars;
    } pstruct, *p ;
    char    msg[101];
    int     pflag, stat;
    int     start_conf ;
    struct rejects  r ;

    r.rejcnt = 0 ;

    start_conf = *concnt ;
    /*  build the arguments for the subroutine call.  */
    p = &pstruct ;
    strcpy(p->obj,"J1");
    strcpy(p->station_id, "ASF") ;
    strcpy(p->asft1,filetime);
    strcpy(p->asft2,late_time);
    strcpy(p->fadtkid," ");
    p->dtkid = 0 ;

    /* if SAR is down   */
    if ( sar_status[0] == 'N' )
    {
        strcpy ( p->acty, "SDN" ) ;
        strcpy(p->transid, "00" ) ;
        pflag = 1;
        /*  printf("msgn_conf 1:  calling dn_time_conf_rej:  SDN.\n");  */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            /* there was an error.  */
            printf("Error detected during activity conflict analysis.\n"); 
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }

    /* if OPS is down   */
    if ( ops_status[0] != 'Y' )
    {
        strcpy ( p->acty, "ODN" ) ;
        strcpy(p->transid, "00" ) ;
        pflag = 1;
        /*  printf("msgn_conf 2:  calling dn_time_conf_rej:  ODN.\n"); */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            /* there was an error.  */
            printf("Error detected during activity conflict analysis.\n");
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }
    /* if OVN is down   */
    if ( ops_status[0] != 'Y' && ops_status[0] != 'V' )
    {
        strcpy ( p->acty, "VDN" ) ;
        strcpy(p->transid, "00" ) ;
        pflag = 1;
        /*  printf("msgn_conf 3:  calling dn_time_conf_rej:  VDN.\n"); */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            printf("Error detected during activity conflict analysis.\n");
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }
    /* if MDR is down   */
    if ( mdr_status[0] == 'N' )
    {
        strcpy ( p->acty, "RDN" ) ;
        strcpy(p->transid, "00" ) ;
        pflag = 1;
        /*  printf("msgn_conf 4:  calling dn_time_conf_rej:  RDN.\n"); */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            /* there was an error.  */
            printf("Error detected during activity conflict analysis.\n");
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }
    /* if MDT 1 is down */
    if ( mdt_status[0] == 'N' || mdt_status[0] == '2' )
    {
        strcpy ( p->acty, "TDN" ) ;
        strcpy(p->transid, "F1" ) ;
        pflag = 1;
        /*  printf("msgn_conf 5:  calling dn_time_conf_rej:  TDNF1.\n"); */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            /* there was an error.  */
            printf("Error detected during activity conflict analysis.\n");
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }
    /* if MDT 2 is down */
    if ( mdt_status[0] == 'N' || mdt_status[0] == '1' )
    {
        strcpy ( p->acty, "TDN" ) ;
        strcpy(p->transid, "F2" ) ;
        pflag = 1;
        /*  printf("msgn_conf 6:  calling dn_time_conf_rej:  TDNF2.\n"); */
        stat = dn_time_conf_rej ( dbproc, p, pflag, &r ) ;
        if(stat < 0)
        {
            /* there was an error.  */
            printf("Error detected during activity conflict analysis.\n");
            printf("dn_time_conf_rej error number: %d \n",stat);
            printf("See dn_time_conf_rej.c.\n");
        }
        /*  O.K.  increment number of conflicts encountered for this file.  */
        *concnt += p->n_confs;
    }

    if ( start_conf == *concnt )
    {
        /* no conflicts found; return a 0 code.  */
        return 0 ;
    }
    /*  some conflicts were found; return code is -1.       */
    printf ( 
"CONFLICT:  SOME DATA-TAKES CANNOT BE CARRIED OUT DUE TO EQUIPMENT DOWN. \n");
    printf(
        "           The status of these data-takes will be  changed to REJ.\n");

    printf ( 
"\n-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -\n\n");
    stat = msgn_reject ( &r ) ;
    if ( stat != 0 )
    {
        printf("Error detected while processing rejections.\n");
    }
    return -1 ;
}

/*****************************************************************
* Name: msgn_add
*
* Purpose
*   add a new j1_dn_times record.  
*
* Input
* Output
*   addcnt      i*  total additions counter
*
*****************************************************************/
int msgn_add(
    int *addcnt )
{
    int did,erev;
    char note[40];
    char date[9], buf[4] ;
    int errno,rcount,stat;

    /*  
    ## message "Adding j1_dn_times record..."
     */

    /* old embedded QUEL commands:  
        ## delete j1_dn_times 
        ## inquire_equel ( errno=errorno, rcount=rowcount )
    ** end of embedded QUEL     */
    /*  Sybase version:  */
    sprintf(buf, "delete j1_dn_times ");
    /*  printf("msgn_add 1:  buf = >%s<\n", buf);  */
    return_code = dbcmd (dbproc, buf);
    return_code = dbsqlexec(dbproc);
    return_code = dbresults(dbproc);
    rcount = DBCOUNT(dbproc);

    if( rcount == 1)
    {
        printf ( "  %d j1_dn_times DB record was deleted.\n", rcount ) ;
    }
    else
    {
        printf ( "  %d j1_dn_times DB records were deleted.\n", rcount);
    }

    /* old embedded QUEL code:  
        ## append to j1_dn_times (
        ##  #strttime       = filetime ,
        ##  #stoptime       = late_time ,
        ##  #sar_status     = sar_status ,
        ##  #ops_status     = ops_status ,
        ##  #mdr_status     = mdr_status ,
        ##  #mdr_op_time    = mdr_op_time ,
        ##  #mdt_status     = mdt_status    )
        ## inquire_equel ( errno=errorno, rcount=rowcount )
    ** end of old embedded QUEL code    */
    sprintf(buf,
        "insert j1_dn_times values('%s', '%s', '%s', '%s', '%s', %d, '%s')",
        filetime, late_time, sar_status, ops_status, mdr_status, mdr_op_time, 
        mdt_status );
    /*  printf("msgn_add 2:  buf = \n%s\n", buf);  */
    return_code = dbcmd ( dbproc, buf);
    return_code = dbsqlexec(dbproc);
    return_code = dbresults(dbproc);
    rcount = 0;
    if ( return_code == SUCCEED )
        rcount = DBCOUNT(dbproc);

    if (rcount == 0)
        return 1;

    printf ( "The latest J1 satellite status was added to the database.\n" );

    /* Increment additions and scheduled counters */
    *addcnt += 1;

    return 0;
}

/*****************************************************************
* Name: msgn_reject
*
* Purpose
*   For data-takes which can no longer be completed due to satellite 
*   equipment down, this routine rejects them.  
*
* Input
* Output                              
*
*****************************************************************/
int msgn_reject ( 
    struct rejects      *r  )
{
    int rej_counter ;
    int errno,rcount,stat,errflag;
    int i,darid,segid,erev;
    int tid;
    char dtkstat[4] ;
    char dstrt[100][22],dstop[100][22];
    char date[9] ;
    char dtkname[14];

    rej_counter = 0 ;
          
    /* Get the current date */
    stat = tc_systime2yyyycddd(date) ;
    if(!stat)
        return 1 ;
    /*     stat = sch_getdate(date); 
    /*   if (stat != 0) 
    /*      return 1;
     */

    for ( i = 0 ;  i < r->rejcnt ; i ++ ) 
    {
        /* for each stored data-take J1/sensor/rev/dtkid    */

        /* Form the datatake id */
        sprintf(dtkname,
            "J1/%.1s/%05d.%02d", r->sensor[i], r->rev[i], r->dtkid[i]);

        /* Log initial message */
        if (i == 0)
        {
            printf(" The following data-takes were CONFLICTS:  \n");
            printf( "\n Old status     dtkname      segments\n" ) ;
            printf( " ----------   -------------  ---------------\n");
        }

        /* Retrieve old status */
        /* old embedded QUEL code:  
            ##  retrieve ( dtkstat = dtk.#dtkstat ) 
            ##      where dtk.#sat    = "J1" 
            ##    and dtk.#sensor = r->sensor[i]
            ##    and dtk.#rev    =   r->rev[i]
            ##    and dtk.#dtkid  = r->dtkid[i]
            ##  inquire_equel(errno=errorno,rcount=rowcount)
        ** end of old embedded QUEL code    */
        /* Sybase version:      */
        sprintf(buf, "select dtkstat from dtk \
    where sat = 'J1' and sensor = '%s' and rev = %d and dtkid = %d ",
        r->sensor[i], r->rev[i], r->dtkid[i]);
        /*  printf("msgn_reject 1:  buf = \n%s\n", buf);   */
        return_code = dbcmd(dbproc, buf);
        return_code = dbsqlexec(dbproc);
        rcount = 0;
        while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
        {
            if (return_code == SUCCEED)
            {
                dbbind(dbproc, 1, NTBSTRINGBIND, (DBINT)0,   (BYTE *) dtkstat);
                while (dbnextrow(dbproc) != NO_MORE_ROWS)
                {
                    rcount ++ ;
                }
            }
        }

        /* If old status is REJ, then skip to next one.  */
        if ( strcmp ( dtkstat, "REJ" ) == 0 ) continue ;

        /* old embedded QUEL commands:  
            ##  replace dtk (#dtkstat="REJ",#dtkdate=date)
            ##      where dtk.#sat    = "J1" 
            ##    and dtk.#sensor = r->sensor[i]
            ##    and dtk.#rev    =   r->rev[i]
            ##    and dtk.#dtkid  = r->dtkid[i]
            ##  inquire_equel(errno=errorno,rcount=rowcount)
            ** end of old embedded QUEL commands        */
        /* Sybase version:      */
        sprintf(buf, "update dtk set dtkstat  = 'REJ', dtkdate = '%s' where \
    dtk.sat = 'J1' and dtk.sensor = '%s' and dtk.rev = %d and dtk.dtkid = %d ",
            date, r->sensor[i], r->rev[i], r->dtkid[i] );
        /*  printf("msgn_reject 2:  buf = \n%s\n", buf);   */
        return_code = dbcmd(dbproc, buf);
        return_code = dbsqlexec(dbproc);
        return_code = dbresults(dbproc);
        rcount      = dbcount(dbproc);

        if (rcount == 0)
        {
            /* perhaps this record got combined out of existence    */
            /* earlier                  */
            r->rev[i]   = 0;
            r->dtkid[i] = 0;
            /* now skip to the next record.         */
            continue ;
        }

        /* Log message for each dtk found. */
        printf ( "     %3.3s      %2.2s/%1.1s/%05.5d.%02.2d\n",
            dtkstat, "J1", r->sensor[i], r->rev[i], r->dtkid[i] ) ;

        rej_counter ++ ;

        /* Update the status of segments associated with the datatakes */
        /* Segment status should be set to REJ when Datatakes are set to REJ */

        sprintf ( dtkname, "%.2s/%.1s/%05d.%02d", "J1", r->sensor[i], 
        r->rev[i], r->dtkid[i] ) ;

        /* old embedded QUEL code:  
        ##      replace seg (#segstat="REJ",#segdate=date)
        ##          where seg.#sat      = "J1"
        ##        and seg.#sensor   = r->sensor[i]
        ##        and seg.#rev      = r->rev[i]
        ##            and seg.#dtkid    = dtkname
        ##      inquire_equel(errno=errorno,rcount=rowcount)
            if (errno != 0) break ;
        ##      retrieve (darid=seg.#darid,segid=seg.#segid)
        ##          where seg.#sat      = "J1"
        ##        and seg.#sensor   = r->sensor[i]
        ##        and seg.#rev   = r->rev[i]
        ##            and seg.#dtkid = dtkname
        ##      {
                fprintf ( logp,
                "                             Segment: %d.%d\n",darid,segid);
        ##      }
        ##      inquire_equel(errno=errorno,rcount=rowcount)
            if (errno != 0) break;
        ** end of old embedded QUEL code    */
        /* Sybase version:      */
        sprintf(buf, "update seg set segstat = 'REJ', segdate = '%s' \
    where sat = 'J1' and sensor = '%s' and rev = %d and dtkid = '%s'",
        date, r->sensor[i], r->rev[i], dtkname);
        /*  printf("msgn_reject 3:  buf = \n%s\n", buf);   */
        return_code = dbcmd(dbproc, buf);
        return_code = dbsqlexec(dbproc);
        return_code = dbresults(dbproc);
        rcount = dbcount(dbproc);
     
        if(rcount > 0)
            printf(" Segment list:  \n" ) ;

        sprintf(buf, "select darid, segid from seg \
    where sat = 'J1' and sensor = '%s' and rev = %d and dtkid = '%s'", 
            r->sensor[i], r->rev[i], dtkname);
        /*  printf("msgn_reject 4:  buf = \n%s\n", buf);    */
        return_code = dbcmd(dbproc, buf);
        return_code = dbsqlexec(dbproc);
        rcount = 0;
        while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
        {
            if (return_code == SUCCEED)
            {
                dbbind(dbproc, 1, INTBIND, (DBINT)0,   (BYTE *) &darid);
                dbbind(dbproc, 2, INTBIND, (DBINT)0,   (BYTE *) &segid);
                while (dbnextrow(dbproc) != NO_MORE_ROWS)
                {
                    printf ( 
                    "                             Segment: %d.%d\n",darid,segid);
                }
            }
        }
    } /* end for rejcnt loop */

    if( rej_counter > 0 )
    {
        printf (
            "\nTotal data-takes rejected due to conflicts: %d\n", rej_counter);
        printf (" The status of these data-takes is now REJ. \n\n");
        if( rej_counter >= 100 )
        {
            printf(
                "########################################################\n");
            printf(
                "# NOTE:                                                #\n");
            printf(
                "# There may be more than 100 data-takes to reject.     #\n");
            printf(
                "# To cause rejection of the rest of the data-takes,    #\n");
            printf(
                "# just run this program again.                         #\n");
            printf(
                "# The program is set up to just reject the first 100.  #\n");
            printf(
                "########################################################\n");
        }
    }

    return 0 ;
}

