static char *sccs = "@(#)ims_valids.c	1.1  06/09/97";
/* *************************************************************
**
** File:        ims_valids.c
**
** Function:    A client program that concerns the ASF valids file.
**              it basically is to set the parameter values to the
**              values in the file.  there is an option as to
**              whether or not to update the db.  other values are
**              checked to determine if the db values are the same.
**              if no check is made, error messages are printed.
**
** Author:      David Pass
**
** Date:        6/2/97
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <time.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_qi.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <unistd.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>

#define  MAX_ID_LEN   65
#define  MAX_LINE     129
#define  MAX_PACK_BUF 200

/*
**  define the buffers to save the dataset info
*/
typedef  struct  dataset__t *pnt_dataset_t;
typedef  struct  dataset__t {
    char  platform[31];
    short  n_sensors;
    char  sensor[6][31];
    char  dataset[81];
    char  campaign[81];
    short  dataset_idx;
    char  md_id[31];
    char  daynight_flag;    /*  Y or N (caps)  */
    short  processing_level; /* if -1, none input  */
    short  n_parameters;
    char  parameters[10][81];
    pnt_dataset_t  pnt_next;
    } dataset_t;


/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT * );
static int ims_closeConnection(  IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ * );
static IMS_QI_DESC_OBJ *openConnection (
    IMS_MSG_STRUCT *, char * );
static  int ims_openQueryConnection2 (  IMS_MSG_STRUCT *, char * );
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ  * );
static int checkRetStatus (  IMS_MSG_STRUCT * );
static  int  get_params( IMS_MSG_STRUCT *,  pnt_dataset_t  );
static  int  get_policy( IMS_MSG_STRUCT *, char *, pnt_dataset_t );
static  int read_valids(  IMS_MSG_STRUCT *, char *, short );
static  int save_param(  IMS_MSG_STRUCT *, short, char * );


/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *odlFile;
    char *logFile;
    char *saveParams;
    char *commandFile;
    char *server;
    char *database;
    char *help;
    char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
    {"-U",              &commands.username},
    {"+username",       &commands.username},
    {"-P",              &commands.password},
    {"+password",       &commands.password},
    {"-O",              &commands.odlFile},
    {"+odlFile",        &commands.odlFile},
    {"-L",              &commands.logFile},
    {"+logFile",        &commands.logFile},
    {"-S",              &commands.saveParams},
    {"+saveParams",     &commands.saveParams},
    {"-C",              &commands.commandFile},
    {"+commandFile",    &commands.commandFile},
    {"-X",              &commands.server},
    {"+server",         &commands.server},
    {"-Y",              &commands.database},
    {"+database",       &commands.database},
    {"-h",              &commands.help},
    {"+help",           &commands.help},
    {"-r",              &commands.release},
    {"+release",        &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",        &commands.username},
    {"password",        &commands.password},
    {"odlFile",         &commands.odlFile},
    {"logFile",         &commands.logFile},
    {"saveParams",      &commands.saveParams},
    {"server",          &commands.server},
    {"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
static char cmdBuf[IMS_COL512_LEN];


/*
** Global Variables
*/
static  char *glb_programName;
static  FILE  *  out_file;

static  IMS_QI_DESC_OBJ *qDesc;

/* *************************************************************
**
** main ()
**
** This is the driver for the valids program.  it reads the parameters,
**      md_id value, sensor, source (platform), processing level,
**      daynight flag, and of course dataset.  All are verified.  Only
**      parameters are changed, and then only if saveParam is set to
**      yes or YES (only 1st char is checked).
**
**************************************************************** */
void main (
    int argc,
    char *argv[])
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    char  program[129];
    short  save_params;
    int fd;
    char  str[256];


    commands.username = NULL;
    commands.password = NULL;
    commands.odlFile = NULL;
    commands.logFile = NULL;
    commands.saveParams = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.help = NULL;
    commands.release = NULL;
    msgDesc = NULL;
    out_file = NULL;

    /*
    ** Get the program name and the node name.
    */
    glb_programName = ims_extractFileName (argv[0]);
    (void) strcpy( program, glb_programName );
    (void) uname (&uname_info);
    (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
    hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

    /*
    ** Allocate message facility structure.
    */
    if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
    {
        (void) fprintf (stderr,
            "Memory allocation for IMS_MSG_STRUCT structure failed.");
        exit (1);
    }

    /*
    ** Initialize the message facility options.
    */
    (void) ims_msgSubSystem (msgDesc, "IMS");
    (void) ims_msgProgramName (msgDesc, glb_programName);
    (void) sprintf (banner, "%s::%s", hostName, glb_programName);
    (void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
    (void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);


    /*
    ** Initialize the signal handler.
    */
    if (ims_setWrapup (runDown) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Initialization of the signal handler failed. %s.",
            strerror (errno));
        goto ERROR;
    }

    /*
    ** Get the command line arguments. The variable status will actually
    ** contain the number of command line arguments processed upon
    ** successful completion.
    */
    if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
        cmdLineElmCount, msgDesc)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Check to see if we got everything off of the command line.
    */
    if (status < argc)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
        "Only %d out of the %d command line arguments were processed.",
            status, argc);
    }

    /*
    ** If release was specified, print it out.
    */
    if (commands.release != (char *) NULL)
    {
        (void) ims_printVersion (stderr);
    }

    /*
    ** If help has been specified, print usage and exit.
    */
    if (commands.help != (char *) NULL)
        {
        usage ();
        exit( 0 );
    }

    /*
    ** If there is a command file present, then get any commands from
    ** this file, then overlay all commands from the commandline, except
    ** password, which will be gone by this point.
    */
    if (commands.commandFile != (char *) NULL){
        if ((status = ims_getFileParms (commands.commandFile,
            cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
            {
            goto ERROR;
        }

        /*
        ** Now, get command line arguments again to overlay file args.
        */
        if ((status = ims_getCmdLine (argc, argv,
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
                {
            goto ERROR;
        }
    }

    /*
    ** Process the information from command-line and/or command-file.
    */
    if( (status = getArgInput (msgDesc ) )  < IMS_OK)
    {
        goto ERROR;
    }

    /*
    **  open the connection
    */
    status = ims_openQueryConnection2( msgDesc, program );
    if(  status  <  IMS_OK ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Could not open connection." );
        goto ERROR;
    }

    /*
    ** Set-up the log file if requested.
    */
    if( commands.logFile  !=  NULL  ){
        /*
        ** Open the log file.
        */
        status = access( commands.logFile, F_OK );
        if(  status  !=  0 ){ /* file not there: create it  */
            if ((out_file = fopen( commands.logFile, "w")) ==  NULL){
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not open log file '%s'. %s",
                    commands.logFile, strerror (errno));
                goto ERROR;
            }
            (void) fclose( out_file );
        }
        if( (fd = open (commands.logFile, O_RDWR|O_CREAT|O_APPEND))
            == -1){
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not open log file '%s'. %s",
                commands.logFile, strerror (errno));
            goto ERROR;
        }

        /*
        ** Change the mode of the log file.
        */
        if (chmod (commands.logFile, 0644) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not change the mode for log file '%s'. %s",
                commands.logFile, strerror (errno));
            goto ERROR;
        }

        /*
        ** Enable message writting to the log file.
        ** Disable message writting to stderr.
        */
        if( (status = ims_msgLogFileDesc( msgDesc, fd ) ) <  IMS_OK)
        {
            goto ERROR;
        }
        (void) ims_msgStderrFlag (msgDesc, IMS_OFF);

        /*
        ** Write a delimiting message to the log file.
        */
        (void) sprintf( str,
            "\n\n>>>>>>>>>>>>>>>>>>  IMS VALIDS STARTUP  "
            "<<<<<<<<<<<<<<<<<<\n\n");
        if ((int) write (fd, str, strlen( str ) )  ==  -1 ){
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not write to log file '%s'. %s",
                commands.logFile, strerror (errno));
            goto ERROR;
        }
        (void) sprintf( str, "** File: '%s'   from '%s'\n",
            commands.logFile, commands.odlFile );
        (void)  write (fd, str, strlen( str ) );
    }

    if(  commands.saveParams  !=  NULL  ){
        save_params = 1;
    }
    else  save_params = 0;

    if ((status = read_valids( msgDesc, commands.odlFile,
        save_params ) ) <   IMS_WARNING ){
        goto ERROR;
    }

    /*
    ** Shutdown the message facility.
    */
    status = ims_closeConnection( msgDesc, qDesc );
    (void) ims_msgStructFree (msgDesc);
    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_valids program had errors." );

    status = ims_closeConnection( msgDesc, qDesc );
    (void) ims_msgStructFree (msgDesc);

    exit (1);
}   /*  main   */


/* *************************************************************
**
**  subr runDown () -  Cleanup and exit from program.
**
**************************************************************** */

static int runDown (
    int sig)
{
    /* Print out the signal caught. */
    (void) fprintf (stderr,
        "\n\nTermination of %s due to signal: %s (%d)\n\n",
        glb_programName, ims_sigMsg (sig), sig);

    return (sig);
}   /*  runDown  */


/* *************************************************************
**
**  subr usage () - Print command line argument switches. called
**      for -help option.
**
**************************************************************** */

static void usage (void)
{
    int i;

    (void) fprintf (stderr,
        "\n%s command-line arguments:\n\n", glb_programName);

    for (i = 0; i < cmdLineElmCount; i++)
    {
        (void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
    }

    (void) fprintf (stderr, "\n\n");
}   /*  usage  */


/* *************************************************************
**
**  subr getArgInput () - process command-line and command-file
**      arguments.
**
**************************************************************** */

static int getArgInput (
    IMS_MSG_STRUCT *msgDesc )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long  i;


    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username == (char *) NULL){
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Username: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.username = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.username, inputBuffer);

    }

    /* password */
    if (commands.password == (char *) NULL){
        if (ims_getPassword (inputBuffer) == NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.password = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.password, inputBuffer);
    }

    /* odlFile */
    if (commands.odlFile  ==  (char *) NULL  ){
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Odl file name: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.odlFile = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.odlFile, inputBuffer);
    }

    /* logFile */
    /*
    **  the log file is not needed anywhere, it may not be set
    */

    return (IMS_OK);
}   /*  getArgInput */


/***************************************************************
**
**  subr read_valids
**
** This event does all the work.
**
**************************************************************** */
static  int read_valids(  IMS_MSG_STRUCT *msgDesc, char *  odl_name,
    short  save_params )
{
    char  dataset[256+1];
    char  option_id[MAX_ID_LEN]; /* option from odl file */
    char  type_id[MAX_ID_LEN];   /* type from odl file  */
    char  format_id[MAX_ID_LEN]; /* format from odl file */
    char  approx_cost[MAX_ID_LEN]; /* cost from odl file  */
    char  platform[MAX_ID_LEN];
    char  sensor[MAX_ID_LEN];
    char  ims_visible_p[MAX_ID_LEN];
    char  package_size[MAX_ID_LEN];

    int status;
    int  i,j;
    char  str[129], str2[129],str3[129];
    unsigned char  flag;

    long  n_sum_out; /*  no. lines in the output file */
    IMS_KEYWORD_LIST * pnt_keyword;
    IMS_KEYWORD_LIST * pnt_keyword_save;
    IMS_KEYWORD_LIST * pnt_keyword_1st;
    long   cnt_keys; /*  no. of keywords read  */
    long   n_datasets;  /* cntr for tot number of datasets  */
    long   n_new_rows; /* cntr for no. of new rows added to db  */
    long   n_updates; /* cntr for no. of updates to rows in db  */
    short  media_class;
    short  print_data;
    short  n_sensors;
    short  n_params;
    short  first_time;
    long   n_warning;
    short  n_diffs;
    char   char_added[20];
    long   n_added;

    long sec_clock;       /* Number of seconds returned by time */
    struct tm *tm_ptr;   /* tm is defined in system library include
                        file time.h */

    pnt_dataset_t  pnt_dataset_1st;
    pnt_dataset_t  pnt_dataset_db;
    pnt_dataset_t  pnt_dataset;


    /*
    **  parse the ODL file using odl parsing system
    */
    pnt_keyword_1st = (IMS_KEYWORD_LIST * ) malloc( sizeof(
        IMS_KEYWORD_LIST ) );
    status = ims_parseODLFile( msgDesc, odl_name, NULL,
        &pnt_keyword_1st );
    if(  status <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "Error in parsing ODL file %s", odl_name );
        return( status );
    }

    if(  save_params  ==  1 ){
        (void) strcpy( char_added, " [added]" );
    }
    else{
        (void) strcpy( char_added, "" );
    }


    /*
    **  For each dataset, we have the following set:
    **  GROUP = DATASET
    **    DATASET_ID = "JERS-1 SAR Computer Compatible Signal Data"
    **    CAMPAIGN = ""
    **    SOURCE = "JERS-1"
    **    SENSOR = ("SAR",
    **              "RADAR")
    **    PARAMETER = ("Radar Cross-Section",
    **                 "Radar Backscatter")
    **    PROCESSING_LEVEL = "1A"
    **    DAY_NIGHT_FLAG = ""
    **    GROUP = DATASET_COVERAGE
    **      SPATIAL  = "Global"
    **      TEMPORAL = "20 May 92 - Present"
    **    END_GROUP = DATASET_COVERAGE
    **
    **    GROUP = GRANULE_COVERAGE
    **      SPATIAL  = "75 KM X 100 KM"
    **      TEMPORAL = "15 Seconds"
    **    END_GROUP = GRANULE_COVERAGE
    **
    **    MD_ENTRY_ID = "ASF02JE1"
    **    GROUP = BROWSE
    **          FTP = "no"
    **          INTEGRATED = "no"
    **    END_GROUP = BROWSE
    **    FTP_PRODUCT_AVAILABLE = "no"
    **  END_GROUP = DATASET
    **
    **  keep the values as read
    **
    */

    pnt_dataset_1st = (pnt_dataset_t) malloc( sizeof( dataset_t ) );
    pnt_dataset = pnt_dataset_1st;
    pnt_dataset->platform[0] = '\0';
    pnt_dataset->n_sensors = 0;
    for( i = 0; i  <  6 ; i++ )
        pnt_dataset->sensor[i][0] = '\0';
    pnt_dataset->dataset[0] = '\0';
    pnt_dataset->dataset_idx = -1;
    pnt_dataset->md_id[0] = '\0';
    pnt_dataset->campaign[0] = '\0';
    pnt_dataset->daynight_flag = ' ';   /*  Y or N (caps)  */
    pnt_dataset->processing_level = -1; /* if -1, none input  */
    pnt_dataset->n_parameters = 0;
    for( i = 0 ; i  < 10 ; i++ )
        pnt_dataset->parameters[i][0] = '\0';
    pnt_dataset->pnt_next = NULL;

    pnt_dataset_db = (pnt_dataset_t) malloc( sizeof( dataset_t ) );
    pnt_dataset = pnt_dataset_db;
    pnt_dataset->platform[0] = '\0';
    pnt_dataset->n_sensors = 0;
    for( i = 0; i  <  6 ; i++ )
        pnt_dataset->sensor[i][0] = '\0';
    pnt_dataset->dataset[0] = '\0';
    pnt_dataset->dataset_idx = -1;
    pnt_dataset->md_id[0] = '\0';
    pnt_dataset->campaign[0] = '\0';
    pnt_dataset->daynight_flag = ' ';   /*  Y or N (caps)  */
    pnt_dataset->processing_level = -1; /* if -1, none input  */
    pnt_dataset->n_parameters = 0;
    for( i = 0 ; i  < 10 ; i++ )
        pnt_dataset->parameters[i][0] = '\0';
    pnt_dataset->pnt_next = NULL;

    pnt_dataset = pnt_dataset_1st;

    flag = TRUE;
    n_sum_out = 0;
    first_time = TRUE;
    n_datasets = 0;
    n_params = 0;
    n_sensors = 0;
    n_warning = 0;
    n_added = 0;

    pnt_keyword = pnt_keyword_1st;
    while(  pnt_keyword  !=  NULL ){
        cnt_keys++;
        (void) strcpy( str, pnt_keyword->keyword );
        i = strlen( str );
        (void) strcpy( str2, pnt_keyword->value_string );
        if(  strcmp( str, "DATASET_ID" )  ==  0  ){
            /* this is dataset name  */
            if(  !first_time  ){
                /*
                **  need to do the work: check the last dataset
                **      and if flagged, save the parameters and
                **      check the other values
                */
                status = get_policy( msgDesc, pnt_dataset->dataset,
                    pnt_dataset_db );
                (void)  ims_msg(  msgDesc, IMS_INFO,
                    "***** Processing dataset %d, '%s'.",
                    pnt_dataset_db->dataset_idx,
                    pnt_dataset->dataset );
                if(  status  <  IMS_OK ){
                    (void) ims_msg( msgDesc, IMS_FATAL,
                        "Could not get info for dataset." );
                    return( IMS_ERROR );
                }
                if(  pnt_dataset_db->dataset_idx  ==  -1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Could not find dataset in DB." );
                }
                else{
                    pnt_dataset->dataset_idx =
                        pnt_dataset_db->dataset_idx;
                    /*
                    **  now compare the values.  if necessary,
                    **      save the parameters.
                    */
                    if(  strcmp( pnt_dataset->platform,
                        pnt_dataset_db->platform )  !=  0  ){
                        (void)  ims_msg( msgDesc, IMS_WARNING,
                            "Valids platform '%s' not same as DB "
                            "platform '%s'.", pnt_dataset->platform,
                            pnt_dataset_db->platform );
                        n_warning++;
                    }
                    if(  strcmp( pnt_dataset->md_id,
                        pnt_dataset_db->md_id )  !=  0  ){
                        (void)  ims_msg( msgDesc, IMS_WARNING,
                            "Valids MD_ID '%s' not same as DB '%s'.",
                            pnt_dataset->md_id, pnt_dataset_db->md_id );
                        n_warning++;
                    }
                    if(  pnt_dataset->campaign[0]  !=  '\0'  &&
                        strcmp( pnt_dataset->campaign,
                        pnt_dataset_db->campaign )  !=  0  ){
                        (void)  ims_msg( msgDesc, IMS_WARNING,
                            "Valids campaign '%s' not same as DB '%s'.",
                            pnt_dataset->campaign,
                            pnt_dataset_db->campaign );
                        n_warning++;
                    }
                    flag = TRUE;
                    for( i=0 ; i  <  pnt_dataset->n_sensors ; i++ ){
                        if(  strcmp( pnt_dataset_db->sensor[0],
                            pnt_dataset->sensor[i] )  ==  0  )
                            flag = FALSE;
                        else{
                            (void) ims_msg( msgDesc, IMS_WARNING,
                                "Valids sensor '%s' not supported"
                                " in DB.",  pnt_dataset->sensor[i] );
                        }
                    }
                    if(  flag  &&  pnt_dataset_db->sensor[0]  !=  '\0'
                        ){
                        (void)  ims_msg( msgDesc, IMS_WARNING,
                            "DB sensor '%s' not included in valids.",
                            pnt_dataset_db->sensor );
                        n_warning++;
                    }
                    if(  pnt_dataset->processing_level  !=  -1  &&
                        (pnt_dataset->processing_level  !=
                        pnt_dataset_db->processing_level )  ){
                        (void)  ims_msg( msgDesc, IMS_WARNING,
                            "Valids processing level %d not same DB "
                            "level %d.", pnt_dataset->processing_level,
                            pnt_dataset_db->processing_level );
                        n_warning++;
                    }
                    /*
                    **  now check the params.  if any differences, need
                    **      to change the db if asked for.
                    */
                    n_diffs = 0;
                    for( i=0 ; i  <  pnt_dataset->n_parameters ; i++ ){
                        flag = FALSE;
                        for( j=0 ; j < pnt_dataset_db->n_parameters ;
                            j++ ){
                            if(  strcmp( pnt_dataset->parameters[i],
                                pnt_dataset_db->parameters[j])  ==  0 )
                                flag = TRUE;
                        }
                        if(  !flag  ){
                            (void) ims_msg( msgDesc, IMS_WARNING,
                                "Valids parameter '%s' not found in "
                                "DB%s.", pnt_dataset->parameters[i],
                                char_added );
                            n_warning++;
                            n_diffs++;
                            if(  save_params  ==  1 ){
                                status = save_param( msgDesc,
                                    pnt_dataset->dataset_idx,
                                    pnt_dataset->parameters[i] );
                                if(  status  <  IMS_OK ){
                                    (void) ims_msg( msgDesc, IMS_ERROR,
                                        "Valids parameter not able to"
                                        " be saved in DB." );
                                    return( IMS_ERROR );
                                }
                                n_added++;
                            }
                        }
                    }
                    for( i=0 ; i  <  pnt_dataset_db->n_parameters ;
                        i++ ){
                        flag = FALSE;
                        for( j=0 ; j < pnt_dataset->n_parameters ; j++
                            ){
                            if(  strcmp( pnt_dataset_db->parameters[i],
                                pnt_dataset->parameters[j] )  ==  0  )
                                flag = TRUE;
                        }
                        if(  !flag  ){
                            (void) ims_msg( msgDesc, IMS_WARNING,
                                "DB parameter '%s' not found in "
                                "Valids.",
                                pnt_dataset_db->parameters[i] );
                            n_warning++;
                            n_diffs++;
                        }
                    }
                }
            }
            n_datasets++;
            /*
            **  make sure all upper case characters in dataset
            */
            i = strlen( str2 );
            for( j=0 ; j  <  i ; j++ )  if(  islower( str2[j] ) )
                str2[j] = toupper( str2[j] );
            (void) strcpy( pnt_dataset->dataset, str2 );
            first_time = FALSE;
            n_params = 0;
            n_sensors = 0;

            pnt_dataset->platform[0] = '\0';
            pnt_dataset->n_sensors = 0;
            for( i = 0; i  <  6 ; i++ )
                pnt_dataset->sensor[i][0] = '\0';
            pnt_dataset->dataset_idx = -1;
            pnt_dataset->md_id[0] = '\0';
            pnt_dataset->campaign[0] = '\0';
            pnt_dataset->daynight_flag = ' ';   /*  Y or N (caps)  */
            pnt_dataset->processing_level = -1; /* if -1, none input  */
            pnt_dataset->n_parameters = 0;
            for( i = 0 ; i  < 10 ; i++ )
                pnt_dataset->parameters[i][0] = '\0';
        }
        else  if(  strcmp( str, "SOURCE" )  ==   0 ){
            (void) strcpy( pnt_dataset->platform, str2 );
        }
        else  if(  strcmp( str, "SENSOR" )  ==  0 ){
            n_sensors++;
            (void) strcpy( pnt_dataset->sensor[n_sensors-1], str2 );
            pnt_dataset->n_sensors = n_sensors;
        }
        else  if(  strcmp( str, "PARAMETER" )  ==  0 ){
            n_params++;
            i = strlen( str2 );
            for( j=0 ; j  <  i ; j++ )  if(  islower( str2[j] ) )
                str2[j] = toupper( str2[j] );
            (void) strcpy( pnt_dataset->parameters[n_params-1], str2 );
            pnt_dataset->n_parameters = n_params;
        }
        else  if(  strcmp(  str, "PROCESSING_LEVEL" )  ==  0 ){
            if(  str2[0]  ==  '\0'  ||  str2[0]  ==  ' ' )
                pnt_dataset->processing_level = -1;
            else  if(  str2[0]  ==  '1'  )
                pnt_dataset->processing_level = 1;
            else  if(  str2[0]  ==  '2'  )
                pnt_dataset->processing_level = 2;
        }
        else  if(  strcmp(   str, "DAY_NIGHT_FLAG" )  ==  0 ){
            i = strlen( str2 );
            for( j=0 ; j  <  i ; j++ )  if(  islower( str2[j] ) )
                str2[j] = toupper( str2[j] );
            if(  str2[0]  ==  '\0'  ||  str2[0]  ==  ' ' )
                pnt_dataset->daynight_flag = ' ';
            else  pnt_dataset->daynight_flag = str2[0];
        }
        else  if(  strcmp( str, "MD_ENTRY_ID" )  ==  0 ){
            (void) strcpy( pnt_dataset->md_id, str2 );
        }
        else  if(  strcmp( str, "CAMPAIGN" )  ==  0 ){
            (void) strcpy( pnt_dataset->campaign, str2 );
        }

        pnt_keyword_save = pnt_keyword;
        pnt_keyword = pnt_keyword->next;
        (void) free( pnt_keyword_save );
    }
    if(  n_warning  ==  0  ){
        (void) ims_msg( msgDesc, IMS_INFO,
            "Sucessful completion of ims_valids: no warnings." );
    }
    else{
        if(  n_added  ==  0  ){
            (void) ims_msg( msgDesc, IMS_INFO,
                "Finished ims_valids with %ld warnings.", n_warning );
        }
        else{
            (void) ims_msg( msgDesc, IMS_INFO,
                "Finished ims_valids with %ld warnings, %ld "
                "parameters added.", n_warning, n_added );
        }
    }
    return (IMS_OK);
} /* read_valids */


/***************************************************************
**
**  subr get_policy  reads the dataset_policy to fill in the
**      pnt_dataset_db structure.  the parameters are filled in
**      another call.
**
**************************************************************** */
static  int get_policy(
    IMS_MSG_STRUCT *msgDesc,
    char*  dataset,
    pnt_dataset_t  pnt_dataset_db )
{
    int status;
    short  i_s;
    short  rowCount;
    char  str[256];



    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select dp.dataset_idx, dp.process_level, dr.md_id, "
        "dp.day_night, dr.sensor, dr.platform, dp.campaign  from   "
        "dataset_relation dr, dataset_policy dp  where  dr.dataset "
        " =  '%s'  and  dr.dataset_idx = dp.dataset_idx", dataset );
    /*
    ** Execute the command.
    */
    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }
        rowCount++;

        /*
        **  now get the values
        */
        (void) memcpy( &i_s, qDesc->valAddr[0], qDesc->valLength[0]);
        pnt_dataset_db->dataset_idx = i_s;

        (void) memcpy( &i_s, qDesc->valAddr[1], qDesc->valLength[1]);
        pnt_dataset_db->processing_level = i_s;

        (void) memcpy( str, qDesc->valAddr[2], qDesc->valLength[2]);
        str[qDesc->valLength[2]] = '\0';
        (void) ims_truncStr( str );
        (void) strcpy( pnt_dataset_db->md_id, str );

        (void) memcpy( str, qDesc->valAddr[3], qDesc->valLength[3]);
        str[qDesc->valLength[3]] = '\0';
        (void) ims_truncStr( str );
        pnt_dataset_db->daynight_flag = str[0];
        if( qDesc->valLength[3]  ==  0  )
            pnt_dataset_db->daynight_flag = ' ';

        (void) memcpy( str, qDesc->valAddr[4], qDesc->valLength[4]);
        str[qDesc->valLength[4]] = '\0';
        (void) ims_truncStr( str );
        (void) strcpy( pnt_dataset_db->sensor[0], str );
        pnt_dataset_db->n_sensors = 1;

        (void) memcpy( str, qDesc->valAddr[5], qDesc->valLength[5]);
        str[qDesc->valLength[5]] = '\0';
        (void) ims_truncStr( str );
        (void) strcpy( pnt_dataset_db->platform, str );

        (void) memcpy( str, qDesc->valAddr[6], qDesc->valLength[6]);
        str[qDesc->valLength[6]] = '\0';
        (void) ims_truncStr( str );
        (void) strcpy( pnt_dataset_db->campaign, str );

    }
    /*
    ** See if we got one row returned.
    */
    if( rowCount < 1 )
    {
        pnt_dataset_db->dataset_idx = -1;
    }
    else{
        /*
        **  now we need to get the parameters for this dataset
        */
        status = get_params( msgDesc, pnt_dataset_db );
        /*
        **  do not count this if an error: assume 0 params
        */
        if(  status  <  IMS_OK  )  pnt_dataset_db->n_parameters = -1;
    }
    return (IMS_OK);
} /* get_policy  */


/***************************************************************
**
**  subr get_params  reads the parameters to fill in the
**      pnt_dataset_db structure.
**
**************************************************************** */
static  int get_params(
    IMS_MSG_STRUCT *msgDesc,
    pnt_dataset_t  pnt_dataset_db )
{
    int status;
    short  i_s;
    short  rowCount;
    char  str[256];


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select parameter  from  dataset_parameter  where  dataset_idx"
        "  =  %d", pnt_dataset_db->dataset_idx );

    /*
    ** Execute the command.
    */
    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            return (status);
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }
        rowCount++;

        /*
        **  now get the value
        */
        (void) memcpy( str, qDesc->valAddr[0], qDesc->valLength[0]);
        str[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr( str );
        (void) strcpy( pnt_dataset_db->parameters[rowCount-1], str );

    }
    /*
    ** See if we got one row returned.
    */
    pnt_dataset_db->n_parameters = rowCount;
    return (IMS_OK);
} /* get_params  */


/***************************************************************
**
**  subr save_param saves one parameter to the db, a row.
**
**************************************************************** */
static  int save_param(
    IMS_MSG_STRUCT *msgDesc,
    short  dataset_idx,
    char  *param )
{
    int status;
    short  i_s;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "save_param: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "insert into dataset_parameter( dataset_idx, parameter ) "
        "  values( %d, '%s' )", dataset_idx, param );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    return( IMS_OK );
}   /*  save_param  */


/***************************************************************
**
**  subr ims_openQueryConnection2 ()
**
** Open a database server connection.
**
**************************************************************** */
static  int ims_openQueryConnection2 (  IMS_MSG_STRUCT *msgDesc,
    char * program )
{
    int status;


    /*
    ** Open the connection.
    */
    if( (qDesc = openConnection ( msgDesc, program ) )
        == (IMS_QI_DESC_OBJ *) NULL)
    {
        status = ims_msgGetSeverity (msgDesc);
        return (status);
    }

    return (IMS_OK);
}  /* ims_openQueryConnection2 */


/***************************************************************
**
**  subr openConnection ()
**
** Open a database server connection.
**
**************************************************************** */
static IMS_QI_DESC_OBJ *openConnection (
    IMS_MSG_STRUCT *msgDesc, char * program )
{
    int status;


    /*
    ** Since this is the first time to access the catalog, we
    ** need a query descriptor allocated.  If we can't get a
    ** descriptor, return with a bad status ... we can't go on.
    */
    if ((qDesc = ims_qiDescAlloc (msgDesc)) ==
        (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, commands.username);
    IMS_SETPSWD (qDesc, commands.password);
    IMS_SETPROG (qDesc, program);

    if( commands.server  !=  NULL ){
        if ((int) strlen (commands.server) > 0){
            IMS_SETSERVER (qDesc, commands.server);
        }
    }

    if( commands.database  !=  NULL ){
        if ((int) strlen ( commands.database) > 0){
            IMS_SETDBNAME (qDesc, commands.database);
        }
    }

    IMS_SET_VERBOSE (qDesc, 10);

    /*
    ** Login to the catalog database.
    */
    if ((status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        (void) ims_qiFreeDesc (qDesc);
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (qDesc);
}  /* openConnection */


/*******************************************************************
**
**  subr ims_closeConnection ()
**
** Close a database server connection.
**
******************************************************************** */
static  int ims_closeConnection (
    IMS_MSG_STRUCT *msgDesc, IMS_QI_DESC_OBJ *qDesc)
{
    int status;

    if( qDesc != (IMS_QI_DESC_OBJ *) NULL)
    {
        if ((status = ims_qiFreeDesc( qDesc) ) < IMS_OK)
        {
            (void) ims_msg (msgDesc, status,
                "Could not free the query descriptor.");
            return (status);
        }
    }

    qDesc = (IMS_QI_DESC_OBJ *) NULL;
    return (IMS_OK);
}  /* ims_closeConnection */


/***************************************************************
**
**  subr execCmd ()
**
** Execute a query. This function should only be used for commands
** that return one or no rows.
**
**************************************************************** */

static int execCmd (
    IMS_MSG_STRUCT *msgDesc, IMS_QI_DESC_OBJ  *qDesc )
{
    int status;


    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION){
        if(  status  <  IMS_OK  )  return (status);
    }

    /*
    ** Check the stored procedure status returned value.
    */
    if (checkRetStatus (msgDesc) < IMS_OK){
        return (IMS_ERROR);
    }

    return (IMS_OK);
}  /* execCmd */

/****************************************************************
**
**  subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
***************************************************************** */

static int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc )
{
    int procReturn;
    int severity;

    /*
    ** Check to see if the Sybase procedure returned a status. If it did
    ** and it is not 0 (the OK value for a return), deal with the error.
    ** Return status of less than -100 correspond to message facility
    ** severity levels modulo 100.
    */
    if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
    {
        if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
        {
            if (procReturn == -103)
            {
                severity = IMS_FATAL;
            }
            else if (procReturn == -102)
            {
                severity = IMS_ERROR;
            }
            else if (procReturn == -101)
            {
                severity = IMS_WARNING;
            }
            else
            {
                severity = IMS_ERROR;
            }
            (void) ims_msg (msgDesc, severity,
                "Sybase procedure '%s' returned a status of %d",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }

    return (IMS_OK);
}  /* checkRetStatus */
