static char *sccs = "@(#)ims_package.c	1.4  08/15/97";
/* *************************************************************
**
** File:        ims_package.c
**
** Function:    A client program that concerns package.odl.  It
**              does 4 things:  checks to see discrepencies between
**              a package.odl and the IMS database, updates the
**              IMS database from the pacage.odl file (includes the
**              verify function), will generate a package.odl from
**              the IMS datasbase (the packaging_options table), and
**              will check the internal consistency between the cost
**              tables (dataset_cost, media_cost) and the
**              packaging_options table.
**
** Author:      David Pass
**
** Date:        10/19/95
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
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
static  int  update_db( IMS_MSG_STRUCT *, char *, char *,
    char * );
static  int  check_db( IMS_MSG_STRUCT *, char * );
static  int  clear_db( IMS_MSG_STRUCT * );
static  int  gen_odl( IMS_MSG_STRUCT *, char *, char * );

static int get_process( IMS_MSG_STRUCT *, char *, char *, char *,
    char *, short *, short );
static  int get_type_ary( IMS_MSG_STRUCT *, char [][], char [][],
    short [], short * );
static int get_format_ary( IMS_MSG_STRUCT *, char [][], char [][],
    short [], short * );
static int get_class_ary( IMS_MSG_STRUCT *, short [], short );
static int check_dataset(  IMS_MSG_STRUCT *, char *, char *, char *,
    char *, short *, short * );
static  int check_process_media(  IMS_MSG_STRUCT *, short, short,
    short, int * );
static int get_table_costs( IMS_MSG_STRUCT *, char *, char *,
    char *, char *, short, short, float * );
static int get_pack_opts( IMS_MSG_STRUCT *, char *, char *, char *,
    char *, char *, char *, char *, char * );
static int update_pack_opts( IMS_MSG_STRUCT *, char *, char *,
    char *, char *, char *, char *, char *, char * );
static  int put_pack_row( IMS_MSG_STRUCT *, char *, char *, char *,
    char *, char *, char *, char *, char * );

static int get_process( IMS_MSG_STRUCT *, char *, char *, char *,
    char *, short *, short );
static  int get_type_ary( IMS_MSG_STRUCT *, char [][], char [][],
    short [], short * );
static int get_format_ary( IMS_MSG_STRUCT *, char [][], char [][],
    short [], short * );
static int get_class_ary( IMS_MSG_STRUCT *, short [], short );
static  int check_dataset(  IMS_MSG_STRUCT *, char *, char *,
    char *, char *, short *, short * );


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
    char *event; /*  this is a flag for determining what action
            to do:  4 possible modes are possible.  verify: check
            for discrepencies between the odl file and the IMS
            database, update: check for discrepencies between the odl
            file and the database, and change the database to the odl
            values, generate: read the IMS database and generate
            a package.odl file from the packaging_options table, and
            internal: check for internal consistencey between the
            packaging_options table and the two cost tables.
            Note that for the generate option, the media_cost and
            dataset_cost are not generated.  Note also that only the
            first character of this keyword is used, and it can be
            upper or lower case.  note also that the update case
            includes the verify option.
            thus the possible values are:
                verify      compare odl and database
                update      update database to be the same as odl
                setup       first clear and then update as above
                generate    generate odl file from database
                internal    check the database for internal consistency.
            */
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
    {"-E",              &commands.event},
    {"+event",          &commands.event},
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
    {"event",           &commands.event},
    {"server",          &commands.server},
    {"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
static char cmdBuf[IMS_COL512_LEN];

/*
**  define the buffers to save the packaging_options rows
*/
typedef  struct  pack__t *pnt_pack_t;
typedef  struct  one_row__t {
    char  platform[31];
    char  sensor[31];
    char  dataset[81];
    char  option_id[31];
    char  type_id[31];
    char  format_id[31];
    char  package_size[31];
    char  ims_visible_p[2];
        }  one_row_t ;

typedef  struct   pack__t {
    pnt_pack_t   pnt_next;   /*  next buf - null if not allocated */
    one_row_t   ary[MAX_PACK_BUF] ;  /*  current buffer - see
        n_pack_tot (total number of members) and n_pack (no. in
        current buffer)  */
    }   pack_t ;

/*
** Global Variables
*/
static  char *glb_programName;
static  short  print_msg;
static  short  print_log;
static  FILE  *  out_file;

static  IMS_QI_DESC_OBJ *qDesc;

/* *************************************************************
**
** main ()
**
** This is the driver for the package program.  it has three
**      modes:  it reads the package.odl file and compares it with the
**      database (check), it reads the odl file and compares, and if
**      there is a difference, it will put the differences into the
**      database (update), and it will read the database and generate
**      an odl file from the packaging_options table (generate).
**  The database basically has three tables that are important.  the
**      dataset_cost table has the default cost of any dataset and the
**      media_cost has the default cost of any type of media generated.
**      however, there can be exceptions.  These are in the
**      packaging_optins table.  if an entry is not here, then the two
**      cost tables are used.  the ims_visible_p flag is from the
**      dataset_policy table.
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

    commands.username = NULL;
    commands.password = NULL;
    commands.odlFile = NULL;
    commands.logFile = NULL;
    commands.event = NULL;
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

    print_msg = IMS_FALSE;
    print_log = IMS_TRUE;
    if(  commands.logFile  ==  NULL  ){
        print_log = IMS_FALSE;
        print_msg = IMS_TRUE;
    }

    if(  isupper( commands.event[0] ) )
        commands.event[0] = tolower( commands.event[0] );
    if(  commands.event[0]  ==  'v'  ||
        commands.event[0]  ==  'u' ){
        /*
        **  read odl file: verify or update the database
        */
        if ((status = update_db( msgDesc, commands.odlFile,
            commands.logFile, commands.event ) ) <
            IMS_WARNING ){
            goto ERROR;
        }
    }
    else  if(  commands.event[0]  ==  's' ){
        /*
        **  setup the database: clear it, then update.
        */
        if( (status = clear_db( msgDesc ) )
            <  IMS_WARNING ){
            (void)  ims_msg( msgDesc, IMS_ERROR,
                "Could not clear the table packaging_options." );
            if(  print_log ){
                if ((out_file = fopen (commands.logFile, "w")) ==
                    NULL){
                    status = IMS_ERROR;
                    (void) ims_msg (msgDesc, status,
                        "ERROR in opening log file %s",
                        commands.logFile );
                    goto ERROR;
                }
                (void) fprintf( out_file,
                    "ERROR: Could not clear the table "
                    "packaging_options." );
            }
            goto ERROR;
        }
        if ((status = update_db( msgDesc, commands.odlFile,
            commands.logFile, commands.event ) ) <
            IMS_WARNING ){
            goto ERROR;
        }
    }
    else  if(  commands.event[0]  ==  'i' ){
        /*
        **  check the internal consistency of the database.
        */
        if ((status = check_db( msgDesc, commands.logFile ) )
            <   IMS_WARNING ){
            goto ERROR;
        }
    }
    else  if(  commands.event[0]  ==  'g' ){
        /*
        **  generate the odl file from the packaging_options table
        */
        if ((status = gen_odl( msgDesc, commands.odlFile,
            commands.logFile ) )  <  IMS_WARNING){
            goto ERROR;
        }
    }

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The ims_package program was successful using event option %s.",
        commands.event );
    if(  print_log   ){
        (void) fprintf( out_file,
            "The ims_package program was successful using event "
            "option %s.\n", commands.event );
        (void)  fclose( out_file );
    }
    status = ims_closeConnection( msgDesc, qDesc );
    (void) ims_msgStructFree (msgDesc);
    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    if(  commands.event  ==  NULL  ){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The ims_package program had errors." );
    }
    else{
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The ims_package program had errors using option %s.",
            commands.event);
    }
    if(  print_log  &&  out_file  !=  NULL ){
        (void) fprintf( out_file,
            "The ims_package program had errors using event "
            "option %s.\n", commands.event );
        (void)  fclose( out_file );
    }
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
                "ERROR detected while reading input string.");
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
                "ERROR detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.password = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.password, inputBuffer);
    }

    /* event */
    try_again:;
    if (commands.event  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "event (verify,update,generate,internal): ") ==
                (char *) NULL){
            (void) ims_msg (msgDesc, IMS_FATAL,
                "ERROR detected while reading input string.");
            return (IMS_FATAL);
        }
        if(  isupper( inputBuffer[0] ) )
            inputBuffer[0] = tolower( inputBuffer[0] );
        if(  inputBuffer[0]  !=  'v'  &&  inputBuffer[0]  !=  'u'  &&
            inputBuffer[0]  !=  'g'  &&  inputBuffer[0]  !=  'i'  &&
            inputBuffer[0]  !=  's'    ){
            (void) ims_msg (msgDesc, IMS_INFO,
                "First character not valid: try again." );
            goto try_again;
        }
        i = strlen( inputBuffer )+1;
        commands.event = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.event, inputBuffer);
        if(  isupper( commands.event[0] ) )
            commands.event[0] = tolower( commands.event[0] );
    }
    else{
        if(  isupper( commands.event[0] ) )
            commands.event[0] = tolower( commands.event[0] );
        if(  commands.event[0]  !=  'v'  &&  commands.event[0]  !=  'u'
            &&  commands.event[0]  !=  'g'  &&
            commands.event[0]  !=  's'  &&
            commands.event[0]  !=  'i'  ){
            (void) ims_msg (msgDesc, IMS_ERROR,
                "ERROR: first character of event not valid." );
            return( IMS_ERROR );
        }
    }

    /* odlFile */
    if (commands.odlFile  ==  (char *) NULL  &&  commands.event[0]  !=
        'i'  ){
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Odl file name: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "ERROR detected while reading input string.");
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
**  subr update_db
**
** This event does all the work for the check and update options.
**
**************************************************************** */
static  int update_db(  IMS_MSG_STRUCT *msgDesc, char *  odl_name,
    char *  log_name, char * event )
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
    int  num_errors;

    char  type_values[20][MAX_ID_LEN]; /*  type_id values  */
    char  type_values2[20][MAX_ID_LEN]; /*  corr media_type names */
    short  idx_type_values[20]; /* media_type values */
    short  n_type_values;   /* no. of type_values entries */

    char  format_values[20][MAX_ID_LEN]; /*  format_id values  */
    char  format_values2[20][MAX_ID_LEN]; /*  corr format names */
    short  idx_format_values[20]; /* media_fmt_type values */
    short  idx_class_values[20]; /* given a media_fmt_type
                value, a media_class exists (media_fmt_class_map) */
    short  n_format_values; /* no. of format_values entries */

    short  dataset_idx; /*  from dataset_relation  */

    short  media_type;  /* index from type_id  */
    short  media_fmt_type;  /*  index from format_id  */
    short  process_type;    /*  index from option_id and dataset */
    short  this_error;  /* error(s) for this set  */
    short  order_item_type; /* from dataset_policy: APR,RPR,FPR,COR,
                TDR,TSR,DAR  */
    short  flag_ended; /* last time through, the last cost
            must be processed: this flags that case  */
    long  n_sum_out; /*  no. lines in the output file */
    IMS_KEYWORD_LIST * pnt_keyword;
    IMS_KEYWORD_LIST * pnt_keyword_save;
    IMS_KEYWORD_LIST * pnt_keyword_1st;
    long  cnt_keys; /*  no. of keywords read  */
    long  n_costs;  /* cntr for tot number of approx. cost values */
    long  n_datasets;  /* cntr for tot number of datasets  */
    long  n_new_rows; /* cntr for no. of new rows added to db  */
    long  n_updates; /* cntr for no. of updates to rows in db  */
    short  media_class;
    short  print_data;
    long  cost_errors;

    long sec_clock;       /* Number of seconds returned by time */
    struct tm *tm_ptr;   /* tm is defined in system library include
                        file time.h */
    float  cost_total;
    float  f_temp;
    double  d_temp;


    /*
    ** Initialize variables.  do selects to get the type_id,
    **      format_id values.  The option_id corresponds to
    **      v0_process_type in process_type_map, but needs the
    **      dataset to identify.
    */
    status = get_type_ary( msgDesc, type_values, type_values2,
        idx_type_values, &n_type_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_type_map values." );
        return (status);
    }
    status = get_format_ary( msgDesc, format_values, format_values2,
        idx_format_values, &n_format_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_fmt_map values." );
        return (status);
    }
    status = get_class_ary( msgDesc, idx_class_values, n_type_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_class values." );
        return (status);
    }

    /*
    **  now parse the ODL file using odl parsing system
    */
    pnt_keyword_1st = (IMS_KEYWORD_LIST * ) malloc( sizeof(
        IMS_KEYWORD_LIST ) );
    status = ims_parseODLFile( msgDesc, odl_name, NULL,
        &pnt_keyword_1st );
    if(  status <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in parsing ODL file %s", odl_name );
        return( status );
    }

    if(  print_log  ){
        if ((out_file = fopen (log_name, "w")) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in opening log file %s", log_name );
            return( status );
        }
        (void) fprintf( out_file, "/***********************************"
            "***********************\n" );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "** File: %s\n", log_name );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file,"** Created by ims_package from %s.\n",
            odl_name );
        (void) fprintf( out_file, "**\n" );

        /* Get the system time and pointer to tm record structure. If
           the address of sec_clock and tm_ptr are NULL, then return
           error status code. */
        (void)  time(&sec_clock);
        if (&sec_clock == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting time." );
            return( status );
        }
        if ((tm_ptr = localtime(&sec_clock)) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting local time." );
            return( status );
        }
        (void) sprintf(  str, "%2d:%2d:%2d",  tm_ptr->tm_hour,
            tm_ptr->tm_min, tm_ptr->tm_sec   );
        if(  tm_ptr->tm_hour  <  10  )  str[0] = '0';
        if(  tm_ptr->tm_min   <  10  )  str[3] = '0';
        if(  tm_ptr->tm_sec   <  10  )  str[6] = '0';
        tm_ptr->tm_mon++;
        (void) sprintf(  str2, "%2d/%2d/%2d",  tm_ptr->tm_mon,
            tm_ptr->tm_mday, tm_ptr->tm_year  );
        if(  tm_ptr->tm_mon    <  10  )  str2[0] = '0';
        if(  tm_ptr->tm_mday   <  10  )  str2[3] = '0';

        (void) fprintf( out_file, "** Date/time:  %s   %s\n", str2,
            str );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "************************************"
            "************************ */\n\n" );
    }

    /*
    **  For each cost value, we have the following set:
    **    DATASET_ID = "ERS-1 SAR COMPUTER COMPATIBLE SIGNAL DATA"
    **            OPTION_ID = "NORMAL"
    **            PACKAGE_SIZE = "300.1 MBYTES"
    **    TYPE_ID = "4MM 2GB CARTRIDGE"
    **        FORMAT_ID = "CEOS SAR TAPE"
    **        APPROX_COST = 30.0
    **    END_GROUP = MEDIA_FORMAT
    **
    **  Once we read approx_cost, then process the
    **      values we have.  need to collect data for dataset,
    **      option_id, type_id, format_id, approx_cost.
    **
    */
    dataset[0] = '\0';
    option_id[0] = '\0';
    type_id[0] = '\0';
    format_id[0] = '\0';
    approx_cost[0] = '\0';
    ims_visible_p[0] = '\0';
    package_size[0] = '\0';

    flag = TRUE;
    num_errors = 0;
    cost_errors = 0;
    cnt_keys = 0;
    if(  print_log  )  n_sum_out = 10;
    else  n_sum_out = 0;
    n_costs = 0;
    n_datasets = 0;
    n_new_rows = 0;
    n_updates = 0;
    cnt_keys = 0;
    pnt_keyword = pnt_keyword_1st;
    while(  pnt_keyword  !=  NULL ){
        cnt_keys++;
        (void) strcpy( str, pnt_keyword->keyword );
        i = strlen( str );
        (void) strcpy( str2, pnt_keyword->value_string );
        if(  strcmp( str, "END_MEDIA_FORMAT" )  ==  0  ){
            /* finished collecting: now do the work. */
            /*  first check the dataset: if not found,
                print error. also, other parameters. */
            this_error = 0;
            status = check_dataset( msgDesc, platform,
                sensor, ims_visible_p, dataset,
                &order_item_type, &dataset_idx );
            ims_visible_p[1] = '\0';
            if(  status  <  IMS_OK ){
                (void) strcpy( platform, "ERROR" );
                (void) strcpy( sensor, "ERROR" );
                (void) strcpy( ims_visible_p, "E" );
                num_errors++;
                this_error++;
            }
            if(  order_item_type  <=  0  ){
                (void) ims_msg (msgDesc, IMS_INFO,
                    "order_item_type = 0 for "
                    " dataset = %s.", dataset );
                if(  print_log ){
                    (void) fprintf( out_file,
                        "order_item_type = 0 for "
                        " dataset = %s.\n", dataset );
                }
            }

            /* now try to match up the input strings */
            flag = TRUE;
            for( i=0 ; i  <  n_type_values  &&  flag ; i++ ){
                if(  strcmp( type_values[i], type_id )  ==  0 ){
                    /* match: get the index */
                    media_type = idx_type_values[i];
                    flag = FALSE;
                }
            }
            if(  flag ){
                status = IMS_ERROR;
                (void) ims_msg (msgDesc, status,
                    "ERROR in obtaining media_type:"
                    " type_id = %s.", type_id );
                if(  print_log ){
                    (void) fprintf( out_file,
                        "ERROR in obtaining media_type:"
                        " type_id = %s.\n", type_id );
                }
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
                media_type = -99;
            }
            flag = TRUE;
            for( i=0 ; i  <  n_format_values  &&  flag ; i++ ){
                if(  strcmp( format_values[i], format_id )  ==
                    0 ){
                    /* match: get the index */
                    media_fmt_type = idx_format_values[i];
                    flag = FALSE;
                }
            }
            media_class = -1;
            if(  flag ){
                status = IMS_ERROR;
                (void) ims_msg (msgDesc, status,
                    "ERROR in obtaining "
                    "media_fmt_type: format_id = %s.",
                    format_id );
                if(  print_log ){
                    (void) fprintf( out_file,
                        "ERROR in obtaining "
                        "media_fmt_type: format_id = %s.\n",
                        format_id );
                }
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
                media_fmt_type = -99;
            }
            else{
                /*
                **  get media_class value
                */
                media_class = idx_class_values[media_type];
                if(  media_class  ==  -1  ){
                    status = IMS_ERROR;
                    (void) ims_msg (msgDesc, status,
                        "ERROR in obtaining "
                        "media_class: format_id = %s.",
                        format_id );
                    if(  print_log ){
                        (void) fprintf( out_file,
                            "ERROR in obtaining "
                            "media_class: format_id = %s.\n",
                            format_id );
                    }
                    if(  this_error  ==  0  ){
                        num_errors++;
                        this_error++;
                    }
                    media_class = -99;
                }
            }
            status = get_process( msgDesc, platform, sensor,
                dataset, option_id, &process_type,
                order_item_type );
            if(  status  <  IMS_OK ){
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
                process_type = -99;
            }
            /*
            **  now check to see if this combination is allowed.
            */
            if(  dataset_idx  >=  0  &&  process_type  >=  0  &&
                media_class  >=  0  ){
                status = check_process_media( msgDesc, dataset_idx,
                    process_type, media_class, &i );
                if(  status  <  IMS_OK ){
                    if(  this_error  ==  0  ){
                        num_errors++;
                        this_error++;
                    }
                }
                else  if(  i  ==  0  ){
                    /*
                    **  no row was found - this combination is not
                    **      processed by ims.
                    */
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "ERROR: This combination of process_type and "
                        "format type not allowed for this dataset."
                        "  dataset = '%s'", dataset );
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "    process '%s', format type '%s'.",
                        option_id, format_id );
                    if(  print_log ){
                        (void) fprintf( out_file,
                            "ERROR: This combination of process_type "
                            "and format type not allowed for this "
                            "dataset '%s'\n", dataset );
                        (void) fprintf( out_file,
                            "    process '%s', format type '%s'.\n",
                            option_id, format_id );
                        n_sum_out += 2;
                        if(  this_error  ==  0  ){
                            num_errors++;
                            this_error++;
                        }
                    }
                }
            }
            if(  this_error  ==  0  ){
                print_data = FALSE;
                /*  first check if this entry is in the
                    packaging_options.  if not, then
                    the two xxx_cost tables are used. */
                str[0] = '\0';
                status = get_pack_opts( msgDesc, platform,
                    sensor, dataset, option_id, type_id,
                    format_id, str, str3 );
                    /*  str = package_size,
                        str3 = visible_flag */
                if(  str[0]  !=  '\0'  ){
                    /*
                    **  the row was found: now check to see if
                    **      the cost and other parameters are
                    **      the same
                    */
                    if(  strcmp( package_size, str )  !=  0  ||
                        strcmp( ims_visible_p, str3 )  !=  0 ) {
                        /*
                        **  we have a difference: print
                        */
                        if(  this_error  ==  0  ){
                            num_errors++;
                            this_error++;
                        }
                        if(  print_msg ){
                            print_data = TRUE;
                            (void) ims_msg( msgDesc, IMS_INFO,
                                " ****  db and ODL diff:"
                                "  line %ld  dataset = %s",
                                cnt_keys, dataset );
                            (void) ims_msg( msgDesc, IMS_INFO,
                                "       media,format,option = %s"
                                "    %s    %s",
                                type_id, format_id, option_id );
                            (void) ims_msg( msgDesc, IMS_INFO,
                                "       cost = %s   "
                                "new package_size = %s   "
                                "new ims_visible_p = %s",
                                approx_cost, package_size,
                                ims_visible_p );
                        }
                        if(  print_log ){
                            print_data = TRUE;
                            (void) fprintf( out_file,
                                "\n ERROR:  db and ODL diff:"
                                "  line %ld  dataset = %s\n",
                                cnt_keys, dataset );
                            (void) fprintf( out_file,
                                "       media,format,option = %s"
                                "    %s    %s\n",
                                type_id, format_id, option_id );
                            (void) fprintf( out_file,
                                "       cost = %s   "
                                "new package_size = %s   "
                                "new ims_visible_p = %s\n",
                                approx_cost, package_size,
                                ims_visible_p );
                            n_sum_out += 3;
                        }
                        if(  event[0]  ==  'u' ||  event[0]  ==  's' ){
                            /*
                            **  need to modify row.
                            */
                            status = update_pack_opts( msgDesc,
                                platform, sensor, dataset, option_id,
                                type_id, format_id, package_size,
                                ims_visible_p );
                            if(  status  <  IMS_OK ){
                                return( status );
                            }
                            n_updates++;
                        }
                    }
                }
                else{
                    /*
                    **  the package option not found: the tables
                    **      dataset_cost and media_cost are then
                    **      used instead to determine cost.
                    **      check the odl value with their sum.
                    */
                    if(  print_msg ){
                        print_data = TRUE;
                        (void) ims_msg( msgDesc, IMS_INFO,
                            " ****  Adding packaging_options row for:"
                            " line %5ld  dataset = %s",
                            cnt_keys, dataset );
                        (void) ims_msg( msgDesc, IMS_INFO,
                            "       media,format,option, cost = %s"
                            "    %s    %s  %s", type_id,
                            format_id, option_id, approx_cost );
                    }
                    if(  print_log ){
                        print_data = TRUE;
                        (void) fprintf( out_file,
                            " ****  Adding packaging_options row for:"
                            " line %5ld  dataset = %s\n",
                            cnt_keys, dataset );
                        (void) fprintf( out_file,
                            "       media,format,option, cost = %s"
                            "    %s    %s  %s\n", type_id,
                            format_id, option_id, approx_cost );
                        n_sum_out += 2;
                    }
                    if(  event[0]  ==  'u' ||  event[0]  ==  's' ){
                        /*
                        **  if update and cost is valid, need to
                        **  put row in db.
                        */
                        status = put_pack_row( msgDesc, platform,
                            sensor, dataset, option_id, type_id,
                            format_id, package_size,
                            ims_visible_p );
                        if(  status  <  IMS_OK ){
                            return( status );
                        }
                        n_new_rows++;
                    }
                }
                /*
                **  get the cost from the db and compare with odl
                */
                status = get_table_costs( msgDesc, platform, sensor,
                    dataset, option_id, media_type, media_fmt_type,
                    &cost_total );
                if(  status  <  IMS_OK )  return( status );
                f_temp = cost_total;
                i = 10.0 * f_temp+.01;
                j = i % 10;
                i = i/10;
                (void) sprintf ( str, "%d.%d", i, j );
                (void) sprintf ( str2, "%d", i );
                if(  strcmp( str, approx_cost )  !=  0  &&
                    strcmp( str2, approx_cost )  !=  0   ){
                    /* check for media ignored */
                    cost_errors++;
                    /*
                    **  make sure that the info for this item
                    **      is printed out
                    */
                    if(  !print_data  ){
                        if(  print_msg ){
                            (void) ims_msg( msgDesc, IMS_INFO,
                                " ** ODL item from:"
                                " line %5ld  dataset = %s",
                                cnt_keys, dataset );
                            (void) ims_msg( msgDesc, IMS_INFO,
                                "       media,format,option, cost = %s"
                                "    %s    %s  %s", type_id,
                                format_id, option_id, approx_cost );
                        }
                        if(  print_log ){
                            (void) fprintf( out_file,
                                " **  ODL item from:"
                                " line %5ld  dataset = %s\n",
                                cnt_keys, dataset );
                            (void) fprintf( out_file,
                                "       media,format,option, cost = %s"
                                "    %s    %s  %s\n", type_id,
                                format_id, option_id, approx_cost );
                            n_sum_out += 2;
                        }
                    }
                    if(  print_msg ){
                        (void) ims_msg( msgDesc, IMS_INFO,
                            " ********  WARNING: cost difference: "
                            "total db cost = %10f  ********",
                            cost_total );
                    }
                    if(  print_log ){
                        (void) fprintf( out_file,
                            " ********  WARNING: cost difference: "
                            "total db cost = %10f  ********\n\n",
                            cost_total );
                        n_sum_out += 2;
                    }
                }
            }
        }
        else  if(  strcmp( str, "DATASET_ID" )  ==   0 ){
            (void) strcpy( dataset, str2 );
            n_datasets++;
        }
        else  if(  strcmp( str, "OPTION_ID" )  ==  0 ){
            (void) strcpy( option_id, str2 );
        }
        else  if(  strcmp(  str, "TYPE_ID" )  ==  0 ){
            (void) strcpy( type_id, str2 );
        }
        else  if(  strcmp(   str, "FORMAT_ID" )  ==  0 ){
            (void) strcpy( format_id, str2 );
        }
        else  if(  strcmp( str, "APPROX_COST" )  ==  0 ){
            d_temp = pnt_keyword->value_real;
            if( d_temp  <=  0.00001  ){
                /*
                **  has quotes around it maybe. so use
                **      the character value
                */
                sscanf( str2, "%f", &f_temp );
                d_temp = f_temp;
            }
            i = d_temp*10.0+.01;
            j = i % 10;
            i = i/10;
            (void) sprintf ( approx_cost, "%d.%d", i, j );
            n_costs++;
        }
        else  if(  strcmp( str, "PACKAGE_SIZE" )  ==  0 ){
            (void) strcpy( package_size, str2 );
        }
        pnt_keyword_save = pnt_keyword;
        pnt_keyword = pnt_keyword->next;
        (void) free( pnt_keyword_save );
    }
    (void) ims_msg( msgDesc, IMS_INFO,
        "***  %d errors, %ld cost differences, %ld cost values, "
        "%ld datasets, approx. %ld lines input, %ld lines output.",
        num_errors, cost_errors, n_costs, n_datasets, cnt_keys,
        n_sum_out );
    if(  event[0]  ==  'u' ||  event[0]  ==  's' ){
        (void)  ims_msg( msgDesc, IMS_INFO,
            "      No. of updates = %ld    No. of new rows = %ld",
            n_updates, n_new_rows );
    }
    if(  print_log ){
        n_sum_out += 3;
        if(  event[0]  ==  'u' ||  event[0]  ==  's' )  n_sum_out++;
        (void) fprintf( out_file,
        "\n\n\n***  %d errors, %ld cost differences, %ld cost values, "
        "%ld datasets, approx. %ld lines input, %ld lines output.\n",
        num_errors, cost_errors, n_costs, n_datasets, cnt_keys,
        n_sum_out );
        if(  event[0]  ==  'u' ||  event[0]  ==  's' ){
            (void)  fprintf( out_file,
                "      No. of updates = %ld    No. of new rows = %ld\n",
                n_updates, n_new_rows );
        }
    }
    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    if(  num_errors  >  0  )  return (IMS_ERROR);
    else    return (IMS_OK);
} /* update_db */


/***************************************************************
**
**  subr get_type_ary  reads the media_type_map table to obtain
**      the ary values.
**
**************************************************************** */
static  int get_type_ary(
    IMS_MSG_STRUCT *msgDesc,
    char  ary[][MAX_ID_LEN],
    char  ary2[][MAX_ID_LEN],
    short  idx_ary[],
    short * n_ary   )
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
        "select v0_media_type, media_type, i.description from "
        " media_type_map, items i  where  i.type = 'media_type'"
        "  and i.instance = media_type  order by media_type"  );
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
        (void) memcpy( str, qDesc->valAddr[0], qDesc->valLength[0]);
        str[qDesc->valLength[0]] = '\0';
        (void) strcpy( &ary[rowCount-1][0], str );
        (void) ims_truncStr( &ary[rowCount-1][0]  );

        (void) memcpy( &i_s, qDesc->valAddr[1], qDesc->valLength[1]);

        (void) memcpy( str, qDesc->valAddr[2], qDesc->valLength[2]);
        str[qDesc->valLength[2]] = '\0';
        (void) strcpy( &ary2[rowCount-1][0], str );
        (void) ims_truncStr( &ary2[rowCount-1][0]  );


        idx_ary[rowCount-1] = i_s;
    }
    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not obtain a media_type_map row." );
        return (IMS_ERROR);
    }

    *n_ary = rowCount;
    return (IMS_OK);
} /* get_type_ary  */


/***************************************************************
**
**  subr get_format_ary  reads the media_fmt_map table to obtain
**      the ary values.
**
**************************************************************** */
static int get_format_ary(
    IMS_MSG_STRUCT *msgDesc,
    char ary[][MAX_ID_LEN],
    char ary2[][MAX_ID_LEN],
    short  idx_ary[],
    short * n_ary )
{
    int status;
    short  rowCount;
    char  str[129];
    short  i_s;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
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
        "select v0_media_fmt_type, media_fmt_type, i.description from "
        " media_fmt_map, items i  where  i.type = 'media_fmt_type'"
        "  and  i.instance = media_fmt_type  order by media_fmt_type" );
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
        (void) memcpy( str, qDesc->valAddr[0],
            qDesc->valLength[0]);
        str[qDesc->valLength[0]] = '\0';
        (void) strcpy( &ary[rowCount-1][0], str );
        (void) ims_truncStr( &ary[rowCount-1][0]  );

        (void) memcpy( &i_s, qDesc->valAddr[1],
            qDesc->valLength[1]);
        idx_ary[rowCount-1] = i_s;

        (void) memcpy( str, qDesc->valAddr[2],
            qDesc->valLength[2]);
        str[qDesc->valLength[2]] = '\0';
        (void) strcpy( &ary2[rowCount-1][0], str );
        (void) ims_truncStr( &ary2[rowCount-1][0]  );
    }
    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "Could not obtain a media_type_map row." );
        return (IMS_ERROR);
    }

    *n_ary = rowCount;
    return (IMS_OK);
} /* get_format_ary  */


/***************************************************************
**
**  subr get_class_ary  reads the media_fmt_class_map table to
**      obtain the ary values.
**
**************************************************************** */
static  int get_class_ary(
    IMS_MSG_STRUCT *msgDesc,
    short  idx_ary2[],
    short  n_ary )
{
    int status;
    long i,j;
    short  rowCount;
    short  i_s;

    for( i=0 ; i  <  n_ary ; i++ )  idx_ary2[i] = -1;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
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
        "select media_type, media_class from "
        " media_type_class_map" );
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
        (void) memcpy( &i_s, qDesc->valAddr[0],
            qDesc->valLength[0]);
        j = i_s;

        (void) memcpy( &i_s, qDesc->valAddr[1],
            qDesc->valLength[1]);
        if(  j  >   -1  )  idx_ary2[j] = i_s;
    }
    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "ERROR: Could not obtain a media_type_class_map row." );
        return (IMS_ERROR);
    }
    return (IMS_OK);
} /* get_class_ary  */


/***************************************************************
**
**  subr get_process  reads the process_type_map to get the
**      process_type.  the dataset name is needed.
**
**************************************************************** */
static int get_process(
    IMS_MSG_STRUCT *msgDesc,
    char  platform[],
    char  sensor[],
    char  dataset[],
    char  v0_process_type[],
    short * process_type,
    short order_item_type )
{
    int status;
    long j;
    short  rowCount;
    char  str[129];
    short  i_s;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        if(  print_log )  (void) fprintf( out_file,
            "***  get_process: ERROR: Could not reset the query "
            "descriptor.\n");
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
    (void) strcpy( str, ims_truncStr( sensor ) );
    j = strlen( str );
    if(  j  ==  0  )  (void) strcpy( str, "null" );

    (void) sprintf (cmdBuf,
        "select process_type  from "
        " process_type_map where dataset = '%s'"
        "  and  v0_process_type = '%s'"
        "  and  platform = '%s'   and  sensor = '%s'",
        dataset, v0_process_type, platform, str );

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
        (void) memcpy( &i_s, qDesc->valAddr[0],
            qDesc->valLength[0]);
        *process_type = i_s;
    }
    /*
    ** See if we got one row returned.
    */
    if( rowCount < 1)
    {
        /*
        **  if order_item_type is 1, then this is an apr and
        **      thus has no processing.  in this case, this
        **      processing index is not necessary.
        */
        *process_type = 0;
        if(  order_item_type  !=  1  ){
            (void) ims_msg (msgDesc, IMS_ERROR,
               "ERROR: Could not obtain a process_type for dataset %s, "
                "platform %s, sensor \"%s\", process type %s.",
                dataset, platform, sensor, v0_process_type );
            if(  print_log ){
                (void) fprintf( out_file,
               "ERROR: Could not obtain a process_type for dataset %s, "
                "platform %s, sensor \"%s\", process type %s.",
                dataset, platform, sensor, v0_process_type );
            }
            return (IMS_ERROR);
        }
    }
    if (IMS_AFFECTED (qDesc) > 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "ERROR: More than 1 row for process_type for dataset %s, "
            "platform %s, sensor \"%s\", process type %s.",
            dataset, platform, sensor, v0_process_type );
        if(  print_log ){
            (void) fprintf( out_file,
                "ERROR: More than 1 row for process_type for dataset "
                "%s, platform %s, sensor \"%s\", process type %s.",
                dataset, platform, sensor, v0_process_type );
        }
        return (IMS_ERROR);
    }
    return (IMS_OK);
} /* get_process */


/***************************************************************
**
**  subr get_table_costs reads the dataset_cost and media_cost to
**      obtain their costs.
**
**************************************************************** */
static int get_table_costs(
    IMS_MSG_STRUCT *msgDesc,
    char * platform,
    char * sensor,
    char * dataset,
    char * option_id,   /* v0_process_type  */
    short media_type,
    short media_fmt_type,
    float * cost_total )
{
    int status;
    short  rowCount;


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "get_table_cost: ERROR: Could not reset the query "
            "descriptor.");
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
        "v0_get_item_cost  '%s', '%s', '%s', '%s', %d, %d, 1",
        platform, sensor, dataset, option_id, media_type,
        media_fmt_type );
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
        (void) memcpy( cost_total,
            qDesc->valAddr[0], qDesc->valLength[0]);
    }
    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
           "ERROR: Could not obtain a dataset cost for dataset '%s'.",
            dataset );
        if( print_log )  {
            (void)  fprintf( out_file,
                "ERROR: Could not obtain a dataset cost for dataset "
                "'%s'.", dataset );
        }
        return (IMS_ERROR);
    }
    return ( status );
} /* get_table_costs  */


/***************************************************************
**
**  subr check_dataset  reads the dataset_relation table to check
**      the dataset.  the platform and sensor is also returned.
**
**************************************************************** */
static  int check_dataset(
    IMS_MSG_STRUCT *msgDesc,
    char  platform[],
    char  sensor[],
    char  ims_visible_p[],
    char  * dataset,
    short * order_item_type,
    short * dataset_idx )
{
    int status;
    short  i_s;
    static  char  last_dataset[IMS_COL80_LEN+1] = "";
    static  char  last_platform[MAX_ID_LEN];
    static  char  last_sensor[MAX_ID_LEN];
    static  char  last_ims_visible_p[2];
    static  short  last_order_item_type;
    static  short  last_dataset_idx;

    if(  strcmp( last_dataset, dataset )  ==  0  ){ /* same dataset:
        put in the values  */
        (void) strcpy( platform, last_platform );
        (void) strcpy( sensor, last_sensor );
        *order_item_type = last_order_item_type;
        *dataset_idx = last_dataset_idx;
        return( IMS_OK );
    }

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "check_dataset: ERROR: Could not reset the query "
            "descriptor.");
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
        "select d.platform, d.sensor, p.order_item_type, "
        " p.ims_visible_p, d.dataset_idx  from  dataset_relation d, "
        "dataset_policy p   where  dataset = '%s'  and  "
        "p.dataset_idx = d.dataset_idx", dataset );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "check_dataset: execCmd had ERROR.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, IMS_ERROR,
            "check_dataset: No rows for dataset %s.",
            dataset );
        if(  print_log ){
            (void) fprintf( out_file,
                "check_dataset: ERROR: No rows for dataset %s.\n",
                dataset );
        }
        return (IMS_ERROR);
    }
    if (IMS_AFFECTED (qDesc) > 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "check_dataset: More than one row for dataset %s.",
            dataset );
        if(  print_log ){
            (void) fprintf( out_file,
                "check_dataset: ERROR: More than one row for "
                "dataset %s.\n", dataset );
        }
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    (void) memcpy ( platform, qDesc->valAddr[0], qDesc->valLength[0]);
    platform[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr ( platform);

    (void) memcpy ( sensor, qDesc->valAddr[1], qDesc->valLength[1]);
    sensor[qDesc->valLength[1]] = '\0';
    (void) ims_truncStr ( sensor);

    (void) memcpy ( &i_s, qDesc->valAddr[2], qDesc->valLength[2]);
    *order_item_type = i_s;

    (void) memcpy ( ims_visible_p, qDesc->valAddr[3],
        qDesc->valLength[3]);
    ims_visible_p[qDesc->valLength[3]] = '\0';
    ims_visible_p[1] = '\0';

    (void) memcpy ( &i_s, qDesc->valAddr[4], qDesc->valLength[4]);
    *dataset_idx = i_s;

    (void) strcpy( last_dataset, dataset );
    (void) strcpy( last_platform, platform );
    (void) strcpy( last_sensor, sensor );
    (void) strcpy( last_ims_visible_p, ims_visible_p );
    last_order_item_type = *order_item_type;
    last_dataset_idx = *dataset_idx;
    return( IMS_OK );
}   /*  check_dataset  */


/***************************************************************
**
**  subr check_process_media  reads the dataset_process_media table
**      to check if this process_type, media_class is valid for this
**      dataset.
**
**************************************************************** */
static  int check_process_media(
    IMS_MSG_STRUCT *msgDesc,
    short  dataset_idx,
    short  process_type,
    short  media_class,
    int  *  n_rows  )
{
    int status;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "check_process_media: Could not reset the query "
            "descriptor.");
        if(  print_log )  (void) fprintf( out_file,
            "check_process_media: ERROR: Could not reset the query "
            "descriptor.");

        *n_rows = -1;
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
    if(  process_type  <= 0  ){
        (void) sprintf (cmdBuf,
            "v0_verify_process_media %d, null, %d", dataset_idx,
            media_class );
    }
    else{
        (void) sprintf (cmdBuf,
            "v0_verify_process_media %d, %d, %d", dataset_idx,
            process_type, media_class );
    }
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        *n_rows = 0;
        return (IMS_OK);
    }

    *n_rows = 1;
    return( IMS_OK );
}   /*  check_process_media  */


/***************************************************************
**
**  subr clear_db clears the table packaging_options.
**
**************************************************************** */
static  int clear_db(
    IMS_MSG_STRUCT *msgDesc )
{
    int status;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "clear_db: Could not reset the query "
            "descriptor.");
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
        "delete from packaging_options" );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        return (IMS_ERROR);
    }
    return( IMS_OK );
}   /*  clear_db  */


/***************************************************************
**
**  subr get_pack_opts reads the packaging_options table to obtain
**      any values for the platform, sensor, dataset, V0_process_type
**      V0_media_type, and v0_media_fmt_type.  there is a good chance
**      that there is no rows: this is not an error.
**
**************************************************************** */
static  int get_pack_opts(
    IMS_MSG_STRUCT *msgDesc,
    char  * platform,
    char  * sensor,
    char  * dataset,
    char  * v0_process_type,    /*  option_id  */
    char  * v0_media_type,      /*  type_id    */
    char  * v0_media_fmt_type,  /*  format_id  */
    char  * package_size,   /* last three args are output  */
    char  * ims_visible_p  )
{
    int status;
    short  rowCount;


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "get_pack_opts: Could not reset the query descriptor.");
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
        "select package_size, ims_visible_p  "
        " from packaging_options  where  platform = '%s' "
        "  and sensor = '%s'  and  dataset = '%s'  and  "
        " v0_process_type = '%s'  and  v0_media_type = '%s' "
        "  and  v0_media_fmt_type = '%s'",
        platform, sensor, dataset, v0_process_type, v0_media_type,
        v0_media_fmt_type );
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
        (void) memcpy( package_size, qDesc->valAddr[0],
            qDesc->valLength[0]);
        package_size[qDesc->valLength[0]] = '\0';
        (void) ims_truncStr( package_size  );

        (void) memcpy( ims_visible_p, qDesc->valAddr[1],
            qDesc->valLength[1]);
        ims_visible_p[1] = '\0';
    }
    return (IMS_OK);
} /* get_pack_opts  */


/***************************************************************
**
**  subr update_pack_opts updates the packaging_options table to save
**      the new vlaues.
**
**************************************************************** */
static  int update_pack_opts(
    IMS_MSG_STRUCT *msgDesc,
    char  * platform,
    char  * sensor,
    char  * dataset,
    char  * v0_process_type,    /*  option_id  */
    char  * v0_media_type,      /*  type_id    */
    char  * v0_media_fmt_type,  /*  format_id  */
    char  * package_size,   /* last three args are output  */
    char  * ims_visible_p  )
{
    int status;


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "update_pack_opts: Could not reset the query descriptor.");
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
        "update packaging_options  set  "
        "  package_size = '%s', ims_visible_p = '%s'  where  "
        "  platform = '%s'   and sensor = '%s'  and  dataset = '%s'  "
        "  and  v0_process_type = '%s'  and  v0_media_type = '%s' "
        "  and  v0_media_fmt_type = '%s'",
        package_size, ims_visible_p, platform, sensor,
        dataset, v0_process_type, v0_media_type, v0_media_fmt_type );
    /*
    ** Execute the command.
    */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "update_pack_opts: Update failed.");
        if(  print_log )  (void) fprintf( out_file,
            "update_pack_opts: ERROR: Update failed.");
        status = IMS_ERROR;
        return (status);
    }

    if (IMS_AFFECTED (qDesc) > 1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "update_pack_opts: Update affected more than 1 row.");
        if(  print_log )  (void) fprintf( out_file,
            "update_pack_opts: ERROR: Update affected more than 1 row."
            );
        status = IMS_ERROR;
        return (status);
    }
    return (IMS_OK);
} /* update_pack_opts  */


/***************************************************************
**
**  subr put_pack_row creates a new row of the new values.
**
**************************************************************** */
static  int put_pack_row(
    IMS_MSG_STRUCT *msgDesc,
    char  * platform,
    char  * sensor,
    char  * dataset,
    char  * v0_process_type,    /*  option_id  */
    char  * v0_media_type,      /*  type_id    */
    char  * v0_media_fmt_type,  /*  format_id  */
    char  * package_size,   /* last three args are output  */
    char  * ims_visible_p  )
{
    int status;


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "put_pack_row: Could not reset the query descriptor.");
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
        "insert into packaging_options( platform, sensor, dataset, "
        "  v0_process_type, v0_media_type, v0_media_fmt_type, "
        "  package_size, ims_visible_p )  values( "
        "  '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s' )",
        platform, sensor, dataset, v0_process_type, v0_media_type,
        v0_media_fmt_type, package_size, ims_visible_p );
    /*
    ** Execute the command.
    */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "put_pack_row: Could not add row.");
        if(  print_log ){
            (void) fprintf( out_file,
                "put_pack_row: ERROR: Could not add row.\n");
        }
        status = IMS_ERROR;
        return (status);
    }
    return (IMS_OK);
} /* put_pack_row  */


/***************************************************************
**
**  subr check_db
**
** This event does all the work for the internal option.
**      The rows of the packaging_options are checked with the
**      dataset_cost and media_cost tables to determine if any
**      differences are there.  also, the v0 values are checked
**      to determine if they are explicitly there.  the odl file
**      is not read.
**
**************************************************************** */
static  int check_db(  IMS_MSG_STRUCT *msgDesc, char *  log_name )

{
    char  dataset[256+1];
    char  option_id[MAX_ID_LEN]; /* option from odl file */
    char  type_id[MAX_ID_LEN];   /* type from odl file  */
    char  format_id[MAX_ID_LEN]; /* format from odl file */
    char  platform[MAX_ID_LEN];
    char  sensor[MAX_ID_LEN];
    char  ims_visible_p[MAX_ID_LEN];
    char  package_size[MAX_ID_LEN];

    int status;
    int  i,j;
    char  str[129], str2[129];
    unsigned char  flag;
    int  num_errors;

    char  type_values[20][MAX_ID_LEN]; /*  type_id values  */
    char  type_values2[20][MAX_ID_LEN]; /*  corr media_type names */
    short  idx_type_values[20]; /* media_type values */
    short  n_type_values;   /* no. of type_values entries */

    char  format_values[20][MAX_ID_LEN]; /*  format_id values  */
    char  format_values2[20][MAX_ID_LEN]; /*  corr format names */
    short  idx_format_values[20]; /* media_fmt_type values */
    short  idx_class_values[20]; /* given a media_fmt_type
                value, a media_class exists (media_fmt_class_map) */
    short  n_format_values; /* no. of format_values entries */

    short  media_type;  /* index from type_id  */
    short  media_fmt_type;  /*  index from format_id  */
    short  process_type;    /*  index from option_id and dataset */
    short  this_error;  /* error(s) for this set  */
    short  order_item_type; /* from dataset_policy: APR,RPR,FPR,COR,
                TDR,TSR,DAR  */
    short  dataset_idx;  /*  from dataset_relation  */
    long  n_sum_out; /*  no. lines in the output file */
    short  media_class;

    long sec_clock;       /* Number of seconds returned by time */
    struct tm *tm_ptr;   /* tm is defined in system library include
                        file time.h */
    int  rowCount;


    /* ************  this describes the package buffers - one_row is a
        single row.  kept in a buffer because need selects to analyze.
           */
    pnt_pack_t  pnt_pack_1st, pnt_pack, pnt_pack2;
    long  n_pack_tot;  /*  tot no. of rows in packaging_options */
    long  n_pack;   /* no. of rows in current buffer  */


    /*
    ** Initialize variables.  do selects to get the type_id,
    **      format_id values.  The option_id corresponds to
    **      v0_process_type in process_type_map, but needs the
    **      dataset to identify.
    */
    if(  print_log ){
        if ((out_file = fopen (log_name, "w")) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in opening log file %s", log_name );
            return( status );
        }
    }

    status = get_type_ary( msgDesc, type_values, type_values2,
        idx_type_values, &n_type_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_type_map values." );
        if(  print_log )  (void) fprintf( out_file,
            "ERROR in obtaining media_type_map values." );
        return (status);
    }
    status = get_format_ary( msgDesc, format_values, format_values2,
        idx_format_values, &n_format_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_fmt_map values." );
        if(  print_log )  (void) fprintf( out_file,
            "ERROR in obtaining media_fmt_map values." );
        return (status);
    }
    status = get_class_ary( msgDesc, idx_class_values, n_type_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_class values." );
        if(  print_log )  (void) fprintf( out_file,
            "ERROR in obtaining media_class values." );
        return (status);
    }

    if(  print_log ){
        (void) fprintf( out_file, "/***********************************"
            "***********************\n" );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "** File: %s\n", log_name );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "** Created by ims_package: check "
            "database.\n" );
        (void) fprintf( out_file, "**\n" );

        /* Get the system time and pointer to tm record structure. If
           the address of sec_clock and tm_ptr are NULL, then return
           error status code. */
        (void) time(&sec_clock);
        if (&sec_clock == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting time." );
            return( status );
        }
        if ((tm_ptr = localtime(&sec_clock)) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting local time." );
            return( status );
        }
        (void) sprintf(  str, "%2d:%2d:%2d",  tm_ptr->tm_hour,
            tm_ptr->tm_min, tm_ptr->tm_sec   );
        if(  tm_ptr->tm_hour  <  10  )  str[0] = '0';
        if(  tm_ptr->tm_min   <  10  )  str[3] = '0';
        if(  tm_ptr->tm_sec   <  10  )  str[6] = '0';
        tm_ptr->tm_mon++;
        (void) sprintf(  str2, "%2d/%2d/%2d",  tm_ptr->tm_mon,
            tm_ptr->tm_mday, tm_ptr->tm_year  );
        if(  tm_ptr->tm_mon    <  10  )  str2[0] = '0';
        if(  tm_ptr->tm_mday   <  10  )  str2[3] = '0';

        (void) fprintf( out_file, "** Date/time:  %s   %s\n", str2,
            str );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "************************************"
            "************************ */\n\n" );
    }

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
           "check_db: Could not reset the query descriptor.");
        if(  print_log )  (void) fprintf( out_file,
           "check_db: ERROR: Could not reset the query descriptor.");
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
        "select platform, sensor, dataset, v0_process_type, "
        " v0_media_type, v0_media_fmt_type, package_size, "
        " ims_visible_p  from  packaging_options"
        "  order by dataset, v0_process_type, v0_media_type" );
    /*
    ** Execute the command.
    */
    rowCount = 0;

    pnt_pack_1st = (pnt_pack_t)  malloc( sizeof( pack_t ) );
    pnt_pack = pnt_pack_1st;
    pnt_pack->pnt_next = NULL;

    n_pack_tot = 0;
    n_pack = -1;
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
        j = 0;                          /*  platform  */
        (void) memcpy ((char *) platform,
            qDesc->valAddr[j], qDesc->valLength[j]);
        platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( platform );

        j++;                            /*  sensor  */
        (void) memcpy ((char *) sensor,
            qDesc->valAddr[j], qDesc->valLength[j]);
        sensor[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( sensor );

        j++;                            /*  dataset  */
        (void) memcpy ((char *) dataset,
            qDesc->valAddr[j], qDesc->valLength[j]);
        dataset[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( dataset );

        j++;                            /*  v0_process_type  */
        (void) memcpy ((char *) option_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        option_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( option_id );

        j++;                            /*  v0_media_type  */
        (void) memcpy ((char *) type_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        type_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( type_id );

        j++;                            /*  v0_media_fmt_type  */
        (void) memcpy ((char *) format_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        format_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( format_id );

        j++;                            /*  package_size  */
        (void) memcpy ((char *) package_size,
            qDesc->valAddr[j], qDesc->valLength[j]);
        package_size[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( package_size );

        j++;                            /*  ims_visible_p  */
        (void) memcpy ((char *) ims_visible_p,
            qDesc->valAddr[j], qDesc->valLength[j]);
        ims_visible_p[qDesc->valLength[j]] = '\0';
        ims_visible_p[1] = '\0';

        /*
        **  now put the row into buffers
        */
        n_pack_tot++;
        n_pack++;
        if(  n_pack  >=  MAX_PACK_BUF  ){ /*  need new buffer  */
            pnt_pack2 = (pnt_pack_t) malloc( sizeof( pack_t ) );
            pnt_pack->pnt_next = pnt_pack2;
            pnt_pack = pnt_pack2;
            pnt_pack->pnt_next = NULL;
            n_pack = 0;
        }
        (void) strcpy( pnt_pack->ary[n_pack].platform, platform );
        (void) strcpy( pnt_pack->ary[n_pack].sensor, sensor );
        (void) strcpy( pnt_pack->ary[n_pack].dataset, dataset );
        (void) strcpy( pnt_pack->ary[n_pack].option_id, option_id );
        (void) strcpy( pnt_pack->ary[n_pack].type_id, type_id );
        (void) strcpy( pnt_pack->ary[n_pack].format_id, format_id );
        (void) strcpy( pnt_pack->ary[n_pack].package_size,
            package_size );
        (void) strcpy( pnt_pack->ary[n_pack].ims_visible_p,
            ims_visible_p );
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    **  now check each row
    */
    pnt_pack = pnt_pack_1st;
    num_errors = 0;
    if(  print_log  )  n_sum_out = 10;
    else  n_sum_out = 0;
    rowCount = 0;
    n_pack = -1;
    while(  rowCount  <  n_pack_tot ){
        rowCount++;
        n_pack++;
        if(  n_pack  >=  MAX_PACK_BUF  ){ /*  need new buffer  */
            pnt_pack2 = pnt_pack;
            pnt_pack = pnt_pack->pnt_next;
            (void) free( pnt_pack2 );
            n_pack = 0;
        }

    (void) strcpy( platform     , pnt_pack->ary[n_pack].platform      );
    (void) strcpy( sensor       , pnt_pack->ary[n_pack].sensor        );
    (void) strcpy( dataset      , pnt_pack->ary[n_pack].dataset       );
    (void) strcpy( option_id    , pnt_pack->ary[n_pack].option_id     );
    (void) strcpy( type_id      , pnt_pack->ary[n_pack].type_id       );
    (void) strcpy( format_id    , pnt_pack->ary[n_pack].format_id     );
    (void) strcpy( package_size , pnt_pack->ary[n_pack].package_size  );
    (void) strcpy( ims_visible_p, pnt_pack->ary[n_pack].ims_visible_p );


        /*  first check the dataset: if not found,
            print error. also, other parameters. */
        this_error = 0;
        status = check_dataset( msgDesc, platform,
            sensor, ims_visible_p, dataset,
            &order_item_type, &dataset_idx );
        if(  status  <  IMS_OK ){
            (void) strcpy( platform, "ERROR" );
            (void) strcpy( sensor, "ERROR" );
            (void) strcpy( ims_visible_p, "E" );
            num_errors++;
            this_error++;
        }
        /* now try to match up the input strings */
        flag = TRUE;
        for( i=0 ; i  <  n_type_values  &&  flag ; i++ ){
            if(  strcmp( type_values[i], type_id )  ==  0 ){
                /* match: get the index */
                media_type = idx_type_values[i];
                flag = FALSE;
            }
        }
        if(  flag ){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in obtaining media_type:"
                " type_id = %s.", type_id );
            if(  print_log ){
                (void) fprintf( out_file,
                    "ERROR in obtaining media_type:"
                    " type_id = %s.\n",  type_id );
                n_sum_out++;
            }
            if(  this_error  ==  0  ){
                num_errors++;
                this_error++;
            }
            media_type = -99;
        }
        media_fmt_type = -1;
        flag = TRUE;
        for( i=0 ; i  <  n_format_values  &&  flag ; i++ ){
            if(  strcmp( format_values[i], format_id )  ==
                0 ){
                /* match: get the index */
                media_fmt_type = idx_format_values[i];
                flag = FALSE;
            }
        }
        media_class = -1;
        if(  media_fmt_type  ==  -1  ){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in obtaining "
                "media_fmt_type: format_id = %s.",
                format_id );
            if(  print_log ){
                (void) fprintf( out_file,
                    "ERROR in obtaining "
                    "media_fmt_type: format_id = %s.",
                    format_id );
                n_sum_out++;
            }
            if(  this_error  ==  0  ){
                num_errors++;
                this_error++;
            }
            media_fmt_type = -99;
        }
        else{
            /*
            **  get media_class value
            */
            media_class = idx_class_values[media_type];
            if(  media_class  ==  -1  ){
                status = IMS_ERROR;
                (void) ims_msg (msgDesc, status,
                    "ERROR in obtaining "
                    "media_class: format_id = %s.",
                    format_id );
                if(  print_log ){
                    (void) fprintf( out_file,
                        "ERROR in obtaining "
                        "media_class: format_id = %s.\n",
                        format_id );
                }
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
                media_class = -99;
            }
        }
        status = get_process( msgDesc, platform, sensor,
            dataset, option_id, &process_type, order_item_type );
        if(  status  <  IMS_OK ){
            if(  this_error  ==  0  ){
                num_errors++;
                this_error++;
            }
            process_type = -99;
        }
        /*
        **  now check to see if this combination is allowed.
        */
        if(  dataset_idx  >=  0  &&  process_type  >=  0  &&
            media_class  >=  0  ){
            status = check_process_media( msgDesc, dataset_idx,
                process_type, media_class, &i );
            if(  status  <  IMS_OK ){
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
            }
            if(  i  ==  0  ){
                /*
                **  no row was found - this combination is not
                **      processed by ims.
                */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "This combination of process_type and "
                    "format type not allowed for this dataset."
                    "  dataset = '%s'", dataset );
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "    process '%s', format type '%s'.",
                    option_id, format_id );
                if(  print_log ){
                    (void) fprintf( out_file,
                        "ERROR: This combination of process_type and "
                        "format type not allowed for this dataset."
                        "  dataset = '%s'\n", dataset );
                    (void) fprintf( out_file,
                        "    process '%s', format type '%s'.\n",
                        option_id, format_id );
                    n_sum_out += 2;
                }
                if(  this_error  ==  0  ){
                    num_errors++;
                    this_error++;
                }
            }
        }
    }

    (void) free( pnt_pack );
    (void)  ims_msg( msgDesc, IMS_INFO,
        " ***  processed %ld rows  with %d errors  *****",
        n_pack_tot, num_errors );
    if(  print_log ){
        (void)  fprintf( out_file,
            "\n\n ***  processed %ld rows  with %d errors  *****\n",
            n_pack_tot, num_errors );
    }

    if(  num_errors  >  0  )  return (IMS_ERROR);
    else    return (IMS_OK);
} /* check_db */


/***************************************************************
**
**  subr gen_odl
**
** This function does all the work for the generate option.  it
**      generates the odl file from the packaging_options table.
**
**************************************************************** */
static  int gen_odl(  IMS_MSG_STRUCT *msgDesc, char *  odl_name,
    char *  log_name  )
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
    char  last_dataset[MAX_ID_LEN];
    char  last_option_id[MAX_ID_LEN];
    char  last_media_type[MAX_ID_LEN];
    IMS_ODL_TREE * pnt_tree, * pnt_tree_1st, * pnt_tree_save;
    IMS_ODL_TREE * pnt_tree_option; /*  place to put next
            processing_option group: null for first in package  */
    IMS_ODL_TREE * pnt_tree_n_options; /*  place to put no_of_optons */
    IMS_ODL_TREE * pnt_tree_n_process;/*  place to put no_of_process */
    IMS_ODL_TREE * pnt_tree_n_media;  /*  place to put no_of_media  */
    IMS_ODL_TREE * pnt_tree_package; /* place to put next
            package group  */
    IMS_ODL_TREE * pnt_tree_type; /* place to put next
            media_type group  */
    long  no_of_media_fmt;/* no. of media_format groups for the current
            media_type group  */
    long  no_of_options; /* no. of processing_option groups for the
            current dataset  */
    long  no_of_process;
    float  f_temp;
    long  n_costs;
    long  n_packages;

    char  type_values[20][MAX_ID_LEN]; /*  type_id values  */
    char  type_values2[20][MAX_ID_LEN]; /*  corr media_type names */
    short  idx_type_values[20]; /* media_type values */
    short  n_type_values;   /* no. of type_values entries */

    char  format_values[20][MAX_ID_LEN]; /*  format_id values  */
    char  format_values2[20][MAX_ID_LEN]; /*  corr format names */
    short  idx_format_values[20]; /* media_fmt_type values */
    short  n_format_values; /* no. of format_values entries */
    float  cost_total;
    short  media_type;  /* index from type_id  */
    short  media_fmt_type;  /*  index from format_id  */

    pnt_pack_t  pnt_pack_1st, pnt_pack, pnt_pack2;
    long  n_pack_tot;  /*  tot no. of rows in packaging_options */
    long  n_pack;   /* no. of rows in current buffer  */

    int status;
    int  i,j;
    char  str[129], str2[129];
    unsigned char  flag;
    long  rowCount;

    long sec_clock;       /* Number of seconds returned by time */
    struct tm *tm_ptr;   /* tm is defined in system library include
                        file time.h */

    FILE * odl_file;


    if(  print_log ){
        if ((out_file = fopen (log_name, "w")) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in opening log file %s", log_name );
            return( status );
        }
        (void) fprintf( out_file, "/***********************************"
            "***********************\n" );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "** File: %s\n", log_name );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "** Created by ims_package: generate "
            "odl file.\n" );
        (void) fprintf( out_file, "**\n" );

        /* Get the system time and pointer to tm record structure. If
           the address of sec_clock and tm_ptr are NULL, then return
           error status code. */
        (void)  time(&sec_clock);
        if (&sec_clock == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting time." );
            return( status );
        }
        if ((tm_ptr = localtime(&sec_clock)) == NULL){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in getting local time." );
            return( status );
        }
        (void) sprintf(  str, "%2d:%2d:%2d",  tm_ptr->tm_hour,
            tm_ptr->tm_min, tm_ptr->tm_sec   );
        if(  tm_ptr->tm_hour  <  10  )  str[0] = '0';
        if(  tm_ptr->tm_min   <  10  )  str[3] = '0';
        if(  tm_ptr->tm_sec   <  10  )  str[6] = '0';
        tm_ptr->tm_mon++;
        (void) sprintf(  str2, "%2d/%2d/%2d",  tm_ptr->tm_mon,
            tm_ptr->tm_mday, tm_ptr->tm_year  );
        if(  tm_ptr->tm_mon    <  10  )  str2[0] = '0';
        if(  tm_ptr->tm_mday   <  10  )  str2[3] = '0';

        (void) fprintf( out_file, "** Date/time:  %s   %s\n", str2,
            str );
        (void) fprintf( out_file, "**\n" );
        (void) fprintf( out_file, "************************************"
            "************************ */\n\n" );
    }
    /*
    ** Initialize variables.  do selects to get the type_id,
    **      format_id values.  The option_id corresponds to
    **      v0_process_type in process_type_map, but needs the
    **      dataset to identify.
    */
    status = get_type_ary( msgDesc, type_values, type_values2,
        idx_type_values, &n_type_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_type_map values." );
        if(  print_log  ){
            (void) fprintf( out_file,
                "ERROR in obtaining media_type_map values." );
        }
        return (status);
    }
    status = get_format_ary( msgDesc, format_values, format_values2,
        idx_format_values, &n_format_values );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "ERROR in obtaining media_fmt_map values." );
        if(  print_log  ){
            (void) fprintf( out_file,
                "ERROR in obtaining media_fmt_map values." );
        }
        return (status);
    }

    /*
    **  first we need to start reading the dataset.  for each row,
    **      add a MEDIA_FORMAT group to the odl file.  if the
    **      dataset is new, add a PACKAGE group.  if the media type
    **      is new, add a MEDIA_TYPE group.
    */
    last_dataset[0] = '\0';
    last_option_id[0] = '\0';
    last_media_type[0] = '\0'; /*   type_id in other places  */
    pnt_tree = NULL;
    pnt_tree_1st = NULL;
    pnt_tree_save = NULL;
    pnt_tree_n_process = NULL;
    pnt_tree_n_media = NULL;
    pnt_tree_n_options = NULL;
    no_of_options = 0;
    no_of_media_fmt = 0;
    no_of_process = 0;
    pnt_tree_package = NULL;
    n_costs = 0;
    n_packages = 0;

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "gen_odl: Could not reset the query descriptor.");
        if(  print_log  ){
            (void) fprintf( out_file,
                "get_odl: ERROR: Could not reset the query descriptor."
                );
        }
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
        "select platform, sensor, dataset, v0_process_type, "
        " v0_media_type, v0_media_fmt_type, package_size, "
        " ims_visible_p  from  packaging_options"
        "  order by dataset, v0_process_type, v0_media_type" );
    /*
    ** Execute the command.
    */
    rowCount = 0;

    pnt_pack_1st = (pnt_pack_t)  malloc( sizeof( pack_t ) );
    pnt_pack = pnt_pack_1st;
    pnt_pack->pnt_next = NULL;

    n_pack_tot = 0;
    n_pack = -1;

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
        j = 0;                          /*  platform  */
        (void) memcpy ((char *) platform,
            qDesc->valAddr[j], qDesc->valLength[j]);
        platform[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( platform );

        j++;                            /*  sensor  */
        (void) memcpy ((char *) sensor,
            qDesc->valAddr[j], qDesc->valLength[j]);
        sensor[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( sensor );

        j++;                            /*  dataset  */
        (void) memcpy ((char *) dataset,
            qDesc->valAddr[j], qDesc->valLength[j]);
        dataset[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( dataset );

        j++;                            /*  v0_process_type  */
        (void) memcpy ((char *) option_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        option_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( option_id );

        j++;                            /*  v0_media_type  */
        (void) memcpy ((char *) type_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        type_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( type_id );

        j++;                            /*  v0_media_fmt_type  */
        (void) memcpy ((char *) format_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        format_id[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( format_id );

        j++;                            /*  package_size  */
        (void) memcpy ((char *) package_size,
            qDesc->valAddr[j], qDesc->valLength[j]);
        package_size[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( package_size );

        j++;                            /*  ims_visible_p  */
        (void) memcpy ((char *) ims_visible_p,
            qDesc->valAddr[j], qDesc->valLength[j]);
        ims_visible_p[qDesc->valLength[j]] = '\0';
        ims_visible_p[1] = '\0';

        /*
        **  now put the row into buffers
        */
        n_pack_tot++;
        n_pack++;
        if(  n_pack  >=  MAX_PACK_BUF  ){ /*  need new buffer  */
            pnt_pack2 = (pnt_pack_t) malloc( sizeof( pack_t ) );
            pnt_pack->pnt_next = pnt_pack2;
            pnt_pack = pnt_pack2;
            pnt_pack->pnt_next = NULL;
            n_pack = 0;
        }
        (void) strcpy( pnt_pack->ary[n_pack].platform, platform );
        (void) strcpy( pnt_pack->ary[n_pack].sensor, sensor );
        (void) strcpy( pnt_pack->ary[n_pack].dataset, dataset );
        (void) strcpy( pnt_pack->ary[n_pack].option_id, option_id );
        (void) strcpy( pnt_pack->ary[n_pack].type_id, type_id );
        (void) strcpy( pnt_pack->ary[n_pack].format_id, format_id );
        (void) strcpy( pnt_pack->ary[n_pack].package_size,
            package_size );
        (void) strcpy( pnt_pack->ary[n_pack].ims_visible_p,
            ims_visible_p );
    }

    /*
    ** Reset the query descriptor.
    */
    (void) ims_qiResetDesc (qDesc);

    /*
    **  now check each row
    */
    pnt_pack = pnt_pack_1st;
    rowCount = 0;
    n_pack = -1;
    while(  rowCount  <  n_pack_tot ){
        rowCount++;
        n_pack++;
        if(  n_pack  >=  MAX_PACK_BUF  ){ /*  need new buffer  */
            pnt_pack2 = pnt_pack;
            pnt_pack = pnt_pack->pnt_next;
            (void) free( pnt_pack2 );
            n_pack = 0;
        }

    (void) strcpy( platform     , pnt_pack->ary[n_pack].platform      );
    (void) strcpy( sensor       , pnt_pack->ary[n_pack].sensor        );
    (void) strcpy( dataset      , pnt_pack->ary[n_pack].dataset       );
    (void) strcpy( option_id    , pnt_pack->ary[n_pack].option_id     );
    (void) strcpy( type_id      , pnt_pack->ary[n_pack].type_id       );
    (void) strcpy( format_id    , pnt_pack->ary[n_pack].format_id     );
    (void) strcpy( package_size , pnt_pack->ary[n_pack].package_size  );
    (void) strcpy( ims_visible_p, pnt_pack->ary[n_pack].ims_visible_p );

        /*
        **  now add this to the odl file
        */
        if(  strcmp( dataset, last_dataset )  !=  0  ){
            /*
            **  need new package group
            **  an example of the package group follows
            **
            **            DATA_CENTER_ID = "ASF"
            **            DATASET_ID = "ERS-1 SAR ...."
            **            PACKAGE_ID = "*"
            **            COMMENT = ""
            **            NUMBER_OF_GRANULES = 1
            **            NUMBER_OF_OPTIONS = 1
            **      Note that number of options is the number
            **          of options groups that follow and is
            **          formed later.
            */
            (void) strcpy( last_dataset, dataset );
            n_packages++;
            /*
            **  first check for options: need number of options at
            **      end of first package group
            */
            if(  pnt_tree_n_options  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_options );
                pnt_tree_save = pnt_tree_n_options->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_options,
                    "NUMBER_OF_OPTIONS", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_options->next = pnt_tree_save;
                }
                pnt_tree_n_options = NULL;
            }
            no_of_options = 0;
            if(  pnt_tree_n_process  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_process );
                pnt_tree_save = pnt_tree_n_process->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_process,
                    "NUMBER_OF_MEDIA_TYPE", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_process->next = pnt_tree_save;
                }
                pnt_tree_n_process = NULL;
            }
            if(  pnt_tree_n_media  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_media_fmt );
                pnt_tree_save = pnt_tree_n_media->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_media,
                    "NUMBER_OF_MEDIA_FORMAT", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_media->next = pnt_tree_save;
                }
                pnt_tree_n_media = NULL;
            }

            if(  pnt_tree_package  ==  NULL  ){
                (void) ims_addODLObject(msgDesc, pnt_tree_package,
                    &pnt_tree, "PACKAGE", TRUE, IMS_ODL_GROUP);
                pnt_tree_package = pnt_tree;
            }
            else{
                (void) ims_addODLObject(msgDesc, pnt_tree_package,
                    &pnt_tree, "PACKAGE", FALSE, IMS_ODL_GROUP);
            }
            if(  pnt_tree_1st  ==  NULL  )  pnt_tree_1st = pnt_tree;

            status = ims_addODLKeyword( msgDesc, pnt_tree,
                "DATA_CENTER_ID", TV_STRING, "ASF" );
            if(  status  <  IMS_OK )  return( status );
            status = ims_addODLKeyword(msgDesc, pnt_tree, "DATASET_ID",
                TV_STRING, dataset );
            if(  status  <  IMS_OK )  return( status );
            status = ims_addODLKeyword(msgDesc, pnt_tree, "PACKAGE_ID",
                TV_STRING, "*" );
            if(  status  <  IMS_OK )  return( status );
            status = ims_addODLKeyword(msgDesc, pnt_tree, "COMMENT",
                TV_STRING, "" );
            if(  status  <  IMS_OK )  return( status );
            status = ims_addODLKeyword(msgDesc, pnt_tree,
                "NUMBER_OF_GRANULES", TV_STRING, "1" );
            if(  status  <  IMS_OK )  return( status );
            pnt_tree_n_options = pnt_tree;
            last_option_id[0] = '\0';
            last_media_type[0] = '\0';
            pnt_tree_option = NULL;
        }
        if(  strcmp( last_option_id, option_id )  !=  0  ){
            /*
            **  need to add processing_option group
            **      the group looks like:
            **
            **    OPTION_ID = "NORMAL"
            **    PACKAGE_SIZE = "300.1 MBYTES"
            **    NUMBER_OF_MEDIA_TYPE = 3
            */
            if(  pnt_tree_n_process  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_process );
                pnt_tree_save = pnt_tree_n_process->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_process,
                    "NUMBER_OF_MEDIA_TYPE", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_process->next = pnt_tree_save;
                }
                pnt_tree_n_process = NULL;
            }
            if(  pnt_tree_n_media  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_media_fmt );
                pnt_tree_save = pnt_tree_n_media->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_media,
                    "NUMBER_OF_MEDIA_FORMAT", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_media->next = pnt_tree_save;
                }
                pnt_tree_n_media = NULL;
            }
            no_of_process = 0;

            if(  pnt_tree_option  ==  NULL  ){
                /*
                **  first time after package was formed
                */
                pnt_tree_option = pnt_tree;
                (void) ims_addODLObject(msgDesc, pnt_tree_option,
                    &pnt_tree, "PROCESSING_OPTION", TRUE,
                    IMS_ODL_GROUP);
                pnt_tree_option = pnt_tree;
            }
            else{
                /*
                **  now not a child, as after the previous
                **      processing_ioption
                */
                (void) ims_addODLObject(msgDesc, pnt_tree_option,
                    &pnt_tree, "PROCESSING_OPTION", FALSE,
                    IMS_ODL_GROUP);
            }
            status = ims_addODLKeyword(msgDesc, pnt_tree, "OPTION_ID",
                TV_STRING, option_id );
            if(  status  <  IMS_OK )  return( status );
            status = ims_addODLKeyword(msgDesc, pnt_tree,
                "PACKAGE_SIZE", TV_STRING, package_size );
            if(  status  <  IMS_OK )  return( status );
            pnt_tree_n_process = pnt_tree;
            pnt_tree_type = NULL;

            no_of_options++;
            last_media_type[0] = '\0';
            (void) strcpy( last_option_id, option_id );
        }
        if(  strcmp( last_media_type, type_id )  !=  0  ){
            /*
            **  need to add media_type group
            **      the group looks like:
            **
            **   TYPE_ID = "4MM 2GB CARTRIDGE"
            **   NUMBER_OF_MEDIA_FORMAT = 2
            */
            if(  pnt_tree_n_media  !=  NULL  ){
                (void) sprintf( str, "%ld", no_of_media_fmt );
                pnt_tree_save = pnt_tree_n_media->next;
                status = ims_addODLKeyword( msgDesc, pnt_tree_n_media,
                    "NUMBER_OF_MEDIA_FORMAT", TV_INTEGER, str );
                if(  status  <  IMS_OK )  return( status );
                if(  pnt_tree_save  !=  NULL  ){
                    pnt_tree_n_media->next = pnt_tree_save;
                }
                pnt_tree_n_media = NULL;
            }
            no_of_media_fmt = 0;

            if(  pnt_tree_type  ==  NULL  ){
                (void) ims_addODLObject(msgDesc, pnt_tree, &pnt_tree,
                    "MEDIA_TYPE", TRUE, IMS_ODL_GROUP);
                pnt_tree_type = pnt_tree;
            }
            else{
                (void) ims_addODLObject(msgDesc, pnt_tree_type,
                    &pnt_tree, "MEDIA_TYPE", FALSE, IMS_ODL_GROUP);
                pnt_tree_type = pnt_tree;
            }
            status = ims_addODLKeyword(msgDesc, pnt_tree, "TYPE_ID",
                TV_STRING, type_id );
            if(  status  <  IMS_OK )  return( status );
            pnt_tree_n_media = pnt_tree;

            no_of_process++;
            (void) strcpy( last_media_type, type_id );
        }
        /*
        **  always need to add media_fmt group
        **      the group looks like:
        **
        **   FORMAT_ID = "CEOS SAR TAPE"
        **   APPROX_COST = 61.0
        */

        if(  no_of_media_fmt  ==  0  ){
            (void) ims_addODLObject(msgDesc, pnt_tree, &pnt_tree,
                "MEDIA_FORMAT", TRUE, IMS_ODL_GROUP);
        }
        else{
            (void) ims_addODLObject(msgDesc, pnt_tree, &pnt_tree,
                "MEDIA_FORMAT", FALSE, IMS_ODL_GROUP);
        }
        status = ims_addODLKeyword(msgDesc, pnt_tree, "FORMAT_ID",
            TV_STRING, format_id );
        if(  status  <  IMS_OK )  return( status );

        /*
        **  now we need to calculate the cost of this product:
        **      need media_type and media_fmt_type
        */
        /* now try to match up the input strings */
        flag = TRUE;
        for( i=0 ; i  <  n_type_values  &&  flag ; i++ ){
            if(  strcmp( type_values[i], type_id )  ==  0 ){
                /* match: get the index */
                media_type = idx_type_values[i];
                flag = FALSE;
            }
        }
        if(  flag ){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in obtaining media_type:"
                " type_id = %s.", type_id );
            media_type = -99;
        }
        flag = TRUE;
        for( i=0 ; i  <  n_format_values  &&  flag ; i++ ){
            if(  strcmp( format_values[i], format_id )  ==
                0 ){
                /* match: get the index */
                media_fmt_type = idx_format_values[i];
                flag = FALSE;
            }
        }
        if(  flag ){
            status = IMS_ERROR;
            (void) ims_msg (msgDesc, status,
                "ERROR in obtaining "
                "media_fmt_type: format_id = %s.",
                format_id );
            media_fmt_type = -99;
        }
        if(  media_type  <  0  ||  media_fmt_type  <  0  ){
            (void) strcpy( approx_cost, "0.0" );
        }
        else{
            status = get_table_costs( msgDesc, platform, sensor,
                dataset, option_id, media_type, media_fmt_type,
                &cost_total );
            if(  status  <  IMS_OK )  return( status );
            f_temp = cost_total;
            i = 10.0 * f_temp+.01;
            j = i % 10;
            i = i/10;
            (void) sprintf ( approx_cost, "%d.%d", i, j );
        }

        status = ims_addODLKeyword(msgDesc, pnt_tree, "APPROX_COST",
            TV_REAL, approx_cost );
        if(  status  <  IMS_OK )  return( status );
        n_costs++;

        no_of_media_fmt++;
    }

    (void)  free( pnt_pack );
    /*
    **  now generate the file.  if a file is there with the same
    **      name, put a % at the end.
    */
    status = access( odl_name, F_OK );
    if(  status  ==  0 ){ /* file there  */
        (void) strcpy( str, odl_name );
        (void) strcat( str, "%" );
        (void) rename( odl_name, str );
    }
    odl_file = NULL;
    status = ims_buildPMF( msgDesc, pnt_tree_1st, odl_name, odl_file );
    if(  status  <  IMS_OK ){
        (void) ims_msg( msgDesc, status,
            "ERROR in building odl file." );
        if(  print_log ){
            (void) fprintf( out_file,
                "ERROR in building odl file." );
        }
        return( status );
    }
    (void)  ims_msg( msgDesc, IMS_INFO,
        "Finished writting file %s with %ld cost values and "
        "%ld datasets.", odl_name, n_costs, n_packages );
    if(  print_log ){
        (void) fprintf( out_file,
            "******  Finished writting file %s with %ld cost values "
            "and %ld datasets.\n", odl_name, n_costs, n_packages );
    }
    return (IMS_OK);
}   /*  gen_odl  */


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
            "openConnection: Could not allocate a query descriptor.");
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
            "openConnection: Could not login to the database server.");
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
