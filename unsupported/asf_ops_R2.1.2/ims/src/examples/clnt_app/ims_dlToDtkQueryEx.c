static char *sccs = "@(#)ims_dlToDtkQueryEx.c	1.1  05/15/97";
/* *************************************************************
**
** File:        ims_dlToDtkQueryEx.c
**
** Function:    An client program that uses the ims_dlToDtkQuery
**              routine.
**
** Author:      David Pass
**
** Date:        4/29/97
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY *,
    int *, int * );

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *platform;
    char *sensor;
    char *rev;
    char *sequence;
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
    {"-L",              &commands.platform},
    {"+platform",       &commands.platform},
    {"-S",              &commands.sensor},
    {"+sensor",         &commands.sensor},
    {"-R",              &commands.rev},
    {"+rev",            &commands.rev},
    {"-E",              &commands.sequence},
    {"+sequence",       &commands.sequence},
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
    {"platform",        &commands.platform},
    {"sensor",          &commands.sensor},
    {"rev",             &commands.rev},
    {"sequence",        &commands.sequence},
    {"server",          &commands.server},
    {"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *glb_programName;

/* *************************************************************
**
** main ()
**
** This is the driver for the scan results update from the PPS
**  system.  The input to ims_scanResults is a query structure
**  for db initialization, and the inputs for the query.
**
**************************************************************** */

void main (
    int argc,
    char *argv[])
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_DL_STRUCT  *pnt_dl_1st;
    IMS_DL_STRUCT  *pnt_dl;
    pnt_ims_dtkStruct  pnt_dtk;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    IMS_CMN_QUERY *query;
    int  rev;   /* revolution  */
    int sequence;   /* can be -1  */
    int inputMatch; /* output from ims_dlToDtkQuery: IMS_DL_MATCH,
            IMS_DTK_MATCH, or IMS_NO_MATCH.  */
    int  num_dls; /* no. downlinks found  */
    int  num_dtks_sum; /* total no. of datatakes: from all dls  */
    int  num_dtks; /* no. of datatakes from current dl  */


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.platform = NULL;
    commands.sensor = NULL;
    commands.rev = NULL;
    commands.sequence = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.help = NULL;
    commands.release = NULL;

    query->qDesc = NULL;
    query->username[0] = '\0';
    query->password[0] = '\0';
    query->program[0] = '\0';
    query->server[0] = '\0';
    query->database[0] = '\0';
    query->msgDesc = NULL;
    query->qDesc = NULL;
    query->retStatus = IMS_OK;
    query->retPtr = NULL;

    /*
    ** Get the program name and the node name.
    */
    glb_programName = ims_extractFileName (argv[0]);
    (void) strcpy( query->program, glb_programName );
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
    query->msgDesc = msgDesc;

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
    if (ims_setWrapup (runDown) < IMS_OK){
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
        cmdLineElmCount, msgDesc)) < IMS_OK){
        goto ERROR;
    }

    /*
    ** Check to see if we got everything off of the command line.
    */
    if (status < argc){
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
            cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK){
            goto ERROR;
        }

        /*
        ** Now, get command line arguments again to overlay file args.
        */
        if ((status = ims_getCmdLine (argc, argv,
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK){
            goto ERROR;
        }
    }

    /*
    ** Process the information from command-line and/or command-file.
    */
    if( (status = getArgInput (msgDesc, query, &rev, &sequence ) )
        < IMS_OK){
        goto ERROR;
    }

    query->qDesc = NULL;
    /*
    **  process query
    */
    query->retPtr = (char *) NULL;
    if ((status = ims_dlToDtkQuery( query, commands.platform,
        commands.sensor, rev, sequence, &inputMatch ) ) < IMS_WARNING)
    {
        goto ERROR;
    }

    (void) ims_msg (msgDesc, IMS_INFO,
        "The ims_dlToDtkQuery select was successful.");

    if(  inputMatch  ==  IMS_DL_MATCH  )
        printf( "\n *** DL match.\n\n" );
    else  if(  inputMatch  ==  IMS_DTK_MATCH  )
        printf( "\n *** DTK match.\n\n" );
    else  if(  inputMatch  ==  IMS_NO_MATCH  )
        printf( "\n *** No match.\n\n" );

    num_dls = 0;
    num_dtks_sum = 0;
    pnt_dl_1st = (IMS_DL_STRUCT *) query->retPtr;
    pnt_dl = pnt_dl_1st;
    while( pnt_dl  !=  NULL  ){
        /*
        **  print out the dl stucture
        */
        num_dls++;
        printf( "\n\nResults of DL structure no. %d:\n", num_dls );
        printf( "   platform = %s\n",         pnt_dl->platform );
        printf( "   sensor = %s\n",           pnt_dl->sensor  );
        printf( "   revolution = %ld\n",      pnt_dl->revolution );
        printf( "   sequence = %ld\n",        pnt_dl->sequence );
        printf( "   activity_id = %s\n",      pnt_dl->activity_id );
        printf( "   station_id = %s\n",       pnt_dl->station_id  );
        printf( "   antenna_id = %s\n",       pnt_dl->antenna_id  );
        printf( "   transmitter_id = %s\n",   pnt_dl->transmitter_id );
        printf( "   fa_schedule_link = %s\n", pnt_dl->fa_schedule_link);
        printf( "   time_on = %s\n",          pnt_dl->time_on  );
        printf( "   time_off = %s\n",         pnt_dl->time_off );
        printf( "   time_aos = %s\n",         pnt_dl->time_aos );
        printf( "   time_los = %s\n",         pnt_dl->time_los );
        printf( "   downlink_status = %s\n",  pnt_dl->downlink_status );
        printf( "   number_of_dtk_entry = %ld\n",
            pnt_dl->number_of_dtk_entry );
        pnt_dtk = pnt_dl->datatakePtr;
        num_dtks = 0;
        while(  pnt_dtk  !=  NULL  ){
            num_dtks++;
            num_dtks_sum++;
            printf( "\n       **** datatake no. %ld *****\n",
                num_dtks );
            printf( "       platform = %s\n", pnt_dtk->platform );
            printf( "       sensor = %s\n",     pnt_dtk->sensor  );
            printf( "       revolution = %ld\n",
                pnt_dtk->revolution );
            printf( "       sequence = %ld\n",  pnt_dtk->sequence );
            printf( "       quicklook_flag = %s\n",
                pnt_dtk->quicklook_flag );
            printf( "       process_auth_flag = %s\n",
                pnt_dtk->process_auth_flag );
            printf( "       mode = %s\n",       pnt_dtk->mode );
            printf( "       frame_mode = %s\n",
                pnt_dtk->frame_mode );
            printf( "       time_on = %s\n",    pnt_dtk->time_on );
            printf( "       time_off = %s\n",   pnt_dtk->time_off );
            printf( "       site_name = %s\n",  pnt_dtk->site_name);
            pnt_dtk = pnt_dtk->next;
        }
        pnt_dl = pnt_dl->downlinkPtr;
    }
    printf( "\n\n\n*****  There are %ld downlinks, %ld total "
        "datatakes.\n", num_dls, num_dtks_sum );

    /*
    **  now free the structure
    */
    ims_dlToDtkFree( pnt_dl_1st );
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_dlToDtkQuery select failed.");
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
    IMS_MSG_STRUCT *msgDesc,
    IMS_CMN_QUERY *query, int * rev, int * sequence )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long i;
    long number;
    short invalid;


    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username != (char *) NULL)
    {
        (void) strcpy( query->username, commands.username );
    }
    else
        {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Username: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        (void) strcpy (query->username, inputBuffer);
    }

    /* password */
    if (commands.password != (char *) NULL)
        {
        (void) strcpy( query->password, commands.password );
    }
    else
        {
        if (ims_getPassword (inputBuffer) == NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        (void) strcpy (query->password, inputBuffer);
    }

    /* platform */
    if (commands.platform  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Platform: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.platform = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.platform, inputBuffer);
    }

    /* sensor  */
    if (commands.sensor  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Sensor (1 char): ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.sensor = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.sensor, inputBuffer);
    }

    /* rev */
    try_rev_again:;
    number = 0;
    if (commands.rev  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Rev: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        invalid = 0;
        for (i=0; inputBuffer[i]; i++)
        {
            if ((i == 0) && ((inputBuffer[i] == '-') ||
                (inputBuffer[i] == '+')))
            {
                continue;
            }

            if (! isdigit (inputBuffer[i]))
            {
                (void) ims_msg( msgDesc, IMS_INFO,
                    "Expecting numerical input. Try again.");
                goto try_rev_again;
                break;
            }
        }

        number = (int) atoi (inputBuffer);
        if( (invalid == 0 ) &&  (number <= 0 ) )
        {
            invalid = 1;
            (void) ims_msg( msgDesc, IMS_INFO,
   "Numerical input is not valid. Try again.\n" );
            goto try_rev_again;
        }
        *rev = number;
    }
    else
    {
        /*
        **  check if this is a valid number
        */
        (void) strcpy( inputBuffer, commands.rev );
        for (i=0; inputBuffer[i]; i++)
        {
            if ((i == 0) && ((inputBuffer[i] == '-') ||
                (inputBuffer[i] == '+')))
            {
                continue;
            }

            if (! isdigit (inputBuffer[i]))
            {
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Expecting numerical input for rev.");
                return( IMS_ERROR );
            }
        }
        number = (int) atoi (inputBuffer);
        if(  number  <=  0 )
        {
            (void) ims_msg( msgDesc, IMS_INFO,
                "Expecting positive numerical input for rev.");
            return( IMS_ERROR );
        }
        *rev = number;
    }


    /* sequence */
    number = 0;
    try_sequence_again:;
    if (commands.sequence  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Sequence: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        invalid = 0;
        for (i=0; inputBuffer[i]; i++)
        {
            if ((i == 0) && ((inputBuffer[i] == '-') ||
                (inputBuffer[i] == '+')))
            {
                continue;
            }

            if (! isdigit (inputBuffer[i]))
            {
                (void) ims_msg( msgDesc, IMS_INFO,
                    "Expecting numerical input. Try again.");
                goto try_sequence_again;
            }
        }

        number = (int) atoi (inputBuffer);
        if ((invalid == 0 ) && ( number  ==  0 ||  number  <=  -2  ) )
        {
            invalid = 1;
            (void) ims_msg( msgDesc, IMS_INFO,
    "Numerical input is not valid. Try again." );
            goto try_sequence_again;
        }

        *sequence = number;
    }
    else
    {
        /*
        **  check if this is a valid number
        */
        (void) strcpy( inputBuffer, commands.sequence );
        for (i=0; inputBuffer[i]; i++)
        {
            if ((i == 0) && ((inputBuffer[i] == '-') ||
                (inputBuffer[i] == '+')))
            {
                continue;
            }

            if (! isdigit (inputBuffer[i]))
            {
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Expecting numerical input for sequence.");
                return( IMS_ERROR );
            }
        }
        number = (int) atoi (inputBuffer);
        if(  number  ==  0 ||  number  <  -1  ) {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Expecting positive or -1 numerical input for "
                "sequence.");
            return( IMS_ERROR );
        }
        *sequence = number;
    }

    /* server */
    if (commands.server != (char *) NULL)
    {
        (void) strcpy( query->server, commands.server );
    }
    else commands.server = NULL;

    /* database */
    if (commands.database != (char *) NULL)
    {
        (void) strcpy( query->database, commands.database );
    }
    else  commands.database = NULL;

    return (IMS_OK);
}   /*  getArgInput */
