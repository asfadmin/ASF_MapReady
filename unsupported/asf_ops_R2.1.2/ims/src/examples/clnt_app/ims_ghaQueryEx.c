static char *sccs = "@(#)ims_ghaQueryEx.c	5.1  03/17/96";
/* *************************************************************
**
** File:        ims_ghaQueryEx.c
**
** Function:    An client program that uses the ims_ghaQueryEx
**              routine.
**
** Author:      David Pass
**
** Date:        10/16/95
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
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY * );
static int checkQueryStatus (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, int);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *pgm;
    char *startTime;
    char *endTime;
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
    {"-U",           &commands.username},
    {"+username",    &commands.username},
    {"-P",           &commands.password},
    {"+password",    &commands.password},
    {"-R",           &commands.pgm},
    {"+program",     &commands.pgm},
    {"-S",           &commands.startTime},
    {"+startTime",   &commands.startTime},
    {"-E",           &commands.endTime},
    {"+endTime",     &commands.endTime},
    {"-C",           &commands.commandFile},
    {"+commandFile", &commands.commandFile},
    {"-X",           &commands.server},
    {"+server",      &commands.server},
    {"-Y",           &commands.database},
    {"+database",    &commands.database},
    {"-h",           &commands.help},
    {"+help",        &commands.help},
    {"-r",           &commands.release},
    {"+release",     &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",        &commands.username},
    {"password",        &commands.password},
    {"program",         &commands.pgm},
    {"startTime",       &commands.startTime},
    {"endTime",         &commands.endTime},
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
** This is the driver for the gha routines:
**      ims_ghaPointQuery and ims_ghaRangeQuery.  the input node
**      -pgm  determines which: 1,2.  1 is default.
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
    char  odl_buffer[2049];
    IMS_CMN_QUERY *query;
    IMS_GHA_STRUCT retbuf;
    char starttime[IMS_DATETIME_LEN+1];
    char endtime[IMS_DATETIME_LEN+1];


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.startTime = NULL;
    commands.endTime = NULL;
    commands.pgm = NULL;
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
    if ((status = getArgInput (msgDesc, query) )
        < IMS_OK){
        goto ERROR;
    }

    if(  commands.pgm[0]  ==  'r' )
    {
        /*
        **  open the connection: only for range version
        */
        status = ims_openQueryConnection( query );
        if(  status  <  IMS_OK ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Could not open connection." );
            goto ERROR;
        }
    }

    /*
    ** Perform the query.
    */
    query->retPtr = (char *) &retbuf;
    if( commands.pgm[0]  ==  'p' )
    {
        status = ims_ghaPointQuery (query, commands.startTime);
    }
    else if( commands.pgm[0]  ==  'r' )
    {
        status = ims_ghaRangeQuery (query, commands.startTime,
            commands.endTime);
    }

    if (status < IMS_OK)
    {
        (void) checkQueryStatus (msgDesc, query, status);

        (void) ims_msg (msgDesc, status,
            "Could not query GHA data.");
        goto ERROR;
    }

    /*
    ** Print out the results.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The GHA query was successful.");
    (void) ims_msg( msgDesc, IMS_INFO,
        "** date = %s   angle = %s", retbuf.date, retbuf.angle);

    /*
    ** Shutdown the message facility.
    */
    (void) ims_closeQueryConnection (query);
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The GHA query failed.");
    (void) ims_closeQueryConnection (query);
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
    IMS_CMN_QUERY *query )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long  i;


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

    /*
    ** pgm: program must be point or range.  only the first
    **  character checked.
    */
    if(  commands.pgm  ==  (char *) NULL )
    {
        try_again:
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Program (point or range): ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        if(  isupper( inputBuffer[0] ) )  inputBuffer[0] = tolower(
            inputBuffer[0] );
        if(inputBuffer[0] != 'p' && inputBuffer[0] != 'r')
        {
            (void) printf( "\n **  Not valid value.  Try again." );
            goto try_again;
        }
        i = strlen( inputBuffer );
        commands.pgm = (char *) malloc( (i+1) * sizeof( char ) );
        (void) strcpy (commands.pgm, inputBuffer);
    }
    else{
        if(  isupper( commands.pgm[0] ) )  commands.pgm[0] = tolower(
            commands.pgm[0] );
        if(commands.pgm[0] != 'p' && commands.pgm[0] != 'r')
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
    " **  Not valid value for program: must be point or range.");
            return( IMS_ERROR );
        }
    }

    /* startTime  */
    if(  commands.startTime  ==  (char *) NULL )
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Start Time (yyyy-nnnThh:mm:ss.hhh): ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer );
        commands.startTime = (char *) malloc( (i+1) * sizeof( char ) );
        (void) strcpy( commands.startTime, inputBuffer );
    }

    /* endTime - this may be null optionally, so if 0, set null */
    if(  commands.endTime  ==  (char *) NULL )
    {
        if(  commands.pgm[0]  !=  'p' )
        {   /* in this case, no endTime to be input  */
            if( ims_getString(IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
                "End Time (yyyy-nnnThh:mm:ss.hhh): ") == (char *) NULL)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Error detected while reading input string.");
                return (IMS_FATAL);
            }
            (void) ims_truncStr( inputBuffer );
            i = strlen( inputBuffer );
            if(  i  >  0 ){
                commands.endTime = (char *) malloc( (i+1) *
                    sizeof( char ) );
                (void) strcpy( commands.endTime, inputBuffer );
            }
        }
    }
    else{
        /*
        **  for the full case, null is valid: this way, putting
        **      endTIme = ;
        **  in the command file (.pvl) will work.
        */
        i = strlen( commands.endTime );
        if(  i  ==  0  )  commands.endTime = NULL;
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


/* **************************************************************
**
** checkQueryStatus ()
**
**************************************************************** */

static int checkQueryStatus (
    IMS_MSG_STRUCT *msgDesc,
    IMS_CMN_QUERY *query,
    int status)
{
    switch (query->retStatus)
    {
        case IMS_OK:
            (void) ims_msg (msgDesc, status,
                "Error is not query related.");
            break;

        case IMS_NOCONNECT:
            (void) ims_msg (msgDesc, status,
                "Error due to connection failure.");
            break;

        case IMS_DEADLOCK:
            (void) ims_msg (msgDesc, status,
                "Error due to query deadlock.");
            break;

        case IMS_NOROWS:
            (void) ims_msg (msgDesc, status,
                "Error due to no rows returned.");
            break;

        case IMS_QUERYFAIL:
            (void) ims_msg (msgDesc, status,
                "Error due to query failure.");
            break;

        default:
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Unknown query return status.");
            return (IMS_ERROR);
    }

    return (IMS_OK);
}   /*  checkQueryStatus */
