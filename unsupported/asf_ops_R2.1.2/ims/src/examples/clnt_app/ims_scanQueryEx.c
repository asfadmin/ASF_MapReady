static char *sccs = "@(#)ims_scanQueryEx.c	5.3  07/19/96";
/* *************************************************************
**
** File:        ims_scanQueryEx.c
**
** Function:    An client program that uses the ims_scanResults
**              routine.
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
    char *mode;
    char *rev;
    char *sequence;
    char *mediaId;
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
    {"-M",              &commands.mode},
    {"+mode",           &commands.mode},
    {"-R",              &commands.rev},
    {"+rev",            &commands.rev},
    {"-S",              &commands.sequence},
    {"+sequence",       &commands.sequence},
    {"-I",              &commands.mediaId},
    {"+mediaId",        &commands.mediaId},
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
    {"mode",            &commands.mode},
    {"rev",             &commands.rev},
    {"sequence",        &commands.sequence},
    {"mediaId",         &commands.mediaId},
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
    IMS_SCAN_STRUCT retbuf;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    IMS_CMN_QUERY *query;
    int  rev;
    int sequence;


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.platform = NULL;
    commands.mode = NULL;
    commands.rev = NULL;
    commands.sequence = NULL;
    commands.mediaId = NULL;
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
    if( (status = getArgInput (msgDesc, query, &rev, &sequence ) )
        < IMS_OK)
    {
        goto ERROR;
    }

    /*
    **  open the connection
    */
    status = ims_openQueryConnection( query );
    if(  status  <  IMS_OK ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Could not open connection." );
        goto ERROR;
    }

    /*
    **  process query
    */
    query->retPtr = (char *) &retbuf;
    if ((status = ims_scanQuery( query, commands.platform,
        rev, sequence, commands.mediaId,
        commands.mode  ) ) < IMS_WARNING)
    {
        goto ERROR;
    }

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Results: FILE_NAME = %s   status = %d",
        retbuf.scan_results_file, query->retStatus );
    status = ims_closeQueryConnection( query );
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, status,
        "The ims_scanQuery select failed.");
    status = ims_closeQueryConnection( query );
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

    /* mode */
    if (commands.mode  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Mode: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.mode = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.mode, inputBuffer);
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
        if ((invalid == 0 ) && ( number  <=  0  ) )
        {
            invalid = 1;
            (void) ims_msg( msgDesc, IMS_INFO,
    "Numerical input is not valie. Try again." );
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
        if(  number  <=  0 )
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Expecting positive numerical input for sequence.");
            return( IMS_ERROR );
        }
        *sequence = number;
    }

    /* mediaId */
    if (commands.mediaId  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Media Id: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.mediaId = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.mediaId, inputBuffer);
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
