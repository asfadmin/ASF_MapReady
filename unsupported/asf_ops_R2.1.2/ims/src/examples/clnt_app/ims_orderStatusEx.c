static char *sccs = "@(#)ims_orderStatusEx.c	5.2  08/11/97";
/* *************************************************************
**
** File:        ims_orderStatusEx.c
**
** Function:    An client program that uses the ims_orderStatus
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
#include <time.h>

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
#include <ims_odlBuffer.h>
#include <ims_util.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, char * );

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *odl_file;
    char *orderId;
    char *itemId;
    char *statusType;
    char *status;
    char *comment;
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
    {"-O",           &commands.odl_file},
    {"+ODLFilename", &commands.odl_file},
    {"-C",           &commands.commandFile},
    {"+commandFile", &commands.commandFile},
    {"-N",           &commands.orderId},
    {"+orderId",     &commands.orderId},
    {"-I",           &commands.itemId},
    {"+itemId",      &commands.itemId},
    {"-T",           &commands.statusType},
    {"+statusType",  &commands.statusType},
    {"-S",           &commands.status},
    {"+status",      &commands.status},
    {"-M",           &commands.comment},
    {"+comment",     &commands.comment},
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
    {"username",    &commands.username},
    {"password",    &commands.password},
    {"ODLFilename", &commands.odl_file},
    {"orderId",     &commands.orderId},
    {"itemId",      &commands.itemId},
    {"statusType",  &commands.statusType},
    {"status",      &commands.status},
    {"comment",     &commands.comment},
    {"server",      &commands.server},
    {"database",    &commands.database}
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
** This is the driver for the order status update from the PPS
**  system.  The input to ims_orderStatus is a query structure
**  for db initialization, and an odl file with the values in it.
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


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.odl_file = NULL;
    commands.orderId = NULL;
    commands.itemId = NULL;
    commands.statusType = NULL;
    commands.status = NULL;
    commands.comment = NULL;
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
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK){
            goto ERROR;
        }
    }

    /*
    ** Process the information from command-line and/or command-file.
    */
    if ((status = getArgInput (msgDesc, query, odl_buffer))
        < IMS_OK){
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
    **  process order
    */
    if ((status = ims_orderStatus( query, odl_buffer ) ) < IMS_WARNING){
        goto ERROR;
    }

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The orderStatus update was successful.");
    status = ims_closeQueryConnection( query );
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The orderStatus update failed.");
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
    IMS_CMN_QUERY *query,
    char * odl_buffer )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long  k;
    FILE  *in_file;
    short  flag;
    short read_eof;
    long  n_lines;  /* no. lines read from odl file  */
    long  sum; /* no. characters so far input from odl file */
    char  str[256], str2[256], str3[256];

    long sec_clock;       /* Number of seconds returned by time */
    struct tm *tm_ptr;   /* tm is defined in system library include
                         file time.h */


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

    if(  commands.odl_file  !=  NULL ){
        /*
        **  now read the odl file, put into buffer
        */
        if( ( in_file = fopen( commands.odl_file, "r" ) )  ==  NULL )
            {
            (void) ims_msg( msgDesc, IMS_ERROR,
                " *** Cannot open input file %s.", commands.odl_file );
            return( IMS_ERROR );
        }
        flag = IMS_TRUE;
        odl_buffer[0] = '\0';
        sum = 0;
        n_lines = 0;
        while( flag   )
            {
            read_eof = ( fgets( str, 256, in_file ) == NULL );
            if(  read_eof  )
            {   /*  read eof: stop reading */
                flag = IMS_FALSE;
            }
            else
            {
                /* note: keep linefeed in the line  */
                k = strlen( str );
                (void) strcat( odl_buffer, str );
                n_lines++;
                sum += k;
            }
        }
        (void) ims_msg( msgDesc, IMS_INFO,
            "Read %ld lines, %ld characters from %s ODL file.",
            n_lines, sum, commands.odl_file );
    }
    else{
        /*
        **  the 4 variables may be input separately on the command
        **      line.  these are then put into the odl buffer as if
        **      from a file.  The only problem is if the status
        **      is completed: then other values must be input.  in this
        **      case, reject the 4 args.
        **  if the 4 args are not input, print error message.
        */
        if(  commands.orderId  ==  NULL  ||  commands.itemId  ==  NULL
            ||  commands.statusType  ==  NULL  ||
            commands.status  ==  NULL  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Need to input an ODL file name or orderId, itemId, "
                "statusType, status values." );
            return( IMS_ERROR );
        }
        if(  strcmp( commands.status, "COMPLETED" )  ==  0  ){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Must use ODL file for status of COMPLETED." );
            return( IMS_ERROR );
        }
        /*
        **  put the arguments in the buffer: just like an odl file.
        */
        (void) strcpy( odl_buffer, "OBJECT = IMS_ORDER_STATUS\n" );
        (void) strcat( odl_buffer, "   OBJECT = COMMON_HEADER\n" );
        (void) strcat( odl_buffer,
            "       TIME = 1995-300T08:15:20.000\n" );
        (void) strcat( odl_buffer,
            "       MSG_TYPE = 'IMS_ORDER_STATUS'\n" );
        (void) strcat( odl_buffer, "       DESTINATION = 'IMS'\n" );
        (void) strcat( odl_buffer, "       SOURCE = 'PPS'\n" );
        (void) strcat( odl_buffer, "       NUMBER_OF_RECORDS = 1\n" );
        (void) strcat( odl_buffer, "   END_OBJECT = COMMON_HEADER\n" );
        (void) strcat( odl_buffer, "   OBJECT = BODY\n" );

        (void) strcat( odl_buffer, "       ORDER_ID = " );
        (void) strcat( odl_buffer, commands.orderId );
        (void) strcat( odl_buffer, "\n" );
        (void) strcat( odl_buffer, "       ITEM_ID = " );
        (void) strcat( odl_buffer, commands.itemId );
        (void) strcat( odl_buffer, "\n" );
        (void) strcat( odl_buffer, "       STATUS_TYPE = '" );
        (void) strcat( odl_buffer, commands.statusType );
        (void) strcat( odl_buffer, "'\n" );
        (void) strcat( odl_buffer, "       STATUS = '" );
        (void) strcat( odl_buffer, commands.status );
        (void) strcat( odl_buffer, "'\n" );

        /*
        **  note: comment need not be input
        */
        (void) strcat( odl_buffer, "       COMMENT = '" );
        if(  commands.comment  ==  NULL  ){
            /*
            **  put new status and date
            */
            (void) strcat( odl_buffer, "Status changed to " );
            (void) strcat( odl_buffer, commands.status );
            (void) strcat( odl_buffer, " on " );

            time(&sec_clock);
            if (&sec_clock == NULL){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Could not get time in seconds." );
                return( IMS_ERROR );
            }
            if ((tm_ptr = localtime(&sec_clock)) == NULL){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Could not get time." );
                return( IMS_ERROR );
            }

            (void) sprintf(  str2, "%2d:%2d:%2d",  tm_ptr->tm_hour,
                tm_ptr->tm_min, tm_ptr->tm_sec   );
            if(  tm_ptr->tm_hour  <  10  )  str2[0] = '0';
            if(  tm_ptr->tm_min   <  10  )  str2[3] = '0';
            if(  tm_ptr->tm_sec   <  10  )  str2[6] = '0';
            tm_ptr->tm_mon++;
            (void) sprintf(  str3, "%2d/%2d/%2d",  tm_ptr->tm_mon,
                tm_ptr->tm_mday, tm_ptr->tm_year  );
            if(  tm_ptr->tm_mon    <  10  )  str3[0] = '0';
            if(  tm_ptr->tm_mday   <  10  )  str3[3] = '0';
            (void) strcat( odl_buffer, str3 );
            (void) strcat( odl_buffer, " at " );
            (void) strcat( odl_buffer, str2 );
            (void) strcat( odl_buffer, ".'\n" );
        }
        else{
            (void) strcat( odl_buffer, commands.comment );
            (void) strcat( odl_buffer, "'\n" );
        }
        /*
        (void) strcat( odl_buffer, "       PLATFORM = 'XX'\n" );
        (void) strcat( odl_buffer, "       SENSOR = 'X'\n" );
        (void) strcat( odl_buffer, "       PRODUCT_ID = 'XXX'\n" );
        (void) strcat( odl_buffer, "       DATASET = 'UNKNOWN'\n" );
        */
        (void) strcat( odl_buffer, "   END_OBJECT = BODY\n" );
        (void) strcat( odl_buffer, "END_OBJECT = IMS_ORDER_STATUS\n" );
        (void) strcat( odl_buffer, "END\n" );
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
