static char *sccs = "@(#)ims_spatialQueryEx.c	1.1  07/19/96";
/*******************************************************************
**
** File:        ims_spatialQueryEx.c
**
** Function:    Perform queries on frames in a spatial and temporal
**              sense: get their status.
**
** Author:      David Pass
**
** Date:        5/7/96
**
**
******************************************************************** */

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
    char *startTime;
    char *endTime;
    char *northLat;
    char *southLat;
    char *westLon;
    char *eastLon;
    char *frameStatus;
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
    {"-U",                  &commands.username},
    {"+username",           &commands.username},
    {"-P",                  &commands.password},
    {"+password",           &commands.password},
    {"-L",                  &commands.platform},
    {"+platform",           &commands.platform},
    {"-M",                  &commands.mode},
    {"+mode",               &commands.mode},
    {"-S",                  &commands.startTime},
    {"+startTime",          &commands.startTime},
    {"-E",                  &commands.endTime},
    {"+endTime",            &commands.endTime},
    {"-1",                  &commands.northLat},
    {"+northLat",           &commands.northLat},
    {"-2",                  &commands.southLat},
    {"+southLat",           &commands.southLat},
    {"-3",                  &commands.westLon},
    {"+westLon",            &commands.westLon},
    {"-4",                  &commands.eastLon},
    {"+eastLon",            &commands.eastLon},
    {"-F",                  &commands.frameStatus},
    {"+frameStatus",        &commands.frameStatus},
    {"-C",                  &commands.commandFile},
    {"+commandFile",        &commands.commandFile},
    {"-X",                  &commands.server},
    {"+server",             &commands.server},
    {"-Y",                  &commands.database},
    {"+database",           &commands.database},
    {"-h",                  &commands.help},
    {"+help",               &commands.help},
    {"-r",                  &commands.release},
    {"+release",            &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",        &commands.username},
    {"password",        &commands.password},
    {"platform",        &commands.platform},
    {"mode",            &commands.mode},
    {"startTime",       &commands.startTime},
    {"endTime",         &commands.endTime},
    {"northLat",        &commands.northLat},
    {"southLat",        &commands.southLat},
    {"westLon",         &commands.westLon},
    {"eastLon",         &commands.eastLon},
    {"frameStatus",     &commands.frameStatus},
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
** This is the driver for the dar frame status query from RGPS to
**  IMS.
**
**************************************************************** */

void main (
    int argc,
    char *argv[])
{
    long  i,j,k;
    IMS_MSG_STRUCT *msgDesc;
    pnt_ims_FrameResults_t fs_results;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    IMS_CMN_QUERY *query;
    long    frame_count; /* no. frames returned  */
    float  northLat_val;
    float  southLat_val;
    float  westLon_val;
    float  eastLon_val;


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.platform = NULL;
    commands.mode = NULL;
    commands.startTime = NULL;
    commands.endTime = NULL;
    commands.northLat = NULL;
    commands.southLat = NULL;
    commands.westLon = NULL;
    commands.eastLon = NULL;
    commands.frameStatus = NULL;
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
    if( (status = getArgInput (msgDesc, query ) )  < IMS_OK)
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
    ** Perform the query.
    */
    (void) sscanf( commands.northLat, "%f", &northLat_val );
    (void) sscanf( commands.southLat, "%f", &southLat_val );
    (void) sscanf( commands.westLon,  "%f", &westLon_val );
    (void) sscanf( commands.eastLon,  "%f", &eastLon_val );
    query->retPtr = (char *) NULL;

    if ((status = ims_spatialQuery ( query, commands.platform,
        commands.mode, commands.startTime, commands.endTime,
        northLat_val, southLat_val, westLon_val, eastLon_val,
        commands.frameStatus, &frame_count ) )  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not perform spatial/temporal frame query.");
        goto ERROR;
    }

    /*
    ** Print out the results.
    */
    (void) printf ("\nSpatial Query Results\n");
    (void) printf ("start time = %s    end time = %s\n", commands.startTime,
        commands.endTime );
    (void) printf ("north lat = %s    south lat = %s\n", commands.northLat,
        commands.southLat );
    (void) printf ("west lon = %s    east lon = %s\n", commands.westLon,
        commands.eastLon );
    (void) printf ("frameStatus = %s\n", commands.frameStatus );

    (void) printf ("no. of frames returned = %ld\n", frame_count );
    fs_results = (pnt_ims_FrameResults_t) query->retPtr;
    for( i=0 ; i  <  frame_count ; i++ ){
        printf( "\n  *****  Frame no. %ld\n",  i+1 );
        printf( "start_time             = %s\n", fs_results->start_time   );
        printf( "frame_status           = %s\n", fs_results->frame_status );
        printf( "media_id               = %s\n", fs_results->media_id     );
        printf( "revolution             = %d\n", fs_results->revolution   );
        printf( "sequence               = %d\n", fs_results->sequence     );
        printf( "mode                   = %s\n", fs_results->mode         );
        printf( "platform               = %s\n", fs_results->platform     );
        printf( "frame_id               = %d\n", fs_results->frame_id     );
        printf( "sensor                 = %s\n", fs_results->sensor       );
        printf( "activity_id            = %s\n", fs_results->activity_id  );
        printf( "asc_desc               = %s\n", fs_results->asc_desc     );
        printf( "end_time               = %s\n", fs_results->end_time     );
        printf( "center_time            = %s\n", fs_results->center_time  );
        printf( "center_lat             = %f\n", fs_results->center_lat   );
        printf( "center_lon             = %f\n", fs_results->center_lon   );
        printf( "near_start_lat         = %f\n", fs_results->near_start_lat );
        printf( "near_start_lon         = %f\n", fs_results->near_start_lon );
        printf( "near_end_lat           = %f\n", fs_results->near_end_lat );
        printf( "near_end_lon           = %f\n", fs_results->near_end_lon );
        printf( "far_start_lat          = %f\n", fs_results->far_start_lat);
        printf( "far_start_lon          = %f\n", fs_results->far_start_lon);
        printf( "far_end_lat            = %f\n", fs_results->far_end_lat  );
        printf( "far_end_lon            = %f\n", fs_results->far_end_lon  );
        printf( "frame_mode             = %s\n", fs_results->frame_mode   );
        printf( "station_id             = %s\n", fs_results->station_id   );
        printf( "scan_results_file      = %s\n", fs_results->scan_results_file  );
        fs_results = fs_results->next;
    }

    (void) freeFrameResults( (pnt_ims_FrameResults_t)  query->retPtr );
    status = ims_closeQueryConnection( query );
    (void) ims_msgStructFree (msgDesc);
    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_spatialQuery failed.");
    if(  query->retPtr != NULL  ){
        (void) freeFrameResults(  (pnt_ims_FrameResults_t)
            query->retPtr );
    }
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
    IMS_CMN_QUERY *query )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long  i,j,k;


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
            "Platform: (e.g. ERS-1) ") == (char *) NULL)
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

    /* startTime */
    if (commands.startTime  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "StartTime: (YYYY-DDDTHH:MM:SS.fff) ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.startTime = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.startTime, inputBuffer);
    }

    /* endTime */
    if (commands.endTime  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "EndTime: (YYYY-DDDTHH:MM:SS.fff) ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.endTime = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.endTime, inputBuffer);
    }

    /* northLat */
    if (commands.northLat  ==  (char *) NULL)
    {
        try_again:;
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "northLat (degrees): ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        k = 0;
        if(  inputBuffer[0]  ==  '+'  ||  inputBuffer[0]  ==  '-' ) k++;
        /*  make sure all are numbers  */
        for( j=k ; j  <  i-1 ; j++  ){
            if(  !isdigit(  inputBuffer[j]  ) &&
                inputBuffer[j]  !=  '.' ){
                printf( " *** Value not valid.  Try again.\n"  );
                goto try_again;
            }
        }
        commands.northLat = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.northLat, inputBuffer);
    }

    /* southLat */
    if (commands.southLat  ==  (char *) NULL)
    {
        try_again2:;
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "southLat (degrees): ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        k = 0;
        if(  inputBuffer[0]  ==  '+'  ||  inputBuffer[0]  ==  '-' ) k++;
        /*  make sure all are numbers  */
        for( j=k ; j  <  i-1 ; j++  ){
            if(  !isdigit(  inputBuffer[j]  ) &&
                inputBuffer[j]  !=  '.' ){
                printf( " *** Value not valid.  Try again.\n"  );
                goto try_again2;
            }
        }
        commands.southLat = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.southLat, inputBuffer);
    }

    /* westLon */
    if (commands.westLon  ==  (char *) NULL)
    {
        try_again3:;
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "westLon (degrees): ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        k = 0;
        if(  inputBuffer[0]  ==  '+'  ||  inputBuffer[0]  ==  '-' ) k++;
        /*  make sure all are numbers  */
        for( j=k ; j  <  i-1 ; j++  ){
            if(  !isdigit(  inputBuffer[j]  ) &&
                inputBuffer[j]  !=  '.' ){
                printf( " *** Value not valid.  Try again.\n"  );
                goto try_again3;
            }
        }
        commands.westLon = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.westLon, inputBuffer);
    }

    /* eastLon */
    if (commands.eastLon  ==  (char *) NULL)
    {
        try_again4:;
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "eastLon (degrees): ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        k = 0;
        if(  inputBuffer[0]  ==  '+'  ||  inputBuffer[0]  ==  '-' ) k++;
        /*  make sure all are numbers  */
        for( j=k ; j  <  i-1 ; j++  ){
            if(  !isdigit(  inputBuffer[j]  ) &&
                inputBuffer[j]  !=  '.' ){
                printf( " *** Value not valid.  Try again.\n"  );
                goto try_again4;
            }
        }
        commands.eastLon = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.eastLon, inputBuffer);
    }

    /* frameStatus */
    if (commands.frameStatus  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "FrameStatus: (e.g. SCHEDULED) ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.frameStatus = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.frameStatus, inputBuffer);
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
