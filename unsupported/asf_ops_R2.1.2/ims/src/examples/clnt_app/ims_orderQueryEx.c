static char *sccs = "@(#)ims_orderQueryEx.c	1.1  07/19/96";
/*******************************************************************
**
** File:        ims_orderQueryEx.c
**
** Function:    Perform query on order_id to get the status of
**              of its items.
**
** Author:      David Pass
**
** Date:        6/6/96
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
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, long * );

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *orderId;
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
    {"-O",                  &commands.orderId},
    {"+orderId",            &commands.orderId},
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
    {"orderId",         &commands.orderId},
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
    pnt_ims_OrderResults_t oi_results;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    IMS_CMN_QUERY *query;
    long    item_count; /* no. frames returned  */
    long    order_id;


    query = (IMS_CMN_QUERY *) malloc( sizeof( IMS_CMN_QUERY ));

    commands.username = NULL;
    commands.password = NULL;
    commands.orderId = NULL;
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
    if( (status = getArgInput (msgDesc, query, &order_id ) )  < IMS_OK)
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
    query->retPtr = (char *) NULL;

    if ((status = ims_orderQuery ( query, order_id, &item_count ) )
        <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not perform ims_orderQuery.");
        goto ERROR;
    }

    /*
    ** Print out the results.
    */
    (void) printf ("\nOrder Query Results\n");
    (void) printf ("order_id = %ld    no. of items = %ld\n", order_id,
        item_count );
    oi_results = (pnt_ims_OrderResults_t) query->retPtr;
    for( i=0 ; i  <  item_count ; i++ ){
    printf( "\n  *****  Item no. %ld\n",  i+1 );
    printf( "order_id          = %ld\n",oi_results->order_id          );
    printf( "item_id           = %d\n", oi_results->item_id           );
    printf( "status_id         = %s\n", oi_results->status_id         );
    printf( "order_item_type_id= %s\n", oi_results->order_item_type_id);
    printf( "priority          = %d\n", oi_results->priority          );
    printf( "granule_idx       = %ld\n",oi_results->granule_idx       );
    printf( "granule_name      = %s\n", oi_results->granule_name      );
    printf( "platform          = %s\n", oi_results->platform          );
    printf( "sensor            = %s\n", oi_results->sensor            );
    printf( "dataset           = %s\n", oi_results->dataset           );
    printf( "p_granule_name    = %s\n", oi_results->p_granule_name    );
    printf( "p_granule_idx     = %ld\n",oi_results->p_granule_idx     );
    printf( "p_data_kbytes     = %ld\n",oi_results->p_data_kbytes     );
    printf( "p_metadata_kbytes = %ld\n",oi_results->p_metadata_kbytes );
    printf( "media_id          = %s\n", oi_results->media_id          );
    printf( "quicklook_p       = %c\n", oi_results->quicklook_p       );
    printf( "deleted_p         = %c\n", oi_results->deleted_p         );
    printf( "process_status    = %s\n", oi_results->process_status_id );
    printf( "step_name         = %s\n", oi_results->step_name         );
    printf( "op_comment        = %s\n", oi_results->op_comment        );
    printf( "process_comment   = %s\n", oi_results->process_comment   );

        oi_results = oi_results->next;
    }

    (void) freeOrderResults( (pnt_ims_OrderResults_t)  query->retPtr );
    status = ims_closeQueryConnection( query );
    (void) ims_msgStructFree (msgDesc);
    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_orderQuery failed.");
    if(  query->retPtr != NULL  ){
        (void) freeOrderResults(  (pnt_ims_OrderResults_t)
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
    IMS_CMN_QUERY *query,
    long  * order_id )
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

    /* order_id */
    if (commands.orderId  ==  (char *) NULL)
    {
        try_again:;
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "orderId: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        for( j=0 ; j  <  i ; j++  ){
            if(  !isdigit(  inputBuffer[j]  ) ){
                printf( " *** Value not valid.  Try again.\n"  );
                goto try_again;
            }
        }
        commands.orderId = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.orderId, inputBuffer);
    }
    /*
    ** convert to integer
    */
    *order_id = atol( commands.orderId );

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
