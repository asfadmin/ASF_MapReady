static char *sccs = "@(#)ims_pmfUpdate.c	1.1  05/15/97";
/* *****************************************************************
**
** File:        ims_pmfUpdate.c
**
** Function:    This program will update the metadata (odl) file for a
**              given record in a granules table.  The keywords are
**              obtained from the catalog, and the values are read in
**              the granules table.  these are then used to re-write
**              the pmf file.  only the catalog_metadata section is
**              re-done.  the prevous and latter stuff is left.
**
** Author:      David Pass
**
** Date:        4/2/97
**
******************************************************************* */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_childHandler.h>

/*
** Local Definitions.
*/
#define ZERO 0
#define  MAX_LINE  128   /* max length of line for pmf file */

/*
** Log File Information structure definition.
*/
typedef struct logInfo
{
    int logFlag;
    char *path;
    char *fileName;
} LOG_SPEC;


/*
**  these structures define the information for keywords
**      for granule variables
*/
typedef  struct  key_data__t *pnt_key_data_t;
typedef  struct  key_data__t {
    char   name[40];/* name of keyword  */
    short  keyword_idx;
    short  data_type;
    short  length; /* max. length of string or var */
    short  significance; /* if 3 or 4, optional  */
    char   line[MAX_LINE+1];
    pnt_key_data_t  pnt_next;
}   key_data_t;


/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, LOG_SPEC *);
static int openConnection (IMS_MSG_STRUCT *);
static int getGranulesTable (IMS_MSG_STRUCT *, short *, char *);
static int getGranulesLastIdx(IMS_MSG_STRUCT *, short, int *);
static int updateGranulePMF (IMS_MSG_STRUCT *, char *, short,
    int, pnt_key_data_t );
static int getGranulesKeywords( IMS_MSG_STRUCT *, short,
    pnt_key_data_t *, short * );
static int getGranulePath( IMS_MSG_STRUCT *, char *, short, int );
static int getGranuleName( IMS_MSG_STRUCT *, pnt_key_data_t, char *,
    int, char * );


/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
    char *username;
    char *password;
    char *dataset;
    char *startIdx;
    char *endIdx;
    char *logFilePath;
    char *logFileName;
    char *commandFile;
    char *server;
    char *database;
    char *logFlag;
    char *updateFlag;
    char *help;
    char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the command-line.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
    {"-U",              &commands.username},
    {"+username",       &commands.username},
    {"-P",              &commands.password},
    {"+password",       &commands.password},
    {"-D",              &commands.dataset},
    {"+dataset",        &commands.dataset},
    {"-S",              &commands.startIdx},
    {"+startIdx",       &commands.startIdx},
    {"-E",              &commands.endIdx},
    {"+endIdx",         &commands.endIdx},
    {"-L",              &commands.logFilePath},
    {"+logFilePath",    &commands.logFilePath},
    {"-N",              &commands.logFileName},
    {"+logFileName",    &commands.logFileName},
    {"-C",              &commands.commandFile},
    {"+commandFile",    &commands.commandFile},
    {"-X",              &commands.server},
    {"+server",         &commands.server},
    {"-Y",              &commands.database},
    {"+database",       &commands.database},
    {"-l",              &commands.logFlag},
    {"+logFlag",        &commands.logFlag},
    {"-u",              &commands.updateFlag},
    {"+updateFlag",     &commands.updateFlag},
    {"-h",              &commands.help},
    {"+help",           &commands.help},
    {"-r",              &commands.release},
    {"+release",        &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from the command-file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",        &commands.username},
    {"password",        &commands.password},
    {"dataset",         &commands.dataset},
    {"startIdx",        &commands.startIdx},
    {"endIdx",          &commands.endIdx},
    {"logFilePath",     &commands.logFilePath},
    {"logFileName",     &commands.logFileName},
    {"server",          &commands.server},
    {"database",        &commands.database},
    {"logFlag",         &commands.logFlag},
    {"updateFlag",      &commands.updateFlag}

};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
IMS_QI_DESC_OBJ *qDesc;
static char *programName;
static char *username;
static char *password;
static char *dataset;
static int  startIdx;
static int  endIdx;
static char *server;
static char *database;
static int  updateFlag;
static char cmdBuf[2048];


/* **************************************************************
**
** main ()
**
**************************************************************** */

void main (
    int argc,
    char *argv[])
{
    IMS_MSG_STRUCT *msgDesc;
    LOG_SPEC logInfo;
    char logFileSpec[IMS_PATH_LEN+1];
    char writeBuffer[100];
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    int fd;
    char granules_table[IMS_COL30_LEN+1];
    int granule_idx;
    short  dataset_idx;
    pnt_key_data_t  pnt_keys_1st;
    pnt_key_data_t  pnt_keys;
    short  num_keywords;
    long  n_done;  /*  no. granules PMF files changed.  */


    /*
    ** Initialize variables.
    */
    qDesc = (IMS_QI_DESC_OBJ *) NULL;
    (void) memset (&logInfo, 0, (size_t) sizeof (LOG_SPEC));

    /*
    ** Get the program name and the node name.
    */
    programName = ims_extractFileName (argv[0]);
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
    (void) ims_msgProgramName (msgDesc, programName);
    (void) sprintf (banner, "%s::%s", hostName, programName);
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
    ** Get the command-line arguments. The variable status will actually
    ** contain the number of command-line arguments processed upon
    ** successful completion.
    */
    if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
        cmdLineElmCount, msgDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occurred parsing the command-line.");
        goto ERROR;
    }

    /*
    ** Check to see if we got everything off of the command-line.
    */
    if (status < argc)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Only %d out of the %d command-line arguments were "
            "processed.", status, argc);

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
        (void) ims_msgStructFree (msgDesc);
        exit (0);
    }

    /*
    ** If there is a command-file present, then get any commands from
    ** this file, then overlay all commands from the command-line,
    ** except password, which will be gone by this point.
    */
    if (commands.commandFile != (char *) NULL)
    {
        if ((status = ims_getFileParms (commands.commandFile,
            cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "An error occurred parsing the command-file.");
            goto ERROR;
        }

        /*
        ** Now, get command-line arguments again to overlay file
        **  arguments.
        */
        if ((status = ims_getCmdLine (argc, argv,
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "An error occurred parsing the command-line.");
            goto ERROR;
        }
    }

    /*
    ** Process the information from command-line and/or command-file.
    */
    if ((status = getArgInput (msgDesc, &logInfo)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Set-up the log file if requested.
    */
    if (logInfo.logFlag == IMS_TRUE)
    {
        /*
        ** Assemble the log file specification.
        */
        ims_concatFilePath(logFileSpec, logInfo.path, logInfo.fileName);

        /*
        ** Open the log file.
        */
        if ((fd = open (logFileSpec, O_WRONLY|O_CREAT|O_APPEND)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not open log file '%s'. %s",
                logFileSpec, strerror (errno));
            goto ERROR;
        }

        /*
        ** Change the mode of the log file.
        */
        if (chmod (logFileSpec, 0644) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not change the mode for log file '%s'. %s",
                logFileSpec, strerror (errno));
            goto ERROR;
        }

        /*
        ** Enable message writting to the log file.
        ** Disable message writting to stderr.
        */
        if ((status = ims_msgLogFileDesc (msgDesc, fd)) < IMS_OK)
        {
            goto ERROR;
        }
        (void) ims_msgStderrFlag (msgDesc, IMS_OFF);

        /*
        ** Write a delimiting message to the log file.
        */
        (void) sprintf (writeBuffer,
            "\n\n>>>>>>>>>>>>>>>>>>>>>>  PMF Update Startup  "
            "<<<<<<<<<<<<<<<<<<<<<<\n\n");

        if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not write to log file '%s'. %s",
                logFileSpec, strerror (errno));
            goto ERROR;
        }
    }

    /*
    ** Open a connection to the database server.
    */
    if ((status = openConnection (msgDesc)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Check to see if the dataset passed in resembles a granules
    ** table name.  If so use it, otherwise use the dataset name
    ** to get the granules table name.
    */
    if ((strncmp (dataset, "granules_", (size_t) 9) == 0) &&
        ((int) strlen (dataset)  <=  IMS_COL30_LEN))
    {
        (void) strcpy (granules_table, dataset);
        (void) strcpy (dataset, "UNKNOWN");
        dataset_idx = atoi( granules_table+9 );
    }
    else /* Query for the granules table name. */
    {
        if ((status = getGranulesTable( msgDesc, &dataset_idx,
            granules_table ) )  <  IMS_OK ){
            (void) ims_msg (msgDesc, status,
                "Could not get granule table name for dataset '%s'.",
                dataset);
            goto ERROR;
        }
    }

    /*
    **  get the start and end index for the granules
    */
    if(  startIdx  <=  0  )  startIdx = 1;
    if(  endIdx  <=  0  ){
        /*
        **  get the last granule index
        */
        status = getGranulesLastIdx( msgDesc, dataset_idx, &endIdx );
        if(  status  <  IMS_OK ){
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get last granule index for dataset_idx = %d",
                dataset_idx );
            goto ERROR;
        }
    }

    /*
    **  get the keywords for this dataset
    */
    status = getGranulesKeywords( msgDesc, dataset_idx, &pnt_keys_1st,
        &num_keywords );
    if(  status  <  IMS_OK ){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get last granule keywords for dataset_idx = %d",
            dataset_idx );
        goto ERROR;
    }

    /*
    ** Display some informational messages.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Starting the PMF update at index '%d' "
        "in granules table '%s' for dataset '%s' with %d keywords.",
        startIdx, granules_table, dataset, num_keywords );

    if (updateFlag == IMS_FALSE)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "The record update flag has been turned off so "
            "instead of updating the records the new "
            "metadata will be displayed.");
    }

    /*
    ** Calculate and update the metadata for each
    ** granule in the range.
    */
    n_done = 0;
    for (granule_idx = startIdx; granule_idx < endIdx+1; granule_idx++)
    {
        if ((status = updateGranulePMF (msgDesc, granules_table,
            dataset_idx, granule_idx, pnt_keys_1st ) ) < IMS_WARNING)
        {
            (void) ims_msg (msgDesc, status,
                "Could not update the granule record for index '%d' "
                "in granules table '%s' for dataset '%s'.",
                granule_idx, granules_table, dataset);
            goto ERROR;
        }

        if (status == IMS_WARNING)
        {
            (void) ims_msg (msgDesc, status,
                "A granule record did not exist for index '%d' in "
                "granules table '%s' for dataset '%s'.",
                granule_idx, granules_table, dataset);
        }
        else  n_done++;
    }

    (void) ims_msg (msgDesc, IMS_INFO,
        "Ending the PMF update at index '%d' "
        "in granules table '%s'.  %ld PMF files changed.",
        endIdx, granules_table, n_done );

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "PMF update processing completed successfully.");

    (void) ims_qiFreeDesc (qDesc);
    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "PMF update processing failed.");

    if (qDesc != (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_qiFreeDesc (qDesc);
    }
    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);

    exit (1);
}   /* main */


/***************************************************************
**
**  subr runDown ()
**
**  Cleanup and exit from program.
**
**************************************************************** */
static int runDown (
    int sig)
{
    /* Print out the signal caught. */
    (void) fprintf (stderr,
        "\n\nTermination of %s due to signal: %s (%d)\n\n",
        programName, ims_sigMsg (sig), sig);

    return (sig);
}   /*  runDown  */


/***************************************************************
**
**  subr usage ()
**
**  Print command line argument switches.
**
**************************************************************** */
static void usage (void)
{
    int i;

    (void) fprintf (stderr,
        "\n%s command-line arguments:\n\n", programName);

    for (i = 0; i < cmdLineElmCount; i++)
    {
        (void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
    }

    (void) fprintf (stderr, "\n\n");
}   /*  usage  */


/***************************************************************
**
**  subr getArgInput ()
**
**  Process command-line and command-file arguments.
**
**************************************************************** */
static int getArgInput (
    IMS_MSG_STRUCT *msgDesc,
    LOG_SPEC *logInfo)
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    int number;
    int invalid;
    int i;

    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username != (char *) NULL)
    {
        username = commands.username;
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

        username = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (username, inputBuffer);
    }

    /* password */
    if (commands.password != (char *) NULL)
    {
        password = commands.password;
    }
    else
    {
        if (ims_getPassword (inputBuffer) == NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        password = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (password, inputBuffer);
    }

    /* dataset */
    if (commands.dataset != (char *) NULL)
    {
        dataset = commands.dataset;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Dataset: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        dataset = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (dataset, inputBuffer);
    }

    /* startIdx */
    /*
    **  if not input, assume 0 (starts at 1)
    */
    number = 0;
    startIdx = 0;
    if (commands.startIdx != (char *) NULL)
    {
        if (ims_isInteger (commands.startIdx) == IMS_TRUE)
        {
            number = (int) atoi (commands.startIdx);
            if ((number < ZERO) || (number > IMS_MAX_INT))
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "The parameter 'startIdx' has a value of '%d', "
                    "which is not in the range of '%d' to '%d'.",
                    number, ZERO, IMS_MAX_INT);
                return (IMS_ERROR);
            }
            else
            {
                startIdx = (int) number;
            }
        }
        else
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
               "The parameter 'startIdx' must have a valid integer"
                " value.");
            return (IMS_ERROR);
        }
    }

    /* endIdx */
    number = 0;
    endIdx = 0;
    /*
    **  if not input, get the largest number from last_granule_idx
    */
    if (commands.endIdx != (char *) NULL)
    {
        if (ims_isInteger (commands.endIdx) == IMS_TRUE)
        {
            number = (int) atoi (commands.endIdx);
            if ((number < ZERO) || (number > IMS_MAX_INT))
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "The parameter 'endIdx' has a value of '%d', "
                    "which is not in the range of '%d' to '%d'.",
                    number, ZERO, IMS_MAX_INT);
                return (IMS_ERROR);
            }
            else if (number < startIdx)
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "The parameter 'endIdx' has a value of '%d', "
                    "which is not greater than or equal to the "
                    "starting index of '%d'.",
                    number, startIdx);
                return (IMS_ERROR);
            }
            else
            {
                endIdx = (int) number;
            }
        }
        else
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The parameter 'endIdx' must have a valid integer "
                "value.");
            return (IMS_ERROR);
        }
    }

    /* logFilePath */
    if (commands.logFilePath != (char *) NULL)
    {
        logInfo->path = commands.logFilePath;
    }
    else
    {
        logInfo->path = ".";
    }

    /* logFileName */
    if (commands.logFileName != (char *) NULL)
    {
        logInfo->fileName = commands.logFileName;
    }
    else
    {
        logInfo->fileName = malloc ((size_t) 9 + IMS_PROGRAM_LEN +
            10 + 1);
        (void) sprintf (logInfo->fileName, "%s_%s.log",
            programName, ims_timeStamp ());
    }

    /* server */
    if (commands.server != (char *) NULL)
    {
        server = commands.server;
    }

    /* database */
    if (commands.database != (char *) NULL)
    {
        database = commands.database;
    }

    /* logFlag */
    if (commands.logFlag != (char *) NULL)
    {
        logInfo->logFlag = IMS_TRUE;
    }
    else
    {
        logInfo->logFlag = IMS_FALSE;
    }

    /* updateFlag */
    if (commands.updateFlag == (char *) NULL)
    {
        updateFlag = IMS_FALSE;
    }
    else
    {
        if(  isupper( commands.updateFlag[0] )  )
            commands.updateFlag[0] = tolower( commands.updateFlag[0] );
        if(  commands.updateFlag[0]  ==  'n'  ) {
            updateFlag = IMS_FALSE;
        }
        else{
            updateFlag = IMS_TRUE;
        }
    }

    return (IMS_OK);
}   /* getArgInput */


/*******************************************************************
**
**  subr openConnection ()
**
**  Open a connection to the database server.
**
******************************************************************** */
static int openConnection (
    IMS_MSG_STRUCT *msgDesc)
{
    int status;

    /*
    ** Allocate a query descriptor.
    */
    if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return (IMS_FATAL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, username);

    IMS_SETPSWD (qDesc, password);

    IMS_SETPROG (qDesc, programName);

    if (server != (char *) NULL)
    {
        IMS_SETSERVER (qDesc, server);
    }

    if (database != (char *) NULL)
    {
        IMS_SETDBNAME (qDesc, database);
    }

    IMS_SET_VERBOSE (qDesc, 10);

    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Login to the catalog database.
    */
    if ((status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        return (status);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (IMS_OK);
}   /*  openConnection  */


/*******************************************************************
**
**  subr getGranulesTable ()
**
**  Query the database server for the granule table name.
**
******************************************************************** */
static int getGranulesTable (
    IMS_MSG_STRUCT *msgDesc,
    short *dataset_idx,
    char *granules_table)
{
    int status;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select granules_table, d.dataset_idx "
        "from dataset_policy d, dataset_relation r "
        "where r.dataset = '%s' and r.dataset_idx = d.dataset_idx",
        dataset);

    /*
    ** Process the result row for this query.
    */
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (status);
        }
    }

    /*
    ** See if we got one row back.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }

    /*
    ** Get the returned data.
    */

    /* granules_table */
    (void) memcpy (granules_table,
        qDesc->valAddr[0], qDesc->valLength[0]);
    granules_table[qDesc->valLength[0]] = '\0';

    /* dataset_idx */
    (void) memcpy ( dataset_idx,
        qDesc->valAddr[1], qDesc->valLength[1]);

    return (IMS_OK);
}   /*  getGranulesTable  */


/*******************************************************************
**
**  subr getGranulesLastIdx()
**
**  Query the database server for the granule's last index
**
******************************************************************** */
static int getGranulesLastIdx(
    IMS_MSG_STRUCT *msgDesc,
    short dataset_idx,
    int * last_idx )
{
    int status;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select granule_idx from  last_granule_idx  where "
        "   dataset_idx = %d", dataset_idx );

    /*
    ** Process the result row for this query.
    */
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (status);
        }
    }

    /*
    ** See if we got one row back.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }

    /*
    ** Get the returned data.
    */

    /* granules_table */
    (void) memcpy ( last_idx,
        qDesc->valAddr[0], qDesc->valLength[0]);

    return (IMS_OK);
}   /*  getGranulesLastIdx  */


/*******************************************************************
**
**  subr getGranulesKeywords()
**
**  Query the database to obtain the keywords and information about
**      the keywords
**
******************************************************************** */
static int getGranulesKeywords(
    IMS_MSG_STRUCT *msgDesc,
    short dataset_idx,
    pnt_key_data_t *pnt_keys_1st,
    short *num_keywords )
{
    int status;
    pnt_key_data_t  pnt_keys;
    pnt_key_data_t  pnt_keys_last;
    short  rowCount;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select ks.keyword_idx, ks.significance, kp.keyword, "
        "kp.data_type, kp.max_len   from  keyword_set ks, "
        "keyword_policy kp   where   ks.dataset_idx = %d  and  "
        " ks.keyword_idx = kp.keyword_idx   order by ks.position",
        dataset_idx );

    /*
    ** Process the result row for this query.
    */
    rowCount = 0;
    pnt_keys_last = NULL;
    pnt_keys = *pnt_keys_1st;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "getGranulesKeywords: Error from db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Grab data: get structure
        */
        pnt_keys = (pnt_key_data_t) malloc( sizeof( key_data_t ) );
        pnt_keys->line[0] = '\0';
        pnt_keys->pnt_next = NULL;
        if(  pnt_keys_last  !=  NULL  )
            pnt_keys_last->pnt_next = pnt_keys;
        else  *pnt_keys_1st = pnt_keys;
        pnt_keys_last = pnt_keys;

        (void) memcpy ( &(pnt_keys->keyword_idx),
            qDesc->valAddr[0], qDesc->valLength[0]);

        (void) memcpy ( &(pnt_keys->significance),
            qDesc->valAddr[1], qDesc->valLength[1]);

        (void) memcpy ( (char *) pnt_keys->name,
            qDesc->valAddr[2], qDesc->valLength[2]);
        pnt_keys->name[qDesc->valLength[2]] = '\0';
        ims_truncStr(  pnt_keys->name );

        (void) memcpy ( &(pnt_keys->data_type),
            qDesc->valAddr[3], qDesc->valLength[3]);

        (void) memcpy ( &(pnt_keys->length),
            qDesc->valAddr[4], qDesc->valLength[4]);

    }

    /*
    ** See if we got one row back.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_ERROR);
    }
    *num_keywords = rowCount;
    return (IMS_OK);
}   /*  getGranulesKeywords  */


/*******************************************************************
**
** subr updateGranulePMF ()
**
** Update the metadata.
**
******************************************************************** */
static int updateGranulePMF (
    IMS_MSG_STRUCT *msgDesc,
    char *granules_table,
    short  dataset_idx,
    int granule_idx,
    pnt_key_data_t pnt_keys_1st )
{
    pnt_key_data_t  pnt_keys;
    int status;
    char  path[256];/* path of pmf file  */
    char  name[65]; /* name of pmf file without path  */
    char  str[1024], str2[1024];
    FILE  *in_file, *out_file;
    char  pmf_name[256]; /* full name of pmf file */
    char  pmf_new_name[256]; /* full name of new pmf file  */
    short  in_cat; /* if true, reading lines inside of the catalog
            metadata section  */
    short  n_lines, n_lines_out;
    int   i,j;


    /*
    **  first we need the path to the metadata file
    */
    status = getGranulePath( msgDesc, path, dataset_idx, granule_idx );
    if(  status  <  IMS_OK ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "updateGranulePMF: Could not obtain path for dataset"
            " %d, granule %ld.", dataset_idx, granule_idx );
        return( IMS_ERROR );
    }

    /*
    **  now we need to get the name of the file: this is in the granule
    **      if it exists.
    */
    status = getGranuleName( msgDesc, pnt_keys_1st, granules_table,
        granule_idx, name );
    if(  status  <  IMS_OK ){
        return( status );
    }

    strcpy( pmf_name, path );
    i = strlen( path );
    if(  path[i-1]  !=  '/'  )  strcat( pmf_name, "/" );
    strcat( pmf_name, name );
    strcat( pmf_name, ".M" );

    /*
    **  now open the PMF file
    */
    if( (in_file = fopen ( pmf_name, "r")) == NULL){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Cannot open PMF file %s.", pmf_name );
        return( IMS_ERROR );
    }

    if(  updateFlag  ){
        /*
        **  first check if user has access to write in the dir
        */
        status = access( path, W_OK );
        if(  status  !=  0 ){ /* do not have write access to dir */
            (void) ims_msg( msgDesc, IMS_INFO,
                "User cannot write to PMF repository %s.",
                path );
            return( IMS_ERROR );
        }
        strcpy( pmf_new_name, path );
        i = strlen( path );
        if(  path[i-1]  !=  '/'  )  strcat( pmf_new_name, "/" );
        strcat( pmf_new_name, name );
        strcat( pmf_new_name, "_temp" );
        strcat( pmf_new_name, ".M" );
        if( (out_file = fopen ( pmf_new_name, "w")) == NULL){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Cannot open new PMF file %s.", pmf_new_name );
            return( IMS_ERROR );
        }
    }
    else{
        printf( "\n ******  Reading file %s  *******\n", pmf_name );
    }
    in_cat = IMS_FALSE;
    n_lines = 0;
    n_lines_out = 0;
    while(  fgets( str, MAX_LINE, in_file )  !=  (char *) NULL ){
        n_lines++;
        if(  !in_cat  ){
            /*
            **  not in catalog section: print it out
            */
            if(  updateFlag ){
                (void) fprintf( out_file, "%s", str );
            }
            else{
                (void) printf( "%s", str );
            }
            n_lines_out++;
            /*
            ** check for start of catalog section
            */
            if(  ims_strIndex( str, "OBJECT" )  !=  -1  &&
                ims_strIndex( str, "CATALOG_METADATA" )  !=  -1  ){
                /*
                **  start of catalog section: put in new lines
                **      first get the indent.  get the no. of blanks
                **      in the current line and add 3 blanks.
                */
                j = 0;
                for( i=0 ; i  <  strlen( str )  &&  str[i]  ==  ' ' ;
                    i++ ) j++;
                strncpy( str2, str, j );
                str2[j] = '\0';
                strcat( str2, "    "  );
                in_cat = IMS_TRUE;
                pnt_keys = pnt_keys_1st;
                while( pnt_keys  !=  NULL ){
                    n_lines_out++;
                    if(  pnt_keys->line[0]  !=  '\0' ){
                        /*
                        **  if null, do not put in file
                        */
                        if(  updateFlag ){
                            (void) fprintf( out_file, "%s%s\n",
                                str2, pnt_keys->line );
                        }
                        else{
                            (void) printf( "%s%s\n", str2,
                                pnt_keys->line );
                        }
                    }
                    pnt_keys = pnt_keys->pnt_next;
                }
            }
        }
        else{
            /*
            **  in cat section: do not write: check for end
            */
            if(  ims_strIndex( str, "END_OBJECT" )  !=  -1  &&
                ims_strIndex( str, "CATALOG_METADATA" )  !=  -1  ){
                /*
                **  end of catalog section: put in this line
                */
                in_cat = IMS_FALSE;
                if(  updateFlag ){
                    (void) fprintf( out_file, "%s", str );
                }
                else{
                    (void)  printf( "%s", str );
                }
                n_lines_out++;
            }
        }
    }
    fclose( in_file );
    if(  updateFlag  ){
        fclose( out_file );
        /*
        **  need to name the output file to the old name
        **      For now, keep the original file
        */
        strcpy( str, path );
        i = strlen( path );
        if(  path[i-1]  !=  '/'  )  strcat( str, "/" );
        strcat( str, name );
        strcat( str, "_PMF_TEMP" );
        strcat( str, ".M" );

        strcpy( str2, "/bin/mv " );
        strcat( str2, pmf_name );
        strcat( str2, "  " );
        strcat( str2, str );
        status = system( str2 );
        if(  status  <  0  ){ /* rename failed: put locally */
            strcpy( str, name );
            strcat( str, "_PMF_TEMP" );
            strcat( str, ".M" );

            strcpy( str2, "/bin/mv " );
            strcat( str2, pmf_name );
            strcat( str2, "  " );
            strcat( str2, str );
            status = system( str2 );
            if(  status  <  0  ){ /* rename locally failled  */
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Could not rename original PMF file name %s.",
                    pmf_name );
                return( IMS_ERROR );
            }
        }
        /*
        **  now rename the new file to the orig name
        */
        strcpy( str2, "/bin/mv " );
        strcat( str2, pmf_new_name );
        strcat( str2, "  " );
        strcat( str2, pmf_name );
        status = system( str2 );
        if(  status  <  0  ){ /* rename locally failled  */
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Could not rename new PMF file to original name %s.",
                pmf_name );
            return( IMS_ERROR );
        }
    }

    return (IMS_OK);
}   /*  updateGranulePMF */


/*******************************************************************
**
**  subr getGranulePath
**
**  Query the database to obtain the path for the given dataset,
**      granule.  due to ranges, last results saved.
**
******************************************************************** */
static int getGranulePath(
    IMS_MSG_STRUCT *msgDesc,
    char  *path_out,
    short dataset_idx,
    int  granule_idx )
{
    int status;
    pnt_key_data_t  pnt_keys;
    short  rowCount;
    static  char  last_path[256];
    static  short  last_dataset_idx = 0;
    static  int  last_start_gran;
    static  int  last_end_gran;
    char  path[256];
    int  i1, i2;
    short  found;


    /*
    **  check if last path is valid
    */
    if(  dataset_idx  ==  last_dataset_idx   &&
        last_start_gran  <=  granule_idx  &&
        last_end_gran  >=  granule_idx  ){
        strcpy( path_out, last_path );
        return( IMS_OK );
    }

    last_dataset_idx = dataset_idx;
    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "getGranulePath: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select path, start_granule_idx, end_granule_idx  from  "
        "  dataset_path_policy   where  dataset_idx = %d",
        dataset_idx );

    /*
    ** Process the result row for this query.
    */
    found = IMS_FALSE;
    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "getGranulePath: Error from db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Grab data
        */

        (void) memcpy ( path,
            qDesc->valAddr[0], qDesc->valLength[0]);
        path[qDesc->valLength[0]] = '\0';
        ims_truncStr(  path );

        /*  start_granule_idx  */
        if(  qDesc->valLength[1]  ==  0  )  i1 = -1;
        else{
            (void) memcpy ( &i1,
                qDesc->valAddr[1], qDesc->valLength[1]);
        }

        /*  end_granule_idx  */
        if(  qDesc->valLength[2]  ==  0  )  i2 = -1;
        else{
            (void) memcpy ( &i2,
                qDesc->valAddr[2], qDesc->valLength[2]);
        }
        if(  i1  ==  -1  &&  i2  ==  -1  ){
            /*
            **  same path for all granules
            */
            last_start_gran = 1;
            last_end_gran = 2000000000;
            strcpy( last_path, path );
            strcpy( path_out, path );
            found = IMS_TRUE;
        }
        else  if(  i1  ==  -1  &&  i2  <=  granule_idx  ){
            /*
            **  found path: set saved limits
            */
            last_start_gran = 1;
            last_end_gran = i2;
            strcpy( last_path, path );
            strcpy( path_out, path );
            found = IMS_TRUE;
        }
        else  if(  i1  >=  granule_idx  &&  i2  ==  -1  ){
            /*
            **  found path: set saved limits
            */
            last_start_gran = i1;
            last_end_gran = 2000000000;
            strcpy( last_path, path );
            strcpy( path_out, path );
            found = IMS_TRUE;
        }
        else  if(  i1  >=  granule_idx  &&  i2  <=  granule_idx  ){
            /*
            **  found path: set saved limits
            */
            last_start_gran = i1;
            last_end_gran = i2;
            strcpy( last_path, path );
            strcpy( path_out, path );
            found = IMS_TRUE;
        }
    }

    /*
    ** See if we found the path
    */
    if(  found  ==  IMS_FALSE  ){
        return (IMS_ERROR);
    }
    return (IMS_OK);
}   /*  getGranulePath  */


/*******************************************************************
**
**  subr getGranuleName
**
**  Query the database to obtain the name in the granule dataset.
**
******************************************************************** */
static int getGranuleName(
    IMS_MSG_STRUCT *msgDesc,
    pnt_key_data_t  pnt_keys_1st,
    char  *granule_table,
    int  granule_idx,
    char  *name )
{
    int status;
    short  rowCount;
    static  char  keyword_section[2048];
    static  short  first_time = 0;
    pnt_key_data_t  pnt_keys;
    char  i_c;
    short i_s;
    int   i_l;
    float  i_float;
    double i_double;
    char   temp[2048];
    char  str[2048];
    long  i,j,k;


    /*
    **  if first time, set up the keywords for the select
    */
    if(  first_time  ==  0  ){
        first_time = 1;
        /*
        **  go through the keywords and put their names in
        */
        pnt_keys = pnt_keys_1st;
        keyword_section[0] = '\0';
        while( pnt_keys  !=  NULL  ){
            strcat( keyword_section, pnt_keys->name );
            strcat( keyword_section, ", " );
            pnt_keys = pnt_keys->pnt_next;
        }
        i = strlen( keyword_section );
        keyword_section[i-2] = '\0'; /* take out the last comma */
    }

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "getGranuleName: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select name, %s  from  %s  where  granule_idx = %ld",
        keyword_section, granule_table, granule_idx );
    i = strlen( cmdBuf );

    /*
    ** Process the result row for this query.
    */
    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "getGranuleName: Error from db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Grab data
        */

        (void) memcpy ( name,
            qDesc->valAddr[0], qDesc->valLength[0]);
        name[qDesc->valLength[0]] = '\0';
        ims_truncStr(  name );

        /*
        **  now read all the keywords: they are in different formats,
        **      mostly character,  but some short, long, float.  I am
        **      also doing datetime as a char.
        **  data_type:  1   int-1
        **              2   int-2
        **              3   int-4
        **              4   float-4
        **              5   float-8
        **              6   char
        **              7   symbol
        **              8   string
        **              9   date-time
        **              10  doy-time
        */
        pnt_keys = pnt_keys_1st;
        j = 0;
        while( pnt_keys  !=  NULL  ){
            j++;
            if(  qDesc->valLength[j]  ==  0  ){
                /*
                **  this value is null, so do not put it in
                */
                pnt_keys->line[0] = '\0';
            }
            else{
                i = pnt_keys->data_type;
                if(  i  <=  3  ){
                    if(  i  ==  1  )  {
                        memcpy( &i_c, qDesc->valAddr[j],
                            qDesc->valLength[j]);
                        i = i_c;
                    }
                    else if(  i  ==  2  ){
                        memcpy( &i_s, qDesc->valAddr[j],
                            qDesc->valLength[j]);
                        i = i_s;
                    }
                    else{
                        memcpy( &i_l, qDesc->valAddr[j],
                            qDesc->valLength[j]);
                        i = i_l;
                    }
                    (void) sprintf( str, "%s = %ld",
                        pnt_keys->name, i );
                    k = strlen( str );
                    if(  k  >  MAX_LINE  ){
                        (void) ims_msg( msgDesc, IMS_ERROR,
                            "Length of line too big for keyword %s: "
                            "only %d allowed, %ld needed.",
                            pnt_keys->name, MAX_LINE, k );
                        return( IMS_ERROR );
                    }
                    strcpy( pnt_keys->line, str );
                }
                else  if(  i  <=  5  ){
                    /*
                    **  float: single or double
                    */
                    if(  i  ==  4  )  {
                        memcpy( &i_float, qDesc->valAddr[j],
                            qDesc->valLength[j]);
                        i_double = i_float;
                    }
                    else{
                        memcpy( &i_double, qDesc->valAddr[j],
                            qDesc->valLength[j]);
                    }
                    (void) sprintf( str, "%s = %lf",
                        pnt_keys->name, i_double );
                    k = strlen( str );
                    if(  k  >  MAX_LINE  ){
                        (void) ims_msg( msgDesc, IMS_ERROR,
                            "Length of line too big for keyword %s: "
                            "only %d allowed, %ld needed.",
                            pnt_keys->name, MAX_LINE, k );
                        return( IMS_ERROR );
                    }
                    strcpy( pnt_keys->line, str );
                }
                else{
                    /*
                    **  all others are character strings
                    */
                    (void) memcpy ( temp,
                        qDesc->valAddr[j], qDesc->valLength[j]);
                    temp[qDesc->valLength[j]] = '\0';
                    ims_truncStr(  temp );

                    if(  i  ==  10  ){
                        /*
                        **  no quotes for time values
                        */
                        (void) sprintf( str, "%s = %s",
                            pnt_keys->name, temp );
                    }
                    else{
                        (void) sprintf( str, "%s = \"%s\"",
                            pnt_keys->name, temp );
                    }
                    k = strlen( str );
                    if(  k  >  MAX_LINE  ){
                        (void) ims_msg( msgDesc, IMS_ERROR,
                            "Length of line too big for keyword %s: "
                            "only %d allowed, %ld needed.",
                            pnt_keys->name, MAX_LINE, k );
                        return( IMS_ERROR );
                    }
                    strcpy( pnt_keys->line, str );
                }
            }
            pnt_keys = pnt_keys->pnt_next;
        }
    }
    /*
    ** See if we found the path
    */
    if(  rowCount  ==  0  )
        return (IMS_WARNING);

    return (IMS_OK);
}   /*  getGranuleName  */
