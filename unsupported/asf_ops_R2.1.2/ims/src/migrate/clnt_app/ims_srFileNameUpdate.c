char *sccs = "@(#)ims_srFileNameUpdate.c	1.1 06/27/97";
/***************************************************************
**
** File:        ims_srFileNameUpdate
**
** Function:    This program is used to change the name of the files
**                  to the ASF standard for the data migration for
**                  R2.1 for scan results files.
**
** Author:      Sean Hardman, David Pass
**
** Date:        6/23/97
**
** Copyright (C) 1996, California Institute of Technology.  U.S.
** Government Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**************************************************************** */

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

#define ZERO 0

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
** Path List Information Structure Definition.
*/
typedef  struct  path_list__t *pnt_path_list_t;
typedef struct path_list__t{
    char path[IMS_COL255_LEN+1];
    int  start_granule_idx;
    int  end_granule_idx;
    pnt_path_list_t pnt_next;
    short  path_done; /* once this dir is analyzed, set to true */
} path_list_t;


/*
** Ext List Information Structure Definition.
*/
typedef  struct  ext_list__t *pnt_ext_list_t;
typedef struct ext_list__t{
    char   format[IMS_COL10_LEN+1];
    short  int type;
    char   extension[IMS_COL10_LEN+1];
    pnt_ext_list_t pnt_next;
} ext_list_t;


/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, LOG_SPEC *);
static int openConnection (IMS_MSG_STRUCT *);
static int getSRInfo (IMS_MSG_STRUCT *, char *, long, char *, char *,
    char *, int *, int *, char *, char * );
static int getGranulesTable (IMS_MSG_STRUCT *, char *, long *);
static  int get_fname_num ( IMS_MSG_STRUCT  *, long * );
static  int update_fname( IMS_MSG_STRUCT *, char *, char *, char *,
    long );
static  int get_path_info ( IMS_MSG_STRUCT *, long, pnt_path_list_t * );
static  int get_ext ( IMS_MSG_STRUCT *, pnt_ext_list_t *, long  );
static  int update_order(  IMS_MSG_STRUCT *, char *, char *, long,
    long );
static int checkRetStatus (IMS_MSG_STRUCT * );


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
    char *historyName;
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
    {"+release",        &commands.release},
    {"-o",              &commands.historyName},
    {"-outputFile",     &commands.historyName}
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
    {"updateFlag",      &commands.updateFlag},
    {"outputFile",      &commands.historyName},

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
static char cmdBuf[255];
static char *historyName;

/***********************************************************
**
** main ()
**
************************************************************ */

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
    long granule_idx;
    char old_name[IMS_COL30_LEN+1];
    char platform[IMS_COL10_LEN+1];
    char mode[6];
    char  frame_mode[11];
    char  media_type[6];
    int revolution, sequence;
    FILE *fileDesc;
    int count;
    char log_msg[IMS_COL255_LEN+1];
    char new_name[65];
    char  str[257], str2[257];
    long  i,j,k;
    long  num_fname_id;
    long  dataset_idx;
    pnt_ext_list_t  pnt_ext_1st, pnt_ext; /* list of suffixes */
    pnt_path_list_t  pnt_path_1st, pnt_path; /* list of directories */
    short  flag;
    static  FILE  *  out_file;


    server = database = username = password = NULL;

    commands.username = NULL;
    commands.password = NULL;
    commands.dataset = NULL;
    commands.startIdx = NULL;
    commands.endIdx = NULL;
    commands.logFilePath = NULL;
    commands.logFileName = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.logFlag = NULL;
    commands.updateFlag = NULL;
    commands.help = NULL;
    commands.release = NULL;
    commands.historyName = NULL;
    out_file = NULL;

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
        ** Now, get command-line arguments again to overlay file args
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
    dataset_idx = -1;
    if ((strncmp (dataset, "granules_", (size_t) 9) == 0) &&
        ((int) strlen (dataset) <= IMS_COL30_LEN))
    {
        (void) strcpy (granules_table, dataset);
        (void) strcpy (dataset, "UNKNOWN");
        dataset_idx = atoi( dataset+9 );
    }
    else /* Query for the granules table name. */
    {
        if ((status = getGranulesTable (msgDesc, granules_table,
            &dataset_idx )) < IMS_OK){
            (void) ims_msg (msgDesc, status,
                "Could not get granule table name for dataset '%s'.",
                dataset);
            goto ERROR;
        }
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
        status = access( logFileSpec, F_OK );
        if(  status  !=  0 ){ /* file not there: create it  */
            if ((out_file = fopen( logFileSpec, "w")) ==
                NULL){
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not open log file '%s'. %s",
                    logFileSpec, strerror (errno));
                goto ERROR;
            }
            (void) fclose( out_file );
        }
        if ((fd = open (logFileSpec, O_WRONLY|O_APPEND)) == -1)
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
            "\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_srFileNameUpdate"
            "  dataset %ld  <<<<<<<<<<<<<<<<<<<<<<\n\n", dataset_idx );

        if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not write to log file '%s'. %s",
                logFileSpec, strerror (errno));
            goto ERROR;
        }
    }

    /*
    ** Open the history file.
    */

    if(  updateFlag  ==  IMS_TRUE  ){
        if ((fileDesc = fopen(historyName, "a")) == NULL){
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not open history file '%s'", historyName);
            goto ERROR;
        }
        /* *****************
        (void) sprintf (log_msg,
            "\n*****  Dataset %ld\n\n", dataset_idx);
        (void) fwrite (log_msg, 1, strlen(log_msg),
            fileDesc);
        ******************** */
    }

    /*
    ** Informational message regarding update status.
    */
    if (updateFlag == IMS_FALSE)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Updating of file names has been disabled.");
    }

    /*
    **  need to get the path and extension info for this dataset
    */
    status = get_ext( msgDesc, &pnt_ext_1st, dataset_idx );
    if(  status  <  IMS_OK  ){
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not get extension info for dataset %ld.",
            dataset_idx );
        goto ERROR;
    }

    status = get_path_info( msgDesc, dataset_idx, &pnt_path_1st );
    if(  status  <  IMS_OK  ){
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not get path info for dataset %ld.",
            dataset_idx );
        goto ERROR;
    }

    /*
    ** Display some informational messages.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Starting the Scan Results File file name update at granule_idx"
        " %ld in granules table '%s' for dataset '%s'.",
        startIdx, granules_table, dataset);


    /*
    ** Process every record in the range.
    */
    for (granule_idx = startIdx; granule_idx < endIdx+1; granule_idx++)
    {
        /*
        ** Get Data Sequence information.
        */
        if ((status = getSRInfo (msgDesc, granules_table,
            granule_idx, old_name, platform, mode,
            &revolution, &sequence, frame_mode, media_type))
            < IMS_WARNING){
            (void) ims_msg (msgDesc, status,
                "Error occurred getting information for granule_idx "
                "'%ld' in granules table '%s' for dataset '%s'.",
                granule_idx, granules_table, dataset);
            goto ERROR;
        }

        if (status == IMS_WARNING)
        {
            (void) ims_msg (msgDesc, status,
                "A granule record did not exist for index '%ld' in "
                "granules table '%s' for dataset '%s'.",
                granule_idx, granules_table, dataset);
        }
        else
        {
            /*
            ** Generate new file name.
            */
            new_name[0] = '\0';
            (void) strcat( new_name, platform );
            (void) sprintf( str, "%ld", revolution );
            i = strlen( str );
            if(  i  ==  1  )  (void) strcat( new_name, "0000" );
            else  if(  i  ==  2  )  (void) strcat( new_name, "000" );
            else  if(  i  ==  3  )  (void) strcat( new_name, "00" );
            else  if(  i  ==  4  )  (void) strcat( new_name, "0" );
            (void) strcat( new_name, str );
            (void) sprintf( str, "%ld", sequence );
            i = strlen( str );
            if(  i  ==  1  )  strcat( new_name, "0" );
            (void) strcat( new_name, str );

            mode[3] = '\0'; /* make sure not more than 3 chars */
            strcat( new_name, mode );
            str[0] = '\0';
            if(  strcmp( frame_mode, "ARCTIC" )  ==  0  )
                (void) strcpy( str, "N" );
            else  if(  strcmp( frame_mode, "ANTARCTIC" ) ==  0  )
                (void) strcpy( str, "S" );
            else{
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "*** frame_mode not recognized for granule %ld:"
                    " '%s'.", granule_idx, frame_mode );
                (void)  strcpy( str, "X" );
            }
            strcat( new_name, str );

            str[0] = '\0';
            if(  strcmp( media_type, "DCRSI" )  ==  0  )
                (void) strcpy( str, "D" );
            else  if(  strcmp( media_type, "ID-1" )  ==  0  )
                (void) strcpy( str, "I" );
            else  if(  strcmp( media_type, "DISK" )  ==  0  )
                (void) strcpy( str, "F" );
            if(  str[0]  ==  '\0'  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "*** media_type not recognized for granule %ld:"
                    " '%s'.", granule_idx, media_type );
                (void) strcpy( str, "X" );
            }
            strcat( new_name, str );
            /*
            **  get the first file name id number
            */
            if (updateFlag == IMS_TRUE)
            {
                status = get_fname_num( msgDesc, &num_fname_id );
            }
            else  num_fname_id = 123;
            if(  num_fname_id  >  1000  ){
                num_fname_id = num_fname_id % 1000;
            }
            (void) sprintf( str, "%ld", num_fname_id );
            i = strlen( str );
            if(  i  ==  1  )  (void) strcat( new_name, "00" );
            else  if(  i  ==  2  )  (void) strcat( new_name, "0" );
            strcat( new_name, str );

            /*
            ** Update affected file and records.
            */
            if (updateFlag == IMS_TRUE){
                /*
                ** Rename file in archive.
                */
                status = update_fname( msgDesc, granules_table,
                    old_name, new_name, granule_idx );

                /*
                ** Update file name in granules_table.
                */
                pnt_path = pnt_path_1st;
                while( pnt_path  !=  NULL  ){
                    i = pnt_path->start_granule_idx;
                    j = pnt_path->end_granule_idx;
                    flag = IMS_FALSE;
                    if(  i  ==  -1  &&  j  ==  -1  ) flag = IMS_TRUE;
                    else  if(  i  ==  -1  &&  j  >=  granule_idx  )
                        flag = IMS_TRUE;
                    else  if(  j  ==  -1  &&  i  <=  granule_idx  )
                        flag = IMS_TRUE;
                    else  if(  i  <=  granule_idx  &&  j  >=
                        granule_idx  )  flag = IMS_TRUE;
                    if(  flag ){
                        /*
                        **  this is the valid path for this granule
                        **      change file name for each extension
                        */
                        pnt_ext = pnt_ext_1st;
                        while( pnt_ext  !=  NULL  ){
                            (void) strcpy( str, pnt_path->path );
                            (void) strcat( str, "/" );
                            (void) strcat( str, old_name );
                            (void) strcat( str, "." );
                            (void) strcat( str, pnt_ext->extension );

                            (void) strcpy( str2, pnt_path->path );
                            (void) strcat( str2, "/" );
                            (void) strcat( str2, new_name );
                            (void) strcat( str2, "." );
                            (void) strcat( str2, pnt_ext->extension );

                            status = 0;
                            status = rename( str, str2 );
                            if(  status  <  0  ){
                                (void) ims_msg( msgDesc, IMS_ERROR,
                                    "Could not rename files: old = "
                                    "'%s'.", str );
                                return;
                            }
                            pnt_ext = pnt_ext->pnt_next;
                        }
                    }
                    pnt_path = pnt_path->pnt_next;
                }
                /*
                ** Update file name in order_item table.
                */
                status = IMS_OK;
                status = update_order( msgDesc, old_name, new_name,
                    dataset_idx, granule_idx );
                if(  status  <  IMS_OK ){
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "Could not update orders from name '%s'.",
                        old_name );
                }

                /*
                ** Write an entry to the history file.
                */
                (void) sprintf (log_msg,
                    "%s   %s\n", old_name, new_name );
                (void) fwrite (log_msg, 1, strlen(log_msg),
                    fileDesc);
            }
            else{
                (void) ims_msg (msgDesc, IMS_INFO,
                    "Would have renamed the file '%s' to '%s'.",
                    old_name, new_name);
            }
        }
    }

    (void) ims_msg (msgDesc, IMS_INFO,
        "Ending the Data Sequence file name update at index '%d' "
        "in granules table '%s' for dataset '%s'.",
        endIdx, granules_table, dataset);

    /*
    ** Clean-up.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The %s application completed successfully.", programName);

    (void) ims_qiFreeDesc(qDesc);
    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);
    if(  updateFlag  ==  IMS_TRUE  )  (void) fclose (fileDesc);

    exit (0);

ERROR:
    /*
    ** Clean-up.
    */
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The %s application was not successful.", programName);

    if (qDesc != (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_qiFreeDesc (qDesc);
    }
    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);
    if(  updateFlag  ==  IMS_TRUE  )  (void) fclose (fileDesc);

    exit (1);
}   /* main */

/***************************************************************
**
** subr runDown ()
**
** Cleanup and exit from program.
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
** Print command line argument switches.
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
** Process command-line and command-file arguments.
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
    number = 0;
    if (commands.startIdx == (char *) NULL)
    {
        /* We expect a number. */
        do
        {
            if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
                "Starting Index: ") == (char *) NULL)
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
                    (void) printf (
                        "Expecting numerical input. Try again.\n");
                    invalid = 1;
                    break;
                }
            }

            number = (int) atoi (inputBuffer);
            if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
                ((number < ZERO) || (number > IMS_MAX_INT)))
            {
                invalid = 1;
                (void) printf (
                    "Numerical input is not in the range of '%d' to "
                    "'%d'.  Try again.\n",
                    ZERO, IMS_MAX_INT);
            }
        }while (invalid);

        startIdx = (int) number;
    }
    else if (ims_isInteger (commands.startIdx) == IMS_TRUE)
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
           "The parameter 'startIdx' must have a valid integer value.");
        return (IMS_ERROR);
    }

    /* endIdx */
    number = 0;
    if (commands.endIdx == (char *) NULL)
    {
        /* We expect a number. */
        do
        {
            if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
                "Ending Index: ") == (char *) NULL)
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
                    (void) printf (
                        "Expecting numerical input. Try again.\n");
                    invalid = 1;
                    break;
                }
            }

            number = (int) atoi (inputBuffer);
            if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
                ((number < ZERO) || (number > IMS_MAX_INT)))
            {
                invalid = 1;
                (void) printf (
                    "Numerical input is not in the range of '%d' "
                    "to '%d'. Try again.\n",
                    ZERO, IMS_MAX_INT);
            }

            if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
                (number < startIdx))
            {
                invalid = 1;
                (void) printf (
                    "Numerical input must be greater than or equal to "
                    "the\nstarting index of '%d'. Try again.\n",
                    startIdx);
            }

        }while (invalid);

        endIdx = (int) number;
    }
    else if (ims_isInteger (commands.endIdx) == IMS_TRUE)
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
            "The parameter 'endIdx' must have a valid integer value.");
        return (IMS_ERROR);
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
    server = getenv("IMS_SERVER");
    if (commands.server != (char *) NULL)
    {
        server = commands.server;
    }

    /* database */
    database = getenv("IMS_DB");
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
    if (commands.updateFlag != (char *) NULL)
    {
        updateFlag = IMS_TRUE;
    }
    else
    {
        updateFlag = IMS_FALSE;
    }

    /* historyName */
    if (commands.historyName != (char *) NULL)
    {
        historyName = commands.historyName;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Output File: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        historyName = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (historyName, inputBuffer);
    }

    return (IMS_OK);
}   /* getArgInput */

/***************************************************************
**
**  subr openConnection ()
**
** Open a connection to the database server.
**
**************************************************************** */

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
}

/***************************************************************
**
**  subr getSRInfo ()
**
** Query the database server for the next scan results record.
**
**************************************************************** */

static int getSRInfo (
    IMS_MSG_STRUCT *msgDesc,
    char *granules_table,
    long granule_idx,
    char *old_name,
    char *platform,
    char *mode,
    int *revolution,
    int *sequence,
    char *frame_mode,
    char *media_type )
{
    short sequence_temp;
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
        "select name, PLATFORM, MODE, "
        "REVOLUTION, SEQUENCE, FRAME_MODE, MEDIA_TYPE  "
        "from %s "
        "where granule_idx = %d",
        granules_table, granule_idx);

    qDesc->cmd = cmdBuf;

    /*
    ** Process the result row for this query.
    */
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (IMS_ERROR);
        }

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
    }

    /*
    ** See if we got one row back.
    */
    if (IMS_AFFECTED (qDesc) < 1)
    {
        return (IMS_WARNING);
    }

    /*
    ** See if we got more than one row back.
    */
    if (IMS_AFFECTED (qDesc) > 1)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Obtained more than one row of matching granule information"
            " for granule_idx '%d': '%s'",
            granule_idx, cmdBuf);
        return (IMS_ERROR);
    }

    /*
    ** Get the returned data.
    */

    /* name */
    (void) memcpy (old_name, IMS_VALUE (qDesc, 0),
        IMS_VALUELENGTH (qDesc, 0));
    old_name[IMS_VALUELENGTH (qDesc, 0)] = '\0';
    (void) ims_truncStr (old_name);

    /* PLATFORM */
    (void) memcpy (platform, IMS_VALUE (qDesc, 1),
        IMS_VALUELENGTH (qDesc, 1));
    platform[IMS_VALUELENGTH (qDesc, 1)] = '\0';
    (void) ims_truncStr (platform);

    /* MODE */
    (void) memcpy (mode, IMS_VALUE (qDesc, 2),
        IMS_VALUELENGTH (qDesc, 2));
    mode[IMS_VALUELENGTH (qDesc, 2)] = '\0';
    (void) ims_truncStr( mode );

    /* REVOLUTION */
    (void) memcpy( revolution, IMS_VALUE (qDesc, 3),
        IMS_VALUELENGTH (qDesc, 3));

    /* SEQUENCE */
    (void) memcpy (&(sequence_temp), IMS_VALUE (qDesc, 4),
        IMS_VALUELENGTH (qDesc, 4));
    *sequence = sequence_temp;

    /* FRAME_MODE  */
    (void) memcpy( frame_mode, IMS_VALUE (qDesc, 5),
        IMS_VALUELENGTH (qDesc, 5));
    frame_mode[IMS_VALUELENGTH (qDesc, 5)] = '\0';
    (void) ims_truncStr( frame_mode );

    /* MEDIA_TYPE  */
    (void) memcpy( media_type, IMS_VALUE (qDesc, 6),
        IMS_VALUELENGTH (qDesc, 6));
    media_type[IMS_VALUELENGTH (qDesc, 6)] = '\0';
    (void) ims_truncStr( media_type );

    return (IMS_OK);
}   /*  getSRInfo   */


/***************************************************************
**
**  subr getGranulesTable ()
**
** Query the database server for the granule table name.
**
**************************************************************** */

static int getGranulesTable (
    IMS_MSG_STRUCT *msgDesc,
    char *granules_table,
    long * dataset_idx )
{
    int status;
    short spatial_type;
    short i_s;

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
    (void) memcpy ( &i_s,
        qDesc->valAddr[1], qDesc->valLength[1]);
    *dataset_idx = i_s;

    return (IMS_OK);
}   /*  getGranulesTable  */


/***************************************************************
**
**  subr get_fname_num ()
**
**  This routine gets the last file name id number
**
**************************************************************** */
static  int get_fname_num (
    IMS_MSG_STRUCT  *msgDesc,
    long *     fname_id )
{
    int status;
    int rowCount;
    short keyword_idx;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "get_fname_num: Could not reset the query descriptor.");
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
        "incr_product_version" );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "get_fname_num: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "get_fname_num: Could not get filename_id." );
        return (IMS_ERROR);
    }

    (void) memcpy ( fname_id, qDesc->valAddr[0],
        qDesc->valLength[0]);
    return( status );
}   /*  get_fname_num  */


/***************************************************************
**
**  subr update_fname ()
**
**  This routine updates the name variable for the given granule row
**
**************************************************************** */
static  int update_fname(
    IMS_MSG_STRUCT  *msgDesc,
    char *  granules_table,
    char *  old_name,
    char *  new_name,
    long  granule_idx )
{
    int status;
    int rowCount;
    short keyword_idx;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "update_fname: Could not reset the query descriptor.");
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
        "update %s  set  name = '%s'  where  granule_idx = %ld",
        granules_table, new_name, granule_idx );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "update_fname: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "update_fname: Could not update the name for granule %ld"
            " in table %s", granule_idx, granules_table );
        return (IMS_ERROR);
    }
    (void)  ims_msg( msgDesc, IMS_INFO,
        "Updated granule name using: %s.", cmdBuf );

    return( status );
}   /*  update_fname  */


/***************************************************************
**
**  subr update_order ()
**
**  This routine updates the order_item table: any orders for the
**      old name must be changed.
**
**************************************************************** */
static  int update_order(
    IMS_MSG_STRUCT  *msgDesc,
    char *  old_name,
    char *  new_name,
    long  dataset_idx,
    long  granule_idx )
{
    int status;
    int rowCount;
    short keyword_idx;
    int  i;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "update_order: Could not reset the query descriptor.");
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
        "update order_item  set  name = '%s'  where  name = '%s'",
        new_name, old_name );

    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "update_order: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    i = IMS_AFFECTED (qDesc);
    if(  i  <  1 ){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "update_order: No orders with name '%s'.",
            old_name );
    }
    else{
        (void)  ims_msg( msgDesc, IMS_INFO,
            "Updated %ld orders using: %s.", i, cmdBuf );
    }
    return( status );
}   /*  update_order  */


/* *****************************************************************
**
**  subr  get_ext is called to get all the extensions for the
**      given dataset.
**
***************************************************************** */
int   get_ext(
    IMS_MSG_STRUCT *msgDesc,
    pnt_ext_list_t  *pnt_ext_out_1st,
    long  dataset_idx )
{
    int status;
    long  i,j;
    char  str[1024];
    short  i_s;
    pnt_ext_list_t  pnt_ext;
    short  extCount;


    status = ims_qiResetDesc( qDesc );

    sprintf(qDesc->cmd, "select format, extension, type "
        "from dataset_file_policy where dataset_idx = %d",
        dataset_idx);

    extCount = 0;
    pnt_ext = NULL;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't cache dataset information. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        extCount++;
        if (extCount == 1){
            pnt_ext = (void *) malloc(sizeof(path_list_t));

            if (pnt_ext == NULL){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory to cache dataset "
                    "info");
                return(IMS_FATAL);
            }
            *pnt_ext_out_1st = pnt_ext;
        }
        else{
            pnt_ext->pnt_next = (void *) malloc(
                sizeof(path_list_t));
            if (pnt_ext->pnt_next == NULL){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory to cache dataset "
                    "info.");
                return(IMS_FATAL);
            }
            pnt_ext = pnt_ext->pnt_next;
        }

        pnt_ext->pnt_next = NULL;


        /*
        ** Get File Extension Format
        */
        memset(pnt_ext->format, 0, sizeof(pnt_ext->format));

        memcpy(pnt_ext->format, qDesc->valAddr[0],
            qDesc->valLength[0]);
        ims_trim(pnt_ext->format);

        /*
        ** Get Extension
        */
        memset(pnt_ext->extension, 0, sizeof(pnt_ext->extension));

        memcpy(pnt_ext->extension, qDesc->valAddr[1],
            qDesc->valLength[1]);
        ims_trim(pnt_ext->extension);

        /*
        ** Get the file type
        */
        memcpy(&(pnt_ext->type), qDesc->valAddr[2],
                qDesc->valLength[2]);
    }
    /*
    ** Check the returned status value.
    */
    if( extCount  ==  0  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "get_ext could not get extension for dataset %ld.",
            dataset_idx );
        return ( IMS_ERROR );
    }
    return( IMS_OK );
}   /*  get_ext  */


/***************************************************************
**
**  subr get_path_info ()
**
**  This routine gets the dataset_path_policy information
**      for each dataset type.
**
**************************************************************** */
static  int get_path_info (
    IMS_MSG_STRUCT  *msgDesc,
    long        dataset_idx,
    pnt_path_list_t * pnt_path_1st )
{
    int status;
    long  i, index;
    int pathCount;
    short  i_s;
    pnt_path_list_t pnt_path;


    /*
    ** Reset Query Descriptor
    */
    if (ims_qiResetDesc(qDesc) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    sprintf(qDesc->cmd, "select path, start_granule_idx, "
        "end_granule_idx from dataset_path_policy where "
        "dataset_idx = %d", dataset_idx);

    pathCount = 0;
    pnt_path = NULL;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't cache dataset information. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        pathCount++;
        if (pathCount == 1){
            pnt_path = (pnt_path_list_t) malloc(sizeof(path_list_t));
            if( pnt_path  ==  NULL ){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory for path list info." );
                return(IMS_FATAL);
            }
            *pnt_path_1st = pnt_path;
        }
        else{
            pnt_path->pnt_next = (pnt_path_list_t) malloc(
                sizeof(path_list_t));
            if( pnt_path->pnt_next  ==  NULL ){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory to cache dataset "
                    "info.");
                return(IMS_FATAL);
            }
            pnt_path = pnt_path->pnt_next;
        }
        pnt_path->pnt_next = NULL;
        pnt_path->path_done = IMS_FALSE;

        /*
        ** Get Dataset Path Specification
        */
        memset(pnt_path->path, 0, sizeof(pnt_path->path));

        memcpy(pnt_path->path, qDesc->valAddr[0],
            qDesc->valLength[0]);
        ims_trim(pnt_path->path);

        /*
        ** Get Start Granule Index
        */
        memcpy((char *) &(pnt_path->start_granule_idx),
            qDesc->valAddr[1], qDesc->valLength[1]);

        if (qDesc->valLength[1] == 0)
            pnt_path->start_granule_idx = -1;

        /*
        ** Get End Granule Index
        */
        memcpy((char *) &(pnt_path->end_granule_idx),
            qDesc->valAddr[2], qDesc->valLength[2]);

        if (qDesc->valLength[2] == 0)
            pnt_path->end_granule_idx = -1;
    }
    if(  pathCount  ==  0  ){
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Could not get path infor for dataset %ld.", dataset_idx );
        return( IMS_ERROR );
    }
    return( IMS_OK );
}   /*  get_path_info  */


/*******************************************************************
**
**  subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************** */

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
